package PPIx::Index;

$VERSION = '0.0.1_01';

use strict;
use warnings;

use base 'Class::Accessor';
__PACKAGE__->mk_accessors(qw/pkg_vars/);

#use Devel::SimpleTrace;
use Carp 'carp', 'croak';
use Data::Dumper;

use Module::Locate Cache => 0, 'locate';
use Scalar::Util qw/ refaddr reftype /;
use List::MoreUtils 'uniq';
use Module::CoreList;

use PPIx::XPath;
use PPI::Document;
use PPI::Dumper;

{
  package PPIx::Index::Script;
  BEGIN{ @PPIx::Index::Script::ISA = 'PPIx::Index' }
  ## XXX - base overwrites @ISA :/
  use base 'Class::Accessor';
  __PACKAGE__->mk_accessors(qw/
    decl_vars lex_vars_in_subs pkg_vars lex_vars packages subs
    doc name
  /);
}

## XXX - allow this to be limited per object (maybe)
our($VERBOSE, $SILENCE);
sub import {
  ## XXX - this could be done better ...
  $VERBOSE = "@_" =~ /-v/;
  $SILENCE = "@_" =~ /-q/;
}

sub BLAB {
  print @_,$/
    if $VERBOSE and !$SILENCE;
}
sub SPEW {
  goto &carp
    unless $SILENCE;
}

sub new {
  my($class, %args) = @_;

  my $obj = $class->_init_obj(\%args);
  $obj->_index_doc;
  ## XXX - undef the PPI objects here ??

  return $obj;
}

## XXX - this probably exists in a param module somewhere
sub _force_array {
  my($args,$k) = @_;
  $args->{$k} = []
    if !exists $args->{$k};
  $args->{$k} = [$args->{$k}]
    if exists $args->{$k} and reftype $args->{$k} ne 'ARRAY';
}

sub _init_obj {
  my($class, $args) = @_;

  my $script = exists $args->{script}   && delete $args->{script}
            || exists $args->{file}     && delete $args->{file}
            || exists $args->{document} && delete $args->{document}
            || croak "No script provided for PPIx::Index constructor";

  _force_array($args, $_)
    for qw/ name_constraint path_constraint /;

  ## default constraints
  push @{ $args->{name_constraint} }, sub {
    return !Module::CoreList->first_release($_[0]);
  } unless exists $args->{include_core};
  push @{ $args->{name_constraint} }, sub {
    return "$_[0]" ne 'PPIx::Index';
  } unless exists $args->{include_indexr};

  ## XXX - add the ability to force the indexing of scripts so things which
  ##       are included dynamically can also be indexd 
  my $obj = bless {
    %$args,
    scripts => {
      $script => bless { name => $script }, "$class\::Script"
    },
    scripts_in_order => [$script],
    root_script      => $script,
  }, $class;
  
  return $obj;
}

sub _index_doc {
  my $obj = shift;
  $obj->_setup_docs( $obj->{root_script} );

  $obj->index_vars;
  $obj->index_pkgs;
  $obj->index_subs;
}

## Accessors, accessors! Get your accessors, get 'em while they're hot, get 'em
## while they're buttered!
sub name_constraints { return $_[0]->{name_constraint} }
sub path_constraints { return $_[0]->{path_constraint} }

sub script {
  my($self, $script) = @_;

  croak "No such script ", Dumper($script)
    if !exists $self->{scripts}{$script};

  return $self->{scripts}{$script}
}
sub scripts {
  return map $_[0]->{scripts}{$_}, @{ $_[0]->{scripts_in_order} };
}
sub script_names {
  return @{ $_[0]->{scripts_in_order} };
}

sub constrain {
  my $self        = shift;
  my $constraints = shift;

  return grep {
    my $el = $_;
    @$constraints == grep $_->($el), @$constraints;
  } @_;
}

## XXX - slim this function down, it is teh ph4t
sub _setup_docs {
  my $self        = shift;
  my $script      = shift;
  my $script_seen = shift || {};

  ## XXX - does this need a better accessor method?
  $self->{scripts}{$script}{doc} = my $doc = PPI::Document->new( $script );
  $doc->index_locations;

  my @includes = grep !/^v?\d+\.[\d_]+$/,
                 map { ($_->schildren)[1] }
                     $doc->xpmatch('//Statement::Include');

  my @script_paths;
  for( $self->constrain($self->name_constraints, @includes) ) {
    local $@;
    my $locstr = $_ =~ $Module::Locate::PkgRe ? "$_" : eval $_;
    ## probably a dynamic include
    SPEW("Couldn't get a meaningful filename [in $script] from: '$_'"), next
      if not defined $locstr or $@;
      
    my $fn = locate( $locstr );

    SPEW("Couldn't find file for '$_'"), next
      if not defined $fn;

    next
      if $script_seen->{$fn}++;

    push @script_paths, $fn;
  }

  for my $fn ( uniq $self->constrain($self->path_constraints, @script_paths) ) {
    $self->{scripts}{$fn} = bless { name => $fn }, ref($self) . "::Script";
    push @{ $self->{scripts_in_order} }, $fn;

    BLAB "setting up document for: $fn in $script";

    $self->_setup_docs($fn, $script_seen);
  }

  return $self;
}

sub index_vars {
  my $self = shift;
  ## package_vars also calls the methods it relies on lexical_vars, which in
  ## turn calls lexicals_in_subs
  $self->package_vars($_)
    for $self->script_names;
}

sub index_subs {
  my $self = shift;
  $self->_index_subs_in_script($_)
    for $self->script_names;
}

sub index_pkgs {
  my $self = shift;
  $self->_index_pkgs_in_script($_)
    for $self->script_names;
}

## all's fair if you predeclare ...
sub Hash_em;

sub lexicals_in_subs {
  my($parent, $name) = @_;
  
  my $script = $parent->script($name);
  my $doc    = $script->doc;
  BLAB "Getting lexicals in subs for: ", $script->name;

  my(@decl_vars, %vars_in_subs);
  my $anon_num = 0;
  for( $script->doc->subs ) {
    my @res = map Names_from_decl($_), $_->xpmatch("//Token::Word[.='my']");
    push @decl_vars, map $_->canonical, @res;

    $vars_in_subs{ eval{$_->name} || '__ANON__'.$anon_num++} = { Hash_em @res }
      if @res;
  }

  @$script{qw/ decl_vars lex_vars_in_subs /} =
    ( { Hash_em @decl_vars }, \%vars_in_subs );

  return $script;
}

sub lexical_vars {
  my($parent, $name) = @_;

  my $script = $parent->script($name);
  my $doc    = $script->doc;
  BLAB "Getting lexicals vars for: ", $script->name;

  $parent->lexicals_in_subs($name)
    unless exists $script->{lexicals_in_subs};

  my $lex_vars = {
    Hash_em
      map  Names_from_decl($_),
      grep !$_->is_in_sub,
           $doc->xpmatch("//Token::Word[.='my']")
  };

  $script->{lex_vars}{$_} = $lex_vars->{$_}
    for uniq grep !exists($script->{lexicals_in_subs}{$_}),
                  keys %$lex_vars;

  return $script;
}

sub package_vars {
  my($parent, $name) = @_;

  my $script = $parent->script($name);
  my $doc    = $script->doc;
  BLAB "Getting package vars for: ", $script->name;

  $parent->lexical_vars($name)
    unless exists $script->{lex_vars};

  my $pkg_vars = {
      Hash_em sub{Fullname(@$_)}, sub{$_->[0]->location->[0]},
      map( [$_, Names_from_decl($_)],
           $doc->xpmatch("//Token::Word[.='our']") ),
      map( [$_, Names_from_use($_)],
           $doc->xpmatch("//Statement::Include/Token::Word[.='vars']") ),
  };

  $script->{pkg_vars}{$_} = $pkg_vars->{$_}
    for keys %$pkg_vars;

  my @globals = keys %{ $parent->{pkg_vars} };
  $script->{decl_vars}{$_} = undef
    for @globals;

  $script->{pkg_vars}{Fullname($_,$_->canonical)} = $_->location->[0]
    for grep {
      !$script->Is_lexical_in_sub( $_ )
        and
      !exists $script->{lex_vars}{$_}
    } uniq $script->doc->vars;

  ## since package variables are inherently global, stick them in the parent too
  $parent->{pkg_vars}{$_}{$name} = $script->{pkg_vars}{$_}
    for keys %{$script->{pkg_vars}};

  return $script;
}

sub _index_subs_in_script {
  my($parent, $name) = @_;

  my $script = $parent->script($name);
  BLAB "Getting subs for: ", $script->name;

  $script->{subs} = {
    Hash_em \&Fullname,
            grep !$_->is_anon_sub, $script->doc->subs
  };

  return $script;
}

sub _index_pkgs_in_script {
  my($parent, $name) = @_;

  my $script = $parent->script($name);
  BLAB "Getting packages for: ", $script->name;

  $script->{packages} = {
    Hash_em sub { $_[0]->namespace },
    $script->doc->xpmatch('//Statement::Package')
  };

  return $script;
}

## helper functions, they have ucfirst names to indicate functionness
sub Hash_em {
  my $key = (ref $_[0] and (reftype($_[0]) eq 'CODE')) ? shift : sub { $_[0] };
  my $val = (ref $_[0] and (reftype($_[0]) eq 'CODE')) ? shift : sub {
              UNIVERSAL::can($_[0], 'location') ? $_[0]->location->[0] : undef
            };
  return map { &$key( $_ ) => &$val( $_ ) } @_;
}

sub Is_lexical_in_sub {
  my($script, $var) = @_;
  $script->lexicals_in_subs
    unless exists $script->{lex_vars_in_subs};
  return ! ! grep {
    exists $_->{$var}
  } values %{ $script->{lex_vars_in_subs} };
}

sub Names_from_decl {
  my $el = shift;
  return $el
    if $el->type eq 'Token::Symbol';
  return $el->schild->xpmatch('//Token::Symbol')
    if $el->type eq 'Structure::List';
  return Names_from_decl($el->snext_sibling);
}

sub Var_name_from_subscript {
  my $el = shift;

  my $subs = $el->snext_sibling;
  my $type = $subs =~ /^\{/ ? '%'
           : $subs =~ /^\[/ ? '@'
           : croak("Unknown subscript in '$el': $subs");

  (my $sym = $el->canonical) =~ s{^\$}{$type};
  return $sym;
}

sub Symbol_to_varname {
  my $el = shift;

  my($cast, $subs) = ( $el->sprevious_sibling, $el->snext_sibling );
  ## look for subscripting only if the variable *hasn't* been cast
  ## i.e @foo{@k} == discern from subscript,               return %foo
  ##    @$foo{@k} == don't discern, the var has been cast, return $foo
  return Var_name_from_subscript($el)
    if   UNIVERSAL::isa $subs, 'PPI::Structure::Subscript'
    and !UNIVERSAL::isa $cast, 'PPI::Token::Cast';

  return $el->canonical;
}

sub Names_from_use {
  my $el     = shift;

  return map {
    my @strs = eval;
    croak "Failed to evaluate '$_' in '$el': $@"
      if $@;
    @strs;
  } grep {
    $_->type =~ '^Token::Quote'
  } $el->parent->schildren;
}

sub Fullname {
  local $@;
  my($node, $name) = ( $_[0], $_[1] || eval{$_[0]->name} || "$_[0]" );
  return $name
    if index($name, '::') != -1;
  $name =~ s/^([\$\@%&*])//;
  return ($1 || '') . In_package($node) . '::' . $name;
}

sub In_package {
  my $n  = shift;

  $n = $n->sprevious_sibling || $n->parent
    while $n and $n->type ne 'Statement::Package';

  return $n ? ($n->xpmatch('//Token::Word'))[-1] : 'main';
}

## XXX - very old piece of code, just look in lex_vars and pkg_vars
# sub find_globals_in_subs {
#   my $parent = shift;
# 
#   for my $sub ( $parent->doc->subs ) {
#     my @vars = $sub->xpmatch('//Token::Symbol')
#       or next;
# 
#     my $lex_vars = $parent->{lex_vars}{$sub->name};
#     $parent->{globals_in_subs}{$sub->name} = {
#       Hash_em grep !exists $lex_vars->{Symbol_to_varname($_)}, @vars
#     };
#   }
# 
#   return $parent;
# }

## helper methods, apologies in advance for namespace pollution
sub PPI::Element::type { substr(ref($_[0]), 5) }
## XXX - this XPath stuff could probably be replaced with a wrapped find()
sub PPI::Element::xpmatch {
  my($el, $path) = @_;
  return PPIx::XPath->new($el)->match($path);
}
sub PPI::Element::is_var {
  return $_[0]->type eq 'Token::Symbol';
}
sub PPI::Element::is_sub {
  return ( $_[0]->type eq 'Statement::Sub'
        or $_[0]->is_anon_sub );
}
sub PPI::Element::is_anon_sub {
  my $block      = shift;
  my $base_type  = eval{$block->sprevious_sibling->content} || return;
  return ( $block->type eq 'Structure::Block'
         and $base_type eq 'sub'
         and $block->parent->type ne 'Statement::Sub' );
}
sub PPI::Element::is_in_sub {
  my $n = shift;
  $n->is_sub and return 1
    while $n = $n->parent;
  return;
}

sub PPI::Document::subs {
  my @declaredsubs = $_[0]->xpmatch('//Statement::Sub');
  my @anonsubs     = grep $_->is_anon_sub,
                          $_[0]->xpmatch('//Structure::Block');

  return ( @declaredsubs, @anonsubs );
}
sub PPI::Document::vars {
  return uniq /^[\$\@\%]/, $_[0]->xpmatch('//Token::Symbol');
}

1;

=pod

=head1 NAME

PPIx::Index - index variable, subroutine and package usage in a perl document

=head1 SYNOPSIS

  use PPIx::Index;

  my $fn = 'somescript.pl';
  my $pa = PPIx::Index->new( script => $fn );

  print "scripts depended on by '$fn':\n", join "\n", $pa->script_names;

  my $subs = $pa->script($fn)->subs;
  print "$_ declared at $subs->{$_}\n"
    for keys %$subs;

=head1 DESCRIPTION

Given a perl script, construct variable, subroutine and package information
about the script and other scripts or modules it depends on. This information,
currently, covers what and where variables, subroutines and packages are used
and declared within their respective files. Due to the fact that this module
performs I<static analysis> any dynamic script/module dependencies will be
missed.

=head2 Purpose

This module was created with the purpose of being able to look at large amounts
of legacy code and making sense out of it quickly. So given a legacy script,
find where globals are declared, what is scoped to a subroutine and so on. As it
stands, this module still needs further modules to produce useful output, but
that's what the helper modules are for.

=head2 Historical Note

This was originally called C<PPIx::Analyze>, but by the time I came to releasing
it I realised it was just an indexer, so switched to the present name.

=head1 METHODS

=over 4

=item new

The constructor method takes the following named parameters I<list>:

=over 8

=item script/file/document

The filename of the document which is to be indexd.

=item name_constraint

Either a coderef or an arrayref of coderefs which will be passed the name of a
an include (something C<use>d or C<require>d to be indexd i.e C<Foo::Bar>,
C<library.pl>. If one of the coderefs returns false then the include will not
be indexd.

By default any core modules (from any version of perl) are skipped and
C<PPIx::Index> is skipped. To override this behaviour see. C<include_core>
and C<include_index>.

=item path_constraint

Either a coderef or an arrayref of coderefs which will be passed the path of a
an include (something C<use>d or C<require>d to be indexd i.e
C</usr/local/perl5/Carp.pm>, C<C:\dev\perllibs\logging.pl>. If one of the
coderefs returns false then the include will not be indexd.

=item include_core

Include core modules when indexing.

=item include_index

Include C<PPIx::Index> when indexing.

=back

=head2 PPIx::Index accessor methods

=item script

Return the C<PPIx::Index::Script> object representing the filename provided.

=item script_names

A list of script names as depended upon the base script, sorted by the order
in which they are required.

=item scripts

A list of script objects sorted by dependency of the base script.

=back

=head2 PPIx::Index::Script accessor methods

Each of the following accessors returns a hash where the keys are the names
approriate to the accessor and values are the line numbers on which they occur.

=over 4

=item lex_vars

Returns a hash of the lexical varables declared outside subroutines in the
script e.g 

  { '%seen' => 130 }

=item pkg_vars

Returns a hash of package variables declared or used (i.e no strictness) e.g

  { '@ISA' => 3 }

=item subs

Returns a hash of subroutines declared in the script e.g

  { 'new' => 21 }

=item packages

Returns a hash of the packages used in the script e.g

  { 'Foo::Bar' => 1 }

=item lex_vars_in_subs

Returns a hash where the keys are the subroutine names and the values are hashes
with the variable names as the keys and the line numbers they're declared on as
values e.g

  {
    get_foo => {
      '$self' => 34,
      '@rest' => 34,
    },
    parse_bar => {
      '$self'  => 56,
      '$chunk' => 60,
      '$delim' => 60,
    },
  }

=back

=head1 IMPORT

When C<use>d or C<import> is explicitly called the following parameters may be
provided:

=over 4

=item --verbose/-v

If this option is passed in then verbose output is enabled.

=item --quiet/-q

If this option is passed then all output relating to the module is silenced
including warnings.

=back

=head1 CAVEATS

=head2 Development release

This is a development release to give the public a peek at what's going on, so
the API isn't necessarily stable and the documentation is incomplete. It also
means that it is a memory and CPU hog so shouldn't be run on anything that is
important, low-end or generally not robust. Have fun!

=head2 Static analysis

The very nature of L<PPI> is a static analysis of a perl I<document> so dynamic
code is likely to yield less informative results.

=head1 BUGS

=over 4

=item *

Currently lexical variables in anonymous subs will also look to live inside
their parent subs too.

=item *

When subs are referred to using the code sigil e.g C<\&foo> they are included in
the package variables.

=back

=head1 TODO

=over 4

=item *

Write some tests already!

=item *

Abstract data storage, defaulting to SQLite, and cache results so indexing only
needs to be performed once.

=item *

Cross-referencing source-highlighting HTML output module (that was the idea
originally, but abstraction is nice too).

=item *

Store the C<PPI> objects instead of strings, this will probably mean
storing the object along with the values pointed to by the stringified object.
Maybe.

=item *

Document internal methods and functions and just generally document more.

=back

=head1 AUTHOR

Dan Brook C<< <cpan@broquaint.com> >>

=cut
