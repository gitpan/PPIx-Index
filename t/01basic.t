use Test::More tests => 2;

my $pkg = 'PPIx::Index';

use_ok( $pkg );
ok( defined $pkg->VERSION, 'version has been set');
