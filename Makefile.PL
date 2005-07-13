# require 5.005;

use ExtUtils::MakeMaker;

WriteMakefile(
  NAME         => 'PPIx::Index',
  VERSION_FROM => 'lib/PPIx/Index.pm',
  AUTHOR       => 'Dan Brook',

  PREREQ_PM    => {
    'PPI'              => 0,
    'PPIx::XPath'      => 0,
    'Module::Locate'   => 1.7,
    'Scalar::Util'     => 0,
    'List::MoreUtils'  => 0,
    'Module::CoreList' => 0,
  },
);
