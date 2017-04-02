#!/usr/bin/perl
use strict;
use warnings;

use Test::More tests => 4;
BEGIN { use_ok 'Dean::TestUtil', qw/:utility/ }

#-----------------------------------------------------------------
#                 :utility - Using Dean::TestUtil
#-----------------------------------------------------------------
use File::Temp qw/tempfile/;
my ($fh, $filename) = tempfile() or die;

# Test 2: list_Dean_Util_functions
#---------------------------------
my $stdout = select($fh);
list_Dean_Util_functions();
select($stdout);
close $fh;

$fh = undef;
open $fh, "<", $filename or die "$!";
my $funcs = join " ", sort split /\s+/, join "", <$fh>, " POD_ONLY ", " INCLUDE_POD";
my $exprt = join " ", sort @Dean::TestUtil::EXPORT_OK;
close $fh;

is( "$funcs", "$exprt", "Exportable files matches export list" );


# Test 3: check_Dean_Util_functions
#----------------------------------
open $fh, ">", $filename or die "$!";
check_Dean_Util_functions;
close $fh;

ok( 1 ); # How to test!?


# Test 4: insert_Dean_Util_functions
#-----------------------------------
open $fh, ">", $filename or die "$!";
print $fh "use Dean::TestUtil qw/ $funcs /;";
close $fh;

insert_Dean_Util_functions $filename;

open $fh, "<", $filename or die "$!";
my %code;
for (<$fh>) {
  $code{$1}++ if /^(?:#BEGIN\:|=head3)\s+([\$\%\@]?\w+)/;
}
close $fh;

my $code = join " ", sort keys(%code), "POD_ONLY", "INCLUDE_POD";
ok( $code eq $funcs );
