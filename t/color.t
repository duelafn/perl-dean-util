#!/usr/bin/perl
use strict;
use warnings;

use Test::More tests => 5;
BEGIN { use_ok 'Dean::TestUtil', qw/:color/ }

#-----------------------------------------------------------------
#                          :color - Color
#-----------------------------------------------------------------
my $sol;

$sol = join " ", colors ["black"], format => "hex", n => 3;
is($sol, "000000 000000 000000",   "Colors: single input color");

$sol = join " ", colors ["black"], format => "hex", n => 3, distribute => 1;
is($sol, "000000 000000 000000",   "Colors: single input color, distribute = 1");

$sol = join " ", colors ["black", "black"], format => "hex", n => 3;
is($sol, "000000 000000 000000",   "Colors: duplicate input color");

$sol = join " ", colors ["black", "white"], format => "hex", n => 3;
is($sol, "000000 808080 ffffff",   "Colors: black grey white");
