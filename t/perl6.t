#!/usr/bin/perl
use strict;
use warnings;

use Test::More tests => 3;
BEGIN { use_ok 'Dean::Util', qw/:perl6/ }

#-----------------------------------------------------------------
#                    :perl6 - Perl 6 functions
#-----------------------------------------------------------------

my  @array = qw<a b b c d e b b b b f b>;
is(join(" ",uniq(@array)), "a b c d e b f b", "uniq");

@array = (undef, 1, 2, 2, undef, undef, 1, 1, 1);
is(join(" ",map $_||0, uniq(@array)), "0 1 2 0 1", "uniq on undefs");
