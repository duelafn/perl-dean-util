#!/usr/bin/perl
use strict;
use warnings;

use Test::More tests => 2;
BEGIN { use_ok 'Dean::Util', qw/:display/ }

#-----------------------------------------------------------------
#                   :display - Display functions
#-----------------------------------------------------------------

# lrtext
is( lrtext("  Oregano 0.35", "Simple View  ", 30), "  Oregano 0.35   Simple View  ", "lrtext - 1" );
