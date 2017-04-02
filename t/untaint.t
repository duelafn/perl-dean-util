#!/usr/bin/perl -T
use strict;
use warnings;

# NOTE: This will fail unless we call: prove -I "$(pwd)" t/*.t
# Needs -I (not PERL5LIB) and an absolute path to satisfy -T
use Test::More;
use Dean::TestUtil qw/:untaint/;
use Scalar::Util qw/ tainted /;
use re 'taint';

my $TAINTED = substr($^X,0,0);
sub taint { join "", $TAINTED, @_ }


{   # untinat_int
    my @int   = map taint($_), "23\n", qw/ 23 +23 -23 -2343241 +3 0 +0 -0 /;
    my @noint = map taint($_), "23 ",  qw/ foo 1.2 1.2.3 12e3 12.3e4 /;

    for (@int) {
        my $val = untaint_int($_);
        ok tainted($_),    "untaint_int($_): orig remains untainted";
        ok !tainted($val), "untaint_int($_): untaints";
        is $val, int($_),  "untaint_int($_): doesn't change value";
    }

    for (@noint) {
        my $val = untaint_int($_);
        ok tainted($_),  "untaint_int($_): orig remains untainted";
        is $val, undef,  "untaint_int($_): doesn't return value";
    }
}


{   # untaint_num
    my @num   = map taint($_), "23\n", "12.23\n", qw/ 23 +23 -23 -2343241 +3 0 +0 -0 23.12 +23.12 -23.12 -2343241.12 +3.12 0.12 +0.12 -0.12 +.12 -.12 .12 /;
    my @nonum = map taint($_), "23 ",  "12.23 ",  qw/ foo 1.2.3 12e3 12.3e4/;

    for (@num) {
        my $val = untaint_num($_);
        ok tainted($_),    "untaint_num($_): orig remains untainted";
        ok !tainted($val), "untaint_num($_): untaints";
        is $val, 0+$_,     "untaint_num($_): doesn't change value";
    }

    for (@nonum) {
        my $val = untaint_num($_);
        ok tainted($_),  "untaint_num($_): orig remains untainted";
        is $val, undef,  "untaint_num($_): doesn't return value";
    }
}



done_testing;
