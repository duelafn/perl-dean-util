#!/usr/bin/perl
use strict;
use warnings;

use Test::More;
use Dean::TestUtil qw/ :shell /;

# safe_pipe
{
    my ($res, $err);

    my @slow = (perl => -E => 'sleep(1), say "$_" for 1..3');
    $res  = safe_pipe \@slow;

    is $res, "1\n2\n3\n",                   "safe_pipe: slow execution waits";


    my @missing = ("Sir not appearing in this picture.");
    $res  = eval { safe_pipe { capture_err => 1 }, \@missing };
    $err  = "$@";

    is $res, undef,                         "safe_pipe: missing executable (no output)";
    ok $err =~ /child died/,                "safe_pipe: missing executable dies";


    my @fails = (perl => -E => 'exit(1)');
    $res  = eval { safe_pipe \@fails };
    $err  = "$@";

    is $res, undef,                         "safe_pipe: command failed (no output)";
    ok $err =~ /child exited with value 1/, "safe_pipe: command failed dies";
}


done_testing;
