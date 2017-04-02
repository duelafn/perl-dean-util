#!/usr/bin/perl
use strict;
use warnings;

use Test::More tests => 24;
BEGIN { use_ok 'Dean::TestUtil', qw/:input/ }

#-----------------------------------------------------------------
#                   :input - Prompting and input
#-----------------------------------------------------------------
ok( Yn("t") eq "y",   "Yn: 1" );
ok( Yn("0") eq "n",   "Yn: 2" );
ok( Yn("!") eq "y",   "Yn: 3" );
ok( Yn(undef) eq "y", "Yn: 4" );
ok( yN("t") eq "y",   "yN: 1" );
ok( yN("!") eq "n",   "Yn: 2" );
ok( yN(undef) eq "n", "yN: 3" );
ok( Tf("t") eq "1",   "Tf: 1" );
ok( Tf("n") eq "0",   "Tf: 2" );
ok( Tf(undef) eq "1", "Tf: 3" );
ok( tF("t") eq "1",   "tF: 1" );
ok( tF("n") eq "0",   "tF: 2" );
ok( tF(undef) eq "0", "tF: 3" );

ok( yn("t") eq "y",     "yn: 1" );
ok( yn("0") eq "n",     "yn: 2" );
ok( yn("!") eq "!",     "yn: 3" );
ok( !defined yn(undef), "yn: 4" );
ok( yn("")  eq "",      "yn: 5" );
ok( tf("t") eq "1",     "tf: 1" );
ok( tf("0") eq "0",     "tf: 2" );
ok( tf("!") eq "!",     "tf: 3" );
ok( !defined tf(undef), "tf: 4" );
ok( tf("")  eq "",      "tf: 5" );


exit unless $ENV{DO_INTERACTIVE_TEST} or $ENV{DO_INTERACTIVE_TEST_ALL};

local $| = 1;
if ($ENV{DO_INTERACTIVE_TEST_ALL}) { # Tested and works as expected: 20 April 2004; 2005-05-13
  print "? prompt: Should echo input\n";
  print "'", prompt(), "'\n";

  print "> prompt: Should echo input\n";
  print "'", prompt("> "), "'\n";

  print "'", prompt("basic help 1: use '?', Should echo input: ",
                    "Help"
                   ), "'\n";

  print "'", prompt("hash help 1: use '? foo', Should echo input: ",
                    {
                     "" => "default help", foo => "Foo Help"}
                   ), "'\n";

  print "'", prompt("No trimming: Should echo input: ",
                    trim => 0
                   ), "'\n";

  print "'", prompt("No trimming, no help: try '?' Should echo input: ",
                    trim => 0
                   ), "'\n";

  print "'", prompt("many help options: try ['?', 'h ', 'help '] Should echo input: ",
                    "Hello", help_command => ['?', 'h ', 'help ']
                   ), "'\n";

  print "'", prompt("default y: Should echo input: ",
                    default =>"y"
                   ), "'\n";

  print "'", prompt("default y: Should echo input: ",
                    default =>"y"
                   ), "'\n";

  print "'", prompt("Empty not allowed: Should echo input: ",
                   ), "'\n";

  print "'", prompt("Empty allowed: Should echo input: ",
                    allow_empty => 1
                   ), "'\n";

  print "'", prompt("Empty + defauly y: Should echo 'y': ",
                    default => 'y', allow_empty => 1
                   ), "'\n";
}

if ($ENV{DO_INTERACTIVE_TEST_ALL}) { # Tested and works as expected: 20 April 2004; 2005-05-13
  print "'", prompt("y/n only (1 more): Should echo y/n: ",
                    allowed => [qw/y n/]
                   ), "'\n";

  print "'", prompt("y/n only (0 more): Should echo y/n: ",
                    allowed => [qw/y n/]
                   ), "'\n";

  print "'", prompt("y/n canonicalizer (2 more): Should echo y/n: ",
                    allowed => qr/^(y|n)/i
                   ), "'\n";

  print "'", prompt("y/n canonicalizer (1 more): Should echo y/n: ",
                    allowed => qr/^(y|n)/i
                   ), "'\n";

  print "'", prompt("y/n canonicalizer (0 more): Should echo y/n: ",
                    allowed => qr/^(y|n)/i
                   ), "'\n";

}
