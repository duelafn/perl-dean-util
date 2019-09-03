#!/usr/bin/perl
use strict;
use warnings;

use Test::More tests => 32;
BEGIN { use_ok 'Dean::Util', qw/:list/ }



#-----------------------------------------------------------------
#                      :list - List Utilities
#-----------------------------------------------------------------
# find_index
is( (find_index { $_ < 3 } [0..10]),            0, "find_index 1" );
is( (find_index { $_ > 3 } [0..10]),            4, "find_index 2" );
is( (find_index { $_ > 3 } [0..10], 5),         5, "find_index 3" );
is( (find_index { $_ > 23 } [0..10], 5),    undef, "find_index 4" );
is( (find_index { $_ > 3 } [0..10], 10, 1),    10, "find_index 5" );
is( (find_index { $_ < 3 } [0..10], 10, 1),     2, "find_index 6" );
is( (find_index { $_ < 3 } [0..10], 10, 0),     2, "find_index 7" );
is( (find_index { $_ < 3 } [0..10], 10, 0, -3), 1, "find_index 8" );
is( (find_index { $_ < 3 } [0..10], 10, 0, 3),  1, "find_index 9" );

# find_index_with_memory
$a = "42";
is( (find_index_with_memory sub { ($_[1] / $_[0]) < 1.2 }, [1..10]), 6, "find_index_with_memory 1" );
is( (find_index_with_memory sub { ($b / $a) < 1.2 }, [1..10]), 6,       "find_index_with_memory 2" );
is( (find_index_with_memory { ($b / $a) < 1.2 } [1..10]), 6,            "find_index_with_memory 2" );
is( $a, 42, "memory preserves mine" );

# even_positions
is( join(" ", even_positions 0..10), '0 2 4 6 8 10', "Even positions 1" );
is( join(" ", even_positions 0..9),  '0 2 4 6 8',    "Even positions 2" );
is( join(" ", even_positions [0..9]), '0 2 4 6 8',   "Even positions 3" );

# odd_positions
is( join(" ", odd_positions 0..10), '1 3 5 7 9',  "Odd positions 1" );
is( join(" ", odd_positions 0..9),  '1 3 5 7 9',  "Odd positions 2" );
is( join(" ", odd_positions [0..9]), '1 3 5 7 9', "Odd positions 3" );

# suggestion_sort
is( join(" ", suggestion_sort [qw/foe fie bum/], [qw/bum barf foe fie/]),          'bum foe fie',      'Suggestion Sort 1' );
is( join(" ", suggestion_sort [qw/fie b a barf bum/], [qw/bum barf foe fie/]),     'bum barf fie b a', 'Suggestion Sort 2' );
is( join(" ", suggestion_sort [qw/fie b a barf bum/], [qw/bum barf foe fie bum/]), 'bum barf fie b a', 'Suggestion Sort 3' );

# lex_sort
is_deeply( [lex_sort( [qw/abc ac a/], [qw/abc ab c d/], [qw/x y z/], [qw/abc ab c/] )],
           [['abc','ab','c'],['abc','ab','c','d'],['abc','ac','a'],['x','y','z']], 'lex_sort 1' );
is_deeply( [lex_sort( [qw/15 2 3/], [qw/5 1 9/], [qw/2 2 3/], [qw/23 1 45/] )],
           [['15','2','3'],['2','2','3'],['23','1','45'],['5','1','9']],  'lex_sort 2' );
is_deeply( [lex_sort( sub{$a<=>$b}, [qw/15 2 3/], [qw/2 1 9/], [qw/2 2 3/], [qw/23 1 45/] )],
           [['2','1','9'],['2','2','3'],['15','2','3'],['23','1','45']],  'lex_sort 3' );
is_deeply( [lex_sort( sub{$a<=>$b}, [qw/15 2 3/], ['1'], [qw/2 2 3/], [qw/2 1 9/], [qw/2/], [], [qw/2 2 3/], [qw/23 1 45/] )],
           [[],['1'],['2'],['2','1','9'],['2','2','3'],['2','2','3'],['15','2','3'],['23','1','45']],  'lex_sort 4' );


# flatten
is( join(" ", flatten([1, 2, 3], [4, 5], [[6, 7], 8, 9])), "1 2 3 4 5 6 7 8 9",  "flatten" );

# natural_sort
is_deeply(
  [natural_sort(qw/ a1.5 a1.5b a1.55 a1.55b a1.6 a1.6b linux-2.4.28.tar
   linux-2.4.29.tar linux-2.4.29a.tar linux-2.4.3.tar linux-2.10.6.tar
   linux-2.10.50.tar/)],
  [qw/a1.5 a1.5b a1.6 a1.6b a1.55 a1.55b linux-2.4.3.tar linux-2.4.28.tar
   linux-2.4.29.tar linux-2.4.29a.tar linux-2.10.6.tar linux-2.10.50.tar/],
  "natural_sort 1" );

# binary_search
my @n = 0..10;
is( binary_search(sub{ $_ > 4  }, @n), 5,      'binary_search 1' );
is( binary_search(sub{ $_ == 60}, @n), undef,  'binary_search 2' );
is( binary_search(sub{ $_ > -3 }, @n), 0,      'binary_search 3' );
