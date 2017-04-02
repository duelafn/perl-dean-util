#!/usr/bin/perl
use strict;
use warnings;

use Test::More;
use Dean::TestUtil qw/:stat_prob sum min max/;

#-----------------------------------------------------------------
#              :stat_prob - Statistical / Probability
#-----------------------------------------------------------------
my @dice_rolls = roll_dice(1000);
my $dice_sum   = pop @dice_rolls;

=head2 roll_dice

=cut

ok( roll_dice =~ /^[1-6]$/,  "roll_dice 1" );

is( sum(@dice_rolls), $dice_sum,      "roll_dice 2" );
ok( min(@dice_rolls) == 1,            "roll_dice 3" );# Most probably true
ok( max(@dice_rolls) == 6,            "roll_dice 4" );# Most probably true


=head2 one_var

=cut

{
    my @y = my @x = @dice_rolls;
    my %res = one_var( \@x );
    ok( eq_array( \@y, \@x ),  "one_var 1" );

    my %res1 = one_var( \@x, 0 );
    ok( !eq_array( \@y, \@x ), "one_var 2" );# Most probably true

    ok( eq_array( [sort {$a<=>$b} @y], \@x ), "one_var 3" );

    my %res2 = one_var( \@x, 1 );
    ok( eq_array( [sort {$a<=>$b} @y], \@x ), "one_var 4" );

    is_deeply( \%res, \%res1, "one_var 4,1" );
    is_deeply( \%res, \%res2, "one_var 4,2" );

    is( int($res{average}), 3,  "one_var 5" );# Most probably true
    is( $res{n}, 1000,          "one_var 7" );

    %res = one_var( [] );
    ok( !%res,  "one_var - empty data" );

    %res = one_var( 3 );
    is( $res{n},     1, "one_var - n=1" );
    is( $res{mean},  3, "one_var - n=1, mean" );
    is( $res{sigma}, 0, "one_var - n=1, std dev" );
    is( $res{se},    0, "one_var - n=1, std err" );
    is( $res{max},   3, "one_var - n=1, max" );
    is( $res{min},   3, "one_var - n=1, min" );
    is( $res{Q1},    3, "one_var - n=1, Q1" );
    is( $res{Q2},    3, "one_var - n=1, Q2" );
    is( $res{Q3},    3, "one_var - n=1, Q3" );
}



=head2 percentile

=cut

my @x = @dice_rolls;
ok( percentile(.5, \@x) =~ /^(?:3(?:\.5)?|4)$/,  "percentile 1" );# Most probably true


=head2 randomize

=cut

ok( eq_set( [randomize(1..40)], [1..40] ),    "randomize 1" );
ok( !eq_array( [randomize(1..40)], [1..40] ), "randomize 2" );# Most probably true

=head2 permutations

=cut

ok(
   eq_set( [map "@$_", permutations(1..3)],
           ["1 2 3", "1 3 2", "2 1 3", "2 3 1", "3 1 2", "3 2 1"]),
   "permutations 1"
  );

ok(
   eq_set( [map "@$_", permutations(1..4)],
           ["1 2 3 4", "1 2 4 3", "1 3 2 4", "1 3 4 2", "1 4 2 3", "1 4 3 2",
            "2 1 3 4", "2 1 4 3", "2 3 1 4", "2 3 4 1", "2 4 1 3", "2 4 3 1",
            "3 1 2 4", "3 1 4 2", "3 2 1 4", "3 2 4 1", "3 4 1 2", "3 4 2 1",
            "4 1 2 3", "4 1 3 2", "4 2 1 3", "4 2 3 1", "4 3 1 2", "4 3 2 1"]),
   "permutations 2"
  );

           # impossible for it to actually compte it w/o using equation:
is( permutations(17), 355687428096000, "scalar permutations" );

=head2 combinations

=cut

ok(
   eq_set( [map "@$_", combinations(1..4)],
           ["", "1", "2", "3", "4", "1 2", "1 3", "1 4", "2 3", "2 4",
            "3 4", "1 2 3", "1 2 4", "1 3 4", "2 3 4", "1 2 3 4"] ),
   "combinations 1"
  );

ok(
   eq_set( [map "@$_", combinations(2, [1..5])],
           ["1 2", "1 3", "1 4", "1 5", "2 3", "2 4", "2 5", "3 4", "3 5", "4 5"]),
   "combinations 2"
  );

is( combinations(37), 137438953472, "scalar combinations 1" );
is( combinations(37,10), 348330136, "scalar combinations 2" );

=head2 arrangements

=cut

ok(
   eq_set( [map "@$_", arrangements(1..3)],
           ["", "1", "2", "3", "1 2", "1 3", "2 1", "2 3", "3 1", "3 2",
            "1 2 3", "1 3 2", "2 1 3", "2 3 1", "3 1 2", "3 2 1"]),
   "arrangements 1"
  );

ok(
   eq_set( [map "@$_", arrangements([1..3], 3)],
           ["1 2 3", "1 3 2", "2 1 3", "2 3 1", "3 1 2", "3 2 1"]),
   "arrangements 2"
  );

ok(
   eq_set( [map "@$_", arrangements([1..3], 33)], []),
   "arrangements 3"
  );

is( arrangements(17), 966858672404690, "scalar arrangements 1" );
is( arrangements(17,9),    8821612800, "scalar arrangements 2" );



done_testing;
