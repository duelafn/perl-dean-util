#!/usr/bin/perl
use strict;
use warnings;

use Test::More tests => 195;
BEGIN { use_ok 'Dean::Util', qw/:numerical/ }

#-----------------------------------------------------------------
#                 :numerical - Numerical Functions
#-----------------------------------------------------------------

=head2 $pi

=cut

ok( $pi > 3,  '$pi 1');
ok( $pi < 4,  '$pi 2');

=head2 $e

=cut

ok( $e > 2,  '$e 1');
ok( $e < 3,  '$e 2');

=head2 max

=cut

is( max(1..10),           10, "max 1" );
is( max(1..10, 4),        10, "max 2" );
is( max(22,1..10),        22, "max 3" );
is( max_dirty(1..10),           10, "max_dirty 1" );
is( max_dirty(1..10, 4, undef), 10, "max_dirty 2" );
is( max_dirty(22,1..10),        22, "max_dirty 3" );

=head2 min

=cut

is( min(1..10),            1, "min 1" );
is( min(1..10, -4),       -4, "min 2" );
is( min(22,1..10),         1, "min 3" );
is( min_dirty(1..10),            1, "min_dirty 1" );
is( min_dirty(1..10, -4, undef),-4, "min_dirty 2" );
is( min_dirty(22,1..10),         1, "min_dirty 3" );


=head2 min_max

=cut

is( join(",",min_max(1..10)),                   "1,10", "min_max 1" );
is( join(",",min_max(1..10, -4)),              "-4,10", "min_max 2" );
is( join(",",min_max(22,1..10)),                "1,22", "min_max 3" );
is( join(",",min_max_dirty(1..10)),             "1,10", "min_max_dirty 1" );
is( join(",",min_max_dirty(1..10, -4, undef)), "-4,10", "min_max_dirty 2" );
is( join(",",min_max_dirty(-1,-10,-4, undef)),"-10,-1", "min_max_dirty 2" );
is( join(",",min_max_dirty(22,1..10)),          "1,22", "min_max_dirty 3" );


=head2 fmin

=cut

is( (fmin { ($_ - 3)**2 } 1..5),    0, "fmin 1" );
is( (fmin { ($_ - 3)**2 } 5..9),    4, "fmin 2" );
is( (fmin { ($_[0] - 3)**2 } 1..5), 0, "fmin 3" );
ok( (fmin { -($_[0] - 3)**2 } 3)==0,    "fmin 4" );
ok( !defined(fmin { -($_[0] - 3)**2 }), "fmin 5" );

=head2 fmax

=cut

ok( (fmax { -($_ - 3)**2 } 1..5) ==  0, "fmax 1" );
is( (fmax { -($_ - 3)**2 } 5..9),   -4, "fmax 2" );
ok( (fmax { -($_[0] - 3)**2 } 1..5)==0, "fmax 3" );
ok( (fmax { -($_[0] - 3)**2 } 3)==0,    "fmax 4" );
ok( !defined(fmax { -($_[0] - 3)**2 }), "fmax 5" );

=head2 fmin_dirty

=cut

ok( (fmin_dirty { $_ ? ($_ - 3)**2 : undef } 0..5) ==    0, "fmin_dirty 1" );
is( (fmin_dirty { $_ ? ($_ - 3)**2 : undef } 0,5..9),    4, "fmin_dirty 2" );
ok( (fmin_dirty { $_ ? ($_[0] - 3)**2 : undef } 1..5,0)==0, "fmin_dirty 3" );

=head2 fmax_dirty

=cut

ok( (fmax_dirty { $_ ? -($_ - 3)**2 : undef } 0..5) ==    0, "fmax_dirty 1" );
is( (fmax_dirty { $_ ? -($_ - 3)**2 : undef } 5..9,0),   -4, "fmax_dirty 2" );
ok( (fmax_dirty { $_ ? -($_[0] - 3)**2 : undef } 1..5,0)==0, "fmax_dirty 3" );

=head2 ceil

=cut

is( ceil(3),     3, "ceil 1" );
is( ceil(3.2),   4, "ceil 2" );
is( ceil(3.5),   4, "ceil 3" );
is( ceil(3.7),   4, "ceil 4" );
is( ceil(0),     0, "ceil 5" );
is( ceil(-3),   -3, "ceil 6" );
is( ceil(-3.7), -3, "ceil 7" );
is( ceil_dirty(undef), undef, "ceil_dirty undef" );

=head2 floor

=cut

is( floor(3),     3, "floor 1" );
is( floor(3.2),   3, "floor 2" );
is( floor(3.5),   3, "floor 3" );
is( floor(3.7),   3, "floor 4" );
is( floor(0),     0, "floor 5" );
is( floor(-3),   -3, "floor 6" );
is( floor(-3.7), -4, "floor 7" );
is( floor_dirty(undef), undef, "floor_dirty undef" );

=head2 round

=cut

is( round(2.03,".02"),  "2.04", "round 0" );
is( round(2.05,".02"),  "2.06", "round 1" );
is( round(2.051,".02"), "2.06", "round 2" );
is( round(2.049,".02"), "2.04", "round 3" );
is( round(2.05,".03"),  "2.04", "round 4" );
is( round(2.07,".02"),  "2.08", "round 5" );
is( round(2.08,".02"),  "2.08", "round 6" );

is( round(2.232,"15"),   "0",  "round 7" );
is( round(12.232,"15"),  "15", "round 8" );
is( round(32.232,"15"),  "30", "round 9" );
is( round(32.232,"10"),  "30", "round 10" );
is( round(36.232,"10"),  "40", "round 11" );
is( round(35,"10"),      "40", "round 12" );
is( round(45,"10"),      "50", "round 13" );

is( round(45),           "45", "round 14" );
is( round(46),           "46", "round 15" );
is( round(46.3),         "46", "round 16" );
# Check for banker's rounding:
is( round(46.5),         "47", "round 17" );
is( round(47.5),         "48", "round 18" );

is( round(0.001, .01), "0.00", "round 19" );

# Negatives
is( round(-2.03,".02"),  "-2.04", "round -0" );
is( round(-2.05,".02"),  "-2.06", "round -1" );
is( round(-2.051,".02"), "-2.06", "round -2" );
is( round(-2.049,".02"), "-2.04", "round -3" );
is( round(-2.05,".03"),  "-2.04", "round -4" );
is( round(-2.07,".02"),  "-2.08", "round -5" );
is( round(-2.08,".02"),  "-2.08", "round -6" );

is( round(-2.232,"15"),   "0",  "round -7" );
is( round(-12.232,"15"),  "-15", "round -8" );
is( round(-32.232,"15"),  "-30", "round -9" );
is( round(-32.232,"10"),  "-30", "round -10" );
is( round(-36.232,"10"),  "-40", "round -11" );
is( round(-35,"10"),      "-40", "round -12" );
is( round(-45,"10"),      "-50", "round -13" );

is( round(-45),           "-45", "round -14" );
is( round(-46),           "-46", "round -15" );
is( round(-46.3),         "-46", "round -16" );
# Check for banker's rounding:
is( round(-46.5),         "-47", "round -17" );
is( round(-47.5),         "-48", "round -18" );

is( round(-0.001, .01), "0.00", "round -19" );

=head2 unbiased_round

=cut

is( unbiased_round(2.05,".02"),  "2.04", "unbiased_round 1" );
is( unbiased_round(2.051,".02"), "2.06", "unbiased_round 2" );
is( unbiased_round(2.049,".02"), "2.04", "unbiased_round 3" );
is( unbiased_round(2.05,".03"),  "2.04", "unbiased_round 4" );
is( unbiased_round(2.07,".02"),  "2.08", "unbiased_round 5" );
is( unbiased_round(2.08,".02"),  "2.08", "unbiased_round 6" );

is( unbiased_round(2.232,"15"),   "0",  "unbiased_round 7" );
is( unbiased_round(12.232,"15"),  "15", "unbiased_round 8" );
is( unbiased_round(32.232,"15"),  "30", "unbiased_round 9" );
is( unbiased_round(32.232,"10"),  "30", "unbiased_round 10" );
is( unbiased_round(36.232,"10"),  "40", "unbiased_round 11" );
is( unbiased_round(35,"10"),      "40", "unbiased_round 12" );
is( unbiased_round(45,"10"),      "40", "unbiased_round 13" );

is( unbiased_round(45),           "45", "unbiased_round 14" );
is( unbiased_round(46),           "46", "unbiased_round 15" );
is( unbiased_round(46.3),         "46", "unbiased_round 16" );
is( unbiased_round(46.5),         "46", "unbiased_round 17" );
is( unbiased_round(47.5),         "48", "unbiased_round 18" );

is( unbiased_round(-0.001, .01), "0.00", "unbiased_round 19" );

=head2 sum

=cut

is( sum(1..10), 55, "sum 1");
my @x = (1..10);
is( sum(@x),    55, "sum 2");
is_deeply( \@x, [1..10], "sum 3");

=head2 sum_dirty

=cut

is( sum_dirty("hello",1..10), 55, "sum_dirty 1");
@x = ("Hello", 1..5, [2,3], " 6", 7..10, undef);
is( sum_dirty(@x),    55, "sum_dirty 2");
is_deeply( \@x, [("Hello", 1..5, [2,3], " 6", 7..10, undef)], "sum_dirty 3");

=head2 product

=cut

is( product(1..10), 3628800, "product 1");
@x = (1..10);
is( product(@x),    3628800, "product 2");
is_deeply( \@x, [1..10], "product 3");

=head2 product_dirty

=cut

is( product_dirty("hello",1..10), 3628800, "product_dirty 1");
@x = ("Hello", 1..5, [2,3], " 6", 7..10, undef);
is( product_dirty(@x),    3628800, "product_dirty 2");
is_deeply( \@x, [("Hello", 1..5, [2,3], " 6", 7..10, undef)], "product_dirty 3");

=head2 average

=cut

is( average(1..10), 5.5, "average 1");
@x = (1..10);
is( average(@x),    5.5, "average 2");
is_deeply( \@x, [1..10], "average 3");

=head2 average_dirty

=cut

is( average_dirty("hello",1..10), 5, "average_dirty 1");
@x = ("Hello", 1..5, [2,3], " 6", 7..10, undef);
ok( average_dirty(@x) =~ /4.2307/, "average_dirty 2");
is_deeply( \@x, [("Hello", 1..5, [2,3], " 6", 7..10, undef)], "average_dirty 3");



=head2 minimizer

=cut

is( (minimizer { ($_ - 3)**2 } 5..9),    5, "minimizer 2" );
is( (minimizer { ($_[0] - 3)**2 } 1..5), 3, "minimizer 3" );
is( (minimizer { ($_[0] - 3)**2 } 1), 1,    "minimizer 4" );
ok( !defined(minimizer { ($_[0] - 3)**2 }), "minimizer 5" );

=head2 maximizer

=cut

is( (maximizer { -($_ - 3)**2 } 5..9),    5, "maximizer 2" );
is( (maximizer { -($_[0] - 3)**2 } 1..5), 3, "maximizer 3" );
is( (maximizer { ($_[0] - 3)**2 } 1), 1,     "maximizer 4" );
ok( !defined(maximizer { ($_[0] - 3)**2 }),  "maximizer 5" );


=head2 minimizer_dirty

=cut

is( (minimizer_dirty { ($_==5)?undef:($_ - 3)**2 } 5..9),    6, "minimizer_dirty 2" );
is( (minimizer_dirty { ($_==6)?undef:($_ - 3)**2 } 5..9),    5, "minimizer_dirty 3" );
is( (minimizer_dirty { ($_[0] - 3)**2 } 1),                  1, "minimizer_dirty 4" );
ok( !defined(minimizer_dirty { ($_[0] - 3)**2 }),               "minimizer_dirty 5" );

=head2 maximizer_dirty

=cut

is( (maximizer_dirty { ($_==5)?undef:-($_ - 3)**2 } 5..9),    6, "maximizer_dirty 2" );
is( (maximizer_dirty { ($_==6)?undef:-($_ - 3)**2 } 5..9),    5, "maximizer_dirty 3" );
is( (maximizer_dirty { ($_[0] - 3)**2 } 1),                   1, "maximizer_dirty 4" );
ok( !defined(maximizer_dirty { ($_[0] - 3)**2 }),                "maximizer_dirty 5" );

=head2 factorial

=cut

is( factorial(15), 1307674368000, "factorial 1" );
is( factorial(2),  2,             "factorial 2" );
is( factorial(1),  1,             "factorial 3" );
is( factorial(0),  1,             "factorial 4" );
is( factorial(-1), undef,         "factorial 5" );
is( factorial(1.5), undef,        "factorial 6" );

=head2 sieve_of_eratosthenes

=cut

my $sieve = sieve_of_eratosthenes(5);
is( length($sieve),  1,    "sieve_of_eratosthenes 1");
sieve_of_eratosthenes(20, $sieve);
is( length($sieve),  3,    "sieve_of_eratosthenes 2");
is( vec($sieve, 0, 1),  0, "sieve_of_eratosthenes 3");
is( vec($sieve, 1, 1),  0, "sieve_of_eratosthenes 4");
is( vec($sieve, 2, 1),  1, "sieve_of_eratosthenes 5");
is( vec($sieve, 5, 1),  1, "sieve_of_eratosthenes 6");
is( vec($sieve, 7, 1),  1, "sieve_of_eratosthenes 7");
is( vec($sieve, 8, 1),  0, "sieve_of_eratosthenes 8");
is( vec($sieve, 20, 1), 0, "sieve_of_eratosthenes 9");
is( vec($sieve, 23, 1), 1, "sieve_of_eratosthenes 10");
is( vec($sieve, 29, 1), 0, "sieve_of_eratosthenes 11");  # have not computed out far enough yet

sieve_of_eratosthenes(15, $sieve); # Shouldn't change anything
is( length($sieve),  3,    "sieve_of_eratosthenes 2b");
is( vec($sieve, 0, 1),  0, "sieve_of_eratosthenes 3b");
is( vec($sieve, 1, 1),  0, "sieve_of_eratosthenes 4b");
is( vec($sieve, 2, 1),  1, "sieve_of_eratosthenes 5b");
is( vec($sieve, 5, 1),  1, "sieve_of_eratosthenes 6b");
is( vec($sieve, 7, 1),  1, "sieve_of_eratosthenes 7b");
is( vec($sieve, 8, 1),  0, "sieve_of_eratosthenes 8b");
is( vec($sieve, 20, 1), 0, "sieve_of_eratosthenes 9b");
is( vec($sieve, 23, 1), 1, "sieve_of_eratosthenes 10b");
is( vec($sieve, 29, 1), 0, "sieve_of_eratosthenes 11b");  # have not computed out far enough yet

=head2 is_prime

=cut

ok( !is_prime(0),   "is_prime 1" );
ok( !is_prime(1),   "is_prime 2" );
ok(  is_prime(2),   "is_prime 3" );
ok(  is_prime(3),   "is_prime 4" );
ok(  is_prime(11),  "is_prime 5" );
is( 0+grep(is_prime($_), reverse 1..100000), 9592, "is_prime 6" );

=head2 next_prime

=cut

is( next_prime(0),   2,   "next_prime 1" );
is( next_prime(2),   3,   "next_prime 2" );
is( next_prime(3),   5,   "next_prime 3" );
is( next_prime(5),   7,   "next_prime 4" );
is( next_prime(7),   11,  "next_prime 5" );
is( next_prime(500), 503, "next_prime 6" );

=head2 base2base

=cut

is( base2base("0", 2, 10),                                   0, "base2base 0" );
is( base2base("01101101", 2, 10),                          109, "base2base 1" );
is( base2base("2aef", [0..9,qw/a b c d e f/], 10), hex("2aef"), "base2base 2" );
is( base2base("2aef", "0123456789abcdef", 10),     hex("2aef"), "base2base 3" );
is( base2base("2aef", base_hash(16), 10),          hex("2aef"), "base2base 4" );
is( sprintf("%.8f",base2base("152.762", 8, 10)),"106.97265625", "base2base 5" );
is( sprintf("%.3f",base2base("106.97265625", 10, 8)),"152.762", "base2base 6" );
