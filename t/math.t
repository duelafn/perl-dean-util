#!/usr/bin/perl
use strict;
use warnings;

use Test::More;
use Dean::Util qw/:math $sqrt2/;

sub is_zero {
    ok( abs($_[0]) < 0.000000000001, $_[1] );
}

#-----------------------------------------------------------------
#                  :math - Mathematical Functions
#-----------------------------------------------------------------

=head1 hypot

=cut

{
    is_zero $sqrt2 - hypot(1,1), "hypot: basic sanity";
}

=head1 ndiff

=cut

{
    my $df = ndiff { 2 * $_ };
    is ref($df), "CODE",  "diff: is code";
    is $df->(1), 2,       "diff: appears to work 1";
    is $df->(5), 2,       "diff: appears to work 2";
    is $df->(14.321), 2,  "diff: appears to work 3";
    is ndiff(sub{3*$_[0]}, 5.53), 3, "diff: appears to work 4";
}

=head1 eq_set

=cut

{
    my $x = [qw/1 2 3 4 5 6 7/];
    my $y = [qw/2 4 5 8 9 3/];
    my $u = [qw/1 2 3 4 5 6 7 8 9/];
    my $i = [qw/2 4 5 3/];
    my $d = [qw/1 6 7/];

    ok eq_set([union($x,$y)], $u),        "union";
    ok eq_set([difference($x,$y)], $d),   "difference";
    ok eq_set([intersection($x,$y)], $i), "intersection";
}

=head1 frac

=cut

{
    my @tests = split /\s*?\n\s*/, <<'TESTS';
1.2222222         =  11 /  9
1.21212121212121  =  40 / 33
1.2121212121212   =  40 / 33
1.222221          = 0/0
1.222223          = 0/0
1.223223223       = 1222 / 999
1.22322323        = 0/0
1.2232232223      = 0/0
0.0022322322      = 223/99900
10.00223223223    = 999223/99900
101.0022322322    = 10090123/99900
-1.2222222         =  -11 /  9
-1.21212121212121  =  -40 / 33
-1.2121212121212   =  -40 / 33
-1.222221          = 0/0
-1.222223          = 0/0
-1.223223223       = -1222 / 999
-1.22322323        = 0/0
-1.2232232223      = 0/0
-0.0022322322      = -223/99900
-10.00223223223    = -999223/99900
-101.0022322322    = -10090123/99900
1.23223            = 123223/100000
1.85714285714285   = 13/7
TESTS

    for (@tests) {
        my @x = split /[^-\d.]+/;
        next unless 3 == @x;
        my @y = frac $x[0];
        if ($x[2] == 0) {
            ok( 0 == @y, "frac( $x[0] )" );
        } else {
            ok( ($x[1] == $y[0] and $x[2] == $y[1]),  "frac( $x[0] )" );
        }
    }

    my $N = "0." . join "", 1..$_Util::frac::decimallength;
    $N = substr($N, 0, $_Util::frac::decimallength+1);
    is( frac($N), undef,    "frac: long unique does not fractionize" );
    chop $N;
    ok( frac($N),           "frac: 'short' unique does fractionize" );
    ok( frac(1.2) eq '6/5', "frac: scalar context" );
}


=head1 lcm

=cut

{
    my @tests = split /\s*\n\s*/, <<'TESTS';
-902 -882 -542: 107798922
912 44: 10032
100 43: 4300
10 70 52: 1820
82 -86 -27 -9 -54: 95202
95 89: 8455
66 81: 1782
-832 -312 -332: 207168
36 72 75: 1800
26 -70 -48: 21840
6 93: 186
83 16: 1328
-50 -64: 1600
4622 5122: 11836942
90 12: 180
-8904 -10944: 4060224
-2580 9060: 389580
98 95: 9310
TESTS

    for (@tests) {
        my ($N, $lcm) = split /\s*:\s*/;
        next unless $N and $lcm;
        is( lcm(split /[^\d-]+/, $N), $lcm,  "lcm( $N )" );
    }
}

=head1 gcd

=cut

{
    my @tests = split /\s*\n\s*/, <<'TESTS';
0 0: 0
0 4: 4
4 6: 2
4 6 8: 2
4 8: 4
-4 8: 4
4 -8: 4
-4 -8: 4
9 4 8 6: 1
209048 262096: 8
-902 -882 -542: 2
912 44: 4
100 43: 1
10 70 52: 2
82 -86 -27 -9 -54: 1
95 89: 1
66 81: 3
-832 -312 -332: 4
36 72 75: 3
26 -70 -48: 2
6 93: 3
83 16: 1
-50 -64: 2
4622 5122: 2
90 12: 6
-8904 -10944: 24
-2580 9060: 60
98 95: 1
TESTS

    for (@tests) {
        my ($N, $gcd) = split /\s*:\s*/;
        next unless $N and $gcd;
        is( gcd(split /[^\d-]+/, $N), $gcd,  "gcd( $N )" );
    }
}

=head1 modular_inverse

=cut

{
    is( join(",", map modular_inverse($_,5), 1..4), "1,3,2,4",          "modular_inverse 1" );
    is( join(",", map modular_inverse($_,7), 1..6), "1,4,5,2,3,6",      "modular_inverse 2" );

    no warnings;
    is( join(",", map modular_inverse($_,8), 0..10), ",1,,3,,5,,7,,1,", "modular_inverse 3" );
}



done_testing;
