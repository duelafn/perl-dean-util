#!/usr/bin/perl
use strict;
use warnings;

use Test::More;
use Dean::TestUtil qw/:time/;



=head2 ymd

=cut

{
ok( ymd() =~ /^\d{4}-\d{2}-\d{2}$/, "test ymd" );
}

=head2 seconds2hms

=cut

{
    my @tests = (
        [ "00:00:00",     0 ],
        [ "00:45:00",  2700 ],
        [ "01:54:00",  6840 ],
        [ "12:00:00", 43200 ],
        [ "24:01:00", 86460 ],
        [ "24:00:01", 86401 ],
        [ "14:34:24", 52464 ],
        [ "08:34:46", 30886 ],
        [ "08/34/46", 30886, "/" ],
        [ "083446",   30886, "" ],
    );

    for (@tests) {
        my ($out, @args) = @$_;
        is( seconds2hms(@args), $out, "seconds2hms(".join(", ", map "'$_'", @args).")" );
    }
}


=head2 seconds2time

=cut

{
is( seconds2time(2700),  "12:45 AM", "seconds2time 1" );
is( seconds2time(6840),   "1:54 AM", "seconds2time 2" );
is( seconds2time(43200), "12:00 PM", "seconds2time 3" );
is( seconds2time(0),     "12:00 AM", "seconds2time 4" );
is( seconds2time(86460), "12:01 AM", "seconds2time 5" );
is( seconds2time(86401), "12:00 AM", "seconds2time 6" );
is( seconds2time(52464, " "), " 2:34 PM", "seconds2time 7" );
is( seconds2time(52464, AM => "", PM => "p"), "2:34p", "seconds2time 8" );
is( seconds2time(30886, AM => "", PM => "p"), "8:34",  "seconds2time 9" );
}

=head2 human2seconds

=cut

{
is( human2seconds("12:45 AM"), 2700,  "human2seconds 1" );
is( human2seconds("1:54 AM"),  6840,  "human2seconds 2" );
is( human2seconds("12:00 PM"), 43200, "human2seconds 3" );
is( human2seconds("12:00 AM"), 0,     "human2seconds 4" );
is( human2seconds("12:01 AM"), 60,    "human2seconds 5" );
is( human2seconds(" 2:34 PM"), 52440, "human2seconds 6" );
is( human2seconds("2:34p"),    52440, "human2seconds 7" );
is( human2seconds("8:34"),     30840, "human2seconds 8" );

is( human2seconds("5d 9hr 5m 40 sec."),      464740, "human2seconds 9");
is( human2seconds("1da 23:44:25"),           171865, "human2seconds 10");
is( human2seconds("1 days 4:44 19 seconds"), 103459, "human2seconds 11");
is( human2seconds("6:28: 33"),                23313, "human2seconds 12");
is( human2seconds("4mo 5m"),             10519275.2, "human2seconds 13");
}


done_testing;
