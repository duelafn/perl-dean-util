#!/usr/bin/perl
use strict;
use warnings;

use Test::More;
use Dean::Util qw/:op/;

#-----------------------------------------------------------------
#                  :op - Core function extensions
#-----------------------------------------------------------------

{
    # subopts
    #--------
    # .  invalid option
    my %opt;
    eval { %opt = subopts( [1,2,3], foobar => [qw/foo bar baz/] ) };
    like( "$@", qr/invalid option: 'foobar'/, 'subopts option failure' );

    # .  positional
    %opt = subopts( [1,2,3], positional => [qw/foo bar baz/] );
    is_deeply( \%opt, {foo => 1, bar => 2, baz => 3}, 'subopts positional' );

    # .  p6_positional
    %opt = subopts( [1, 2, baz => 3], p6_positional => [qw/foo bar baz/] );
    is_deeply( \%opt, {foo => 1, bar => 2, baz => 3}, 'subopts p6_positional' );
    # .  p6_positional failure
    eval { %opt = subopts( [1, 2, bar => 3], p6_positional => [qw/foo bar baz/] ) };
    like( "$@", qr/Forbidden \(duplicated\) key 'bar' at index 2/, 'subopts p6_positional failure' );

    # .  allowed
    %opt = subopts( [foo => 2, baz => 3], allowed => [qw/foo bar baz/] );
    is_deeply( \%opt, {foo => 2, baz => 3}, 'subopts allowed' );
    # .  allowed failure
    eval { %opt = subopts( [ foo => 2, bar => 3 ], allowed => [qw/foo baz/] ) };
    like( "$@", qr/Unrecognized key 'bar' at index 2/, 'subopts allowed failure' );

    # .  defaults
    %opt = subopts( [foo => 0, baz => ""], defaults => {qw/foo 9 bar 9 baz 9/}, );
    is_deeply( \%opt, {foo => 0, bar => 9, baz => ""}, 'subopts defaults' );

    # .  required
    %opt = subopts( [foo => 0, baz => ""], allowed => [qw/foo baz/] );
    is_deeply( \%opt, {foo => 0, baz => ""}, 'subopts required' );
    # .  required failure
    eval { %opt = subopts( [ foo => 2, bar => 3 ], required => [qw/foo bar baz/] ) };
    like( "$@", qr/Missing parameter 'baz' in subroutine call/, 'subopts required failure' );

    # .  sloppy_known true
    %opt = subopts( [ foo => 2, bar => 3 ], allowed => [qw/foo baz/], defaults => { bar => 99 }, sloppy_known => 1 );
    is_deeply( \%opt, {foo => 2, bar => 3}, 'subopts sloppy_known true' );
    # .  sloppy_known false
    eval { %opt = subopts( [ foo => 2, bar => 3 ], allowed => [qw/foo baz/], defaults => { bar => 99 }, sloppy_known => 0 ) };
    like( "$@", qr/Unrecognized key 'bar' at index 2/, 'subopts sloppy_known false/fail' );

    # .  no_dups false
    %opt = subopts( [ foo => 2, bar => 3, bar => 99], no_dups => 0 );
    is_deeply( \%opt, {foo => 2, bar => 99}, 'subopts no_dups false' );
    # .  no_dups true
    eval { %opt = subopts( [ foo => 2, bar => 3, bar => 99 ], no_dups => 1 ) };
    like( "$@", qr/Forbidden \(duplicated\) key 'bar' at index 4/, 'subopts no_dups true/fail' );

    # .  eval_defaults
    %opt = subopts( [ foo => 2 ], defaults => { bar => sub { $_[0] }, baz => sub { 1 + $_[1]{foo} }, bip => [3] }, eval_defaults => 1 );
    is_deeply( \%opt, {foo => 2, bar => 'bar', baz => 3, bip => [3]}, 'subopts eval_defaults true' );
};


{
    # parse_date
    use DateTime;
    {   my ($Y,$m,$d) = (localtime)[5,4,3];
        $Y+=1900; $m++;
        sub _today { DateTime->new( year => $Y, month => $m, day => $d ) }
    }

    use POSIX qw/ strftime /;
    use Scalar::Util qw/ refaddr /;

    my $d_now = parse_date("now");
    my $l_now = strftime("%Y-%m-%d %H:%M:%S", localtime);

    my ($Y,$m,$d,$H,$M,$S);
    ($Y,$m,$d,$H,$M,$S) = split /\D/, $d_now;
    $d_now = DateTime->new( year => $Y, month => $m, day => $d, hour => $H, minute => $M, second => $S );

    ($Y,$m,$d,$H,$M,$S) = split /\D/, $l_now;
    $l_now = DateTime->new( year => $Y, month => $m, day => $d, hour => $H, minute => $M, second => $S );

    my $delta = $d_now->delta_ms( $l_now );
    ok( ( $delta->seconds <= 1
          and $delta->minutes == 0
          and $delta->hours == 0
          and $delta->days == 0
          and $delta->weeks == 0
          and $delta->months == 0
          and $delta->years == 0
      ), "parse_date: now" );

    my $t1 = $d_now->clone;
    my $t2 = parse_date( $t1, clone => 0 );
    is   refaddr($t1), refaddr($t2),  "parse_date: clone => 0";

    $t1 = $d_now->clone;
    $t2 = parse_date( $t1 );
    is   "$t1", "$t2",                "parse_date: DT in -> same result";
    isnt refaddr($t1), refaddr($t2),  "parse_date: DT in -> cloned";

    my @days = qw/ Sunday Monday Tuesday Wednesday Thursday Friday Saturday Sunday Monday /;
    my $dow   = $days[_today->dow];
    my $dow_y = $days[_today->dow - 1];
    my $dow_t = $days[_today->dow + 1];

    my %date_t = (
        "Aug 30, 1977" => "1977-08-30",
        "30 Aug 1977"  => "1977-08-30",
        "Aug 1977"     => "1977-08-01",
        "1977"         => "1977-01-01",
        "5/6/2012"     => "2012-05-06",
        "now"          => strftime("%Y-%m-%d", localtime),
        "2 days ago"   => _today->add(days => -2),
    );

    my %datetime_t = (
        "yesterday"     => _today->add(days => -1),
        "tomorrow"      => _today->add(days =>  1),
        "5pm yesterday" => _today->add(days => -1, hours => 17),
        "yesterday 5pm" => _today->add(days => -1, hours => 17),
        "5pm"           => _today->add(hours => 17),
        "5am"           => _today->add(hours => 5),
        "5:45"          => _today->add(hours => 5, minutes => 45),
        "5:45pm"        => _today->add(hours => 17, minutes => 45),
        "noon"          => _today->add(hours => 12),

        "$dow"         => _today,
        "last $dow"    => _today->add(days => -7),
        "next $dow"    => _today->add(days =>  7),
#         "$dow_y"       => _today->add(days => -1),# naturally ambiguous; Date::Manip "fails" on Monday
        "last $dow_y"  => _today->add(days => -1),
        "next $dow_y"  => _today->add(days =>  6),
#         "$dow_t"       => _today->add(days =>  1),# Alas, Data::Manip "fails" this one on Sundays (reports Monday of current week)
        "last $dow_t"  => _today->add(days => -6),
        "next $dow_t"  => _today->add(days =>  1),

        "2012-03-07"          => "2012-03-07T00:00:00",
        "2006:08:28 20:56:25" => "2006-08-28T20:56:25",
        "2006-08-28 20:56:25" => "2006-08-28T20:56:25",
        "2006-08-28T20:56:25" => "2006-08-28T20:56:25",
        "28 Aug 2006 8:56pm"  => "2006-08-28T20:56:00",
        "8:56pm 28 Aug 2006"  => "2006-08-28T20:56:00",
    );

    while (my ($in, $out) = each %date_t) {
        $out = $out->ymd if ref($out);
        my $parsed = parse_date($in);
        ok( $parsed,            "parse_date: $in  (parsed)" );
        is( $parsed->ymd, $out, "parse_date: $in  (as YMD)" );

        $parsed = parse_date($in, floating => 1);
        is( $parsed->ymd, $out, "parse_date: $in  (floating: as YMD)" );
    }

    while (my ($in, $out) = each %datetime_t) {
        is( "".parse_date($in), "$out", "parse_date: $in" );
        is( "".parse_date($in, floating => 1), "$out", "parse_date: $in  (floating)" );
    }

};


{
    # map_pairs
    my %h = qw/a 1 b 2 c 3/;
    is_deeply( [sort +(map_pairs { "$a: $b" } %h)],    ["a: 1", "b: 2", "c: 3"],  "map_pairs 1" );
    {   no warnings 'uninitialized';
        is_deeply( [map_pairs { "$a: $b" } qw/a 1 b 2 c/], ["a: 1", "b: 2", "c: "],   "map_pairs 2" );
    }

    # map_pair
    sub f { shift()  + 2 }
    sub g { shift() ** 2 }
    is( (join " ", map_pair \&f, \&g, 1..20),
        "3 4 5 16 7 36 9 64 11 100 13 144 15 196 17 256 19 324 21 400", "map_pair 1" );
    is( (join " ", map_pair { $_ + 2 } sub { $_ ** 3 }, 1..20),
        "3 8 5 64 7 216 9 512 11 1000 13 1728 15 2744 17 4096 19 5832 21 8000", "map_pair 2" );
}

{
    # deep_eq
    ok(  deep_eq([1,2,3], [1,2,3]),                           "deep eq 1" );
    ok( !deep_eq([1,2,3], [4,5,6]),                           "deep eq 2" );
    ok( !deep_eq(bless([1,2,3],"Foo"), bless([1,2,3],"Bar")), "deep eq 3" );
    ok(  deep_eq(bless([1,2,3],"Foo"), bless([1,2,3],"Foo")), "deep eq 4" );

    ok(  deep_eq("Foo","Foo"),                                "deep eq 5" );
    ok( !deep_eq("Foo","Bar"),                                "deep eq 6" );
    ok( !deep_eq("Foo",undef),                                "deep eq 7" );
    ok(  deep_eq(undef,undef),                                "deep eq 8" );

    my $A = [1,2,3];
    my $B = [1,2,3];
    my $C = {1 => 2, 3 => $A}; $$C{5} = $C;
    my $D = {1 => 2, 3 => $B}; $$D{5} = $D;

    ok(  deep_eq($C, $D),                                     "deep eq 9" );
    ok(  deep_eq([ $A, $D ], [ $B, $C ]),                     "deep eq 10" );

    # unique_id
    ok( unique_id ne unique_id,   "unique_id $_" ) for 1..20;
    ok( unique_id() !~ /[^\w\.]/, "unique_id $_" ) for 21..40;
}


{   # EXISTS, HAS, TRUE
    my $x = {
        a => {
            b => {
                c => 1,
                d => 0,
                e => undef,
            },
            c => {
                A => [ ],
                B => { },
                C => { foo => 1 },
                D => [ 0 ],
                E => "true",
                F => "false",
            },
            'e/f' => {
                g => 1,
                h => { i => 0 },
            },
            A => [ 3, { foo => "bar" } ],
        },
    };

    ok(  EXISTS($x, qw| a/b a/b/c a/b/d a/b/e|), "EXISTS succeeds" );
    ok( !EXISTS($x, qw| f/g/h |),                "EXISTS fails 1" );
    ok( !EXISTS($x, qw| f |),                    "EXISTS fails 2" );
    ok(  EXISTS($x, "a\0e/f\0h", {sep => "\0"}), "EXISTS w/sep" );
    ok(  EXISTS($x, 'a/c/D/0'),                  "EXISTS with arrays" );
    ok(  EXISTS($x, [qw'a c D 0']),              "EXISTS arrayref with arrays" );

    ok(  HAS($x, qw| a/b a/b/c a/b/d |),      "HAS succeeds" );
    ok( !HAS($x, qw| f/g/h |),                "HAS fails 1" );
    ok( !HAS($x, qw| a/b/e |),                "HAS fails 2" );
    ok( !HAS($x, qw| f |),                    "HAS fails 3" );
    ok(  HAS($x, "a\0e/f\0h", {sep => "\0"}), "HAS w/sep" );
    ok(  HAS($x, 'a/c/D/0'),                  "HAS with arrays" );
    ok(  HAS($x, 'a/A/1/foo'),                "HAS with arrays" );
    ok(  HAS($x, [qw|a A 1 foo|]),            "HAS arrayref with arrays" );

    ok(  TRUE($x, qw| a/b a/b/c a/c/C a/c/D a/c/E |), "TRUE succeeds" );
    ok( !TRUE($x, qw| f/g/h |),                       "TRUE fails 1" );
    ok( !TRUE($x, qw| f |),                           "TRUE fails 2" );
    ok( !TRUE($x, qw| a/C/A |),                       "TRUE fails 3" );
    ok( !TRUE($x, qw| a/c/B |),                       "TRUE fails 4" );
    ok( !TRUE($x, qw| a/c/F |),                       "TRUE fails 5" );
    ok( !TRUE($x, qw| a/b/d |),                       "TRUE fails 6" );
    ok(  TRUE($x, "a\0e/f\0h", {sep => "\0"}),        "TRUE w/sep" );
    ok( !TRUE($x, 'a/c/D/0'),                         "TRUE with arrays" );
    ok( !TRUE($x, [qw'a c D 0']),                     "TRUE arrayref with arrays" );

    # GETPATH, SETPATH
    is( GETPATH($x, 'a/c/F'), 'false',              "GETPATH" );
    is( GETPATH($x, 'a/c/D/0'), 0,                  "GETPATH with arrays" );
    is( GETPATH($x, 'a/A/1/foo'), "bar",            "GETPATH with arrays" );
    is( GETPATH($x, "a\0e/f\0g", {sep => "\0"}), 1, "GETPATH w/sep" );
    is( GETPATH($x, [qw"a e/f g"]), 1,              "GETPATH arrayref" );

    ok( SETPATH($x, 'a/c/F', "Hello"),              "SETPATH" );
    ok( SETPATH($x, 'a/c/D/1', 21),                 "SETPATH with arrays" );
    ok( SETPATH($x, "a\0e/f\0g", 2, {sep => "\0"}), "SETPATH w/sep" );

    is( GETPATH($x, 'a/c/F'), 'Hello',              "GETPATH checks SETPATH" );
    is( GETPATH($x, 'a/c/D/1'), 21,                 "GETPATH checks SETPATH with arrays" );
    is( GETPATH($x, "a\0e/f\0g", {sep => "\0"}), 2, "GETPATH checks SETPATH w/sep" );
}

{   # SPLIT
    my $string = q|Foo, Bar, the "Baz, is good", Bif|;
    is_deeply( [SPLIT qr/\s*,\s*/, $string],    ['Foo', 'Bar', 'the "Baz, is good"', 'Bif'], "SPLIT - basic test" );
    is_deeply( [SPLIT qr/\s*,\s*/, $string, 3], ['Foo', 'Bar', 'the "Baz, is good", Bif'],   "SPLIT - with limit" );

    my $splitter = SPLIT qr/\s*,\s*/;
    is( ref($splitter), 'CODE', "Split with no arg gives coderef" );
    is_deeply( [$splitter->($string)],    ['Foo', 'Bar', 'the "Baz, is good"', 'Bif'], "SPLIT - coderef works" );
    is_deeply( [$splitter->($string, 3)], ['Foo', 'Bar', 'the "Baz, is good", Bif'],   "SPLIT - coderef works with limit" );

    $splitter = SPLIT;
    is( ref($splitter), 'CODE', "Split with no arg gives coderef" );
    is_deeply( [$splitter->($string)],    ['Foo,', 'Bar,', 'the', '"Baz, is good",', 'Bif'], "SPLIT - noarg SPLIT works" );
}

{   # pmap, pgrep
    use Time::HiRes qw/ sleep /;

    my $n   = 10;
    my @n   = (1..$n);
    my @odd = (1,3,5,7,9);
    my @sleep = map abs((5-$_)/100), @n; # encourage out-of-order completion

    open STDERR, ">", "/dev/null" or die;

    # multi-threaded
    #---------------
    $_Util::pmap::threads = 2;
    is_deeply( [pmap { ++$_ } @n], [2..$n+1], "pmap: increment" );

    is_deeply( \@n, [1..$n], "pmap: no modifications of \$_" );

    is_deeply( [pgrep { $_ % 2 } @n], \@odd, "pgrep: grep for odds" );

    is_deeply( [pmap { [$_] } @n], [map [$_], @n], "pmap: returning arrays" );

    is_deeply( [pmap { bless {$_,$_}, "APackage" } @n], [map bless({$_,$_}, "APackage"), @n], "pmap: returning objects" );

    is_deeply( [pmap { sleep $sleep[$_-1]; ++$_ } @n], [2..$n+1], "pmap: out-of-order execution" );

    is_deeply( \@n, [1..$n], "pmap: still no modifications of \@n" );

    eval { pmap { die "__XYZZY__" } 1..10 };
    like( $@, qr/__XYZZY__/, "pmap: death is propagated" );
    is( threads->list, 0, "pmap: Threads cleaned up upon death" );


    # single-threaded shortcut
    #-------------------------
    $_Util::pmap::threads = 1;
    is_deeply( [pmap { ++$_ } @n], [2..$n+1], "pmap: increment single-threaded" );
    is_deeply( \@n, [1..$n], "pmap: no modifications of \$_ even in single-threaded shortcut" );

    eval { pmap { die "__XYZZY__" } 1..10 };
    like( $@, qr/__XYZZY__/, "pmap: death is propagated single-threaded" );
    is( threads->list, 0, "pmap: Threads cleaned up upon death single-threaded" );
}


done_testing;
