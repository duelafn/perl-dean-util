#!/usr/bin/perl
use strict;
use warnings;
use Encode;

use Test::More;
use Dean::Util qw/:canonicalize/;

#-----------------------------------------------------------------
#                 :canonicalize - Canonicalization
#-----------------------------------------------------------------


=head2 decode_english

=cut

{
    my $decoded_full    = decode("UTF-8", "3x²+2x+π : 紫は、素敵な 色 です。");
    my $decoded_partial = decode("UTF-8", "3x²+2x+µ");# Codepoint 181 MICRO SIGN, not codepoint 956 GREEK SMALL LETTER MU

    is( decode_english($decoded_full), $decoded_full, "decode_english: Already decoded" );
    is( decode_english("3x²+2x+π : 紫は、素敵な 色 です。"), $decoded_full, "decode_english: raw UTF-8" );
    is( decode_english("\063\170\302\262\053\062\170\053\317\200\040\072\040\347\264\253\343\201\257\343\200\201\347\264\240\346\225\265\343\201\252\040\350\211\262\040\343\201\247\343\201\231\343\200\202"), $decoded_full, "decode_english: UTF-8" );
    is( decode_english("\xFE\xFF\000\063\000\170\000\262\000\053\000\062\000\170\000\053\003\300\000\040\000\072\000\040\175\053\060\157\060\001\175\040\145\165\060\152\000\040\202\162\000\040\060\147\060\131\060\002"), $decoded_full, "decode_english: UTF-16BE with/BOM" );
    is( decode_english("\xFF\xFE\063\000\170\000\262\000\053\000\062\000\170\000\053\000\300\003\040\000\072\000\040\000\053\175\157\060\001\060\040\175\165\145\152\060\040\000\162\202\040\000\147\060\131\060\002\060"), $decoded_full, "decode_english: UTF-16LE with/BOM" );

    is( decode_english("3x²+2x+µ"), $decoded_partial, "decode_english: raw UTF-8, short" );
    is( decode_english("\063\170\302\262\053\062\170\053\302\265"), $decoded_partial, "decode_english: UTF-8, short" );
    is( decode_english("\063\170\262\053\062\170\053\265"), $decoded_partial, "decode_english: cp1252 == latin1, short" );

    isnt( decode_english("\063\170\077\053\062\170\053\033\044\102\046\120\033\050\102\040\072\040\033\044\102\073\147\044\117\041\042\101\107\105\050\044\112\033\050\102\040\033\044\102\077\047\033\050\102\040\033\044\102\044\107\044\071\041\043\033\050\102"), $decoded_full, "decode_english: iso-2022-jp, decodes, but ain't what you wanted!" );

    ok( (not eval { decode_english("\000\063\000\170\000\262\000\053\000\062\000\170\000\053\003\300\000\040\000\072\000\040\175\053\060\157\060\001\175\040\145\165\060\152\000\040\202\162\000\040\060\147\060\131\060\002"); 1 }), "decode_english: UTF-16BE w/o BOM -> error" );
    ok( "$@" =~ /Unable to decode/, "decode_english: UTF-16BE raises correct error" );
}



=head2 nicef

=cut

{
    my %nicef_tests =
    qw(
          2     2       0   0         0.00   0      13.20  13.2    0.01  0.01
          0.10  0.1     1.234  1.23   1.245  1.25
          9993432.232 9993432.23      1000   1000   100.0  100
      );

    while (my ($in, $out) = each %nicef_tests) {
        is( nicef($in), $out,  "nicef($in)" );
    }

    is( nicef("1.235", 5),    "1.235",    "nicef(1.235, 5)" );
    is( nicef("1.235232", 5), "1.23523",  "nicef(1.235232, 5)" );
    is( nicef("1.235236", 5), "1.23524",  "nicef(1.235236, 5)" );
    is( nicef(100, 0),        "100",      "nicef(100, 0)" );
    is( nicef(100.1, 0),      "100",      "nicef(100.1, 0)" );

    # Check for precision oddities
    #   float("2.05") == 2.04999999999999982236431605997495353221893310546875
    is( nicef(2.05, 1),       "2.1",      "nicef(2.05, 1)");

    # Check for banker's rounding:
    is( nicef(46.5, 0),       "47",       "nicef(46.5, 0)" );
    is( nicef(47.5, 0),       "48",       "nicef(47.5, 0)" );
    is( nicef(1.125, 2),      "1.13",     "nicef(1.125, 2)" );
    is( nicef(1.375, 2),      "1.38",     "nicef(1.375, 2)" );
}


=head2 commify

=cut

{
    my %commas =
    ( '12345' => '12,345',
      '12345.45' => '12,345.45',
      '12345.4567123' => '12,345.4567123',
      '-12345' => '-12,345',
      '-12345.45' => '-12,345.45',
      '-12345.4567123' => '-12,345.4567123',
      '123' => '123',
      '123.45' => '123.45',
      '123.4567123' => '123.4567123',
      '-123' => '-123',
      '-123.45' => '-123.45',
      '-123.4567123' => '-123.4567123',
      '234312345' => '234,312,345',
      '234312345.45' => '234,312,345.45',
      '234312345.456712' => '234,312,345.456712',
      '-234312345' => '-234,312,345',
      '-234312345.45' => '-234,312,345.45',
      '-234312345.456712' => '-234,312,345.456712',
      '0' => '0',
      '0.45' => '0.45',
      '0.4567123' => '0.4567123',
      '-0.45' => '-0.45',
      '-0.4567123' => '-0.4567123',

      '(0)' => '(0)',
      '(0.4567123)' => '(0.4567123)',
      '(-0.4567123)' => '(-0.4567123)',
      '(234312345)' => '(234,312,345)',
      '(234312345.45)' => '(234,312,345.45)',

      '$0' => '$0',
      '$0.4567123' => '$0.4567123',
      '-$0.4567123' => '-$0.4567123',
      '$234312345' => '$234,312,345',
      '$234312345.45' => '$234,312,345.45',
    );

    is commify($_), $commas{$_}, "commify($_)" for sort keys %commas;
}

=head2 qbash

=cut

{
    my $akira_bytes = "芸能山城組";# note: no utf8;
    my $has_printf  = 'Hello' eq qx|printf %s Hello|;

  SKIP: {
        my @ROUND_TRIPS =
        ( qq|Hello\tWorld|, q|Hello; World!|, q|That's it!|, q|Bob\'s|,
          q|Bob\\'s|, q|Bob\\\'s|, q|Bob\\\\'s|, $akira_bytes,
      );

        skip "no sane 'printf' command available", 0+@ROUND_TRIPS unless $has_printf;

        for (@ROUND_TRIPS) {
            my $q = qbash $_;
            is( qx| printf %s $q |, $_,  "qbash: $_ -> $q" );
        }
    }

  SKIP: {
        eval { qr/[^\pL\pM\pN\pP\pS\pZ[:print:]\s]/ };
        skip "perl is not unicode aware", 1 if $@;
        skip "no sane 'printf' command available", 1 unless $has_printf;

        my $akira = decode("UTF-8", $akira_bytes);
        my $q = encode("UTF-8", qbash($akira));
        is qx| printf %s $q |, $akira_bytes,  "qbash: UTF-8 allowed: $akira_bytes -> $q";
        # is( decode("UTF-8", qx| printf %s $q |), $akira,  "qbash: UTF-8 roundtrip" );
    }

    my @UNQUOTABLE = ("Hello\0World", "Foo\x{07}bar");
    my $i = 1;
    for (@UNQUOTABLE) {
        eval { qbash($_) };
        ok( $@ =~ /^Unquotable expression/, "qbash unquotable ".($i++) );
    }
}


=head2 length2pt

=cut

{
    is length2pt("4in"),       288,   "length2pt 1";
    is length2pt("1.34pt"),    1.34,  "length2pt 2";
    is length2pt("4.7"),       4.7,   "length2pt 3";
    is length2pt("2ft - 7in"), 1224,  "length2pt 4";
}


=head2 uri_rel2abs

=head2 uri_rel2abs_fast

=cut

{
    my $i;
    my @URIs = (
        ["foo.txt",                        "http://bob.serenevy.net/bar/",   "http://bob.serenevy.net/bar/foo.txt"       ],
        ["foo/baz.txt",                    "http://bob.serenevy.net/bar/",   "http://bob.serenevy.net/bar/foo/baz.txt"   ],
        ["/foo.txt",                       "http://bob.serenevy.net/bar/",   "http://bob.serenevy.net/foo.txt"           ],
        ["/foo/bar/baz.txt",               "http://bob.serenevy.net/bar/",   "http://bob.serenevy.net/foo/bar/baz.txt"   ],
        ["http://bob.com/bax.txt",         "http://bob.serenevy.net/bar/",   "http://bob.com/bax.txt"                    ],
        ["http://bob.com/foo/bar/bax.txt", "http://bob.serenevy.net/bar/",   "http://bob.com/foo/bar/bax.txt"            ],

        ["foo.txt",                        "http://bob.serenevy.net/a/bar",  "http://bob.serenevy.net/a/foo.txt"         ],
        ["foo/baz.txt",                    "http://bob.serenevy.net/a/bar",  "http://bob.serenevy.net/a/foo/baz.txt"     ],
        ["/foo.txt",                       "http://bob.serenevy.net/a/bar",  "http://bob.serenevy.net/foo.txt"           ],
        ["/foo/bar/baz.txt",               "http://bob.serenevy.net/a/bar",  "http://bob.serenevy.net/foo/bar/baz.txt"   ],
        ["http://bob.com/bax.txt",         "http://bob.serenevy.net/a/bar",  "http://bob.com/bax.txt"                    ],
        ["http://bob.com/foo/bar/bax.txt", "http://bob.serenevy.net/a/bar",  "http://bob.com/foo/bar/bax.txt"            ],

        ["foo.txt",                        "http://bob.serenevy.net/",       "http://bob.serenevy.net/foo.txt"           ],
        ["foo/baz.txt",                    "http://bob.serenevy.net/",       "http://bob.serenevy.net/foo/baz.txt"       ],
        ["/foo.txt",                       "http://bob.serenevy.net/",       "http://bob.serenevy.net/foo.txt"           ],
        ["/foo/bar/baz.txt",               "http://bob.serenevy.net/",       "http://bob.serenevy.net/foo/bar/baz.txt"   ],
        ["http://bob.com/bax.txt",         "http://bob.serenevy.net/",       "http://bob.com/bax.txt"                    ],
        ["http://bob.com/foo/bar/bax.txt", "http://bob.serenevy.net/",       "http://bob.com/foo/bar/bax.txt"            ],

        ["foo.txt",                        "http://bob.serenevy.net",        "http://bob.serenevy.net/foo.txt"           ],
        ["foo/baz.txt",                    "http://bob.serenevy.net",        "http://bob.serenevy.net/foo/baz.txt"       ],
        ["/foo.txt",                       "http://bob.serenevy.net",        "http://bob.serenevy.net/foo.txt"           ],
        ["/foo/bar/baz.txt",               "http://bob.serenevy.net",        "http://bob.serenevy.net/foo/bar/baz.txt"   ],
        ["http://bob.com/bax.txt",         "http://bob.serenevy.net",        "http://bob.com/bax.txt"                    ],
        ["http://bob.com/foo/bar/bax.txt", "http://bob.serenevy.net",        "http://bob.com/foo/bar/bax.txt"            ],

        ["foo.txt",                        "http://bob.serenevy.net/bar",    "http://bob.serenevy.net/foo.txt"           ],
        ["foo/baz.txt",                    "http://bob.serenevy.net/bar",    "http://bob.serenevy.net/foo/baz.txt"       ],
        ["/foo.txt",                       "http://bob.serenevy.net/bar",    "http://bob.serenevy.net/foo.txt"           ],
        ["/foo/bar/baz.txt",               "http://bob.serenevy.net/bar",    "http://bob.serenevy.net/foo/bar/baz.txt"   ],
        ["http://bob.com/bax.txt",         "http://bob.serenevy.net/bar",    "http://bob.com/bax.txt"                    ],
        ["http://bob.com/foo/bar/bax.txt", "http://bob.serenevy.net/bar",    "http://bob.com/foo/bar/bax.txt"            ],

        ["foo.txt",                        "http://bob.serenevy.net/a/bar/", "http://bob.serenevy.net/a/bar/foo.txt"     ],
        ["foo/baz.txt",                    "http://bob.serenevy.net/a/bar/", "http://bob.serenevy.net/a/bar/foo/baz.txt" ],
        ["/foo.txt",                       "http://bob.serenevy.net/a/bar/", "http://bob.serenevy.net/foo.txt"           ],
        ["/foo/bar/baz.txt",               "http://bob.serenevy.net/a/bar/", "http://bob.serenevy.net/foo/bar/baz.txt"   ],
        ["http://bob.com/bax.txt",         "http://bob.serenevy.net/a/bar/", "http://bob.com/bax.txt"                    ],
        ["http://bob.com/foo/bar/bax.txt", "http://bob.serenevy.net/a/bar/", "http://bob.com/foo/bar/bax.txt"            ],
    );

    $i = 1;
    is( uri_rel2abs(@$_[0,1]), $$_[2], "uri_rel2abs ".($i++) ) for @URIs;

    $i = 1;
    is( uri_rel2abs_fast(@$_[0,1]), $$_[2], "uri_rel2abs_fast ".($i++) ) for @URIs;
}


=head2 stringify

=cut

{
    is( stringify(2),       2,       "stringify 1" );
    is( stringify("2"),     "2",     "stringify 2" );
    is( stringify("hello"), "hello", "stringify 3" );
    is( stringify(\\2),     2,       "stringify 4" );
    is( stringify(\"Hi"),   "Hi",    "stringify 5" );

    is( stringify([2,[3,4]]), "[2,[3,4]]", "stringify 6" );

    my $hash = stringify({Hello=>2, foo=>{bar=>42}});
    ok( (($hash eq "{Hello=>2,foo=>{bar=>42}}") or ($hash eq "{foo=>{bar=>42},Hello=>2}")), "stringify 7" );

    eval 'use Math::Complex;'; die $@ if $@;
    my $z = cplx(3, 4);
    is( stringify($z),      "3+4i",  "stringify 8" );
    ok( stringify($z, stringify_underlying_object => 1) =~ /cartesian=>\[3,4\]/,  "stringify 9" );

    eval 'use Data::Dumper;'; die $@ if $@;
    ok( stringify(Data::Dumper->new([3,["4"]])) =~ /todump=>\[3,\[4\]\]/,         "stringify 10" );
}

=head2 simple_range2list

=cut

{
    is join(":", simple_range2list "1..5", "2-6"), "1:2:3:4:5:2:3:4:5:6",                             "simple_range2list 1";
    is join(":", simple_range2list "1:5", "2:2:6"), "1:2:3:4:5:2:4:6",                                "simple_range2list 2";
    is join(":", simple_range2list "1:5", "2:2:7"), "1:2:3:4:5:2:4:6",                                "simple_range2list 3";
    is join(":", simple_range2list "a:d", "a:12:ez"), "a:b:c:d:a:m:y:ak:aw:bi:bu:cg:cs:de:dq:ec:eo",  "simple_range2list 4";
    is join(":", simple_range2list "a:d", "a:13:dz"), "a:b:c:d:a:n:aa:an:ba:bn:ca:cn:da:dn",          "simple_range2list 5";
    is join(":", simple_range2list "d", 12), "a:b:c:d:1:2:3:4:5:6:7:8:9:10:11:12",                    "simple_range2list 6";
    is join(":", simple_range2list "d,12"), "a:b:c:d:1:2:3:4:5:6:7:8:9:10:11:12",                     "simple_range2list 7";
    is join(":", simple_range2list "1:.2:3"), "1:1.2:1.4:1.6:1.8:2:2.2:2.4:2.6:2.8:3",                "simple_range2list 8";
}

=head2 glob2regexp

=cut

{
    my $r;
    is $r=glob2regexp(q/Fo*o[.:+\a-z]o*{foo,bar,baz,bi bo}ot/), '^Fo.*o[\.\:\+\\\\a-z]o.*(?:foo|bar|baz|bi\ bo)ot$',   'glob2regexp big';

    is $r=glob2regexp(q/*Foo/),  '^(?=[^.]).*Foo$', 'glob2regexp *Foo';
    ok("Foo" =~ /$r/,                               'glob2regexp *Foo matches 1' );
    ok("abcFoo" =~ /$r/,                            'glob2regexp *Foo matches 2' );
    ok("Foobar" !~ /$r/,                            'glob2regexp *Foo matches 3' );

    is $r=glob2regexp(q/Foo*/),  '^Foo',          'glob2regexp Foo*';
    ok("Foo" =~ /$r/,                             'glob2regexp Foo* matches 1' );
    ok("Foobar" =~ /$r/,                          'glob2regexp Foo* matches 2' );
    ok("barFoo" !~ /$r/,                          'glob2regexp Foo* matches 3' );

    is $r=glob2regexp(q/.*Foo/), '^\..*Foo$',     'glob2regexp .*Foo';
    ok(".Foo" =~ /$r/,                            'glob2regexp .*Foo matches' );
    ok(".zFoo" =~ /$r/,                           'glob2regexp .*Foo matches' );

    is $r=glob2regexp(q/.*/),    '^\.',           'glob2regexp .*';
    ok("Foo" !~ /$r/,                             'glob2regexp .* matches 1' );
    ok(".Foo" =~ /$r/,                            'glob2regexp .* matches 2' );
    ok("" !~ /$r/,                                'glob2regexp .* matches 3' );

    is $r=glob2regexp(q/*/),     '^[^.]',         'glob2regexp *';
    ok("Foo" =~ /$r/,                             'glob2regexp * matches 1' );
    ok(".Foo" !~ /$r/,                            'glob2regexp * matches 2' );
    ok("" !~ /$r/,                                'glob2regexp * matches 3' );
}

=head2 canonicalize_filename

=cut

{
    is canonicalize_filename("/foo/bar/baz"),                                                   "/foo/bar/baz",       "canonicalize_filename 0";
    is canonicalize_filename("/foo!/bar!/baz [1]"),                                             "/foo!/bar!/baz [1]", "canonicalize_filename 1";
    is canonicalize_filename("/f\0\0oo/bar/baz"),                                               "/foo/bar/baz",       "canonicalize_filename 2";
    is canonicalize_filename("/foo/bar/baz", allow_subdirs => 0),                               "foobarbaz",          "canonicalize_filename 3";
    is canonicalize_filename("/foo!/bar!/baz [1]", allow =>'basic'),                            "/foo/bar/baz1",      "canonicalize_filename 4";
    is canonicalize_filename("/foo!/bar!/baz [1]", replacement => "_", allow =>'basic'),        "/foo_/bar_/baz_1_",  "canonicalize_filename 5";
    is canonicalize_filename("/foo/bar/baz", replacement => "_", allow_subdirs => 0),           "_foo_bar_baz",       "canonicalize_filename 6";
    is canonicalize_filename("/foo/bar/baz", allow => '[fab]'),                                 "/f/ba/ba",           "canonicalize_filename 7";
    is canonicalize_filename("/foo/bar/baz", allow => '[fab]', allow_subdirs => 0),             "fbaba",              "canonicalize_filename 8";
    is canonicalize_filename("/foo!/bar!/baz [1]", replacement => "_", squash_duplicates => 0, allow =>'basic'), "/foo_/bar_/baz__1_", "canonicalize_filename 9";

    my $f = "/f\0\0oo/bar/baz";
    canonicalize_filename($f);
    is $f, "/foo/bar/baz", "canonicalize_filename 10";

    $f = "/foo!/bar!!!/baz  [1]";
    my $ans = canonicalize_filename($f, allow => basic => replacement => { '!' => 1, '[' => "(", '/' => ':' });
    is $ans, "/foo1/bar1/baz(1", "canonicalize_filename 11";
    is $f, "/foo!/bar!!!/baz  [1]", "canonicalize_filename 12";

    $ans = canonicalize_filename($f, allow => basic => replacement => { '!' => 1, '[' => "(", '/' => ':' }, allow_subdirs => 0, squash_duplicates => 0);
    is $ans, ":foo1:bar111:baz(1", "canonicalize_filename 13";
    is $f, "/foo!/bar!!!/baz  [1]", "canonicalize_filename 14";

    $ans = canonicalize_filename($f, allow => basic => replacement => { '!' => 1, '[' => "(", '/' => ':', DEFAULT => "FOO" }, allow_subdirs => 0);
    is $ans, ":foo1:bar1:bazFOO(1FOO", "canonicalize_filename 15";
}


=head2 trim

=cut

{
    local $_ = " Hello ";
    trim;
    is $_, "Hello",                           "trim topic variable";
    is trim(" \tFoo Bar\r\n \n"), "Foo Bar",  "trim tabs and newlines";
    my $x = " There ";
    is trim($x), "There",                     "trim explicit var";
    is $x, " There ",                         "Does not modify argument";
    my @x = (" Foo", "bar ");
    trim @x;
    is $x[0], "Foo",                          "trim in-place [0]";
    is $x[1], "bar",                          "trim in-place [1]";
}



done_testing;
