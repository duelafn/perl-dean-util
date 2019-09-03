#!/usr/bin/perl
use strict;
use warnings;
use YAML;

use Test::More tests => 21;
BEGIN { use_ok 'Dean::Util', qw/:parse deep_eq/ }

#-----------------------------------------------------------------
#             :parse - General Interpreters / Parsers
#-----------------------------------------------------------------


=head1 str2hash

=cut

my %h = str2hash 'foo, bar => "Hmmm, a comma", :baz<23>, :!bip, quxx => Spaces are fine, :bop';
is_deeply( \%h,
 { foo => 1,
   bar => 'Hmmm, a comma',
   baz => 23,
   bip => 0,
   bop => 1,
   quxx => 'Spaces are fine',
 },
           "str2hash seems to work" );


=head1 unformat

=cut

my $str = join ":", @{unformat q/%s - %s/, q/My Evil Twin - They Might Be Giants/};
ok $str eq "My Evil Twin:They Might Be Giants",  "basic unformat";

my $h = unformat { format => q/%t - %a/, as => "hash", conversion_aliases => {qw/t s a s/} }, q/My Evil Twin - They Might Be Giants/;
ok deep_eq($h, { '%a' => "They Might Be Giants", '%t' => "My Evil Twin" }),  "hash with aliases";

$h = unformat { format => q/%15t - % 10a/, as => "hash", conversion_aliases => {qw/t s a s/} }, q/   My Evil Twin - They Might Be Giants/;
ok deep_eq($h, { '%a' => "They Might Be Giants", '%t' => "My Evil Twin" }),  "fancy formatted hash with aliases";

$h = unformat { format => q/%t - %a/, as => "pairs", conversion_aliases => {qw/t s a s/} }, q/My Evil Twin - They Might Be Giants/;
ok deep_eq($h, [ '%t' => "My Evil Twin", '%a' => "They Might Be Giants" ]),  "pairs with aliases";

$h = unformat { format => q/%a - %t/, as => "pairs", conversion_aliases => {qw/t s a s/} }, q/They Might Be Giants - My Evil Twin/;
ok deep_eq($h, [ '%a' => "They Might Be Giants", '%t' => "My Evil Twin" ]),  "pairs with aliases order matters";

$h = unformat { format => q/%t - %a/, as => "tuples", conversion_aliases => {qw/t s a s/} }, q/My Evil Twin - They Might Be Giants/;
ok deep_eq($h, [ ['%t' => "My Evil Twin"], ['%a' => "They Might Be Giants"] ]),  "tuples with aliases";

$h = unformat { format => q/%t - %a/, as => "list_list", conversion_aliases => {qw/t s a s/} }, q/My Evil Twin - They Might Be Giants/;
ok deep_eq($h, [ ["My Evil Twin", "They Might Be Giants"], [qw/ %t %a /] ]),  "tuples with aliases";

$h = unformat { format => q/%t - %a/, as => "hash", conversion_aliases => {qw/t s a s/}, conversion_map => { qw/t Title a Artist/ } },
  q/My Evil Twin - They Might Be Giants/;
ok deep_eq($h, { Artist => "They Might Be Giants", Title => "My Evil Twin" }),  "hash with aliases & conversion map";

my @h = unformat { format => q/%t - %a/, as => "hash", conversion_aliases => {qw/t s a s/}, conversion_map => { qw/t Title a Artist/ } },
  q/My Evil Twin - They Might Be Giants/,
  q/Blossom Lake - Morin Khuur Quartet/;
ok deep_eq(\@h, [ { Artist => "They Might Be Giants", Title => "My Evil Twin" },
                  { Artist => "Morin Khuur Quartet",  Title => "Blossom Lake" },
                ] ),  "hash with aliases & conversion map";

$h = unformat { format => "%s%D", special_conversions => { D => '(\d{4}:\d{2}:\d{2})' }, as => "hash" },
  "Foo:Bar:Baz:2004:2006:12:14";
ok deep_eq($h, {"%s" => "Foo:Bar:Baz:2004:", "%D" => "2006:12:14"}),  "special_conversions";

for (qw/list list_list pairs tuples/) {
  $h = unformat { format => "%s%D", special_conversions => { D => '(\d{4}:\d{2}:\d{2})' }, as => $_ },
    "Foo:Bar:Baz:2004:2006:124:14";
  ok deep_eq($h, []),  "no match $_";
}

$h = unformat { format => "%s%D", special_conversions => { D => '(\d{4}:\d{2}:\d{2})' }, as => "hash" },
  "Foo:Bar:Baz:2004:2006:124:14";
ok deep_eq($h, {}),  "no match hash";

# Token "test everything"
$h = unformat { format => "%s%b %x %05d %.3f%d %g" }, "Foo1010 3efa 00010 3.1231234 -3.75e+10";
ok "@$h" eq "Foo 1010 3efa 10 3.123 1234 -3.75e+10", '"big and hairy"';

$h = unformat { format => "%s%b %x %05d %.3f%d %g", as => "regex" };
ok $h eq '(.*?)([01]+?)\ ([0-9a-f]+?)\ 0{0,5}(\-?\d+)\ (\-?\d+\.\d{3})(\-?\d+)\ (\-?\d+\.\d{0,}(?:e[+-]\d+)?)', 'as regex in scalar context';

{
  $h = [ unformat { format => "%s%b %x %05d %.3f%d %g", as => "regex" } ];
  local $" = " :: ";
  ok "@$h" eq
    '(.*?)([01]+?)\ ([0-9a-f]+?)\ 0{0,5}(\-?\d+)\ (\-?\d+\.\d{3})(\-?\d+)\ (\-?\d+\.\d{0,}(?:e[+-]\d+)?) :: %s :: %b :: %x :: %05d :: %.3f :: %d :: %g',
      'as regex in list context';
}

$h = unformat { format => "%s%b %x %05d %.3f%d %g" }, "Foo1010 3efa 00010 3.1231234 +3.75e+10";
ok !@$h, "Off by a technicality (%g doesn't allow +)";


=head1 parse_user_agent

=cut

my @ua_correct = @{Load(<<'YAML')};
---
- engine: Gecko
  engine-version: 20050610
  generic_os: Windows
  os: Windows Vista
  program: K-meleon
  type: browser
  user-agent: Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.7.13) Gecko/20050610 K-Meleon/0.9
  version: 0.9
- engine: KHTML
  engine-version: 3.1
  generic_os: Linux
  program: Konqueror
  type: browser
  user-agent: 'Mozilla/5.0 (compatible; Konqueror/3.1; Linux 2.4.22-10mdk; X11; i686; fr, fr_FR)'
  version: 3.1
- engine: KHTML
  engine-version: 3.5.5
  generic_os: Linux
  os: Debian
  program: Konqueror
  type: browser
  user-agent: Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.5 (like Gecko) (Debian)
  version: 3.5
- engine: custom
  generic_os: Linux
  os: Gentoo
  program: Links
  type: textbrowser
  user-agent: Links (2.1pre17; Linux 2.6.11-gentoo-r8 i686; 80x24)
  version: 2.1
- engine: Gecko
  engine-version: 20050915
  generic_os: Mac OS
  os: Macintosh
  program: Firefox
  type: browser
  user-agent: Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.7.12) Gecko/20050915 Firefox/1.0.7
  version: 1.0.7
- engine: Gecko
  engine-version: 20060512
  generic_os: Mac OS
  os: Macintosh
  program: Bonecho
  type: browser
  user-agent: Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8.1a2) Gecko/20060512 BonEcho/2.0a2
  version: 2.0
- engine: Gecko
  engine-version: 20020920
  generic_os: Solaris
  os: Solaris
  program: Netscape
  type: browser
  user-agent: Mozilla/5.0 (X11; U; SunOS sun4u; en-US; rv:1.0.1) Gecko/20020920 Netscape/7.0
  version: 7.0
- engine: Mozilla
  engine-version: 3.0
  generic_os: Solaris
  obsolete: 1
  os: Solaris
  program: Netscape
  type: browser
  user-agent: Mozilla/3.0 (X11; I; SunOS 5.4 sun4m)
  version: 3.0
- engine: Mozilla
  engine-version: 4.8
  generic_os: Windows
  obsolete: 1
  os: Windows 2000
  program: Netscape
  type: browser
  user-agent: 'Mozilla/4.8 [en] (Windows NT 5.0; U)'
  version: 4.8
- engine: Presto
  generic_os: Linux
  program: Opera
  type: browser
  user-agent: Opera/8.0 (X11; Linux i686; U; cs)
  version: 8.0
- engine: WebKit
  engine-version: 412
  generic_os: Mac OS
  os: Macintosh
  program: Safari
  type: browser
  user-agent: 'Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en) AppleWebKit/412 (KHTML, like Gecko) Safari/412'
  version: 412
- engine: Gecko
  engine-version: 20060910
  generic_os: Linux
  program: SeaMonkey
  type: browser
  user-agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.0.7) Gecko/20060910 SeaMonkey/1.0.5
  version: 1.0.5
- generic_os: Windows
  obsolete: 1
  os: Windows CE
  type: mobile
  user-agent: Mozilla/4.0 (compatible; MSIE 4.01; Windows CE; Smartphone; 240x320)
- engine: Mozilla
  engine-version: 4.0
  generic_os: Windows
  obsolete: 1
  os: Windows 98
  program: Netscape
  type: browser
  user-agent: Mozilla/4.0 (compatible; MSIE 6.0; MSN 2.5; Windows 98)
  version: 4.0
- engine: Mozilla
  engine-version: 1.22
  generic_os: Windows
  obsolete: 1
  os: Windows 95
  program: Netscape
  type: browser
  user-agent: Mozilla/1.22 (compatible; MSIE 2.0; Windows 95)
  version: 1.22
- engine: custom
  generic_os: Windows
  obsolete: 1
  os: Windows XP
  program: AOL Browser
  type: browser
  user-agent: Mozilla/4.0 (compatible; MSIE 6.0; America Online Browser 1.1; rev1.1; Windows NT 5.1;)
  version: 1.1
- engine: custom
  program: Lynx
  type: textbrowser
  user-agent: Lynx/2.8.5rel.1 libwww-FM/2.14 SSL-MM/1.4.1 OpenSSL/0.9.8a
  version: 2.8.5
- engine: Mozilla
  engine-version: 4.7
  generic_os: Windows
  obsolete: 1
  os: Windows NT
  program: Netscape
  type: browser
  user-agent: 'Mozilla/4.7 [en] (WinNT; U)'
  version: 4.7
- engine: Mozilla
  engine-version: 4.72
  generic_os: Linux
  obsolete: 1
  program: Netscape
  type: browser
  user-agent: 'Mozilla/4.72 [en] (X11; U; Linux 2.4.18 i686)'
  version: 4.72
YAML

my @ua;
push @ua, scalar parse_user_agent($_) for
  "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.7.13) Gecko/20050610 K-Meleon/0.9",
  'Mozilla/5.0 (compatible; Konqueror/3.1; Linux 2.4.22-10mdk; X11; i686; fr, fr_FR)',
  'Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.5 (like Gecko) (Debian)',
  'Links (2.1pre17; Linux 2.6.11-gentoo-r8 i686; 80x24)',
  'Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.7.12) Gecko/20050915 Firefox/1.0.7',
  'Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8.1a2) Gecko/20060512 BonEcho/2.0a2',
  'Mozilla/5.0 (X11; U; SunOS sun4u; en-US; rv:1.0.1) Gecko/20020920 Netscape/7.0',
  'Mozilla/3.0 (X11; I; SunOS 5.4 sun4m)',
  'Mozilla/4.8 [en] (Windows NT 5.0; U)',
  'Opera/8.0 (X11; Linux i686; U; cs)',
  'Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en) AppleWebKit/412 (KHTML, like Gecko) Safari/412',
  'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.0.7) Gecko/20060910 SeaMonkey/1.0.5',
  'Mozilla/4.0 (compatible; MSIE 4.01; Windows CE; Smartphone; 240x320)',
  'Mozilla/4.0 (compatible; MSIE 6.0; MSN 2.5; Windows 98)',
  'Mozilla/1.22 (compatible; MSIE 2.0; Windows 95)',
  'Mozilla/4.0 (compatible; MSIE 6.0; America Online Browser 1.1; rev1.1; Windows NT 5.1;)',
  'Lynx/2.8.5rel.1 libwww-FM/2.14 SSL-MM/1.4.1 OpenSSL/0.9.8a',
  'Mozilla/4.7 [en] (WinNT; U)',
  'Mozilla/4.72 [en] (X11; U; Linux 2.4.18 i686)',
  ;

# is_deeply( $ua[$_], $ua_correct[$_], "parse_user_agent $_" ) for 0..$#ua;
