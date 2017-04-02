#!/usr/bin/perl
use strict;
use warnings;

use Test::More;
use Dean::TestUtil qw/:file/;

#-----------------------------------------------------------------
#                     :file - File Operations
#-----------------------------------------------------------------


=head2 find

=cut

{
    my ($find, $want);

    $find = join ", ", sort( find( ["t"], -regex => "file" ) );
    $want = "t/file.t, t/file_comp.t";
    is $find, $want, "find: regex";

    $find = join ", ", sort( find( ["t"], -no_chdir => -regex => "file" ) );
    $want = "t/file.t, t/file_comp.t";
    is $find, $want, "find: regex (no-chdir)";

    $find = join ", ", sort( find( ["t"], -type => "f", -regex => "file" ) );
    $want = "t/file.t, t/file_comp.t";
    is $find, $want, "find: type f";

    $find = join ", ", sort( find( ["t"], -type => "d", -regex => "file" ) );
    $want = "";
    is $find, $want, "find: type d";
}


=head2 canonpath

=cut

{
    is canonpath("/usr/bin/../foo"),                   "/usr/foo",         "canonpath 1";
    is canonpath("/usr/../bin/../foo"),                "/foo",             "canonpath 2";
    is canonpath("/usr/..bin/../foo"),                 "/usr/foo",         "canonpath 3";
    is canonpath("/usr/bin../foo"),                    "/usr/bin../foo",   "canonpath 4";
    is canonpath("/usr/..bin../foo"),                  "/usr/..bin../foo", "canonpath 5";
    is canonpath("/../foo"),                           "/foo",             "canonpath 6";
    is canonpath("../foo"),                            "../foo",           "canonpath 7";
    is canonpath("/usr/bin/be/bim/bap/../../../foo"),  "/usr/bin/foo",     "canonpath 8";
    is canonpath("/usr/bin/be/../bim/../bap/../foo"),  "/usr/bin/foo",     "canonpath 9";
    is canonpath("/usr/bin/foo/.."),                   "/usr/bin",         "canonpath 10";
    is canonpath("/usr/bin/foo/../"),                  "/usr/bin",         "canonpath 11";
    is canonpath("/..foo/"),                           "/..foo",           "canonpath 12";
    is canonpath("/foo../"),                           "/foo..",           "canonpath 13";
}


=head2 rofh, wofh, rifhz, wofhz

=cut

{
    use File::Temp 'tempfile';
    local $/ = undef;
    my ($fh, $filename) = tempfile;
    print $fh "Hello";
    close $fh;

    $fh = rwfh $filename;
    is( scalar(<$fh>), "Hello",   "rwfh" );
    seek($fh, 0, 0);
    print $fh "There";
    close $fh;

    $fh = rofh $filename;
    is( scalar(<$fh>), "There",   "rofh" );
    close $fh;

    $fh = rofhz $filename;
    is( scalar(<$fh>), "There",   "rofhz (autopop)" );
    close $fh;

    $fh = wofh $filename;
    print $fh "World!";
    close $fh;

    $fh = rwfhz $filename;
    is( scalar(<$fh>), "World!",   "rwfhz (autopop)" );
    seek($fh, 0, 0);
    print $fh "UNCOMPRESSED";
    close $fh;

    $fh = rofh $filename;
    is( scalar(<$fh>), "UNCOMPRESSED", "rofh" );
    close $fh;

    $fh = wofhz $filename;
    print $fh "COMPRESSED!";
    close $fh;

    my $len = do { use bytes; length("COMPRESSED!") };
    ok( (-s $filename) != $len, "compressed looks compressed" );

    $fh = rofhz $filename;
    is( scalar(<$fh>), "COMPRESSED!",   "rofhz" );
    close $fh;

    open $fh, "<:raw", $filename;
    my $data = <$fh>;
    is( substr($data, 0, 2), "\037\213", "compressed data has right magic" );

    isnt( $data, "COMPRESSED", "compressed data is compressed" );
    close $fh;

    unlink $filename;
}



done_testing;
