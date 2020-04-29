#!/usr/bin/perl
use strict;
use warnings;

use File::Glob qw/ bsd_glob /;
use File::Temp;
use File::stat;
use Test::More;

use Dean::Util qw/:file/;

#-----------------------------------------------------------------
#                     :file - File Operations
#-----------------------------------------------------------------


=head2 sed

=cut

{
    my $tmpdir = File::Temp->newdir();
    my $CONTENT = '#
# You can also edit it by hand, if you so choose.

DAY="6"
';
    my $CONTENT2 = '#
# You can also edit it by hand, if you so choose.

DAY="7"
';
    my $CONTENT3 = '1. #
2. # You can also edit it by hand, if you so choose.
3. 
4. DAY="6"
';

    my $file = "$tmpdir/A";
    fprint $file, $CONTENT;
    my $time = time - 1000;
    utime $time, $time, $file;
    my $stat = stat($file);
    ok 500 < (time - $stat->mtime()), "utime check";
    ok !(sed { s/^MISSING/PLUGH/ } $file), "false on no match";
    my $stat2 = stat($file);
    is cat($file), $CONTENT, "no change leaves file as-is";
    is $stat2->mtime, $stat->mtime, "does not modify mtime";
    my @files = bsd_glob("$tmpdir/*");
    is 0+@files, 1, "No new files created 1";

    ok +(sed { s/^DAY=\K.*/"7"/ } $file), "true on change";
    $stat2 = stat($file);
    is cat($file), $CONTENT2, "changed file";
    ok $stat2->mtime > $stat->mtime, "modifies mtime";
    @files = bsd_glob("$tmpdir/*");
    is 0+@files, 1, "No new files created 2";

    ok !(sed { s/^DAY=\K.*/"7"/ } "$tmpdir/B", ignore_errors => 1), "false on error";
    @files = bsd_glob("$tmpdir/*");
    is 0+@files, 1, "No new files created 3";

    fprint $file, $CONTENT;
    sed { s/^DAY=\K.*/"7"/ } $file, backup => ".bup";
    ok -f "$file.bup", "Backup created";
    is cat($file), $CONTENT2, "changed file";
    is cat("$file.bup"), $CONTENT, "backup unchanged";
    @files = bsd_glob("$tmpdir/*");
    is 0+@files, 2, "Only one new file created";
    unlink "$file.bup";

    fprint $file, $CONTENT;
    sed { /^DAY/ ? qq[DAY="7"\n] : $_ } $file, returns => 1;
    is cat($file), $CONTENT2, "changed file";
    @files = bsd_glob("$tmpdir/*");
    is 0+@files, 1, "No new files created 4";

    fprint $file, $CONTENT;
    my $nonempty = 0;
    sed { $nonempty++ if /^\S/ } $file;
    is $nonempty, 3, "called once per line";
    is cat($file), $CONTENT, "file unchanged";
    @files = bsd_glob("$tmpdir/*");
    is 0+@files, 1, "No new files created 5";

    fprint $file, $CONTENT;
    $nonempty = 0;
    sed { $nonempty++ if /^\S/; s/^DAY=\K.*/"7"/m } $file, whole_file => 1;
    is $nonempty, 1, "called once per file";
    is cat($file), $CONTENT2, "changed file";
    @files = bsd_glob("$tmpdir/*");
    is 0+@files, 1, "No new files created 6";

    fprint $file, $CONTENT;
    sed { "$.. $_" } $file, returns => 1;
    is cat($file), $CONTENT3, "\$. works";
}


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
