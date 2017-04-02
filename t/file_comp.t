#!/usr/bin/perl
use strict;
use warnings;

use Test::More;
use Dean::TestUtil  qw/:file_comp/;



=head2 bytes2size

=cut

is( bytes2size(.4),                                 "0.40B",      "bytes2size 1" );
is( bytes2size(1024),                               "1KiB",       "bytes2size 2" );
is( bytes2size(1024*1024*5.3),                      "5.30MiB",    "bytes2size 3" );
is( bytes2size(1024*1024*5.3, 3, 2),                "5.30MiB",    "bytes2size 4" );
is( bytes2size(1024*1024*5.3, 4, 4),                "5.3000MiB",  "bytes2size 5" );
is( bytes2size(1024*1024*1024*1024*4.3524),         "4.35TiB",    "bytes2size 6" );
is( bytes2size(1024*1024*1024*1024*4.3524, 2),      "4.4TiB",     "bytes2size 7" );
is( bytes2size(1024*1024*1024*1024*4.3524, 2, 5),   "4.35240TiB", "bytes2size 8" );
is( bytes2size(-16462643.2),                        "-15.7MiB",   "bytes2size 9 (negative values)" );
is( bytes2size(0),                                  "0.00B",      "bytes2size 10 (zero bytes)" );
is( bytes2size(1024*1024*1024*1024*1024*4.3524, 2), "4.4PiB",     "bytes2size 11" );


=head2 bytes2size_SI

=cut

is( bytes2size_SI(.4),                                 "0.40B",      "bytes2size_SI 1" );
is( bytes2size_SI(1000),                               "1KB",        "bytes2size_SI 2" );
is( bytes2size_SI(1000*1000*5.3),                      "5.30MB",     "bytes2size_SI 3" );
is( bytes2size_SI(1000*1000*5.3, 3, 2),                "5.30MB",     "bytes2size_SI 4" );
is( bytes2size_SI(1000*1000*5.3, 4, 4),                "5.3000MB",   "bytes2size_SI 5" );
is( bytes2size_SI(1000*1000*1000*1000*4.3524),         "4.35TB",     "bytes2size_SI 6" );
is( bytes2size_SI(1000*1000*1000*1000*4.3524, 2),      "4.4TB",      "bytes2size_SI 7" );
is( bytes2size_SI(1000*1000*1000*1000*4.3524, 2, 5),   "4.35240TB",  "bytes2size_SI 8" );
is( bytes2size_SI(-16500000.4),                        "-16.5MB",    "bytes2size_SI 9 (negative values)" );
is( bytes2size_SI(0),                                  "0.00B",      "bytes2size_SI 10 (zero bytes)" );
is( bytes2size_SI(1000*1000*1000*1000*1000*4.3524, 2), "4.4PB",      "bytes2size_SI 11" );


=head2 size2bytes

=cut

is( size2bytes('15.7MiB'),   16462643.2, "size2bytes 1" );
is( size2bytes('7358kiB'),   7534592,    "size2bytes 2" );
is( size2bytes('-7358kiB'), -7534592,    "size2bytes 3" );
is( size2bytes('7358piB'), 8284371514548027392,    "size2bytes 4" );

is( size2bytes('15.7MB'),   15700000, "size2bytes 1 SI" );
is( size2bytes('7358kB'),   7358000,  "size2bytes 2 SI" );
is( size2bytes('-7358kB'), -7358000,  "size2bytes 3 SI" );
is( size2bytes('7358pB'), 7358000000000000000,  "size2bytes 4 SI" );

is( size2bytes('128K'),  size2bytes('128KB'), "size2bytes 4" );
is( size2bytes('128k'),  size2bytes('128Kb'), "size2bytes 5" );
is( size2bytes('128kb'), size2bytes('128Kb'), "size2bytes 6" );
is( size2bytes('128kB'), size2bytes('128KB'), "size2bytes 7" );

ok( size2bytes('128KB') > size2bytes('128kb'), "size2bytes 8" );



=head2 size_sum

=cut

is( size_sum(qw/1.5MB -650kB -1253kB/),    "-393.6KiB", "size_sum 1" );
is( size_sum(qw/1.5MiB -650kiB -1253kiB/), "-367KiB", "size_sum 1" );



done_testing;
