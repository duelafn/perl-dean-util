package Dean::Util;
## no critic (ReusedNames, MismatchedOperators);
use 5.020; use experimental 'signatures';
use strict; use warnings; use Carp;
use re 'taint';

use vars qw(@ISA @EXPORT_OK @EXPORT %EXPORT_TAGS $VERSION);
use base "Exporter";

@EXPORT = qw/insert_Dean_Util_functions list_Dean_Util_functions get_Dean_Util_function_string
             get_Dean_Util_code check_Dean_Util_functions upgrade_Dean_Util_functions
             remove_Dean_Util_functions
            /;

$EXPORT_TAGS{_fake} = [qw/INCLUDE_POD POD_ONLY/];
our $VERSION = '1.087';

sub INCLUDE_POD { 1 };
sub POD_ONLY    { 1 };
sub TO_FINISH   { 1 };


=pod

=head1 NAME

Dean::Util - Utilities created by Dean Serenevy

=head1 SYNOPSIS

 use Dean::Util qw/map_pair nsign min_max/;
 ...

Then later, to remove dependence on Dean::Util

 perl -MDean::Util -we insert_Dean_Util_functions The/Module.pm

=head1 DESCRIPTION

This is a set of utility functions for the perl programming language that I
find myself rewriting frequently. Normally, putting functions into a module
introduces a dependency on that module which can be a hassle in some
situations. This is a "smart" module which is capable of replacing the
C<use Dean::Util...> line with the code for the requested functions.
Thus, machines that have Dean::Util installed can use it as a module,
but when requested, a (Dean::Util) dependency-free version of the file
may be made.

=head1 EXPORTED FUNCTIONS

=cut

$EXPORT_TAGS{utility} =
[qw/list_Dean_Util_functions check_Dean_Util_functions
    get_Dean_Util_code insert_Dean_Util_functions
    get_Dean_Util_function_string upgrade_Dean_Util_functions
    remove_Dean_Util_functions
   /];

=head2 :utility - Using Dean::Util

=head3 list_Dean_Util_functions

This function prints a column-formatted list of the functions included in
the Dean::Util package.

=cut

#BEGIN: list_Dean_Util_functions, 1 line; depends: get_Dean_Util_code, format_cols
sub list_Dean_Util_functions { print format_cols( [sort(keys %{ get_Dean_Util_code() })], col_space => 2 ) }


=head3 check_Dean_Util_functions

This function attempts to verify that the Dean/Util.pm is properly
structured. This function is intended to be run only by people who make
changes to the Dean/Util.pm file to check that their code is properly
formatted for the module to parse.

=cut

#BEGIN: check_Dean_Util_functions, depends: get_Dean_Util_code, str
my %_Dean_Util_ignore_paren_rule = map +($_,1), qw/ unique NOCOLOR pmap find NOTICE INFO WARNING ERR ERROR ALERT CRIT DEBUG EMERG untaint max min check_Dean_Util_functions sprint_one_var xml_encode subopts /;
my %_Dean_Util_ignore_dependencies = map +($_,1), qw/ NOCOLOR pmap find check_Dean_Util_functions sprint_one_var /;
sub check_Dean_Util_functions {
  my %opt = @_;
  my %x = %{get_Dean_Util_code()};
  for (grep {!exists $x{$_} and !/^(?:INCLUDE_POD|POD_ONLY)$/} @EXPORT_OK) {
    print "$_ in \@EXPORT_OK but does not exist.\n";
  }
  for my $f (keys %x) {
    $x{$f}{line} = str($x{$f}{line}) || "???";
    unless (grep $f eq $_, @EXPORT_OK) { print "$f not exportable, add to \@EXPORT_OK\n" }
    unless ($x{$f}{count})         { print "$f never defined, line $x{$f}{line}\n"; $x{$f}{count}=0; }
    unless ($x{$f}{code})          { print "$f has no code, line $x{$f}{line}\n" }
    if     ($x{$f}{count} > 1)     { print "$f multiply defined, line $x{$f}{line}\n" }
    if     ($x{$f}{pod_redefined}) { print "POD for $f redefined, line $x{$f}{pod_redefined}\n" }
    if     ($x{$f}{line} eq '???') { print "Can't determine line number of $f!?!\n" }
    if     ($opt{check_pod} and !$x{$f}{pod}) { print "$f has no POD documentation\n" }

    # prepare to analize code more thoroughly
    my $code = str($x{$f}{code}); # so we can apply regexps
    $code =~ s/(?<!\$|m|s)#.*?$//smg;         # so comments don't mess us up

    if ($code =~ /(?<![\>"'])\&(\w+)[^\w\(]/) {
      print "& subroutine, &$1, used without ( ) in $f, line $x{$f}{line}\n" unless exists $_Dean_Util_ignore_paren_rule{$f}
    }

    for my $g (keys %x) {
      # check for uses of $g as a function (not a variable, or a string)
      if ($g =~ /^\w/ and $code =~ /(?<!sub)[^>\*\w\$\@\%\"'\{:]\Q$g\E(?=[^\w]|$)(?!\s*=>|\()/s)
        { print "$g used without () in $f, line $x{$f}{line}\n" unless exists $_Dean_Util_ignore_paren_rule{$f} }
      if ($code =~ /(?<!sub)[^\w\$\@\%\"'\{](?<!\-\>)\Q$g\E(?!\s*=>)(?:[^\w]|$)/s and not grep /^\Q$g\E$/, $f, @{$x{$f}{depends}})
        { print "$g used in $f but not included in dependencies, line $x{$f}{line}\n" unless exists $_Dean_Util_ignore_dependencies{$f} }
    }

    for my $g (@{$x{$f}{depends}}) {
      unless (exists $x{$g})
        { print "$f depends on $g, but $g is not available\n" }
      unless ($code =~ /\Q$g\E/)
        { print "$f depends on $g, but doesn't seem to use it, line $x{$f}{line}\n" }
    }
  }
}
#END: check_Dean_Util_functions

=head3 get_Dean_Util_code

Returns a hash ref with an entry of the following type for each function
and variable defined in Dean::Util.

 name => { code    => '...',
           pod     => '...',
           depends => [ 'thing 1', 'thing 2', ... ]
         }

Some additional information may be included in each sub-hash for debugging
purposes or internal use.

=cut

#BEGIN: get_Dean_Util_code
#-----------------------------------------------------------------
# Returns a hash ref of all functions defined in Dean::Util together with
# their code and dependencies.
#   { name => { code => "code", depends => [ qw/func1 func2 .../ ] } }

sub get_Dean_Util_code {
  my ($file) = ($INC{"Dean/Util.pm"});
  my $UTIL;
  die "Can't "."find Dean::Util in %INC" unless $file and -r $file;
  open $UTIL, "<", $file or die "Error can't open $file for reading: $!";

  my $token = '[\$\@\%\&\*]?[_a-zA-Z]\w*';
  my $nontoken = '[^\$\@\%\&\*\w]';
  my ($lines,$func,%code,$pod_func);
  while (<$UTIL>) {
    if    ($lines and $func) { $code{$func}{code}    .= $_; $func = undef unless --$lines }
    elsif ($pod_func)        { $code{$pod_func}{pod} .= $_ }
    elsif ($func)            { $code{$func}{code}    .= $_ }

    # We break the if statement here for some extra error checking.
    if (/^#BEGIN(?:$nontoken)+($token)/) {
      die "Error: BEGINing function $1 before function $func has ended, at Dean::Util line $.\n" if $lines or $func;
      print STDERR "POD for $pod_func terminated by BEGIN for $1: line $.\n"                         if $pod_func;
      $func  = $1;
      $code{$func}{count}++;
      $code{$func}{line} = $.;

      if (/\W(\d+)\s+line/i) { $code{$func}{code} = $_; $lines = $1 }
      else                   { $code{$func}{code} = $_ }

      if (/\W(?i:requires?|depends?)(?:$nontoken)+(.+?)\s*$/) { my $x = $1; $code{$func}{depends} = [ $x =~ /($token)/g ] }
    }
    elsif (/^#END(?:$nontoken)+($token)/) {
      $code{$func}{code} .= $/;
      $func = undef;
    }
    elsif (/^=head3(?:$nontoken)+($token)/) {
      die "Error: Starting POD for $1 before function $func has ended: line $.\n" if $lines or $func;
      print STDERR "POD for $pod_func terminated by POD for $1: line $.\n"        if $pod_func;
      $pod_func = $1;
      $code{$pod_func}{pod_redefined} = $. if $code{$pod_func}{pod};
      $code{$pod_func}{pod} = $_;
    }
    elsif (/^=cut\s*$/) { undef $pod_func }
  }
  close $UTIL;

  return \%code;
}
#END: get_Dean_Util_code

=head3 insert_Dean_Util_functions

Replaces all occurrences of "use Dean::Util ...;" ("..." is everything up to
first semi-colon, so don't use C<qw; ;>) with the actual source code of the
functions requested from Dean::Util. The original files are saved to a
backup file which is just the original file name with a C<~> appended. The
list of files to modify is either included as a list of arguments or is
read from C<@ARGV>.

As in the function
L<get_Dean_Util_function_string|/get_Dean_Util_function_string>, the
special symbols C<INCLUDE_POD> and C<POD_ONLY> may be used to indicate that
all further inclusions (restricted to each individual "use" block) should
include their POD documentation before the code, or exclude the code and
only output the POD documentation. Example:

 use Dean::Util qw/max min INCLUDE_POD join_multi map_pair/;
 use Dean::Util qw/is_num is_int/;
 # ... later, possibly even after __END__
 use Dean::Util qw/POD_ONLY is_num is_int/;

Would include code and POD documentation for L<join_multi|/join_multi> and
L<map_pair|/map_pair(&&@)>. The code and POD documentation for L<is_num|/is_num>
and L<is_int|/is_int> would be inserted separately.

Note: Multiple C<use Dean::Util> inclusions may result in multiple
subroutine definitions so don't C<use> the same function twice unless they
are in different scopes.

=cut

#BEGIN: insert_Dean_Util_functions; depends: get_Dean_Util_function_string, get_Dean_Util_code
#-----------------------------------------------------------------
# Replaces all occurances of "use Dean::Util ...;" with the actual
# source code of the functions requested from Dean::Util.
sub insert_Dean_Util_functions {
  my @files = @_ ? @_ : @ARGV;
  my $code = get_Dean_Util_code();
  my ($IN,$OUT);
  local ($,, $\);

  for my $file (@files) {
    rename $file,   $file."~" or die "Error making backup of file $file: $!";
    open $IN,  "<", $file."~" or die "Error opening $file~ for reading: $!";
    open $OUT, ">", $file     or die "Error opening $file for writing: $!";
    chmod( ((stat $IN)[2] & 07777), $OUT );
    while (local $_ = <$IN>) {
      unless (/^\s*use Dean::Util\b(.*)/) { print $OUT $_; next }
      my ($list, $include_pod) = ($1, 0);
      until ($list =~ /;/) { $list .= <$IN> }

      # determine requested functions
      my @funcs    = eval $list;
      $list =~ s/\n/\n#  /g;
      $list =~ s/#\s+$//;
      print $OUT "#", "-"x65, $/, "# BEGIN             Dean::Util code version $VERSION\n#\n";
      print $OUT "#  use Dean::Util$list\n\n";
      print $OUT +get_Dean_Util_function_string($code, @funcs);
      print $OUT "#\n# END               Dean::Util code version $VERSION\n", "#", "-"x65, $/;
    }
    close $IN;
    close $OUT;
  }
}
#END: insert_Dean_Util_functions

=head3 upgrade_Dean_Util_functions

Once C<insert_Dean_Util_functions> has been used to "export" a list of
Dean::Util functions, this command will replace Dean::Util function
blocks with more recent function versions, thus upgrading the exported
script.

=cut

#BEGIN: upgrade_Dean_Util_functions; depends: get_Dean_Util_function_string, get_Dean_Util_code
#-----------------------------------------------------------------
sub upgrade_Dean_Util_functions {
  my %opt = ('HASH' eq ref($_[0])) ? %{shift()} : ();
  my @files = @_ ? @_ : @ARGV;
  my $code = get_Dean_Util_code();
  my ($IN,$OUT);
  local ($,, $\, $_);

  my $token = '[\$\@\%\&\*]?[_a-zA-Z]\w*';
  my $nontoken = '[^\$\@\%\&\*\w]';
  for my $file (@files) {
    rename $file,   $file."~" or die "Error making backup of file $file: $!";
    open $IN,  "<", $file."~" or die "Error opening $file~ for reading: $!";
    open $OUT, ">", $file     or die "Error opening $file for writing: $!";
    chmod( ((stat $IN)[2] & 07777), $OUT );
    my ($inblock, $useline, $version, @funcs);
    while ($_ = <$IN>) {
      if (/^# BEGIN(?>\s+)Dean::(?:Test)?Util code version ([\d.]+)/) {
        $version = $1;
        if ($version < $VERSION) {
          $inblock = 1;
          next;
        } elsif ($version == $VERSION) {
          print STDERR "found Dean::Util block of same version as current version...";
          $inblock = $opt{replace};
          say STDERR $inblock ? "replacing anyway" : "skipping";
          next if $inblock;
        } else {
          print STDERR "found Dean::Util block of NEWER version as current version! ...";
          $inblock = $opt{downgrade};
          say STDERR $inblock ? "DOWNGRADING!" : "skipping";
          next if $inblock;
        }
      }

      print $OUT $_ and next unless $inblock;
      next if $useline and !/^# END(?>\s+)Dean::(?:Test)?Util code version/;

      if (!defined($useline) and /^#  use Dean::(?:Test)?Util(.*)/m) {
        $useline = $1;
        $useline =~ s/^\s+[\d.]+//;
        until ($useline =~ /;/) { $useline .= <$IN> }
        $useline =~ s/^#  //mg;
        @funcs = eval $useline;
        next;
      }

      if (/^# END(?>\s+)Dean::(?:Test)?Util code version/) {
        # Finalize!
        $useline =~ s/\n/\n#  /g if $useline;
        $useline =~ s/#\s+$//    if $useline;
        print $OUT "# BEGIN             Dean::Util code version $VERSION\n#\n";
        print $OUT "#  use Dean::Util$useline\n\n" if $useline;
        print $OUT +get_Dean_Util_function_string($code, @funcs);
        print $OUT "#\n# END               Dean::Util code version $VERSION\n";

        $inblock = 0;
        undef $useline;
        next;
      }

      $useline = 0 unless /^#/;
      push @funcs, $1 if /^#BEGIN(?:$nontoken)+($token)/;
    }
    close $IN;
    close $OUT;
  }
}
#END: upgrade_Dean_Util_functions

=head3 remove_Dean_Util_functions

Once C<insert_Dean_Util_functions> has been used to "export" a list of
Dean::Util functions, this command can be used to remove them and
restore the use Dean::Util line.

=cut

#BEGIN: remove_Dean_Util_functions
#---------------------------------
sub remove_Dean_Util_functions {
  my @files = @_ ? @_ : @ARGV;
  my ($IN,$OUT);
  local ($,, $\, $_);

  for my $file (@files) {
    rename $file,   $file."~" or die "Error making backup of file $file: $!";
    open $IN,  "<", $file."~" or die "Error opening $file~ for reading: $!";
    open $OUT, ">", $file     or die "Error opening $file for writing: $!";
    chmod( ((stat $IN)[2] & 07777), $OUT );
    my ($inblock, $useline, $version, @funcs);
    while ($_ = <$IN>) {
      if (/^# BEGIN(?>\s+)Dean::Util code version ([\d.]+)/) {
        $version = $1;
        $inblock = 1;
        if ($version > $VERSION) {
          print STDERR "Warning: found Dean::Util block of NEWER version as current version!\n";
        }
      }

      print $OUT $_ and next unless $inblock;
      next if $useline and !/^# END(?>\s+)Dean::Util code version/;

      if (!$useline and /^#  (use Dean::Util.*)/m) {
        $useline = $1;
        until ($useline =~ /;/) { $useline .= <$IN> }
        $useline =~ s/^#  //mg;
        print $OUT $useline, "\n";
        next;
      }

      if (/^# END(?>\s+)Dean::Util code version/) {
        # Finalize!
        $inblock = 0;
        undef $useline;
        next;
      }
    }
    close $IN;
    close $OUT;
  }
}
#END: remove_Dean_Util_functions


=head3 get_Dean_Util_function_string

Returns the source code for the functions provided as arguments. If the
argument list is empty, the function list is taken from C<@ARGV>.

The special symbols C<INCLUDE_POD> and C<POD_ONLY> may be used to indicate
that all further inclusions should include their POD documentation before
the code, or exclude the code and only output the POD documentation.
Example:

 get_Dean_Util_function_string qw/max min INCLUDE_POD join_multi map_pair/;

Would include the POD documentation for only L<join_multi|/join_multi> and
L<map_pair|/map_pair(&&@)>.

 get_Dean_Util_function_string qw/POD_ONLY format_cols/;

Would return just the POD documentation for L<format_cols|/format_cols>.

=cut

#BEGIN: get_Dean_Util_function_string, depends: get_Dean_Util_code, str, first_pos
sub get_Dean_Util_function_string {
  my %code  = (@_ and ref($_[0]) eq 'HASH') ? %{shift()} : %{ get_Dean_Util_code() };
  my @funcs = (@_) ? @_ : @ARGV;
  my ($include_pod, $out) = (0, '');

  # determine requested functions
  my @pod_only;
  @pod_only = splice @funcs, $_ if defined($_ = first_pos(sub { str($_) eq "POD_ONLY" }, @funcs));
  my %funcs    = map( ($_ => 1), @funcs );
  shift @pod_only; # the first entry is "POD_ONLY"

  # Calculate function dependencies
  my @temp;
  do {
    @temp = ();
    for my $f (@funcs) {
      push @temp, grep 1 == ++$funcs{$_}, @{$code{$f}{depends}} if $code{$f}{depends};
    }
    unshift @funcs, @temp;
  } while (@temp);

  # Print out the requested code and POD
  for (@funcs) {
    if ($_ eq "INCLUDE_POD") { $include_pod++; next }
    print STDERR "$_ has no POD, Dean::Util line ",  (str($code{$_}{line})||"???"), $/ if $include_pod && !$code{$_}{pod};
    print STDERR "$_ has no code, Dean::Util line ", (str($code{$_}{line})||"???"), $/ unless $code{$_}{code};
    $out .= $/ . str($code{$_}{pod}) . $/ if $include_pod;
    $out .= str($code{$_}{code});
  }
  for (@pod_only) {
    print STDERR "$_ has no POD, Dean::Util line ", (str($code{$_}{line})||"???"), $/ unless $code{$_}{pod};
    $out .= $/ . str($code{$_}{pod}) . $/;
  }
  $out = "use Carp;\n$out" if $out =~ /\b(?:croak|confess)\b/;
  return $out;
}
#END: get_Dean_Util_function_string


=head1 EXPORTABLE FUNCTIONS

=cut

#-----------------------------------------------------------------
                      $EXPORT_TAGS{numerical}
                                 =
[qw/max min fmax fmin minimizer maximizer ceil floor round unbiased_round
    sum product average min_max max_min pct_change

    max_dirty min_dirty fmax_dirty fmin_dirty minimizer_dirty maximizer_dirty
    ceil_dirty floor_dirty sum_dirty product_dirty average_dirty
    min_max_dirty max_min_dirty

    is_prime sieve_of_eratosthenes next_prime

    base_hash base2base base2decimal base2integer decimal2base factorial
    $pi $tau $e $sqrt2 rad2deg deg2rad/];
#-----------------------------------------------------------------

=head2 :numerical - Numerical Functions

=head3 $pi

See also: Math::Trig pi

The string, pi, to 30 digits after the decimal.

=cut

#BEGIN: $pi, 1 line
our $pi  = "3.141592653589793238462643383280";

=head3 $tau

See also: Math::Trig pi2

The string, tau = 2*$pi, to 30 digits after the decimal.

=cut

#BEGIN: $tau, 1 line
our $tau = "6.283185307179586476925286766559";

=head3 $e

The string, e, to 30 digits after the decimal.

=cut

#BEGIN: $e, 1 lines
our $e   = "2.718281828459045235360287471353";

=head3 $sqrt2

The string, sqrt(2), to 30 digits after the decimal.

=cut

#BEGIN: $sqrt2, 1 lines
our $sqrt2 = "1.414213562373095048801688724209";


=head3 rad2deg

See also: Math::Trig rad2deg

  my $deg = rad2deg( 0.32 )
  my $deg = 0.32 * rad2deg();

Convert radians to degrees

=cut

#BEGIN: rad2deg, 1 lines
sub rad2deg { return 180 * (shift // 1) / 3.141592653589793238462643383280 }

=head3 deg2rad

See also: Math::Trig deg2rad

  my $rad = deg2rad( 45 )
  my $rad = 45 * deg2rad();

Convert degrees to radians

=cut

#BEGIN: deg2rad, 1 lines
sub deg2rad { return 3.141592653589793238462643383280 * (shift // 1) / 180 }


=head3 max

See also: List::Util max

Return the maximum number in a list of values. All arguments must be
numeric, use L<max_dirty|/max_dirty> for untrusted or mixed data.

=cut

=head3 min

See also: List::Util min

Return the minimum number in a list of values. All arguments must be
numeric, use L<min_dirty|/min_dirty> for untrusted or mixed data.

=cut

#BEGIN: max, 1 line
sub max { my $x = shift; for (@_) { $x = $_ if $_ > $x } $x }
#BEGIN: min, 1 line
sub min { my $x = shift; for (@_) { $x = $_ if $_ < $x } $x }

=head3 max_dirty

Return the maximum number in a list of values. This version of max should
be used for untrusted data since undefined or non-numeric values are
silently ignored rather than trowing errors.

=cut

=head3 min_dirty

Return the minimum number in a list of values. This version of min should
be used for untrusted data since undefined or non-numeric values are
silently ignored rather than trowing errors.

=cut

#BEGIN: max_dirty, 2 lines; depends: is_num, strip_space
sub max_dirty { shift while @_ and !is_num($_[0]);
          my $x = shift; for (map(strip_space($_),@_)) { $x = $_ if is_num($_) and $_ > $x } $x }
#BEGIN: min_dirty, 2 lines; depends: is_num, strip_space
sub min_dirty { shift while @_ and !is_num($_[0]);
          my $x = shift; for (map(strip_space($_),@_)) { $x = $_ if is_num($_) and $_ < $x } $x }

=head3 fmax

 fmax { block } @list
 fmax \&sub, @list

Return the maximum function value given by evaluating the given code at
each element of the list. The code may be either a subroutine reference or
a code block. C<$_> will be set to each list entry and will also be passed
in as the first (and only) argument. If the code returns any undefined or
non-numeric values, perl will issue warnings.

=cut

=head3 fmin

 fmin { block } @list
 fmin \&sub, @list

Return the minimum function value given by evaluating the given code at
each element of the list. The code may be either a subroutine reference or
a code block. C<$_> will be set to each list entry and will also be passed
in as the first (and only) argument. If the code returns any undefined or
non-numeric values, perl will issue warnings.

=cut

#BEGIN: fmax, 1 line
sub fmax :prototype(&@) { local $_; my ($f,$x,$m) = shift; return unless @_; $x = $f->($_ = shift); for (@_) { $x=$m if $x < ($m=$f->($_)) } $x }
#BEGIN: fmin, 1 line
sub fmin :prototype(&@) { local $_; my ($f,$x,$m) = shift; return unless @_; $x = $f->($_ = shift); for (@_) { $x=$m if $x > ($m=$f->($_)) } $x }

=head3 fmax_dirty

 fmax_dirty { block } @list
 fmax_dirty \&sub, @list

Return the maximum function value given by evaluating the given code at
each element of the list. The code may be either a subroutine reference or
a code block. C<$_> will be set to each list entry and will also be passed
in as the first (and only) argument. If the code returns any undefined or
non-numeric values, they will be ignored.

=cut

=head3 fmin_dirty

 fmin_dirty { block } @list
 fmin_dirty \&sub, @list

Return the minimum function value given by evaluating the given code at
each element of the list. The code may be either a subroutine reference or
a code block. C<$_> will be set to each list entry and will also be passed
in as the first (and only) argument. If the code returns any undefined or
non-numeric values, they will be ignored.

=cut

#BEGIN: fmax_dirty, 2 lines; depends: is_num, strip_space
sub fmax_dirty :prototype(&@) { local $_; my ($f, $x, $m) = shift; 1 while @_ and !is_num($x = strip_space($f->($_ = shift)));
                     for (@_) { $x = $m if is_num($m = strip_space($f->($_))) and $m > $x } is_num($x) ? $x : undef }
#BEGIN: fmin_dirty, 2 lines; depends: is_num, strip_space
sub fmin_dirty :prototype(&@) { local $_; my ($f, $x, $m) = shift; 1 while @_ and !is_num($x = strip_space($f->($_ = shift)));
                     for (@_) { $x = $m if is_num($m = strip_space($f->($_))) and $m < $x } is_num($x) ? $x : undef }

=head3 minimizer

 minimizer { block } @list
 minimizer \&sub, @list

Return the item of C<@list> which yields the minimum value when evaluated
by the given code. The code may be provided either as a subroutine
reference or a code block. C<$_> will be set to each list entry and will
also be passed in as the first (and only) argument. If the code returns any
undefined or non-numeric values, perl will issue warnings.

=cut

=head3 maximizer

 maximizer { block } @list
 maximizer \&sub, @list

Return the item of C<@list> which yields the maximum value when evaluated
by the given code. The code may be provided either as a subroutine
reference or a code block. C<$_> will be set to each list entry and will
also be passed in as the first (and only) argument. If the code returns any
undefined or non-numeric values, perl will issue warnings.

=cut

#BEGIN: maximizer, 2 lines
sub maximizer :prototype(&@) { local $_; my ($f,$x,$m,$y) = shift; return unless @_; $x = $f->($y = $_ = shift);
                    for (@_) { if ($x < ($m = $f->($_))) { $x=$m; $y=$_ } } $y }
#BEGIN: minimizer, 2 lines
sub minimizer :prototype(&@) { local $_; my ($f,$x,$m,$y) = shift; return unless @_; $x = $f->($y = $_ = shift);
                    for (@_) { if ($x > ($m = $f->($_))) { $x=$m; $y=$_ } } $y }

=head3 minimizer_dirty

 minimizer_dirty { block } @list
 minimizer_dirty \&sub, @list

Return the item of C<@list> which yields the minimum value when evaluated
by the code. C<code> may be either a subroutine reference or a code block.
C<$_> will be set to each list entry and will also be passed in as the
first (and only) argument. If the code returns any undefined or non-numeric
values, they will be ignored and the corresponding list item will not be
considered as a minimizer.

Note however that no filtering is performed on C<@list> so undefined values
I<will> be passed to the subroutine as a normal element.

=cut

=head3 maximizer_dirty

 maximizer_dirty { block } @list
 maximizer_dirty \&sub, @list

Return the item of C<@list> which yields the maximum value when evaluated
by the code. C<code> may be either a subroutine reference or a code block.
C<$_> will be set to each list entry and will also be passed in as the
first (and only) argument. If the code returns any undefined or non-numeric
values, they will be ignored and the corresponding list item will not be
considered as a minimizer.

Note however that no filtering is performed on C<@list> so undefined values
I<will> be passed to the subroutine as a normal element.

=cut

#BEGIN: maximizer_dirty, 2 lines; depends: is_num, strip_space
sub maximizer_dirty :prototype(&@) { local $_; my ($f, $x, $m, $y) = shift; 1 while @_ and !is_num($x = strip_space($f->($y = $_ = shift)));
                    for(@_){ if (is_num($m = strip_space($f->($_))) and $m > $x) {$x=$m;$y=$_} } is_num($x) ? $y : undef }
#BEGIN: minimizer_dirty, 2 lines; depends: is_num, strip_space
sub minimizer_dirty :prototype(&@) { local $_; my ($f, $x, $m, $y) = shift; 1 while @_ and !is_num($x = strip_space($f->($y = $_ = shift)));
                    for(@_){ if (is_num($m = strip_space($f->($_))) and $m < $x) {$x=$m;$y=$_} } is_num($x) ? $y : undef }

=head3 ceil($)

If the argument is numeric, then returns the smallest integer which is
greater than or equal to the given argument. Otherwise this function will
spew warnings.

See also: POSIX::ceil   [identical functionality]

=cut

#BEGIN: ceil, 1 line
sub ceil :prototype($) { my $x = shift; return ($x == int $x) ? $x : ($x > 0) ? int( $x+1 ) : int($x) }

=head3 ceil_dirty($)

If the argument is numeric, then returns the smallest integer which is
greater than or equal to the given argument. Otherwise this function will
return undef.

=cut

#BEGIN: ceil_dirty, 2 line; depends: is_num, strip_space
sub ceil_dirty :prototype($) { my $x = strip_space(shift()); return unless defined $x and is_num($x);
                    return ($x == int $x) ? $x : ($x > 0) ? int( $x+1 ) : int($x) }

=head3 floor($)

If the argument is numeric, then returns the largest integer which is less
than or equal to the given argument. Otherwise this function spews
warnings.

See also: POSIX::floor   [identical functionality]

=cut

#BEGIN: floor, 1 line
sub floor :prototype($) { my $x = shift; return ($x == int $x) ? $x : ($x > 0) ? int($x) : int($x - 1) }

=head3 floor_dirty($)

If the argument is numeric, then returns the largest integer which is less
than or equal to the given argument. Otherwise this function returns undef.

=cut

#BEGIN: floor_dirty, 2 line; depends: is_num, strip_space
sub floor_dirty :prototype($) { my $x = strip_space(shift()); return unless defined $x and is_num($x);
                     return ($x == int $x) ? $x : ($x > 0) ? int($x) : int($x - 1) }

=head3 round

 round( $value )          # round to integer
 round( $value, 2 )       # round to even
 round( $value, "0.01" )  # round to cent

Round C<$value> to multiple of second parameter. Applies traditional
algorithm. Namely, C<round( $value ) == int($value + .5)>.

Internal comparisons are performed at "string precision" to combat
numerical precision problems. Thus, do not expect to to be able to round to
too many digits.

=cut

#BEGIN: round
sub round {
  my ($value, $r) = @_;
  unless ($r) { # shortcut the common case
      return int($value - .5) if $value < 0;
      return int($value + .5);
  }

  $r = "$r";
  $r =~ s/0+$// if index($r,'.') >= 0;
  my $pow = ($r =~ s/\.(.*)/$1/) ? length($1) : 0;
  my $sign = ($value < 0) ? "-" : "";
  $value = abs($value) * 10 ** $pow;
  # some integer math
  my $lower = int($value) - (int($value) % $r);
  my $upper = $lower + $r;

  my $new = ("$value" - $lower < $upper - "$value") ? $lower : $upper;
  $sign = "" if $new == 0;

  # shift decimal on the string for precision
  if ($pow) {
    if (length($new) > $pow) { substr( $new, length($new)-$pow, 0, '.' ) }
    else { $new = "0.".("0"x($pow-length($new)))."$new"; }
  }

  return "$sign$new";
}
#END: round

=head3 unbiased_round

 unbiased_round( $value )          # round to integer
 unbiased_round( $value, 2 )       # round to even
 unbiased_round( $value, "0.01" )  # round to cent

An unbiased round removes the upward bias of the traditional rounding
algorithm by rounding the midpoint value up sometimes and down other times.
The convention is to round midpoint values to even multiples, and round all
other values normally.

For example, C<unbiased_round( 2.5 ) == 2> since 2 is even, however
C<unbiased_round( 1.5 ) == 2> as well since 2 is even.

This system can be extended to the generalized rounding algorithm:

 unbiased_round( 1, 2 ) == 0   # since 0 is an even multiple of 2
 unbiased_round( 3, 2 ) == 4   # since 4 is an even multiple of 2

=cut

#BEGIN: unbiased_round
sub unbiased_round {
  my ($value, $r) = @_;
  unless ($r) { # shortcut the common case
    my $v = int($value + .5);
    return ($value =~ /\.5$/ and $v % 2) ? $v - 1 : $v;
  }

  $r = "$r";
  $r =~ s/0+$// if index($r,'.') >= 0;
  my $pow = ($r =~ s/\.(.*)/$1/) ? length($1) : 0;
  my $sign = ($value < 0) ? "-" : "";
  $value = abs($value) * 10 ** $pow;
  # some integer math
  my $lower = int($value) - (int($value) % $r);
  my $upper = $lower + $r;

  my $new = ("$value" - $lower < $upper - "$value") ? $lower
          : ("$value" - $lower > $upper - "$value") ? $upper
          : (($lower / $r) % 2) ? $upper : $lower;
  $sign = "" if $new == 0;

  # shift decimal on the string for precision
  if ($pow) {
    if (length($new) > $pow) { substr( $new, length($new)-$pow, 0, '.' ) }
    else { $new = "0.".("0"x($pow-length($new)))."$new"; }
  }

  return "$sign$new";
}
#END: unbiased_round


=head3 sum

See also: List::Util sum

Returns the sum of all numeric entries in a list. Undefined/non-numeric values cause warnings.

Unlike sum from List::Util, returns 0 if argument list is empty.

=cut

=head3 product

See also: List::Util reduce

Returns the product of all numeric entries in a list. Undefined/non-numeric values cause warnings.

Returns 1 if argument list is empty.

=cut

=head3 average

Returns the average over all entries in a list. Undefined or non-numeric
entries will spew warnings.

Returns undef if argument list is empty.

Uses an appropriately stable algorithm: http://www.nu42.com/2015/03/how-you-average-numbers.html

=cut

#BEGIN: sum, 1 line
sub sum { my $x=0; $x += $_ for @_; $x}
#BEGIN: product, 1 line
sub product { my $x=1; $x *= $_ for @_; $x}

#BEGIN: average
sub average {
    return unless @_;
    my $x = $_[0];
    for my $i (1 .. $#_) {
        $x += ($_[$i] - $x) / ($i + 1);
    }
    $x;
}
#END: average


=head3 sum_dirty

Returns the sum of all numeric entries in a list. Undefined/non-numeric values are ignored.

Returns 0 if argument list is empty.

=cut

=head3 product_dirty

Returns the product of all numeric entries in a list. Undefined/non-numeric values are ignored.

Returns 1 if argument list is empty.

=cut

=head3 average_dirty

Returns the average over all entries in a list. Undefined or non-numeric
entries contribute a 0 to the average.

Returns undef if argument list is empty.

=cut

#BEGIN: sum_dirty, 1 line; depends: is_num, strip_space
sub sum_dirty { my $x=0; is_num() && ($x += strip_space($_)) for @_; $x}
#BEGIN: product_dirty, 1 line; depends: is_num, strip_space
sub product_dirty { my $x=1; is_num() && ($x *= strip_space($_)) for @_; $x}
#BEGIN: average_dirty, 1 line; depends: sum_dirty
sub average_dirty { @_ ? sum_dirty(@_)/@_ : undef }


=head3 min_max

Returns a pair C<($m, $M)> which is the minimum and maximum numbers,
respectively, in a list of values without looping over the list twice.
Undefined or non-numeric values will cause warnings.

=cut

=head3 max_min

Returns a pair C<($M, $m)> which is the maximum and minimum numbers,
respectively, in a list of values without looping over the list twice.
Undefined or non-numeric values will cause warnings.

=cut

#BEGIN: min_max, 1 line; depends: max_min
sub min_max { return reverse max_min(@_) }
#BEGIN: max_min
sub max_min {
  my $M = shift; my $m = $M;
  for (@_) { $M = $_ if $_ > $M;
             $m = $_ if $_ < $m; }
  return( $M, $m );
}
#END: max_min

=head3 min_max_dirty

Returns a pair C<($m, $M)> which is the minimum and maximum numbers,
respectively, in a list of values without looping over the list twice.
Undefined or non-numeric values are silently ignored.

=cut

=head3 max_min_dirty

Returns a pair C<($M, $m)> which is the maximum and minimum numbers,
respectively, in a list of values without looping over the list twice.
Undefined or non-numeric values are silently ignored.

=cut

#BEGIN: min_max_dirty, 1 line; depends: max_min_dirty
sub min_max_dirty { return reverse max_min_dirty(@_) }

#BEGIN: max_min_dirty; depends is_num, strip_space
sub max_min_dirty {
  shift while @_ and !is_num($_[0]);
  my $M = strip_space(shift); my $m = $M;
  for (map strip_space($_), @_) {
    next unless is_num();
    $M = $_ if $_ > $M;
    $m = $_ if $_ < $m;
  }
  return( $M, $m );
}
#END: max_min_dirty

=head3 sieve_of_eratosthenes

 my $sieve = sieve_of_eratosthenes( $n );
 sieve_of_eratosthenes( $m, $sieve );

Constructs a bit string C<$sieve> using the Sieve of Eratosthenes so that:

 vec($sieve, $n, 1) == 1   iff   $n is prime

If a sieve (or an undefined scalar) is provided as a second argument, it
will be appended to.

Note: Since perl's C<length> command deals only in bytes, this subroutine
will round C<$n> up to make sure that C<$sieve> is correct to a whole
number of bytes. In particular, you are guaranteed to be able to trust
C<$sieve> up to C<$n = 8 * length($sieve) - 1>.

=cut

#BEGIN: sieve_of_eratosthenes
sub sieve_of_eratosthenes {
  my ($n, $sieve) = @_;
  # "length" rounds to the neqarest byte, so we make sure that our sieve is accurate to the byte.
  $n     = ($n % 8 == 7) ? $n : 8 * int($n/8) + 7;
  $_[1] = '' unless defined $_[1];

  my $a = length($_[1]) ? 8*length($_[1]) : 2;
  return $_[1] if $n <= $a; # good enough

  # expand sieve
  # DeanUtil '$x = chr(255)x3; say vec($x, $_, 1) for 0..23;'
  # if ($_[1]) { append } else { $_[1] = chr(252) . ( chr(255)x(int($n/8)) ) } # untested
  vec($_[1], $_, 1) = 1 for $a..$n;

  # useful values
  my $q = int(sqrt($n));
  my $i = 2;
  my $k;

  # print "Computing sieve from '$a' to '$n' sieve = ", unpack("b*", $_[1]), "\n";
  while ($i <= $q) {
    next unless vec($_[1], $i, 1);
    vec($_[1], $_ * $i, 1) = 0 for ((($k = int($a/$i)) > 2) ? $k : 2)..int($n/$i);
  } continue { $i++ }

  return $_[1];
}
#END: sieve_of_eratosthenes


=head3 is_prime

Determine primality. Constructs the Sieve of Eratosthenes to determine
primality. The sieve is reused for each call to C<is_prime> so scripts are
encouraged to prepare the sieve by calling is_prime on a large number
before making multiple calls to is_prime.

 # SLOW: takes 21.89 seconds
 @primes = grep is_prime($_), 1..400000;

 # FAST: takes 1.387 seconds
 @primes = reverse grep is_prime($_), reverse 1..400000;

This function may take some shortcuts if it can so if you want to prepare
the sieve append the option "force_sieve",

 # SLOW:
 is_prime( 400000 ); # this test shortcuts since 400000 is even
 @primes = grep is_prime($_), 1..400000;

 # FAST:
 is_prime( 400000, force_sieve => 1 );
 @primes = grep is_prime($_), 1..400000;

=cut

# Table of small primes: http://primes.utm.edu/lists/small/10000.txt
#
#

# Probabalistic Fermat Primality Test is probably fastest of these; GAP
# method next fastest; My method is SLOW and requires lots of memory :(
#
# Note: Probabalistic Fermat Primality Test is O(k × log^2 n × log log n × log log log n)
#       Where n is the BIT LENGTH of the number. and "guarantees" primeness
#       with probability (1/2)^k [k = 100 makes the probability of failure
#       at most 2^-100 , which is miniscule: far less, for instance, than
#       the probability that a random cosmic ray will sabotage the computer
#       during the computation!
#
#
#F  IsProbablyPrimeInt( <n> ) . . . . . . . . . . . . . . .  test for a prime
##
##  `IsPrimeInt' returns `false'  if it can  prove that <n>  is composite and
##  `true' otherwise.
##  By  convention `IsPrimeInt(0) = IsPrimeInt(1) = false'
##  and we define `IsPrimeInt( -<n> ) = IsPrimeInt( <n> )'.
##
##  `IsPrimeInt' will return  `true' for every prime $n$.  `IsPrimeInt'  will
##  return `false' for all composite $n \< 10^{13}$ and for all composite $n$
##  that have   a factor  $p \<  1000$.   So for  integers $n    \< 10^{13}$,
##  `IsPrimeInt' is  a    proper primality test.    It  is  conceivable  that
##  `IsPrimeInt' may  return `true' for some  composite $n > 10^{13}$, but no
##  such $n$ is currently known.  So for integers $n > 10^{13}$, `IsPrimeInt'
##  is a  probable-primality test. `IsPrimeInt' will issue a
##  warning when its argument is probably prime but not a proven prime.
##  (The function `IsProbablyPrimeInt' will do the same calculations but not
##  issue a warning.) The warning can be switched off by
##  `SetInfoLevel( InfoPrimeInt, 0 );', the default level is $1$.
##
##  If composites that  fool `IsPrimeInt' do exist, they  would be extremely
##  rare, and finding one by pure chance might be less likely than finding a
##  bug in {\GAP}. We would appreciate being informed about any example of a
##  composite number <n> for which `IsPrimeInt' returns `true'.
##
##  `IsPrimeInt' is a deterministic algorithm, i.e., the computations involve
##  no random numbers, and repeated calls will always return the same result.
##  `IsPrimeInt' first   does trial divisions  by the  primes less than 1000.
##  Then it tests  that  $n$  is a   strong  pseudoprime w.r.t. the base   2.
##  Finally it  tests whether $n$ is  a Lucas pseudoprime w.r.t. the smallest
##  quadratic nonresidue of  $n$.  A better  description can be found in  the
##  comment in the library file `integer.gi'.
##
##  The time taken by `IsPrimeInt' is approximately proportional to the third
##  power  of  the number  of  digits of <n>.   Testing numbers  with several
##  hundreds digits is quite feasible.
#
# BindGlobal( "IsProbablyPrimeIntWithFail", function( n )
#     local  p, e, o, x, i;
#
#     # make $n$ positive and handle trivial cases
#     if n < 0         then n := -n;       fi;
#     if n in Primes   then return true;   fi;
#     if n in Primes2  then return true;   fi;
#     if n in ProbablePrimes2  then return fail;   fi;
#     if n <= 1000     then return false;  fi;
#
#     # do trial divisions by the primes less than 1000
#     # faster than anything fancier because $n$ mod <small int> is very fast
#     for p  in Primes  do
#         if n mod p = 0  then return false;  fi;
#         if n < (p+1)^2  then AddSet( Primes2, n );  return true;   fi;
#     od;
#
#     # do trial division by the other known primes
#     for p  in Primes2  do
#         if n mod p = 0  then return false;  fi;
#     od;
#     # do trial division by the other known probable primes
#     for p  in ProbablePrimes2  do
#         if n mod p = 0  then return false;  fi;
#     od;
#
#     # find $e$ and $o$ odd such that $n-1 = 2^e * o$
#     e := 0;  o := n-1;   while o mod 2 = 0  do e := e+1;  o := o/2;  od;
#
#     # look at the seq $2^o, 2^{2 o}, 2^{4 o}, .., 2^{2^e o}=2^{n-1}$
#     x := PowerModInt( 2, o, n );
#     i := 0;
#     while i < e  and x <> 1  and x <> n-1  do
#         x := x * x mod n;
#         i := i + 1;
#     od;
#
#     # if it is not of the form $.., -1, 1, 1, ..$ then $n$ is composite
#     if not (x = n-1 or (i = 0 and x = 1))  then
#         return false;
#     fi;
#
#     # make sure that $n$ is not a perfect power (especially not a square)
#     if SmallestRootInt(n) < n  then
#         return false;
#     fi;
#
#     # find a quadratic nonresidue $d = p^2/4-1$ mod $n$
#     p := 2;  while Jacobi( p^2-4, n ) <> -1  do p := p+1;  od;
#
#     # for a prime $n$ the trace of $(p/2+\sqrt{d})^n$ must be $p$
#     # and the trace of $(p/2+\sqrt{d})^{n+1}$ must be 2
#     if TraceModQF( p, n+1, n ) = [ 2, p ]  then
#         # n < 10^13 fulfilling the tests so far are prime
#         if n < 10^13 then
#           return true;
#         else
#           return fail;
#         fi;
#     fi;
#
#     # $n$ is not a prime
#     return false;
# end);


#BEGIN: is_prime, Depends: sieve_of_eratosthenes
sub is_prime {
    state $sieve = "";
    return $sieve unless @_;
    my ($n, %o) = @_;
    return vec($sieve, $n, 1) == 1 if $n < 8*length($sieve);
    unless ($o{force_sieve}) { (0 == ($n % $_)) and return $n == $_ for 2,3,5,7,11; }
    sieve_of_eratosthenes($n, $sieve);
    return vec($sieve, $n, 1) == 1;
}
#END: is_prime

=head3 next_prime

 my $m = next_prime( $n )

Compute the next prime integer larger than C<$n>.

=cut

#BEGIN: next_prime, Depends: is_prime
# Algorithm taken from GAP's NextPrimeInt function
sub next_prime {
  my ($n) = @_;
  if    ($n < 2)      { return 2 }
  elsif ($n % 2 == 0) { $n++ }
  else                { $n += 2 }

  # jump start since building up from low numbers is so slow
  is_prime(10_000 + $n, force_sieve => 1) unless $n+1000 < 8*length(is_prime()) - 1;

  $n += ($n % 6 == 1) ? 4 : 2 until is_prime($n);
  return $n;
}
#END: next_prime


=head3 base_hash

Given a base, this function returns a hash which may be used in future
calls to the other base functions.

A base is described by:

 integer <= 36 (0-9 a-z)
 array ref     (list of symbols, length == base, index i == i, yes you get to define zero)
 string        (string of symbols, shortcut for [split //, $str]
 hash ref      (the output of a previous call to base_hash, this is silly in this case)

=cut

=head3 base2base

 base2base( string, base, base )

String may be decimal. The following symbols are tried (in order) to be
used as the punctuation between the integer and fraction part of the
number:

 . , : ; _ | / \ - + ' ` "

Bases are described by:

 integer <= 36 (0-9 a-z)
 array ref     (list of symbols, length == base, index i == i, yes you get to define zero)
 string        (string of symbols, shortcut for [split //, $str]
 hash ref      (the output of base_hash)

=cut

=head3 base2integer

 base2integer( string, base )

Convert a string to another base. The string may not be a decimal.

Base is described by:

 integer <= 36 (0-9 a-z)
 array ref     (list of symbols, length == base, index i == i, yes you get to define zero)
 string        (string of symbols, shortcut for [split //, $str]
 hash ref      (the output of base_hash or symbol => value pairs)

=cut

=head3 base2decimal

 base2decimal( string, base )

String may be decimal. The following symbols are tried (in order) to be
used as the punctuation between the integer and fraction part of the
number:

 . , : ; _ | / \ - + ' ` "

Base is described by:

 integer <= 36 (0-9 a-z)
 array ref     (list of symbols, length == base, index i == i, yes you get to define zero)
 string        (string of symbols, shortcut for [split //, $str]
 hash ref      (the output of base_hash)

=cut

=head3 decimal2base

 decimal2base( string, base )

String may be decimal. The following symbols are tried (in order) to be
used as the punctuation between the integer and fraction part of the
number:

 . , : ; _ | / \ - + ' ` "

Base is described by:

 integer <= 36 (0-9 a-z)
 array ref     (list of symbols, length == base, index i == i, yes you get to define zero)
 string        (string of symbols, shortcut for [split //, $str]
 hash ref      (the output of base_hash)

=cut

#BEGIN: base_hash
sub base_hash {
  local $_ = shift; my (@x,%x);
  if    (ref($_) =~ /HASH/)             { return $_ }
  elsif (ref($_) =~ /ARRAY/)            { @x = @{ $_ } }
  elsif (/^0/ or length > 2 or $_ > 36) { @x = split // }
  else                                  { @x = (0..9,'a'..'z')[0..$_-1] }
  $x{$x[$_]} = $_ for 0..$#x;
  $x{base} = @x;
  return \%x;
}
#END: base_hash

#BEGIN: base2base, 1 line; depends decimal2base, base2decimal
sub base2base { decimal2base(base2decimal($_[0],$_[1]), $_[2]) }


#BEGIN: base2integer, depends: base_hash
sub base2integer {
  local $_ = shift;
  return unless @_ and defined and defined $_[0];
  my $hash = base_hash($_[0]);
  my $base = (exists $hash->{base}) ? $hash->{base} : 0+(keys %$hash);

  my $num = 0;
  $num = ($num * $base) + ($hash->{$_}) for /./g;
  return $num;
}
#END: base2integer


#BEGIN: base2decimal, depends: base_hash
sub base2decimal {
  my $n = shift;
  return unless @_ and defined $n and defined $_[0];
  my $base = base_hash($_[0]);
  my ($int,$dec);

  $n =~ /(?:\.|\,|\:|\;|\_|\||\/|\\|\-|\+|\'|\`|\")/ and do {
    # separate "." and "," to avoid "Possible attempt to separate words with commas" warning
    for (".", ",", qw/: ; _ | \/ \\ - + ' ` "/) {
      next if exists $base->{$_} or not $n =~ /\Q$_\E/;
      ($int, $dec) = split /\Q$_\E/, $n;
      $dec = '' unless defined $dec;
    }};
  ($int,$dec) = ($n, '') unless defined $int;

  my $p = 1; $n = 0;
  for (reverse split //, $int) { $n += $p * $base->{$_}; $p *= $base->{base}; }
  $p = 1/$base->{base};
  for (split //, $dec)         { $n += $p * $base->{$_}; $p /= $base->{base}; }
  return $n;
}
#END: base2decimal

#BEGIN: decimal2base, depends: base_hash
sub decimal2base {
  my $n = shift;
  my $precision = (@_ > 1) ? pop : 0.0000000000001;
  return unless @_ and defined $n and defined $_[0];
  my %base = %{base_hash($_[0])};
  return $base{0} if $n == 0;
  my ($int,$comma,$base,$res,$p,$inv_base);
  { no warnings;
    for (qw/. , : ; _ | \/ \\ - + ' ` "/) { unless (exists $base{$_}) { $comma = $_; last } }
  }
  $base = delete $base{base};
  %base = reverse %base;

  $res = ''; $inv_base = 1/$base;
  $p = $base ** int(log($n)/log($base));
  while ($n and $p > $precision) {
    $res .= $comma if $p == $inv_base;
    $res .= $base{$int = int($n/$p)};
    $n -= $p * $int;
    $p *= $inv_base;
  }
  while ($p > $inv_base) { $res .= $base{0}; $p /= $base }
  return $res;
}
#END: decimal2base

=head3 factorial

 factorial( $n )

Returns $n! if $n is a non-negative integer.

=cut

#BEGIN: factorial
sub factorial {
  return if not defined $_[0] or $_[0] =~ /\D/;
  return 1 if $_[0] < 2; my $n = 1; $n *= $_ for 2..$_[0]; return $n;
}
#END: factorial

=head3 pct_change

 pct_change( $orig, $new )

Simply returns the percent change between the two values
C<($new-$orig)/$orig>. Exists solely because I don't like how the formula
looks in a line of real code.

=cut

#BEGIN: pct_change; 1 line
sub pct_change { my ($a,$b) = @_; ($b-$a)/$a }


#-----------------------------------------------------------------
                      $EXPORT_TAGS{ai}
                                 =
[qw/ perceptron /];
#-----------------------------------------------------------------

=head3 perceptron

See: https://en.wikipedia.org/wiki/Perceptron

Simple linear classifier. Requires linearly-separable classes. Sensitive to
training data - if training data are too easily distinguished, resulting
classifier will not be reliable. However, can easily be further trained as
new data arrives.

 my $p = perceptron($n); # n is number of inputs
 my $p = perceptron($n, \@weights); # need exactly $n+1 weights

 $p->arity;   # $n
 $p->bias;    # affine portion of linear model
 $p->weights; # weight vector (including bias as last compinent)
 say "@$p";   # overload: same as weights

 $p->accept(\@in);
 $p->reject(\@in);
 $p->learn($ar, \@in);# $ar is 1 or 0

 $p->accepts(\@in); # 1 or 0
 $p->rejects(\@in); # 1 or 0
 $p->compute(\@in); # signed percentage confidence (positive for accepts)
 $p->(\@in);        # overload: same as compute

 # Each argument is array of arrays, return value is percentage error after
 # training (percent of inputs incorrectly classified after training complete).
 my $error = $p->train(\@accepts, \@rejects);

=cut

#BEGIN: perceptron
package Perceptron {
    use overload (
        '&{}' => 'compute',
        '@{}' => 'compute',
    );

    sub new {
        my $class = shift;
        my %opt = @_;
        # Weights is list of weights and last element is a "bias".
        # (our model is an affine plane in the n+1 dimensional space)
        $opt{weights} //= [ (0) x (1 + $opt{n}) ];
        my $self = bless { %opt }, $class;
        return $self;
    }

    sub arity   { shift->{'n'} }
    sub bias    { shift->{'weights'}->[-1] }
    sub weights { @{shift->{'weights'}} }

    sub accept { shift->learn(1, @_) }
    sub reject { shift->learn(0, @_) }
    sub learn {
        my ($self, $d, $x) = @_;
        my $y = $self->compute($x);
        $$self{weights}[-1] += ($d - $y);
        $$self{weights}[$_] += ($d - $y) * $$x[$_] for 0..($#{$$self{weights}} - 1);
        return $self;
    }

    sub accepts { (shift->compute(@_) > 0) ? 1 : 0 }
    sub rejects { (shift->compute(@_) > 0) ? 0 : 1 }
    sub compute {
        my ($self, $x) = @_;
        my $rv = $$self{weights}[-1];
        $rv += $$self{weights}[$_] * $$x[$_] for 0..($#{$$self{weights}} - 1);
        return $rv / $#{$$self{weights}};# Pretty sure this is right (n not n+1 - I.e., exclude bias)
    }

    sub train {
        my ($self, $accept, $reject) = @_;
        $self->accept($_) for @$accept;
        $self->reject($_) for @$reject;
        return unless defined(wantarray);# save some work if possible

        my $error = 0;
        $error += $self->rejects($_) for @$accept;# only error if rejected
        $error += $self->accepts($_) for @$reject;# only error if accepted
        return $error / (@$accept + @$reject);
    }
}

sub perceptron {
    my ($n, $weights) = @_;
    return Perceptron->new(n => $n, ($weights ? (weights => $weights) : ()));
}
#END: perceptron


#-----------------------------------------------------------------
                      $EXPORT_TAGS{stat_prob}
                                 =
[qw/roll_dice randomize one_var percentile npdf ncdf
    permutations k_arrangements arrangements k_combinations combinations
    correlation prob_model_invariants random_binomial pascals_triangle
    /];
#-----------------------------------------------------------------

=head2 :stat_prob - Statistical / Probability

=head3 pascals_triangle

 pascals_triangle( Int $n )

Return nth row of pascal's triangle (starting at 0).

=cut

#BEGIN: pascals_triangle
sub pascals_triangle {
    state $pascal = [ map [split//], qw/ 1 11 121 1331 14641 / ];
    my $n = shift;
    return $$pascal[$n] if $n < @$pascal;
    my @p = @{$$pascal[-1]};
    while (@p <= $n) {
      @p = (1, map($p[$_]+$p[$_+1], 0..$#p-1), 1);
      $$pascal[@p-1] = [ @p ];
    }
    return $$pascal[$n];
}
#END: pascals_triangle


=head3 random_binomial

 random_binomial( Int $n )

Return random integer from 0 to n (inclusive) following a binomial
distribution. This is only useful up to C<n == 8 * $Config{intsize} - 1>

=cut

#BEGIN: random_binomial, DEPENDS: pascals_triangle
{ my @binomial;
  use Config;
  sub random_binomial {
    my $n = shift;
    unless ($n < 8*$Config{intsize}) {
      die "random_binomial: Sorry, this sub is stupid and can only handle n < @{[8*$Config{intsize}]}";
    }
    unless ($binomial[$n]) {
      my $d = 1<<$n;
      my @pas = @{ pascals_triangle($n) };
      pop @pas;
      $pas[$_] = $pas[$_]+$pas[$_-1] for 1..$#pas;
      $binomial[$n] = [ map $_/$d, @pas ];
    }
    my $r = rand;
    my $b = $binomial[$n];
    for (0..$n-1) {
      return $_ if $r < $$b[$_];
    }
    return $n;
  }
}
#END: random_binomial


=head3 prob_model_invariants

 prob_model_invariants( \%model, %options )

The model is a hash with keys the outcomes and values the corresponding
probabilities. At most one of the probabilities may be undefined in which
case it will be computed automatically (as $1 - \sum p_i$) and added to
your passed probability model.

=cut

#BEGIN: prob_model_invariants
sub prob_model_invariants {
  my $model = shift;
  my $missing_key;
  my ($x, $mu, $px2, $sd, $P);
  for $x (keys %$model) {
    if (not defined $$model{$x}) {
      croak "both P($missing_key) and P($x) are undefined when at most one may be missing" if defined $missing_key;
      $missing_key = $x;
      next;
    }
    $P   += $$model{$x};
    $mu  += $x * $$model{$x};
    $px2 += $x * $x * $$model{$x};
  }

  if (defined $missing_key) {
    $x = $missing_key;
    $$model{$x} = 1 - $P;
    $P   += $$model{$x};# Had better give us 1
    $mu  += $x * $$model{$x};
    $px2 += $x * $x * $$model{$x};
  }

  carp "Warning: Probabilities do not "."sum to 1 (\\"."sum p_i = $P)" unless 1 == sprintf("%.10f",$P);
  return { mean => $mu, sd => sqrt($px2 - $mu * $mu) };
}
#END: prob_model_invariants


=head3 roll_dice

Roll I<n> dice (default 1) and return the results. In scalar context, only
the sum is returned. In list context, the individual rolls are returned as
well as the final sum of the values (the sum is returned in the last
position).

=cut

#BEGIN: roll_dice
sub roll_dice {
  my ($s, %o, @R) = (0);
  @_ = ($_[0]."d6") if @_ == 1 and $_[0] =~ /^\d+$/;
  @_ = ("1d6")      unless @_;

  for (@_) {
    my ($r, $n) = /^(\d+)d(\d+)$/ ? ($1,$2) : /^d(\d+)$/ ? (1,$1) : croak "unable to parse die: '$_'";
    for (1..$r) {
      push @R, 1+int(rand($n));
      $s += $R[-1]
    }
  }
  wantarray ? (@R, $s) : $s
}
#END: roll_dice

=head3 randomize

See also: List::Util shuffle

Randomize a list of values. Essentially the Fisher-Yates shuffle code from
L<perlfaq4|perlfaq4> ("How do I shuffle an array randomly?"). If the array
is passed by reference then it will be altered, otherwise a copy is made.
Returns a new list or a reference to a list depending on context.

=cut

#BEGIN: randomize
sub randomize {
  my $x = (@_ == 1 and ref($_[0]) eq 'ARRAY') ? shift() : \@_;
  my $i;
  for ($i = @$x; --$i; ) {
    my $j = int rand ($i+1);
    @$x[$i,$j] = @$x[$j,$i];
  }
  return wantarray ? @$x : $x;
}
#END: randomize


=head3 one_var

 one_var( @data );
 one_var( \@data );
 one_var( \@data, $sorted );

Returns a hash (or hash reference if called in scalar context) of
one-variable statistics on the input data. If the C<$sorted> parameter is
not defined then the data is assumed to be not sorted and the subroutine
will make its own sorted copy of the data. If the C<$sorted> parameter is
defined but false, then the subroutine will sort C<@data> in place
(C<@data> will be altered). If the C<$sorted> parameter is true then the
data will be assumed to be already sorted. The returned hash will have the
following keys:

=over 4

=item average

=item mean

=item x-bar

The average value of the data

=item sum

=item sum x

The summation of the data

=item sum_sq

=item sum x^2

The sum of the squares of the data

=item Svar

=item sample_variance

The sample variance, C<1/n-1 * sum (x_i - average)^2>

=item Sx

=item sample_standard_deviation

The sample standard deviation, C<sqrt( Svar )>

=item variance

=item sigma_sq

The population variance, C<E( (X - E(X))^2 )>

=item sigma

=item standard_deviation

The population standard deviation, C<sqrt( variance )>

=item se

=item standard_error

The standard error of the mean, for computing confidence intervals

=item n

The number of measurements in the sample

=item min

The smallest data element

=item max

The smallest data element

=item Q1

The first quartile computed using broken "Basic Math Course Method".

=item Q2

=item med

=item median

The sample median

=item Q3

The third quartile computed using broken "Basic Math Course Method".

=item char:sum

=item char:Sigma

=item char:sigma

The corresponding Unicode characters: "\x{2211}", "\x{03A3}", "\x{03C3}".
Be warned that char:sum is a different symbol than char:Sigma and that the
terminal that you are writing to will need to understand UTF-8 font
encoding.

=back

Note: the list only needs to be sorted to compute the quartiles, min,
median, and max values. If you are not interested in these values then you
can speed up the computation by providing C<$sorted> with a true valued
(regardless of whether the data is sorted) and simply ignore those values
in the output.

=cut

#BEGIN: one_var, depends: sum, percentile
sub one_var {
  my ($data, %s);
  return unless @_;
  if (ref $_[0] eq 'ARRAY') {
    $data = shift;
    if (@_) { @$data = sort {$a<=>$b} @$data unless $_[0] }
    else    { $data = [sort {$a<=>$b} @$data] }
  }
  else { $data = [sort {$a<=>$b} @_] }
  return unless @$data;

  # Need this before we can compute sample variance, so we do it separately:
  my $n = $s{n} = @$data;
  $s{sum} = $s{"sum x"} = sum(@$data);
  my $mean = $s{mean} = $s{average} = $s{"x-bar"} = 1/$n * $s{sum};

  my ($sum_sq, $Svar) = (0,0);
  for (@$data) {
    $sum_sq += $_ * $_;
    $Svar   += ($_ - $mean)*($_ - $mean);
  }

  $s{"char:sigma"}  = "\x{03C3}";
  $s{"char:Sigma"}  = "\x{03A3}";
  $s{"char:"."sum"} = "\x{2211}";

  $s{sum_sq} = $s{"sum x^2"} = $sum_sq;
  $s{Svar} = $s{sample_variance} = ($n > 1) ? 1/($n-1) * $Svar : 0;
  $s{Sx} = $s{sample_standard_deviation} = sqrt($s{Svar});
  $s{variance} = $s{sigma_sq} = 1/$n * $sum_sq - $mean * $mean;
  $s{variance} = $s{sigma_sq} = 0 if $s{variance} < 0;
  $s{standard_deviation} = $s{sigma} = sqrt($s{variance});
  $s{min} = $$data[0];
  $s{max} = $$data[-1];
  $s{se}  = $s{standard_error} = $s{sigma} / sqrt($n);

  my $med_idx = percentile(.5, $data, sorted => 1, method => "midpoint", return => "index");
  $s{Q1}  = (1 == $n) ? $s{mean} : percentile(.5, [@$data[0..(($med_idx == int($med_idx)) ? $med_idx-1 : $med_idx)]], sorted => 1, method => "midpoint");
  $s{Q2}  = $s{med} = $s{median} = percentile(.5, $data, sorted => 1, method => "midpoint");
  $s{Q3}  = (1 == $n) ? $s{mean} : percentile(.5, [@$data[($med_idx+1)..$#{$data}]], sorted => 1, method => "midpoint");

#   $s{Q1}  = percentile(.25, $data, sorted => 1, method => "midpoint");
#   $s{Q2}  = $s{med} = $s{median} = percentile(.5, $data, sorted => 1, method => "midpoint");
#   $s{Q3}  = percentile(.75, $data, sorted => 1, method => "midpoint");
  wantarray ? %s : \%s;
}
#END: one_var

=head3 percentile

 percentile($p, @data)
 percentile($p, \@data)
 percentile($p, \@data, $sorted)
 percentile($p, \@data, %options)

Return the C<$p>-th percentile using the weighted average at X_{(n+1)p}
method (http://www.xycoon.com/method_2.htm) That is, the number such that
approximately C<100 * $p> of the data values are less than or equal to the
given value. If an array reference is given as well as a third true value,
the data will be assumed to be already sorted. The following options are
available.

=over 4

=item sorted

Boolean value indicating whether the data are sorted already. If not, they
will be sorted numerically.

=item method

One of "midpoint", "floor", "ceil", or "scaled". This controls what to do
when a percentile divider is between two entries. The default behavior is
"scaled", the returned percentile will be an appropriate linear combination
of the neighboring entries. The "midpoint" method always returns the
midpoint of the neighboring entries. Finally, the "floor" and "ceil"
methods always return the lower or higher neighbor respectively.

The "method" also affects the return value when C<return =E<gt> "index"> is
enabled.

=item return

Either "value" or "index". Affects whether we return the actual percentile
value, or simply its index in the array.

=back

=cut

#BEGIN: percentile
sub percentile {
  my ($p,$data,$n,$f,%o,@a) = (shift);
  return unless @_ and $p >= 0 and $p <= 1;
  if    (ref $_[0] eq 'ARRAY' and 0 == (@_ % 2)) { $data = shift; %o = (sorted => @_) if @_ }
  elsif (ref $_[0] eq 'ARRAY') { ($data, %o) = @_ }
  else { $data = \@_ }

  $n = int($p * (1+@$data));
  $f = $p * (1+@$data) - $n;
  $n-- if $n;  # perl arrays start at zero

  if (!$o{method} or $o{method} =~ /^[Ss]/) { 1 }
  elsif ($o{method} =~ /^[Mm]/) { $f = .5 if $f != 0 }
  elsif ($o{method} =~ /^[Ff]/) { $f = 0 }
  elsif ($o{method} =~ /^[Cc]/) { ++$n and ($f = 0) if $f != 0 }

  return $n + $f if $o{return} and $o{return} =~ /^[Ii]/;

  if ($o{sorted}) { @a = @$data[$n,(($n == $#{$data}) ? $n : $n+1)] }
  else            { @a = (sort {$a<=>$b} @$data)[$n,$n+1] }

  if ($n+1 > $#{$data}) { return $a[0] }
  return $a[0] + $f * ($a[1] - $a[0]);
}
#END: percentile


=head3 correlation

 my $r = correlation( \@X, \@Y );
 my %I = correlation( \@X, \@Y );
 my $r = correlation( \@X, \@Y, %options );

Pearson product-moment correlation coefficient.

=over 4

=item one_var_x

=item one_var_y

The result hash from C<one_var()>

=item sd_x

=item sd_y

=item mean_x

=item mean_y

The sample standard deviation and mean of x and y.

=back

=cut

#BEGIN: correlation, depends: one_var
sub correlation {
  my ($X, $Y, %o, $r) = @_;
  my $n = (@$X < @$Y) ? $#{$X} : $#{$Y};
  $o{one_var_x} = one_var($X) unless $o{one_var_x} or (defined($o{sd_x}) and defined($o{mean_x}));
  $o{sd_x}   ||= $o{one_var_x}{Sx};
  $o{mean_x} ||= $o{one_var_x}{mean};

  $o{one_var_y} = one_var($Y) unless $o{one_var_y} or (defined($o{sd_y}) and defined($o{mean_y}));
  $o{sd_y}   ||= $o{one_var_y}{Sx};
  $o{mean_y} ||= $o{one_var_y}{mean};

  my ($x_, $y_) = @o{qw/mean_x mean_y/};
  $r += ($$X[$_] - $x_) * ($$Y[$_] - $y_) for 0..$n;
  $r *= 1 / ($o{sd_x} * $o{sd_y} * $n);

  # The following pseudocode (from wikipedia) computes correlation in a single pass
  #--------------------------------------------------------------------------------
  # sum_sq_x = 0
  # sum_sq_y = 0
  # sum_coproduct = 0
  # mean_x = x[1]
  # mean_y = y[1]
  # for i in 2 to N:
  #     sweep = (i - 1.0) / i
  #     delta_x = x[i] - mean_x
  #     delta_y = y[i] - mean_y
  #     sum_sq_x += delta_x * delta_x * sweep
  #     sum_sq_y += delta_y * delta_y * sweep
  #     sum_coproduct += delta_x * delta_y * sweep
  #     mean_x += delta_x / i
  #     mean_y += delta_y / i
  # pop_sd_x = sqrt( sum_sq_x / N )
  # pop_sd_y = sqrt( sum_sq_y / N )
  # cov_x_y = sum_coproduct / N
  # correlation = cov_x_y / (pop_sd_x * pop_sd_y)

  if (($o{one_var_x} or $o{one_var_y}) and wantarray) {
    return (correlation => $r,
            ($o{one_var_x} ? (one_var_x => $o{one_var_x}) : ()),
            ($o{one_var_x} ? (one_var_y => $o{one_var_y}) : ()));
  } else { return $r }
}
#END: correlation


=head3 permutations

 permutations( $n );
 permutations( @list );  # 1 < @list !!
 permutations( \@list );

Return a list of all permutations of the given input list.

Note: This subroutine is slow and inefficient. If you want to use this for
any real purpose then you should consider using Algorithm::Permute or
Algorithm::FastPermute from CPAN.

=cut

#BEGIN: permutations: depends k_arrangements, factorial
sub permutations {
  return factorial($_[0])                  if @_ == 1 and not ref($_[0]);
  return k_arrangements($_[0], 0+@{$_[0]}) if @_ == 1 and ref($_[0]) eq 'ARRAY';
  return k_arrangements(\@_, 0+@_);
}
#END: permutations


=head3 k_arrangements

 k_arrangements( \@list, $k );
 k_arrangements( $n, $k );

Return a list of all arrangements (sub-permutations) of the given input
list of length $k. If C<$n> and C<$k> are both integers, then simply the
number of C<$k> arrangements is returned.

Note: This subroutine is slow and inefficient. If you want to use this for
any real purpose then you should consider looking up an XS module on CPAN.

=cut

=head3 arrangements

 arrangements( $n );
 arrangements( \@list );
 arrangements( \@list, $k );
 arrangements( $n, $k );

 arrangements( @list );  # @list > 2 !!!

Return a list of all arrangements (sub-permutations) of the given input
list (regardless of length). If the list is provided as a reference and an
integer $k is provided then the results will be restricted to length $k as
in the L<k_arrangements|/k_arrangements> subroutine.

Note: This subroutine is slow and inefficient. If you want to use this for
any real purpose then you should consider looking up an XS module on CPAN.

=cut

#BEGIN: k_arrangements, depends: product, sum, factorial
sub k_arrangements {
  my ($n, $k) = @_;
  ($n, $k) = ($k, $n) if ref($k); # provided array in wrong order

  # provided two numbers
  if (not ref($n)) {
    return sum(map(factorial($_), 0..$n)) unless defined $k;
    return 0       if $k <  0 or $k  > $n;
    return 1       if $k == 0;
    return product($n-$k+1..$n);
  }

  return wantarray ? []  : [[]]  if $k == 0;
  return wantarray ? ()  : []    if $k > @$n or $k < 0;

  return wantarray ? map([$_],@$n) : [map([$_],@$n)] if $k == 1;

  my @A;
  for my $i (0..$#{$n}) {
    for (k_arrangements([@$n[0..$i-1,$i+1..$#{$n}]], $k-1)) {
      push @$_, $$n[$i];
      push @A, $_;
    }
  }
  return wantarray ? @A : \@A;
}
#END: k_arrangements

#BEGIN: arrangements: depends k_arrangements, sum
sub arrangements {
  my ($n, $k) = @_;
  ($n, $k) = ($k, $n) if ref($k); # provided array in wrong order

  if (@_ == 1) {
    if (ref($n)) {
      return wantarray ?   map(k_arrangements($n, $_), 0..@$n)
                       : [ map(k_arrangements($n, $_), 0..@$n) ];
    } else { return sum(map(k_arrangements($n,$_), 0..$n)) }
  }

  return k_arrangements($n, $k) if @_ == 2; # meant to call k_arrangements

  # otherwise, more that 2 elements.
  wantarray ?   map k_arrangements(\@_, $_), 0..@_
            : [ map k_arrangements(\@_, $_), 0..@_ ];
}
#END: arrangements


=head3 k_combinations

 k_combinations( \@list, $k );
 k_combinations( $n, $k );

Return a list of all combinations of the given input list of length $k.

Note: This subroutine is slow and inefficient. If you want to use this for
any real purpose then you should consider looking up an XS module on CPAN.

=cut

=head3 combinations

 combinations( $n );
 combinations( \@list );
 combinations( \@list, $k );
 combinations( $n, $k );

 combinations( @list );  # @list > 2 !!!

Return a list of all combinations of the given input list (regardless of
length). If the list is provided as a reference and an integer $k is
provided then the results will be restricted to length $k as in the
L<k_combinations|/k_combinations> subroutine.

Note: This subroutine is slow and inefficient. If you want to use this for
any real purpose then you should consider looking up an XS module on CPAN.

=cut

#BEGIN k_combinations, depends: product
sub k_combinations {
  my ($n, $k) = @_;
  ($n, $k) = ($k, $n) if ref($k); # provided array in wrong order

  # provided two numbers
  if (not ref($n)) {
    return 2 ** $n unless defined $k;
    return 0       if $k <  0 or $k  > $n;
    return 1       if $k == 0 or $k == $n;

    # provide both as a (slight?) optimization
    return product($n-$k+1..$n)/product(2..$k)   if $k < $n/2;
    return product($k+1..$n)/product(2..$n-$k);# if $k > $n/2;
  }

  return wantarray ? []  : [[]]  if $k == 0;
  return wantarray ? ()  : []    if $k > @$n or $k < 0;

  return wantarray ? map([$_],@$n) : [map([$_],@$n)]              if $k ==  1;
  return wantarray ? [@$n]         : [[@$n]]                      if $k == $n;
  return wantarray ?  map([@$n[0..$_-1,$_+1..$#{$n}]],0..$#{$n})
                   : [map([@$n[0..$_-1,$_+1..$#{$n}]],0..$#{$n})] if $k == $n-1;

  my @A;
  for my $i (0..$#{$n}-$k+1) {
    for (k_combinations([@$n[$i+1..$#{$n}]], $k-1)) {
      unshift @$_, $$n[$i];  # unshift to make combinations be in expected order
      push @A, $_;
    }
  }
  return wantarray ? @A : \@A;
}
#END k_combinations


#BEGIN: combinations: depends k_combinations
sub combinations {
  my ($n, $k) = @_;
  ($n, $k) = ($k, $n) if ref($k); # provided array in wrong order

  if (@_ == 1) {
    if (ref($n)) {
      return wantarray ?   map(k_combinations($n, $_), 0..@$n)
                       : [ map(k_combinations($n, $_), 0..@$n) ];
    } else { return 2 ** $n }
  }

  return k_combinations($n, $k) if @_ == 2; # meant to call k_combinations

  # otherwise, more that 2 elements.
  wantarray ?   map k_combinations(\@_, $_), 0..@_
            : [ map k_combinations(\@_, $_), 0..@_ ];
}
#END: combinations


=head3 npdf

 npdf $x
 npdf $x, $mu
 npdf $x, $mu, $sigma

Compute the probability S<P( X = C<$x> )> assuming a normal distribution
with mean C<$mu> and standard deviation C<$sigma>. C<$mu> and C<$sigma> are
assumed to be C<0> and C<1> respectively if they are missing. C<$sigma>
must be positive.

=cut

#BEGIN: npdf, depends: $pi
sub npdf {
  return exp(-$_[0]*$_[0]/2) / 2.506628274631000502415765284811 if @_ == 1;

  my ($x,$mu,$s) = @_;
  $mu ||= 0; $s ||= 1; $s = abs($s);
  return exp(($mu-$x)*($x-$mu)/(2*$s*$s))/(sqrt(2*$pi) * $s)
}
#END: npdf


=head3 ncdf

 ncdf $x
 ncdf $x, $mu
 ncdf $x, $mu, $sigma

Compute the probability S<P( X E<lt>= C<$x> )> assuming a normal
distribution with mean C<$mu> and standard deviation C<$sigma>. C<$mu> and
C<$sigma> are assumed to be C<0> and C<1> respectively if they are missing.
C<$sigma> must be positive.

=cut

#BEGIN: ncdf, depends: interpolating_function
{ my @data = qw/
0.503989 0.507978 0.511966 0.515953 0.519939 0.523922 0.527903 0.531881
0.535856 0.539828 0.543795 0.547758 0.551717 0.55567 0.559618 0.563559
0.567495 0.571424 0.575345 0.57926 0.583166 0.587064 0.590954 0.594835
0.598706 0.602568 0.60642 0.610261 0.614092 0.617911 0.62172 0.625516
0.6293 0.633072 0.636831 0.640576 0.644309 0.648027 0.651732 0.655422
0.659097 0.662757 0.666402 0.670031 0.673645 0.677242 0.680822 0.684386
0.687933 0.691462 0.694974 0.698468 0.701944 0.705401 0.70884 0.71226
0.715661 0.719043 0.722405 0.725747 0.729069 0.732371 0.735653 0.738914
0.742154 0.745373 0.748571 0.751748 0.754903 0.758036 0.761148 0.764238
0.767305 0.77035 0.773373 0.776373 0.77935 0.782305 0.785236 0.788145
0.79103 0.793892 0.796731 0.799546 0.802337 0.805105 0.80785 0.81057
0.813267 0.81594 0.818589 0.821214 0.823814 0.826391 0.828944 0.831472
0.833977 0.836457 0.838913 0.841345 0.843752 0.846136 0.848495 0.85083
0.853141 0.855428 0.85769 0.859929 0.862143 0.864334 0.8665 0.868643
0.870762 0.872857 0.874928 0.876976 0.879 0.881 0.882977 0.88493 0.886861
0.888768 0.890651 0.892512 0.89435 0.896165 0.897958 0.899727 0.901475
0.9032 0.904902 0.906582 0.908241 0.909877 0.911492 0.913085 0.914657
0.916207 0.917736 0.919243 0.92073 0.922196 0.923641 0.925066 0.926471
0.927855 0.929219 0.930563 0.931888 0.933193 0.934478 0.935745 0.936992
0.93822 0.939429 0.94062 0.941792 0.942947 0.944083 0.945201 0.946301
0.947384 0.948449 0.949497 0.950529 0.951543 0.95254 0.953521 0.954486
0.955435 0.956367 0.957284 0.958185 0.95907 0.959941 0.960796 0.961636
0.962462 0.963273 0.96407 0.964852 0.96562 0.966375 0.967116 0.967843
0.968557 0.969258 0.969946 0.970621 0.971283 0.971933 0.972571 0.973197
0.97381 0.974412 0.975002 0.975581 0.976148 0.976705 0.97725 0.977784
0.978308 0.978822 0.979325 0.979818 0.980301 0.980774 0.981237 0.981691
0.982136 0.982571 0.982997 0.983414 0.983823 0.984222 0.984614 0.984997
0.985371 0.985738 0.986097 0.986447 0.986791 0.987126 0.987455 0.987776
0.988089 0.988396 0.988696 0.988989 0.989276 0.989556 0.98983 0.990097
0.990358 0.990613 0.990863 0.991106 0.991344 0.991576 0.991802 0.992024
0.99224 0.992451 0.992656 0.992857 0.993053 0.993244 0.993431 0.993613
0.99379 0.993963 0.994132 0.994297 0.994457 0.994614 0.994766 0.994915
0.99506 0.995201 0.995339 0.995473 0.995604 0.995731 0.995855 0.995975
0.996093 0.996207 0.996319 0.996427 0.996533 0.996636 0.996736 0.996833
0.996928 0.99702 0.99711 0.997197 0.997282 0.997365 0.997445 0.997523
0.997599 0.997673 0.997744 0.997814 0.997882 0.997948 0.998012 0.998074
0.998134 0.998193 0.99825 0.998305 0.998359 0.998411 0.998462 0.998511
0.998559 0.998605 0.99865 0.998694 0.998736 0.998777 0.998817 0.998856
0.998893 0.99893 0.998965 0.998999 0.999032 0.999065 0.999096 0.999126
0.999155 0.999184 0.999211 0.999238 0.999264 0.999289 0.999313 0.999336
0.999359 0.999381 0.999402 0.999423 0.999443 0.999462 0.999481 0.999499
0.999517 0.999534 0.99955 0.999566 0.999581 0.999596 0.99961 0.999624
0.999638 0.999651 0.999663 0.999675 0.999687 0.999698 0.999709 0.99972
0.99973 0.99974 0.999749 0.999758 0.999767 0.999776 0.999784 0.999792
0.9998 0.999807 0.999815 0.999822 0.999828 0.999835 0.999841 0.999847
0.999853 0.999858 0.999864 0.999869 0.999874 0.999879 0.999883 0.999888
0.999892 0.999896 0.9999 0.999904 0.999908 0.999912 0.999915 0.999918
0.999922 0.999925 0.999928 0.999931 0.999933 0.999936 0.999938 0.999941
0.999943 0.999946 0.999948 0.99995 0.999952 0.999954 0.999956 0.999958
0.999959 0.999961 0.999963 0.999964 0.999966 0.999967 0.999968 0.99997
0.999971 0.999972 0.999973 0.999974 0.999975 0.999976 0.999977 0.999978
0.999979 0.99998 0.999981 0.999982 0.999983 0.999983 0.999984 0.999985
0.999985 0.999986 0.999987 0.999987 0.999988 0.999988 0.999989 0.999989
0.99999 0.99999 0.999991 0.999991 0.999991 0.999992 0.999992 0.999993
0.999993 0.999993 0.999993 0.999994 0.999994 0.999994 0.999995 0.999995
0.999995 0.999995 0.999996 0.999996 0.999996 0.999996 0.999996 0.999996
0.999997 0.999997 0.999997 0.999997 0.999997 0.999997 0.999997 0.999998
0.999998 0.999998 0.999998 0.999998 0.999998 0.999998 0.999998 0.999998
0.999998 0.999998 0.999999 0.999999 0.999999 0.999999 0.999999 0.999999
0.999999 0.999999 0.999999 0.999999 0.999999 0.999999 0.999999 0.999999
0.999999 0.999999 0.999999 0.999999 0.999999 0.999999 0.999999 0.999999
1 1 1 1 1/;

  sub ncdf {
    state $_std_ncdf;
    unless ($_std_ncdf) {
        my $_f = { "0" => 0.5 }; my $x = 0;
        for (@data) { $x += 0.01; $$_f{$x} = $_; $$_f{-$x} = 1 - $_; }
        $_std_ncdf = interpolating_function($_f,"",1);
    }

    return &$_std_ncdf(@_) if @_ == 1;

    my ($x,$mu,$s) = @_;
    $mu ||= 0; $s ||= 1; $s = abs($s);
    return &$_std_ncdf( ($x-$mu)/$s );
  }
}
#END: ncdf



#-----------------------------------------------------------------
                         $EXPORT_TAGS{math}
                                 =
[qw/Nintegrate interpolating_function interpolate
    continuous_compounding discrete_compounding savings_plan loan_payment
    union intersection difference frac gcd lcm extended_euclidean_algorithm
    modular_inverse ndiff dotprod hypot leg
   /];
#-----------------------------------------------------------------

=head2 :math - Mathematical Functions

=head3 hypot

 my $h = hypot($x, $y);
 my $h = hypot($x, $y, $z, ...);

Euclidean distance function: returns sqrt(x^2+y^2+...) without risk of
undue overflow.

=cut

#BEGIN: hypot, depends: max
sub hypot {
    my @val = map abs($_), @_;
    my $max = max(@val);
    return 0 unless $max;
    $_ /= $max for @val;
    my $sum = 0;
    $sum += $_*$_ for @val;
    return $max * sqrt($sum);
}
#END: hypot

=head3 leg

 my $x = leg($h, $y);

Leg of a triangle: returns sqrt(h^2-y^2)

=cut

#BEGIN: leg
sub leg {
    # Without overflow https://en.wikipedia.org/wiki/Hypot
    my ($h, $y) = (abs($_[0]), abs($_[1]));
    return $y unless $h;
    my $t = $y / $h;
    return $h * sqrt(1 - $t*$t)
}
#END: leg

=head3 dotprod(\@\@)

 my $d = dotprod @x, @y;
 my $d = &dotprod(\@x, [1,2,3]);

Compute the dot product of two vectors

=cut

#BEGIN: dotprod
sub dotprod :prototype(\@\@) {
  Carp::croak("dotprod: Vectors must have same length ($#{$_[0]} != $#{$_[1]})") unless $#{$_[0]} == $#{$_[1]};
  my $p = 0;
  $p += $_[0][$_] * $_[1][$_] for 0..$#{$_[0]};
  $p;
}
#END: dotprod

=head3 modular_inverse

 $inverse = modular_inverse( $x, $m );

Compute the inverse of $x in the group Z_m. The inverse will be within
the set [0..$m-1].

Note: $x must be relatively prime to $m.

=cut

#BEGIN: modular_inverse, depends: extended_euclidean_algorithm
sub modular_inverse {
  my ($i, undef, $d ) = extended_euclidean_algorithm(@_);
  return $i % $_[1] if $d == 1 and $i;
}
#END: modular_inverse


=head3 gcd

Compute the Greatest Common Divisor of a list of integers using the
Euclidean algorithm.

=cut

# http://www.perlmonks.org/?node_id=56906
#BEGIN: gcd
sub gcd {
  my $x = shift;
  while (@_) {
    my $y = shift;
    ($x, $y) = ($y, $x % $y) while $y;
  }
  return abs $x;
}
#END: gcd


=head3 lcm

Compute the Least Common Multiple of a list of integers.

=cut

#BEGIN: lcm, depends: gcd
sub lcm{
  my $x = shift;
  for (@_) {
    $x = ($x/gcd($x,$_)) * $_
  }
  return abs $x;
}
#END: lcm


=head3 extended_euclidean_algorithm

 ($alpha, $beta, $d) = extended_euclidean_algorithm($a, $b)

For a pair of integers, a and b, perform the extended Euclidean
algorithm to compute alpha, beta, and d such that:

 d = alpha * a  +  beta * b

In particular, if d = 1 then alpha = a^-1 mod b.

=cut

#BEGIN: extended_euclidean_algorithm
sub extended_euclidean_algorithm {
  my ($A, $B) = @_;
  my ($x0, $x1, $y0, $y1, $q) = (1, 0, 0, 1);
  while ($B != 0) {
    ($q, $B, $A) = ((int($A/$B)), ($A % $B), $B);
    ($x0, $x1)   = ($x1, ($x0 - ($q * $x1)));
    ($y0, $y1)   = ($y1, ($y0 - ($q * $y1)));
  }
  return ($x0, $y0, $A);
}
#END: extended_euclidean_algorithm


=head3 frac

 my ($N, $D) = frac( $dec )

Convert a decimal to a fraction. Returns undef if number is not
rationalizable (must have repeating decimals).

=cut

#BEGIN: frac, depends: gcd
$_Util::frac::decimallength = 16;
sub frac {
  my $n = shift;
  my $gcd;
  return $n unless defined $n and $n =~ /\./;
  if ($n =~ /([-+]?)([\d\.]+?)(\d+?)(?:\3)+((??{ '\d{0,'.(length($3)-1).'}' }))$/) {
    my ($sign, $pre, $pat, $post, $mul) = ($1,$2,$3,$4, 0);
    return unless $post eq substr($pat, 0, length($post));
    if ($pre =~ /^\d+\.(\d+)/) { $mul = length($1) }
    $pre =~ s/\.//;
    $pre =~ s/^0+(\d)/$1?$1:0/e;
    my ($N, $D) = ("$pre$pat"-$pre, ("9"x(length $pat)).("0"x$mul));
    $gcd = gcd($N,$D);
    return wantarray ? ("$sign$N"/$gcd, $D/$gcd) : ("$sign$N"/$gcd).'/'.($D/$gcd);
  } else {
    my ($a, $b) = split /\./, $n;
    return unless $_Util::frac::decimallength > length("$a.$b");
    ($a, $b) = ($a.$b, "1".("0"x(length $b)));
    $gcd = gcd($a,$b);
    return wantarray ? ($a/$gcd, $b/$gcd) : ($a/$gcd).'/'.($b/$gcd);
  }
}
#END: frac


=head3 ndiff(&;@)

 my $df = ndiff \&f;
 my $df = ndiff \&f, $x;

Perform numerical differentiation using the central difference formula.

 f'(a) \approx ( f(a+h) - f(a-h) ) / (2h)

If M \approx f(a) \approx f''(c) for all c \in [a-h, a+h], then the total
error (truncation plus round-off) is on the order of:

 error = M * (h^2/6 + eps/h)

where eps is the machine epsilon (eps = 2E-16 on 32-bit perl; (1 + 2E-16 !=
1), however (1 + (2E-16)/2 == 1) ). Thus, error is minimized when h \approx
\sqrt[3]{eps}. We choose h = 2**(-20) = 0.00000095367431640625.

Examples:

 sub f { $_[0]**2 }
 my $df = ndiff \&f;
 printf "%.5f  |  %.5f\n", f($_), $df->($_) for 0..10;

 say "f'(3) = ", ndiff(\&f, 3);

 $df = ndiff { $_ ** 2 };

=cut

#BEGIN: ndiff
{ my $delta  = 0.00000095367431640625;
  my $ddelta = 0.0000019073486328125;
  sub ndiff :prototype(&;@) {
    my $f  = shift;
    my $df = sub { local $_; ($f->($_ = $_[0]+$delta) - $f->($_ = $_[0]-$delta)) / $ddelta };
    @_ ? $df->(@_) : $df;
  }
}
#END: ndiff

=head3 Nintegrate

 Nintegrate { block } $a, $b, $n
 Nintegrate \&sub, $a, $b, $n

Integrate a function between two values using a composite Simpson's rule.
The last argument C<$n> is optional and specifies the number of intervals
to divide the region into. The default is 1000.

The function is assumed to be continuous with continuous derivatives up to
order 4. C<$n> should be even, but we adjust it if it is not. The error is
given by,

             5
        (b-a)     (4)
 err = --------  f  ( x )
             4
        180 n

for some x in the interval (a,b).

=cut

#BEGIN: Nintegrate
sub Nintegrate :prototype(&@) {
  local $_;
  my ($f, $A, $B, $n) = @_; $n ||= 1000; $n += 1 if $n % 2;
  my $h = ($B-$A)/$n;
  my ($x0, $x1, $x2) = (0, 0, 0);
  $_ = $A; $x0 += &$f($A);
  $_ = $B; $x0 += &$f($B);

  for my $i (1..$n-1) {
    $_ = $A + $i * $h;
    ($i % 2) ? $x1 = $x1 + &$f($_)
             : $x2 = $x2 + &$f($_);
  }
  return $h * ($x0 + 2 * $x2 + 4 * $x1) / 3;
}
#END: Nintegrate


=head3 interpolating_function

 interpolating_function \%function, $message, $nowarn

Returns a perl subroutine which interpolates C<%function> linearly using
L<interpolate|/interpolate>. C<$message> is an optional message that will
be used if an input value is given which is out of range of the
interpolator.

=cut

#BEGIN: interpolating_function, depends: interpolate
sub interpolating_function {
  my ($func, $message, $nowarn) = @_;
  $message = "" unless defined $message;
  my $keys = [ sort {$a<=>$b} keys %$func ];
  return sub { interpolate(shift(), $func, $keys, $message, $nowarn) }
}
#END: interpolating_function


=head3 interpolate

 interpolate $x, \%function, \@keys, $message, $nowarn

Perform an interpolation of the provided function at the point C<$x>. The
keys of the function need not be evenly spaced, the value is approximated
linearly. The last two parameters are optional, C<@keys> is a sorted list
of the keys of the function and C<$message> is used in the error message
that is printed if C<$x> is out of range of the interpolator.

=cut

#BEGIN: interpolate
{ use Carp;
  sub interpolate {
    my ($x, $hash, $keys, $mesg, $nowarn) = @_;
    carp "Use of uninitialized value in interpolation" unless defined $x;
    $keys = [ sort {$a<=>$b} keys %$hash ] unless $keys;
    $mesg = ""                             unless defined $mesg;

    if ($x < $$keys[0]) {
      print STDERR "\nWarning! Hit lower endpoint in interpolation. $mesg\n",
        "   Requested: x = $x    Using: (x, y) = ($$keys[0], $$hash{$$keys[0]})\n" unless $nowarn;
      return $$hash{$$keys[0]};

    } elsif ($x > $$keys[-1]) {
      print STDERR "\nWarning! Hit upper endpoint in interpolation. $mesg\n",
        "   Requested: x = $x    Using: (x, y) = ($$keys[-1], $$hash{$$keys[-1]})\n" unless $nowarn;
      return $$hash{$$keys[-1]};

    } elsif ($x == $$keys[-1]) {
      return $$hash{$$keys[-1]}

    } elsif ($x == $$keys[0]) {
      return $$hash{$$keys[0]}

    } else {
      # find $i0 so that $keys[$i0] < $x < $keys[$i0+1].
      my ($i0,$i1,$i) = (0,$#{$keys});
      while ($i1-$i0 > 1) {
        $i = ($i1+$i0) >> 1;
        if ($x == $$keys[$i]) { $i0 = $i; last }
        ($x < $$keys[$i]) ? ($i1 = $i) : ($i0 = $i);
      }

      # Note: This gives an exact answer if $x == $$keys[$i] for some $i.
      my ($x1, $x2, $y1, $y2) = ($$keys[$i0], $$keys[$i0+1], $$hash{$$keys[$i0]}, $$hash{$$keys[$i0+1]});
      return ($y2-$y1)/($x2-$x1) * ($x-$x1) + $y1;
    }
  }
}
#END: interpolate


=head3 continuous_compounding

 continuous_compounding P => $P, r => $r, t => $t;
 continuous_compounding A => $A, P => $P, r => $r, t => $t, solve_for => $q;

Given any three of "A" (Accumulated balance), "P" (Principal balance), "r"
(interest Rate), and "t" (Time to withdrawal), this function will return
the fourth. If all four values are provided (presumedly one of them will be
undefined or contain garbage) then you must provide a "solve_for" key which
points to one of "A", "P", "r", or "t". All values are case insensitive.

=cut

#BEGIN: continuous_compounding, depends: str
sub continuous_compounding {
  my %opt = map lc(str($_)), @_;
  unless ($opt{solve_for}) {
    my $pat = join "|", map quotemeta, keys %opt;
    my @missing = grep !/^(?:$pat)$/, qw/a p r t/;
    return unless @missing == 1;
    $opt{solve_for} = $missing[0];
  }

  my ($A, $P, $r, $t) = @opt{qw/a p r t/};
  for ($opt{solve_for}) {
    /a/ and return $P * exp($r*$t);
    /p/ and return $A * exp(-$r*$t);
    /r/ and return log($A/$P) / $t;
    /t/ and return log($A/$P) / $r;
  }
}
#END: continuous_compounding


=head3 discrete_compounding

 discrete_compounding P => $P, r => $r, t => $t, n => $n;
 discrete_compounding A => $A, P => $P, r => $r, t => $t, n => $n, solve_for => $q;

Given "n" (Number of compoundings per year) and any three of "A"
(Accumulated balance), "P" (Principal balance), "r" (interest Rate), and
"t" (Time to withdrawal), this function will return the fourth. If all five
values are provided (presumedly one of them will be undefined or contain
garbage) then you must provide a "solve_for" key which points to one of
"A", "P", "r", or "t". All values are case insensitive.

=cut

#BEGIN: discrete_compounding, depends: str
sub discrete_compounding {
  my %opt = map lc(str($_)), @_;
  unless ($opt{solve_for}) {
    my $pat = join "|", map quotemeta, keys %opt;
    my @missing = grep !/^(?:$pat)$/, qw/a p r t/;
    return unless @missing == 1;
    $opt{solve_for} = $missing[0];
  }

  my ($A, $P, $r, $n, $t) = @opt{qw/a p r n t/};
  for ($opt{solve_for}) {
    /a/ and return $P*(1+$r/$n)**($n*$t);
    /p/ and return $A*(1+$r/$n)**(-$n*$t);
    /r/ and return $n*(exp(log($A/$P)/($n*$t))-1);
    /t/ and return log($A/$P)/($n*log(1+$r/$n));
  }
}
#END: discrete_compounding


=head3 savings_plan

 savings_plan pmt => $pmt, r => $r, t => $t, n => $n;
 savings_plan A => $A, pmt => $pmt, r => $r, t => $t, n => $n, solve_for => $q;

Given "n" (Number of deposits per year), "r" (interest Rate), and any two
of "A" (Accumulated balance), "pmt" (Payment amount), and "t" (Time to
withdrawal), this function will return the third. If all five values are
provided (presumedly one of them will be undefined or contain garbage) then
you must provide a "solve_for" key which points to one of "A", "pmt", "r",
or "t". All values are case insensitive.

=cut

#BEGIN: savings_plan, depends: str
sub savings_plan {
  my %opt = map lc(str($_)), @_;
  unless ($opt{solve_for}) {
    my $pat = join "|", map quotemeta, keys %opt;
    my @missing = grep !/^(?:$pat)$/, qw/a pmt t/;
    return unless @missing == 1;
    $opt{solve_for} = $missing[0];
  }

  my ($A, $pmt, $r, $n, $t) = @opt{qw/a pmt r n t/};
  for ($opt{solve_for}) {
    /a/ and return $pmt*( (1+$r/$n)**($n*$t)-1 )/($r/$n);
    /p/ and return $A*($r/$n)/( (1+$r/$n)**($n*$t)-1 );
    /t/ and return log(1+($A*$r)/($pmt*$n))/($n*log(1+$r/$n));
  }
}
#END: savings_plan


=head3 loan_payment

 loan_payment pmt => $pmt, r => $r, t => $t, n => $n;
 loan_payment L => $L, pmt => $pmt, r => $r, t => $t, n => $n, solve_for => $q;

Given "n" (Number of deposits per year), "r" (interest Rate), and any two
of "L" (Loan amount), "pmt" (Payment amount), and "t" (Time to full
payback), this function will return the third. If all five values are
provided (presumedly one of them will be undefined or contain garbage) then
you must provide a "solve_for" key which points to one of "A", "pmt", "r",
or "t". All values are case insensitive.

=cut

#BEGIN: loan_payment, depends: str
sub loan_payment {
  my %opt = map lc(str($_)), @_;
  unless ($opt{solve_for}) {
    my $pat = join "|", map quotemeta, keys %opt;
    my @missing = grep !/^(?:$pat)$/, qw/l pmt t/;
    return unless @missing == 1;
    $opt{solve_for} = $missing[0];
  }

  my ($L, $pmt, $r, $n, $t) = @opt{qw/l pmt r n t/};
  for ($opt{solve_for}) {
    /l/ and return $pmt/($r/$n+($r/$n)/((1+$r/$n)**($n*$t)-1));
    /p/ and return $L*($r/$n+($r/$n)/((1+$r/$n)**($n*$t)-1));
    /t/ and return log(1+1/( ($n*$pmt)/($r*$L)-1 ))/($n*log(1+$r/$n));
  }
}
#END: loan_payment

=head3 union

use Set::Object

 union( $L1, $L2, ... )

Return the list of (string) elements which appear in any of the given
arrays. Objects are stringified, and the string values are returned. This
may be upgraded to be smarter someday.

=cut

#BEGIN: union, 1 line
sub union { my %x; for (@_) { next unless @$_; undef(@x{@$_}) } keys %x }

=head3 intersection

use Set::Object

 intersection( $L1, $L2, ... )

Return the list of (string) elements which appear in all of the given
arrays. Objects are stringified, and the string values are returned. This
may be upgraded to be smarter someday.

=cut

#BEGIN: intersection, 1 line
sub intersection { my %x; for (@_) { my %i; $i{$_}++ or $x{$_}++ for @$_ } grep($x{$_} == @_, keys %x) }

=head3 difference

use Set::Object

 difference( $L1, $L2, ... )

Return the list of (string) elements which appear in C<$L1> but not in any
of the subsequent arrays. Objects are stringified, and the string values
are returned.

=cut

#BEGIN: difference
sub difference {
  my %x;
  my $orig = shift;
  return unless @$orig;# "Modification of a read-only value attempted" if $orig is empty unless we check here
  undef(@x{@$orig});
  for (@_) {
    delete(@x{@$_});
  }
  keys %x
}
#END: difference


#-----------------------------------------------------------------
                         $EXPORT_TAGS{list}
                                 =
[qw/find_index find_index_with_memory first first_pos partition
    even_positions odd_positions suggestion_sort unique lex_sort
    flatten transposed cartesian natural_sort natural_cmp binary_search
    bucketize text_sort text_sort_by list
/];
#-----------------------------------------------------------------

=head2 :list - List Utilities

=head3 list

 for (list($x, $y, ...)) { ... }

Unwraps a single level of ARRAYs and removes single level of undefined
values. Does not operate deeply (see flatten() for that).

=cut

#BEGIN: list
sub list {
    return map { (ref($_) eq 'ARRAY') ? @$_ : $_ } grep defined($_), @_;
}
#END: list


=head3 binary_search(&@)

 binary_search { $_ > 4 } @sorted_nums;
 binary_search \&f, @sorted_nums;

Implements a binary search. Second argument must be an array (not a list)
and must be sorted. Returns the index of the first element for which the
function C<&f> returns true. Returns C<undef> if there is no such element.

Function must return true for all elements larger than desired element. To
search for a particular element, the following must be done:

 my $i = binary_search { $_ >= 4 } @sorted_nums;
 $i = undef unless $sorted_nums[$i] == 4;

=cut

#BEGIN: binary_search
sub binary_search :prototype(&\@) {
  my ($f, $x) = @_;
  local $_;
  return 0   if &$f($_ = $$x[0]);
  return unless &$f($_ = $$x[-1]);
  my ($a, $b, $i) = (0, $#{$x}, $#{$x} >> 1);
  while ($i != $a) {
    &$f($_ = $$x[$i]) ? ($b = $i) : ($a = $i);
  } continue { $i = $a + (($b-$a) >> 1) }
  # Note: $i = int(($a+$b)/2)  has possible overflow issues since a+b may be large
  return $b;
}
#END: binary_search


=head3 text_sort

Try also: Sort::Key::Natural with a fc() pre-processor.

Natural sort with case folding and Unicode support. Mostly a direct use of
Unicode::Collate with automatic binary string decoding (assumes UTF-8) and
numerical substring extraction as in natural_sort.

Limitations:

  It doesn't "properly" sort negative numbers, non-fixed decimal values,
  nor integers larger than 10^24 ≈ 2^83.

=cut

#BEGIN: text_sort
sub text_sort {
    require Encode;
    require Unicode::Collate;
    state $collator = Unicode::Collate->new(
        preprocess => sub {
            my $key = shift;
            $key = Encode::decode("UTF-8", $key, 1) if $key =~ /[^\x21-\x7E]/ and !Encode::is_utf8($key);
            $key =~ s[(\d+)][ sprintf "%025d", $1 ]ge;
            $key
        }
    );
    return $collator->sort(@_);
}
#END: text_sort


=head3 text_sort_by(&@)

Try also: Sort::Key::Natural with a fc() pre-processor.

  @sorted = text_sort_by { $_->title } @books;

Natural sort with case folding and Unicode support. Mostly a direct use of
Unicode::Collate with automatic binary string decoding (assumes UTF-8) and
numerical substring extraction as in natural_sort. Callback is called on
each item and should return a string for comparison.

Limitations:

Necessarily, it does not "properly" sort negative numbers or non-fixed decimal values.

It also can not sort integers larger than 10^24 ≈ 2^83.

=cut

#BEGIN: text_sort_by
sub text_sort_by :prototype(&@) {
    require Unicode::Collate;
    require Encode;

    my $cb = shift;
    my $preprocessor = sub {
        local $_ = shift;
        $_ = $cb->($_);
        $_ = Encode::decode("UTF-8", $_, 1) if /[^\x21-\x7E]/ and !Encode::is_utf8($_);
        s[(\d+)][ sprintf "%025d", $1 ]ge;
        $_;
    };

    return Unicode::Collate->new( preprocess => $preprocessor )->sort(@_);
}
#END: text_sort_by


=head3 natural_sort

See also: Sort::Key::Natural

A "fast, flexible, stable sort" that sorts strings naturally (that is,
numerical substrings are compared as numbers).

Code lifted from tye on perlmonks: http://www.perlmonks.org/?node_id=442285

Limitations: http://www.perlmonks.org/?node_id=483466

  It doesn't "properly" sort negative numbers, non-fixed decimal values,
  nor integers larger than 2^32-1.

=cut

#BEGIN: natural_sort
sub natural_sort {
  @_[
    map { unpack "N", substr($_,-4) }
    sort
    map {
        my $key = $_[$_];
        $key =~ s[(\d+)][ pack "N", $1 ]ge;
        $key . pack "N", $_
    } 0..$#_
  ];
}
#END: natural_sort

=head3 natural_cmp

See also: Sort::Key::Natural

A fast, flexible, stable comparator that sorts strings naturally (that is,
numerical substrings are compared as numbers).

Code lifted from tye on perlmonks: http://www.perlmonks.org/?node_id=442285

Limitations: http://www.perlmonks.org/?node_id=483466

  It doesn't "properly" sort negative numbers, non-fixed decimal values,
  nor integers larger than 2^32-1.

=cut

#BEGIN: natural_cmp
sub natural_cmp {
  my ($x,$y) = map { my $key = $_; $key =~ s[(\d+)][ pack "N", $1 ]ge; $key } @_;
  $x cmp $y;
}
#END: natural_cmp


=head3 cartesian

 cartesian \@list1, \@list2, ...
 cartesian $n1, $n2, ...

Form the cartesian product of the elements in the lists. That is, all lists
of the form C<[ $e1, $e2, ... ]> where C<$e1> comes from C<@list1>, and so
on. This function returns an array reference in scalar context, and a list
in list context.

In the second form, the lists C<[1..$n1]>, C<[1..$n2]>, ... will be
constructed, and the cartesian product of those lists will be computed.
Note however, that the two forms can not be combined, you must either
provide only arrays or only numbers.

=cut

#BEGIN: cartesian
{ my $_X = sub {
    my ($A, $b) = @_;
    my @X; $b = [1..$b] unless ref($b);
    for my $a (@$A) {
      push @X, [@$a, $_] for @$b;
    }
    return \@X;
  };

  sub cartesian {
    return wantarray ? () : [] unless @_;
    my $X = [map [$_], @{ref($_[0]) ? shift() : [1..shift]}];
    $X = $_X->($X, $_) for @_;
    wantarray ? @$X : $X;
  }
}
#END: cartesian

=head3 transposed

 transposed \@LoL

Transpose the (possibly non-regular) list of lists C<@LoL>. Returns a new
list reference containing the objects in C<@LoL>.

=cut

#BEGIN: transposed
sub transposed {
  my $M = shift;
  my $N = [];
  my $i = 0;
  for (@$M) {
    $$N[$_][$i] = $$M[$i][$_] for 0..$#{$_};
  } continue { $i++ }
  return $N;
}
#END: transposed


=head3 flatten

 flatten @LoLoLoL

Will recursively run through each element of the input list and will return
all components as a single large list. Lists may be arbitrarily nested and
any objects which are not perl ARRAY's will be considered plain elements.
The expansion is done depth-first. Returns a reference in scalar context,
and the list of elements in list context.

Example:

 @y = flatten [1, 2, 3], [4, 5], [[6, 7], 8, 9];
 say "Hooray!" if "@y" eq "1 2 3 4 5 6 7 8 9";

=cut

#BEGIN: flatten
sub flatten {
  my @x;
  for (@_) {
    if ('ARRAY' eq ref) { push @x, flatten( @$_ ) }
    else { push @x, $_ }
  }
  return wantarray ? @x : \@x;
}
#END: flatten


=head3 find_index

 find_index \&f, \@array
 find_index { BLOCK } \@array
 find_index { BLOCK } \@array, $start, $stop, $step

May be called with either a function or a block as the first argument. The
function will then begin at C<$start> (or zero) and then step by C<$step>
(or 1) until we reach C<$stop> (or the end of the array).

C<$_> will be set to the current array entry which will also be passed to
the function as its only argument. Thus you may use either C<$_> or
C<$_[0]> within your function.

C<$start> may be greater then C<$stop> in which case we will proceed
backwards. In all cases the sign of C<$d> will be adjusted if necessary so
that we finish in finite time.

=cut

=head3 find_index_with_memory

 find_index_with_memory \&f, \@array
 find_index_with_memory { BLOCK } \@array
 find_index_with_memory { BLOCK } \@array, $start, $stop, $step

May be called with either a function or a block as the first argument. The
function will then begin at C<$start> (or zero) and then step by C<$step>
(or 1) until we reach C<$stop> (or the end of the array).

The function will set the caller's C<$a> to the previous array entry and
C<$b> to the current array entry and will also pass the two entries to the
function as its only arguments. Thus you may use either C<$a, $b> or
C<$_[0], $_[1]> as the previous and current entries respectively.

C<$start> may be greater then C<$stop> in which case we will proceed
backwards. In all cases the sign of C<$d> will be adjusted if necessary so
that we finish in finite time.

=cut

#BEGIN: find_index
sub find_index :prototype(&@) {
  local $_;
  my ($f, $A, $i, $n, $d) = @_;
  $i = 0      unless defined $i;
  $n = $#{$A} unless defined $n;
  $d ||= 1; # May not be zero!
  $d = -$d if $d*($n-$i) < 0;

  if ($i < $n) { while ($i <= $n) { return $i if &$f($_ = $$A[$i]); $i += $d } }
  else         { while ($i >= $n) { return $i if &$f($_ = $$A[$i]); $i += $d } }
  return;
}
#END: find_index


#   local ($a, $b);
#   $a = $$A[$i]; $i += $d;
#   if ($i < $n) { while ($i <= $n) { return $i if &$f($a, $b = $$A[$i]); $a = $b; $i += $d } }
#   else         { while ($i >= $n) { return $i if &$f($a, $b = $$A[$i]); $a = $b; $i += $d } }

#BEGIN: find_index_with_memory
sub find_index_with_memory :prototype(&@) {
  my ($f, $A, $i, $n, $d) = @_;
  $i = 0      unless defined $i;
  $n = $#{$A} unless defined $n;
  $d ||= 1; # May not be zero!
  $d = -$d if $d*($n-$i) < 0;

  # Ah, glorious Perl!
  no strict 'refs';
  no warnings 'once';
  my $caller = caller;
  local(*{$caller."::a"}) = \my $a;
  local(*{$caller."::b"}) = \my $b;

  $a = $$A[$i]; $i += $d;
  if ($i < $n) { while ($i <= $n) { return $i if &$f($a, $b = $$A[$i]); $a = $b; $i += $d } }
  else         { while ($i >= $n) { return $i if &$f($a, $b = $$A[$i]); $a = $b; $i += $d } }
  return;
}
#END: find_index_with_memory


=head3 first

See also: List::Util first

 first \&sub, @list         # if @list is not list of arrays
 first { block }  @list     # if @list is not list of arrays
 first { block } \@list
 first { block } \@list, $start_pos

Return the first item of C<@list> for which the code returns true. Code may
be either a subroutine reference or a code block. C<$_> will be set to each
list entry and will also be passed in as the first (and only) argument. You
may pass C<@list> by reference (which means that you must pass it by
reference if it contains an array reference in its first entry). If you
pass C<@list> by reference and provide a third argument, then the third
argument will be taken to be the first position that should be checked.

=cut

=head3 first_pos

See also: List::MoreUtils first_index

 first_pos \&sub, @list
 first_pos { block } @list
 first_pos { block } \@list, $start_pos

Return the index of the first item of C<@list> for which the code returns
true. Code may be either a subroutine reference or a code block. C<$_> will
be set to each list entry and will also be passed in as the first (and
only) argument. You may pass C<@list> by reference (which means that you
must pass it by reference if it contains an array reference in its first
entry). If you pass C<@list> by reference and provide a third argument,
then the third argument will be taken to be the first position that should
be checked. In this case the returned index will still correspond correctly
to a position in C<@list>.

=cut

#BEGIN: first
sub first :prototype(&@)     {
  my $f = shift;
  if (ref $_[0] eq 'ARRAY') {
    if (@_ > 1) { for (@{$_[0]}[$_[1]..$#{$_[0]}]) { return $_ if &$f($_) } }
    else        { for (@{$_[0]})                   { return $_ if &$f($_) } }
  } else        { for (@_)                         { return $_ if &$f($_) } }
  undef
}
#END: first

#BEGIN: first_pos
sub first_pos :prototype(&@) {
  my $f = shift;
  if (ref $_[0] eq 'ARRAY') {
    if (@_ > 1) { for my $i ($_[1]..$#{$_[0]}) { return $i if &$f(local $_ = $_[0][$i]) } }
    else        { for my $i (0..$#{$_[0]})     { return $i if &$f(local $_ = $_[0][$i]) } }
  } else        { for my $i (0..$#_)           { return $i if &$f(local $_ = $_[$i]) } }
  undef
}
#END: first_pos


=head3 bucketize

 my %buckets = bucketize { block } @list;
 my %buckets = bucketize \&tagger, @list;
 my $buckets = bucketize \&tagger, @list;

Partition items into buckets given a generic tagger. Returns hash ref in
scalar context. Tagger should accept a single argument (or use C<$_>) and
should return a tag indicating the bucket to place the item in. Function is
called in list context so that the following works as expected:

 %by_file_type = bucketize { /\.([^\.]+)$/ } @images;

Also note that values are given as bound aliases, so they can also be
"cleverly" modified:

 # ("foo-bar", "foo-baz", "bip-bop")
 #  becomes: ( foo => ["bar","baz"], bip => ["bop"] )
 my %buckets = bucketize { s/^([^-]+)-//; $1 } @x;

=cut

#BEGIN: bucketize
sub bucketize :prototype(&@) {
  my ($f,%h) = (shift);
  for (@_) { my ($key) = $f->($_); push @{$h{$key}}, $_ }
  return wantarray ? %h : \%h;
}
#END: bucketize


=head3 partition

See also: List::MoreUtils part

 ($true, $false) = partition { block } @list
 ($true, $false) = partition \&test_func, @list

Partitions a list into two lists based on the truth value of a subroutine
or block. The return value is two array references, the first of which is
the elements of the original list for which the function returned true, and
the second are those elements for which the function returned false.

=cut

#BEGIN: partition
sub partition :prototype(&@) {
  my ($f,@a,@b) = (shift);
  for (@_) { $f->($_) ? push(@a,$_) : push(@b,$_) }
  return (\@a, \@b)
}
#END: partition


=head3 even_positions

 @list_2 = even_positions @list_1;
 @list_2 = even_positions \@list_1;

Returns the elements of the list that have even indices. Argument may be
list or arrayref, always returns a list of values.

=cut

=head3 odd_positions

 @list_2 = odd_positions @list_1;
 @list_2 = odd_positions \@list_1;

Returns the elements of the list that have even indices. Argument may be
list or arrayref, always returns a list of values.

=cut

#BEGIN: even_positions
sub even_positions {
  return even_positions(\@_) if @_ > 1;
  return @_ if @_ == 0 or (@_ == 1 and ref($_[0]) ne 'ARRAY');
  my $x = shift;
  @$x[map 2*$_, 0..($#{$x} >> 1)];
}
#END: even_positions

#BEGIN: odd_positions
sub odd_positions {
  return odd_positions(\@_) if @_ > 1;
  return () if @_ == 0 or (@_ == 1 and ref($_[0]) ne 'ARRAY');
  my $x = shift;
  @$x[map 2*$_-1, 1..@$x/2];
}
#END: odd_positions

=head3 suggestion_sort

See Also: Sort::ByExample

 suggestion_sort \@list, \@preferred

Returns @list sorted by the order of the objects in @preferred. All
elements are matched as strings and elements of @list that are not in
@preferred are placed at the end of the resulting list in a way that
preserves their original ordering within @list.

Notes: Undefined entries will be ignored. Only the first appearance of an
element in the C<@preferred> list will be considered. Repetitions in C<@list>
will be reduced to a single occurrence.

=cut

#BEGIN: suggestion_sort
sub suggestion_sort {
  my ($toSort, $Suggestion) = @_;
  my ($i, %sugg, @sorted) = 1;
  for (@$Suggestion) { $sugg{$_} ||= $i++ };
  for (@$toSort) {
    if (defined $sugg{$_}) { $sorted[$sugg{$_}] = $_ }
    else { $sorted[$i++] = $_ }
  }
  grep defined, @sorted;
}
#END: suggestion_sort


=head3 unique

See also: List::MoreUtils uniq

 my @u = unique @list;
 my @u = unique \@list;
 my $h = unique @list;
 my $h = unique \@list;

Takes a list (or reference to an array) and returns a list of unique (up to
stringification) objects in apparently random order. In scalar context, a
histogram (hash with objects as keys, and counts as values) is returned.

Note: List::MoreUtils::uniq preserves the original order of the elements.

=cut

#BEGIN: unique
sub unique {
  my %x;
  my $A = (@_ == 1 and ref($_[0]) eq 'ARRAY') ? $_[0] : \@_;
  return unless @$A;
  if (wantarray) { # be more efficient when we only care about existence
    undef(@x{@$A});
    return keys %x;
  } else {
    $x{$_}++ for @$A;
    return \%x;
  }
}
#END: unique


=head3 lex_sort

 lex_sort @list_of_lists
 lex_sort sub{  }, @list_of_lists

Sort the lists lexicographically element-wise. The sorting subroutine may
use the package variables C<$a> and C<$b> or may take two arguments, but
need only worry about element-wise comparison.

Example:

 lex_sort( [qw/abc ac a/], [qw/abc ab c d/], [qw/x y z/], [qw/abc ab c/] )
 # gives:
 #  ( [qw/abc ab c/],
 #    [qw/abc ab c d/],
 #    [qw/abc ac a/],
 #    [qw/x y z/]
 #  )

Similarly with numerical data using: C<sub{ $a E<lt>=E<gt> $b }>

=cut

#BEGIN: lex_sort
sub lex_sort {
    return unless @_;
    my $f = (ref($_[0]) eq 'CODE') ? shift() : sub{ $_[0] cmp $_[1] };

    # Ah, glorious Perl!
    no strict 'refs';
    no warnings 'once';
    my $caller = caller;
    local(*{$caller."::a"}) = \my $a;
    local(*{$caller."::b"}) = \my $b;
    my ($rlex,$x);

    $rlex = sub :prototype($$){
        my ($A, $B) = @_;
        return @$A <=> @$B unless @$A and @$B;
        my $idx = 0;
        while ($idx <= $#{$A}) {
            return $x if $x = $f->($a = $$A[$idx], $b = $$B[$idx]);
            return  1 if ++$idx >= @$B;
        }
        return 0;
    };

    sort $rlex @_;
}
#END: lex_sort



#-----------------------------------------------------------------
                       $EXPORT_TAGS{patterns}
                                 =
[qw/$_re_int $_re_num $_re_exp $_re_wrd is_int is_num is_float
    is_word readonly like_array like_hash like_scalar
    $_re_image_ext is_image_file/];
#-----------------------------------------------------------------

=head2 :patterns - Tests and Patterns

=head3 $_re_int

Pattern which matches an integer expression. Beware, this pattern allows
whitespace in the string which perl may not allow when interpreting strings
as numbers. You may need to remove all whitespace from strings which match
this pattern.

=cut

=head3 $_re_num

Pattern which matches an floating-point expression. Beware, this pattern
allows whitespace in the string which perl may not allow when interpreting
strings as numbers. You may need to remove all whitespace from strings
which match this pattern.

=cut

=head3 $_re_exp

Pattern which matches an exponent part (Ex: S<2.3 e -10>) of a
floating-point expression. Beware, this pattern allows whitespace in the
string which perl may not allow when interpreting strings as numbers. You
may need to remove all whitespace from strings which match this pattern.

=cut

=head3 $_re_wrd

Pattern which matches safe "word-like" data. This pattern does not match
whitespace and most punctuation but does allow hyphens "-" and underscores.

=cut

#BEGIN: $_re_int, 1 line
our $_re_int = '[\+\-]?\s*\d+';
#BEGIN: $_re_exp, 1 line; depends: $_re_int
our $_re_exp = '[eE]\s*'.$_re_int;
#BEGIN: $_re_num, 1 line; depends: $_re_exp
our $_re_num = '[\+\-]?\s*(?:\d*\.\d+|\d+\.?\d*)(?:'.$_re_exp.')?';
#BEGIN: $_re_wrd, 1 line
our $_re_wrd = '[\w\-]+';

=head3 is_int

Returns a true value if the argument looks like an integer expression. If
no argument is provided, C<$_> is examined. Beware, this subroutine allows
whitespace in the string which perl may not allow when interpreting strings
as numbers. You may need to remove all whitespace from strings for which
this subroutine returns true.

=cut

=head3 is_num

Returns a true value if the argument looks like a floating-point (or
integer) expression. If no argument is provided, C<$_> is examined. Beware,
this subroutine allows whitespace in the string which perl may not allow
when interpreting strings as numbers. You may need to remove all whitespace
from strings for which this subroutine returns true.

=cut

=head3 is_float

Returns a true value if the argument looks like a floating-point (or
integer) expression. If no argument is provided, C<$_> is examined. Beware,
this subroutine allows whitespace in the string which perl may not allow
when interpreting strings as numbers. You may need to remove all whitespace
from strings for which this subroutine returns true.

=cut

=head3 is_word

Returns a true value if the argument looks like a word. If no argument is
provided, C<$_> is examined. Words do not have spaces and do not typically
have punctuation, though hyphens "-" and underscores are allowed.

=cut

#BEGIN: is_num, 1 line; depends: str $_re_num
sub is_num { @_ ? str($_[0]) =~ /^\s*$_re_num\s*$/o : str($_) =~ /^\s*$_re_num\s*$/o }
#BEGIN: is_float, 1 line; depends: str $_re_num
sub is_float { @_ ? str($_[0]) =~ /^\s*$_re_num\s*$/o : str($_) =~ /^\s*$_re_num\s*$/o }
#BEGIN: is_int, 1 line; depends: str $_re_int
sub is_int { @_ ? str($_[0]) =~ /^\s*$_re_int\s*$/o : str($_) =~ /^\s*$_re_int\s*$/o }
#BEGIN: is_word, 1 line; depends: str $_re_wrd
sub is_word { @_ ? str($_[0]) =~ /^$_re_wrd$/o : str($_) =~ /^$_re_wrd$/o }


=head3 $_re_image_ext

Pattern which matches image-type file name extensions. The list of
extensions matched (case insensitive) are:

BMP CMYK CMYKA DCM DCX DIB DPS DPX EPI EPS EPS2 EPS3 EPSF EPSI EPT FAX FITS
FPX G3 GIF GIF87 GRAY ICB ICM ICO ICON IPTC JBG JBIG JP2 JPC JPEG JPG MAP
MIFF MNG MONO MPC MTV MVG OTB P7 PAL PALM PBM PCD PCDS PCL PCT PCX PDB PGM
PICON PICT PIX PLASMA PNG PNM PPM PSD PTIF RAS RGB RGBA RLA RLE ROSE SGI
SUN SVG TGA TIF TIFF UYVY VDA VICAR VID VIFF VST WBMP X XBM XC XCF XPM XV
XWD YUV

=cut

#BEGIN: $_re_image_ext, 1 line
our $_re_image_ext = '(?i:bmp|cmyk|cmyka|dcm|dcx|dib|dps|dpx|epi|eps|eps2|eps3|epsf|epsi|ept|fax|fits|fpx|g3|gif|gif87|gray|icb|icm|ico|icon|iptc|jbg|jbig|jp2|jpc|jpeg|jpg|map|miff|mng|mono|mpc|mtv|mvg|otb|p7|pal|palm|pbm|pcd|pcds|pcl|pct|pcx|pdb|pgm|picon|pict|pix|plasma|png|pnm|ppm|psd|ptif|ras|rgb|rgba|rla|rle|rose|sgi|sun|svg|tga|tif|tiff|uyvy|vda|vicar|vid|viff|vst|wbmp|x|xbm|xc|xcf|xpm|xv|xwd|yuv)';


=head3 is_image_file

Returns a true value if the argument looks like an image file. If no
argument is provided, C<$_> is examined. The ist of extensions matched
(case insensitive) are:

BMP CMYK CMYKA DCM DCX DIB DPS DPX EPI EPS EPS2 EPS3 EPSF EPSI EPT FAX FITS
FPX G3 GIF GIF87 GRAY ICB ICM ICO ICON IPTC JBG JBIG JP2 JPC JPEG JPG MAP
MIFF MNG MONO MPC MTV MVG OTB P7 PAL PALM PBM PCD PCDS PCL PCT PCX PDB PGM
PICON PICT PIX PLASMA PNG PNM PPM PSD PTIF RAS RGB RGBA RLA RLE ROSE SGI
SUN SVG TGA TIF TIFF UYVY VDA VICAR VID VIFF VST WBMP X XBM XC XCF XPM XV
XWD YUV

=cut

#BEGIN: is_image_file; depends: str $_re_image_ext
sub is_image_file  {
  my $pat = reverse substr($_re_image_ext, 4, -1);
  my $str = reverse( @_ ? str($_[0]) : str($_) );
  $str =~ /^(?:$pat)\./oi;
}
#END: is_image_file


=head3 readonly

Returns true if scalar argument is readonly. (Taken from Scalar::Util.)

=cut

#BEGIN: readonly
# Taken straight from Scalar::Util
sub readonly {
  return 0 if tied($_[0]) || (ref(\($_[0])) ne "SCALAR");

  local($@, $SIG{__DIE__}, $SIG{__WARN__});
  my $tmp = $_[0];

  !eval { $_[0] = $tmp; 1 };
}
#END: readonly

=head3 like_array

Returns true if the object can behave like an array. (This is just a nicer
way to call UNIVERSAL::isa)

=cut

=head3 like_hash

Returns true if the object can behave like a hash. (This is just a nicer
way to call UNIVERSAL::isa)

=cut

=head3 like_scalar

Returns true if the object can behave like a scalar. (This is just a nicer
way to call UNIVERSAL::isa)

=cut

#BEGIN: like_array
sub like_array :prototype($) {
  require UNIVERSAL;
  UNIVERSAL::isa($_[0],'ARRAY');
}
#END: like_array

#BEGIN: like_hash
sub like_hash :prototype($) {
  require UNIVERSAL;
  UNIVERSAL::isa($_[0],'HASH');
}
#END: like_hash

#BEGIN: like_scalar
sub like_scalar :prototype($) {
  require UNIVERSAL;
  UNIVERSAL::isa($_[0],'SCALAR');
}
#END: like_scalar



#-----------------------------------------------------------------
                        $EXPORT_TAGS{parse}
                                 =
[qw/str2hash unformat parse_user_agent parse_debian_control_format/];
#-----------------------------------------------------------------

=head2 :parse - General Interpreters / Parsers

=head3 parse_debian_control_format

Parses text in Debian Control file format
(http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-controlsyntax).
Returns an arrayref of records (one for each paragraph).

=cut

#BEGIN: parse_debian_control_format
sub parse_debian_control_format {
  my $para = {};
  my $field;
  my @rec;

  for (split /\n/,$_[0]) {
    if (s/^([\w-]+):\s*//) {
      $field = $1;
      $$para{$field} = $_ if length;
    }

    elsif (/^\s*$/) {
      if (%$para) {
        push @rec, $para;
        $para = {};
        undef $field;
      }
    }

    elsif (/^ \.\s*$/) {
      die "Parse Error" unless defined $field;
      $$para{$field} .= "\n";
    }

    elsif (s/^ //) {
      die "Parse Error" unless defined $field;
      $$para{$field} .= "\n" if length($$para{$field}||"");
      $$para{$field} .= $_;
    }
  }

  push @rec, $para if %$para;
  return \@rec;
}
#END: parse_debian_control_format

=head3 parse_user_agent

WARNING: This function is out of date, naïve, and generally broken. See
HTTP::BrowserDetect for a more up-to-date solution. I intend to eventually
send some patches or provide a wrapper (HTTP::BrowserDetect::Practical(?))
to that module (for instance, it is my belief that agent strings like
"msie" should be reported as a bot).


 my $hashref = parse_user_agent( $string );
 my %hash    = parse_user_agent( $string );

Given a user-agent string returns a hash containing the following fields.
Fields which can not be determined are left undefined.

=over 4

=item generic_os

Returns the generic operating system type: Windows, Mac, OS2, Linux, UNIX

=item os

Returns the specific operating system type: Windows Vista, Windows Server
2003, Windows XP, Windows 2000, Debian, ...

=item type

One of: browser, textbrowser, bot, downloader, mobile

Note: For this field, we try to make our best guess at which class the
agent string fits into.

=item program

Quasi-canonicalized program name: Internet Explorer, Netscape, Mozilla,
Firefox, wget, ...

=item version

Our best guess at the program version

=item engine

The Browser's rendering engine: Gecko, KHTML, MSHTML, Presto (opera),
WebCore (apple), custom (other custom engines)

=item engine-version

The version of the rendering engine

=item user-agent

The unmodified user-agent string

=item obsolete

If true, the agent appears to be an obsolete web browser

=back

=cut

#BEGIN: parse_user_agent, DEPENDS: fappend
{
my %kw =
( sie => "Siemens",
  sonyericsson => "SonyEricsson",
  ericsson => "SonyEricsson",
  blackberry => "BlackBerry",
  mot => "Motorola",
  palmos => "Palm OS",
  symbian => "Symbian OS",
  j2me => "Java Platform Micro Edition",
  beos => "BeOS",
  openvms => "OpenVMS",
  irix => "IRIX",
  sunos => "Solaris",
  seamonkey => "SeaMonkey",
  iceape => "IceApe",
  netscape6 => "Netscape",
  mdk => "Mandrake",
  iphone => "iPhone",
  "mac os x" => "Mac OS X",
  "america online browser" => "AOL Browser",
);
my %winver =
( "nt 6.0" => "Windows Vista",
  "nt 5.2" => "Windows Server 2003",
  "nt 5.1" => "Windows XP",
);
sub parse_user_agent {
  return wantarray ? (qw/type bot user-agent -/) : {qw/type bot user-agent -/} if $_[0] eq '-';
  my %ua = ("user-agent" => $_[0]);
  my $guessed_type;
  local $_ = lc shift;
  study;

  # OPERATING SYSTEM
  #-----------------
  if (/windows(?: (nt(?: \d\.\d)?))?/) {
    $ua{generic_os} = 'Windows';
    unless ($ua{os} = $winver{$1||""}) {
      $ua{os} = "Windows (old)";
      $ua{obsolete} = 1;
    }
  }
  elsif (/OS\/2/)                      { $ua{generic_os} = 'OS/2';    $ua{os} = 'OS/2';               $ua{obsolete} = 1; }

  elsif (/(android \d+\.\d+)/)
    { $ua{generic_os} = "Android"; $ua{os} = ucfirst $1; $ua{type} = 'mobile'; }

  elsif (/iphone os (\d+)_(\d+)/)
    { $ua{generic_os} = "iPhone"; $ua{os} = "iPhone $1.$2"; $ua{type} = 'mobile'; }

  elsif (/linux/) {
    $ua{generic_os} = "Linux";
    if (/\b(gentoo|debian|fedora|ubuntu|redhat|slackware|mdk|kanotix|suse|lycoris|knoppix|centos)\b/) {
      $ua{os} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1;
    }
  }

  elsif (/\b(freebsd|openbsd|netbsd)\b/) { $ua{generic_os} = "BSD"; $ua{os} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; }

  elsif (/(macintosh|mac os x|mac os|macos|mac_\w+|mc68|\bmac\b)/)
    { $ua{generic_os} = "Mac OS"; $ua{os} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{obsolete} = 1 unless $ua{os} eq 'Mac OS X'; }

  elsif (/\b(blackberry|acer|philips|panasonic|alcatel|(?:sony)?ericsson|samsung|sie|mot|nokia|palmos|symbian)[ \-\/]?([\w.]*)\b/)
    { $ua{generic_os} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{os} = "$ua{generic_os} $2"; $ua{type} = 'mobile'; }

  elsif (/\b(beos|openvms|irix|amiga|sunos|j2me)\b/)
    { $ua{generic_os} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{os} = $ua{generic_os}; }

  elsif (/X11/) { $ua{generic_os} = "UNIX"; }


  # BROWSERS
  #---------
  # Gecko-based browsers (netscape 6 and later)
  if (m!\bgecko/(\d+).*(seamonkey|firefox|bonecho|minefield|firebird|phoenix|iceape|iceweasel|camino|netscape6?|epiphany|galeon|flock|minimo|k\-meleon|k\-ninja|kazehakase)/([\d.]+)!)
    { $ua{program} = (exists $kw{$2}) ? $kw{$2} : ucfirst $2; $ua{version} = $3; $ua{type} ||= 'browser'; $ua{obsolete} = 1 if $1 < 19990000; }

  if (m!microsoft internet explorer!) { $ua{program} = 'Internet Explorer'; $ua{version} = "1.0"; $ua{type} ||= 'browser'; $ua{obsolete} = 1; }
  if (m!\bMSIE ([\d.]+)!) { $ua{program} = 'Internet Explorer'; $ua{version} = $1; $ua{type} ||= 'browser'; $ua{obsolete} = 1 if $1 < 6; }

  # Other non-obsolete browsers
  if (m!\b(shiira|omniweb|sunrisebrowser|icab|deskbrowse|safari|opera|konqueror|dillo)[ /]([\d.]+)!)
    { $ua{program} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{version} = $2; $ua{type} ||= 'browser'; }

  if (m!\b(lynx|e?links)[ /\(]+([\d.]+)!)
    { $ua{program} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{version} = $2; $ua{type} ||= 'textbrowser'; $ua{engine} = 'custom'; }

  if (m!(america online browser)[ /]([\d.]+)!)
    { $ua{program} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{version} = $2; $ua{type} ||= 'browser'; $ua{obsolete} = 1; $ua{engine} = 'custom'; }

  # Bots with versions
  if (m!\b(w3c\-checklink|python-urllib|googlebot(?:\-image)?|gigabot|w3c_(?:css_)validator(?:_[a-z]+)?|msnbot|cfetch|voyager|becomebot|grub-client|scooter|sbider|exabot)[/ \-]([\d.]*)!)
    { $ua{program} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{version} = $2; $ua{type} = 'bot'; }

  # downloaders
  if (m!\b(curl|wget|svn|apt\-http)/([\d.]+)!)
    { $ua{program} = (exists $kw{$1}) ? $kw{$1} : $1; $ua{version} = $2; $ua{type} = 'downloader'; }

  # Bots without versions
  if (m!\b(ia_archiver|adbeat.com|aol/http|kw\-lp\-suggest|pirst|ask\s+jeeves|baiduspider|gamespy|yahoo|looksmart\.net|slurp|http://[^/\s]+|\S*[Bb][Oo][Tt]\b\S*)\b! and !$ua{program})
    { $ua{program} = (exists $kw{$1}) ? $kw{$1} : ucfirst $1; $ua{type} = 'bot'; }


  # ENGINES
  #--------
  if    (m!gecko/([0-9]+)!)       { $ua{engine} = 'Gecko';  $ua{"engine-version"} = $1; delete $ua{obsolete} }
  elsif (m!khtml/([\d.]+)!)       { $ua{engine} = 'KHTML';  $ua{"engine-version"} = $1; delete $ua{obsolete} }
  elsif (m!applewebkit/([\d.]+)!) { $ua{engine} = 'WebKit'; $ua{"engine-version"} = $1; delete $ua{obsolete} }

  if (exists($ua{program}) and $ua{program} eq 'Opera' and exists($ua{version})) {
    if ($ua{version} < 7) { $ua{engine} = 'Elektra'; $ua{obsolete} = 1 }
    else { $ua{engine} = 'Presto'; delete $ua{obsolete} }
  }

  if (exists($ua{program}) and $ua{program} eq 'Internet Explorer' and exists($ua{version})) {
    if    ($ua{version} < 5)     { $ua{engine} = 'Trident'; $ua{"engine-version"} = 1; $ua{obsolete} = 1 }
    elsif ($ua{version} < 5.5)   { $ua{engine} = 'Trident'; $ua{"engine-version"} = 2; $ua{obsolete} = 1 }
    elsif ($ua{version} < 6)     { $ua{engine} = 'Trident'; $ua{"engine-version"} = 3; $ua{obsolete} = 1 }
    elsif ($ua{version} < 7)     { $ua{engine} = 'Trident'; $ua{"engine-version"} = 4; $ua{obsolete} = 1 }
    elsif ($ua{version} =~ /^7/) { $ua{engine} = 'Trident'; $ua{"engine-version"} = 5; $ua{obsolete} = 1 }

    else { $ua{engine} = 'Trident'; }
  }

  if (exists($ua{program}) and $ua{program} eq 'Konqueror' and exists($ua{version}) and !exists($ua{engine})) {
    $ua{engine} = 'KHTML';  $ua{"engine-version"} = $ua{version}; delete $ua{obsolete};
  }

  # Old mozilla versions
  if (m|mozilla/([1234]\.\d+)|) {
    $ua{obsolete} = 1;
    if(!exists($ua{type}) and !exists($ua{program}) and (exists($ua{os}) or exists($ua{generic_os}))) {
      $ua{type} = 'browser';   $ua{program} = "Netscape"; $ua{version} = $1;
      $ua{engine} = "Mozilla"; $ua{"engine-version"} = $1;
    }
  }

  $ua{type} = 'mobile' if /\bmobile\b/ and $ua{type} eq 'browser';

  if (!exists($ua{type})) {
    if (exists($ua{engine}))         { $guessed_type = $ua{type} = /mobile/ ? "mobile" : 'browser'; }
    elsif (exists($ua{generic_os}))  { $guessed_type = $ua{type} = /mobile/ ? "mobile" : 'browser'; }
    else { $guessed_type = $ua{type} = 'bot'; }
  }

  if ((exists($ua{type}) and $ua{type} eq 'browser' and !exists($ua{engine})) or
      ($guessed_type)
     ) {
    require YAML;
    fappend("/tmp/unrecognized_user-agent_strings", YAML::Dump(\%ua));
  }

  return wantarray ? %ua : \%ua;
}
}
#END: parse_user_agent


=head3 str2hash

Parse a string into a hash using Text::Balanced::extract_delimited. This
function recognizes perl 5 style hashes as well as the basic perl 6
adverbial form. Items missing a value will set the corresponding hash value
to true.

Example:

 str2hash 'foo, bar => "Hmmm, a comma", :baz<23>, :!bip, quxx => Spaces are fine'

Parses to:

 { foo => 1,
   bar => 'Hmmm, a comma',
   baz => 23,
   bip => 0,
   quxx => 'Spaces are fine',
 }

Unfortunately, the adverbial form will behave strangely with embedded commas:

 str2hash ':baz<Well, how odd>'

becomes

 { ':baz<Well' => 1,
   'how odd>'  => 1,
 }

=cut

#BEGIN: str2hash; depends SPLIT
sub str2hash {
    state $comma_splitter = SPLIT(qr/\s*,\s*/);
    state $pair_splitter  = SPLIT(qr/\s*=>\s*/);
    return unless defined $_[0];
    my %o;
    for ($comma_splitter->($_[0])) {
      my ($x,$y) = $pair_splitter->($_);
      if (defined $y) {
        $o{$x} = ($y =~ /^([`'"])/ and substr($y,-1) eq $1) ? substr($y, 1, -1) : $y;
      } elsif (':!' eq substr($x,0,2)) {
        $o{substr($x,2)} = 0;
      } elsif (':' eq substr($x,0,1)) {
        if ($x =~ /^:(\w+)<(.*)>$/) {
          $o{$1} = $2;
        } else {
          $o{substr($x,1)} = 1;
        }
      } else {
        $o{$x} = 1;
      }
    }
    return %o;
}
#END: str2hash


=head3 unformat

WARNING: still quite experimental!

 unformat $fmt, @strings
 unformat \%options, @strings

Attempts to reverse the actions of L<sprintf|perlfunc/sprintf> or other
formatted output (for instance date formats or apache logs). The return
value is a list of reports (see below) unless these was only a single input
string to parse in which case C<unformat> may be safely called in scalar
context.

=over 4

=item format

The format string

=item as

Specify how to return the findings. By default just a list of matched
components is returned however, we can also return the following reports:

=over 4

=item hash

A hash mapping conversions (or their corresponding names, if given) to
their corresponding strings. BEWARE KEY COLLISION

  { ~conv, str, ~conv, str, ... }

=item list

The default, the return values are each an array of strings that could have
been used to generate one of the input strings.

  [ str, str, ... ]

=item list_list

Each return value is an array of two arrays the first of which is the list
of strings returned by the "list" option. The second is the conversion
instructions giving each corresponding string.

  [ [ str, str, ... ],  [ conv, conv, ... ] ]

Note, in this case, each list of conversions is an array reference pointing
to the same array, so altering one will alter them all.

=item pairs

Each return value is a flat array of pairs:

  [ conv, str, conv, str, ... ]

=item regex

Return a regular expression that will match the given pattern. In scalar
context just the list is returned. In list context the conversions will be
returned also.

  ( regex, conv, conv, ... )

=item tuples

Each return value is an array of arrays each with two elements. First the
conversion instruction and second the string that it matched.

  [ [conv, str], [conv, str], ... ]

=back

In all cases except for the hash, the conversion instructions are the
precise ones given in the format string, including any formatting options.
For the hash however, the conversion are the simplified two-character
labels (E.g. "%s" instead of "% 35s").

Additionally, the escape '%%' is treated as a string literal '%' and will
not appear in any of the report types. A "formatted percent" (for instance
"%-05%") will pass through the conversions and will appear in the reports
if you define a special conversion for it (since we define no standard
conversion for this case).

=item conversion_aliases

A hash of aliases between conversion types. Use this to map your custom
conversion (for instance from the date formatting commands) to standard
perl conversions. Conversions of the form C<( a =E<gt> "s" )> will preserve
formatting options while aliases that start with '%' C<( Y =E<gt> "%04d" )>
will use the formatting options "04" rather than any options that may have
appeared before the "Y". (Which would presumedly cause "0035" to parse to
35.) Conversion aliases are searched before conversions or special
conversions. Once can also add aliases that include the conversion options
to override other behavior C<( '02Y' =E<gt> '%02d', Y =E<gt> 's' )>.

=item special_conversions

A hash of conversions as in the L<conversions|conversions> option but these conversions
will be added to the list of standard conversions and will be consulted
first should a standard conversion type appear in this listing.

=item conversions

A hash of conversions C<( type =E<gt> action )>. Each "type" is simply the
conversion type (E.g. the "s" in "%- 10s") and each action is a pattern
that CAPTURES (preferably non-greedily) the conversion type (for instance
C<(s =E<gt> '(.*?)')>). The action could also be a subroutine which accepts
two arguments. First the formatting options and second the conversion type.
For instance, a sub action for the "f" conversion type might convert its
arguments C<(".1", "f")> into the pattern '(\d+\.\d{1})'.

Be sure that all of your conversions produce a pattern that captures
exactly one substring.

Specifying this option replaces the built-in conversions which attempt to
reverse the standard perl conversions listed in the
L<sprintf|perlfunc/sprintf> documentation.

=item conversion_map

If defined and a hash then the conversions in the above reports will be
transformed by this hash. conversions will be first searched for in their
full form (including formatting options) both with and without their
leading '%', then searched for under only the conversions type (both with
and without the '%'). Anything not appearing in the conversion map will be
treated normally as described above.

=item conversion_pattern

 Default: '(%([^a-zA-Z%]*)([%a-zA-Z]))'

Should capture three strings. The entire conversion pattern, any formatting
options that may be present, and the conversion type. The default pattern
captures single character conversions as well as the '%' escape ("%%"). See
also the "Limitations" below.

=back

Limitations: format conversions are assumed to be one character long. That
is, conversions like "%ld" will be interpreted as "%l". This can be fixed
by altering the L<conversion_pattern|/conversion_pattern> but I don't have
the need to be careful about it. If you code up a more careful parser and
are willing to share, feel free to send it and I will add it in.

Also, no locale information is considered. sprintf considers the "LC_NUMERIC"
value to affect how numbers are formatted. We do not make such considerations
here.

=cut


#BEGIN: unformat, depends: zip, max
{ my $s_like_sub_maker = sub {
    my $pat = shift;
    sub {
      local $_ = shift;
      return "($pat)"        unless defined and length;
      return " {0,$1}($pat)" if /^ ?([1-9]\d*)$/;
      return "0{0,$1}($pat)" if /^0(\d+)$/;
      return "($pat) {0,$1}" if /^\- ?(\d+)$/;
      return "$1*($pat)"     if /^( |0)?\*$/;
      return "($pat)$1*"     if /^\-( |0)?\*$/;
      return;
    };
  };

  my $replace_null = sub { my ($str, $repl) = @_; $str =~ s/\0/$repl/g; $str; };
  my $e_like_sub_maker = sub {
    my $pat    = shift;
    my $simple = $s_like_sub_maker->($replace_null->($pat,"0,"));
    sub {
      local $_ = shift;
      my $ret  = $simple->($_);
      return $ret if defined $ret;
      return "(".$replace_null->($pat,$1).")"        if /^\.(\d+)$/;
      return " {0,$1}(".$replace_null->($pat,$2).")" if /^ ?([1-9]\d*)\.(\d+)$/;
      return "0{0,$1}(".$replace_null->($pat,$2).")" if /^0(\d+)\.(\d+)$/;
      return "(".$replace_null->($pat,$2).") {0,$1}" if /^\- ?(\d+)\.(\d+)$/;
      return "$1*(".$replace_null->($pat,$2).")"     if /^( |0)?\*\.(\d+)$/;
      return "(".$replace_null->($pat,$2).")$1*"     if /^\-( |0)?\*\.(\d+)$/;
      return;
    };
  };

  # XXX: Off by one error! Need $1-1 (since must have a digit), though it isn't that careful of a measurement anyway.
  my $d_like_sub_maker = sub {
    my $pat = shift;
    sub {
      local $_ = shift;
      return "(\\-?$pat)"           unless defined and length;
      return " {0,$1}(\\-?$pat)"    if /^([1-9]\d*)$/;
      return "  {0,$1}(\\-?$pat)"   if /^\+? +([1-9]\d*)$/;
      return " {0,$1}([+-]$pat)"    if /^ *\+([1-9]\d*)$/;

      return "0{0,$1}(\\-?$pat)"    if /^0(\d+)$/;
      return " 0{0,$1}(\\-?$pat)"   if /^\+?(?: 0|0 ) *(\d+)$/;
      return "([+-]0{0,$1}$pat)"    if /^(?: 0|0 ) *\+(\d+)$/;

      return "(\\-?$pat) {0,$1}"    if /^\-(\d+)$/;
      return " (\\-?$pat) {0,$1}"   if /^ *(?: \-|\- ) *(\d+)$/;
      return "([+-]$pat) {0,$1}"    if /^(?:\-\+|\+\-)(\d+)$/;

      return " *(\\-?$pat)"         if /^\*\s*$/;
      return "(\\-?$pat) *"         if /^\-\*\s*$/;
      return "([+-]$pat) *"         if /^(?:\-\+|\+\-)\*$/;
      return " *([+-]$pat)"         if /^ *\+\*$/;
      return;
    };
  };

  # XXX: Will be incorrect for %.0f
  my $f_like_sub_maker = sub {
    my $pat    = shift;
    my $simple = $d_like_sub_maker->($replace_null->($pat,"0,"));
    sub {
      local $_ = shift;
      my $ret  = $simple->($_);
      return $ret if defined $ret;
      return "(\\-?".$replace_null->($pat,$1).")"           if /^\.(\d+)$/;
      return " {0,$1}(\\-?".$replace_null->($pat,$2).")"    if /^([1-9]\d*)\.(\d+)$/;
      return "  {0,$1}(\\-?".$replace_null->($pat,$2).")"   if /^\+? +([1-9]\d*)\.(\d+)$/;
      return " {0,$1}([+-]".$replace_null->($pat,$2).")"    if /^ *\+([1-9]\d*)\.(\d+)$/;

      return "0{0,$1}(\\-?".$replace_null->($pat,$2).")"    if /^0(\d+)\.(\d+)$/;
      return " 0{0,$1}(\\-?".$replace_null->($pat,$2).")"   if /^\+?(?: 0|0 ) *(\d+)\.(\d+)$/;
      return "([+-]0{0,$1}".$replace_null->($pat,$2).")"    if /^(?: 0|0 ) *\+(\d+)\.(\d+)$/;

      return "(\\-?".$replace_null->($pat,$2).") {0,$1}"    if /^\-(\d+)\.(\d+)$/;
      return " (\\-?".$replace_null->($pat,$2).") {0,$1}"   if /^ *(?: \-|\- ) *(\d+)\.(\d+)$/;
      return "([+-]".$replace_null->($pat,$2).") {0,$1}"    if /^(?:\-\+|\+\-)(\d+)\.(\d+)$/;

      return " *(\\-?".$replace_null->($pat,$2).")"         if /^\*\.(\d+)$/;
      return "(\\-?".$replace_null->($pat,$2).") *"         if /^\-\*/;
      return "([+-]".$replace_null->($pat,$2).") *"         if /^(?:\-\+|\+\-)\*$/;
      return " *([+-]".$replace_null->($pat,$2).")"         if /^ *\+\*$/;
      return;
    };
  };

my $DEBUG = 0;
sub unformat {
    state $conversions;
    unless ($conversions) {
        $conversions = {
            s => $s_like_sub_maker->('.*?'),
            # XXX: length issues. Not sure how long these can be...
            b => $s_like_sub_maker->('[01]+?'),
            u => $s_like_sub_maker->('[0-9]+?'),
            o => $s_like_sub_maker->('[0-7]+?'),
            x => $s_like_sub_maker->('[0-9a-f]+?'),
            X => $s_like_sub_maker->('[0-9A-F]+?'),
            d => $d_like_sub_maker->('\d+'),
            e => $e_like_sub_maker->("\\-?\\d\\.\\d{\0}e[+-]\\d+"),
            E => $e_like_sub_maker->("\\-?\\d\\.\\d{\0}E[+-]\\d+"),
            f => $f_like_sub_maker->("\\d+\\.\\d{\0}"),

            g => $f_like_sub_maker->("\\d+\\.\\d{\0}(?:e[+-]\\d+)?"), # XXX: rather broken, but will probably work in the common case
            G => $f_like_sub_maker->("\\d+\\.\\d{\0}(?:E[+-]\\d+)?"), # XXX: rather broken, but will probably work in the common case
        };
        @$conversions{qw/i D U O F p/} = @$conversions{qw/d d u o f x/};
    }

  $DEBUG && (local $\ = "\n");
  my %o;
  if (ref($_[0]) eq 'HASH') { %o = %{ shift() } }
  else                      { %o = ( format => shift ) }
  $o{conversion_pattern} = '(%([^a-zA-Z%]*)([%a-zA-Z]))' unless exists $o{conversion_pattern};
  $o{conversions} = $conversions unless exists $o{conversions};

  my @format = split /$o{conversion_pattern}/, $o{format};
  my $map = $o{conversion_map};
  my $pat = '';
  my @conv;

  while (@format) {
    my $oconv = shift @format;
    $DEBUG && print "Examining component: '$oconv'";
    unless (substr($oconv,0,1) eq '%') {
      $pat .= quotemeta($oconv);
      $DEBUG && print "  string literal -> pat = $pat";
      next;
    }
    if ($oconv eq '%%') { # This is boring and is skipped
      $DEBUG && print "  literal %";
      $pat .= '%';
      next;
    }

    my ($oopt, $otype) = splice(@format, 0, 2);
    my ($conv, $opt, $type) = ($oconv, $oopt, $otype);

    # Aliases
    for ($oopt.$otype, $otype) {
      next unless exists $o{conversion_aliases}{$_} and defined $o{conversion_aliases}{$_};
      ($conv, $opt, $type) = substr($o{conversion_aliases}{$_},0,1) eq '%' ?
                                ($o{conversion_aliases}{$_} =~ /$o{conversion_pattern}/) :
                                ("%".$oopt.$o{conversion_aliases}{$_}, $oopt, $o{conversion_aliases}{$_});
      $DEBUG && print "  has alias $oconv -> $conv";
      last; # stop when we get a hit.
    }

    # Conversions
    my $conversion_pattern;
    for (@o{qw/special_conversions conversions/}) {
      next unless exists $$_{$type} and defined $$_{$type};
      if (ref($$_{$type}) eq 'CODE') {
        $conversion_pattern = $$_{$type}->($opt, $type);
      } elsif (ref($$_{$type}) eq 'Regexp' or !ref($$_{$type})) {
        $conversion_pattern = $$_{$type};
      } else {
        croak "Invalid conversion handler in "."unformat: '$type' => $$_{$type}";
      }
      $DEBUG && print "  converted '$type' -> '$conversion_pattern'";
      last;
    }

    unless (defined $conversion_pattern) {
      croak "Invalid conversion in "."unformat: '$oconv'" if $oconv eq $conv;
      croak "Invalid conversion in "."unformat: '$oconv' (aliased to '$conv')";
    }

    $pat .= $conversion_pattern;
    next if !$o{as} or $o{as} eq 'list';
    my $flag;
    if ($map) {
      for ($oconv, substr($oconv, 1), '%'.$otype, $otype) {
        next unless exists $$map{$_};
        $oconv = $$map{$_};
        $flag = 1;
        last;
      }
    }
    if (!$flag and $o{as} and $o{as} eq 'hash') {
      $oconv = '%'.substr($oconv,-1);
    }
    push @conv, $oconv;
  }

  # DO IT!
  $DEBUG && print "FINAL PATTERN: ^$pat\$";
  if ($o{as} and $o{as} eq 'regex') {
    return wantarray ? ($pat, @conv) : $pat;
  }
  my $match = qr/^$pat$/;
  my @res   = map [$_ =~ $match], @_;

  # Generate the report
  if (!$o{as} or $o{as} eq 'list') {
    1; # done, we'll just return res as it is.
  }
  elsif ($o{as} eq 'hash') {
    @res = map +{ @{$res[$_]} ? zip( \@conv, $res[$_] ) : () }, 0..$#res;
  }
  elsif ($o{as} eq 'list_list') {
    @res = map +( @{$res[$_]} ? [$res[$_], \@conv] : []), 0..$#res;
  }
  elsif ($o{as} eq 'pairs') {
    @res = map +( @{$res[$_]} ? [zip(\@conv, $res[$_])] : []), 0..$#res;
  }
  elsif ($o{as} eq 'tuples') {
    for my $a (0..$#res) {
      $res[$a] = [ @{$res[$a]} ? (map [$conv[$_], $res[$a][$_]], 0..max($#conv, $#{$res[$a]})) : () ];
    }
  }
  else {
    croak "Invalid report format for "."unformat: $o{as}";
  }

  if (@_ == 1 and !wantarray) { return $res[0] }
  else                        { return @res    }
}
}
#END: unformat



#-----------------------------------------------------------------
                    $EXPORT_TAGS{canonicalize}
                                 =
[qw/str replace_windows_characters strip_space sign nsign
    canonicalize_newlines canonicalize_newlines_copy
    canonicalize_timeword qbash qbsbash stringify simple_range2list
    glob2regexp canonicalize_filename trim uri_rel2abs uri_rel2abs_fast
    length2pt nicef rtf2txt commify approx_date nice_date nice_time nice_datetime
    decode_english decode_first
    /];
#-----------------------------------------------------------------

=head2 :canonicalize - Canonicalization

=head3 decode_english

 $text = decode_english($text);

Ensures that text is a decoded string. First tries to observe any
Byte-Order Marks and decode appropriately (then removes the BOM).
Otherwise, attempts to interpret as UTF-8 byte string, then as Windows-1252
byte string. Will throw an error if the parsing yields any control
characters (Specifically: C<[\x00-\x08\x0C\x0E-\x1F\x7f]>).

For English text, this should usually do what you want, though, we
unfortunately can't detect UTF-16 unless it contains a BOM.

=cut

#BEGIN: decode_english
sub decode_english {
    use Encode;
    my $content = shift;

    return $content if Encode::is_utf8($content);

    # Look for Byte-Order Marks
    return substr(decode("UTF-8",     $content, Encode::FB_CROAK), 1) if substr($content, 0, 3) eq "\xEF\xBB\xBF";
    return substr(decode("UTF-16BE",  $content, Encode::FB_CROAK), 1) if substr($content, 0, 2) eq "\xFE\xFF";
    return substr(decode("UTF-16LE",  $content, Encode::FB_CROAK), 1) if substr($content, 0, 2) eq "\xFF\xFE";

    my $decoded;
    for my $enc (qw/ UTF-8 cp1252 /) {
        return $decoded if eval { $decoded = decode($enc, $content, Encode::FB_CROAK); 1 } and $decoded !~ /[\x00-\x08\x0C\x0E-\x1F\x7f]/;
    }

    die "Unable to decode '$content' appears to be neither UTF-8 nor cp1252";
}
#END: decode_english

=head3 decode_first

 $text = decode_first($text, @encodings);

Ensures that text is a decoded string. First attempts to decode using the
given encodings (in order) until one succeeds. Dies if none of the
encodings succeed.

=cut

#BEGIN: decode_first
sub decode_first {
    require Encode;
    my $content = shift;

    return $content if Encode::is_utf8($content);

    for my $enc (@_) {
        # NOTE: returns from decode_first() sub on success!
        eval { return decode($enc, $content, Encode::FB_CROAK) };
    }

    local $" = ", ";
    die "Unable to decode string! Is none of @_: $content";
}
#END: decode_first

=head3 approx_date

Format date into one of the following depending on how far away the date is:

 tomorrow
 today
 yesterday
 Fri, 30 Sep 2011
 10 Sep 2011
 Mar 2011
 2009

=cut

#BEGIN: approx_date, depends: parse_date
sub approx_date {
    my $dt = parse_date(@_);
    my $now = parse_date("now");
    my $delta = abs($now->epoch - $dt->epoch)/86400;
    return "tomorrow"                    if $dt->clone->add(days => -1)->ymd eq $now->ymd;
    return "today"                       if $dt->ymd eq $now->ymd;
    return "yesterday"                   if $dt->clone->add(days => 1)->ymd eq $now->ymd;
    # Each is about 15% error in the expected loss of accuracy
    return $dt->strftime('%a, %e %b %Y') if $delta <   20;
    return $dt->strftime('%e %b %Y')     if $delta <  200;
    return $dt->strftime('%b %Y')        if $delta < 1000;
    return $dt->strftime('%Y');
}
#END: approx_date

=head3 nice_date

My preferred date format: '%a, %e %b %Y'. Also collapses spaces. Can
override by setting C<$_Util::nice_date::format>, but why would you want
to?

=cut

#BEGIN: nice_date, depends: parse_date
$_Util::nice_date::format = '%a, %e %b %Y';
sub nice_date {
    my $dt = parse_date(@_);
    my $str = $dt->strftime($_Util::nice_date::format);
    $str =~ s/\s+/ /g;
    return $str;
}
#END: nice_date


=head3 nice_time

My preferred time format: '%l:%M%P'. Also collapses spaces. Can override by
setting C<$_Util::nice_time::format>, but why would you want to?

=cut

#BEGIN: nice_time, depends: parse_date
$_Util::nice_time::format = '%l:%M%P';
sub nice_time {
    my $dt = parse_date(@_);
    my $str = $dt->strftime($_Util::nice_time::format);
    $str =~ s/\s+/ /g;
    $str =~ s/^ //;
    return $str;
}
#END: nice_time


=head3 nice_datetime

My preferred datetime format: '%a, %e %b %Y %l:%M%P'. Also collapses
spaces. Can override by setting C<$_Util::nice_datetime::format>, but why
would you want to?

=cut

#BEGIN: nice_datetime, depends: parse_date
$_Util::nice_datetime::format = '%a, %e %b %Y %l:%M%P';
sub nice_datetime {
    my $dt = parse_date(@_);
    my $str = $dt->strftime($_Util::nice_datetime::format);
    $str =~ s/\s+/ /g;
    return $str;
}
#END: nice_datetime


=head3 commify

 my $val = commify(1234342.32);
 my $val = commify("%.2f", 1234342.3234234);

Insert commas into number. If passed two parameters, the first will be
taken as a sprintf format string which will be applied to the value before
commifying.

=cut

#BEGIN: commify
sub commify {
    local $_ = reverse( (@_ > 1) ? sprintf(@_) : pop );
    /\./g; /(?=\d)/g;# sets pos()
    s/\G(\d{3})(?=\d)/$1,/g;
    return scalar reverse $_;
}
#END: commify


=head3 rtf2txt

 rtf2txt( file => $filename_or_handle )
 rtf2txt( string => $rtf_text )

 rtf2txt( $existing_file )
 rtf2txt( $rtf_text )

=cut

#BEGIN: rtf2txt
sub rtf2txt {
  require RTF::Tokenizer;
  unshift @_, (-e $_[0]) ? "file" : "string" if @_ == 1;
  my $tokenizer = RTF::Tokenizer->new( @_ );
  my ($token_type, $argument, $parameter, $TEXT);
  my $level = 0;
  while (($token_type, $argument, $parameter) = $tokenizer->get_token()) {
    if ($token_type eq 'text') {
      $TEXT .= $argument if $level == 1;
    } elsif ($token_type eq 'control') {
      $TEXT .= "\n" if $argument eq 'par' and $level == 1;
    } elsif ($token_type eq 'group') {
      $level += $argument ? 1 : -1;
    } elsif ($token_type eq 'eof') {
      last;
#     } elsif ($token_type eq 'escape') {
#
    }
  }
  return $TEXT;
}
#END: rtf2txt


=head3 nicef

 nicef( $num, $digits )

Nicely formats sprintf("%.${digits}f", $num) by removing trailing 0's and
unnecessary decimals. C<$digits> defaults to 2. Also ensures that we do not
return -0.

=cut

#BEGIN: nicef, depends: untaint_int, round
sub nicef {
  my ($n, $d) = @_;
  $d = 2 unless defined $d;
  $d = untaint_int($d);
  local $_ = round($n, ($d ? "." . ("0"x($d-1)) . "1" : 1));
  s/0+$// if /\./; s/\.$//;
  return 0 if $_ eq '-0';
  return $_;
}
#END: nicef


=head3 length2pt

Given a string like "4in" or "2ft - 7in", return the value as a number of
points (72 points per inch). C<undef> is returned if we can't parse the
string.

Recognized units:

 pt
 in, ft, mi
 km, m, cm, mm, nm

=cut

#BEGIN: length2pt, depends: $_re_num
{ my %conv = qw/ pt 1  in 72  ft 864  mi 4561920
                 km 2834645.688  m 2834.645688  cm 28.34645688  mm 2.834645688  nm 0.000002834645688
               /;
  sub length2pt {
    local $_ = shift || 0; s/\s+//g;
    return unless /$_re_num(?:pt|in|ft|mi|[kcmn]?m|)/i;

    my $total = 0;
    while (s/($_re_num)(pt|in|ft|mi|[kcmn]?m|)//i) {
      my ($num, $units) = ($1, lc $2);
      my $size;

      if (exists $conv{$units}) {
        $size = $num * $conv{$units};
      } elsif (!length($units)) {
        $size = $num;
      } else { croak "error parsing remainder: '$1$2$_'\n"; }
      $total += $size;
    }
    return if /\d/; # Can't parse string.
    return $total;
  }
}
#END: length2pt


=head3 uri_rel2abs

 my $url = uri_rel2abs( $path, $base )

Converts a path into an absolute path based at the given base unless the
path is already absolute. Any file part of the base is ignored.

This subroutine is should be a proper rfc3986 uri parser as it is simply
calls URI-E<gt>new_abs. However, proper parsing pays a penalty in execution
time. Compare the benchmarks between uri_rel2abs and uri_rel2abs_fast:

        Rate   URI  FAST
 URI   208/s    --  -93%
 FAST 3012/s 1350%    --

=cut

#BEGIN: uri_rel2abs
sub uri_rel2abs {
  require URI;
  URI->new_abs( @_ )
}
#END: uri_rel2abs


=head3 uri_rel2abs_fast

 my $url = uri_rel2abs_fast( $path, $base )

Converts a path into an absolute path based at the given base unless the
path is already absolute. Any file part of the base is ignored.

This subroutine is not and will likely never be a reasonable implementation
of a proper rfc3986 uri parser. At the moment, however, it appears to be
"good enough" for typical web address (http, ftp, mms, ...) handling.

The uri_rel2abs function uses the URI module to properly produce an
absolute uri, however at a significant speed cost.

        Rate   URI  FAST
 URI   208/s    --  -93%
 FAST 3012/s 1350%    --

=cut

#BEGIN: uri_rel2abs_fast
{ my $scheme = '[a-zA-Z][a-zA-Z0-9\+\-.]*';
  my $host   = '[^/?]+';
  sub uri_rel2abs_fast {
    my ($path, $base) = @_;
    return $path if $path =~ /^$scheme:/o;
    if ($path =~ m|^/|) {
      return $base if $base =~ s|^($scheme:/+$host).*|$1$path|o;
    } elsif ($base =~ m|^($scheme:/+$host)/*$|o) {
      return "$1/$path";
    } else {
      return $base if $base =~ s|/[^/]*$|/$path|;
    }
  }
}
#END: uri_rel2abs_fast

=head3 glob2regexp

Constructs a regular expression pattern (string) that matches the same
patterns as the given glob. The pattern matches a whole string and is
anchored using C<^> and C<$> unless the glob ends with C<*> in which case
the trailing C<.*$> will be removed. Keep this in mind if you wish to
capture the pattern matched by the glob.

Current capabilities:

=over 4

=item Globby chars

C<*> match many chars; C<?> match one char

=item Escaping of globby chars

C<\**> matches C<'\*Hello'>, C<\\\**> matches C<"\\*Hello">

=item Grouping constructs

C<[abc]> match a character, C<[^abc]> don't match chars, C<{foo,bar}> match options

=back

Current restrictions:

=over 4

=item The globby chars '*' and '?' may not appear within grouping constructs ('[]' and '{}').

=item Can't match grouping chars in groups: '[ab\]]' does not work.

=back

=cut

#BEGIN: glob2regexp
sub glob2regexp {
  my @glob = reverse map scalar reverse, split /(\*|\?|\][^\[]+\[|\}[^\{]+\{)(?=(?:\\\\)*(?:[^\\]|$))/, reverse shift;
  @glob = grep +(defined and length), @glob;

  for (@glob) {
    if    ($_ eq '?') { $_ = '.'  }
    elsif ($_ eq '*') { $_ = '.*' }
    elsif (substr($_,0,1) eq '{' and substr($_,-1) eq '}') {
      $_ = '(?:' . join("|", map quotemeta, split /,/, substr($_,1,-1)) . ')';
    } elsif (substr($_,0,1) eq '[' and substr($_,-1) eq ']') {
      my $s = '';
      for (split /(\w\-\w|)/, substr($_,1,-1)) {
        if (/\w\-\w/) { $s .= $_ }
        else   { $s .= quotemeta }
      }
      $_ = "[$s]";
    } else { $_ = quotemeta }
  }

  if (@glob == 1 and $glob[0] eq '.*') { @glob    = ('[^.]', '.*') }
  if (@glob and $glob[0] eq '.*')      { $glob[0] = '^(?=[^.]).*' }
  else { unshift @glob, '^' }

  if (@glob and $glob[-1] eq '.*') { pop @glob }
  else { push @glob, '$' }

  join '', @glob;
}
#END: glob2regexp

=head3 str($)

Returns string form of argument (forces string context) if it is defined,
otherwise returns the empty string.

=cut

#BEGIN: str, 1 line
sub str :prototype($) {(defined$_[0])?''.$_[0]:''}

=head3 replace_windows_characters

Replaces unsightly Extended Windows characters with reasonable ASCII
equivalents.

 See: http://www.cs.tut.fi/~jkorpela/www/windows-chars.html
 See: http://search.cpan.org/~barbie/Text-Demoroniser
 (and probably a million other places)

=cut

#BEGIN: replace_windows_characters
#-----------------------------------------------------------------
# These lines fix the famous evil "Windows Characters"
#    http://www.cs.tut.fi/~jkorpela/www/windows-chars.html
sub replace_windows_characters { for (@_) {
  next unless $_;
  # 130 -- 139
  s/\x82/'/g;s/\x83/f/g;s/\x84/"/g;s/\x85/.../g;s/\x86/*/g;s/\x87/**/g;s/\x88/^/g;s|\x89|0/00|g;s/\x8a/S/g;s/\x8b/</g;
  # 140 -- 149
  s/\x8c/OE/g;s/\x8d//g;s/\x8e//g;s/\x8f//g;s/\x90//g;s/\x91/`/g;s/\x92/'/g;s/\x93/``/g;s/\x94/''/g;s/\x95/*/g;
  # 150 -- 159
  s/\x96/-/g;s/\x97/--/g;s/\x98/~/g;s/\x99/TM/g;s/\x9a/s/g;s/\x9b/>/g;s/\x9c/oe/g;s/\x9d//g;s/\x9e//g;s/\x9f/Y/g;
}}
#END: replace_windows_characters

=head3 strip_space

Remove all space from the provided argument. If the argument is undefined,
return the empty string.

=cut

#BEGIN: strip_space, 1 line
sub strip_space :prototype($) {local $_=shift; defined || return ''; s/\s+//g; $_}

=head3 sign($)

Returns "+" or "-" depending on the sign of the argument.

=cut

=head3 nsign($)

Returns "" or "-" depending on the sign of the argument.

=cut

#BEGIN: sign, 1 line
sub sign :prototype($) { ($_[0] >= 0) ? '+' : '-' }
#BEGIN: nsign, 1 line
sub nsign :prototype($) { ($_[0] >= 0) ? '' : '-' }

=head3 canonicalize_newlines

Replace CRLF, CR, LF with the Perl magic C<\n>. Arguments are modified
in-place. If no arguments are provided then C<$_> is altered instead. Any
undefined arguments are ignored. (though C<canonicalize_newlines(undef)>
will not alter C<$_>).

=cut

=head3 canonicalize_newlines_copy

Replace CRLF, CR, LF with the Perl magic C<\n>. Arguments are copied before
canonicalization. If no arguments are provided then C<$_> is used instead.
Any undefined arguments result in undefined output values.

=cut

#BEGIN: canonicalize_newlines, 1 line
sub canonicalize_newlines { @_ ? do{ defined && s/(?:\015?\012|\015)/\n/ for @_ } : defined && s/(?:\015?\012|\015)/\n/; 1 }

#BEGIN: canonicalize_newlines_copy
sub canonicalize_newlines_copy {
  my $x;
  if (@_) { return wantarray ? map { defined() ? do { $x = $_; $x =~ s/(?:\015?\012|\015)/\n/; $x } : undef } @_
                             : do  { $x = $_; $x =~ s/(?:\015?\012|\015)/\n/; $x } }
  else    { $x = $_; $x =~ s/(?:\015?\012|\015)/\n/; return $x }
}
#END: canonicalize_newlines_copy


=head3 canonicalize_timeword

Transform a reasonable (case-insensitive) abbreviations (or plural forms)
of "second", "minute", "hour", "day", "week", "month", "year" into one of
these canonical forms. Whitespace and numerical values are allowed at the
beginning of the string and will be ignored (and not included in the return
value).

NOTE: minutes are preferred over months, thus "m" will return "minute"
rather than "month".

=cut

#BEGIN: canonicalize_timeword, depends: $_re_num, str
sub canonicalize_timeword {
  local $_ = (@_) ? str(shift) : str($_);
  s/^\s*(?:$_re_num)?\s*s(?:econds?|ecs?\.?)?//io  && return "second";
  s/^\s*(?:$_re_num)?\s*mo(?:\.|nths?|ns?\.?)?//io && return "month" ; # months before minutes since
  s/^\s*(?:$_re_num)?\s*m(?:inutes?|ins?\.?)?//io  && return "minute"; # m defaults to minutes
  s/^\s*(?:$_re_num)?\s*h(?:ours?|rs?\.?)?//io     && return "hour"  ;
  s/^\s*(?:$_re_num)?\s*d(?:ays?|ys?\.?)?//io      && return "day"   ;
  s/^\s*(?:$_re_num)?\s*w(?:eeks?|ks?\.?)?//io     && return "week"  ;
  s/^\s*(?:$_re_num)?\s*y(?:ears?|rs?\.?)?//io     && return "year"  ;
  return;
}
#END: canonicalize_timeword, depends: $_re_num, str


=head3 qbash($)

Returns a string quoted for bash-like shells. The string must contain only
printable characters or whitespace, otherwise the subroutine will C<die>.
The return value is an untainted string wrapped in single quotes C<'> that
is ready (and safe) to pass to a shell.

A note on encoding:

If a string would be considered otherwise unquotable, an attempt will be
made to interpret it as encoded UTF-8. If this is successful, then the
string will be re-checked and, if acceptable, escaped and then re-encoded.
If your expressions are in some other encoding, you will need to decode
them yourself (and probably re-encode them before use).

=cut

#BEGIN: qbash
sub qbash :prototype($) {
    state $unprintable = '[^\pL\pM\pN\pP\pS\pZ[:print:]\s]';
    require Encode;
    local $_ = shift;
    if (/$unprintable/o) {
      my $utf8 = eval { !Encode::is_utf8($_) && Encode::decode( "UTF-8", $_, 1 ) };
      croak "Unquotable expression: $_" if !$utf8 or $@ or $utf8 =~ /$unprintable/o;

      # Encoded UTF-8!
      $utf8 =~ s/'/'\\''/g;
      no re 'taint';
      $utf8 =~ /^(.*)$/s and return "'".Encode::encode( "UTF-8", $1 )."'";
    }
    s/'/'\\''/g;
    no re 'taint';
    /^([\w\-\+\.\/]+)$/a and return "$1"; # Pretty print simple things
    /^(.*)$/s and return "'$1'";
}
#END: qbash


=head3 qbsbash($)

Returns a string backslash quoted for bash-like shells. The string must
contain only printable characters or whitespace, otherwise the subroutine
will C<die>. The return value is an untainted string with backslash escapes
before each dangerous character that is ready to pass to a shell.

A note on encoding:

If a string would be considered otherwise unquotable, an attempt will be
made to interpret it as encoded UTF-8. If this is successful, then the
string will be re-checked and, if acceptable, escaped and then re-encoded.
If your expressions are in some other encoding, you will need to decode
them yourself (and probably re-encode them before use).

=cut

#BEGIN: qbsbash
sub qbsbash :prototype($) {
    state $unprintable = '[^\pL\pM\pN\pP\pS\pZ[:print:]\s]';
    state $quotable    = '[^\w\-\+\.\/]';
    require Encode;
    local $_ = shift;
    if (/$unprintable/o) {
        my $utf8 = eval { !Encode::is_utf8($_) && Encode::decode( "UTF-8", $_, 1 ) };
        croak "Unquotable expression: $_" if !$utf8 or $@ or $utf8 =~ /$unprintable/o;

        # Encoded UTF-8!
        $utf8 =~ s/($quotable)/\\$1/g;
        no re 'taint';
        $utf8 =~ /^(.*)$/s and return Encode::encode( "UTF-8", $1 );
    }
    s/($quotable)/\\$1/g;
    no re 'taint';
    /^(.*)$/s and return $1;
}
#END: qbsbash


=head3 stringify

 stringify( $thing, %options )

Stringifies Perl objects (SCALAR, HASH, or ARRAY based). Stringifies only a
single object at a time, and accepts the options below. Note: CODE, GLOB,
LVALUE, and Regexp references are not supported.

=over 4

=item stringify_underlying_object

By default, overloaded stringification will be respected. Set this option
to true to stringify the underlying object rather than use its overload
function.

=item list_type

List which describes how lists are translated.

 DEFAULT: [ "[", ",", "]" ]

=item hash_type

List which describes how hashes are translated.

 DEFAULT: [ "{", "=>", ",", "}" ]

=back

=cut

#BEGIN: stringify, depends: str
sub stringify {
  my $data = shift;
  my $ref  = ref($data);

  return str($data) if !$ref;
  my %opt = @_;

  if (!$opt{stringify_underlying_object} and $ref !~ /(?:SCALAR|ARRAY|HASH|CODE|REF|GLOB|LVALUE|Regexp)/) {
    # Is an interesting object, does it overload stringification?
    require overload;
    return "$data" unless overload::StrVal($data) eq "$data";
  }

  if (UNIVERSAL::isa( $data, "SCALAR" ) or UNIVERSAL::isa( $data, "REF" )) {
    # XXX: perhaps not the right thing to do?
    return stringify($$data, %opt);
  }

  if (UNIVERSAL::isa( $data, "ARRAY" )) {
    $opt{list_type} ||= ["[", ",", "]"];
    return $opt{list_type}->[0].join($opt{list_type}->[1], map(stringify($_, %opt), @$data)).$opt{list_type}->[2];
  }

  if (UNIVERSAL::isa( $data, "HASH" )) {
    $opt{hash_type} ||= ["{", "=>", ",", "}"];
    return $opt{hash_type}->[0] .
               join($opt{hash_type}->[2],
                    map( stringify($_, %opt).$opt{hash_type}->[1].stringify($$data{$_}, %opt), keys %$data)) .
           $opt{hash_type}->[3];
  }
  return;
}
#END: stringify


=head3 simple_range2list

 simple_range2list @ranges

Expand "#,#..#,#-#,a..z,a-z,2:23,2:5:23,a:5:zz" strings to lists. Beginning
ending blocks may be anything matching C<[\w\.]+>, though I'm not sure how
well underscores will behave. Commas may separate multiple range chunks.

A plain value C<v> (numerical or non-numerical) will produce the range
C<1..v> or C<'a'..v>.

If no step size is given, The standard perl C<..> is used to expand the range.

Ranges with step sizes are incremented by the step size (may only be
decimal valued if both start and end values are numerical) until the value
exceeds the right hand value.

For integers, see also Set::IntSpan::Fast:

 $set->add_from_string(
   { sep => qr/(?:\s*,\s*|\s+)/, range => qr/(?:\.\.|\-|\:)/ },
   $string
 );

=cut

#BEGIN: simple_range2list, depends: $_re_num
sub simple_range2list {
  my @ranges = map split(/\s*,\s*/), @_;
  my (@l, $i);
  for (@ranges) {
    if (/^([\w\.]+)(?:\.\.|\-|\:)([\w\.]+)$/) { # simple range
      push @l, $1..$2;
    } elsif (/^($_re_num)\:($_re_num)\:($_re_num)$/) { # numerical with step size
      $i = $1;
      while ("$i" <= "$3") { push @l, $i; $i += $2 }   # stringify else 1:.2:3 stops at 2.8
    } elsif (/^([\w\.]+)\:(\d+)\:([\w\.]+)$/) { # non-numerical with step size
      my @i = ($1..$3);
      push @l, @i[ map $2*$_, 0..($#i/$2) ];
    } elsif (/^($_re_num)$/) { push @l, 1..$1 }
    elsif (/^([\w.]+)$/)     { push @l, 'a'..$1 }
    else {
      require Carp;
      Carp::carp "Unable to parse simple range: '$_'";
    }
  }
  return @l;
}
#END: simple_range2list

=head3 canonicalize_filename

 canonicalize_filename $f;
 $new = canonicalize_filename $f;
 canonicalize_filename $f, %options;

Removes anything too exotic from the file name C<$f>. In void context,
C<$f> is modified, otherwise, C<$f> is left unaltered and the modified file
name is returned. In all cases the canonicalized name will be untainted.
The following options will affect the behavior of this subroutine. The
default values are shown:

=over 4

=item replacement =E<gt> ""

If a string value, invalid characters will be replaced with this value. If
a hash reference then B<characters> will be replaced by their corresponding
values. Any values not present in the replacement hash will be replaced
with the value in the 'DEFAULT' key (if present) or the empty string.

=item allow =E<gt> 'print'

Must be one of 'print', 'basic', 'ascii', or a pattern matching A SINGLE
legal character. The 'print' class will allow just about anything through
that is not a control character including unicode characters and
punctuation if your perl supports that. The 'basic' class should only allow
characters that do not require escaping or quoting in a Linux shell
(currently allows: \w-+.~%). The 'ascii' class permits regular printable
windows and MacOS safe ascii (not unicode).

=item allow_subdirs =E<gt> 1

If true, subdirectory separators will be allowed (uses File::Spec to
determine volume and directory separators for your system).

=item squash_duplicates =E<gt> 'dwim'

If false, each invalid character will be replaced separately. If the value
is 'like' then, repeated illegal values are replaced by only a single
replacement value. If the value is any true value other than 'dwim' then,
consecutive illegal values (even if they do not match) will be replaced
with the replacement value for the first illegal character in the
substring. Finally, if the value is 'dwim' then a replacement hash will
cause the "like" behavior and a replacement string will result in "true"
behavior.

Example:

 %replace = ( replacement => { ':' => "-", " " => "+" } );

 # 'dwim' default using replacement hash: gives "foo-+bar"
 canonicalize_filename( "foo: bar", allow => 'basic', %replace );

 # 'dwim' default using replacement string: gives "foo-bar"
 canonicalize_filename( "foo: bar", allow => 'basic', replacement => "-" );

=back

=cut

#BEGIN: canonicalize_filename
{ my %class = ( print => '[[:print:]\s]',
                basic => '[\w\-\+\.~%]',
                ascii => qr/[\x20\x21\x23-\x29\x2b-\x39\x3d\x40-\x5B\x5D-\x5f\x61-\x7B\x7d\x7e]/,
              );

  sub canonicalize_filename {
      state $dirchars;
      unless ($dirchars) {
          require File::Spec;
          $dirchars = File::Spec->catpath(qw/a b c/);
          $dirchars =~ tr/abc//d;
          $dirchars = '['.quotemeta($dirchars).']';
      }

    if (!defined wantarray) {
      return $_ = canonicalize_filename(@_) for $_[0];
    }

    local $_ = shift;
    my %o = ( replacement => '', allow => 'print', allow_subdirs => 1, squash_duplicates => 'dwim', @_ );
    my $pat = exists($class{$o{allow}}) ? $class{$o{allow}} : $o{allow};
    $pat = $class{print} unless defined $pat;
    if ($o{allow_subdirs}) {
      $pat = "(?!$pat|$dirchars)";
    } else {
      $pat = "(?:(?!$pat)|(?=$dirchars))";
    }

    if (ref $o{replacement}) {
      my $h = $o{replacement};
      if ($o{squash_duplicates} eq 'like' or $o{squash_duplicates} eq 'dwim') {
        s/(?:$pat(.))\1*/exists($$h{$1}) ? $$h{$1} : exists($$h{DEFAULT}) ? $$h{DEFAULT} : ''/eg;
      } elsif ($o{squash_duplicates}) {
        s/(?:$pat(.))+/exists($$h{$1}) ? $$h{$1} : exists($$h{DEFAULT}) ? $$h{DEFAULT} : ''/eg;
      } else {
        s/$pat(.)/exists($$h{$1}) ? $$h{$1} : exists($$h{DEFAULT}) ? $$h{DEFAULT} : ''/eg;
      }
    } else {
      my $r = defined($o{replacement}) ? $o{replacement} : '';
      if ($r eq '' or !$o{squash_duplicates}) {
        s/$pat./$r/g
      } elsif ($o{squash_duplicates} eq 'like') {
        s/(?:$pat(.))\1*/$r/g
      } else {
        s/(?:$pat(.))+/$r/g
      }
    }

    # We did all this work, it might as well qualify as being untainted.
    no re 'taint';
    /(.*)/ and return $1;
  }
}
#END: canonicalize_filename


=head3 trim

Trim leading/trailing whitespace. Trims C<$_> if no arguments provided. In
void context, the arguments are altered, otherwise they are not changed and
the trimmed values are returned.

=cut

# Speed Optimized: 2010-10-13, perl v5.10.1
#BEGIN: trim
sub trim {
  my ($n, $wantarray) = (scalar @_, wantarray);
  if ($n == 0 and not defined $wantarray)
    { s/^\s+//; s/\s+$//; return }
  if ($n == 0)
    { local $_ = $_; s/^\s+//; s/\s+$//; return $_ }
  if (defined $wantarray)
    { my @x = @_; for (@x) { s/^\s+//; s/\s+$//; } return $wantarray ? @x : $x[0] }

  for (@_) { s/^\s+//; s/\s+$//; }
}
#END: trim


#-----------------------------------------------------------------
                         $EXPORT_TAGS{time}
                                 =
[qw/seconds2human seconds2hms seconds2time human2seconds ymd ymd_hms
    %as_month %as_month_number now
/];
#-----------------------------------------------------------------

=head2 :time - Time Management

=head3 now

If the floating option is passed, a DateTime object will be created with no
time zone information. Otherwise, creates a DateTime object in the local time zone.

Keep in mind, time is difficult. If wall time in Eastern time zone (-0500)
is "3:11 pm" and time() == 1298664681, then:

 |------------------+------------+----------+----------------+------------------------|
 | Function         |      epoch |      mjd |         RFC822 | set_time_zone("+0300") |
 |------------------+------------+----------+----------------+------------------------|
 | now()            | 1298664681 | 55617.84 | 15:11:21 -0500 |        23:11:21 +0300  |
 | now(floating=>1) | 1298646681 | 55617.63 | 15:11:21 +0000 |        15:11:21 +0300  |
 | DateTime->now    | 1298664681 | 55617.84 | 20:11:21 +0000 |        23:11:21 +0300  |
 |------------------+------------+----------+----------------+------------------------|

Think carefully about what exactly you want.

=cut

#BEGIN: now, depends: parse_date
sub now {
  my %opt = @_;
  require DateTime;
  $opt{floating} ? parse_date("now", floating => 1) : DateTime->now(time_zone => "local")
}
#END: now

=head3 ymd

Behaves like localtime in scalar context, but returns the date as
"YYYY-MM-DD". Returns the components of that string in list context.

=cut

#BEGIN: ymd
sub ymd {
  my ($Y,$m,$d) = @_ ? (localtime($_[0]))[5,4,3] : (localtime)[5,4,3];
  $Y+=1900; $m++;
  $m = sprintf("%02i",$m);
  $d = sprintf("%02i",$d);
  return wantarray ? ($Y,$m,$d) : "$Y-$m-$d";
}
#END: ymd

=head3 ymd_hms

Behaves like localtime in scalar context, but returns the date as
"YYYY-MM-DD HH:MM:SS". Returns the components of that string in list
context. Hours are presented in 24 hour format.

=cut

#BEGIN: ymd_hms
sub ymd_hms {
  my ($Y,$M,$d,$h,$m,$s) = @_ ? (localtime($_[0]))[5,4,3,2,1,0] : (localtime)[5,4,3,2,1,0];
  $Y+=1900; $M++;
  $_ = sprintf("%02i",$_) for $M, $d, $h, $m, $s;
  return wantarray ? ($Y,$M,$d,$h,$m,$s) : "$Y-$M-$d $h:$m:$s";
}
#END: ymd_hms

=head3 seconds2human

 seconds2human( seconds, start-unit, end-unit )

Convert an arbitrary number of seconds to a "nice" human-readable form. the
second and third arguments are optional and specify the first and last time
units presented (note specifying a start unit rounds the precision of your
result to the given unit). The resulting data are separated by the value of
C<$">. Units available are: seconds, minutes, hours, days, months, and
years. If the input seconds include a decimal portion, then the seconds
value will be rounded to three places using the format C<"%.3f">.

Example:

 seconds2human 99999999, 'd', 'mos.'   # gives: "38 months 17 days"

 local $" = ', ';
 seconds2human 99999999, 'm', 'hour'   # gives: "27777 hours, 46 minutes"

=cut

#BEGIN: seconds2human, depends is_num, canonicalize_timeword
sub seconds2human {
  my %t = qw/second 60 minute 60 hour 24 day 30 month 12 year -1/; # -1 <-> infinity
  my %s = qw/second 0 minute -1 hour -2 day -3 month -4 year -5/;  # start-time offsets
  my ($x, $start, @x, $float, $i) = (shift);
  return unless is_num($x);
  $float = $x - int($x);
  $start = $s{canonicalize_timeword(shift())} || 0    if @_ and defined $_[0]; # start unit
  $t{canonicalize_timeword(shift()) || "year"} = -1   if @_ and defined $_[0]; #   end unit
  for (qw/second minute hour day month year/) {
    if ($t{$_} < 0) { unshift @x, $x." $_".(($x > 1)?"s":""); last }
    $i = $x % $t{$_} + (($float and $_ eq 'second') ? sprintf("%.3f",$float) : 0);
    unshift @x, "$i $_".(($i != 1)?"s":"") if ++$start > 0 and $i;
    last unless $x = int $x / $t{$_};
  } "@x";
}
#END: seconds2human


=head3 seconds2hms

 seconds2hms $sec
 seconds2hms $sec, $sep

Convert an arbitrary number of seconds to a "hh:mm:ss" string. The "hh"
portion of the string will always be at least two digits long (but may be
more if more than 99 hours are represented by given number of seconds.

=cut

#BEGIN: seconds2hms
sub seconds2hms {
  my ($t,$sep) = (shift, ':'); @_ && ($sep = shift);
  sprintf "%02d%s%02d%s%02d", int($t/3600), $sep, ($t/60)%60, $sep, $t%60;
}
#END: seconds2hms


=head3 seconds2time

 seconds2time $sec
 seconds2time $sec, $pad
 seconds2time $sec, %options

Convert a number of seconds (from 0 to 86400) to a "h:mm AM/PM" string. If
a second C<$pad> parameter is given, that symbol will be used to force the
hour portion to be precisely 2 characters wide (typical values are 0 and
S<" ">). You may also fully specify "pad", "AM", "PM", and "sep"
(separator, default ":") options. The AM and PM strings should include a
leading space if you want it.

=cut

#BEGIN: seconds2time
sub seconds2time {
  my $t = shift;
  my %opt = (pad => '', AM => ' AM', PM => ' PM', sep => ':');
  %opt = ( %opt, ((@_ % 2) ? (pad => shift()) : ()), @_ );
  my $h = ($t/3600)%24;
  my $m = ($t/60)%60;
  if ($h >= 22) { return sprintf "%02d%s%02d%s", $h-12, $opt{sep}, $m, $opt{PM} }
  if ($h >  12) { return sprintf "%s%s%02d%s",   $opt{pad}.($h-12), $opt{sep}, $m, $opt{PM} }
  if ($h == 12) { return sprintf "%02d%s%02d%s", $h, $opt{sep}, $m, $opt{PM} }
  if ($h >= 10) { return sprintf "%02d%s%02d%s", $h, $opt{sep}, $m, $opt{AM} }
  if ($h >  0)  { return sprintf "%s%s%02d%s", $opt{pad}.$h, $opt{sep}, $m, $opt{AM} }
  if ($h == 0)  { return sprintf "12%s%02d%s", $opt{sep}, $m, $opt{AM} }
}
#END: seconds2time


=head3 human2seconds

Converts a human-written string of a timespan expressed in various
abbreviations of seconds, minutes, hours, days, weeks, months, and years
into an integer representing the same time span in seconds.

Subroutine dies if it is incapable of parsing the input string.

Examples:

 human2seconds "3 dys. 2hr 15m"   # 260820
 human2seconds "3q 2wk"           # dies: doesn't recognize 3q

=cut

#BEGIN: human2seconds, depends: $_re_num, strip_space, strip_color
sub human2seconds {
  my %t = qw/second 0 minute 0 hour 0 day 0 week 0 month 0 year 0/;
  my $hms;
  local $_ = strip_color( @_ ? $_[0] : $_ );
  if (/^\s*$_re_num\s*$/) { return strip_space($_) }
  s/($_re_num)\s*s(?:econds?|ecs?\.?)?//io  && do{ $hms = 2; $t{second} += strip_space($1) };
  s/($_re_num)\s*mo(?:\.|nths?|ns?\.?)?//io && do{ $hms = 2; $t{month}  += strip_space($1) };# months before minutes
  s/($_re_num)\s*m(?:inutes?|ins?\.?)?//io  && do{ $hms = 2; $t{minute} += strip_space($1) };# => m defaults to minutes
  s/($_re_num)\s*h(?:ours?|rs?\.?)?//io     && do{ $hms = 2; $t{hour}   += strip_space($1) };
  s/($_re_num)\s*d(?:ays?|ys?\.?)?//io      && do{ $hms = 2; $t{day}    += strip_space($1) };
  s/($_re_num)\s*w(?:eeks?|ks?\.?)?//io     && do{ $hms = 2; $t{week}   += strip_space($1) };
  s/($_re_num)\s*y(?:ears?|rs?\.?)?//io     && do{ $hms = 2; $t{year}   += strip_space($1) };
  s/($_re_num):($_re_num):($_re_num)//o     && do{ $hms++; $t{hour}   += strip_space($1);
                                                           $t{minute} += strip_space($2);
                                                           $t{second} += strip_space($3) };
  s/($_re_num):($_re_num)//o                && do{ $hms++; $t{hour}   += strip_space($1);
                                                           $t{minute} += strip_space($2) };
  croak "Unable to parse input: '$_' remains (found $1)" if /(\d)/;
  my $t = $t{second} + 60 * $t{minute} + 3600 * $t{hour} +
          86400 * $t{day} + 604800 * $t{week} +
          2629743.8 * $t{month} + 31556926 * $t{year};

  # Try to parse time of day
  if ($hms and $hms == 1 and /am?|pm?/i) {
    return $t       if $t < 43200 and /[aA]/;
    return $t+43200 if $t < 43200 and /[pP]/;
    # Some form of 12:XX
    return $t-43200 if /[aA]/;
    return $t;
  } else { return $t }
}
#END: human2seconds

=head3 %as_month

A hash containing mappings between various months and abbreviations to
their full month names (all keys are lowercase):

  month => Month
  mon   => Month
  mon.  => Month
  ##    => Month
  #     => Month

Also includes 4 letter keys for September.

=cut

#BEGIN: %as_month
our %as_month =
qw( jan January   jan. January   1 January   01 January   january   January
    feb February  feb. February  2 February  02 February  february  February
    mar March     mar. March     3 March     03 March     march     March
    apr April     apr. April     4 April     04 April     april     April
    may May       may. May       5 May       05 May
    jun June      jun. June      6 June      06 June      june      June
    jul July      jul. July      7 July      07 July      july      July
    aug August    aug. August    8 August    08 August    august    August
    sep September sep. September 9 September 09 September september September sept September sept. September
    oct October   oct. October               10 October   october   October
    nov November  nov. November              11 November  november  November
    dec December  dec. December              12 December  december  December
  );
#END: %as_month

=head3 %as_month_number

A hash containing mappings between various months and abbreviations to
their B<two digit> month numbers (all keys are lowercase):

  month => ##
  mon   => ##
  mon.  => ##
  #     => ##

Also includes 4 letter keys for September.

=cut

#BEGIN: %as_month_number
our %as_month_number =
qw( jan 01  jan. 01  1 01  01 01  january   01
    feb 02  feb. 02  2 02  02 02  february  02
    mar 03  mar. 03  3 03  03 03  march     03
    apr 04  apr. 04  4 04  04 04  april     04
    may 05  may. 05  5 05  05 05
    jun 06  jun. 06  6 06  06 06  june      06
    jul 07  jul. 07  7 07  07 07  july      07
    aug 08  aug. 08  8 08  08 08  august    08
    sep 09  sep. 09  9 09  09 09  september 09  sept 09  sept. 09
    oct 10  oct. 10        10 10  october   10
    nov 11  nov. 11        11 11  november  11
    dec 12  dec. 12        12 12  december  12
  );
#END: %as_month_number


#-----------------------------------------------------------------
                      $EXPORT_TAGS{file_comp}
                                 =
[qw/size2bytes bytes2size size_sum size2bytes_2 size2bytes_SI bytes2size_SI size_sum_SI/];
#-----------------------------------------------------------------

=head2 :file_comp - File related computations

=head3 size_sum

Given a list of sizes (possibly negative) converts each entry to its
corresponding number of bytes, sums the values and then converts the result
back to a human readable size. Prefixes are computed base 2 (K = 1024, M =
1048576, ...).

Example:

 print size_sum qw/1.5MB -650kB -1253kB/;

=cut

#BEGIN: size_sum, depends: size2bytes, bytes2size
sub size_sum {
  my $sum=0;
  $sum += size2bytes($_) for @_;
  return bytes2size($sum)
}
#END: size_sum


=head3 size_sum_SI

DEPRECATED: size_sum now uses MB and MiB

Given a list of sizes (possibly negative) converts each entry to its
corresponding number of bytes, sums the values and then converts the result
back to a human readable size. Prefixes are treated as standard SI prefixes
(K = 1000, M = 1000000, ...).

Example:

 print size_sum_SI qw/1.5MB -650kB -1253kB/;

=cut

#BEGIN: size_sum_SI, depends: size2bytes_SI, bytes2size_SI
sub size_sum_SI {
  my $sum=0;
  $sum += size2bytes_SI($_) for @_;
  return bytes2size_SI($sum)
}
#END: size_sum_SI


=head3 size2bytes

Given a string like "4MB" or "3TiB - 400G", return the value as a number of
bytes. C<undef> is returned if we can't parse the string. Prefixes are
computed base 2 (Ki = 1024, Mi = 1048576, ...) or using standard SI
prefixes (K + 1000, M = 1000000).

=cut

#BEGIN: size2bytes, depends: $_re_num
sub size2bytes {
  local $_ = shift || 0; s/\s+//g;
  return unless /($_re_num)([bkmgtpezy]?i?)(b?)/i;

  my $total = 0;
  while (s/($_re_num)([bkmgtpezy]?i?)(b?)//i) {
    my ($num, $pow, $bits) = ($1, $2 || 'B', $3);
    $bits = 'b' if !$bits and lc($pow) ne 'b' and $pow eq lc($pow);
    my $size;

    if (uc $pow  eq 'QI') { $size = $num * 1267650600228229401496703205376 }
    if (uc $pow  eq 'RI') { $size = $num * 1237940039285380274899124224 }
    if (uc $pow  eq 'YI') { $size = $num * 1208925819614629174706176 }
    if (uc $pow  eq 'ZI') { $size = $num * 1180591620717411303424 }
    if (uc $pow  eq 'EI') { $size = $num * 1152921504606846976 }
    if (uc $pow  eq 'PI') { $size = $num * 1125899906842624 }
    if (uc $pow  eq 'TI') { $size = $num * 1099511627776 }
    if (uc $pow  eq 'GI') { $size = $num * 1073741824 }
    if (uc $pow  eq 'MI') { $size = $num * 1048576 }
    if (uc $pow  eq 'KI') { $size = $num * 1024 }

    if (uc $pow  eq 'Q') { $size = $num * 1000000000000000000000000000000 }
    if (uc $pow  eq 'R') { $size = $num * 1000000000000000000000000000 }
    if (uc $pow  eq 'Y') { $size = $num * 1000000000000000000000000 }
    if (uc $pow  eq 'Z') { $size = $num * 1000000000000000000000 }
    if (uc $pow  eq 'E') { $size = $num * 1000000000000000000 }
    if (uc $pow  eq 'P') { $size = $num * 1000000000000000 }
    if (uc $pow  eq 'T') { $size = $num * 1000000000000 }
    if (uc $pow  eq 'G') { $size = $num * 1000000000 }
    if (uc $pow  eq 'M') { $size = $num * 1000000 }
    if (uc $pow  eq 'K') { $size = $num * 1000 }

    if (   $pow  eq 'B') { $size = $num }
    if (   $pow  eq 'b') { $size = $num / 8 }
    if (   $bits eq 'b') { $size /= 8 }
    $total += $size;
  }
  return if /\d/; # Can't parse string.
  return $total;
}
#END: size2bytes

=head3 size2bytes_2

Given a string like "4MB" or "3TB - 400G", return the value as a number of
bytes. C<undef> is returned if we can't parse the string. Prefixes are
computed base 2 (K = 1024, M = 1048576, ...).

=cut

#BEGIN: size2bytes_2, depends: $_re_num
sub size2bytes_2 {
  local $_ = shift || 0; s/\s+//g;
  return unless /($_re_num)([bkmgtpezy]?i?)(b?)/i;

  my $total = 0;
  while (s/($_re_num)([bkmgtpezy]?i?)(b?)//i) {
    my ($num, $pow, $bits) = ($1, $2 || 'B', $3);
    $bits = 'b' if !$bits and lc($pow) ne 'b' and $pow eq lc($pow);
    my $size;

    if (uc $pow  eq 'Q') { $size = $num * 1267650600228229401496703205376 }
    if (uc $pow  eq 'R') { $size = $num * 1237940039285380274899124224 }
    if (uc $pow  eq 'Y') { $size = $num * 1208925819614629174706176 }
    if (uc $pow  eq 'Z') { $size = $num * 1180591620717411303424 }
    if (uc $pow  eq 'E') { $size = $num * 1152921504606846976 }
    if (uc $pow  eq 'P') { $size = $num * 1125899906842624 }
    if (uc $pow  eq 'T') { $size = $num * 1099511627776 }
    if (uc $pow  eq 'G') { $size = $num * 1073741824 }
    if (uc $pow  eq 'M') { $size = $num * 1048576 }
    if (uc $pow  eq 'K') { $size = $num * 1024 }
    if (   $pow  eq 'B') { $size = $num }
    if (   $pow  eq 'b') { $size = $num / 8 }
    if (   $bits eq 'b') { $size /= 8 }
    $total += $size;
  }
  return if /\d/; # Can't parse string.
  return $total;
}
#END: size2bytes_2


=head3 size2bytes_SI

DEPRECATED: size2bytes now uses MB and MiB

Given a string like "4MB" or "3TB - 400G", return the value as a number of
bytes. C<undef> is returned if we can't parse the string. Prefixes are
treated as standard SI prefixes (K = 1000, M = 1000000, ...).

=cut

#BEGIN: size2bytes_SI, depends: $_re_num
sub size2bytes_SI {
  local $_ = shift || 0; s/\s+//g;
  return unless /($_re_num)([bkmgtpezy]?)(b?)/i;

  my $total = 0;
  while (s/($_re_num)([bkmgtpezy]?)(b?)//i) {
    my ($num, $pow, $bits) = ($1, $2 || 'B', $3);
    $bits = 'b' if !$bits and lc($pow) ne 'b' and $pow eq lc($pow);
    my $size;

    if (uc $pow  eq 'Q') { $size = $num * 1000000000000000000000000000000 }
    if (uc $pow  eq 'R') { $size = $num * 1000000000000000000000000000 }
    if (uc $pow  eq 'Y') { $size = $num * 1000000000000000000000000 }
    if (uc $pow  eq 'Z') { $size = $num * 1000000000000000000000 }
    if (uc $pow  eq 'E') { $size = $num * 1000000000000000000 }
    if (uc $pow  eq 'P') { $size = $num * 1000000000000000 }
    if (uc $pow  eq 'T') { $size = $num * 1000000000000 }
    if (uc $pow  eq 'G') { $size = $num * 1000000000 }
    if (uc $pow  eq 'M') { $size = $num * 1000000 }
    if (uc $pow  eq 'K') { $size = $num * 1000 }
    if (   $pow  eq 'B') { $size = $num }
    if (   $pow  eq 'b') { $size = $num / 8 }
    if (   $bits eq 'b') { $size /= 8 }
    $total += $size;
  }
  return if /\d/; # Can't parse string.
  return $total;
}
#END: size2bytes_SI


=head3 bytes2size

Print a human-readable string of the form 20.4MiB from the corresponding
number of bytes (an integer). An optional second parameter specifies the
minimal digits of accuracy which is 3 by default, 1.21 but 12.1). An
optional third parameter specifies the minimum number of digits after the
decimal place to keep which is 1 by default. Prefixes are computed using
either base 2 (Ki = 1024, Mi = 1048576, ...).

=cut

#BEGIN: bytes2size, depends: max, nsign, untaint_int
{ my %sizes = qw/B 1 KiB 1024 MiB 1048576 GiB 1073741824 TiB 1099511627776
                 PiB 1125899906842624 EiB 1152921504606846976 ZiB 1180591620717411303424
                 YiB 1208925819614629174706176
                 RiB 1237940039285380274899124224
                 QiB 1267650600228229401496703205376
                /;
  sub bytes2size {
    my $B = shift;
    my $sign = nsign($B).'1';
    $B = abs($B);
    my $A = (@_)?untaint_int(shift):3;
    my $p = (@_)?shift:1;
    return sprintf "%.".($A-1)."fB", $sign * $B if $B <= 1;
    my ($unit) = grep( ($B/$sizes{$_} >= .97 and $B/$sizes{$_} < 993.28), keys %sizes );
    unless ($unit) { ($unit) = sort { $sizes{$b} <=> $sizes{$a} } keys %sizes }
    my $q = $B/$sizes{$unit};
    return $sign * $q . "$unit" unless $q=~/(\d+)\.(\d+)/;
    $p = untaint_int(max($A-length($1), $p));
    return sprintf "%.${p}f%s", $sign * $q, $unit;
  }
}
#END: bytes2size

=head3 bytes2size_SI

DEPRECATED: bytes2size now emits KiB, MiB, ...

Print a human-readable string of the form 20.4MB from the corresponding
number of bytes (an integer). An optional second parameter specifies the
minimal digits of accuracy which is 3 by default, 1.21 but 12.1). An
optional third parameter specifies the minimum number of digits after the
decimal place to keep which is 1 by default. Prefixes are treated as
standard SI prefixes (K = 1000, M = 1000000, ...).

=cut

#BEGIN: bytes2size_SI, depends: max, nsign, untaint_int
{ my %sizes = qw/B 1 KB 1000 MB 1000000 GB 1000000000 TB 1000000000000
                 PB 1000000000000000  EB 1000000000000000000  ZB 1000000000000000000000
                 YB 1000000000000000000000000
                 RB 1000000000000000000000000000
                 QB 1000000000000000000000000000000
                /;
  sub bytes2size_SI {
    my $B = shift;
    my $sign = nsign($B).'1';
    $B = abs($B);
    my $A = (@_)?untaint_int(shift):3;
    my $p = (@_)?shift:1;
    return sprintf "%.".($A-1)."fB", $sign * $B if $B <= 1;
    my ($unit) = grep( ($B/$sizes{$_} >= .97 and $B/$sizes{$_} < 993.28), keys %sizes );
    unless ($unit) { ($unit) = sort { $sizes{$b} <=> $sizes{$a} } keys %sizes }
    my $q = $B/$sizes{$unit};
    return $sign * $q . "$unit" unless $q=~/(\d+)\.(\d+)/;
    $p = untaint_int(max($A-length($1), $p));
    return sprintf "%.${p}f%s", $sign * $q, $unit;
  }
}
#END: bytes2size_SI


#-----------------------------------------------------------------
                         $EXPORT_TAGS{file}
                                 =
[qw/newer lastline fprint fprint_bu fappend fincrement cat bcat
    rofh wofh rwfh rofhz wofhz rwfhz in_and_out sed sync
    bu_open catfile realfile find fmap fgrep canonpath touch/];
#-----------------------------------------------------------------

=head2 :file - File Operations

=head3 rofh

Read only filehandle

 my $fh = rofh $filename;
 my $fh = rofh \$mode, $filename;

Simply performs an open or croak with an appropriate message. If a string
reference C<$mode> is provided as a first argument it will be taken as the
file mode (the default is "E<lt>").

=cut

#BEGIN: rofh
sub rofh {
  my $f;
  if (ref($_[0]) =~ /IO|GLOB/) {
    seek $_[0], 0, 0;
    return $_[0];
  }
  my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : "<";
  open $f, $mode, $_[0] or do { require Carp; croak "Unable to open file '$_[0]' for reading: $!"; };
  return $f;
}
#END: rofh

=head3 wofh

Write only filehandle

 my $fh = wofh $filename;
 my $fh = wofh \$mode, $filename;

Simply performs an open or croak with an appropriate message. If a string
reference C<$mode> is provided as a first argument it will be taken as the
file mode (the default is "E<gt>").

=cut

#BEGIN: wofh
sub wofh {
  my $f;
  if (ref($_[0]) =~ /IO|GLOB/) {
    seek $_[0], 0, 0;
    return $_[0];
  }
  my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : ">";
  open $f, $mode, $_[0] or do { require Carp; croak "Unable to open file '$_[0]' for writing: $!"; };
  return $f;
}
#END: wofh

=head3 rwfh

Read-write filehandle

 my $fh = rwfh $filename;
 my $fh = rwfh \$mode, $filename;

Simply performs an open or croak with an appropriate message. If a string
reference C<$mode> is provided as a first argument it will be taken as the
file mode (the default is "+E<lt>").

=cut

#BEGIN: rwfh
sub rwfh {
  my $f;
  if (ref($_[0]) =~ /IO|GLOB/) {
    seek $_[0], 0, 0;
    return $_[0];
  }
  my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : "+<";
  open $f, $mode, $_[0] or do { require Carp; croak "Unable to open file '$_[0]' for read-write: $!"; };
  return $f;
}
#END: rwfh

=head3 rofhz

Read only compressed filehandle

 my $fh = rofhz $filename;
 my $fh = rofhz \$mode, $filename;

Simply performs an open or croak with an appropriate message. Requires perl
compiled with PerlIO support (perl 5.8, I believe). The gzip PerlIO layer
is loaded with the autopop option so that uncompressed files can be open
using this function. If a string reference C<$mode> is provided as a first
argument it will be taken as the file mode (the default is
"E<lt>:gzip(autopop)").

Note: To properly decode UTF-8 files use the mode
"E<lt>:gzip(autopop):encoding(UTF-8)"

=cut

#BEGIN: rofhz
sub rofhz {
  my $f;
  if (ref($_[0]) =~ /IO|GLOB/) {
    seek $_[0], 0, 0;
    binmode $_[0], ":gzip";
    return $_[0];
  }
  my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : "<:gzip(autopop)";
  open $f, $mode, $_[0] or do { require Carp; croak "Unable to open file '$_[0]' for (possibly compressed) reading: $!"; };
  return $f;
}
#END: rofhz

=head3 wofhz

Write only compressed filehandle

 my $fh = wofhz $filename;
 my $fh = wofhz \$mode, $filename;

Simply performs an open or croak with an appropriate message. Requires perl
compiled with PerlIO support (perl 5.8, I believe). If a string reference
C<$mode> is provided as a first argument it will be taken as the file mode
(the default is "E<gt>:gzip:encoding(UTF-8)").

Note: To properly encode UTF-8 files use the mode
"E<gt>:gzip:encoding(UTF-8)"

=cut

#BEGIN: wofhz
sub wofhz {
  my $f;
  if (ref($_[0]) =~ /IO/) {
    seek $_[0], 0, 0;
    binmode $_[0], ":gzip";
    return $_[0];
  }
  my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : ">:gzip";
  open $f, $mode, $_[0] or do { require Carp; croak "Unable to open file '$_[0]' for compressed writing: $!"; };
  return $f;
}
#END: wofhz

=head3 rwfhz

Read-write compressed filehandle

 my $fh = rwfhz $filename;
 my $fh = rwfhz \$mode, $filename;

Simply performs an open or croak with an appropriate message. Requires perl
compiled with PerlIO support (perl 5.8, I believe). The gzip PerlIO layer
is loaded with the autopop option so that uncompressed files can be open
using this function. If a string reference C<$mode> is provided as a first
argument it will be taken as the file mode (the default is
"+E<lt>:gzip(autopop)").

Note: To properly decode UTF-8 files use the mode
"+E<lt>:gzip(autopop):encoding(UTF-8)"

=cut

#BEGIN: rwfhz
sub rwfhz {
  my $f;
  if (ref($_[0]) =~ /IO/) {
    seek $_[0], 0, 0;
    binmode $_[0], ":gzip";
    return $_[0];
  }
  my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : "+<:gzip(autopop)";
  open $f, $mode, $_[0] or do { require Carp; croak "Unable to open file '$_[0]' for (possibly compressed) read-write: $!"; };
  return $f;
}
#END: rwfhz

=head3 in_and_out

 my ($IN, $OUT) = in_and_out( @ARGV[0,1] );
 my ($IN, $OUT) = in_and_out( @ARGV[0,1], %options );

Open file handles for text processing. Solves typical command line:
"do_something input output" where input/output may be missing or "-" (use
STDIN/STDOUT) and input may equal output (this sub handles file copying).

Return value is a pair of filehandles ready for processing. Use binmode to
append PerlIO layers if desired.

=over 4

=item bak

String to append for backups when input == output. Defaults to: ~

=back

Example:

 my ($IN, $OUT) = in_and_out( @ARGV[0,1] );
 binmode $IN, ":encoding(UTF-8)";
 binmode $OUT, ":encoding(iso-8859-1)";
 print $OUT $_ while defined( $_ = <$IN> );

=cut

#BEGIN: in_and_out
sub in_and_out {
  my ($in, $out, %o) = @_;

  # Magic file copy when input == output. Copy input first so that output
  # file has same inode (preserve hard links). Is worth the extra disk access.
  if (defined($in) and defined($out) and length($in) and $in ne '-' and $in eq $out) {
    require File::Copy;
    $in .= $o{bak} || "~";# needs to be SOMETHING so $in ne $out
    # """as of version 2.15, cp preserves the source file's permission bits"""
    File::Copy::cp( $out, $in ) or croak "Copy of $out -> $in failed: $!";
  }

  # Default to STDIN when missing or '-'
  if (!defined($in) or !length($in) or $in eq '-') {
    Carp::cluck("WARNING: replacing if_mode '$o{if_mode}' with '<&'") if $o{if_mode};
    $in = \*STDIN;
    $o{if_mode} = '<&';
  }

  # Default to STDOUT when missing or '-'
  if (!defined($out) or !length($out) or $out eq '-') {
    Carp::cluck("WARNING: replacing of_mode '$o{of_mode}' with '<&'") if $o{of_mode};
    $out = \*STDOUT;
    $o{of_mode} = '>&';
  }

  open my $IN,  $o{if_mode} || "<", $in  or croak "Can not open '$in' for reading: $!";
  open my $OUT, $o{of_mode} || ">", $out or croak "Can not open '$out' for writing: $!";
  return ($IN, $OUT);
}
#END: in_and_out


=head3 touch

 touch @files;
 touch \MODE @files;

Create files using optional B<numeric> mode (e.g: touch \0700, "foo"). If
files exist, their atime and mtime will be updated to the current time.

=cut

#BEGIN: touch
sub touch {
  my $mode = (ref($_[0]) eq 'SCALAR') ? ${shift()} : undef;
  for (@_) {
    if (-e $_) {
      my ($atime, $mtime);
      $atime = $mtime = time;
      utime $atime, $mtime, $_;
    } else {
      open my $f, ">", $_ or croak "Error creating '$_': $!";
      close $f;
    }
    chmod $mode, $_ if defined $mode;
  }
}
#END: touch

=head3 canonpath

Like canonpath command in L<File::Spec|File::Spec>, but only works on unix
filesystems (also cygwin if $^O eq 'cygwin'). However, it will clean up
"/../" components whereas File::Spec-E<gt>canonpath will not.

The code has been modified from File::Spec::Unix::canonpath in the
PathTools package by Ken Williams.

=cut

#BEGIN: canonpath
# The following subroutine has been modified from File::Spec::Unix::canonpath
# in the PathTools package by Ken Williams. It is licensed under the same
# terms as perl itself: http://dev.perl.org/licenses/
sub canonpath {
  my $path = shift;

  # Handle POSIX-style node names beginning with double slash (qnx, nto)
  # (POSIX says: "a pathname that begins with two successive slashes
  # may be interpreted in an implementation-defined manner, although
  # more than two leading slashes shall be treated as a single slash.")
  my $node = '';
  my $double_slashes_special = $^O eq 'qnx' || $^O eq 'nto';

  if ( $double_slashes_special
       && ( $path =~ s{^(//[^/]+)/?\z}{}s || $path =~ s{^(//[^/]+)/}{/}s ) ) {
      $node = $1;
  }

  # This used to be
  # $path =~ s|/+|/|g unless ($^O eq 'cygwin');
  # but that made tests 29, 30, 35, 46, and 213 (as of #13272) to fail
  # (Mainly because trailing "" directories didn't get stripped).
  # Why would cygwin avoid collapsing multiple slashes into one? --jhi
  $path =~ s|/{2,}|/|g;                             # xx////xx  -> xx/xx
  $path =~ s{(?:/\.)+(?:/|\z)}{/}g;                 # xx/././xx -> xx/xx
  $path =~ s|^(?:\./)+||s unless $path eq "./";     # ./xx      -> xx
  $path =~ s|^/(?:\.\./)+|/|;                       # /../../xx -> xx
  $path =~ s|^/\.\.$|/|;                            # /..       -> /
  1 while $path =~ s@(^|/)([^/]+/)\.\.(?:/|$)@$1@g; # xx/yy/../ -> xx/
  $path =~ s|/\z|| unless $path eq "/";             # xx/       -> xx
  return "$node$path";
}
#END: canonpath


=head3 fmap

 my @foos = fmap { s/^FOO: (.*)/$_Util::fmap::file: '$1' line $./ } @files
 my @foos = fmap { s/^FOO: (.*)/$_Util::fmap::file: '$1' line $./ } \%options, @files

Transform files. Loop through the lines of each file and apply a function.
Replace each line with the new value of C<$_>. The current file name will
be available in the variable C<$_Util::fmap::file> and will be one of the
entries in the file list given to the subroutine. Of course, the standard
perl variable C<$.> (C<$INPUT_LINE_NUMBER> when C<use English;> is in
effect) will be available for your use as well.

In scalar or list context returns a hashref (or hash) of C<(filename =E<gt>
[ new contents ])> pairs. The values are arrayrefs containing the modified
lines of each file.

In void context, alters files in-place, just like using C<perl -pi -e> from
the command line.

=over 4

=item if_mode

File mode when reading the file (the default is simply "E<lt>").

=item of_mode

File mode when writing the file (the default is simply "E<gt>").

=item backup

If a single character string (E.g., '~') or if starts with a leading dot
(E.g., '.bak'), is appended to the filename as a backup suffix, Otherwise
is treated as the backup file name ((E.g., 'old_foo'). The default is '~'.

=back

=cut

#BEGIN: fmap
sub fmap :prototype(&@) {
  my ($f, %res, $count) = (shift);
  my %o = ( if_mode => '<', of_mode => '>', backup => '~', ((ref($_[0]) eq "HASH") ? %{shift()} : ()) );
  local $_;

  #DO NOT ALTER FILE:
  if (defined(wantarray)) {
    for my $file (@_) {
      my $A = $res{($_Util::fmap::file = $file)} = [];
      open my $F, $o{if_mode}, $file or croak "Error opening file '$file' for reading: $!";
      while (defined( $_ = <$F> )) { $f->($_); push @$A, $_; }
      close $F;
    }
    return wantarray ? %res : \%res;

  # ALTER FILE:
  } else {
    require File::Copy;
    for my $file (@_) {
      my ($Fout, $Fin, $delete) = ($_Util::fmap::file = $file);# $Fin yet undefined

      if (defined($o{backup})) {
        if ((1 == length($o{backup})) or ('.' eq substr($o{backup},0,1))) {
          $Fin = $Fout.$o{backup};
        } else {
          $Fin = $o{backup};
        }
      } else {
        $delete = 1;
        my $i = 1;
        $i++ while -e "$Fout.$$.$i";
        $Fin = "$Fout.$$.$i";# Note: Goal is file on same partiton for fast move.
      }

      File::Copy::move( $Fout, $Fin );
      open my $FIN,  $o{if_mode}, $Fin  or croak "Error opening file '$Fin' for reading: $!";
      open my $FOUT, $o{of_mode}, $Fout or croak "Error opening file '$Fout' for writing: $!";
      while (defined( $_ = <$FIN> )) { $f->($_); print $FOUT $_ }
      close $FIN;
      close $FOUT;
      unlink $Fin if $delete;
    }
  }
}
#END: fmap


=head3 fgrep

 my @foos = fgrep { s/^FOO: (.*)/$_Util::fgrep::file: '$1' line $./ } @files
 my @foos = fgrep { s/^FOO: (.*)/$_Util::fgrep::file: '$1' line $./ } \"<:encoding(UTF-8)", @files

Grep files. Loop through the lines of each file and apply a function. If
the function returns a true value then C<$_> (after the function
application) will be appended to a list to be returned. The current file
name will be available in the variable C<$_Util::fgrep::file> and will be
one of the entries in the file list given to the subroutine. Of course, the
standard perl variable C<$.> (C<$INPUT_LINE_NUMBER> when C<use English;> is
in effect) will be available for your use as well.

In scalar context just the number of matches will be returned.

NOTE: If you want to chomp your lines note that the last line of a file may
not contain a newline (or whatever C<$E<sol>> is) so use something like
either of the following:

 my @foos = fgrep { chomp; /^FOO/ } @files;
 my @foos = fgrep { /^FOO/ and chomp || 1 } @files;

If a string reference C<$mode> is provided as the first argument after the
subroutine block it will be taken as the file mode (the default is simply
"E<lt>").

=cut

#BEGIN: fgrep
sub fgrep :prototype(&@) {
  my ($f, @res, $count) = (shift);
  my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : "<";
  local $_;
  if (wantarray) {
    for my $file (@_) {
      $_Util::fgrep::file = $file;
      open my $F, $mode, $file or croak "Error opening file '$file' for reading: $!";
      while (defined( $_ = <$F> )) { push @res, $_ if $f->($_) }
      close $F;
    }
    return @res;

  } else { # a little repetition in the name of speed and memory
    for my $file (@_) {
      $_Util::fgrep::file = $file;
      open my $F, $mode, $file or croak "Error opening file '$file' for reading: $!";
      while (defined( $_ = <$F> )) { $count++ if $f->($_) }
      close $F;
    }
    return $count || 0;
  }
}
#END: fgrep


=head3 sed

  sed { CODE } $file, %options

=over 4

=item backup

Save a backup and append this tring to it. DEFAULTS TO NO BACKUP!

=item ignore_errors

If there are errors reading the file, just ignore them and don't process
anything.

=item whole_file

When true, the function is called ony one with the whole file passed in.

=item returns

When true, the function return value will be used, not the result in $_.

=item temp =E<gt> '.tmp'

Temporary file extension. This file is written and then renamed to the
original file name. Set to emtpy string to live dangerously and modify the
file in-place.

=item sync =E<gt> 1

When true, call sync(1) between dangerous operations.

=back

=cut

#BEGIN: sed, DEPENDS: sync
sub sed :prototype(&@) {
    my ($func, $file, %opt) = @_;
    $opt{temp} = '.tmp' unless exists($opt{temp});

    my ($orig, $new, $F);
    unless (open $F, "<", "$file") {
        return if $opt{ignore_errors};
        die "Error reading $file: $!";
    }

    local $_;
    if ($opt{whole_file}) {
        { local $/; $orig = <$F>; }
        $_ = $orig;
        $new = $func->($_);
        $new = $_ unless $opt{returns};
    }
    else {
        my (@orig, @tmp);
        while (defined($_ = <$F>)) {
            push @orig, $_;
            my $x = $func->($_);
            $x = $_ unless $opt{returns};
            push @tmp, $x if defined($x);
        }
        $orig = join "", @orig;
        $new  = join "", @tmp;
    }

    close $F;
    return if $new eq $orig;

    if (length($opt{backup})) {
        open my $F, ">", "$file$opt{backup}" or die "Error creating backup $file$opt{backup}: $!";
        print $F $orig;
        close $F;
        sync() if $opt{sync} and not length($opt{temp});# Limit syncs
    }

    my $tmp = "$file$opt{temp}";
    {
        open my $F, ">", $tmp or die "Error writing $tmp: $!";
        print $F $new;
        close $F;
        sync() if $opt{sync};
    }

    if ($tmp ne "$file") {
        rename $tmp, "$file";
        sync() if $opt{sync};
    }

    return 1;
}
#END: sed


=head3 sync

Calls sync(1)

=cut

#BEGIN: sync
sub sync {
    state $sync;
    ($sync) = grep -e $_, qw# /usr/bin/sync /bin/sync # unless defined($sync);
    if ($sync) { system $sync; }
    else {
        say STDERR "Can't locate bin/sync" unless defined($sync);
        $sync = 0;
    }
}
#END: sync


=head3 find

  #XXX: BUGS!
  Currently not entirely correct but getting better. Known bug:
    * -mindepth available but broken

 my @files = find [ '/' ], qw/-type f -name *.pm/;

File::Find using L<find(1)|find> semantics. Currently supported find options are
given below (descriptions taken from L<find(1)|find>). Unlike find, this
subroutine defaults to returning the list of matches rather than defaulting
to the C<-print> action. Tests are performed in the order specified so a
failure early on will prevent further tests/actions from being performed.
Note: this function will never be a full find2perl replacement.

=over 4

=item -depth

Process each directory's contents before the directory itself.

=item -follow

Dereference symbolic links. This is the option that most closely follows
L<find(1)|find>'s behavior but is not a perfect match. In particular, a
symbolic link which (if followed) would actually result in a circular
reference will be processed by L<find(1)|find>, but not by this function.

NOTE: This option corresponds to the follow_fast option to L<File::Find|File::Find>

=item -follow_smart

Dereference symbolic links. Circular references (as well as links that
would cause a circular reference) will be automatically removed (symbolic
links will only appear if the "real" file would not have been found
otherwise). Dangling symbolic links will be ignored.

NOTE: This option corresponds to the follow option to L<File::Find|File::Find>

=item -no_chdir

Sets corresponding L<File::Find|File::Find> option: Does not "chdir()" to
each directory as it recurses. When true, the first argument to C<-wanted>
and C<-exec> routines will bee a full path. For example, when examining the
file "/some/path/foo.ext" while doing C<find ["/some"]> you will have:

 @_ = ($_ = '/some/path/foo.ext', '/some/path/', '/some/path/foo.ext', '/the/realpath/foo.ext')

=item -untaint

=item -untaint_pattern

Untaint directory names before "chdir()"'ing into them. Untaints using
C<-untaint_pattern>. C<-untaint_pattern> defaults to C<qr|^([-+@\w./]+)$|>.
Your untaint pattern may be a string or pre-compiled (C<qr>) pattern, but
MUST capture the directory name to $1.

=item -maxdepth levels

Descend at most levels (a non-negative integer) levels of directories below
the command line arguments. '-maxdepth 0' means only apply the tests and
actions to the command line arguments.

=item -quiet

Disable "Permission denied" warnings for unreadable directories.

=back

Tests

=over 4

=item -iname pattern

Like -name, but the match is case insensitive. For example, the patterns
'fo*' and 'F??' match the file names 'Foo', 'FOO', 'foo', 'fOo', etc.

=item -iregex pattern

Like -regex, but the match is case insensitive.

=item -name pattern

Base of file name (the path with the leading directories removed) matches
glob pattern (or regexp if passed as C<qr//> compiled regexp). The
metacharacters ('*', '?', and '[]') do not match a '.' at the start of the
base name.

=item -regex pattern

File name matches regular expression pattern. This is a match on the whole
path, not a search. For example, to match a file named './fubar3', you can
use the regular expression '.*bar.' or '.*b.*3', but not 'b.*r3'.

=item -type char

File is of type "char":

  b      block (buffered) special
  c      character (unbuffered) special
  d      directory
  p      named pipe (FIFO)
  f      regular file
  l      symbolic link
  s      socket
  D      door (Solaris)

=back

Actions

=over 4

=item -wanted subroutine

=item -exec subroutine

Execute subroutine; The subroutine is executed in the directory containing
the file and is passed three parameters: the file's name, the current
directory (relative to the starting directory), the file's full path
(relative to the starting directory). If the "-follow" option is provided
then the "true" filename (all symbolic links resolved) will be provided as
a fourth argument. That is:

 @_ = ($_, $File::Find::dir, $File::Find::name, \%info);

For example, when examining the file "/some/path/foo.ext" while doing
C<find ["/some"]> you will have:

 @_ = ($_ = 'foo.ext', '/some/path', '/some/path/foo.ext', \%info)

Where

 %info = (
   path     => $_                    = "foo.ext",
   dir      => $File::Find::dir      = "/some/path/",
   name     => $File::Find::name     = "/some/path/foo.ext",
   fullname => $File::Find::fullname = "/the/realpath/foo.ext",
   top_dir  => "/some",              # current path being examined
   rel_dir  => "path",               # relative to top_dir
   rel_path => "path/foo.ext",       # relative to top_dir
   filename => "foo.ext",            # even when -no_chdir
   basename => "foo",                # removes last extension only
   stat     => File::stat            # File::stat object
 );

If we call C<find ["D"]> from "/foo",

 D/
 |-- bar
 `-- bip
     `-- baz.txt

Then C<%info> will be:

 | path    | dir   | name          | fullname | top_dir | rel_dir | rel_path    | filename | basename |
 |---------+-------+---------------+----------+---------+---------+-------------+----------+----------|
 | .       | D     | D             | undef    | D       | .       | .           | .        | .        |
 | bar     | D     | D/bar         | undef    | D       | .       | bar         | bar      | bar      |
 | bip     | D     | D/bip         | undef    | D       | .       | bip         | bip      | bip      |
 | baz.txt | D/bip | D/bip/baz.txt | undef    | D       | bip     | bip/baz.txt | baz.txt  | baz      |

A "-wanted" subroutine will automatically set "$File::Find::prune" if the
subroutine returns false. An "-exec" subroutine will do no such magic.

=item -print0

print the full file name on the standard output, followed by a null
character. This allows file names that contain new-lines to be correctly
interpreted by programs that process the find output.

=item -print

print the full file name on the standard output, followed by a newline.

=item -prune_all_failures

Discard and prune any files for which any test fails.

=item -prune_hidden

Discard and prune any hidden files. At the moment this means anything
starting with '.' since I don't know how to detect "hidden" files on any
systems other than Linux.

=item -prune_iname pattern

Like -prune_name, but the match is case insensitive. For example, the
patterns 'fo*' and 'F??' match the file names 'Foo', 'FOO', 'foo', 'fOo',
etc.

=item -prune_name pattern

Discard and prune any files where base of file name (the path with the
leading directories removed) matches shell pattern pattern. The
metacharacters ('*', '?', and '[]') do not match a '.' at the start of the
base name.

=item -prune_tex

Discard and prune any auto-generated LaTeX files. Since some extensions are
used for other purposes (especially, .log), will only prune if a .tex file
of the same name exists in the same location.

=item -prune_rcs

Discard and prune any files or directories that look like they belong to a
revision control system. At the moment this means any I<directories> named:
".svn", "CVS", "blib", "{arch}", ".bzr", "_darcs", "RCS", "SCCS", ".git", ".pc"

=item -prune_backup

Discard and prune any files or directories that look like backups. This
includes:

  * ends in "~" or ".bak"
  * matches "#*#" or ".#*"
  * matches "*.tmp" or ".tmp-[_a-zA-Z0-9]+"

=item -prune_regex pattern

Discard and prune any names matching the regular expression pattern. This
is a match on the whole path, not a search. For example, to match a file
named './fubar3', you can use the regular expression '.*bar.' or '.*b.*3',
but not 'b.*r3'.

=back

Main Limitations:

No grouping via (), no -or.

=cut


#BEGIN: find, depends: glob2regexp
{
  use File::stat 1.02;
  my $type_sub = sub {
    my $t = pop;
    my $info = $_[3];
    return $t eq 'l' if -l $$info{stat};
    return -d $$info{stat}  if $t eq 'd';
    return -f $$info{stat}  if $t eq 'f';
    return -p $$info{stat}  if $t eq 'p';
    return -b $$info{stat}  if $t eq 'b';
    return -c $$info{stat}  if $t eq 'c';
    return -S $$info{stat}  if $t eq 's';
  };
  my $re_sub = sub { $_[$_[5]] =~ $_[6] };
  my $prune_re_sub = sub { if ($_[$_[5]] =~ $_[6]) { $File::Find::prune = 1; return 0 } return 1 };
  my $backup_re = qr/~$|^#.*#$|(?:^|\/)\.#|\.bak$|\.tmp(?:\-\w+)?$/;
  my $rcs_pat = qr#^(?:\.svn|CVS|blib|\{arch\}|\.bzr|_darcs|RCS|SCCS|\.git|\.pc|\.\#.*)$#;
  my $prune_rcs_sub = sub { if (-d $_[3]->{stat} and $_[3]->{filename} =~ $rcs_pat) { $File::Find::prune = 1; return 0 } return 1 };
  my $tex_pat = q#(?:log|dvi|idx|ind|toc|out|aux|lab|bbl|blg|ilg|six|nav|snm|rel|pdf|ps|synctex\.gz)#;
  my $prune_derived_sub = sub { my $f = $_[3]->{filename}; if ($f =~ s/\.($_[6])$// and -f "$f.$_[5]") { $File::Find::prune = 1; return 0 } return 1 };

  sub find {
    my ($dirs, %o, @hits, @exec, %ffo);
    require File::Find;
    require File::Spec;
    $dirs = (@_ and ref($_[0]) eq 'ARRAY') ? shift : ['.'];

    while (@_) {
      local $_ = shift;
      next unless defined;
      $_ eq '-untaint_pattern' and do { $ffo{untaint_pattern} = shift; next };
      $_ eq '-no_chdir' and do { $ffo{no_chdir} = 1; next };
      $_ eq '-untaint'  and do { $ffo{untaint} = 1; next };
      $_ eq '-depth'    and do { $ffo{bydepth} = 1; next };
      $_ eq '-follow'   and do { $ffo{follow_fast} = 1; $ffo{follow_skip} = 2; next };
      $_ eq '-follow_smart' and do { $ffo{follow} = 1; $ffo{follow_skip} = 2; next };
      $_ eq '-maxdepth' and do { $o{_maxdepth} = shift; next };
      $_ eq '-mindepth' and do { $o{_mindepth} = shift; next };
      $_ eq '-quiet'    and do { $o{quiet} = 'no warnings; '; next };
      $_ eq '-name'     and do { push @exec, map [$re_sub, 0, qr/$_/ ], (ref($_[0]) ? shift() : glob2regexp(shift)); next };
      $_ eq '-iname'    and do { push @exec, map [$re_sub, 0, qr/$_/i], (ref($_[0]) ? shift() : glob2regexp(shift)); next };
      $_ eq '-regex'    and do { push @exec, map [$re_sub, 2, qr/$_/ ], shift; next };
      $_ eq '-iregex'   and do { push @exec, map [$re_sub, 2, qr/$_/i], shift; next };
      $_ eq '-wanted'   and do { push @exec, [sub{ !($File::Find::prune ||= !pop->(@_[0..3])) }, shift]; next };
      $_ eq '-exec'     and do { push @exec, shift; next };
      $_ eq '-print'    and do { push @exec, sub{ printf "%s\n", $_[2] }; next };
      $_ eq '-print0'   and do { push @exec, sub{ printf "%s\0", $_[2] }; next };
      $_ eq '-type'     and do { push @exec, [$type_sub, shift] if $_[0] =~ /^[bcdpfls]$/; next };
      $_ eq '-prune_hidden' and do { push @exec, [$prune_re_sub, 0, qr/^\.[^\.\\\/]/ ]; next };
      $_ eq '-prune_backup' and do { push @exec, [$prune_re_sub, 0, $backup_re ]; next };
      $_ eq '-prune_rcs'    and do { push @exec, $prune_rcs_sub; next };
      $_ eq '-prune_tex'    and do { push @exec, [$prune_derived_sub, tex => $tex_pat]; next };
      $_ eq '-prune_iname'  and do { push @exec, map [$prune_re_sub, 0, qr/$_/i], (ref($_[0]) ? shift() : glob2regexp(shift)); next };
      $_ eq '-prune_name'   and do { push @exec, map [$prune_re_sub, 0, qr/$_/ ], (ref($_[0]) ? shift() : glob2regexp(shift)); next };
      $_ eq '-prune_regex'  and do { push @exec, map [$prune_re_sub, 2, qr/$_/ ], shift; next };
      $_ eq '-prune_on_false'     and do { $o{prune_on_false} = 1; next };
      $_ eq '-prune_all_failures' and do { $o{prune_all_failures} = 1; next };
      $_ eq '-xdev'     and do { $o{xdev}  = 1; next };
      $_ eq '-DEBUG'    and do { $o{DEBUG} = 1; next };
      croak "No such option '$_'!";
    }

    $o{_maxdepth} = 99999999 unless defined $o{_maxdepth};
    $o{_mindepth} = 0        unless defined $o{_mindepth};
    $o{noaccum}   = 1        unless defined wantarray;
    my $top_dev;
    my $top_dir;

    $ffo{wanted} = sub {
      no warnings 'once';
#       $o{DEBUG} and print "Dean::Util::find: TESTING '".($File::Find::fullname || $File::Find::name)."'\n";
      my $stat  = stat($_);
      unless ($stat) {
          say STDERR "Unable to stat $File::Find::name";
          return;
      }
      my $depth = grep(length, File::Spec->splitdir($File::Find::dir));
      $depth++ if -d $stat and ($File::Find::name ne $File::Find::dir);
      return $File::Find::prune = $o{prune_all_failures} if $depth <= $o{mindepth};
      return $File::Find::prune = 1                      if $depth >  $o{maxdepth};
      return $File::Find::prune = 1                      if $o{xdev} and lstat($File::Find::name)->dev != $top_dev;
#       $o{DEBUG} and print "Dean::Util::find:   not pruned (depth: $depth;  maxdepth: $o{maxdepth}; name: $File::Find::name)\n";
      my $f = $_;
      my %info = ( top_dir => $top_dir, fullname => $File::Find::fullname, dir => $File::Find::dir, name => $File::Find::name,
                   rel_dir  => File::Spec->abs2rel( File::Spec->rel2abs($File::Find::dir),  $top_dir ),
                   rel_path => File::Spec->abs2rel( File::Spec->rel2abs($File::Find::name), $top_dir ),
                   path => $f,
                   stat => $stat,
                 );
      $info{filename} = (File::Spec->splitpath( $info{rel_path} ))[2];
      my $last_dot = rindex($info{filename},".");
      $info{basename} = $last_dot > 0 ? substr($info{filename}, 0, $last_dot) : $info{filename};
      for my $e (@exec) {
        ref($e) eq 'CODE'  && do { return $File::Find::prune ||= $o{prune_all_failures} unless     $e->($f, $File::Find::dir, $File::Find::name, \%info ) };
        ref($e) eq 'ARRAY' && do { return $File::Find::prune ||= $o{prune_all_failures} unless $$e[0]->($f, $File::Find::dir, $File::Find::name, \%info, @$e) };
      }
      push @hits, $File::Find::name unless $o{noaccum};
      return 1;
    };

    for (@$dirs) {
      $top_dir = $_;
      $o{quiet} and eval "no warnings;";
      my $lstat = lstat($top_dir);
      next unless $lstat and -e $lstat;
      $top_dev = lstat($top_dir)->dev;
      $o{maxdepth} = $o{_maxdepth} + grep(length, File::Spec->splitdir($top_dir));
      $o{mindepth} = $o{maxdepth} - $o{_maxdepth} + $o{_mindepth} - 1;
      File::Find::find( \%ffo, $top_dir );
    }
    return @hits;
  }
}
#END: find


=head3 newer

Returns true if first file is newer than second file. Also returns true if
first file exists but second does not.

=cut

#BEGIN: newer
sub newer {
  return   unless @_ == 2;
  return 0 unless -f $_[0];
  return 1 unless -f $_[1];
  return -M $_[0] < -M $_[1]
}
#END: newer

=head3 lastline

 my $line = lastline $file;
 my $line = lastline "<:encoding(UTF-8)", $file;

Returns the last line of a file. Includes a seek() optimization based on
the lengths of the first several lines so that reading the last line of a
large file should be reasonably efficient.

By default the input will not be decoded. Either provide an initial scalar
reference containing the file mode (with proper encoding, for example
\"E<lt>:encoding(UTF-8)") or decode the string before using it.

=cut

#BEGIN: lastline
sub lastline {
    my $mode = (ref($_[0]) eq "SCALAR") ? ${shift()} : "<";
    my $file = shift;
    my $FILE;
    local $_;

    if (-e $file and -s $file and open $FILE, $mode, $file or croak "Can't open $file for reading: $!") {
        my $lastline;
        my $n = 0;
        my $bytes = 0;

        while ($_ = <$FILE>) {
            $lastline = $_;
            $bytes += length($_);

            # Optimization for large files, seek to near the end once we have
            # some clue about expected line length.
            if (++$n == 10 and $bytes / (-s $file) < .2) {
                my $target = -s $file;
                my $from_end = $bytes;
                my $tmp;
                until (defined($tmp)) {
                    $target -= $from_end;
                    if ($target <= $bytes) {
                        # Optimization failed miserably
                        seek($FILE, $bytes, 0);
                        $tmp = $lastline;
                        last;
                    }
                    $from_end *= 2;
                    seek($FILE, $target, 0);
                    scalar <$FILE>; # garbage partial line
                    $tmp = <$FILE>;
                }
                $lastline = $tmp;
            }
        }

        return $lastline;
    }
    else { return '' }
}
#END: lastline

=head3 fprint

See also: File::Slurp

 fprint $filename, @stuff
 fprint \$mode, $filename, @stuff

Prints stuff to the indicated filename. If a mode is provided (for example,
C<\"E<gt>:encoding(UTF-8)">) then it will be used instead of the default
mode ("E<gt>").

=cut

=head3 fprint_bu

 fprint_bu $filename, @stuff
 fprint_bu \$mode, $filename, @stuff

Prints stuff to the indicated filename, but backup filename (by appending a
~) first. If a mode is provided (for example, C<\"E<gt>:encoding(UTF-8)">)
then it will be used instead of the default mode ("E<gt>").

=cut

=head3 fappend

See also: File::Slurp

 fappend $filename, @stuff
 fappend \$mode, $filename, @stuff

Append stuff to the indicated filename. If a mode is provided (for example,
C<\"E<gt>E<gt>:encoding(UTF-8)">) then it will be used instead of the
default mode ("E<gt>E<gt>").

=cut

#BEGIN: fprint, 1 line
sub fprint {my ($f,$mode,$F)=(shift,">");($mode,$f)=($$f,shift) if'SCALAR'eq ref$f;open $F,$mode, $f or croak "Can't open $f for writing: $!";print $F @_;close $F}

#BEGIN: fappend, 1 line
sub fappend {my ($f,$mode,$F)=(shift,">>");($mode,$f)=($$f,shift) if'SCALAR'eq ref$f;open $F,$mode,$f or croak "Can't open $f for writing: $!";print $F @_;close $F}

#BEGIN: fprint_bu, 1 line, depends: bu_open
sub fprint_bu { my ($f,$mode)=(shift,">");($mode,$f)=($$f,shift) if'SCALAR'eq ref$f;my $F; bu_open(\$mode, $F, $f); print $F @_; close $F; }



=head3 fincrement

 fincrement $filename
 fincrement $filename, $amount
 fincrement $filename, pre => $pre, post => $post, layers => $perlio_layers
 fincrement $filename, $amount, pre => $pre, post => $post

Increments the number contained in C<$filename>. On success, the new value
is returned (Note: may be zero if C<$filename> contained "-1"). On failure,
C<undef> is returned.

The amount to add to the file's value may be provided. If it is missing,
then a value of one is assumed. The optional parameters C<$pre> and
C<$post> specify strings to print to the file before and after the number.
These strings default to the empty string and a single newline
respectively.

Note: C<$filename> must contain only a number (with possible whitespace),
or must exactly contain the concatenation of C<$pre>, number, and C<$post>.

If C<$filename> does not exist, then it will be initialized to "0"

The "layers" option can be used to set the PerlIO layers for the opened
files (for example layers =E<gt> ":encoding(UTF-8)"). By default, no layers
are applied.

=cut

#BEGIN: fincrement, depends: is_num, $_re_num
#-----------------------------------------------------------------
# Returns the incremented number on success (may be zero!) and undef on failure.
{ use Fcntl ':flock';
  sub fincrement {
    my ($f,$dn,$n,$F)=(shift, 1);
    $dn = shift if @_ % 2;
    my ($pre, $post, $layers) = @{ {pre => '', post => $/, layers => "", @_ } }{qw/pre post layers/};
    $layers ||= "";
    if (-e $f) {
      open $F,"+<$layers", $f or croak "Can't open $f for writing: $!";
      flock($F, LOCK_EX);
      { local $/ = undef; $n = <$F>; }
      seek $F, 0, 0;
    } else {
      open $F,">$layers", $f or croak "Can't open $f for writing: $!";
      flock($F, LOCK_EX);
      $n = 0
    }
    if (is_num($n) or $n =~ s/\Q$pre\E($_re_num)\Q$post\E/$1/) {
      $n =~ s/\s+//g;
      $n += $dn;
      print $F $pre, $n, $post;
      flock($F, LOCK_UN);
      close $F;
      return $n;
    } return;
  }
}
#END: fincrement

=head3 cat

See also: File::Slurp

 my $stuff = cat $file;
 my $stuff = cat \$mode, $file;

Read in the entirety of a file. If requested in list context, the lines are
returned. In scalar context, the file is returned as one large string. If a
string reference C<$mode> is provided as a first argument it will be taken
as the file mode (the default is "E<lt>").

=cut

=head3 bcat

See also: File::Slurp

Read in the entirety of a binary file. If requested in list context, the
lines are returned. In scalar context, the file is returned as one large
string.

=cut

#BEGIN: cat
sub cat {
  my $mode = (ref($_[0]) eq 'SCALAR') ? ${shift()} : "<";
  my $f = (@_) ? $_[0] : $_;
  open my $F, $mode, $f or croak "Can't open $f for reading: $!";
  if (wantarray) {
    my @x = <$F>; close $F; return @x;
  } else {
    local $/ = undef; my $x = <$F>; close $F; return $x;
  }
}
#END: cat

#BEGIN: bcat, 2 lines
sub bcat {my $f=(@_)?$_[0]:$_;my$F;open $F,"<:raw", $f or croak "Can't open $f for reading: $!"; binmode $F;
          if (wantarray) { my@x=<$F>;close $F;return @x } else { local$/=undef;my$x=<$F>;close $F;return $x}}

=head3 bu_open

 bu_open $file
 bu_open $fh, $file
 bu_open $fh, $file, "$file.bak"
 bu_open \$mode, $file
 bu_open \$mode, $fh, $file
 bu_open \$mode, $fh, $file, "$file.bak"

 ($writer, $reader) = bu_open \$mode, $file

Backup and open. The general idea is, if the file exists, rename it by
appending a "~" to its name, then open the original name in write mode.
This sub croaks if any operation fails. The backup file is created new so
that the inode of the original file does not change.

If only a single string variable argument is given and the function is
called in void context, then the requested file is backed up and opened,
"upgrading" the given argument to a filehandle. Example:

 $file = "foo";
 bu_open $file;         # Note: bu_open "foo"; would be a fatal error
 print $file "Bar\n";

In scalar context, C<$file> is unchanged and a write-only filehandle is returned.

In list context, a filehandle for both the new file (write only) and the
backup (read only) are returned.

If a mode is provided as a C<SCALAR> reference (for example, C<\"E<gt>:encoding(UTF-8)">)
then it will be used instead of the default mode ("E<gt>").

If two arguments are given, the first will be used to store the newly
opened filehandle, and the second should hold the file name.

Finally, the final argument (if provided) will be used for the backup file
(rather than the C<$file> argument with a "~" appended).

=cut

#BEGIN: bu_open, depends: readonly
{ use Carp;
  sub bu_open {
    require File::Copy;
    my $mode   = (@_ > 1 and (ref($_[0]) eq 'SCALAR')) ? ${+shift} : ">";
    my $file   = (@_ > 1) ? $_[1] : $_[0];
    my $bufile = (@_ > 2) ? $_[2] : $file."~";
    File::Copy::copy($file, $bufile) or croak "Error backing up $file as $bufile: $!" if -e $file;
    if (@_ == 1 and defined wantarray) {
      my ($writer, $reader);
      open $writer, $mode, $file or croak "Error opening $file for writing: $!";
      return $writer unless wantarray;
      open $reader, "<", $bufile or croak "Error opening $bufile for reading: $!";
      return ($writer, $reader);
    }
    elsif (@_ == 1 and readonly($_[0])) { croak "Can't use read-only value as filehandle" }
    else {
      $_[0] = undef;
      open $_[0], $mode, $file or croak "Error opening $file for writing: $!";
      return $_[0] unless wantarray;
      open my $reader, "<", $bufile or croak "Error opening $bufile for reading: $!";
      return ($_[0], $reader);
    }
  }
}
#END: bu_open

=head3 catfile

Calls the File::Spec catfile and canonpath methods.

=cut

#BEGIN: catfile, depends: canonpath
sub catfile {
  canonpath( File::Spec->catfile( @_ ) )
}
#END: catfile

=head3 realfile

Unnecessary! use Cwd::realpath

=cut

#BEGIN: realfile: 1 line
{ sub realfile { require Cwd; Cwd::realpath( @_ ) } }


#-----------------------------------------------------------------
                         $EXPORT_TAGS{shell}
                                 =
[qw/safe_pipe bash_complete/];
#-----------------------------------------------------------------

=head2 :shell - Shell Operations

=head3 bash_complete

 bash_complete \@completions, %options;
 bash_complete \%completions, %options;

Prints strings appropriate for user-friendly bash completion.

There are various desirable tricks to get user-friendly bash completions
which require escaping strings at the right time and potentially removing
common prefixes (e.g., leading directories). This function tries to do all
that.

Additionally, it can make it easier to support action words (e.g., "git
add" vs "git checkout"), simply pass in completions as a hash with action
names as keys.


Recognized options

=over 4

=item paths =E<gt> Bool | Code

Include path completions. If passed a code ref, will call function for
return value (See parameters passed to CODE ref below).

=item separator =E<gt> Str | Regexp

Component separator. Defaults to "/" if not set and "paths" parameter
evaluates to true. Leading common substrings up to the last separator will
be trimmed from display when completing.

=item noprint =E<gt> Bool

Return list of matches instead of printing results to STDOUT.

=item prefix =E<lt> Str

Override match prefix which is, by default, taken from @ARGV.

=item key =E<lt> Str

Override "previous argument" key which is, by default, taken from @ARGV.

=item program =E<lt> Str

Override program name which is, by default, taken from @ARGV.

=back

Each value of the completion array should be one of:

=over 4

=item plain scalar

value will be printed if it matches the prefix

=item ARRAY of scalars

items in ARRAY will be printed if they match the prefix

=item CODE ref

CODE which returns one of these (list of scalars, array of scalars, or code
ref) that will be checked agains the prefix. CODE will be called with
arguments:

=over 4

=item C<$program>

the program ($0 of the program being completed for)

=item C<$prefix>

prefix string to match

=item C<$key>

the "key" (argument before prefix)

=back

=back

=cut

#BEGIN: bash_complete, DEPENDS: qbsbash
sub bash_complete {
    use File::Glob qw/ bsd_glob GLOB_MARK GLOB_NOSORT GLOB_QUOTE GLOB_TILDE /;
    my ($c, %o) = @_;
    $c = { '' => $c } if ref($c) eq 'ARRAY';

    my $prog  = $o{program} // $ARGV[0];
    my $pre   = $o{prefix}  // $ARGV[1];
    my $key   = $o{key}     // $ARGV[2];

    # Probably fails in complex un-escaping: 'Foo "the"'"Bar't"
    $pre =~ s/\\(.)/$1/g;
    my $pat = qr/^\Q$pre\E/;
    my @hits;

    # path completion:
    my $want_paths = $o{paths};
    $want_paths = $o{paths}->($prog, $pre, $key) if 'CODE' eq ref($o{paths});
    if ($want_paths) {
        my $quoted = $pre;
        $quoted =~ s/([\\\*\?])/\\$1/g;# Purposefully leave "~" out of this.
        # GLOB_MARK appends "/" to directories, we use this later.
        push @hits, bsd_glob("$quoted*", GLOB_MARK | GLOB_NOSORT | GLOB_QUOTE | GLOB_TILDE);
    }

    # keyword completion:
    my @potentials = exists($$c{$key}) ? $$c{$key} : exists($$c{""}) ? $$c{""} : ();
    while (@potentials) {
        local $_ = shift @potentials;
        my $ref = ref;
        if (!$ref) { push @hits, $_ if /$pat/ }
        elsif ($ref eq 'ARRAY') {
            push @potentials, @$_;
        } elsif ($ref eq 'CODE') {
            push @potentials, $_->($prog, $pre, $key);
        } else {
            carp "Error in completion hash: object must be ARRAY, CODE, or plain scalar";
        }
    }

    # Display the results:
    unless ($o{noprint}) {
        my $sep = exists($o{separator}) ? $o{separator} : ($want_paths ? "/" : undef);

        # Need hack so that directories do not match as fully complete, but
        # only if we are advancing the completion.
        if (defined($sep) and @hits == 1 and $hits[0] =~ m|$sep$| and $pre ne $hits[0]) {
            push @hits, $hits[0] . " ";
        }

        if (@hits) {
            # We don't really want to see escape chars in our list of
            # options, but when we auto-complete, we do want the escapes.
            # Therefore, we have to lie to bash a bit. First, we need to
            # know what the length of the common substring:
            my $common = length($hits[0]);
            for (1..$#hits) {
                $common-- while substr($hits[$_], 0, $common) ne substr($hits[$_-1], 0, $common);
                last unless $common;
            }
            my $start = substr($hits[0], 0, $common);

            # Display vs Advancement
            # ----------------------
            # Now, if that substring differs from what we started with we
            # will advance the completion and bash will not show any
            # results. Thus we should escape (at least) the common prefix.
            # Otherwise, we are at a point where more input is needed to
            # continue so show whatever is most convenient to the user.
            if ($start eq $pre and @hits > 1) {
                # Display: Show whatever is logically better for user
                #
                # Trim common leading directories:
                if (defined($sep)) {
                    $start =~ s|^.*$sep||;
                }
            } else {
                # Advance: Show only correct data
                $start = qbsbash($start);
            }
            say $start . substr($_, $common) for @hits;
        }
    }

    # Return, if you really want that
    return @hits;
}
#END: bash_complete


=head3 safe_pipe

 safe_pipe [ options, ] command, input

 my $results = safe_pipe [ 'command', 'arg' ], @input;
 my @results = safe_pipe [ 'command', 'arg' ], @input;
 my $results = safe_pipe \%opt, [ 'command', 'arg' ], @input;

Pipe data to a shell command safely (without touching a command line) and
retrieve the results. Notably, this is the situation that
L<IPC::Open2|IPC::Open2> says that is dangerous (may block forever) using
L<open2|IPC::Open2>. If process execution fails for any reason an error is
thrown.

In void context, all command output will be directed to STDERR making this
command almost equivalent to:

 my $pid = open my $F, "|-", 'command', 'arg' or die;
 print $F @input; close $F;
 waitpid( $pid, 0 );

Options:

=over 4

=item chomp

If true, and function called in list context, lines will be chomp()-ed.

=item capture_err

If true, STDERR will also be captured and included in returned results.

=item allow_error_exit

By default, this sub will verify that the command exited successfully.
(C<0 == $?>) and throw an error if anything went wrong. Setting
C<allow_error_exit> to a true value will prevent this sub from examining
the return value of the command.

Setting C<allow_error_exit> to an array of allowed exit status will ignore
only those (error) exit codes (code 0 will be considered a success).

=item debug

If true, print command to STDERR as it is executed.

=back

Modified code from merlyn: http://www.perlmonks.org/index.pl?node_id=339092

Note: Input and output will not be encoded/decoded thus should be octets.

Note: locally alters $SIG{CHLD}

=cut

#BEGIN: safe_pipe
sub safe_pipe {
    my $opt = {};
    $opt = shift if 'HASH' eq ref($_[0]);
    my $command = shift;
    $command = [$command] unless ref $command;
    say STDERR "@$command" if exists($$opt{debug}) and $$opt{debug};
    local $SIG{CHLD};
    my @exit = ('ARRAY' eq ref($$opt{allow_error_exit})) ? @{$$opt{allow_error_exit}} : ();

    my $chld = open my $RESULT, "-|";
    die "Can't fork: $!" unless defined($chld);

    if ($chld) { # original process: receiver (reads $RESULT)
        my @x = <$RESULT>;
        waitpid $chld, 0;
        if (@exit or !$$opt{allow_error_exit}) {
            local $" = "";
            my $stat = $? >> 8;
            if    ($? == -1) { croak "failed to execute: $!" }
            elsif ($? & 127) { croak sprintf("child died with signal %d, %s coredump\n%s", ($? & 127),  (($? & 128) ? 'with' : 'without'), "@x") }
            elsif ($stat and !grep($_ == $stat, @exit)) { croak sprintf("child exited with value %d\n%s", $stat, "@x") }
        }
        if (wantarray) {
            chomp(@x) if $$opt{chomp};
            return @x;
        } else {
            return join('', @x);
        }
    }

    # Note below: Can't just exit() or die() or we will run END{} blocks...
    # The solution used  ripped indirectly from POE::Wheel::Run:
    #    http://stackoverflow.com/questions/4307482/how-do-i-disable-end-blocks-in-child-processes

    else { # child
        if (open STDIN, "-|") { # child: processor (reads STDIN, writes STDOUT)
            open STDOUT, ">&STDERR" or die "Can't dup STDERR: $!" if !defined(wantarray) and !$$opt{capture_err};
            open STDERR, ">&STDOUT" or die "Can't dup STDOUT: $!" if $$opt{capture_err};
            { exec { $$command[0] } @$command; }
            # Kill ourselves (causes "child died with signal 9"):
            close STDIN; close STDOUT; close STDERR;
            eval { CORE::kill( KILL => $$ ); };
            exit 1;
        } else {                # grandchild: sender (writes STDOUT)
            print @_;
            # Kill ourselves
            close STDIN; close STDOUT; close STDERR;
            eval { CORE::kill( KILL => $$ ); };
            exit 0;
        }
    }
}
#END: safe_pipe



#-----------------------------------------------------------------
                        $EXPORT_TAGS{color}
                                 =
[qw/$_re_color_escape strip_color strip_color_violently clength %color
    wavelength2rgb colors rainbow hsl2rgb NOCOLOR
    /];
#-----------------------------------------------------------------

=head2 :color - Color

=head3 NOCOLOR

 NOCOLOR(__PACKAGE__) if !$opt{color};
 NOCOLOR()            if !$opt{color};

Replaces subroutines and package variables whose name matches one of the
names in the :color_subs or :color_strings export tags with inert versions
which do not insert any color sequences. Subroutines are replaced by the
identity function and strings are replaced with the empty string. The
default package is the caller's current package.

WARNING: This subroutine has no good way of knowing that the subroutines
and variables that it finds are really color subroutines and variables. It
does however check that subroutines have a '$' prototype and it only has
access to package variables (those not declared by C<my>). This combined
with the fact that there is only so many things that a function called
"BLUE" could reasonably do means that this should not generally be a
problem.

SUBS affected:

 BOLD UNDERLINE DARK BLINK REVERSE CONCEALED STRIKE
 BLACK RED GREEN YELLOW BLUE MAGENTA CYAN WHITE
 GREY GRAY BRIGHT_RED BRIGHT_GREEN BRIGHT_YELLOW BRIGHT_BLUE BRIGHT_MAGENTA BRIGHT_CYAN
 ON_BLACK ON_RED ON_GREEN ON_YELLOW ON_BLUE ON_MAGENTA ON_CYAN ON_WHITE
 ON_GREY ON_GRAY ON_BRIGHT_RED ON_BRIGHT_GREEN ON_BRIGHT_YELLOW ON_BRIGHT_BLUE ON_BRIGHT_MAGENTA ON_BRIGHT_CYAN

SCALARS affected:

 $BOLD $BOLD_OFF $UNDERLINE $UNDERLINE_OFF $DARK $DARK_OFF $BLINK $BLINK_OFF $REVERSE $REVERSE_OFF
 $CONCEALED $CONCEALED_OFF $STRIKE $STRIKE_OFF $NORMAL $DEFAULT_FG $DEFAULT_BG
 $BLACK $RED $GREEN $YELLOW $BLUE $MAGENTA $CYAN $WHITE
 $GREY $GRAY $BRIGHT_RED $BRIGHT_GREEN $BRIGHT_YELLOW $BRIGHT_BLUE $BRIGHT_MAGENTA $BRIGHT_CYAN
 $ON_BLACK $ON_RED $ON_GREEN $ON_YELLOW $ON_BLUE $ON_MAGENTA $ON_CYAN $ON_WHITE
 $ON_GREY $ON_GRAY $ON_BRIGHT_RED $ON_BRIGHT_GREEN $ON_BRIGHT_YELLOW $ON_BRIGHT_BLUE $ON_BRIGHT_MAGENTA $ON_BRIGHT_CYAN

=cut

#BEGIN: NOCOLOR
{ my $subs =
    [qw/ BOLD UNDERLINE DARK BLINK REVERSE CONCEALED STRIKE
         BLACK RED GREEN YELLOW BLUE MAGENTA CYAN WHITE
         GREY GRAY BRIGHT_RED BRIGHT_GREEN BRIGHT_YELLOW BRIGHT_BLUE BRIGHT_MAGENTA BRIGHT_CYAN
         ON_BLACK ON_RED ON_GREEN ON_YELLOW ON_BLUE ON_MAGENTA ON_CYAN ON_WHITE
         ON_GREY ON_GRAY ON_BRIGHT_RED ON_BRIGHT_GREEN ON_BRIGHT_YELLOW ON_BRIGHT_BLUE ON_BRIGHT_MAGENTA ON_BRIGHT_CYAN
         /];
  my $scalars =
    [qw/ $BOLD $BOLD_OFF $UNDERLINE $UNDERLINE_OFF $DARK $DARK_OFF $BLINK $BLINK_OFF $REVERSE $REVERSE_OFF
         $CONCEALED $CONCEALED_OFF $STRIKE $STRIKE_OFF $NORMAL $DEFAULT_FG $DEFAULT_BG
         $BLACK $RED $GREEN $YELLOW $BLUE $MAGENTA $CYAN $WHITE
         $GREY $GRAY $BRIGHT_RED $BRIGHT_GREEN $BRIGHT_YELLOW $BRIGHT_BLUE $BRIGHT_MAGENTA $BRIGHT_CYAN
         $ON_BLACK $ON_RED $ON_GREEN $ON_YELLOW $ON_BLUE $ON_MAGENTA $ON_CYAN $ON_WHITE
         $ON_GREY $ON_GRAY $ON_BRIGHT_RED $ON_BRIGHT_GREEN $ON_BRIGHT_YELLOW $ON_BRIGHT_BLUE $ON_BRIGHT_MAGENTA $ON_BRIGHT_CYAN
         /];
  sub NOCOLOR {
    no warnings 'redefine';
    no strict 'refs';
    my $pkg = @_ ? shift : caller;
    for (@$subs) {
      *{$pkg."::$_"} = sub :prototype($) { $_[0] } if defined(*{$pkg."::$_"}{CODE}) and '$' eq prototype(\&{$pkg."::$_"});
    }
    for (@$scalars) {
      my $name = substr($_, 1);
      ${$pkg."::$name"} = "" if defined(${$pkg."::$name"});
    }
  }
}
#END: NOCOLOR


=head3 hsl2rgb

 my $rgb    = hsl2rgb( $H, $S, $L );
 my @colors = hsl2rgb( @hsl_colors );

Convert HSL colors (triples from 0 to 1) to RGB colors (triples from 0 to 255).

=cut

#BEGIN: hsl2rgb
{ my $h2rgb = sub {
    my ($v1, $v2, $vh) = @_;
    $vh += 1 if $vh < 0;
    $vh -= 1 if $vh > 1;

    return $v1 + ($v2 - $v1) * 6 * $vh         if 6 * $vh < 1;
    return $v2                                 if 2 * $vh < 1;
    return $v1 + ($v2 - $v1) * (2/3 - $vh) * 6 if 3 * $vh < 2;
    return $v1;
  };

  sub hsl2rgb {
    return hsl2rgb([@_]) unless ref($_[0]) eq 'ARRAY';
    my @res;
    for (@_) {
      my ($H,$S,$L) = @$_;
      if (0 == $S) {
        push @res, [ ($L * 255)x3 ]
      } else {
        my ($x, $y);
        if ($L < .5) { $y = $L * (1 + $S) }
        else         { $y = ($L + $S) - ($S * $L) }
        $x = 2 * $L - $y;
        push @res, [ map 255 * $_, &$h2rgb($x, $y, $H+1/3), &$h2rgb($x, $y, $H), &$h2rgb($x, $y, $H-1/3) ]
      }
    }
    return $res[0] if 1 == @_;
    return @res;
  }
}
#END: hsl2rgb


=head3 rainbow

 rainbow( $n );
 rainbow( $n, %colors_options);

Return a list of C<$n> rainbow colors (ROYGBIV).

Any options supported by L<colors|/colors> can be provided and will be
passed along, including the L<n|/colors> and L<colors|/colors> options, so
you probably don't want to include those options.

=cut

#BEGIN: rainbow, depends: colors
{ no warnings 'qw';
  my $rainbow = [qw/#df0000 #ff8000 #ffff00 #00ff00 #00e0e0 #0000ff #50007f/];
  sub rainbow {
    my $n = shift;
    return colors(distribute => 1, format => "ps", n => $n, colors => $rainbow, @_);
  }
}
#END: rainbow


=head3 wavelength2rgb

Convert a wavelength (a number between 380 nm and 780 nm) to a RGB triplet
(0 ≤ x_i ≤ 1). Returns undef if given an out-of-range wavelength.

Formulas taken from Dan Bruton's color science page
(http://www.midnightkite.com/color.html).

=cut

# Potential upgrade: CORRECTED WAVELENGTH to XYZ,
#   http://cvision.ucsd.edu/database/text/cmfs/ciexyzjv.htm
# would be an interpolation

#BEGIN: wavelength2rgb
# RGB VALUES FOR VISIBLE WAVELENGTHS   formulas by Dan Bruton (astro@tamu.edu)
# Dan's FORTRAN code can be found at: http://www.midnightkite.com/color.html
sub wavelength2rgb {
  my (@c, $r, $g, $b, $intensity);
  my $gamma = .80;
  for (@_) {
    if    ($_ >= 380 and $_ <= 440) { ($r, $g, $b) = ( -($_-440)/(440-380), 0, 1 ); }
    elsif ($_ >= 440 and $_ <= 490) { ($r, $g, $b) = ( 0, ($_-440)/(490-440), 1 ); }
    elsif ($_ >= 490 and $_ <= 510) { ($r, $g, $b) = ( 0, 1, -($_-510)/(510-490) ); }
    elsif ($_ >= 510 and $_ <= 580) { ($r, $g, $b) = ( ($_-510)/(580-510), 1, 0 ); }
    elsif ($_ >= 580 and $_ <= 645) { ($r, $g, $b) = ( 1, -($_-645)/(645-580), 0 ); }
    elsif ($_ >= 645 and $_ <= 780) { ($r, $g, $b) = ( 1, 0, 0 ); }
    elsif ($_ <  380 or  $_ >  780) { return }

    # LET THE INTENSITY FALL OFF NEAR THE VISION LIMITS
    if    ($_ > 700) { $intensity = .3 + .7 * (780-$_)/(780-700); }
    elsif ($_ < 420) { $intensity = .3 + .7 * ($_-380)/(420-380); }
    else  { $intensity = 1; }

    push @c, [ map $_ * $gamma * $intensity, $r, $g, $b ];
  }
  @_>1 ? @c : $c[0];
}
#END: wavelength2rgb


=head3 $_re_color_escape

A pre-compiled regular expression that matches any of the colors or
font manipulations provided in this package.

=cut

=head3 strip_color

Remove the color tags from a list of strings. The uncolored strings are
returned. Does not modify the input strings and can be used on constant
strings.

=cut

=head3 strip_color_violently

Remove the color tags from a list of strings. The uncolored strings are
returned. Modifies the input strings and therefore may not be used on
constant strings.

=cut

=head3 clength

Compute the length of a possibly colored string. The standard perl
L<length|perlfunc/length> function gets confused about how long a colored
or decorated string is. This function fixes that so that you can center or
align data.

=cut

#BEGIN: $_re_color_escape, 1 line
our $_re_color_escape = '\e\[(?:\d+(?:;\d+)*)?m';

#BEGIN: strip_color_violently, 1 line, depends: $_re_color_escape
sub strip_color_violently { s/$_re_color_escape//g for @_; wantarray ? @_ : $_[0] }

#BEGIN: strip_color; depends: $_re_color_escape
sub strip_color {
  my (@x, $x);
  @x = map { defined() ? do { $x = $_; $x =~ s/$_re_color_escape//g; $x } : undef } @_;
  return wantarray ? @x : shift @x;
}
#END: strip_color

#BEGIN: clength; depends: strip_color, sum
sub clength {
  if (wantarray) { return map((defined() ? length : 0), strip_color(@_) )  }
  else           { return sum( map((defined() ? length : 0), strip_color(@_) ) ) }
}
#END: clength

=head3 %color

A hash of color names => escape sequences. Included are text style
sequences,

  BOLD UNDERLINE DARK BLINK REVERSE CONCEALED

Also, the following colors:

  BLACK GREY GRAY WHITE
  RED GREEN YELLOW BLUE MAGENTA CYAN
  BRIGHT_RED BRIGHT_GREEN BRIGHT_YELLOW BRIGHT_BLUE BRIGHT_MAGENTA BRIGHT_CYAN

And their corresponding backgrounds:

  ON_BLACK ON_GREY ON_GRAY ON_WHITE
  ON_RED ON_GREEN ON_YELLOW ON_BLUE ON_MAGENTA ON_CYAN

  ON_BRIGHT_RED ON_BRIGHT_GREEN ON_BRIGHT_YELLOW ON_BRIGHT_BLUE
  ON_BRIGHT_MAGENTA ON_BRIGHT_CYAN

=cut

#BEGIN: %color
# cute, no?
our %color = ( NORMAL => "\e[0m", BOLD => "\e[1m", DARK => "\e[2m",
  UNDERLINE => "\e[4m", BLINK => "\e[5m", REVERSE => "\e[7m",
  CONCEALED => "\e[8m", BLACK => "\e[30m", RED => "\e[31m", GREEN =>
  "\e[32m", YELLOW => "\e[33m", BLUE => "\e[34m", MAGENTA => "\e[35m",
  CYAN => "\e[36m", WHITE => "\e[37m", GREY => "\e[90m", GRAY =>
  "\e[90m", BRIGHT_RED => "\e[91m", BRIGHT_GREEN => "\e[92m",
  BRIGHT_YELLOW => "\e[93m", BRIGHT_BLUE => "\e[94m", BRIGHT_MAGENTA
  => "\e[95m", BRIGHT_CYAN => "\e[96m", ON_BLACK => "\e[40m", ON_RED
  => "\e[41m", ON_GREEN => "\e[42m", ON_YELLOW => "\e[43m", ON_BLUE =>
  "\e[44m", ON_MAGENTA => "\e[45m", ON_CYAN => "\e[46m", ON_WHITE =>
  "\e[47m", ON_GREY => "\e[100m", ON_GRAY => "\e[100m", ON_BRIGHT_RED
  => "\e[101m", ON_BRIGHT_GREEN => "\e[102m", ON_BRIGHT_YELLOW =>
  "\e[103m", ON_BRIGHT_BLUE => "\e[104m", ON_BRIGHT_MAGENTA =>
  "\e[105m", ON_BRIGHT_CYAN => "\e[106m" );
#END: %color


=head3 colors

At the most basic level, converts colors to different formats, however this
subroutine is capable of quite a bit more than that.

Examples:

 colors [qw/red green blue/], format => "ps";
 colors [qw/red green blue/], format => "ps", n => 2;

=over 4

=item colors

A list of colors, can be an X11 color name or any of the other formats
recognized by Color::Calc.

=item n

Only return n colors.

=item interpolate

If false, requesting more colors than available in the colors list will
throw a fatal error. The default is to create new colors between the given
colors if there are insufficient colors provided. The interpolate command
will also cause colors to be interpolated if the distribute option is set.

=item distribute

By default, if fewer colors are requested than are contained in the colors
list, this subroutine will select the first n colors. Providing a true
value for distribute will cause the subroutine to evenly spread out the
choice of colors over the range of colors provided (if n E<gt> 2 then the
first and last colors are guaranteed to be included).

=item format

Specify the style of the returned colors. Can be anything supported by
Color::Calc which is currently (Color::Calc::VERSION == 1.0): "tuple",
"hex", "html", "object" (a Graphics::ColorObject object), "pdf". The
default format is "object".

The following formats are also accepted and are handled by this subroutine
directly: "ps" | "postscript".

=item background

Try to make the colors appear on the given background color. Colors B<will>
be altered if this option is provided.

=back

=cut

#BEGIN: colors
sub colors {
  require Color::Calc;
  unshift @_, "colors" if @_ % 2;
  my %o = @_;
  my $c = $o{colors};
  my $n = $o{n} ||= @$c;
  $o{interpolate} = 1 unless exists $o{interpolate};
  croak "Not enough "."colors in plot color database" unless $o{interpolate} or $n <= @$c;

  $o{format} ||= 'object';
  $o{format}   = lc $o{format};

  @o{qw/format _format/} = qw/tuple ps/ if $o{format} =~ /^(?:ps|postscript)$/;
  my $cct = Color::Calc->new( 'ColorScheme' => 'X', OutputFormat => 'tuple' );
  my $cc  = Color::Calc->new( 'ColorScheme' => 'X', OutputFormat => $o{format} );

  $o{listed} = 1 if $o{format} eq 'tuple';

  if (($n > @$c) or ($o{distribute} and $o{interpolate})) {
    $c = [map [($_==int($_))?$$c[$_]:$cct->mix($$c[int $_],$$c[int($_)+1], $_-int($_))], map +($_*($#{$c})/(($n-1)||1)), 0..$n-1];
  } elsif ($o{distribute} and $n < @$c) {
    $c = [map $$c[int($_*($#{$c})/(($n-1)||1))], 0..$n-1];
  }

  if ($o{background}) {
    my $fg = $cc->contrast_bw($o{background});
    if ($o{listed}) {
      $c = [ map [$cc->mix($_, $fg, .35)], @$c[0..$n-1] ];
    } else {
      $c = [ map $cc->mix($_, $fg, .35), @$c[0..$n-1] ];
    }
  } else {
    if ($o{listed}) {
      $c = [ map [$cc->get($_)], @$c[0..$n-1] ];
    } else {
      $c = [ map $cc->get($_), @$c[0..$n-1] ];
    }
  }

  if ($o{_format}) {
    $o{_format} eq 'ps' and do { for (@$c) { $_ /= 255 for @$_ } };
  }

  return wantarray ? @$c : $c;
}
#END: colors


#-----------------------------------------------------------------
                      $EXPORT_TAGS{color_subs}
                                 =
[qw/ BOLD UNDERLINE DARK BLINK REVERSE CONCEALED STRIKE
     BLACK RED GREEN YELLOW BLUE MAGENTA CYAN WHITE
     GREY GRAY BRIGHT_RED BRIGHT_GREEN BRIGHT_YELLOW BRIGHT_BLUE BRIGHT_MAGENTA BRIGHT_CYAN
     ON_BLACK ON_RED ON_GREEN ON_YELLOW ON_BLUE ON_MAGENTA ON_CYAN ON_WHITE
     ON_GREY ON_GRAY ON_BRIGHT_RED ON_BRIGHT_GREEN ON_BRIGHT_YELLOW ON_BRIGHT_BLUE ON_BRIGHT_MAGENTA ON_BRIGHT_CYAN
/];
#-----------------------------------------------------------------

=head2 :color_subs - Color Subroutines

=cut

=head3 BOLD($)

Make text bold

=cut

#BEGIN: BOLD, 1 line
sub BOLD :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[1m".$x."\e[22m" : '' }

=head3 DARK($)

Make text dark

=cut

#BEGIN: DARK, 1 line
sub DARK :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[2m".$x."\e[22m" : '' }

=head3 UNDERLINE($)

Make text underline

=cut

#BEGIN: UNDERLINE, 1 line
sub UNDERLINE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[4m".$x."\e[24m" : '' }

=head3 BLINK($)

Make text blink

=cut

#BEGIN: BLINK, 1 line
sub BLINK :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[5m".$x."\e[25m" : '' }

=head3 REVERSE($)

Make text reverse

=cut

#BEGIN: REVERSE, 1 line
sub REVERSE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[7m".$x."\e[27m" : '' }

=head3 CONCEALED($)

Make text concealed

=cut

#BEGIN: CONCEALED, 1 line
sub CONCEALED :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[8m".$x."\e[28m" : '' }

=head3 STRIKE($)

Strike-through text (rarely implemented)

=cut

#BEGIN: STRIKE, 1 line
sub STRIKE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[9m".$x."\e[29m" : '' }

=head3 BLACK($)

Make text black

=cut

#BEGIN: BLACK, 1 line
sub BLACK :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[30m".$x."\e[39m" : '' }

=head3 RED($)

Make text red

=cut

#BEGIN: RED, 1 line
sub RED :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[31m".$x."\e[39m" : '' }

=head3 GREEN($)

Make text green

=cut

#BEGIN: GREEN, 1 line
sub GREEN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[32m".$x."\e[39m" : '' }

=head3 YELLOW($)

Make text yellow

=cut

#BEGIN: YELLOW, 1 line
sub YELLOW :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[33m".$x."\e[39m" : '' }

=head3 BLUE($)

Make text blue

=cut

#BEGIN: BLUE, 1 line
sub BLUE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[34m".$x."\e[39m" : '' }

=head3 MAGENTA($)

Make text magenta

=cut

#BEGIN: MAGENTA, 1 line
sub MAGENTA :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[35m".$x."\e[39m" : '' }

=head3 CYAN($)

Make text cyan

=cut

#BEGIN: CYAN, 1 line
sub CYAN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[36m".$x."\e[39m" : '' }

=head3 WHITE($)

Make text white

=cut

#BEGIN: WHITE, 1 line
sub WHITE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[37m".$x."\e[39m" : '' }

=head3 GREY($)

Make text grey

=cut

#BEGIN: GREY, 1 line
sub GREY :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[90m".$x."\e[39m" : '' }

=head3 GRAY($)

Make text gray

=cut

#BEGIN: GRAY, 1 line
sub GRAY :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[90m".$x."\e[39m" : '' }

=head3 BRIGHT_RED($)

Make text bright_red

=cut

#BEGIN: BRIGHT_RED, 1 line
sub BRIGHT_RED :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[91m".$x."\e[39m" : '' }

=head3 BRIGHT_GREEN($)

Make text bright_green

=cut

#BEGIN: BRIGHT_GREEN, 1 line
sub BRIGHT_GREEN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[92m".$x."\e[39m" : '' }

=head3 BRIGHT_YELLOW($)

Make text bright_yellow

=cut

#BEGIN: BRIGHT_YELLOW, 1 line
sub BRIGHT_YELLOW :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[93m".$x."\e[39m" : '' }

=head3 BRIGHT_BLUE($)

Make text bright_blue

=cut

#BEGIN: BRIGHT_BLUE, 1 line
sub BRIGHT_BLUE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[94m".$x."\e[39m" : '' }

=head3 BRIGHT_MAGENTA($)

Make text bright_magenta

=cut

#BEGIN: BRIGHT_MAGENTA, 1 line
sub BRIGHT_MAGENTA :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[95m".$x."\e[39m" : '' }

=head3 BRIGHT_CYAN($)

Make text bright_cyan

=cut

#BEGIN: BRIGHT_CYAN, 1 line
sub BRIGHT_CYAN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[96m".$x."\e[39m" : '' }

=head3 ON_BLACK($)

Make text on_black

=cut

#BEGIN: ON_BLACK, 1 line
sub ON_BLACK :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[40m".$x."\e[49m" : '' }

=head3 ON_RED($)

Make text on_red

=cut

#BEGIN: ON_RED, 1 line
sub ON_RED :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[41m".$x."\e[49m" : '' }

=head3 ON_GREEN($)

Make text on_green

=cut

#BEGIN: ON_GREEN, 1 line
sub ON_GREEN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[42m".$x."\e[49m" : '' }

=head3 ON_YELLOW($)

Make text on_yellow

=cut

#BEGIN: ON_YELLOW, 1 line
sub ON_YELLOW :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[43m".$x."\e[49m" : '' }

=head3 ON_BLUE($)

Make text on_blue

=cut

#BEGIN: ON_BLUE, 1 line
sub ON_BLUE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[44m".$x."\e[49m" : '' }

=head3 ON_MAGENTA($)

Make text on_magenta

=cut

#BEGIN: ON_MAGENTA, 1 line
sub ON_MAGENTA :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[45m".$x."\e[49m" : '' }

=head3 ON_CYAN($)

Make text on_cyan

=cut

#BEGIN: ON_CYAN, 1 line
sub ON_CYAN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[46m".$x."\e[49m" : '' }

=head3 ON_WHITE($)

Make text on_white

=cut

#BEGIN: ON_WHITE, 1 line
sub ON_WHITE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[47m".$x."\e[49m" : '' }

=head3 ON_GREY($)

Make text on_grey

=cut

#BEGIN: ON_GREY, 1 line
sub ON_GREY :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[100m".$x."\e[49m" : '' }

=head3 ON_GRAY($)

Make text on_gray

=cut

#BEGIN: ON_GRAY, 1 line
sub ON_GRAY :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[100m".$x."\e[49m" : '' }

=head3 ON_BRIGHT_RED($)

Make text on_bright_red

=cut

#BEGIN: ON_BRIGHT_RED, 1 line
sub ON_BRIGHT_RED :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[101m".$x."\e[49m" : '' }

=head3 ON_BRIGHT_GREEN($)

Make text on_bright_green

=cut

#BEGIN: ON_BRIGHT_GREEN, 1 line
sub ON_BRIGHT_GREEN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[102m".$x."\e[49m" : '' }

=head3 ON_BRIGHT_YELLOW($)

Make text on_bright_yellow

=cut

#BEGIN: ON_BRIGHT_YELLOW, 1 line
sub ON_BRIGHT_YELLOW :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[103m".$x."\e[49m" : '' }

=head3 ON_BRIGHT_BLUE($)

Make text on_bright_blue

=cut

#BEGIN: ON_BRIGHT_BLUE, 1 line
sub ON_BRIGHT_BLUE :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[104m".$x."\e[49m" : '' }

=head3 ON_BRIGHT_MAGENTA($)

Make text on_bright_magenta

=cut

#BEGIN: ON_BRIGHT_MAGENTA, 1 line
sub ON_BRIGHT_MAGENTA :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[105m".$x."\e[49m" : '' }

=head3 ON_BRIGHT_CYAN($)

Make text on_bright_cyan

=cut

#BEGIN: ON_BRIGHT_CYAN, 1 line
sub ON_BRIGHT_CYAN :prototype($) { my $x = shift; (defined $x and $x ne '') ? "\e[106m".$x."\e[49m" : '' }


#-----------------------------------------------------------------
                    $EXPORT_TAGS{color_strings}
                                 =
[qw/ $BOLD $BOLD_OFF $UNDERLINE $UNDERLINE_OFF $DARK $DARK_OFF $BLINK $BLINK_OFF $REVERSE $REVERSE_OFF
     $CONCEALED $CONCEALED_OFF $STRIKE $STRIKE_OFF $NORMAL $DEFAULT_FG $DEFAULT_BG
     $BLACK $RED $GREEN $YELLOW $BLUE $MAGENTA $CYAN $WHITE
     $GREY $GRAY $BRIGHT_RED $BRIGHT_GREEN $BRIGHT_YELLOW $BRIGHT_BLUE $BRIGHT_MAGENTA $BRIGHT_CYAN
     $ON_BLACK $ON_RED $ON_GREEN $ON_YELLOW $ON_BLUE $ON_MAGENTA $ON_CYAN $ON_WHITE
     $ON_GREY $ON_GRAY $ON_BRIGHT_RED $ON_BRIGHT_GREEN $ON_BRIGHT_YELLOW $ON_BRIGHT_BLUE $ON_BRIGHT_MAGENTA $ON_BRIGHT_CYAN
/];
#-----------------------------------------------------------------

=head2 :color_strings - Color Strings

=cut

=head3 $NORMAL

Undo all color modifications

=cut

#BEGIN: $NORMAL, 1 line
our $NORMAL = "\e[0m";

=head3 $DEFAULT_FG

Remove foreground coloring

=cut

#BEGIN: $DEFAULT_FG, 1 line
our $DEFAULT_FG = "\e[39m";

=head3 $DEFAULT_BG

Remove background coloring

=cut

#BEGIN: $DEFAULT_BG, 1 line
our $DEFAULT_BG = "\e[49m";

=head3 $BOLD

Make text bold

=cut

#BEGIN: $BOLD, 1 line
our $BOLD = "\e[1m";

=head3 $BOLD_OFF

Undo make text bold

=cut

#BEGIN: $BOLD_OFF, 1 line
our $BOLD_OFF = "\e[22m";

=head3 $DARK

Make text dark

=cut

#BEGIN: $DARK, 1 line
our $DARK = "\e[2m";

=head3 $DARK_OFF

Undo make text dark

=cut

#BEGIN: $DARK_OFF, 1 line
our $DARK_OFF = "\e[22m";

=head3 $UNDERLINE

Make text underline

=cut

#BEGIN: $UNDERLINE, 1 line
our $UNDERLINE = "\e[4m";

=head3 $UNDERLINE_OFF

Undo make text underline

=cut

#BEGIN: $UNDERLINE_OFF, 1 line
our $UNDERLINE_OFF = "\e[24m";

=head3 $BLINK

Make text blink

=cut

#BEGIN: $BLINK, 1 line
our $BLINK = "\e[5m";

=head3 $BLINK_OFF

Undo make text blink

=cut

#BEGIN: $BLINK_OFF, 1 line
our $BLINK_OFF = "\e[25m";

=head3 $REVERSE

Make text reverse

=cut

#BEGIN: $REVERSE, 1 line
our $REVERSE = "\e[7m";

=head3 $REVERSE_OFF

Undo make text reverse

=cut

#BEGIN: $REVERSE_OFF, 1 line
our $REVERSE_OFF = "\e[27m";

=head3 $CONCEALED

Make text concealed

=cut

#BEGIN: $CONCEALED, 1 line
our $CONCEALED = "\e[8m";

=head3 $CONCEALED_OFF

Undo make text concealed

=cut

#BEGIN: $CONCEALED_OFF, 1 line
our $CONCEALED_OFF = "\e[28m";

=head3 $STRIKE

Make text strike-through

=cut

#BEGIN: $STRIKE, 1 line
our $STRIKE = "\e[9m";

=head3 $STRIKE_OFF

Undo make text strike-through

=cut

#BEGIN: $STRIKE_OFF, 1 line
our $STRIKE_OFF = "\e[9m";

=head3 $BLACK

Make text black

=cut

#BEGIN: $BLACK, 1 line
our $BLACK = "\e[30m";

=head3 $RED

Make text red

=cut

#BEGIN: $RED, 1 line
our $RED = "\e[31m";

=head3 $GREEN

Make text green

=cut

#BEGIN: $GREEN, 1 line
our $GREEN = "\e[32m";

=head3 $YELLOW

Make text yellow

=cut

#BEGIN: $YELLOW, 1 line
our $YELLOW = "\e[33m";

=head3 $BLUE

Make text blue

=cut

#BEGIN: $BLUE, 1 line
our $BLUE = "\e[34m";

=head3 $MAGENTA

Make text magenta

=cut

#BEGIN: $MAGENTA, 1 line
our $MAGENTA = "\e[35m";

=head3 $CYAN

Make text cyan

=cut

#BEGIN: $CYAN, 1 line
our $CYAN = "\e[36m";

=head3 $WHITE

Make text white

=cut

#BEGIN: $WHITE, 1 line
our $WHITE = "\e[37m";

=head3 $GREY

Make text grey

=cut

#BEGIN: $GREY, 1 line
our $GREY = "\e[90m";

=head3 $GRAY

Make text gray

=cut

#BEGIN: $GRAY, 1 line
our $GRAY = "\e[90m";

=head3 $BRIGHT_RED

Make text bright_red

=cut

#BEGIN: $BRIGHT_RED, 1 line
our $BRIGHT_RED = "\e[91m";

=head3 $BRIGHT_GREEN

Make text bright_green

=cut

#BEGIN: $BRIGHT_GREEN, 1 line
our $BRIGHT_GREEN = "\e[92m";

=head3 $BRIGHT_YELLOW

Make text bright_yellow

=cut

#BEGIN: $BRIGHT_YELLOW, 1 line
our $BRIGHT_YELLOW = "\e[93m";

=head3 $BRIGHT_BLUE

Make text bright_blue

=cut

#BEGIN: $BRIGHT_BLUE, 1 line
our $BRIGHT_BLUE = "\e[94m";

=head3 $BRIGHT_MAGENTA

Make text bright_magenta

=cut

#BEGIN: $BRIGHT_MAGENTA, 1 line
our $BRIGHT_MAGENTA = "\e[95m";

=head3 $BRIGHT_CYAN

Make text bright_cyan

=cut

#BEGIN: $BRIGHT_CYAN, 1 line
our $BRIGHT_CYAN = "\e[96m";

=head3 $ON_BLACK

Make text on_black

=cut

#BEGIN: $ON_BLACK, 1 line
our $ON_BLACK = "\e[40m";

=head3 $ON_RED

Make text on_red

=cut

#BEGIN: $ON_RED, 1 line
our $ON_RED = "\e[41m";


=head3 $ON_GREEN

Make text on_green

=cut

#BEGIN: $ON_GREEN, 1 line
our $ON_GREEN = "\e[42m";

=head3 $ON_YELLOW

Make text on_yellow

=cut

#BEGIN: $ON_YELLOW, 1 line
our $ON_YELLOW = "\e[43m";

=head3 $ON_BLUE

Make text on_blue

=cut

#BEGIN: $ON_BLUE, 1 line
our $ON_BLUE = "\e[44m";

=head3 $ON_MAGENTA

Make text on_magenta

=cut

#BEGIN: $ON_MAGENTA, 1 line
our $ON_MAGENTA = "\e[45m";


=head3 $ON_CYAN

Make text on_cyan

=cut

#BEGIN: $ON_CYAN, 1 line
our $ON_CYAN = "\e[46m";


=head3 $ON_WHITE

Make text on_white

=cut

#BEGIN: $ON_WHITE, 1 line
our $ON_WHITE = "\e[47m";

=head3 $ON_GREY

Make text on_grey

=cut

#BEGIN: $ON_GREY, 1 line
our $ON_GREY = "\e[100m";

=head3 $ON_GRAY

Make text on_gray

=cut

#BEGIN: $ON_GRAY, 1 line
our $ON_GRAY = "\e[100m";

=head3 $ON_BRIGHT_RED

Make text on_bright_red

=cut

#BEGIN: $ON_BRIGHT_RED, 1 line
our $ON_BRIGHT_RED = "\e[101m";

=head3 $ON_BRIGHT_GREEN

Make text on_bright_green

=cut

#BEGIN: $ON_BRIGHT_GREEN, 1 line
our $ON_BRIGHT_GREEN = "\e[102m";

=head3 $ON_BRIGHT_YELLOW

Make text on_bright_yellow

=cut

#BEGIN: $ON_BRIGHT_YELLOW, 1 line
our $ON_BRIGHT_YELLOW = "\e[103m";

=head3 $ON_BRIGHT_BLUE

Make text on_bright_blue

=cut

#BEGIN: $ON_BRIGHT_BLUE, 1 line
our $ON_BRIGHT_BLUE = "\e[104m";

=head3 $ON_BRIGHT_MAGENTA

Make text on_bright_magenta

=cut

#BEGIN: $ON_BRIGHT_MAGENTA, 1 line
our $ON_BRIGHT_MAGENTA = "\e[105m";

=head3 $ON_BRIGHT_CYAN

Make text on_bright_cyan

=cut

#BEGIN: $ON_BRIGHT_CYAN, 1 line
our $ON_BRIGHT_CYAN = "\e[106m";




#-----------------------------------------------------------------
                       $EXPORT_TAGS{display}
                                 =
[qw/sprint_hash print_hash text_wrap_paragraphs text_wrap
    text_justify_paragraphs text_justify print_cols format_cols
    histogram ctext lrtext clprint mk_progressbar
    sprint_one_var
   /];
#-----------------------------------------------------------------

=head2 :display - Display functions

=head3 sprint_one_var

 binmode STDOUT, ":encoding(UTF-8)";
 print sprint_one_var scalar one_var \@data;

Returns a string describing data set, such as:

  N =    12: μ = 11.42, σ =  9.19  95% CI ( 6.11, 16.72)  «  3.55,  6.70,  7.72, 12.05, 36.25 »

=over 4

=item five_nr

Show five number summary (default true)

=item ci

Show 95% confidence interval (default false)

=item dpad

Width of digit (N) field (default 3)

=item pad

Width of floating point fields (default 5)

=item digits

Digits after decimal, use negative number to format with nicef instead of
%f (default 2)

=back

=cut

#BEGIN: sprint_one_var, DEPENDS: nicef
sub sprint_one_var {
    use utf8;
    my $d = shift;
    my %o = ( dpad => 3, pad => 5, digits => 2, five_nr => 1, @_ );
    my @info;
    $$d{se} //= $$d{sigma} / sqrt($$d{n}) if $$d{n};

    push @info, sprintf "N = %$o{dpad}d: μ = %$o{pad}s, σ = %$o{pad}s",
        map +($o{digits} <= 0 ? nicef($_, (-$o{digits})||2) : sprintf("%.$o{digits}f", $_)),
        @$d{qw/ n mean sigma /};

    push @info, sprintf "95%% CI (%$o{pad}s, %$o{pad}s)",
        map +($o{digits} <= 0 ? nicef($_, (-$o{digits})||2) : sprintf("%.$o{digits}f", $_)),
        $$d{mean} - 2*$$d{se}, $$d{mean} + 2*$$d{se}
        if $o{ci};

    push @info, sprintf "5-nr « %$o{pad}s, %$o{pad}s, %$o{pad}s, %$o{pad}s, %$o{pad}s »",
        map +($o{digits} <= 0 ? nicef($_, (-$o{digits})||2) : sprintf("%.$o{digits}f", $_)),
        @$d{qw/ min   Q1   Q2   Q3  max /}
        if $o{five_nr};

    return join "  ", @info;
}
#END: sprint_one_var


=head3 mk_progressbar

Generates a progress subroutine. Sample usage might be (you provide the
C<$items> iterator and C<do_something> sub or something equivalent):

 my $nr_items = $items->count;
 my $progress = mk_progressbar( total => $nr_items, countdown => 1 );
 print STDERR "Processing items ";
 while (my $item = $items->next) {
   $progress->($nr_items--);
   do_something($item);
 }
 $progress->(0);

With the above, your code now has a nice progress bar.

=over 4

=item type

"bar", "dot", "percent", or "spinner". DEFAULT: bar

=item total

Number of items to process. Note: "total" and progress counts may be
decimal. DEFAULT: 1

=item countdown

When true, progress sub expects value to decrease from total to 0 rather
than increase from 0 to total. DEFAULT: undef (false)

=item format (percent type only)

sprintf format to display percentage. DEFAULT: "%.2f%%"

=item length (bar type only)

DEFAULT: 20

=item symbol (bar and dot types only)

DEFAULT: "*" for bar; "." for dot

=item symbols (spinner type only)

DEFAULT: [ qw( - \ | / ) ]

=item break (dot type only)

DEFAULT: 50

Print newline after every 10 dots.

=item space (dot type only)

DEFAULT: 10

Print space after every 10 dots.

=item fh

Output file handle. DEFAULT: STDERR

=item prefix

String to print before progress info.

=item suffix

String to print after progress info.

=back

=cut

#BEGIN: mk_progressbar, depends: clprint
sub mk_progressbar {
  my %opt = (
    # shared default options
    total => 1, fh => \*STDERR,
    # bar-type default options
    type => "bar", length => 20,
    # dot type default options
    break => 50, space => 10,
    # percent-type default options
    format => "%.2f%%",
    # spinner-type defaults
    symbols => [ qw( - \ | / ) ],
    @_
  );

  if ($opt{type} eq "bar") {
    my $printed = 0;
    $opt{symbol} //= "*";
    return sub {
      print { $opt{fh} } delete $opt{prefix} if defined($opt{prefix});
      my $val = $opt{countdown} ? $opt{total}-$_[0] : $_[0];
      my $wanted = int( .5 + $opt{length} * $val / $opt{total} );
      $wanted = $opt{length} if $val >= $opt{total};
      $printed += print { $opt{fh} } $opt{symbol} while $printed < $wanted;
      print { $opt{fh} } delete $opt{suffix} if defined($opt{suffix}) and $printed == $opt{length};
    };
  }

  elsif ($opt{type} eq "dot") {
      require IO::Handle;
      my $major = 0;
      my $minor = 0;
      $opt{symbol} //= ".";
      $opt{fh}->autoflush(1);
      return sub {
          my $val = $opt{countdown} ? $opt{total}-$_[0] : $_[0];
          print { $opt{fh} } $opt{prefix}//"" if 0 == $major;
          $minor++;
          $major++;

          if ($major > $opt{break}) {
              print { $opt{fh} } $opt{suffix}//"\n";
              $minor = $major = 1;
          }

          if ($minor > $opt{space}) {
              print { $opt{fh} } " ";
              $minor = 1;
          }

          print { $opt{fh} } $opt{symbol};
      };
  }

  elsif ($opt{type} eq "percent") {
    my $buffer = "";
    my $suffix = defined($opt{suffix}) ? $opt{suffix} : "";
    return sub {
      print { $opt{fh} } delete $opt{prefix} if defined($opt{prefix});
      my $val = $opt{countdown} ? $opt{total}-$_[0] : $_[0];
      my $percent = $val / $opt{total};
      $percent = 1 if $percent > 1;
      clprint($opt{fh}, \$buffer, sprintf($opt{format},100*$percent).$suffix);
    };
  }

  elsif ($opt{type} eq "spinner") {
    my $buffer = "";
    my $idx = 0;
    my @symbols = @{ $opt{symbols} };
    my $suffix = defined($opt{suffix}) ? $opt{suffix} : "";
    return sub {
      print { $opt{fh} } delete $opt{prefix} if defined($opt{prefix});
      clprint($opt{fh}, \$buffer, $symbols[($idx %= $#symbols)++].$suffix);
    };
  }

}
#END: mk_progressbar

=head3 clprint

  my ($i, @mark) = (0, qw[ - \ | / ]);
  print "Working: ";
  for (@things) {
    clprint $mark[($i %= 4)++];
    # ... other stuff ...
  }
  clprint;

  clprint \$var, @stuff;
  clprint \*STDOUT, \$var, @stuff;

A CLearing print. Erases whatever was printed last time and prints the next
thing. This subroutine is smart enough not to try to erase past a newline
even if you are using the perl variables C<$,> or C<$\>. This subroutine
makes use of the clength subroutine so that color escape sequences are
properly measured.

Calling the subroutine with no arguments forgets the previously printed
thing without erasing it from the screen.

If a C<GLOB> or C<IO::*> is given as a first parameter then, that will be
used for output. The default is C<STDERR> and is stored in the
C<$_Util::clprint::out> variable if you want to change it.

If a reference to a scalar is given then that variable will be used to
store the text history. This allows for multiple clprint levels. (Though it
is up to you to nest them properly.)

=cut

#BEGIN: clprint, depends: clength
{ my $var = "";
  $_Util::clprint::out = \*STDERR;
  sub clprint {
    local $_Util::clprint::out = (@_ and ref($_[0]) =~ /^(?:GLOB|IO::)/) ? shift() : $_Util::clprint::out;
    my $previous = (@_ and ref($_[0]) eq 'SCALAR') ? shift() : \$var;
    my $i;
    if (@_) {
      print $_Util::clprint::out "\b \b" x clength($$previous);
      $$previous = (defined $,) ? join($, , @_) : join("" , @_);
      print $_Util::clprint::out $$previous;
      $$previous .= $\ if defined $\;
      $$previous = substr $$previous, $i+1 if defined ($i = rindex($$previous, "\n")) and $i >= 0;
    } else {
      $$previous = "";
    }
  }
}
#END: clprint


=head3 sprint_hash

 sprint_hash $sep, %hash

Returns a string:

 "key" => "value"$sep"key" => "value"$sep...

If C<$sep> is not provided (I.E., sprint_hash is called with an even number
of arguments) C<$sep> will default to C<$/> (typically "\n").

=cut

=head3 print_hash

Prints the results of L<sprint_hash|/sprint_hash>

=cut

#BEGIN: sprint_hash, 1 line
sub sprint_hash  {my$sep=$/;$sep=shift()if@_%2;my$x='';$x.= '"'.shift().'" => "'.shift().'"'.$sep while @_;$x}
#BEGIN: print_hash, 1 line; depends: sprint_hash
sub print_hash   {print sprint_hash(@_)}

=head3 ctext

 ctext( $text, $width, "left" | "right" )

Center a string horizontally over a given width (both left and right sides
are padded with space). An optional third parameter specifies whether to
err to the left or to the right. The default is left, to put an extra space
to the right if necessary. undef is returned if C<$width E<lt> length
$text>.

=cut

#BEGIN: ctext, depends: clength, ceil
sub ctext {
  my ($text, $width, $err) = @_;
  my $l = clength($text);
  return unless $width >= $l;
  if ($err and lc($err) eq 'right') {
    return (' ' x ceil(($width-$l)/2)) . $text . (' ' x int(($width-$l)/2));
  } else {
    return (' ' x int(($width-$l)/2)) . $text . (' ' x ceil(($width-$l)/2));
  }
}
#END: ctext

=head3 lrtext

 lrtext( $left, $right, $width )

Return a string with enough space separating the C<$left> and C<$right>
text so that the line fills the entire C<$width>.

=cut

#BEGIN: lrtext, depends: clength
{ use Carp;
  sub lrtext {
    my ($l, $r, $w) = @_;
    $w = $w - clength($l) - clength($r);
    carp "With too small for text" if $w < 0;
    $l . (' ' x $w) . $r;
  }
}
#END: lrtext



=head3 text_wrap_paragraphs

See also: Text::Autoformat

Splits a string on multiple consecutive newlines and passes each chunk to
L<text_wrap|/text_wrap>. Returns the resulting paragraphs as a list of
paragraphs. This function takes the same arguments as
L<text_wrap|/text_wrap>.

=cut

#BEGIN: text_wrap_paragraphs, 4 lines, depends: text_wrap
sub text_wrap_paragraphs {
  my @para = grep /\S/, split /\n\s*\n/, shift;
  @para = map text_wrap($_, @_), @para;
}

=head3 text_wrap

See also: Text::Autoformat

Takes a string and wraps the test to be at most a certain width. Text is
split at whitespace, and hyphens (though actual hyphenation is beyond the
scope of my interest). Long words are placed on lines by themselves, all
whitespace is canonicalized, and the resulting string does not have a
trailing newline.

This function uses the non-core package Term::ReadKey. Available options:

=over 4

=item width

Total width of the paragraph, including any indentation. The default is the
width of the terminal or the value of C<$ENV{COLUMNS}> or 80. If width is
negative, then that value will be subtracted from whatever width is
auto-detected.

=item indent

A per-line indentation amount. The default is zero.

=item fill

If true, spaces will be added to the END of each line to make them exactly
the right width. You might want this if you are colorizing the background
so that the background color extends the full width on each line. The
default is false.

=item wrap_chars

A list of characters that we are allowed to wrap on. The default is C<[ '-', ' ' ]>.

=back

=cut

#BEGIN text_wrap, depends: clength
sub text_wrap {
  my $text = shift;
  my %opt = ( indent => 0, fill => 0, wrap_chars => ['-',' '], @_ );
  my $wrap_chars = join '|', map quotemeta, @{$opt{wrap_chars}};
  unless ($opt{width} and $opt{width} > 0) {
    require Term::ReadKey;
    $opt{_width} = (Term::ReadKey::GetTerminalSize(\*STDOUT))[0] || $ENV{COLUMNS} || 80;
    $opt{width} = $opt{_width} + ($opt{width}||0)
  }

  for ($text) { s/\s+/ /g; s/^\s+//; s/\s+$//; }
  my @words = split /(?<=$wrap_chars)/, $text;
  $text = '';
  while (@words) {
    my $line = ' ' x $opt{indent};
    $line .= shift @words while @words and $opt{width} >= clength($line . $words[0])-((substr($words[0],-1)eq' ')?1:0);
    if ($line !~ /\S/) { $line .= shift @words } # word is too big, bummer
    $line =~ s/ $//;
    $line .= ' ' x ($opt{width}-clength($line)) if $opt{fill};
    $text .= $line . $/;
  }
  chomp($text);
  return $text;
}
#END text_wrap


=head3 text_justify_paragraphs

Splits a string on multiple consecutive newlines and passes each chunk to
L<text_justify|/text_justify>. Returns the resulting paragraphs as a list
of paragraphs. This function takes the same arguments as
L<text_justify|/text_justify>.

=cut

#BEGIN: text_justify_paragraphs, 4 lines, depends: text_justify
sub text_justify_paragraphs {
  my @para = grep /\S/, split /\n\s*\n/, shift;
  @para = map text_justify($_, @_), @para;
}

=head3 text_justify

Takes a string and wraps the test to exactly be a certain width. Text is
split at whitespace, and hyphens (though actual hyphenation is beyond the
scope of my interest). Long words are placed on lines by themselves, all
whitespace is canonicalized, and the resulting string does not have a
trailing newline.

This function uses the non-core package Term::ReadKey. Available options:

=over 4

=item width

Total width of the paragraph, including any indentation. The default is the
width of the terminal or the value of C<$ENV{COLUMNS}> or 80. If width is
negative, then that value will be subtracted from whatever width is
auto-detected.

=item indent

A per-line indentation amount. The default is zero.

=item justify_last

If true the last line of the paragraph will be justified also. The default
is false.

=item fill

If true, spaces will be added to the END of each line to make them exactly
the right width. You might want this if you are colorizing the background
so that the background color extends the full width on each line. The
default is true.

=item wrap_chars

A list of characters that we are allowed to wrap on. The default is C<[ '-', ' ' ]>.

=back

=cut

# Note: Algorithm could be better. For instance:
#
# Splits  a  string on multiple consecutive newlines
# and  passes each chunk to C<text_justify>. Returns
# the  resulting paragraphs as a list of paragraphs.
# This   function   takes   the  same  arguments  as
# C<text_justify>
#
# The spacing around "a" in first line should be spread out to the later
# parts. Perhaps a random distribution of extra space, or distribution
# based on neighboring word length. Maybe check the lynx source code.

#BEGIN: text_justify, depends: text_wrap, clength, ceil
sub text_justify {
  my $text = shift;
  my $last = '';
  my %opt = ( indent => 0, justify_last => 0, fill => 1, @_ );
  unless ($opt{width} and $opt{width} > 0) {
    require Term::ReadKey;
    $opt{_width} = (Term::ReadKey::GetTerminalSize(\*STDOUT))[0] || $ENV{COLUMNS} || 80;
    $opt{width} = $opt{_width} + ($opt{width}||0)
  }

  my @lines = split /\n/, text_wrap($text, %opt, fill => 0);
  $last = pop @lines unless $opt{justify_last};

  for my $line (@lines) {
    my $delta = $opt{width} - clength($line);
    next unless $delta > 0;

    my @words = split /(?<=\S )/, $line;
    unless ($#words) { $line .= ' ' x $delta if $opt{fill}; next } # There's no justifying single words.

    my $w; $line = shift @words;
    while (@words) { $line .= (' ' x ($w = ceil($delta/@words))) . shift(@words); $delta -= $w; }
  }
  if ($last ne '') {
    $last .= ' ' x ($opt{width} - clength($last)) if $opt{fill};
    push @lines, $last;
  }
  return join $/, @lines;
}
#END: text_justify


=head3 print_cols

Prints the results of L<format_cols|/format_cols>

=cut

#BEGIN: print_cols, 1 line, depends: format_cols
sub print_cols { print format_cols(@_) }

=head3 format_cols

 format_cols \@array, %options

Format the given list of items into columns according to the given options.
This function has a couple of improvements over Term::PrintCols. In
particular, it has more options, and is capable of correctly formatting
lists with embedded ANSI color codes. This function uses the non-core
package Term::ReadKey if the L<total_width|/total_width> option is not
specified. The layout algorithm was inspired by GNU ls.

Available options are.

=over 4

=item align =E<gt> alignment string

The alignment string is a word in the characters: l, r, c, standing for
Left, Right, and Center respectively. These control the alignment of each
column. The last character is repeated as many times as necessary for the
number of columns used in the formatted table. For example an alignment
string of "lc" would center all columns after the first. The default value
is "l".

=item col_width =E<gt> integer

The minimum allowed column width. This number will be used if there are no
items longer than the given integer minus L<col_space|/col_space> (to allow
for a space).

=item col_space =E<gt> integer

The minimal amount of spacing to place between each column. The actual
column spacing may be larger since this function expands the columns to
occupy the total available width. This value defaults to 2.

=item col_join =E<gt> string || array

String(s) used to join columns. This option overrides the C<col_space>
option. If more columns are used than elements available in the C<col_join>
array then the last element will be repeated for all subsequent column
dividers.

=item enumerate =E<gt> bool

=item enumerate_start =E<gt> integer

=item enumerate_append =E<gt> string

If C<enumerate> is true, right aligned numbers will be prepended to each
item. Enumeration will being with C<enumerate_start> (default 1), and the
C<enumerate_append> string will be appended to each number (default ". ").

=item indent =E<gt> integer

Amount of indentation to include on the left side of each line. This number
will be taken from the total_width option before the list is formatted. The
default is no indentation.

=item max_cols =E<gt> integer

The maximum number of columns to create. Sometimes it may be preferable to
specify the maximum number of columns rather than the minimum column width.

=item cols =E<gt> integer

The exact number of columns to create.

=item orientation =E<gt> 'horizontal' | 'vertical'

Specify whether the columns are to be filled horizontally or vertically.
For example, if the list of items is (1..9), then the resulting column
layouts would be:

 horizontal:              vertical:
    1  2  3  4               1  4  6  8
    5  6  7  8               2  5  7  9
    9                        3

The default orientation is vertical

=item total_width =E<gt> integer

The total number of terminal columns to use. This option tries to find the
correct width of the terminal first by using
L<Term::ReadKey|Term::ReadKey>, then by examining C<$ENV{COLUMNS}> and
finally defaults to 80 characters. If the total_width is negative, then
that value will be subtracted from whatever width is auto-detected.

=item max_width =E<gt> integer

The maximum number of terminal columns to use. The default is to not
constrain the "total_width".

=item fill_last_column =E<gt> bool

If true, spaces will be added to the end of the last column in the same way
that space is added to the end of all other columns. Otherwise, the last
column will not be space padded on the right. The default is false, do not
fill the right side of the last column.

=item uninitialized =E<gt> 'warn' | 'die' | 'ignore'

Behavior upon finding uninitialized values. The default is 'warn'.

=back

=cut

#BEGIN: format_cols, depends: ceil, min, clength, sum
{ # The column separator after the $n'th column
  my $col_space = sub {# 1 based "array"
    my ($opt, $n) = @_;
    return ' 'x$$opt{col_space} unless $$opt{col_join};
    return $$opt{col_join}[$n-1] if $#{$$opt{col_join}} >= $n-1;
    return $$opt{col_join}[-1];
  };
  # Total column space needed if we have $n columns
  my $total_col_space = sub {# 1 based "array"
    my ($opt, $n) = @_;
    return 0 if $n == 1;
    return ($n-1)*$$opt{col_space} unless $$opt{col_join};
    return sum(map length(), @{$$opt{col_join}}[0..$n-2]) if $#{$$opt{col_join}} >= $n-2;
    return sum(length($$opt{col_join}[-1]) * ($n-1-@{$$opt{col_join}}), map length(), @{$$opt{col_join}});
  };
sub format_cols {
  my ($array, %opt, @col_formats, %format);
  my $output = '';

  $array = shift;
  return unless $array and @$array;
  %opt = ( col_width => 1, indent => 0, orientation => 'vertical', col_space => 2, align => "l", uninitialized => 'warn',
           enumerate_start => 1, enumerate_append => '. ', ((@_ and ref($_[0]) eq 'HASH') ? %{$_[0]} : @_) );
  $opt{col_join} = [$opt{col_join}] if defined($opt{col_join}) and not ref($opt{col_join});

  unless ($opt{total_width} and $opt{total_width} > 0) {
    require Term::ReadKey;
    { local $SIG{__WARN__} = sub { 1 }; # Prevent "Unable to get Terminal Size." warning from Term::ReadKey
      $opt{total_width} = ((Term::ReadKey::GetTerminalSize(\*STDOUT))[0] || $ENV{COLUMNS} || 80) + ($opt{total_width} || 0);
    }
    if ($opt{max_width}) {
      $opt{total_width} = min($opt{total_width}, $opt{max_width});
    }
  }

  my $real_width = $opt{total_width} - $opt{indent};


  # We either know the number of columns or we have to try a bunch
  my $max_possible_columns = min( 0+@$array,
                                  $opt{max_cols} || $real_width,
                                  int( ($real_width + $opt{col_space}) / (($opt{col_width} + $opt{col_space})||1) )
                                );

  for ( $opt{cols} ? ($opt{cols}) : (1..$max_possible_columns) ) {
    push @col_formats, { total_width => $total_col_space->(\%opt, $_) + $_ * $opt{col_width},
                         col_widths  => [ ($opt{col_width})x$_ ],
                         cols => $_,
                         rows => ceil( @$array / $_ ),
                       };
  }

  my $enum_fmt = "%".length($opt{enumerate_start}+$#{$array})."d%s%s";

  # Find optimal column layout
  for my $i (0..$#{$array}) {
    my $val = $opt{enumerate} ? sprintf($enum_fmt, $i + $opt{enumerate_start}, $opt{enumerate_append}, $$array[$i] // '') : $$array[$i];
    unless (defined($$array[$i])) {
      carp  "Use of uninitialized value in column entry (index $i)" if $opt{uninitialized} eq 'warn';
      croak "Use of uninitialized value in column entry (index $i)" if $opt{uninitialized} eq 'die';
    }
    my $len = clength($val);
    for my $c (@col_formats) {
      my $pos = ($opt{orientation} eq 'horizontal')
                   ? $i % $$c{cols}
                   : int( $i / int((@$array + $$c{cols} - 1) / $$c{cols}) );

      if ($$c{col_widths}[$pos] < $len) {
        $$c{total_width} += $len - $$c{col_widths}[$pos];
        $$c{col_widths}[$pos] = $len;
      }
    }
  }

  for (reverse 0..$#col_formats) {
    if ($col_formats[$_]{total_width} < $real_width) {
      %format = %{$col_formats[$_]};
      last;
    }
  }
  %format = %{$col_formats[0]} unless %format;

  $opt{align} = [split //, $opt{align}];
  push @{$opt{align}}, ($opt{align}[-1])x($format{cols}-@{$opt{align}});

  # Construct the output
  for my $row (0..($format{rows}-1)) {
    $output .= ' ' x $opt{indent};
    for my $col (0..($format{cols}-1)) {
      my $idx = ($opt{orientation} eq 'horizontal')
                   ? ($row * $format{cols} + $col)
                   : ($row + $format{rows} * $col);
      next if $idx > $#{$array};
      my $val = $opt{enumerate} ? sprintf($enum_fmt, $idx + $opt{enumerate_start}, $opt{enumerate_append}, $$array[$idx]//'') : $$array[$idx];
      my ($lpad, $rpad) = (0,0);
      my $pad = $format{col_widths}[$col]-clength($val);
      if    ($opt{align}[$col] eq 'r') { $lpad = $pad }
      elsif ($opt{align}[$col] eq 'c') { $lpad = int($pad/2); $rpad = $pad - $lpad; }
      else { $rpad = $pad; }
      $rpad = 0 if $col == $format{cols}-1 and !$opt{fill_last_column};

      $output .= ' 'x($lpad);
      $output .= $val;
      $output .= ' 'x$rpad;
      $output .= $col_space->(\%opt, $col+1) unless $col == $format{cols}-1;
    }
    $output .= $/;
  }

  return $output;
}
}
#END: format_cols

=head3 histogram

 print histogram( \%data, %options )

Returns a text histogram. The data hash consists of C<id =E<gt> frequency>.
The graph looks best if the id's are short and all approximately the same
length. The following options may also be provided:

=over 4

=item height

The height of the tallest bar of the histogram. DEFAULT: 10

=item key_order

Either an array containing the order in which to display the histogram data
or the keyword 'sort'. DEFAULT: sort

Note: C<%data> may contain more data than is requested in C<key_order>. We
will only create a histogram with the C<key_order> data.

=item max_frequency

The largest frequency. You might want to provide this for two reasons. To
provide a uniform scaling over multiple histograms or as an optimization
(if you already have this value it would save us the work of recomputing
it). By default we will compute it from the key data that we are actually
displaying. DEFAULT: undef

=item bar_width

The width of each histogram bar. If undefined either 1 or the width of the
widest label will be used (depending on the value of
L<show_labels|/show_labels>). DEFAULT: undef

=item bar_char

The character to use to draw the bars. DEFAULT: "*"

=item col_skip

The inter-column spacing. DEFAULT: 1

=item indent

Amount of indentation to include on the left side of each line. DEFAULT: 0

=item axis_overhang

The distance beyond the end bars that the histogram extends. DEFAULT: 2

=item show_axis

Print a horizontal bar beneath the histogram but above the labels. DEFAULT:
true

=item show_labels

Print the labels centered under their respective bars. DEFAULT: true

=back

=cut

#BEGIN: histogram, depends: fmax, ctext
sub histogram {
  my $data = shift;
  my %o = ( height => 10, indent => 0, axis_overhang => 2, col_skip => 1,
            bar_char => "*", key_order => "sort",
            show_axis => 1, show_labels => 1,
            @_ );
  my @keys = (ref $o{key_order}) ? @{$o{key_order}} : sort keys %$data;
  my $colwidth = $o{show_labels} ? fmax(sub{length},@keys) : 1;
  $colwidth = $o{bar_width} if $o{bar_width}; # Can't (shouldn't allow to) be zero
  $o{axis_overhang} = 0 unless $o{show_axis};

  my $max        = $o{max_frequency};
  unless ($max) {
    $max = 1;
    exists($$data{$_}) and defined($$data{$_}) and $$data{$_} > $max and ($max = $$data{$_}) for @keys;
  }
  my $indent     = ' ' x  $o{indent};
  my $fullindent = ' ' x ($o{indent}+$o{axis_overhang});
  my $height     = $o{height};
  my $bar        = $o{bar_char} x $colwidth;
  my $space      = ' ' x $colwidth;
  my $colspace   = ' ' x $o{col_skip};

  my @chart;

  push @chart, $fullindent . join($colspace, map ctext($_,$colwidth), @keys) if $o{show_labels};
  push @chart, $indent . '-' x (2 * $o{axis_overhang} + $colwidth * @keys + $o{col_skip} * (@keys-1)) if $o{show_axis};

  for my $i (1..$height) {
    push @chart, $fullindent . join $colspace,
      map((.5+$height*((exists($$data{$_}) and defined($$data{$_})) ? $$data{$_} : 0)/$max < $i)? $space : $bar, @keys);
  }

  join $/, reverse @chart;
}
#END: histogram


#-----------------------------------------------------------------
                        $EXPORT_TAGS{input}
                                 =
[qw/prompt
    get_boolean Yn yN Tf tF yn tf
/];
#-----------------------------------------------------------------

=head2 :input - Prompting and input

=head3 get_boolean

 get_boolean { default => $d, true => $t, false => $f }, $input

Canonicalizes boolean input (with default). C<$input> may be a string or a
filehandle. If C<$input> is omitted then one line of data is read from
C<E<lt>STDINE<gt>>. This subroutine just tries to match either
C</^\s*[yYtT1]\w*\s*$/> or C</^\s*[nNfF0]\w*\s*$/>. Anything else causes
the default value to be returned.

If the default value is not set (which is not the same as being set to
C<undef>) then the input will be returned as-is if it does not appear to be
boolean or is empty or undefined. Using this subroutine in this way is
somewhat fragile since something like "truck" or "Typhoon" will be
canonicalized to "true" but "The Clash" will not. Thus, it is not very
sophisticated in its distinction between boolean and non-boolean inputs.

=cut

#BEGIN: get_boolean
sub get_boolean {
  my $h = shift;
  my $x;
  if (@_) {
    if (ref($_[0])) { $x = <$_[0]> }
    else { $x = shift }
  } else { $x = <STDIN> }
  return ((exists $$h{default})?$$h{default}:$x) unless defined $x and $x =~ /\S/;
  return $$h{true}    if $x =~ /^\s*[yYtT1]/;
  return $$h{false}   if $x =~ /^\s*[nNfF0]/;
  return ((exists $$h{default})?$$h{default}:$x);
}
#END: get_boolean


=head3 Yn

Returns "y" or "n", defaulting to "y", depending on the input. Argument may
be a string, filehandle, or empty. If called with no arguments, then a
single line is read from C<E<lt>E<gt>>.

WARNING: This subroutine returns "y" if the input is the empty string or undefined.

=cut

#BEGIN: Yn, 1 line; depends: get_boolean
sub Yn { get_boolean({qw/default y true y false n/}, @_) }

=head3 yN

Returns "y" or "n", defaulting to "n", depending on the input. Argument may
be a string, filehandle, or empty. If called with no arguments, then a
single line is read from C<E<lt>E<gt>>.

This subroutine returns "n" if the input is the empty string or undefined.

=cut

#BEGIN: yN, 1 line; depends: get_boolean
sub yN { get_boolean({qw/default n true y false n/}, @_) }

=head3 yn

Returns "y" or "n" if the input appears to be boolean. Argument may be a
string, filehandle, or empty. If called with no arguments, then a single
line is read from C<E<lt>E<gt>>.

WARNING: The empty string and C<undef> are not considered to be boolean and
will not be canonicalized to "y" or "n".

=cut

#BEGIN: yn, 1 line; depends: get_boolean
sub yn { get_boolean({qw/true y false n/}, @_) }

=head3 Tf

Returns "1" or "0", defaulting to "1", depending on the input. Argument may
be a string, filehandle, or empty. If called with no arguments, then a
single line is read from C<E<lt>E<gt>>.

WARNING: This subroutine returns "1" if the input is the empty string or undefined.

=cut

#BEGIN: Tf, 1 line; depends: get_boolean
sub Tf { get_boolean({qw/default 1 true 1 false 0/}, @_) }

=head3 tF

Returns "1" or "0", defaulting to "0", depending on the input. Argument may
be a string, filehandle, or empty. If called with no arguments, then a
single line is read from C<E<lt>E<gt>>.

This subroutine returns "0" if the input is the empty string or undefined.

=cut

#BEGIN: tF, 1 line; depends: get_boolean
sub tF { get_boolean({qw/default 0 true 1 false 0/}, @_) }

=head3 tf

Returns "1" or "0" if the input appears to be boolean. Argument may be a
string, filehandle, or empty. If called with no arguments, then a single
line is read from C<E<lt>E<gt>>.

WARNING: The empty string and C<undef> are not considered to be boolean and
will not be canonicalized to "1" or "0".

=cut

#BEGIN: tf, 1 line; depends: get_boolean
sub tf { get_boolean({qw/true 1 false 0/}, @_) }


=head3 prompt

See Also: IO::Prompt(?)

 my $x = prompt();
 my $x = prompt( "prompt" );
 my $x = prompt( "prompt", "help string" );
 my $x = prompt( "prompt", { help hash } );

 my $x = prompt( "prompt", %options );
 my $x = prompt( "prompt", help string/hash, %options );

Prompt the user until valid input is received. The default prompt is '? '.
The return value is the user input without the trailing newline.

The provided help may be either a help string which will be printed to
screen when the help command is given (see below) or may be a hash of
C<command =E<gt> "help string"> pairs which will be used if help for a
particular command is requested. The hash value corresponding to the empty
hash key (C<"" =E<gt> "General help">) will be used for the general help
response.

=over 4

=item help

Declare the help message/hash explicitly.

=item default

Specify a default response which will be returned if the user provides no
response. Specifying this option makes the value of "allow_empty" irrelevant.
Value may be set globally by setting C<$_Util::prompt::default>.

=item allowed

An expression like "help_command" which specifies the allowed input values.
A list provides a list of all possible case insensitive inputs. A regular
expression may capture a sub-portion of the input line and the captured
portion will be used as a canonicalized value. Finally a subroutine is
expected to return the canonicalized value of the input. The default is to
allow any DEFINED input value. Value may be set globally by setting
C<$_Util::prompt::allowed>.

=item allow_empty

Boolean value which (if true) allows an empty response value. The default
is false. Value may be set globally by setting C<$_Util::prompt::allow_empty>.

=item help_command

A literal string, list of literals, regular expression pattern, or
subroutine which determines whether the user has asked for help. If a help
hash was provided then patterns should capture the requested command in
C<$1> and subroutines should return the requested command (or undef if the
input is not a request for help). The default C<help_command> is '?'.

Some valid examples:

 help_command => '?'
 help_command => ['?', 'h ', 'help ']
 help_command => qr/\?\s*(\w*)/
 help_command => sub { ($_[0] =~ /\?\s*(\w*)/) ? ($1 || "help_bar") : undef }

Value may be set globally by setting C<$_Util::prompt::help_command>.

=item trim

A shortcut to set both "trim_leading" and "trim_trailing" to the same value.

=item trim_leading

=item trim_trailing

If true, any leading (resp. trailing) whitespace will be removed from the
user's input prior to any processing by this subroutine. The default is
true. Values may be set globally by setting C<$_Util::prompt::trim_leading>
and C<$_Util::prompt::trim_trailing>.

=item input_filehandle

Specify the input filehandle. The default is STDIN. Value may be set
globally by setting C<$_Util::prompt::input_filename>.

=item output_filehandle

Specify the output filehandle. The default is STDOUT. Value may be set
globally by setting C<$_Util::prompt::output_filename>.

=item on_undef

Specify what to do when an undefined value is given as input. The following
values are recognized:

  return     : causes "prompt" subroutine to immediately return undef
  make_empty : replaces the undefined value with the empty string and continues
  continue   : do nothing in particular ("default" and "allowed" will still apply)

Any other value will cause the script to croak with the the value as the
error message. The default value is "make_empty". Value may be set globally
by setting C<$_Util::prompt::on_undef>.

=item no_echo

If true, user's input is not echoed to the screen. Value may be set
globally by setting C<$_Util::prompt::no_echo>.

=back

=cut

#BEGIN: prompt
{ use Carp;
  $_Util::prompt::default            = undef;
  $_Util::prompt::allow_empty        = 0;
  $_Util::prompt::on_undef           = "make_empty";
  $_Util::prompt::input_filehandle   = \*STDIN;
  $_Util::prompt::output_filehandle  = \*STDOUT;
  $_Util::prompt::trim_leading       = 1;
  $_Util::prompt::trim_trailing      = 1;
  $_Util::prompt::allowed            = sub { $_[0] };
  $_Util::prompt::help_command       = sub { ($_[0] =~ /^\?\s*(.*)/) ? $1 : undef };
  $_Util::prompt::no_echo            = 0;

  sub prompt {
    require Term::ReadKey;
    my $p = shift;
    $p = "? " unless defined $p;
    my %opt = (
               default            => $_Util::prompt::default,
               on_undef           => $_Util::prompt::on_undef,
               allow_empty        => $_Util::prompt::allow_empty,
               input_filehandle   => $_Util::prompt::input_filehandle,
               output_filehandle  => $_Util::prompt::output_filehandle,
               trim_leading       => $_Util::prompt::trim_leading,
               trim_trailing      => $_Util::prompt::trim_trailing,
               allowed            => $_Util::prompt::allowed,
               help_command       => $_Util::prompt::help_command,
               no_echo            => $_Util::prompt::no_echo,
               ((@_ % 2) ? (help => @_) : @_)
              );
    $opt{trim_leading} = $opt{trim_trailing} = $opt{trim} if exists $opt{trim};
    my ($in, $out) = @opt{qw/input_filehandle output_filehandle/};

    if (ref ($opt{help_command}) ne 'CODE') {
      my $h = $opt{help_command};
      if (not ref($h))            { $opt{help_command} = sub { ($_[0] =~ /^\Q$h\E\s*(.*)/) ? $1 : undef } }
      elsif (ref($h) eq 'Regexp') { $opt{help_command} = sub { ($_[0] =~ $h) ? $1 : undef } }
      elsif (ref($h) eq 'ARRAY')  { $h = join "|", map quotemeta, @$h;
                                    $opt{help_command} = sub { ($_[0] =~ /^(?:$h)\s*(.*)/i) ? $1 : undef } }
      else { carp "Invalid help command: $h";
             $opt{help_command} = sub { croak "Invalid help command: $h" }
           }
    }

    if (ref ($opt{allowed}) ne 'CODE') {
      my $h = $opt{allowed};
      if (not ref($h))            { $opt{allowed} = sub { ($_[0] eq $h) ? $h : ($_[0]eq'')?'': undef } }
      elsif (ref($h) eq 'Regexp') { $opt{allowed} = sub { ($_[0] =~ $h) ? (defined $1) ? $1 : $_[0] : ($_[0]eq'')?'':undef } }
      elsif (ref($h) eq 'ARRAY')  { $opt{allowed} = sub { my $needle = lc($_[0]); my ($found) = grep $needle eq lc($_), @$h; defined($found) ? $found : ($_[0]eq'')?'': undef } }
      else { carp "Invalid 'allowed' description: $h";
             $opt{allowed} = sub { croak "Invalid 'allowed' description: $h" }
           }
    }

    my ($ans, $need_help);
    while (not defined $ans) {
      print $out $p;
      Term::ReadKey::ReadMode('noecho') if $opt{no_echo};
      if (defined ($ans = <$in>)) {
        if    ($opt{no_echo}) { Term::ReadKey::ReadMode('normal'); print $out "\n" }
        chomp($ans);
        $ans =~ s/^\s+// if $opt{trim_leading};
        $ans =~ s/\s+$// if $opt{trim_trailing};

        # Print help if needed
        if ($opt{help} and defined ($need_help = $opt{help_command}->($ans))) {
          #       print "ans: $ans    need_help: $need_help\n";
          $need_help = "" unless ref($opt{help}) and exists $opt{help}{$need_help} and defined $opt{help}{$need_help};
          print $out ((ref($opt{help})) ? $opt{help}{$need_help} : $opt{help}), "\n";
          $ans = undef;
          next;
        }
      } else {
        if    ($opt{no_echo}) { Term::ReadKey::ReadMode('normal'); print $out "\n" }
        if    ($opt{on_undef} eq 'return')     { return }
        elsif ($opt{on_undef} eq 'make_empty') { $ans = '' }
        elsif ($opt{on_undef} eq 'continue')   { 1 }
        else { croak $opt{on_undef} }
      }

      # apply default if needed
      $ans = $opt{default} if( (not defined $ans or $ans eq '') and defined $opt{default} );

      # Make sure that the final answer is actually allowed.
      $ans = $opt{allowed}->($ans);

      # reject empty input if appropriate
      if( (not defined $ans or $ans eq '') and not $opt{allow_empty} ) { $ans = undef }
    }

    return $ans;
  }
}
#END: prompt



#-----------------------------------------------------------------
                        $EXPORT_TAGS{plot}
                                 =
[qw/plot_colors ps_barchart/];
#-----------------------------------------------------------------

=head2 :plot - Graphs and Plots

=head3 plot_colors

 plot_colors( $n );
 plot_colors( $n, %colors_options);

Return a list of C<$n> colors that are nice for making a plot of. The
colors are chosen to be visually distinct, however if C<$n> is large enough
(more than 13) you will get a rainbow of colors.

Any options supported by L<colors|/colors> can be provided and will be
passed along, including the L<n|/colors> and L<colors|/colors> options, so
you probably don't want to include those options.

=cut

#BEGIN: plot_colors, depends: colors
{ no warnings 'qw';
  # Perhaps instead the "web colors" listed here: http://en.wikipedia.org/wiki/Green
  my $plot_colors = [qw/red blue green magenta goldenrod navy DarkTurquoise
    coral darkslategray Plum orange Violet ForestGreen/];
  my $rainbow = [qw/#df0000 #ff8000 #ffff00 #00ff00 #00e0e0 #0000ff #50007f/];
  sub plot_colors {
    my $n = shift;
    if ($n <= @$plot_colors) {
      return colors(format => "ps", n => $n, colors => $plot_colors, @_);
    } else {
      return colors(format => "ps", n => $n, colors => $rainbow, @_);
    }
  }
}
#END: plot_colors

=head3 ps_barchart !!incomplete

 ps_barchart( \@data );
 ps_barchart( \@data, %options );
 ps_barchart( %data_and_options );

Generate a postscript barchart.

Examples:

 my @x = map { int(rand(20)) } 1..15;
 my @y = map { int(rand(20)) } 1..15;
 my @z = map { int(rand(20)) } 1..15;

 # A simple dynamic web graph:
 print "Content-Type: image/png\n\n", ps_barchart \@x;

 # Neighboring bars:
 ps_barchart file => "graph.png",
             data => [ foo => \@x, bar => \@y, baz => \@z ];

 # Stacked bars: ( [ [foo => \@x], [bar => \@y], ... ]  is also OK. )
 ps_barchart file    => "graph.gif", style => "stacked",
             xlabels => [qw/ay bee cee dee ee ef gee ach eye jay kay ell em en oh/],
             data    => [ foo => \@x, bar => \@y, baz => \@z ];

 # xlabels are dates, bars are already tiered ($x[$i] <= $y[$i] <= $z[$i] for all $i):
 ps_barchart file    => "graph.gif", style => "prestacked",
             xlabels => [qw/2005-01 2005-02 2005-03 2005-04 2005-05 2005-06 2005-07 2005-08/],
             timefmt => "%Y-$m", format => ["x %b %y", "y %g" ],
             data    => [ foo => \@x, bar => \@y, baz => \@z ];


##XXX: Alas, I still have to go through and make it be able to handle a proper histogram

=cut

# gnuplot> set output "/tmp/file.svg"
# gnuplot> plot 'data' using 1:4 w boxes t 'Catherine', 'data' using 1:3 w boxes t 'Rebecca', 'data' using 1:2 w boxes t 'Aaltje'
# set xtics ("foo" 1, "bar" 2, "baz" 3, "zip" 4, "zap" 5)
# plot [0:6] [-.5:5] 'data' using 1:4 w boxes t 'Catherine', 'data' using 1:3 w boxes t 'Rebecca', 'data' using 1:2 with boxes
# set border 3

# set xdata time
# set timefmt "%Y-$m"
# set format x "%b %y"

# timefmt => "%d%m%y%Y..."  # requires 'set xdata time'
# format  => "x %b %y"
# time data demo
#     set boxwidth .5
#     set term svg dynamic

#BEGIN: ps_barchart, depends: max, plot_colors
{ sub ps_barchart {
    return unless @_;
    require File::Temp;
    require File::Spec;
    require File::Copy;

    unshift @_, "data" if 1 == (@_ % 2);
    my %o = @_;
    $o{font_size} ||= 12;                                                 # option: font_size
    $o{grayscale} ||= $o{greyscale} ||= $o{grayscale};                    # option: grayscale
    return unless $o{data} and ref($o{data}) eq 'ARRAY';

    if ($o{_nr_series}    =  grep +(ref($_) eq 'ARRAY'), @{$o{data}}) {
      $o{series_labels} ||= [grep +(ref($_) ne 'ARRAY'), @{$o{data}}];    # option: series_labels
      $o{data}            = [grep +(ref($_) eq 'ARRAY'), @{$o{data}}];    # option: data
      $o{_nr_x}           = max(map 0+@$_, @{$o{data}}) || 0;
      $o{colors} = $o{grayscale} ? plot_colors($o{_nr_series}, colors=>['#bfbfbf', '#404040'])
                                 : plot_colors($o{_nr_series});
    } else {
      $o{colors} = [[.65, .65, .65]];
    }

    if (ref $o{aspect_ratio}) {                                           # option: aspect_ratio
      if (!$o{width} and !$o{height}) {                                   # option: width, height
        $o{width}  = 100;
        $o{height} = $o{aspect_ratio}[1]*$o{width}/$o{aspect_ratio}[0];
      } elsif ($o{width}) {
        $o{height} = $o{aspect_ratio}[1]*$o{width}/$o{aspect_ratio}[0];
      } elsif ($o{height}) {
        $o{width}  = $o{aspect_ratio}[0]*$o{height}/$o{aspect_ratio}[1];
      }
    }


    # _nr_series = 0






    # Massage $o{data} into amenable form: [ list of #'s ] or [ [ tag => [ #'s ] ], ... ]
    return unless ref($o{data}) eq 'ARRAY' and @{$o{data}};
    if (@{$o{data}} > 1 and !ref($o{data}[0]) and ref($o{data}[1])) {
      $o{data} = [ map [ @{$o{data}}[2*$_, 2*$_+1] ], 0..$#{$o{data}}/2 ];
    }

    # Native: jpeg, png, svg, postscript
    my $dir   = File::Temp::tempdir(CLEANUP => 1);
    my $ofile = File::Spec->catfile($dir, 'graph.svg');
    my $dfile = File::Spec->catfile($dir, 'data');
    my $plot_commands = <<"    PLOT_COMMANDS";
    set style fill solid
    set zeroaxis
    set terminal postscript eps enhanced color solid
    set output "$ofile"
    PLOT_COMMANDS
  }
}
#END: ps_barchart


#-----------------------------------------------------------------
                        $EXPORT_TAGS{image}
                                 =
[qw/compile_latex tex2image magic_convert/];
#-----------------------------------------------------------------

=head2 :image - Image Routines

=head3 compile_latex

Compiles a LaTeX file. The following options are accepted.

=over 4

=item latex

An integer specifying the number of times latex is to be run. Reasonable
values are 1 (the default) or 2 (if your document has references which need
to be resolved).

=item compiler

Arrayref containing compile command to use. Auto-chosen from latex,
pdflatex, or perltex (each running in batch mode; perltex can handle either
latex or pdflatex documents) by looking for C<pdftex> option on
C<\documentclass> command line (may be in comment at end of line) or
(uncommented) C<perltex> C<\usepackage> command.

=item pdftex

Set to true if latex compiler produces pdf documents rather than dvi
documents.

=item print

1 or printer name. Will be printed using dvips.

=item dvips

1/0 creates a PostScript file.

=item dvipdf

1/0 creates a PDF file.

=item bibtex

1/0 runs BibTeX at the right time.

=item index

1/0 runs makeindex at the right time.

=back

Comments: Proposed future interface:

 compile_doc $file | $dir | [ paths ],
   output => [qw/ pdf ps /],

   compile => [qw/ latex1 bibtex makeindex ... latex /],

   # set to reasonable defaults for all known thinguns
   compile_bibtex_command => [ command prefix ],# only reasonable for $file or $dir calls
   compile_bibtex_command => sub { passed file or dir or paths },
   # called with output of prev command in $_; returns true if need to call bibtex
   # called with arguments: ( command_chain => [qw/ latex1 bibtex /], command_output => { latex1 => ..., bibtex => ... } )
   #                          ^- commands called so far (in order)    ^- output from commands (most recent call only)
   compile_bibtex_test => sub{ grep /\.bib$/, glob('*') },
   compile_makeindex_test=>sub{my%o=@_;my $res=$o{command_output}{latex1}||$o{command_output}{latex};$res=~/run makeindex/},
   # If true, test will be performed up to Int times and bibtex will be
   # called up to Int times
   compile_bibtex_multi => Int,

   convertto_pdf => [ ... list of preferred sources ... ],

   convert_dvi_pdf => [ ... command prefix, filename.dvi is appended ... ],
   convert_pdf_ps => sub { called in chdir, given filename.pdf as arg, must produce filename.ps },
   ...

=cut

#BEGIN: compile_latex, depends: SYSTEM
sub compile_latex {
  require Path::Class;
  require Cwd;
  my $file = Path::Class::file(shift);
  my $f = $file->basename;
  $f =~ s/\.tex$//;
  my %opt = (qw/latex 1/,
             @_);
  my $curr_dir = Cwd::cwd();
  Cwd::chdir( $file->dir ) or croak "Could not change to TeX directory";

  unless ($opt{compiler}) {
    local $_;
    my $fh = $file->openr;
    while (defined($_ = <$fh>)) {
      last if /^\s*\\begin\{document\}/;
      if (/^\s*\\documentclass\[.*?pdf(?:la)?tex/) {
        $opt{compiler} = [qw/pdflatex -interaction batchmode/];
        $opt{pdftex} = 1;
      }
      elsif (/^\s*\\documentclass\[.*?dvips/) {
        $opt{compiler} = [qw/latex -interaction batchmode/];
      }
      elsif (/^\s*\\documentclass[\[\{]/) {
        $opt{compiler} = [qw/xelatex -interaction batchmode/];
      }
      elsif (/^\s*\\usepackage[\[\{].*?xltxtra/) {
        $opt{compiler} = [qw/xelatex -interaction batchmode/];
      }
      elsif (/^\s*\\usepackage[\[\{].*?perltex/) {
        my @extra;
        push @extra, '--latex='.$opt{compiler}[0] if $opt{compiler};
        $opt{compiler} = [qw/perltex --nosafe/, @extra];
      }
    }
    $opt{compiler} ||= [qw/tex -interaction batchmode/];
    close $fh;
  }

  my @null = (\">/dev/null 2>/dev/null");

  # latex
  SYSTEM(@{$opt{compiler}}, $file->basename, @null)   for 1..$opt{latex};
  if ($opt{bibtex}) { # bibtex
    SYSTEM("bibtex", $f, @null);
    SYSTEM(@{$opt{compiler}}, $file->basename, @null) for 1..$opt{latex};
  }
  if ($opt{index}) {  # makeindex
    SYSTEM("makeindex", $f, @null);
    SYSTEM(@{$opt{compiler}}, $file->basename, @null) for 1..$opt{latex};
  }

  # output and printing
  if ($opt{pdftex}) {
    SYSTEM("pdf2ps", "$f.pdf", "$f.ps", @null)      if  $opt{dvips};
    SYSTEM("lpr", "$f.pdf", @null)                  if  $opt{print} and $opt{print} eq "1";
    SYSTEM("lpr", "-P$opt{print}", "$f.pdf", @null) if  $opt{print} and $opt{print} ne "1";
  } else {
    SYSTEM("dvips", $f, "-o", "$f.ps", @null)       if  $opt{dvips};
    SYSTEM("dvipdf", $f, @null)                     if  $opt{dvipdf};
    SYSTEM("dvips", $f, "-f", \"2>/dev/null | lpr") if  $opt{print} and $opt{print} eq "1";
    SYSTEM("dvips", $f, "-P$opt{print}", @null)     if  $opt{print} and $opt{print} ne "1";
  }

  Cwd::chdir($curr_dir);
}
#END: compile_latex


=head3 tex2image

Given a string of LaTeX code, returns an image file as a "string". The
following options may be provided after the LaTeX string. Also, all options
available to L<compile_latex|/compile_latex> are accepted in this function.

=over 4

=item file

Save output to the indicated file instead of returning the image as a
string.

=item type

Specify the save file type. This should be a standard "file extension" for
the desired output (E.g. "gif" or "png"). The default output is an EPS
file. (The ImageMagick command "convert" must be available on your system
for this option to succeed.)

=item header

A header string placed between C<\documentclass{article}> and
C<\begin{document}>. Only useful if input tex code does not include
C<\begin{document}> or C<\documentclass{article}>.

Note: C<\usepackage{color}> and C<\pagestyle{empty}> are always included if
either C<\begin{document}> or C<\documentclass{article}> are missing in the
provided LaTeX string.

=item convert_args

Additional arguments to pass to convert when making the image. By default
this is C<["-transparent", "white"]>.

=item X

Specify the X resolution (default 144)

=item Y

Specify the Y resolution (default 144)

=item color

=item pagecolor

Specify the color or page color. Each may be an RGB hex triplet ("#40036f",
the "#" is required!) LaTeX named color (red E<verbar> green E<verbar> blue
E<verbar> yellow E<verbar> cyan E<verbar> magenta E<verbar> black E<verbar>
white and perhaps others depending on the DVI driver), a single number
representing gray value, an "r,g,b" triplet, or a "c,m,y,k" quadruple. All
numbers are percentages between 0 and 1, inclusive. The default values are
"black" and "white" respectively.

=back

=cut

#BEGIN tex2image, depends: compile_latex, QX
sub tex2image {
  require File::Temp;
  my $tex = shift;
  my %opt = (X => 144, Y => 144, header => '', convert_args => [qw/-transparent white/],
             color => "black", pagecolor => "white",
             @_
            );
  unless (defined $opt{type}) {
    if (defined $opt{file} and $opt{file} =~ /\.(\w+)$/) { $opt{type} = $1 }
    else                                                 { $opt{type} = "eps" }
  }

  for (@opt{qw/color pagecolor/}) {
    /#([0-9a-fA-F]{6})/ and $_ = "[rgb]{".join(',',map hex($_)/255 ,grep/\S/,split/(..)/,$1)."}" and next;
    /[a-zA-Z]/  and $_ = "{$_}"       and next;
    tr/,// == 0 and $_ = "[gray]{$_}" and next;
    tr/,// == 2 and $_ = "[rgb]{$_}"  and next;
    tr/,// == 3 and $_ = "[cymk]{$_}" and next;
  }

  $opt{convert_args} = [] unless defined $opt{convert_args} and $opt{convert_args} ne '';
  $opt{convert_args} = [split(/ /,$opt{convert_args})] unless ref($opt{convert_args}) eq 'ARRAY';

  return unless $tex;
  $tex = <<"    TEX" unless $tex =~ /begin\{document\}/s;
    \\documentclass{article}
    $opt{header}
    \\usepackage{color}
    \\pagestyle{empty}
    \\begin{document}
      \\color$opt{color} \\pagecolor$opt{pagecolor}
      $tex
    \\end{document}
    TEX
  $tex = <<"    TEX" unless $tex =~ /documentclass/s;
    \\documentclass{article}
    $opt{header}
    \\pagestyle{empty}
    \\usepackage{color}
    \\color$opt{color} \\pagecolor$opt{pagecolor}
      $tex
    TEX

  my $dir = File::Temp::tempdir( CLEANUP => 1 );
  my ($tex_fh, $file) = File::Temp::tempfile( DIR => $dir, SUFFIX => '.tex' );
  $tex_fh or croak "Error creating temporary files: $!";
  print $tex_fh $tex;
  close $tex_fh;
  compile_latex($file, %opt, compiler => [qw/latex -interaction batchmode/]);

  $file =~ s/\.tex$//;
  return unless -e "$file.dvi";
  QX("cd", $dir, \";", qw/dvips -E -X/, $opt{X}, '-Y', $opt{Y}, '-o', "$file.eps", "$file.dvi", \"2>/dev/null");

  if ($opt{type} ne "eps") {
    QX("cd", $dir, \";", "convert", @{$opt{convert_args}}, '-density', "$opt{X}x$opt{Y}", "$file.eps", "$file.$opt{type}");
  }

  return unless -e "$file.$opt{type}";
  if (defined $opt{file}) {
    require File::Copy;
    return File::Copy::move( "$file.$opt{type}", $opt{file} );
  } else {
    my $f;
    open $f, "<", "$file.$opt{type}" or croak "Error opening $file.$opt{type} for reading: $!";
    binmode $f;
    local $/ = undef;
    my $x = <$f>;
    close $f;
    return $x;
  }
}
#END tex2image


=head3 magic_convert !UNIMPLEMENTED

 magic_convert $file, %options
 magic_convert $old_file, $new_file, %options
 magic_convert \@files, %options
 magic_convert \@files, $dir, %options

Convert file types and resize images. Valid options are given below.
Colors may be given as (X11) color names or RGB hex triplets.

=over 4

=item format =E<gt> $ext

Specify an output file format. This "option" is required for all invocation
styles except for the second, where the output format will be guessed from
the C<$new_file> name if this option is not provided.

=item transparent =E<gt> $color

If the target image type supports transparency, then the specified color
will be made transparent during the conversion.

=item grow =E<gt> $grow

A boolean value which, if true, indicates that the image should be enlarged
in order to fit maximally into the specified resolution / size.

=item size =E<gt> $WIDTHxHEIGHT or \@width_and_height

Specified either as a list of two elements or a string of the form
"640x480", this option forces the image to fit within a box of the given
size.

=item resolution =E<gt> $value

For vector-based inputs the resolution will affect the resulting image
size. Note that the "max_size" option will override this option under most
circumstances.

=item intent =E<gt> "icon" | "thumbnail" | "web" | "email" | "screen" | "print" | "hires"

A fuzzy way to set the "resolution" and "size" options to reasonable (by
current technology standards) sizes. The "icon" intent will aim for an
image of size 128x128. The "thumbnail" intent will limit the image to a
250x250 box. The "web" and "email" intents assume 640x480 screens, while
the "screen" intent assumes a 1024x768 screen. The "print" intent assumes a
5"x5" image at 300 DPI and the "hires" intent assumes 5"x5" at 600 DPI.

=back

=cut

#BEGIN: magic_convert, depends: $_re_image_ext, QX, TO_FINISH
sub magic_convert {
  TO_FINISH();
  my %o = splice @_, ((@_%2)?1:2);
  my ($old, $new, $old_format) = @_;
  $o{format}  = $1 if not defined $o{format} and $new =~ /\.($_re_image_ext)$/;
  $old_format = lc $1 if defined $old and $old =~ /\.($_re_image_ext|fig)$/;
  return unless defined $o{format} and defined $old_format;

  if ($old_format eq 'fig') {
    require File::Temp;
    require File::Spec;
    my $dir = File::Temp::tempdir(CLEANUP => 1) or croak "Can't make temporary directory";
    system "fig2dev", "-L", "eps", $old, File::Spec->catfile( $dir, "temp.eps" ) == 0 or die "Failed to do conversion";
    unless (defined $new) {
      my (undef, $d, $f) = File::Spec->splitpath( $old );
      $f =~ s/\.fig$/\.eps/;
      $new = File::Spec->catpath( '', $d, $f );
    }
    $old = File::Spec->catfile( $dir, "temp.eps" );
    $old_format = 'eps';
  }

  my $info = QX("identify", "-ping", $old);
  $o{old_res} = [$1,$2] if $info =~ / (\d+)x(\d+) /;
  die "Can't parse resolution of old file" unless $o{old_res}


}
#END: magic_convert


#-----------------------------------------------------------------
                        $EXPORT_TAGS{LaTeX}
                                 =
[qw/quotetex tree2tex/];
#-----------------------------------------------------------------

=head2 :LaTeX - LaTeX generating routines

=head3 quotetex

Like quotemeta, but makes strings LaTeX safe. Replaces all LaTeX special
characters with replacements which will correctly compile in LaTeX.

=cut

#BEGIN: quotetex
{ no warnings 'qw';
  our %tex_special =
    (# First some easy ones:
     qw/# \#   $ \$   & \&   % \%   { \{   } \}/,
     # now the harder ones:
     '~'  => '\mbox{\lower.5ex\hbox{\ensuremath{\widetilde{\phantom{n}}\,}}}',
     '_'  => '\mbox{\underline{\phantom{n}}}\ensuremath{\,}',
     '^'  => '\ensuremath{\widehat{\phantom{n}}\,}',
     '\\' => '\ensuremath{\backslash}',
     '<'  => '\ensuremath{<}',
     '>'  => '\ensuremath{>}',
    );
  our %tex_extra =
    (# For "preformatted" text
     ' '  => "~",
     "\r" => "",
     "\n" => "\\\\\n\\mbox{}",
     "\t" => "~~~~~~~~",
    );
  sub quotetex {
    local $_ = @_ ? shift : $_;
    s/(?:^|(?<=\s))"/``/mg; s/"/''/g;
    @_ && (local %tex_special = (%tex_special, ((@_%2 and shift)? %tex_extra : ()), @_));
    join '', map( (exists $tex_special{$_})?$tex_special{$_}:$_, split // );
  }
}
#END: quotetex



=head3 tree2tex

 tree2tex \%tree, %options

Convert arbitrarily nested HoH's to the LaTeX code which will produce a
tree diagram,

 # This,
 { A => { b => 1, c => 1 }, B => { f => { e => 1, f => 1 }, g => 1 } }

 # Becomes code that produces this,

  A -+- b
     |
     +- c

  B -+- f -+- e
     |     |
     |     +- f
     +- g

The leaf nodes may point to any value which is not a reference. You will
need to C<\usepackage{pstricks,pst-node}> for the code produced by this
subroutine to function properly. Accepted options,

=over 4

=item column_spacing

A LaTeX measurement for the amount of spacing to use between each column.
This amount is placed before and after each column (using the LaTeX
\tabcolsep variable) so should be half of the actual desired column
spacing. The default is "1.5em".

=item row_stretch

A multiplier for the row stretch. Used to set the LaTeX multiplier
C<\arraystretch>. The default is 1.

=item tabular_format

The tree is built using the tabular environment. This option sets the
format for the tabular. If the format is a single character then it will be
duplicated for each level of your tree. Otherwise, you will need to make
sure that you include enough columns for your diagram (one column for each
level of the tree). The default is "l".

=item node_label_start

The starting node label. Useful if you are using alphabetic node labels
elsewhere in your document. The default node labeling is "A".."Z","AA",...
using the perl magic incrementer. Meaningful values for C<node_label_start>
are all-caps words, all-lower-case words, or numbers.

=item sort

Boolean value dictating whether we should sort the key values. The default
is to sort tree nodes. Set this to false if you have a tied hash which will
return keys in your desired order. Some modules which may help with this,

 Tie::Hash::Sorted                    - specify your own sort function
 Tie::IxHash or Tie::Hash::Indexed    - key order is insert order

=item use_leaf_values

If the leaves of your tree point to useful string values then you may
specify C<use_leaf_values =E<gt> 1> to have this subroutine use the leaf
values as labels for the leaves rather than the leaf keys.

=item vertical

Boolean value which, if true, tells the subroutine to transpose the
resulting tree. This has the effect of putting the root nodes across the
top rather than down the left side.

Note: This still needs some work. In particular, when the matrix is
transposed, the labels are not centered above their children.

=item nc

Node connection type. May be any LaTeX node connection type. Currently must
be one of: line, Line, curve, arc, bar, diag, diagg, angle, angles, loop,
circle. The default is "angles".

=item node_sep

A LaTeX measurement for the amount of spacing to place around each node.
The default is "1ex".

=back

=cut

#BEGIN tree2tex, depends: fmax
sub tree2tex {
  my $tree = shift;
  my %o = (qw/column_spacing 1.5em row_stretch 1 tabular_format l
              node_label_start A sort 1 nc angles node_sep 1ex/, @_);
  my $label = $o{node_label_start};
  my (@M,@nc,$tex);

  # The main recursion subroutine. args: \%tree, $parentID, \@indent1, \@indent2
  my $tree2matrix;
  $tree2matrix = sub {
    my ($tree,$parentID) = splice @_, 0, 2; my $i = 0; my @M;
    return $_[0] unless ref $tree;
    for ($o{sort} ? sort keys %$tree : keys %$tree) { my $_label = $label++;
      push @nc, "\\nc".$o{nc}."{$parentID}{$_label}" if defined $parentID;
      push @M, &$tree2matrix($tree->{$_},
                             $_label,
                             [@{$_[$i]}, "\\rnode{$_label}{".
                                          ((!ref($tree->{$_}) and $o{use_leaf_values}) ? $tree->{$_} : $_).
                                          "}"],
                             [@{$_[1]}, '']
                            );
      $i = 1;
    } return @M;
  };

  @M   = &$tree2matrix($tree, undef, [], []);
  my $cols = fmax(sub{0+@$_},@M);
  if ($o{transpose}) {
    my $orientation = $cols > $#M;
    for my $i (0..$#M) {
      for my $j ($orientation ? $i+1..$cols-1 : 0..$i-1) {
        ($M[$i][$j], $M[$j][$i]) = ($M[$j][$i], $M[$i][$j]);
      }
    }
    $cols = @{$M[0]};
  }

  no warnings 'uninitialized';
  $tex = "{ "
        ."  \\psset{nodesep=$o{node_sep}}\n"
        ."  \\setlength{\\tabcolsep}{$o{column_spacing}}\n"
        ."  \\renewcommand{\\arraystretch}{$o{row_stretch}}\n"
        ."  \\begin{tabular}{"
        .((length($o{tabular_format}) == 1) ? ($o{tabular_format}x$cols) : $o{tabular_format})
        ."}\n";
  local $" = '&';
  $tex .= join "\\\\\n", map "    @$_", @M;
  local $" = ' ';
  $tex .= "  \\end{tabular} \\psset{angleB=".($o{transpose}?90:180)."}\n@nc\n}\n";

  return $tex
}
#END tree2tex


#-----------------------------------------------------------------
                        $EXPORT_TAGS{html}
                                 =
[qw/ libxml_doc js_toggle xml_attr xml_tag xml_btag xml_encode uri /];
#-----------------------------------------------------------------

=head2 :html - HTML utilities

=head3 uri

 my $uri = uri( $base, @path_components, \%query_params );

All arguments optional. The first argument (C<$base>) is treated specially
only in that a leading "/" will be respected. Any path components may
include query parameters and/or an anchor. Query parameters will be merged
(order is preserved). The last anchor defined takes precedence. Any hash
references will be URI escaped and appended as query parameters. You may
include as many hash refs as you like and they may appear anywhere in the
argument list.

=cut

#BEGIN: uri
sub uri {
  require URI::Escape;
  my (@q, @p, $anchor);

  for (@_) {
    # Hash refs only
    if ('HASH' eq ref()) {
      for my $k (keys %$_) {
        push @q, join "=", map URI::Escape::uri_escape($_), $k, $$_{$k};
      }
    }

    else {
      local $_ = $_;# make a copy before modifying
      $anchor = $1 if s/(#.*)//;
      push @q, $1  if s/\?(.*)//;

      s#^/+## if @p;
      s#/+$##;

      push @p, $_;
    }
  }

  my $url = join "/", @p;
  $url .= "?" . join("&", @q) if @q;
  $url .= $anchor             if $anchor;
  return $url;
}
#END: uri


=head3 xml_attr

 xml_attr( %attr )
 xml_attr( @kv )

Creates XML attribute string. Encodes keys and values. Attributes will be
included as long as their values are defined, even if empty. Attribute
names must be both defined and non-empty.

If any attribute names are passed as SCALAR references, then their values
will be interpreted as boolean values controlling whether the valueless
attribute name should be included.

 xml_attr( class => "foo", \"bar" => 1 );             #  'class="foo" bar'
 xml_attr( \'class="foo"' => 1, \'bar="baz"' => 0 );  #  'class="foo"'

=cut

#BEGIN: xml_attr, depends: xml_encode, map_pairs
sub xml_attr {
    join " ", map_pairs(
        sub { (defined($a) and length($a) and defined($b)) ?
              ('SCALAR' eq ref($a) ? ($b ? $$a : ()) : xml_encode($a).'="'.xml_encode($b).'"')
              : ()
        }, @_
    );
}
#END: xml_attr

=head3 xml_tag

 xml_tag( $name )
 xml_tag( $name => %attr )
 xml_tag( $name => $content )
 xml_tag( $name => \$content )
 xml_tag( $name => $content, %attr )
 xml_tag( $name => \$content, %attr )

Creates XML tag with content and attributes. Name is assumed to not contain
XML special characters (E<gt>E<lt>&"), but all other values will be xml
encoded except perhaps the content, which will pass through unchanged if a
scalar reference is passed.

=cut

#BEGIN: xml_tag, depends: xml_attr, xml_encode
sub xml_tag {
  my $name = shift;
  my $content = (@_ % 2) ? shift : undef;
  "<$name"
    . (@_ ? " ".xml_attr(@_) : "")
    . (defined($content) ? ">".('SCALAR' eq ref($content) ? $$content : xml_encode($content))."</$name>" : "/>");
}
#END: xml_tag

=head3 xml_btag

 xml_btag( $name )
 xml_btag( $name => %attr )

Creates beginning XML tag with attributes. Name is assumed to not contain
XML special characters (E<gt>E<lt>&"), but all other values will be xml
encoded.

=cut

#BEGIN: xml_btag, depends: xml_attr
sub xml_btag {
  my $name = shift;
  "<$name" . (@_ ? " ".xml_attr(@_) : "") . ">";
}
#END: xml_btag

=head3 xml_encode

Minimal encoding of XML entities. Behaves like C<encode_entities($input, 'E<lt>E<gt>&"')>.

=cut

#BEGIN: xml_encode
sub xml_encode {
  local $_ = $_[0];
  return "" unless defined;
  s/&/&amp;/g;
  s/>/&gt;/g;
  s/</&lt;/g;
  s/"/&quot;/g;
  $_;
}
#END: xml_encode

=head3 js_toggle

 js_toggle [ label1 => id1, label2 => id2, ... ], %options;
 js_toggle [ label1 => [id1a, id1b, ...], label2 => id2, ... ], %options;
 js_toggle [ [displayed_label1, hidden_label1], [id1a, id1b, ...], ... ], %options;

Constructs a list of html snippets that can be placed in a document that
will switch on and off the indicated ids. Ids may be associated to multiple labels,

The options supported are given below.

=over 4

=item type =E<gt> "radio" | "toggle"

Toggle buttons simply show and hide the corresponding IDs. Radio buttons
always show the associated ids and hide all other ids. The default behavior
is "toggle".

=item id_prefix =E<gt> $prefix

Each label will be wrapped in a C<< <span id="ID"> >> tag. The "ID" must be
unique for each page. To ensure this, the function appends an integer to
the "id_prefix" which is incremented as necessary (the incremented value is
remembered between calls to js_toggle). The default prefix is "JST", but
can be changed using this option.

=item reset_counter =E<gt> $bool

If true, the counter used to ensure that ids are unique will be reset to
zero at the end of the function call. This can be helpful if you want to
include style information for the generated labels in a style sheet (though
you could also wrap your label in a C<< <span> >> tag before passing it to
this function). The default is to not reset the counter.

=item visibility =E<gt> $visibility

Indicates the initial visibility state of the items (only relevant if
"hidden" labels are provided). C<$visibility> may be a label or list of
labels listing those labels which will be displayed when the page loads
(you will need to manage the page styles to ensure this). Alternatively,
C<$visibility> may be "1", "0", or a list of "1"'s and "0"'s indicating
which items (by position) will be visible. Specifying just "1" or "0"
indicates that all or none of the objects will be initially visible. The
default is to assume that the first item is visible and that all others are
hidden.

#XXX: Ugh, this is confusingly worded! (and wrong!)

=item display =E<gt> \@displaystyles

A list of display styles to be used for making objects visible. This will
typically be "block" or "inline", but CSS 2 allows lots of things
(list-item, table, ...). Any display styles left undefined will default to
"block". The size of the displaystyles list should correspond to the length
of the concatenated ids without removing duplicates. For example:

 js_toggle [ foo => ["A", "B"], bar => ["B", "C"], baz => "D" ],
   display => [qw/  block inline       block block       table/];

Thus, display styles may depend on the label the object is currently
associated with.

=item use_functions =E<gt> $bool

*** NOT IMPLEMENTED ***

The variable C<$_Util::js_toggle_functions> includes a function
"js_toggle_display" which can be used by this subroutine to decrease the
amount of inline javascript. This can reduce bandwidth by quite a bit if
this code is places in an external file, or by a little bit if placed in
the page C<< <head> >> in a C<< <script> >> block. If this option is set to
a true value, then it will be assumed that the function "js_toggle_display"
is available and it will be used. You will need to ensure that the code in
C<$_Util::js_toggle_functions> is inserted into the web page in the
appropriate fashion.

=back

=cut

#BEGIN: js_toggle
{ my $count = 0; no warnings 'once';
  $_Util::js_toggle_functions = '';
  sub js_toggle {
    my $A = shift;
    my %o = @_;
    $o{type}        = 'toggle' unless $o{type} and $o{type} eq 'radio';
    $o{id_prefix} ||= 'JST'; # HTML ids must start with [A-Za-z], thus ||= is as good as //=
    $o{display}     = [] unless 'ARRAY' eq ref $o{display};

    my ($i, @A, %ids) = (0); # Ah the confusion! However, $A is only used in the next block
    # For everything, /^S/ associated to User-"show"; /^H/ associated to User-"hide"
    for (map 2*$_, 0..(@$A/2-1)) {
      push @A, { Slab => (ref($$A[$_]) ? ($$A[$_][0], Hlab => $$A[$_][1]) : ($$A[$_])),
                 ids  => (ref($$A[$_+1]) ? [@{$$A[$_+1]}] : [$$A[$_+1]]),
               };
      $ids{$_}++ for @{$A[-1]{ids}};

      $A[-1]{visible} = 1 if $o{visibility} and ($o{visibility} eq '1' or $o{visibility} eq $A[-1]{Slab} or
                                                 (ref($o{visibility}) and ((exists($o{visibility}[$_/2]) and $o{visibility}[$_/2] eq '1') or
                                                                           grep($A[-1]{Slab} eq $_, @{$o{visibility}}))));

      $A[-1]{display} = [map +(defined($o{display}[$i++]) ? $o{display}[$i-1] : 'block'), @{$A[-1]{ids}}];
      $A[-1]{Hlab}    = $A[-1]{Slab} unless $o{type} eq 'radio' or defined $A[-1]{Hlab};

      $A[-1]{Slabid}  = $o{id_prefix}.($count++);
      $A[-1]{Hlabid}  = $o{id_prefix}.($count++) if defined $A[-1]{Hlab};
    }

    $A[0]{visible} = 1 unless defined $o{visibility};
    $count = 0 if $o{reset_counter};

    my ($l, @labels);
    my $frm = $o{use_functions} ? q|['%s','%s'],| : q|document.getElementById('%s').style.display='%s';|;
    for my $t (@A) {
      # FIRST THE "SHOW" BUTTON
      #-------------------------
      $l  = sprintf q|<span id="%s" class="js_toggle"|, $$t{Slabid};
      $l .= ' style="display: none;"' if defined($$t{Hlab}) and $$t{visible};
      $l .= ' onclick="';
      $l .= 'js_toggle_display(' if $o{use_functions};
      # Things to hide
      $l .= sprintf $frm, $_, 'none' for (($o{type} eq "radio") ? (keys %ids) : ());
      $l .= sprintf $frm, $_, 'none' for (defined($$t{Hlab}) ? $$t{Slabid} : ()); # YES! H AND S
      if ($o{type} eq "radio") {
        $l .= sprintf $frm, $_, 'none' for map +(defined($$_{Hlab}) ? $$_{Hlabid} : ()), @A;
      }

      # Things to display
      $l .= sprintf $frm, $$t{ids}[$_], $$t{display}[$_] for 0..$#{$$t{ids}};
      $l .= sprintf $frm, $_, 'inline' for (defined($$t{Hlab}) ? $$t{Hlabid} : ());
      if ($o{type} eq "radio") {
        $l .= sprintf $frm, $_, 'inline' for map +(defined($$_{Hlab}) ? $$_{Slabid} : ()), @A; # YES! H AND S
      }

      $l .= ')' if $o{use_functions};
      $l .= sprintf q|">%s</span>|, $$t{Slab};

      # NOW THE "HIDE" BUTTON
      #-----------------------
      unless (defined $$t{Hlab}) { push @labels, $l; next }
      $l .= sprintf q|<span id="%s" class="js_toggle"|, $$t{Hlabid};
      $l .= ' style="display: none;"' if !$$t{visible};
      if ($o{type} eq 'radio')   { push @labels, qq|$l onclick="">$$t{Hlab}</span>|; next }

      $l .= ' onclick="';
      $l .= 'js_toggle_display(' if $o{use_functions};
      # Things to hide - assuming toggle button
      $l .= sprintf $frm, $_, 'none' for @{$$t{ids}}, $$t{Hlabid};

      # Things to display
      $l .= sprintf $frm, $_, 'inline' for $$t{Slabid};

      $l .= ')' if $o{use_functions};
      $l .= sprintf q|">%s</span>|, $$t{Hlab};

      push @labels, $l;
    }

    return @labels;
  }
}
#END: js_toggle


#     my @labs = map +(ref() ? [@$_] : [$_]), even_positions($_[0]);
#     my @ids  = map +(ref() ? [@$_] : [$_]), odd_positions(shift);
#     $o{visibility} = (ref $labs[0]) ? [$labs[0][0]] : [$labs[0]] unless defined $o{visibility};
#     $o{visibility} = [] if $o{visibility} eq "0";
#     $o{visibility} = [map +(ref()?$$_[0]:$_), @labs] if $o{visibility} eq "1";
#     $o{visibility} = [$o{visibility}] unless 'ARRAY' eq ref $o{visibility};
#     $o{visibility} = [map +(ref()?$$_[0]:$_), @labs[grep +($o{visibility}[$_] and $o{visibility}), 0..$#{$o{visibility}}]] if $o{visibility}[0] =~ /^0|1$/;
#     my @all_ids = unique flatten(\@ids);
#     my $i = 0;
#     for (0..$#labs) {
#       push @{$labs[$_]}, @{$labs[$_]} if $o{type} ne 'radio' and 1 == @{$labs[$_]};
#       push @{$labs[$_]}, map $o{id_prefix}.($count++), @{$labs[$_]};
#       push @{$labs[$_]}, $i;
#       $i += @{$ids[$_]};
#     }
#
#
#     my (@t, @Sshow, @Shide, @Hshow, @Hhide); # @S*: * when click "show"; @H*: * when click "hide"
#     for (0..$#labs) {
#       # Ok, all kinds of uglyness here to save the web page from some unnecessary work. (We like "The Snappy")
#       if ($o{type} eq "radio") {
#         @Sshow = (@{$ids[$_]},                                                          # The IDs we are supposed to show
#                   ((5 == @{$labs[$_]})? $labs[$_][3] : ()),                             # Our corresponding "hide" button
#                   map +((5 == @{$labs[$_]})? $labs[$_][2] : ()), 0..$_-1, $_+1..$#labs, # All other "show" buttons (since we just turned them off)
#                  );
#         @Shide = (@all_ids,                                                             # Clean slate!
#                   ((5 == @{$labs[$_]})? $labs[$_][2] : ()),                             # Our corresponding "show" button
#                   map +((5 == @{$labs[$_]})? $labs[$_][3] : ()), 0..$_-1, $_+1..$#labs, # All other "hide" buttons
#                  );
#
#         @Hshow = @Sshow; # radio buttons can't "hide"
#         @Hhide = @Shide;
#       } else { # We are toggle buttons
#         @Sshow = (@{$ids[$_]},                                                          # The IDs we are supposed to show
#                   ((5 == @{$labs[$_]})? $labs[$_][3] : ()),                             # Our corresponding "hide" button
#                  );
#         @Shide = (((5 == @{$labs[$_]})? $labs[$_][2] : ()));                            # Our corresponding "show" button
#
#         @Hshow = (((5 == @{$labs[$_]})? $labs[$_][2] : ()));                            # Our corresponding "show" button
#         @Hhide = (@{$ids[$_]},                                                          # The IDs we are supposed to hide
#                   ((5 == @{$labs[$_]})? $labs[$_][3] : ()));                            # Our corresponding "hide" button
#       }
#
#       push @t, sprintf q|<span id="%s" %s onclick="%s">%s</span>|,
#         $labs[$_][(3 == @{$labs[$_]})?1:2],
#         ((3 == @{$labs[$_]}) ? '' : grep($labs[$_]eq$_, @{$o{visibility}}) ? ' style="display: none;"' :''),
#
#         document.getElementById('$i.hide').style.display='inline';
#
#         $labs[$_][0];
#
#       if (5 == @{$labs[$_]}) { # now the (optional) "hide" button
#
#       }




=head3 libxml_doc

 libxml_doc( $thing )
 libxml_doc( $thing, $parser )
 libxml_doc( $thing, type => '<TYPE>' )
 libxml_doc( $thing, $parser, type => '<TYPE>' )

Construct a XML::LibXML HTML document object easily and quietly. C<$thing>
can be a filename (or something that stringifies to a filename), a URL, or
actual HTML. Alternatively, you can be specific and specify one of the
three types: 'FILE', 'URL', or 'HTML'. The real benefit of this subroutine
though is that all XML::LibXML error messages are discarded.

XML::LibXML and LWP::Simple will be automatically loaded if necessary.

=cut

#BEGIN: libxml_doc, depends: cat
{ my $_parser;
  sub libxml_doc {
    require XML::LibXML;
    my $thing = shift or return;
    return $thing if ref($thing) =~ /XML/;
    my $parser;

    if (@_ % 2) {
      $parser = shift;
    } elsif (!$_parser) {
      $_parser = XML::LibXML->new();
      $_parser->recover(1);
    }
    $parser = $_parser unless $parser;

    my %opt = @_; $opt{type} = $opt{type} ? uc $opt{type} : '';
    my $page;
    if ($opt{type} eq 'FILE' or (!$opt{type} and -f "$thing")) {
      $page = cat("$thing");
    } elsif ($opt{type} eq 'URL' or (!$opt{type} and $thing =~ /^http/)) {
      require LWP::Simple;
      $page = LWP::Simple::get($thing);
    } else { $page = $thing }

    # libxml writes to stderr directly and complains a lot about malformed
    # html. I can't deal with that so, we have to get violent.
    no warnings 'once';
    open(my $OLDERR, ">&STDERR")   || die "Can't dup stdout";
    open(STDERR, ">", "/dev/null") || die "Can't dup stdout";
    my $doc = $parser->parse_html_string($page);
    close(STDERR);
    open(STDERR, ">&", $OLDERR);

    return $doc;
  }
}
#END: libxml_doc



#-----------------------------------------------------------------
                        $EXPORT_TAGS{sql}
                                 =
[qw/sql_insert sql_value sql_one sql_all sql_col sql_hash sql_hash_multi
/];
#-----------------------------------------------------------------

=head2 :sql - Database manipulation routines

=head3 sql_hash_multi

Use DBIx::Simple: map_pairs { push @{$hash{$a}}, $b } $db->query($sql, @stuff)->flat;

 sql_hash_multi( $dbh, $sql, \@stuff, %options );

Prepares and executes a database request for database pairs returned by the
query. Query should produce exactly two values per row. Return value is the
hashref constructed from the row pairs with first column as keys and B<arrays>
of the second column as values. Hash values will be arrays even if only
one result is returned for the corresponding key. @stuff is an optional
list of placeholder substitutions (note: not optional when using the
closure form).

options:

=over 4

=item closure

If true, a sub ref is returned instead that will execute and fetch values
using a prepared statement handle. When calling the closure, an array of
parameters must be provided (even if empty).

 my $get_hash = sql_hash_multi( $dbh, $sql, closure => 1 );
 $hash = $get_hash->([]);
 $hash = $get_hash->([]);
 $hash = $get_hash->([]);

 # clean up statement handle when finished
 $get_hash->();

=back

=cut

#BEGIN: sql_hash_multi
sub sql_hash_multi {
  my ($dbh, $query) = splice @_, 0, 2;
  my $stuff = (@_ % 2) ? shift : [];
  my %opt = @_;
  my $sth = $dbh->prepare( $query );

  my $closure = sub {
    if (@_) {
      my $stuff = (1 == @_ and ref($_[0]) eq 'ARRAY') ? shift : [@_];
      Carp::croak("SQL Execute failed: ".$sth->errstr) unless $sth->execute( @$stuff );
      my ($k, $v, %res);
      Carp::croak("Binding columns failed: ".$sth->errstr) unless $sth->bind_columns(\($k, $v));
      push @{$res{$k}}, $v while $sth->fetch;
      Carp::croak("Fetch failed: ".$sth->errstr) if $sth->err;
      return \%res;
    } else {
      undef $sth;
    }
  };

  return $opt{closure} ? $closure : $closure->($stuff);
}
#END: sql_hash_multi

=head3 sql_hash

Use DBIx::Simple: %hash = $db->query($sql, @stuff)->flat;


 sql_hash( $dbh, $sql, \@stuff, %options );

Prepares and executes a database request for database pairs returned by the
query. Query should produce exactly two values per row. Return value is the
hashref constructed for the row pairs. @stuff is an optional list of
placeholder substitutions (note: not optional when using the closure form).

options:

=over 4

=item closure

If true, a sub ref is returned instead that will execute and fetch values
using a prepared statement handle. When calling the closure, an array of
parameters must be provided (even if empty).

 my $get_hash = sql_hash( $dbh, $sql, closure => 1 );
 $hash = $get_hash->([]);
 $hash = $get_hash->([]);
 $hash = $get_hash->([]);

 # clean up statement handle when finished
 $get_hash->();

=back

=cut

#BEGIN: sql_hash
sub sql_hash {
  my ($dbh, $query) = splice @_, 0, 2;
  my $stuff = (@_ % 2) ? shift : [];
  my %opt = @_;
  my $sth = $dbh->prepare( $query );

  my $closure = sub {
    if (@_) {
      my $stuff = (1 == @_ and ref($_[0]) eq 'ARRAY') ? shift : [@_];
      Carp::croak("SQL Execute failed: ".$sth->errstr) unless $sth->execute( @$stuff );
      my ($k, $v, %res);
      Carp::croak("Binding columns failed: ".$sth->errstr) unless $sth->bind_columns(\($k, $v));
      $res{$k} = $v while $sth->fetch;
      Carp::croak("Fetch failed: ".$sth->errstr) if $sth->err;
      return \%res;
    } else {
      undef $sth;
    }
  };

  return $opt{closure} ? $closure : $closure->($stuff);
}
#END: sql_hash

=head3 sql_col

Use DBIx::Simple: @col = $db->query($sql, @stuff)->flat;

 sql_col( $dbh, $sql, \@stuff, %options );

Prepares and executes a database request for an entire table column
returned by the query. Return value is an arrayref of zero or more values
in the column. @stuff is an optional list of placeholder substitutions
(note: not optional when using the closure form).

options:

=over 4

=item closure

If true, a sub ref is returned instead that will execute and fetch values
using a prepared statement handle. When calling the closure, an array of
parameters must be provided (even if empty).

 my $get_col = sql_col( $dbh, $sql, closure => 1 );
 $col = $get_col->([]);
 $col = $get_col->([]);
 $col = $get_col->([]);

 # clean up statement handle when finished
 $get_col->();

=back

=cut

#BEGIN: sql_col
sub sql_col {
  my ($dbh, $query) = splice @_, 0, 2;
  my $stuff = (@_ % 2) ? shift : [];
  my %opt = @_;
  my $sth = $dbh->prepare( $query );

  my $closure = sub {
    if (@_) {
      my $stuff = (1 == @_ and ref($_[0]) eq 'ARRAY') ? shift : [@_];
      my $res = $dbh->selectcol_arrayref($sth,{},@$stuff);
      Carp::croak("SQL Execute failed: ".$sth->errstr) if $sth->err;
      return $res;
    } else {
      undef $sth;
    }
  };

  return $opt{closure} ? $closure : $closure->($stuff);
}
#END: sql_col

=head3 sql_all

Use DBIx::Simple: @hashes = $db->query($sql, @stuff)->hashes;

 sql_all( $dbh, $sql, \@stuff, %options );

Prepares and executes a database request for a all table rows returned by
the query. Return value is an arrayref of zero or more hashrefs describing
the result. @stuff is an optional list of placeholder substitutions (note:
not optional when using the closure form).

options:

=over 4

=item closure

If true, a sub ref is returned instead that will execute and fetch values
using a prepared statement handle. When calling the closure, an array of
parameters must be provided (even if empty).

 my $get_em = sql_all( $dbh, $sql, closure => 1 );
 $rows = $get_em->([]);
 $rows = $get_em->([]);
 $rows = $get_em->([]);

 # clean up statement handle when finished
 $get_em->();

=item name

Used to set name canonicalization option in DBI fetcher. May be "lc" or
"uc". Anything else returns keys in database case (depends on database).

=back

=cut

#BEGIN: sql_all
sub sql_all {
  my ($dbh, $query) = splice @_, 0, 2;
  my $stuff = (@_ % 2) ? shift : [];
  my %opt = (@_);
  $opt{name} = ($opt{name} and $opt{name} =~ /^(lc|uc)$/i) ? "NAME_".lc($1) : "";
  my $old_name;
  ($old_name, $dbh->{FetchHashKeyName}) = ($dbh->{FetchHashKeyName}, $opt{name}) if $opt{name};
  my $sth = $dbh->prepare( $query );
  $dbh->{FetchHashKeyName} = $old_name if $opt{name};

  my $closure = sub {
    if (@_) {
      my $stuff = (1 == @_ and ref($_[0]) eq 'ARRAY') ? shift : [@_];
      Carp::croak("SQL Execute failed: ".$sth->errstr) unless $sth->execute( @$stuff );
      my $res = $sth->fetchall_arrayref({});
      Carp::croak("Fetchall failed: ".$sth->errstr) if $sth->err;
      return $res;
    } else {
      undef $sth;
    }
  };

  return $opt{closure} ? $closure : $closure->($stuff);
}
#END: sql_all

=head3 sql_one

Use DBIx::Simple: $result = $db->query($sql, @stuff); while ($row = $result->hash) { ... }

 sql_one( $dbh, $sql, \@stuff, %options );

Prepares and executes a database request for a single table row. Return
value is a unique hashref describing the result row or undef if no results
were found. @stuff is an optional list of placeholder substitutions (note:
not optional when using the closure form).

options:

=over 4

=item closure

If true, a sub ref is returned instead that will execute and fetch values
using a prepared statement handle. When calling the closure, an array of
parameters must be provided (even if empty).

 my $get_one = sql_one( $dbh, $sql, closure => 1 );
 $row = $get_one->([]);
 $row = $get_one->([]);
 $row = $get_one->([]);

 # clean up statement handle when finished
 $get_one->();

=item name

Used to set name canonicalization option in DBI fetcher. May be "lc" or
"uc". Anything else returns keys in database case (depends on database).

=back

=cut

#BEGIN: sql_one
sub sql_one {
  my ($dbh, $query) = splice @_, 0, 2;
  my $stuff = (@_ % 2) ? shift : [];
  my %opt = (@_);
  $opt{name} = ($opt{name} and $opt{name} =~ /^(lc|uc)$/i) ? "NAME_".lc($1) : "";
  my $old_name;
  ($old_name, $dbh->{FetchHashKeyName}) = ($dbh->{FetchHashKeyName}, $opt{name}) if $opt{name};
  my $sth = $dbh->prepare( $query );
  $dbh->{FetchHashKeyName} = $old_name if $opt{name};

  my $closure = sub {
    if (@_) {
      my $stuff = (1 == @_ and ref($_[0]) eq 'ARRAY') ? shift : [@_];
      Carp::croak("SQL Execute failed: ".$sth->errstr) unless $sth->execute( @$stuff );
      my $res = $sth->fetchrow_hashref();
      Carp::croak("Fetchrow failed: ".$sth->errstr) if $sth->err;
      $sth->finish if $res;
      return $res;
    } else {
      undef $sth;
    }
  };

  return $opt{closure} ? $closure : $closure->($stuff);
}
#END: sql_one

=head3 sql_value

Use DBIx::Simple: ($value) = $db->query($sql, @stuff)->list;

 sql_value( $dbh, $sql, \@stuff, %options );

Prepares and executes a database request for a single value. Return value
is requested value. Sub returns an empty list (that is, no value) if no
rows matched the query. @stuff is an optional list of placeholder
substitutions (note: not optional when using the closure form). If more
than one column is returned by the query, an array ref of the row (will be
a copy of the DBI arrayref) will be returned.

options:

=over 4

=item closure

If true, a sub ref is returned instead that will execute and fetch values
using a prepared statement handle. When calling the closure, an array of
parameters must be provided (even if empty).

 my $get_val = sql_value( $dbh, $sql, closure => 1 );
 $value = $get_val->([]);
 $value = $get_val->([]);
 $value = $get_val->([]);

 # clean up statement handle when finished (not really necessary here)
 $get_val->();

=back

=cut

#BEGIN: sql_value
sub sql_value {
  my ($dbh, $query) = splice @_, 0, 2;
  my $stuff = (@_ % 2) ? shift : [];
  my %opt = @_;
  my $sth = $dbh->prepare( $query );

  my $closure = sub {
    if (@_) {
      my $stuff = (1 == @_ and ref($_[0]) eq 'ARRAY') ? shift : [@_];
      Carp::croak("SQL Execute failed: ".$sth->errstr) unless $sth->execute( @$stuff );
      my $res = $sth->fetchrow_arrayref;
      Carp::croak("Fetchrow failed: ". $sth->errstr) if $sth->err;
      $sth->finish;
      return unless $res;
      return (@$res > 1) ? [@$res] : @$res ? $$res[0] : ();
    } else {
      undef $sth;
    }
  };

  return $opt{closure} ? $closure : $closure->($stuff);
}
#END: sql_value

=head3 sql_insert

Use DBIx::Simple: $db->insert($table, \%stuff);

 sql_insert( $dbh, $table, \%stuff, %options );

Prepares and executes a table insert. Return value is the result of the
statement execution.

options:

=over 4

=item closure

If true, a sub ref is returned instead that will perform inserts using a
prepared statement handle. The data in the initial call B<will not be
inserted> so can simply be a "template" hash showing which columns will be
needed.

 my $my_insert =  sql_insert( $dbh, $table, \%stuff, closure => 1 );
 $my_insert->( \%stuff );
 $my_insert->( \%stuff1 );
 $my_insert->( \%stuff2 );

 # clean up statement handle when finished
 $my_insert->();

=item on_conflict

Algorithm to deal with conflicts. See:
http://www.sqlite.org/lang_conflict.html

Support Matrix (partial support ):

          ROLLBACK   ABORT   FAIL   IGNORE   REPLACE
 Pg                   Yes            Yes       Yes
 SQLite     Yes       Yes    Yes     Yes       Yes

=item primary_key

Possibly needed for C<on_conflict =E<gt> REPLACE> by some drivers. Is
scalar or arrayref of columns to use as primary key. If not provided, the
DBI method C<primary_key> will be used to try to determine the primary key
columns. This option can be used to override or to avoid computation of the
auto-determined values.

=back

=cut

#BEGIN: sql_insert
sub sql_insert {
  my ($dbh, $table, $stuff, %opt) = @_;
  my @keys = keys %$stuff;
  my $cols = join ",", map $dbh->quote_identifier($_), @keys;
  my $q = join ",", map "?", @keys;
  my ($conflict) = grep +($_ eq uc($opt{on_conflict}||"")), qw/ROLLBACK ABORT FAIL IGNORE REPLACE/;
  Carp::croak("bad on_conflict action: '$opt{on_conflict}'") if $opt{on_conflict} and !$conflict;
  my $qtab = $dbh->quote_identifier($table);

  my $driver = $dbh->{Driver}->{Name};
  my ($sth, $execute, $finish);

  if ($driver eq 'Pg') {
    my $TO_INSERT;
    $sth = $dbh->prepare( "INSERT INTO $qtab ($cols) VALUES ($q)" );
    my $previous_handler = $sth->{HandleError};

    # CONFLICT: 23505 - UNIQUE VIOLATION [http://www.postgresql.org/docs/8.1/interactive/errcodes-appendix.html]
    if ($conflict and $conflict eq 'IGNORE')  {
      $sth->{HandleError} = sub {
        print STDERR "** Custom HandleError: $_[0]\n";
        return 1 if $sth->state and $sth->state eq '23505';
        return ($previous_handler and &$previous_handler(@_));
      }
    }
    elsif ($conflict and $conflict eq 'REPLACE') {
      my $pk = ('ARRAY' eq ref($opt{primary_key})) ?  $opt{primary_key}  :
                       defined($opt{primary_key})  ? [$opt{primary_key}] :
               [ $dbh->primary_key( "","", $table ) ];
      Carp::croak("Missing primary key on table '$table'. Need primary_key option.") unless @$pk and defined $$pk[0];

      $sth->{HandleError} = sub {
        return ($previous_handler and &$previous_handler(@_))
          unless $sth->state and $sth->state eq '23505';
        my $set   = join ",", map $dbh->quote_identifier($_).'=?', @keys;
        my $where = join ",", map $dbh->quote_identifier($_).'=?', @$pk;
        my $val = eval { $dbh->do( "UPDATE $qtab SET $set WHERE $where", {}, @$TO_INSERT{@keys}, @$TO_INSERT{@$pk} ) };
        return ($previous_handler and &$previous_handler(@_)) if $@;
        $sth->set_err(undef,undef); # turn off the error
        $_[2] = $val;               # supply alternative return value
        return 1;
      };
    }
    elsif ($conflict and $conflict eq 'ABORT') { 1; }# The default anyway
    elsif ($conflict) { Carp::croak("No '$driver' support on_conflict action '$opt{on_conflict}'") }

    $execute = sub { $TO_INSERT = shift; $sth->execute( @$TO_INSERT{@keys} ) };
  }

  elsif ($driver eq 'SQLite') {
    $conflict = $conflict ? " OR $conflict " : "";
    $sth = $dbh->prepare( "INSERT $conflict INTO $qtab ($cols) VALUES ($q)" );
  }

  $execute ||= sub { $sth->execute( @{shift()}{@keys} ) };
  $finish  ||= sub { $sth->finish; undef $sth; };
  if ($opt{closure}) {
    return sub {
      my $insert = (@_ > 1) ? {@_} : shift;
      return $execute->( $insert ) if $insert;
      $finish->();
    }
  }

  my $res = $execute->( $stuff );
  $finish->();
  return $res;
}
#END: sql_insert


#-----------------------------------------------------------------
                        $EXPORT_TAGS{postscript}
                                 =
[qw/psplot_sub psplot_parametric_sub epsplot_linear_forms
/];
#-----------------------------------------------------------------

=head2 :postscript - PostScript generating routines

=head3 psplot_sub

This subroutine takes one argument, a subroutine reference followed by
some or all of the following options. In scalar context the postscript code
which draws the graph is returned. In list context, a hash reference which
contains the actual option values used is also returned. This can be used
to position other information around the graph.

=over 4

=item at

Relative translation (an array ref, [dx, dy], specifying bottom left
corner). Default: [0,0]

=item color

RGB triplet in percentages (0 <= percent <= 1). Default: [0,0,0]

=item intervals

Number of intervals to cut the region into. Default: 100

=item xscale

Length in points of a unit vector on the x-axis. Default: 1

=item xmin

Minimal x value. Default: -10

=item xmax

Maximal x value (will be set from width/xscale if not defined). Default:
undef

=item yscale

Length in points of a unit vector on the y-axis (will be set from
height/ymin/ymax if a height is provided). Default: 1

=item ymin

Minimal y value (will DWIM if not defined. Will chop the graph if set too
high). Default: undef

=item ymax

Maximal y value (will DWIM if not defined. Will chop the graph if set too
low). Default: undef

=item width

Width of the graph in points (72 points = 1 inch). Default: undef

=item height

Height of the graph in points (72 points = 1 inch). Default: undef

=back

To create a postscript document, you will need a header something like the
following:

 print <<HEADER;
 %!PS-Adobe-2.0
 %Creator: $ENV{USER}
 %%Title: Raster Plot
 %%BoundingBox: -10 -10 500 500
 %%Magnification: 1.0000
 %%EndComments
 HEADER

 print psplot_sub( ... );

 print "\nshowpage\n";

=cut

#BEGIN: psplot_sub
sub psplot_sub {
  my ($f) = splice @_, 0, 1;
  my %o   = ( at     => [0,0],    color  => [0,0,0],    intervals => 100,
              xmin   => -10,      ymin   => -(~0),      ymax => ~0,
              xscale => 1,        yscale => 1,
              lineto => "lineto", moveto => "moveto",
              @_ );
  my ($x, $graph)  = (0, ' ');
  my ($fx, @points, $MAX, $dx_ps, $dx_func, $_ymin, $_ymax);
  $o{ymin} = -(~0) unless defined $o{ymin};
  $o{ymax} =   ~0  unless defined $o{ymax};
  $o{$_}  .= "\n"  for qw/lineto moveto/;

  # Deal with the X direction first.
  unless (defined $o{xmax}) {
    if    ($o{width}) { $o{xmax} = $o{xmin} + $o{width}/$o{xscale} }
    else              { $o{xmax} = 10 }
  }

  $o{xscale} = $o{width}  / ( $o{xmax} - $o{xmin} ) if $o{width};
  $o{width}  = $o{xscale} * ( $o{xmax} - $o{xmin} );
  $MAX       = $o{xscale} * ( $o{xmax} - $o{xmin} );
  $dx_ps     =                    $MAX / $o{intervals};
  $dx_func   = ( $o{xmax} - $o{xmin} ) / $o{intervals};

  # Function setup
  local $_ = $o{xmin}; $fx = &$f($_);
  @points = ( [$x, $fx, 'moveto '] );
  $_ymin = $_ymax = $fx;

  # We need to apply the function before tweaking the Y direction
  while ( ($x += $dx_ps) <= $MAX ) {
    $_ += $dx_func;
    $fx = &$f($_);
    $_ymin = $fx if $fx < $_ymin;
    $_ymax = $fx if $fx > $_ymax;
    push @points, [$x, $fx, ($o{ymin} < $fx and $fx < $o{ymax})?$o{lineto}:$o{moveto}];
  }

  # Now deal with the Y direction
  $o{ymin} = $_ymin if $o{ymin} == -(~0);
  $o{ymax} = $_ymax if $o{ymax} ==   ~0;
  $o{yscale} = $o{height}  / ( $o{ymax} - $o{ymin} ) if $o{height};
  $o{height} = $o{yscale} * ( $o{ymax} - $o{ymin} );

  # basic setup (quite easy)
  $graph .= "gsave 2 setlinecap @{$o{color}} setrgbcolor @{$o{at}} translate ";

  my $_shift = ($o{ymin} < 0)?abs($o{ymin}):0;
  $graph .= join ' ', $$_[0], ($_shift+$$_[1])*$o{yscale}, $$_[2] for @points;

  # final touches (stroke and restore the graphics state)
  $graph .= "stroke grestore\n";
  if (wantarray) { return ($graph, \%o) }
  else           { return $graph        }
}
#END: psplot_sub



=head3 psplot_parametric_sub

This subroutine takes one argument, a subroutine reference followed by
some or all of the following options. The provided subroutine should return
a list of two values for a given input. In scalar context the postscript
code which draws the graph is returned. In list context, a hash reference
which contains the actual option values used is also returned. This can be
used to position other information around the graph.

=over 4

=item at

Relative translation (an array ref, [dx, dy], specifying bottom left
corner). Default: [0,0]

=item color

RGB triplet in percentages (0 <= percent <= 1). Default: [0,0,0]

=item intervals

Number of intervals to cut the t-interval into. Default: 100

=item tmin

Minimum t value. Default: 0

=item tmax

Maximal t value. Default: 10

=item xscale

Length in points of a unit vector on the x-axis (will be set from
width/xmin/xmax if a width is provided). Default: 1

=item xmin

Minimal x value (will DWIM if not defined. Will chop the graph if set too
high). Default: undef

=item xmax

Maximal x value (will DWIM if not defined. Will chop the graph if set too
low). Default: undef

=item yscale

Length in points of a unit vector on the y-axis (will be set from
height/ymin/ymax if a height is provided). Default: 1

=item ymin

Minimal y value (will DWIM if not defined. Will chop the graph if set too
high). Default: undef

=item ymax

Maximal y value (will DWIM if not defined. Will chop the graph if set too
low). Default: undef

=item width

Width of the graph in points (72 points = 1 inch). Default: undef

=item height

Height of the graph in points (72 points = 1 inch). Default: undef

=back

To create a postscript document, you will need a header something like the
following:

 print <<HEADER;
 %!PS-Adobe-2.0
 %Creator: $ENV{USER}
 %%Title: Raster Plot
 %%BoundingBox: 0 0 500 500
 %%Magnification: 1.0000
 %%EndComments
 HEADER

 print psplot_parametric_sub( ... );

 print "\nshowpage\n";

=cut

#BEGIN: psplot_parametric_sub
sub psplot_parametric_sub {
  my ($f) = splice @_, 0, 1;
  my %o   = ( at     => [0,0], color => [0,0,0],  intervals => 100,
              tmin   => 0,      tmax => 10,
              xmin   => -(~0),  xmax => ~0,
              ymin   => -(~0),  ymax => ~0,
              xscale => 1,    yscale => 1,
              @_ );
  my ($graph)  = (' ');
  my ($fx, $fy, @points, $tmax, $dt, $_ymin, $_ymax, $_xmin, $_xmax);

  $tmax = $o{tmax};
  $dt   = $o{tmax} / $o{intervals};

  # Function setup
  local $_ = $o{tmin}; ($fx,$fy) = &$f($_);
  @points = ( [$fx, $fy, 'moveto '] );
  $_xmin = $_xmax = $fx;
  $_ymin = $_ymax = $fy;

  # We need to apply the function before tweaking the Y direction
  while ( ($_ += $dt) <= $tmax ) {
    ($fx, $fy) = &$f($_);
    $_xmin = $fx if $fx < $_xmin; $_xmax = $fx if $fx > $_xmax;
    $_ymin = $fy if $fy < $_ymin; $_ymax = $fy if $fy > $_ymax;
    push @points, [$fx, $fy, ($o{xmin} < $fx and $fx < $o{xmax} and
                              $o{ymin} < $fy and $fy < $o{ymax})?'lineto ':'moveto '];
  }

  # Deal with the X direction
  $o{xmin} = $_xmin if $o{xmin} == -(~0);
  $o{xmax} = $_xmax if $o{xmax} ==   ~0;
  $o{xscale} = $o{width}  / ( $o{xmax} - $o{xmin} ) if $o{width};
  $o{width}  = $o{xscale} * ( $o{xmax} - $o{xmin} );

  # Now deal with the Y direction
  $o{ymin} = $_ymin if $o{ymin} == -(~0);
  $o{ymax} = $_ymax if $o{ymax} ==   ~0;
  $o{yscale} = $o{height} / ( $o{ymax} - $o{ymin} ) if $o{height};
  $o{height} = $o{yscale} * ( $o{ymax} - $o{ymin} );

  # basic setup (quite easy)
  $graph .= "gsave 2 setlinecap @{$o{color}} setrgbcolor @{$o{at}} translate ";

  my $x_shift = ($o{xmin} < 0)?abs($o{xmin}):0;
  my $y_shift = ($o{ymin} < 0)?abs($o{ymin}):0;
  $graph .= join ' ', ($x_shift+$$_[0])*$o{xscale},
                      ($y_shift+$$_[1])*$o{yscale}, $$_[2]
                        for @points;

  # final touches (stroke and restore the graphics state)
  $graph .= "stroke grestore\n";
  if (wantarray) { return ($graph, \%o) }
  else           { return $graph        }
}
#END: psplot_parametric_sub


=head3 epsplot_linear_forms

 epsplot_linear_forms \@linear_forms
 epsplot_linear_forms $file, \@linear_forms
 epsplot_linear_forms $file, \@linear_forms, %options

Create a 1" x 1" eps file of the given linear forms. The given matrix
should be in general position if you want that, otherwise any elements of
the form C<[0,0,*]> will cause a "round box" to be drawn around the lines
that represents a line at infinity. If a file name or file handle is
provided, then the resulting image will be written to the file, otherwise
the image code will be returned.

Any subset of the following options may be provided. If the subroutine is
called in list context then the return value will be a hash of the actual
option values used together with a C<plot> entry that contains the actual
postscript code.

=over 4

=item center_origin =E<gt> I<boolean>

Boolean value indicating whether the center of the generated eps document
must correspond to the origin C<(0,0)>. (DEFAULT: 0)

=item force_square =E<gt> I<boolean>

Boolean value indicating whether the x and y scales must be the same.
(DEFAULT: 0)

=item x_min =E<gt> I<number>

=item x_max =E<gt> I<number>

=item y_min =E<gt> I<number>

=item y_max =E<gt> I<number>

Numbers giving the corresponding values. If these values are not provided,
then they will be computed to ensure that all pair-wise intersection points
fit within the final view. You can get (for instance) all positive
intersections by only specifying C<x_min =E<gt> 0> and C<y_min =E<gt> 0>.

=item line_width =E<gt> I<number>

Width of the lines drawn (in points, 72 pt = 1 in). (DEFAULT: 1.5)

=item in_sep =E<gt> I<multiple>

Multiples (possibly non-integral) of the line_width to separate the
arrangement graph from the line at infinity since intersecting the line at
infinity is bad chi. (DEFAULT: 3)

=item x_pad =E<gt> I<percentage>

=item y_pad =E<gt> I<percentage>

If this subroutine computes any of the *_min/max values, then this will
cause intersections to occur on the edge of the graph which is ugly. The x
and y paddings are percentages of the graph that should be dedicated to
the space beyond the intersections on each side. (DEFAULT: .15)

=item precision =E<gt> I<integer> or C<undef>

Number of places after the decimal to use when comparing slopes of lines.
Due to machine precision, determining whether two lines are parallel is
tricky. Thus, by default this subroutine throws away some precision in the
computation of the bounding box. The down side is that we may sometimes
miss the intersection of two lines if they occur "far away". Setting this
value to C<undef> will use the standard perl C<==> test for numbers.
Setting this value to an integer will use a string equality test rounding
to the given number of places after the decimal. (DEFAULT: 7)

=back

=cut

#BEGIN: epsplot_linear_forms, depends: min_max, max, min
sub epsplot_linear_forms {
  my $file = (ref($_[0]) eq 'ARRAY') ? undef : shift;
  my $pm   = shift;
  my %o    = (precision => 7, line_width => 1.5, in_sep => 3, x_pad => .15, y_pad => .15, @_);
  $o{f}    = "%.$o{precision}f" if defined $o{precision};
  my (@data, @isect, @x, @y, $d, $we_computed_bb);

  if (grep !exists($o{$_}), qw/x_min x_max y_min y_max/) {
    $we_computed_bb = 1;
    for (0..$#{$pm}) {
      my $i = $_;
      for (($i+1)..$#{$pm}) {
        my $j = $_;
        if (defined $o{f}) { next if sprintf($o{f}, 0) eq sprintf($o{f}, ($d = $$pm[$i][0]*$$pm[$j][1] - $$pm[$i][1]*$$pm[$j][0])) }
        else               { next if 0 == ($d = $$pm[$i][0]*$$pm[$j][1] - $$pm[$i][1]*$$pm[$j][0]) }
        push @x, ($$pm[$i][2]*$$pm[$j][1] - $$pm[$i][1]*$$pm[$j][2])/-$d;
        push @y, ($$pm[$i][2]*$$pm[$j][0] - $$pm[$i][0]*$$pm[$j][2])/$d;
        # say("Lines $i and $j intersect at ($x[-1], $y[-1]); d = $d; precision = $o{precision}; ", sprintf($o{f}, 0), " == ", sprintf($o{f}, $d));
      }
    }
    @x = min_max(@x); @y = min_max(@y);
    $o{x_min} = $x[0] unless exists $o{x_min};
    $o{x_max} = $x[1] unless exists $o{x_max};
    $o{y_min} = $y[0] unless exists $o{y_min};
    $o{y_max} = $y[1] unless exists $o{y_max};
  }

  if ($o{center_origin}) {
    $o{x_min} = -($o{x_max} = max(map abs, @o{qw/x_min x_max/}));
    $o{y_min} = -($o{y_max} = max(map abs, @o{qw/y_min y_max/}));
  }

  if ($o{force_square}) {
    my $xscale = $o{x_max} - $o{x_min};
    my $yscale = $o{y_max} - $o{y_min};
    my  $scale = max($xscale, $yscale);

    $o{x_min} -= ($scale-$xscale)/2;
    $o{x_max} += ($scale-$xscale)/2;
    $o{y_min} -= ($scale-$yscale)/2;
    $o{y_max} += ($scale-$yscale)/2;
  }

  if ($we_computed_bb) {
    my $xpad = $o{x_pad}*($o{x_max}-$o{x_min});
    my $ypad = $o{y_pad}*($o{y_max}-$o{y_min});
    $o{x_min} -= $xpad;
    $o{x_max} += $xpad;
    $o{y_min} -= $ypad;
    $o{y_max} += $ypad;
  }

  # no zero-width boxes.
  for (qw/x y/) { ($o{"${_}_min"}, $o{"${_}_max"}) = ($o{"${_}_min"}-1, $o{"${_}_max"}+1) if $o{"${_}_min"} == $o{"${_}_max"} }

  push @data, join " ", $o{in_sep}*$o{line_width}, $o{in_sep}*$o{line_width}, "translate"; # Absolute coordinates

  my ($xscl, $yscl) = ((72-2*$o{in_sep}*$o{line_width})/($o{x_max}-$o{x_min}), (72-2*$o{in_sep}*$o{line_width})/($o{y_max}-$o{y_min}));
  push @data, join " ", $xscl, $yscl, "scale"; # do scaling
  push @data, join " ", -$o{x_min}, -$o{y_min}, "translate";
  push @data, join " ", @o{qw/x_min y_min/}, $o{x_max}-$o{x_min}, $o{y_max}-$o{y_min}, "rectclip";

  for my $l (@$pm) {
    if ($$l[0] == 0 and $$l[1] == 0) {
      unshift @data, <<"      ROUND_BOX";
        gsave
        72 72 scale
        newpath
        .1  0 moveto
        .9 .1 .1 270   0 arc
        .9 .9 .1   0  90 arc
        .1 .9 .1  90 180 arc
        .1 .1 .1 180 270 arc
        closepath
        1 72 div 1 72 div scale stroke
        grestore
      ROUND_BOX
      next;
    }

    push @data, "% line: @$l";

    if ($$l[1] == 0) {
      push @data, join " ", -$$l[2]/$$l[0], $o{y_min}, "moveto",
                            -$$l[2]/$$l[0], $o{y_max}, "lineto";
      next;
    } else {
      push @data, join " ", $o{x_min}, (-$$l[0]*$o{x_min}-$$l[2])/$$l[1], "moveto",
                            $o{x_max}, (-$$l[0]*$o{x_max}-$$l[2])/$$l[1], "lineto";
    }
  }

  unshift @data, $o{line_width}." setlinewidth",         # line width
                 ($o{line_width}/2)." ".($o{line_width}/2)." translate",           # translate to fit line at infinity
                 ((72-$o{line_width})/72)." ".((72-$o{line_width})/72)." scale",   # scale to fit line at infinity
          ;

  local $" = "\n";
  $o{plot} = <<"FILE";
%!PS-Adobe-2.0 EPSF-2.0
%%Creator: Dean::Util
%%CreationDate: @{[scalar localtime]}
%%BoundingBox: 0 0 72 72

@data
gsave 1 $xscl div 1 $yscl div scale stroke grestore
FILE

  if (defined $file) {
    open my $F, ">", $file or croak "Can't open image '$file' for writing: $!";
    print $F $o{plot};
    close $F;
  }
  wantarray ? %o : $o{plot};
}
#END: epsplot_linear_forms





#-----------------------------------------------------------------
                       $EXPORT_TAGS{untaint}
                                 =
[qw/ untaint untaint_file untaint_int untaint_num /];
#-----------------------------------------------------------------

=head2 :untaint - Untainting

See also L<Tests and Patterns|/":patterns - Tests and Patterns">

=head3 untaint

BAD PROGRAMMER, do not use!

=cut

=head3 untaint_int

Strict int untainter/converter. Must match /^[-+]?[0-9]+$/

=cut

=head3 untaint_num

Strict numeric untainter/converter. Must match /^[-+]?([0-9]+\.?|[0-9]*\.[0-9]+)$/

=cut

=head3 untaint_file

Returns untainted string if argument could be passed unquoted to a bash
shell (for instance, spaces are not allowed). If argument is undefined or
contains any illegal characters at all, C<undef> is returned. See also:
L<canonicalize_filename|/canonicalize_filename>.

Note: Is currently more restrictive than necessary. This will improve over time.

=cut

#BEGIN: untaint, 1 line
sub untaint :prototype($) {no re 'taint'; ((defined$_[0])?$_[0]=~/(.*)/sm:'')[0]}
#BEGIN: untaint_file, 1 line
sub untaint_file :prototype($) {no re 'taint'; ((defined$_[0])?$_[0]=~m|^([\w\-\+\,\~\.\/]+)$|:undef)[0]}
#BEGIN: untaint_int, 1 line
sub untaint_int :prototype($) {no re 'taint'; return unless defined($_[0]) and $_[0] =~ /^([-+]?[0-9]+)$/; return 0 + $1 }
#BEGIN: untaint_num, 1 line
sub untaint_num :prototype($) {no re 'taint'; return unless defined($_[0]) and $_[0] =~ /^([-+]?(?:[0-9]+\.?|[0-9]*\.[0-9]+))$/; return 0 + $1 }



#-----------------------------------------------------------------
                        $EXPORT_TAGS{logging}
                                 =
 [qw/info DEBUG INFO NOTICE WARNING ERR ERROR CRIT ALERT EMERG /];
#-----------------------------------------------------------------

=head2 :logging - Simple logging utilities

=head3 info

 info( $level, [\$info_level], [\$ofstream], @text )

Print the information C<@text> to C<$ofstream> if C<$info_level> is greater
than or equal to C<$level>. Returns 1 if message was printed and 0 if it
was not. C<$info_level> defaults to C<$CALLER::INFO_LEVEL> and C<$ofstream>
defaults to C<$CALLER::LOG> if it is a C<GLOB> or C<IO::*> object. A
newline will be appended to the last string of C<@text> if it is not
already present.

The default INFO_LEVEL is 0. The default LOG is STDERR.

NOTE: C<$INFO_LEVEL> and C<$LOG> must be package variables (declared with
C<our> or C<use vars>) for this function to work correctly.

=cut

#BEGIN: info
sub info {
  no strict 'refs';
  my $caller = caller;
  my $l = shift;
  my ($L, $LOG);
  if (ref($_[0]) eq 'SCALAR') { $L = ${shift()} }
  else { $L = ${$caller."::INFO_LEVEL"} || 0 }
  return 0 unless $L >= $l and @_;

  if    (ref($_[0]) =~ /^(?:GLOB|IO::)/)              { $LOG = shift }
  elsif (ref(${$caller."::LOG"}) =~ /^(?:GLOB|IO::)/) { $LOG = ${$caller."::LOG"} }
  else                                                { $LOG = *STDERR }

  print $LOG @_ if @_;
  print $LOG "\n" unless @_ and $_[-1] and $_[-1] =~ /\n$/;
  return 1;
}
#END: info

=head3 DEBUG

 DEBUG( @text );

Calls info with a level of 0. Also prefixes each line of text with "DEBUG: ".

=cut

#BEGIN: DEBUG, depends info
sub DEBUG {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/DEBUG: /sg;
    info(0, "DEBUG: ", $msg );
}
#END: DEBUG

=head3 INFO

 INFO( @text );

Calls info with a level of 1. Also prefixes each line of text with "INFO: ".

=cut

#BEGIN: INFO, depends info
sub INFO {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/INFO: /sg;
    info(1, "INFO: ", $msg );
}
#END: INFO

=head3 NOTICE

 NOTICE( @text );

Calls info with a level of 2. Also prefixes each line of text with "NOTICE: ".

=cut

#BEGIN: NOTICE, depends info
sub NOTICE {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/NOTICE: /sg;
    info(2, "NOTICE: ", $msg );
}
#END: NOTICE

=head3 WARNING

 WARNING( @text );

Calls info with a level of 3. Also prefixes each line of text with "WARNING: ".

=cut

#BEGIN: WARNING, depends info
sub WARNING {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/WARNING: /sg;
    info(3, "WARNING: ", $msg );
}
#END: WARNING

=head3 ERR

 ERR( @text );

Calls info with a level of 4. Also prefixes each line of text with "ERR: ".

=cut

#BEGIN: ERR, depends info
sub ERR {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/ERR: /sg;
    info(4, "ERR: ", $msg );
}
#END: ERR

=head3 ERROR

 ERROR( @text );

Calls info with a level of 4. Also prefixes each line of text with "ERROR: ".
(is an alias for ERR())

=cut

#BEGIN: ERROR, depends info
sub ERROR {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/ERROR: /sg;
    info(4, "ERROR: ", $msg );
}
#END: ERROR

=head3 CRIT

 CRIT( @text );

Calls info with a level of 5. Also prefixes each line of text with "CRIT: ".

=cut

#BEGIN: CRIT, depends info
sub CRIT {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/CRIT: /sg;
    info(5, "CRIT: ", $msg );
}
#END: CRIT

=head3 ALERT

 ALERT( @text );

Calls info with a level of 6. Also prefixes each line of text with "ALERT: ".

=cut

#BEGIN: ALERT, depends info
sub ALERT {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/ALERT: /sg;
    info(6, "ALERT: ", $msg );
}
#END: ALERT

=head3 EMERG

 EMERG( @text );

Calls info with a level of 7. Also prefixes each line of text with "EMERG: ".

=cut

#BEGIN: EMERG, depends info
sub EMERG {
    my $msg = join "", @_;
    $msg =~ s/\n\K(?=.)/EMERG: /sg;
    info(7, "EMERG: ", $msg );
}
#END: EMERG


#-----------------------------------------------------------------
                        $EXPORT_TAGS{system}
                                 =
 [qw/ do_as pidof /];
#-----------------------------------------------------------------

=head2 :system - System / sysadmin tools

=cut

=head3 pidof

 my @progs = pidof $program;
 my @progs = pidof %opts;

Searches /proc for running programs matching the given name or options. Any
options will match against the correcponding value via smartmatch EXCEPT for
the pid option which must be an exact PID or array of PIDs.

=over 4

=item program

Name of command (excludes path part).

=item command

Command - includes path if used in execution of program which makes this a
bit unreliable if the command is started from a command prompt.

=item cmdline

Contents of /proc/$pid/cmdline, namely the command and command line
arguments joined by NULL characters. Programs without command line
arguments will immediately fail.

=item args

Matched against array of just the command line arguments C<$VALUE ~~ @args>.
Programs without command line arguments will immediately fail.

=item pid

PID or array of PIDs to examine.

=item user

User name

=item uid

User id

=item group

Group name

=item gid

Group id

=back

=cut

#BEGIN: pidof, depends: cat
sub pidof {
    BEGIN {
        warnings->unimport('experimental::smartmatch') if exists($warnings::Offsets{'experimental::smartmatch'});
    }
    no re 'taint';
    require File::stat;
    require File::Basename;

    unshift @_, "program" if 1 == @_;
    my %o = @_;

    my @pids;
    if ($o{pid}) {
        @pids = 'ARRAY' eq ref($o{pid}) ? @{$o{pid}} : $o{pid};
    } else {
        opendir my $PROC, "/proc" or die "Error reading proc directory: $!";
        @pids = readdir $PROC;
    }

    # Fallback to user/group in case someone passes "user => 1001"
    $o{uid} = getpwnam(delete $o{user})  // $o{user}  if exists $o{user};
    $o{gid} = getpwnam(delete $o{group}) // $o{group} if exists $o{group};

    my @procs;
  PROC:
    for (@pids) {
        next PROC unless /^(\d+)$/;
        my $dir = "/proc/$1";
        my %p = ( pid => $1, procdir => $dir );
        next PROC unless eval {
            $p{stat}    = File::stat::stat("$dir");
            $p{cmdline} = [ split /\0/, scalar(cat("$dir/cmdline")) // "" ];
            1
        };
        for (keys %o) {
            my $val = $o{$_};
            if ($_ eq 'program') {
                next PROC unless $p{cmdline}[0];
                $p{program} = File::Basename::fileparse($p{cmdline}[0]);
                next PROC unless $p{program} ~~ $val;
            }
            elsif ($_ eq 'command') { next PROC if !$p{cmdline}[0] or not $p{cmdline}[0] ~~ $val }
            elsif ($_ eq 'cmdline') { next PROC unless @{$p{cmdline}} > 1 and join("\0", @{$p{cmdline}}) ~~ $val }
            elsif ($_ eq 'args')    { next PROC unless @{$p{cmdline}} > 1 and $val ~~ [ @{$p{cmdline}}[1..$#{$p{cmdline}}] ] }
            elsif ($_ eq 'uid')     { next PROC unless $p{stat}->uid  ~~ $val }
            elsif ($_ eq 'gid')     { next PROC unless $p{stat}->gid  ~~ $val }
        }
        push @procs, \%p;
    }
    return @procs;
}
#END: pidof



=head3 do_as

 do_as "username", sub { ... };
 do_as "username:groupname", sub { ... };

Locally change the effective user id and execute some code. Only works if
current user is root!

Ensures that $ENV{USER} and $ENV{HOME} are set appropriately. Will
eventually include options which will attempt to setup DISPLAY, DBUS,
XAUTH, SSH_AGENT, GPG_AGENT, and other variables useful for running and
connecting to existing X sessions, apps, and daemons of the user.

=cut

# What doees this do? - looks like nothing useful for us [http://www.jwz.org/xscreensaver/faq.html]
#   xauth -f /home/user/.Xauthority nextract - $DISPLAY | xauth nmerge -

#BEGIN: do_as
sub do_as {
  my $func = pop;
  my ($user, %opt) = @_;
  my $group = ($user =~ s/:(.*)//) ? $1 : "";

  my (undef,undef,$uid,$gid,$quota,$comment,$gcos,$dir) = getpwnam($user);
  $gid = getgrnam($group) if $group;
  die unless defined($uid) and defined($gid);
  local %ENV = %ENV;

#   for (`pgrep -u "$user" startkde`) {
#     next unless /^(\d+)$/;
#     next unless my $file = `cat "/proc/$1/environ"`;
#     for (split /\0/, $file) {
#       next unless /^(\w+)=(.*)$/;
#       $ENV{$1} = $2;
#     }
#   }

  $! = 0; local $) = $gid; die "Can't set gid: $!" if 0+$!;
  $! = 0; local $> = $uid; die "Can't set uid: $!" if 0+$!;
  $ENV{USER} = $user;
  $ENV{HOME} = $dir;

  $func->();
}
#END: do_as



#-----------------------------------------------------------------
                          $EXPORT_TAGS{op}
                                 =
 [qw/map_pairs map_pair join_pair join_multi deep_eq deep_diff SYSTEM QX EXEC SELECT EXISTS
     HAS TRUE GETPATH SETPATH unique_id parse_date hpush hdefaults subhash SPRINTF gzdo
     FORK SPLIT modtime subopts pmap pgrep
    /];
#-----------------------------------------------------------------

=head2 :op - Core function extensions


=head3 pmap(&@)

Parallel map. Applies function to each item in input list. Evaluation order
is not defined, however, result array will be ordered as if the map were
performed sequentially. Function is called in list context and may produce
any list of items serializable by Storable.

 # Quickly convert a bunch of images to png:
 pmap { my $old = $_; s/\.[^.]+/.png/; system convert => $old => $_ } @images;

 # Result order matches input order
 use Time::HiRes qw/ sleep /;
 say join " ", pmap { sleep(my $sleep = rand); say "$_: Hello ($sleep)"; $_ } 0..9;

C<$_> is set to each value in turn, though note that C<$_> will be a
I<copy>, not an alias. Therefore modifications to C<$_> will not be
preserved as they are using normal C<map>.

Overhead is reasonably small, but there is little reason to use this
function if your tasks finish quickly. Rough "worst case" benchmarks
(on Linux):

 $_Util::pmap::threads = 2;
 pmap { say "Hello" } 1..1;          # 0.028559 seconds
 pmap { say "Hello" } 1..1_000;      # 0.027256 seconds
 pmap { say "Hello" } 1..10_000;     # 0.067582 seconds
 pmap { say "Hello" } 1..100_000;    # 0.556916 seconds
 say "Hello" for 1..100_000;         # 0.011928 seconds

 @x = pmap { $_ + 1 } 1..1;          # 0.032267 seconds
 @x = pmap { $_ + 1 } 1..1_000;      # 0.030821 seconds
 @x = pmap { $_ + 1 } 1..10_000;     # 0.098850 seconds
 @x = pmap { $_ + 1 } 1..100_000;    # 0.660198 seconds
 @x = map  { $_ + 1 } 1..100_000;    # 0.024077 seconds

Optimizations:

=over 4

=item * Does not collect results when called in void context

=item * Degrades to plain map when  1 == $Parallel::Map::threads

=back

Configuration:

The following variables can be used to control the thread objects used.
Their default values are shown.

 %_Util::pmap::t_opts  = (stack_size => 16*4096);
 $_Util::pmap::threads = sub { ... };

C<$_Util::pmap::threads> should be a number or a sub which tries to
determine the number of CPUs on the system. The default sub should work for
Linux, Windows, and BSD|Darwin and will fall back to "2" if it can not
determine the proper number of CPUs. This sub will be called at most once
as its value will be cached upon the first call of pmap. Setting the
C<NUMBER_OF_PROCESSORS> environment variable is probably the easiest way to
control the number of threads used.

This sub uses only: threads and Storable (core modules since perl 5.008)

=cut

#BEGIN: pmap
$_Util::pmap::threads = sub {
  # ENV variable (reportedly set automatically in Windows)
  $ENV{NUMBER_OF_PROCESSORS}
  || # Linux
    eval {
      no warnings;
      my $nr_cpus = 0;
      if (-r "/proc/cpuinfo") {
        open my $c, "<", "/proc/cpuinfo" or die;
        $nr_cpus = 0+(grep /^processor/, <$c>);
      }
      $nr_cpus
    }
  || # *BSD|*bsd|Darwin
    eval {
      no warnings;
      0+(`sysctl -n hw.ncpu 2>/dev/null`)
    }
  || # sensible default - wrap in eval to clear $@
    eval { 2 }
  # AIX)        lsdev -C -c processor -S Available|grep -c . # wc -l formats funny
  # HP-UX)      ioscan -fnkC processor|grep -c '^processor'
  # IRIX*)      hinv -c processor|head -1|awk '{print $1}'
  # OSF1|SunOS) psrinfo|grep -c on-line
};
%_Util::pmap::t_opts  = (stack_size => 16*4096);
sub pmap :prototype(&@) {
  $_Util::pmap::threads = $_Util::pmap::threads->() || 1 if 'CODE' eq ref($_Util::pmap::threads);
  my $func = shift;
  my $keep_results = defined(wantarray);

  # copy $_ to de-alias as in parallel case
  return map { local $_ = $_; $func->($_) } @_ if 1 == $_Util::pmap::threads;

  require threads;
  require threads::shared;
  require Storable;

  my $data = \@_;
  my $idx :shared = 0;

  my $wrap = sub {
    my ($i, @res);

    while (do { lock($idx); $i = $idx++ } <= $#{$data}) {
      local $_ = $$data[$i];
      if ($keep_results) {
        push @res, [$i, $func->($_)];
      } else {
        $func->($_)
      }
    }
    return Storable::freeze(\@res);
  };

  my @pool = map threads->create(\%_Util::pmap::t_opts, $wrap),
                   1..$_Util::pmap::threads;

  # Join all threads before checking that thread returns result.
  #
  # This has the sad effect of performing a lot of (probably useless) work
  # before die-ing, however, does allow us to fully clean up all our
  # threads in case the caller is wrapped in eval().
  #
  # The best solution is probably to poll for joinable threads over some
  # increasing interval (say, min(1, max(.001*runtime, .001))). If any
  # joinable threads have exited abnormally, kill the remaining threads,
  # and die. The caller can stop this behavior by wrapping their commands
  # in eval (though $@ will not be set for them in that case).
  my @res;
  for my $t (@pool) {
    my $r = $t->join;
    push @res, $r;
    $@ .= $t->error if defined($t->error);
  }

  return map { shift @$_; @$_ } sort { $$a[0] <=> $$b[0] } map @{Storable::thaw($_ || die($@ || "pmap: Thread exited abnormally"))}, @res;
}
#END: pmap


=head3 pgrep(&@)

Parallel grep. Returns items in input list for which the function returns
true. Evaluation order is not defined, however, result array will be
ordered as if the grep were performed sequentially.

 # order is preserved in @good
 my @good = pgrep { expensive_test($_) } @data;

=cut

#BEGIN: pgrep, 1 line; depends: pmap
sub pgrep :prototype(&@) { my $f = shift; pmap(sub { $f->($_) ? ($_) : () }, @_); }


=head3 subopts

 my %opt = subopts( \@_, OPTIONS )

"Parse" subroutine options into an options hash. Handles mixtures of
positional and named parameters, default values, parameter validation, and
other features. Parsing of the argument array is controlled by the
following options:

=over 4

=item positional =E<gt> A

 my %opt = subopts( [1,2,3], positional => [qw/foo bar baz/] );
 # %opt = (foo => 1, bar => 2, baz => 3)

List of positional argument names.

=item p6_positional =E<gt> A

Like positional, but processing stops at the first "known" key value. This
allows for Perl 6-like flexi-parameters. This, however, is somewhat
dangerous if data may match key names. For example:

 # Uh-oh: parses as ( date => "Jan 1", late => 0 )
 my %opt = subopts( ["Jan 1", "late"],
                    p6_positional => [qw/date note/],
                    allowed => ["late"],
                    validate => { late => "bool" }
                  );

=item defaults =E<gt> Ho*

Default values. Values may be arbitrary objects. Subroutine values may be
expanded if 'eval_defaults' option is provided.

=item required =E<gt> A

Any required parameters must not be undefined. Defaults are processed
before requiredness is considered so any required parameter with a valid
default will never cause an error.

=item validate =E<gt> Ho*

Hash of validators. Keys are parameter names, values are validators.

 Sub validators:
   arguments: $value, \%params_so_far
   return: BOOL | SCALAR_REF

 Regexp validators:
   not automatically anchored, be sure to anchor your patterns if you want that

=item untaint =E<gt> BOOL | HoBOOL

If true, any parameters satisfying their corresponding validator will be
untainted. Parameters without a validator will not be untainted.

=item allowed =E<gt> A

Key names (in addition to 'required' key names which may appear in the options.

=item sloppy_known =E<gt> BOOL

By default, 'defaults' and 'validate' hashes are ignored when considering
'allowed' option keys (so that the same default and validate hashes may be
used for multiple subs). Specifying this option will include their keys in
the list of 'allowed' keys.

=item no_dups =E<gt> BOOL

By default,

 %opt = subopts( [ foo => 1, foo => 2 ] )

Will set C<$opt{foo} = 2>. If 'no_dups' is true, this sub will throw an
error at any duplicated kay names.

=item eval_defaults =E<gt> BOOL

If true, any default values which are sub refs will be executed and their
return values used. Useful if default value is expensive to compute.
default subs are called with two parameters: the key name and the current
options hash. he options hash WILL contain ALL user-set parameters but
there are no guarantees about the order in which the defaults are expanded.

=back

=cut

#BEGIN: subopts, depends: untaint
sub subopts {
  my ($opts, %o) = @_;
  my %opt;
  my $nopts = @$opts;

  # Alas, we must do this manually for ourselves
  for (grep !/^(?:(?:p6_)?positional|defaults|required|allowed|untaint|validate|sloppy_known|no_dups|eval_defaults)$/, keys %o) {
    croak "invalid option: '$_'";
  }

  my (%known_opts, %forbidden);

  # for allowed parameter validation, we need a lookup hash
  # Note: positional names are added later
  for (qw/ p6_positional allowed required /) {
    $known_opts{$_} = 1 for @{$o{$_} || []};
  }
  if ($o{sloppy_known}) {
    $known_opts{$_} = 1 for keys(%{$o{defaults} || {}});
  }

  # just pull them off
  if ($o{positional}) {
    $opt{$_} = shift @$opts for @{$o{positional}};
  }

  # p6_positionals loaded as positionals should not appear as named parameters later
  if ($o{p6_positional}) {
    for (@{$o{p6_positional}}) {
      last if exists($known_opts{$$opts[0]});
      $forbidden{$_} = 1;
      $opt{$_} = shift @$opts
    }
  }

  # Add these AFTER processing p6_positionals since they aren't really valid stop words
  $forbidden{$_} = $known_opts{$_} = 1 for @{$o{positional} || []};

  # At this point only named parameters should exist, so even number of opts
  croak "Expected even number of parameters starting at index ".($nopts-@$opts) if @$opts % 2;

  # load the rest of the values
  while (@$opts) {
    my $idx = $nopts - @$opts;
    my ($k, $v) = splice @$opts, 0, 2;

    croak "Forbidden (duplicated) key '$k' at index $idx" if exists($forbidden{$k});
    $forbidden{$k} = 1 if $o{no_dups};

    croak "Unrecognized key '$k' at index $idx" if $o{allowed} and !exists($known_opts{$k});
    $opt{$k} = $v;
  }

  # load necessary defaults. However, respect 'allowed' wishes (in case defaults has extra stuff)
  if ($o{defaults}) {
    my @keys = $opt{allowed} ? (keys %known_opts) : (keys %{$o{defaults}});
    for my $k (@keys) {
      next unless exists $o{defaults}{$k} and defined $o{defaults}{$k};
      next if defined $opt{$k};
      if ($o{eval_defaults} and 'CODE' eq ref($o{defaults}{$k})) {
        $opt{$k} = $o{defaults}{$k}->($k, \%opt);
      } else {
        $opt{$k} = $o{defaults}{$k};
      }
    }
  }

  # check for definedness
  if ($o{required}) {
    for (@{$o{required}}) {
      croak "Missing parameter '$_' in subroutine call" unless defined $opt{$_};
    }
  }

  # Validate here
  if ($o{validate}) {
    for my $key (keys %opt) {
      next unless exists($o{validate}{$key}) and defined($o{validate}{$key});
      my $validator = $o{validate}{$key};
      my $ref = ref($validator);
      my $value = $opt{$key};
      my $valid;
      if ('CODE' eq $ref)      { $valid = $o{validate}{$key}->($value, \%opt) }
      elsif ('Regexp' eq $ref) { $valid = ($value =~ $validator) }
      else                     { die "Invalid validator type $ref" }

      croak "Invalid value for '$key'" unless $valid;
      $opt{$key} = ${$valid}           if ref $valid;
      $opt{$key} = untaint($opt{$key}) if ref($o{untaint}) ? $o{untaint}{$key} : $o{untaint};
    }
  }


  wantarray ? %opt : \%opt;
}
#END: subopts

=head3 modtime

Stupid! use File::stat

Computes the epoch time when the file was last modified.

 print "File last modified: " . localtime( modtime($f) )

=cut

#BEGIN: modtime
sub modtime {
  my $f = $_[0];
  local $^T = time;
  my $time = -M $f;# in days
  return unless defined $time;
  carp "file '$f' modification time is in the future" if $time < 0;
  time - 86400 * $time;
}
#END: modtime

=head3 SPLIT

Split an expression on a pattern ignoring split patterns within delimited
text.

 SPLIT PATTERN, EXPR, LIMIT
 SPLIT PATTERN, EXPR
 SPLIT PATTERN
 SPLIT

Split PATTERN may be a string literal, qr// regular expression, or hashref
containing splitting options. Beware, unlike Perl's split builtin, this
function does not currently support captures in PATTERN. This may be fixed
at some point in the future.

If EXPR is missing, a splitting subroutine is generated and returned.

 my $splitter = SPLIT;
 my $splitter = SPLIT qr/\s*,\s*/;
 my $splitter = SPLIT \%options;

 my @pieces = $splitter->( $text );
 my @pieces = $splitter->( $text, $limit );

 my @pieces = SPLIT qr/\s*,\s*/, $text;
 my @pieces = SPLIT \%options, $text;
 my @pieces = SPLIT \%options, $text, $limit;

The following options are recognized in the options hash:

=over 4

=item on =E<gt> PATTERN

String literal or qr// pattern, see above.

=item delimiters =E<gt> q|"'`|

Delimiters passed to the Text::Balanced::gen_delimited_pat subroutine.

=item escape =E<gt> '\'

Escape characters passed as the second argument to the
Text::Balanced::gen_delimited_pat subroutine.

=back

=cut

#BEGIN: SPLIT
sub SPLIT {
  require Text::Balanced;
  my %o = ( delimiters => q|"'`|, escape => '\\', ((ref($_[0]) eq 'HASH') ? %{shift()} : (on => shift())) );
  $o{on} = qr/\s+/          unless defined $o{on};
  $o{on} = quotemeta $o{on} unless ref $o{on};
  my $delimited = Text::Balanced::gen_delimited_pat(@o{qw/delimiters escape/});

  my $splitter = sub {
    my ($text, $limit) = @_;
    my ($idx, @res) = (0);
    while ((!defined($limit) or ($limit > $idx+1)) and
           $text =~ /\G(?:($delimited)|($o{on})|(.+?)(?=$delimited|$o{on}|\Z))/g) {
      if    (defined $1) { $res[$idx] .= $1 }
      elsif (defined $3) { $res[$idx] .= $3 }
      elsif (defined $2) { $idx++ }
    }
    return @res, (defined pos($text)) ? substr($text, pos($text)) : ();
  };

  return $splitter unless @_;
  return $splitter->(@_);
}
#END: SPLIT




=head3 FORK

 FORK { Child Code }
 FORK \&child, \&parent, \&error
 FORK { Child Code } %options

 &FORK(%options)

Returns child process id on success and nothing (undef) on failure unless
parent action is defined.

options:

=over 4

=item parent =E<gt> CODE || 'exit' || $exec_string || \@exec_command

=item child  =E<gt> CODE || $exec_string || \@exec_command

=item error  =E<gt> CODE || 'text to die by'

=item ignore =E<gt> BOOLEAN

=back

BUGS: "ignore" option doesn't work (local SIGCHLD is useless).

Not sure how to fix it. Want:

* no zombies
* children to be killed when parent exits
* to be able to fork from subs without globals (thus, open "|-" probably
  bad unless we keep them in a package var)

=cut

#BEGIN: FORK
sub FORK :prototype(&@) {
  my %o;
  if (@_ and (ref($_[0]) or $_[0] !~ /^(?:parent|child|error|ignore)$/i)) {
    $o{child} = shift;
    if (@_ and (ref($_[0]) or $_[0] !~ /^(?:parent|child|error|ignore)$/i)) {
      $o{parent} = shift;
      if (@_ and (ref($_[0]) or $_[0] !~ /^(?:parent|child|error|ignore)$/i)) {
        $o{error} = shift;
      }
    }
    %o = (%o, @_);
  } else {
    %o = @_;
  }

  # use YAML; print Dump \%o;
  $o{ignore} and local $SIG{CHLD} = "IGNORE";
  my $res = fork;
  if (!defined $res) {
    ref($o{error}) eq 'CODE' and do { @_ = (); goto &{$o{error}}; };
    $o{error} = "FORK: Unable to fork '$!'" unless defined $o{error};
    croak $o{error};
  }

  if ($res == 0) {
    local $SIG{CHLD} = "";
    ref($o{child}) eq 'CODE'  and do { $o{child}->(); exit; };
    ref($o{child}) eq 'ARRAY' and exec { $o{child}[0] } @{$o{child}};
    defined($o{child}) and exec $o{child};
    croak "FORK: Unable to perform action as child";
  }

  # print STDERR "HELLO\n";
  return $res unless defined($o{parent});
  ref($o{parent}) eq 'CODE'  and do { @_ = (); goto &{$o{parent}}; };
  ref($o{parent}) eq 'ARRAY' and exec { $o{parent}[0] } @{$o{parent}};
  $o{parent} eq 'exit' and exit;
  exec $o{parent};
}
#END: FORK

=head3 gzdo

Works just like the builtin C<do> command, but reads the file using the
PerlIO gzip layer. Just like the builtin command, this function will search
C<@INC> and update C<%INC>. However, in addition this function will also
attempt to append a ".gz" extension and will read that file if it exists
(or exists in C<@INC>). The following package variables modify the behavior
of this subroutine:

=over 4

=item $_Util::gzdo::gzip_layer_options

Defaults to "(autopop)", this string is appended to the open MODE. Set to
the empty string to disable automatic file type checking.

=item $_Util::gzdo::gz_extension

Defaults to ".gz", setting this string affects the gzip extension that will
be appended to the file name if necessary. Set to a false value to disable.

=back

=cut

#BEGIN: gzdo
$_Util::gzdo::gzip_layer_options = "(autopop)";
$_Util::gzdo::gz_extension = ".gz";
sub gzdo {
  my $f = shift;
  require PerlIO::gzip;
  require File::Spec;
  my ($F, $loc);
  if    (exists $INC{$f} and defined $INC{$f})                                { $loc = $INC{$f} }
  elsif (-e $f)                                                               { $loc = $f }
  elsif ($_Util::gzdo::gz_extension and -e ($f . $_Util::gzdo::gz_extension)) { $loc = $f . $_Util::gzdo::gz_extension }
  elsif ($f =~ m|^/|)                                                         { $loc = $f }
  else {
    for (@INC) {
      $loc = File::Spec->catfile($_, $f);
      last if -e $loc;
      ($loc = $loc.$_Util::gzdo::gz_extension) and last if $_Util::gzdo::gz_extension and -e $loc.$_Util::gzdo::gz_extension;
    }
  }

  $INC{$f} = $loc;
  open $F, "<:gzip$_Util::gzdo::gzip_layer_options", $loc or croak "Error reading '$loc': $!";
  my $data = join "", <$F>;
  close $F;
  my $caller = caller;
  eval "package $caller;\n".$data;
}
#END: gzdo


=head3 hpush(\%@)

 hpush %hash, key1 => $value, key2 => $value2, ...

Add pairs to an existing hash. Keys already existing in %hash are
overwritten.

=cut

#BEGIN: hpush
sub hpush :prototype(\%@) {
  my ($h,$k,$v) = (shift);
  $$h{$k} = $v while ($k, $v) = splice(@_, 0, 2);
}
#END: hpush


=head3 hdefaults(\%@)

 hdefaults %hash, key1 => $value, key2 => $value2, ...

Add pairs to an existing hash. Keys already B<defined> in %hash are
preserved.

=cut

#BEGIN: hdefaults
sub hdefaults :prototype(\%@) {
  my ($h,$k,$v) = (shift);
  $$h{$k} //= $v while ($k, $v) = splice(@_, 0, 2);
}
#END: hdefaults


=head3 subhash

 my %shallow = subhash( \%orig, @keys );

Extract keys from a hash. Similar to:

 @shallow{@keys} = @orig{@keys};

But does not auto-vivify when key does not B<exist> in %orig (and does not
create key in %shallow).

=cut

#BEGIN: subhash
sub subhash {
  my $h = shift;
  map { exists($$h{$_}) ? ($_, $$h{$_}) : () } @_;
}
#END: subhash


=head3 parse_date

 my $dt  = parse_date( $string, %opt );
 my $dt2 = parse_date( $dt1, %opt );

Parses a date and then converts it to a L<DateTime|DateTime> object.

If the input is already a DateTime object, it will be CLONED and returned.

Some date formats specifically guaranteed by this function:

 2006:08:28 20:56:25     # Stored in exif date fields by my camera

=over 4

=item floating

If true, time zone information will not be included the DateTime object.

=item clone

Defaults to true. When true, DateTime objects passed to this function will
be cloned before being returned.

=back

=cut

### Almost passes:
# use DateTime::Format::Natural;
# my $parser = DateTime::Format::Natural->new(
#     format        => 'mm/dd/yy',
#     time_zone     => 'local',
# );
# sub parse_date { scalar $parser->parse_datetime( shift ) }

#BEGIN: parse_date
sub parse_date {
  require Date::Manip;
  require DateTime;
  local $_ = shift;
  my %opt = ( clone => 1, @_ );
  return unless defined;
  return $opt{clone} ? $_->clone : $_ if eval { ref($_) and $_->DOES("DateTime") };

  # Work around oddity in parsing of "Aug 2012":
  $_ = "1 $_" if /^\s*(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\w*\s+\d{4}\s*$/;

  # Work around odd date format in my camera
  s|^(\d{4}):(\d\d):(\d\d\s+\d{1,2}:\d\d:\d\d)$|$1-$2-$3|;

  my $dm = Date::Manip::ParseDate($_);
  return unless $dm;
  my @fmt = qw/year %Y month %m day %d hour %H minute %M second %S/;
  push @fmt, qw/ time_zone %z / unless $opt{floating};
  DateTime->new(Date::Manip::UnixDate($dm, @fmt));
}
#END: parse_date


=head3 map_pairs(&@)

See also: List::Util pairmap

Applies a function passing two elements from the array at a time. That is,
given a function C<&f> and a list of inputs, C<x1>, C<x2>, ..., this
function returns the list C<( f(x1, x2), f(x3, x4), ... )>.

The function may be a code block and may take either two arguments or use
C<$a> and C<$b> as in perl's C<sort> function. If there are an odd number
of elements in the list the last iteration will be called with undef as the
second parameter (C<$b>).

Example:

 @z = map_pairs { "$a: $b\n" } %hash;

Note: The return list will not be constructed if this function is called in
void context. Therefore you are not a bad person if you do the following:

 map_pairs { print "$a: $b\n" } %hash;

=cut

#BEGIN: map_pairs
#-----------------------------------------------------------------
sub map_pairs :prototype(&@) {
  my $f = shift;
  my @res;

  # Ah, glorious Perl!
  no strict 'refs';
  no warnings 'once';
  my $caller = caller;
  local(*{$caller."::a"}) = \my $a;
  local(*{$caller."::b"}) = \my $b;

  if (defined(wantarray)) {
    push @res, $f->($a,$b) while ($a, $b) = splice @_, 0, 2;
    return @res;
  } else {
    $f->($a,$b)            while ($a, $b) = splice @_, 0, 2;
  }
}
#END: map_pairs


=head3 map_pair(&&@)

Applies a pair of functions on a flat list of tuples. Given two functions,
C<\&f> and C<\&g>, and a list of inputs, C<x1>, C<x2>, ..., this function
returns the list C<( f(x1), g(x2), f(x3), g(x4), ... )>.

The first argument may be a code block, either of the first two arguments
may take a single argument or use C<$_>. Some Examples:

 sub f { shift()  + 2 }
 sub g { shift() ** 2 }

 @y = map_pair \&f, \&g, 1..20;     # as expected

 # NOT ALLOWED, second argument may not be code block
 @z = map_pair { $_ + 2 } { $_ ** 3 } 1..20

 # Use this instead (note comma after second argument)
 @z = map_pair { $_ + 2 } sub { $_ ** 3 }, 1..20

=cut

#BEGIN: map_pair
#-----------------------------------------------------------------
# map_pair { f } { g }, @x --> f(x1), g(x2), f(x3), ...
sub map_pair :prototype(&&@) {
  my ($f, $g, @x) = @_; my @res; local $_; my $odd = @x % 2;
  while (my ($x, $y) = splice @x, 0, 2) {
    $_ = $x; push @res, $f->($_);
    $_ = $y; push @res, $g->($_) unless $odd and !@x;
  }
  return @res;
}
#END: map_pair

=head3 join_pair

 join_pair $a, $b, @x

Does an "alternating join". Returns the string "$x[0]$a$x[1]$b$x[2]$a...".

=cut

#BEGIN: join_pair
#-----------------------------------------------------------------
# join_pair $a, $b, @x --> x1 $a x2 $b x3 $a ... xn
sub join_pair {
  my ($i, $j) = splice @_, 0, 2;
  join $j, map join($i, splice(@_, 0, 2)), 0..($#_/2);
}
#END: join_pair


=head3 join_multi

 join_multi \@a, @x

Does an "alternating join". Returns the string
"$x[0]$a[0]$x[1]$a[1]$x[2]...". The list C<@a> may be cycled through multiple
times if C<@a E<lt> @x>.

=cut

#BEGIN: join_multi
#-----------------------------------------------------------------
# join_multi [$a, $b, ...], @x --> x1 $a x2 $b x3 $c ... xn
sub join_multi {
  my $f = shift;
  return '' unless @_;
  my ($i,$n,$res,$last)  = (0, 0+@$f, '', pop @_);
  $res .= $_ . $$f[$i++ % $n] for @_;
  return $res . $last;
}
#END: join_multi


=head3 deep_eq

Test if two complex (possibly circular) data structures are equal.

Solution based on code by Roy Johnson
(http://www.perlmonks.org/?node_id=304250). Modified to match my indenting
style and to fix some bugs in the original. I have also made it safe to use
on blessed and circular objects.

See also: Test::Deep

=cut

#BEGIN: deep_eq
#----- Copied from PerlMonks Q&A section
# How do I test the deep equality of two hash tables?
# Solution by: Roy Johnson (http://www.perlmonks.org/?node_id=304250)
#    Slightly modified to match my indenting style and to fix some bugs in
#    the original. Also made it safe to use on blessed and circular objects.
sub deep_eq {
    my ($A, $B, $seen) = @_;
    $seen ||= {};
    require UNIVERSAL;
    require Scalar::Util;
    my $rA = Scalar::Util::refaddr($A);
    my $rB = Scalar::Util::refaddr($B);

    if    (not defined $A)                { return not defined $B }
    elsif (not defined $B)                { return 0 } # both are defined
    elsif (not ref $A)                    { return ((not ref $B) and ($A eq $B)) }
    elsif (ref $A ne ref $B)              { return 0 } # and are of same class
    elsif (UNIVERSAL::isa($A,'SCALAR'))   { return (UNIVERSAL::isa($B,'SCALAR') and $$A eq $$B) }
    elsif (exists $$seen{$rA}
       and exists $$seen{$rA}{$rB})       { return 1 }
    elsif (UNIVERSAL::isa($A,'ARRAY'))    {
      if (UNIVERSAL::isa($B,'ARRAY') and @$A == @$B) {
        $$seen{$rA}{$rB} = 1;
        for (0..$#$A) {
          my $rval;
          return $rval unless ($rval = deep_eq($A->[$_], $B->[$_], $seen));
        }
        return 1;
      }
      else { return 0 }
    }
    elsif (UNIVERSAL::isa($A,'HASH'))     {
      if (UNIVERSAL::isa($B,'HASH') and keys %$A == keys %$B) {
        $$seen{$rA}{$rB} = 1;
        for (keys %$A) {
          my $rval;
          return $rval unless ($rval = ((exists $B->{$_}) and deep_eq($A->{$_}, $B->{$_}, $seen)));
        }
        return 1;
      }
      else { return 0 }
    }

    else { warn 'Cannot test '.(ref $A)."\n"; undef }
}
#END: deep_eq


=head3 deep_diff

Print differences between two complex (possibly circular) data structures.

=cut

#BEGIN: deep_diff
sub deep_diff {
    my ($A, $B, $seen, $path, @diffs) = @_;
    $path //= '';
    $seen ||= {};
    require UNIVERSAL;
    require Scalar::Util;
    my $rA = Scalar::Util::refaddr($A);
    my $rB = Scalar::Util::refaddr($B);

    if    (not defined $A)                { return "$path: not defined in B" }
    elsif (not defined $B)                { return "$path: not defined in A" }
    elsif (not ref $A)                    {
        return if (not ref $B) and ($A eq $B);
        return "$path: A is scalar but B is reference of type " . ref($B) if ref $B;
        return "$path: A and B differ";
    }
    elsif (ref $A ne ref $B)              { return "$path: A is @{[ ref($B) ]} reference but B is @{[ ref($B) ]} reference" }
    elsif (UNIVERSAL::isa($A,'SCALAR'))   {
        return if UNIVERSAL::isa($B,'SCALAR') and $$A eq $$B;
        return "$path: B not UNIVERSAL::isa 'SCALAR' (is this even possible?)" unless UNIVERSAL::isa($B,'SCALAR');
        return "$path: A and B differ";
    }
    elsif (exists $$seen{$rA}
       and exists $$seen{$rA}{$rB})       { return }
    elsif (UNIVERSAL::isa($A,'ARRAY'))    {
        if (UNIVERSAL::isa($B,'ARRAY') and @$A == @$B) {
            $$seen{$rA}{$rB} = 1;
            my @diff;
            push @diff, deep_diff($A->[$_], $B->[$_], $seen, "$path/$_") for 0..$#$A;
            return @diff;
        }
        else {
            return "$path: B not UNIVERSAL::isa 'ARRAY' (is this even possible?)" unless UNIVERSAL::isa($B,'ARRAY');
            return "$path: A and B are arrays of differing length";
        }
    }
    elsif (UNIVERSAL::isa($A,'HASH'))     {
        if (UNIVERSAL::isa($B,'HASH')) {
            $$seen{$rA}{$rB} = 1;
            my @diff;
            for (keys %$A) {
                if (exists $B->{$_}) {
                    push @diff, deep_diff($A->{$_}, $B->{$_}, $seen, "$path/$_");
                } else {
                    push @diff, "$path/$_: missing in B";
                }
            }
            for (keys %$B) {
                push @diff, "$path/$_: missing in A" if !exists $A->{$_};
            }
            return @diff;
        }
        else {
            return "$path: B not UNIVERSAL::isa 'HASH' (is this even possible?)";
        }
    }

    else { croak 'Cannot test '.(ref $A) }
    die "How did I get here?";# I don't think we get here
}
#END: deep_diff


=head3 SYSTEM

Note: The Dean::Util::safe_pipe() is generally a better choice since that
sub checks the exit status of the command.

Works like the perl L<system|perlfunc/system> command except that any
string expressions passed by reference will not be quoted. This allows, for
example, pipes and redirects while still allowing safe escaping of
arguments.

If the first argument to C<SYSTEM> is a reference to the string "DEBUG"
then the escaped command will be printed to STDERR before being executed.

EXAMPLES:

 # A truly silent mplayer
 SYSTEM "mplayer", "Movie Files/LOTR: trailer 2.mov", \">/dev/null", \"2>/dev/null";

 # multiple commands in one line.
 SYSTEM "echo", ";", \";", "echo", q/$( This is echoed properly )/;

 # See what exactly is happening.
 SYSTEM \"DEBUG", "echo", ";", \";", "echo", q/$( This is echoed properly )/;

=cut

#BEGIN: SYSTEM; depends: qbash
sub SYSTEM {
  my $DEBUG = (@_ and ref $_[0] and ${$_[0]} eq 'DEBUG') ? shift : undef;
  my $cmd = join " ", map( ((ref($_)eq'SCALAR') ? $$_ : qbash($_)), @_ );
  print STDERR $cmd,$/ if $DEBUG;
  system $cmd
}
#END: SYSTEM


=head3 QX

Note: The Dean::Util::safe_pipe() is generally a better choice since that
sub checks the exit status of the command.

Works like a combination of L<SYSTEM|/SYSTEM> and the L<qx|perlop/qx>
operator. It behaves like L<SYSTEM|/SYSTEM> in that it is a subroutine
which takes a list of string expressions that are quoted before being
passed to the shell. Any string expressions passed by reference will not be
quoted. This allows, for example, pipes and redirects while still allowing
safe escaping of arguments. The return value is the C<STDOUT> of the
executed command, just like with the L<qx|perlop/qx> operator.

=cut

#BEGIN: QX; depends: qbash
sub QX {
  my $DEBUG = (@_ and ref $_[0] and ${$_[0]} eq 'DEBUG') ? shift : undef;
  my $cmd = join " ", map( ((ref($_)eq'SCALAR') ? $$_ : qbash($_)), @_ );
  print STDERR $cmd,$/ if $DEBUG;
  qx"$cmd"
}
#END: QX


=head3 EXEC

Works like a combination of L<SYSTEM|/SYSTEM> and L<exec|perlfunc/exec>. It
behaves like L<SYSTEM|/SYSTEM> in that it is a subroutine which takes a
list of string expressions that are quoted before being passed to the
shell. Any string expressions passed by reference will not be quoted. This
allows, for example, pipes and redirects while still allowing safe escaping
of file names. This function never returns, just like with the
L<exec|perlfunc/exec> function.

=cut

#BEGIN: EXEC; depends: qbash
sub EXEC {
  my $DEBUG = (@_ and ref $_[0] and ${$_[0]} eq 'DEBUG') ? shift : undef;
  my $cmd = join " ", map( ((ref($_)eq'SCALAR') ? $$_ : qbash($_)), @_ );
  print STDERR $cmd,$/ if $DEBUG;
  exec $cmd
}
#END: EXEC


=head3 SELECT

Works like perl's select function, but is instead given a string which is
opened as a file and then selected. The special string '-' will not change
the default output stream. An undefined or empty string will select
"/dev/null" instead.

If a string reference C<$mode> is provided as a first argument it will be
taken as the file mode (the default is "E<gt>").

=cut

#BEGIN: SELECT
sub SELECT {
  my $mode = (ref($_[0]) eq 'SCALAR') ? ${shift()} : ">";
  my $f = shift;
  if (defined $f and $f ne '' and $f ne '-') {
    my $F;
    open $F, $mode, $f or croak "Can't open file $f for output: $!";
    return select $F;
  }

  if (!defined $f or $f eq '') {
    my $F;
    open $F, $mode, "/dev/null" or croak "Can't open /dev/null for output: $!";
    return select $F;
  }

  # $f defined and $f eq '-'
  return select;
}
#END: SELECT


=head3 EXISTS

 EXISTS $hash_ref, qw| key1 arbitrarily/deep/key |;
 EXISTS $hash_ref, @paths, { sep => $separator };

Safely test for deep key existence. Recursion happens by splitting on
C<$separator> ("/" by default), there is no means for escaping. Returns
true only if all keys exist. Array refs are allowed if corresponding path
components are numeric.

=cut

#BEGIN: EXISTS
sub EXISTS {
  my $x = shift;
  return 0 unless ref($x);
  my $o = {};
  $o = pop if @_ and 'HASH' eq ref($_[-1]);
  $$o{sep} = '/' unless exists $$o{sep};
  for (@_) {
    my @x = ('ARRAY' eq ref) ? @$_ : defined($$o{sep}) ? split($$o{sep}, $_) : ($_);
    if (ref($x) eq 'ARRAY') {
      $#{$x} >= $x[0] or return 0;
      EXISTS($$x[$x[0]], [@x[1..$#x]], $o) or return 0 if @x > 1;
    } else {
      exists $$x{$x[0]} or return 0;
      EXISTS($$x{$x[0]}, [@x[1..$#x]], $o) or return 0 if @x > 1;
    }
  }
  return 1;
}
#END: EXISTS

=head3 HAS

 HAS $hash_ref, qw| key1 arbitrarily/deep/key |;
 HAS $hash_ref, @paths, { sep => $separator };

Safely test for deep key definedness. Recursion happens by splitting on
C<$separator> ("/" by default), there is no means for escaping. Returns
true only if all keys exist and are defined. Array refs are allowed if
corresponding path components are numeric.

=cut

#BEGIN: HAS
sub HAS {
  my $x = shift;
  return 0 unless ref($x);
  my $o = {};
  $o = pop if @_ and 'HASH' eq ref($_[-1]);
  $$o{sep} = '/' unless exists $$o{sep};
  for (@_) {
    my @x = ('ARRAY' eq ref) ? @$_ : defined($$o{sep}) ? split($$o{sep}, $_) : ($_);
    if (ref($x) eq 'ARRAY') {
      ($#{$x} >= $x[0] and defined $$x[$x[0]]) or return 0;
      HAS($$x[$x[0]], [@x[1..$#x]], $o) or return 0 if @x > 1;
    } else {
      (exists $$x{$x[0]} and defined $$x{$x[0]}) or return 0;
      HAS($$x{$x[0]}, [@x[1..$#x]], $o) or return 0 if @x > 1;
    }
  }
  return 1;
}
#END: HAS

=head3 TRUE

 TRUE $hash_ref, qw| key1 arbitrarily/deep/key |;
 TRUE $hash_ref, @paths, { sep => $separator, false_pat => $pattern };

Safely test for deep key truth. Recursion happens by splitting on
C<$separator> ("/" by default, set C<$separator> to C<undef> to disable
this behavior), there is no means for escaping. Returns true only if all
keys exist and are true. Values matched by C<$pattern> (C<^(?i:false)$> by
default) as well as an empty list or empty hash will all cause 0 to be
returned. Array refs are allowed if corresponding path components are
numeric.

=cut

#BEGIN: TRUE
sub TRUE {
  my $x = shift;
  return 0 unless ref($x);
  my $o = {};
  $o = pop if @_ and 'HASH' eq ref($_[-1]);
  $$o{sep} = '/' unless exists $$o{sep};
  $$o{false_pat} = '^(?i:false)$' unless exists $$o{false_pat} and defined $$o{false_pat};
  for (@_) {
    my @x = ('ARRAY' eq ref) ? @$_ : defined($$o{sep}) ? split($$o{sep}, $_) : ($_);
    if (ref($x) eq 'ARRAY') {
      ($#{$x} >= $x[0] and $$x[$x[0]]) or return 0;
      return 0 if !ref($$x[$x[0]]) and $$x[$x[0]] =~ /$$o{false_pat}/;
      @{$$x[$x[0]]} or return 0 if ref($$x[$x[0]]) eq 'ARRAY';
      %{$$x[$x[0]]} or return 0 if ref($$x[$x[0]]) eq 'HASH';
      TRUE($$x[$x[0]], [@x[1..$#x]], $o) or return 0 if @x > 1;
    } else {
      (exists $$x{$x[0]} and $$x{$x[0]}) or return 0;
      return 0 if !ref($$x{$x[0]}) and $$x{$x[0]} =~ /$$o{false_pat}/;
      @{$$x{$x[0]}} or return 0 if ref($$x{$x[0]}) eq 'ARRAY';
      %{$$x{$x[0]}} or return 0 if ref($$x{$x[0]}) eq 'HASH';
      TRUE($$x{$x[0]}, [@x[1..$#x]], $o) or return 0 if @x > 1;
    }
  }
  return 1;
}
#END: TRUE

=head3 GETPATH

 GETPATH $hash_ref, 'deeply/nested/value'
 GETPATH $hash_ref, $path, { sep => $separator }

Fetch values in recursive hashes using the handy path notation. Keys are
separated by C<$separator> which defaults to "/". There is no way to escape
C<$separator>s in a key, so choose your separator carefully. Array refs
are allowed if corresponding path components are numeric. This function
does not perform autovivification at any level. Non-existent keys at any
level of the requested path immediately returns.

=cut

#BEGIN: GETPATH
sub GETPATH {
  my ($A, $path, $o) = @_;
  $$o{sep} = '/' unless exists $$o{sep};
  my @path = ('ARRAY' eq ref($path)) ? @$path : defined($$o{sep}) ? split($$o{sep}, $path) : ($path);
  return unless @path;
  return unless +(ref($A) eq 'ARRAY') ? ($#{$A} >= $path[0]) : exists($$A{$path[0]});
  if (@path == 1) { return +(ref($A) eq 'ARRAY') ? $$A[$path[0]] : $$A{$path[0]} }
  else {
    return GETPATH(((ref($A) eq 'ARRAY') ? $$A[$path[0]] : $$A{$path[0]}), [@path[1..$#path]], $o);
  }
}
#END: GETPATH

=head3 SETPATH

 SETPATH $hash_ref, 'deeply/nested/value', $value
 SETPATH $hash_ref, $path, $value, { sep => $separator }

Set values in recursive hashes using the handy path notation. Keys are
separated by C<$separator> which defaults to "/". There is no way to escape
C<$separator>s in a key, so choose your separator carefully. Nested hashes
are created explicitly so that this subroutine is safe to use on tied
hashes (see CAVEATS / ISSUES / BUGS in L<DBM::Deep|DBM::Deep>). Array refs
are allowed if corresponding path components are numeric. HOWEVER! Any path
components which do not already exist will be created as hashes regardless
of the value of the keys.

=cut

#BEGIN: SETPATH
sub SETPATH {
  my ($A, $path, $v, $o) = @_;
  $$o{sep} = '/' unless exists $$o{sep};
  my @path = ('ARRAY' eq ref($path)) ? @$path : defined($$o{sep}) ? split($$o{sep}, $path) : ($path);
  if (@path == 1) { (ref($A) eq 'ARRAY') ? ($$A[$path[0]] = $v) : ($$A{$path[0]} = $v) }
  else {
    $$A{$path[0]} ||= { };
    SETPATH(((ref($A) eq 'ARRAY') ? $$A[$path[0]] : $$A{$path[0]}), [@path[1..$#path]], $v, $o);
  }
}
#END: SETPATH


=head3 unique_id

Returns a unique identifier. This not as "unique" as Sys::UniqueID, but
uses only core modules.

Code copied from Sys::UniqueID except that Socket is used to get the IP
address rather than Sys::HostIP. This works fine as long as hostname
returns a good host name that gethostbyname(3) can resolve to a
sufficiently distinct IP address (distinct among all machines generating
unique ids). A typical pool of servers on the same network should have no
problems. However, ids generated by code running on a client may have
issues.

When the counter wraps, this function may need to sleep before proceeding
to ensure a unique timestamp. The Time::HiRes module is used for this (core
module in 5.8.0). The amount of time slept before polling the new time can
be adjusted using the C<$_Util::unique_id::sleep> package variable. The
default sleep time is 0.1 seconds.

=cut

#BEGIN: unique_id
{ my ($host, $addr, $idnum, $last_time);
  $_Util::unique_id::sleep = .1;
  sub unique_id {
    unless ($host) {
      require Time::HiRes;
      require Socket;
      require Sys::Hostname;
      $host = Sys::Hostname::hostname();
      $addr = Socket::inet_ntoa(scalar gethostbyname($host || 'localhost'));
      $addr = sprintf '%02X%02X%02X%02X', (split /\./, $addr || '127.0.0.1');
      $idnum = 0;
    }

    # absolutely ensure that id is unique: < 0x10000/second
    my $time = time;
    unless (++$idnum < 0x10000) {
      # Don't sleep unless necessary
      while ($last_time == $time) {
        Time::HiRes::sleep($_Util::unique_id::sleep);
        $time = time
      }
      $idnum = 0;
    }
    return sprintf '%012X.%s.%08X.%04X', ($last_time = $time), $addr, $$, $idnum;
  }
}
#END: unique_id


=head3 SPRINTF

 SPRINTF $o, $fmt, $h1, $h2, ...

Format the data in the hashes $h1, $h2, ... into the format string
$fmt given in the language specified in the option hash $o.

Example:

 $o = { a => [ s => "artist" ],
        t => [ s => "title", "name" ],
        N => sub { scalar localtime },
      };
 @songs = ( { artist => "Arlo Guthrie", title => "The Motorcycle Song" },
            { artist => "Cypress Hill", name  => "Psycobetabuckdown" },
          );

 @formatted_songs = SPRINTF $o, "%20t - %a", @songs;

See also: String::Formatter

=cut

#BEGIN: SPRINTF
my $_SPRINTF = sub {
  my ($h,$data,$param,$key) = @_;
  my ($path) = grep exists($$h{$_}), "$param$key", $key;
  unless (defined $path) {
    # Some reasonable fallbacks
    return "%"                  if $key eq '%';
    return $$data{"$param$key"} if exists $$data{"$param$key"};
    return $$data{$key}         if exists $$data{$key};
    die;
  }

  return $$h{$path}                      if !ref($$h{$path});
  return $$h{$path}->($data,$param,$key) if 'CODE' eq ref($$h{$path});

  if ('ARRAY' eq ref($$h{$path})) {
    if ('CODE' eq ref($$h{$path}[0])) {
      for (@{$$h{$path}}[1..$#{$$h{$path}}]) {
        return $$h{$path}[0]->($$data{$_}, $param, $key) if exists $$data{$_};
      }
      return;
    }

    if (1 == length($$h{$path}[0])) {
      for (@{$$h{$path}}[1..$#{$$h{$path}}]) {
        return sprintf "%${param}$$h{$path}[0]", $$data{$_} if exists $$data{$_};
      }
      return;
    } elsif (1 == $$h{$path}[0] =~ tr/%/%/) {
      for (@{$$h{$path}}[1..$#{$$h{$path}}]) {
        return sprintf $$h{$path}[0], $$data{$_} if exists $$data{$_};
      }
      return;
    } else {
      return sprintf $$h{$path}[0],
        map +(exists($$data{$_}) ? $$data{$_} : undef), @{$$h{$path}}[1..$#{$$h{$path}}];
    }
  }

  croak "Unrecognized _SPRINTF format";
};

sub SPRINTF {
  my $o   = shift;
  my $fmt = shift;
  my @res;

  for my $h (@_) {
    my $str = $fmt;
    die unless eval { $str =~ s/%([^%a-zA-Z]*)([%a-zA-Z])/&$_SPRINTF($o,$h,$1,$2)/ge; 1 };
    push @res, $str;
  }
  if (1 == @_ and !wantarray) { return $res[0] }
  else { return @res }
}
#END: SPRINTF



#-----------------------------------------------------------------
                        $EXPORT_TAGS{perl6}
                                 =
 [qw/zip uniq smartmatch/];
#-----------------------------------------------------------------

=head2 :perl6 - Perl 6 functions

=head3 smartmatch

Perl 5.010 pretty much killed the need for this...

 smartmatch( $X, $Y );

smartmatches $X ~~ $Y. Inspired by the Perl6 operator, but a complete
deviation since this is not designed to be the deciding form of a switch
statement. Primarily tests for thing which are annoying to type out.

Returns 1 or '' as long a CODE is not one of the match variables. Returns
undef if comparison is not possible.

Matches are commutative unless explicitly presented as otherwise

 str|num ~~   str|num      natural equality test, though see convert_string_to_regexp option
 str|num ~~   Regexp       natural pattern match

 ARRAY   ~~   Regexp       all(ARRAY) =~ Regexp
 HASH    ~~   Regexp       all(keys(HASH)) =~ Regexp

 undef   ~~   ARRAY        undef \in ARRAY
 num|str ~~   ARRAY        str \in @ARRAY

 HASH    ~~   num          keys(%HASH) == num
 ARRAY   ~~   num          @ARRAY == num

 ARRAY   ~~   ARRAY        @ARRAY <<~~>> @ARRAY  # test elements individually
 HASH    ~~   ARRAY        exists(@HASH{@ARRAY})
 HASH    ~~   HASH         have same keys

 CODE    ~~   Any          CODE( Any )
 ARRAY   ~~   CODE         CODE( all(ARRAY) )


 Any     ~~   CODE         reserved
 ARRAY   ~~   undef        reserved
 ARRAY   ~~   str          reserved

=over 4

=item convert_string_to_regexp: 'left' | 'right' | Bool

Strings of the form: qr(...), qr/.../, qr(\W)...(\1), ... are automatically
upgraded to regular expressions.

=back

=cut

#BEGIN: smartmatch
{ my $false = sub { 0 };
  my %match =
( "str,"."str"    => sub { $_[0] eq $_[1] },
  'num,num'       => sub { $_[0] == $_[1] },
  "num,"."str"    => $false,# Can't be string equal!
  ","."str"       => $false,
  'num,'          => $false,
  ',Regexp'       => $false,

  "str".',Regexp' => sub { $_[0] =~ $_[1] },
  'num,Regexp'    => sub { $_[0] =~ $_[1] },

  'HASH,Regexp'   => sub { for (keys %{$_[0]}) { return '' unless $_ =~ $_[1] } return 1 },
  'ARRAY,Regexp'  => sub { for (@{$_[0]})      { return '' unless defined() and $_ =~ $_[1] } return 1 },

  'num,ARRAY'     => sub { for (@{$_[1]}) { return 1 if smartmatch($_[0],$_,@_[2..$#_])  } return '' },
  "str".',ARRAY'  => sub { for (@{$_[1]}) { return 1 if smartmatch($_[0],$_,@_[2..$#_])  } return '' },
  ',ARRAY'        => sub { for (@{$_[1]}) { return 1 if !defined($_) } return '' },

  'HASH,num'      => sub { $_[1] == (keys %{$_[0]}) },
  'ARRAY,num'     => sub { $_[1] == @{$_[0]}        },

  'ARRAY,ARRAY'   => sub { return '' unless @{$_[0]}==@{$_[1]}; for (0..$#{$_[0]}) { return '' unless smartmatch($_[0][$_],$_[1][$_],@_[2..$#_]) } return 1 },
  'HASH,ARRAY'    => sub { return '' unless keys(%{$_[0]}) == @{$_[1]};       for (@{$_[1]})      { return '' unless exists $_[0]{$_} } return 1 },
  'HASH,HASH'     => sub { return '' unless keys(%{$_[0]}) == keys(%{$_[1]}); for (keys %{$_[0]}) { return '' unless exists $_[1]{$_} } return 1 },

  'ARRAY,CODE'    => sub { for (@{$_[0]}) { return '' unless $_[1]->($_) } return 1 },
);
sub smartmatch {
  my ($x, $y) = @_;
  require Scalar::Util;
  my %o = @_[2..$#_];

  ## pre-emptive checks and recursion check
  my $addry = Scalar::Util::refaddr($y); my $addrx = Scalar::Util::refaddr($x);
  if (defined $addry and defined $addrx) {
    return 1 if $addry == $addrx;
    ## return false if we repeat a comparison
    croak "deep smartmetch" if $addry > $addrx ? $o{seen}{$addry}{$addrx}++ : $o{seen}{$addrx}{$addry}++;
  }

  my ($rx, $ry) = (ref($x), ref($y));
  if ($o{convert_string_to_regexp}) {
    if ($o{convert_string_to_regexp} ne 'left' and ($ry eq '') and defined($y)) {
      $y = qr/$2/ if $y =~ m#^qr([\(\{\[\<]).*[\)\}\]\>]$# or $y =~ m#^qr(\W)(.*)\1$#
    } elsif ($o{convert_string_to_regexp} ne 'right' and ($rx eq '') and defined($x)) {
      $x = qr/$2/ if $x =~ m#^qr([\(\{\[\<]).*[\)\}\]\>]$# or $x =~ m#^qr(\W)(.*)\1$#
    }
    ($rx, $ry) = (ref($x), ref($y));
  }
  $rx = Scalar::Util::looks_like_number($x) ? 'num' : "str" if defined($x) and !$rx;
  $ry = Scalar::Util::looks_like_number($y) ? 'num' : "str" if defined($y) and !$ry;

  if ('CODE' eq $rx) { return scalar $x->($y) }
  if (exists($match{$rx.",".$ry})) { return $match{$rx.",".$ry}->($x,$y, %o) }
  if (exists($match{$ry.",".$rx})) { return $match{$ry.",".$rx}->($y,$x, %o) }

  return;
}
}
#END: smartmatch


=head3 zip

See also: List::MoreUtils zip

 zip \@x, \@y, ...

The Perl6 zip function (almost). Given a list of arrays, returns a list of
the array elements "zipped" together, that is: C<$x[0], $y[0], ..., $x[1],
$y[1], ...>. The lists need not be the same length the short lists will
simply be ignored after they run out.

=cut

#BEGIN: zip, depends: fmax
sub zip {
  my ($i, @x) = (0);
  my $l = fmax(sub{@$_-1}, @_);
  while ($i <= $l) { push @x, map( (@$_ > $i) ? ($$_[$i]) : (), @_ ); $i++ }
  return @x;
}
#END: zip


=head3 uniq

Takes a list (or reference to an array) and discards all but one of
successive identical objects (up to stringification) from the list. In
scalar context, an array reference is returned.

Note: This is different from the L<unique|/unique> function which will
remove all duplicates from the list.

=cut

#BEGIN: uniq
sub uniq {
  my @out;
  if (@_ == 1 and ref($_[0]) eq 'ARRAY') {
    for (@{$_[0]}) { push @out, $_ if( !@out or (defined&&!defined$out[-1]) or (defined$out[-1]&&!defined) or (defined and ($_ ne $out[-1]))) }
  } else {
    for (@_)       { push @out, $_ if( !@out or (defined&&!defined$out[-1]) or (defined$out[-1]&&!defined) or (defined and ($_ ne $out[-1]))) }
  }
  wantarray ? @out : \@out;
}
#END: uniq



#-----------------------------------------------------------------
                        $EXPORT_TAGS{security}
                                 =
[qw/nonce
   /];
#-----------------------------------------------------------------

=head2 :security - Security

=head3 nonce

=cut

#BEGIN: nonce
sub nonce {
    state $alphabet = [ 'a'..'z', 'A'..'Z', '0'..'9' ];
    my $n = shift // 32;
    join "", map @$alphabet[rand @$alphabet], 1..$n;
}
#END: nonce



#-----------------------------------------------------------------
#                Finalize the exportable functions
#-----------------------------------------------------------------
@EXPORT_OK = map @$_, values %EXPORT_TAGS;
$EXPORT_TAGS{all} = \@EXPORT_OK;

1;

__END__

=pod

=head1 TODO

=over 4

=item range2list and list2range

convert #..#,#-#,a..z,a-z,2:23,2:5:23 strings to lists and back. split /,/
first.

A more general form "suggested" on PerlMonks (http://www.perlmonks.org/?node_id=427615):

 foo[01:100]bar-[fred,barney,wilma]

Though, shell syntax might be better (see bash(1) /EXPANSION):

 foo{001..100}bar-{fred,barney,wilma{1,2}}

=item Adaptive Simpson's rule

 sub f { sqrt($_[0]) }
 print adaptive( \&f, 0, 1, 0.0005 ), $/;

 sub adaptive {
  my ($f, $a, $b, $eps) = @_;
  my $s1 = simp($f, $a, $b);
  my $s2 = simp2($f, $a, $b);
  my $err = abs($s2-$s1)/15;
  if ($err < $eps) {
    return $s2;
  } else {
    return adaptive($f, $a, ($a+$b)/2, $eps/2) + adaptive($f, ($a+$b)/2, $b, $eps/2);
  }
 }

 sub simp {
  my ($f, $a, $b) = @_;
  return ($b-$a)*($f->($a) + 4*$f->(($a+$b)/2) + $f->($b))/6;
 }

 sub simp2 {
  my ($f, $a, $b) = @_;
  return simp($f,$a,($a+$b)/2) + simp($f,($a+$b)/2,$b);
 }

=back

=head1 BUGS

No known bugs, if you find one, please report it via email.

=head1 AUTHOR

 Dean Serenevy
 dean@serenevy.net

=head1 LICENSE

This software (except where attributed to another author) is hereby placed
into the public domain. If you use this code, a simple comment in the code
giving credit and an email letting me know that you find it useful would be
courteous but is not required.

The software is provided "as is" without warranty of any kind, either
expressed or implied including, but not limited to, the implied warranties
of merchantability and fitness for a particular purpose. In no event shall
the authors or copyright holders be liable for any claim, damages or other
liability, whether in an action of contract, tort or otherwise, arising
from, out of or in connection with the software or the use or other
dealings in the software.

=head1 SEE ALSO

perl(1).

=cut
