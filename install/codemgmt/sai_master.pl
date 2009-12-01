#!/usr/local/bin/perl
#
# $Id$

print "$<: $>\n";
#$< = $>;
#print "$<: $>\n";
#print "$(: $)\n";

$AIPSPATH = (split (' ', $ENV{AIPSPATH}))[0];
# Untaint.
if ($AIPSPATH =~ /(.*)/) {
  $AIPSPATH = $1;
}
if ($ENV{PATH} =~ /(.*)/) {
  $PATH = $1;
}
# print "AIPSPATH: $AIPSPATH\n";
$ENV{PATH} = "$PATH";
# print "$ENV{PATH}\n";
$ARGS = join (' ', @ARGV);
if ($ARGS =~ /(.*)/) {
  $ARGS = $1;
}
exec ("$AIPSPATH/master/etc/sai_master.sh $ARGS") ||
  die "can't exec $AIPSPATH/master/etc/sai_master.sh\n";
