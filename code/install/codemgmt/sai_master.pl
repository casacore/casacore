#!/usr/bin/suidperl
print "$<: $>\n";
print "$(: $)\n";
$ENV{PATH} = '/usr/bin';
exec 'id' || die "can't exec";
