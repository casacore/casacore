#!/usr/local/bin/perl4

#
#
#    Command line flags:
#
#       -sd       Show starting date
#       -st       Show starting time
#       -ed       Show ending date
#       -et       Show ending time
#       -f        Show name of log file
#       -l        Show labels for each of the fields
#       -h        Show the host which ran the inhale
#       -u        Show the type of update
#
#
require "timelocal.pl";

%opts = ();
$opts{"DoSneeze"} = 1;

while ($_ = @ARGV[0], /^-/) {
    shift(@ARGV);
    last if /^--$/;
    /^-sd/ && ($opts{"PrintStartDate"} = 1, next);
    /^-ed/ && ($opts{"PrintEndDate"} = 1, next);
    /^-st/ && ($opts{"PrintStartTime"} = 1, next);
    /^-et/ && ($opts{"PrintEndTime"} = 1, next);
    /^-f/ && ($opts{"PrintFile"} = 1, next);
    /^-l/ && ($opts{"PrintLabel"} = 1, next);
    /^-h/ && ($opts{"PrintHost"} = 1, next);
    /^-u/ && ($opts{"PrintUpdate"} = 1, next);
    /^-sneeze/ && ($opts{"DoSneeze"} = !$opts{"DoSneeze"}, next);
    /^-inhale/ && ($opts{"DoInhale"} = !$opts{"DoInhale"}, next);
    print STDERR "Unknown option: $_ (ignoring)\n";
}

foreach $file (@ARGV) {
    @i_start = ();
    @i_end = ();
    $collect_i_start = $collect_s_start = 0;
    $collect_i_end = $collect_s_end = 0;
    $host = $s_host = "";
    $update = $s_update = "";
    open(FILE, "<" .$file) || die "Couldn't open file $file\n";
    while(<FILE>) {
	##
	## Inhale
	##
	if (m@^inhale:\s*Execution\s*on\s*([a-zA-Z0-9]*)\s*by\s*([a-zA-Z0-9]*)\s*commenced\s*at\s*$@) {
	    $host = $1;
	    $collect_i_start = 1;
	}
	if (m@^inhale:\s*Execution\s*terminated\s*at\s*$@) {
	    $collect_i_end = 1;
	}
	##
	## Sneeze
	##
	if (m@^sneeze:\s*Execution\s*on\s*([a-zA-Z0-9]*)\s*by\s*([a-zA-Z0-9]*)\s*commenced\s*at\s*$@) {
	    $s_host = $1;
	    $collect_s_start = 1;
	}
	if (m@^sneeze:\s*Execution\s*terminated\s*at\s*$@) {
	    $collect_s_end = 1;
	}
	#
	# Match on something like:
	#
	#           Slave: 03.104.00 Sun 1994/01/30 00:21:10 GMT  (cumulative)
	#
	#
	if (m@^\s*Slave:\s*[0-9\.]+\s*\w{3,3}\s*\d{4,4}/\d{2,2}/\d{2,2}\s*\d{2,2}:\d{2,2}:\d{2,2}\s*\w{3,3}\s*\((\w+)\)\s*$@) {
	    $mode = $1;
	    if ($mode =~ m/cumulative/) {
		$update = "C";
	    } elsif ($mode =~ m/incremental/) {
		$update = "I";
	    } elsif ($mode =~ m/unchanged/) {
		$update = "U";
	    }
	}

	#
	# Parse time stamp of the form:
	#
	#      inhale: Thu 1994/01/27 00:15:04 GMT
	#               $1  $2  $3 $4 $5 $6 $7 $8
	#
	##
	## Inhale
	##
	if (m@^inhale:\s*(\w{3,3})\s*(\d{4,4})/(\d{2,2})/(\d{2,2})\s*(\d{2,2}):(\d{2,2}):(\d{2,2})\s*(\w{3,3})\s*$@) {
	    @match = ($7,$6,$5,$4,$3,$2,$2,$8,$1);
	    $match[5] =~ s/^[0-9]{2,2}//;
	    if ($match[7] =~ m/^[A-Z]{3,3}$/ &&
		$match[7] !~ m/^GMT$/ ) {
		if ($collect_i_start) {
		    @i_start = @match;
		    $collect_i_start = 0;
		} elsif ($collect_i_end) {
		    @i_end = @match;
		    $collect_i_end = 0;
		}
	    }
	}
	##
	## Sneeze
	##
	if (m@^sneeze:\s*(\w{3,3})\s*(\d{4,4})/(\d{2,2})/(\d{2,2})\s*(\d{2,2}):(\d{2,2}):(\d{2,2})\s*(\w{3,3})\s*$@) {
	    @match = ($7,$6,$5,$4,$3,$2,$2,$8,$1);
	    $match[5] =~ s/^[0-9]{2,2}//;
	    if ($match[7] =~ m/^[A-Z]{3,3}$/ &&
		$match[7] !~ m/^GMT$/ ) {
		if ($collect_s_start) {
		    @s_start = @match;
		    $collect_s_start = 0;
		} elsif ($collect_s_end) {
		    @s_end = @match;
		    $collect_s_end = 0;
		}
	    }
	}
    }
    close(FILE);

    if ($opts{"DoInhale"}) {
	if ($#i_start < 0) {
	    print STDERR "*err($file): no inhale start time*\n";
	} elsif ($#i_end < 0) {
	    print STDERR "*wrn($file): no inhale end time*\n";
	} else {
	    print "I> " if $opts{"DoSneeze"};
	    &GenOutput(*i_start,*i_end,*opts,$update,$host);
	    if ($opts{"PrintFile"} && ($opts{"DoSneeze"} || $opts{"DoInhale"})) {
		print "file% " if $opts{"PrintLabel"};
		print "$file\t";
	    }
	    print "\n";
	}
    }

    if ($opts{"DoSneeze"}) {
	if ($#s_start < 0) {
	    print STDERR "*err($file): no sneeze start time*\n";
	} elsif ($#s_end < 0) {
	    print STDERR "*wrn($file): no sneeze end time*\n";
	} else {
	    print "S> " if $opts{"DoInhale"};
	    &GenOutput(*s_start,*s_end,*opts,$update,$host);
	    if ($opts{"PrintFile"} && ($opts{"DoSneeze"} || $opts{"DoInhale"})) {
		print "file% " if $opts{"PrintLabel"};
		print "$file\t";
	    }
	    print "\n";
	}
    }

}

exit 0;

sub GenOutput {
    local(*start) = @_[0];
    local(*end) = @_[1];
    local(*opts) = @_[2];
    local($update) = @_[3];
    local($host) = @_[4];
    local($start) = 0;
    local($end) = 0;
    local($duration) = 0;
    local($dur_hrs) = 0;
    local($dur_min) = 0;
    local($dur_sec) = 0;

    if ($#start < 0 || $#end < 0) {
	return ();
    }
    if ($opts{"PrintStartDate"}) {
	print "sdate% " if $opts{"PrintLabel"};
	print "$start[5]/$start[4]/$start[3] ";
    }
    if ($opts{"PrintStartTime"}) {
	print "stime% " if $opts{"PrintLabel"};
	print "$start[2]:$start[1]:$start[0]$start[7] ";
    }
    if ($opts{"PrintEndDate"}) {
	print "edate% " if $opts{"PrintLabel"};
	print "$end[5]/$end[4]/$end[3] ";
    }
    if ($opts{"PrintEndTime"}) {
	print "etime% " if $opts{"PrintLabel"};
	print "$end[2]:$end[1]:$end[0]$end[7] ";
    }

    if ($opts{"PrintUpdate"} && $update) {
	print "update% " if $opts{"PrintLabel"};
	print "$update ";
    }

    if ($opts{"PrintHost"}) {
	print "host% " if $opts{"PrintLabel"};
	print "$host";
	print " " x (7 - length($host)) if length($host) < 7;
	print "\t";
    }

    $start = &timelocal(@start);
    $end = &timelocal(@end);
    $duration = $end-$start;
    $dur_hrs = int($duration/3600);
    $dur_min = int($duration%3600/60);
    $dur_sec = int($duration%3600%60);
    print "duration% " if $opts{"PrintLabel"};

    $dur_str = "";
    $dur_str .= "0" x (2 - length($dur_hrs)) if length($dur_hrs) < 2;
    $dur_str .= "$dur_hrs:";
    $dur_str .= "0" x (2 - length($dur_min)) if length($dur_min) < 2;
    $dur_str .= "$dur_min:";
    $dur_str .= "0" x (2 - length($dur_sec)) if length($dur_sec) < 2;
    $dur_str .= "$dur_sec";
    print "$dur_str\t";
}
