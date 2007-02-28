#
#   This is part of cxx2html Version 1.2 Patchlevel 4
#   Created by Darrell Schiebel (drs@nrao.edu)
#
#   This utility, cxx2html, is part of AIPS++, a software project
#   centered at the National Radio Astronomy Observatory.
#
#   Copyright (C) 1995
#   Associated Universities, Inc. Washington DC, USA.
#  
#   This library is free software; you can redistribute it and/or modify it
#   under the terms of the GNU Library General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or (at your
#   option) any later version.
#  
#   This library is distributed in the hope that it will be useful, but WITHOUT
#   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
#   License for more details.
#  
#   You should have received a copy of the GNU Library General Public License
#   along with this library; if not, write to the Free Software Foundation,
#   Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
#  
#   Correspondence concerning AIPS++ should be addressed as follows:
#          Internet email: aips2-request@nrao.edu.
#          Postal address: AIPS++ Project Office
#                          National Radio Astronomy Observatory
#                          520 Edgemont Road
#
package Class;
require 5.000;

$_FileSeed="aaaaaaaa";

sub file {
    "Class::" . $_FileSeed++;
}

sub new {
    my $type = shift;
    my $self = {};
    bless $self;
    $self->{'head'} = 0;
    $self->{'tail'} = 0;
    $self->{'func'} = 0;
    $self->{'enum'} = 0;
    $self->set(@_);
    $self->{'module'} = [];
    $self->{'module start'} = [];
    $self->{'linkfrom'} = [];
    $self->{'linkfrom start'} = [];
    $self->{'linkfrom text'} = '';
    $self->{'summary'} = '';
    $self->{'file ref'} = file();
    $self->{'open'} = 0;
    $self->open();
    $self;
}

sub set {    
    my $self = shift;
    my %params = @_;

    ##### number the lines 	#####
    $self->{'num'} = $params{'num'} if defined $params{'num'};

    ##### strip out white space	#####
    $self->{'strip'} = $params{'strip'} if defined $params{'strip'};

    ##### file to process 	#####
    $self->{'file'} = $params{'file'} if defined $params{'file'};

    ##### return first 	#####
    $self->{'head'} = $params{'head'} if defined $params{'head'};

    ##### return last 	#####
    $self->{'tail'} = $params{'tail'} if defined $params{'tail'};

    ##### return global functions #####
    $self->{'func'} = $params{'func'} if defined $params{'func'};

    ##### return global enum information#####
    $self->{'enum'} = $params{'enum'} if defined $params{'enum'};
}

sub open {
    my $self = shift;

    if ($#_ >= 0) {
	$self->{'file'} = $_[0];
    }

    if ($self->{'file'}) {
	if ($self->{'open'}) {
	    close $self->{'file ref'};
	    $self->{'open'} = 0;
	}
	$self->{'open'} = "< $self->{'file'}";
	if (!open($self->{'file ref'},$self->{'open'})) {
	    $self->{'open'} = 0;
	    return 0;
	}
	$self->{'line num'} = 1;
	
	$self->{'module'} = [];
	$self->{'module start'} = [];
	$self->{'summary'} = '';

	$self->{'linkfrom'} = [];
	$self->{'linkfrom start'} = [];
	$self->{'linkfrom text'} = '';

	if ($self->{'save'}) {
	    $self->{'QUEUE'} = ();
	}
	return 1;
    }
    return 0;
}

sub module {
    my $self = shift;
    my $TMP = $self->{'module'};
    return @$TMP;
}

sub linkfrom {
    my $self = shift;
    my $TMP = $self->{'linkfrom'};
    return @$TMP;
}

sub summary {
    my $self = shift;
    my $TMP = $self->{'summary'};
    return $TMP;
}

sub read {
    my $self = shift;
    my @out = ();
    my @subs = ();
    my @enumout = ();
    my $FOO = $self->{'file ref'};
    my $gotClass = 0;
    my $gettingFuncgrp = 0;
    my $nestedFuncgrp = 0;
    my $gettingClass = 0;
    my $gettingEnum = 0;
    my $name = "";
    my $start = 0;
    my $line = "";
#   my $checkingClass = 0;
    my $braceCount = 0;
    my $startBraceCount = 0;
    my $killingDef = 0;
    my $killingCstyle = 0;
    my $summaryStr = "";
    my $gettingSummary = 0;
    my $gettingLinkfrom = 0;

    $self->{'summary'} = '';
    while(!$gotClass && ($_ = <$FOO>)) {

	##
	## Kill blank lines
	##
	if ( /^\s*$/ ) {
	    ++$self->{'line num'};
	    next;
	}
		    
	##
	## Kill C-style comments
	##
	redo if s@^\s*/\*.*?\*/@@;
	if ( m@^\s*/\*@ ) {
	    $killingCstyle = 1;
	}
	if ( $killingCstyle ) {
	    if ( s@^.*?\*/@@ ) {
		$killingCstyle = 0;
		redo;
	    } else {
		++$self->{'line num'};
		next;
	    }
	}

	##
	## Kill the #define
	##
	if ( m/\\$/ ) {
	    if ($killingDef) {
		++$self->{'line num'};
		next;
	    }
	} else {
	    $killingDef = 0;
	}
	if (m/^\s*\#define/) {
	    $killingDef = 1 if m/\\$/;
	    ++$self->{'line num'};
	    next;
	}

	##
	## Kill #if
	##
	if ( m/^\s*\#(?:if|else|endif|ifdef|ifndef|include)/ ) {
	    $killingDef = 1 if m/\\$/;
	    ++$self->{'line num'};
	    next;
	}

	##
	## Save the line numbers for module documentation
	##
	if ( m|.*?//.*?<\s*module\s*>|i ) {
	    $TMP_MOD = $self->{'module start'};
	    push(@$TMP_MOD,$self->{'line num'});
	}
	if ( m|.*?//.*?<\s*/\s*module\s*>|i ) {
	    $TMP_MOD = $self->{'module start'};
	    $TMP_LINE = pop(@$TMP_MOD);
	    $TMP_MOD = $self->{'module'};
	    push(@$TMP_MOD,$TMP_LINE . ":" . $self->{'line num'});
	}

	##
	## Save the line numbers for linkfrom links
	##
	if ( m|^.*?//.*?<\s*linkfrom\s*(.*?)>(.*)|i ) {
	    $attr = $1;
	    $rest = $2;
	    @attr = split(/(?:=["'][^"']+['"]|=\w+)\s*/,$attr);
	    $attr =~ s/.*?=//;
	    my @val = split(/\s*\w+=/,$attr);
	    my %attr = ();
	    my $attrcnt = 0;
	    while ( ($_ = shift(@attr)) && ($val = shift(@val)) ) {
		if ( /^anchor/ ) {
		    $val =~ s/^["']\s*//;
		    $val =~ s/\s*["']$//;
		    if ( $val ) {
			$attr{$_} = $val;
			$attrcnt++;
		    }
		}
		if ( /^classes$/ || /^modules$/ ) {
		    $val =~ s/^["']\s*//;
		    $val =~ s/\s*["']$//;
		    if ( $val ) {
			my @list = split(/[, ]+/,$val);
			$attr{$_} = \@list;
			$attrcnt++;
		    }
		}
	    }
	    if ( $attr{'anchor'} && $attrcnt > 1 ) {
		if ( $rest =~ m@\s*(.*?)\s*</linkfrom>@ ) {
		    $TMP_REL = $self->{'linkfrom'};
		    push(@$TMP_REL,[$self->{'line num'},
				     $attr{'anchor'},
				     $attr{'classes'},
				     $attr{'modules'},$1]);
		} else {
		    $TMP_REL = $self->{'linkfrom start'};
		    push(@$TMP_REL, [$self->{'line num'},
				     $attr{'anchor'},
				     $attr{'classes'},
				     $attr{'modules'}]);
		    $gettingLinkfrom = 1;
		    $rest =~ s/^\s*//;
		    $rest =~ s/\s*$//;
		    $self->{'linkfrom text'} .= $rest;
		    ++$self->{'line num'};
		    next;
		}
	    }
	}
	if ( $gettingLinkfrom ) {
	    if ( m|^.*?//\s*(.*?)\s*</linkfrom>| ) {
		$self->{'linkfrom text'} .= " $1";
		$gettingLinkfrom = 0;
		$TMP_REL = $self->{'linkfrom start'};
		$TMP_START = pop(@$TMP_REL);
		$self->{'linkfrom text'} =~ s/^\s*//;
		push(@$TMP_START,$self->{'linkfrom text'});
		$TMP_REL = $self->{'linkfrom'};
		push(@$TMP_REL,$TMP_START);
		$self->{'linkfrom text'} = '';
	    } elsif ( m|^.*?//\s*(.*?)\s*$| ) {
		$self->{'linkfrom text'} .= " $1" if $1;
		++$self->{'line num'};
		next;
	    }
	}

	##
	## Also collect global function groups, if requested
	##
	if ( $self->{'func'} && !$gettingClass && !$nestedFuncgrp &&
			( m@^\s*//.*?<group.*?name=["'](.+?)['"].*?>@i ||
			m@^\s*//.*?<group.*?name=(\w+).*?>@i ) ) {
	    $_ = "<group> $1";
	    push(@out,$self->{'num'} ? "$self->{'line num'} " . $_ : $_) unless $self->{'tail'} && !$self->{'head'};
	    ++$self->{'line num'};
	    $gettingFuncgrp++;
	    next;
	}

	if ( $self->{'func'} && !$gettingClass && m@^\s*//.*?<\s*group\s*>@i ) {
	    if ( $gettingFuncgrp ) {
		$gettingFuncgrp++;
	    } else {
		$nestedFuncgrp++;
	    }
	    ++$self->{'line num'};
	    next;
        }

	if ( $self->{'func'} && !$gettingClass && m@^\s*//.*?<\s*/\s*group\s*>@i ) {
	    if ( $gettingFuncgrp ) {
		if ( ! --$gettingFuncgrp ) {
		    $_ = "</group>";
		    push(@out,$self->{'num'} ? "$self->{'line num'} " . $_ : $_) if !$self->{'tail'} && !$self->{'head'} ||
										$self->{'tail'};
		    ++$self->{'line num'};
		    last;
		} else {
		    ++$self->{'line num'};
		    next;
		}
	    } elsif ( $nestedFuncgrp ) {
		--$nestedFuncgrp;
		++$self->{'line num'};
		next;
	    }
	}

	##
	## Pull off the summaries in comments
	##
	if ( ! $gettingSummary && m@.*?//.*?<summary>\s*(.*)$@i ) {
	    $self->{'summary'} = $1;
	    if ( ! ($self->{'summary'} =~ s@^(.*?)\s*</summary>.*$@$1@i) ) {
		$gettingSummary = 1;
	    }
	} else {
	    if ( $gettingSummary && m@^.*?//(.*?)\s*</summary>@ ) {
		$self->{'summary'} .= $1;
		$gettingSummary = 0;
	    }
	    if ( $gettingSummary && m@^.*?//(.*?)$@ ) {
		$self->{'summary'} .= $1;
	    }
	}

	##
	## Kill comments
	##
	s@(.*?)//.*@$1@;

	##
	## Kill blank lines if requested
	##
	next if m/^\s*$/ && $self->{'strip'} && ++$self->{'line num'};


	if ( $gettingFuncgrp ) {
	    push(@out,$self->{'num'} ? "$self->{'line num'} " . $_ : $_) if !$self->{'tail'} && !$self->{'head'};
	    ++$self->{'line num'};
	    next;
	}

	##
	## Also collect global enums, if requested
	##
	if ( $self->{'enum'} && !$gettingFuncgrp && !$nestedFuncgrp && $gettingEnum < 2 ) {
	    if ( !$gettingEnum && ( m@(?:.*?\s+|^)(enum(?:\s+(?![^<]*?>)|$))@ ) ) {
		$gettingEnum++;
		$start = $self->{'line num'};
		$line = $1;
		s/(?:.*?\s+|^)enum(?:\s+|$)//;
		$startBraceCount = $braceCount;
	    }
	    if ( $gettingEnum ) {
		if ( !$name && m/^(\s*)([a-zA-Z0-9_]+)/ ) {
		    $name = $2;
		    $line .= $1 . $2;
		    s/\s*[a-zA-Z0-9_]+//;
		}
		if ( $name && m/^([^;]*?){/ ) {
		    ++$gettingEnum;
		    $line .= $1;
		    s/^([^;]*?){/{/;
		    redo;
		}
	    }
	} elsif ( $self->{'enum'} && !$gettingFuncgrp && !$nestedFuncgrp && $gettingEnum >= 2 ) {
	    if ( m/[}{]/ ) {
		($tmp = $_) =~ s/[^}{]//g;
		$cnt = 0;
		foreach (split(//,$tmp)) {
		    ++$cnt;
		    m/^{$/ && (++$braceCount);
		    m/^}$/ && (--$braceCount);
		    last if $braceCount == $startBraceCount;
		}
	    }
	    if ($braceCount == $startBraceCount) {
		s/((?:.*?[}{]){$cnt}).*/$1/;
	    }
	    if ( $line ) {
		$_ = $line . $_;
		$line = "";
		push(@enumout,$self->{'num'} ? "$self->{'line num'} " . $_ : $_) unless $self->{'tail'} && !$self->{'head'};
	    } else {
		push(@enumout,$self->{'num'} ? "$self->{'line num'} " . $_ : $_) if !$self->{'tail'} && !$self->{'head'} ||
										 $self->{'tail'} && $braceCount == $startBraceCount;
	    }
	    if ( $braceCount == $startBraceCount ) {
		    ++$gotClass if !$gettingClass;
		    push(@subs, [ @enumout ] );
		    @enumout = ();
		    $gettingEnum = 0;
	    }

	    ++$self->{'line num'};
	    next;
	}
	    
	if ( $gettingClass < 2 ) {
	    ##
	    ## Should be replaced with:
	    ##
	    ##		m/(?:\s+|^)((?:class|struct)(?:\s+(?![^<]*?>)|$))/
	    ##
	    ## when later version of Perl 5.0 is released...
	    ##
	    if ( !$gettingClass && (m/(?:\s+|^)(class(?:\s+(?![^<]*?>)|$))/ ||
				    m/(?:\s+|^)(struct(?:\s+(?![^<]*?>)|$))/ ) ) {
		$gettingClass++;
		$start = $self->{'line num'};
		$line = $1;
		s/(?:.*?\s+|^)(?:class|struct)(?:\s+|$)//;
	    }
	    if (!$gettingClass) {
		$line="";
		++$self->{'line num'};
		next;
	    }

	    if ( !$name && m/^(\s*)([a-zA-Z0-9_]+)/ ) {
		$name = $2;
		$line .= $1 . $2;
		s/\s*[a-zA-Z0-9_]+//;
	    }

	    ##
	    ## Strip out stuff which looks like:
	    ##     struct {
	    ##        ...
	    ##     }
	    ##
	    if ( !$name && m/^\s*{(.*$)/ ) {
		$_ = $1;
		++$braceCount;
		if ( !$_ ) {
		    $_ = <$FOO>;
		    ++$self->{'line num'};
		}
		while ($braceCount) {
		    if ( m/[}{]/ ) {
			($tmp = $_) =~ s/[^}{]//g;
			$cnt = 0;
			foreach (split(//,$tmp)) {
			    ++$cnt;
			    m/^{$/ && (++$braceCount);
			    m/^}$/ && (--$braceCount);
			    last if !$braceCount;
			}
		    }
		} continue { 
		    if ($braceCount) {
			$_ = <$FOO>;
			++$self->{'line num'};
		    }
		}

		s/(?:.*?[}{]){$cnt}(.*)/$1/ if !$braceCount;
		$gettingClass = 0;
		next if $braceCount;
		redo;
	    }

	    if ( $name && m/^([^;]*?){/ ) {
		++$gettingClass;
		$line .= $1;
		s/^([^;]*?){/{/;
		redo;
	    }

	    if ($name && m/^.*?;/ ) {
		$name="";
		$line="";
		$gettingClass=0;
		s/^.*?;//;
		redo;
	    }

	    if ( $gettingClass ) {
		push(@out,$self->{'num'} ? "$self->{'line num'} " . $line . $_ : $line . $_) unless $self->{'tail'} && !$self->{'head'};
		$line="";
		++$self->{'line num'};
		next;
	    }
	} else {
	    if ( m/[}{]/ ) {
		($tmp = $_) =~ s/[^}{]//g;
		$cnt = 0;
		foreach (split(//,$tmp)) {
		    ++$cnt;
		    m/^{$/ && (++$braceCount);
		    m/^}$/ && (--$braceCount);
		    last if !$braceCount;
		}
	    }
	    if (!$braceCount) {
		s/((?:.*?[}{]){$cnt}).*/$1/;
		++$gotClass;
	    }

	    if ( $line ) {
		$_ = $line . $_;
		$line = "";
		push(@out,$self->{'num'} ? "$self->{'line num'} " . $_ : $_) unless $self->{'tail'} && !$self->{'head'};
	    } else {
		push(@out,$self->{'num'} ? "$self->{'line num'} " . $_ : $_) if !$self->{'tail'} && !$self->{'head'} ||
										 $self->{'tail'} && !$braceCount;
	    }
	}

	++$self->{'line num'};

    }

    unshift(@subs,\@out) if scalar(@out);
    @subs;
}

sub DESTROY {
    my $self = shift;
    if ($self->{'open'}) {
	close $self->{'file ref'};
	$self->{'open'} = 0;
    }
}
1;
