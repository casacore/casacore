#
#   This is part of cxx2html Version 1.2 Patchlevel 4
#   Created by Darrell Schiebel (drs@nrao.edu)
#
#   This utility, cxx2html, is part of AIPS++, a software project
#   centered at the National Radio Astronomy Observatory.
#
#   Copyright (C) 1995,1999
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
package ScanCxx;
require 5.000;

$_FileSeed="aaaaaaaa";

sub file {
    "file_" . $_FileSeed++;
}

sub new {
    my $type = shift;
    my $self = {};
    bless $self;
    $self->{'limit'} = 0;
    $self->set(@_);
    $self->{'file ref'} = file();
    $self->{'open'} = 0;
    $self->{'put back'} = [];
    $self->{'class section'} = "";
    $self->{'brace count'} = 0;
    $self->open();
    $self;
}


sub set {    
    my $self = shift;
    my %params = @_;

    ##### file to process 	#####
    $self->{'file'} = $params{'file'} if defined $params{'file'};

    ##### file to process 	#####
    $self->{'limit'} = $params{'limit'} if defined $params{'limit'};
}

sub open {
    my $self = shift;

    $self->{'brace count'} = 0;
    if ($#_ >= 0) {
	$self->{'file'} = $_[0];
    }

    if ($self->{'file'}) {
	$self->{'put back'} = [];
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

	return 1;
    }
    return 0;
}

sub eof {
    my $self = shift;
    if ( $#_ < 0 ) {
	return eof($self->{'file ref'});
    } else {
	return eof($_[0]);
    }
}

sub line {
    my $self = shift;

    return $self->{'line num'};
}

sub atlimit {
    my $self = shift;

    return $self->{'limit'} ? $self->{'line num'} >= $self->{'limit'} : 0;
}

sub section {
    my $self = shift;

    return $self->{'class section'};
}

sub putback {
    my $self = shift;

    push(@{$self->{'put back'}},@_);
    $self->{'line num'} -= scalar(@_);
}

##
## There is a subtle off-by-one error with this
## subroutine. It starts at 1, and increments with
## the first element. (this should be looked at in
## the future)
##
sub next {
    my $self = shift;
    my $fref = $self->{'file ref'};

    if ($#{$self->{'put back'}} < 0) {
	$_ = <$fref>;
    } else {
	$_ = pop(@{$self->{'put back'}});
    }
    ++$self->{'line num'};
    return $_;
}

sub skipto {
#    my $self = shift;
#    my $num = @_[0];
#    my @comment = ();

    $self = shift;
    $num = @_[0];
    @comment = ();

    while ( $self->{'line num'}<$num && 
	   ! $self->eof() && 
	   ! $self->atlimit() ) {
	$_ = $self->next();
	if ( m@^\s*//(.*)\n?@ ) {
	    push(@comment,$1);
	} elsif ( /^\s*$/ ) {
	    push(@comment,"");
	} else {
# Don't start again if within 5 lines of the class name.
# This is to avoid problems when the class name declaration is
# given in more than one line (e.g. when using template).
# This is a hack; a better solution should be implemented one time.
	    if ($self->{'line num'} < $num-5) {
	        @comment = ();
	    }
	}
    }

    return \@comment;
}


sub skiptoNoProc {
#    my $self = shift;
#    my $num = @_[0];
#    my @comment = ();

 $self = shift;
 $num = @_[0];

    for ($_ = $self->next(); $self->{'line num'}<$num && 
	   ! $self->eof() && 
	   ! $self->atlimit(); 
	 $_ = $self->next()) { ; }

}

sub comment {
    my $self = shift;
    my @comment = ();

    $_ = $self->next(); 
    while ( ! $self->eof() && 
	    m@^\s*(?://(.*))?\n?$@ ) {
	push(@comment,$1);
    } continue {
	last if $self->atlimit();
	$_ = $self->next();
    }

    $self->putback($_) unless m@^\s*//@;
    return \@comment;
}

sub elementditchbraces {
    my $self = shift;
    my $element = "";
    my $killingDef = 0;
    my $killingCstyle = 0;
    my $braceCount = $self->{'brace count'};
    my $doingEnum = 0;

    if ( scalar(@_) ) {
	$doingEnum = shift;
    }

    $_ = $self->next(); 
    while ( $braceCount ||
	    (! $self->eof() && 
	     ! m@^\s*(?://.*)?\n?$@) ) {
	##
	## Kill blank lines and statementless semicolons
	##
	next if /^\s*;?\s*$/;

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
		next;
	    }
	}

	##
	## Kill the #define
	##
	if (  /\\$/ ) {
	    next if $killingDef;
	} else {
	    $killingDef = 0;
	}
	if ( /^\s*#define/ ) {
	    $killingDef = 1 if m/\\$/;
	    next;
	}

	##
	## Kill the #if
	##
	if ( /^\s*\#(?:if|else|endif)/ ) {
	    next;
	}

	##
	## Access
	##
	if ( /^\s*(public|private|protected)\s*:(.*\n?)$/ ) {
	    $self->{'class section'} = $1;
	    $self->putback($2) if $2 !~ m/^\s*\n?$/;
	    next;
	}

	##
	## Last on comma for enumerations
	if ( $doingEnum && ( /^\s*([a-zA-Z0-9_]+\s*(?:=\s*\d+\s*)?)([,}].*\n?)/ ||
			     m@^\s*([a-zA-Z0-9_]+\s*(?:=\s*\d+\s*)?)(//.*)?$@ ) ) {
	    $element =~ s/\s*\}\s*$//;
	    $element .= $1 if $1 !~ /^\s*$/;
	    ($rest = $2) =~ s/^,\s*//;
	    $self->putback($rest) if $rest !~ /^\s*$/ && ! $self->atlimit();
	    last;
	}

	##
	## Ditch braces
	##
	if ( ! $braceCount ) {
	    if ( /^(.*?)\{(.*\n?)/ ) {
		my $two = $2;
		$element .= $1 if $1 !~ /^\s*$/;
		$self->putback($two) if $two !~ /^\s*$/;
		++$braceCount;
		if ( $element !~ /^\s*enum/ ) {
		    next;
		} else {
		    $_ = "";
		    last;
		}
	    }
	} elsif ( /[}{]/ ) {
	    ($tmp = $_) =~ s/[^}{]//g;
	    $cnt = 0;
	    foreach (split(//,$tmp)) {
		++$cnt;
		/^{$/ && (++$braceCount);
		   /^}$/ && (--$braceCount);
		break if !$braceCount;
	    }
	    if ( !$braceCount ) {
		$self->putback($1) if /(?:.*?[}{]){$cnt}(.*)/;
		last;
	    }
	    next;
	} else {
	    next if ! $doingEnum;
	}

	##
	## Last on semicolon
	if ( /^(.*?);(.*\n?)/ ) {
	    $element .= $1 if $1 !~ /^\s*$/;
	    $self->putback($2) if $2 !~ /^\s*$/ && ! $self->atlimit();
	    last;
	}

	##
	## Stop when a comment is encountered
	if ( m@\s*(//.*)$@ ) {
	    $self->putback($1) if ! $self->atlimit();
	    last;
	}

	chop;
	$element .= $_;
    } continue {
	if ( $element =~ /^\s*enum/ ) {
	    last;
	}
	last if $self->atlimit();
	$_ = $self->next();
    }

    $self->putback($_) if m@^\s*//@;
    $self->{'brace count'} = $braceCount;
    return ($braceCount, $element);
}

sub tobrace {
    my $self = shift;
    my $num = @_[0];
    my $code = "";

    for ($_ = $self->next(); 
	 ! $self->eof() && 
	 ! m/^.*?{.*\n?/ && 
	 ! $self->atlimit(); 
	 $_ = $self->next()) {
	chop;
	$code .= $_;
    }
    $code .= $1 if m/^(.*?){(.*\n?)/;
    $self->putback($2) if $2 !~ m/^\s*$/;
    return $code;
}

sub slice {
    my $self = shift;
    my $num = @_[0];
    my @slice = ();

    for (; $self->{'line num'}<$num && 
	   ! $self->eof() && 
	   ! $self->atlimit();
	 ++$self->{'line num'}) {
	push(@slice,$self->next());
    }
    return \@slice;
}

sub DESTROY {
    my $self = shift;
    if ($self->{'open'}) {
	close $self->{'file ref'};
	$self->{'open'} = 0;
    }
}
1;
