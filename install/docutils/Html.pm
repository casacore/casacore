#
#   This is part of cxx2html Version 1.2 Patchlevel 4
#   Created by Darrell Schiebel (drs@nrao.edu)
#
#   This utility, cxx2html, is part of AIPS++, a software project
#   centered at the National Radio Astronomy Observatory.
#
#   Copyright (C) 1995,1997,1999,2000
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
package Html;
require 5.000;

use File::Basename;
use Path;
use AnyDBM_File;
$_FileSeed="aaaaaaaa";


sub file {
    "file_" . $_FileSeed++;
}

sub new {
    my $type = shift;
    my $db = shift;
    my $self = {};

    bless $self;
    $CxxOps='operator\s*(?:new|delete|\+=|\-=|\*=|\/=|%=|^=|&=|\|=|<<|>>|>>=|<<=|==|!=|<=|>=|&&|\|\||\+\+|\-\-|,|->*|->|\(\)|\[\]|\+|\-|\*|\/|%|^|&|\||~|!|=|<|>)';
    $self->{'output'} = { dir => '',		# directory for html ($pwd)
			  hierarchy => 1,	# preserve hierarchy?
			  create => 0,		# create any needed dirs
			  root => [],		# hierarchy root
			  absolute => 1,	# use absolute paths for 
						#       non-generated files
			  # INTERNAL
			  path => '',
			  'do note' => 1,
			};

    $self->{'AIPS++ extensions'} = 1;
    $self->{'File Extensions'} = [];
    $self->{'HTML Extension'} = 'html';
    $self->{'Max Summary Length'} = 80;
    $self->{'Copyright Notice'} = [];

    $self->set(@_);
    $self->{'file ref'} = file();
    $self->{'open'} = 0;
    $self->{'contents'} = [];
    $self->{'section'} = {};
    ##
    ## Used to maintain order of the classes (i.e. not hash order)
    ##
    $self->{'section'}{'class order'} = [];
    $self->{'class section'} = "";
    $self->{'first class section'} = 1;
    $self->{'first global function'} = 1;
    $self->{'first see also'} = 1;

    $self->{'group'}{'comment stack'} = [];
    $self->{'group'}{'in group'} = 0;
    $self->{'group'}{'started group'} = 0;

    $self->{'group'}{'transition'} = 0;
    $self->{'group'}{'stack of header stack'} = [];
    $self->{'group'}{'last comment glop'} = [];
    $self->{'group'}{'last header glop'} = [];
    $self->{'group'}{'anchor stack'} = [];
    $self->{'group'}{'last anchor'} = [];

    $self->{'module doc'} = [];
    $self->{'module contents'} = [];
    $self->{'module name'} = '';

    $self->{'header db'} = $db;

    $self->set_bitmaps();

    $self->open();

    $self;
}

sub set {    
    my $self = shift;
    my %params = @_;

    ##### file to process 	#####
    $self->{'file'} = $params{'file'} if defined $params{'file'};

    ##### file to process 	#####
    $self->{'title'} = $params{'title'} if defined $params{'title'};

    ##### html in a directory 	#####
    if (defined $params{'dir'}) {
	my $dir = $params{'dir'};
	$dir = CachedCwd() if ! $dir;
	$self->{'output'}{'dir'} = fullPath($dir);
    }

    ##### maintain the hierarchy 	#####
    $self->{'output'}{'hierarchy'} = $params{'hierarchy'} if defined $params{'hierarchy'};

    ##### create needed directories? 	#####
    $self->{'output'}{'create'} = $params{'create'} if defined $params{'create'};

    ##### Where is the root for duplicating the hierarchy ? 	#####
    $self->{'output'}{'root'} = $params{'root'} if defined $params{'root'};

    ##### Use absolute paths to non generated files 	#####
    $self->{'output'}{'absolute'} = $params{'absolute'}
                                      if defined $params{'absolute'};

    ##### Add AIPS++ specific features 	#####
    $self->{'AIPS++ extensions'} = $params{'AIPS++ extensions'} 
				      if defined $params{'AIPS++ extensions'};

    ##### Add File Extensions 	#####
    $self->{'File Extensions'} = $params{'File Extensions'} 
				      if defined $params{'File Extensions'};

    ##### Add HTML Extensions 	#####
    $self->{'HTML Extension'} = $params{'HTML Extension'} 
				      if defined $params{'HTML Extension'};

    $self->{'Max Summary Length'} = $params{'Max Summary Length'} 
				      if defined $params{'Max Summary Length'};

    $self->{'Copyright Notice'} = $params{'Copyright Notice'}
				      if defined $params{'Copyright Notice'};

}

sub html_loc {
    my $self = shift;
    my $file = shift;
    my $dir = '';
    my $db = $self->{'header db'};
    my $regexExt = join('|',@{$self->{'File Extensions'}});

    $dir = $self->{'output'}{'dir'} if $self->{'output'}{'dir'};
    if ($self->{'output'}{'hierarchy'}) {
	my $f = $file;
	my $n = '';
	my $s = '';
	$f =~ s/(?:$regexExt|\.$self->{'HTML Extension'})$//;
	$f = $$db{"long*$f"};
	($n,$f,$s) = fileparse($f,@{$self->{'File Extensions'}});
	foreach (@{$self->{'output'}{'root'}}) {
	    $f =~ s!^\Q$_\E!! if $f;
	}
	$f =~ s/^\/// if ! $dir;
	$dir .= $f;
    }
    $dir .= '/' if $dir && $dir !~ /\/$/;
    return $dir;
}

sub htmlpath {
    my $self = shift;
    my $ofile = shift;
    my $file = $ofile;
    my $dir = '';
    my $db = $self->{'header db'};
    my $regexExt = join('|',@{$self->{'File Extensions'}});

    if ($file =~ /\.$self->{'HTML Extension'}$/) {
	$dir = $self->relfile($self->html_loc($file));
	($name,$path,$s) = fileparse($ofile,'\.' . $self->{'HTML Extension'}) if 
	    $ofile =~ /\.$self->{'HTML Extension'}$/;
	if ($name) {
	    if ($dir) {
		$dir .= "/$name$s";
	    } else {
		$dir = "$name$s";
	    }
	}
    } elsif ($file =~ /(?:$regexExt)$/) {
	($name,$path,$s) = fileparse($file,@{$self->{'File Extensions'}});
	if ( $self->{'output'}{'absolute'} ) {
	    $dir = $$db{"long*$name"};
	} else {
	    $dir = $self->relfile($$db{"long*$name"});
	}
    }
    if ($dir) {
	$ret = $dir;
    } else {
	$ret = $ofile;
    }
    $ret =~ s/\+/\%2b/g;
    return $ret;
}

sub open {
    my $self = shift;
    my $dir = '';

    if ($#_ >= 0) {
	$self->{'file'} = $_[0];
    }
    if ($self->{'file'}) {
	if ($self->{'open'}) {
	    close $self->{'file ref'};
	    $self->{'open'} = 0;
	}

	$dir = $self->html_loc($self->{'file'});
	$self->maketree($dir) if $dir && $self->{'output'}{'create'};

	$dir .= '/' if $dir && $dir !~ /\/$/;

	$self->{'output'}{'path'} = $dir;

	$self->{'open'} = "> $dir$self->{'file'}";
	if (!open($self->{'file ref'},$self->{'open'})) {
	    print STDERR "err: opening $dir/$self->{'file'}\n";
	    $self->{'open'} = 0;
	    return 0;
	}
	return 1;
    }
    return 0;
}

sub relfile {
    my $self = shift;
    my $path = @_[0];
    my $opath = $path;
    my $name = '';
    my $db = $self->{'header db'};
    my $cur = $self->{'output'}{'path'};
    my $regexExt = join('|',@{$self->{'File Extensions'}});
    $cur =~ s|/$||;

    ($name,$path,$s) = fileparse($path,(@{$self->{'File Extensions'}},
					'\.'.$self->{'HTML Extension'}) ) if 
	$path =~ /(?:$regexExt|\.$self->{'HTML Extension'})$/;
    if (! $path) {
	return $opath;
    }

    my $rel = relPath($cur,$path);
    $rel =~ s/\/$//;
    if ($name) {
	if ( $rel ) {
	    $rel .= "/$name$s";
	} else {
	    $rel = "$name$s";
	}
    }
    return $rel;
    
}
    

sub maketree {
    my $self = shift;
    my $opath = shift;
    my $path = $opath;
    my @path = ();
    my $cur = '';
    my $mode = 0755;
    my $regexExt = join('|',@{$self->{'File Extensions'}});

    if (! -e $path) {
	($name,$path,$suffix) = fileparse($opath,(@{$self->{'File Extensions'}}, '\.' . $self->{'HTML Extension'}))
	    if $path =~ /(?:$regexExt|\.$self->{'HTML Extension'})$/;
	$path =~ s/\/$//;
	$mode = $self->{'output'}{'create mode'} if $self->{'output'}{'create mode'};
	$path =~ s@^/@@;
	$path =~ s@/$@@;
	foreach (split(/\//,$path)) {
	    push(@path,$_);
	    if ($opath =~ /^\//) {
		$cur = '/';
	    } else {
		$cur = '';
	    }
	    $cur .= join('/',@path);
	    if (! -e $cur ) {
		die "err($!): Can't create $opath\n" unless mkdir($cur,$mode);
	    }
	}
    }
}
		
     
##
## Modifies Comment  -- removes summary line
##
sub getsummary {
    my $self = shift;
    my $doc = @_[0];
    my $name = @_[1];
    my $summary = "";
    my $getting = 0;
    my $cnt = 0;
    my @remline = ();
    my $foundsum = 0;
    my $maxlen = $self->{'Max Summary Length'};
    my $DOAIPS = $self->{'AIPS++ extensions'};

    foreach (@$doc) {
	if ( $getting ) {
	    if ( m@^(.*?)</summary>(.*)$@i ) {
		$getting = 0;
		$summary .= $1;
		$_ = $2;
		if ( /^\s*$/ ) {
		    unshift(@remline,$cnt);
		} else {
		    splice(@$doc,$cnt,1,$_);
		}
		next;
	    } else {
		$summary .= $_;
		unshift(@remline,$cnt);
		next;
	    }
	}
	if ( m@^(.*?)<summary>(.*?)</summary>(.*)$@i ) {
	    $foundsum++;
	    $summary = $2;
	    $_ = $1 . $3;
	    if ( /^\s*$/ ) {
		unshift(@remline,$cnt);
	    } else {
		splice(@$doc,$cnt,1,$_);
	    }		
	    next;
	}
	if ( m@^(.*?)<summary>(.*)$@i ) {
	    $foundsum++;
	    $summary = $2;
	    $_ = $1;
	    if ( /^\s*$/ ) {
		unshift(@remline,$cnt);
	    } else {
		splice(@$doc,$cnt,1,$_);
	    }
	    $getting = 1;
	    next;
	}
    } continue { ++$cnt; }
# Remove the empty lines.
    foreach (@remline) {
        splice(@$doc,$_,1);
    }
# Check if summary is given correctly.
    if ( $DOAIPS  &&  $foundsum == 0 ) {
        print STDERR "Error: no <summary> given in " . $name . " of \"";
	print STDERR $self->{'file'} . "\"! \n" ;
    }
    if ( $foundsum > 1 ) {
        print STDERR "Error: <summary> multiply used in " . $name . " of \"";
	print STDERR $self->{'file'} . "\"! \n" ;
    }
    if ( $getting ) {
        print STDERR "Error: <summary>/</summary> mismatch in " . $name . " of \"";
	print STDERR $self->{'file'} . "\"! \n" ;
    }
    if ( $maxlen > 0  &&  length($summary) > $maxlen ) {
        print STDERR "Error: summary > " . $maxlen . " characters in " . $name . " of \"";
	print STDERR $self->{'file'} . "\"! \n" ;
    }
    return $summary;
}

sub fixcomment {
    my $self = shift;
    my $class = shift;
    my $comment = shift;
    my $killgroup = 0;
    $killgroup = shift if scalar(@_);
    my $commentstack = $self->{'group'}{'comment stack'};
    my $anchorstack = $self->{'group'}{'anchor stack'};
    my $DOAIPS = $self->{'AIPS++ extensions'};
    my $code = 0;
    my $doindent = 0;
    my $checkindent = 1;
    my $inlist = 0;
    my @listtype = ();
    my $notgotitem = 0;
    my @itemnum = ();
    my $reallines = 0;
    my @linkfromstack = ();
    my $regexExt = join('|',@{$self->{'File Extensions'}});

    my @dumAry;
    my $commentIsScalar = 0;

    if ( ref($comment) eq "SCALAR" ) {
	$commentIsScalar = $comment;
	$dumAry[0] = $$comment;
	$comment = \@dumAry;
    } elsif ( ref($comment) ne "ARRAY" ) {
	return;
    }

    ## Used to keep track of started and ended group
    $self->{'group'}{'started group'} = 0;

    for ($i=0; $i < scalar(@$comment); ++$i) {

	while ( $killgroup && $$comment[$i] =~ s|</?group.*?>||i ) {}

	if ( $$comment[$i] =~ m/^\s*$/ ) {
	    $$comment[$i] =~ s|^\s*$|<p>|ig if ! $code;
	    next;
	}
	++$reallines;
	if (! $code) {
	    while ( $$comment[$i] =~ /^\*display\s+[0-9]/ || $$comment[$i] =~ /^\#/ ) {
		splice(@$comment,$i,1);
		$i-- if $i;
	    }
	    if ( $$comment[$i] =~ s/^(.*?)<group(.*?)>//i ) {
		my $name = $2;
		my $head = $1;
		$name =~ s/.*?name=([^\s>]+|["'][^'"]+['"]).*/$1/;
		$name =~ s/^["']//;
		$name =~ s/["']$//;
		my $tmp = [];
		my $tmp2 = [];
		my $stack_of_header_stacks = $self->{'group'}{'stack of header stack'};
		@$tmp = splice(@$comment,0,$i);
		push(@$tmp,$head) if $head;
		push(@$commentstack,$tmp);
		shift(@$comment) if !$$comment[0];
		$i = 0;
		$self->{'group'}{'in group'} += 1;
		$self->{'group'}{'started group'} = 1;
		push(@$stack_of_header_stacks,$tmp2);
		$TMP = $self->{'section'}{$class}{'members'};
		push(@$anchorstack,[0,$name]);
		$reallines = 0;
		redo;
	    }
	    if ( $$comment[$i] =~ m|</group>|i ) {
		my $stack_of_header_stacks = $self->{'group'}{'stack of header stack'};
		push(@{$self->{'group'}{'last comment glop'}},pop(@$commentstack));
		push(@{$self->{'group'}{'last header glop'}},pop(@$stack_of_header_stacks));
		push(@{$self->{'group'}{'last anchor'}},pop(@$anchorstack));
		splice(@$comment,$i,1);
		$self->{'group'}{'in group'} -= 1;
		if ( $self->{'group'}{'in group'} < 0 ) {
		    print STDERR "Error: <group>/</group> mismatch generating \"";
		    print STDERR $self->{'file'} . "\"! (resetting)\n" ;
		    $self->{'group'}{'in group'} = 0;
		} else {
		    $self->{'group'}{'transition'} += 1;
		}
		$i-- if $i;
		$reallines = 0;
		redo;
	    }
	}
	##
	## <src> and <srcblock>
	##
	if ( ! $code && $$comment[$i] =~ m|(.*?)<src>(.*?)</src>(.*)|i ) {
	    my $a = $1;
	    my $b = $2;
	    my $c = $3;
	    $b =~ s/&/&amp;/g;
	    $b =~ s/"/&quot;/g;
	    $b =~ s/</&lt;/g;
	    $b =~ s/>/&gt;/g;
	    $$comment[$i] = "$a<tt>$b</tt>$c";
	    redo;
	}
	if ( ! $code && $$comment[$i] =~ m|(.*?)<src>(.*)|i ) {
	    my $a = $1;
	    my $b = $2;
	    $b =~ s/&/&amp;/g;
	    $b =~ s/"/&quot;/g;
	    $b =~ s/</&lt;/g;
	    $b =~ s/>/&gt;/g;
	    $$comment[$i] = "$a<tt>$b";
	    $code = $i;
	    $doindent=0;
	    $checkindent=0;
	    next;	## must do a next to avoid having <tt> mucked with

	}
	if ( $code && $$comment[$i] =~ m|(.*?)</src>(.*)|i ) {
	    my $a = $1;
	    my $b = $2;
	    $a =~ s/&/&amp;/g;
	    $a =~ s/"/&quot;/g;
	    $a =~ s/</&lt;/g;
	    $a =~ s/>/&gt;/g;
	    $$comment[$i] = "$a</tt>$b";
	    $code = 0;
	    redo;
	}
	if ( ! $code && $$comment[$i] =~ s|<srcblock>|<pre>|gi ) {
	    splice(@$comment,$i++,0,$1) if $$comment[$i] =~ s/^(.*?<pre>)//;
	    splice(@$comment,$i,1) if $$comment[$i] =~ /^\s*$/;
	    $code = $i;
	    $doindent=0;
	    $checkindent=1;
	}
	if ( $code && $$comment[$i] =~ s|</srcblock>|</pre>|gi ) {

	    splice(@$comment,$i+1,0,$2) if $$comment[$i] =~ s|^(.*?)(</pre>.*)|$1|;
	    $$comment[$i] =~ s/&/&amp;/g;
	    $$comment[$i] =~ s/"/&quot;/g;
	    $$comment[$i] =~ s/</&lt;/g;
	    $$comment[$i] =~ s/>/&gt;/g;
	    splice(@$comment,$i,1) if $$comment[$i] =~ /^\s*$/;

	    ++$i;
	    if ( $doindent ) {
		for ($j=$code; $j < $i; ++$j) {
		    $$comment[$j] =~ s/^\s?/    /;
		}
	    }
	    $code = 0;
	    redo;
	}

	$doindent = 1 if $checkindent && $code && $$comment[$i] =~ /^\s?\S/;
	($doindent = 0, $checkindent = 0) if $code && $$comment[$i] =~ /^\t/;

	$$comment[$i] =~ s/&/&amp;/g if $code;
	$$comment[$i] =~ s/"/&quot;/g if $code;
	$$comment[$i] =~ s/</&lt;/g if $code;
	$$comment[$i] =~ s/>/&gt;/g, next if $code;

	if ( $$comment[$i] =~ /<note\s+role\s*=\s*['"]?(tip|caution|warning)["']?.*?>/i ) {
	    my $type = lc $1;
	    my $outtype = ucfirst $type;
	    if ( $self->{'output'}{'do note'} ) {
		if (! -f "$self->{'output'}{'path'}$type.gif" ) {
		    if ( ! open(GIF,"> $self->{'output'}{'path'}$type.gif") ) {
			print STDERR "Sorry, can't write to $self->{'output'}{'path'}$type.gif using boring $type...\n";
			$$comment[$i] =~ s|<note.*?>|<blockquote><strong>$outtype:</strong><em>|i;
			$self->{'output'}{'do note'} = 0;
		    } else {
			print GIF $ {$self->{"$type gif"}};
		        close(GIF);
		        $$comment[$i] =~ s|<note.*?>|<blockquote><img src="$type.gif" alt="$outtype"><em>|i;
		    }
		} else {
		    $$comment[$i] =~ s|<note.*?>|<blockquote><img src="$type.gif" alt="$outtype"><em>|i;
		}
	    } else {
		$$comment[$i] =~ s|<note.*?>|<blockquote><strong>$outtype:</strong><em>|i;
	    }
	    for ($j=$i; $j < scalar(@$comment); ++$j) {
		last if $$comment[$j] =~ s|</note>|</em></blockquote>|i;
		last if $$comment[$j] =~ s|^\s*$|</em></blockquote>|;
	    }
	    splice(@$comment,$j,0,"</em></blockquote>") if $j >= scalar(@$comment);
	}

	##
	## Handle class/anchor references
	##
	while ($$comment[$i] =~ /<linkto\s+[^>]*?class=[\"\'](\w+)([#:])([^\"\']+)[\"\'][^>]*?>/i ||
	       $$comment[$i] =~ /<linkto\s+[^>]*?class=(\w+)([#:])([^\s\>]+)[^>]*?>/i ) {
	    my $cls = $1;
	    my $type = $2;
	    my $anchor = $3;
	    my $db = $self->{'header db'};
	    $file = $$db{"class*$cls"};
	    $file =~ s/.*?:([^:]+)$/$1/;
	    if ( $cls && $file ) {
		$file = $self->htmlpath("$file.$self->{'HTML Extension'}");
		if ( $type eq '#' ) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file#$anchor">/i;
		} elsif ( $type eq ':' ) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file#$cls:$anchor">/i;
		}
	    } else {
		print STDERR "Can't do class reference in generating $self->{'file'}:\n";
		print STDERR "\t$$comment[$i]\n";
		last;
	    }
	}

	##
	## Handle class references
	##
	while ($$comment[$i] =~ /<linkto\s+[^>]*?class=[\"\']?(\w+)[\"\']?[^>]*?>/i ) {
	    my $cls = $1;
	    my $db = $self->{'header db'};
	    $file = $$db{"class*$cls"};
	    $file =~ s/.*?:([^:]+)$/$1/;
	    if ( $cls && $file ) {
		$file = $self->htmlpath("$file.$self->{'HTML Extension'}");
		$$comment[$i] =~ s/<linkto.*?>/<a href="$file#$cls">/i;
	    } else {
		print STDERR "Can't do class reference in generating $self->{'file'}:\n";
		print STDERR "\t$$comment[$i]\n";
		last;
	    }
	}

	##
	## Handle func/anchor references
	##
	while ($$comment[$i] =~ /<linkto\s+[^>]*?group=[\"\']([\w\.]+)\#(\w+)([#:])([^\"\']+)[\"\'][^>]*?>/i ||
	       $$comment[$i] =~ /<linkto\s+[^>]*?group=([\w\.]+)\#(\w+)([#:])([^\s\>]+)[^>]*?>/i ) {
	    my $hdr = $1;
	    my $cls = $2;
	    my $type = $3;
	    my $anchor = $4;
	    my $db = $self->{'header db'};
	    my $hdrs = $$db{"group*$cls"};
	    my $n,$f,$s;
	    ($n,$f,$s) = fileparse($hdr,@{$self->{'File Extensions'}});
	    my $test = ($hdrs =~ m|\Q$n\E|);
	    if ( $hdr && $cls && $test ) {
		$file = $self->htmlpath("$n.$self->{'HTML Extension'}");
		if ( $type eq '#' ) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file#$anchor">/i;
		} elsif ( $type eq ':' ) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file#$cls:$anchor">/i;
		}
		$$comment[$i] =~ s/<linkto.*?>/<a href="$file#$cls">/i;
	    } else {
		print STDERR "Can't do func reference in generating $self->{'file'}:\n";
		print STDERR "\t$$comment[$i]\n";
		last;
	    }
	}

	##
	## Handle func references
	##
	while ($$comment[$i] =~ /<linkto\s+[^>]*?group=[\"\']([\w\.]+)\#([^\'\"]+)[\"\'][^>]*?>/i ||
		$$comment[$i] =~ /<linkto\s+[^>]*?group=([\w\.]+)\#([^\s>]+)[^>]*?>/i ) {
	    my $hdr = $1;
	    my $cls = $2;
	    my $db = $self->{'header db'};
	    my $hdrs = $$db{"group*$cls"};
	    my $n,$f,$s;
	    ($n,$f,$s) = fileparse($hdr,@{$self->{'File Extensions'}});
	    my $test = ($hdrs =~ m|\Q$n\E|);
	    if ( $hdr && $cls && $test ) {
		$file = $self->htmlpath("$n.$self->{'HTML Extension'}");
		$$comment[$i] =~ s/<linkto.*?>/<a href="$file#$cls">/i;
	    } else {
		print STDERR "Can't do func reference in generating $self->{'file'}:\n";
		print STDERR "\t$$comment[$i]\n";
		last;
	    }
	}

	##
	## Handle module/anchor references
	##
	while ($$comment[$i] =~ /<linkto\s+[^>]*?module=[\"\'](\w+)([#:])([^\"\']+)[\"\'][^>]*?>/i ||
	       $$comment[$i] =~ /<linkto\s+[^>]*?module=(\w+)([#:])([^\s\>]+)[^>]*?>/i ) {
	    my $mod = $1;
	    my $type = $2;
	    my $anchor = $3;
	    my $db = $self->{'header db'};
	    my $test = $$db{"module*$mod"};
	    if ( $anchor && $mod && $test ) {
		$file = $self->htmlpath("$mod.$self->{'HTML Extension'}");
		if ( $type eq '#' ) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file#$anchor">/i;
		} elsif ( $type eq ':' ) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file#$mod:$anchor">/i;
		}
	    } else {
		print STDERR "Can't do module reference in generating $self->{'file'}:\n";
		print STDERR "\t$$comment[$i]\n";
		last;
	    }
	}

	##
	## Handle module references
	##
	while ($$comment[$i] =~ /<linkto\s+[^>]*?module=[\"\']?(\w+)[\"\']?[^>]*?>/i ) {
	    my $mod = $1;
	    my $db = $self->{'header db'};
	    my $test = $$db{"module*$mod"};
	    if ( $mod && $test ) {
		$file = $self->htmlpath("$mod.$self->{'HTML Extension'}");
		$$comment[$i] =~ s/<linkto.*?>/<a href="$file">/i;
	    } else {
		print STDERR "Can't do module reference in generating $self->{'file'}:\n";
		print STDERR "\t$$comment[$i]\n";
		last;
	    }
	}

	##
	## Handle file references
	##
	while ($$comment[$i] =~ /<linkto\s+[^>]*?file=[\"\']([\w\.]+)(#)([^\"\']+)[\"\'][^>]*?>/i ||
	       $$comment[$i] =~ /<linkto\s+[^>]*?file=([\w\.]+)(#)([^\s\>]+)[^>]*?>/i ||
	       $$comment[$i] =~ /<linkto\s+[^>]*?file=[\"\']([^\"\']+)[\"\'][^>]*?>/i ||
	       $$comment[$i] =~ /<linkto\s+[^>]*?file=([^\s\>]+)[^>]*?>/i ) {
	    my $n,$f,$s;
	    my $hdr = $1;
	    my $tag = $2;
	    my $anchor = $3;
	    my $db = $self->{'header db'};
	    ($n,$f,$s) = fileparse($hdr,@{$self->{'File Extensions'}});
	    my $test = $$db{"long*$n"};
	    if ( $hdr && $test) {
		$file = $self->htmlpath("$n.$self->{'HTML Extension'}");
		if ($tag && $anchor) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file#$anchor">/i;
		} elsif ( !$tag ) {
		    $$comment[$i] =~ s/<linkto.*?>/<a href="$file">/i;
		} else {
		    print STDERR "Can't do func reference in generating $self->{'file'}:\n";
		    print STDERR "\t$$comment[$i]\n";
		    last;
		}
	    } else {
		print STDERR "Can't do func reference in generating $self->{'file'}:\n";
		print STDERR "\t$$comment[$i]\n";
		last;
	    }
	}
	$$comment[$i] =~ s|</linkto>|</a>|ig;

	##
	## Strip out linkfrom
	##
	if ( $$comment[$i] =~ /<linkfrom.*?>/i ) {
	    if ( ! ($$comment[$i] =~ s|<linkfrom.*?>.*?</linkfrom>||i) ) {
		$$comment[$i] =~ s|<linkfrom.*?>.*$||i;
		for ($j=$i+1; $j < scalar(@$comment); ++$j) {
		    if ( $$comment[$j] =~ s|.*?</linkfrom>||i ) {
			last;
		    } else {
			push(@linkfromstack,splice(@$comment,$j,1));
			--$j;
		    }
		}
		if ( $j >= scalar(@$comment) ) {
		    print STDERR "Error: <linkfrom>/</linkfrom> mismatch generating \"";
		    print STDERR $self->{'file'} . "\"! (ignoring)\n" ;
		    push(@$comment,@linkfromstack);
		} else {
		    @linkfromstack = ();
		}
	    }
	}

	##
	## Misc.
	##
	$$comment[$i] =~ s|</?module>||gi;
	if ( $DOAIPS ) {
	    $$comment[$i] =~ s|<prerequisite>|<h3>Prerequisite</h3><ul>|ig;
	    $$comment[$i] =~ s|</prerequisite>|</ul>|ig;
	    $$comment[$i] =~ s|<motivation>|<h3>Motivation</h3>|ig;
	    $$comment[$i] =~ s|</motivation>||ig;
	    $$comment[$i] =~ s|<synopsis>|<h3>Synopsis</h3>|ig;
	    $$comment[$i] =~ s|</synopsis>||ig;
	    $$comment[$i] =~ s|<example>|<h3>Example</h3>|ig;
	    $$comment[$i] =~ s|</example>||ig;
	    $$comment[$i] =~ s|<etymology>|<h3>Etymology</h3>|ig;
	    $$comment[$i] =~ s|</etymology>||ig;
	    $$comment[$i] =~ s|<thrown>|<h3>Thrown Exceptions</h3><ul>|ig;
	    $$comment[$i] =~ s|</thrown>|</ul>|ig;
	    if ( $$comment[$i] =~ /<todo\s*(?:asof\s*=\s*["']?(.*?)['"]?.*?)?>/ ) {
		my $date = $1;
		if ( $date ) {
		    $$comment[$i] =~ s|<todo.*?>|<h3>To Do ($date)</h3><ul>|ig;
		} else {
		    $$comment[$i] =~ s|<todo.*?>|<h3>To Do</h3><ul>|ig;
		}
	    }
	    $$comment[$i] =~ s|</todo>|</ul>|ig;
	    if ( $$comment[$i] =~ /<templating\s*(?:arg\s*=\s*(\w+).*?)?>/ ||
		 $$comment[$i] =~ /<templating\s*(?:arg\s*=\s*["']([^'"]+)['"].*?)?>/ ) {
		my $arg = $1;
		if ( $arg ) {
		    $$comment[$i] =~ s|<templating.*?>|<h3>Template Type Argument Requirements ($arg)</h3><ul>|ig;
		} else {
		    $$comment[$i] =~ s|<templating.*?>|<h3>Template Type Argument Requirements</h3><ul>|ig;
		}
	    }
	    $$comment[$i] =~ s|</templating>|</ul>|ig;
	    if ( $$comment[$i] =~ /<use\s*(?:visibility\s*=\s*["']?(.*?)['"]?.*?)?>/ ) {
		my $arg = $1;
		if ( $arg =~ /export/i ) {
		    $$comment[$i] =~ s|<use.*?>|<h3>Exported</h3>|ig;
		} elsif ( $arg =~ /local/i ) {
		    $$comment[$i] =~ s|<use.*?>|<h3>Local Use Only</h3>|ig;
		} else {
		    $$comment[$i] =~ s|<use.*?>||ig;
		}
	    }
	    $$comment[$i] =~ s|</use>||ig;
	    if ( $$comment[$i] =~ /<reviewed\s*(.*?)\s*>/i ) {
		$attr = $1;
		@attr = split(/(?:=["'][^'"]*['"]|=[^\s>]*)\s*/,$attr);
		$attr =~ s/.*?=//;
		my @val = split(/\s*\w+=/,$attr);
		my %attr = ();
		my $attrcnt = 0;
		while ( ($_ = shift(@attr)) && ($val = shift(@val)) ) {
		    if ( /^reviewer$/ || /^date$/ ) {
			$val =~ s/^["']\s*//;
			$val =~ s/\s*["']$//;
			if ( $val ) {
			    $attr{$_} = $val;
			    $attrcnt++;
                        }
		    }
		    if ( /^tests$/ || /^demos$/ ) {
			$val =~ s/^["']\s*//;
			$val =~ s/\s*["']$//;
			if ( $val ) {
			    my @list = split(/[, ]+/,$val);
			    $attr{$_} = \@list;
			    $attrcnt++;
			}
		    }
		}
		if ( $attrcnt ) {
		    my $str = "<h3>Review Status</h3>\n<dl>\n";
		    $str .= "<dt>Reviewed By:\n<dd> " . $attr{'reviewer'} . "\n"
			if defined $attr{'reviewer'};
		    $str .= "<dt>Date Reviewed:\n<dd> " . $attr{'date'} . "\n"
			if defined $attr{'date'};
		    if ( defined $attr{'demos'} || defined $attr{'tests'} ) {
			$str .= "<dt> Programs:\n";
			foreach $t ('demos', 'tests') {
			    my $d = $attr{$t};
			    if ( scalar(@$d) ) {
				my $tx = ucfirst $t;
				$str .= "<dd>$tx:\n<ul>\n";
				foreach ( @$d ) { $str .= "<li> $_\n"; }
				$str .= "</ul>";
			    }
			}
		    }
		    $str .= "</dl>\n";
		    $$comment[$i] =~ s/<reviewed(.*?)>/$str/i;
		} else {
		    $$comment[$i] =~ s/<reviewed(.*?)>//i;
		}
	    }
	    $$comment[$i] =~ s|</reviewed>||ig;
        }

	if ( $DODEPR ) {
	    $$comment[$i] =~ s|<div>|<p>|ig;
	    $$comment[$i] =~ s|</div>||ig;
	    $$comment[$i] =~ s|<title>|<h3>|ig;
	    $$comment[$i] =~ s|</title>|</h3>|ig;
	}
    }

    if ( $commentIsScalar ) {
	$$commentIsScalar = join(' ',@$comment);
    } elsif ( ! $reallines ) {
	@$comment = ();
    }
}

##
## **MODIFIES DBM FILE**
##
sub createsection {
    my $self = shift;
    my $class = @_[0];
    my $header = @_[1];
    my $comment = @_[2];
    my $source = $self->htmlpath(@_[3]);
    my $db = $self->{'header db'};
    my $str = "";
    my $cnt = 0;
    my @parent = ();
    my $refname = "";
    my $reffile = "";
    my $outfile = "";
    my $outname = "";
    my $path = "";
    my $suffix = "";


    $$db{"link*$class"} = "$self->{'output'}{'path'}$self->{'file'}";
    $header =~ s|//.*||;
    ($str = $header) =~ s/.*(class|struct)\s+\Q$class\E\s*(?:<.*?>\s*)?:?\s*(.*)/$2/;
    if ( $1 =~ m/struct/ ) {
	$self->{'class section'} = "public";
    }

    @parent = split(/\s*,\s*/,$str);
    
    foreach (@parent) {
	s/<.*>//g;
	s/^\s+//;
	s/\s+\n?$//;
	s/^(?:virtual\s*)?(?:private|protected|public)\s*(?:virtual\s*)?//;
	if ( /^\s*$/ ) {
	    splice(@parent,$cnt--,1);
	} else {
	    splice(@parent,$cnt,1,$_);
	}
    } continue { ++$cnt; }

    $$db{"parent*$class"} = join(',',@parent);

    foreach $parent ( @parent ) {
	if ( defined $$db{"enums*$parent"} ) {
	    @enums = split(/,/, $$db{"enums*$parent"});
	    foreach $enum ( @enums ) {
		$$db{"enum*$class\:\:$enum"} = $$db{"enum*$parent\:\:$enum"};
	    }
	}
    }

    $header =~ s/&/&amp;/g;
    $header =~ s/"/&quot;/g;
    $header =~ s/</&lt;/g;
    $header =~ s/>/&gt;/g;
    foreach (@parent) {
	($refname = $$db{"class*$_"}) =~ s/.*?:([^:]+)$/$1/;
        if ( $refname ) {
	  ($outname,$path,$suffix) = fileparse($refname,@{$self->{'File Extensions'}});
	  if ( $outname ) {
	    $outfile = $self->htmlpath("$outname.$self->{'HTML Extension'}");
	    $header =~ s@(\s+)$_(?![a-zA-Z0-9_])@$1<a href="$outfile#$_">$_</a>@;
          }
	}
    }

    $header =~ s@(\s+)\Q$class\E(?![a-zA-Z0-9_])@$1<a name="$class" href="$source">$class</a>@;

    $TMP = $self->{'section'}{'class order'};
    push(@$TMP,$class);
	
    $self->{'section'}{$class} = {};
    $self->{'section'}{$class}{'header'} = "<h1>$header</h1>";

    $self->{'section'}{$class}{'members'} = [];
    $self->{'section'}{$class}{'description'} = [];
    $self->{'section'}{$class}{'type'} = 'class';

    $TMP = $self->{'section'}{$class}{'description'};
    push(@$TMP,"<h1><a name=\"$class:description\">Description</a></h1>\n");
    push(@$TMP,@$comment);
    $self->{'section'}{$class}{'contents'} = [];
    $self->{'section'}{$class}{'types'} = [];
}

sub createsimplesection {
    my $self = shift;
    my $class = @_[0];
    my $header = @_[1];
    my $comment = @_[2];
    my $source = $self->htmlpath(@_[3]);

    $self->relfile($source);
    $header = "Global Functions" if ! $header;
    $header =~ s|<here>||gi;
    $header =~ s|</here>||gi;
    $header = "<a name=\"$class\">$header</a> (<a href=\"$source\"><em>source</em></a>)";

    $TMP = $self->{'section'}{'class order'};
    push(@$TMP,$class);
	
    $self->{'section'}{$class} = {};
    $self->{'section'}{$class}{'header'} = "<h1>$header</h1>";

    $self->{'section'}{$class}{'members'} = [];
    $self->{'section'}{$class}{'description'} = [];
    $self->{'section'}{$class}{'type'} = 'globals';

    $TMP = $self->{'section'}{$class}{'description'};
    push(@$TMP,"<h1><a name=\"$class:description\">Description</a></h1>\n");
    push(@$TMP,@$comment);
    $self->{'section'}{$class}{'contents'} = [];
    $self->{'section'}{$class}{'types'} = [];
}

##
## **MODIFIES DBM FILE**
##
sub createenumsection {
    my $self = shift;
    my $class = @_[0];
    my $header = @_[1];
    my $comment = @_[2];
    my $source = $self->htmlpath(@_[3]);
    my $db = $self->{'header db'};
    my $str = "";
    my $cnt = 0;
    my @parent = ();
    my $refname = "";
    my $reffile = "";
    my $outfile = "";
    my $outname = "";
    my $path = "";
    my $suffix = "";


    $header =~ s/&/&amp;/g;
    $header =~ s/"/&quot;/g;
    $header =~ s/</&lt;/g;
    $header =~ s/>/&gt;/g;

    $header =~ s@(\s+)\Q$class\E(?![a-zA-Z0-9_])@$1<a name="$class" href="$source">$class</a>@;

    $TMP = $self->{'section'}{'class order'};
    push(@$TMP,$class);
	
    $self->{'section'}{$class} = {};
    $self->{'section'}{$class}{'header'} = "<h1>$header</h1>\n<blockquote>\n";

    $self->{'section'}{$class}{'members'} = [];
    $self->{'section'}{$class}{'description'} = [];
    $self->{'section'}{$class}{'type'} = 'enum';

    $TMP = $self->{'section'}{$class}{'description'};
    push(@$TMP,"</blockquote>\n<h1><a name=\"$class:description\">Description</a></h1>\n");
    push(@$TMP,@$comment);
    $self->{'section'}{$class}{'contents'} = [];
    $self->{'section'}{$class}{'types'} = [];
}

sub cannoparam {
    my $self = shift;
    my $sig = "";
    my $tmp = "";

    $_ = shift;

    s/(\([^)]*)\(/$1^@/g;		## PAREN
    s/\)([^(]*\))/@^$1/g;

    s/(\<[^>]*)\</$1^#/g;		## ANGLE
    s/\>([^<]*\>)/#^$1/g;

    ### Function parameter
    if ( /(.*?)\((.*?)\)\s*\((.*?)\)\s*(.*)/ ) {
	$sig =  $1 . "(*)" . "(" . $self->cannoparamlist($3) . ")" . $4;
    ### Regular parameter
    } elsif ( /(.*?(?:const)?\s*(?:[a-zA-Z0-9_]|::)+\s*(?:<[^>]*?>)?\s*[*&]*).*/ ) {
	$sig = $1;
    }

    $sig =~ s/^#/\</g;			## ANGLE
    $sig =~ s/#^/\>/g;

    $sig =~ s/^@/\(/g;			## PAREN
    $sig =~ s/@^/\)/g;

    return $sig;
}

sub cannoparamlist {
    my $self = shift;
    my $line = shift;
    my @sigs = ();
    my $cnt = 0;
    
    ##
    ## Avoid splitting inside of parens
    ##
    $line =~ s/(\([^)]*)\(/$1^@/g;		# PAREN
    $line =~ s/\)([^(]*\))/@^$1/g;

    $line =~ s/(\<[^>]*)\</$1^#/g;		# ANGLE
    $line =~ s/\>([^<]*\>)/#^$1/g;

    $line =~ s/(\([^)]*),([^)]*\))/$1\{@\}$2/g;	# PAREN
    $line =~ s/(\<[^>]*),([^>]*\>)/$1\{@\}$2/g;	# ANGLE

    $line =~ s/^@/\(/g;				# PAREN
    $line =~ s/@^/\)/g;

    $line =~ s/^#/\</g;				# ANGLE
    $line =~ s/#^/\>/g;

    @sigs = split( /\s*,\s*/, $line);
    
    foreach ( @sigs ) {
        s/\{@\}/,/;
	splice(@sigs,$cnt,1,$self->cannoparam($_));
    } continue { ++$cnt; }

    return join(",",@sigs);
}
        
sub cannosig {
  my $self = shift;
  my $sig = "";
  my $head = "";

    $_ = shift;

    if ( /^.*?($CxxOps)\s*\((.*)\)\s*((?:const)?)/ ) {
	$head = $1;
	$sig = "(" . $self->cannoparamlist($2) . ")" . $3;
    } elsif ( /^.*?([~a-zA-Z0-9_]+)\s*\((.*)\)\s*((?:const)?)/ ) {
	$head = $1;
	$sig =  "(" . $self->cannoparamlist($2) . ")" . $3;
    }

    $sig =~ s/(?:^|\s+)virtual\s+//;
    $sig =~ s/^\s+//;
    $sig =~ s/\s+$//;
    $sig =~ s/\s*([,)(><*&=!\/|%~])\s*/$1/g;
    $sig =~ s/\s+/ /g;

#   $sig =~ s/(\<[^>]*)\</$1^#/g;		# ANGLE
#   $sig =~ s/\>([^<]*\>)/#^$1/g;
#   $sig =~ s/^(?:[A-Za-z0-9_]+(?:<[^>]*?)?[>\s*&:]+)+(?!\()//;
#   $sig =~ s/^#/\</g;				# ANGLE
#   $sig =~ s/#^/\>/g;

    return $head . $sig;
};

sub startenumentry {
    my $self = shift;
    my $class = @_[0];
    my $element = @_[1];
    my $comment = @_[2];
    my $section = @_[3];
    my $sig = "";
    my $tag = "";
    my $mtag = "";

    $element =~ s/^\s+//;
    $element =~ s/\s+$//;
    $element =~ s/\s{2,}/\ /;
    $sig = $element;

    if ( $sig ) {

	($name = $sig) =~ s/^enum\s+([a-zA-Z0-9_]+).*$/$1/;
	$sig =~ s@(\s+)\Q$name\E(?![a-zA-Z0-9_])@$1<a href="#$class:$name"><b>$name</b></a>@;

	$TMP = $self->{'section'}{$class}{'types'};

	push(@$TMP, "<h3><a name=\"$class:type:$name\">$sig</a></h3>\n<blockquote>");
	push(@$TMP, "<dl>");

	$TMP = $self->{'section'}{$class}{'members'};
	push(@$TMP,"<h3><a name=\"$class:$name\">$element</a></h3>");
	push(@$TMP,@$comment);
    }
}

sub endenumentry {
    my $self = shift;
    my $class = shift;

    $TMP = $self->{'section'}{$class}{'types'};
    push(@$TMP, "</dl>");
    push(@$TMP, "</blockquote>");
}
	    
sub sectionentry {
    my $self = shift;
    my $class = @_[0];
    my $element = @_[1];
    my $comment = @_[2];
    my $section = @_[3];
    my $db = $self->{'header db'};
    my $sig = $self->cannosig($element);
    my $tag = "";
    my $mtag = "";

    if ( $sig ) {

	$TMP = $self->{'section'}{$class}{'contents'};

	if ($self->{'class section'} ne $section) {

	    ##
	    ## insert some space
	    ##
	    if (!$self->{'first class section'}) {
		push(@$TMP,"</dl>  \n");
	    } else {
		$self->{'first class section'} = 0;
	    }

	    if ($section) {
		push(@$TMP,"<dd> <b>" . ucfirst $section . " Members</b>\n");
		$self->{'class section'} = $section;
		push(@$TMP,"<dl>\n");
	    } elsif ( ! $self->{'class section'} ) {
		push(@$TMP,"<dd> <b>Private Members</b>\n");
		push(@$TMP,"<dl>\n");
		$self->{'class section'} = "private";
	    }
	}

	if ( $sig =~ /^($CxxOps)/ ) {
	    $mtag = $1;
	    ($othercls = $element) =~ s/^(.+\Q$mtag\E)\s*\(.*/$1/;
	    $othercls .= $1 if $sig =~ m/^.*\Q$mtag\E\s*(\(.*)$/;
	    $mtag =~ s/&/&amp;/g;
	    $mtag =~ s/"/&quot;/g;
	    $mtag =~ s/</&lt;/g;
	    $mtag =~ s/>/&gt;/g;
	    $tag = $mtag;
	    $sig =~ s/^operator\s+/operator/;
	} else {
	    ($tag = $sig) =~ s/([^(]+?)\(.*/$1/;
	    $mtag = $tag;
	    ($othercls = $element) =~ s/^(.+)\(.*/$1/;
	    $othercls .= $1 if $sig =~ m/^.*(\(.*)/;
	}

	while ( $othercls =~ s/(?:\W\Q$mtag\E\W|^\Q$mtag\E\W|\W\Q$mtag\E$|\W\Q$class\E\W|^\Q$class\E\W|\W\Q$class\E$|int|float|double|short|char|void|unsigned|static|const|bool|virtual|signed|volatile|new|delete|inline|template|extern|struct|register|operator)/ /g ) {}
	$othercls =~ s/(?:Int|uInt|Float|Double|Char|uChar|Short|uShort|Long|uLong|lDouble|Bool)//g;
	$othercls =~ s/[^\w\d_]+/ /g;

	while ($othercls =~ s/(?:^|\s+)([\w\d_]+)\s+(.*?)\1/ $1 $2/g) {}	## get rid of redundant classes
	$othercls =~ s/^\s+//;
	$othercls =~ s/\s+$//;
	$othercls =~ s/\s+/ /g;
##	print "-->\t'$othercls'\n";
	
	$element =~ s/&/&amp;/g;
	$element =~ s/"/&quot;/g;
	$element =~ s/</&lt;/g;
	$element =~ s/>/&gt;/g;
	$sig =~ s/&/&amp;/g;
	$sig =~ s/"/&quot;/g;
	$sig =~ s/</&lt;/g;
	$sig =~ s/>/&gt;/g;

	@othercls = split(/ /,$othercls) if $othercls;
	@where_options = ( "class*", "enum*$class\:\:", "enum*" );
	$option_cnt = 0;
	$found_where = 0;
	@outfiles = ();
	$clscnt = 0;
	foreach ( @othercls ) {
	    $option_cnt = 0;
	    $found_where = 0;
	    foreach $opt ( @where_options ) {
		if ( defined $$db{"$opt$_"} && ($file = $$db{"$opt$_"}) ) {
		    $file =~ s/.*?:([^:]+)$/$1/;
		    ($outname,$path,$suffix) = fileparse($file,@{$self->{'File Extensions'}});
		    if ( $outname ) {
			$outfile = $self->htmlpath("$outname.$self->{'HTML Extension'}");
			push(@outfiles,"$option_cnt:*:$outfile");
			$element =~ s@(^|[^A-Za-z0-9_])$_(?![a-zA-Z0-9_])@$1&$clscnt&@g;
			$centry =~ s@(^|[^A-Za-z0-9_])$_(?![a-zA-Z0-9_])@$1&$clscnt&@g;
			$found_where = 1;
			last;
		    }
		}
	    } continue { ++$option_cnt; }
	    if ( ! $found_where ) {
	       splice(@othercls,$clscnt--,1);
	    }
	} continue { ++$clscnt }
		
	($centry = $element) =~ s|\Q$mtag\E|<a href="#$class:$sig">$tag</a>|;
	$element =~ s|\Q$mtag\E|<a name="$class:$sig">$tag</a>|;
	
	$clscnt = 0;
	foreach ( @othercls ) {
	    ($opt, $outfile) = split( /:\*:/, shift(@outfiles));
	    if ( $opt == 0 || $opt == 2 ) {
		($s = $_) =~ s/::/:type:/g;
		$element =~ s@&$clscnt&@<a href="$outfile#$s">$_</a>@g;
		$centry =~ s@&$clscnt&@<a href="$outfile#$s">$_</a>@g;
	    } elsif ( $opt == 1 ) {
		($cls = $$db{"$where_options[1]$_"}) =~ s/^.*?:([^:]+):[^:]+$/$1/;
		$element =~ s@&$clscnt&@<a href="$outfile#$cls:type:$_">$_</a>@g;
		$centry =~ s@&$clscnt&@<a href="$outfile#$cls:type:$_">$_</a>@g;
	    }
	} continue { ++$clscnt }

	push(@$TMP,"<dd> $centry");

	$didtransition = 0;
	while ( $self->{'group'}{'transition'} ) {
	    $didtransition = 1;

 	    my $stack = pop(@{$self->{'group'}{'last header glop'}});
	    ##
	    ## Sometimes there is nothing to dump
	    ##
	    if ( scalar(@$stack) ) {
		$TMP = $self->{'section'}{$class}{'members'};
		$commentstack = $self->{'group'}{'comment stack'};
		my $lastcomment = pop(@{$self->{'group'}{'last comment glop'}});
		my $lastanchor = pop(@{$self->{'group'}{'last anchor'}});

		push(@$TMP,"<h3>");
		push(@$TMP,join("<br>\n",@$stack));
		push(@$TMP,"</h3>");

		if (scalar(@$commentstack)) {
		    my $limit = $self->{'group'}{'started group'} ? 
					scalar(@$commentstack) - 1 :
					scalar(@$commentstack);
		    for ($i=0; $i < $limit; ++$i) {
			$tmp = $$commentstack[$i];
			push(@$TMP,@$tmp);
			push(@$TMP,"<p>");
		    }
		}
		push(@$TMP,@$lastcomment);
	    }
	    push(@$TMP,"</a>") if $$lastanchor[0];
	} continue {
	    $self->{'group'}{'transition'} -= 1;
	}

	if ( $self->{'group'}{'in group'} ) {
	    $anchorstack=$self->{'group'}{'anchor stack'};
	    $top = $$anchorstack[$#$anchorstack];
	    if (!$$top[0] && $$top[1]) {
		$TMP = $self->{'section'}{$class}{'members'};
		push(@$TMP,"<a name=\"$$top[1]\">");
		$$top[0] = 1;
	    }
	}

	if ( $self->{'group'}{'in group'} && scalar(@$comment) && !$didtransition) {
	    my $commentstack = $self->{'group'}{'comment stack'};

	    $TMP = $self->{'section'}{$class}{'members'};
	    push(@$TMP,"<h3>$element</h3>");
	    if (scalar(@$commentstack)) {
		for ($i=0; $i < scalar(@$commentstack); ++$i) {
		    $tmp = $$commentstack[$i];
		    push(@$TMP,@$tmp);
		    push(@$TMP,"<p>");
		}
	    }
	    push(@$TMP,@$comment);
	} elsif ( $self->{'group'}{'in group'} ) {
	    $TMP = $self->{'section'}{$class}{'members'};
	    my $tmp = $self->{'group'}{'stack of header stack'};
	    my $stack = $$tmp[$#$tmp];
	    push(@$stack,"$element");
	} else {
	    $TMP = $self->{'section'}{$class}{'members'};
	    push(@$TMP,"<h3>$element</h3>");
	    push(@$TMP,@$comment);
	}
    }
}

sub enumsectionentry {
    my $self = shift;
    my $class = @_[0];
    my $element = @_[1];
    my $comment = @_[2];
    my $section = @_[3];
    my $where = @_[4];
    my $db = $self->{'header db'};
    my $sig = $element;
    my $tag = "";
    my $mtag = "";

    $where = 'contents' if ! $where;
    $sig =~ s/\s//g;
    $sig =~ s/=/ = /;

    if ( $sig ) {
	$sig =~ s@^([a-zA-Z0-9_]+)@<b>$1</b>@;
	$TMP = $self->{'section'}{$class}{$where};
	push(@$TMP,"<dt> $sig");
	push(@$TMP,"<dd>");
	push(@$TMP,@$comment);
	push(@$TMP,"</dd>");
    }
}

sub funcsectionentry {
    my $self = shift;
    my $class = @_[0];
    my $file = @_[1];
    my $lbl = @_[2];
    my $text = @_[3];

    if ( $file && $lbl ) {

	($outname,$path,$suffix) = fileparse($file,@{$self->{'File Extensions'}});
	if ( $outname ) {
	    $outfile = $self->htmlpath("$outname.$self->{'HTML Extension'}");
	    $text = "<here>Global Functions</here>" if !$text;
	    if ( $text =~ m|<here>|i && $text =~ m|</here>|i ) {
		($header = $text) =~ s|<here>|<a href="$outfile#$lbl">|ig;
		$header =~ s|</here>|</a>|ig;
	    } else {
		$header = "$text (<em><a href=\"$outfile#$lbl\">functions</a></em>)";
	    }
	}

	$TMP = $self->{'section'}{$class}{'contents'};

	if ($self->{'first global function'}) {
  
	    ##
	    ## insert some space
	    ##
	    if (!$self->{'first class section'}) {
		push(@$TMP,"</dl>  \n");
	    } else {
		$self->{'first class section'} = 0;
	    }
  
	    push(@$TMP,"<dd> <b> Global Functions</b>\n");
	    push(@$TMP,"<dl>\n");
	    $self->{'first global function'} = 0;
	}

	push(@$TMP,"<dd> $header");

    }
}

sub relsectionentry {
    my $self = shift;
    my $class = @_[0];
    my $file = @_[1];
    my $lbl = @_[2];
    my $text = @_[3];
    my $module = @_[4];

    if ( $file && $lbl ) {

	($outname,$path,$suffix) = fileparse($file,@{$self->{'File Extensions'}});
	if ( $outname ) {
	    $outfile = $self->htmlpath("$outname.$self->{'HTML Extension'}");
	    if ( $text ) {
		$self->fixcomment($class,\$text,1);
	    } else {
		$text = "<here>Related Information</here>";
	    }
	    if ( $text =~ m|<here>|i && $text =~ m|</here>|i ) {
		($header = $text) =~ s|<here>|<a href="$outfile#$lbl">|ig;
		$header =~ s|</here>|</a>|ig;
	    } else {
		$header = "$text (<em><a href=\"$outfile#$lbl\">click here</a></em>)";
	    }
	}

	if ($self->{'first see also'}) {
  
	    if ( ! $module ) {
		##
		## insert some space
		##
		$TMP = $self->{'section'}{$class}{'contents'};

		if (!$self->{'first class section'}) {
		    push(@$TMP,"</dl>");
		} else {
		    $self->{'first class section'} = 0;
		}
  
		push(@$TMP,"<dd> <b> See Also</b>");
		push(@$TMP,"<dl>");
		$self->{'first see also'} = 0;
	    } else {
		$TMP = $self->{'module doc'};
		push(@$TMP,"<h2>See Also</h2>");
		push(@$TMP,"<dl>");
		$self->{'first see also'} = 0;
	    }
	}

	push(@$TMP,"<dd> $header");
    }
}

sub flush {
    my $self = shift;
    my $class = @_[0];

    if ( $self->{'group'}{'in group'} ) {
	print STDERR "Error, unterminated group in class $class.\n";
	my $commentstack = $self->{'group'}{'comment stack'};
	my $stack_of_header_stacks = $self->{'group'}{'stack of header stack'};
	$self->{'group'}{'transition'} += 1;
	push(@{$self->{'group'}{'last comment glop'}},pop(@$commentstack));
	push(@{$self->{'group'}{'last header glop'}},pop(@$stack_of_header_stacks));
	push(@{$self->{'group'}{'last anchor'}},pop(@$anchorstack));
    }

    while ( $self->{'group'}{'transition'} ) {
	$stack = pop(@{$self->{'group'}{'last header glop'}});
	if ( scalar(@$stack) ) {
	    $lastcomment = pop(@{$self->{'group'}{'last comment glop'}});
	    $lastanchor = pop(@{$self->{'group'}{'last anchor'}});
	    $commentstack = $self->{'group'}{'comment stack'};

	    $TMP = $self->{'section'}{$class}{'members'};
	    push(@$TMP,"<h3>");
	    push(@$TMP,join("<br>\n",@$stack));
	    push(@$TMP,"</h3>");

	    if (scalar(@$commentstack)) {
		for ($i=0; $i < scalar(@$commentstack); ++$i) {
		    $tmp = $commentstack[$i];
		    unshift(@$comment,@$tmp);
		}
	    }
	    push(@$TMP,@$lastcomment);
	    push(@$TMP,"</a>") if $$lastanchor[0];
	}
    } continue {
	$self->{'group'}{'transition'} -= 1;
    }

    $self->{'first global function'} = 1;
    $self->{'first see also'} = 1;
}

sub contententry {
    my $self = shift;
    my $class = @_[0];
    my $summary = @_[1];
    my $type = @_[2];

    $TMP = $self->{'contents'};
    if ( $type eq 'G' ) {
	if ( $summary =~ m|<here>|i && $summary =~ m|</here>|i ) {
	    $summary =~ s|<here>|<a href="#$class">|gi;
	    $summary =~ s|</here>|</a>|gi;
	    push(@$TMP, "<dd> $summary (<em><a href=\"#$class:description\">full description</a></em>)");
	} else {
	    push(@$TMP, "<dd> <a href=\"#$class\"> Global Functions </a> -- $summary (<em><a href=\"#$class:description\">full description</a></em>)");
	}
    } else {
	if ( $summary =~ m|<here>|i && $summary =~ m|</here>|i ) {
	    $summary =~ s|<here>|<a href="#$class">|gi;
	    $summary =~ s|</here>|</a>|gi;
	    push(@$TMP, "<dd> $summary (<em><a href=\"#$class:description\">full description</a></em>)");
	} else {
	    push(@$TMP, "<dd> <a href=\"#$class\"> $class </a> -- $summary (<em><a href=\"#$class:description\">full description</a></em>)");
	}
    }
}

##
## Depends on:
##     $main'PACKAGE
##     $main'VERSION
##     $main'PATCHLEVEL
##
sub timestamp {
    my $self = shift;
    my $fref = $self->{'file ref'};
    my @TIME = localtime(time);	## ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)

    $TIME[5] += 1900;		## YEAR
    $TIME[4] += 1;		## MONTH
    for ($i=0;$i<5;$i++) { $TIME[$i] = "0$TIME[$i]" if $TIME[$i] < 10; }

    if (! ($self->{'User'})) {
	my @USER = getpwuid($<);
	$self->{'User'} = $USER[0];
    }
    print $fref "<!-- This file is generated automatically; modifications will be lost -->\n";
    if ( defined $main'PACKAGE ) {
	print $fref "<!-- Generated by $main'PACKAGE";
	print $fref " version $main'VERSION" if defined $main'VERSION;
	print $fref "PL$main'PATCHLEVEL" if defined $main'PATCHLEVEL;
	print $fref " -->\n";
    }
    print $fref "<!-- Created by user ",$self->{'User'}," at $TIME[5]/$TIME[4]/$TIME[3] $TIME[2]:$TIME[1]:$TIME[0] -->\n";
}

sub showpage {
    my $self = shift;
    my $fref = $self->{'file ref'};
    my $first = 0;

    print $fref "<html>\n";
    $self->timestamp();

    if ( $self->{'title'} ) {
	print $fref "<head>\n";
	print $fref "<title> ",$self->{'title'}," </title>\n";
	print $fref "</head>\n";
	print $fref "<body>\n";
	print $fref "<h1> ",$self->{'title'}," </h1>\n";
    } else {
	print $fref "<head>\n";
	print $fref "<title> HTML file generated by $0</title>\n";
	print $fref "</head>\n";
	print $fref "<body>\n";
	print $fref "<h1> HTML file generated by $0</h1>\n";
    }
    
    print $fref "<h1>Classes</h1>\n<dl>\n";

    $TMP = $self->{'contents'};
    foreach (@$TMP) {
	print $fref "$_\n" if ! /^\s*$/;
    }
    print $fref "</dl>\n";

    foreach $key (@{$self->{'section'}{'class order'}}) {

	print $fref "<hr>\n";

    	print $fref "\n";
	print $fref $self->{'section'}{$key}{'header'},"\n";

	$TMP2 = $self->{'section'}{$key}{'types'};
	print $fref "<h1>Types</h1>\n" if scalar(@$TMP2) && $self->{'section'}{$key}{'type'} ne 'enum';
	foreach (@$TMP2) {
	    print $fref "$_\n";
	}

	print $fref "<h1>Interface</h1>\n" if $self->{'section'}{$key}{'type'} ne 'enum';

	$TMP2 = $self->{'section'}{$key}{'contents'};
	print $fref "<dl>\n";
	foreach (@$TMP2) {
	    print $fref "$_\n";
	}
	print $fref "</dl>\n"		## Private/Protected/Public List
	    if $self->{'section'}{$key}{'type'} ne 'enum';
	print $fref "</dl>\n";		## Interface List

	$TMP2 = $self->{'section'}{$key}{'description'};
	foreach (@$TMP2) {
	    print $fref "$_\n";
	}
	if ( $self->{'section'}{$key}{'type'} ne 'enum' ) {
	    print $fref "<h1>Member Description</h1>\n";
	    $TMP2 = $self->{'section'}{$key}{'members'};
	    foreach (@$TMP2) {
		print $fref "$_\n";
	    }
	}
    }

    ##
    ## Insert Copyright, if available
    ##
    if ( scalar(@{$self->{'Copyright Notice'}}) ) {
	print $fref @{$self->{'Copyright Notice'}};
    }

    print $fref "</body>\n";
    print $fref "</html>\n";
}

##
## **MODIFIES DBM FILE**
##
sub createmodule {
    my $self = shift;
    my $module_name = @_[0];
    my $comment = @_[1];
    my $db = $self->{'header db'};

    $$db{"mlink*$module_name"} = "$self->{'output'}{'path'}$self->{'file'}";

    $TMP = $self->{'module doc'};
    $self->{'module name'} = $module_name;
    push(@$TMP,"<h2><a name=\"$module_name:description\">Description</a> (<em><a href=\"#$module_name:classes\">classes</a></em>)</h2>");
    push(@$TMP,@$comment);
}

sub moduleentry {
    my $self = shift;
    my $class = @_[0];
    my $h_file = @_[1];
    my $summary= @_[2];
    my $regexExt = join('|',@{$self->{'File Extensions'}});

    $TMP = $self->{'module contents'};
    unless ( $h_file =~ s/(?:$regexExt)$/\.$self->{'HTML Extension'}/ ) {
	$h_file = "$h_file.$self->{'HTML Extension'}";
    }
    $h_file = $self->htmlpath($h_file);
    if ( $summary =~ m|<here>|i && $summary =~ m|</here>|i ) {
	$summary =~ s|<here>|<a href="$h_file#$class">|gi;
	$summary =~ s|</here>|</a>|gi;
	$entry = "<dd> $summary (<em><a href=\"$h_file#$class:description\">full description</a></em>)";
    } else {
	$entry = "<dd> <a href=\"$h_file#$class\">$class</a>";
	$entry .= " -- $summary (<em><a href=\"$h_file#$class:description\">full description</a></em>)";
    }
    push(@$TMP,$entry);
}

sub showmodule {
    my $self = shift;
    my $fref = $self->{'file ref'};
    my $DOAIPS = $self->{'AIPS++ extensions'};

    print $fref "<html>\n";
    $self->timestamp();

    if ( $self->{'title'} ) {
	print $fref "<head>\n";
	print $fref "<title> ",$self->{'title'}," </title>\n";
	print $fref "</head>\n";
	print $fref "<body>\n";
	print $fref "<h1> ",$self->{'title'}," </h1>\n";
    } else {
	print $fref "<head>\n";
	print $fref "<title> HTML module file generated by $0</title>\n";
	print $fref "</head>\n";
	print $fref "<body>\n";
	print $fref "<h1> HTML module file generated by $0</h1>\n";
    }

    if ($DOAIPS) {
        # The one but last part of the directory is the package.
	my $dir = $self->{'output'}{'path'};
	$dir =~ s|/+|/|g;
	my @dirs = split (/\//, $dir);
	if ($#dirs > 0) {
	    $dir = $dirs[$#dirs - 1];
	}
	my $name = $self->{'file'};
	$name =~ s|\.[^.]+$||;
        print $fref "Changes made in the current development cycle can be\n";
	print $fref "found in the <A HREF=\"../../project/releasenotes/";
	print $fref "changelogs/changelog_Library_${dir}_$name.html";
        print $fref "\">changelog</A>.\n<P>\n";
    }

    $TMP = $self->{'module doc'};
    foreach (@$TMP) {
	print $fref "$_\n";
    }
    print $fref "</dl>\n" if ! $self->{'first see also'};

    print $fref "<hr>\n";
    print $fref "<h2><a name=\"$self->{'module name'}:classes\">Classes</a></h2>\n";
    print $fref "<dl>\n";
    $TMP = $self->{'module contents'};
    foreach (@$TMP) {
	print $fref "$_\n";
    }
    print $fref "</dl>\n";

    ##
    ## Insert Copyright, if available
    ##
    if ( scalar(@{$self->{'Copyright Notice'}}) ) {
	print $fref @{$self->{'Copyright Notice'}};
    }

    print $fref "</body>\n";
    print $fref "</html>\n";
}

sub DESTROY {
    my $self = shift;
    if ($self->{'open'}) {
	close $self->{'file ref'};
	$self->{'open'} = 0;
    }
}

sub uudecode {
    my $in = shift;
    my $out = "";

    foreach( split(/\n/,$in) ) {
	$_ .= "\n";
	last if /^end/;
	next if /[a-z]/;
	next unless int((((ord() - 32) & 077) + 2) / 3) == int(length() / 4);
	$out .= unpack("u",$_);
    }
    return $out;
}

sub set_bitmaps {
    my $self = shift;

my $warning_encode = <<'EndOfGif';
begin 644 warning.gif
M1TE&.#EA(``@`/(``("`@````/__`/\``/___P```````````"'Y!`$`````
M+``````@`"````.["+K<SN%)&&IL0=SIPOC?!F2"QF%@>I&E>8X@(1,AV[J<
M-\QR?;>BAXXW&]B`)]V.2#B6@L(/L_G30#M2IO.*S?*<QA=L^:UZN!#O;$L+
M3Y1$=L\=)<OD<RY\;6:&NG9W+`%3($%#4S:$?G1C4U0_BT1_8X%WD9*3;HAQ
MD8]%89Q\0)F/?Z*73Y^:$:B#JZR.HT"PH*V(BJ6?E)6#2BG`P2(60,'&P!48
5-Q;,S<[)RD]H8@LDT-1).-@G"0`[
`
end
EndOfGif
my $warning_gif = uudecode($warning_encode);

my $tip_encode = <<'EndOfGif';
begin 644 tip.gif
M1TE&.#EA(``@`/(``("`@````/\``/___P``_P```````````"'Y!`$`````
M+``````@`"````.F"+JL\?"U21<4.&-8Z],@*'46.)SH*9)`D*4PG`6=*\0#
MH>/#1MDXG9#G8P"#0@*O)Z"5EDGELEFZ07=+IO.8[6H=UJXT2^5>Q\2F.1;M
M;M:P-EF-$0_GGS`OZ:;KD5A3?G:!:30NA&@Q17`I?();=6>*,E1@?XYW.$67
M>W*53D:2<9\IG**87J<--JF;JQ.MHY4S+`ZR(1HCMK>X,[N\K!$<P<7!"0`[
`
end
EndOfGif
my $tip_gif = uudecode($tip_encode);

my $caution_encode = <<'EndOfGif';
begin 644 caution.gif
M1TE&.#EA(``@`/(``("`@````/\``/___P``_P```````````"'Y!`$`````
M+``````@`"````.="+K<_C"&.:-]06@=K@?9MG4?%(ICB6E#VW)J<[HO'(-;
M2^RU0*HAW8XWL)6"PJ'+>)DEB46FA.4:0J._J8`VL!)H*>V6YN6&5V.NDHLU
M4=EKLQ3W5E_!4B2[>\=G]7MQ;&%.>X:#/@J`AXPU'8MP?7*/=9&"B)1I@5:,
D,(6;7X>$.8VB1B<HJ:JI?ZBKKQQ9"Q2TM;:U-[FZN[P."0`[
`
end
EndOfGif
my $caution_gif = uudecode($caution_encode);

    $self->{'warning gif'} = \$warning_gif;
    $self->{'caution gif'} = \$caution_gif;
    $self->{'tip gif'} = \$tip_gif;
}
1;
