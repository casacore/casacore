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
package Path;
require 5.000;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(fullPath cwd syncCwd relPath Cwd CachedCwd);

sub Cwd {
    my $cwd = `pwd`;
    chop($cwd);
    $cwd =~ s|/$||;
    return $cwd;
}

sub CachedCwd {
    $cwd = Cwd() if ! $cwd;
    return $cwd;
}

sub fullPath {
    $path = shift;
    my $cwd = CachedCwd();

    $path =~ s/^\s*//;
    $path =~ s|/?\s*$||;
    if ( $path =~ /^\.\./ ) {
	$parent = $cwd;
	while ( $path =~ s|^\.\./?|| ) {
	    $parent =~ s|(.*)/[^/]+/?$|$1|;
	}
	$path = $parent . "/" . $path;
    }
    $path =~ s|^\.|$cwd|;
    $path = $cwd . "/" . $path if $path !~ m|^/|;
    return $path;
}

##
## Given two PATHS this returns the relative path from
## the first to the second.
##
sub relPath {
    my $from = fullPath(shift);
    my $to = fullPath(shift);
    my @from = split(/\//,$from);
    my @to = split(/\//,$to);

    while ( scalar(@from) && scalar(@to) ) {
	$from = shift(@from);
	$to = shift(@to);
	if ( $from ne $to ) {
	    unshift(@from,$from);
	    unshift(@to,$to);
	    last;
	}
    }

    if ( scalar(@to) && ! scalar(@from) ) {
	return join('/',@to);
    } elsif ( scalar(@to) && scalar(@from) ) {
	$from = '../' x scalar(@from);
	return $from . join('/',@to);
    } elsif ( ! scalar(@to) && scalar(@from) ) {
	$from = '../' x (scalar(@from)-1) . '..';
	return $from;
    }
    return '';
}

sub cwd {
    if (! $cwd ) {
	$cwd = `pwd`;
	chop($cwd);
	$cwd =~ s|/$||;
    }
    return $cwd;
}

sub syncCwd {
    $cwd = `pwd`;
    chop($cwd);
    $cwd =~ s|/$||;
}
1;
