//# tHostInfo.cc: Test the HostInfo class.
//# Copyright (C) 1997,2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

// For sleep().
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main()
{
    cout << "Host: " << HostInfo::hostName() << endl;
    cout << "Process ID: " << HostInfo::processID() << endl;
    Double s = HostInfo::secondsFrom1970();
    cout << "Time in seconds from 1970/1/1: " << s << " (about " <<
	Int(s/3600.0/24.0/365.25*10.0 + 0.5)/10.0 << " years)" << endl;
    cout << "Number of CPUs: " << HostInfo::numCPUs( ) << endl;
    cout << "Physical Memory: " << HostInfo::memoryTotal( ) <<
      "K [ " << HostInfo::memoryUsed( ) << " used, " <<
      HostInfo::memoryFree( ) << " free ]" << endl;
    FILE *aipsrc = fopen( "aipsrc", "w" );
    fprintf( aipsrc, "system.resources.cores: 1\n" );
    fprintf( aipsrc, "system.resources.memory: 512\n" );
    fclose( aipsrc );
    Aipsrc::reRead( );
    cout << "Number of CPUs (from ./aipsrc): " << HostInfo::numCPUs(true) << endl;
    cout << "Physical Memory (from ./aipsrc): " << HostInfo::memoryTotal(true) << "K" << endl;
    unlink( "aipsrc" );
    aipsrc = fopen( "aipsrc", "w" );
    fprintf( aipsrc, "system.resources.memfrac: 50\n" );
    fclose( aipsrc );
    Aipsrc::reRead( );
    cout << "50% Physical Memory (from ./aipsrc): " << HostInfo::memoryTotal(true) << "K" << endl;
    unlink( "aipsrc" );
    cout << "Swap Space: " << HostInfo::swapTotal( ) <<
      "K [ " << HostInfo::swapUsed( ) << " used, " <<
      HostInfo::swapFree( ) << " free ]" << endl;

    // OK, we can't do much of a test, but we can do a bit of one:
    Int id = HostInfo::processID();
    AlwaysAssertExit(id == HostInfo::processID()); // make sure no chang

    Int cpus = HostInfo::numCPUs( );
    AlwaysAssertExit(cpus == HostInfo::numCPUs( )); // make sure no chang

    Int memory = HostInfo::memoryTotal( );
    AlwaysAssertExit(memory == HostInfo::memoryTotal( )); // make sure no chang

    Double now = HostInfo::secondsFrom1970();
    sleep(5);
    Double diff = HostInfo::secondsFrom1970() - now;
    // Assume granularity could be as bad as 100ms
    AlwaysAssertExit(diff >= 4.9 && diff <= 5.1);

    // No good way to test hostName, other than using the same library call
    // that hostName is built upon!

    return 0;
}
