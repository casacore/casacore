//# HostInfo.h: Miscellaneous information about this host and process.
//# Copyright (C) 1997,2002
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

#if !defined(AIPS_HOSTINFO_H)
#define AIPS_HOSTINFO_H

#include <aips/aips.h>

class String;
class HostMachineInfo;

// <summary>
// Miscellaneous information about this host and process.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> None
// </prerequisite>
//
// <synopsis>
// This class is meant to encapsulate miscellaneous information about the
// environment that the current process is running in.
// 
// At present, the class can be used to obtain the hostname the process is 
// running on, the process id of this process, and the a count of the number
// of seconds from January 1, 1970. Generally you should use the classes
// <linkto class=Time>Time</linkto> and <linkto class=MVTime>MVTime</linkto> if
// your primary interest is related to time and dates. If your interest is in
// timing, you would generally use class <linkto class=Timer>Timer</linkto>,
// which can also report CPU times as well as "clock" times.
// </synopsis>
//
// <example>
// <srcblock>
// cout << "I am process #" << HostInfo::processID() << " running on host " <<
//         HostInfo::hostName() << endl;
// Double now = HostInfo::secondsFrom1970();
// doSomething();
// cout << "Function doSomething() took " << 
//         HostInfo::secondsFrom1970() - now << " seconds to execute."  << endl;
// </srcblock>
// </example>
//
// <motivation>
// The <linkto class=ObjectID>ObjectID</linkto> class uses a combination of
// hostname, process id, time, and a sequence number to ensure that ObjectID's
// are distinct. This class encapsulates the way the first three of those items
// are obtained.
// </motivation>
//
// <todo asof="1997/10/07">
//   <li> OS version?
//   <li> Physical memory, number of CPU's?
// </todo>

class HostInfo
{
public:

    static String hostName();
    static Int processID();
    static Double secondsFrom1970();

    static Int numCPUs( );

    static Int memoryTotal();
    static Int memoryUsed();
    static Int memoryFree();

    static Int swapTotal();
    static Int swapUsed();
    static Int swapFree();

private:
    // we don't want folks creating these...
    HostInfo( );
    const HostInfo &operator=( const HostInfo & );

    static HostMachineInfo *info;
};

#endif
