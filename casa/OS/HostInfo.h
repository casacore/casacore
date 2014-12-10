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

#ifndef CASA_HOSTINFO_H
#define CASA_HOSTINFO_H

#include <casacore/casa/aips.h>
#include <cstring>
#include <cstddef>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;
class HostMachineInfo;

// <summary>
// Miscellaneous information about this host and process.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> None
// </prerequisite>
//
// <synopsis>
// This class is meant to encapsulate miscellaneous information about the
// environment that the current process is running in.
// 
// At present, the class can be used to obtain the name of the host on which
// this process is running, the process id of this process, the amount of
// physical memory (total, used, & free) on this host, the amount of swap
// space (total, used, and free), the number of CPUs on this host, and a
// count of the number of seconds from January 1, 1970. Generally you
// should use the classes <linkto class=Time>Time</linkto> and
// <linkto class=MVTime>MVTime</linkto> if your primary interest is related
// to time and dates. If your interest is in timing, you would generally use
// class <linkto class=Timer>Timer</linkto>, which can also report CPU times
// as well as "clock" times.
//
// Determination of the number of CPUs, swap space, and physical memory is
// particularly OS dependent. In cases where this functionality has not yet
// been implemented, the memory and swap functions will return -1 and the
// CPU function will return 0.
// </synopsis>
//
// <example>
// <srcblock>
// cout << "I am process #" << HostInfo::processID() << " running on host " <<
//         HostInfo::hostName() << ", which has " << HostInfor::numCPUs( ) << " CPUs." << endl;
// cout << "This host has " << HostInfo::memoryTotal( ) << "K of memory [ " <<
//         HostInfo::memoryUsed( ) << " used, " <<
//         HostInfo::memoryFree( ) << " free ]." << endl;
// cout << "This host has " << HostInfo::swapTotal( ) << "K of swap space [ " <<
//         HostInfo::swapUsed( ) << " used, " <<
//         HostInfo::swapFree( ) << " free ]." << endl;
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
// <todo asof="2002/08/01">
//   <li> OS version?
// </todo>

class HostInfo
{
public:

    static String hostName();
    static Int processID();
    static Double secondsFrom1970();

    // Returns True for big endian machines (like SUN).
    // Returns False for little endian machines (like PC).
    static Bool bigEndian();

    // Returns 0 if unable to determine the number of CPUs.
    static Int numCPUs(bool use_aipsrc=false);

    // Get memory info (in KBytes).
    // Returns -1 if unable to determine memory info.
    // <group>
    static ptrdiff_t memoryTotal(bool use_aipsrc=false);
    static ptrdiff_t memoryUsed();
    static ptrdiff_t memoryFree();
    // </group>

    // Get swap space info (in KBytes).
    // Returns -1 if unable to determine swap info.
    // <group>
    static ptrdiff_t swapTotal();
    static ptrdiff_t swapUsed();
    static ptrdiff_t swapFree();
    // </group>

private:
    // we don't want folks creating these...
    HostInfo( );
    const HostInfo &operator=( const HostInfo & );

    static HostMachineInfo *info;
};


inline Bool HostInfo::bigEndian()
{
#if defined(AIPS_LITTLE_ENDIAN)
    return False;
#else
    return True;
#endif
}



} //# NAMESPACE CASACORE - END

#endif
