//# OS.h: Classes for operating system services, and assorted other things
//# Copyright (C) 1995,1996,1998,1999,2001,2002
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
//# $Id$

#ifndef CASA_OS_H
#define CASA_OS_H

//# Includes
#include <casacore/casa/aips.h>

#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/DirectoryIterator.h>

#include <casacore/casa/OS/Time.h>
#include <casacore/casa/OS/Timer.h>

#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/DataConversion.h>
#include <casacore/casa/OS/CanonicalDataConversion.h>

#include <casacore/casa/OS/Memory.h>
#include <casacore/casa/OS/MemoryTrace.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// Classes for operating system services, and assorted other things
// </summary>

// <prerequisite>
//   <li> Nothing special
// </prerequisite>
//

// <reviewed reviewer="Paul Shannon" date="1995/06/02/ demos="">
// </reviewed>

// <etymology>
// 'OS' is the standard abbreviation for 'Operating System'.
// </etymology>
//
// <synopsis>
// This module's main purpose is to provide convenient and uniform
// access to operating system features:  environment variables, file
// and directory access, time and date services, and uniform data 
// representation (which solves the variety of ways the fundamental
// data types are represented in hardware).
// <p>
// The following functionality is available:
// <ul>
//  <li> Class <linkto class=EnvironmentVariable:description>
//       EnvironmentVariable</linkto>
//       for access to environment variables.
//  <li> Class <linkto class=Path:description>Path</linkto>,
//       <linkto class=RegularFile:description>RegularFile</linkto>,
//       <linkto class=SymLink:description>SymLink</linkto>, and
//       <linkto class=Directory:description>Directory</linkto>
//       for dealing with the file system.
//       Note that module <linkto module=IO>IO</linkto> deals with
//       reading and writing data to files and other IO streams.
//  <li> Class <linkto class=Time:description>Time</linkto>
//       to get the system time.
//  <li> Class <linkto class=Timer:description>Timer</linkto>
//       to measure elapsed, user, and system time of a piece of code.
//  <li> Framework <linkto class=Conversion:description>Conversion</linkto>
//       to convert data from one format to another. There are
//       classes to convert to/from
//       <linkto class=CanonicalConversion:description>canonical</linkto>
//       format,
//       <linkto class=VAXConversion:description>VAX</linkto> format, and
//       <linkto class=IBMConversion:description>IBM/360</linkto> format.
//       The structure of the framework is shown in the 
//       <a href="OS/OS_1.html">OMT diagram</a>.
//  <li> A class to encapsulate <linkto class=Memory>Memory</linkto> usage.
//       Class MemoryTrace makes it possible to trace all memory (de)allocations
//       through malloc and free (new and delete).
//       It only works on Linux systems though.
// </ul>

// </synopsis>

// <example>
// See the various class header files.
// </example>
//
// <motivation>
// We want to provide a simple and uniform interface to OS services and 
// features for the application and library programmer.  To pick a few
// examples:
// <ol>
//    <li> Recursive deletion of a directory.
//    <li> Access to time and date information.
//    <li> Get and set environment variables.
// </ol>
// </motivation>

// <todo asof="1995/06/02">
//   <li> The OS module is a bit fuzzy: for example, canonical data 
//        format conversion is more a matter of hardware than of operating 
//        system differences.  Perhaps these particular classes should be 
//        moved to a new module.
//   <li> Time and Date classes should be revised after studying
//        similar classes designed by others.  Roel Martinez is tenatively
//        scheduled to do this in the late summer of 1995.
//   <li> There was once some discussion of a 'VOS' (virtual operating system)
//        module.  This seems like a good idea.  Nested subdirectories for
//        specific operating systems would contain the implementations, but
//        a common interface would be used for all.  And perhaps local
//        site makedefs could be used to select the proper implementation
//        to link against, with no special action required of the programmer.
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
