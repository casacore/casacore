//# AppInfo.h: General information for applications
//# Copyright (C) 1996,1997
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

#if !defined(AIPS_APPLICATION_ENVIRONMENT_H)
#define AIPS_APPLICATION_ENVIRONMENT_H

//# Includes
#include <aips/aips.h>
#include <aips/Tasking/AipsrcValue.h>

//# Forward declarations
class String;

// <summary>
// General information for applications.
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="1997/10/30" tests="tAppInfo" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Aipsrc>Aipsrc</linkto> class
// </prerequisite>
//
// <synopsis>
// This class provides general information that an application might want to
// know about it's processing environment. This will be based either on
// information coded into <linkto class=Aipsrc>aipsrc</linkto> variables, or
// on information which can be obtained directly from some other source. For
// example, the time zone will generally be obtained from the host OS, but it
// can be over-ridden by an <linkto class=Aipsrc>aipsrc</linkto> variable if
// necessary.
//
// Generally speaking, this class is provided to hide the details of how the
// information is encoded into an <linkto class=Aipsrc>aipsrc</linkto> variables
// and to avoid having to change applications if the information moves from
// being coded into a variable to being deduced at runtime.
//
// It is expected that the information which is available from this class will
// accrete with time.
// </synopsis>
//
// <example>
// <srcBlock>
// if (AppInfo::nProcessors() > 1) {
//    ... do something clever in parallel ...
// } else {
//   ... serial code ...
// }
// </srcBlock>
// </example>
//
// <motivation>
// Further encapsulate information which is usually in aipsrc variables.
// </motivation>
//
// <thrown>
//   <li> AipsError if nProcessors() <= 0
//   <li> AipsError if memoryInMB() <= 0
//   <li> AipsError if abs(timeZone()) > 0.625 
// </thrown>
//
// <todo asof="1997/10/21">
//   <li> Figure out the memory/num processors from the OS?
// </todo>

class AppInfo {
public:
  // Memory in MB. Reports  <src>system.resources.memory</src> if it is set,
  // otherwise return 64 (considered to be the base level for AIPS++).
  static uInt memoryInMB();
  
  // Number of processors on the local host. Returns 
  // <src>system.resources.numcpu</src> if it is set, otherwise 1.
  static uInt nProcessors();
  
  // Return the local time zone offset in day fractions. This value has to be
  // added to UTC to get local time. Generally the OS supplied value will be 
  // used, however it can be overridden with
  // <src>system.time.tzoffset</src> if necessary.
  static Double timeZone();
  
private:
  //# Data
  static Bool need_init_p;
  static uInt memory_r;
  static uInt nproc_r;
  static uInt tz_r;
  //# Methods
  // Force an initialization of the AppInfo values.
  static void init();
};

//# Inlines

inline uInt AppInfo::memoryInMB() {if (need_init_p) init(); 
	return (uInt) AipsrcValue<Int>::get(memory_r);};
inline uInt AppInfo::nProcessors() {if (need_init_p) init();
	 return (uInt) AipsrcValue<Int>::get(nproc_r);};
inline Double AppInfo::timeZone() {if (need_init_p) init();
	 return AipsrcValue<Double>::get(tz_r);};

#endif
