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

#ifndef CASA_APPINFO_H
#define CASA_APPINFO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/System/AipsrcValue.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class String;
template<class T> class Vector;

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
// know about its processing environment. This will be based either on
// information coded into <linkto class=Aipsrc>aipsrc</linkto> variables, or
// on information which can be obtained directly from some other source. For
// example, the time zone will generally be obtained from the host OS, but it
// can be overridden by an <linkto class=Aipsrc>aipsrc</linkto> variable if
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
// <motivation>
// Further encapsulate information which is usually in aipsrc variables.
// </motivation>
//
// <thrown>
//   <li> AipsError if abs(timeZone()) > 0.625 
// </thrown>
//
// <todo asof="1997/11/11">
// </todo>

class AppInfo {
public:
    // Return a list of directory names into which the user may write data. If
    // <src>minimumFreeSpace</src> is set (>0) then only directories with at
    // least that much free space (in megabytes) are returned. If the aipsrc
    // variable <src>user.directories.work</src> is set, the candidate
    // directories are taken from that variable, otherwise the current working
    // directory (".")  is chosen if it exists and is writeable, otherwise /tmp
    // is the candidate. Only one of "." and "/tmp" is chosen, not both.
    //
    // If no suitable directories are found (i.e., writable directories with
    // enough free space), a zero-length vector is returned. A warning is
    // issued to the logging system for directories which do not exist or are
    // not writable.
    static Vector<String> workDirectories(uInt minimumFreeSpaceInMB=0);

    // Choose a workDirectory with at least <src>minimumFreeSpace</src> MB of
    // free space available. It uses <src>workDirectories</src>. If there is
    // more than one valid directory it arranges to choose different
    // directories in succession in an attempt to spread out the I/O. That is,
    // on the first call it will return directory1, on the second it will
    // return directory2, etc. in a cyclical fashion. One can imagine more
    // elaborate algorithms than this, however this should suffice for some
    // time, if not forever.
    // <thrown>
    //  <li> An <linkto class=AipsError>AipsError</linkto> is thrown if no
    //       directory with enough free space is found.
    // </thrown>
    static String workDirectory(uInt minimumFreeSpaceInMB=0);

    // This function returns a fully qualified filename for a non-existent file
    // in a work directory with enough free space. That is, you can create a
    // temporary file with the name returned from this function. This function
    // calls <src>workDirectory</src> and then appends a unique (file does not
    // exist) filename. By default the prefix of temporary file name is
    // <src>aipstmp_</src>, but you can override this if you choose.
    // <thrown>
    //  <li> An <linkto class=AipsError>AipsError</linkto> is thrown if no
    //       directory with enough free space is found.
    // </thrown>
    static String workFileName(uInt minimumFreeSpaceInMB=0,
			       const String &filenamePrefix="aipstmp_");
    
    // Return the local time zone offset in day fractions. This value has to be
    // added to UTC to get local time. Generally the OS supplied value will be 
    // used, however it can be overridden with
    // <src>system.time.tzoffset</src> if necessary.
    static Double timeZone();
private:
    //# Data
    static Bool need_init_p;
    static uInt tz_r;
    //# Methods
    // Force an initialization of the AppInfo values.
    static void init();
};

//# Inlines

inline Double AppInfo::timeZone() {if (need_init_p) init();
	                  return AipsrcValue<Double>::get(tz_r);}


} //# NAMESPACE CASACORE - END

#endif
