//# version.h: Version information for AIPS++
//# Copyright (C) 1996,1997,1999,2000,2001,2002,2004
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

#ifndef CASA_VERSION_H
#define CASA_VERSION_H

//# Includes
#include <casa/aips.h>

//# Forward declarations
#include <casa/iosfwd.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Version information for AIPS++
// </summary>

// <use visibility=export>

// <reviewed reviewer="nkilleen" date="1996/10/24" tests="tversion.cc" demos="">
// </reviewed>

// <synopsis>
// VersionInfo is a small class that reports information on the  version
// of AIPS++ that an executable was linked with. Normally an application
// programmer won't use this class directly, rather it will be used by
// the ``Tasking'' system which will report this information in a standard
// way.
//
// The available information is:
// <ul>
//    <li> <src>majorVersion()</src>: Major version of AIPS++, changes about twice
//         a year.
//    <li> <src>minorVersion()</src>: Minor version of AIPS++, changes about three
//         times a day. (every exhale).
//    <li> <src>patch()</src>: Patch number of this release. Changes when a
//         bug-fix patch is created.
//    <li> <src>date()</src>: String representation of the date when this
//         release was created (when "exhale" was run).
//    <li> <src>info()</src>: Extra information about this release, e.g.
//         "beta release."
// </ul>
//
// Additionally, there is a <src>report()</src> member function which
// summarizes the above to an <src>ostream</src>.
//
// When released to end users, the minor number is always 0. On the other
// hand, along the development path the patch number is always zero. The
// <src>report()</src> member function takes advantage of this to reformat
// the version information to be more in line with what people are used to.
// For example, 07.247.00 becomes "0.7 (build 247)" and 08.000.5 becomes
// 0.8.5. Note that major version 10 will thus be reported as version
// 1.0
//
// The version information is maintained automatically by "exhale" and
// is made available at link time by the make system.
// </synopsis>
//
// <example>
// If you knew that a format change occurred at release 10.0 you could write
// code like:
// <srcBlock>
// if (VersionInfo::majorVersion() >= 10) {
//    ... process the new way
// } else {
//    ... process the old way
// }
// </srcBlock>
// Of course generally it would be better to provide a conversion program
// for the persistent data, rather than filling applications with tests
// like this.
// </example>
//
// <motivation>
// It is important for bug-reporting and other such purposes to be able
// to uniquely identify the version of AIPS++ that some problem occurs
// in.
// </motivation>
//
// <todo asof="1996/10/24">
// <li> It would be useful to document the compiler/platform as well.
// </todo>

class VersionInfo
{
public:
    // Major version of AIPS++, changes about twice a year.
    static int majorVersion();
    // Minor version of AIPS++, changes about three times a
    // day (every exhale).
    static int minorVersion();
    // Patch number of this release. Changes when a bug-fix patch is
    // created.
    static int patch();
    // String representation of the date when this release was created
    // (when "exhale" was run).
    static const char *date();
    // Extra information about this release, e.g. "beta release."
    static const char *info();
    // Summarize the above into an ostream. Note that an 
    // <src>ostringstream</src> can be converted to a 
    // <linkto class="String">String</linkto> via a constructor.
    // This information is NOT prepended with "AIPS++ version:" or anything
    // like that. You may wish to add this yourself. The date is also not
    // included.
    static void report(std::ostream &os);
};

//# Inlines ------------------------------------------------------------------
inline int VersionInfo::majorVersion()
{
    extern const int   aips_major_version;
    return aips_major_version;
}

inline int VersionInfo::minorVersion()
{
    extern const int   aips_minor_version;
    return aips_minor_version;
}

inline int VersionInfo::patch()
{
    extern const int   aips_patch_version;
    return aips_patch_version;
}

inline const char *VersionInfo::date()
{
    extern const char* aips_version_date;
    return aips_version_date;
}

inline const char *VersionInfo::info()
{
    extern const char* aips_version_info;
    return aips_version_info;
}


} //# NAMESPACE CASA - END

#endif


