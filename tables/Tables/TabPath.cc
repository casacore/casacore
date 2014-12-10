//# TabPath.cc: Search path for table files
//# Copyright (C) 1993,1994,1995,1998
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

//# Includes
#include <casacore/tables/Tables/TabPath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>                    // for system call access

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# This is the implementation of the class TabPath.

TabPath::TabPath()
: tabDir_p (10)
{
    tabDir_p[0] = "./";
    tabDir_p[1] = "~/TabDir/";
    nrDir_p     = 2;
}

TabPath::TabPath (const String& dir)
: tabDir_p (10)
{
    tabDir_p[0] = dir;
    nrDir_p     = 1;
}

TabPath::~TabPath ()
{ ; }


Bool TabPath::found (const String& name, String& dir) const
{
    uInt dirnr;
    Bool sw = False;
    for (dirnr=0; dirnr<nrDir_p; dirnr++) {
	if (access ((tabDir_p[dirnr] + name).chars(), R_OK) == 0) {
	    sw = True;             // found
	    break;
	}
    }
    dirnr = 0;                     // use first directory for new tables
    dir   = tabDir_p[dirnr];
    return sw;
}


// <thrown>
//   <li> indexError<Int>
// </thrown>
const String& TabPath::dir (uInt dirnr) const
{
    if (dirnr >= nrDir_p) {
	throw (indexError<Int> ((Int)dirnr, "TabPath"));
    }
    return tabDir_p[dirnr];
}

} //# NAMESPACE CASACORE - END

