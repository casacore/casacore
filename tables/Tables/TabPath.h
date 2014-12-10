//# TabPath.h: Search path for table files
//# Copyright (C) 1993,1994,1995
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

#ifndef TABLES_TABPATH_H
#define TABLES_TABPATH_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Block.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Search path for table files
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <synopsis>
// TabPath is the class containing the search path for table files.
// It is used by the TabDesc class to find the directory of a table
// description.
// </synopsis>

// <todo asof="$DATE:$">
//  <li> This class has to be replaced by a more general path class.
// </todo>


class TabPath
{
public:
    // Create a table file search path with a .COD{Directory}
    // Use default path  ., ~/TabDir
    TabPath ();

    // Create a table file search path with given path name.
    TabPath (const String&);

    // Remove a table file search path.
    ~TabPath ();

    // Find a file in one of the directories.
    Bool found (const String&, String&) const;

    // Get the directory name.
    const String& dir (uInt dirnr) const;
   
private:
    Block<String>  tabDir_p;           // file directories
    uInt           nrDir_p;            // # directories
};



} //# NAMESPACE CASACORE - END

#endif
