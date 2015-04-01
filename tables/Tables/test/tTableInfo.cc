//# tTableInfo.cc: Test program for class TableInfo
//# Copyright (C) 1996,2000,2001
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
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for the Table classes
// </summary>

// This program tests the class TableInfo.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


void writeInfo()
{
    TableInfo info;
    info.setType ("infotype");
    info.setSubType ("infosubtype");
    info.readmeAddLine ("first line");
    info.readmeAddLine ("second line");
    info.readmeAddLine ("third line\nfourth line");
    info.flush ("tTableInfo_tmp.data");
}

void readInfo()
{
    TableInfo info("tTableInfo_tmp.data");
    cout << info.type() << endl;
    cout << info.subType() << endl;
    cout << info.readme() << endl;
}


int main()
{
    try {
	writeInfo();
	readInfo();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
