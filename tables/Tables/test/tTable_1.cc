//# tTable_1.cc: Test program for the SetupNewTable class
//# Copyright (C) 1994,1995,1996,2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/sstream.h>

#include <casacore/casa/namespace.h>
// <summary> Test program for the SetupNewTable class </summary>

// This program tests the class SetupNewTable and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a(uInt);

int main (int argc, const char* argv[])
{
    uInt nr = 500;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    try {
	a(nr);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

// First build a description.
void a(uInt nrrow) {
    // Build the table description.
    TableDesc td("tTableDesc","1",TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ScalarColumnDesc<Int> ("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<Int> ("ac"));
    td.addColumn (ScalarColumnDesc<uInt> ("ad","comment for ad"));
    td.addColumn (ScalarColumnDesc<float> ("ae"));
    td.addColumn (ScalarColumnDesc<String> ("af"));
    td.addColumn (ArrayColumnDesc<float> ("arr2",0));

    // Now create a new table from the description.
    SetupNewTable newtab("tTable_1_tmp.data", td, Table::New);
    // Create a storage manager for it.
    StManAipsIO sm1;
    StManAipsIO sm2;
//#//    StManKarma smk1();
    newtab.bindAll (sm1);
    newtab.bindColumn ("ab",sm2);
//#//    newtab.bindColumn ("ae",smk1);
//#//    newtab.bindColumn ("arr3",smk1);
    Table tab(newtab, nrrow);

    //# Do some erronous things.
    try {
	newtab.bindColumn ("ab", sm1);     // newtab is already in use
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } 
///    try {
///	Table tab2(newtab, 10);            // newtab is already in use
///    } catch (AipsError x) {
///	cout << x.getMesg() << endl;
///    } 
}
