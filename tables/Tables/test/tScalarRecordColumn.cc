//# tScalarRecordColumn.cc: Test program for the ScalarRecordColumn classes
//# Copyright (C) 1998,1999,2000,2001
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
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScaRecordColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the ScalarRecordColumn classes
// </summary>


void a()
{
    // Test constructors and assignment.
    ScalarRecordColumnDesc abd1("ab1");
    AlwaysAssertExit (abd1.comment() == "");
    ScalarRecordColumnDesc abd2("ab2", "comment");
    AlwaysAssertExit (abd2.comment() == "comment");
    AlwaysAssertExit (abd2.name() == "ab2");
    abd2 = abd1;
    AlwaysAssertExit (abd2.comment() == "");
    AlwaysAssertExit (abd2.name() == "ab1");
    AlwaysAssertExit (abd2.comment() == "");
    ScalarRecordColumnDesc abd3a("ab3", "comm1", "IncrementalStMan", "A1");
    ScalarRecordColumnDesc abd3(abd3a);
    AlwaysAssertExit (abd3.name() == "ab3");
    AlwaysAssertExit (abd3.comment() == "comm1");
    AlwaysAssertExit (abd3.dataManagerType() == "IncrementalStMan");
    AlwaysAssertExit (abd3.dataManagerGroup() == "A1");
    AlwaysAssertExit (abd3.dataType() == TpRecord);

    // Build and show the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (abd1);
    td.addColumn (abd3);
    td.show (cout);

    // Now create a new table from the description.
    SetupNewTable newtab("tScalarRecordColumn_tmp.data", td, Table::New);
    Table tab(newtab, 10);

    // Get the column ( consisting of empty records only).
    // Put the column.
    ScalarColumn<TableRecord> ab1(tab,"ab1");
    ScalarColumn<TableRecord> ab3(tab,"ab3");
    Vector<TableRecord> vec1 = ab1.getColumn();
    Vector<TableRecord> vec3 = ab3.getColumn();
    ab1.putColumn (vec1);
    TableRecord rec1;
    TableRecord rec3;
    uInt i;
    // Check if they are empty.
    // Fill each cell with a record (add a field for each row).
    AlwaysAssertExit (vec1.nelements() == 10);
    AlwaysAssertExit (vec3.nelements() == 10);
    for (i=0; i<10; i++) {
	AlwaysAssertExit (vec1(i).nfields() == 0);
	AlwaysAssertExit (vec3(i).nfields() == 0);
	AlwaysAssertExit (ab1.isDefined(i));
	AlwaysAssertExit (ab1(i).nfields() == 0);
	AlwaysAssertExit (ab3.isDefined(i));
	// In IncrementalStMan rows default to the previous one.
	if (i<2) {
	    AlwaysAssertExit (ab3(i).nfields() == 0);
	} else {
	    AlwaysAssertExit (ab3(i).nfields() == 1);
	}
	rec1.define (i, i);
	ab1.put (i, rec1);
	if (i%2 == 1) {
	    rec3.define ("fld1", i/2);
	    ab3.put (i, rec3);
	}
    }
    // Check if the column is correctly filled.
    for (i=0; i<10; i++) {
	ab1.get (i, rec1);
	AlwaysAssertExit (rec1.nfields() == i+1);
	for (uInt j=0; j<=i; j++) {
	    AlwaysAssertExit (rec1.asuInt(j) == j);
	}
	ab3.get (i, rec1);
	if (i==0) {
	    AlwaysAssertExit (rec1.nfields() == 0);
	} else {
	    AlwaysAssertExit (rec1.nfields() == 1);
	    AlwaysAssertExit (rec1.asuInt("fld1") == (i-1)/2);
	}
    }
    // Let us change rows 1, 5, and 9.
    Slicer cells(Slice(1,3,4));
    Vector<TableRecord> vec = ab3.getColumnRange (cells);
    AlwaysAssertExit (vec.nelements() == 3);
    for (i=0; i<3; i++) {
	vec(i).define ("fld2", i+10);
    }
    ab3.putColumnRange (cells, vec);
    for (i=0; i<10; i++) {
	rec1 = ab3(i);
	if (i==0) {
	    AlwaysAssertExit (rec1.nfields() == 0);
	} else {
	    if (i%4 == 1) {
		AlwaysAssertExit (rec1.nfields() == 2);
		AlwaysAssertExit (rec1.asuInt("fld2") == 10+i/4);
	    } else {
		AlwaysAssertExit (rec1.nfields() == 1);
	    }
	    AlwaysAssertExit (rec1.asuInt("fld1") == (i-1)/2);
	}
    }
    // At this point the table is destructed, thus written.
}

void b()
{
    // Open the table.
    Table tab("tScalarRecordColumn_tmp.data");
    ScalarColumn<TableRecord> ab1(tab,"ab1");
    ScalarColumn<TableRecord> ab3(tab,"ab3");
    Vector<TableRecord> vec = ab1.getColumn();
    TableRecord rec;
    uInt i;
    // Check if the columns are written and read back correctly.
    for (i=0; i<10; i++) {
	ab1.get (i, rec);
	AlwaysAssertExit (rec.nfields() == i+1);
	AlwaysAssertExit (vec(i).nfields() == i+1);
	for (uInt j=0; j<=i; j++) {
	    AlwaysAssertExit (rec.asuInt(j) == j);
	    AlwaysAssertExit (vec(i).asuInt(j) == j);
	}
	rec = ab3(i);
	if (i==0) {
	    AlwaysAssertExit (rec.nfields() == 0);
	} else {
	    if (i%4 == 1) {
		AlwaysAssertExit (rec.nfields() == 2);
		AlwaysAssertExit (rec.asuInt("fld2") == 10+i/4);
	    } else {
		AlwaysAssertExit (rec.nfields() == 1);
	    }
	    AlwaysAssertExit (rec.asuInt("fld1") == (i-1)/2);
	}
    }
    vec.resize(0);
    vec = ab1.getColumnRange (Slice(1,4));
    AlwaysAssertExit (vec.nelements() == 4);
    for (i=0; i<4; i++) {
	AlwaysAssertExit (vec(i).nfields() == i+2);
	for (uInt j=0; j<=i+1; j++) {
	    AlwaysAssertExit (vec(i).asuInt(j) == j);
	}
    }
}

int main()
{
    try {
	a();
	b();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
