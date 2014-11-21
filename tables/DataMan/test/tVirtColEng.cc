//# tVirtColEng.cc: Test program for virtual column engine
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2002
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

//# Define the variable to exclude the main from dVirtColEng.cc
#define DVIRTCOLENG_MAIN


//# This test program is in fact also a demo program.
//# A virtual column engine is declared in dVirtColEng.h and
//# implemented in dVirtColEng.cc.
//# Both files are included here; the .h file for the definitions
//# and the .cc file to get compiled.


//# Includes
#include "dVirtColEng.h"
#include "dVirtColEng.cc"
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary> Test program for virtual column engine </summary>

// This program tests the class DummyVirtualEngine and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.
// The Dummy* classes are declared and implemented in dVirtColEng.h
// and dVirtColEng.cc, resp..

void a();
void b();

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

// First build a description.
void a() {
    // First register the virtual column engine.
    DummyVirtualEngine::registerClass();

    // Build the table description.
    // Define a group name Engine for the columns intended to be virtual.
    TableDesc td("tTableDesc","1",TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ScalarColumnDesc<Int> ("DATA1"));
    td.addColumn (ScalarColumnDesc<double> ("DOUB1", "", "", "Engine"));
    td.addColumn (ArrayColumnDesc<Int> ("DATA2"));
    td.addColumn (ArrayColumnDesc<double> ("DOUB2", "", "", "Engine"));

    // Now create a new table from the description.
    SetupNewTable newtab("tVirtColEng_tmp.data", td, Table::New);
    // Create the virtual column engine with the scale factors
    // and bind the columns to them.
    DummyVirtualEngine engine(2.0, 3.0);
    newtab.bindGroup ("Engine", engine);
    Table tab(newtab, 10);

    // Fill the table via the virtual columns.
    ScalarColumn<double> doub1(tab,"DOUB1");
    ArrayColumn<double> doub2(tab,"DOUB2");
    Cube<double> arrd(IPosition(3,2,3,4));
    uInt i;
    i=0;
    for (uInt i2=0; i2<4; i2++)
	for (uInt i1=0; i1<3; i1++)
	    for (uInt i0=0; i0<2; i0++) {
		arrd(i0,i1,i2) = i;
		i += 3;
	    }
    for (i=0; i<10; i++) {
	doub1.put (i, i*2);
	doub2.put (i,arrd);
	arrd += (double)(3*arrd.nelements());
    }
}

void b()
{
    // Read back the table.
    Table tab("tVirtColEng_tmp.data");
    ScalarColumn<double> doub1(tab,"DOUB1");
    ArrayColumn<double> doub2(tab,"DOUB2");
    ScalarColumn<Int> data1(tab,"DATA1");
    ArrayColumn<Int> data2(tab,"DATA2");
    uInt i;
    double dval;
    Int ival;
    Cube<Int> arri(IPosition(3,2,3,4));
    Cube<Int> arrvali(IPosition(3,2,3,4));
    Cube<double> arrd(IPosition(3,2,3,4));
    Cube<double> arrval(IPosition(3,2,3,4));
    Cube<double> arrvalslice(arrval(Slice(0,1),Slice(0,1,2),Slice(0,2,2)));
    Slice tmp;
    Slicer nslice (tmp, tmp, tmp, Slicer::endIsLength);
    Slicer nslice2(Slice(0,1), Slice(0,1,2), Slice(0,2,2),
		   Slicer::endIsLength);
    i=0;
    for (uInt i2=0; i2<4; i2++)
	for (uInt i1=0; i1<3; i1++)
	    for (uInt i0=0; i0<2; i0++) {
		arri(i0,i1,i2) = i;
		arrd(i0,i1,i2) = 3*i;
		i++;
	    }
    for (i=0; i<10; i++) {
	cout << "get scalar row " << i << endl;
	ival = data1(i);
	dval = doub1(i);
	if (ival != Int(i)  ||  dval != 2*i) {
	    cout << "error in row " << i << ": " << ival << " " << dval << endl;
	}
	data2.get (i, arrvali);
	if (!allEQ (arrvali, arri)) {
	    cout << "error in DATA2 in row " << i << endl;
	}
	doub2.get (i, arrval);
	if (!allEQ (arrval, arrd)) {
	    cout << "error in DOUB2 in row " << i << endl;
	}
	doub2.getSlice (i, nslice, arrval);
	if (!allEQ (arrval, arrd)) {
	    cout << "error in DOUB2 (entire slice) in row " << i << endl;
	}
	doub2.getSlice (i, nslice2, arrvalslice);
	if (!allEQ (arrval, arrd)) {
	    cout << "error in DOUB2 (partial slice) in row " << i << endl;
	}
	arrd += (double)(3*arrd.nelements());
	arri += (Int)(arrd.nelements());
    }
    Vector<double> vec = doub1.getColumn();
    cout << tab.nrow() << " " << vec.nelements() << endl;
    for (i=0; i<10; i++) {
	if (vec(i) != 2*i) {
	    cout << "error in getColumn " << i << ": " << vec(i) << endl;
	}
    }
}
