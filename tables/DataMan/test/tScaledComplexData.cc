//# tScaledComplexData.cc: Test program for class ScaledComplexData
//# Copyright (C) 1999,2000,2002
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
#include <casacore/tables/DataMan/ScaledComplexData.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary> Test program for class ScaledComplexData </summary>

// This program tests the virtual column engine ScaledComplexData.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a();
void b();

int main () {
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
void a()
{
    {
        // First ensure it can do the things it should.
        ScaledComplexData<Complex,Short> engine("", "", Complex(1.,1.),
						Complex(0.,0.));
	Bool reask;
	AlwaysAssertExit (engine.canAccessArrayColumn(reask));
	AlwaysAssertExit (engine.canAccessArrayColumnCells(reask));
	AlwaysAssertExit (engine.canAccessSlice(reask));
	AlwaysAssertExit (engine.canAccessColumnSlice(reask));
    }
    // First register the virtual column engine.
    ScaledComplexData<Complex,Short>::registerClass();
    ScaledComplexData<DComplex,Int>::registerClass();

    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<Int> ("target1"));
    td.addColumn (ArrayColumnDesc<DComplex> ("source1"));
    td.addColumn (ArrayColumnDesc<Short> ("target2"));
    td.addColumn (ArrayColumnDesc<Complex> ("source2","",
					  IPosition(2,3,4),
					  ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<Int> ("target3", "",
					IPosition(3,2,3,4),
					ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<DComplex> ("source3", "",
					   IPosition(2,3,4),
					   ColumnDesc::Direct));
    td.addColumn (ScalarColumnDesc<DComplex> ("scale3"));

    // Now create a new table from the description.
    SetupNewTable newtab("tScaledComplexData_tmp.data", td, Table::New);
    // Create the virtual column engine with the scale factors
    // and bind the columns to them.
    ScaledComplexData<DComplex,Int> engine1("source1", "target1",
					    DComplex(2.0, 3.0),
					    DComplex(4.0, 5.0));
    ScaledComplexData<Complex,Short> engine2("source2", "target2",
					     Complex(6.0, 7.0),
					     Complex(2.0, 3.0));
    ScaledComplexData<DComplex,Int> engine3("source3", "target3", "scale3");
    newtab.bindColumn ("source1", engine1);
    newtab.bindColumn ("source2", engine2);
    newtab.bindColumn ("source3", engine3);
    Table tab(newtab, 10);

    // Fill the table via the virtual columns.
    ArrayColumn<DComplex> source1 (tab, "source1");
    ArrayColumn<Complex> source2 (tab, "source2");
    ArrayColumn<DComplex> source3 (tab, "source3");
    ScalarColumn<DComplex> scale3 (tab,"scale3");

    Matrix<DComplex> arrd(IPosition(2,3,4));
    Matrix<Complex> arrf(IPosition(2,3,4));
    Int i=0;
    for (uInt i2=0; i2<4; i2++) {
        for (uInt i1=0; i1<3; i1++) {
	    arrd(i1,i2) = DComplex(4+i, 2+i);
	    arrf(i1,i2) = Complex(8+i, 3+i);
	    i += 420;
	}
    }
    for (i=0; i<10; i++) {
	scale3.put (i, Complex(1+i%3, 1+i%4));
	source1.put (i, arrd);
	source2.put (i, arrf);
	source3.put (i, arrd + DComplex(2, 22));
	arrd += DComplex(840, 840);
	arrf += Complex(840, 840);
    }

    //# Do an erronous thing.
    //# 2005-02-15 GvD: this test does not work correctly, because the
    //# exception calls the BaseTable dtor. In there the table directory
    //# gets removed, but there is still an open file which NFS renames
    //# to .nfs... So the remove throws an exception resulting in an abort.
    //# So all open files should be closed first (in one way or another).
        SetupNewTable newtab2("tScaledComplexData_tmp.dat2", td, Table::Scratch);
        newtab2.bindColumn ("source2", engine1);
        try {
    	Table tab2(newtab2, 10);                // bound to incorrect column
        } catch (AipsError x) {
    	cout << x.getMesg() << endl;
        } 
    }

void b()
{
    // Read back the table.
    Table tab("tScaledComplexData_tmp.data");
    ArrayColumn<DComplex> source1 (tab, "source1");
    ArrayColumn<Complex> source2 (tab, "source2");
    ArrayColumn<DComplex> source3 (tab, "source3");
    ArrayColumn<Int> target1 (tab, "target1");
    ArrayColumn<Short> target2 (tab, "target2");
    ArrayColumn<Int> target3 (tab, "target3");
    Cube<Int> arri1(IPosition(3,2,3,4));
    Cube<Int> arri3(IPosition(3,2,3,4));
    Cube<Int> arrvali(IPosition(3,2,3,4));
    Cube<Short> arrc2(IPosition(3,2,3,4));
    Cube<Short> arrvalc(IPosition(3,2,3,4));
    Matrix<DComplex> arrd1(IPosition(2,3,4));
    Matrix<DComplex> arrd3(IPosition(2,3,4));
    Matrix<DComplex> arrvald(IPosition(2,3,4));
    Matrix<Complex> arrf2(IPosition(2,3,4));
    Matrix<Complex> arrvalf(IPosition(2,3,4));
    Matrix<DComplex> arrvalslice(arrvald(Slice(0,1,2),Slice(0,2,2)));
    Slice tmp;
    Slicer nslice (tmp, tmp, Slicer::endIsLength);
    Slicer nslice2(Slice(0,1,2), Slice(0,2,2), Slicer::endIsLength);
    for (uInt j=0; j<10; j++) {
        { 
	  Int sc1 = 1+j%3;
	  Int sc2 = 1+j%4;
	  Int i=0;
	  for (uInt i2=0; i2<4; i2++) {
	    for (uInt i1=0; i1<3; i1++) {
	      Int val = j*840 + i;
	      arrd1(i1,i2) = DComplex(val+4, val+2);
	      arrf2(i1,i2) = Complex(val+8, val+3);
	      arrd3(i1,i2) = arrd1(i1,i2) + DComplex(2,22);
	      arri1(0,i1,i2) = (val+4 - 4)/2;
	      arri1(1,i1,i2) = (val+2 - 5)/3;
	      arrc2(0,i1,i2) = (val+8 - 2)/6;
	      arrc2(1,i1,i2) = (val+3 - 3)/7;
	      arri3(0,i1,i2) = (val+4+2)/sc1;
	      arri3(1,i1,i2) = (val+2+22)/sc2;
	      i += 420;
	    }
	  }
        }
	cout << "get row " << j << endl;
	source1.get (j, arrvald);
	if (!allEQ (arrvald, arrd1)) {
	    cout << "error in source1 in row " << j << endl;
	    cout << arrvald << endl;
	    cout << arrd1 << endl;
	}
	target1.get (j, arrvali);
	if (!allEQ (arrvali, arri1)) {
	    cout << "error in target1 in row " << j << endl;
	    cout << arrvali << endl;
	    cout << arri1 << endl;
	}
	source2.get (j, arrvalf);
	if (!allEQ (arrvalf, arrf2)) {
	    cout << "error in source2 in row " << j << endl;
	    cout << arrvalf << endl;
	    cout << arrf2 << endl;
	}
	target2.get (j, arrvalc);
	if (!allEQ (arrvalc, arrc2)) {
	    cout << "error in target2 in row " << j << endl;
	    cout << arrvalc << endl;
	    cout << arrc2 << endl;
	}
	source3.get (j, arrvald);
	if (!allEQ (arrvald, arrd3)) {
	    cout << "error in source3 in row " << j << endl;
	    cout << arrvald << endl;
	    cout << arrd3 << endl;
	}
	target3.get (j, arrvali);
	if (!allEQ (arrvali, arri3)) {
	    cout << "error in target3 in row " << j << endl;
	    cout << arrvali << endl;
	    cout << arri3 << endl;
	}
	source1.getSlice (j, nslice, arrvald);
	if (!allEQ (arrvald, arrd1)) {
	    cout << "error in source1 (entire slice) in row " << j << endl;
	    cout << arrvald << endl;
	    cout << arrd1 << endl;
	}
	source1.getSlice (j, nslice2, arrvalslice);
	if (!allEQ (arrvald, arrd1)) {
	    cout << "error in source1 (partial slice) in row " << j << endl;
	    cout << arrvald << endl;
	    cout << arrd1 << endl;
	}
    }

    // Now test getting the columns.
    {
      Cube<DComplex> arrd1(IPosition(3,3,4,10));
      Slicer nslice2(Slice(0,2,1), Slice(1,2,2), Slicer::endIsLength);
      for (uInt j=0; j<10; j++) {
	Int i = 0;
	for (uInt i2=0; i2<4; i2++) {
	  for (uInt i1=0; i1<3; i1++) {
	    Int val = j*840 + i;
	    arrd1(i1,i2,j) = DComplex(val+4, val+2);
	    i += 420;
	  }
	}
      }
      {
	Cube<DComplex> arrvald = source1.getColumn();
	if (!allEQ (arrvald, arrd1)) {
	  cout << "error in source1 getcolumn " << endl;
	  cout << arrvald << endl;
	  cout << arrd1 << endl;
	}
      }
      {
	Cube<DComplex> arrvald = source1.getColumnRange(Slice(1,4,2));
	if (!allEQ (arrvald, arrd1(Slice(0,3,1),Slice(0,4,1),Slice(1,4,2)))) {
	  cout << "error in source1 getcolumnrange " << endl;
	  cout << arrvald << endl;
	  cout << arrd1 << endl;
	}
      }
      {
	Cube<DComplex> arrvald = source1.getColumn(nslice2);
	if (!allEQ (arrvald, arrd1(Slice(0,2,1),Slice(1,2,2),Slice(0,10,1)))) {
	  cout << "error in source1 getcolumnslice " << endl;
	  cout << arrvald << endl;
	  cout << arrd1 << endl;
	}
      }
      {
	Cube<DComplex> arrvald = source1.getColumnRange(Slice(1,4,2), nslice2);
	if (!allEQ (arrvald, arrd1(Slice(0,2,1),Slice(1,2,2),Slice(1,4,2)))) {
	  cout << "error in source1 getcolumnrangeslice " << endl;
	  cout << arrvald << endl;
	  cout << arrd1 << endl;
	}
      }
    }
}
