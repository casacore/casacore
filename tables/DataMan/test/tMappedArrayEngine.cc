//# tMappedArrayEngine.cc: Test program for class MappedArrayEngine
//# Copyright (C) 2005
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
#include <casacore/tables/DataMan/MappedArrayEngine.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary> Test program for class MappedArrayEngine </summary>

// This program tests the virtual column engine MappedArrayEngine.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a();
void b();

int main ()
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
void a()
{
  // First register the virtual column engine.
  MappedArrayEngine<float,uShort>::registerClass();

  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.comment() = "A test of class TableDesc";
  td.addColumn (ArrayColumnDesc<DComplex> ("target1"));
  td.addColumn (ArrayColumnDesc<Complex> ("source1"));
  td.addColumn (ArrayColumnDesc<uShort> ("target2"));
  td.addColumn (ArrayColumnDesc<float> ("source2","",
					IPosition(3,2,3,4),
					ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<DComplex> ("target3", "",
					   IPosition(3,2,3,4),
					   ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<Complex> ("source3", "",
					  IPosition(3,2,3,4),
					  ColumnDesc::Direct));

  // Now create a new table from the description.
  SetupNewTable newtab("tMappedArrayEngine_tmp.data", td, Table::New);
  // Create the virtual column engine with the scale factors
  // and bind the columns to them.
  MappedArrayEngine<Complex,DComplex> engine1("source1", "target1");
  MappedArrayEngine<float,uShort> engine2("source2", "target2");
  MappedArrayEngine<Complex,DComplex> engine3("source3", "target3");
  newtab.bindColumn ("source1", engine1);
  newtab.bindColumn ("source2", engine2);
  newtab.bindColumn ("source3", engine3);
  Table tab(newtab, 10);

  // Fill the table via the virtual columns.
  ArrayColumn<Complex> source1 (tab, "source1");
  ArrayColumn<float> source2 (tab, "source2");
  ArrayColumn<Complex> source3 (tab, "source3");

  Cube<Complex> arrd(IPosition(3,2,3,4));
  Cube<float> arrf(IPosition(3,2,3,4));
  uInt i;
  i=2;
  for (uInt i2=0; i2<4; i2++)
    for (uInt i1=0; i1<3; i1++)
      for (uInt i0=0; i0<2; i0++) {
	arrd(i0,i1,i2) = Complex(i,i+1);
	arrf(i0,i1,i2) = i;
	i += 6;
      }
  for (i=0; i<10; i++) {
    source1.put (i, arrd);
    source2.put (i, arrf);
    source3.put (i, arrd + Complex(4,5));
    arrd += Complex(6*arrd.nelements(), 6*arrd.nelements());
    arrf += (float)(6*arrf.nelements());
  }

  //# Do an erronous thing.
  SetupNewTable newtab2("tMappedArrayEngine_tmp.dat2", td, Table::Scratch);
  newtab2.bindColumn ("source2", engine1);
///    try {
///	Table tab2(newtab2, 10);                // bound to incorrect column
///    } catch (AipsError x) {
///	cout << x.getMesg() << endl;
///    } 
}

void b()
{
  // Read back the table.
  Table tab("tMappedArrayEngine_tmp.data");
  ArrayColumn<Complex> source1 (tab, "source1");
  ArrayColumn<float> source2 (tab, "source2");
  ArrayColumn<Complex> source3 (tab, "source3");
  ArrayColumn<DComplex> target1 (tab, "target1");
  ArrayColumn<uShort> target2 (tab, "target2");
  ArrayColumn<DComplex> target3 (tab, "target3");
  Cube<DComplex> arri1(IPosition(3,2,3,4));
  Cube<DComplex> arri3(IPosition(3,2,3,4));
  Cube<DComplex> arrvali(IPosition(3,2,3,4));
  Cube<uShort> arrc2(IPosition(3,2,3,4));
  Cube<uShort> arrvalc(IPosition(3,2,3,4));
  Cube<Complex> arrd1(IPosition(3,2,3,4));
  Cube<Complex> arrd3(IPosition(3,2,3,4));
  Cube<Complex> arrvald(IPosition(3,2,3,4));
  Cube<float> arrf2(IPosition(3,2,3,4));
  Cube<float> arrvalf(IPosition(3,2,3,4));
  Cube<Complex> arrvalslice(arrvald(Slice(0,1),Slice(0,1,2),Slice(0,2,2)));
  Slice tmp;
  Slicer nslice (tmp, tmp, tmp,  Slicer::endIsLength);
  Slicer nslice2(Slice(0,1), Slice(0,1,2), Slice(0,2,2),
		   Slicer::endIsLength);
  uInt i=2;
  for (uInt i2=0; i2<4; i2++)
    for (uInt i1=0; i1<3; i1++)
      for (uInt i0=0; i0<2; i0++) {
	arrd1(i0,i1,i2) = Complex(i, i+1);
	arrf2(i0,i1,i2) = i;
	arrd3(i0,i1,i2) = Complex(i+4, i+6);
	arri1(i0,i1,i2) = DComplex(i, i+1);
	arrc2(i0,i1,i2) = i;
	arri3(i0,i1,i2) = DComplex(i+4, i+6);
	i+=6;
      }
  for (i=0; i<10; i++) {
    cout << "get row " << i << endl;
    source1.get (i, arrvald);
    if (!allEQ (arrvald, arrd1)) {
      cout << "error in source1 in row " << i << endl;
      cout << arrvald << arrd1;
    }
    target1.get (i, arrvali);
    if (!allEQ (arrvali, arri1)) {
      cout << "error in target1 in row " << i << endl;
    }
    source2.get (i, arrvalf);
    if (!allEQ (arrvalf, arrf2)) {
      cout << "error in source2 in row " << i << endl;
    }
    target2.get (i, arrvalc);
    if (!allEQ (arrvalc, arrc2)) {
      cout << "error in target2 in row " << i << endl;
    }
    source3.get (i, arrvald);
    if (!allEQ (arrvald, arrd3)) {
      cout << "error in source3 in row " << i << endl;
    }
    target3.get (i, arrvali);
    if (!allEQ (arrvali, arri3)) {
      cout << "error in target3 in row " << i << endl;
    }
    source1.getSlice (i, nslice, arrvald);
    if (!allEQ (arrvald, arrd1)) {
      cout << "error in source1 (entire slice) in row " << i << endl;
    }
    source1.getSlice (i, nslice2, arrvalslice);
    if (!allEQ (arrvald, arrd1)) {
      cout << "error in source1 (partial slice) in row " << i << endl;
    }
    arrd1 += Complex(6*arrd1.nelements(), 6*arrd1.nelements());
    arrf2 += (float)(6*arrf2.nelements());
    arrd3 += Complex(6*arrd3.nelements(), 6*arrd3.nelements());
    arri1 += DComplex(6*arri1.nelements(), 6*arri1.nelements());
    arrc2 += (uShort)(6*arrc2.nelements());
    arri3 += DComplex(6*arri3.nelements(), 6*arri3.nelements());
  }
}
