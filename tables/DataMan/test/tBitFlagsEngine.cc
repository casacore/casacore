//# tBitFlagsEngine.cc: Test program for class BitFlagsEngine
//# Copyright (C) 2009
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
#include <casacore/tables/DataMan/BitFlagsEngine.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
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

// <summary> Test program for class BitFlagsEngine </summary>

// This program tests the virtual column engine BitFlagsEngine.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// Build a description.
void createTable()
{
  {
    // First ensure it can do the things it should.
    BitFlagsEngine<Short> engine("", "");
    Bool reask;
    AlwaysAssertExit (engine.canAccessArrayColumn(reask));
    AlwaysAssertExit (engine.canAccessSlice(reask));
    AlwaysAssertExit (engine.canAccessColumnSlice(reask));
  }
  // First register the virtual column engine.
  BitFlagsEngine<Short>::registerClass();
  BitFlagsEngine<Int>::registerClass();
  BitFlagsEngine<uChar>::registerClass();

  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Bool> ("virtualcol1"));
  td.addColumn (ArrayColumnDesc<Int> ("storedcol1"));
  td.addColumn (ArrayColumnDesc<Bool> ("virtualcol2"));
  td.addColumn (ArrayColumnDesc<Short> ("storedcol2"));
  td.addColumn (ArrayColumnDesc<Bool> ("virtualcol3", "",
                                       IPosition(3,2,3,4),
                                       ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<uChar> ("storedcol3", "",
                                        IPosition(2,3,4),
                                        ColumnDesc::Direct));
  // Define keywords telling the bitmask.
  ColumnDesc& cdesc = td.rwColumnDesc("storedcol1");
  Record brec;
  brec.define ("bit01", 3);
  brec.define ("bit12", 6);
  brec.define ("bit23", 12);
  cdesc.rwKeywordSet().defineRecord ("FLAGSETS", brec);
  // Now create a new table from the description.
  SetupNewTable newtab("tBitFlagsEngine_tmp.data", td, Table::New);
  // Create the virtual column engines and bind the columns to them.
  // Use keywords to define the mask.
  Vector<String> writeMask(2);
  writeMask[0] = "bit01";
  writeMask[1] = "bit12";
  BitFlagsEngine<Int> engine1("virtualcol1", "storedcol1",
                              Vector<String>(1,"bit23"), writeMask);
  BitFlagsEngine<Short> engine2("virtualcol2", "storedcol2");
  BitFlagsEngine<uChar> engine3("virtualcol3", "storedcol3");
  newtab.bindColumn ("virtualcol1", engine1);
  newtab.bindColumn ("virtualcol2", engine2);
  newtab.bindColumn ("virtualcol3", engine3);
  Table tab(newtab, 10);
  // Fill the table via the flag columns.
  ArrayColumn<Int> storedcol1 (tab, "storedcol1");
  ArrayColumn<Short> storedcol2 (tab, "storedcol2");
  ArrayColumn<uChar> storedcol3 (tab, "storedcol3");
  Matrix<Int> arri(IPosition(2,3,4));
  Matrix<Short> arrs(IPosition(2,3,4));
  Matrix<uChar> arrc(IPosition(2,3,4));
  for (Int j=0; j<10; j++) {
    Int i=0;
    for (uInt i2=0; i2<4; i2++) {
      for (uInt i1=0; i1<3; i1++) {
        arri(i1,i2) = (j + i)%2;
        arrs(i1,i2) = (j + i)%5;
        arrc(i1,i2) = (j + i)%4;
        ++i;
      }
    }
    storedcol1.put (j, arri);
    storedcol2.put (j, arrs);
    storedcol3.put (j, arrc);
    arri += 840;
    arrs += Short(210);
    arrc += uChar(1);
  }
}

void readTable()
{
  // Read back the table.
  Table tab("tBitFlagsEngine_tmp.data");
  ArrayColumn<Int> storedcol1 (tab, "storedcol1");
  ArrayColumn<Short> storedcol2 (tab, "storedcol2");
  ArrayColumn<uChar> storedcol3 (tab, "storedcol3");
  ArrayColumn<Bool> virtualcol1 (tab, "virtualcol1");
  ArrayColumn<Bool> virtualcol2 (tab, "virtualcol2");
  ArrayColumn<Bool> virtualcol3 (tab, "virtualcol3");
  Matrix<Bool> arrd1(IPosition(2,3,4));
  Matrix<Bool> arrd2(IPosition(2,3,4));
  Matrix<Bool> arrd3(IPosition(2,3,4));
  Matrix<Bool> arrd3slice(arrd3(Slice(0,1,2),Slice(0,2,2)));
  Matrix<Int> arri(IPosition(2,3,4));
  Matrix<Short> arrs(IPosition(2,3,4));
  Matrix<uChar> arrc(IPosition(2,3,4));
  Slice tmp;
  Slicer nslice (tmp, tmp, Slicer::endIsLength);
  Slicer nslice2(Slice(0,1,2), Slice(0,2,2), Slicer::endIsLength);

  for (uInt j=0; j<10; j++) {
    { 
      Int i=0;
      for (uInt i2=0; i2<4; i2++) {
        for (uInt i1=0; i1<3; i1++) {
          arri(i1,i2) = (j + i)%2;
          arrs(i1,i2) = (j + i)%5;
          arrc(i1,i2) = (j + i)%4;
          arrd1(i1,i2) = arri(i1,i2) & 12;
          arrd2(i1,i2) = arrs(i1,i2);
          arrd3(i1,i2) = arrc(i1,i2);
          ++i;
        }
      }
    }
    Array<Int> ai;
    Array<Short> as;
    Array<uChar> ac;
    Array<Bool> arrbool;
    Array<Bool> arrboolslice;
    cout << "get row " << j << endl;
    storedcol1.get (j, ai);
    if (!allEQ (ai, arri)) {
      cout << "error in storedcol1 in row " << j << endl;
      cout << ai << endl;
      cout << arri << endl;
    }
    virtualcol1.get (j, arrbool);
    if (!allEQ (arrbool, arrd1)) {
      cout << "error in virtualcol1 in row " << j << endl;
      cout << arrbool << endl;
      cout << arrd1 << endl;
    }
    storedcol2.get (j, as);
    if (!allEQ (as, arrs)) {
      cout << "error in storedcol2 in row " << j << endl;
      cout << as << endl;
      cout << arrs << endl;
    }
    virtualcol2.get (j, arrbool);
    if (!allEQ (arrbool, arrd2)) {
      cout << "error in virtualcol2 in row " << j << endl;
      cout << arrbool << endl;
      cout << arrd2 << endl;
    }
    storedcol3.get (j, ac);
    if (!allEQ (ac, arrc)) {
      cout << "error in storedcol3 in row " << j << endl;
      cout << ac << endl;
      cout << arrc << endl;
    }
    virtualcol3.get (j, arrbool);
    if (!allEQ (arrbool, arrd3)) {
      cout << "error in virtualcol3 in row " << j << endl;
      cout << arrbool << endl;
      cout << arrd3 << endl;
    }
    virtualcol3.getSlice (j, nslice, arrbool);
    if (!allEQ (arrbool, arrd3)) {
      cout << "error in virtualcol3 (entire slice) in row " << j << endl;
      cout << arrbool << endl;
      cout << arrd3 << endl;
    }
    virtualcol3.getSlice (j, nslice2, arrboolslice);
    if (!allEQ (arrboolslice, arrd3slice)) {
      cout << "error in virtualcol3 (partial slice) in row " << j << endl;
      cout << arrboolslice << endl;
      cout << arrd3slice << endl;
    }
  }

  // Now test getting the columns.
  {
    Cube<Bool> arrd2(IPosition(3,3,4,10));
    Slicer nslice2(Slice(0,2,1), Slice(1,2,2), Slicer::endIsLength);
    for (uInt j=0; j<10; j++) {
      Int i = 0;
      for (uInt i2=0; i2<4; i2++) {
        for (uInt i1=0; i1<3; i1++) {
          Int val = (j + i)%5;
          arrd2(i1,i2,j) = val;
          ++i;
        }
      }
    }
    {
      Cube<Bool> arrvald = virtualcol2.getColumn();
      if (!allEQ (arrvald, arrd2)) {
        cout << "error in virtualcol2 getcolumn " << endl;
        cout << arrvald << endl;
        cout << arrd2 << endl;
      }
    }
    {
      Cube<Bool> arrvald = virtualcol2.getColumnRange(Slice(1,4,2));
      if (!allEQ (arrvald, arrd2(Slice(0,3,1),Slice(0,4,1),Slice(1,4,2)))) {
        cout << "error in virtualcol2 getcolumnrange " << endl;
        cout << arrvald << endl;
        cout << arrd2 << endl;
      }
    }
    {
      Cube<Bool> arrvald = virtualcol2.getColumn(nslice2);
      if (!allEQ (arrvald, arrd2(Slice(0,2,1),Slice(1,2,2),Slice(0,10,1)))) {
        cout << "error in virtualcol2 getcolumnslice " << endl;
        cout << arrvald << endl;
        cout << arrd2 << endl;
      }
    }
    {
      Cube<Bool> arrvald = virtualcol2.getColumnRange(Slice(1,4,2), nslice2);
      if (!allEQ (arrvald, arrd2(Slice(0,2,1),Slice(1,2,2),Slice(1,4,2)))) {
        cout << "error in virtualcol2 getcolumnrangeslice " << endl;
        cout << arrvald << endl;
        cout << arrd2 << endl;
      }
    }
  }
}


int main ()
{
  //  return 0;
  try {
    createTable();
    readTable();
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
