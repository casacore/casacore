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
#include <casacore/casa/IO/ArrayIO.h>
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
  // First register the virtual column engine.
  BitFlagsEngine<int16_t>::registerClass();
  BitFlagsEngine<int32_t>::registerClass();
  BitFlagsEngine<unsigned char>::registerClass();

  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<bool> ("virtualcol1"));
  td.addColumn (ArrayColumnDesc<int32_t> ("storedcol1"));
  td.addColumn (ArrayColumnDesc<bool> ("virtualcol2"));
  td.addColumn (ArrayColumnDesc<int16_t> ("storedcol2"));
  td.addColumn (ArrayColumnDesc<bool> ("virtualcol3", "",
                                       IPosition(3,2,3,4),
                                       ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<unsigned char> ("storedcol3", "",
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
  BitFlagsEngine<int32_t> engine1("virtualcol1", "storedcol1",
                              Vector<String>(1,"bit23"), writeMask);
  BitFlagsEngine<int16_t> engine2("virtualcol2", "storedcol2");
  BitFlagsEngine<unsigned char> engine3("virtualcol3", "storedcol3");
  newtab.bindColumn ("virtualcol1", engine1);
  newtab.bindColumn ("virtualcol2", engine2);
  newtab.bindColumn ("virtualcol3", engine3);
  Table tab(newtab, 10);
  // Fill the table via the flag columns.
  ArrayColumn<int32_t> storedcol1 (tab, "storedcol1");
  ArrayColumn<int16_t> storedcol2 (tab, "storedcol2");
  ArrayColumn<unsigned char> storedcol3 (tab, "storedcol3");
  Matrix<int32_t> arri(IPosition(2,3,4));
  Matrix<int16_t> arrs(IPosition(2,3,4));
  Matrix<unsigned char> arrc(IPosition(2,3,4));
  for (int32_t j=0; j<10; j++) {
    int32_t i=0;
    for (uint32_t i2=0; i2<4; i2++) {
      for (uint32_t i1=0; i1<3; i1++) {
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
    arrs += int16_t(210);
    arrc += static_cast<unsigned char>(1);
  }
}

void readTable()
{
  // Read back the table.
  Table tab("tBitFlagsEngine_tmp.data");
  ArrayColumn<int32_t> storedcol1 (tab, "storedcol1");
  ArrayColumn<int16_t> storedcol2 (tab, "storedcol2");
  ArrayColumn<unsigned char> storedcol3 (tab, "storedcol3");
  ArrayColumn<bool> virtualcol1 (tab, "virtualcol1");
  ArrayColumn<bool> virtualcol2 (tab, "virtualcol2");
  ArrayColumn<bool> virtualcol3 (tab, "virtualcol3");
  Matrix<bool> arrd1(IPosition(2,3,4));
  Matrix<bool> arrd2(IPosition(2,3,4));
  Matrix<bool> arrd3(IPosition(2,3,4));
  Matrix<bool> arrd3slice(arrd3(Slice(0,1,2),Slice(0,2,2)));
  Matrix<int32_t> arri(IPosition(2,3,4));
  Matrix<int16_t> arrs(IPosition(2,3,4));
  Matrix<unsigned char> arrc(IPosition(2,3,4));
  Slice tmp;
  Slicer nslice (tmp, tmp, Slicer::endIsLength);
  Slicer nslice2(Slice(0,1,2), Slice(0,2,2), Slicer::endIsLength);

  for (uint32_t j=0; j<10; j++) {
    { 
      int32_t i=0;
      for (uint32_t i2=0; i2<4; i2++) {
        for (uint32_t i1=0; i1<3; i1++) {
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
    Array<int32_t> ai;
    Array<int16_t> as;
    Array<unsigned char> ac;
    Array<bool> arrbool;
    Array<bool> arrboolslice;
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
    Cube<bool> arrd2(IPosition(3,3,4,10));
    Slicer nslice2(Slice(0,2,1), Slice(1,2,2), Slicer::endIsLength);
    for (uint32_t j=0; j<10; j++) {
      int32_t i = 0;
      for (uint32_t i2=0; i2<4; i2++) {
        for (uint32_t i1=0; i1<3; i1++) {
          int32_t val = (j + i)%5;
          arrd2(i1,i2,j) = val;
          ++i;
        }
      }
    }
    {
      Cube<bool> arrvald = virtualcol2.getColumn();
      if (!allEQ (arrvald, arrd2)) {
        cout << "error in virtualcol2 getcolumn " << endl;
        cout << arrvald << endl;
        cout << arrd2 << endl;
      }
    }
    {
      Cube<bool> arrvald = virtualcol2.getColumnRange(Slice(1,4,2));
      if (!allEQ (arrvald, arrd2(Slice(0,3,1),Slice(0,4,1),Slice(1,4,2)))) {
        cout << "error in virtualcol2 getcolumnrange " << endl;
        cout << arrvald << endl;
        cout << arrd2 << endl;
      }
    }
    {
      Cube<bool> arrvald = virtualcol2.getColumn(nslice2);
      if (!allEQ (arrvald, arrd2(Slice(0,2,1),Slice(1,2,2),Slice(0,10,1)))) {
        cout << "error in virtualcol2 getcolumnslice " << endl;
        cout << arrvald << endl;
        cout << arrd2 << endl;
      }
    }
    {
      Cube<bool> arrvald = virtualcol2.getColumnRange(Slice(1,4,2), nslice2);
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
  } catch (std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
