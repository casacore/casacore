//# tArrayColumnSlices.cc: Test program for the ArrayColumn slices functions
//# Copyright (C) 2008
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
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

using namespace casacore;

// Create the table.
void createTab()
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<float>("arr1",IPosition(2,20,30),
                                       ColumnDesc::FixedShape));
  // Now create a new table from the description.
  SetupNewTable newtab("tArrayColumnSlices_tmp.data", td, Table::New);
  Table tab(newtab, 10, False, Table::LocalEndian);
  ArrayColumn<float> arr1(tab, "arr1");
  Array<float> arrf(IPosition(2,20,30));
  indgen (arrf);
  for (uInt i=0; i<10; i++) {
    arr1.put(i, arrf);
    arrf += (float)(arrf.nelements());
  }
}

void readCellSlices()
{
  Table tab("tArrayColumnSlices_tmp.data");
  ArrayColumn<float> arr1(tab, "arr1");
  {
    // No axes, thus all entire axes.
    Vector<Vector<Slice> > slices;
    for (uInt i=0; i<tab.nrow(); ++i) {
      AlwaysAssertExit (allEQ (arr1(i), arr1.getSlice(i, slices)));
    }
  }
  {
    // Empty axes, thus entire axes.
    Vector<Vector<Slice> > slices(2);
    for (uInt i=0; i<tab.nrow(); ++i) {
      AlwaysAssertExit (allEQ (arr1(i), arr1.getSlice(i, slices)));
    }
  }
  {
    // Entire axes, but in parts.
    Vector<Vector<Slice> > slices(2);
    slices[0].resize(2);
    slices[0][0] = Slice(0,5);
    slices[0][1] = Slice(5,15);
    slices[1].resize(3);
    slices[1][0] = Slice(0,10);
    slices[1][1] = Slice(10,4);
    slices[1][2] = Slice(14,16);
    for (uInt i=0; i<tab.nrow(); ++i) {
      cout << "get row " << i << endl;
      AlwaysAssertExit (allEQ (arr1(i), arr1.getSlice(i, slices)));
    }
  }
  {
    // Axes parts with strides.
    Vector<Vector<Slice> > slices(2);
    slices[0].resize(2);
    slices[0][0] = Slice(2,5,2);
    slices[0][1] = Slice(12,3,2);
    slices[1].resize(3);
    slices[1][0] = Slice(1,3,3);
    slices[1][1] = Slice(10,2,3);
    slices[1][2] = Slice(16,1);
    for (uInt i=0; i<tab.nrow(); ++i) {
      cout << "get row " << i << endl;
      AlwaysAssertExit (allEQ (arr1.getSlice(i, Slicer(IPosition(2,2,1),
                                                       IPosition(2,8,6),
                                                       IPosition(2,2,3))),
                               arr1.getSlice(i, slices)));
    }
  }
}

void readColumnSlices()
{
  Table tab("tArrayColumnSlices_tmp.data");
  ArrayColumn<float> arr1(tab, "arr1");
  {
    // No axes, thus all entire axes.
    Vector<Vector<Slice> > slices;
    AlwaysAssertExit (allEQ (arr1.getColumn(), arr1.getColumn(slices)));
  }
  {
    // Axes parts with strides.
    Vector<Vector<Slice> > slices(2);
    slices[0].resize(2);
    slices[0][0] = Slice(2,5,2);
    slices[0][1] = Slice(12,3,2);
    slices[1].resize(3);
    slices[1][0] = Slice(1,3,3);
    slices[1][1] = Slice(10,2,3);
    slices[1][2] = Slice(16,1);
    AlwaysAssertExit (allEQ (arr1.getColumn(Slicer(IPosition(2,2,1),
                                                   IPosition(2,8,6),
                                                   IPosition(2,2,3))),
                             arr1.getColumn(slices)));
  }
}

void writeCellSlices()
{
  Table tab("tArrayColumnSlices_tmp.data", Table::Update);
  ArrayColumn<float> arr1(tab, "arr1");
  {
    // Axes parts with strides.
    Vector<Vector<Slice> > slices(2);
    slices[0].resize(2);
    slices[0][0] = Slice(2,5,2);
    slices[0][1] = Slice(12,3,2);
    slices[1].resize(3);
    slices[1][0] = Slice(1,3,3);
    slices[1][1] = Slice(10,2,3);
    slices[1][2] = Slice(16,1);
    for (uInt i=0; i<tab.nrow(); ++i) {
      cout << "put row " << i << endl;
      Array<float> arr = float(1) + arr1.getSlice(i, Slicer(IPosition(2,2,1),
                                                            IPosition(2,8,6),
                                                            IPosition(2,2,3)));
      
      arr1.putSlice (i, slices, arr);
      AlwaysAssertExit (allEQ (arr1.getSlice(i, Slicer(IPosition(2,2,1),
                                                       IPosition(2,8,6),
                                                       IPosition(2,2,3))),
                               arr));
    }
  }
}

void writeColumnSlices()
{
  Table tab("tArrayColumnSlices_tmp.data", Table::Update);
  ArrayColumn<float> arr1(tab, "arr1");
  {
    // Entire axes, but in parts.
    Vector<Vector<Slice> > slices(2);
    slices[0].resize(2);
    slices[0][0] = Slice(0,5);
    slices[0][1] = Slice(5,15);
    slices[1].resize(3);
    slices[1][0] = Slice(0,10);
    slices[1][1] = Slice(10,4);
    slices[1][2] = Slice(14,16);
    Array<float> arr = float(12) + arr1.getColumn();
    arr1.putColumn (slices, arr);
    AlwaysAssertExit (allEQ (arr1.getColumn(), arr));
  }
}

int main()
{
  try {
    createTab();
    readCellSlices();
    readColumnSlices();
    writeCellSlices();
    writeColumnSlices();
    readColumnSlices();
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
