//# tColumnsIndexArray.cc: Test program for the ColumnsIndexArray class
//# Copyright (C) 2001,2002
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

#include <casacore/tables/Tables/ColumnsIndexArray.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for the ColumnsIndexArray class
// </summary>


// First build a description.
void a()
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.comment() = "A test of class Table";
  td.addColumn (ArrayColumnDesc<Int>("aint"));
  td.addColumn (ArrayColumnDesc<uInt>("auint"));
  td.addColumn (ArrayColumnDesc<String>("astring"));

  // Now create a new table from the description.
  const Int nrrow = 10;
  SetupNewTable newtab("tColumnsIndexArray_tmp.data", td, Table::New);
  Table tab(newtab, nrrow);
  ArrayColumn<Int> aint(tab, "aint");
  ArrayColumn<uInt> auint(tab, "auint");
  ArrayColumn<String> astring(tab, "astring");
  Array<Int> arri(IPosition(1,3));
  indgen(arri);
  Array<uInt> arrui(IPosition(1,3));
  indgen(arrui);
  Array<String> arrstr(IPosition(1,3));
  arrstr = "aa";
  for (Int i=0; i<nrrow; i++) {
    if (i%2 == 0) {
      aint.put (i, arri);
      auint.put (i, arrui);
      astring.put (i, arrstr);
      arri += 2;
      arrui += uInt(arrui.nelements());
    }
  }
}

void b()
{
  Table tab("tColumnsIndexArray_tmp.data");
  ColumnsIndexArray colInx3 (tab, "aint");
  ColumnsIndexArray colInx4 (tab, "auint");
  ColumnsIndexArray colInx9 (tab, "astring");
  AlwaysAssertExit (!colInx3.isUnique());
  AlwaysAssertExit (colInx4.isUnique());
  AlwaysAssertExit (!colInx9.isUnique());
  RecordFieldPtr<Int> aint (colInx3.accessKey(), "aint");
  RecordFieldPtr<uInt> auint (colInx4.accessKey(), "auint");
  RecordFieldPtr<String> astring (colInx9.accessKey(), "astring");
  Record rec;
  Bool found;
  // Find the 15 values.
  for (uInt i=0; i<15; i++) {
    rec.define ("auint", i);
    AlwaysAssertExit ( (colInx4.getRowNumber(found, rec) == 2*(i/3)
			&& found));
  }
  // The 16th one should not be found.
  *auint = 15;
  colInx4.getRowNumber(found);
  AlwaysAssertExit (!found);

  // Find the values in the other index. They are not unique.
  for (Int i=0; i<12; i++) {
    *aint = i;
    cout << colInx3.getRowNumbers() << endl;
  }
  *astring = "a";
  cout << colInx9.getRowNumbers() << endl;
  cout << colInx9.getRowNumbers(True) << endl;
  *astring = "aa";
  cout << colInx9.getRowNumbers() << endl;
  cout << colInx9.getRowNumbers(True) << endl;
  // Test a not unique index in an erronous way.
  try {
    colInx9.getRowNumber(found);
  } catch (AipsError x) {
    cout << x.getMesg() << endl;       // values are not unique
  } 
  // Test a range.
  Record lower, upper;
  lower.define ("auint", uInt(2));
  upper.define ("auint", uInt(6));
  cout << colInx4.getRowNumbers (lower, upper, False, False) << endl;
  cout << colInx4.getRowNumbers (lower, upper, True, False) << endl;
  cout << colInx4.getRowNumbers (lower, upper, False, True) << endl;
  cout << colInx4.getRowNumbers (lower, upper, True, True) << endl;
  cout << colInx4.getRowNumbers (lower, upper, False, False, True) << endl;
  cout << colInx4.getRowNumbers (lower, upper, True, False, True) << endl;
  cout << colInx4.getRowNumbers (lower, upper, False, True, True) << endl;
  cout << colInx4.getRowNumbers (lower, upper, True, True, True) << endl;
  upper.define ("auint", uInt(3));
  cout << colInx4.getRowNumbers (lower, upper, True, True) << endl;
  cout << colInx4.getRowNumbers (lower, upper, False, False) << endl;
  cout << colInx4.getRowNumbers (lower, upper, True, True, True) << endl;
  cout << colInx4.getRowNumbers (lower, upper, False, False, True) << endl;
}

void c()
{
  Table tab("tColumnsIndexArray_tmp.data", Table::Update);
  // Create the index with the special compare function.
  ColumnsIndexArray colInx0 (tab, "aint");
  RecordFieldPtr<Int> keyint (colInx0.accessKey(), "aint");
  ArrayColumn<Int> aint(tab, "aint");
  ArrayColumn<uInt> auint(tab, "auint");
  // Change the values of a few columns.
  Array<Int> arri(IPosition(2,2,4));
  Array<uInt> arrui(IPosition(3,2,3,3));
  indgen(arri);
  indgen(arrui, uInt(15));
  aint.put (3, arri);
  auint.put (7, arrui);
  // Tell the index that some columns have changed.
  colInx0.setChanged ("auint");
  *keyint = 2;
  // Should not change anything yet.
  cout << colInx0.getRowNumbers() << endl;
  colInx0.setChanged ("aint");
  // Now the index knows it is changed.
  cout << colInx0.getRowNumbers() << endl;
}


int main()
{
  try {
    a();
    b();
    c();
  } catch (AipsError x) {
    cout << "Exception caught: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
