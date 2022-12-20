//# tVACEngine.cc: Test program for class VACEngine
//# Copyright (C) 1994,1995,1996,1999,2000,2001,2002
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


//# Includes
#include "dVACEngine.h"
#include "dVACEngine.cc"
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary> Test program for class VACEngine </summary>

// This program tests the virtual column engine VACEngine.
// It is using the example class VACExampleVACEngine for that purpose
// (note that BaseColumnDesc adds VACEngine to the default datamanager type).
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a();
void b();

int main ()
{
    try {
	a();
	b();
    } catch (const AipsError& x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

// First build a description.
void a() {
    // First register the virtual column engine.
    VACExampleVACEngine::registerClass();
    // Add ArrayColumnDesc<VACExample> to column type map.
    ArrayColumnDesc<VACExample>("x").registerClass();

    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class TableDesc";
    td.addColumn (ArrayColumnDesc<Int>    ("x"));
    td.addColumn (ArrayColumnDesc<float>  ("y"));
    td.addColumn (ArrayColumnDesc<String> ("z"));
    td.addColumn (ArrayColumnDesc<VACExample> ("colA"));

    // Now create a new table from the description.
    SetupNewTable newtab("tVACEngine_tmp.data", td, Table::New);
    // Create the virtual column engine with the target columns x and y.
    VACExampleVACEngine engine ("colA", "x", "y", "z");
    newtab.bindColumn ("colA", engine);
    Table tab(newtab, 10);

    // Fill the table via the virtual columns.
    ArrayColumn<VACExample> colA (tab,"colA");
    for (int i=0; i<10; i++) {
      Vector<VACExample> vac(i);
      for (int j=0; j<i; ++j) {
        vac[j] = VACExample(i+j, i+j+1, String::toString(i+j+2));
      }
      colA.put (i, vac);
    }

    //# Do an erroneous thing (binding an incorrect column).
    SetupNewTable newtab2("tVACEngine_tmp.dat2", td, Table::Scratch);
    newtab2.bindColumn ("x", engine);
    try {
	Table tab2(newtab2, 10);                // bound to incorrect column
    } catch (const AipsError& x) {
        cout << "Expected exception: " << x.getMesg() << endl;
    } 
}

void checkArrs (int st, int len, const Array<Int>& x, const Array<float>& y,
                const Array<String>& z)
{
  uInt sz = len;
  AlwaysAssertExit(x.size()==sz && y.size()==sz && z.size()==sz);
  AlwaysAssertExit(x.ndim()==1 && y.ndim()==1 && z.ndim()==1);
  for (int j=0; j<len; ++j) {
    String expz = String::toString(st+j+2);
    AlwaysAssertExit (x.data()[j] == st+j);
    AlwaysAssertExit (y.data()[j] == st+j+1);
    AlwaysAssertExit (z.data()[j] == expz);
  }
}

void checkVac (int st, int len, const Array<VACExample>& vac)
{
  AlwaysAssertExit(vac.size()==uInt(len));
  AlwaysAssertExit(vac.ndim()==1);
  for (int j=0; j<len; ++j) {
    String expz = String::toString(st+j+2);
    AlwaysAssertExit (vac.data()[j].x() == st+j);
    AlwaysAssertExit (vac.data()[j].y() == st+j+1);
    AlwaysAssertExit (vac.data()[j].z() == expz);
  }
}

void b()
{
  cout << 'x'<<endl;
    // Read back the table.
    Table tab("tVACEngine_tmp.data");
    ArrayColumn<Int>    colx (tab, "x");
    ArrayColumn<float>  coly (tab, "y");
    ArrayColumn<String> colz (tab, "z");
    ArrayColumn<VACExample> colA(tab, "colA");
    for (int i=0; i<10; i++) {
	cout << "get row " << i << endl;
        Array<Int> x = colx.get(i);
        Array<float> y = coly.get(i);
        Array<String> z = colz.get(i);
        Array<VACExample> vac = colA.get(i);
        checkArrs (i, i, x, y, z);
        checkVac (i, i, vac);
    }
    // Read a slice from a range of rows.
    cout << "get row range" << endl;
    Array<VACExample> vecA = colA.getColumnRange
      (Slicer(IPosition(1,3), IPosition(1,7)),      // rows
       Slicer(IPosition(1,1), IPosition(1,2)));     // array slice
    AlwaysAssertExit (vecA.ndim() == 2 && vecA.size() == 14);
    for (int i=0; i<7; i++) {
      checkVac (i+4, 2, vecA[i]);
    }
}
