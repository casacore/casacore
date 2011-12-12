//# tTable_4.cc: Interactive test program for adding/removing columns
//# Copyright (C) 2001
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

#include <tables/Tables/TableDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/StManAipsIO.h>
#include <tables/Tables/IncrementalStMan.h>
#include <tables/Tables/StandardStMan.h>
#include <tables/Tables/TiledShapeStMan.h>
#include <tables/Tables/TiledColumnStMan.h>
#include <tables/Tables/ForwardCol.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Utilities/Regex.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

// <summary>
// Interactive test program for adding/removing columns.
// </summary>

// Create a table with the given storage manager and data type.
// Write 10 rows with the given value.
template<typename T>
Table createSSM (T value, T step)
{
  TableDesc desc;
  desc.addColumn (ScalarColumnDesc<T>("scacol"));
  desc.addColumn (ArrayColumnDesc<T>("arrcol"));
  SetupNewTable newtab ("tTableAccess_tmp.tab", desc, Table::New);
  int nrow = 2;
  Table tab(newtab, nrow);
  ScalarColumn<T> scacol(tab, "scacol");
  ArrayColumn<T>  arrcol(tab, "arrcol");
  Array<T> arr(IPosition(1,3));
  for (int i=0; i<nrow; ++i) {
    scacol.put (i, value);
    arr = value;
    arrcol.put (i, arr);
    value += step;
  }
  tab.flush();
  return tab;
}

template<typename T>
Table createISM (T value, T step)
{
  TableDesc desc;
  desc.addColumn (ScalarColumnDesc<T>("scacol"));
  desc.addColumn (ArrayColumnDesc<T>("arrcol"));
  SetupNewTable newtab ("tTableAccess_tmp.tab", desc, Table::New);
  IncrementalStMan ism;
  newtab.bindAll (ism);
  int nrow = 2;
  Table tab(newtab, nrow);
  ScalarColumn<T> scacol(tab, "scacol");
  ArrayColumn<T>  arrcol(tab, "arrcol");
  Array<T> arr(IPosition(1,3));
  for (int i=0; i<nrow; ++i) {
    scacol.put (i, value);
    arr = value;
    arrcol.put (i, arr);
    value += step;
  }
  tab.flush();
  return tab;
}

template<typename T>
Table createForward (const Table& table)
{
  TableDesc desc;
  desc.addColumn (ScalarColumnDesc<T>("scacol"));
  desc.addColumn (ArrayColumnDesc<T>("arrcol"));
  SetupNewTable newtab ("tTableAccess_tmp.tab1", desc, Table::New);
  ForwardColumnEngine fcsm(table);
  newtab.bindAll (fcsm);
  int nrow = 2;
  Table tab(newtab, nrow);
  return tab;
}

// Remove the dirname from the table name in an error message.
String removeDir (const String& msg)
{
  String s = msg;
  s.gsub (Regex("/.*/t"), "t");
  return s;
}

template<typename T>
void readSca (const Table& tab)
{
  ROScalarColumn<T> col(tab, "scacol");
  uInt nrow = tab.nrow();
  for (uInt i=0; i<nrow; ++i) {
    cout << col.get(i) << " ";
  }
  cout << endl;
  RefRows rr(0, tab.nrow()-1);
  cout << col.getColumnCells (rr);
}

template<typename T>
void readArr (const Table& tab)
{
  ROArrayColumn<T> col(tab, "arrcol");
  uInt nrow = tab.nrow();
  for (uInt i=0; i<nrow; ++i) {
    cout << col.get(i) << endl;
  }
  RefRows rr(0, tab.nrow()-1);
  cout << col.getColumnCells (rr);
}

void readTable (const Table& tab)
{
  readSca<Int> (tab);
  readArr<Int> (tab);
  Vector<uInt> rownrs(1,1);
  Table tab2 = tab(rownrs);
  readSca<Int> (tab2);
  readArr<Int> (tab2);
  Block<Table> tables(1);
  tables[0] = tab;
  Table tabc1(tables);
  readSca<Int> (tabc1);
  readArr<Int> (tabc1);
  tables[0] = tab2;
  Table tabc2(tables);
  readSca<Int> (tabc2);
  readArr<Int> (tabc2);
}

void doIt()
{
  readTable (createSSM<Int> (1,1));
  readTable (createISM<Int> (1,2));
  readTable (createForward<Int> (Table("tTableAccess_tmp.tab")));
}

int main()
{
  try {
    doIt();
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
