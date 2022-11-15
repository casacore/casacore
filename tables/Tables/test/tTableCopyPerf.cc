//# tTableCopy.cc: Test program for column copy performance
//# Copyright (C) 2016
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/Tables.h>
#include <casacore/casa/OS/Timer.h>
#include <stdexcept>
#include <iostream>
using namespace casacore;
using namespace std;

void testPerf (Int nrowPerf)
{
  cout << "testPerf with " << nrowPerf << " rows ..." << endl;
  // First create a table.
  TableDesc td;
  td.addColumn (ArrayColumnDesc<Complex>("DATA", IPosition(2,4,256)));
  td.addColumn (ScalarColumnDesc<Int>("SCALAR", 1));
  SetupNewTable newtab("tTableCopyPerf_tmp.data", td, Table::New);
  StandardStMan ssm;
  TiledShapeStMan tsm("DATA_stm", IPosition(3,4,256,4));
  newtab.bindAll (ssm);
  newtab.bindColumn ("DATA", tsm);
  Table tab(newtab, nrowPerf);
  ArrayColumn<Complex> col(tab, "DATA");
  Array<Complex> arr(IPosition(2,4,256));
  indgen(arr);
  Timer timer;
  for (uInt row=0; row<tab.nrow(); ++row) {
    col.put (row, arr);
  }
  timer.show ("put rows");
  TableCopy::cloneColumn (tab, "DATA", tab, "DATA2");
  timer.mark();
  TableCopy::copyColumnData (tab, "DATA", tab, "DATA2");
  timer.show ("copycol ");
  TableCopy::cloneColumnTyped<DComplex> (tab, "DATA", tab, "DATA3");
  timer.mark();
  TableCopy::copyColumnData (tab, "DATA", tab, "DATA3");
  timer.show ("copycold");
  TableCopy::cloneColumnTyped<DComplex> (tab, "DATA", tab, "DATA4");
  timer.mark();
  tableCommand ("update $1 set DATA4=DATA", tab);
  timer.show ("copytaql");
}

int main (int argc, const char* argv[])
{
  Int nrowPerf = 10;
  if (argc > 1) {
    nrowPerf = atoi(argv[1]);
  }
  try {
    testPerf (nrowPerf);
  } catch (const exception& x) {
    cout << x.what() << endl;
    return 1;
  }
  return 0;
}
