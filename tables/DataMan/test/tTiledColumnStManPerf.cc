//# tTiledColumnStManPerf.cc: Test program for TiledColumnStMan performance
//# Copyright (C) 2022
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
#include <casacore/tables/Tables/TableIter.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/TiledStManAccessor.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>


// <summary>
// Test program for the TiledColumnStMan performance.
// </summary>

// This program writes a Data column using TiledColumnStMan. It simulates the
// data in a MeasurementSet (including TIME and ANTENNA1/2).
// The rows in it are written in a tiled and non-tiled way to compare the two.
// The data are read back in sequential and baseline order to test performance.
// The shapes can be given on the command line.


void createTable (const IPosition& dataShape, const IPosition& tileShape,
                  const IPosition& rowShape, uInt nant, uInt ntime,
                  Bool useSSM, Bool useMF)
{
  uInt nbl = nant*(nant+1)/2;
  cout << "Create data table with " << nbl*ntime << " rows" << endl;
  if (useMF) {
    cout << " using MultiFile  ";
  }
  cout << " DATA column is stored using ";
  if (useSSM) {
    cout << "StandardStMan" << endl;
  } else {
    cout << "TiledColumnStMan" << endl;
  }
  cout << "  npol  = " << dataShape[0] << endl;
  cout << "  nfreq = " << dataShape[1] << endl;
  cout << "  nant  = " << nant <<  "   (nbl = " << nbl << ')' << endl;
  cout << "  ntime = " << ntime << endl;
  if (! useSSM) {
    cout << "  tileShape = " << tileShape << "   rowShape = " << rowShape << endl;
  }
  Timer timer;
  // Build the table description.
  TableDesc td ("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<uInt> ("ANT1"));
  td.addColumn (ScalarColumnDesc<uInt> ("ANT2"));
  td.addColumn (ScalarColumnDesc<double> ("TIME"));
  td.addColumn (ArrayColumnDesc<Complex>  ("DATA", dataShape));
  // Now create a new table from the description.
  StorageOption stopt (useMF ? StorageOption::MultiFile : StorageOption::SepFile);
  SetupNewTable newtab("tTiledColumnStManPerf_tmp.tab", td, Table::New, stopt);
  // Create a storage manager for it. First fill the tile shape.
  if (! useSSM) {
    TiledColumnStMan tcs ("TSM", tileShape, rowShape);
    newtab.bindColumn ("DATA", tcs);
  }
  // Create the table.
  Table tab (newtab, nbl*ntime);
  // Fill the table.
  Matrix<Complex> data(dataShape);
  indgen(data);
  ArrayColumn<Complex> dataCol(tab, "DATA");
  ScalarColumn<uInt>   ant1Col(tab, "ANT1");
  ScalarColumn<uInt>   ant2Col(tab, "ANT2");
  ScalarColumn<double> timeCol(tab, "TIME");
  // Write all rows.
  rownr_t rownr = 0;
  for (uInt t=0; t<ntime; ++t) {
    for (uInt a2=0; a2<nant; ++a2) {
      for (uInt a1=a2; a1<nant; ++a1) {
        data += Complex(1,2);
        dataCol.put (rownr, data);
        ant1Col.put (rownr, a1);
        ant2Col.put (rownr, a2);
        timeCol.put (rownr, t + 0.5);
        ++rownr;
      }
    }
  }
  AlwaysAssertExit (tab.nrow() == nbl*ntime);
  timer.show ("Create table");
  if (! useSSM) {
    ROTiledStManAccessor acc(tab, "TSM");
    acc.showCacheStatistics (cout);
  }
}

void readSequential (Bool useMMap)
{
  cout << "Read in sequential order";
  if (useMMap) cout << " using memory-mapped files";
  cout << endl;
  Timer timer;
  TSMOption tsmOpt (useMMap ? TSMOption::MMap : TSMOption::Cache);
  Table tab("tTiledColumnStManPerf_tmp.tab", Table::Old, tsmOpt);
  ArrayColumn<Complex> dataCol(tab, "DATA");
  Matrix<Complex> data(dataCol.shape(0));
  indgen(data);
  // Write all rows.
  uInt ndone = 0;
  for (rownr_t rownr=0; rownr<tab.nrow(); ++rownr) {
    //data += Complex(1,2);
    ndone++;
    AlwaysAssertExit (allNear (dataCol(rownr),
                               data + float(ndone)*Complex(1,2), 1e-3));
  }
  timer.show ("Read sequentially");
  try {
    ROTiledStManAccessor acc(tab, "TSM");
    acc.showCacheStatistics (cout);
  } catch (std::exception&) {
  }
}

void readBaseline (Bool useMMap)
{
  cout << "Read in baseline order";
  if (useMMap) cout << " using memory-mapped files";
  cout << endl;
  Timer timer;
  TSMOption tsmOpt (useMMap ? TSMOption::MMap : TSMOption::Cache);
  Table tab("tTiledColumnStManPerf_tmp.tab", Table::Old, tsmOpt);
  // Iterate on baseline
  Block<String> columns(2);
  columns[0] = "ANT2";
  columns[1] = "ANT1";
  TableIterator tabIter(tab, columns);
  uInt nbl = tab.nrow() / tabIter.table().nrow();
  uInt nant = round((-1 + sqrt(1 + 8*nbl)) / 2);
  cout << "  nant  = " << nant <<  "   (nbl = " << nbl << ')' << endl;
  cout << "  ntime = " << tabIter.table().nrow() << endl;
  uInt ndone = 0;
  for (uInt a2=0; a2<nant; ++a2) {
    for (uInt a1=a2; a1<nant; ++a1) {
      ndone++;
      ArrayColumn<Complex> dataCol(tabIter.table(), "DATA");
      ScalarColumn<uInt>   ant1Col(tabIter.table(), "ANT1");
      ScalarColumn<uInt>   ant2Col(tabIter.table(), "ANT2");
      ScalarColumn<double> timeCol(tabIter.table(), "TIME");
      Matrix<Complex> data(dataCol.shape(0));
      indgen(data, float(ndone)*Complex(1,2));
      Double tm = 0.5;
      // Read all rows.
      for (rownr_t rownr=0; rownr<tabIter.table().nrow(); ++rownr) {
        AlwaysAssertExit (ant1Col(rownr) == a1);
        AlwaysAssertExit (ant2Col(rownr) == a2);
        AlwaysAssertExit (near (timeCol(rownr), tm));
        if (! (allNear (dataCol(rownr), data, 1e-3))) {
          cout<<dataCol(rownr)<<data<<rownr<<' '<<a1<<' '<<a2<<' '<<tm<<endl;
        }
        AlwaysAssertExit (allNear (dataCol(rownr), data, 1e-3));
        tm += 1;
        data += float(nbl)*Complex(1,2);
      }
      tabIter.next();
    }
  }
  AlwaysAssertExit (tabIter.pastEnd());
  timer.show ("Read in bl order ");
  try {
    ROTiledStManAccessor acc(tab, "TSM");
    acc.showCacheStatistics (cout);
  } catch (std::exception&) {
  }
}

void showHelp()
{
  cerr << "This program tests MS access performance by creating and reading back an"
       << endl;
  cerr << "MS-like table. The table rows can be stored in a tiled way to optimize"
       << endl;
  cerr << "for access in order of baseline." << endl;
  cerr << "Reading back can be done in sequential or baseline order." << endl;
  cerr << "It is also possible to use memory-mapped data access using mmap." << endl;
  cerr << endl;
  cerr << "Run as:   tTiledColumnStManPerf np nf na nt ntp ntf ntb ntt [ssm/mf]";
  cerr << "      to create" << endl;
  cerr << "      where nx gives the data size and ntx the tile size" << endl;
  cerr << "      and in nx and ntx:  p=pol  f=freq  a=antenna  b=baseline  t=time"
       << endl;
  cerr << "      ntt<=0  means that rows are not tiled on baseline/time" << endl;
  cerr << "      If 'ssm' is given, StandardStMan instead of TiledColumnStMan is used."
       << endl;
  cerr << "      If 'mf' is given the MultiFile option is used. 'mfssm' is both." <<endl;
  cerr << "or as:    tTiledColumnStManPerf type [mmap]              ";
  cerr << "      to read back" << endl;
  cerr << "      type = bl     read in order of baseline" << endl;
  cerr << "             else   read sequentially" << endl;
  cerr << "      mmap=1  means use mmap instead of normal reading" << endl;
}

int main (int argc, char* argv[])
{
  if (argc <= 1  ||  (argc > 3  &&  argc <= 8)) {
    showHelp();
    exit(0);
  }
  try {
    if (argc > 8) {
      int  nt4   = atoi(argv[8]);
      uInt nant  = atoi(argv[3]);
      uInt ntime = atoi(argv[4]);
      uInt nbl = nant * (nant+1) / 2;
      IPosition dataShape(2, atoi(argv[1]), atoi(argv[2]));
      IPosition rowShape, tileShape;
      if (nt4 <= 0) {
        tileShape = IPosition(3, atoi(argv[5]), atoi(argv[6]), atoi(argv[7]));
      } else {
        tileShape = IPosition(4, atoi(argv[5]), atoi(argv[6]), atoi(argv[7]), nt4);
        rowShape  = IPosition(2, nbl, ntime);
      }
      Bool useSSM = (argc > 9  &&
                     (String(argv[9]) == "ssm" || String(argv[9]) == "mfssm"));
      Bool useMF  = (argc > 9  &&
                     (String(argv[9]) == "mf"  || String(argv[9]) == "mfssm"));
      createTable (dataShape, tileShape, rowShape, nant, ntime, useSSM, useMF);
    } else {
      Bool useMMap = (argc > 2  &&  argv[2][0] == '1');
      if (argc > 1  &&  String(argv[1]) == "bl") {
        readBaseline (useMMap);
      } else {
        readSequential (useMMap);
      }
    }
  } catch (const std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
