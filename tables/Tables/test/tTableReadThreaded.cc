//# tVeryBigTable.cc: Test program for a very large table
//# Copyright (C) 2019
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
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>
#include <thread>
#include <pthread.h>
#include <random>
#include <casacore/tables/Tables/PlainTable.h>
// <summary>
// Test program for threaded table reading with locks
// </summary>

size_t num_threads = 50;//std::thread::hardware_concurrency(); 
rownr_t nrowStep = 2 << 10;


template <class TStorageMan, typename... Args>
void createTable(const String& tablename, Args... args)
{
  cout << "Creating table with " << num_threads << "*" << nrowStep << \
          " rows (" << num_threads*nrowStep / (1024.*1024.) << "MiB)...";
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Int>("ad", IPosition(1,1)));
  // Now create a new table from the description.
  SetupNewTable newtab(tablename, td, Table::New);
  // Create a storage manager for it.
  TStorageMan sm1 ("SM", args...);
  newtab.bindAll (sm1);
  Table tab(newtab, 0);
  ArrayColumn<Int> ad(tab,"ad");
  Vector<Int> vec(1);
  rownr_t rownr = 0;
  for (rownr_t i=0; i<num_threads; i++) {
    tab.addRow (nrowStep);
    vec[0] = i;
    ad.put (rownr, vec);
    ad.put (rownr+1, vec);
    rownr +=  nrowStep/2;
    ad.put (rownr-1, vec);
    ad.put (rownr, vec);
    ad.put (rownr+1, vec);
    rownr +=  nrowStep/2;
    ad.put (rownr-2, vec);
    ad.put (rownr-1, vec);
  }
  AlwaysAssertExit (tab.nrow() == num_threads * nrowStep);
  // for testing purposes close the Cache so that we can keep tally in the reading later
  //PlainTable::tableCache().remove("tVeryBigTable_tmp.tbl");
}

void readTableChunk (const String& name, bool doLock=true, size_t chunkNo=0)
{
  //cout << endl << "\nCache number" << &PlainTable::tableCache() << endl;
  //AlwaysAssertExit (PlainTable::tableCache().getTableNames().size() == 0);
  Table* tab = nullptr;
  if (doLock) {
    TableLock lock(TableLock::LockOption::UserLocking);
    tab = new Table(name, lock, Table::Old);
  } else {
    tab = new Table(name);
  }
  AlwaysAssertExit (PlainTable::tableCache().getTableNames().size() == 1);
  if (doLock) tab->lock(false); //no-write
  AlwaysAssertExit (tab->nrow() == num_threads * nrowStep);
  ArrayColumn<Int> ad(*tab,"ad");
  // seq access
  for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
    Vector<int> res = ad.get(i);
    if (i % nrowStep == 0)
      AlwaysAssertExit (res[0] == Int(i / nrowStep));
    ad(i); // just read the rest
  }
  std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<rownr_t> distr;

  // random access
  for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i++) {
    rownr_t rr = distr(eng) % (num_threads * nrowStep) ;
    Vector<int> res = ad.get(rr);
  }
  if (doLock) tab->unlock();
  delete tab;
}

int main()
{
  try {
    createTable<IncrementalStMan>("tVeryBigTable_tmp.tbl", 
                                  256, //bucket size
                                  True, //check bucket
                                  30); //cachesize
    //createTable<StandardStMan>("tVeryBigTable_tmp.tbl", 4*1024*1024);
    //createTable<TiledShapeStMan>("tVeryBigTable_tmp.tbl", IPosition(2,1,1024*1024));
    cout << "\t<OK>" << endl;
  } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
  } 
  // Usage pattern 1 - SingleThreaded
  {
    cout << "Running single threaded test" << endl;
    try { 
      cout << "\tReading from table with " << \
        num_threads*nrowStep << " rows...";
      for (size_t iChunk=0; iChunk<num_threads; ++iChunk)
        readTableChunk("tVeryBigTable_tmp.tbl", false, iChunk);
      cout << "\t<OK>" << endl;
    } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
    } 
  }
  // Usage pattern 2 - MultiThreaded - table per thread
  // (The performant case)
  {
    // empty the cache - lets check if it stays empty in the parent
    //PlainTable::tableCache().remove("tVeryBigTable_tmp.tbl");
    cout << "Running multi-threaded test" << endl;
    try {
      //parent thread does not contain table at this point
      //AlwaysAssertExit (PlainTable::tableCache().getTableNames().size() == 0);
      cout << "\tReading from table with " << \
        num_threads*nrowStep << " rows with " << num_threads << " threads...";
      std::vector<std::thread> threads;
      // async start a few threads each reading
      for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
        threads.push_back(std::thread(readTableChunk, "tVeryBigTable_tmp.tbl", false, iChunk));
      }
      // await results
      for (size_t i = 0; i < num_threads; ++i) {
        threads.back().join();
        threads.pop_back();
      }
      //parent thread does not contain table at this point
      //AlwaysAssertExit (PlainTable::tableCache().getTableNames().size() == 0);
      cout << "\t<OK>" << endl;
    } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
    } 
  }
  // Usage pattern 1 - Threads each with their own table -- cache should not be shared between them
  cout << "OK" << endl;
  return 0;                           // exit with success status
}
