//# tTableReadThreaded.cc: Test program for threaded access for table
//# WARNING:: mostly a problem reproducer at the moment
//# Table system is NOT threadsafe
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
#include <casacore/tables/Tables/TableProxy.h>
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
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Exceptions/Error.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>

// <summary>
// Test program for threaded table reading with locks
// </summary>

size_t num_threads = std::thread::hardware_concurrency(); 
rownr_t nrowStep = 2 << 14;
// fine grain locking.... make this a bit faster with a few random checks
rownr_t rwRowStep = nrowStep >> 3;
// single proxy -- internal locking between threads, decrase number of rows read
rownr_t rSPStep = nrowStep >> 1;

// helper method to create a non persistent fork pool
template <typename func, typename... Args>
int processpool(size_t no_processes, func& afunc, Args... args){
  // create "list" of processes using fork from child each time
  for(size_t id = 0; id < no_processes; ++id) {
    pid_t c_pid = fork();
    if (c_pid > 0) { // parent
      //nothing continue on to next thread
    } else if(c_pid == 0) { // child process
      try { 
        afunc(id, std::forward<Args>(args)...);
      } catch (AipsError& x) {
        cout << "Caught an AIPS exception in child process: " << x.getMesg() << endl;
        _exit(1);
      } catch (std::exception& x) {
        cout << "Caught an exception in child process: " << x.what() << endl;
        _exit(1);
      }
      _exit(0);
    } else {
      cout << "Forking failed" << endl;
      return -1;
    }
  }
  int retval = 0;
  for(size_t id = 0; id < no_processes; ++id) {
    int childres = 0;
    wait(&childres);
    retval = childres | retval;
  }
  return retval;
}

// Build a fresh table with given StorageMan
template <class TStorageMan, typename... Args>
void createTable(const String& tablename, const String& smName, Args... args)
{
  cout << "\tCreating " << smName << " table with " << num_threads << "*" << nrowStep << \
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
}

// Opens the table, do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
void readTableChunk (size_t chunkNo, const String& name, bool doLock=true)
{
  Table* tab = nullptr;
  if (doLock) {
    TableLock lock(TableLock::LockOption::UserLocking);
    tab = new Table(name, lock, Table::Old);
  } else {
    tab = new Table(name);
  }
  if (doLock) tab->lock(false); //no-write
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
    // read anywhere in the array not just the assigned chunk
    rownr_t rr = distr(eng) % (nrowStep*num_threads);
    Vector<int> res = ad.get(rr);
  }
  if (doLock) tab->unlock();
  delete tab;
}

// Reads from a provided Table object, do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
void readTableChunkSharedTable (size_t chunkNo, Table& tab, bool doLock=true)
{
  if (doLock) tab.lock(false); //no-write
  ArrayColumn<Int> ad(tab,"ad");
  // seq access
  for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
    if (i % nrowStep == 0) {
      Vector<int> res = ad.get(i);
      AlwaysAssertExit (res[0] == Int(i / nrowStep));
    }
  }
  std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<rownr_t> distr;

  // random access
  for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i+=rSPStep) {
    // read anywhere in the array not just the assigned chunk
    rownr_t rr = distr(eng) % (nrowStep*num_threads);
    Vector<int> res = ad.get(rr);
  }
  if (doLock) tab.unlock();
}

// Reads from a provided TableProxy object, do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
int readTableChunkSharedTableProxy (size_t chunkNo, TableProxy & tab, bool doLock=true)
{
  if (doLock) tab.lock(false, 0); //no-write
  // seq access
  for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
    if (i % nrowStep == 0) {
      Vector<Int> res = tab.getColumn("ad",i,1,1).asArrayInt();
      AlwaysAssertExit (res[0] == Int(i / nrowStep));
    }
  }
  std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<rownr_t> distr;

  // random access
  for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i+=rSPStep) {
    // read anywhere in the array not just the assigned chunk
    rownr_t rr = distr(eng) % (nrowStep*num_threads);
    Vector<Int> res = tab.getColumn("ad",rr,1,1).asArrayInt();
  }
  if (doLock) tab.unlock();
  return 0;
}

// Reads from a provided TableProxy object, 
// Copy constructs a reference TP of its own
// do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
int readTableChunkSharedTableProxyCreateTPRefAssign (size_t chunkNo, TableProxy & tab, bool doLock=true)
{
  try {
    TableProxy thisthreadTP;
    thisthreadTP = tab; // test assignment op
    if (doLock) thisthreadTP.lock(false, 0); //no-write
    // seq access
    for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
      if (i % nrowStep == 0) {
        Vector<Int> res = thisthreadTP.getColumn("ad",i,1,1).asArrayInt();
        AlwaysAssertExit (res[0] == Int(i / nrowStep));
      }
    }
    std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
    std::mt19937_64 eng(rd());
    std::uniform_int_distribution<rownr_t> distr;

    // random access
    for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i+=rSPStep) {
      // read anywhere in the array not just the assigned chunk
      rownr_t rr = distr(eng) % (nrowStep*num_threads);
      Vector<Int> res = thisthreadTP.getColumn("ad",rr,1,1).asArrayInt();
    }
    if (doLock) thisthreadTP.unlock();
  } catch (casacore::NotThreadSafeError& x) {
    return 1;
  }
  return 0;
}

// Reads from a provided TableProxy object, 
// copy constructs a reference table proxy
// do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
int readTableChunkSharedTableProxyCreateTPRefs (size_t chunkNo, TableProxy & tab, bool doLock=true)
{
  try {
    TableProxy thisthreadTP = tab; // test copy construct (ref)
    if (doLock) thisthreadTP.lock(false, 0); //no-write
    // seq access
    for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
      if (i % nrowStep == 0) {
        Vector<Int> res = thisthreadTP.getColumn("ad",i,1,1).asArrayInt();
        AlwaysAssertExit (res[0] == Int(i / nrowStep));
      }
    }
    std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
    std::mt19937_64 eng(rd());
    std::uniform_int_distribution<rownr_t> distr;

    // random access
    for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i+=rSPStep) {
      // read anywhere in the array not just the assigned chunk
      rownr_t rr = distr(eng) % (nrowStep*num_threads);
      Vector<Int> res = thisthreadTP.getColumn("ad",rr,1,1).asArrayInt();
    }
    if (doLock) thisthreadTP.unlock();
  } catch (casacore::NotThreadSafeError& x) {
    return 1;
  }
  return 0;
}

// Opens the table and alternate randomly between reading and writing
// with a UserLock anywhere inside the chunk assigned to this call
void readWriteTableChunk (size_t chunkNo, const String& name)
{
  
  TableLock lock(TableLock::LockOption::UserLocking);
  Table tab(name, lock, Table::Update);
  ArrayColumn<Int> ad(tab,"ad");
  
  std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<rownr_t> distr;

  // random access
  for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i+=rwRowStep) {
    rownr_t rr = distr(eng) % (nrowStep) + chunkNo*nrowStep;
    // flip flop between even/odd random numbers to determine if writing or reading
    if (rr % 1) { 
      tab.lock(false); //read lock
      Vector<int> res = ad.get(rr);
      tab.unlock();
    } else {
      tab.lock(true); //write lock
      ad.put(rr, Vector<int>(1));
      tab.unlock();
    }
  }
}

// Runs a storage manager locking test use case with threads
// can set whether the use case should be supported or not
int runSManTestLockTPThreaded(int (*fn)(size_t, TableProxy &, bool), 
                              bool doLock,
                              bool should_be_supported=true) {
  {
    Table* tab = nullptr;
    
    cout << "\t\tReading from table with " << \
      num_threads*nrowStep << " rows with " << num_threads << " threads...";
    if (doLock) {
      TableLock lock(TableLock::LockOption::UserLocking);
      tab = new Table("tVeryBigTable_tmp.tbl", lock, Table::Old);
    } else {
      tab = new Table("tVeryBigTable_tmp.tbl");
    }
    TableProxy tabp(*tab);
    std::vector<std::thread> threads;
    // async start a few threads each reading
    for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
      threads.push_back(std::thread(fn, iChunk, std::ref(tabp), false));
    }
    // await results
    for (size_t i = 0; i < num_threads; ++i) {
      threads.back().join();
      threads.pop_back();
    }
    cout << (should_be_supported ? "\t<OK>" : "\t<NotImplemented -- OK>") << endl;
    if (!should_be_supported) {
      // Reset the stale cache due to exception for the next test to start afresh
      for (size_t i = 0; i < PlainTable::tableCache().getTableNames().size(); ++i) {
        PlainTable::tableCache().remove(PlainTable::tableCache().getTableNames()[i]);
      }
      AlwaysAssertExit(PlainTable::tableCache().getTableNames().size() == 0) 
    }
    delete tab;
  }
  return 0;
}

// Do a series of *ReadOnly* tests with or without UserLocking
// Currently tests:
//
int runSManTestLock(bool doLock) {
  // Usage pattern 1 - SingleThreaded
  {
    cout << "\tRunning single threaded test (" << (doLock ? "Lock":"NoLock") << ")" << endl;
    try { 
      cout << "\t\tReading from table with " << \
        num_threads*nrowStep << " rows...";
      for (size_t iChunk=0; iChunk<num_threads; ++iChunk)
        readTableChunk(iChunk, "tVeryBigTable_tmp.tbl", false);
      cout << "\t<OK>" << endl;
    } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
    } 
  }
  // Usage pattern 2 - MultiThreaded - table per thread
  // (The performant case)
  {
    cout << "\tRunning multi-threaded test (" << (doLock ? "Lock":"NoLock") << ")" << endl;
    try {
      cout << "\t\tReading from table with " << \
        num_threads*nrowStep << " rows with " << num_threads << " threads...";
      std::vector<std::thread> threads;
      // async start a few threads each reading
      for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
        threads.push_back(std::thread(readTableChunk, iChunk, "tVeryBigTable_tmp.tbl", false));
      }
      // await results
      for (size_t i = 0; i < num_threads; ++i) {
        threads.back().join();
        threads.pop_back();
      }
      cout << "\t<OK>" << endl;
    } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
    } 
  }
  
  // Usage pattern 3 - MultiThreaded - one table proxy across many threads
  {
    cout << "\tRunning single TableProxy object with multiple threads test (" << (doLock ? "Lock":"NoLock") << ")" << endl;
    runSManTestLockTPThreaded(readTableChunkSharedTableProxy,
                              doLock, true);
  }
  // Segfaults -- Unimplemented not threadsafe
  // // Usage pattern 4 - MultiThreaded - one Table across many threads
  // // Currently not implemented -- should bomb with a decent exception
  // // prior behaviour is to fall over with segfault...
  // {
  //   cout << "\tRunning single Table object with multiple threads test (" << (doLock ? "Lock":"NoLock") << ")" << endl;
  //   Table* tab = nullptr;
  //   try {
  //     cout << "\t\tReading from table with " << \
  //       num_threads*nrowStep << " rows with " << num_threads << " threads...";
  //     if (doLock) {
  //       TableLock lock(TableLock::LockOption::UserLocking);
  //       tab = new Table("tVeryBigTable_tmp.tbl", lock, Table::Old);
  //     } else {
  //       tab = new Table("tVeryBigTable_tmp.tbl");
  //     }
  //     std::vector<std::thread> threads;
  //     // async start a few threads each reading
  //     for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
  //       threads.push_back(std::thread(readTableChunkSharedTable, iChunk, std::ref(*tab), false));
  //     }
  //     // await results
  //     for (size_t i = 0; i < num_threads; ++i) {
  //       threads.back().join();
  //       threads.pop_back();
  //     }
  //     cout << "\t<NotImplemented -- OK>" << endl;
  //     // Reset the stale cache due to exception for the next test to start afresh
  //     for (size_t i = 0; i < PlainTable::tableCache().getTableNames().size(); ++i) {
  //       PlainTable::tableCache().remove(PlainTable::tableCache().getTableNames()[i]);
  //     }
  //     AlwaysAssertExit(PlainTable::tableCache().getTableNames().size() == 0)
  //   } catch (AipsError& x) {
  //     cout << "Caught an exception: " << x.getMesg() << endl;
  //     if (tab != nullptr) { delete tab; }
  //     return 1;
  //   }
  //   delete tab;
  // }
  // Usage pattern 5 reading with a pool of processes
  {
    cout << "\tRunning multiple processes test (MPI-style) (" << (doLock ? "Lock":"NoLock") << ")" << endl;
    try { 
      cout << "\t\tReading from table with " << \
        num_threads*nrowStep << " rows...";
      processpool(num_threads, readTableChunk, "tVeryBigTable_tmp.tbl", false);
      cout << "\t<OK>" << endl;
    } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
    } 
  }
  // Usage pattern 6 - MultiThreaded - one table proxy across many threads, assign ref
  {
    cout << "\tRunning single TableProxy object with multiple threads test, " \
              "thread copy construct shallow copy TabProxy (" << (doLock ? "Lock":"NoLock") << ")" << endl;
    runSManTestLockTPThreaded(readTableChunkSharedTableProxyCreateTPRefAssign,
                              doLock, false);
  }
  // Usage pattern 7 - MultiThreaded - one table proxy across many threads, shallow copy
  {
    cout << "\tRunning single TableProxy object with multiple threads test, " \
              "thread construct shallow copy TabProxy via assignment (" << (doLock ? "Lock":"NoLock") << ")" << endl;
    runSManTestLockTPThreaded(readTableChunkSharedTableProxyCreateTPRefs,
                              doLock, false);
  }
  return 0;
}

int runSManTestRW() {
  // Usage pattern 1 - SingleThreaded
  {
    cout << "\tRunning single threaded test (UserLock)" << endl;
    try { 
      cout << "\t\tReading from and writing to table with " << \
        num_threads*nrowStep << " rows...";
      for (size_t iChunk=0; iChunk<num_threads; ++iChunk)
        readWriteTableChunk(iChunk, "tVeryBigTable_tmp.tbl");
      cout << "\t<OK>" << endl;
    } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
    } 
  }
  // Segfaults even with a pooled TableCache
  // // Usage pattern 2 - MultiThreaded - table per thread
  // // (The performant case)
  // {
  //   cout << "\tRunning multi-threaded test (UserLock)" << endl;
  //   try {
  //     cout << "\t\tReading from and writing to table with " << \
  //       num_threads*nrowStep << " rows with " << num_threads << " threads...";
  //     std::vector<std::thread> threads;
  //     // async start a few threads each reading
  //     for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
  //       threads.push_back(std::thread(readWriteTableChunk, iChunk, "tVeryBigTable_tmp.tbl"));
  //     }
  //     // await results
  //     for (size_t i = 0; i < num_threads; ++i) {
  //       threads.back().join();
  //       threads.pop_back();
  //     }
  //     cout << "\t<OK>" << endl;
  //   } catch (AipsError& x) {
  //     cout << "Caught an exception: " << x.getMesg() << endl;
  //     return 1;
  //   } 
  // }
  // Usage pattern 3 - Processpooled processing
  {
    cout << "\tRunning multiple processes test (MPI-style) (UserLock)" << endl;
    try { 
      cout << "\t\tReading from and writing to table with " << \
        num_threads*nrowStep << " rows...";
      processpool(num_threads, readWriteTableChunk, "tVeryBigTable_tmp.tbl");
      cout << "\t<OK>" << endl;
    } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
    } 
  }
  return 0;
}

template<class StMan, typename... Args>
int runSManTest(const String& smName, Args... smArgs) {
  cout << "Testing " << smName << " storage manager" << endl;
  try {
    createTable<StMan>("tVeryBigTable_tmp.tbl",
                                  smName,
                                  smArgs...); //cachesize
    cout << "\t<OK>" << endl;
  } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
  }
  if (runSManTestRW()) return 1;
  if (runSManTestLock(false)) return 1;
  if (runSManTestLock(true)) return 1;
  cout << "\t<" << smName << " -- all OK>" << endl;
  return 0;
}

int main()
{  
  if (runSManTest<IncrementalStMan>("IncrementalStMan",
                                    256, //bucket size
                                    True, //check bucket
                                    30)) { //cachesize
      return 1;
  }
  if (runSManTest<StandardStMan>("StandardStMan",
                                 4*1024*1024)) {
      return 1;
  }
  if (runSManTest<TiledShapeStMan>("TiledStMan",
                                   IPosition(2,1,1024*1024))) {
      return 1;
  }
  cout << "OK" << endl;
  return 0;                           // exit with success status
}
