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
const rownr_t nrowStep = 2 << 14;
// fine grain locking.... make this a bit faster with a few random checks
const rownr_t rwRowStep = nrowStep >> 3;
// single proxy -- internal locking between threads, decrase number of rows read
const rownr_t rSPStep = nrowStep >> 1;

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
  TableDesc td("MYTABLESCHEMA", "1", TableDesc::Scratch);
  td.addColumn (ArrayColumnDesc<Int>("ad", IPosition(1,1)));
  td.addColumn (ScalarColumnDesc<Int>("indexcol"));
  // Now create a table setup from the description.
  SetupNewTable newtab(tablename, td, Table::New);
  // Create and bind a storage manager for it.
  newtab.bindColumn("ad", TStorageMan("SM", args...));
  
  // Create a concrete table for this table setup
  Table concreteTab(newtab);
  ArrayColumn<Int> ad(concreteTab, "ad");
  ScalarColumn<Int> ic(concreteTab, "indexcol");
  // Place some data now
  Vector<Int> vec(1);
  rownr_t rownr = 0;
  concreteTab.addRow (num_threads * nrowStep, true);
  //initialize index column for the entire array
  for (rownr_t i=0; i<num_threads * nrowStep; i++) {
    ic.put(i, Int(i));
  }
  //place a pattern in our data column "ad"
  for (rownr_t i=0; i<num_threads; i++) {
    vec[0] = i;
    ad.put (rownr, vec);
    ad.put (rownr+1, vec);
    rownr +=  nrowStep/2;
    vec[0] = -i;
    ad.put (rownr-1, vec);
    vec[0] = i;
    ad.put (rownr, vec);
    ad.put (rownr+1, vec);
    rownr +=  nrowStep/2;
    ad.put (rownr-2, vec);
    ad.put (rownr-1, vec);
  }
  AlwaysAssertExit (concreteTab.nrow() == num_threads * nrowStep);
}

void __verifyPattern(rownr_t i, ArrayColumn<Int>& ad, bool doReverse=false) {
  Int resread;
  switch (i % nrowStep){
      case 0:
        resread = *(ad.get(i).begin());
        AlwaysAssertExit (resread == Int(i/nrowStep));
        break;
      case 1:
        resread = *(ad.get(i).begin());
        AlwaysAssertExit (resread == Int(i/nrowStep));
        break;
      case nrowStep/2-1:
        resread = *(ad.get(i).begin());
        AlwaysAssertExit (resread == Int(i/nrowStep) * (doReverse ? 1 : -1));
        break;
      case nrowStep/2:
        resread = *(ad.get(i).begin());
        AlwaysAssertExit (resread == Int(i/nrowStep));
        break;
      case nrowStep/2+1:
        resread = *(ad.get(i).begin());
        AlwaysAssertExit (resread == Int(i/nrowStep) * (doReverse ? -1 : +1));
        break;
      case nrowStep-2:
        resread = *(ad.get(i).begin());
        AlwaysAssertExit (resread == Int(i/nrowStep));
        break;
      case nrowStep-1:
        resread = *(ad.get(i).begin());
        AlwaysAssertExit (resread == Int(i/nrowStep));
        break;
      default:
        break;
    }
}

void __verifyPattern(rownr_t i, Vector<Int>& col, bool doReverse=false) {
  AlwaysAssertExit (col[0] == Int(i));    
  AlwaysAssertExit (col[1] == Int(i));    
  AlwaysAssertExit (col[nrowStep/2-1] == Int(i) * (doReverse ? 1 : -1));    
  AlwaysAssertExit (col[nrowStep/2] == Int(i));    
  AlwaysAssertExit (col[nrowStep/2+1] == Int(i) * (doReverse ? -1 : +1));    
  AlwaysAssertExit (col[nrowStep-2] == Int(i));
  AlwaysAssertExit (col[nrowStep-1] == Int(i));    
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
  Vector<int> res;
  for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
    __verifyPattern(i, ad);
  }
  std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<rownr_t> distr;

  // random access
  for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i++) {
    // read anywhere in the array not just the assigned chunk
    rownr_t rr = distr(eng) % (nrowStep*num_threads);
    Vector<int> res = ad.get(rr);
    __verifyPattern(i, ad);
  }
  if (doLock) tab->unlock();
  delete tab;
}

// Opens the table, do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
// exec TaQL
void readTableChunkDoSelect (size_t chunkNo, const String& name, bool doLock=true)
{
  Table* tab = nullptr;
  if (doLock) {
    TableLock lock(TableLock::LockOption::UserLocking);
    tab = new Table(name, lock, Table::Old);
  } else {
    tab = new Table(name);
  }
  TableProxy tabP(*tab);
  std::vector<TableProxy> tabPs(1);
  tabPs[0] = tabP;
  AlwaysAssertExit(doLock); // TaQL requires the userlock locked
  if (doLock) tab->lock(false); //no-write
  TableProxy tabPTaQL("SELECT * FROM " + tabP.table().getPartNames()[0] + " ORDERBY DISTINCT indexcol DESC", tabPs);
  ArrayColumn<Int> ad(*tab,"ad");
  // seq access
  for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
    Vector<int> res = ad.get(i);
    __verifyPattern(i, ad, false);
  }
  std::random_device rd;     //Get a random seed from the OS entropy device, or whatever
  std::mt19937_64 eng(rd());
  std::uniform_int_distribution<rownr_t> distr;

  // random access
  for (rownr_t i=chunkNo; i<(chunkNo+1) * nrowStep; i++) {
    // read anywhere in the array not just the assigned chunk
    rownr_t rr = distr(eng) % (nrowStep*num_threads);
    Vector<int> res = ad.get(rr);
    __verifyPattern(i, ad);
  }
  if (doLock) tab->unlock();
  delete tab;
}

// Reads from a provided TableProxy object, do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
void readTableChunkSharedTableProxy (size_t chunkNo, TableProxy & tab, bool doLock=true)
{
  if (doLock) tab.lock(false, 0); //no-write
  // seq access
  for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
    if (i % nrowStep == 0) {
      Vector<Int> res = tab.getColumn("ad",i,1,1).asArrayInt();
      AlwaysAssertExit (res[0] == Int(i / nrowStep));
    }
  }

  Vector<Int> res = tab.getColumn("ad",chunkNo*nrowStep,nrowStep,1).asArrayInt();
  __verifyPattern(chunkNo, res);
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
}

// Reads from a provided TableProxy object, 
// Copy constructs a reference TP of its own
// do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
void readTableChunkSharedTableProxyCreateTPRefAssign (size_t chunkNo, TableProxy & tab, bool doLock=true)
{
  bool doExcept = false;
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

    Vector<Int> res = tab.getColumn("ad",chunkNo*nrowStep,nrowStep,1).asArrayInt();
    __verifyPattern(chunkNo, res);
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
    doExcept = true;
  }
  AlwaysAssertExit (doExcept);
}

// Reads from a provided TableProxy object, 
// copy constructs a reference table proxy
// do basic verification reads and then read randomly anywhere
// 0...nthread*nchunk, up to chunksize per thread times
void readTableChunkSharedTableProxyCreateTPRefs (size_t chunkNo, TableProxy & tab, bool doLock=true)
{
  bool doExcept = false;
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

    Vector<Int> res = tab.getColumn("ad",chunkNo*nrowStep,nrowStep,1).asArrayInt();
    __verifyPattern(chunkNo, res);
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
    doExcept = true;
  }
  AlwaysAssertExit (doExcept);
}

// // Reads from a provided TableProxy object, 
// // performs selection
void readTableChunkSharedTableProxyDoSelect (size_t chunkNo, TableProxy & tab, bool doLock=true)
{
  bool doExcept = false;
  try {
    std::vector<TableProxy> tabs(1);
    tabs[0] = tab;
    TableProxy tabsel(String("SELECT * FROM " + tab.table().getPartNames()[0] + " ORDERBY DISTINCT indexcol DESC"), tabs);

    if (doLock) tabsel.lock(false, 0); //no-write
    // seq access
    for (rownr_t i=chunkNo*nrowStep; i<(chunkNo+1) * nrowStep; i++) {
      if (i % nrowStep == 0) {
        Vector<Int> res = tabsel.getColumn("ad2",i,1,1).asArrayInt();
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
      Vector<Int> res = tabsel.getColumn("ad2",rr,1,1).asArrayInt();
    }
    if (doLock) tab.unlock();
  } catch (casacore::NotThreadSafeError& x) {
    doExcept = true;
  }
  AlwaysAssertExit (doExcept);
}

// Runs a storage manager locking test use case with threads
// using a shared TableProxy 
void runSManTestLockSharedTPThreaded(void (*fn)(size_t, TableProxy &, bool), 
                                     const String& name,
                                     bool doLock,
                                     bool should_be_supported=true) { // just a string -- test case to implement fail check
  {
    Table* tab = nullptr;
    
    cout << "\t\t\tReading from table with " << \
      num_threads*nrowStep << " rows with " << num_threads << " threads...";
    if (doLock) {
      TableLock lock(TableLock::LockOption::UserLocking);
      tab = new Table(name, lock, Table::Old);
    } else {
      tab = new Table(name);
    }
    TableProxy tabp(*tab);
    std::vector<std::thread> threads;
    // async start a few threads each reading
    for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
      threads.push_back(std::thread(fn, iChunk, std::ref(tabp), doLock));
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
}

// Runs a storage manager locking test use case with threads
// can set whether the use case should be supported or not
void runSManTestLockUniqTab(void (*fn)(size_t, const String&, bool), 
                            const String& name,
                            bool doLock,
                            bool should_be_supported,
                            bool use_threads) {
  
  // async start a few threads each reading
  if (use_threads) {
    cout << "\t\t\tReading from table with " << \
            num_threads*nrowStep << " rows with " << num_threads << " threads...";
    std::vector<std::thread> threads;
    for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
      threads.push_back(std::thread(fn, iChunk, name, doLock));
    }
    // await results
    for (size_t i = 0; i < num_threads; ++i) {
      threads.back().join();
      threads.pop_back();
    }
  } else {
    cout << "\t\t\tReading from table with " << \
            num_threads*nrowStep << " rows with main thread";
    for (size_t iChunk = 0; iChunk < num_threads; ++iChunk) {
      fn(iChunk, name, doLock);
    }
  }
  cout << (should_be_supported ? "\t<OK>" : "\t<NotImplemented -- OK>") << endl;
  if (!should_be_supported) {
    // Reset the stale cache due to exception for the next test to start afresh
    for (size_t i = 0; i < PlainTable::tableCache().getTableNames().size(); ++i) {
      PlainTable::tableCache().remove(PlainTable::tableCache().getTableNames()[i]);
    }
    AlwaysAssertExit(PlainTable::tableCache().getTableNames().size() == 0) 
  }
}

// Do a series of *ReadOnly* tests with or without UserLocking
// Currently tests:
//
void runSManTestLock(bool doLock, const String & tbname) {
  // Usage pattern 1 - SingleThreaded
  {
    cout << "\t\tRunning single threaded test with a unique table"<<endl;
    runSManTestLockUniqTab(readTableChunk, tbname, doLock, true, false);
  }
  // Usage pattern 2 - SingleThreaded
  if (doLock) // must have a lock to use TaQL
  {
    cout << "\t\tRunning single threaded test with a unique table, exec TaQL op"<<endl;
    runSManTestLockUniqTab(readTableChunkDoSelect, tbname, doLock, true, false);
  }
  // Usage pattern 3 - MultiThreaded - table per thread
  // (The performant case)
  {
    cout << "\t\tRunning multithreaded test with unique table per thread"<<endl;
    runSManTestLockUniqTab(readTableChunk, tbname, doLock, true, true);
  }
  // Usage pattern 4 - MultiThreaded - table per thread, exec some TaQL
  // (The performant case)
  if (doLock) // must have a lock to use TaQL
  {
    cout << "\t\tRunning multithreaded test with unique table per thread, exec TaQL op per table"<<endl;
    runSManTestLockUniqTab(readTableChunkDoSelect, tbname, doLock, true, true);
  }
  // Usage pattern 5 - MultiThreaded - one table proxy across many threads
  {
    cout << "\t\tRunning single TableProxy object with multiple threads test "<< endl;
    runSManTestLockSharedTPThreaded(readTableChunkSharedTableProxy,
                                    tbname, doLock, true);
  }
  // Usage pattern 6 reading with a pool of processes
  {
    cout << "\t\tRunning multiple processes test (MPI-style) "<< endl;
    cout << "\t\t\tReading from table with " << \
      num_threads*nrowStep << " rows...";
    processpool(num_threads, readTableChunk, "tVeryBigTable_tmp.tbl", false);
    cout << "\t\t<OK>" << endl;
  }
  // Usage pattern 7 - MultiThreaded - one table proxy across many threads, assign ref
  {
    cout << "\t\tRunning single TableProxy object with multiple threads test, " \
              "thread copy construct shallow copy TabProxy "<< endl;
    runSManTestLockSharedTPThreaded(readTableChunkSharedTableProxyCreateTPRefAssign,
                                    tbname, doLock, false);
  }
  // Usage pattern 8 - MultiThreaded - one table proxy across many threads, shallow copy
  {
    cout << "\t\tRunning single TableProxy object with multiple threads test, " \
              "thread construct shallow copy TabProxy via assignment "<< endl;
    runSManTestLockSharedTPThreaded(readTableChunkSharedTableProxyCreateTPRefs,
                                    tbname, doLock, false);
  }
  // Usage pattern 9 - MultiThreaded - exec taql select
  {
    cout << "\t\tRunning single TableProxy object with multiple threads test, " \
              "exec TaQL "<< endl;
    runSManTestLockSharedTPThreaded(readTableChunkSharedTableProxyDoSelect,
                                    tbname, doLock, false);
  }
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

void runSManTestRW(const String& tbname) {
  // Usage pattern 1 - SingleThreaded
  {
    cout << "\t\tRunning single threaded test" << endl;
    cout << "\t\t\tReading from and writing to table with " << \
      num_threads*nrowStep << " rows...";
    for (size_t iChunk=0; iChunk<num_threads; ++iChunk)
      readWriteTableChunk(iChunk, tbname);
    cout << "\t<OK>" << endl;
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
  //       threads.push_back(std::thread(readWriteTableChunk, iChunk, tbname));
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
    cout << "\t\tRunning multiple processes test (MPI-style)" << endl; 
    cout << "\t\t\tReading from and writing to table with " << \
      num_threads*nrowStep << " rows...";
    processpool(num_threads, readWriteTableChunk, tbname);
    cout << "\t<OK>" << endl;
  }
}

template<class StMan, typename... Args>
int runSManTest(const String& smName, Args... smArgs) {
  String tbname = "tVeryBigTable_tmp.tbl";
  cout << "Testing " << smName << " storage manager" << endl;
  try {
    createTable<StMan>(tbname,
                       smName,
                       smArgs...); //cachesize
    cout << "\t<OK>" << endl;
    cout<<"\tRunning ReadOnly tests (UserLock)"<<endl;
    runSManTestLock(true, tbname);
    cout<<"\tRunning ReadOnly tests (NoLock)..."<<endl;
    runSManTestLock(false, tbname);
    cout<<"\tRunning ReadWrite tests..."<<endl;
    runSManTestRW(tbname);
    cout << "\t<" << smName << " -- all OK>" << endl;
  } catch (AipsError& x) {
      cout << "Caught an exception: " << x.getMesg() << endl;
      return 1;
  }
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
