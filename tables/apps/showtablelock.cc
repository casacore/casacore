//# showtablelock.cc: This program shows if a table is used and/or locked
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

#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableSyncData.h>
#include <casacore/casa/IO/LockFile.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/OS/Path.h>
#include <stdexcept>
#include <iostream>

using namespace casacore;
using namespace std;

void showVerbose (const String& lockFileName)
{
  LockFile lfile(lockFileName);
  TableSyncData data;
  // Read the lock info from the file into a buffer.
  // Thereafter interpret the data read.
  lfile.getInfo (data.memoryIO());
  rownr_t nrrow;
  uInt nrcolumn;
  Bool tableChanged;
  Block<Bool> dataManChanged;
  data.read (nrrow, nrcolumn, tableChanged, dataManChanged);
  // Show the data.
  cout << "Lock file info   (of " << data.memoryIO().length() << " bytes)" << endl;
  cout << "  nrows:           " << nrrow << endl;
  cout << "  ncolumns:        " << nrcolumn << endl;
  cout << "  table changed:   " << tableChanged << endl;
  cout << "  dataman changed: " << dataManChanged << endl;
  cout << "  modify counter:  " << data.getModifyCounter() << endl;
  int nrid = lfile.reqIds()[0];
  cout << "  " << nrid << " outstanding lock requests from other processes" << endl;
}

int main (int argc, char* argv[])
{
  if (TableLock::lockingDisabled()) {
    cerr << "Note: table locking is disabled because Casacore "
         << "was built with -DAIPS_TABLES_NOLOCKING" << endl;
  }
  int starg = 1;
  Bool verbose = False;
  if (argc > starg  &&  String(argv[starg]) == "-v") {
    verbose = True;
    starg += 1;
  }
  if (argc <= starg) {
    cerr << "Use as:   showtablelock [-v] tablename" << endl;
    cerr << "      -v    verbose" << endl;
    return 1;
  }
  try {
    String tablename(argv[starg]);
    tablename = Path(tablename).absoluteName();
    if (! Table::isReadable (tablename)) {
      cerr << "Table " << tablename
           << " does not exist (or not readable)" << endl;
      return 1;
    }
    String lockFileName(tablename + "/table.lock");
    uInt pid = 0;
    Bool permLocked = False;
    uInt type = LockFile::showLock (pid, permLocked, lockFileName);
    String perm;
    if (permLocked) {
      perm = "permanently ";
    }
    if (type == 1) {
      cout << "Table " << tablename
           << " is opened (but not locked) in process " << pid << endl;
    } else if (type == 2) {
      cout << "Table " << tablename
           << " is " << perm << "read-locked in process " << pid << endl;
    } else if (type == 3) {
      cout << "Table " << tablename
           << " is " << perm << "write-locked in process " << pid << endl;
    } else {
      cout << "Table " <<  tablename
           << " is neither opened nor locked in another process" << endl;
    }
    if (verbose) {
      showVerbose (lockFileName);
    }
  } catch (std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  }
  return 0;
}
