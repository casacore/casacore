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
//#
//# $Id$

#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/IO/LockFile.h>
#include <casacore/casa/OS/Path.h>
#include <stdexcept>
#include <iostream>

using namespace casacore;
using namespace std;

int main (int argc, char* argv[])
{
  if (TableLock::lockingDisabled()) {
    cerr << "Note: table locking is disabled because Casacore "
         << "was built with -DAIPS_TABLES_NOLOCKING" << endl;
  }
  if (argc < 2) {
    cerr << "Use as:   showtablelock tablename" << endl;
    return 1;
  }
  try {
    String tablename(argv[1]);
    tablename = Path(tablename).absoluteName();
    if (! Table::isReadable (tablename)) {
      cerr << "Table " << tablename
           << " does not exist (or not readable)" << endl;
      return 1;
    }
    uInt pid = 0;
    Bool permLocked = False;
    uInt type = LockFile::showLock (pid, permLocked, tablename + "/table.lock");
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
  } catch (std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  }
  return 0;
}
