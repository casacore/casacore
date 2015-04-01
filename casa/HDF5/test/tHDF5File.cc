//# tHDF5File.cc: Test program for class HDF5File
//# Copyright (C) 2008
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
//#
//# $Id$

#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

using namespace casacore;

int main()
{
  // Exit with untested if no HDF5 support.
  if (! HDF5Object::hasHDF5Support()) {
    return 3;
  }
  try {
    {
      // Create the file.
      HDF5File file("tHDF5File_tmpx", ByteIO::New);
      AlwaysAssertExit (Path(file.getName()).baseName() == "tHDF5File_tmpx");
      AlwaysAssertExit (! file.isClosed());
      AlwaysAssertExit (file.isWritable());
      AlwaysAssertExit (! file.isOpenedForDelete());
      // Close it temporarily.
      file.close();
      AlwaysAssertExit (file.isClosed());
      AlwaysAssertExit (file.isWritable());
      AlwaysAssertExit (! file.isOpenedForDelete());
      // Flush is a no-op if the file is closed.
      file.flush();
      // Reopen it.
      file.reopen();
      AlwaysAssertExit (! file.isClosed());
      AlwaysAssertExit (file.isWritable());
      AlwaysAssertExit (! file.isOpenedForDelete());
      file.flush();
    }
    {
      // Open readonly.
      HDF5File file("tHDF5File_tmpx", ByteIO::Old);
      AlwaysAssertExit (! file.isClosed());
      AlwaysAssertExit (! file.isWritable());
      AlwaysAssertExit (! file.isOpenedForDelete());
      // Reopen for read/write.
      file.reopenRW();
      AlwaysAssertExit (! file.isClosed());
      AlwaysAssertExit (file.isWritable());
      AlwaysAssertExit (! file.isOpenedForDelete());
    }
    {
      // Open for delete.
      HDF5File file("tHDF5File_tmpx", ByteIO::Delete);
      AlwaysAssertExit (! file.isClosed());
      AlwaysAssertExit (file.isWritable());
      AlwaysAssertExit (file.isOpenedForDelete());
    }
    {
      // File name gets absolute, so ignore for output comparison.
      cout << ">>>" << endl;
      Bool succ = True;
      try {
	HDF5File file("tHDF5File_tmpx", ByteIO::Old);
      } catch (AipsError& x) {
	succ = False;
	cout << x.what() << endl;
      }
      AlwaysAssertExit (!succ);
      cout << "<<<" << endl;
    }
    {
      // Create the file for scratch.
      HDF5File file("tHDF5File_tmp", ByteIO::Scratch);
      AlwaysAssertExit (! file.isClosed());
      AlwaysAssertExit (file.isWritable());
      AlwaysAssertExit (file.isOpenedForDelete());
    }
    {
      // Create the file. Succeeds, because scratch file is deleted.
      HDF5File file("tHDF5File_tmp", ByteIO::NewNoReplace);
    }
    {
      // Create the file. Fails, because already exists.
      cout << ">>>" << endl;
      Bool succ = True;
      try {
      HDF5File file("tHDF5File_tmp", ByteIO::NewNoReplace);
      } catch (AipsError& x) {
	succ = False;
	cout << x.what() << endl;
      }
      AlwaysAssertExit (!succ);
      cout << "<<<" << endl;
    }

  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
