//# lsmf.cc: This program shows meta info of a multifile
//# Copyright (C) 2014
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
//# $Id: showtable.cc 21480 2014-08-27 08:01:36Z gervandiepen $

#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <vector>
#include <stdexcept>
#include <iostream>

using namespace casacore;
using namespace std;

void show (MultiFileBase& mfile, Bool showbl, const String& mftype)
{
  cout << endl;
  cout << mftype << " = " << mfile.fileName() << endl;
  cout << "  blocksize = " << mfile.blockSize()
       << "    nfile = " << mfile.nfile()
       << "    nfreeblocks = " << mfile.freeBlocks().size() << endl;
  if (showbl) {
    cout << "  freeblocks = " << mfile.freeBlocks() << endl;
  }
  for (uInt i=0; i<mfile.nfile(); ++i) {
    const MultiFileInfo& info = mfile.info()[i];
    cout << ' ' << info.name
         << "   size=" << info.fsize
         << "   nblocks="
         << (info.fsize+mfile.blockSize()-1) / mfile.blockSize()
         << endl;
    if (showbl) {
      cout << ' ' << info.blockNrs << endl;
    }
  }
}

int main (int argc, char* argv[])
{
  try {
    vector<String> fname;
    Bool showbl = False;
    for (int argnr=1; argnr<argc; ++argnr) {
      if (String(argv[argnr]) == "-b") {
        showbl = True;
      } else {
        fname.push_back (argv[argnr]);
      }
    }
    if (fname.empty()) {
      cerr << "Run as:  lsmf [-b] filename1 ..." << endl;
      cerr << "         -b   show all block numbers (and free blocks)" << endl;
      return 0;
    }
    // Open each multifile and show internal files and sizes.
    for (vector<String>::const_iterator iter=fname.begin();
         iter!=fname.end(); ++iter) {
      if (iter->empty()) {
        cerr << "*** Empty file name given" << endl;
      } else {
        if (HDF5File::isHDF5(*iter)) {
          MultiHDF5 mfile (*iter, ByteIO::Old);
          show (mfile, showbl, "MultiHDF5");
        } else {
          MultiFile mfile (*iter, ByteIO::Old);
          show (mfile, showbl, "MultiFile");
        }
      }
    }
    cout << endl;
  } catch (const std::exception& x) {
    cerr << x.what() << endl;
    return 1;
  }
  return 0;
}
