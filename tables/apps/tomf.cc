//# tomf.cc: This program copies files to a multifile
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

#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/Containers/Block.h>
#include <vector>
#include <stdexcept>
#include <iostream>

using namespace casacore;
using namespace std;


int main (int argc, char* argv[])
{
  try {
    vector<String> fname;
    String outName;
    Int64 blockSize = 1048576;
    Bool useHDF5 = False;
    for (int argnr=1; argnr<argc; ++argnr) {
      if (String(argv[argnr]) == "-b") {
        argnr++;
        if (argnr < argc) {
          blockSize = atoi (argv[argnr]);
        }
      } else if (String(argv[argnr]) == "-h") {
        useHDF5 = True;
      } else if (argnr == argc-1) {
        outName = argv[argnr];
      } else {
        fname.push_back (argv[argnr]);
      }
    }
    if (fname.empty()  ||  outName.empty()) {
      cerr << "Run as:    tomf [-h] [-b blocksize] filename1 ... outname" << endl;
      cerr << "      -h   create MultiFile as HDF5 instead of regular file" << endl;
      cerr << "      -b   blocksize in bytes; default 1048576" << endl;
      return 0;
    }
    // Open each file and copy to the MultiFile.
    CountedPtr<MultiFileBase> mfile;
    if (useHDF5) {
      mfile = new MultiHDF5 (outName, ByteIO::New, blockSize);
    } else {
      mfile = new MultiFile (outName, ByteIO::New, blockSize);
    }
    Block<char> buffer (blockSize);
    for (vector<String>::const_iterator iter=fname.begin();
         iter!=fname.end(); ++iter) {
      if (iter->empty()) {
        cerr << "*** Empty file name given" << endl;
      } else {
        int fd = RegularFileIO::openCreate (*iter, ByteIO::Old);
        FiledesIO file(fd, *iter);
        Int64 todo = file.length();
        cout << "  copying " << todo << " bytes of " << *iter
             << " ..." << endl;
        MFFileIO outfile (*mfile, *iter, ByteIO::New);
        while (todo > 0) {
          Int64 sz = file.read (std::min(todo, blockSize), buffer.storage());
          outfile.write (sz, buffer.storage());
          todo -= sz;
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
