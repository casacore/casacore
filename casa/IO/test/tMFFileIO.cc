//# tMFFileIO.cc: Test program for class MFFileIO
//# Copyright (C) 2014
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
//# $Id: RegularFileIO.h 20551 2009-03-25 00:11:33Z Malte.Marquarding $

//# Includes
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <iostream>
#include <stdexcept>

using namespace casacore;
using namespace std;

void showMultiFile (MultiFileBase& mfile)
{
  cout << mfile.fileName() << ' ' << mfile.blockSize() << ' '
       << mfile.nfile() << ' ' << mfile.size() << ' '
       << mfile.freeBlocks() << endl;
}

void showMFFile (MFFileIO& mfile)
{
  cout << mfile.fileName() << ' ' << mfile.length() << ' '
       << mfile.seek(0, ByteIO::Current) << endl;
  
}

void makeFile (MultiFileBase& mfile)
{
  MFFileIO mff(mfile, "mff1", ByteIO::New);
  showMultiFile(mfile);
  showMFFile (mff);
}

void writeFiles1 (MultiFileBase& mfile)
{
  MFFileIO mff(mfile, "mff1", ByteIO::Update);
  Vector<Int64> buf(120);
  indgen(buf);
  mff.write (960, buf.data());
  buf += Int64(120);
  mff.write (960, buf.data());
  buf += Int64(120);
  mff.write (80, buf.data());
  showMFFile (mff);
  cout << mfile.info() << endl;
}

void checkFiles1 (MultiFileBase& mfile)
{
  MFFileIO mff(mfile, "mff1", ByteIO::Update);
  Vector<Int64> buf1(250), buf(250);
  indgen(buf1);
  mff.read (2000, buf.data());
  AlwaysAssertExit (allEQ(buf, buf1));
  showMFFile (mff);
}

int main()
{
  try {
    {
      MultiFile mfile("tMFFileIO_tmp.dat1", ByteIO::New, 512);
      makeFile(mfile);
      writeFiles1(mfile);
      checkFiles1(mfile);
    }
    if (HDF5Object::hasHDF5Support()) {
      MultiHDF5 mfile("tMFFileIO_tmp.dat2", ByteIO::New, 512);
      makeFile(mfile);
      writeFiles1(mfile);
      checkFiles1(mfile);
    }
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
