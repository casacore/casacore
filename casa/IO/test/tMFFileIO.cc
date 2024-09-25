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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <iostream>
#include <stdexcept>

using namespace casacore;
using namespace std;

void showMultiFile (MultiFileBase& mfile)
{
  cout << mfile.fileName() << ' ' << mfile.blockSize() << ' '
       << mfile.nfile() << ' ' << mfile.nblock() << ' '
       << mfile.freeBlocks() << endl;
}

void showMFFile (MFFileIO& mfile)
{
  cout << mfile.fileName() << ' ' << mfile.length() << ' '
       << mfile.seek(0, ByteIO::Current) << endl;
  
}

void makeFile (const std::shared_ptr<MultiFileBase>& mfile)
{
  MFFileIO mff(mfile, "mff1", ByteIO::New);
  showMultiFile(*mfile);
  showMFFile (mff);
}

void writeFiles1 (const std::shared_ptr<MultiFileBase>& mfile)
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
  cout << mfile->info() << endl;
}

void checkFiles1 (const std::shared_ptr<MultiFileBase>& mfile)
{
  MFFileIO mff(mfile, "mff1", ByteIO::Update);
  Vector<Int64> bufcheck(250);
  Vector<Int64> buf(250);
  indgen(bufcheck);
  mff.read (2000, buf.data());
  AlwaysAssertExit (allEQ(buf, bufcheck));
  showMFFile (mff);
}

void testWriteNested (const std::shared_ptr<MultiFileBase>& parent)
{
  cout << "testWriteNested ..." << endl;
  std::shared_ptr<MultiFileBase> mfile1
    (parent->makeNested (parent, "mfile1", ByteIO::New, 500));
  MFFileIO mff11(mfile1, "mff11", ByteIO::New);
  Vector<Int64> buf(300);
  indgen(buf);
  mff11.write (8*150, buf.data());
  MFFileIO mff12(mfile1, "mff12", ByteIO::New);
  buf += Int64(150);
  mff12.write (8*300, buf.data());
  std::shared_ptr<MultiFileBase> mfile2
    (parent->makeNested (parent, "mfile2", ByteIO::New, 500));
  MFFileIO mff21(mfile2, "mff21", ByteIO::New);
  mff21.write (8*250, buf.data());
  mff11.write (8*150, buf.data());
  showMultiFile (*mfile1);
  showMultiFile (*mfile2);
  showMultiFile (*parent);
  showMFFile (mff11);
  showMFFile (mff12);
  showMFFile (mff21);
}

void testReadNested (const std::shared_ptr<MultiFileBase>& parent)
{
  cout<<"testReadNested ..."<<endl;
  std::shared_ptr<MultiFileBase> mfile1
    (parent->makeNested (parent, "mfile1", ByteIO::Old, 0));
  MFFileIO mff11(mfile1, "mff11");
  Vector<Int64> bufcheck(300);
  Vector<Int64> buf(300);
  indgen(bufcheck);
  mff11.read (8*300, buf.data());
  AlwaysAssertExit (allEQ(buf, bufcheck));
  MFFileIO mff12(mfile1, "mff12");
  bufcheck += Int64(150);
  mff12.read (8*300, buf.data());
  AlwaysAssertExit (allEQ(buf, bufcheck));
  std::shared_ptr<MultiFileBase> mfile2
    (parent->makeNested (parent, "mfile2", ByteIO::Old, 0));
  MFFileIO mff21(mfile2, "mff21");
  Int64 nread = mff21.read (8*300, buf.data(), False);  // only 250 were written
  AlwaysAssertExit (nread == 8*250);
  AlwaysAssertExit (allEQ(buf, bufcheck));   // last 50 elements not overwritten
}

void testTruncate (const std::shared_ptr<MultiFileBase>& parent)
{
  cout << "testTruncate ..." << endl;
  std::shared_ptr<MultiFileBase> mfile1
    (parent->makeNested (parent, "mfile1", ByteIO::Update, 0));
  MFFileIO mff11(mfile1, "mff11", ByteIO::Update);
  mff11.truncate (8*120);
  AlwaysAssertExit (mff11.length() == 8*120);
  Vector<Int64> bufcheck(120);
  Vector<Int64> buf(300);
  indgen(bufcheck);
  Int64 nread = mff11.read (8*300, buf.data(), False);  // only 120 are left
  AlwaysAssertExit (nread == 8*120);
  AlwaysAssertExit (allEQ(buf(Slice(0,120)), bufcheck));
  mfile1->flush();
  parent->flush();
  showMultiFile (*mfile1);
  showMultiFile (*parent);
  showMFFile (mff11);
}

int main()
{
  try {
    {
      cout << "Test using MultiFile ..." << endl;
      std::shared_ptr<MultiFileBase> mfile
        (new MultiFile ("tMFFileIO_tmp.dat1", ByteIO::New, 512));
      makeFile(mfile);
      writeFiles1(mfile);
      checkFiles1(mfile);
      // Test nested with parent still open and reopen of parent.
      {
        std::shared_ptr<MultiFileBase> parent
          (new MultiFile ("tMFFileIO_tmp.dat3", ByteIO::New, 1024));
        testWriteNested(parent);
        testReadNested(parent);
      }
      std::shared_ptr<MultiFileBase> parent
        (new MultiFile ("tMFFileIO_tmp.dat3", ByteIO::Update, 1024));
      testTruncate(parent);
    }
    if (HDF5Object::hasHDF5Support()) {
      cout << "Test using MultiHDF5 ..." << endl;
      std::shared_ptr<MultiFileBase> mfile
        (new MultiHDF5 ("tMFFileIO_tmp.dat2", ByteIO::New, 512));
      makeFile(mfile);
      writeFiles1(mfile);
      checkFiles1(mfile);
      // Test nested with parent still open and reopen of parent.
      {
        std::shared_ptr<MultiFileBase> parent
          (new MultiHDF5 ("tMFFileIO_tmp.dat4", ByteIO::New, 1024));
        testWriteNested(parent);
        showMultiFile (*parent);
        testReadNested(parent);
      }
      std::shared_ptr<MultiFileBase> parent
        (new MultiHDF5 ("tMFFileIO_tmp.dat4", ByteIO::Update));
      testTruncate(parent);
    }
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
