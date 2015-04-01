//# tMultiHDF5.cc: Test program for class MultiHDF5
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
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/OS/Timer.h>
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

void makeFile (Int64 blockSize)
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::New, blockSize);
  AlwaysAssertExit (mfile.isWritable());
  showMultiFile(mfile);
}

void readFile()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Old);
  AlwaysAssertExit (! mfile.isWritable());
  showMultiFile(mfile);
  for (uInt i=0; i<mfile.info().size(); ++i) {
    String nm = "file" + String::toString(i);
    cout << nm << ' ' << mfile.fileId(nm, False) << endl;
  }
}

void addFiles()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
  AlwaysAssertExit (mfile.isWritable());
  Int fid0 = mfile.addFile ("file0");
  Int fid1 = mfile.addFile ("file1");
  Int fid2 = mfile.addFile ("file2");
  AlwaysAssertExit (mfile.nfile()==3 && fid0==0 && fid1==1 && fid2==2);
  showMultiFile(mfile);
}

void writeFiles1()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
  Vector<Int64> buf(128);
  indgen(buf);
  mfile.write (0, buf.data(), 1024, 0);
  buf += Int64(128);
  mfile.write (2, buf.data(), 1024, 0);
  buf += Int64(128);
  mfile.write (0, buf.data(), 1024, 1024);
  buf += Int64(128);
  mfile.write (0, buf.data(), 1024, 2048);
  buf += Int64(128);
  mfile.write (1, buf.data(), 1024, 1024);
  buf += Int64(128);
  mfile.write (2, buf.data(), 1024, 1024);
  cout << mfile.info() << endl;
}

void checkFiles1 (Bool do1=True)
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Old);
  Vector<Int64> buf1(128), buf(128),buff(3*128);
  indgen(buf1);
  mfile.read (0, buf.data(), 1024, 0);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (2, buf.data(), 1024, 0);
  if (!allEQ(buf, buf1)) {
    cout << buf << endl;
  }
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (0, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (0, buf.data(), 1024, 2048);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  if (do1) {
    mfile.read (1, buf.data(), 1024, 1024);
    AlwaysAssertExit (allEQ(buf, buf1));
  }
  buf1 += Int64(128);
  mfile.read (2, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
  // Check a single read.
  indgen(buf1);
  mfile.read (0, buff.data(), 3072, 0);
  AlwaysAssertExit (allEQ(buff(Slice(0,128)), buf1));
  AlwaysAssertExit (allEQ(buff(Slice(128,128)), buf1+Int64(256)));
  AlwaysAssertExit (allEQ(buff(Slice(256,128)), buf1+Int64(384)));
  mfile.read (0, buff.data(), 3072-24, 8);
  AlwaysAssertExit (allEQ(buff(Slice(0,127)), buf1(Slice(1,127))));
  AlwaysAssertExit (allEQ(buff(Slice(127,128)), buf1+Int64(256)));
  AlwaysAssertExit (allEQ(buff(Slice(255,126)), buf1(Slice(0,126))+Int64(384)));
  AlwaysAssertExit (buff[380]==509 && buff[381]==509);   // check not overwritten
}

void deleteFile()
{
  cout <<"test deleteFile"<<endl;
  {
    MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
    mfile.deleteFile (1);
    cout << mfile.info() << endl;
  }
  readFile();
}

void writeFiles2()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
  Vector<Int64> buf(128), buf1(128);
  indgen(buf);
  mfile.write (0, buf.data(), 1016, 8);
  mfile.read (0, buf1.data(), 1024, 0);
  AlwaysAssertExit(buf[0]==buf1[0] && allEQ(buf1(Slice(1,127)), buf(Slice(0,127))));
  mfile.write (0, buf.data(), 1024, 0);
  mfile.write (2, buf.data(), 16, 2048);
  cout << mfile.info() << endl;
}

void checkFiles2()
{
  checkFiles1(False);
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Old);
  Vector<Int64> buf1(2), buf(2);
  indgen(buf1);
  mfile.read (2, buf.data(), 16, 2048);
  AlwaysAssertExit (allEQ(buf, buf1));
}

void timeExact()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::New, 32768);
  Int id = mfile.addFile ("file0");
  Vector<Int64> buf(32768/8, 0);
  for (Int j=0; j<2; ++j) {
    Timer timer;
    for (uInt i=0; i<1000; ++i) {
      mfile.write (id, buf.data(), 32768, i*32768);
    }
    mfile.fsync();
    timer.show ("exact ");
  }
}

void timeDouble()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::New, 16384);
  Int id = mfile.addFile ("file0");
  Vector<Int64> buf(32768/8, 0);
  for (Int j=0; j<2; ++j) {
    Timer timer;
    for (uInt i=0; i<1000; ++i) {
      mfile.write (id, buf.data(), 32768, i*32768);
    }
    mfile.fsync();
    timer.show ("double");
  }
}

void timePartly()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::New, 32768);
  Int id = mfile.addFile ("file0");
  Vector<Int64> buf(16384/8, 0);
  for (Int j=0; j<2; ++j) {
    Timer timer;
    for (uInt i=0; i<2000; ++i) {
      mfile.write (id, buf.data(), 16384, i*16384);
    }
    mfile.fsync();
    timer.show ("partly");
  }
}

void timeMove1()
{
  Vector<Int64> buf1(4, 3);
  Vector<Int64> buf2(4, 0);
  Timer timer;
  for (uInt i=0; i<5000000; ++i) {
    memcpy (buf2.data(), buf1.data(), 8*4);
  }
  timer.show ("move1 ");
}

typedef void* moveFunc(void*, const void*, size_t);
void* mymemcpy (void* to, const void* from, size_t n)
  { return memcpy (to, from, n); }

void timeMove2 (moveFunc func)
{
  Vector<Int64> buf1(4, 3);
  Vector<Int64> buf2(4, 0);
  Timer timer;
  for (uInt i=0; i<5000000; ++i) {
    func (buf2.data(), buf1.data(), 8*4);
  }
  timer.show ("move2 ");
}

void timeMove3()
{
  Vector<Int64> buf1(4, 3);
  Vector<Int64> buf2(4, 0);
  Timer timer;
  for (uInt i=0; i<5000000; ++i) {
    for (uInt j=0;j<4; ++j) {
      buf2.data()[j] = buf1.data()[j];
    }
  }
  timer.show ("move3 ");
}

void doTest (Int64 blockSize)
{
  cout << "MultiHDF5 test with blockSize=" << blockSize << endl;
  makeFile (blockSize);
  readFile();
  addFiles();
  readFile();
  writeFiles1();
  readFile();
  checkFiles1();
  deleteFile();
  writeFiles2();
  readFile();
  checkFiles2();
  cout << endl;
}

int main()
{
  if (! HDF5Object::hasHDF5Support()) {
    cout << "tMultiHDF5 not run; HDF5 is not supported in casacore build"
         << endl;
    return 3;
  }
  try {
    doTest (128);     // requires extra header file
    doTest (1024);    // no extra header file
    timeExact();
    timeDouble();
    timePartly();
    //timeMove1();
    //timeMove2(memcpy);
    //timeMove2(mymemcpy);
    //timeMove3();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
