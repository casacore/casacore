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

//# Includes
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/OS/Timer.h>
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

void makeFile (int64_t blockSize)
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
  for (uint32_t i=0; i<mfile.info().size(); ++i) {
    String nm = "file" + String::toString(i);
    cout << nm << ' ' << mfile.fileId(nm, false) << endl;
  }
}

void addFiles()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
  AlwaysAssertExit (mfile.isWritable());
  int32_t fid0 = mfile.createFile ("file0");
  int32_t fid1 = mfile.createFile ("file1");
  int32_t fid2 = mfile.createFile ("file2");
  AlwaysAssertExit (mfile.nfile()==3 && fid0==0 && fid1==1 && fid2==2);
  showMultiFile(mfile);
  mfile.closeFile (fid0);
  mfile.closeFile (fid1);
  mfile.closeFile (fid2);
}

void writeFiles1()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
  int32_t id0 = mfile.openFile ("file0");
  int32_t id1 = mfile.openFile ("file1");
  int32_t id2 = mfile.openFile ("file2");
  Vector<int64_t> buf(128);
  indgen(buf);
  mfile.write (id0, buf.data(), 1024, 0);
  buf += int64_t(128);
  mfile.write (id2, buf.data(), 1024, 0);
  buf += int64_t(128);
  mfile.write (id0, buf.data(), 1024, 1024);
  buf += int64_t(128);
  mfile.write (id0, buf.data(), 1024, 2048);
  buf += int64_t(128);
  mfile.write (id1, buf.data(), 1024, 1024);
  buf += int64_t(128);
  mfile.write (id2, buf.data(), 1024, 1024);
  cout << mfile.info() << endl;
  mfile.closeFile (id0);
  mfile.closeFile (id1);
  mfile.closeFile (id2);
}

void checkFiles1 (bool do1=true)
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Old);
  int32_t id0 = mfile.openFile ("file0");
  int32_t id2 = mfile.openFile ("file2");
  Vector<int64_t> buf1(128), buf(128),buff(3*128);
  indgen(buf1);
  mfile.read (id0, buf.data(), 1024, 0);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += int64_t(128);
  mfile.read (id2, buf.data(), 1024, 0);
  if (!allEQ(buf, buf1)) {
    cout << buf << endl;
  }
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += int64_t(128);
  mfile.read (id0, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += int64_t(128);
  mfile.read (id0, buf.data(), 1024, 2048);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += int64_t(128);
  if (do1) {
    int32_t id1 = mfile.openFile ("file1");
    mfile.read (id1, buf.data(), 1024, 1024);
    AlwaysAssertExit (allEQ(buf, buf1));
    mfile.closeFile (id1);
  }
  buf1 += int64_t(128);
  mfile.read (id2, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
  // Check a single read.
  indgen(buf1);
  mfile.read (id0, buff.data(), 3072, 0);
  AlwaysAssertExit (allEQ(buff(Slice(0,128)), buf1));
  AlwaysAssertExit (allEQ(buff(Slice(128,128)), buf1+int64_t(256)));
  AlwaysAssertExit (allEQ(buff(Slice(256,128)), buf1+int64_t(384)));
  mfile.read (id0, buff.data(), 3072-24, 8);
  AlwaysAssertExit (allEQ(buff(Slice(0,127)), buf1(Slice(1,127))));
  AlwaysAssertExit (allEQ(buff(Slice(127,128)), buf1+int64_t(256)));
  AlwaysAssertExit (allEQ(buff(Slice(255,126)), buf1(Slice(0,126))+int64_t(384)));
  AlwaysAssertExit (buff[380]==509 && buff[381]==509);   // check not overwritten
  mfile.closeFile (id0);
  mfile.closeFile (id2);
}

void deleteFile()
{
  cout <<"test deleteFile"<<endl;
  {
    MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
    int32_t id1 = mfile.openFile ("file1");
    mfile.deleteFile (id1);
    cout << mfile.info() << endl;
  }
  readFile();
}

void writeFiles2()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Update);
  int32_t id0 = mfile.openFile ("file0");
  int32_t id2 = mfile.openFile ("file2");
  Vector<int64_t> buf(128), buf1(128);
  indgen(buf);
  mfile.write (id0, buf.data(), 1016, 8);
  mfile.read (id0, buf1.data(), 1024, 0);
  AlwaysAssertExit(buf[0]==buf1[0] && allEQ(buf1(Slice(1,127)), buf(Slice(0,127))));
  mfile.write (id0, buf.data(), 1024, 0);
  mfile.write (id2, buf.data(), 16, 2048);
  cout << mfile.info() << endl;
  mfile.closeFile (id0);
  mfile.closeFile (id2);
}

void checkFiles2()
{
  checkFiles1(false);
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::Old);
  int32_t id2 = mfile.openFile ("file2");
  Vector<int64_t> buf1(2), buf(2);
  indgen(buf1);
  mfile.read (id2, buf.data(), 16, 2048);
  AlwaysAssertExit (allEQ(buf, buf1));
  mfile.closeFile (id2);
}

void timeExact()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::New, 32768);
  int32_t id = mfile.createFile ("file0");
  Vector<int64_t> buf(32768/8, 0);
  for (int32_t j=0; j<2; ++j) {
    Timer timer;
    for (uint32_t i=0; i<1000; ++i) {
      mfile.write (id, buf.data(), 32768, i*32768);
    }
    mfile.fsync();
    timer.show ("exact ");
  }
  mfile.closeFile (id);
}

void timeDouble()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::New, 16384);
  int32_t id = mfile.createFile ("file0");
  Vector<int64_t> buf(32768/8, 0);
  for (int32_t j=0; j<2; ++j) {
    Timer timer;
    for (uint32_t i=0; i<1000; ++i) {
      mfile.write (id, buf.data(), 32768, i*32768);
    }
    mfile.fsync();
    timer.show ("double");
  }
  mfile.closeFile (id);
}

void timePartly()
{
  MultiHDF5 mfile("tMultiHDF5_tmp.dat", ByteIO::New, 32768);
  int32_t id = mfile.createFile ("file0");
  Vector<int64_t> buf(16384/8, 0);
  for (int32_t j=0; j<2; ++j) {
    Timer timer;
    for (uint32_t i=0; i<2000; ++i) {
      mfile.write (id, buf.data(), 16384, i*16384);
    }
    mfile.fsync();
    timer.show ("partly");
  }
  mfile.closeFile (id);
}

void timeMove1()
{
  Vector<int64_t> buf1(4, 3);
  Vector<int64_t> buf2(4, 0);
  Timer timer;
  for (uint32_t i=0; i<5000000; ++i) {
    memcpy (buf2.data(), buf1.data(), 8*4);
  }
  timer.show ("move1 ");
}

typedef void* moveFunc(void*, const void*, size_t);
void* mymemcpy (void* to, const void* from, size_t n)
  { return memcpy (to, from, n); }

void timeMove2 (moveFunc func)
{
  Vector<int64_t> buf1(4, 3);
  Vector<int64_t> buf2(4, 0);
  Timer timer;
  for (uint32_t i=0; i<5000000; ++i) {
    func (buf2.data(), buf1.data(), 8*4);
  }
  timer.show ("move2 ");
}

void timeMove3()
{
  Vector<int64_t> buf1(4, 3);
  Vector<int64_t> buf2(4, 0);
  Timer timer;
  for (uint32_t i=0; i<5000000; ++i) {
    for (uint32_t j=0;j<4; ++j) {
      buf2.data()[j] = buf1.data()[j];
    }
  }
  timer.show ("move3 ");
}

void doTest (int64_t blockSize)
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

void testNested (int64_t blockSizeParent, int64_t blockSizeChild)
{
  cout << "Test nested with block sizes " << blockSizeParent
       << " and " << blockSizeChild << endl;
  {
    MultiHDF5* parentmf = new MultiHDF5 ("tMultiHDF5_tmp.nest",
                                         ByteIO::New, blockSizeParent);
    std::shared_ptr<MultiFileBase> parent (parentmf);
    MultiHDF5 child("tnested", parent, ByteIO::New, blockSizeChild);
    int32_t fidp = parent->createFile ("file0");
    int32_t fidc = child.createFile ("file0");
    AlwaysAssertExit (fidp == 1  &&  fidc == 0);
    showMultiFile (*parent);
    showMultiFile (child);
    child.closeFile (fidc);
    parent->closeFile (fidp);
  }
  {
    MultiHDF5* parentmf = new MultiHDF5 ("tMultiHDF5_tmp.nest",
                                         ByteIO::Old);
    std::shared_ptr<MultiFileBase> parent (parentmf);
    MultiHDF5 child("tnested", parent, ByteIO::Old);
    int32_t fidp = parent->openFile ("file0");
    int32_t fidc = child.openFile ("file0");
    AlwaysAssertExit (fidp == 1  &&  fidc == 0);
    showMultiFile (*parent);
    showMultiFile (child);
    child.closeFile (fidc);
    parent->closeFile (fidp);
  }
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
    testNested (512, 0);
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
