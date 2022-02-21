//# tMultiFile.cc: Test program for class MultiFile
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
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MultiHDF5.h>
#include <casacore/casa/IO/MFFileIO.h>
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

void makeFile (Int64 blockSize, Bool useODirect, Bool useCRC)
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::New, blockSize,
                  useODirect, useCRC);
  AlwaysAssertExit (mfile.isWritable());
  mfile.show (cout);
}

void readFile (const String& name = "tMultiFile_tmp.dat")
{
  MultiFile mfile(name, ByteIO::Old);
  AlwaysAssertExit (! mfile.isWritable());
  mfile.show (cout);
  for (uInt i=0; i<mfile.info().size(); ++i) {
    String nm = "file" + String::toString(i);
    cout << nm << ' ' << mfile.fileId(nm, False) << endl;
  }
}

void addFiles()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Update);
  AlwaysAssertExit (mfile.isWritable());
  Int fid0 = mfile.createFile ("file0");
  Int fid1 = mfile.createFile ("file1");
  Int fid2 = mfile.createFile ("file2");
  AlwaysAssertExit (mfile.nfile()==3 && fid0==0 && fid1==1 && fid2==2);
  mfile.show (cout);
  mfile.closeFile (fid0);
  mfile.closeFile (fid1);
  mfile.closeFile (fid2);
}

void writeFiles1()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Update);
  Int id0 = mfile.openFile ("file0");
  Int id1 = mfile.openFile ("file1");
  Int id2 = mfile.openFile ("file2");
  Vector<Int64> buf(128);
  indgen(buf);
  mfile.write (id0, buf.data(), 1024, 0);
  buf += Int64(128);
  mfile.write (id2, buf.data(), 1024, 0);
  buf += Int64(128);
  mfile.write (id0, buf.data(), 1024, 1024);
  buf += Int64(128);
  mfile.write (id0, buf.data(), 1024, 2048);
  buf += Int64(128);
  mfile.write (id1, buf.data(), 1024, 1024);  // also creates block at offset 0
  buf += Int64(128);
  mfile.write (id2, buf.data(), 1024, 1024);
  cout << mfile.info() << endl;
  mfile.closeFile (id0);
  mfile.closeFile (id1);
  mfile.closeFile (id2);
}

void checkFiles1 (Bool do1=True)
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Old);
  Int id0 = mfile.openFile ("file0");
  Int id2 = mfile.openFile ("file2");
  Vector<Int64> buf1(128), buf(128),buff(3*128);
  indgen(buf1);
  mfile.read (id0, buf.data(), 1024, 0);
  if (do1) {
    AlwaysAssertExit (allEQ(buf, buf1));
  } else {
    AlwaysAssertExit(buf[0]==buf1[0] && allEQ(buf(Slice(1,127)), buf1(Slice(0,127))));
  }
  buf1 += Int64(128);
  mfile.read (id2, buf.data(), 1024, 0);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (id0, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (id0, buf.data(), 1024, 2048);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  if (do1) {
    Int id1 = mfile.openFile ("file1");
    mfile.read (id1, buf.data(), 1024, 1024);
    AlwaysAssertExit (allEQ(buf, buf1));
    mfile.closeFile (id1);
  }
  buf1 += Int64(128);
  mfile.read (id2, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
  // Check a single read.
  if (do1) {
    indgen(buf1);
    mfile.read (id0, buff.data(), 3072, 0);
    AlwaysAssertExit (allEQ(buff(Slice(0,128)), buf1));
    AlwaysAssertExit (allEQ(buff(Slice(128,128)), buf1+Int64(256)));
    AlwaysAssertExit (allEQ(buff(Slice(256,128)), buf1+Int64(384)));
    // Read partial blocks.
    mfile.read (id0, buff.data(), 3072-24, 8);
    AlwaysAssertExit (allEQ(buff(Slice(0,127)), buf1(Slice(1,127))));
    AlwaysAssertExit (allEQ(buff(Slice(127,128)), buf1+Int64(256)));
    AlwaysAssertExit (allEQ(buff(Slice(255,126)), buf1(Slice(0,126))+Int64(384)));
    // Check that remainder of receiving buffer is not overwritten by read
    AlwaysAssertExit (buff[380]==509 && buff[381]==509);
  }
  mfile.closeFile (id0);
  mfile.closeFile (id2);
}

void deleteFile()
{
  cout <<"test deleteFile"<<endl;
  {
    MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Update);
    Int id1 = mfile.openFile ("file1");
    mfile.deleteFile (id1);
  }
  readFile();
}

void writeFiles2()
{
  // Overwrite a few values.
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Update);
  Int id0 = mfile.openFile ("file0");
  Int id2 = mfile.openFile ("file2");
  Vector<Int64> buf(128), buf1(128);
  indgen(buf);
  mfile.write (id0, buf.data(), 1016, 8);
  mfile.read (id0, buf1.data(), 1024, 0);
  AlwaysAssertExit(buf[0]==buf1[0] && allEQ(buf1(Slice(1,127)), buf(Slice(0,127))));
  mfile.write (id2, buf.data(), 16, 2048);
  cout << mfile.info() << endl;
  mfile.closeFile (id0);
  mfile.closeFile (id2);
}

void checkFiles2 (const String& name = "tMultiFile_tmp.dat")
{
  checkFiles1(False);
  MultiFile mfile(name, ByteIO::Old);
  Int id2 = mfile.openFile ("file2");
  Vector<Int64> buf1(2), buf(2);
  indgen(buf1);
  mfile.read (id2, buf.data(), 16, 2048);
  AlwaysAssertExit (allEQ(buf, buf1));
  mfile.closeFile (id2);
}

void doTest (Int64 blockSize, Bool useODirect=False, Bool useCRC=False)
{
  cout << "MultiFile test with blockSize=" << blockSize << ", useCRC="
       << useCRC << ", useODirect=" << useODirect << endl;
  makeFile (blockSize, useODirect, useCRC);
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

void timeExact()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::New, 32768);
  Int id = mfile.createFile ("file0");
  Vector<Int64> buf(32768/8, 0);
  for (Int j=0; j<2; ++j) {
    Timer timer;
    for (uInt i=0; i<1000; ++i) {
      mfile.write (id, buf.data(), 32768, i*32768);
    }
    mfile.fsync();
    timer.show ("exact ");
  }
  mfile.closeFile (id);
}

void timeDouble()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::New, 16384);
  Int id = mfile.createFile ("file0");
  Vector<Int64> buf(32768/8, 0);
  for (Int j=0; j<2; ++j) {
    Timer timer;
    for (uInt i=0; i<1000; ++i) {
      mfile.write (id, buf.data(), 32768, i*32768);
    }
    mfile.fsync();
    timer.show ("double");
  }
  mfile.closeFile (id);
}

void timePartly()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::New, 32768);
  Int id = mfile.createFile ("file0");
  Vector<Int64> buf(16384/8, 0);
  for (Int j=0; j<2; ++j) {
    Timer timer;
    for (uInt i=0; i<2000; ++i) {
      mfile.write (id, buf.data(), 16384, i*16384);
    }
    mfile.fsync();
    timer.show ("partly");
  }
  mfile.closeFile (id);
}

void timePack (Int64 incr)
{
  vector<Int64> bl(1000000);
  for (size_t i=0; i<bl.size(); ++i) {
    bl[i] = i*incr;
  }
  Timer timer;
  vector<Int64> pck = MultiFile::packIndex (bl);
  timer.show("pack  ");
  Timer timer2;
  vector<Int64> orig = MultiFile::packIndex (pck);
  timer2.show ("unpack");
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

void testV1()
{
  cout << "Test a version 1 MultiFile ..." << endl;
  readFile("tMultiFile.in");
  checkFiles2("tMultiFile.in");
  cout << endl;
}

void testNested (Int64 blockSizeParent, Int64 blockSizeChild)
{
  cout << "Test nested with block sizes " << blockSizeParent
       << " and " << blockSizeChild << endl;
  {
    MultiFile* parentmf = new MultiFile ("tMultiFile_tmp.nest",
                                         ByteIO::New, blockSizeParent);
    std::shared_ptr<MultiFileBase> parent (parentmf);
    MultiFile child("tnested", parent, ByteIO::New, blockSizeChild);
    Int fidParent = parent->createFile ("file0");
    Int fidChild  = child.createFile ("file0");
    AlwaysAssertExit (fidParent == 1  &&  fidChild == 0);
    parentmf->show (cout);
    child.show (cout);
    child.closeFile (fidChild);
    parent->closeFile (fidParent);
  }
  {
    MultiFile* parentmf = new MultiFile ("tMultiFile_tmp.nest",
                                         ByteIO::Old);
    std::shared_ptr<MultiFileBase> parent (parentmf);
    MultiFile child("tnested", parent, ByteIO::Old);
    Int fidParent = parent->openFile ("file0");
    Int fidChild  = child.openFile ("file0");
    AlwaysAssertExit (fidParent == 1  &&  fidChild == 0);
    parentmf->show (cout);
    child.show (cout);
    child.closeFile (fidChild);
    parent->closeFile (fidParent);
  }
  cout << endl;
}

void testLarge (Bool oDirect, Bool useCRC, int testMode)
{
  cout << "Test a large MultiFile ..." << endl;
  {
    // Use 4 KB blocks, but optionally use test mode (do not write actual data).
    MultiFile mfile("tMultiFile_tmp.large", ByteIO::New, 4096,
                    oDirect, useCRC, testMode);
    Int fid0 = mfile.createFile ("file0");
    Int fid1 = mfile.createFile ("file1");
    Vector<Int64> buf(4096/sizeof(Int64));
    indgen(buf);
    Int64 offs0 = 0;
    Int64 offs1 = 0;
    for (size_t i=0; i<1024; ++i) {
      for (size_t j=0; j<128; ++j) {
        mfile.write (fid0, buf.data(), 4096, offs0);
        offs0 += 4096;
      }
      for (size_t j=0; j<32; ++j) {
        mfile.write (fid1, buf.data(), 4096, offs1);
        offs1 += 4096;
      }
      // Do an occasional flush.
      if (i%200 == 5) {
        if (! mfile.flush()) {
          break;
        }
      }
    }
    mfile.closeFile (fid0);
    mfile.closeFile (fid1);

  }
  cout << "read the large file" << endl;
  readFile ("tMultiFile_tmp.large");
  cout << endl;
}

void testTruncate()
{
  cout << "Test truncation of MultiFile (also nested) ..." << endl;
  MultiFile* mfilePtr = new MultiFile("tMultiFile_tmp.dat", ByteIO::New, 256);
  std::shared_ptr<MultiFileBase> mfile(mfilePtr);
  AlwaysAssertExit (mfile->isWritable());
  // Add a file.
  MFFileIO file1 (mfile, "file1", ByteIO::New);
  // Write some blocks;
  Vector<Int> vec(64);
  indgen (vec);
  for (uInt i=0; i<10; ++i) {
    file1.write (256, vec.data());
    vec += 64;
  }
  mfilePtr->show (cout);
  AlwaysAssertExit (file1.length() == 2560);
  file1.truncate (1000);
  mfilePtr->show (cout);
  AlwaysAssertExit (file1.length() == 1000);
  cout<< mfile->info()[0].blockNrs.size() <<endl;
  AlwaysAssertExit (mfile->info()[0].blockNrs.size() == 4);
  AlwaysAssertExit (mfile->freeBlocks().size() == 0);
}

void doPackTest (const vector<Int64>& bl, const vector<Int64>& exp)
{
  vector<Int64> pck = MultiFile::packIndex (bl);
  AlwaysAssertExit (pck.size() <= bl.size());
  AlwaysAssertExit (pck.size() == exp.size());
  for (size_t i=0; i<pck.size(); ++i) {
    AlwaysAssertExit (pck[i] == exp[i]);
  }
  vector<Int64> orig = MultiFile::unpackIndex (pck);
  AlwaysAssertExit (orig.size() == bl.size());
  for (size_t i=0; i<bl.size(); ++i) {
    AlwaysAssertExit (orig[i] == bl[i]);
  }
}


int main()
{
  try {
    // First test if packing/unpacking works correctly.
    doPackTest (vector<Int64>(), vector<Int64>());
    doPackTest (vector<Int64>({1,2,3,4,5}), vector<Int64>({1,-4}));
    doPackTest (vector<Int64>({1,3,5,7,9}), vector<Int64>({1,3,5,7,9}));
    doPackTest (vector<Int64>({1,3,4,7,8}), vector<Int64>({1,3,-1,7,-1}));
    // Do some MultiFile tests.
    doTest (1024);              // no extra header file
    doTest (128);               // requires extra header file
    doTest (4096, True, True);  // with O_DIRECT (if possible) and CRC
    // Test if a version 1 file can still be read.
    testV1();
    // Test if nested MultiFiles work fine.
    testNested (512, 0);
    // Test a large MultiFile without actually writing data.
    testLarge (False, False, -1);
    testLarge (False, True, -1);
    testLarge (True, False, -1);
    testLarge (True, True, -1);
    // Test a large MultiFile forcing an exception when itsNrBlock > 10000
    testLarge (False, False, 100000);
    // Test file truncation.
    testTruncate();
    // Do some timings.
    // Exclude timings from checked output.
    cout << ">>>" << endl;
    timePack(1);
    timePack(2);
    timeMove1();
    timeMove2(memcpy);
    timeMove2(mymemcpy);
    timeMove3();
    timeExact();
    timeDouble();
    timePartly();
    cout << "<<<" << endl;
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "tMultiFile ended OK" << endl;
  return 0;
}
