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
#include <casa/IO/MultiFile.h>
#include <casa/Utilities/Assert.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/BasicSL/STLIO.h>
#include <iostream>
#include <stdexcept>

using namespace casa;
using namespace std;

void showMultiFile (MultiFile& mfile)
{
  cout << mfile.fileName() << ' ' << mfile.blockSize() << ' '
       << mfile.nfile() << ' ' << mfile.size() << endl;
}

void makeFile()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::New, 1024);
  AlwaysAssertExit (mfile.isWritable());
  showMultiFile(mfile);
}

void readFile()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Old);
  AlwaysAssertExit (! mfile.isWritable());
  showMultiFile(mfile);
  for (Int i=0; i<mfile.nfile(); ++i) {
    String nm = "file" + String::toString(i);
    cout << nm << ' ' << mfile.fileId(nm) << endl;
  }
}

void addFiles()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Update);
  AlwaysAssertExit (mfile.isWritable());
  Int fid0 = mfile.add ("file0");
  Int fid1 = mfile.add ("file1");
  Int fid2 = mfile.add ("file2");
  AlwaysAssertExit (mfile.nfile()==3 && fid0==0 && fid1==1 && fid2==2);
  showMultiFile(mfile);
}

void writeFiles1()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Update);
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

void checkFiles1()
{
  MultiFile mfile("tMultiFile_tmp.dat", ByteIO::Old);
  Vector<Int64> buf1(128), buf(128),buff(3*128);
  indgen(buf1);
  mfile.read (0, buf.data(), 1024, 0);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (2, buf.data(), 1024, 0);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (0, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (0, buf.data(), 1024, 2048);
  AlwaysAssertExit (allEQ(buf, buf1));
  buf1 += Int64(128);
  mfile.read (1, buf.data(), 1024, 1024);
  AlwaysAssertExit (allEQ(buf, buf1));
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

int main()
{
  try {
    makeFile();
    readFile();
    addFiles();
    readFile();
    writeFiles1();
    readFile();
    checkFiles1();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
