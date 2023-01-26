//# tFileUnbufferedIO.cc: Test program for class FileUnbufferedIO
//# Copyright (C) 2019
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

#include <casacore/casa/IO/FileUnbufferedIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <unistd.h>
#include <fcntl.h>
#include <vector>


#include <casacore/casa/namespace.h>
void check (int bufSize, const char* buf)
{
  FileUnbufferedIO fio("tFileUnbufferedIO_tmp.dat", ByteIO::Old);
  fio.seek (0);
  std::vector<char> buf1(bufSize);
  for (int j=0; j<2000/bufSize; j++) {
    AlwaysAssertExit (fio.read (bufSize, buf1.data(), true) == bufSize);
    for (int i=0; i<bufSize; i++) {
      AlwaysAssertExit (buf1[i] == *buf++);
    }
  }
  int rem = 2000%bufSize;
  AlwaysAssertExit (fio.read(bufSize, buf1.data(), false) == rem);
  for (int i=0; i<rem; i++) {
    AlwaysAssertExit (buf1[i] == *buf++);
  }
}


void doIt()
{
  char buf[2000];
  for (int i=0; i<2000; i++) {
    buf[i] = i;
  }
  {
    FileUnbufferedIO file ("tFileUnbufferedIO_tmp.dat", ByteIO::New);
    file.write (2000, buf);
  }
  FileUnbufferedIO file ("tFileUnbufferedIO_tmp.dat", ByteIO::Old);
  check (2000, buf);
  check (100, buf);
  check (1, buf);
  check (3, buf);
  check (101, buf);
  check (2001, buf);
  check (10000, buf);
}

void testTruncate()
{
  FileUnbufferedIO file ("tFileUnbufferedIO_tmp.dat", ByteIO::Update);
  AlwaysAssertExit (file.length() == 2000);
  file.truncate (2000);
  AlwaysAssertExit (file.length() == 2000);
  file.truncate (1546);
  AlwaysAssertExit (file.length() == 1546);
  file.truncate (2000);
  AlwaysAssertExit (file.length() == 2000);
}

int main()
{
  try {
    doIt();
    testTruncate();
  } catch (AipsError& x) {
    cout << "Caught exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "tFileUnbufferedIO ended OK" << endl;
  return 0;
}
