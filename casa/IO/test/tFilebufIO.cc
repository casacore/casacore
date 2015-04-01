//# tFilebufIO.cc: Test program for class FilebufIO
//# Copyright (C) 2002
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

#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <unistd.h>
#include <fcntl.h>


#include <casacore/casa/namespace.h>
void check (int fd, int bufSize, const char* buf)
{
  FilebufIO fio(fd, bufSize);
  fio.seek (0);
  char* buf1 = new char[bufSize];
  
  for (int j=0; j<2000/bufSize; j++) {
    AlwaysAssertExit (fio.read (bufSize, buf1, True) == bufSize);
    for (int i=0; i<bufSize; i++) {
      AlwaysAssertExit (buf1[i] == *buf++);
    }
  }
  int rem = 2000%bufSize;
  AlwaysAssertExit (fio.read(bufSize, buf1, False) == rem);
  for (int i=0; i<rem; i++) {
    AlwaysAssertExit (buf1[i] == *buf++);
  }

  delete [] buf1;
}


void doIt()
{
  char buf[2000];
  for (int i=0; i<2000; i++) {
    buf[i] = i;
  }
  {
    RegularFileIO file (RegularFile("tFilebufIO_tmp.dat"),
			ByteIO::New);
    file.write (2000, buf);
  }
  int fd = open ("tFilebufIO_tmp.dat", O_RDONLY);
  check (fd, 2000, buf);
  check (fd, 100, buf);
  check (fd, 1, buf);
  check (fd, 3, buf);
  check (fd, 101, buf);
  check (fd, 2001, buf);
  check (fd, 10000, buf);
  close(fd);
}


int main()
{
  try {
    doIt();
  } catch (AipsError& x) {
    cout << "Caught exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
