//# tLargeFileIO.cc: Test program for performance of file IO
//# Copyright (C) 2001,2002,2003
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

#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <unistd.h>


#include <casacore/casa/namespace.h>
int main (int argc, const char* argv[])
{
  try {
    int mode = 0;
    if (argc > 1) {
      if (String(argv[1]) == String("w")) {
	mode = 1;
      } else if (String(argv[1]) == String("r")) {
	mode = -1;
      }
    }
    int nr = 100;
    if (argc > 2) {
      istringstream istr(argv[2]);
      istr >> nr;
    }
    int leng = 1024;
    if (argc > 3) {
      istringstream istr(argv[3]);
      istr >> leng;
    }
    int incr = 0;
    Bool setpos = False;
    if (argc > 4) {
      istringstream istr(argv[4]);
      istr >> incr;
      setpos = True;
    }
    leng /= sizeof(int);
    int tleng = leng * sizeof(int);
    cout << "tLargeFileIO  mode=" << mode << " nrrec=" << nr
	 << " reclength=" << tleng << " incr=" << incr
	 << " setpos=" << setpos << endl;
    int* buf = new int[leng];
    int i;
    for (i=0; i<leng; i++) {
      buf[i] = 0;
    }

    if (mode >= 0) {
      Timer timer;
      int fd = FiledesIO::create("tLargeFileIO_tmp.dat2");
      FiledesIO file2 (fd, "");
      timer.mark();
      for (i=0; i<nr; i++) {
	buf[0] = i;
	file2.write (tleng, buf);
	if (setpos) {
	  file2.seek (incr, ByteIO::Current);
	}
      }
      ::fsync(fd);
      timer.show ("LargeFiledesIO     write");
      FiledesIO::close (fd);
    }
    if (mode <= 0) {
      Timer timer;
      int fd2 = FiledesIO::open ("tLargeFileIO_tmp.dat2");
      FiledesIO file2 (fd2, "");
      timer.mark();
      for (i=0; i<nr; i++) {
	file2.read (tleng, buf);
	if (buf[0] != i) {
	  cout << "Mismatch for record nr " << i << endl;
	}
	if (setpos) {
	  file2.seek (incr, ByteIO::Current);
	}
      }
      timer.show ("LargeFiledesIO     read ");
      FiledesIO::close (fd2);
    }

    delete [] buf;
    return 0;                           // exit with success status

  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }  
}
