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

#include <aips/IO/LargeFiledesIO.h>
#include <aips/OS/Timer.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>
#include <aips/iostream.h>
#include <aips/sstream.h>
#include <unistd.h>


int main (int argc, char** argv)
{
  try {
    int nr = 100;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    int leng = 1024;
    if (argc > 2) {
	istringstream istr(argv[2]);
	istr >> leng;
    }
    leng /= sizeof(int);
    int tleng = leng * sizeof(int);
    cout << "tLargeFileIO  nrrec=" << nr << " reclength=" << tleng
	 << endl;
    int* buf = new int[leng];
    int i;
    for (i=0; i<leng; i++) {
	buf[i] = 0;
    }

    {
	Timer timer;
	int fd = LargeFiledesIO::create("tLargeFileIO_tmp.dat2");
	LargeFiledesIO file2 (fd);
	timer.mark();
	for (i=0; i<nr; i++) {
	    buf[0] = i;
	    file2.write (tleng, buf);
	}
	::fsync(fd);
	timer.show ("LargeFiledesIO     write");
    }
    {
	Timer timer;
	LargeFiledesIO file2 (LargeFiledesIO::open ("tLargeFileIO_tmp.dat2"));
	timer.mark();
	for (i=0; i<nr; i++) {
	    file2.read (tleng, buf);
	    if (buf[0] != i) {
	        cout << "Mismatch for record nr " << i << endl;
	    }
	}
	timer.show ("LargeFiledesIO     read ");
    }

    delete [] buf;
    return 0;                           // exit with success status

  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }  
}
