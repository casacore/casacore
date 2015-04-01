//# tFileIO.cc: Test program for performance of file IO
//# Copyright (C) 1997,2000,2001,2003
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
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
int main (int argc, const char* argv[])
{
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
    int size = 0;
    if (argc > 3) {
	istringstream istr(argv[3]);
	istr >> size;
    }
    int seek = 0;
    if (argc > 4) {
	istringstream istr(argv[4]);
	istr >> seek;
    }
    cout << "tFileIO  nrrec=" << nr << " reclength=" << leng
	 << " buffersize=" << size << " seek=" << seek << endl;
    char* buf = new char[leng];
    int i;
    for (i=0; i<leng; i++) {
	buf[i] = 0;
    }

    {
	RegularFileIO file1(RegularFile("tFileIO_tmp.dat1"),
			    ByteIO::New, size);
	Timer timer;
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file1.seek (i*leng, ByteIO::Begin);
	    }
	    file1.write (leng, buf);
	}
	timer.show ("RegularFileIO write");
	int fd2 = FiledesIO::create ("tFileIO_tmp.dat1");
	FiledesIO file2 (fd2, "");
	timer.mark();
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file2.seek (i*leng, ByteIO::Begin);
	    }
	    file2.write (leng, buf);
	}
	timer.show ("FiledesIO     write");
	fsync(fd2);
	timer.show ("FiledesIO     +sync");
        FiledesIO::close (fd2);
    }
    {
	RegularFileIO file1(RegularFile("tFileIO_tmp.dat1"),
			    ByteIO::Old, size);
	Timer timer;
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file1.seek (i*leng, ByteIO::Begin);
	    }
	    file1.read (leng, buf);
	    if (buf[0] != 0) cout << "mismatch" << endl;
	}
	timer.show ("RegularFileIO read ");
	int fd2 = FiledesIO::open ("tFileIO_tmp.dat1");
	FiledesIO file2 (fd2, "");
	timer.mark();
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file2.seek (i*leng, ByteIO::Begin);
	    }
	    file2.read (leng, buf);
	    if (buf[0] != 0) cout << "mismatch" << endl;
	}
	timer.show ("FiledesIO     read ");
        FiledesIO::close (fd2);
    }
    {
	RegularFileIO file1(RegularFile("tFileIO_tmp.dat2"),
				 ByteIO::New, size);
	Timer timer;
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file1.seek (i*leng, ByteIO::Begin);
	    }
	    file1.write (leng, buf);
	}
	timer.show ("LargeRegularFileIO write");
	int fd2 = FiledesIO::create ("tFileIO_tmp.dat2");
	FiledesIO file2 (fd2, "");
	timer.mark();
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file2.seek (i*leng, ByteIO::Begin);
	    }
	    file2.write (leng, buf);
	}
	timer.show ("LargeFiledesIO     write");
	fsync(fd2);
	timer.show ("LargeFiledesIO     +sync");
        FiledesIO::close (fd2);
    }
    {
	RegularFileIO file1(RegularFile("tFileIO_tmp.dat2"),
				 ByteIO::Old, size);
	Timer timer;
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file1.seek (i*leng, ByteIO::Begin);
	    }
	    file1.read (leng, buf);
	}
	timer.show ("LargeRegularFileIO read ");
	int fd2 = FiledesIO::open ("tFileIO_tmp.dat2");
	FiledesIO file2 (fd2, "");
	timer.mark();
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file2.seek (i*leng, ByteIO::Begin);
	    }
	    file2.read (leng, buf);
	}
	timer.show ("LargeFiledesIO     read ");
	FiledesIO::close (fd2);
    }

    {
	RegularFileIO file1(RegularFile("tFileIO_tmp.dat2"),
			    ByteIO::Old, size);
	Timer timer;
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file1.seek (i*leng, ByteIO::Begin);
	    }
	    file1.read (leng, buf);
	}
	timer.show ("RegularFileIO large ");
	int fd2 = FiledesIO::open ("tFileIO_tmp.dat2");
	FiledesIO file2 (fd2, "");
	timer.mark();
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file2.seek (i*leng, ByteIO::Begin);
	    }
	    file2.read (leng, buf);
	}
	timer.show ("FiledesIO     large");
        FiledesIO::close (fd2);
    }
    {
	RegularFileIO file1(RegularFile("tFileIO_tmp.dat1"),
                            ByteIO::Old, size);
	Timer timer;
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file1.seek (i*leng, ByteIO::Begin);
	    }
	    file1.read (leng, buf);
	}
	timer.show ("LargeRegularFileIO small");
	int fd2 = FiledesIO::open ("tFileIO_tmp.dat1");
	FiledesIO file2 (fd2, "");
	timer.mark();
	for (i=0; i<nr; i++) {
	    if (seek  &&  i%3 == 0) {
		file2.seek (i*leng, ByteIO::Begin);
	    }
	    file2.read (leng, buf);
	}
	timer.show ("LargeFiledesIO     small");
        FiledesIO::close (fd2);
    }

    delete [] buf;
    return 0;                           // exit with success status
}
