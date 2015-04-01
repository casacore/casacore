//# tMappedIO.cc: Test program for performance of mapped file IO
//# Copyright (C) 2005
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
#include <casacore/casa/IO/LargeIOFuncDef.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <cstring>          //# needed for memcpy

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
    int nrch = 1;
    if (argc > 2) {
      istringstream istr(argv[2]);
      istr >> nrch;
    }
    int nr = 100;
    if (argc > 3) {
      istringstream istr(argv[3]);
      istr >> nr;
    }
    int leng = 1024;
    if (argc > 4) {
      istringstream istr(argv[4]);
      istr >> leng;
    }
    leng /= sizeof(int);
    int tleng = leng * sizeof(int);
    cout << "tMappedIO  mode=" << mode << " nrchunk=" << nrch << " nrrec=" << nr
	 << " reclength=" << tleng << endl;
    int* buf = new int[leng];
    int i;
    for (i=0; i<leng; i++) {
      buf[i] = 0;
    }
    if (nr*tleng > 100*1024*1024) {
      cout << "Chunk length " << nr*tleng << " too large (> 100 MB)" << endl;
      return 1;
    }

    Int64 size = nr*tleng;
    Int64 pagesz = getpagesize();
    if (mode >= 0) {
      Timer timer;
      int fd = FiledesIO::create("tMappedIO_tmp.dat2");
      timer.mark();
      ::traceLSEEK(fd, size*nrch-1, SEEK_SET);
      AlwaysAssert (::traceWRITE (fd, buf, 1) == 1, AipsError);
      timer.show("write");
      // Map chunk by chunk.
      for (int k=0; k<nrch; k++) {
	Int64 offset = k*size;
	// Calculate start of page on which offset is located.
	Int64 pageStartOffset = (offset/pagesz) * pagesz;
	// Add the difference to the nr of bytes to map.
	Int64 nrBytes = size + offset-pageStartOffset;
	// Do mmap
	int protect= PROT_READ | PROT_WRITE;
	void* pageStart = ::mmap (0, nrBytes, protect, MAP_SHARED, fd,
				  pageStartOffset);
	if (pageStart == MAP_FAILED) {
	  cout << "MMap::MMap - mmap failed: " << strerror(errno) << endl; 
	  return 1;
	}
	// Get requested pointer
	char* ptr = (char*)pageStart + offset-pageStartOffset;
	for (i=0; i<nr; i++) {
	  buf[0] = i + k*nr;
	  memcpy (ptr, buf, tleng);
	  ptr += tleng;
	}
	::munmap ((char*)pageStart, nrBytes);
      }
      timer.show ("Mapped IO     write");
      FiledesIO::close (fd);
    }
    if (mode <= 0) {
      Timer timer;
      int fd = FiledesIO::open ("tMappedIO_tmp.dat2");
      timer.mark();
      // Map chunk by chunk.
      for (int k=0; k<nrch; k++) {
	Int64 offset = k*size;
	// Calculate start of page on which offset is located.
	Int64 pageStartOffset = (offset/pagesz) * pagesz;
	// Add the difference to the nr of bytes to map.
	Int64 nrBytes = size + offset-pageStartOffset;
	// Do mmap
	int protect= PROT_READ;
	void* pageStart = ::mmap (0, nrBytes, protect, MAP_SHARED, fd,
				  pageStartOffset);
	if (pageStart == MAP_FAILED) {
	  cout << "MMap::MMap - mmap failed: " << strerror(errno) << endl; 
	  return 1;
	}
	// Get requested pointer
	char* ptr = (char*)pageStart + offset-pageStartOffset;
	for (i=0; i<nr; i++) {
	  memcpy (buf, ptr, tleng);
	  if (buf[0] != i + k*nr) {
	    cout << "Mismatch for record nr " << i+k*nr << endl;
	  }
	  ptr += tleng;
	}
	::munmap ((char*)pageStart, nrBytes);
      }
      timer.show ("Mapped IO     read ");
      FiledesIO::close (fd);
    }

    delete [] buf;
    return 0;                           // exit with success status

  } catch (const AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }  
}
