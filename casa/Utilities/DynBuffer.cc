//# DynBuffer.cc: Store data in dynamically allocated buffers
//# Copyright (C) 1993,1994,1995
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

#include <casacore/casa/Utilities/DynBuffer.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// This is the implementation of the DynBuffer class.

// Construct the class.
// Allocate a first buffer of bufsz (default 4096) bytes.
// If we do not allocate it here, it will be done by newbuf.
// However, then we may get a huge buffer, which may be not so nice.
// bufsz may be sufficient for many purposes.
DynBuffer::DynBuffer (uInt bsz)
: bufsz_p   (bsz),
  nrbuf_p   (0),
  maxnrbuf_p(10),
  uselen_p  (10),
  totlen_p  (10),
  bufptr_p  (10)
{
    allocstart ();
    bufptr_p[0] = new Char[bufsz_p];
    totlen_p[0] = bufsz_p;
    nrbuf_p = 1;
}


DynBuffer::~DynBuffer ()
{
    remove (0);
}


void DynBuffer::remove (uInt n)
{
    for (Int i=n; i<nrbuf_p; i++) {
	delete [] bufptr_p[i];
    }
    nrbuf_p = n;
    allocstart ();
}


// Start the buffers by resetting the current buffer.
void DynBuffer::allocstart ()
{
    curbuf_p = -1;
    curtotlen_p = 0;
    curuselen_p = 0;
}
    

// Get a new buffer
uInt DynBuffer::newbuf (uInt nr, uInt valsz)
{
    // Get the nr of free values in the current buffer.
    // If nothing left, use next buffer.
    // Store the used length of the current buffer.
    uInt n;
    while ((n = (curtotlen_p - curuselen_p) / valsz) == 0) {
	if (curbuf_p >= 0) {
            uselen_p[curbuf_p] = curuselen_p;
        }

	// If no more buffers, get new one with required length.
	// Use a minimum length of bufsz.
	// Extend the administration blocks if they are full.
	if (curbuf_p == nrbuf_p-1) {
	    if (nrbuf_p == maxnrbuf_p) {
		maxnrbuf_p += 10;
		bufptr_p.resize (maxnrbuf_p);
		totlen_p.resize (maxnrbuf_p);
		uselen_p.resize (maxnrbuf_p);
	    }
	    totlen_p[nrbuf_p] = (nr*valsz > bufsz_p ? nr*valsz : bufsz_p);
	    bufptr_p[nrbuf_p] = new Char[totlen_p[nrbuf_p]];
	    nrbuf_p++;
	}

	// Okay, we have another buffer.
	// Set the current lengths and buffer pointer.
	curbuf_p++;
	curuselen_p = 0;
	curtotlen_p = totlen_p[curbuf_p];
	curbufptr_p = bufptr_p[curbuf_p];
    }
    return (n<nr ? n : nr);                 // #values fitting or needed
}


// Start for the next function.
// Store used length of last block used.
void DynBuffer::nextstart ()
{
    nextbuf_p = 0;
    if (curbuf_p >= 0) {
	uselen_p[curbuf_p] = curuselen_p;
    }
}

Bool DynBuffer::next (uInt& len, Char*& ptr)
{
    if (nextbuf_p > curbuf_p) {
	len = 0;
	return False;                         // no more buffers
    }else{
	len = uselen_p[nextbuf_p];
	ptr = bufptr_p[nextbuf_p];
	nextbuf_p++;
	return True;
    }
}

} //# NAMESPACE CASACORE - END

