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

#include <aips/Utilities/DynBuffer.h>
#include <aips/Exceptions/Error.h>

// This is the implementation of the DynBuffer class.

// Construct the class.
// Allocate a first buffer of bufsz (default 4096) bytes.
// If we do not allocate it here, it will be done by newbuf.
// However, then we may get a huge buffer, which may be not so nice.
// bufsz may be sufficient for many purposes.
DynBuffer::DynBuffer (const uInt bsz) :
                          bufsz   (bsz),
                          nrbuf   (0),
                          bufptr  (10),
			  uselen  (10),
			  totlen  (10),
			  maxnrbuf(10)
{
    allocstart ();
    bufptr[0] = new Char[bufsz];
    if (bufptr[0] == 0) {
	throw (AllocError("DynBuffer constructor", bufsz));
    }else{
        totlen[0] = bufsz;
        nrbuf = 1;
    }
}


DynBuffer::~DynBuffer ()
{
    remove (0);
}


void DynBuffer::remove (const uInt n)
{
    for (uInt i=n; i<nrbuf; i++) {
	delete [] bufptr[i];
    }
    nrbuf = n;
    allocstart ();
}


// Start the buffers by resetting the current buffer.
void DynBuffer::allocstart ()
{
    curbuf = -1;
    curtotlen = 0;
    curuselen = 0;
}
    

// Get a new buffer
uInt DynBuffer::newbuf (const uInt nr, const uInt valsz)
{
    // Get the nr of free values in the current buffer.
    // If nothing left, use next buffer.
    // Store the used length of the current buffer.
    uInt n;
    while ((n = (curtotlen - curuselen) / valsz) == 0) {
	if (curbuf >= 0) {
            uselen[curbuf] = curuselen;
        }

	// If no more buffers, get new one with required length.
	// Use a minimum length of bufsz.
	// Extend the administration blocks if they are full.
	if (curbuf == nrbuf-1) {
	    if (nrbuf == maxnrbuf) {
		maxnrbuf += 10;
		bufptr.resize (maxnrbuf);
		totlen.resize (maxnrbuf);
		uselen.resize (maxnrbuf);
	    }
	    totlen[nrbuf] = (nr*valsz > bufsz ? nr*valsz : bufsz);
	    bufptr[nrbuf] = new Char[totlen[nrbuf]];
	    if (bufptr[nrbuf] == 0) {
		throw (AllocError("DynBuffer", totlen[nrbuf]));
		return 0;
	    }
	    nrbuf++;
	}

	// Okay, we have another buffer.
	// Set the current lengths and buffer pointer.
	curbuf++;
	curuselen = 0;
	curtotlen = totlen[curbuf];
	curbufptr = bufptr[curbuf];
    }
    return (n<nr ? n : nr);                 // #values fitting or needed
}


// Start for the next function.
// Store used length of last block used.
void DynBuffer::nextstart ()
{
    nextbuf = 0;
    if (curbuf >= 0) {
	uselen[curbuf] = curuselen;
    }
}

Bool DynBuffer::next (uInt& len, Char*& ptr)
{
    if (nextbuf > curbuf) {
	len = 0;
	return False;                         // no more buffers
    }else{
	len = uselen[nextbuf];
	ptr = bufptr[nextbuf];
	nextbuf++;
	return True;
    }
}
