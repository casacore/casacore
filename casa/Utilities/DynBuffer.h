//# DynBuffer.h: Store data in dynamically allocated buffers
//# Copyright (C) 1993,1994,1995,1996
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

#ifndef CASA_DYNBUFFER_H
#define CASA_DYNBUFFER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Store data in dynamically allocated buffers
// </summary>

// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/16" tests="tDynBuffer" demos="">
// </reviewed>

// <synopsis> 
// DynBuffer allows one to store data in dynamically allocated buffers.
// When a buffer is full, an additional buffer can be allocated and
// "linked" to the existing one; so, the data may not be stored contiguously
// You can loop through all the linked buffers and get their individual
// addresses and sizes, so that you can access the data.
// </synopsis> 

// <example>
// Example (without exception handling):
// <srcblock>
// uInt nrOfValues, nrNeeded, nrAvailable;// nr of data values
// float* pData = floatarr;               // ptr to data to be handled
// Char* pBuffer;                         // ptr to buffer
//
// DynBuffer buffer;                      // create buffer
// buffer.allocstart();                   // prepare for storing
// nrNeeded = nrOfValues;                 // nr of values to store
// // copy data into dynamic buffer
// while (nrNeeded > 0) {
//     nrAvailable = buffer.alloc (nrNeeded, sizeof(float), pBuffer);
//                                        // get buffer space:
//                                        // room for nrAvailable values
//     memcpy (pBuffer, pData, nrAvailable*sizeof(float));
//                                        // copy that many data values
//     nrNeeded -= nrAvailable;           // how much more needed?
//     pData += nrAvailable;              // pointer to as yet unstored data
// }
// // Maybe store more values
//     .
//     .
// // Retrieve all the data values from the buffers and write them
// buffer.nextstart();                    // goto buffer start
// while (buffer.next (nrAvailable, pBuffer)) {
//                                        // get next buffer
//     write (fd, nrAvailable, pBuffer);  // write data from that buffer
// }
// </srcblock>
// </example>

// <motivation>
// This class is developed as an intermediate buffer for
// class <linkto class=AipsIO>AipsIO</linkto>,
// but it may serve other purposes as well.
// </motivation>

class DynBuffer
{
public:

    // Allocate a first buffer of the specified number of bytes
    // (default 4096). When the allocation fails, an exception is thrown.
    DynBuffer (uInt nrOfBytes=4096);

    // Remove the whole buffer, i.e. the first buffer and all the 
    // buffers appended to it.
    ~DynBuffer ();

    // Prepare for storing data (re-initialize the buffer)
    void allocstart ();

    // Allocate buffer space for <src>nrOfValues</src> values of size
    // <src>valueSize</src> bytes, and return the pointer <src>ptr</src>
    // to the buffer and the number of values that fit in the buffer.
    // 
    // When not all values fit in the current buffer, new buffer space
    // is added (probably non-contiguous). If that allocation fails an
    // exception is thrown.
    uInt alloc (uInt nrOfValues, uInt valueSize, Char*& ptr);

    // Remove buffer <src>nrOfBuffer</src> and the buffers appended to it,
    // and re-initialize the current buffer. By default we keep the first
    // buffer (i.e. the one numbered 0).
    //
    // The idea is that you may want to free intermediate storage
    // space taken up by data that you no longer need, and that the
    // first buffer is often big enough to hold further data. So, you
    // only remove the first buffer in special cases.
    void remove (uInt nrOfBuffer=1);

    // Prepare for data retrieval (set up for looping through the buffers).
    void nextstart ();

    // Get the pointer to the next buffer and its used length in bytes.
    // The function returns a <src>False</src> value if there are no more
    // buffers.
    Bool next (uInt& usedLength, Char*& ptr);

private:
    // Get the next buffer for storing <src>nrOfValues</src> values of
    // size <src>valueSize</src> bytes, and return the number of values
    // that can be stored in the free space of that buffer (maybe less
    // than <src>nrOfValues</src>).
    //
    // The new current buffer can be the present one (if it has free
    // space), the next buffer already allocated (if there is one), or
    // a newly allocated and linked-in buffer. If, in the last case,
    // the allocation fails an exception is thrown.
    uInt newbuf (uInt nrOfValues, uInt valueSize);


    // size of 1st buffer and min. bufsize
    uInt         bufsz_p;
    // buffernr for next function
    Int          nextbuf_p;
    // current buffernr
    Int          curbuf_p;
    // nr of buffers allocated
    Int          nrbuf_p;
    // size of Blocks
    Int          maxnrbuf_p;
    // used length per buffer
    Block<uInt>  uselen_p;     
    // total length per buffer
    Block<uInt>  totlen_p;      
    // pointer to buffer
    PtrBlock<Char*> bufptr_p;     
    // used length of current buffer
    uInt         curuselen_p;      
    // total length of current buffer
    uInt         curtotlen_p;      
    // pointer to current buffer
    Char*        curbufptr_p;      
};


//# Allocate buffer space for the nrOfValues values.
//# Return pointer to the buffer and nr of values that fit in it.
//# Use a more specialized function if not all values fit.
//# In this way the function can be kept small and thus used inline.
//# newbuf will seldom be required, unless large vectors are stored.
inline uInt DynBuffer::alloc (uInt nrOfValues, uInt valueSize, Char*& ptr)
{
    uInt n = nrOfValues;
    if (n*valueSize > curtotlen_p-curuselen_p) {
	n = newbuf (nrOfValues, valueSize);
    }
    ptr = curbufptr_p + curuselen_p;
    curuselen_p += n*valueSize;
    return n;
}



} //# NAMESPACE CASACORE - END

#endif
