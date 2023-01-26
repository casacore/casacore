//# MemoryIO.h: Class for IO in memory
//# Copyright (C) 1996,1999,2001
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

#ifndef CASA_MEMORYIO_H
#define CASA_MEMORYIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>Class for IO to a memory buffer.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
// </reviewed>

// <prerequisite> 
//  <li> <linkto class=ByteIO>ByteIO</linkto>
// </prerequisite>

// <synopsis>
// This class is doing IO in a buffer in memory.
// It is part of the entire IO framework. It can for
// instance be used to store data in canonical format in a
// memory string and obtain it later.
// <p>
// The memory buffer can be dynamic, so it will be expanded when needed.
// This is done by allocating a larger buffer, copy the contents and
// throw the old buffer away.
// <br>
// The memory buffer can also be static to be sure that the pointer to
// the buffer will not change.
// The expand size determines if the memory buffer is static or dynamic.
// An expand size zero indicates a static buffer.
// <p>
// The memory buffer is seekable and readable. It depends on the
// constructor whether it is writable.
// <p>
// There are several ways in which the buffer can be created/passed:
// <ul>
//  <li> Dynamic by passing an initial size and an expand size.
//       However, an expand size zero can be used to assure that no more
//       data is written than fits in the initial buffer (so once the
//       buffer is created it gets static).
//       In this way the buffer is readable and writable.
//  <li> Static by passing a const buffer and its length.
//       In this way the buffer is not writable.
//  <li> Dynamic or static by passing a non-const buffer, its length,
//       and an expand size (zero = static, >0 = dynamic)
//     . The OpenOption indicates whether the buffer will be writable.
//       Only for ByteIO::Old it will not be writable.
//       The OpenOption also determines the initial seek position.
//       Usually it is 0, but for ByteIO::Append it is the end of the buffer.
// </ul>
// The user can obtain a pointer to the buffer to extract the
// stored data from it. The length of the data can also be obtained.
// <p>
// Usually this class will be used in combination with, say, CanonicalIO
// and AipsIO.

// <example>
// <srcblock>
//    // Create dynamic (expandable) memory buffer of length 100.
//    // Use that as the sink of RawIO in AipsIO.
//    MemoryIO membuf (100);
//    RawIO rawio (&membuf);
//    AipsIO stream (&rawio);
//    // Write values.
//    stream << (int32_t)10;
//    stream << true;
//    // Seek to beginning of buffer and read data in.
//    stream.setpos (0);
//    int32_t vali;
//    bool valb;
//    stream >> vali >> valb;
//
//    // One can obtain the buffer and its length and use it later.
//    // (e.g. to write it in a non-AipsIO file).
//    unsigned char* bufptr = membuf.getBuffer();
//    uint64_t length = membuf.length();
//
//    // It can also used to construct another MemoryIO object from it.
//    // The following memory buffer is static and readonly.
//    MemoryIO membuf2 (bufptr, length);
//    membuf2.read (sizeof(vali), vali);
//    membuf2.read (sizeof(valb), valb);
// </srcblock>
// </example>

// <motivation> 
// Make it possible to do IO in a memory buffer.
// The first implementation used strstreambuf from the iostream package.
// However, that did not allow seeking and it was hard to get the length.
// </motivation>


class MemoryIO: public ByteIO
{
public:
    // Construct a dynamic object with the given initial length.
    explicit MemoryIO (uint64_t initialSize=65536, uint64_t expandSize=32768);

    // Construct from a buffer with the given length.
    // The buffer is readonly and cannot be expanded.
    MemoryIO (const void* buffer, uint64_t size);

    // Construct from the given buffer with the given length.
    // The Byte::Option determines how the buffer will be used.
    // The seek pointer is set to the beginning of the buffer, unless
    // told otherwise below.
    // <dl>
    //  <dt> New, NewNoReplace and Scratch
    //  <dd> The buffer is empty and is read/write.
    //  <dt> Old
    //  <dd> The buffer contains <src>size</src> bytes and is readonly.
    //  <dt> Update, Delete
    //  <dd> The buffer contains <src>size</src> bytes and is read/write.
    //  <dt> Append
    //  <dd> The buffer contains <src>size</src> bytes and is read/write.
    //       The seek pointer is set to the end of the buffer.
    // </dl>
    // When the buffer is writable, it will be expanded if needed.
    // This means that <src>buffer</src> does not point to the data
    // anymore. However, when <src>expandSize==0</src>, the buffer
    // cannot be expanded and the pointer is always valid.
    // <br>When canDelete is true, buffer expansion means that the
    // old buffer gets deleted.
    MemoryIO (void* buffer, uint64_t size, ByteIO::OpenOption,
	      uint64_t expandSize=0, bool canDelete=false);

    // Delete the Memory object.
    // The data buffer is not deleted when constructed with the
    // constructor taking a buffer pointer.
    ~MemoryIO();

    // Write the number of bytes.
    // When needed it expands the buffer.
    // An exception is thrown when the buffer is not writable or
    // when buffer expansion fails or is not possible.
    virtual void write (int64_t size, const void* buf);

    // Read <src>size</src> bytes from the memory buffer. Returns the number of
    // bytes actually read. Will throw an Exception (AipsError) if the
    // requested number of bytes could not be read unless throwException is set
    // to false. Will always throw an exception if the buffer is not readable
    // or the buffer pointer is at an invalid position.
    virtual int64_t read (int64_t size, void* buf, bool throwException=true);    

    // Clear the buffer; i.e. set the data length and seek pointer to zero.
    void clear();

    // Get the buffer containing the data.
    // <br>The length of the data in the buffer can be obtained using the
    // length() function.
    const unsigned char* getBuffer() const;

    // Get the length of the data in the buffer.
    virtual int64_t length();

    // Get the allocated length of the buffer.
    uint64_t allocated() const;
       
    // Get the expand size (0 = not expandable).
    uint64_t expandSize() const;
       
    // Is the IO stream readable?
    virtual bool isReadable() const;

    // Is the IO stream writable?
    virtual bool isWritable() const;

    // Is the IO stream seekable?
    virtual bool isSeekable() const;

    // resize the internal buffer (if necessary) so that it is big enough
    // to hold the specified number of bytes. Returns a non-const pointer
    // to the buffer that can be used to write up to the specified number
    // of bytes into the buffer. If less data is written into the buffer
    // then the setUsed member funtion should be used to indicate how much
    // of the buffer is valid. Throws an exception if the MemoryIO object
    // is not writable or if it needs to increase the size of the internal
    // buffer and the MemoryIO object is not expandable.
    // <note role=warning> You should not use the supplied pointer to write
    // more than length data points to the buffer</note>
    unsigned char* setBuffer(uint64_t length);

    // tell the MemoryIO object how much of its internal buffer is valid
    // data. You only need to use this function if you are directly writing to
    // the buffer using the pointer returned by the non-const getBuffer
    // function. This function throws an exception if the number of bytes used
    // is greater than the number allocated or if the MemoryIO object is not
    // writeable.
    void setUsed(uint64_t bytesUsed);

private:
    //# Copy constructor, should not be used.
    MemoryIO (const MemoryIO& that);

    //# Assignment, should not be used.
    MemoryIO& operator= (const MemoryIO& that);

    // Reset the position pointer to the given value. It returns the
    // new position.
    // An exception is thrown when seeking before the start of the
    // buffer or when seeking past the end of a readonly buffer.
    // When seeking past the end of a writable buffer, the required
    // amount of bytes is added and initialized to zero.
    virtual int64_t doSeek (int64_t offset, ByteIO::SeekOption);

    //# Expand the buffer to at least the given size. The specified size
    //# will be used if it results in a larger buffer size. In this way the
    //# buffer does not get reallocated too often.  It returns a false status
    //# when the buffer cannot be expanded. 
    bool expand (uint64_t minSize);


    unsigned char* itsBuffer;
    int64_t  itsAlloc;
    int64_t  itsExpandSize;
    int64_t  itsUsed;
    int64_t  itsPosition;
    bool   itsReadable;
    bool   itsWritable;
    bool   itsCanDelete;
};


inline void MemoryIO::clear()
{
    itsUsed = itsPosition = 0;
}
inline const unsigned char* MemoryIO::getBuffer() const
{
    return itsBuffer;
}
inline uint64_t MemoryIO::allocated() const
{
    return itsAlloc;
}
inline uint64_t MemoryIO::expandSize() const
{
    return itsExpandSize;
}



} //# NAMESPACE CASACORE - END

#endif
