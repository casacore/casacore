//# ByteIO.h: Abstract base class for IO on a byte stream
//# Copyright (C) 1996,1999
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

#if !defined(AIPS_BYTEIO_H)
#define AIPS_BYTEIO_H

#if defined (_AIX)
#pragma implementation ("ByteIO.cc")
#endif

//# Includes
#include <aips/aips.h>


// <summary> 
// Abstract base class for IO on a byte stream.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
// </reviewed>

// <synopsis> 
// ByteIO is the abstract base class for all classes doing IO on
// byte streams. Examples of derived classes are
// <linkto class=RegularFileIO>RegularFileIO</linkto> and
// <linkto class=MemoryIO>MemoryIO</linkto>.
// <p>
// ByteIO contains two enumerations, which define the possible
// open and seek options on byte streams. These enumerations
// are used throughout the IO framework.
// </synopsis>

// <motivation> 
// Make polymorphic operations on byte streams possible.
// </motivation>


class ByteIO
{
public:
    // Define the possible ByteIO open options.
    enum OpenOption {
	Old=1,
	// read/write; file must exist.
	Update,
	// read/write; create file if not exist.
	Append,
	// read/write; create file if not exist.
	New,
	// read/write; file may not exist yet.
	NewNoReplace,
	// read/write; delete file at close.
	Scratch,
	// read/write; file must exist; delete at close.
	Delete
    };

    // Define the possible seek options.
    enum SeekOption {
	// Seek from beginning of file.
	Begin=1,
	// Seek from current position.
	Current,
	// Seek from the end of the file.
	End
    };


    // The constructor does nothing.
    ByteIO();

    virtual ~ByteIO();

    // Write <src>size</src> bytes to the byte stream.
    virtual void write (uInt size, const void* buf) = 0;

    // Read <src>size</src> bytes from the byte stream.
    virtual void read (uInt size, void* buf) = 0;    

    // Reopen the underlying IO stream for read/write access.
    // Nothing will be done if the stream is writable already.
    // Otherwise it will be reopened and an exception will be thrown
    // if it is not possible to reopen it for read/write access.
    // The default implementation in this base class throws a "not possible"
    // exception if a reopen has to be done.
    virtual void reopenRW();

    // This function sets the position on the given offset.
    // The seek option defines from which file position the seek is done.
    // -1 is returned if not seekable.
    virtual Long seek (Long offset, ByteIO::SeekOption = ByteIO::Begin) = 0;

    // Get the length of the byte stream.
    virtual Long length() = 0;
    
    // Is the byte stream readable?
    virtual Bool isReadable() const = 0;

    // Is the byte stream writable?
    virtual Bool isWritable() const = 0;

    // Is the byte stream seekable?
    virtual Bool isSeekable() const = 0;


protected:
    // Make copy constructor and assignment protected, so a user cannot
    // use them (but a derived class can).
    // <group>
    ByteIO (const ByteIO& byteIO);
    ByteIO& operator= (const ByteIO& byteIO);
    // </group>
};



inline ByteIO::ByteIO()
{}

inline ByteIO::ByteIO (const ByteIO&)
{}

inline ByteIO& ByteIO::operator= (const ByteIO&)
{
   return *this;
}


#endif
