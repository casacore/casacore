//# FilebufIO.h: Class for IO on a file using standard IO
//# Copyright (C) 1996,1997,1999
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

#if !defined(AIPS_FILEBUFIO_H)
#define AIPS_FILEBUFIO_H


//# Includes
#include <aips/aips.h>
#include <aips/IO/ByteIO.h>
#include <aips/Utilities/String.h>
#include <stdio.h>

// <summary> Class for IO on a file using standard IO.</summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class
//    <li> stdio package (see man stdio)
// </prerequisite>

// <synopsis> 
// This class is a specialization of class
// <linkto class=ByteIO>ByteIO</linkto>. It uses <src>stdio</src>
// to read/write data.
// <p>
// <src>stdio</src> uses an internal buffer. The size of the
// buffer influences the performance. Usually it is best to use
// the default buffer size (which stdio optimizes for the disk).
// However, it is possible to specify a particular buffer size.
// <p>
// It is also possible to construct a <src>FilebufIO</src> object
// from a file descriptor (e.g. for a pipe or socket).
// The constructor will determine automatically if the file is
// readable, writable and seekable.
// </synopsis>

// <example>
// This example shows how FilebufIO can be used with an fd.
// It uses the fd for a regular file, which could be done in an easier
// way using class <linkto class=RegularFileIO>RegularFileIO</linkto>.
// However, when using pipes or sockets, this would be the only way.
// <srcblock>
//    // Get a file descriptor for the file.
//    int fd = open ("file.name");
//    // Use that as the source of AipsIO (which will also use CanonicalIO).
//    FilebufIO fio (fd);
//    AipsIO stream (&fio);
//    // Read the data.
//    Int vali;
//    Bool valb;
//    stream >> vali >> valb;
// </srcblock>
// </example>

// <motivation> 
// Make it possible to use the AIPS++ IO functionality on any file.
// In this way any device can be hooked to the IO framework.
// </motivation>


class FilebufIO: public ByteIO
{
public: 
    // Default constructor.
    // A stream can be attached using the attach function.
    FilebufIO();

    // Construct from a given file pointer which is taken over
    // (thus the file is closed by function detach or the destructor).
    // It will determine itself if the file is readable, writable,
    // and seekable.
    // A buffer with the given length will be used by the FILE object.
    // A zero buffer size means an appropriate buffer size will be used.
    explicit FilebufIO (FILE*, uInt bufferSize=0);

    // Construct from a given file pointer.
    // When takeOver is true, the file will be closed by the
    // detach function or the destructor.
    // It will determine itself if the file is readable, writable,
    // and seekable.
    FilebufIO (FILE*, Bool takeOver);

    // Construct from the given file descriptor.
    // Note that the destructor and the detach function implicitly close
    // the file descriptor.
    explicit FilebufIO (int fd, uInt bufferSize=16384);

    // Attach the given file pointer which is taken over
    // (thus the file is closed by function detach or the destructor).
    // It will determine itself if the file is readable, writable,
    // and seekable.
    // A buffer with the given length will be used by the FILE object.
    // A zero buffer size means an appropriate buffer size will be used.
    void attach (FILE*, uInt bufferSize=0);

    // Attach the given file pointer.
    // When takeOver is true, the file will be closed by the
    // detach function or the destructor.
    // It will determine itself if the file is readable, writable,
    // and seekable.
    void attach (FILE*, Bool takeOver);

    // Attach to the given file descriptor.
    // Note that the destructor and the detach function implicitly close
    // the file descriptor.
    void attach (int fd, uInt bufferSize=16384);

    // The destructor closes the file when it was owned and opened and not
    // closed yet.
    ~FilebufIO();
    
    // Write the number of bytes.
    virtual void write (uInt size, const void* buf);

    // Read <src>size</src> bytes from the File. Returns the number of bytes
    // actually read. Will throw an Exception (AipsError) if the requested
    // number of bytes could not be read unless throwException is set to
    // False. Will always throw an exception if the file is not readable or
    // the system call returns an undocumented value.
    virtual Int read (uInt size, void* buf, Bool throwException=True);    

    // Reset the position pointer to the given value. It returns the
    // new position.
    virtual Long seek (Long offset, ByteIO::SeekOption = ByteIO::Begin);

    // Get the length of the byte stream.
    virtual Long length();
       
    // Is the IO stream readable?
    virtual Bool isReadable() const;

    // Is the IO stream writable?
    virtual Bool isWritable() const;

    // Is the IO stream seekable?
    virtual Bool isSeekable() const;

    // Get the file name of the file attached.
    virtual String fileName() const;


protected:
    // Attach the given FILE pointer.
    // If bufferSize>0, it allocates a buffer for the file.
    void attach (FILE* file, uInt bufferSize, Bool readable, Bool writable);

    // Detach the FILE. Close it when it is owned.
    void detach();

    // Determine if the file descriptor is readable and/or writable.
    void fillRWFlags (int fd);

    // Determine if the file is seekable.
    void fillSeekable();

    // Get the file for the derived class.
    FILE* getFilePtr();

    // Get the buffer size used.
    uInt bufferSize() const;

private:
    Bool        itsOwner;
    Bool        itsSeekable;
    Bool        itsReadable;
    Bool        itsWritable;
    FILE*       itsFile;
    uInt        itsBufSize;
    char*       itsBuffer;
    Bool        itsReadDone;
    Bool        itsWriteDone;

    // Copy constructor, should not be used.
    FilebufIO (const FilebufIO& that);

    // Assignment, should not be used.
    FilebufIO& operator= (const FilebufIO& that);
};


inline FILE* FilebufIO::getFilePtr()
{
    return itsFile;
}
inline uInt FilebufIO::bufferSize() const
{
    return itsBufSize;
}


#endif
