//# FilebufIO.h: Class for IO on a file using a filebuf object
//# Copyright (C) 1996
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

#if defined (_AIX)
#pragma implementation ("FilebufIO.cc")
#endif

//# Includes
#include <aips/aips.h>
#include <aips/IO/ByteIO.h>
#include <aips/Utilities/String.h>

#if !defined(AIPS_STDLIB)
imported class filebuf;
#endif

// <summary> 
// Class for IO on a file using a filebuf object.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class
//    <li> filebuf class in iostream package (see man filebuf)
// </prerequisite>

// <synopsis> 
// This class is a specialization of class
// <linkto class=ByteIO>ByteIO</linkto>. It uses a <src>filebuf</src>
// object to read/write data.
// <p>
// A <src>filebuf</src> uses an internal buffer. The size of the
// buffer influences the performance. The larger the buffer, the less
// physical IO has to be done. However, memory usage will increase
// when the buffer is large.
// It is possible to define the buffer size in the constructor of
// <src>FilebufIO</src>. Usually the default will do.
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
// Make it possible to use the AIPS++ IO functionality on a filebuf.
// In this way any device can be hooked to the IO framework.
// </motivation>


class FilebufIO: public ByteIO
{
public: 
    // Default constructor.
    // A stream can be attached using the attach function.
    FilebufIO();

    // Construct from a given file descriptor.
    // It will determine itself if the file is readable, writable,
    // and seekable.
    explicit FilebufIO (int fd, uInt filebufSize=65536);

    // The destructor closes the filebuf when not closed yet.
    ~FilebufIO();
    
    // Attach an fd to the filebuf.
    void attach (int fd, uInt bufferSize);

    // Write the number of bytes.
    virtual void write (uInt size, const void* buf);

    // Read the number of bytes.
    // An exception is thrown if <src>size</src> bytes are not available.
    virtual void read (uInt size, void* buf);    

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
    // Prepare for the attach.
    // It allocates a buffer with the given size for the filebuf.
    void prepareAttach (uInt bufferSize, Bool readable, Bool writable);

    // Detach the filebuf.
    void detach();

    // Test if the file is seekable.
    void fillSeekable();

    // Get the filebuf for the derived class.
    filebuf* getFilebuf();

    // Set to writable.
    void setWritable();

private:
    Bool        itsOwner;
    Bool        itsSeekable;
    Bool        itsReadable;
    Bool        itsWritable;
    filebuf*    itsFilebuf;
    char*       itsBuffer;
    Bool        itsReadDone;
    Bool        itsWriteDone;

    // Copy constructor, should not be used.
    FilebufIO (const FilebufIO& that);

    // Assignment, should not be used.
    FilebufIO& operator= (const FilebufIO& that);
};


inline filebuf* FilebufIO::getFilebuf()
{
    return itsFilebuf;
}
inline void FilebufIO::setWritable()
{
    itsWritable = True;
}


#endif
