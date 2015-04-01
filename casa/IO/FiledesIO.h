//# FiledesIO.h: Class for unbuffered IO on a file
//# Copyright (C) 1996,1997,1999,2001
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

#ifndef CASA_FILEDESIO_H
#define CASA_FILEDESIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class for unbuffered IO on a file.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1996/11/06" tests="tByteIO" demos="">
// </reviewed>

// <prerequisite> 
//    <li> <linkto class=ByteIO>ByteIO</linkto> class
//    <li> file descriptors
// </prerequisite>

// <synopsis> 
// This class is a specialization of class
// <linkto class=ByteIO>ByteIO</linkto>. It uses a file descriptor
// to read/write data.
// <p>
// The file associated with the file descriptor has to be opened
// before hand.
// The constructor will determine automatically if the file is
// readable, writable and seekable.
// Note that on destruction the file descriptor is NOT closed.
// </synopsis>

// <example>
// This example shows how FiledesIO can be used with an fd.
// It uses the fd for a regular file, which could be done in an easier
// way using class <linkto class=RegularFileIO>RegularFileIO</linkto>.
// However, when using pipes or sockets, this would be the only way.
// <srcblock>
//    // Get a file descriptor for the file.
//    int fd = open ("file.name");
//    // Use that as the source of AipsIO (which will also use CanonicalIO).
//    FiledesIO fio (fd);
//    AipsIO stream (&fio);
//    // Read the data.
//    Int vali;
//    Bool valb;
//    stream >> vali >> valb;
// </srcblock>
// </example>

// <motivation> 
// Make it possible to use the Casacore IO functionality on any file.
// In this way any device can be hooked to the IO framework.
// </motivation>


class FiledesIO: public ByteIO
{
public: 
    // Default constructor.
    // A stream can be attached using the attach function.
    FiledesIO();

    // Construct from the given file descriptor.
    // The file name is only used in possible error messages.
    explicit FiledesIO (int fd, const String& fileName=String());

    // Attach to the given file descriptor.
    // An exception is thrown if it is not in a detached state.
    // The file name is only used in error messages.
    void attach (int fd, const String& fileName);

    // Detach from the file descriptor. The file is not closed.
    void detach();

    // The destructor detaches, but does not close the file.
    virtual ~FiledesIO();

    // Write the number of bytes.
    virtual void write (Int64 size, const void* buf);

    // Read <src>size</src> bytes from the descriptor. Returns the number of
    // bytes actually read or a negative number if an error occurred. Will throw
    // an Exception (AipsError) if the requested number of bytes could not be
    // read, or an error occured, unless throwException is set to False. Will
    // always throw an exception if the descriptor is not readable or the
    // system call returned an undocumented value.
    virtual Int64 read (Int64 size, void* buf, Bool throwException=True);    

    // Get the length of the byte stream.
    virtual Int64 length();
       
    // Is the IO stream readable?
    virtual Bool isReadable() const;

    // Is the IO stream writable?
    virtual Bool isWritable() const;

    // Is the IO stream seekable?
    virtual Bool isSeekable() const;

    // Set that the IO stream is writable.
    void setWritable()
      { itsWritable = True; }

    // Get the file name of the file attached.
    virtual String fileName() const;

    // Fsync the file (i.e. force the data to be physically written).
    virtual void fsync();

    // Some static convenience functions for file create/open/close.
    // Close is only done if the fd is non-negative.
    // <group>
    static int create (const Char* name, int mode = 0666);
    static int open   (const Char* name, Bool writable = False,
		       Bool throwExcp = True);
    static void close (int fd);
    // </group>


protected:
    // Get the file descriptor.
    int fd() const
      { return itsFile; }

    // Determine if the file descriptor is readable and/or writable.
    void fillRWFlags (int fd);

    // Determine if the file is seekable.
    void fillSeekable();

    // Reset the position pointer to the given value. It returns the
    // new position.
    virtual Int64 doSeek (Int64 offset, ByteIO::SeekOption);

private:
    Bool   itsSeekable;
    Bool   itsReadable;
    Bool   itsWritable;
    int    itsFile;
    String itsFileName;

    // Copy constructor, should not be used.
    FiledesIO (const FiledesIO& that);

    // Assignment, should not be used.
    FiledesIO& operator= (const FiledesIO& that);
};


} //# NAMESPACE CASACORE - END

#endif
