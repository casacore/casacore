//# BucketFile.h: File object for Tiled hypercube Storage Manager
//# Copyright (C) 1995,1996,1999,2001
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

#ifndef CASA_BUCKETFILE_H
#define CASA_BUCKETFILE_H

//# Includes
#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <unistd.h>


namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// File object for the bucket cache.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

//# <prerequisite>
//# Classes you should understand before using this one.
//# </prerequisite>

// <etymology>
// BucketFile represents a data file for the BucketCache class.
// </etymology>

// <synopsis>
// A BucketFile object represents a data file. Currently it is used
// by the Table system, but it can easily be turned into
// a more general storage manager file class.
// <br>
// Creation of a BucketFile object does not open the file yet.
// An explicit open call has to be given before the file can be used.
// <p>
// Underneath it uses a file descriptor to access the file.
// It is straightforward to replace this by a mapped file or a filebuf.
// </synopsis> 

// <motivation>
// Encapsulate the file creation and access into a single class
// to hide the file IO details.
// </motivation>

// <example>
// <srcblock>
//     // Create the file for the given storage manager.
//     BucketFile file ("file.name");
//     // Open the file and write into it.
//     file.open();
//     file.write (someBuffer, someLength);
//     // Get the length of the file.
//     uInt size = file.fileSize();
// </srcblock>
// </example>

// <todo asof="$DATE:$">
//  <li> Use the ByteIO classes when they are ready.
// </todo>


class BucketFile
{
public:
    // Create a BucketFile object for a new file.
    // The file with the given name will be created.
    explicit BucketFile (const String& fileName);

    // Create a BucketFile object for an existing file.
    // The file should be opened by the <src>open</src>.
    // Tell if the file must later be opened writable.
    BucketFile (const String& fileName, Bool writable);

    // The destructor closes the file (if open).
    ~BucketFile();

    // Open the file if not open yet.
    void open();

    // Close the file (if open).
    void close();

    // Remove the file (and close it if needed).
    void remove();

    // Fsync the file (i.e. force the data to be physically written).
    void fsync();

    // Set the file to read/write access.
    // It does nothing if the file is already writable.
    void setRW();

    // Get the file name.
    const String& name() const;
    
    // Has the file logically been indicated as writable?
    Bool isWritable() const;

    // Read bytes from the file.
    uInt read (void* buffer, uInt length) const;

    // Write bytes into the file.
    uInt write (const void* buffer, uInt length);

    // Seek in the file.
    // <group>
    void seek (Int64 offset) const;
    void seek (Int offset) const;
    // </group>

    // Get the (physical) size of the file.
    // This is doing a seek and sets the file pointer to end-of-file.
    Int64 fileSize() const;

    // Get the file descriptor of the internal file.
    int fd();

private:
    // The file name.
    String name_p;
    // The (logical) writability of the file.
    Bool isWritable_p;
    // The file descriptor.
    int fd_p;
	    

    // Forbid copy constructor.
    BucketFile (const BucketFile&);

    // Forbid assignment.
    BucketFile& operator= (const BucketFile&);
};


inline const String& BucketFile::name() const
    { return name_p; }

inline Bool BucketFile::isWritable() const
    { return isWritable_p; }

inline int BucketFile::fd()
    { return fd_p; }

inline void BucketFile::seek (Int offset) const
    { seek (Int64(offset)); }



} //# NAMESPACE CASA - END

#endif
