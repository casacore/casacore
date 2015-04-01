//# BucketFile.h: File object for BucketCache
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
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/IO/MMapfdIO.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <unistd.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MultiFileBase;

// <summary>
// File object for BucketCache.
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
// The file can be opened as an ordinary file (with a file descriptor)
// or as a file in a MultiFileBase object. An ordinary file can be accessed
// in 3 ways:
// <ul>
//  <li> In an unbuffered way, where the parent BucketCache class accesses
//       a bucket at a time (and possibly keeps it in a cache).
//  <li> In a memory-mapped way, where the parent BucketMapped class does
//       the access using the MMapfdIO member.
//  <li> In a buffered way, where the parent BucketBuffered class does
//       the access using the FilebufIO member.
// </ul>
// A MultiFileBase file can only be accessed in the unbuffered way.
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
    // The file with the given name will be created as a normal file or
    // as part of a MultiFileBase (if mfile != 0).
    // It can be indicated if a MMapfdIO and/or FilebufIO object must be
    // created for the file. If a MultiFileBase is used, memory-mapped IO
    // cannot be used and mappedFile is ignored.
    explicit BucketFile (const String& fileName,
                         uInt bufSizeFile=0, Bool mappedFile=False,
                         MultiFileBase* mfile=0);

    // Create a BucketFile object for an existing file.
    // The file should be opened by the <src>open</src>.
    // Tell if the file must be opened writable.
    // It can be indicated if a MMapfdIO and/or FilebufIO object must be
    // created for the file. If a MultiFileBase is used, memory-mapped IO
    // cannot be used and mappedFile is ignored.
    BucketFile (const String& fileName, Bool writable,
                uInt bufSizeFile=0, Bool mappedFile=False,
                MultiFileBase* mfile=0);

    // The destructor closes the file (if open).
    virtual ~BucketFile();

    // Make a (temporary) buffered IO object for this file.
    // That object should not close the file.
    virtual CountedPtr<ByteIO> makeFilebufIO (uInt bufferSize);

    // Get the mapped file object.
    MMapfdIO* mappedFile()
      { return mappedFile_p; }

    // Get the buffered file object.
    FilebufIO* bufferedFile()
      { return bufferedFile_p; }

    // Open the file if not open yet.
    virtual void open();

    // Close the file (if open).
    virtual void close();

    // Remove the file (and close it if needed).
    virtual void remove();

    // Fsync the file (i.e. force the data to be physically written).
    virtual void fsync();

    // Set the file to read/write access. It is reopened if not writable.
    // It does nothing if the file is already writable.
    virtual void setRW();

    // Get the file name.
    virtual const String& name() const;
    
    // Has the file logically been indicated as writable?
    Bool isWritable() const;

    // Read bytes from the file.
    virtual uInt read (void* buffer, uInt length);

    // Write bytes into the file.
    virtual uInt write (const void* buffer, uInt length);

    // Seek in the file.
    // <group>
    virtual void seek (Int64 offset);
    void seek (Int offset);
    // </group>

    // Get the (physical) size of the file.
    // This is doing a seek and sets the file pointer to end-of-file.
    virtual Int64 fileSize() const;

    // Is the file cached, mapped, or buffered?
    // <group>
    Bool isCached() const;
    Bool isMapped() const;
    Bool isBuffered() const;
    // </group>

private:
    // The file name.
    String name_p;
    // The (logical) writability of the file.
    Bool isWritable_p;
    Bool isMapped_p;
    uInt bufSize_p;
    int  fd_p;    //  fd (if used) of unbuffered file
    // The unbuffered file.
    CountedPtr<ByteIO> file_p;
    // The optional mapped file.
    MMapfdIO* mappedFile_p;
    // The optional buffered file.
    FilebufIO* bufferedFile_p;
    // The possibly used MultiFileBase.
    MultiFileBase* mfile_p;
	    

    // Forbid copy constructor.
    BucketFile (const BucketFile&);

    // Forbid assignment.
    BucketFile& operator= (const BucketFile&);

    // Create the mapped or buffered file object.
    void createMapBuf();

    // Delete the possible mapped or buffered file object.
    void deleteMapBuf();
};


inline const String& BucketFile::name() const
    { return name_p; }

inline Bool BucketFile::isWritable() const
    { return isWritable_p; }

inline void BucketFile::seek (Int offset)
    { seek (Int64(offset)); }

inline Bool BucketFile::isCached() const
    { return !isMapped_p && bufSize_p==0; }
inline Bool BucketFile::isMapped() const
    { return isMapped_p; }
inline Bool BucketFile::isBuffered() const
    { return bufSize_p>0; }


} //# NAMESPACE CASACORE - END

#endif
