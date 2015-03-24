//# TSMFile.h: File object for Tiled Storage Manager
//# Copyright (C) 1995,1996,1997,1999,2001
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

#ifndef TABLES_TSMFILE_H
#define TABLES_TSMFILE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/BucketFile.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TSMOption;
class TiledStMan;
class MultiFileBase;
class AipsIO;

// <summary>
// File object for Tiled Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TiledStMan>TiledStMan</linkto>
// </prerequisite>

// <etymology>
// TSMFile represents a data file for the Tiled Storage Manager.
// </etymology>

// <synopsis>
// A TSMFile object represents a data file. Currently it is meant
// for the TiledStMan classes, but it can easily be turned into
// a more general storage manager file class.
// <br>
// Creation of a TSMFile object does not open the file.
// An explicit open call has to be given before the file can be used.
// <p>
// Underneath it uses a BucketFile to access the file.
// In this way the IO details are well encapsulated.
// </synopsis> 

// <motivation>
// Encapsulate the Tiled Storage Manager file details.
// </motivation>

//# <todo asof="$DATE:$">
//# </todo>


class TSMFile
{
public:
    // Create a TSMFile object (with corresponding file).
    // The sequence number gets part of the file name.
    TSMFile (const TiledStMan* stMan, uInt fileSequenceNr,
             const TSMOption&, MultiFileBase* mfile=0);

    // Create a TSMFile object for the given existing file.
    TSMFile (const String& fileName, Bool writable, const TSMOption&,
             MultiFileBase* mfile=0);

    // Read the object back.
    // The file is not opened until the first access,
    // thus until the file descriptor is asked for the first time.
    // It checks if the sequence number matches the expected one.
    TSMFile (const TiledStMan* stMan, AipsIO& ios, uInt seqnr,
             const TSMOption&, MultiFileBase* mfile=0);

    // The destructor closes the file.
    ~TSMFile();

    // Write the object.
    void putObject (AipsIO& ios) const;

    // Get the object.
    void getObject (AipsIO& ios);

    // Open the file if not open yet.
    void open();

    // Return the BucketFile object (to be used in the BucketCache).
    BucketFile* bucketFile();

    // Return the logical file length.
    Int64 length() const;

    // Return the file sequence number.
    uInt sequenceNumber() const;

    // Increment the logical file length.
    void extend (Int64 increment);


private:
    // The file sequence number.
    uInt fileSeqnr_p;
    // The file object.
    BucketFile* file_p;
    // The (logical) length of the file.
    Int64 length_p;
	    

    // Forbid copy constructor.
    TSMFile (const TSMFile&);

    // Forbid assignment.
    TSMFile& operator= (const TSMFile&);
};


inline Int64 TSMFile::length() const
    { return length_p; }

inline uInt TSMFile::sequenceNumber() const
    { return fileSeqnr_p; }

inline void TSMFile::extend (Int64 increment)
    { length_p += increment; }

inline BucketFile* TSMFile::bucketFile()
    { return file_p; }

inline void TSMFile::open()
    { file_p->open(); }



} //# NAMESPACE CASACORE - END

#endif
