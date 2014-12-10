//# ISMIndex.h: The Index of the Incremental Storage Manager
//# Copyright (C) 1996,1997,1999,2005
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

#ifndef TABLES_ISMINDEX_H
#define TABLES_ISMINDEX_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class ISMBase;
class AipsIO;


// <summary>
// The Index of the Incremental Storage Manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=ISMBase>ISMBase</linkto>
// </prerequisite>

// <etymology>
// ISMIndex represents the index in the Incremental Storage Manager.
// </etymology>

// <synopsis>
// ISMIndex maintains an index of all buckets in an ISM (Incremental Storage
// Manager). The index consists of the starting row number and the
// bucket number of each bucket in the BucketCache object of the ISM.
// When the ISM is opened, the entire index is read in and kept in memory.
// When the ISM is closed or flushed, the index is written back after
// all buckets in the file. A little header at the beginning of the file
// indicates the starting offset of the index.
// </synopsis> 

// <motivation>
// ISMIndex encapsulates all operations on the ISM index.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class ISMIndex
{
public:
    // Create a ISMIndex object with the given parent for a new table.
    // It keeps the pointer to its parent (but does not own it).
    explicit ISMIndex (ISMBase* parent);

    // The destructor closes the file (if opened).
    ~ISMIndex();

    // Add a row.
    void addRow (uInt nrrow);

    // Remove a row from the index.
    // If the result of this is that the entire bucket gets empty,
    // that bucketnr is returned. Otherwise -1 is returned.
    Int removeRow (uInt rownr);

    // Get the bucket number for the given row.
    // Also return the start row of the bucket and the number of rows in it.
    uInt getBucketNr (uInt rownr, uInt& bucketStartRow,
		      uInt& bucketNrrow) const;

    // Read the bucket index from the AipsIO object.
    void get (AipsIO& os);

    // Write the bucket index into the AipsIO object.
    void put (AipsIO& os);

    // Add a bucket number to the index.
    // Argument <src>rownr</src> gives the starting row of the bucket.
    // It is used to add the bucket number at the correct place
    // (such that the row numbers are kept in ascending order).
    void addBucketNr (uInt rownr, uInt bucketNr);

    // Get the number of the next bucket from the index and return
    // it in <src>bucketNr</src>. The starting row of that bucket and
    // the number of rows in the bucket are also returned.
    // Return status False indicates that no more buckets are available.
    // <br>The start of the iteration is indicated by cursor=0.
    // The first bucket returned is the bucket containing the rownr
    // given in <src>bucketStartRow</src> (thus set bucketStartRow
    // to 0 if you to start at the first bucket).
    // <br>The next iterations return the next bucket number and fill
    // the starting row and number of rows.
    Bool nextBucketNr (uInt& cursor, uInt& bucketStartRow, uInt& bucketNrrow,
		       uInt& bucketNr) const;

    // Show the index.
    void show (std::ostream&) const;

private:
    // Forbid copy constructor.
    ISMIndex (const ISMIndex&);

    // Forbid assignment.
    ISMIndex& operator= (const ISMIndex&);

    // Get the index of the bucket containing the given row.
    uInt getIndex (uInt rownr) const;


    //# Declare member variables.
    // Pointer to the parent storage manager.
    ISMBase*          stmanPtr_p;
    // Number of entries used.
    uInt              nused_p;
    // Rownr index (i.e. row rows_p[i] starts in bucketNr_p[i]).
    Block<uInt>       rows_p;
    // Corresponding bucket number.
    Block<uInt>       bucketNr_p;
};




} //# NAMESPACE CASACORE - END

#endif
