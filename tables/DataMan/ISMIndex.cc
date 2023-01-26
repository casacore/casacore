//# ISMIndex.cc: The Index of the Incremental Storage Manager
//# Copyright (C) 1996,1997,1998
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

//# Includes
#include <casacore/tables/DataMan/ISMIndex.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

ISMIndex::ISMIndex (ISMBase* parent)
: stmanPtr_p (parent),
  nused_p    (1),
  rows_p     (2, 0),
  bucketNr_p (1, 0)
{}

ISMIndex::~ISMIndex()
{}

void ISMIndex::get (AipsIO& os)
{
    uint32_t version = os.getstart ("ISMIndex");
    os >> nused_p;
    if (version > 1) {
      // stored as 64-bit
      getBlock (os, rows_p);
    } else {
      // stored as 32-bit
      Block<uint32_t> rows;
      getBlock (os, rows);
      rows_p.resize (rows.size());
      std::copy (rows.begin(), rows.end(), rows_p.begin());
    }
    getBlock (os, bucketNr_p);
    os.getend();
}

void ISMIndex::put (AipsIO& os)
{
    // If the last rownr fits in 32-bit, write as version 1 (thus 32-bit).
    uint32_t version = (rows_p[nused_p] > DataManager::MAXROWNR32  ?  2 : 1);
    os.putstart ("ISMIndex", version);
    os << nused_p;
    if (version > 1) {
      putBlock (os, rows_p, nused_p + 1);
    } else {
      Block<uint32_t> rows(nused_p +1);
      std::copy (rows_p.begin(), rows_p.begin() + nused_p + 1, rows.begin());
      putBlock (os, rows, nused_p + 1);
    }
    putBlock (os, bucketNr_p, nused_p);
    os.putend();
}

void ISMIndex::addBucketNr (rownr_t rownr, uint32_t bucketNr)
{
    if (nused_p >= bucketNr_p.nelements()) {
	rows_p.resize (nused_p + 64 + 1);
	bucketNr_p.resize (nused_p + 64);
    }
    bool found;
    uint32_t index = binarySearchBrackets (found, rows_p, rownr, nused_p);
    AlwaysAssert (!found, AipsError);
    objmove (&rows_p[index+1], &rows_p[index], nused_p + 1 - index);
    if (nused_p > index) {
	objmove (&bucketNr_p[index+1], &bucketNr_p[index], nused_p - index);
    }
    rows_p[index] = rownr;
    bucketNr_p[index] = bucketNr;
    nused_p++;
}

void ISMIndex::addRow (rownr_t nrrow)
{
    rows_p[nused_p] += nrrow;
}

int32_t ISMIndex::removeRow (rownr_t rownr)
{
    // Decrement the row number for all intervals after the row
    // to be removed.
    uint32_t index = getIndex (rownr);
    for (uint32_t i=index+1; i<=nused_p; i++) {
	rows_p[i]--;
    }
    // Remove the entire bucket when no row is left.
    int32_t emptyBucket = -1;
    if (rows_p[index] == rows_p[index+1]) {
	emptyBucket = bucketNr_p[index];
	if (nused_p > index+1) {
	    objmove (&rows_p[index+1], &rows_p[index+2], nused_p - index - 1);
	    objmove (&bucketNr_p[index], &bucketNr_p[index+1],
		     nused_p - index - 1);
	}
	rows_p[nused_p] = 0;
	// There should always be one interval.
	if (nused_p > 1) {
	    nused_p--;
	}
    }
    return emptyBucket;
}

uint32_t ISMIndex::getIndex (rownr_t rownr) const
{
    // If no exact match, the interval starts at the previous index.
    bool found;
    uint32_t index = binarySearchBrackets (found, rows_p, rownr, (uint32_t)nused_p+1);
    if (!found) {
	index--;
    }
    AlwaysAssert (index <= nused_p, AipsError);
    return index;
}

uint32_t ISMIndex::getBucketNr (rownr_t rownr, rownr_t& bucketStartRow,
			    rownr_t& bucketNrrow) const
{
    uint32_t index = getIndex (rownr);
    bucketStartRow = rows_p[index];
    bucketNrrow    = rows_p[index+1] - bucketStartRow;
    return bucketNr_p[index];
}

bool ISMIndex::nextBucketNr (uint32_t& cursor, rownr_t& bucketStartRow,
			     rownr_t& bucketNrrow, uint32_t& bucketNr) const
{
    // When first time, get the index of the bucket containing the row.
    // End the iteration when the first row is past the end.
    if (cursor == 0) {
	if (bucketStartRow >= rows_p[nused_p]) {
	    return false;
	}
	cursor = getIndex (bucketStartRow);
    }else{
	// Not the first time.
	// End the iteration when no more buckets.
	if (cursor >= nused_p) {
	    return false;
	}
    }
    bucketStartRow = rows_p[cursor];
    bucketNrrow    = rows_p[cursor+1] - bucketStartRow;
    bucketNr       = bucketNr_p[cursor++];
    return true;
}

void ISMIndex::show (ostream& os) const
{
    os << "ISMIndex " << nused_p << " strow:bucket";
    for (uint32_t i=0; i<nused_p; ++i) {
      cout << ' ' << rows_p[i] << ':' << bucketNr_p[i];
    }
    cout << endl;
}

} //# NAMESPACE CASACORE - END

