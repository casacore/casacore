//# MSFeedIndex.cc:  this defined MSFeedIndex
//# Copyright (C) 2000,2001,2002
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

#include <casacore/ms/MSSel/MSFeedIndex.h>

#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/ms/MeasurementSets/MSFeed.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MSSel/MSFeedParse.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSFeedIndex::MSFeedIndex() 
    : MSTableIndex(), msFeedCols_p(0)
{;}

MSFeedIndex::MSFeedIndex(const MSFeed &feed)
  : MSTableIndex(feed, stringToVector("ANTENNA_ID,FEED_ID,SPECTRAL_WINDOW_ID"),
		 compare),
    msFeedCols_p(0)
{ attachIds();}

MSFeedIndex::MSFeedIndex(const MSFeedIndex &other)
    : MSTableIndex(other), msFeedCols_p(0)
{ attachIds();}

MSFeedIndex::~MSFeedIndex()
{
  if (msFeedCols_p) delete(msFeedCols_p);
}

MSFeedIndex &MSFeedIndex::operator=(const MSFeedIndex &other)
{
    if (this != &other) {
	MSTableIndex::operator=(other);
	attachIds();
    }
    return *this;
}

void MSFeedIndex::attach(const MSFeed &feed)
{
    MSTableIndex::attach(feed, stringToVector("ANTENNA_ID,FEED_ID,SPECTRAL_WINDOW_ID"),
                         compare);
    attachIds();
}

void MSFeedIndex::attachIds()
{
    antennaId_p.attachToRecord(accessKey(), "ANTENNA_ID");
    feedId_p.attachToRecord(accessKey(), "FEED_ID");
    spwId_p.attachToRecord(accessKey(), "SPECTRAL_WINDOW_ID");

    // Attach the MSFeed columns accessor
    msFeedCols_p = new MSFeedColumns(static_cast<MSFeed&>(table()));
}

int32_t MSFeedIndex::compare (const Block<void*>& fieldPtrs,
                          const Block<void*>& dataPtrs,
                          const Block<int32_t>& dataTypes,
                          rownr_t index)
{
  // this implementation has been adapted from the default compare function in 
  // ColumnsIndex.cc.  The support for data types other than Integer have been
  // removed, since, according to the constructor's documentation, the index 
  // columns must be of integer type.  At present, this is in practice true in 
  // this case.   A consequence of this simplified implementation is that is 
  // supports a -1 value for all IDs, rather than just for SPECTRAL_WINDOW_ID;
  // since MS2 only allows a -1 value for SPECTRAL_WINDOW_ID, this should not
  // cause problems for users with valid MS2 datasets.
  uint32_t nfield = fieldPtrs.nelements();
  for (uint32_t i=0; i<nfield; i++) {
    if (dataTypes[i] == TpInt) {
      const int32_t left = *(*(RecordFieldPtr<int32_t>*)(fieldPtrs[i]));
      const int32_t right = ((const int32_t*)(dataPtrs[i]))[index];
      if (right != -1) {        // consider -1 equal to any requested id
          if (left < right) {
              return -1;
          } else if (left > right) {
              return 1;
          }
      }
    }
    else {
      throw (TableError ("MSFeedIndex: non-Integer index type"));
    }
  }
  return 0;
}

Vector<int32_t> MSFeedIndex::matchFeedPolznAndAngle (const int32_t& antennaId,
						 const Vector<String>& 
						 polznType,
						 const Vector<float>& 
						 receptorAngle,
						 const float& tol,
						 Vector<int32_t>& rowNumbers)
{
  // Return all matching row numbers for a given antenna id., and set
  // of feed receptor polarizations and receptor angles. The receptor
  // angles are matched to within the specified tolerance in deg.
  //
  // Do the receptor polarization match per row
  uint32_t nReceptors = std::min (polznType.nelements(), receptorAngle.nelements());
  uint32_t nrows = msFeedCols_p->nrow();
  Vector<bool> receptorMatch(nrows, false);
  for (uint32_t row=0; row<nrows; row++) {
    Vector<Quantity> rowAngle;
    msFeedCols_p->receptorAngleQuant().get(row, rowAngle);
    Vector<String> rowType;
    msFeedCols_p->polarizationType().get(row, rowType);
    receptorMatch(row) = (rowAngle.nelements() == nReceptors &&
			  rowType.nelements() == nReceptors);

    if (receptorMatch(row)) {
      for (uint32_t i=0; i<nReceptors; i++) {
	receptorMatch(row) = (receptorMatch(row) &&
			      nearAbs(Quantity(receptorAngle(i),"deg"), 
				      rowAngle(i), tol) &&
			      rowType(i)==polznType(i));
      }
    }
  }

  LogicalArray maskArray = (msFeedCols_p->antennaId().getColumn()==antennaId &&
			    receptorMatch);
  Vector<int32_t> rows(nrows);
  indgen(rows);
  MaskedArray<int32_t> maskRowNumbers(rows, maskArray);
  rowNumbers = maskRowNumbers.getCompressedArray();
  MaskedArray<int32_t> maskFeedIds(msFeedCols_p->feedId().getColumn(), maskArray);
  return maskFeedIds.getCompressedArray();
}


Vector<int32_t> MSFeedIndex::matchAntennaId (const int32_t& antennaId,
					 Vector<int32_t>& rowNumbers)
{
  // Return all matching row numbers for a given antenna id.
  // 
  LogicalArray maskArray = (msFeedCols_p->antennaId().getColumn()==antennaId);
  uint32_t nrows = msFeedCols_p->nrow();
  Vector<int32_t> rows(nrows);
  indgen(rows);
  MaskedArray<int32_t> maskRowNumbers(rows, maskArray);
  return maskRowNumbers.getCompressedArray();
  rowNumbers = maskRowNumbers.getCompressedArray();
  MaskedArray<int32_t> maskFeedIds(msFeedCols_p->feedId().getColumn(), maskArray);
  return maskFeedIds.getCompressedArray();
}

Vector<int32_t> MSFeedIndex::matchFeedId(const Vector<int32_t>& sourceId)
{
    Vector<int32_t> feedIds = msFeedCols_p->feedId().getColumn();
    Vector<int32_t> IDs = set_intersection(sourceId, feedIds);
    if (IDs.nelements() == 0)
      {
        ostringstream mesg;
        mesg << "No match found for requested feeds [ID(s): " << sourceId << "]";
        // Use the error handler if defined, otherwise throw.
        if (MSFeedParse::thisMSFErrorHandler) {
          MSFeedParse::thisMSFErrorHandler->reportError ("", mesg.str());
        } else {
          throw (MSSelectionFeedParseError(mesg));
        }
      }
    return IDs;
}

} //# NAMESPACE CASACORE - END

