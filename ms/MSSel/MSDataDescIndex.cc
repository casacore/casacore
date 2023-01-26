//# MSDataDescIndex.cc: implementation of MSDataDescIndex.h
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

#include <casacore/ms/MSSel/MSDataDescIndex.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//-------------------------------------------------------------------------

MSDataDescIndex::MSDataDescIndex(const MSDataDescription& dataDescription)
  : msDataDescCols_p(dataDescription)
{ 
// Construct from an MS DATA_DESC subtable
// Input:
//    dataDescription    const MSDataDescription&   Input MSDataDescription
//                                                  sub-table
// Output to private data:
//    msDataDescCols_p   MSDataDescColumns        MSDataDesc columns accessor
//    dataDescIds_p      Vector<int32_t>                Data desc id's
//    nrows_p            int32_t                        Number of rows
//
  // Generate an array of data desc id's, used in later queries
  nrows_p = msDataDescCols_p.nrow();
  dataDescIds_p.resize(nrows_p);
  indgen(dataDescIds_p);
}

//-------------------------------------------------------------------------

Vector<int32_t> MSDataDescIndex::matchSpwId(const int32_t& spwId)
{
// Match a spectral window id to a set of data desc id's
// Input:
//    spwId               const int32_t&               Spw id to match
// Output:
//    matchSpwId          Vector<int32_t>              Matching data desc id's
//
  LogicalArray maskArray = 
    (msDataDescCols_p.spectralWindowId().getColumn()==spwId &&
     !msDataDescCols_p.flagRow().getColumn());
  MaskedArray<int32_t> maskDataDescId(dataDescIds_p, maskArray);
  return maskDataDescId.getCompressedArray();
} 

//-------------------------------------------------------------------------

Vector<int32_t> MSDataDescIndex::matchSpwId(const Vector<int32_t>& spwIds)
{
// Match a set of spectral window id's to a set of data desc id's
// Input:
//    spwIds              const Vector<int32_t>&       Spw id's to match
// Output:
//    matchSpwId          Vector<int32_t>              Matching data desc id's
//
  Vector<int32_t> matchedDataDescIds;
  // Match each spw id individually
  for (uint32_t spwid=0; spwid<spwIds.nelements(); spwid++) {
    // Add to list of datadesc id's
    Vector<int32_t> currentMatch = matchSpwId(spwIds(spwid));
    if (currentMatch.nelements() > 0) {
      Vector<int32_t> temp(matchedDataDescIds);
      matchedDataDescIds.resize(matchedDataDescIds.nelements() +
				currentMatch.nelements(), true);
      matchedDataDescIds = concatenateArray(temp, currentMatch);
    }
  }
  return matchedDataDescIds;
}

//-------------------------------------------------------------------------

Vector<int32_t> MSDataDescIndex::matchPolId(const int32_t& polId)
{
// Match a polarization id to a set of data desc id's
// Input:
//    polId               const int32_t&               pol id to match
// Output:
//    matchPolId          Vector<int32_t>              Matching data desc id's
//
  LogicalArray maskArray = 
    (msDataDescCols_p.polarizationId().getColumn()==polId &&
     !msDataDescCols_p.flagRow().getColumn());
  MaskedArray<int32_t> maskDataDescId(dataDescIds_p, maskArray);
  return maskDataDescId.getCompressedArray();
} 

//-------------------------------------------------------------------------

Vector<int32_t> MSDataDescIndex::matchPolId(const Vector<int32_t>& polIds)
{
// Match a set of polarization id's to a set of data desc id's
// Input:
//    polIds              const Vector<int32_t>&       pol id's to match
// Output:
//    matchPolId          Vector<int32_t>              Matching data desc id's
//
  Vector<int32_t> matchedDataDescIds;
  // Match each pol id individually
  for (uint32_t polid=0; polid < polIds.nelements(); polid++) {
    // Add to list of datadesc id's
    Vector<int32_t> currentMatch = matchPolId(polIds(polid));
    if (currentMatch.nelements() > 0) {
      Vector<int32_t> temp(matchedDataDescIds);
      matchedDataDescIds.resize(matchedDataDescIds.nelements() +
				currentMatch.nelements(), true);
      matchedDataDescIds = concatenateArray(temp, currentMatch);
    }
  }
  return matchedDataDescIds;
}

//-------------------------------------------------------------------------

Vector<int32_t> MSDataDescIndex::matchSpwIdAndPolznId(const int32_t& spwId,
						  const int32_t& polznId)
{
// Match a spw. id. and polzn. id. to a set of data desc id.'s
// Input:
//    spwId                  const int32_t&            Spw id. to match
//    polznId                const int32_t&            Polzn. id. to match
// Output:
//    matchSpwIdAndPolznId   Vector<int32_t>           Matching data desc id's
//
  LogicalArray maskArray = 
    (msDataDescCols_p.spectralWindowId().getColumn()==spwId &&
     msDataDescCols_p.polarizationId().getColumn()==polznId &&
     !msDataDescCols_p.flagRow().getColumn());
  MaskedArray<int32_t> maskDataDescId(dataDescIds_p, maskArray);
  return maskDataDescId.getCompressedArray();
} 

//-------------------------------------------------------------------------




} //# NAMESPACE CASACORE - END

