//# MSSourceIndex.cc:  this defined MSSourceIndex
//# Copyright (C) 2000,2001
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

#include <trial/MeasurementSets/MSSourceIndex.h>

#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/MeasurementSets/MSSource.h>

MSSourceIndex::MSSourceIndex() 
  : MSTableIndex(), msSourceCols_p(0)
{;}

MSSourceIndex::MSSourceIndex(const MSSource &source)
  : MSTableIndex(source, stringToVector("SOURCE_ID,SPECTRAL_WINDOW_ID"))
{ 
  attachIds();
  msSourceCols_p = new ROMSSourceColumns(source);
}

MSSourceIndex::MSSourceIndex(const MSSourceIndex &other)
    : MSTableIndex(other)
{ attachIds();}

MSSourceIndex::~MSSourceIndex()
{
  if (msSourceCols_p) delete msSourceCols_p;
}

MSSourceIndex &MSSourceIndex::operator=(const MSSourceIndex &other)
{
    if (this != &other) {
	MSTableIndex::operator=(other);
	attachIds();
    }
    return *this;
}

void MSSourceIndex::attach(const MSSource &source)
{
    MSTableIndex::attach(source, stringToVector("SOURCE_ID,SPECTRAL_WINDOW_ID"));
    attachIds();
}

void MSSourceIndex::attachIds()
{
    sourceId_p.attachToRecord(accessKey(), "SOURCE_ID");
    spwId_p.attachToRecord(accessKey(), "SPECTRAL_WINDOW_ID");
}

Vector<Int> MSSourceIndex::matchSourceName(const String& name)
{
// Match a source name to a set of source id's
// Input:
//    name             const String&            Source name to match
// Output:
//    matchSourceName  Vector<Int>              Matching source id's
//
  Vector<Int> retval;
  if (!msSourceCols_p->isNull() && msSourceCols_p->nrow() > 0) {
    LogicalArray maskArray = (msSourceCols_p->name().getColumn()==name);
    MaskedArray<Int> maskSourceId(msSourceCols_p->sourceId().getColumn(), 
				  maskArray);
    retval = maskSourceId.getCompressedArray();
  };
  return retval;
}; 

Vector<Int> MSSourceIndex::matchSourceName(const Vector<String>& names)
{
// Match a set of source names to a set of source id's
// Input:
//    names            const Vector<String>&    Source names to match
// Output:
//    matchSourceNames Vector<Int>              Matching source id's
//
  Vector<Int> matchedSourceIds;
  // Match each source name individually
  for (Int fld=0; fld<names.nelements(); fld++) {
    // Add to list of source id's
    Vector<Int> currentMatch = matchSourceName(names(fld));
    if (currentMatch.nelements() > 0) {
      Vector<Int> temp(matchedSourceIds);
      matchedSourceIds.resize(matchedSourceIds.nelements() +
			     currentMatch.nelements(), True);
      matchedSourceIds = concatenateArray(temp, currentMatch);
    };
  };
  return matchedSourceIds;
};






