//# MSFieldIndex.cc: implementation of MSFieldIndex.h
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
//#
//# $Id$

#include <ms/MeasurementSets/MSFieldIndex.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//-------------------------------------------------------------------------

MSFieldIndex::MSFieldIndex(const MSField& field)
  : msFieldCols_p(field)
{ 
// Construct from an MS FIELD subtable
// Input:
//    field           const MSField&           Input MSField object
// Output to private data:
//    msFieldCols_p   ROMSFieldColumns         MSField columns accessor
//    fieldIds_p      Vector<Int>              Field id's
//    nrows_p         Int                      Number of rows
//
  // Generate an array of field id's, used in later queries
  nrows_p = msFieldCols_p.nrow();
  fieldIds_p.resize(nrows_p);
  indgen(fieldIds_p);
};

//-------------------------------------------------------------------------

Vector<Int> MSFieldIndex::matchFieldName(const String& name)
{
// Match a field name to a set of field id's
// Input:
//    name             const String&            Field name to match
// Output:
//    matchFieldName   Vector<Int>              Matching field id's
//
  LogicalArray maskArray = (msFieldCols_p.name().getColumn()==name &&
			    !msFieldCols_p.flagRow().getColumn());
  MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
  return maskFieldId.getCompressedArray();
}; 

//-------------------------------------------------------------------------

Vector<Int> MSFieldIndex::matchFieldName(const Vector<String>& names)
{
// Match a set of field names to a set of field id's
// Input:
//    names            const Vector<String>&    Field names to match
// Output:
//    matchFieldNames  Vector<Int>              Matching field id's
//
  Vector<Int> matchedFieldIds;
  // Match each field name individually
  for (uInt fld=0; fld<names.nelements(); fld++) {
    // Add to list of field id's
    Vector<Int> currentMatch = matchFieldName(names(fld));
    if (currentMatch.nelements() > 0) {
      Vector<Int> temp(matchedFieldIds);
      matchedFieldIds.resize(matchedFieldIds.nelements() +
			     currentMatch.nelements(), True);
      matchedFieldIds = concatenateArray(temp, currentMatch);
    };
  };
  return matchedFieldIds;
};

//-------------------------------------------------------------------------

Vector<Int> MSFieldIndex::matchSourceId(const Int& sourceId)
{
// Match a source id to a set of field id's
// Input:
//    sourceId        const Int&               Source id to match
// Output:
//    matchSourceId   Vector<Int>              Matching field id's
//
  LogicalArray maskArray = 
    (msFieldCols_p.sourceId().getColumn()==sourceId &&
     !msFieldCols_p.flagRow().getColumn());
  MaskedArray<Int> maskFieldId(fieldIds_p, maskArray);
  return maskFieldId.getCompressedArray();
}; 

//-------------------------------------------------------------------------

Vector<Int> MSFieldIndex::matchSourceId(const Vector<Int>& sourceIds)
{
// Match a set of source id's to a set of field id's
// Input:
//    sourceIds       const Vector<Int>&       Source id's to match
// Output:
//    matchSourceIds  Vector<Int>              Matching field id's
//
  Vector<Int> matchedFieldIds;
  // Match each field name individually
  for (uInt fld=0; fld<sourceIds.nelements(); fld++) {
    // Add to list of field id's
    Vector<Int> currentMatch = matchSourceId(sourceIds(fld));
    if (currentMatch.nelements() > 0) {
      Vector<Int> temp(matchedFieldIds);
      matchedFieldIds.resize(matchedFieldIds.nelements() +
			     currentMatch.nelements(), True);
      matchedFieldIds = concatenateArray(temp, currentMatch);
    };
  };
  return matchedFieldIds;
};

//-------------------------------------------------------------------------



} //# NAMESPACE CASA - END

