//# MSObsIndex.cc: implementation of MSObsIndex.h
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

#include <casacore/ms/MSSel/MSObsIndex.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//-------------------------------------------------------------------------

MSObservationIndex::MSObservationIndex(const MSObservation& observationTable)
  : msObservationCols_p(observationTable)
{ 
// Construct from an MS OBSERVATION subtable
// Input:
//    observationTable      const MSObservation&    Input MSObservation 
//                                                  sub-table
// Output to private data:
//    msObservationCols_p   ROMSObservationColumns  MSObservation columns 
//                                                  accessor
//    observationIds_p      Vector<Int>             Observation id.'s
//    nrows_p               Int                     Number of rows
//
  // Generate an array of observation id's, used in later queries
  nrows_p = msObservationCols_p.nrow();
  observationIds_p.resize(nrows_p);
  indgen(observationIds_p);
}

//-------------------------------------------------------------------------

Vector<Int> MSObservationIndex::matchProjectCode(const String& projectCode)
{
// Match a project code
// Input:
//    projectCode        const String&            Project code
// Output:
//    matchProjectCode   Vector<Int>              Matching observation id.'s
//

  // Match the project code
  // by row and correlation index
  LogicalArray maskArray(msObservationCols_p.project().getColumn() == 
			 projectCode);
  MaskedArray<Int> maskObsIds(observationIds_p, maskArray);
  return maskObsIds.getCompressedArray();
}

//-------------------------------------------------------------------------



} //# NAMESPACE CASACORE - END

