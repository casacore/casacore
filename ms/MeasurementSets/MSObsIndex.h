//# MSObsIndex: index or lookup in an MS OBSERVATION subtable
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
//#
//# $Id$

#ifndef MS_MSOBSINDEX_H
#define MS_MSOBSINDEX_H

//# includes
#include <casa/aips.h>
#include <ms/MeasurementSets/MSObservation.h>
#include <ms/MeasurementSets/MSObsColumns.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# forward declarations

// <summary>
// Class to handle lookup or indexing into an MS OBSERVATION subtable
// </summary>

// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSObservation
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "OBSERVATION subtable" and "index".
// </etymology>
//
// <synopsis>
// This class provides lookup and indexing into an MS OBSERVATION
// subtable. These services include returning rows numbers (which 
// for the OBSERVATION subtable are OBSERVATION_ID's) associated 
// with specific data in the subtable.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect together all subtable indexing and lookup for the
// OBSERVATION subtable, for encapsulation and efficiency.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSObservationIndex 
{
public:
  // Construct from an MS OBSERVATION subtable
  MSObservationIndex(const MSObservation& observationTable);

  // Null destructor
  virtual ~MSObservationIndex() {};

  // Look up OBSERVATION_ID's for a given project code
  Vector<Int> matchProjectCode(const String& projectCode);

private:
  // Disallow null constructor
  MSObservationIndex();

  // OBSERVATION subtable column accessor
  ROMSObservationColumns msObservationCols_p;

  // Vector cache of observation id's
  Vector<Int> observationIds_p;
  Int nrows_p;
};


} //# NAMESPACE CASA - END

#endif
    
