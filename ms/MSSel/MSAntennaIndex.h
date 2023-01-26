//# MSAntennaIndex: index or lookup in a MeasurementSet ANTENNA subtable
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

#ifndef MS_MSANTENNAINDEX_H
#define MS_MSANTENNAINDEX_H

//# includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MSAntenna.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations

// <summary>
// Class to handle lookup or indexing into a MS ANTENNA subtable
// </summary>

// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSAntenna
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "ANTENNA subtable" and "index".
// </etymology>
//
// <synopsis>
// This class provides lookup and indexing into an MS ANTENNA
// subtable. These services include returning rows numbers
// (which for the ANTENNA subtable are ANTENNA_ID's) associated 
// with specific data in the subtable.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect together all subtable indexing and lookup for the
// ANTENNA subtable, for encapsulation and efficiency.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSAntennaIndex 
{
public:
  // Construct from an MS ANTENNA subtable
  MSAntennaIndex(const MSAntenna &antenna);

  // Null destructor
  virtual ~MSAntennaIndex() {}

  // Look up ANTENNA_ID's for a given a regular expression or pattern
  Vector<int32_t> matchAntennaRegexOrPattern(const String& pattern, const bool regex=false);
  // Look up ANTENNA_ID's for a given antenna name, or set of antenna names
  Vector<int32_t> matchAntennaName(const String& name);
  Vector<int32_t> matchAntennaName(const Vector<String>& names);

  // Look up ANTENNA_ID's for a given antenna station
  Vector<int32_t> matchStationRegexOrPattern(const String& pattern, const bool regex=false);
  Vector<int32_t> matchStationName(const String& station);
  Vector<int32_t> matchStationName(const Vector<String>& station);

  // Look up ANTENNA_ID's for a given antenna and station name pair
  Vector<int32_t> matchAntennaNameAndStation(const String& name,
					 const String& station);

  Vector<int32_t> matchId(const Vector<int32_t>& sourceId);
private:
  // Default constructor
  MSAntennaIndex();
  // ANTENNA subtable column accessor
  MSAntennaColumns msAntennaCols_p;

  // Vector cache of antenna id's
  Vector<int32_t> antennaIds_p, stationIds_p;
  int32_t nrows_p;
};


} //# NAMESPACE CASACORE - END

#endif
    
