//# MSStateIndex: index or lookup in a MeasurementSet FIELD subtable
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

#ifndef MS_MSSTATEINDEX_H
#define MS_MSSTATEINDEX_H

//# includes
#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MSState.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MeasurementSets/MSStateColumns.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations

// <summary>
// Class to handle lookup or indexing into a MS FIELD subtable
// </summary>

// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSState
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "FIELD subtable" and "index".
// </etymology>
//
// <synopsis>
// This class provides lookup and indexing into an MS FIELD
// subtable. These services include returning rows numbers
// (which for the FIELD subtable are FIELD_ID's) associated 
// with specific data in the subtable.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect together all subtable indexing and lookup for the
// FIELD subtable, for encapsulation and efficiency.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSStateIndex 
{
public:
  // Construct from an MS FIELD subtable
  MSStateIndex(const MSState &state);

  // Null destructor
  virtual ~MSStateIndex() {}

  Vector<int32_t> matchStateIntent(const String& name);
  Vector<int32_t> matchStateIntent(const Vector<String>& names);

  //ADD for file name wildcard selection
  Vector<int32_t> matchStateObsMode(const String& name);
  Vector<int32_t> matchStateObsMode(const Vector<String>& names);

  // Look up FIELD_ID's for a given pattern/regex for source name/code
  Vector<int32_t> matchStateRegexOrPattern(const String& pattern,
				       const bool regex=false);
  Vector<int32_t> matchStateObsModeRegexOrPattern(const String& pattern,
					   const bool regex=false);
  // Look up FIELD_ID's for a given source id
  Vector<int32_t> matchStateId(const int32_t& sourceId);
  Vector<int32_t> matchStateId(const Vector<int32_t>& sourceIds);

  Vector<int32_t> maskStateIDs(const Vector<int32_t>& ids);

  Vector<int32_t> matchStateIDLT(const int32_t n);
  Vector<int32_t> matchStateIDGT(const int32_t n);
  Vector<int32_t> matchStateIDGTAndLT(const int32_t n0, const int n1);
private:
  // Disallow null constructor
  MSStateIndex();

  // FIELD subtable column accessor
  MSStateColumns msStateCols_p;

  // Vector cache of field id's
  Vector<int32_t> stateIds_p;
  int32_t nrows_p;
  int32_t matchAnyRegex(const Vector<String>& strList, const Regex& regex, const int32_t pos=0);
};


} //# NAMESPACE CASACORE - END

#endif
    
