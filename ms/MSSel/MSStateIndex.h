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
//#
//#
//# $Id$

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

  Vector<Int> matchStateIntent(const String& name);
  Vector<Int> matchStateIntent(const Vector<String>& names);

  //ADD for file name wildcard selection
  Vector<Int> matchStateObsMode(const String& name);
  Vector<Int> matchStateObsMode(const Vector<String>& names);

  // Look up FIELD_ID's for a given pattern/regex for source name/code
  Vector<Int> matchStateRegexOrPattern(const String& pattern,
				       const Bool regex=False);
  Vector<Int> matchStateObsModeRegexOrPattern(const String& pattern,
					   const Bool regex=False);
  // Look up FIELD_ID's for a given source id
  Vector<Int> matchStateId(const Int& sourceId);
  Vector<Int> matchStateId(const Vector<Int>& sourceIds);

  Vector<Int> maskStateIDs(const Vector<Int>& ids);

  Vector<Int> matchStateIDLT(const Int n);
  Vector<Int> matchStateIDGT(const Int n);
  Vector<Int> matchStateIDGTAndLT(const Int n0, const int n1);
private:
  // Disallow null constructor
  MSStateIndex();

  // FIELD subtable column accessor
  ROMSStateColumns msStateCols_p;

  // Vector cache of field id's
  Vector<Int> stateIds_p;
  Int nrows_p;
  Int matchAnyRegex(const Vector<String>& strList, const Regex& regex, const Int pos=0);
};


} //# NAMESPACE CASACORE - END

#endif
    
