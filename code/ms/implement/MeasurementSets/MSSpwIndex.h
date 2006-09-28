//# MSSpwIndex: index or lookup in a MeasurementSet subtable (DDID and SPW)
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

#ifndef MS_MSSPWINDEX_H
#define MS_MSSPWINDEX_H

//# includes
#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
#include <ms/MeasurementSets/MSColumns.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# forward declarations

// <summary> 
// Class to handle lookup or indexing into a MS Data_Desc_ID
// and SpectralWindow subtables 
// </summary>

// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "SpectralWindwo subtable" and "index".
// </etymology>
//
// <synopsis> 
//
// This class provides lookup and two level indexing into an MS
// DataDescription and SpectralWindow subtable. These services include
// returning list of integer data description IDs (DDID), given a list
// of Spectral window IDs or frequencies.  The DDIDs are then used for
// selecting rows from the MS (since DDIDs are the primary keys in the
// maintable - not the spectral window IDs).
//
// </synopsis>
//
// <example>
// </example>
//
// <motivation> 
// Collect together all subtable indexing and lookup for the data
// description and spectral window subtable, for encapsulation and
// efficiency.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSSpwIndex 
{
public:
  // Construct from an MS FIELD subtable
  MSSpwIndex(const MSSpectralWindow& msSpw);

  // Null destructor
  virtual ~MSSpwIndex() {};

  // Look up FIELD_ID's for a given field name, or set of field names
  Vector<Int> matchName(const String& name);
  Vector<Int> matchName(const Vector<String>& names);

  Vector<Int> matchFrequencyRange(const Float f0,const Float f1,Bool approx);

  // Look up FIELD_ID's for a given pattern/regex for source name/code
  Vector<Int> matchRegexOrPattern(const String& pattern,
				       const Bool regex=False);
  // Look up FIELD_ID's for a given source id
  Vector<Int> matchId(const Vector<Int>& spwIds);

  Vector<Int> matchIDLT(const Int n);
  Vector<Int> matchIDGT(const Int n);
  Vector<Int> matchIDGTAndLT(const Int n0, const int n1);
  Vector<Float> convertToHz(const Float f0, const Float f1, const String& unit); 
private:
  // Construct from an MS FIELD subtable
  MSSpwIndex();
  // FIELD subtable column accessor
  ROMSSpWindowColumns msSpwSubTable_p;
  //  ROMSDataDescColumns msDataDescSubTable_p;
  enum MODES {EXACT=1, APPROX, RANGE};
  Vector<Int> spwIDs;
};


} //# NAMESPACE CASA - END

#endif
    
