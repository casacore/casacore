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

#ifndef MS_MSSPWINDEX_H
#define MS_MSSPWINDEX_H

//# includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/casa/Arrays/Matrix.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
  enum MSSpwTypes {MSSPW_INDEXRANGE=0,MSSPW_INDEX=2, MSSPW_UNITHZ=4, MSSPW_UNITVELOCITY=5};

  // Construct from an MS FIELD subtable
  MSSpwIndex(const MSSpectralWindow& msSpw);

  // Null destructor
  virtual ~MSSpwIndex() {};

  // Look up FIELD_ID's for a given field name, or set of field names
  Vector<int32_t> matchName(const String& name);
  Vector<int32_t> matchName(const Vector<String>& names);
  void matchNameAsIntID(Vector<int>& list);

  Vector<int32_t> matchFrequencyRange(const float f0,const float f1,bool approx, const float f3=0);

  // A version of match freq range that does not throw an exception but returns
  // false if no match...else spw, start, nchan returns the matches 
  // f0 and f1 are in Hz and the match is done in the frame defined in the 
  // SpectralWindow table.
  bool matchFrequencyRange(const double f0, const double f1, 
			   Vector<int32_t>& spw, Vector<int32_t>& start, 
			   Vector<int32_t>& nchan);
  // Look up FIELD_ID's for a given pattern/regex for source name/code
  Vector<int32_t> matchRegexOrPattern(const String& pattern,
				       const bool regex=false);
  // Look up FIELD_ID's for a given source id
  Vector<int32_t> matchId(const Vector<int32_t>& spwIds);

  Vector<int32_t> matchLT(const int32_t n);
  Vector<int32_t> matchGT(const int32_t n);
  Vector<int32_t> matchGTAndLT(const int32_t n0, const int n1);
  Vector<int32_t> matchLT(const float*);
  Vector<int32_t> matchGT(const float*);
  Vector<int32_t> matchGTAndLT(const float* phyValMin, const float *phyValMax);
  Vector<float> convertToMKS(const float f0, const float f1, const String& unit); 
  Vector<int32_t> convertToChannelIndex(const Vector<int32_t>& spw, const Vector<float>& freqList,
				    int32_t& nFSpec);
  Vector<int32_t> convertToSpwIndex(const Vector<float>& freqList,
				int32_t &nFSpec);
private:
  int32_t findChanIndex_p(const float& freq, const Vector<double>& chanFreqList,
		      const bool& greaterThan,
		      const bool& ascendingOrder);
  // Construct from an MS FIELD subtable
  MSSpwIndex();
  // FIELD subtable column accessor
  MSSpWindowColumns msSpwSubTable_p;
  //  MSDataDescColumns msDataDescSubTable_p;
  enum MODES {EXACT=1, APPROX, RANGE};
  Vector<int32_t> spwIDs;
};


} //# NAMESPACE CASACORE - END

#endif
    
