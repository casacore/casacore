//# MSSpWindowIndex: index/lookup in a MeasurementSet SPECTRAL_WINDOW subtable
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

#ifndef MS_MSSPWINDOWINDEX_H
#define MS_MSSPWINDOWINDEX_H

//# includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MFrequency.h>
#include <casacore/ms/MeasurementSets/MSSpectralWindow.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward declarations

// <summary>
// Class to handle lookup or indexing into a MS SPECTRAL_WINDOW subtable
// </summary>

// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSSpWindowription
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "SPECTRAL_WINDOW subtable" and "index".
// </etymology>
//
// <synopsis>
// This class provides lookup and indexing into an MS SPECTRAL_WINDOW
// subtable. These services include returning rows numbers
// (which for the SPECTRAL_WINDOW subtable are SPECTRAL_WINDOW_ID's) 
// associated with specific data in the subtable.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect together all subtable indexing and lookup for the
// SPECTRAL_WINDOW subtable, for encapsulation and efficiency.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//

class MSSpWindowIndex 
{
public:
  // Construct from an MS SPECTRAL_WINDOW subtable
  MSSpWindowIndex(const MSSpectralWindow& spectralWindow);

  // Null destructor
  virtual ~MSSpWindowIndex() {}

  // Look up SPECTRAL_WINDOW_ID's for a given frequency group or groups
  Vector<Int> matchFreqGrp(const Int& freqGrp);
  Vector<Int> matchFreqGrp(const Vector<Int>& freqGrps);

  // Look up SPECTRAL_WINDOW_ID's for a given frequency group name
  Vector<Int> matchFreqGrpName(const String& freqGrpName);

  // Look up SPECTRAL_WINDOW_ID's for a given frequency axis sampling
  Vector<Int> matchFreq(const Vector<MFrequency>& chanFreq,
			const Vector<MVFrequency>& chanWidth,
			const Double& freqTol);

private:
  // Disallow null constructor
  MSSpWindowIndex();

  // SPECTRAL_WINDOW subtable column accessor
  ROMSSpWindowColumns msSpWindowCols_p;

  // Vector cache of SpWindow id's
  Vector<Int> spWindowIds_p;
  Int nrows_p;
};


} //# NAMESPACE CASACORE - END

#endif
    
