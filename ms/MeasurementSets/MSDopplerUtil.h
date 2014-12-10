//# MSDopplerUtil.h: utility class for MS Doppler tracking information
//# Copyright (C) 2000,2003
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

#ifndef MS_MSDOPPLERUTIL_H
#define MS_MSDOPPLERUTIL_H

#include <casacore/casa/aips.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A utility class for MS Doppler tracking information
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MeasurementSet
//   <li> MSDoppler
// </prerequisite>
//
// <etymology>
// From "MeasurementSet", "Doppler" and "utility".
// </etymology>
//
// <synopsis>
// This class provides utilities for handling Doppler tracking
// information in a MeasurementSet. 
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect all utilities for handling MS Doppler tracking information.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="2000/09/01">
//   <li> averaging over other indices.
// </todo>

class MSDopplerUtil
{
public:
  // Construct from an existing MeasurementSet
  MSDopplerUtil (const MeasurementSet& ms);

  // Null destructor
  ~MSDopplerUtil();

  // Retrieve a list of all rest frequencies used for a given 
  // spectral window id and field id.
  // Note that the output vector combines information for multiple spectral lines
  // and (unlikely) multiple source table entries.
  // If the doppler sub table doesn't exist, the information is
  // retrieved from directly from the source sub table.
  Bool dopplerInfo (Vector<Double>& restFrequency, Int spwId, Int fieldId);

private:
  // Prohibit null constructor, copy constructor and assignment for now
  MSDopplerUtil();
  MSDopplerUtil& operator= (const MSDopplerUtil&);
  MSDopplerUtil (const MSDopplerUtil&);

  // Underlying MeasurementSet
  MeasurementSet ms_p;
};


} //# NAMESPACE CASACORE - END

#endif


