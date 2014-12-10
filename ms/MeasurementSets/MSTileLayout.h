//# MSTileLayout.h: Determine appropriate tiling for a MeasurementSet
//# Copyright (C) 2002
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

#ifndef MS_MSTILELAYOUT_H
#define MS_MSTILELAYOUT_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# forward decl
class IPosition;
class String;

// <summary> 
// An helper class for deciding on tile shapes in MeasurementSets
// </summary>
 
// <use visibility=export>
 
// <prerequisite>
//   <li> <linkto class="MeasurementSet:description">MeasurementSet</linkto> 
//   <li> <linkto class="TiledStMan:description">TiledStMan</linkto> 
// </prerequisite>
//
// <etymology>
// MSTileLayout stands for the MeasurementSet tile layout chooser
// </etymology>
//
// <synopsis> 
// MSTileLayout is a class to determine an appropriate tile shape choice
// for the MeasurementSet DATA columns, based on the shape of the DATA,
// the observing mode and the number of interferometers
// </synopsis> 
//
// <example>
// <srcblock>
// // The following code returns the tile shape given the DATA shape,
// // the observing type and the array name.
// IPosition dataShape(2,4,1024); // 4 polarizations, 1024 channels
// IPosition tileShape = MSTileLayout::tileShape(dataShape,
//                                               MSTileLayout::FastMosaic,
//                                               "ATCA");
// cout << "tileShape = "<< tileShape << endl;
// // Output is: 
// tileShape = (4,11,15)
// </srcblock>
// </example>

// <motivation>
// This class is intended to replace bits of code scattered throughout various
// fillers and collect all tiling decisions in one place.
// </motivation>
//
// <todo>
// </todo>

class MSTileLayout
{
public:
  enum ObsType {
    // Standard, optimizes i/o by using large tiles (128 kB)
    Standard=0,
    // Fast Mosaic, specify this if you have many (>30) fields and you 
    // spend only a few integrations per pointing before moving on.
    // Avoids useless i/o caching by using small tiles
    FastMosaic=1
  };

  // Suggest tile shape based on the data shape, the observing mode, 
  // the number of interferometers and the number of integrations per
  // pointing.
  //
  // First argument should be a 2-dimensional IPosition with the data matrix
  // shape.
  // The second argument is one of the enums above specifying the type of
  // observation. 
  // The third argument is an estimate of the number of interferometers
  // present throughout most of the data.
  // The last argument is an estimate of the number of integrations per
  // pointing. 
  //
  // The last three arguments only need to be specified if the observing mode 
  // is non Standard.
  // Basically the choice is between large tiles for efficient I/O and small
  // tiles when a common access pattern (field_id order) would result
  // in very inefficient caching. The latter occurs for large tiles if the 
  // field_id changes rapidly AND there are many fields AND the data 
  // matrix is small (e.g., continuum mosaic of a large area with only
  // one or two integrations per pointing). Note that accessing fast mosaic
  // data with large tiles in field_id order can be 10-100 times slower than
  // sequential access.
  static IPosition tileShape(const IPosition& dataShape,
			     Int observationType = Standard,
			     Int nIfr = 0, Int nInt = 1);

  // same as above, but pick standard nIfr (number of interferometers)
  // for named array and default nInt.
  static IPosition tileShape(const IPosition& dataShape,
			     Int observationType,
			     const String& array);
};


} //# NAMESPACE CASACORE - END

#endif
