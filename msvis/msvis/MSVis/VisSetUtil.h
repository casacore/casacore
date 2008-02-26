//# VisSetUtil.h: Definitions for Stokes Image utilities
//# Copyright (C) 1996,1997,1998
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#ifndef MSVIS_VISSETUTIL_H
#define MSVIS_VISSETUTIL_H

#include <casa/aips.h>
#include <casa/BasicSL/Complex.h>
#include <casa/Quanta/Quantum.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <msvis/MSVis/VisSet.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// Utilities for operating on VisSets.
// </summary>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis> 
// </synopsis> 
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="">
// </todo>


class VisSetUtil {
  
public:

  // Natural weighting
  static void WeightNatural(VisSet& vs, Double& sumwt);

  // Uniform weighting (robust possible) with specified image size
  static void WeightUniform(VisSet& vs, const String& rmode,
			    const Quantity& noise,
			    const Double robust, const Int nx, const Int ny,
			    const Quantity& cellx, const Quantity& celly,
			    Double& sumwt,
			    const Int uBox=0, const Int vBox=0);
  
  // Radial weighting (as uv distance)
  static void WeightRadial(VisSet& vs, Double& sumwt);

  // Filtering
  static void Filter(VisSet& vs, const String& type, const Quantity& bmaj,
		     const Quantity& bmin, const Quantity& bpa,
		     Double& sumwt, Double& minfilter, Double& maxfilter);

  // Implement a uv range
  static void UVRange(VisSet &vs, const Double& uvmin, const Double& uvmax,
		      Double& sumwt);


  // Calculate sensitivity
  static void Sensitivity(VisSet &vs, Quantity& pointsourcesens, Double& relativesens,
			  Double& sumwt);

  // Hanning smoothing of spectral channels
  static void HanningSmooth(VisSet &vs);

  // Subtract/add model from/to corrected visibility data
  static void UVSub(VisSet &vs, Bool reverse=False);

};

} //# NAMESPACE CASA - END

#endif


