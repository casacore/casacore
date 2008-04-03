//# FluxStandard.h: Compute flux densities for standard reference sources
//# Copyright (C) 1996,1997,1999,2001
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

#ifndef COMPONENTS_FLUXSTANDARD_H
#define COMPONENTS_FLUXSTANDARD_H

//# include directives
#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <measures/Measures/MFrequency.h>
#include <components/ComponentModels/Flux.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// FluxStandard: Compute flux densities for standard reference sources
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
// <li><linkto class="Flux">Flux</linkto> module
// </prerequisite>
//
// <etymology>
// From "flux density" and "standard".
// </etymology>
//
// <synopsis>
// The FluxStandard class provides a means to compute total flux
// densities for specified non-variable sources on a standard
// flux density scale, such as that established by Baars or
// Perley and Taylor.
// </etymology>
//
// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Encapsulate information on standard flux density computation in one class.
// </motivation>
//
// <todo asof="99/06/01">
// <li> closer integration into component models.
// </todo>

class FluxStandard
{
 public:
  // Flux scale types
  enum FluxScale {

    // Perley (1990); plus Reynolds (1934-638; 7/94); Baars (3C138)
    PERLEY_90,

    // Perley and Taylor (1995.2); plus Reynolds (1934-638; 7/94)
    PERLEY_TAYLOR_95,

    // Perley and Taylor (1999.2); plus Reynolds (1934-638; 7/94)
    PERLEY_TAYLOR_99,

    // Baars scale
    BAARS};

  // Default constructor, and destructor
  FluxStandard (FluxStandard::FluxScale scale = 
		FluxStandard::PERLEY_TAYLOR_99);
  ~FluxStandard();

  // Compute the flux density for a specified source at a specified frequency
  Bool compute (const String& sourceName, const MFrequency& mfreq,
		Flux <Double>& value, Flux<Double>& error);

  // Decode a string representation of the standard or catalog name
  static Bool matchStandard(const String& name, 
			    FluxStandard::FluxScale& stdEnum,
			    String& stdName);

  // Return a standard string description for each scale or catalog
  static String standardName(const FluxStandard::FluxScale& stdEnum);

 private:
  // Flux scale in use
  FluxStandard::FluxScale itsFluxScale;

};


} //# NAMESPACE CASA - END

#endif
