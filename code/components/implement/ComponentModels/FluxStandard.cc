//# FluxStandard.cc: Implementation of FluxStandard.h
//# Copyright (C) 1996,1997,1999
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
//----------------------------------------------------------------------------

#include <trial/ComponentModels/FluxStandard.h>

//----------------------------------------------------------------------------

FluxStandard::FluxStandard (FluxStandard::FluxScale scale) : 
  itsFluxScale(scale)
{
// Default constructor
// Output to private data:
//    itsFluxScale      FluxStandard::FluxScale     Flux scale (eg. BAARS)
//
};

//----------------------------------------------------------------------------

FluxStandard::~FluxStandard()
{
// Default destructor
//
};

//----------------------------------------------------------------------------

Bool FluxStandard::compute (const String& sourceName, const MFrequency& mfreq,
			    Flux <Double>& value, Flux <Double>& error)
{
// Compute the flux density for a specified source at a specified frequency
// Inputs:
//    sourceName       const String&             Source name
//    mfreq            const MFrequency&         Desired frequency
// Output:
//    value            Flux&                     Computed total flux density
//    error            Flux&                     Flux density error;
//                                               (0=> not known).
//    compute          Bool                      False if source not recognized
//                                               as a standard reference.
//
  Bool found = False;
  Double fluxDensity = 0.0;
  Double fluxError = 0.0;
  Double fluxCoeff = 0.0;
  Double coeffErr = 0.0;
  
  // Get frequency in MHz
  Quantity mvfreq = mfreq.get ("MHz");
  // Flux density polynomials expressed in terms of log_10 (f/MHz)
  Double dt = log10 (mvfreq.getValue());

  // Select on source name

  // *** 3C286 ***

  if (sourceName == "3C286" || sourceName == "1328+307" || 
      sourceName == "1331+305") {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 1.480 + dt * (0.292 + dt * (-0.124));
      coeffErr = square (0.018) + square (0.006*dt) + square (0.001*dt*dt);
      break;
    };
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 0.50344 + dt * (1.05026 + dt * (-0.31666 + dt * 0.01602));
      break;
    };
    };

    // *** 3C48 ***

  } else if (sourceName == "3C48" || sourceName == "0134+329" ||
	     sourceName == "0137+331") {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 2.345 + dt * (0.071 + dt * (-0.138));
      coeffErr = square (0.03) + square (0.001*dt) + square (0.001*dt*dt);
      break;
    };
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 1.16801 + dt * (1.07526 + dt * (-0.42254 + dt * 0.02699));
      break;
    };
    };

    // *** 3C147 ***

  } else if (sourceName == "3C147" || sourceName == "0538+498" ||
	     sourceName == "0542+498") {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 1.766 + dt * (0.447 + dt * (-0.184));
      coeffErr = square (0.017) + square (0.006*dt) + square (0.001*dt*dt);
      break;
    };
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 0.05702 + dt * (2.09340 + dt * (-0.7076 + dt * 0.05477));
      break;
    };
    };

    // *** 3C138 ***

  } else if (sourceName == "3C138" || sourceName == "0518+165" ||
	     sourceName == "0521+166") {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 2.009 + dt * (-0.07176 + dt * (-0.0862));
      // No flux density error specified
      break;
    };
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 1.97498 + dt * (-0.23918 + dt * (0.01333 + dt * -0.01389));
      break;
    };
    };

    // *** 1934-638 ***

  } else if (sourceName == "1934-638") {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = -23.839 + dt * (19.569 + dt * (-4.8168 + dt * 0.35836));
      // No flux density error specified
      break;
    };
    case PERLEY_TAYLOR_95: {
      fluxCoeff = -30.7667 + dt * (26.4908 + dt * (-7.0977 + dt * 0.605334));
      break;
    };
    };

    // *** 3C295 ***

  } else if (sourceName == "3C295" || sourceName == "1409+524" ||
	     sourceName == "1411+522") {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 1.485 + dt * (0.759 + dt * (-0.255));
      coeffErr = square (0.013) + square (0.009*dt) + square (0.001*dt*dt);
      break;
    };
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 1.28872 + dt * (0.94172 + dt * (-0.31113 + dt * 0.00569));
      break;
    };
    };
  };

  // Compute the flux density value and its error
  fluxDensity = pow (10.0, fluxCoeff);
  if (coeffErr > 0) {
    fluxError = log (10.0) * fluxDensity * sqrt (coeffErr);
  };

  // Set flux density value and its error
  value.setValue (fluxDensity);
  error.setValue (fluxError);

  return found;
};

//----------------------------------------------------------------------------




