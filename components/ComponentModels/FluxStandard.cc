//# FluxStandard.cc: Implementation of FluxStandard.h
//# Copyright (C) 1996,1997,1999,2001,2002
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

#include <components/ComponentModels/FluxStandard.h>
#include <casa/BasicMath/Math.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//----------------------------------------------------------------------------

FluxStandard::FluxStandard (FluxStandard::FluxScale scale) : 
  itsFluxScale(scale)
{
// Default constructor
// Output to private data:
//    itsFluxScale      FluxStandard::FluxScale     Flux scale (eg. BAARS)
//
}

//----------------------------------------------------------------------------

FluxStandard::~FluxStandard()
{
// Default destructor
//
}

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
  // Perley_Taylor 1999.2 coefficients are for GHz
  Double dt3 = dt - 3.0;

  // Select on the source name

  // *** 3C286 ***

  if (sourceName.contains("3C286") || sourceName.contains("1328+307") || 
      sourceName.contains("1331+305")) {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 1.480 + dt * (0.292 + dt * (-0.124));
      coeffErr = square (0.018) + square (0.006*dt) + square (0.001*dt*dt);
      break;
    }
    case PERLEY_90: {
      fluxCoeff = 1.35899 + dt * (0.3599 + dt * (-0.13338));
      break;
    }
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 0.50344 + dt * (1.05026 + dt * (-0.31666 + dt * 0.01602));
      break;
    }
    case PERLEY_TAYLOR_99: {
      fluxCoeff =1.23734 + dt3 * (-0.43276 + dt3 * (-0.14223 + dt3 * 0.00345));
      break;
    }
    }

    // *** 3C48 ***

  } else if (sourceName.contains("3C48") || sourceName.contains("0134+329") 
	     || sourceName.contains("0137+331")) {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 2.345 + dt * (0.071 + dt * (-0.138));
      coeffErr = square (0.03) + square (0.001*dt) + square (0.001*dt*dt);
      break;
    }
    case PERLEY_90: {
      fluxCoeff = 2.0868 + dt * (0.20889 + dt * (-0.15498));
      break;
    }
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 1.16801 + dt * (1.07526 + dt * (-0.42254 + dt * 0.02699));
      break;
    }
    case PERLEY_TAYLOR_99: {
      fluxCoeff =1.31752 + dt3 * (-0.74090 + dt3 * (-0.16708 + dt3 * 0.01525));
      break;
    }
    }

    // *** 3C147 ***

  } else if (sourceName.contains("3C147") || sourceName.contains("0538+498")
	     || sourceName.contains("0542+498")) {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 1.766 + dt * (0.447 + dt * (-0.184));
      coeffErr = square (0.017) + square (0.006*dt) + square (0.001*dt*dt);
      break;
    }
    case PERLEY_90: {
      fluxCoeff = 1.92641 + dt * (0.36072 + dt * (-0.17389));
      break;
    }
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 0.05702 + dt * (2.09340 + dt * (-0.7076 + dt * 0.05477));
      break;
    }
    case PERLEY_TAYLOR_99: {
      fluxCoeff =1.44856 + dt3 * (-0.67252 + dt3 * (-0.21124 + dt3 * 0.04077));
      break;
    }
    }

    // *** 3C138 ***

  } else if (sourceName.contains("3C138") || sourceName.contains("0518+165") 
	     || sourceName.contains("0521+166")) {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 2.009 + dt * (-0.07176 + dt * (-0.0862));
      // No flux density error specified
      break;
    }
    case PERLEY_90: {
      fluxCoeff = 2.009 + dt * (-0.07176 + dt * (-0.0862));
      break;
    }
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 1.97498 + dt * (-0.23918 + dt * (0.01333 + dt * -0.01389));
      break;
    }
    case PERLEY_TAYLOR_99: {
      fluxCoeff =1.00761 + dt3 * (-0.55629 + dt3 * (-0.11134 + dt3 * -0.0146));
      break;
    }
    }

    // *** 1934-638 ***

  } else if (sourceName.contains("1934-638")) {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = -23.839 + dt * (19.569 + dt * (-4.8168 + dt * 0.35836));
      // No flux density error specified
      break;
    }
    case PERLEY_90: {
      fluxCoeff = -30.7667 + dt * (26.4908 + dt * (-7.0977 + dt * 0.605334));
      break;
    }
    case PERLEY_TAYLOR_95: {
      fluxCoeff = -30.7667 + dt * (26.4908 + dt * (-7.0977 + dt * 0.605334));
      break;
    }
    case PERLEY_TAYLOR_99: {
      if (dt<=4) 
        fluxCoeff = -30.7667 + dt * (26.4908 + dt * (-7.0977 + dt * 0.605334));
      else
        fluxCoeff = -202.6259 + dt * (149.7321 + dt * (-36.4943 + dt * 2.9372));
      break;
    }
    }

    // *** 3C295 ***

  } else if (sourceName.contains("3C295") || sourceName.contains("1409+524")
	     || sourceName.contains("1411+522")) {

    found = True;
    switch (itsFluxScale) {
    case BAARS: {
      fluxCoeff = 1.485 + dt * (0.759 + dt * (-0.255));
      coeffErr = square (0.013) + square (0.009*dt) + square (0.001*dt*dt);
      break;
    }
    case PERLEY_90: {
      fluxCoeff = 1.485 + dt * (0.759 + dt * (-0.255));
      break;
    }
    case PERLEY_TAYLOR_95: {
      fluxCoeff = 1.28872 + dt * (0.94172 + dt * (-0.31113 + dt * 0.00569));
      break;
    }
    case PERLEY_TAYLOR_99: {
      fluxCoeff = 1.46744 + dt3 * (-0.7735 + dt3 * (-0.25912 + dt3 * 0.00752));
      break;
    }
    }
  }

  // Compute the flux density value and its error
  fluxDensity = pow (10.0, fluxCoeff);
  if (coeffErr > 0) {
    fluxError = log (10.0) * fluxDensity * sqrt (coeffErr);
  }

  // Set flux density value and its error
  value.setValue (fluxDensity);
  error.setValue (fluxError);

  return found;
}

//----------------------------------------------------------------------------

Bool FluxStandard::matchStandard (const String& name, 
				  FluxStandard::FluxScale& stdEnum,
				  String& stdName)
{
// Match an input string to a standard/catalog enum and descriptor
// Inputs:
//    name             const String&             Input string
// Output:
//    stdEnum          FluxStandard::FluxScale   Matching enum
//    stdName          String                    Standard descriptor for 
//                                               the matching enum.
//    matchStandard    Bool                      True if matched; False
//                                               if default returned.
//
  // Set default enum
  stdEnum = FluxStandard::PERLEY_TAYLOR_99;

  // Local lowercase copy of input string
  String lname = name;
  lname.downcase();
  Bool matched = True;

  // Case input string match of:
  //
  // Perley (1990)
  if (lname.contains("perley") && 
      (lname.contains("90") || lname.contains("1990"))) {
    stdEnum = FluxStandard::PERLEY_90;
  }
  // Perley-Taylor (1995)
  else if (lname.contains("perley") && lname.contains("taylor") &&
      (lname.contains("95") || lname.contains("1995"))) {
    stdEnum = FluxStandard::PERLEY_TAYLOR_95;
  }
  // Perley-Taylor (1999)
  else if (lname.contains("perley") && lname.contains("taylor") &&
      (lname.contains("99") || lname.contains("1999"))) {
    stdEnum = FluxStandard::PERLEY_TAYLOR_99;
  }
  // Baars
  else if (lname.contains("baars")) {
    stdEnum = FluxStandard::BAARS;
  } else {
    //
    matched = False;
  }

  // Retrieve standard descriptor
  stdName = standardName (stdEnum);

  return matched;
}

//----------------------------------------------------------------------------

String FluxStandard::standardName (const FluxStandard::FluxScale& stdEnum)
{
// Return the standard descriptor for a specified standard/catalog enum
// Inputs:
//    stdEnum          FluxStandard::FluxScale   Standard/catalog enum
// Output:
//    standardName     String                    Standard descriptor
//
  // Case scale enum of:
  //
  String stdName;
  switch (stdEnum) {
  case BAARS: {
    stdName = "Baars";
    break;
  }
  case PERLEY_90: {
    stdName = "Perley 90";
    break;
  }
  case PERLEY_TAYLOR_95: {
    stdName = "Perley-Taylor 95";
    break;
  }
  case PERLEY_TAYLOR_99: {
    stdName = "Perley-Taylor 99";
    break;
  }
  }
  return stdName;
}

//----------------------------------------------------------------------------




} //# NAMESPACE CASA - END

