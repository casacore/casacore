//# ComponentType.cc:  this defines ComponentType.cc
//# Copyright (C) 1997,1998
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

#include <trial/ComponentModels/ComponentType.h>
#include <aips/Utilities/String.h>

String ComponentType::name(ComponentType::Shape shapeEnum) {
  switch (shapeEnum) {
  case ComponentType::POINT: 
    return "Point";
  case ComponentType::GAUSSIAN:
    return "Gaussian";
  case ComponentType::DISK:
    return "Disk";
  default:
    return "Unknown";
  };
}

ComponentType::Shape ComponentType::shape(const String & shapeName) {
  String canonicalCase(shapeName);
  canonicalCase.capitalize();
  ComponentType::Shape s;
  for (uInt i = 0; i < NUMBER_SHAPES; i++) {
    s = (ComponentType::Shape) i;
    if (canonicalCase.matches(ComponentType::name(s))) {
      return s;
    }
  }
  return ComponentType::UNKNOWN_SHAPE;
}

String ComponentType::name(ComponentType::Polarisation fluxEnum) {
  switch (fluxEnum) {
  case ComponentType::STOKES: 
    return "Stokes";
  case ComponentType::LINEAR:
    return "Linear";
  case ComponentType::CIRCULAR:
    return "Circular";
  default:
    return "Unknown";
  };
}

ComponentType::Polarisation ComponentType::polarisation(const String & 
						 polarisationName) {
  String canonicalCase(polarisationName);
  canonicalCase.capitalize();
  ComponentType::Polarisation s;
  for (uInt i = 0; i < NUMBER_POLARISATIONS; i++) {
    s = (ComponentType::Polarisation) i;
    if (canonicalCase.matches(ComponentType::name(s))) {
      return s;
    }
  }
  return ComponentType::UNKNOWN_POLARISATION;
}

String ComponentType::name(ComponentType::SpectralShape spectralEnum) {
  switch (spectralEnum) {
  case ComponentType::CONSTANT_SPECTRUM:
    return "Constant";
  case ComponentType::SPECTRAL_INDEX:
    return "Spectral Index";
  default:
    return "Unknown";
  };
}

ComponentType::SpectralShape ComponentType::spectralShape(const String & 
							  spectralName) {
  String canonicalCase(spectralName);
  canonicalCase.capitalize();
  ComponentType::SpectralShape s;
  for (uInt i = 0; i < NUMBER_SPECTRAL_SHAPES; i++) {
    s = (ComponentType::SpectralShape) i;
    if (canonicalCase.matches(ComponentType::name(s))) {
      return s;
    }
  }
  return ComponentType::UNKNOWN_SPECTRAL_SHAPE;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentType"
// End: 
