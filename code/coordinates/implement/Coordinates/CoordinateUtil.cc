//# CoordUtils.cc: 
//# Copyright (C) 1996,1997
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

#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/Projection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>

void addDirAxes(CoordinateSystem & coords){
  Matrix<Double> xform(2, 2); xform = 0.0; xform.diagonal() = 1.0;
  DirectionCoordinate dirAxes(MDirection::J2000, 
			       Projection(Projection::SIN),
			       0.0, 0.0, // Ref is at RA = 0, Dec = 0
			       1.0, 1.0, // The increment is overwritten below
			       xform,    // Rotation matrix
			       0.0, 0.0  // Ref pixel is 0,0
			       );
  // reset the increment to 1 minute of arc on both axes
  Vector<String> units(2); units = String("'"); 
  dirAxes.setWorldAxisUnits(units);
  Vector<Double> inc(2); inc = 1.0; 
  AlwaysAssert(dirAxes.setIncrement(inc) == True, AipsError);
  // Add the direction coordinates to the system. 
  coords.addCoordinate(dirAxes);
}
void addIQUVAxis(CoordinateSystem & coords){
  Vector<Int> pols(4);
  pols(0) = Stokes::I;
  pols(1) = Stokes::Q;
  pols(2) = Stokes::U;
  pols(3) = Stokes::V;
  StokesCoordinate polAxis(pols);
  // Add the stokes coordinates to the system. 
  coords.addCoordinate(polAxis);
}
void addIAxis(CoordinateSystem & coords){
  Vector<Int> pols(1);
  pols(0) = Stokes::I;
  StokesCoordinate polAxis(pols);
  // Add the stokes coordinates to the system. 
  coords.addCoordinate(polAxis);
}

void addFreqAxis(CoordinateSystem & coords){
  SpectralCoordinate freqAxis(MFrequency::LSR, // Local standard of rest
			      1415E6,          // ref. freq. = 1415MHz
			      1E3,             // 1 kHz bandwidth/channel
			      0.0);            // channel 0 is the ref.
  // Add the frequency coordinate to the system. 
  coords.addCoordinate(freqAxis);
}

CoordinateSystem defaultCoords2D(){
  CoordinateSystem coords;
  addDirAxes(coords);
  return coords;
}
CoordinateSystem defaultCoords3D(){
  CoordinateSystem coords;
  addDirAxes(coords);
  addIQUVAxis(coords);
  return coords;
}
CoordinateSystem defaultCoords4D(){
  CoordinateSystem coords;
  addDirAxes(coords);
  addIQUVAxis(coords);
  addFreqAxis(coords);
  return coords;
}

CoordinateSystem defaultCoords(uInt dims){
  switch (dims){
  case 2:
    return defaultCoords2D();
  case 3:
    return defaultCoords3D();
  case 4:
    return defaultCoords4D();
  default:
    throw(AipsError("defaultCoords() - cannot create cordinates except "
		    "for a 2, 3 or 4-dimensional image"));
  }
}
