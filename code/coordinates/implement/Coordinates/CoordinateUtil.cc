//# CoordinateUtils.cc: 
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
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/Projection.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/Stokes.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/GenSort.h>
#include <aips/Utilities/String.h>

void CoordinateUtil::addDirAxes(CoordinateSystem & coords){
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
  Vector<Double> inc(2); inc = 1.0;
  dirAxes.setWorldAxisUnits(units);
  AlwaysAssert(dirAxes.setIncrement(inc) == True, AipsError);
  // Add the direction coordinates to the system. 
  coords.addCoordinate(dirAxes);
}
void CoordinateUtil::addIQUVAxis(CoordinateSystem & coords){
  Vector<Int> pols(4);
  pols(0) = Stokes::I;
  pols(1) = Stokes::Q;
  pols(2) = Stokes::U;
  pols(3) = Stokes::V;
  StokesCoordinate polAxis(pols);
  // Add the stokes coordinates to the system. 
  coords.addCoordinate(polAxis);
}
void CoordinateUtil::addIAxis(CoordinateSystem & coords){
  Vector<Int> pols(1);
  pols(0) = Stokes::I;
  StokesCoordinate polAxis(pols);
  // Add the stokes coordinates to the system. 
  coords.addCoordinate(polAxis);
}

void CoordinateUtil::addFreqAxis(CoordinateSystem & coords){
  SpectralCoordinate freqAxis(MFrequency::LSR, // Local standard of rest
			      1415E6,          // ref. freq. = 1415MHz
			      1E3,             // 1 kHz bandwidth/channel
			      0.0);            // channel 0 is the ref.
  // Add the frequency coordinate to the system. 
  coords.addCoordinate(freqAxis);
}

CoordinateSystem CoordinateUtil::defaultCoords2D(){
  CoordinateSystem coords;
  CoordinateUtil::addDirAxes(coords);
  return coords;
}
CoordinateSystem CoordinateUtil::defaultCoords3D(){
  CoordinateSystem coords;
  CoordinateUtil::addDirAxes(coords);
  CoordinateUtil::addFreqAxis(coords);
  return coords;
}
CoordinateSystem CoordinateUtil::defaultCoords4D(){
  CoordinateSystem coords;
  CoordinateUtil::addDirAxes(coords);
  CoordinateUtil::addIQUVAxis(coords);
  CoordinateUtil::addFreqAxis(coords);
  return coords;
}

CoordinateSystem CoordinateUtil::defaultCoords(uInt dims){
  switch (dims){
  case 2:
    return CoordinateUtil::defaultCoords2D();
  case 3:
    return CoordinateUtil::defaultCoords3D();
  case 4:
    return CoordinateUtil::defaultCoords4D();
  default:
    throw(AipsError("defaultCoords() - cannot create cordinates except "
		    "for a 2, 3 or 4-dimensional image"));
    // The following line is just to suppress a compiler warning that this
    // function does not always return a CoordinateSystem. It is never
    // executed.
    return CoordinateUtil::defaultCoords2D();
  }
}

Int CoordinateUtil::findSpectralAxis(const CoordinateSystem & coords) {
  const Int freqCoordAxis = coords.findCoordinate(Coordinate::SPECTRAL);
  if (freqCoordAxis < 0) 
    return freqCoordAxis;
  AlwaysAssert(coords.findCoordinate(Coordinate::SPECTRAL, freqCoordAxis)
	       == -1, AipsError);
  const Vector<Int> pixelAxes = coords.pixelAxes(freqCoordAxis);
  AlwaysAssert(pixelAxes.nelements() == 1, AipsError);
  return pixelAxes(0);
}

Vector<uInt> CoordinateUtil::findDirectionAxes(const CoordinateSystem & coords) {
  const Int dirCoordAxis = coords.findCoordinate(Coordinate::DIRECTION);
  Vector<uInt> retVal;
  if (dirCoordAxis < 0) 
    return retVal;
  AlwaysAssert(coords.findCoordinate(Coordinate::DIRECTION, dirCoordAxis)
	       == -1, AipsError);
  const Vector<Int> pixelAxes = coords.pixelAxes(dirCoordAxis);
  AlwaysAssert(pixelAxes.nelements() == 2, AipsError);
  if ((pixelAxes(0) >= 0) && (pixelAxes(1) >= 0)) {
    retVal.resize(2);
    retVal(0) = pixelAxes(0);
    retVal(1) = pixelAxes(1);
  }
  else if ((pixelAxes(0) >= 0) && (pixelAxes(1) < 0)) {
    retVal.resize(1);
    retVal(0) = pixelAxes(0);
  }
  else if ((pixelAxes(0) < 0) && (pixelAxes(1) >= 0)) {
    retVal.resize(1);
    retVal(1) = pixelAxes(1);
  }
  return retVal;
}

Int CoordinateUtil::findStokesAxis(Vector<Int> & whichPols, const CoordinateSystem & coords) {
  const Int polCoordAxis = coords.findCoordinate(Coordinate::STOKES);
  if (polCoordAxis < 0) {
    whichPols.resize(1);
    whichPols(0) = Stokes::I;
    return polCoordAxis;
  }
  AlwaysAssert(coords.findCoordinate(Coordinate::STOKES, polCoordAxis)
	       == -1, AipsError);
  const Vector<Int> pixelAxes = coords.pixelAxes(polCoordAxis);
  AlwaysAssert(pixelAxes.nelements() == 1, AipsError);
  const StokesCoordinate polCoord = coords.stokesCoordinate(polCoordAxis);
  whichPols.resize(0);
  whichPols = polCoord.stokes();
  if (pixelAxes(0) < 0) {
    AlwaysAssert(whichPols.nelements() == 1, AipsError);
  }
  else {
    AlwaysAssert(whichPols.nelements() > 0, AipsError);
  }
  return pixelAxes(0);
}


Bool CoordinateUtil::removeAxes(CoordinateSystem& cSys,
                                Vector<Double>& worldReplacement,
                                Vector<Double>& pixelReplacement,
                                const Vector<uInt>& worldAxes,
                                const Bool removeThem,
                                const Bool removePixelAxesToo=True)
//
// Remove all the world axes and optionally associated pixel axes
// derived from the given list (a list to keep or remove).
// This is awkward because as soon as you remove an 
// axis, they all shuffle down one !  The replacement values
// are optional.  If these vectors are the wrong length,
// (e.g. 0), the reference pixels/values are used.  The used
// values are returned.
//
{
// Bug out if nothing to do

   if (worldAxes.nelements() == 0) return True;

// Make sure the world axes are valid

   for (uInt i=0; i<worldAxes.nelements(); i++) {
      if (worldAxes(i) >= cSys.nWorldAxes()) return False;         
   }


// Make a list of the axes to remove ins ascending order
// with no duplicates

   Vector<uInt> remove(cSys.nWorldAxes());
   if (removeThem) {
      remove.resize(worldAxes.nelements());
      remove = worldAxes;
      GenSort<uInt>::sort(remove, Sort::Ascending, Sort::NoDuplicates);
   } else {
      for (uInt i=0,j=0; i<cSys.nWorldAxes(); i++) {
         if (!anyEQ(worldAxes.ac(), i)) remove(j++) = i;
      }
      remove.resize(j,True);
   }
//   cout << "remove = " << remove.ac() << endl;

// Set the replacement values

   if (worldReplacement.nelements() != worldAxes.nelements()) {
      worldReplacement.resize(worldAxes.nelements());
      for (i=0; i<worldAxes.nelements(); i++) {
         worldReplacement(i) = cSys.referenceValue()(worldAxes(i));
      }
   }

// Pixel replacement is much more awkward. 

   Int pixelAxis;
   if (removePixelAxesToo) {
      uInt nPixelAxes = 0;
      for (i=0; i<worldAxes.nelements(); i++) {
          pixelAxis = cSys.worldAxisToPixelAxis(i);
          if (pixelAxis != -1) nPixelAxes++;
      }

      if (pixelReplacement.nelements() != nPixelAxes) {

// Vector is wrong size, give them reference pixels

         pixelReplacement.resize(nPixelAxes);
         uInt j = 0;
         for (i=0; i<worldAxes.nelements(); i++) {
            pixelAxis = cSys.worldAxisToPixelAxis(i);
            if (pixelAxis != -1) {
               pixelReplacement(j++) = cSys.referencePixel()(pixelAxis);
            }
         }
      }
   }


// Now for each world axis in the list, get rid of it
 
   uInt worldAxis = remove(0);
   uInt j = 0;
   for (i=0; i<remove.nelements(); i++) {

       if (removePixelAxesToo) { 

// Find the corresponding pixel axis (first !)

          pixelAxis = cSys.worldAxisToPixelAxis(worldAxis);
       }

// Remove the axes
 
        cSys.removeWorldAxis(worldAxis, worldReplacement(i));
        if (removePixelAxesToo && pixelAxis != -1) {
           cSys.removePixelAxis(uInt(pixelAxis), pixelReplacement(j++));
        }

// Find the next world axis to eradicate

        if (i+1<remove.nelements()) worldAxis = remove(i+1) - 1;
   }
   return True;
}      






