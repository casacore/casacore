//# CoordinateSystem.cc: Interconvert pixel and image coordinates. 
//# Copyright (C) 1997,1998,1999,2000
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

#include <trial/Coordinates/CoordinateSystem.h>

#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/TabularCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>


#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Measures/MDoppler.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/VelocityMachine.h>
#include <aips/Utilities/Assert.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/UnitMap.h>

#include <aips/Containers/Record.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>

#include <strstream.h>
#include <iomanip.h>
#include <iostream.h>


CoordinateSystem::CoordinateSystem()
    : coordinates_p(0), 
      world_maps_p(0), world_tmps_p(0), world_replacement_values_p(0),
      pixel_maps_p(0), pixel_tmps_p(0), pixel_replacement_values_p(0),
      worldAxes_tmps_p(0), pixelAxes_tmps_p(0),
      worldOut_tmps_p(0), pixelOut_tmps_p(0),
      worldMin_tmps_p(0), worldMax_tmps_p(0)
{
    // Nothing
}

void CoordinateSystem::copy(const CoordinateSystem &other)
{
    if (this == &other) {
	return;
    }

    clear();

    obsinfo_p = other.obsinfo_p;

    coordinates_p = other.coordinates_p;
    const uInt n = coordinates_p.nelements();

    uInt i;
    for (i=0; i < n; i++) {
	coordinates_p[i] = coordinates_p[i]->clone();
	AlwaysAssert(coordinates_p[i], AipsError);
    }
//
    world_maps_p.resize(n);
    world_tmps_p.resize(n);
    world_replacement_values_p.resize(n);
    pixel_maps_p.resize(n);
    pixel_tmps_p.resize(n);
    pixel_replacement_values_p.resize(n);
//
    worldAxes_tmps_p.resize(n);
    pixelAxes_tmps_p.resize(n);
    worldOut_tmps_p.resize(n);
    pixelOut_tmps_p.resize(n);
    worldMin_tmps_p.resize(n);
    worldMax_tmps_p.resize(n);
//
    for (i=0; i<n; i++) {
	world_maps_p[i] = new Block<Int>(*(other.world_maps_p[i]));
	world_tmps_p[i] = new Vector<Double>(other.world_tmps_p[i]->copy());
	world_replacement_values_p[i] = 
	    new Vector<Double>(other.world_replacement_values_p[i]->copy());
	AlwaysAssert(world_maps_p[i] != 0 &&
		     world_tmps_p[i] != 0 &&
		     world_replacement_values_p[i] != 0, AipsError);
	pixel_maps_p[i] = new Block<Int>(*(other.pixel_maps_p[i]));
	pixel_tmps_p[i] = new Vector<Double>(other.pixel_tmps_p[i]->copy());
	pixel_replacement_values_p[i] = 
	    new Vector<Double>(other.pixel_replacement_values_p[i]->copy());
	AlwaysAssert(pixel_maps_p[i] != 0 &&
		     pixel_tmps_p[i] != 0 &&
		     pixel_replacement_values_p[i] != 0, AipsError);
//
	worldAxes_tmps_p[i] = new Vector<Bool>(other.worldAxes_tmps_p[i]->copy());
	pixelAxes_tmps_p[i] = new Vector<Bool>(other.pixelAxes_tmps_p[i]->copy());
	AlwaysAssert(worldAxes_tmps_p[i] != 0 && pixelAxes_tmps_p[i] != 0, AipsError);
//
	worldOut_tmps_p[i] = new Vector<Double>(other.worldOut_tmps_p[i]->copy());
	pixelOut_tmps_p[i] = new Vector<Double>(other.pixelOut_tmps_p[i]->copy());
	AlwaysAssert(worldOut_tmps_p[i] != 0 && pixelOut_tmps_p[i] != 0, AipsError);
//
	worldMin_tmps_p[i] = new Vector<Double>(other.worldMin_tmps_p[i]->copy());
	worldMax_tmps_p[i] = new Vector<Double>(other.worldMax_tmps_p[i]->copy());
	AlwaysAssert(worldMin_tmps_p[i] != 0 && worldMax_tmps_p[i] != 0, AipsError);
    }
}

void CoordinateSystem::clear()
{
    const uInt n = coordinates_p.nelements();

    for (uInt i=0; i<n; i++) {
	delete world_maps_p[i]; world_maps_p[i] = 0;
	delete world_tmps_p[i]; world_tmps_p[i] = 0;
	delete world_replacement_values_p[i]; world_replacement_values_p[i] = 0;
	delete pixel_maps_p[i]; pixel_maps_p[i] = 0;
	delete pixel_tmps_p[i]; pixel_tmps_p[i] = 0;
	delete pixel_replacement_values_p[i]; pixel_replacement_values_p[i] = 0;
	delete coordinates_p[i]; coordinates_p[i] = 0;
//
        delete worldAxes_tmps_p[i]; worldAxes_tmps_p[i] = 0;
        delete pixelAxes_tmps_p[i]; pixelAxes_tmps_p[i] = 0;
        delete worldOut_tmps_p[i]; worldOut_tmps_p[i] = 0;
        delete pixelOut_tmps_p[i]; pixelOut_tmps_p[i] = 0;
        delete worldMin_tmps_p[i]; worldMin_tmps_p[i] = 0;
        delete worldMax_tmps_p[i]; worldMax_tmps_p[i] = 0;
    }
}

CoordinateSystem::CoordinateSystem(const CoordinateSystem &other)
    : coordinates_p(0), 
      world_maps_p(0), world_tmps_p(0), world_replacement_values_p(0),
      pixel_maps_p(0), pixel_tmps_p(0), pixel_replacement_values_p(0),
      worldAxes_tmps_p(0), pixelAxes_tmps_p(0),
      worldOut_tmps_p(0), pixelOut_tmps_p(0),
      worldMin_tmps_p(0), worldMax_tmps_p(0)
{
    copy(other);
}

CoordinateSystem &CoordinateSystem::operator=(const CoordinateSystem &other)
{
    if (this != &other) {
	clear();
	copy(other);
    }

    return *this;
}

CoordinateSystem::~CoordinateSystem()
{
    clear();
}

void CoordinateSystem::addCoordinate(const Coordinate &coord)
{
    uInt oldWorldAxes = nWorldAxes();
    uInt oldPixelAxes = nPixelAxes();
//
// coordinates_p
//
    const uInt n = coordinates_p.nelements(); // "before" n, index of new coord
    coordinates_p.resize(n+1);
    coordinates_p[n] = coord.clone();
    AlwaysAssert(coordinates_p[n] != 0, AipsError);
//
// world_maps_p
//
    world_maps_p.resize(n+1);
    world_maps_p[n] = new Block<Int>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_maps_p[n], AipsError);
    uInt i;
    for (i=0; i < world_maps_p[n]->nelements(); i++) {
	world_maps_p[n]->operator[](i) = oldWorldAxes + i;
    }
//
// world_tmps_p
//
    world_tmps_p.resize(n+1);
    world_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_tmps_p[n], AipsError);
//
// pixel_maps_p
//
    pixel_maps_p.resize(n+1);
    pixel_maps_p[n] = new Block<Int>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_maps_p[n], AipsError);
    for (i=0; i < pixel_maps_p[n]->nelements(); i++) {
	pixel_maps_p[n]->operator[](i) = oldPixelAxes + i;
    }
//
// pixel_tmps_p
//
    pixel_tmps_p.resize(n+1);
    pixel_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_tmps_p[n], AipsError);
//
// pixel_replacement_values_p
//
    pixel_replacement_values_p.resize(n+1);
    pixel_replacement_values_p[n] = 
	new Vector<Double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_replacement_values_p[n], AipsError);
    *(pixel_replacement_values_p[n]) = 0.0;
//
// world_replacement_values_p
//
    world_replacement_values_p.resize(n+1);
    world_replacement_values_p[n] = 
	new Vector<Double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_replacement_values_p[n], AipsError);
    coordinates_p[n] -> toWorld(*(world_replacement_values_p[n]),
				*(pixel_replacement_values_p[n]));
//
// worldAxes_tmps_p
//
    worldAxes_tmps_p.resize(n+1);
    worldAxes_tmps_p[n] = new Vector<Bool>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldAxes_tmps_p[n], AipsError);
//
// pixelAxes_tmps_p
//
    pixelAxes_tmps_p.resize(n+1);
    pixelAxes_tmps_p[n] = new Vector<Bool>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixelAxes_tmps_p[n], AipsError);
//
// worldOut_tmps_p
//
    worldOut_tmps_p.resize(n+1);
    worldOut_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldOut_tmps_p[n], AipsError);
//
// pixelOut_tmps_p
//
    pixelOut_tmps_p.resize(n+1);
    pixelOut_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixelOut_tmps_p[n], AipsError);
//
// worldMin_tmps_p
//
    worldMin_tmps_p.resize(n+1);
    worldMin_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldMin_tmps_p[n], AipsError);
//
// worldMax_tmps_p
//
    worldMax_tmps_p.resize(n+1);
    worldMax_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldMax_tmps_p[n], AipsError);
}

void CoordinateSystem::transpose(const Vector<Int> &newWorldOrder,
				 const Vector<Int> &newPixelOrder)
{
    AlwaysAssert(newWorldOrder.nelements() == nWorldAxes(), AipsError);
    AlwaysAssert(newPixelOrder.nelements() == nPixelAxes(), AipsError);

    const uInt nc = nCoordinates();
    const uInt nw = newWorldOrder.nelements();
    const uInt np = newPixelOrder.nelements();

    // Verify that all axes are in new*Order once (only)
    Block<Bool> found(nw);
    found = False;
    uInt i;
    for (i=0; i<found.nelements(); i++) {
	Int which = newWorldOrder(i);
	AlwaysAssert(which >=0 && uInt(which) < nw && !found[which], AipsError);
	found[which] = True;
    }
    found.resize(np);
    found = False;
    for (i=0; i<found.nelements(); i++) {
	Int which = newPixelOrder(i);
	AlwaysAssert(which >=0 && uInt(which) < np && !found[which], AipsError);
	found[which] = True;
    }

    PtrBlock<Block<Int> *> newWorldMaps(nc);
    PtrBlock<Block<Int> *> newPixelMaps(nc);
    newWorldMaps.set(static_cast<Block<Int> *>(0));
    newPixelMaps.set(static_cast<Block<Int> *>(0));

    // copy the maps (because the deleted axes will be staying put)
    for (i=0; i<nc; i++) {
	newWorldMaps[i] = new Block<Int>(*world_maps_p[i]);
	newPixelMaps[i] = new Block<Int>(*pixel_maps_p[i]);
	AlwaysAssert((newWorldMaps[i] && newPixelMaps[i]), AipsError);
    }

    // Move the world axes to their new home
    for (i=0; i<nw; i++) {
	Int coord, axis;
	findWorldAxis(coord, axis, newWorldOrder(i));
	newWorldMaps[coord]->operator[](axis) = i;
    }

    // Move the pixel axes to their new home
    for (i=0; i<np; i++) {
	Int coord, axis;
	findPixelAxis(coord, axis, newPixelOrder(i));
	newPixelMaps[coord]->operator[](axis) = i;
    }

    // OK, now overwrite the new locations (after deleting the old pointers to avoid
    // a leak
    for (i=0; i<nc; i++) {
	delete world_maps_p[i];
	world_maps_p[i] = newWorldMaps[i];
	delete pixel_maps_p[i];
	pixel_maps_p[i] = newPixelMaps[i];
    }
}



Bool CoordinateSystem::worldMap(Vector<Int>& worldAxisMap,
                                Vector<Int>& worldAxisTranspose,
                                Vector<Bool>& refChange,
                                const CoordinateSystem& other) const
//
// Make a map from "*this" to "other"
//
// . Returns False if either "*this" or "other" have no valid
//   world axes.   Otherwise true.
// . The coordinate systems can have arbitrary numbers of coordinates
//   in any relative order.
// . Removed world and pixel axes are handled.
// . The value of worldAxisMap(i) is the world axis of "*this" matching 
//   world axis i in "other".  A value of -1 indicates that 
//   a world axis could not be matched.  
// . The value of worldAxisTranspose(i) is the world axis of "other"
//   matching world axis i of "*this"  It tells you how to transpose
//   "other" to be in the order of "*this".  A value of -1 indicates
//   that a world axis could not be matched. 
// . If refChange(i) is True, it means that if the coordinate matched,
//   there is a difference in reference type (E.g J2000->B1950)
//   for worldAxis i in "other"
{

// Resize the maps and initialize

   worldAxisMap.resize(other.nWorldAxes());
   worldAxisMap = -1;
   worldAxisTranspose.resize(nWorldAxes());
   worldAxisTranspose = -1;
   refChange.resize(other.nWorldAxes());
   refChange = False;
//
   if (other.nWorldAxes() ==0) {
      set_error(String("The supplied CoordinateSystem has no valid world axes"));
      return False;
   }
   if (nWorldAxes() ==0) {
      set_error(String("The current CoordinateSystem has no valid world axes"));
      return False;
   }


// Loop over "other" coordinates

   const uInt nCoord  =        nCoordinates();
   const uInt nCoord2 = other.nCoordinates();
   Vector<Bool> usedCoords(nCoord,False);
   for (uInt coord2=0; coord2<nCoord2; coord2++) {

// If all the world axes for this coordinate have been removed,
// we do not attempt to match with anything.

      if (!allEQ(other.worldAxes(coord2), -1)) {

      
// Try and find this coordinate type in "*this". If there
// is more than one coordinate of this type in "*this", we
// try them all looking for the first one that conforms.
// "other" may also contain more than one coordinate of a given
// type, so make sure we only use a coordinate in "*this" once

         for (uInt coord=0; coord<nCoord; coord++) {
            if (!usedCoords(coord)) {
               if (type(coord) == other.type(coord2)) {
                  if (mapOne(worldAxisMap, worldAxisTranspose, 
                             refChange, *this, other, coord, coord2)) {
                     usedCoords(coord) = True;
                     break;
                  }
               }
            }
         }

// break jumps here

      }
   }

   return True;
}




Bool CoordinateSystem::mapOne(Vector<Int>& worldAxisMap,
                              Vector<Int>& worldAxisTranspose,
                              Vector<Bool>& refChange,
                              const CoordinateSystem& cSys1,
                              const CoordinateSystem& cSys2,
                              const uInt coord1,
                              const uInt coord2) const
//
// Update the world axis mappings from one coordinate system to another.  
//
{

// Make tests on specific coordinate types here. We already
// know that the two cSys are the same coordinate type 
// (e.g. DIRECTION)

   Bool refDiff = False;
   if (cSys2.coordinate(coord2).type() == Coordinate::DIRECTION) {
      if (cSys1.directionCoordinate(coord1).directionType() != 
          cSys2.directionCoordinate(coord2).directionType()) {
         refDiff = True;
      }
   } else if (cSys2.coordinate(coord2).type() == Coordinate::SPECTRAL) {
      if (cSys1.spectralCoordinate(coord1).frequencySystem() != 
         cSys2.spectralCoordinate(coord2).frequencySystem()) {
         refDiff = True;
      }
   }


// How many world and pixel axes for these coordinates

   uInt nWorld1 = cSys1.worldAxes(coord1).nelements();
   uInt nWorld2 = cSys2.worldAxes(coord2).nelements();
   uInt nPixel1 = cSys1.pixelAxes(coord1).nelements();
   uInt nPixel2 = cSys2.pixelAxes(coord2).nelements();

// These tests should never fail

   if (nWorld1 != nWorld2) return False;
   if (nPixel1 != nPixel2) return False;

// Find their world  and pixel axes

   Vector<Int> world1 = cSys1.worldAxes(coord1);
   Vector<Int> pixel1 = cSys1.pixelAxes(coord1);
   Vector<Int> world2 = cSys2.worldAxes(coord2);
   Vector<Int> pixel2 = cSys2.pixelAxes(coord2);
 
// Compare quantities for the world axes.  

   for (uInt j=0; j<nWorld2; j++) {
      if (world2(j) != -1) {
         if (world1(j) != -1) {

// Compare intrinsic axis units

            if (Unit(cSys1.coordinate(coord1).worldAxisUnits()(j)) !=
                Unit(cSys2.coordinate(coord2).worldAxisUnits()(j))) return False;

// Set the world axis maps

            worldAxisMap(world2(j)) = world1(j);
            worldAxisTranspose(world1(j)) = world2(j);
            refChange(world1(j)) = refDiff;
         } else {

// The world axis is missing in cSys1 and present in cSys2.  

           return False;
         }

// The world axis has been removed in cSys2 so we aren't interested in it
     
      }
   }
//
   return True;
}





Bool CoordinateSystem::removeWorldAxis(uInt axis, Double replacement) 
{
    if (axis >= nWorldAxes()) {
       ostrstream oss;
       oss << "Illegal removal world axis number (" << axis << "), max is ("
           << nWorldAxes() << ")" << endl;
       set_error (String(oss));
       return False;
    }

// Remove the corresponding pixel axis (if there)..

    Int pixAxis = worldAxisToPixelAxis (axis);
    if (pixAxis >= 0) {

// Find pixel coordinate corresponding to world replacement value

       Vector<Double> world(referenceValue());
       world(axis) = replacement;
       Vector<Double> pixel(nPixelAxes());
       if (!toPixel(pixel, world)) return False;
//
       removePixelAxis (pixAxis, pixel(pixAxis));
    }

    const uInt nc = nCoordinates();

    Int coord, caxis;
    findWorldAxis(coord, caxis, axis);
    world_replacement_values_p[coord]->operator()(caxis) = replacement;
    world_maps_p[coord]->operator[](caxis) = -1;

    for (uInt i=0; i<nc; i++) {
	for (uInt j=0; j<world_maps_p[i]->nelements(); j++) {
	    if (world_maps_p[i]->operator[](j) > Int(axis)) {
		world_maps_p[i]->operator[](j)--;
	    }
	}
    }
   return True;
}

Bool CoordinateSystem::removePixelAxis(uInt axis, Double replacement) 
{
    if (axis >= nPixelAxes()) {
       ostrstream oss;
       oss << "Illegal removal pixel axis number (" << axis << "), max is ("
           << nPixelAxes() << ")" << endl;
       set_error (String(oss));
       return False;
    }


    const uInt nc = nCoordinates();

    Int coord, caxis;
    findPixelAxis(coord, caxis, axis);
    pixel_replacement_values_p[coord]->operator()(caxis) = replacement;
    pixel_maps_p[coord]->operator[](caxis) = -1;

    for (uInt i=0; i<nc; i++) {
	for (uInt j=0; j<pixel_maps_p[i]->nelements(); j++) {
	    if (pixel_maps_p[i]->operator[](j) > Int(axis)) {
		pixel_maps_p[i]->operator[](j)--;
	    }
	}
    }
   return True;
}


CoordinateSystem CoordinateSystem::subImage(const Vector<Int> &originShift,
					    const Vector<Int> &pixincFac) const
{
    AlwaysAssert(originShift.nelements() == nPixelAxes() &&
                 pixincFac.nelements() == nPixelAxes(), AipsError);

    // We could get rid of this assumption by multiplying by accounting for the PC
    // matrix as well as cdelt, or going through group-by-group, but it doesn't
    // seem necessary now, or maybe ever.
    AlwaysAssert(originShift.nelements() == pixincFac.nelements(), AipsError);

    uInt n = nPixelAxes();

    CoordinateSystem coords = *this;
    Vector<Double> crpix = coords.referencePixel();
    Vector<Double> cdelt = coords.increment();

    // Not efficient, but easy and this code shouldn't be called often
    for (uInt i=0; i<n; i++) {
        AlwaysAssert(pixincFac(i) >= 1, AipsError);
        crpix(i) -= originShift(i);
        crpix(i) /= pixincFac(i);
        cdelt(i) *= pixincFac(i);
    }
    coords.setReferencePixel(crpix);
    coords.setIncrement(cdelt);
    return coords;
}

void CoordinateSystem::restoreOriginal()
{
    CoordinateSystem coord;

    // Make a copy and then assign it back
    uInt n = coordinates_p.nelements();
    for (uInt i=0; i<n; i++) {
	coord.addCoordinate(*(coordinates_p[i]));
    }
    
    *this = coord;
}

uInt CoordinateSystem::nCoordinates() const
{
    return coordinates_p.nelements();
}

Coordinate::Type CoordinateSystem::type(uInt whichCoordinate) const
{
    AlwaysAssert(whichCoordinate<nCoordinates(), AipsError);
    return coordinates_p[whichCoordinate]->type();
}

String CoordinateSystem::showType(uInt whichCoordinate) const
{
    AlwaysAssert(whichCoordinate<nCoordinates(), AipsError);
    return coordinates_p[whichCoordinate]->showType();
}

const Coordinate& CoordinateSystem::coordinate(uInt which) const
{
    AlwaysAssert(which < nCoordinates(), AipsError);
    return *(coordinates_p[which]);
}

const LinearCoordinate& CoordinateSystem::linearCoordinate(uInt which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::LINEAR, AipsError);
    return (const LinearCoordinate &)(*(coordinates_p[which]));
}

const DirectionCoordinate& CoordinateSystem::directionCoordinate(uInt which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::DIRECTION, AipsError);
    return (const DirectionCoordinate &)(*(coordinates_p[which]));
}

const SpectralCoordinate& CoordinateSystem::spectralCoordinate(uInt which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::SPECTRAL, AipsError);
    return (const SpectralCoordinate &)(*(coordinates_p[which]));
}

const StokesCoordinate& CoordinateSystem::stokesCoordinate(uInt which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::STOKES, AipsError);
    return (const StokesCoordinate &)(*(coordinates_p[which]));
}

const TabularCoordinate& CoordinateSystem::tabularCoordinate(uInt which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::TABULAR, 
		 AipsError);
    return (const TabularCoordinate &)(*(coordinates_p[which]));
}

void CoordinateSystem::replaceCoordinate(const Coordinate &newCoordinate, uInt which)
{
    AlwaysAssert(which < nCoordinates() &&
		 newCoordinate.nPixelAxes() == coordinates_p[which]->nPixelAxes() &&
		 newCoordinate.nWorldAxes() == coordinates_p[which]->nWorldAxes(),
		 AipsError);
    delete coordinates_p[which];
    coordinates_p[which] = newCoordinate.clone();
    AlwaysAssert(coordinates_p[which], AipsError);
}


Int CoordinateSystem::findCoordinate(Coordinate::Type type, Int afterCoord) const
{
    if (afterCoord < -1) {
	afterCoord = -1;
    }

    Int n = nCoordinates();
    Bool found = False;
    while (++afterCoord < n) {
	if (coordinates_p[afterCoord]->type() == type) {
	    found = True;
	    break;
	}
    }
    if (found) {
	return afterCoord;
    } else {
	return -1;
    }
}

void CoordinateSystem::findWorldAxis(Int &coordinate, Int &axisInCoordinate, 
			  uInt axisInCoordinateSystem) const
{
    coordinate = axisInCoordinate = -1;

    AlwaysAssert(axisInCoordinateSystem < nWorldAxes(), AipsError);

    const uInt orig = axisInCoordinateSystem; // alias for less typing
    const uInt nc = nCoordinates();

    for (uInt i=0; i<nc; i++) {
	const uInt na = world_maps_p[i]->nelements();
	for (uInt j=0; j<na; j++) {
	    if (world_maps_p[i]->operator[](j) == Int(orig)) {
		coordinate = i;
		axisInCoordinate = j;
		return;
	    }
	}
    }
}

void CoordinateSystem::findPixelAxis(Int &coordinate, Int &axisInCoordinate, 
			  uInt axisInCoordinateSystem) const
{
    coordinate = axisInCoordinate = -1;

    AlwaysAssert(axisInCoordinateSystem < nPixelAxes(), AipsError);

    const uInt orig = axisInCoordinateSystem; // alias for less typing
    const uInt nc = nCoordinates();

    for (uInt i=0; i<nc; i++) {
	const uInt na = pixel_maps_p[i]->nelements();
	for (uInt j=0; j<na; j++) {
	    if (pixel_maps_p[i]->operator[](j) == Int(orig)) {
		coordinate = i;
		axisInCoordinate = j;
		return;
	    }
	}
    }
}

Int CoordinateSystem::pixelAxisToWorldAxis(uInt pixelAxis) const
{
   Int coordinate, axisInCoordinate;
   findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
   if (axisInCoordinate>=0 && coordinate>=0) {
      return worldAxes(coordinate)(axisInCoordinate);
   }
   return -1;
}

Int CoordinateSystem::worldAxisToPixelAxis(uInt worldAxis) const
{
   Int coordinate, axisInCoordinate;
   findWorldAxis(coordinate, axisInCoordinate, worldAxis);
   if (axisInCoordinate>=0 && coordinate>=0) {
      return pixelAxes(coordinate)(axisInCoordinate);
   }
   return -1;
}

Vector<Int> CoordinateSystem::worldAxes(uInt whichCoord) const
{
    // Implemented in terms of the public member functions. It would be more
    // efficient to use the private data, but would be harder to maintain.
    // This isn't apt to be called often, so choose the easier course.
    AlwaysAssert(whichCoord < nCoordinates(), AipsError);
    Vector<Int> retval(coordinate(whichCoord).nWorldAxes());

    retval = -1;  // Axes which aren't found must be removed!
    const uInt naxes = nWorldAxes();
    for (uInt i=0; i<naxes; i++) {
	Int coord, axis;
	findWorldAxis(coord, axis, i);
	if (coord == Int(whichCoord)) {
	    retval(axis) = i;
	}
    }
    return retval;
}

Vector<Int> CoordinateSystem::pixelAxes(uInt whichCoord) const
{
    AlwaysAssert(whichCoord < nCoordinates(), AipsError);
   Vector<Int> retval(coordinate(whichCoord).nPixelAxes());

    retval = -1;  // Axes which aren't found must be removed!
    const uInt naxes = nPixelAxes();
    for (uInt i=0; i<naxes; i++) {
	Int coord, axis;
	findPixelAxis(coord, axis, i);
	if (coord == Int(whichCoord)) {
	    retval(axis) = i;
	}
    }
    return retval;
}

Coordinate::Type CoordinateSystem::type() const
{
    return Coordinate::COORDSYS;
}

String CoordinateSystem::showType() const
{
    return String("System");
}

uInt CoordinateSystem::nWorldAxes() const
{
    uInt count = 0;
    const uInt nc = nCoordinates();
    for (uInt i=0; i<nc; i++) {
	const uInt na = world_maps_p[i]->nelements();
	for (uInt j=0; j<na; j++) {
	    if (world_maps_p[i]->operator[](j) >= 0) {
		count++;
	    }
	}
    }
    return count;
}

uInt CoordinateSystem::nPixelAxes() const
{
    uInt count = 0;
    const uInt nc = nCoordinates();
    for (uInt i=0; i<nc; i++) {
	const uInt na = pixel_maps_p[i]->nelements();
	for (uInt j=0; j<na; j++) {
	    if (pixel_maps_p[i]->operator[](j) >= 0) {
		count++;
	    }
	}
    }
    return count;
}

Bool CoordinateSystem::toWorld(Vector<Double> &world, 
			       const Vector<Double> &pixel) const
{
    AlwaysAssert(pixel.nelements() == nPixelAxes(), AipsError);
    if (world.nelements()!=nWorldAxes()) world.resize(nWorldAxes());

    const uInt nc = coordinates_p.nelements();
    Bool ok = True;
    for (uInt i=0; i<nc; i++) {

// For each coordinate, put the appropriate pixel or
// replacement values in the pixel temporary, call the
// coordinates own toWorld, and then copy the output values
// from the world temporary to the world coordinate

	const uInt npa = pixel_maps_p[i]->nelements();
	uInt j;
	for (j=0; j<npa; j++) {
	    Int where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) {

// cerr << "i j where " << i << " " << j << " " << where <<endl;

		pixel_tmps_p[i]->operator()(j) = pixel(where);
	    } else {
		pixel_tmps_p[i]->operator()(j) = 
		    pixel_replacement_values_p[i]->operator()(j);
	    }
	}

// cout << "world pixel map: " << *(world_maps_p[i]) << " " <<
// *(pixel_maps_p[i]) << endl;
// cout << "toWorld # " << i << "pix=" << *pixel_tmps_p[i] << endl;

	Bool oldok = ok;
	ok = coordinates_p[i]->toWorld(
		       *(world_tmps_p[i]), *(pixel_tmps_p[i]));

// cout << "toWorld # " << i << "wld=" << *world_tmps_p[i] << endl;

	if (!ok) {

// Transfer the error message. Note that if there is more than
// one error message this transfers the last one. I suppose this
// is as good as any.

	    set_error(coordinates_p[i]->errorMessage());
	}
	ok = ToBool(ok && oldok);
	const uInt nwa = world_maps_p[i]->nelements();
	for (j=0; j<nwa; j++) {
	    Int where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		world(where) = world_tmps_p[i]->operator()(j);
	    }
	}
    }

    return ok;
}

Bool CoordinateSystem::toWorld(Vector<Double> &world, 
			       const IPosition &pixel) const
{
    static Vector<Double> pixel_tmp;
    if (pixel_tmp.nelements()!=pixel.nelements()) pixel_tmp.resize(pixel.nelements());
//
    const uInt& n = pixel.nelements();
    for (uInt i=0; i<n; i++) {
	pixel_tmp(i) = pixel(i);
    }
    return toWorld(world, pixel_tmp);
}

Bool CoordinateSystem::toPixel(Vector<Double> &pixel, 
			       const Vector<Double> &world) const
{
    AlwaysAssert(world.nelements() == nWorldAxes(), AipsError);
    if (pixel.nelements()!=nPixelAxes()) pixel.resize(nPixelAxes());

    const uInt nc = coordinates_p.nelements();
    Bool ok = True;
    for (uInt i=0; i<nc; i++) {
	// For each coordinate, putt the appropriate world or replacement values
	// in the world temporary, call the coordinates own toPixel, and then
	// copy the output values from the pixel temporary to the pixel
	// coordinate
	const uInt nwra = world_maps_p[i]->nelements();
	uInt j;
	for (j=0; j<nwra; j++) {
	    Int where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		world_tmps_p[i]->operator()(j) = world(where);
	    } else {
		world_tmps_p[i]->operator()(j) = 
		    world_replacement_values_p[i]->operator()(j);
	    }
	}
	Bool oldok = ok;
	ok = coordinates_p[i]->toPixel(
			    *(pixel_tmps_p[i]), *(world_tmps_p[i]));
	if (!ok) {
	    // Transfer the error message. Note that if there is more than
	    // one error message this transfers the last one. I suppose this
	    // is as good as any.
	    set_error(coordinates_p[i]->errorMessage());
	}
	ok = ToBool(ok && oldok);
	const uInt npxa = pixel_maps_p[i]->nelements();
	for (j=0; j<npxa; j++) {
	    Int where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) {
		pixel(where) = pixel_tmps_p[i]->operator()(j);
	    }
	}
    }

    return ok;
}


Bool CoordinateSystem::toMix(Vector<Double>& worldOut,
                             Vector<Double>& pixelOut,
                             const Vector<Double>& worldIn,
                             const Vector<Double>& pixelIn, 
                             const Vector<Bool>& worldAxes,
                             const Vector<Bool>& pixelAxes,
                             const Vector<Double>& minWorld,
                             const Vector<Double>& maxWorld) const
{
   const uInt nWorld = worldAxes.nelements();
   const uInt nPixel = pixelAxes.nelements();
   AlwaysAssert(nWorld == nWorldAxes(), AipsError);
   AlwaysAssert(worldIn.nelements()==nWorld, AipsError);
   AlwaysAssert(nPixel == nPixelAxes(), AipsError);
   AlwaysAssert(pixelIn.nelements()==nPixel, AipsError);
   AlwaysAssert(minWorld.nelements()==nWorld, AipsError);
   AlwaysAssert(maxWorld.nelements()==nWorld, AipsError);
//
   const uInt nCoord = coordinates_p.nelements();
   if (worldOut.nelements()!=nWorldAxes()) worldOut.resize(nWorldAxes());
   if (pixelOut.nelements()!=nPixelAxes()) pixelOut.resize(nPixelAxes());

   for (uInt i=0; i<nCoord; i++) {

// For each coordinate, put the appropriate pixel or replacement values 
// in the pixel temporary, call the coordinates own toMix, and then copy 
// the output values from the world temporary to the world coordinate

      const uInt nAxes = world_maps_p[i]->nelements();
      const uInt nPixelAxes = pixel_maps_p[i]->nelements();
      AlwaysAssert(nAxes==nPixelAxes, AipsError);
//
      for (uInt j=0; j<nAxes; j++) {
         Int where = world_maps_p[i]->operator[](j);
         if (where >= 0) {
            world_tmps_p[i]->operator()(j) = worldIn(where);
            worldAxes_tmps_p[i]->operator()(j) = worldAxes(where);
            worldMin_tmps_p[i]->operator()(j) = minWorld(where);
            worldMax_tmps_p[i]->operator()(j) = maxWorld(where);
         } else {
//
// Axis removed.  
//
            world_tmps_p[i]->operator()(j) = 
               world_replacement_values_p[i]->operator()(j);
// 
// We have to decide what conversion to do (pixel<->world) for
// the removed axis.  For coupled axes like DirectionCOordinate,
// I do for the removed axis whatever I did for
// the unremoved axis, if there is one...  If both world
// axes are removed, ultinately it doesn't really matter what
// I do since the pixel axes will be gone as well, and there
// is nowhere to put the output !  For uncoupled axes it
// doesn't matter.
//
            if (type(i)==Coordinate::DIRECTION) {
               Vector<String> units(coordinate(i).worldAxisUnits());
//
               Int where2;
               if (j==0) {      // 0 or 1
                  where2 = world_maps_p[i]->operator[](1);
//
                  Quantum<Double> tmp180(180.0, Unit("deg"));
                  worldMin_tmps_p[i]->operator()(0) = -tmp180.getValue(units(0));
                  worldMax_tmps_p[i]->operator()(0) =  tmp180.getValue(units(0));
               } else {
                  where2 = world_maps_p[i]->operator[](0);
//
                  Quantum<Double> tmp90(90.0, Unit("deg"));
                  worldMin_tmps_p[i]->operator()(1) = -tmp90.getValue(units(1));
                  worldMax_tmps_p[i]->operator()(1) =  tmp90.getValue(units(1));
               }
               if (where2 >= 0) {
                  worldAxes_tmps_p[i]->operator()(j) = worldAxes(where2);
               } else {
                  worldAxes_tmps_p[i]->operator()(j) = False;
               }
            } else {
               worldAxes_tmps_p[i]->operator()(j) = False;
//
// worldMin/Max irrelevant except for DirectionCoordinate
//
            }
         }
      }
//
      for (uInt j=0; j<nAxes; j++) {
         Int where = pixel_maps_p[i]->operator[](j);
         if (where >= 0) {
            pixel_tmps_p[i]->operator()(j) = pixelIn(where);
            pixelAxes_tmps_p[i]->operator()(j) = pixelAxes(where);
         } else {
            pixel_tmps_p[i]->operator()(j) = 
               pixel_replacement_values_p[i]->operator()(j);
// 
// Here I assume nPixelAxes=nWorldAxes and the order
// is the same. This is the truth as far as I know it.
//
               pixelAxes_tmps_p[i]->operator()(j) = !worldAxes_tmps_p[i]->operator()(j);    
         }
      }
//
/*
      Vector<Double> w = *(world_tmps_p[i]);
      Vector<Double> p = *(pixel_tmps_p[i]);
      cout << "worldIn, pixelIn = " << w << p << endl;
      cout << "worldAxes2, pixelAxes2" << worldAxes2 << pixelAxes2 << endl;
*/

      if (!coordinates_p[i]->toMix(*(worldOut_tmps_p[i]), *(pixelOut_tmps_p[i]),
		       *(world_tmps_p[i]), *(pixel_tmps_p[i]),
                       *(worldAxes_tmps_p[i]), *(pixelAxes_tmps_p[i]), 
                       *(worldMin_tmps_p[i]), *(worldMax_tmps_p[i]))) {
         set_error(coordinates_p[i]->errorMessage());
         return False;
      }
//
      for (uInt j=0; j<nAxes; j++) {
         Int where = world_maps_p[i]->operator[](j);
         if (where>=0) worldOut(where) = worldOut_tmps_p[i]->operator()(j);
         where = pixel_maps_p[i]->operator[](j);
         if (where>=0) pixelOut(where) = pixelOut_tmps_p[i]->operator()(j);
      }
   }
   return True;
}

Vector<String> CoordinateSystem::worldAxisNames() const
{
    Vector<String> retval(nWorldAxes());
    for (uInt i=0; i<retval.nelements(); i++) {
	Int coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<String> tmp = coordinates_p[coord]->worldAxisNames();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}




Vector<String> CoordinateSystem::worldAxisUnits() const
{
    Vector<String> retval(nWorldAxes());
    for (uInt i=0; i<retval.nelements(); i++) {
	Int coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<String> tmp = coordinates_p[coord]->worldAxisUnits();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

Vector<Double> CoordinateSystem::referencePixel() const
{
    Vector<Double> retval(nPixelAxes());
    for (uInt i=0; i<retval.nelements(); i++) {
	Int coord, coordAxis;
	findPixelAxis(coord, coordAxis, i);
	Vector<Double> tmp = coordinates_p[coord]->referencePixel();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

Matrix<Double> CoordinateSystem::linearTransform() const
{
    uInt nr = nWorldAxes();
    uInt nc = nPixelAxes();

    Matrix<Double> retval(nr,nc);
    retval = 0.0;

    for (uInt i=0; i<nr; i++) {
	for (uInt j=0; j<nc; j++) {
	    Int worldCoord, worldAxis, pixelCoord, pixelAxis;
	    findWorldAxis(worldCoord, worldAxis, i);
	    findPixelAxis(pixelCoord, pixelAxis, j);
	    // By definition, only axes in the same coordinate may be coupled
	    if (worldCoord == pixelCoord &&
		worldCoord >= 0 && worldAxis >= 0 && pixelAxis >= 0) {
		Matrix<Double> tmp = 
		    coordinates_p[worldCoord]->linearTransform();
		retval(i,j) = tmp(worldAxis, pixelAxis);
	    }
	}
    }
    return retval;
}

Vector<Double> CoordinateSystem::increment() const
{
    Vector<Double> retval(nWorldAxes());
    for (uInt i=0; i<retval.nelements(); i++) {
	Int coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<Double> tmp = coordinates_p[coord]->increment();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

Vector<Double> CoordinateSystem::referenceValue() const
{
    Vector<Double> retval(nWorldAxes());
    for (uInt i=0; i<retval.nelements(); i++) {
	Int coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<Double> tmp = coordinates_p[coord]->referenceValue();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

Bool CoordinateSystem::setWorldAxisNames(const Vector<String> &names)
{
    Bool ok = ToBool(names.nelements()==nWorldAxes());
    if (!ok) {
      set_error("names vector must be of length nWorldAxes()");
      return False;
    }
//
    const uInt nc = nCoordinates();
    for (uInt i=0; i<nc; i++) {
	Vector<String> tmp = coordinates_p[i]->worldAxisNames();
	const uInt na = tmp.nelements();
	for (uInt j=0; j<na; j++) {
	    Int which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = names(which);
	    }
	}
	ok = ToBool(coordinates_p[i]->setWorldAxisNames(tmp) && ok);
    }

    return ok;
}

Bool CoordinateSystem::setWorldAxisUnits(const Vector<String> &units)
{
    Bool ok = ToBool(units.nelements()==nWorldAxes());
    if (!ok) {
      set_error("units vector must be of length nWorldAxes()");
      return False;
    }
//
    const uInt nc = nCoordinates();
    for (uInt i=0; i<nc; i++) {
	Vector<String> tmp = coordinates_p[i]->worldAxisUnits();
	uInt na = tmp.nelements();
	for (uInt j=0; j<na; j++) {
	    Int which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = units(which);
	    }
	}
	ok = ToBool(coordinates_p[i]->setWorldAxisUnits(tmp) && ok);
    }

    return ok;
}

Bool CoordinateSystem::setReferencePixel(const Vector<Double> &refPix)
{
    Bool ok = ToBool(refPix.nelements()==nPixelAxes());
    if (!ok) {
      set_error("ref. pix vector must be of length nPixelAxes()");
      return False;
    }
//
    const uInt nc = nCoordinates();
    for (uInt i=0; i<nc; i++) {
	Vector<Double> tmp = coordinates_p[i]->referencePixel();
	uInt na = tmp.nelements();
	for (uInt j=0; j<na; j++) {
	    Int which = pixel_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = refPix(which);
	    }
	}
	ok = ToBool(coordinates_p[i]->setReferencePixel(tmp) && ok);
    }

    return ok;
}

Bool CoordinateSystem::setLinearTransform(const Matrix<Double> &xform)
{
    const uInt nc = nCoordinates();
    Bool ok = True;
    for (uInt i=0; i<nc; i++) {
	Matrix<Double> tmp = coordinates_p[i]->linearTransform();
	uInt nrow = tmp.nrow();
	uInt ncol = tmp.ncolumn();
	for (uInt j=0; j<nrow; j++) {
	    for (uInt k=0; k<ncol; k++) {
		Int whichrow = world_maps_p[i]->operator[](j);
		Int whichcol = pixel_maps_p[i]->operator[](k);
		if (whichrow >= 0 && whichcol >= 0) {
		    tmp(j,k) = xform(whichrow,whichcol);
		}
	    }
	}
	ok = ToBool(coordinates_p[i]->setLinearTransform(tmp) && ok);
    }
    return ok;
}

Bool CoordinateSystem::setIncrement(const Vector<Double> &inc)
{
    Bool ok = ToBool(inc.nelements()==nWorldAxes());
    if (!ok) {
      set_error("increment vector must be of length nWorldAxes()");
      return False;
    }
//
    const uInt nc = nCoordinates();
    for (uInt i=0; i<nc; i++) {
	Vector<Double> tmp = coordinates_p[i]->increment();
	uInt na = tmp.nelements();
	for (uInt j=0; j<na; j++) {
	    Int which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = inc(which);
	    }
	}
	ok = ToBool(coordinates_p[i]->setIncrement(tmp) && ok);
    }

    return ok;
}

Bool CoordinateSystem::setReferenceValue(const Vector<Double> &refval)
{
    Bool ok = ToBool(refval.nelements()==nWorldAxes());
    if (!ok) {
      set_error("ref. val vector must be of length nWorldAxes()");
      return False;
    }
//
    const uInt nc = nCoordinates();
    for (uInt i=0; i<nc; i++) {
	Vector<Double> tmp = coordinates_p[i]->referenceValue();
	uInt na = tmp.nelements();
	for (uInt j=0; j<na; j++) {
	    Int which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = refval(which);
	    }
	}
	ok = ToBool(coordinates_p[i]->setReferenceValue(tmp) && ok);
    }

    return ok;
}


Bool CoordinateSystem::near(const Coordinate& other, 
                            Double tol) const
//
// Compare this CoordinateSystem with another. 
//
{
   Vector<Int> excludePixelAxes;
   return near(other,excludePixelAxes,tol);
}


Bool CoordinateSystem::near(const Coordinate& other, 
                            const Vector<Int>& excludePixelAxes,
                            Double tol) const
//
// Compare this CoordinateSystem with another. 
//
// Do not compare axis descriptors on the specified pixel axes; 
// a dubious thing to do.
// 
//
// The separation of world axes and pixel axes, and the ability to
// remove axes makes this function a great big mess.
//
{
// Basic checks

   if (this->type() != other.type()) {
      set_error("Comparison is not with another CoordinateSystem");
      return False;
   }

   const CoordinateSystem& cSys = dynamic_cast<const CoordinateSystem&>(other);

   if (nCoordinates() != cSys.nCoordinates()) {
      set_error("The CoordinateSystems have different numbers of coordinates");
      return False;
   }

   if (nPixelAxes() != cSys.nPixelAxes()) {
      set_error("The CoordinateSystems have different numbers of pixel axes");
      return False;
   }
   if (nWorldAxes() != cSys.nWorldAxes()) {
      set_error("The CoordinateSystems have different numbers of world axes");
      return False;
   }



// Loop over number of coordinates

   ostrstream oss;
   for (Int i=0; i<Int(nCoordinates()); i++) {

// Although the coordinates are checked for their types in
// the coordinate comparison routines, we can save ourselves
// some time by checking here too

      if (coordinate(i).type() != cSys.coordinate(i).type()) {
         oss << "The coordinate types differ for coordinate number " << i << ends;
         set_error(String(oss));
         return False;
      }

// Find which pixel axes in the CoordinateSystem this coordinate
// inhabits and compare the vectors.   Here we don't take into 
// account the exclusion axes vector; that's only used when we are 
// actually comparing the axis descriptor values on certain axes

      if (pixelAxes(i).nelements() != cSys.pixelAxes(i).nelements()) {
         oss << "The number of pixel axes differs for coordinate number " << i << ends;
         set_error(String(oss));
         return False;
      }
      if (!allEQ(pixelAxes(i), cSys.pixelAxes(i))) {
         oss << "The pixel axes differ for coordinate number " << i << ends;
         set_error(String(oss));
         return False;
      }

// Find which world axes in the CoordinateSystem this
// coordinate inhabits and compare the vectors
    
      if (worldAxes(i).nelements() != cSys.worldAxes(i).nelements()) {
         oss << "The number of world axes differs for coordinate number " << i << ends;
         set_error(String(oss));
         return False;
      }
      if (!allEQ(worldAxes(i), cSys.worldAxes(i))) {
         oss << "The world axes differ for coordinate number " << i << ends;
         set_error(String(oss));
         return False;
      }
 

// Were all the world axes for this coordinate removed ? If so
// we don't check it

      Bool allGone = True;
      Int j;
      for (j=0; j<Int(worldAxes(i).nelements()); j++) {
         if (worldAxes(i)(j) >= 0) {
            allGone = False;
            break;
         }
      }
      

// Continue if we have some unremoved world axes in this coordinate

      Int excSize = coordinate(i).nPixelAxes();
      Vector<Int> excludeAxes(excSize);
      if (!allGone) {

// If any of the list of CoordinateSystem exclusion pixel axes
// inhabit this coordinate, make a list of the axes in this
// coordinate that they correspond to.  

         Int coord, axisInCoord;
         Int k = 0;
         for (j=0; j<Int(excludePixelAxes.nelements()); j++) {

// Any invalid excludePixelAxes are dealt with here.  If they are
// rubbish, we just don't find them ! 

            findPixelAxis(coord, axisInCoord, excludePixelAxes(j));
            if (coord == i) {

// OK, this pixel axis is in this coordinate, so stick it in the list
// We may have to resize if the stupid user has given us duplicates
// in the list of exclusion axes

               if (k == Int(excludeAxes.nelements())) {
                  Int n = Int(excludeAxes.nelements()) + excSize;
                  excludeAxes.resize(n,True);
               }
               excludeAxes(k++) = axisInCoord;
            }
         }
         excludeAxes.resize(k,True);


// Now, for the current coordinate, convert the world axes in 
// the CoordinateSystems to axes in the current coordinate
// and compare the two 

         Int coord1, coord2, axisInCoord1, axisInCoord2;
         for (j=0; j<Int(worldAxes(i).nelements()); j++) {
            if (worldAxes(i)(j) >= 0) {

// Not removed (can't find it if it's been removed !)
  
                     findWorldAxis(coord1, axisInCoord1, worldAxes(i)(j));
               cSys.findWorldAxis(coord2, axisInCoord2, worldAxes(i)(j));

// This better not happen !  

               if (coord1 != coord2) {
                  oss << "The coordinate numbers differ (!!) for coordinate number "
                      << i << ends;
                  set_error(String(oss));
                  return False;
               }

// This might
               if (axisInCoord1 != axisInCoord2) {
                  oss << "World axis " << j << " in the CoordinateSystems"
                      << "has a different axis number in coordinate number "
                      << i << ends;
                  set_error(String(oss));
                  return False;
               }
            }
         }
         
// Now, finally, compare the current coordinate from the two 
// CoordinateSystems except on the specified axes. Leave it
// this function to set the error message

         if (!coordinate(i).near(cSys.coordinate(i),excludeAxes,tol)) {
           set_error(coordinate(i).errorMessage());
           return False;
         }
      }
   }
   return True;
}




String CoordinateSystem::format(String& units,
                                Coordinate::formatType format,
                                Double worldValue,
                                uInt worldAxis,
                                Bool absolute,
                                Int precision,
                                Bool native) const   
{
    AlwaysAssert(worldAxis < nWorldAxes(), AipsError);
// 
    Int coord, axis;
    findWorldAxis(coord, axis, worldAxis);
     
// Should never fail  

    AlwaysAssert(coord>=0 && axis >= 0, AipsError);
    
    return coordinate(coord).format(units, format, worldValue, axis, 
                                    absolute, precision, native);
}

ObsInfo CoordinateSystem::obsInfo() const
{
    return obsinfo_p;
}

void CoordinateSystem::setObsInfo(const ObsInfo &obsinfo)
{
    obsinfo_p = obsinfo;
}

Bool CoordinateSystem::save(RecordInterface &container,
			    const String &fieldName) const
{
    Record subrec;
    if (container.isDefined(fieldName)) {
       set_error(String("The fieldName is already defined in the supplied record"));
       return False;
    }

    uInt nc = coordinates_p.nelements();
    if (nc==0) {
       set_error(String("The CoordinateSystem is empty"));
       return False;
    }
//
    for (uInt i=0; i<nc; i++)
    {
	// Write eaach string into a field it's type plus coordinate
	// number, e.g. direction0
	String basename = "unknown";
	switch (coordinates_p[i]->type()) {
	case Coordinate::LINEAR:    basename = "linear"; break;
	case Coordinate::DIRECTION: basename = "direction"; break;
	case Coordinate::SPECTRAL:  basename = "spectral"; break;
	case Coordinate::STOKES:    basename = "stokes"; break;
	case Coordinate::TABULAR:    basename = "tabular"; break;
	case Coordinate::COORDSYS:  basename = "coordsys"; break;
	}
	ostrstream onum;
	onum << i;
	String num = onum;
	String name = basename + num;
	coordinates_p[i]->save(subrec, name);
	name = String("worldmap") + num;
	subrec.define(name, Vector<Int>(*world_maps_p[i]));
	name = String("worldreplace") + num;
	subrec.define(name, Vector<Double>(*world_replacement_values_p[i]));
	name = String("pixelmap") + num;
	subrec.define(name, Vector<Int>(*pixel_maps_p[i]));
	name = String("pixelreplace") + num;
	subrec.define(name, Vector<Double>(*pixel_replacement_values_p[i]));
    }
    // Write the obsinfo
    String error;
    Bool ok = obsinfo_p.toRecord(error, subrec);
    if (!ok) {
       set_error (error);
       return False;
    }

    // Write some of the info out again in a different order in a 
    // more convenient format for use.  This is used in regionmanager.g
    // for making of region objects.  It is not required when restoring
    // the CS from the record.  These things are written out
    // in *pixel axis* order

    Vector<String> axisUnits(nPixelAxes());
    Vector<String> axisNames(nPixelAxes());
    Vector<String> coordinateTypes(nPixelAxes());
    Int c, axisInCoordinate, worldAxis;
    for (uInt i=0; i<nPixelAxes(); i++) {
       findPixelAxis(c, axisInCoordinate, i);
       coordinateTypes(i) = coordinate(c).showType();
#
       worldAxis = pixelAxisToWorldAxis(i);
       if (worldAxis != -1) {
          axisUnits(i) = worldAxisUnits()(worldAxis);
          axisNames(i) = worldAxisNames()(worldAxis);
       } else {
          axisUnits(i) = "removed";
          axisNames(i) = "removed";
       }
   }
   Vector<Int> toWorldMap(nPixelAxes());
   for (uInt i=0; i<nPixelAxes(); i++) {
      toWorldMap(i) = pixelAxisToWorldAxis(i);
   }
   Vector<Int> toPixelMap(nWorldAxes());
   for (uInt i=0; i<nWorldAxes(); i++) {
      toPixelMap(i) = worldAxisToPixelAxis(i);
   }
   subrec.define("nPixelAxes",Int(nPixelAxes()));
   subrec.define("nWorldAxes",Int(nWorldAxes()));
   subrec.define("axisUnits", axisUnits);
   subrec.define("axisNames", axisNames);
   subrec.define("coordinateTypes", coordinateTypes);
   subrec.define("toWorldAxisMap", toWorldMap);
   subrec.define("toPixelAxisMap", toPixelMap);
#
    if (ok) {
	container.defineRecord(fieldName, subrec);
    }    

    return ok;
}

CoordinateSystem* CoordinateSystem::restore(const RecordInterface &container,
                                            const String &fieldName)
{
    CoordinateSystem *retval = 0;

// Handle an empty field name

    Record subrec;
    if (fieldName.empty()) {
       subrec = Record(container);
    } else {
       if (container.isDefined(fieldName)) {
          subrec = Record(container.asRecord(fieldName));
       } else {
 	  return retval;
       }
    }
//

    PtrBlock<Coordinate *> tmp;
    Int nc = 0;                         // num coordinates
    PtrBlock<Coordinate *> coords;
    String linear = "linear";
    String direction = "direction";
    String spectral = "spectral";
    String stokes = "stokes";
    String tabular = "tabular";
    String coordsys = "coordsys";
    while(True) {
	ostrstream onum;
	onum << nc;
	String num = onum;
	nc++;
	if (subrec.isDefined(linear + num)) {
	    coords.resize(nc);
	    coords[nc - 1] = LinearCoordinate::restore(subrec, linear+num);
	} else if (subrec.isDefined(direction + num)) {
	    coords.resize(nc);
	    coords[nc - 1] = 
		DirectionCoordinate::restore(subrec, direction+num);
	} else if (subrec.isDefined(spectral + num)) {
	    coords.resize(nc);
	    coords[nc - 1] = SpectralCoordinate::restore(subrec, spectral+num);
	} else if (subrec.isDefined(stokes + num)) {
	    coords.resize(nc);
	    coords[nc - 1] = StokesCoordinate::restore(subrec, stokes+num);
	} else if (subrec.isDefined(tabular + num)) {
	    coords.resize(nc);
	    coords[nc - 1] = TabularCoordinate::restore(subrec, tabular+num);
	} else if (subrec.isDefined(coordsys + num)) {
	    coords.resize(nc);
	    coords[nc - 1] = CoordinateSystem::restore(subrec, coordsys+num);
	} else {
	    break;
	}
	AlwaysAssert(coords[nc-1] != 0, AipsError);
    }
    nc = coords.nelements();
//
    retval = new CoordinateSystem;
    Int i;
    for (i=0; i<nc; i++) {
	retval->addCoordinate(*(coords[i]));
	delete coords[i];
	coords[i] = 0;
    }
    for (i=0; i<nc; i++) {
//
// Copy values
//
	ostrstream onum;
	onum << i;
	Vector<Int> dummy;
	String num(onum), name;
	name = String("worldmap") + num;
	subrec.get(name, dummy);
	dummy.toBlock(*(retval->world_maps_p[i]));
	name = String("worldreplace") + num;
	subrec.get(name, *(retval->world_replacement_values_p[i]));
	name = String("pixelmap") + num;
	subrec.get(name, dummy);
	dummy.toBlock(*(retval->pixel_maps_p[i]));
	name = String("pixelreplace") + num;
	subrec.get(name, *(retval->pixel_replacement_values_p[i]));
    }
//
// Get the obsinfo
//
    String error;
    Bool ok = retval->obsinfo_p.fromRecord(error, subrec);
    AlwaysAssert(ok, AipsError); // Should never happen

    return retval;
}


Coordinate *CoordinateSystem::clone() const
{
    return new CoordinateSystem(*this);
}



Bool CoordinateSystem::toFITSHeader(RecordInterface &header, 
				    IPosition &shape,
				    Bool oneRelative,
				    char prefix, Bool writeWCS,
				    Bool preferVelocity, 
				    Bool opticalVelocity) const
{
    LogIO os(LogOrigin("CoordinateSystem", "toFITSHeader", WHERE));

// If we have any tabular axes that aren't pure linear report that the
// table will be lost.

    Int tabCoord = -1;
    while ((tabCoord = findCoordinate(Coordinate::TABULAR, tabCoord)) > 0) {
	if (tabularCoordinate(tabCoord).pixelValues().nelements() > 0) {
	    os << LogIO::WARN <<
		"Note: Your coordinate system has one or more TABULAR axes.\n"
		"The lookup table will be lost in the conversion to FITS, and\n"
		"will be replaced by averaged (i.e. linearized) axes." <<
		LogIO::POST;
	    break;
	}
    }

// Validation

    const Int n = nWorldAxes();
    String sprefix = prefix;
    if (header.isDefined(sprefix + "rval") ||
	header.isDefined(sprefix + "rpix") ||
	header.isDefined(sprefix + "delt") ||
	header.isDefined(sprefix + "type") ||
	header.isDefined(sprefix + "unit")) {
	os << LogIO::SEVERE << "Already contains one or more of *rval, *rpix, "
	    "*delt, *type, *unit";
	return False;
    }

    Double offset = 0.0;
    if (oneRelative) {
	offset = 1.0;
    }

// Canonicalize units and find sky axes

    CoordinateSystem coordsys = *this;

// Find the sky coordinate, if any

    Int skyCoord = coordsys.findCoordinate(Coordinate::DIRECTION);
    Int longAxis = -1, latAxis = -1;

// Find the spectral axis, if any

    Int specCoord = coordsys.findCoordinate(Coordinate::SPECTRAL);
    Int specAxis = -1;
    
// Find the stokes axis, if any

    Int stokesCoord = coordsys.findCoordinate(Coordinate::STOKES);
    Int stokesAxis = -1;
    Int i;
    for (i=0; i<n ; i++) {
	Int c, a;
	coordsys.findWorldAxis(c, a, i);
	if (c == skyCoord) {
	    if (a == 0) {
		longAxis = i;
	    } else if (a == 1) {
		latAxis = i;
	    }
	} else if (c == specCoord) {
	    specAxis = i;
	} else if (c == stokesCoord) {
	    stokesAxis = i;
	}
    }

// change the units to degrees for the sky axes

    Vector<String> units = coordsys.worldAxisUnits();
    if (longAxis >= 0) units(longAxis) = "deg";
    if (latAxis >= 0) units(latAxis) = "deg";
    if (specAxis >= 0) units(specAxis) = "Hz";
    if (stokesAxis >= 0) units(stokesAxis) = "";
    coordsys.setWorldAxisUnits(units);

// Generate keywords

    Vector<Double> crval, crpix, cdelt, projp, crota;
    Vector<String> ctype, cunit;
    Matrix<Double> pc;
    Bool isNCP = False;
    if (!toFITSHeaderGenerateKeywords (os, isNCP, crval, crpix, cdelt, crota, projp, 
                                       ctype, cunit, pc, coordsys, skyCoord, 
                                       longAxis, latAxis, specAxis, stokesAxis,
                                       writeWCS, offset, sprefix)) {
       return False;
    }

// Special stokes handling

    if (stokesCoord >= 0) {
        if (!toFITSHeaderStokes (crval, crpix, cdelt, os, coordsys,
                                 stokesAxis, stokesCoord)) return False;
    }


// If there are more world than pixel axes, we will need to add
// degenerate pixel axes and modify the shape.

    if (Int(nPixelAxes()) < n) {
	IPosition shapetmp = shape; shape.resize(n);
	Vector<Double> crpixtmp = crpix.copy(); crpix.resize(n);
	Int count = 0;
	for (Int worldAxis=0; worldAxis<n; worldAxis++) {
	    Int coordinate, axisInCoordinate;
	    coordsys.findWorldAxis(coordinate, axisInCoordinate, worldAxis);
	    Int pixelAxis = coordsys.pixelAxes(coordinate)(axisInCoordinate);
	    if (pixelAxis >= 0) {
		// We have a pixel axis
		shape(worldAxis) = shapetmp(count);
		crpix(worldAxis) = crpixtmp(count);
		count++;
	    } else {
		// No corresponding pixel axis.
		shape(worldAxis) = 1;
		crpix(worldAxis) = 1.0;
	    }
	}
    }

// Try to work out the epoch/equinox

    if (skyCoord >= 0) {
	MDirection::Types radecsys = 
	    directionCoordinate(skyCoord).directionType();
	Double equinox = -1.0;
	switch(radecsys) {
	case MDirection::J2000:
	    equinox = 2000.0;
	    break;
	case MDirection::B1950:
	    equinox = 1950.0;
	    break;
	default:
	    ; // Nothing
	}
	if (equinox > 0) {
	    if (writeWCS) {
		header.define("equinox", equinox);
	    } else {
		header.define("epoch", equinox);
	    }
	}
    }

// Actually write the header

    if (writeWCS && Int(coordsys.nPixelAxes()) == n) {
	header.define("pc", pc);
    } else if (writeWCS) {
	os << LogIO::SEVERE << "writeWCS && nPixelAxes() != n. Requires "
	  "development!!!"  << LogIO::POST;
    }

    header.define(sprefix + "type", ctype);
    header.define(sprefix + "rval", crval);
    header.define(sprefix + "delt", cdelt);
    header.define(sprefix + "rota", crota);
    header.define(sprefix + "rpix", crpix);
    header.define(sprefix + "unit", cunit);
    if (!isNCP && projp.nelements() > 0) {
	if (!writeWCS) {
	    for (uInt k=0; k<projp.nelements(); k++) {
		if (!::nearAbs(projp(k), 0.0)) {
		    os << LogIO::NORMAL << 
			"PROJPn not all zero.Information lost in FITS"
			" conversion. Try WCS?." <<
			LogIO::POST;
		    break;
		}
	    }
	}
	if (writeWCS) {
	    header.define("projp", projp);
	}
    }
    if (specAxis > 1) {
	const SpectralCoordinate &spec = spectralCoordinate(specCoord);
	spec.toFITS(header, specAxis, os, oneRelative, preferVelocity, 
		    opticalVelocity);
    }

// Write out the obsinfo

    String error;
    Bool ok = obsinfo_p.toFITS(error, header);
    if (!ok) {
	os << LogIO::SEVERE << "Error converting ObsInfo: " << error << 
	    LogIO::POST;
    }

    return ok;
}


Bool CoordinateSystem::toFITSHeaderStokes(Vector<Double>& crval,
                                          Vector<Double>& crpix,
                                          Vector<Double>& cdelt,
                                          LogIO& os,
                                          const CoordinateSystem& coordsys,
                                          Int stokesAxis, Int stokesCoord) const
{
   Vector<Int> stokes(coordsys.stokesCoordinate(stokesCoord).stokes());
   Int inc = 1;
   Bool inorder = True;
   if (stokes.nelements() > 1) {
      inc = Stokes::FITSValue(Stokes::StokesTypes(stokes(1))) - 
            Stokes::FITSValue(Stokes::StokesTypes(stokes(0)));
      for (uInt k=2; k<stokes.nelements(); k++) {
         if ((Stokes::FITSValue(Stokes::StokesTypes(stokes(k))) - 
              Stokes::FITSValue(Stokes::StokesTypes(stokes(k-1)))) !=
              inc) {
            inorder = False;
         }
      }
   }
   if (inorder) {
      crval(stokesAxis) = Stokes::FITSValue(Stokes::StokesTypes(stokes(0)));
      crpix(stokesAxis) = 1;
      cdelt(stokesAxis) = inc;
   } else {

// The idea here is to write non-standard records, to indicate something
// funny, and then write the rest of the Stokes axis as non-standard
// keywords.  Since fromFITSHeader can't decode this anyway, for now
// return False.

/*
      crval(stokesAxis) = Stokes::FITSValue(Stokes::StokesTypes(stokes(0))) + 200;
      crpix(stokesAxis) = 1;
      cdelt(stokesAxis) = 1;
*/

      os << LogIO::SEVERE 
         <<  "The Stokes coordinate in this CoordinateSystem is too" << endl;
      os << LogIO::SEVERE 
         << "complex to convert to the FITS convention" << LogIO::POST;
      return False;
   }
//
   return True;
}


Bool CoordinateSystem::toFITSHeaderGenerateKeywords (LogIO& os, 
                                                     Bool& isNCP,
                                                     Vector<Double>& crval,
                                                     Vector<Double>& crpix,
                                                     Vector<Double>& cdelt,
                                                     Vector<Double>& crota,
                                                     Vector<Double>& projp,
                                                     Vector<String>& ctype,
                                                     Vector<String>& cunit,
                                                     Matrix<Double>& pc,
						     const CoordinateSystem& coordsys,
                                                     Int skyCoord, 
                                                     Int longAxis, Int latAxis,
                                                     Int specAxis, Int stokesAxis,
                                                     Bool writeWCS, Double offset,
                                                     const String& sprefix) const
{
   const Int n = nWorldAxes();
   crval = coordsys.referenceValue();
   crpix = coordsys.referencePixel() + offset;
   cdelt = coordsys.increment();
   if (skyCoord >= 0) {
      projp = coordsys.directionCoordinate(skyCoord).projection().parameters();
   }
   Vector<String> cctype(2);
   if (skyCoord>=0 && !writeWCS) {
      if (latAxis>=0) {
         const DirectionCoordinate &dc = coordsys.directionCoordinate(skyCoord);
         cctype = make_Direction_FITS_ctype (isNCP, dc.projection(), 
                                             DirectionCoordinate::axisNames(dc.directionType(),
                                             True),
                                             C::pi/180.0*crval(latAxis), True);
      } else {
         os << LogIO::SEVERE 
            << "Cannot handle conversion to WCS for DirectionCoordinate with  lat axis removed"
            << LogIO::POST;
         return False;
      }
   }
//
   ctype = coordsys.worldAxisNames();
   for (Int i=0; i < n; i++) {
      if ((i == longAxis || i == latAxis) && writeWCS) {
         const DirectionCoordinate &dc = coordsys.directionCoordinate(skyCoord);
         String name = dc.axisNames(dc.directionType(), True)(i==latAxis);
         while (name.length() < 4) name += "-";
         name = name + "-" + dc.projection().name();
         ctype(i) = name.chars();
      } else if (i == longAxis || i == latAxis) { // && !writeWCS
         if (i==longAxis) {
            ctype(i) = cctype(0);
         } else {
            ctype(i) = cctype(1);
         }
      } else if (i == specAxis) {
	    // Nothing - will be handled in SpectralCoordinate
      } else if (i == stokesAxis) {
         ctype(i) = "STOKES  ";
      } else {
         ctype(i).upcase();
         if (ctype(i).length() > 8) {
            ctype(i) = ctype(i).at(0,8);
         }
         while (ctype(i).length() < 8) ctype(i) += " ";
      }
   }

//
   cunit = coordsys.worldAxisUnits();
   for (Int i=0; i<n; i++) {
      cunit(i).upcase();
      if (cunit(i).length() > 8) {
         cunit(i) = cunit(i).at(0,8);
      }
      while (cunit(i).length() < 8) cunit(i) += " ";
   }
//
   pc = linearTransform();

// crota: See Greisen and Calabretta "Converting Previous Formats"

   crota.resize(n);
   crota = 0;
   if (longAxis >= 0 && latAxis >= 0) {
      Double rholong = atan2(pc(latAxis, longAxis)*C::pi/180.0,
			pc(longAxis, longAxis)*C::pi/180.0)*180.0/C::pi;
      Double rholat = atan2(-pc(longAxis, latAxis)*C::pi/180.0,
			pc(latAxis, latAxis)*C::pi/180.0)*180.0/C::pi;
      crota(latAxis) = (rholong + rholat)/2.0;
      if (!::near(rholong, rholat)) {
         os << LogIO::WARN << sprefix + "rota is not very accurate." 
            <<  " PC matrix is not a pure rotation.";
         if (!writeWCS) {
            os << endl << "Consider writing the DRAFT WCS convention to avoid losing information.";
         }
         os << LogIO::POST;
      }
   }
   return True;
}


Bool CoordinateSystem::fromFITSHeader(CoordinateSystem &coordsys, 
				      const RecordInterface &header,
                                      const IPosition& shape,
				      Bool oneRelative,
				      char prefix)
{
    LogIO os(LogOrigin("CoordinateSystem", "fromFITSHeader", WHERE));

    if (coordsys.nCoordinates() != 0) {
	CoordinateSystem empty;
	coordsys = empty;
    }

    String sprefix = prefix;
    Double offset = 0.0;
    if (oneRelative) {
	offset = 1.0;
    }

// FITS angular units are degrees

    Vector<Double> cdelt, crval, crpix;
    Vector<Int> naxes;
    Vector<String> ctype, cunit;
    Matrix<Double> pc;
    Bool haveUnit = False;
    Int rotationAxis = -1;
    Bool hasCD = False;
    Int n = 0;
    try {
	header.get(sprefix + "rval", crval);
	header.get(sprefix + "rpix", crpix);
	crpix -= offset;
	header.get(sprefix + "type", ctype);
        n = ctype.nelements();

	// Units are optional
	if (header.isDefined(sprefix + "unit")) {
	    header.get(sprefix + "unit", cunit);
	    UnitMap::addFITS();
	    haveUnit = True;
	}

	// PC, CD (PC*CDELT) and/or CROTA is optional. We prefer CD if it is defined.
        // We will use variable "pc" to house the true PC matrix, or the CD matrix

	hasCD = getCDFromHeader(pc, n, header);
	if (hasCD) {
           if (header.isDefined(sprefix + "delt")) {
              os << LogIO::NORMAL << "Ignoring meaningless " << sprefix << "delt because CD cards present" << LogIO::POST;
           }
           cdelt.resize(pc.nrow());
           cdelt = 1.0;               // degrees
        } else {

// Get cdelt  and PC from header
   
           header.get(sprefix + "delt", cdelt);
           getPCFromHeader(os, rotationAxis, pc, n, header, sprefix);
       }  
    } catch (AipsError x) {
	os << LogIO::WARN << "Error retrieving *rval, *rpix, *delt, *type "
	    "from header";
	return False;
    } 

 
// Make some consistency checks.  We don't rely on naxis, rather we 
// look at the length of the descriptor vectors.  If some missing 
// values, give guesses

    if (Int(crval.nelements()) != n) {
       Int n2 = crval.nelements();
       crval.resize(n,True);
       for (Int i=n2; i<n; i++) crval(i) = 0.0;
       os << LogIO::WARN << "Padding missing crval values with 0.0" << LogIO::POST;
    }
    if (!hasCD && Int(cdelt.nelements()) != n) {
       Int n2 = cdelt.nelements();
       cdelt.resize(n,True);
       for (Int i=n2; i<n; i++) cdelt(i) = 1.0;       // Degrees
       os << LogIO::WARN << "Padding missing cdelt values with 1.0deg" << LogIO::POST;
    }
    if (Int(crpix.nelements()) != n) {
       Int n2 = crpix.nelements();
       crpix.resize(n,True);
       for (Int i=n2; i<n; i++) crpix(i) = 1.0;
       os << LogIO::WARN << "Padding missing crpix values with 1.0" << LogIO::POST;
    }
//
    if (Int(pc.nrow()) != n ||  Int(pc.ncolumn()) != n ||
	(cunit.nelements() > 0 && Int(cunit.nelements()) != n)) {
	os << LogIO::SEVERE << "Inconsistent number of axes in header";
	return False;
    }

// OK, find out what standard axes we have.

    Int longAxis=-1, latAxis=-1, stokesAxis=-1, specAxis=-1;
    Int i;
    for (i=0; i<n; i++) {
        String subRA(ctype(i).at(0,2));
        String subDEC(ctype(i).at(0,3));
	if (subRA==String("RA") || ctype(i).contains("LON")) {
	    if (longAxis >= 0) {
		os << LogIO::SEVERE << "More than one longitude axis is "
		    "present in header!";
		return False;
	    }
	    longAxis = i;
	} else if (subDEC==String("DEC") || ctype(i).contains("LAT")) {
	    if (latAxis >= 0) {
		os << LogIO::SEVERE << "More than one latitude axis is "
		    "present in header!";
		return False; // we already have a latitude axis!
	    }
	    latAxis = i;
	} else if (ctype(i).contains("STOKES")) {
	    stokesAxis = i;
	} else if (ctype(i).contains("FREQ") || 
		   ctype(i).contains("FELO") ||
		   ctype(i).contains("VELO")) {
	    specAxis = i;
	}
    }

// We must have longitude AND latitude

    if (longAxis >= 0 && latAxis < 0) {
	os << LogIO::SEVERE << "We have a longitude axis but no latitude axis!";
	return False; 
    }
    if (latAxis >= 0 && longAxis < 0) {
	os << LogIO::SEVERE << "We have a latitude axis but no longitude axis!";
	return False; 
    }

// Sanity check that PC is only non-diagonal for the longitude and
// latitude axes.

    for (Int j=0; j<n; j++) {
	for (i=0; i<n; i++) {
	    if (i == j) {
		continue;
	    } else {
		if (!::near(pc(i,j), 0.0)) {
		    if (rotationAxis < 0 || (i == longAxis && j == latAxis) ||
			(i == latAxis  && j == longAxis)) {
			continue;
		    } else {
			os << LogIO::WARN << sprefix + "rota may only" <<
			    " be set for longitude/latitude axes" << 
			    LogIO::POST;
		    }
		}
	    }
	}
    }

// DIRECTION

    if (longAxis >= 0) {
        String proj = ctype(longAxis);
        Bool isGalactic = False;
	if (proj.contains("GLON")) {
	    isGalactic = True;
	}

// Get rid of the first 4 characters, e.g., RA--

	proj.gsub(Regex("^...."), "");  
	String proj2 = ctype(latAxis);
	proj2.gsub(Regex("^...."), "");  

// Get rid of leading -'s

	proj.gsub(Regex("^-*"), "");
	proj2.gsub(Regex("^-*"), "");
	proj.gsub(Regex(" *"), "");    // Get rid of spaces
	proj2.gsub(Regex(" *"), "");
	if (proj == "" && proj2 == "") {

// Default to cartesian if no projection is defined.

	    proj = Projection::name(Projection::CAR); proj2 = proj;
	    os << WHERE << LogIO::WARN << 
	      "No projection has been defined (e.g., SIN), assuming\n"
	      "cartesian (CAR). Some FITS readers will not recognize\n"
	       "the CAR projection." << LogIO::POST;
	}
	if (proj != proj2) {

// Maybe instead I should switch to CAR, or use the first?

	    os << LogIO::SEVERE << "Longitude and latitude axes have different"
	        " projections (" << proj << "!=" << proj2 << ")" << LogIO::POST;
	    return False;
	}

// OK, let's make our Direction coordinate and add it to the
// coordinate system. We'll worry about transposing later. FITS
// should always be degrees, but if the units are set we'll honor
// them.

// First, work out what the projection actually is.
// Special case NCP - now SIN with  parameters

	Vector<Double> projp;
	Projection::Type ptype;
	
	if (proj == "NCP") {
	    os << LogIO::NORMAL << "NCP projection is now SIN projection in"
		" WCS.\nOld FITS readers will not handle this correctly." <<
	        LogIO::POST;
	    ptype = Projection::SIN;
	    projp.resize(2);

// According to Greisen and Calabretta

	    projp(0) = 0.0;
	    projp(1) = 1.0/tan(crval(latAxis)*C::pi/180.0);
	} else {
	    ptype = Projection::type(proj);
	    if (ptype == Projection::N_PROJ) {
		os << LogIO::SEVERE << "Unknown projection: (" << proj << ")";
		return False;
	    }
	    if (header.isDefined("projp")) {
		header.get("projp", projp);
	    }
	}

// OK, now try making the projection

	Projection projn;
	try {
	    projn = Projection(ptype, projp);
	} catch (AipsError x) {
	    os << LogIO::WARN << "Error forming projection, maybe the "
		"wrong number of parameters\n(" << x.getMesg() << ")" << 
		LogIO::POST;
	    return False;
	} 

// Assume the units are degrees unless we are told otherwise

	Double toRadX = C::pi/180.0;
	Double toRadY = toRadX;
	if (cunit.nelements() > 0) {
	    Unit longu = cunit(longAxis);
	    Unit latu = cunit(latAxis);
	    Unit rad = "rad";
	    if (longu.getValue() != rad.getValue() ||
		latu.getValue() != rad.getValue()) {
		os << LogIO::SEVERE << "Longitude or latitude units are unknwon "
		    "or incompatible with angle (" << cunit(longAxis) <<
		    "," << cunit(latAxis) << ")" << LogIO::POST;
	    }
	    toRadX = longu.getValue().getFac()/rad.getValue().getFac();
	    toRadY = latu.getValue().getFac()/rad.getValue().getFac();
	}
	
// DEFAULT

	MDirection::Types radecsys = MDirection::J2000;
	if (isGalactic) {
	    radecsys = MDirection::GALACTIC;
	} else {
	    if (header.isDefined("epoch") && 
		(header.dataType("epoch") == TpDouble || 
		 header.dataType("epoch") == TpFloat || 
		 header.dataType("epoch") == TpInt)) {
		Double epoch = header.asdouble("epoch");
		if (::near(epoch, 1950.0)) {
		    radecsys = MDirection::B1950;
		} else if (::near(epoch, 2000.0)) {
		    radecsys = MDirection::J2000;
		}
	    } else if (header.isDefined("equinox") && 
		       (header.dataType("equinox") == TpDouble ||
			header.dataType("equinox") == TpDouble ||
			header.dataType("equinox") == TpInt)) {
		Double epoch = header.asdouble("equinox");
		if (::near(epoch, 1950.0)) {
		    radecsys = MDirection::B1950;
		} else if (::near(epoch, 2000.0)) {
		    radecsys = MDirection::J2000;
		}
	    } else {
		os << LogIO::NORMAL << "Could not find or figure out the "
		    "equinox from the FITS header, using J2000" << LogIO::POST;
	    }
	}	

	Matrix<Double> dirpc(2,2);
	dirpc(0,0) = pc(longAxis, longAxis);
	dirpc(0,1) = pc(longAxis, latAxis);
	dirpc(1,0) = pc(latAxis, longAxis);
	dirpc(1,1) = pc(latAxis, latAxis);

// watch for cdelt=0 - its okay if that axis is degenerate
// and (crpix+offset)=1 and rotationAxis < 0 = i.e. the only
// pixel on that axis is the reference pixel and there is
// no rotation specified - then cdelt=1 on that axis.  If that
// isn't done, that coord. can't be constructed because the
// PC matrix will be reported as singular since its first 
// multiplied by cdelt before its used and in this case, that
// doesn't matter since other pixels on that axis are never used.

        if (!hasCD) {
          if (::near(cdelt(latAxis), 0.0) && 
              ::near(crpix(latAxis)+offset, 1.0) && rotationAxis < 0) {
             cdelt(latAxis) = 1.0;            // degrees
          }
//
          if (::near(cdelt(longAxis), 0.0) && 
              ::near(crpix(longAxis)+offset, 1.0) && rotationAxis < 0) {
             cdelt(longAxis) = 1.0;          // degrees
          }
        }

	DirectionCoordinate dir(radecsys,
				projn,
				crval(longAxis)*toRadX,	crval(latAxis)*toRadY,
				cdelt(longAxis)*toRadX, cdelt(latAxis)*toRadY,
				dirpc,
				crpix(longAxis), crpix(latAxis));
	coordsys.addCoordinate(dir);
    }

// STOKES.   shape is used only here as the StokesCoordinate
// is a bit peculiar, and not really separable from the shape

    if (stokesAxis >= 0) {
        if (shape(stokesAxis)>4) {
          os << "Stokes axis longer than 4 pixels.  This is not acceptable" 
             << LogIO::EXCEPTION;       
          return False;
        }
	Vector<Int> stokes(shape(stokesAxis)); 
//
        Bool foundI = False;
	for (Int k=0; k<shape(stokesAxis); k++) {

// crpix is 0-relative

	    Double tmp = crval(stokesAxis) + 
		(k - crpix(stokesAxis))*cdelt(stokesAxis);
	    if (tmp >= 0) {
		stokes(k) = Int(tmp + 0.01);
	    } else {
		stokes(k) = Int(tmp - 0.01);
	    }
//
   	    switch (stokes(k)) {
            case 1:  
             {
                if (foundI) {
                   os << LogIO::SEVERE
                      << "Stokes I already used for this image, possibly for a beam" << endl;
                   os << "Cannot continue building the StokesCoordinate" << LogIO::POST;
                   return False;
                }
//
                stokes(k) = Stokes::I; 
                foundI = True;
                break;
             }
	    case 2: 
              stokes(k) = Stokes::Q; 
              break;
	    case 3: 
              stokes(k) = Stokes::U; 
              break;
	    case 4: 
              stokes(k) = Stokes::V; 
              break;
	    case -1: 
              stokes(k) = Stokes::RR; 
              break;
	    case -2: 
              stokes(k) = Stokes::LL; 
              break;
	    case -3: 
              stokes(k) = Stokes::RL; 
              break;
	    case -4: 
              stokes(k) = Stokes::LR; 
              break;
	    case -5: 
              stokes(k) = Stokes::XX; 
              break;
	    case -6: 
              stokes(k) = Stokes::YY; 
              break;
	    case -7: 
              stokes(k) = Stokes::XY; 
              break;
	    case -8: 
              stokes(k) = Stokes::YX; 
              break;
            case 0:
             {
              if (!foundI) {
                 stokes(k) = Stokes::I;
                 os << LogIO::WARN 
                    << "Detected Stokes coordinate = 0; this is an unoffical" << endl;
                 os << "Convention for an image containing a beam.  Assuming Stokes=I" << endl;
                 os << "Better would be to write your FITS image with the correct Stokes" << LogIO::POST;
                 foundI = True;
              } else {
                 os << LogIO::SEVERE
                    << "Detected Stokes coordinate = 0; this is an unoffical" << endl;
                 os << "Convention for an image containing a beam.  Cannot assume Stokes=I" << endl;
                 os << "for it because Stokes I has already been used for this image" << endl;
                 os << "Cannot continue building the StokesCoordinate" << LogIO::POST;
                 return False;
              }
              break;
             }
            case 5:

// Percentage linear polarization not supported

              {
                 os << LogIO::SEVERE << "The FITS image Stokes axis has the unofficial percentage polarization value." << endl;
                 os << "This is not supported.  Will use fractional polarization instead " << endl;
                 os << "You must scale the image by 0.01" << LogIO::POST;
                 stokes(k) = Stokes::PFlinear;
                 break;
              }
            case 6:
              stokes(k) = Stokes::PFlinear;
              break;
            case 7:
              stokes(k) = Stokes::Pangle;
              break;
            case 8:

// Spectral index not supported

              {
                 os << LogIO::SEVERE << "The FITS image Stokes axis has the unofficial spectal index value." << endl;
                 os << "This is not supported, cannot continue building the StokesCoordinate" << LogIO::POST;
                 return False;
              }
            case 9:

// Optical depth not supported

              {
                 os << LogIO::SEVERE << "The Stokes axis has the unofficial optical depth" << endl;
                 os << "value.  This is not supported, cannot continue building the StokesCoordinate" << LogIO::POST;
                 return False;
              }
	    default:
              os << LogIO::SEVERE << "A Stokes coordinate of " << stokes(k) 
                 << " was detected; this is not valid." << endl;
              os << "Cannot continue building the StokesCoordinate" << LogIO::POST;
              return False;
	    }
	}
	try {
	    StokesCoordinate sc(stokes);
	    coordsys.addCoordinate(sc);
	} catch (AipsError x) {
	    os << "Error forming stokes axis : " << x.getMesg() <<
		LogIO::SEVERE << LogIO::POST;
	    return False;
	} 
    }

// SPECTRAL

    if (specAxis >= 0) {

// Will be overwritten or ignored.

	SpectralCoordinate tmp;
	String error;
	if (SpectralCoordinate::fromFITS(tmp, error, header, specAxis,
					 os)) {
	    coordsys.addCoordinate(tmp);
	} else {
	    os << LogIO::WARN << "Cannot convert apparent spectral axis " <<
		ctype(specAxis) << " into a true spectral coordinate (error="
	       << error << "). Turning it into a linear axis." << LogIO::POST;
	    specAxis = -1;
	}
    }

// Remaining axes are LINEAR

    uInt nlin = n;
    if (longAxis >= 0) {nlin--;}
    if (latAxis >= 0) {nlin--;}
    if (specAxis >= 0) {nlin--;}
    if (stokesAxis >= 0) {nlin--;}
    if (nlin > 0) {
        if (nlin > 1) {
	    os << LogIO::NORMAL << 
	      "Assuming no rotation/skew/... in linear axes." 
	       << LogIO::POST;
	}
	Matrix<Double> linpc(nlin, nlin); linpc = 0; linpc.diagonal() = 1.0;
	Vector<Double> lincrpix(nlin), lincdelt(nlin), lincrval(nlin);
	Vector<String> linctype(nlin), lincunit(nlin);
	Int where_i = 0;
	for (i=0; i<n; i++) {
	    if (i != longAxis && i != latAxis && i != stokesAxis &&
		i != specAxis) {
		lincrpix(where_i) = crpix(i);
		lincrval(where_i) = crval(i);
		lincdelt(where_i) = cdelt(i);
		linctype(where_i) = ctype(i);
		if (cunit.nelements() > 0) {
		    lincunit(where_i) = cunit(i);
		} else if (specAxis < 0 && (ctype(i).contains("FELO") ||
					    ctype(i).contains("VELO"))) {
		  lincunit(where_i) = "m/s";
		}
		where_i++;
	    }
	}
	Int where_j = 0;
	for (Int j=0; j<n; j++) {
	    where_i = 0;
	    if (j != longAxis && j != latAxis && j != stokesAxis &&
		j != specAxis) {
		for (i=0; i<n; i++) {
		    if (i != longAxis && i != latAxis && i != stokesAxis &&
			i != specAxis) {
			linpc(where_i, where_j) = pc(i,j);
			where_i++;
		    }
		}
		where_j++;
	    }
	}
	LinearCoordinate lc(linctype, lincunit, lincrval, lincdelt,
			    linpc, lincrpix);
	coordsys.addCoordinate(lc);
    }


// Now we need to work out the transpose order

    Vector<Int> order(n);
    Int nspecial = 0;
    if (longAxis >= 0) nspecial++;
    if (latAxis >= 0) nspecial++;
    if (stokesAxis >= 0) nspecial++;
    if (specAxis >= 0) nspecial++;

    Int linused = 0;
    for (i=0; i<n; i++) {
	if (i == longAxis) {
           order(i) = 0; // long is always first if it exist
	} else if (i == latAxis) {
	   order(i) = 1; // lat is always second if it exists
        } else if (i == stokesAxis) {
           if (longAxis >= 0) { // stokes is axis 0 if no dir, otherwise 2
              order(i) = 2;
           } else {
              order(i) = 0;
           }
	} else if (i == specAxis) {
           if (longAxis >= 0 && stokesAxis >= 0) {
              order(i) = 3; // stokes and dir
           } else if (longAxis >= 0) {
              order(i) = 2; // dir only
           } else if (stokesAxis >= 0) {
              order(i) = 0; // neither stokes or dir
           }
	} else {
           order(i) = nspecial + linused;
           linused++;
	}
    }
//
    coordsys.transpose(order, order);
    ObsInfo oi;
    String error;
    Bool ok = oi.fromFITS(error, header);
    if (ok) {
	coordsys.setObsInfo(oi);
    } else {
	os << LogIO::SEVERE << 
	    "Error reading ObsInfo: " << error << LogIO::POST;
    }
    return ok;
}


Coordinate* CoordinateSystem::makeFourierCoordinate (const Vector<Bool>& axes,
                                                     const Vector<Int>& shape) const
{
   LogIO os(LogOrigin("CoordinateSystem", "makeFourierCoordinate", WHERE));
//
   if (axes.nelements() != nPixelAxes()) {  
      throw (AipsError("Invalid number of specified pixel axes"));
   } 
   if (axes.nelements()==0) {
      throw (AipsError("There are no pixel axes in this CoordinateSystem"));
   }
//
   if (allEQ(axes,False)) {
      throw (AipsError("You have not specified any axes to transform"));
   }
//
   if (shape.nelements() != nPixelAxes()) {
      throw (AipsError("Invalid number of elements in shape"));
   }

// Make a copy of the CS.  The caste is safe.

   Coordinate* pC = clone();
   CoordinateSystem* pCS = dynamic_cast<CoordinateSystem*>(pC);
//
   uInt nReplaced = 0;
   const uInt nCoord = nCoordinates();
   for (uInt i=0; i<nCoord; i++) {

// Are there some axes True for this coordinate and are their
// world/pixel axes not removed ?

      if (checkAxesInThisCoordinate(axes, i)) {

// Find the coordinate-based axes and shape vectors

         nReplaced++;
         Vector<Int> coordSysAxes = pixelAxes(i);
         Vector<Bool> coordAxes(coordSysAxes.nelements(),False);
         Vector<Int> coordShape(coordAxes.nelements(),0);
//
         for (uInt j=0; j<coordSysAxes.nelements(); j++) {
            if (axes(coordSysAxes(j))) coordAxes(j) = True;
            coordShape(j) = shape(coordSysAxes(j));        
         }

// Make Fourier coordinate

         const Coordinate& coord = coordinate(i);
         Coordinate* pC2 = coord.makeFourierCoordinate(coordAxes, coordShape);

// Replace in CS.  Note we don't change any pixel/world axis mappings
// or removal lists

         pCS->replaceCoordinate(*pC2, i);
         delete pC2;
      }
   }
//
   pCS = 0;
   return pC;
}


Bool CoordinateSystem::checkAxesInThisCoordinate(const Vector<Bool>& axes, uInt which) const
//
// 1) See if this coordinate has any axes to be FTd
// 2) Make sure they are all good.
//
{
   LogIO os(LogOrigin("CoordinateSystem", "checkAxesInThisCoordinate", WHERE));
//
   Bool wantIt = False;

// Loop over pixel axes in the coordinatesystem

   Int coord, axisInCoord, worldAxis;
   for (uInt i=0; i<axes.nelements(); i++) {

// For axes the user wants to FT, find the coordinate

      if (axes(i)) {
         findPixelAxis(coord, axisInCoord, i);

// It should not be possible for the pixel axis to be missing,
// because that means the user gave a wrong length vector
// for "axes" in makeFourierCoordinate and that has already been
// checked

         if (coord<0) {
            ostrstream oss;
            oss << "Pixel axis " << axes(i) << " has been removed" << endl;
            os << String(oss) << LogIO::EXCEPTION;
         }

// Is it this coordinate ?

         if (coord==Int(which)) {
            wantIt = True;

// If the world axis has been removed, issue a warning.  It doesn't 
// actually matter to the Coordinate that is doing the FT (doesn't
// know the CS has removed one of its world axes).  Possibly better for
// the user to remain ignorant on this one !

            worldAxis = pixelAxisToWorldAxis(i);
            if (worldAxis<0) {
//               ostrstream oss;
//               oss << "World axis for pixel axis " << axes(i) << " has been removed" << endl;
//               os << LogIO::WARN << String(oss) << endl;
//               os << LogIO::WARN << "This does not affect the Fourier Transform" << LogIO::POST;
            }
         }
      }
   }
   return wantIt;
}


Bool CoordinateSystem::getCDFromHeader(Matrix<Double>& cd, uInt n, const RecordInterface& header) 
//
// We have to read the CDj_i cards and ultimately pack them into the 
// WCS linprm structure in the right order.  
// The expected order in WCS linprm is 
//
//  lin.pc = {CD1_1, CD1_2, CD2_1, CD2_2}  ...
//
// You can get this via
//
//  pc[2][2] = {{CD1_1, CD1_2},
//              {CD2_1, CD2_2}}
//
// which is to say,
//
//  pc[0][0] = CD1_1,
//  pc[0][1] = CD1_2,
//  pc[1][0] = CD2_1,
//  pc[1][1] = CD2_2,
//
// for which the storage order is
//
//  CD1_1, CD1_2, CD2_1, CD2_2
//
// so linprm will be happy if you set 
//
//  lin.pc = *pc;
//
// This packing and unpacking actually happens in
// LinearXform::set_linprm and LinearXform::pc
//
// as we stuff the CD matrix inro the PC matrix 
// and set cdelt = 1 deg
//
{
   cd.resize(n,n);
   cd = 0.0;
   cd.diagonal() = 1.0;
//
   for (uInt i=0; i<n; i++) {
      for (uInt j=0; j<n; j++) {
         ostrstream oss;
         oss << "cd" << j+1 << "_" << i+1;
         String field(oss);
  	 if (header.isDefined(field)) {         
            header.get(field, cd(i,j));
         } else {
            cd.resize(0,0);
            return False;
         }
      }
   }
   return True;
}


void CoordinateSystem::getPCFromHeader(LogIO& os, Int& rotationAxis, 
                                       Matrix<Double>& pc, 
                                       uInt n, 
                                       const RecordInterface& header,
                                       const String& sprefix)
{
   if (header.isDefined("pc")) {

// Unlikely to encounter this, as the current WCS papers
// use the CD rather than PC matrix. The aips++ user binding
// (Image tool) does not allow the WCS definition to be written
// so probably we could remove this

      if (header.isDefined(sprefix + "rota")) {
         os << "Ignoring redundant " << sprefix << "rota in favour of "
	       "pc matrix." << LogIO::NORMAL << LogIO::POST;
      }
      header.get("pc", pc);
      if (pc.ncolumn() != pc.nrow()) {
         os << "The PC matrix must be square" << LogIO::EXCEPTION;
      }
   } else if (header.isDefined(sprefix + "rota")) {
      Vector<Double> crota;
      header.get(sprefix + "rota", crota);

// Turn crota into PC matrix

      pc.resize(crota.nelements(), crota.nelements());
      pc = 0.0;
      pc.diagonal() = 1.0;

// We can only handle one non-zero angle

      for (uInt i=0; i<crota.nelements(); i++) {
         if (!::near(crota(i), 0.0)) {
            if (rotationAxis >= 0) {
               os << LogIO::SEVERE << "Can only convert one non-zero"
                   " angle from " << sprefix << 
                   "rota to pc matrix. Using the first." << LogIO::POST;
            } else {
               rotationAxis = i;
            }
         }
      }
//
      if (rotationAxis >= 0 && pc.nrow() > 1) { // can't rotate 1D!
         if (rotationAxis > 0) {
            pc(rotationAxis-1,rotationAxis-1) =
               pc(rotationAxis,rotationAxis) = cos(crota(rotationAxis)*C::pi/180.0);
            pc(rotationAxis-1,rotationAxis)=
               -sin(crota(rotationAxis)*C::pi/180.0);
            pc(rotationAxis,rotationAxis-1)=
               sin(crota(rotationAxis)*C::pi/180.0);
         } else {
            os << LogIO::NORMAL << "Unusual to rotate about first"
		   " axis." << LogIO::POST;
            pc(rotationAxis+1,rotationAxis+1) =
               pc(rotationAxis,rotationAxis) = cos(crota(rotationAxis)*C::pi/180.0);

// Might be backwards?

            pc(rotationAxis+1,rotationAxis)=-sin(crota(rotationAxis)*C::pi/180.0);
            pc(rotationAxis,rotationAxis+1)= sin(crota(rotationAxis)*C::pi/180.0);
         }
      }
   } else {

// Pure diagonal PC matrix

      pc.resize(n, n);
      pc = 0.0;
      pc.diagonal() = 1.0;
   }
}




void CoordinateSystem::list (LogIO& os,
                             MDoppler::Types velocityType,
                             Bool nativeFormat, 
                             const IPosition& latticeShape,
                             const IPosition& tileShape) const
//
// List information about a CoordinateSystem to the logger
//
// Input:
//   velocityType  Speciy velocity definition
//   nativeFormat  If True, reference values and axis increments
//                 are formatted in their native format.  E.g.
//                 RA and DEC in radians.  Otherwise, they are
//                 possibly converted to some other unit and
//                 formatted nicely (e.g. HH:MM:SS.S)
//
{
   os << LogIO::NORMAL << endl;

// List DirectionCoordinate type from the first DirectionCoordinate we find

   listDirectionSystem(os);

// List rest frequency and reference frame from the first spectral axis we find

   listFrequencySystem(os, velocityType);

//
   os << "Telescope        : " << obsinfo_p.telescope() << endl;
   os << "Observer         : " << obsinfo_p.observer() << endl;
//
   MEpoch epoch = obsinfo_p.obsDate();
   if (epoch.getValue().getDay() != Double(0.0)) { 
      MVTime time = MVTime(epoch.getValue());
      os << "Date observation : " << time.string(MVTime::YMD) << endl;
   } else {
      os << "Date observation : " << "UNKNOWN" << endl;
   }
   os << endl;


// Determine the widths for all the fields that we want to list

   Bool doShape = tileShape.nelements()>0 && 
                  latticeShape.nelements()>0 &&
                  tileShape.nelements()==latticeShape.nelements();
   uInt widthName, widthProj, widthShape, widthTile, widthRefValue;
   uInt widthRefPixel, widthInc, widthUnits, totWidth;

   String nameName, nameProj, nameShape, nameTile, nameRefValue;
   String nameRefPixel, nameInc, nameUnits;
   
   Int precRefValSci, precRefValFloat, precRefValRADEC;
   Int precRefPixFloat, precIncSci;
   getFieldWidths (os, widthName, widthProj, widthShape, widthTile,
                   widthRefValue, widthRefPixel, widthInc,
                   widthUnits, precRefValSci, precRefValFloat, 
                   precRefValRADEC, precRefPixFloat,
                   precIncSci, nameName, nameProj, nameShape, 
                   nameTile, nameRefValue, nameRefPixel, nameInc, 
                   nameUnits, nativeFormat, velocityType, latticeShape,
                   tileShape);

// Write headers

   os.output().fill(' ');
   os.output().setf(ios::left, ios::adjustfield);

   os.output().width(widthName);
   os << nameName;

   os.output().setf(ios::right, ios::adjustfield);
   os.output().width(widthProj);
   os << nameProj;

   if (doShape) {
      os.output().width(widthShape);
      os << nameShape;

      os.output().width(widthTile);
      os << nameTile;
   }

   os.output().width(widthRefValue);
   os << nameRefValue;

   os.output().width(widthRefPixel);
   os << nameRefPixel;

   os.output().width(widthInc);
   os << nameInc;

   os << nameUnits << endl;

   totWidth = widthName + widthProj + widthShape + widthTile +
              widthRefValue + widthRefPixel + widthInc + widthUnits;
   os.output().fill('-');
   os.output().width(totWidth);
   os.output().setf(ios::right, ios::adjustfield);
   os << " " << endl;
   os.output() << setfill(' ');


// Loop over the number of pixel axes in the coordinate system (same
// as number of axes in image) 

   uInt pixelAxis;
   Int axisInCoordinate, coordinate;
   for (pixelAxis=0; pixelAxis<nPixelAxes(); pixelAxis++) {

// Find coordinate number for this pixel axis
 
      findPixelAxis(coordinate, axisInCoordinate, pixelAxis);

// List it

      Coordinate* pc = CoordinateSystem::coordinate(coordinate).clone();
//      cout << "type = " << pc->type() << endl;

      listHeader(os, pc, widthName, widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 False, axisInCoordinate, pixelAxis, nativeFormat,
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci, latticeShape, tileShape);

// If the axis is spectral, we might like to see it as
// velocity as well as frequency.  Since the listing is row
// based, we have to do it like this.  Urk.

      if (pc->type() == Coordinate::SPECTRAL) {
         listVelocity (os, pc, widthName, widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 False, axisInCoordinate, pixelAxis, velocityType,
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci);

      }
      delete pc;
   }
   os << endl;


// Now find those pixel axes that have been removed and list their
// associated coordinate information.

   uInt worldAxis;
   for (worldAxis=0; worldAxis<nWorldAxes(); worldAxis++) {


// Find coordinate number for this pixel axis
 
      findWorldAxis(coordinate, axisInCoordinate, worldAxis);


// See if this world axis has an associated removed pixel axis
      
      Vector<Int> pixelAxes = CoordinateSystem::pixelAxes(coordinate);
      if (pixelAxes(axisInCoordinate) == -1) {

// List it

        Coordinate* pc = CoordinateSystem::coordinate(coordinate).clone();
        listHeader(os, pc, widthName, widthProj, widthShape, 
                   widthTile, widthRefValue, widthRefPixel, 
                   widthInc, widthUnits, False, axisInCoordinate, 
                   -1, nativeFormat, precRefValSci, precRefValFloat, 
                   precRefValRADEC, precRefPixFloat, precIncSci, 
                   latticeShape, tileShape);
        delete pc;
     }
   }


// Post it

   os.post();
}

void CoordinateSystem::getFieldWidths (LogIO& os, uInt& widthName, uInt& widthProj,  uInt& widthShape,               
                                       uInt& widthTile, uInt& widthRefValue, uInt& widthRefPixel, 
                                       uInt& widthInc, uInt& widthUnits,  Int& precRefValSci, 
                                       Int& precRefValFloat, Int& precRefValRADEC, Int& precRefPixFloat, 
                                       Int& precIncSci, String& nameName, String& nameProj,
                                       String& nameShape, String& nameTile, String& nameRefValue,
                                       String& nameRefPixel, String& nameInc, String& nameUnits,
                                       Bool nativeFormat, MDoppler::Types velocityType,
                                       const IPosition& latticeShape, const IPosition& tileShape) const
//
// All these silly format and precision things should really be
// in  a little class, but I can't be bothered !
//
{

// Precision for scientific notation, floating notation,
// HH:MM:SS.SSS and sDD:MM:SS.SSS for the reference value formatting.
// Precision for the reference pixel and increment formatting.

   precRefValSci = 6;
   precRefValFloat = 3;
   precRefValRADEC = 3;
   precRefPixFloat = 2;
   precIncSci = 6;   
   Bool doShape = tileShape.nelements()>0 && 
                  latticeShape.nelements()>0 &&
                  tileShape.nelements()==latticeShape.nelements();

// Header names for fields

   nameName = "Name";
   nameProj = "Proj";
   nameShape = "Shape";
   nameTile = "Tile";
   nameRefValue = "Coord value";
   nameRefPixel = "at pixel";
   nameInc = "Coord incr";
   nameUnits = " Units";

// Initialize (logger will never be actually used in this function)

   widthName = widthProj = widthShape = widthTile = widthRefValue = 0;
   widthRefPixel = widthInc = widthUnits = 0;

// Loop over number of pixel axes

   uInt pixelAxis;
   Int coordinate, axisInCoordinate;
   for (pixelAxis=0; pixelAxis<nPixelAxes(); pixelAxis++) {

// Find coordinate number for this pixel axis
 
      findPixelAxis(coordinate, axisInCoordinate, pixelAxis);

// Update widths of fields

      Coordinate* pc = CoordinateSystem::coordinate(coordinate).clone();
      listHeader (os, pc,  widthName, widthProj, widthShape, widthTile, 
                  widthRefValue, widthRefPixel, widthInc, widthUnits,
                  True, axisInCoordinate, pixelAxis,
                  nativeFormat, precRefValSci, precRefValFloat,
                  precRefValRADEC, precRefPixFloat, precIncSci, 
                  latticeShape, tileShape);
//
      if (pc->type() == Coordinate::SPECTRAL) {
         listVelocity (os, pc, widthName, widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 True, axisInCoordinate, pixelAxis, velocityType,
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci);
      }
//
      delete pc;
   }


// Compare with header widths

   widthName = max(nameName.length(), widthName) + 1;
   widthProj = max(nameProj.length(), widthProj) + 1;
   if (doShape) {
      widthShape = max(nameShape.length(), widthShape) + 1;
      widthTile = max(nameTile.length(), widthTile) + 1;
   }
   widthRefValue = max(nameRefValue.length(), widthRefValue) + 1;
   widthRefPixel = max(nameRefPixel.length(), widthRefPixel) + 1;
   widthInc = max(nameInc.length(), widthInc) + 1;
   widthUnits = max(nameUnits.length(), widthUnits);
}


void CoordinateSystem::listHeader (LogIO& os,  Coordinate* pc, uInt& widthName,  uInt& widthProj,
                                   uInt& widthShape, uInt& widthTile, uInt& widthRefValue, 
                                   uInt& widthRefPixel, uInt& widthInc,  uInt& widthUnits, 
                                   Bool findWidths, Int axisInCoordinate,  Int pixelAxis,
                                   Bool nativeFormat, Int precRefValSci, Int precRefValFloat, 
                                   Int precRefValRADEC, Int precRefPixFloat, Int precIncSci,
                                   const IPosition& latticeShape, const IPosition& tileShape) const
//
// List all the good stuff
//
//  Input:
//     os               The LogIO to write to
//     pc               Pointer to the coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//     nativeFormat     If true don't convert any units
//           
{

// Clear flags

   if (!findWidths) clearFlags(os);

// Axis name

   String string = pc->worldAxisNames()(axisInCoordinate);
   if (findWidths) {
      widthName = max(widthName, string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthName);
      os << string;
   }


// Projection

   if (pc->type() == Coordinate::DIRECTION) {
      DirectionCoordinate* dc = (DirectionCoordinate*)pc;
      string = dc->projection().name();
   } else {
      string = " ";
   }
   if (findWidths) {
      widthProj = max(widthProj, string.length());
   } else {
      os.output().setf(ios::right, ios::adjustfield);
      os.output().width(widthProj);
      os << string;
   }


// Number of pixels

   Bool doShape = tileShape.nelements()>0 && 
                  latticeShape.nelements()>0 &&
                  tileShape.nelements()==latticeShape.nelements();
   if (doShape) {
      if (pixelAxis != -1) {
         ostrstream oss;
         oss << latticeShape(pixelAxis);
         string = String(oss);
      } else {
         string = " ";
      }
      if (findWidths) {
         widthShape = max(widthShape, string.length());
      } else {
         os.output().width(widthShape);
         os << string;
      }

// Tile shape

      if (pixelAxis != -1) {
         ostrstream oss;
         oss << tileShape(pixelAxis);
         string = String(oss);
      } else {
         string = " ";
      }
      if (findWidths) {
         widthTile = max(widthTile, string.length());
      } else {
         os.output().width(widthTile);
         os << string;
      }
   }

// Remember units

   Vector<String> oldUnits(pc->nWorldAxes());
   oldUnits = pc->worldAxisUnits();
   Vector<String> units(pc->nWorldAxes());

// Reference value

   if (nativeFormat && pc->type() != Coordinate::STOKES) {
      ostrstream oss;
      oss.setf(ios::scientific, ios::floatfield);
      oss.precision(precRefValSci);
      oss << pc->referenceValue()(axisInCoordinate);
      string = String(oss);
   } else {
      Coordinate::formatType form;
      Bool absolute = True;
      String listUnits;
      Int prec;
//
      if (pc->type() == Coordinate::STOKES) {
         Vector<Double> world(1);
         Vector<Double> pixel(1);
         String sName;
         form = Coordinate::DEFAULT;

         if (pixelAxis != -1) {
            StokesCoordinate* sc = dynamic_cast<StokesCoordinate*>(pc);
            const uInt nPixels = sc->stokes().nelements();
            for (uInt i=0; i<nPixels; i++) {
               pixel(0) = Double(i);
               Bool ok = pc->toWorld(world, pixel);
               String temp;
               if (ok) {
                  temp = pc->format(listUnits, form, world(0), 
                                    axisInCoordinate, absolute, -1);
               } else {
                  temp = "?";
               }
               sName += temp;
            }
         } else {
            pixel(0) =pc->referencePixel()(axisInCoordinate);
            Bool ok = pc->toWorld(world, pixel);
            if (ok) {
               sName = pc->format(listUnits, form, world(0), 
                                  axisInCoordinate, absolute, -1);
            } else {
               sName = "?";
            }
         }
         string = sName;
      } else {
         form = Coordinate::DEFAULT;
         pc->getPrecision(prec, form, absolute, precRefValSci, 
                          precRefValFloat, precRefValRADEC);
         string = pc->format(listUnits, form, 
                            pc->referenceValue()(axisInCoordinate),
                            axisInCoordinate, absolute, prec);
      }
   }
   if (findWidths) {
      widthRefValue = max(widthRefValue,string.length());
   } else {
      os.output().width(widthRefValue);
      os << string;
   }

// Reference pixel

   if (pc->type() != Coordinate::STOKES) {
      if (pixelAxis != -1) {
         ostrstream oss;
         oss.setf(ios::fixed, ios::floatfield);
         oss.precision(precRefPixFloat);
         oss << pc->referencePixel()(axisInCoordinate) + 1.0;
         string = String(oss);
      } else {
         string = " ";
      }
      if (findWidths) {
         widthRefPixel = max(widthRefPixel,string.length());
      } else {
         os.output().width(widthRefPixel);
         os << string;
      }
   }


// Increment

   String incUnits;
   if (pc->type() != Coordinate::STOKES) {
      if (pixelAxis != -1) {
         if (nativeFormat) {
            ostrstream oss;
            oss.setf(ios::scientific, ios::floatfield);
            oss.precision(precIncSci);
            oss << pc->increment()(axisInCoordinate);
            string = String(oss);
            incUnits = pc->worldAxisUnits()(axisInCoordinate);
         } else {
            Coordinate::formatType form;
            const Bool absolute = False;
            Int prec;

            form = Coordinate::SCIENTIFIC;
            pc->getPrecision(prec, form, absolute, precRefValSci, 
                             precRefValFloat, precRefValRADEC);
            string = pc->format(incUnits, form, 
                                pc->increment()(axisInCoordinate),
                                axisInCoordinate, absolute, prec);
         }
      } else {
         string = " ";
      }
      if (findWidths) {
         widthInc = max(widthInc,string.length());
      } else {
         os.output().width(widthInc);
         os << string;
      }
   }


// Increment units

   if (pc->type()!= Coordinate::STOKES) {
      if (pixelAxis != -1) {
         string = " " + incUnits;
      } else {
         string = " ";
      }
      if (findWidths) {
         widthUnits = max(widthUnits,string.length());
      } else {
         os.output().setf(ios::left, ios::adjustfield);
         os << string;
      }
   }

   if (!findWidths) os << endl;    
}


void CoordinateSystem::listVelocity (LogIO& os,  Coordinate* pc, uInt& widthName, uInt& widthProj,
                                    uInt& widthShape, uInt& widthTile, uInt& widthRefValue, 
                                    uInt& widthRefPixel, uInt& widthInc,  uInt& widthUnits, 
                                    const Bool findWidths, const Int axisInCoordinate, 
                                    const Int pixelAxis, const MDoppler::Types velocityType,
                                    const Int precRefValSci, const Int precRefValFloat, 
                                    const Int precRefValRADEC, const Int precRefPixFloat, 
                                    const Int precIncSci) const
//
// List all the good stuff
//
//  Input:
//     os               The LogIO to write to
//     pc               Pointer to the coordinate
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//           
{

// Clear flags

   if (!findWidths) clearFlags(os);


// Axis name

   String string("Velocity");
   if (findWidths) {
      widthName = max(widthName, string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthName);
      os << string;
   }

// Projection

   if (!findWidths) {
     os.output().setf(ios::right, ios::adjustfield);
      os.output().width(widthProj);
      string = " ";
      os << string;
   }

// Number of pixels
   
   if (widthShape>0 && widthTile>0) {
      if (!findWidths) {
         os.output().width(widthShape);
         os << string;
      }

// Tile shape

      if (!findWidths) {   
         os.output().width(widthTile);
         os << string;
      }
   }


// Caste the coordinate to a spectral coordinate

//   SpectralCoordinate* sc = (SpectralCoordinate*)pc;
   SpectralCoordinate* sc = dynamic_cast<SpectralCoordinate*>(pc);

// Remember units

   Vector<String> oldUnits(pc->nWorldAxes());
   oldUnits = pc->worldAxisUnits();
   Vector<String> units(pc->nWorldAxes());


// Convert reference pixel it to a velocity and format 

   Double velocity;
   String velUnits("km/s");
   Bool ok = pixelToVelocity(velocity, sc->referencePixel()(axisInCoordinate),
                             sc, velocityType, velUnits);
   if (!ok) {
      string = "Fail";
   } else {
      Coordinate::formatType form;
      Bool absolute = True;
      String listUnits;
      Int prec;
      form = Coordinate::DEFAULT;
      pc->getPrecision(prec, form, absolute, precRefValSci, 
                       precRefValFloat, precRefValRADEC);
      string = sc->format(listUnits, form, velocity,
                          axisInCoordinate, absolute, prec);
   }
   if (findWidths) {
      widthRefValue = max(widthRefValue,string.length());
   } else {
      os.output().width(widthRefValue);
      os << string;
   }


// Reference pixel

   if (pixelAxis != -1) {
      ostrstream oss;
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(precRefPixFloat);
      oss << sc->referencePixel()(axisInCoordinate) + 1.0;
      string = String(oss);
   } else {
      string = " ";
   }
   if (!findWidths) {
      os.output().width(widthRefPixel);
      os << string;
   }
  

// Increment

   if (pixelAxis != -1) {
     Double velocityInc;
     if (!velocityIncrement(velocityInc, sc, velocityType, velUnits)) {
        string = "Fail";
     } else {
        ostrstream oss;
        oss.setf(ios::scientific, ios::floatfield);
        oss.precision(precIncSci);
        oss << velocityInc;
        string = String(oss);
     }
  } else {
     string = " ";
  }
  if (findWidths) {
     widthInc = max(widthInc,string.length());
  } else {
     os.output().width(widthInc);
     os << string;
  }
 

// Increment units

   if (pixelAxis != -1) {
      string = " " + velUnits;
   } else {
      string = " ";
   }
   if (findWidths) {
      widthUnits = max(widthUnits,string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os << string;
   } 

   if (!findWidths) os << endl;    
}



void CoordinateSystem::clearFlags(LogIO& os) const
//
// Clear all the formatting flags
//
{  
   os.output().unsetf(ios::left);
   os.output().unsetf(ios::right);
   os.output().unsetf(ios::internal);
 
   os.output().unsetf(ios::dec);
   os.output().unsetf(ios::oct);
   os.output().unsetf(ios::hex);
 
   os.output().unsetf(ios::showbase | ios::showpos | ios::uppercase | ios::showpoint);
 
   os.output().unsetf(ios::scientific);
   os.output().unsetf(ios::fixed);
 
}



Bool CoordinateSystem::pixelToVelocity(Double& velocity, Double pixel, const SpectralCoordinate* sc,
                                       MDoppler::Types velocityType, const String& velUnits) const
{

// Find rest frequency.  Assumes only one axis in SpectralCoordinate

   if (sc->nWorldAxes() != 1) return False;
   if (sc->restFrequency() <= 0) return False;

   Quantum<Double> restFreq;
   restFreq.setValue(sc->restFrequency());
   restFreq.setUnit(sc->worldAxisUnits()(0));

// Convert pixel to MFrequency

   MFrequency frequency;
   if (!sc->toWorld(frequency, pixel)) return False;


// Create velocity machine

   MDoppler::Ref ref;
   ref = velocityType;
   VelocityMachine m(sc->frequencySystem(), Unit("Hz"),
                     MVFrequency(restFreq),
   		     ref, Unit(velUnits));
   velocity = m(frequency.get("Hz")).getValue();

   return True;
}

         
Bool CoordinateSystem::velocityIncrement(Double& velocityInc, const SpectralCoordinate* sc,
                                         MDoppler::Types velocityType, const String& velUnits) const
{

// DO this the hard way for now until Wim gives me spectralMachine

   if (sc->nWorldAxes() != 1) return False;
   Double refPix = sc->referencePixel()(0);

// Find world values at refPix +/- 0.5 and take difference

   Double pixel;
   pixel = refPix + 0.5;
   Double velocity1;
   if (!pixelToVelocity(velocity1, pixel, sc, velocityType, velUnits)) return False;

//

   pixel = refPix - 0.5;
   Double velocity2;
   if (!pixelToVelocity(velocity2, pixel, sc, velocityType, velUnits)) return False;


// Return increment
   
   velocityInc = velocity1 - velocity2;

   return True;
}




void CoordinateSystem::listDirectionSystem(LogIO& os) const
{
   Int afterCoord = -1;
   Int ic = findCoordinate(Coordinate::DIRECTION, afterCoord);
   if (ic >= 0) {
      MDirection::Types dirType = directionCoordinate(uInt(ic)).directionType();
      os << "Direction system : " << MDirection::showType(dirType) << endl;
   }
}



void CoordinateSystem::listFrequencySystem(LogIO& os, MDoppler::Types velocityType) const
{
   Int afterCoord = -1;
   Int ic = findCoordinate(Coordinate::SPECTRAL, afterCoord);
   if (ic >= 0) {
      const SpectralCoordinate& specCoord = spectralCoordinate(uInt(ic));
      MFrequency::Types freqType = specCoord.frequencySystem();
//      
      os << "Frequency system : " << MFrequency::showType(freqType) << endl;
      os << "Velocity  system : " << MDoppler::showType(velocityType) << endl;
//
      Double rf = specCoord.restFrequency();
      if (rf > 0.0) {
         Quantum<Double> restFreq;
         restFreq.setValue(rf);
         restFreq.setUnit(specCoord.worldAxisUnits()(0));
//
         ostrstream oss;
         oss << restFreq << endl;
         os << "Rest frequency   : " << String(oss) << endl;
      }
   }
}
