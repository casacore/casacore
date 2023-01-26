//# CoordinateSystem.cc: hold a collection of coordinates
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003,2004
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


#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/coordinates/Coordinates/Coordinate.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/QualityCoordinate.h>
#include <casacore/coordinates/Coordinates/FITSCoordinateUtil.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MDoppler.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

const String CoordinateSystem::_class = "CoordinateSystem";

std::mutex CoordinateSystem::_mapInitMutex;
map<String, String> CoordinateSystem::_friendlyAxisMap = map<String, String>();


CoordinateSystem::CoordinateSystem()
: coordinates_p(0), 
  world_maps_p(0), world_tmps_p(0), world_replacement_values_p(0),
  pixel_maps_p(0), pixel_tmps_p(0), pixel_replacement_values_p(0),
  worldAxes_tmps_p(0), pixelAxes_tmps_p(0),
  worldOut_tmps_p(0), pixelOut_tmps_p(0),
  worldMin_tmps_p(0), worldMax_tmps_p(0)
{
   setDefaultWorldMixRanges();
}

void CoordinateSystem::copy(const CoordinateSystem &other)
{
    if (this == &other) {
	return;
    }
//
    clear();
//
    obsinfo_p = other.obsinfo_p;
    coordinates_p = other.coordinates_p;
    const uint32_t n = coordinates_p.nelements();
//
    uint32_t i;
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
	world_maps_p[i] = new Block<int32_t>(*(other.world_maps_p[i]));
	world_tmps_p[i] = new Vector<double>(other.world_tmps_p[i]->copy());
	world_replacement_values_p[i] = 
	    new Vector<double>(other.world_replacement_values_p[i]->copy());
	AlwaysAssert(world_maps_p[i] != 0 &&
		     world_tmps_p[i] != 0 &&
		     world_replacement_values_p[i] != 0, AipsError);
	pixel_maps_p[i] = new Block<int32_t>(*(other.pixel_maps_p[i]));
	pixel_tmps_p[i] = new Vector<double>(other.pixel_tmps_p[i]->copy());
	pixel_replacement_values_p[i] = 
	    new Vector<double>(other.pixel_replacement_values_p[i]->copy());
	AlwaysAssert(pixel_maps_p[i] != 0 &&
		     pixel_tmps_p[i] != 0 &&
		     pixel_replacement_values_p[i] != 0, AipsError);
//
	worldAxes_tmps_p[i] = new Vector<bool>(other.worldAxes_tmps_p[i]->copy());
	pixelAxes_tmps_p[i] = new Vector<bool>(other.pixelAxes_tmps_p[i]->copy());
	AlwaysAssert(worldAxes_tmps_p[i] != 0 && pixelAxes_tmps_p[i] != 0, AipsError);
//
	worldOut_tmps_p[i] = new Vector<double>(other.worldOut_tmps_p[i]->copy());
	pixelOut_tmps_p[i] = new Vector<double>(other.pixelOut_tmps_p[i]->copy());
	AlwaysAssert(worldOut_tmps_p[i] != 0 && pixelOut_tmps_p[i] != 0, AipsError);
//
	worldMin_tmps_p[i] = new Vector<double>(other.worldMin_tmps_p[i]->copy());
	worldMax_tmps_p[i] = new Vector<double>(other.worldMax_tmps_p[i]->copy());
	AlwaysAssert(worldMin_tmps_p[i] != 0 && worldMax_tmps_p[i] != 0, AipsError);
    }
}

void CoordinateSystem::clear()
{
    const uint32_t n = coordinates_p.nelements();
//
    for (uint32_t i=0; i<n; i++) {
        deleteTemps (i);
	delete coordinates_p[i]; 
        coordinates_p[i] = 0;
    }
}

CoordinateSystem::CoordinateSystem(const CoordinateSystem &other)
    : Coordinate(),
      coordinates_p(0), 
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
    uint32_t oldWorldAxes = nWorldAxes();
    uint32_t oldPixelAxes = nPixelAxes();
//
// coordinates_p
//
    const uint32_t n = coordinates_p.nelements(); // "before" n, index of new coord
    coordinates_p.resize(n+1);
    coordinates_p[n] = coord.clone();
    AlwaysAssert(coordinates_p[n] != 0, AipsError);
//
// world_maps_p
//
    world_maps_p.resize(n+1);
    world_maps_p[n] = new Block<int32_t>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_maps_p[n], AipsError);
    uint32_t i;
    for (i=0; i < world_maps_p[n]->nelements(); i++) {
	world_maps_p[n]->operator[](i) = oldWorldAxes + i;
    }
//
// world_tmps_p
//
    world_tmps_p.resize(n+1);
    world_tmps_p[n] = new Vector<double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_tmps_p[n], AipsError);
//
// pixel_maps_p
//
    pixel_maps_p.resize(n+1);
    pixel_maps_p[n] = new Block<int32_t>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_maps_p[n], AipsError);
    for (i=0; i < pixel_maps_p[n]->nelements(); i++) {
	pixel_maps_p[n]->operator[](i) = oldPixelAxes + i;
    }
//
// pixel_tmps_p
//
    pixel_tmps_p.resize(n+1);
    pixel_tmps_p[n] = new Vector<double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_tmps_p[n], AipsError);
//
// pixel_replacement_values_p
//
    pixel_replacement_values_p.resize(n+1);
    pixel_replacement_values_p[n] = 
	new Vector<double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_replacement_values_p[n], AipsError);
    *(pixel_replacement_values_p[n]) = 0.0;
//
// world_replacement_values_p
//
    world_replacement_values_p.resize(n+1);
    world_replacement_values_p[n] = 
	new Vector<double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_replacement_values_p[n], AipsError);
    coordinates_p[n] -> toWorld(*(world_replacement_values_p[n]),
				*(pixel_replacement_values_p[n]));
//
// worldAxes_tmps_p
//
    worldAxes_tmps_p.resize(n+1);
    worldAxes_tmps_p[n] = new Vector<bool>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldAxes_tmps_p[n], AipsError);
//
// pixelAxes_tmps_p
//
    pixelAxes_tmps_p.resize(n+1);
    pixelAxes_tmps_p[n] = new Vector<bool>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixelAxes_tmps_p[n], AipsError);
//
// worldOut_tmps_p
//
    worldOut_tmps_p.resize(n+1);
    worldOut_tmps_p[n] = new Vector<double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldOut_tmps_p[n], AipsError);
//
// pixelOut_tmps_p
//
    pixelOut_tmps_p.resize(n+1);
    pixelOut_tmps_p[n] = new Vector<double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixelOut_tmps_p[n], AipsError);
//
// worldMin_tmps_p
//
    worldMin_tmps_p.resize(n+1);
    worldMin_tmps_p[n] = new Vector<double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldMin_tmps_p[n], AipsError);
//
// worldMax_tmps_p
//
    worldMax_tmps_p.resize(n+1);
    worldMax_tmps_p[n] = new Vector<double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(worldMax_tmps_p[n], AipsError);
}

void CoordinateSystem::transpose(const Vector<int32_t> &newWorldOrder,
				 const Vector<int32_t> &newPixelOrder)
{
    AlwaysAssert(newWorldOrder.nelements() == nWorldAxes(), AipsError);
    AlwaysAssert(newPixelOrder.nelements() == nPixelAxes(), AipsError);

    const uint32_t nc = nCoordinates();
    const uint32_t nw = newWorldOrder.nelements();
    const uint32_t np = newPixelOrder.nelements();

// Verify that all axes are in new*Order once (only)

    Vector<bool> found(nw);
    found = false;
    uint32_t i;
    for (i=0; i<found.nelements(); i++) {
	int32_t which = newWorldOrder(i);
	AlwaysAssert(which>=0 && uint32_t(which)<nw && !found(which), AipsError);
	found(which) = true;
    }
//
    found.resize(np);
    found = false;
    for (i=0; i<found.nelements(); i++) {
	int32_t which = newPixelOrder(i);
	AlwaysAssert(which >=0 && uint32_t(which) < np && !found(which), AipsError);
	found(which) = true;
    }
//
    PtrBlock<Block<int32_t> *> newWorldMaps(nc);
    PtrBlock<Block<int32_t> *> newPixelMaps(nc);
    newWorldMaps.set(static_cast<Block<int32_t> *>(0));
    newPixelMaps.set(static_cast<Block<int32_t> *>(0));

// copy the maps (because the deleted axes will be staying put)

    for (i=0; i<nc; i++) {
	newWorldMaps[i] = new Block<int32_t>(*world_maps_p[i]);
	newPixelMaps[i] = new Block<int32_t>(*pixel_maps_p[i]);
	AlwaysAssert((newWorldMaps[i] && newPixelMaps[i]), AipsError);
    }

// Move the world axes to their new home

    for (i=0; i<nw; i++) {
	int32_t coord, axis;
	findWorldAxis(coord, axis, newWorldOrder(i));
	newWorldMaps[coord]->operator[](axis) = i;
    }

// Move the pixel axes to their new home

    for (i=0; i<np; i++) {
	int32_t coord, axis;
	findPixelAxis(coord, axis, newPixelOrder(i));
	newPixelMaps[coord]->operator[](axis) = i;
    }

// OK, now overwrite the new locations 

    for (i=0; i<nc; i++) {
	delete world_maps_p[i];
	world_maps_p[i] = newWorldMaps[i];
	delete pixel_maps_p[i];
	pixel_maps_p[i] = newPixelMaps[i];
    }
}


bool CoordinateSystem::pixelMap(Vector<int32_t>& pixelAxisMap,
                                Vector<int32_t>& pixelAxisTranspose,
                                const CoordinateSystem& other) const
//
// . pixelAxisMap(i) is the location of pixel axis
//   i (from other) in *this.   A value of -1 indicates that 
//   a pixel axis could not be matched.  
// . pixelAxisTranspose(i) is the location of pixel axis i (from *this)
//   in other.   It tells you how to transpose
//   "other" to be in the order of "*this".  A value of -1 indicates
//   that a world axis could not be matched. 
//
{
   if (other.nPixelAxes() ==0) {
      set_error(String("The supplied CoordinateSystem has no valid pixel axes"));
      return false;
   }
   if (nPixelAxes() ==0) {
      set_error(String("The current CoordinateSystem has no valid pixel axes"));
      return false;
   }
//
   pixelAxisMap.resize(other.nPixelAxes());
   pixelAxisMap = -1;
   pixelAxisTranspose.resize(nPixelAxes());
   pixelAxisTranspose = -1;
//
   Vector<int32_t> worldAxisMap, worldAxisTranpose;
   Vector<bool> refChange;
   bool ok = worldMap(worldAxisMap, worldAxisTranpose, refChange, other);
   if (!ok) return false;
//
   int32_t w, tw;
   for (uint32_t i=0; i<other.nPixelAxes(); i++) {
      w = other.pixelAxisToWorldAxis(i);      // Every pixel axis has a world axis
      tw = worldAxisMap(w); 
      if (tw >= 0) pixelAxisMap(i) = worldAxisToPixelAxis(tw);
   }
//
   for (uint32_t i=0; i<nPixelAxes(); i++) {
      w = pixelAxisToWorldAxis(i);        
      tw = worldAxisTranpose(w);  
      if (tw >= 0) pixelAxisTranspose(i) = other.worldAxisToPixelAxis(tw); 
   }
//
   return true;
}

bool CoordinateSystem::worldMap(Vector<int32_t>& worldAxisMap,
                                Vector<int32_t>& worldAxisTranspose,
                                Vector<bool>& refChange,
                                const CoordinateSystem& other) const
//
// Make a map from "*this" to "other"
//
// . Returns false if either "*this" or "other" have no valid
//   world axes.   Otherwise true.
// . The coordinate systems can have arbitrary numbers of coordinates
//   in any relative order.
// . Removed world and pixel axes are handled.
// . worldAxisMap(i) is the location of world axis
//   i (from other) in *this.   A value of -1 indicates that 
//   a world axis could not be matched.  
// . worldAxisTranspose(i) is the location of world axis i (from *this)
//   in other.   It tells you how to transpose
//   "other" to be in the order of "*this".  A value of -1 indicates
//   that a world axis could not be matched. 
// . If refChange(i) is true, it means that if the coordinate matched,
//   there is a difference in reference type (E.g J2000->B1950)
//   for worldAxis i in "other"
{

// Resize the maps and initialize

   worldAxisMap.resize(other.nWorldAxes());
   worldAxisMap = -1;
   worldAxisTranspose.resize(nWorldAxes());
   worldAxisTranspose = -1;
   refChange.resize(nWorldAxes());
   refChange = false;
//
   if (other.nWorldAxes() ==0) {
      set_error(String("The supplied CoordinateSystem has no valid world axes"));
      return false;
   }
   if (nWorldAxes() ==0) {
      set_error(String("The current CoordinateSystem has no valid world axes"));
      return false;
   }

// Loop over "other" coordinates

   const uint32_t nCoord  =        nCoordinates();
   const uint32_t nCoord2 = other.nCoordinates();
   Vector<bool> usedCoords(nCoord,false);
   for (uint32_t coord2=0; coord2<nCoord2; coord2++) {

// If all the world axes for this coordinate have been removed,
// we do not attempt to match with anything.

      if (!allEQ(other.worldAxes(coord2), -1)) {

      
// Try and find this coordinate type in "*this". If there
// is more than one coordinate of this type in "*this", we
// try them all looking for the first one that conforms.
// "other" may also contain more than one coordinate of a given
// type, so make sure we only use a coordinate in "*this" once

         for (uint32_t coord=0; coord<nCoord; coord++) {
            if (!usedCoords(coord)) {
               if (type(coord) == other.type(coord2)) {
                  if (mapOne(worldAxisMap, worldAxisTranspose, 
                             refChange, *this, other, coord, coord2)) {
                     usedCoords(coord) = true;
                     break;
                  }
               }
            }
         }

// break jumps here

      }
   }

   return true;
}


bool CoordinateSystem::removeWorldAxis(uint32_t axis, double replacement) 
{
    if (axis >= nWorldAxes()) {
       ostringstream oss;
       oss << "Illegal removal world axis number (" << axis << "), max is ("
           << nWorldAxes() << ")" << endl;
       set_error (String(oss));
       return false;
    }

// Remove the corresponding pixel axis (if there)..

    int32_t pixAxis = worldAxisToPixelAxis (axis);
    if (pixAxis >= 0) {

// Find pixel coordinate corresponding to world replacement value

       Vector<double> world(referenceValue());
       world(axis) = replacement;
       Vector<double> pixel(nPixelAxes());
       if (!toPixel(pixel, world)) return false;
//
       removePixelAxis (pixAxis, pixel(pixAxis));
    }

    const uint32_t nc = nCoordinates();
    int32_t coord, caxis;
    findWorldAxis(coord, caxis, axis);
    world_replacement_values_p[coord]->operator()(caxis) = replacement;
    int32_t oldValue =  world_maps_p[coord]->operator[](caxis);
    world_maps_p[coord]->operator[](caxis) = -1 * (oldValue+1);   // Makes the actual axis recoverable
    for (uint32_t i=0; i<nc; i++) {
	for (uint32_t j=0; j<world_maps_p[i]->nelements(); j++) {
	    if (world_maps_p[i]->operator[](j) > int32_t(axis)) {
		world_maps_p[i]->operator[](j)--;
	    }
	}
    }
    return true;
}

bool CoordinateSystem::removePixelAxis(uint32_t axis, double replacement) 
{
    if (axis >= nPixelAxes()) {
       ostringstream oss;
       oss << "Illegal removal pixel axis number (" << axis << "), max is ("
           << nPixelAxes() << ")" << endl;
       set_error (String(oss));
       return false;
    }
//
    const uint32_t nc = nCoordinates();
    int32_t coord, caxis;
    findPixelAxis(coord, caxis, axis);
    pixel_replacement_values_p[coord]->operator()(caxis) = replacement;
    int32_t oldValue = pixel_maps_p[coord]->operator[](caxis);
    pixel_maps_p[coord]->operator[](caxis) = -1 * (oldValue+1);     // Makes the actual axis recoverable
//
    for (uint32_t i=0; i<nc; i++) {
	for (uint32_t j=0; j<pixel_maps_p[i]->nelements(); j++) {
	    if (pixel_maps_p[i]->operator[](j) > int32_t(axis)) {
		pixel_maps_p[i]->operator[](j)--;
	    }
	}
    }
   return true;
}


/*
//This implementation is flawed.  It only works if you have  removed
//one axis.  I think you need to specify coordinate and axis in coordinate

bool CoordinateSystem::worldReplacementValue (double& replacement, uint32_t axis) const
{
   int32_t coordinate = -1;
   int32_t axisInCoordinate = -1;
   if (checkWorldReplacementAxis(coordinate, axisInCoordinate, axis)) {
      replacement = world_replacement_values_p[coordinate]->operator()(axisInCoordinate);
      return true;
   }
//
   return false;
}

bool CoordinateSystem::setWorldReplacementValue (uint32_t axis, double replacement) 
{
   int32_t coordinate = -1;
   int32_t axisInCoordinate = -1;
   if (checkWorldReplacementAxis(coordinate, axisInCoordinate, axis)) {
      world_replacement_values_p[coordinate]->operator()(axisInCoordinate) = replacement;
      return true;
   }
//
   return false;
}

bool CoordinateSystem::pixelReplacementValue (double& replacement, uint32_t axis) const
{
   int32_t coordinate = -1;
   int32_t axisInCoordinate = -1;
   if (checkPixelReplacementAxis(coordinate, axisInCoordinate, axis)) {
      replacement = pixel_replacement_values_p[coordinate]->operator()(axisInCoordinate);
      return true;
   }
//
   return false;
}

bool CoordinateSystem::setPixelReplacementValue (uint32_t axis, double replacement) 
{
   int32_t coordinate = -1;
   int32_t axisInCoordinate = -1;
   if (checkPixelReplacementAxis(coordinate, axisInCoordinate, axis)) {
      pixel_replacement_values_p[coordinate]->operator()(axisInCoordinate) = replacement;
      return true;
   }
//
   return false;
}
*/


CoordinateSystem CoordinateSystem::subImage(const Vector<float> &originShift,
					    const Vector<float> &pixincFac,
                                            const Vector<int32_t>& newShape) const
{
    CoordinateSystem coords = *this;
    coords.subImageInSitu(originShift, pixincFac, newShape);
    return coords;
}


void CoordinateSystem::subImageInSitu(const Vector<float> &originShift,
                                      const Vector<float> &pixincFac,
                                      const Vector<int32_t>& newShape)
{
    AlwaysAssert(originShift.nelements() == nPixelAxes() &&
                 pixincFac.nelements() == nPixelAxes(), AipsError);
    const uint32_t nShape = newShape.nelements();
    AlwaysAssert(nShape==0 || nShape==nPixelAxes(), AipsError);

// We could get rid of this assumption by multiplying by accounting for the PC
// matrix as well as cdelt, or going through group-by-group, but it doesn't
// seem necessary now, or maybe ever.

    AlwaysAssert(originShift.nelements() == pixincFac.nelements(), AipsError);
    uint32_t n = nPixelAxes();
    Vector<double> crpix = referencePixel().copy();
    Vector<double> cdelt = increment().copy();
// Not efficient, but easy and this code shouldn't be called often

    int32_t coordinate, axisInCoordinate;
    for (uint32_t i=0; i<n; i++) {
    	findPixelAxis(coordinate, axisInCoordinate, i);
    	Coordinate::Type cType = type(coordinate);
        if (cType == Coordinate::STOKES) {

// Only integer shift and factor for Stokes

        	int32_t s = -1;
        	if (nShape!=0) s = newShape(i);
        	StokesCoordinate sc = stokesSubImage(stokesCoordinate(coordinate),
        			int32_t(originShift(i)+0.5),
        			int32_t(pixincFac(i)+0.5), s);
        	replaceCoordinate(sc, coordinate);
        }
        else if (cType == Coordinate::QUALITY) {
        	int32_t s = -1;
        	if (nShape!=0) s = newShape(i);
        	QualityCoordinate qc = qualitySubImage(qualityCoordinate(coordinate),
        			int32_t(originShift(i)+0.5),
        			int32_t(pixincFac(i)+0.5), s);
        	replaceCoordinate(qc, coordinate);
        }
        if (
        	cType == Coordinate::SPECTRAL
        	&& spectralCoordinate(coordinate).worldValues().size() > 0
        ) {
        	// tabular spectral coordinate
        	SpectralCoordinate spCoord = spectralCoordinate(coordinate);
        	SpectralCoordinate::SpecType nativeType = spCoord.nativeType();
                Vector<double> newWorldValues(newShape.nelements() > 0 ? newShape[i] : spCoord.worldValues().size());
		// switch off reference conversion if necessary
		MFrequency::Types baseType = spCoord.frequencySystem(false);
		MFrequency::Types convType = spCoord.frequencySystem(true);
		MEpoch convEpoch;
		MPosition convPos;
		MDirection convDir;
		if(baseType!=convType){
		  spCoord.getReferenceConversion(convType, convEpoch, convPos, convDir);
		  spCoord.setReferenceConversion(baseType, convEpoch, convPos, convDir);
		}
        	for (uint32_t j=0; j< newWorldValues.size(); j++) {
        		double pix = originShift[i] + j*pixincFac[i];
        		if (! spCoord.toWorld(newWorldValues[j], pix)) {
        			throw AipsError("Unable to convert tabular spectral coordinate");
        		}
        	}
		// restore reference conversion
		if(baseType!=convType){
		  spCoord.setReferenceConversion(convType, convEpoch, convPos, convDir);
		}
        	SpectralCoordinate newCoord(
        		baseType, newWorldValues, spCoord.restFrequency()
        	);
        	if (! newCoord.setNativeType(nativeType)) {
        		throw AipsError("Unable to set native type of tabular spectral coordinate.");
        	}

		// adding the conversion layer if one exists
		if(!newCoord.setReferenceConversion(convType, convEpoch, convPos, convDir)){
		  throw AipsError("Unable to set reference conversion layer of tabular spectral coordinate.");
		}

         	crpix(i) = 0;
         	cdelt[i] = newCoord.increment()[0];

        	replaceCoordinate(newCoord, coordinate);
        }
        else {
           AlwaysAssert(pixincFac(i) > 0, AipsError);
           crpix(i) -= originShift(i);
           crpix(i) /= pixincFac(i);
           cdelt(i) *= pixincFac(i);
        }
    }
    setReferencePixel(crpix);
    setIncrement(cdelt);
}


void CoordinateSystem::restoreOriginal()
{
    CoordinateSystem coord;

// Make a copy and then assign it back

    uint32_t n = coordinates_p.nelements();
    for (uint32_t i=0; i<n; i++) {
	coord.addCoordinate(*(coordinates_p[i]));
    }
//    
    *this = coord;
}

uint32_t CoordinateSystem::nCoordinates() const
{
    return coordinates_p.nelements();
}

Coordinate::Type CoordinateSystem::type(uint32_t whichCoordinate) const
{
    AlwaysAssert(whichCoordinate<nCoordinates(), AipsError);
    return coordinates_p[whichCoordinate]->type();
}

String CoordinateSystem::showType(uint32_t whichCoordinate) const
{
    AlwaysAssert(whichCoordinate<nCoordinates(), AipsError);
    return coordinates_p[whichCoordinate]->showType();
}

const Coordinate& CoordinateSystem::coordinate(uint32_t which) const
{
    AlwaysAssert(which < nCoordinates(), AipsError);
    return *(coordinates_p[which]);
}

const LinearCoordinate& CoordinateSystem::linearCoordinate(uint32_t which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::LINEAR, AipsError);
    return dynamic_cast<const LinearCoordinate &>(*(coordinates_p[which]));
}

const DirectionCoordinate& CoordinateSystem::directionCoordinate(uint32_t which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::DIRECTION, AipsError);
    return dynamic_cast<const DirectionCoordinate &>(*(coordinates_p[which]));
}

const DirectionCoordinate& CoordinateSystem::directionCoordinate() const {
	if (! hasDirectionCoordinate()) {
		throw AipsError(
			String(__FUNCTION__)
			+ ": Coordinate system has no direction coordinate"
		);
	}
	return directionCoordinate(directionCoordinateNumber());
}

const SpectralCoordinate& CoordinateSystem::spectralCoordinate(uint32_t which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::SPECTRAL, AipsError);
    return dynamic_cast<const SpectralCoordinate &>(*(coordinates_p[which]));
}

const SpectralCoordinate& CoordinateSystem::spectralCoordinate() const {
	if (! this->hasSpectralAxis()) {
		throw AipsError(
			String(__FUNCTION__)
			+ ": Coordinate system has no spectral coordinate"
		);
	}
	return spectralCoordinate(spectralCoordinateNumber());
}

const StokesCoordinate& CoordinateSystem::stokesCoordinate(uint32_t which) const {
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::STOKES, AipsError);
    return dynamic_cast<const StokesCoordinate &>(*(coordinates_p[which]));
}

const StokesCoordinate& CoordinateSystem::stokesCoordinate() const {
	if (! this->hasPolarizationCoordinate()) {
		throw AipsError(
			String(__FUNCTION__)
			+ ": Coordinate system has no polarization coordinate"
		);
	}
	return stokesCoordinate(polarizationCoordinateNumber());
}


const QualityCoordinate& CoordinateSystem::qualityCoordinate(uint32_t which) const {
    AlwaysAssert(which < nCoordinates() &&
		 coordinates_p[which]->type() == Coordinate::QUALITY, AipsError);
    return dynamic_cast<const QualityCoordinate &>(*(coordinates_p[which]));
}

const TabularCoordinate& CoordinateSystem::tabularCoordinate(uint32_t which) const
{
    AlwaysAssert(which < nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::TABULAR, 
		 AipsError);
    return dynamic_cast<const TabularCoordinate &>(*(coordinates_p[which]));
}

bool CoordinateSystem::replaceCoordinate(const Coordinate &newCoordinate, uint32_t which)
{

// Basic checks.  The number of axes must be the same as this function does not
// change any of the axis removal or mappings etc.

    AlwaysAssert(which < nCoordinates() &&
		 newCoordinate.nPixelAxes() == coordinates_p[which]->nPixelAxes() &&
		 newCoordinate.nWorldAxes() == coordinates_p[which]->nWorldAxes(),
		 AipsError);
    bool typesEqual = newCoordinate.type()==coordinates_p[which]->type();
    const Vector<String>& oldUnits(coordinates_p[which]->worldAxisUnits());
    const Vector<String>& newUnits(newCoordinate.worldAxisUnits());

// Replace coordinate

    delete coordinates_p[which];
    coordinates_p[which] = newCoordinate.clone();
    AlwaysAssert(coordinates_p[which], AipsError);

// Now, the world replacement values are a bother.  They may well have the wrong
// units now.    So try to find scale factors if the Coordinates were of
// the same type.  

    bool ok = false;
    int32_t where = 0;
    if (typesEqual) {
       String errMsg;
       Vector<double> factor;
       ok = Coordinate::find_scale_factor(errMsg, factor, newUnits, oldUnits);
//
       if (ok) {

// Apply scale factor

          for (uint32_t i=0; i<factor.nelements(); i++) {
            world_replacement_values_p[which]->operator()(i) *= factor[i];
          }
       }
    }
    if (ok) return true;

// If different types, or the scale factors could not be found (non-conformant units) the 
// best we can do is set the reference values for the replacement values

   if (!ok || !typesEqual) {
     const Vector<double>& refVal(newCoordinate.referenceValue());
     for (uint32_t i=0; i<refVal.nelements(); i++) {
       where = world_maps_p[which]->operator[](i);
       if (where < 0) {
          world_replacement_values_p[which]->operator()(i) = refVal[i];
       } else {
          world_replacement_values_p[which]->operator()(i) = 0.0;
       }
     }
   }
   return false;
}


int32_t CoordinateSystem::findCoordinate(Coordinate::Type type, int32_t afterCoord) const
{
    if (afterCoord < -1) {
	afterCoord = -1;
    }
    int32_t n = nCoordinates();
    bool found = false;
    while (++afterCoord < n) {
	if (coordinates_p[afterCoord]->type() == type) {
	    found = true;
	    break;
	}
    }
    if (found) {
	return afterCoord;
    } else {
	return -1;
    }
}

void CoordinateSystem::findWorldAxis(int32_t &coordinate, int32_t &axisInCoordinate, 
                                     uint32_t axisInCoordinateSystem) const
{
    coordinate = axisInCoordinate = -1;
    AlwaysAssert(axisInCoordinateSystem < nWorldAxes(), AipsError);
//
    const uint32_t orig = axisInCoordinateSystem; // alias for less typing
    const uint32_t nc = nCoordinates();
//
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t na = world_maps_p[i]->nelements();
	for (uint32_t j=0; j<na; j++) {
	    if (world_maps_p[i]->operator[](j) == int32_t(orig)) {
		coordinate = i;
		axisInCoordinate = j;
		return;
	    }
	}
    }
}

void CoordinateSystem::findPixelAxis(int32_t &coordinate, int32_t &axisInCoordinate, 
			  uint32_t axisInCoordinateSystem) const
{
    coordinate = axisInCoordinate = -1;

    AlwaysAssert(axisInCoordinateSystem < nPixelAxes(), AipsError);

    const uint32_t orig = axisInCoordinateSystem; // alias for less typing
    const uint32_t nc = nCoordinates();

    for (uint32_t i=0; i<nc; i++) {
	const uint32_t na = pixel_maps_p[i]->nelements();
	for (uint32_t j=0; j<na; j++) {
	    if (pixel_maps_p[i]->operator[](j) == int32_t(orig)) {
		coordinate = i;
		axisInCoordinate = j;
		return;
	    }
	}
    }
}

int32_t CoordinateSystem::pixelAxisToWorldAxis(uint32_t pixelAxis) const
{
   int32_t coordinate, axisInCoordinate;
   findPixelAxis(coordinate, axisInCoordinate, pixelAxis);
   if (axisInCoordinate>=0 && coordinate>=0) {
      return worldAxes(coordinate)(axisInCoordinate);
   }
   return -1;
}

int32_t CoordinateSystem::worldAxisToPixelAxis(uint32_t worldAxis) const
{
   int32_t coordinate, axisInCoordinate;
   findWorldAxis(coordinate, axisInCoordinate, worldAxis);
   if (axisInCoordinate>=0 && coordinate>=0) {
      return pixelAxes(coordinate)(axisInCoordinate);
   }
   return -1;
}

Vector<int32_t> CoordinateSystem::worldAxes(uint32_t whichCoord) const
{
// Implemented in terms of the public member functions. It would be more
// efficient to use the private data, but would be harder to maintain.
// This isn't apt to be called often, so choose the easier course.

    AlwaysAssert(whichCoord < nCoordinates(), AipsError);
    Vector<int32_t> retval(coordinate(whichCoord).nWorldAxes());

    retval = -1;  // Axes which aren't found must be removed!
    const uint32_t naxes = nWorldAxes();
    for (uint32_t i=0; i<naxes; i++) {
	int32_t coord, axis;
	findWorldAxis(coord, axis, i);
	if (coord == int32_t(whichCoord)) {
	    retval(axis) = i;
	}
    }
    return retval;
}

Vector<int32_t> CoordinateSystem::pixelAxes(uint32_t whichCoord) const
{
    AlwaysAssert(whichCoord < nCoordinates(), AipsError);
    Vector<int32_t> retval(coordinate(whichCoord).nPixelAxes());
//
    retval = -1;  // Axes which aren't found must be removed!
    const uint32_t naxes = nPixelAxes();
    for (uint32_t i=0; i<naxes; i++) {
	int32_t coord, axis;
	findPixelAxis(coord, axis, i);
	if (coord == int32_t(whichCoord)) {
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

uint32_t CoordinateSystem::nWorldAxes() const
{
    uint32_t count = 0;
    const uint32_t nc = nCoordinates();
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t na = world_maps_p[i]->nelements();
	for (uint32_t j=0; j<na; j++) {
	    if (world_maps_p[i]->operator[](j) >= 0) {
		count++;
	    }
	}
    }
    return count;
}

uint32_t CoordinateSystem::nPixelAxes() const
{
    uint32_t count = 0;
    const uint32_t nc = nCoordinates();
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t na = pixel_maps_p[i]->nelements();
	for (uint32_t j=0; j<na; j++) {
	    if (pixel_maps_p[i]->operator[](j) >= 0) {
		count++;
	    }
	}
    }
    return count;
}

Vector<double> CoordinateSystem::toWorld(const Vector<double> &pixel) const {
	Vector<double> world;
	if (! toWorld(world, pixel)) {
		throw AipsError("Cannot convert pixel to world coordinates");
	}
	return world;
}

Vector<double> CoordinateSystem::toWorld(const IPosition& pixel) const {
	Vector<double> world;
	if (! toWorld(world, pixel)) {
		throw AipsError("Cannot convert pixel to world coordinates");
	}
	return world;
}

bool CoordinateSystem::toWorld(Vector<double> &world, 
			       const Vector<double> &pixel, bool useConversionFrame) const
{
    if(pixel.nelements() != nPixelAxes()){
	ostringstream oss;
	oss << "pixel.nelements() != nPixelAxes(): "
	    << pixel.nelements() << ", " << nPixelAxes();
	throw (AipsError(String(oss)));
    }

    if (world.nelements()!=nWorldAxes()) world.resize(nWorldAxes());

    const uint32_t nc = coordinates_p.nelements();
    bool ok = true;
    for (uint32_t i=0; i<nc; i++) {

// For each coordinate, put the appropriate pixel or
// replacement values in the pixel temporary, call the
// coordinates own toWorld, and then copy the output values
// from the world temporary to the world coordinate

	const uint32_t npa = pixel_maps_p[i]->nelements();
	uint32_t j;
	for (j=0; j<npa; j++) {
	    int32_t where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) {

 //cerr << "i j where " << i << " " << j << " " << where <<endl;

		pixel_tmps_p[i]->operator()(j) = pixel(where);
	    } else {
		pixel_tmps_p[i]->operator()(j) = 
		    pixel_replacement_values_p[i]->operator()(j);
	    }
	}
	bool oldok = ok;
	ok = coordinates_p[i]->toWorld(
		       *(world_tmps_p[i]), *(pixel_tmps_p[i]), useConversionFrame);

	if (!ok) {

// Transfer the error message. Note that if there is more than
// one error message this transfers the last one. I suppose this
// is as good as any.

	    set_error(coordinates_p[i]->errorMessage());
	}
	ok = (ok && oldok);
	const uint32_t nwa = world_maps_p[i]->nelements();
	for (j=0; j<nwa; j++) {
	    int32_t where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		world(where) = world_tmps_p[i]->operator()(j);
	    }
	}
    }

    return ok;
}

Vector<double> CoordinateSystem::toPixel(const Vector<double> &world) const {
	Vector<double> pixel;
	if (! toPixel(pixel, world)) {
		throw AipsError("Cannot convert world to pixel coordinates");
	}
	return pixel;
}

bool CoordinateSystem::toWorld(Vector<double> &world, 
			       const IPosition &pixel) const
{
    static Vector<double> pixel_tmp;
    if (pixel_tmp.nelements()!=pixel.nelements()) pixel_tmp.resize(pixel.nelements());
//
    const uint32_t& n = pixel.nelements();
    for (uint32_t i=0; i<n; i++) {
	pixel_tmp(i) = pixel(i);
    }
    return toWorld(world, pixel_tmp);
}

bool CoordinateSystem::toPixel(Vector<double> &pixel, 
			       const Vector<double> &world) const
{
    AlwaysAssert(world.nelements() == nWorldAxes(), AipsError);
    if (pixel.nelements()!=nPixelAxes()) pixel.resize(nPixelAxes());

    const uint32_t nc = coordinates_p.nelements();
    bool ok = true;
    int32_t where;
    for (uint32_t i=0; i<nc; i++) {
	// For each coordinate, put the appropriate world or replacement values
	// in the world temporary, call the coordinates own toPixel, and then
	// copy the output values from the pixel temporary to the pixel
	// coordinate
	const uint32_t nwra = world_maps_p[i]->nelements();
	uint32_t j;
	for (j=0; j<nwra; j++) {
	    where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		world_tmps_p[i]->operator()(j) = world(where);
	    } else {
		world_tmps_p[i]->operator()(j) = 
		    world_replacement_values_p[i]->operator()(j);
	    }
	}
	bool oldok = ok;
	ok = coordinates_p[i]->toPixel(
			    *(pixel_tmps_p[i]), *(world_tmps_p[i]));
	if (!ok) {
	    // Transfer the error message. Note that if there is more than
	    // one error message this transfers the last one. I suppose this
	    // is as good as any.
	    set_error(coordinates_p[i]->errorMessage());
	}
	ok = (ok && oldok);
	const uint32_t npxa = pixel_maps_p[i]->nelements();
	for (j=0; j<npxa; j++) {
	    where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) {
		pixel(where) = pixel_tmps_p[i]->operator()(j);
	    }
	}
    }

    return ok;
}

Quantity CoordinateSystem::toWorldLength(
    const double nPixels,
   	const uint32_t pixelAxis
) const {
	if (pixelAxis >= nPixelAxes()) {
		throw AipsError(
			String(__FUNCTION__)
				+ "pixelAxis greater or equal to nPixelAxes"
		);
	}
	int32_t coord, coordAxis;
	findWorldAxis(coord, coordAxis, pixelAxis);
	return Quantity(
		fabs(nPixels*coordinates_p[coord]->increment()[coordAxis]),
		worldAxisUnits()[pixelAxisToWorldAxis(pixelAxis)]
	);
}

bool CoordinateSystem::toWorldMany(Matrix<double>& world,
                                   const Matrix<double>& pixel,
                                   Vector<bool>& failures) const
{
    AlwaysAssert(nPixelAxes()==pixel.nrow(), AipsError);
    const uint32_t nTransforms = pixel.ncolumn();
    world.resize(nWorldAxes(), nTransforms);
//
// Matrix indexing::
//     matrix(nCoords,  nTransforms)   == matrix(nrows, ncolumns)
//     matrix(i,j); j=0->nTransforms, i=0->nCoordinates;
//     matrix.column(j) = vector of coordinates for one transformation
//     matrix.row(i)    = vector of transforms for one coordinate
//
// Loop over coordinates

    uint32_t i, k;
    int32_t where;
    bool ok = true;
//
    const uint32_t nCoords = coordinates_p.nelements();
    for (k=0; k<nCoords; k++) {

//     Put the appropriate pixel or replacement values in the pixel temporary, call the
//     coordinates own toWorldMany, and then copy the output values  from the world 
//     temporary to the world coordinate
//
	const uint32_t nPixelAxes = pixel_maps_p[k]->nelements();
        Matrix<double> pixTmp(nPixelAxes,nTransforms);
//
	for (i=0; i<nPixelAxes; i++) {
	    where = pixel_maps_p[k]->operator[](i);
	    if (where >= 0) {
                pixTmp.row(i) = pixel.row(where);
	    } else {
		pixTmp.row(i) = pixel_replacement_values_p[k]->operator()(i);
	    }
	}

// Do conversion using Coordinate specific implementation

	const uint32_t nWorldAxes = world_maps_p[k]->nelements();
        Matrix<double> worldTmp(nWorldAxes,nTransforms);
        Vector<bool> failuresTmp;
	ok = coordinates_p[k]->toWorldMany(worldTmp, pixTmp, failuresTmp);

// We get the last error message from whatever coordinate it is

        if (!ok) {
	    set_error(coordinates_p[k]->errorMessage());
	}

// Now copy result from temporary into output world matrix

	for (i=0; i<nWorldAxes; i++) {
	    where = world_maps_p[k]->operator[](i);
	    if (where >= 0) {
		world.row(where) = worldTmp.row(i);
            }
	}
    }

// Code should be written to merge the failures vectors
// Code should be written to merge the OK statuses

   failures.resize(nCoords);
   failures = false;
//
   return ok;
}




bool CoordinateSystem::toPixelMany(Matrix<double>& pixel,
                                   const Matrix<double>& world,
                                   Vector<bool>& failures) const
{
    AlwaysAssert(nWorldAxes()==world.nrow(), AipsError);
    const uint32_t nTransforms = world.ncolumn();
    pixel.resize(nPixelAxes(), nTransforms);
//
// Matrix indexing::
//     matrix(nCoords,  nTransforms)   == matrix(nrows, ncolumns)
//     matrix(i,j); j=0->nTransforms, i=0->nCoordinates;
//     matrix.column(j) = vector of coordinates for one transformation
//     matrix.row(i)    = vector of transforms for one coordinate
//
// Loop over coordinates

    uint32_t i, k;
    int32_t where;
    bool ok = true;
//
    const uint32_t nCoords = coordinates_p.nelements();
    for (k=0; k<nCoords; k++) {
	const uint32_t nWorldAxes = world_maps_p[k]->nelements();
        Matrix<double> worldTmp(nWorldAxes,nTransforms);
//
	for (i=0; i<nWorldAxes; i++) {
	    where = world_maps_p[k]->operator[](i);
	    if (where >= 0) {
                worldTmp.row(i) = world.row(where);
	    } else {
		worldTmp.row(i) = world_replacement_values_p[k]->operator()(i);
	    }
	}

// Do conversion using Coordinate specific implementation

	const uint32_t nPixelAxes = pixel_maps_p[k]->nelements();
        Matrix<double> pixTmp(nPixelAxes,nTransforms);
        Vector<bool> failuresTmp;
	ok = coordinates_p[k]->toPixelMany(pixTmp, worldTmp, failuresTmp);

// We get the last error message from whatever coordinate it is

	if (!ok) {
	    set_error(coordinates_p[k]->errorMessage());
	}

// Now copy result from temporary into output pixel matrix

	for (i=0; i<nPixelAxes; i++) {
	    where = pixel_maps_p[k]->operator[](i);
	    if (where >= 0) {
		pixel.row(where) = pixTmp.row(i);
            }
	}
    }

// Code should be written to merge the failures vectors

   failures.resize(nCoords);
   failures = false;
//
   return ok;
}




bool CoordinateSystem::toMix(Vector<double>& worldOut,
                             Vector<double>& pixelOut,
                             const Vector<double>& worldIn,
                             const Vector<double>& pixelIn, 
                             const Vector<bool>& worldAxes,
                             const Vector<bool>& pixelAxes,
                             const Vector<double>& minWorld,
                             const Vector<double>& maxWorld) const
{
   const uint32_t nWorld = worldAxes.nelements();
   const uint32_t nPixel = pixelAxes.nelements();
   AlwaysAssert(nWorld == nWorldAxes(), AipsError);
   AlwaysAssert(worldIn.nelements()==nWorld, AipsError);
   AlwaysAssert(nPixel == nPixelAxes(), AipsError);
   AlwaysAssert(pixelIn.nelements()==nPixel, AipsError);
   AlwaysAssert(minWorld.nelements()==nWorld, AipsError);
   AlwaysAssert(maxWorld.nelements()==nWorld, AipsError);
//
   const uint32_t nCoord = coordinates_p.nelements();
   if (worldOut.nelements()!=nWorldAxes()) worldOut.resize(nWorldAxes());
   if (pixelOut.nelements()!=nPixelAxes()) pixelOut.resize(nPixelAxes());

   for (uint32_t i=0; i<nCoord; i++) {

// For each coordinate, put the appropriate pixel or replacement values 
// in the pixel temporary, call the coordinates own toMix, and then copy 
// the output values from the world temporary to the world coordinate

      const uint32_t nAxes = world_maps_p[i]->nelements();
      const uint32_t nPixelAxes = pixel_maps_p[i]->nelements();
      AlwaysAssert(nAxes==nPixelAxes, AipsError);
//
      for (uint32_t j=0; j<nAxes; j++) {
         int32_t where = world_maps_p[i]->operator[](j);
         if (where >= 0) {
            world_tmps_p[i]->operator()(j) = worldIn(where);
            worldAxes_tmps_p[i]->operator()(j) = worldAxes(where);
            worldMin_tmps_p[i]->operator()(j) = minWorld(where);
            worldMax_tmps_p[i]->operator()(j) = maxWorld(where);
         } else {
//
// World axis removed.   Use replacement value.  If the world axis
// is removed, so will the pixel axis be.  
//
            world_tmps_p[i]->operator()(j) = world_replacement_values_p[i]->operator()(j);
// 
// We have to decide what conversion to do (pixel<->world) for the removed axis.  
// For coupled axes like DirectionCoordinate, I do for the removed axis whatever I did for
// the unremoved axis, if there is one...  If both world axes are removed, ultimately 
// it doesn't really matter what I do since the pixel axes will be gone as well, and there
// is nowhere to put the output !  For uncoupled axes it doesn't matter.
//
            if (type(i)==Coordinate::DIRECTION) {
               Vector<String> units(coordinate(i).worldAxisUnits());
//
               int32_t where2;
               if (j==0) {      // 0 or 1
                  where2 = world_maps_p[i]->operator[](1);
                  worldMin_tmps_p[i]->operator()(0) = coordinates_p[i]->worldMixMin()(0);
                  worldMax_tmps_p[i]->operator()(0) = coordinates_p[i]->worldMixMax()(0);
               } else {
                  where2 = world_maps_p[i]->operator[](0);
                  worldMin_tmps_p[i]->operator()(1) = coordinates_p[i]->worldMixMin()(1);
                  worldMax_tmps_p[i]->operator()(1) = coordinates_p[i]->worldMixMax()(1);
               }
               if (where2 >= 0) {
                  worldAxes_tmps_p[i]->operator()(j) = worldAxes(where2);
               } else {
                  worldAxes_tmps_p[i]->operator()(j) = false;
               }
            } else {
               worldAxes_tmps_p[i]->operator()(j) = true;
//
// worldMin/Max irrelevant except for DirectionCoordinate
//
            }
         }
      }
//
      for (uint32_t j=0; j<nAxes; j++) {
         int32_t where = pixel_maps_p[i]->operator[](j);
         if (where >= 0) {
            pixel_tmps_p[i]->operator()(j) = pixelIn(where);
            pixelAxes_tmps_p[i]->operator()(j) = pixelAxes(where);
         } else {

// Pixel axis removed.  It is possible to remove the pixel axis but not
// the world axis.

            pixel_tmps_p[i]->operator()(j) = pixel_replacement_values_p[i]->operator()(j);
// 
// Here I assume nPixelAxes=nWorldAxes for the specific coordinate 
// and the order is the same. This is the truth as far as I know it.
//
// We set pixelAxes to the opposite of worldAxes.  Thus if its
// given as world, use it. If its not given as world, use the
// replacement pixel value

            pixelAxes_tmps_p[i]->operator()(j) = !worldAxes_tmps_p[i]->operator()(j);    
         }
      }
//
      if (!coordinates_p[i]->toMix(*(worldOut_tmps_p[i]), *(pixelOut_tmps_p[i]),
		       *(world_tmps_p[i]), *(pixel_tmps_p[i]),
                       *(worldAxes_tmps_p[i]), *(pixelAxes_tmps_p[i]), 
                       *(worldMin_tmps_p[i]), *(worldMax_tmps_p[i]))) {
         set_error(coordinates_p[i]->errorMessage());
         return false;
      }
//
      for (uint32_t j=0; j<nAxes; j++) {
         int32_t where = world_maps_p[i]->operator[](j);
         if (where>=0) worldOut(where) = worldOut_tmps_p[i]->operator()(j);
         where = pixel_maps_p[i]->operator[](j);
         if (where>=0) pixelOut(where) = pixelOut_tmps_p[i]->operator()(j);
      }
   }
   return true;
}



void CoordinateSystem::makeWorldRelative (Vector<double>& world) const
{
    AlwaysAssert(world.nelements() == nWorldAxes(), AipsError);
//
    const uint32_t nc = coordinates_p.nelements();
    int32_t where;
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t nwa = world_maps_p[i]->nelements();

// Copy elements for this coordinate and replace removed axis values

	uint32_t j;
	for (j=0; j<nwa; j++) {
	    where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		world_tmps_p[i]->operator()(j) = world(where);
	    } else {
		world_tmps_p[i]->operator()(j) = 
		    world_replacement_values_p[i]->operator()(j);
	    }
	}

// Convert for this coordinate.  

        coordinates_p[i]->makeWorldRelative(*(world_tmps_p[i]));

// Copy to output

	for (j=0; j<nwa; j++) {
	    where = world_maps_p[i]->operator[](j);
	    if (where >= 0) world(where) = world_tmps_p[i]->operator()(j);
	}
    }
}



void CoordinateSystem::makeWorldAbsoluteRef (Vector<double>& world,
                                             const Vector<double>& refVal) const
{
    AlwaysAssert(world.nelements() == nWorldAxes(), AipsError);
    AlwaysAssert(refVal.nelements() == nWorldAxes(), AipsError);
//
    const uint32_t nc = coordinates_p.nelements();
    int32_t where;
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t nwa = world_maps_p[i]->nelements();

// Copy elements for this coordinate and replace removed axis values
// Borrow worldOut_tmps vector from toMix

	uint32_t j;
	for (j=0; j<nwa; j++) {
	    where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		world_tmps_p[i]->operator()(j) = world(where);
                worldOut_tmps_p[i]->operator()(j) = refVal(where); 
	    } else {
		world_tmps_p[i]->operator()(j) = 
		    world_replacement_values_p[i]->operator()(j);
		worldOut_tmps_p[i]->operator()(j) = 
		    coordinates_p[i]->referenceValue()(j);  // Use refval
	    }
	}

// Convert for this coordinate. 

	coordinates_p[i]->makeWorldAbsoluteRef (*(world_tmps_p[i]),
                                                *(worldOut_tmps_p[i]));

// Copy to output

	for (j=0; j<nwa; j++) {
	    where = world_maps_p[i]->operator[](j);
	    if (where >= 0) world(where) = world_tmps_p[i]->operator()(j);
	}
    }
}


void CoordinateSystem::makeWorldAbsolute (Vector<double>& world) const
{
    AlwaysAssert(world.nelements() == nWorldAxes(), AipsError);
//
    const uint32_t nc = coordinates_p.nelements();
    int32_t where;
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t nwa = world_maps_p[i]->nelements();

// Copy elements for this coordinate and replace removed axis values

	uint32_t j;
	for (j=0; j<nwa; j++) {
	    where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		world_tmps_p[i]->operator()(j) = world(where);
	    } else {
		world_tmps_p[i]->operator()(j) = 
		    world_replacement_values_p[i]->operator()(j);
	    }
	}

// Convert for this coordinate.  Make private temporary to optimize further

	coordinates_p[i]->makeWorldAbsolute(*(world_tmps_p[i]));

// Copy to output

	for (j=0; j<nwa; j++) {
	    where = world_maps_p[i]->operator[](j);
	    if (where >= 0) world(where) = world_tmps_p[i]->operator()(j);
	}
    }
}


void CoordinateSystem::makePixelRelative (Vector<double>& pixel) const
{
    AlwaysAssert(pixel.nelements() == nPixelAxes(), AipsError);
//
    const uint32_t nc = coordinates_p.nelements();
    int32_t where;
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t npa = pixel_maps_p[i]->nelements();

// Copy elements for this coordinate and replace removed axis values

	uint32_t j;
	for (j=0; j<npa; j++) {
	    where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) {
		pixel_tmps_p[i]->operator()(j) = pixel(where);
	    } else {
		pixel_tmps_p[i]->operator()(j) = 
		    pixel_replacement_values_p[i]->operator()(j);
	    }
	}

// Convert for this coordinate.  

        coordinates_p[i]->makePixelRelative(*(pixel_tmps_p[i]));

// Copy to output

	for (j=0; j<npa; j++) {
	    where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) pixel(where) = pixel_tmps_p[i]->operator()(j);
	}
    }
}



void CoordinateSystem::makePixelAbsolute (Vector<double>& pixel) const
{
    AlwaysAssert(pixel.nelements() == nPixelAxes(), AipsError);
//
    const uint32_t nc = coordinates_p.nelements();
    int32_t where;
    for (uint32_t i=0; i<nc; i++) {
	const uint32_t npa = pixel_maps_p[i]->nelements();

// Copy elements for this coordinate and replace removed axis values

	uint32_t j;
	for (j=0; j<npa; j++) {
	    where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) {
		pixel_tmps_p[i]->operator()(j) = pixel(where);
	    } else {
		pixel_tmps_p[i]->operator()(j) = 
		    pixel_replacement_values_p[i]->operator()(j);
	    }
	}

// Convert for this coordinate.  Make private temporary to optimize further

	coordinates_p[i]->makePixelAbsolute(*(pixel_tmps_p[i]));

// Copy to output

	for (j=0; j<npa; j++) {
	    where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) pixel(where) = pixel_tmps_p[i]->operator()(j);
	}
    }
}


bool CoordinateSystem::convert (Vector<double>& coordOut,
                                const Vector<double>& coordIn,
                                const Vector<bool>& absIn,
                                const Vector<String>& unitsIn,
                                MDoppler::Types dopplerIn,
                                const Vector<bool>& absOut,
                                const Vector<String>& unitsOut,
                                MDoppler::Types dopplerOut,
                                double pixInOffset, double pixOutOffset)
{
   Matrix<double> coordsIn(coordIn.nelements(), 1);
   Matrix<double> coordsOut(coordIn.nelements(), 1);
   coordsIn.column(0) = coordIn;
//
   if (convert(coordsOut, coordsIn, absIn, unitsIn, dopplerIn,
               absOut, unitsOut, dopplerOut, pixInOffset, 
               pixOutOffset)) {
      coordOut = coordsOut.column(0);
      return true;
   }
   return false;
}

bool CoordinateSystem::convert (Matrix<double>& coordsOut,
                                const Matrix<double>& coordsIn,
                                const Vector<bool>& absIn,
                                const Vector<String>& unitsIn,
                                MDoppler::Types dopplerIn,
                                const Vector<bool>& absOut,
                                const Vector<String>& unitsOut,
                                MDoppler::Types dopplerOut,
                                double pixInOffset, double pixOutOffset)
{
   if (nWorldAxes() != nPixelAxes()) {
      throw (AipsError("Number of pixel and world axes differs"));
   }

// Copy CS so we can mess about with it

   CoordinateSystem cSysIn = *this;
   CoordinateSystem cSysOut = *this;
//
   const uint32_t n = nWorldAxes();
   if (n != coordsIn.nrow()) {
      set_error("Coordinates must all be of length nWorldAxes");
      return false;
   }
//
   bool ok = absIn.nelements()==n && 
             unitsIn.nelements()==n && 
             absOut.nelements()==n && 
             unitsOut.nelements()==n;
   if (!ok) {
      set_error("Inputs must all be of length nWorldAxes");
      return false;
   }
   coordsOut.resize(coordsIn.shape());

// Find input and output velocity axes, set native units strings
// and figure out the allThing Bools

   IPosition velAxesIn(n);
   IPosition velAxesOut(n);
   PtrBlock<SpectralCoordinate*> specCoordsIn(n, (SpectralCoordinate*)0);
   PtrBlock<SpectralCoordinate*> specCoordsOut(n, (SpectralCoordinate*)0);
//
   Vector<String> unitsIn2(cSysIn.worldAxisUnits().copy());
   Vector<String> unitsOut2(cSysOut.worldAxisUnits().copy());
//
   Vector<bool> worldAxesIn(n,false), worldAxesOut(n, false);
   Vector<bool> pixelAxesIn(n,false), pixelAxesOut(n, false);
//
   bool allPixIn = true;     // All input are pixel units
   bool allWorldIn = true;   // All input are consistent with native units
   bool allAbsIn = true;
   bool allRelIn = true;
//
   bool allPixOut = true;     // All output are pixel units
   bool allWorldOut = true;   // All output are consistent with native units
   bool allAbsOut = true;
   bool allRelOut = true;
//
   String sPix("pix");
   Unit velUnit("km/s");
   int32_t coordinate, axisInCoordinate;
   uint32_t jIn = 0;
   uint32_t jOut = 0;
   uint32_t idx;
   for (uint32_t i=0; i<n; i++) {

// Input axes

      if (unitsIn(i)!=sPix) {             // System doesn't like pix units
         Unit uu(unitsIn(i));              // unless I define them...
         if (uu==velUnit) {
            cSysIn.findWorldAxis(coordinate, axisInCoordinate, i);
            if (cSysIn.type(coordinate) == Coordinate::SPECTRAL) {
               specCoordsIn[i] = new SpectralCoordinate(cSysIn.spectralCoordinate(coordinate));
               specCoordsIn[i]->setVelocity (unitsIn(i), dopplerIn);
//
               velAxesIn(jIn) = i;
               jIn++;
               allWorldIn = false;
            } else if (cSysIn.type(coordinate) == Coordinate::LINEAR) {
               unitsIn2(i) = unitsIn(i);
               worldAxesIn(i) = true;
            } else {
              set_error("axis with km/s units is neither Spectral nor Linear");
              cleanUpSpecCoord(specCoordsIn, specCoordsOut);
              return false;
            }
         } else {
            unitsIn2(i) = unitsIn(i);
            worldAxesIn(i) = true;
         }
         allPixIn = false;
      } else {
         allWorldIn = false;
         pixelAxesIn(i) = true;
      }
      if (absIn(i)) {
         allRelIn = false;
      } else {
         allAbsIn = false;
      }

// Output axes

      if (unitsOut(i)!=sPix) {
         Unit uu(unitsOut(i));  
         if (uu==velUnit) {
            cSysOut.findWorldAxis(coordinate, axisInCoordinate, i);
            if (cSysOut.type(coordinate) == Coordinate::SPECTRAL) {
               specCoordsOut[i] = new SpectralCoordinate(cSysOut.spectralCoordinate(coordinate));
               specCoordsOut[i]->setVelocity (unitsOut(i), dopplerOut);
//
               velAxesOut(jOut) = i;
               jOut++;
               allWorldOut = false;
            } else if (cSysOut.type(coordinate) == Coordinate::LINEAR) {
               unitsOut2(i) = unitsOut(i);
               worldAxesOut(i) = true;
            } else {
              set_error("axis with km/s units is neither Spectral nor Linear");
              cleanUpSpecCoord(specCoordsIn, specCoordsOut);
              return false;
            }
         } else {
            unitsOut2(i) = unitsOut(i);
            worldAxesOut(i) = true;
         }
         allPixOut = false;
      } else {
         allWorldOut = false;
         pixelAxesOut(i) = true;
      }
//
      if (absOut(i)) {
         allRelOut = false;
      } else {
         allAbsOut = false;
      }
   }
   velAxesIn.resize(jIn,true);
   velAxesOut.resize(jOut,true);
   uint32_t nVelIn = velAxesIn.nelements();
   uint32_t nVelOut = velAxesOut.nelements();

// Set coordinate system units

   if (!cSysIn.setWorldAxisUnits(unitsIn2)) {
      set_error(cSysIn.errorMessage());
      cleanUpSpecCoord(specCoordsIn, specCoordsOut);
      return false;
   }
//
   if (!cSysOut.setWorldAxisUnits(unitsOut2)) {
      set_error(cSysOut.errorMessage());
      cleanUpSpecCoord(specCoordsIn, specCoordsOut);
      return false;
   }

// Generate toMix ranges.  The user *MUST* have called
// setWorldMixRanges first.  They will be adjusted automatically
// for the unit changes

   Vector<double> worldMin, worldMax;
   worldMin = cSysIn.worldMixMin();
   worldMax = cSysIn.worldMixMax();

// Set up vectors of which we will overwrite bits and pieces

   Vector<double> absWorldIn(n), relWorldIn(n);
   Vector<double> absPixelIn(n), relPixelIn(n);
   Vector<double> absWorldIn2(n), absPixelIn2(n);
   Vector<double> absWorldOut(n), relWorldOut(n);
   Vector<double> absPixelOut(n), relPixelOut(n);
//
   Vector<double> world(n), coordOut(n);
   double absVel, absVelRef, absFreq;
//
   Vector<double> coordIn(n);
   Vector<float> coordInFloat(n);
   Vector<int32_t> coordInInt(n);
//
   Vector<double> relWorldRefIn(cSysIn.referenceValue().copy());
   cSysIn.makeWorldRelative(relWorldRefIn);
   Vector<double> relPixelRefIn(cSysIn.referencePixel().copy());
   cSysIn.makePixelRelative(relPixelRefIn);

// Loop over fields in record.  First we convert to absolute pixel.
// Then we convert to whatever we want.  We take as many short cuts 
// as we can.

   const uint32_t nCoords = coordsIn.ncolumn();
   for (uint32_t j=0; j<nCoords; j++) {

// Get data.  Vectorlengths must be correct or a conformance error will occur

      coordIn = coordsIn.column(j);

// Our first goal is a vector of absolute world
// and/or a vector of absolute pixel for use in toMix
// Take some short cuts.

      if (allPixIn && allAbsIn) {
         absPixelOut = coordIn + pixInOffset;
         if (!cSysIn.toWorld(absWorldOut, absPixelOut)) {
            set_error(cSysIn.errorMessage());
            cleanUpSpecCoord(specCoordsIn, specCoordsOut);
            return false;
         }
      } else if (allPixIn && allRelIn) {
         absPixelOut = coordIn;
         cSysIn.makePixelAbsolute(absPixelOut);
         if (!cSysIn.toWorld(absWorldOut, absPixelOut)) {
            set_error(cSysIn.errorMessage());
            cleanUpSpecCoord(specCoordsIn, specCoordsOut);
            return false;
         }
      } else if (allWorldIn && allAbsIn) {
         absWorldOut = coordIn;
         if (!cSysIn.toPixel(absPixelOut, absWorldOut)) {
            set_error(cSysIn.errorMessage());
            cleanUpSpecCoord(specCoordsIn, specCoordsOut);
            return false;
         }
      } else if (allWorldIn && allRelIn) {
         absWorldOut = coordIn;
         cSysIn.makeWorldAbsolute(absWorldOut);
         if (!cSysIn.toPixel(absPixelOut, absWorldOut)) {
            set_error(cSysIn.errorMessage());
            cleanUpSpecCoord(specCoordsIn, specCoordsOut);
            return false;
         }
      } else {

// On with the mixed cases.  

         absWorldIn = cSysIn.referenceValue();
         absPixelIn = cSysIn.referencePixel();
//
         relWorldIn = relWorldRefIn;
         relPixelIn = relPixelRefIn;

// Pick out each of abs/rel world/pixel values into vectors of that type
// Presently, worldAxes(i) will be false for velocity.  We must
// do that after

         for (uint32_t i=0; i<n; i++) {
            if (pixelAxesIn(i)) {
               if (absIn(i)) {
                  absPixelIn(i) = coordIn(i) + pixInOffset;
               } else {
                  relPixelIn(i) = coordIn(i);
               }
            } else if (worldAxesIn(i)) {
               if (absIn(i)) {
                  absWorldIn(i) = coordIn(i);
               } else {
                  relWorldIn(i) = coordIn(i);    
               }
            }
         }

// Convert relative world/pixel to absolute 

         if (!allAbsIn) {
            absWorldIn2 = relWorldIn;
            cSysIn.makeWorldAbsolute(absWorldIn2);
            absPixelIn2 = relPixelIn;
            cSysIn.makePixelAbsolute(absPixelIn2);

// Now poke in the new absolute values

            for (uint32_t i=0; i<n; i++) {
               if (!absIn(i)) {
                  if (unitsIn(i)==sPix) {
                     absPixelIn(i) = absPixelIn2(i);
                  } else if (specCoordsIn[i]==0) {    
                     absWorldIn(i) = absWorldIn2(i);
                  }
               }
            }
         }

// OK now we have vector of absolute world and/or absolute pixel,
// except for velocity.  Convert that to absolute world

         if (nVelIn > 0) {
            world = cSysIn.referenceValue();
            for (uint32_t i=0; i<nVelIn; i++) {
               idx = velAxesIn(i);

// Make absolute velocity

               absVel = coordIn(idx);
               if (!absIn(idx)) {
                  if (!(specCoordsIn[idx]->frequencyToVelocity(absVelRef, world(idx)))) {
                     set_error(specCoordsIn[idx]->errorMessage());
                     cleanUpSpecCoord(specCoordsIn, specCoordsOut);
                     return false;
                  }
                  absVel += absVelRef;       // rel = abs - ref
               }

// Convert to absolute world

               if (!(specCoordsIn[idx]->velocityToFrequency(absFreq, absVel))) {
                  set_error(specCoordsIn[idx]->errorMessage());
                  cleanUpSpecCoord(specCoordsIn, specCoordsOut);
                  return false;
               }
               absWorldIn(idx) = absFreq;
               worldAxesIn(idx) = true;
            }
         }
// Do mixed conversion to get abs world AND abs pixel

         if (!cSysIn.toMix(absWorldOut, absPixelOut, absWorldIn, absPixelIn,
                           worldAxesIn, pixelAxesIn, worldMin, worldMax)) {
            set_error(cSysIn.errorMessage());
            cleanUpSpecCoord(specCoordsIn, specCoordsOut);
            return false;
         }
      }

// At this point we have a vector of absolute world (cSysIn units)
// AND absolute pixel.  Bug out now if we can. 

      if (allAbsOut && allPixOut) {
         coordOut = absPixelOut + pixOutOffset;
         coordsOut.column(j) = coordOut;
         continue;
      }
      relPixelOut = absPixelOut;
      cSysOut.makePixelRelative(relPixelOut);
      if (allRelOut && allPixOut) {
         coordsOut.column(j) = relPixelOut;
         continue;
      }

// We must convert our world values to cSysOut units.
// We can use the absPixelOut vector for that

      if (!cSysOut.toWorld(absWorldOut, absPixelOut)) {
         set_error(cSysOut.errorMessage());
         cleanUpSpecCoord(specCoordsIn, specCoordsOut);
         return false;
      }
//
      if (allAbsOut && allWorldOut) {
         coordsOut.column(j) = absWorldOut;
         continue;
      }
      relWorldOut = absWorldOut;
      cSysOut.makeWorldRelative(relWorldOut);
      if (allRelOut && allWorldOut) {
         coordsOut.column(j) = relWorldOut;
         continue;
      }

// OK on with mixed cases. Pick out the output value for everything 
// except velocity

      for (uint32_t i=0; i<n; i++) {
         if (pixelAxesOut(i)) {
            if (absOut(i)) {
               coordOut(i) = absPixelOut(i) + pixOutOffset;
            } else {
               coordOut(i) = relPixelOut(i);
            }
         } else if (worldAxesOut(i)) {
            if (absOut(i)) {
               coordOut(i) = absWorldOut(i);
            } else {
               coordOut(i) = relWorldOut(i);
            }
         }
      }

// Now do velocity

      if (nVelOut > 0) {
         world = cSysOut.referenceValue();
         for (uint32_t i=0; i<nVelOut; i++) {
            idx = velAxesOut(i);

// Make absolute velocity

            if (!(specCoordsOut[idx]->frequencyToVelocity(absVel, absWorldOut(idx)))) {
               set_error(specCoordsOut[idx]->errorMessage());
               cleanUpSpecCoord(specCoordsIn, specCoordsOut);
               return false;
            }
//
            if (absOut(idx)) {
               coordOut(idx) = absVel;
            } else {
              if (!(specCoordsOut[idx]->frequencyToVelocity(absVelRef, world(idx)))) {
                 set_error(specCoordsOut[idx]->errorMessage());
                 cleanUpSpecCoord(specCoordsIn, specCoordsOut);
                 return false;
              }
              coordOut(idx) = absVel - absVelRef;     // rel = abs - ref
            }
         }
      }
      coordsOut.column(j) = coordOut;
   }
//
   cleanUpSpecCoord(specCoordsIn, specCoordsOut);
   return true;
}



Vector<String> CoordinateSystem::worldAxisNames() const
{
    Vector<String> retval(nWorldAxes());
    for (uint32_t i=0; i<retval.nelements(); i++) {
	int32_t coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<String> tmp = coordinates_p[coord]->worldAxisNames();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}




Vector<String> CoordinateSystem::worldAxisUnits() const
{
    Vector<String> retval(nWorldAxes());
    for (uint32_t i=0; i<retval.nelements(); i++) {
	int32_t coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<String> tmp = coordinates_p[coord]->worldAxisUnits();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

Vector<double> CoordinateSystem::referencePixel() const
{
    Vector<double> retval(nPixelAxes());
    for (uint32_t i=0; i<retval.nelements(); i++) {
	int32_t coord, coordAxis;
	findPixelAxis(coord, coordAxis, i);
	Vector<double> tmp = coordinates_p[coord]->referencePixel();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

Matrix<double> CoordinateSystem::linearTransform() const
{
    uint32_t nr = nWorldAxes();
    uint32_t nc = nPixelAxes();

    Matrix<double> retval(nr,nc);
    retval = 0.0;

    for (uint32_t i=0; i<nr; i++) {
	for (uint32_t j=0; j<nc; j++) {
	    int32_t worldCoord, worldAxis, pixelCoord, pixelAxis;
	    findWorldAxis(worldCoord, worldAxis, i);
	    findPixelAxis(pixelCoord, pixelAxis, j);
	    // By definition, only axes in the same coordinate may be coupled
	    if (worldCoord == pixelCoord &&
		worldCoord >= 0 && worldAxis >= 0 && pixelAxis >= 0) {
		Matrix<double> tmp(coordinates_p[worldCoord]->linearTransform());
		retval(i,j) = tmp(worldAxis, pixelAxis);
	    }
	}
    }
    return retval;
}

Vector<double> CoordinateSystem::increment() const
{
    Vector<double> retval(nWorldAxes());
    for (uint32_t i=0; i<retval.nelements(); i++) {
	int32_t coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<double> tmp = coordinates_p[coord]->increment();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

Vector<double> CoordinateSystem::referenceValue() const
{
    Vector<double> retval(nWorldAxes());
    for (uint32_t i=0; i<retval.nelements(); i++) {
	int32_t coord, coordAxis;
	findWorldAxis(coord, coordAxis, i);
	Vector<double> tmp = coordinates_p[coord]->referenceValue();
	retval(i) = tmp(coordAxis);
    }
    return retval;
}

bool CoordinateSystem::setWorldAxisNames(const Vector<String> &names)
{
    bool ok = (names.nelements()==nWorldAxes());
    if (!ok) {
      set_error("names vector must be of length nWorldAxes()");
      return false;
    }
//
    const uint32_t nc = nCoordinates();
    for (uint32_t i=0; i<nc; i++) {
	Vector<String> tmp(coordinates_p[i]->worldAxisNames().copy());
	const uint32_t na = tmp.nelements();
	for (uint32_t j=0; j<na; j++) {
	    int32_t which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = names(which);
	    }
	}
	ok = (coordinates_p[i]->setWorldAxisNames(tmp) && ok);
        if (!ok) set_error (coordinates_p[i]->errorMessage());
    }

    return ok;
}

bool CoordinateSystem::setWorldAxisUnits(const Vector<String> &units)
{
    return setWorldAxisUnits (units, false);
}

bool CoordinateSystem::setWorldAxisUnits(const Vector<String> &units,
                                         bool throwException)
{
    String error;
    if (units.nelements() != nWorldAxes()) {
      error = "units vector must be of length nWorldAxes()";
    } else {
      const uint32_t nc = nCoordinates();
      for (uint32_t i=0; i<nc; i++) {
        Vector<String> tmp(coordinates_p[i]->worldAxisUnits().copy());
        uint32_t na = tmp.nelements(); 
        for (uint32_t j=0; j<na; j++) {
           int32_t which = world_maps_p[i]->operator[](j);
           if (which >= 0) tmp[j] = units[which];
        }

        // Set new units
        if  (! coordinates_p[i]->setWorldAxisUnits(tmp)) {
          error = coordinates_p[i]->errorMessage();
        }
      }
    }
    // Check if an error has occurred. If so, throw if needed.
    if (error.empty()) {
      return true;   // no error
    } else if (throwException) {
      throw AipsError (error);
    }
    set_error (error);
    return false;
}

bool CoordinateSystem::setReferencePixel(const Vector<double> &refPix)
{
    bool ok = (refPix.nelements()==nPixelAxes());
    if (!ok) {
      set_error("ref. pix vector must be of length nPixelAxes()");
      return false;
    }
//
    const uint32_t nc = nCoordinates();
    for (uint32_t i=0; i<nc; i++) {
	Vector<double> tmp(coordinates_p[i]->referencePixel().copy());
	uint32_t na = tmp.nelements();
	for (uint32_t j=0; j<na; j++) {
	    int32_t which = pixel_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = refPix(which);
	    }
	}
	ok = (coordinates_p[i]->setReferencePixel(tmp) && ok);
        if (!ok) set_error (coordinates_p[i]->errorMessage());
    }

    return ok;
}

bool CoordinateSystem::setLinearTransform(const Matrix<double> &xform)
{
    const uint32_t nc = nCoordinates();
    bool ok = true;
    for (uint32_t i=0; i<nc; i++) {
	Matrix<double> tmp(coordinates_p[i]->linearTransform().copy());
	uint32_t nrow = tmp.nrow();
	uint32_t ncol = tmp.ncolumn();
	for (uint32_t j=0; j<nrow; j++) {
	    for (uint32_t k=0; k<ncol; k++) {
		int32_t whichrow = world_maps_p[i]->operator[](j);
		int32_t whichcol = pixel_maps_p[i]->operator[](k);
		if (whichrow >= 0 && whichcol >= 0) {
		    tmp(j,k) = xform(whichrow,whichcol);
		}
	    }
	}
	ok = (coordinates_p[i]->setLinearTransform(tmp) && ok);
        if (!ok) set_error (coordinates_p[i]->errorMessage());
    }
    return ok;
}

bool CoordinateSystem::setIncrement(const Vector<double> &inc)
{
    bool ok = (inc.nelements()==nWorldAxes());
    if (!ok) {
      set_error("increment vector must be of length nWorldAxes()");
      return false;
    }
//
    const uint32_t nc = nCoordinates();
    for (uint32_t i=0; i<nc; i++) {
	Vector<double> tmp(coordinates_p[i]->increment().copy());
	uint32_t na = tmp.nelements();
	for (uint32_t j=0; j<na; j++) {
	    int32_t which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = inc(which);
	    }
	}
	ok = (coordinates_p[i]->setIncrement(tmp) && ok);
        if (!ok) set_error (coordinates_p[i]->errorMessage());
    }
    return ok;
}

bool CoordinateSystem::setReferenceValue(const Vector<double> &refval)
{
    bool ok = (refval.nelements()==nWorldAxes());
    if (!ok) {
      set_error("ref. val vector must be of length nWorldAxes()");
      return false;
    }
//
    const uint32_t nc = nCoordinates();
    for (uint32_t i=0; i<nc; i++) {
	Vector<double> tmp(coordinates_p[i]->referenceValue().copy());
	uint32_t na = tmp.nelements();
	for (uint32_t j=0; j<na; j++) {
	    int32_t which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = refval(which);
	    }
	}
	ok = (coordinates_p[i]->setReferenceValue(tmp) && ok);
        if (!ok) set_error (coordinates_p[i]->errorMessage());
    }

    return ok;
}


bool CoordinateSystem::near(const Coordinate& other, 
                            double tol) const
//
// Compare this CoordinateSystem with another. 
//
{
   Vector<int32_t> excludePixelAxes;
   return near(other,excludePixelAxes,tol);
}


bool CoordinateSystem::near(const Coordinate& other, 
                            const Vector<int32_t>& excludePixelAxes,
                            double tol) const
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
      return false;
   }

   const CoordinateSystem& cSys = dynamic_cast<const CoordinateSystem&>(other);
   if (nCoordinates() != cSys.nCoordinates()) {
      set_error("The CoordinateSystems have different numbers of coordinates");
      return false;
   }

   if (nPixelAxes() != cSys.nPixelAxes()) {
      set_error("The CoordinateSystems have different numbers of pixel axes");
      return false;
   }
   if (nWorldAxes() != cSys.nWorldAxes()) {
      set_error("The CoordinateSystems have different numbers of world axes");
      return false;
   }



// Loop over number of coordinates

   ostringstream oss;
   for (int32_t i=0; i<int32_t(nCoordinates()); i++) {

// Although the coordinates are checked for their types in
// the coordinate comparison routines, we can save ourselves
// some time by checking here too

      if (coordinate(i).type() != cSys.coordinate(i).type()) {
         oss << "The coordinate types differ for coordinate number " << i;
         set_error(String(oss));
         return false;
      }

// Find which pixel axes in the CoordinateSystem this coordinate
// inhabits and compare the vectors.   Here we don't take into 
// account the exclusion axes vector; that's only used when we are 
// actually comparing the axis descriptor values on certain axes

      if (pixelAxes(i).nelements() != cSys.pixelAxes(i).nelements()) {
         oss << "The number of pixel axes differs for coordinate number " << i;
         set_error(String(oss));
         return false;
      }
      if (!allEQ(pixelAxes(i), cSys.pixelAxes(i))) {
         oss << "The pixel axes differ for coordinate number " << i;
         set_error(String(oss));
         return false;
      }

// Find which world axes in the CoordinateSystem this
// coordinate inhabits and compare the vectors

      if (worldAxes(i).nelements() != cSys.worldAxes(i).nelements()) {
         oss << "The number of world axes differs for coordinate number " << i;
         set_error(String(oss));
         return false;
      }
      if (!allEQ(worldAxes(i), cSys.worldAxes(i))) {
         oss << "The world axes differ for coordinate number " << i;
         set_error(String(oss));
         return false;
      }
 

// Were all the world axes for this coordinate removed ? If so
// we don't check it

      bool allGone = true;
      int32_t j;
      for (j=0; j<int32_t(worldAxes(i).nelements()); j++) {
         if (worldAxes(i)(j) >= 0) {
            allGone = false;
            break;
         }
      }
      

// Continue if we have some unremoved world axes in this coordinate

      int32_t excSize = coordinate(i).nPixelAxes();
      Vector<int32_t> excludeAxes(excSize);
      if (!allGone) {

// If any of the list of CoordinateSystem exclusion pixel axes
// inhabit this coordinate, make a list of the axes in this
// coordinate that they correspond to.  

         int32_t coord, axisInCoord;
         int32_t k = 0;
         for (j=0; j<int32_t(excludePixelAxes.nelements()); j++) {

// Any invalid excludePixelAxes are dealt with here.  If they are
// rubbish, we just don't find them ! 

            findPixelAxis(coord, axisInCoord, excludePixelAxes(j));
            if (coord == i) {

// OK, this pixel axis is in this coordinate, so stick it in the list
// We may have to resize if the stupid user has given us duplicates
// in the list of exclusion axes

               if (k == int32_t(excludeAxes.nelements())) {
                  int32_t n = int32_t(excludeAxes.nelements()) + excSize;
                  excludeAxes.resize(n,true);
               }
               excludeAxes(k++) = axisInCoord;
            }
         }
         excludeAxes.resize(k,true);


// Now, for the current coordinate, convert the world axes in 
// the CoordinateSystems to axes in the current coordinate
// and compare the two 

         int32_t coord1, coord2, axisInCoord1, axisInCoord2;
         for (j=0; j<int32_t(worldAxes(i).nelements()); j++) {
            if (worldAxes(i)(j) >= 0) {

// Not removed (can't find it if it's been removed !)
  
                     findWorldAxis(coord1, axisInCoord1, worldAxes(i)(j));
               cSys.findWorldAxis(coord2, axisInCoord2, worldAxes(i)(j));

// This better not happen !  

               if (coord1 != coord2) {
                  oss << "The coordinate numbers differ (!!) for coordinate number "
                      << i;
                  set_error(String(oss));
                  return false;
               }

// This might
               if (axisInCoord1 != axisInCoord2) {
                  oss << "World axis " << j << " in the CoordinateSystems"
                      << "has a different axis number in coordinate number "
                      << i;
                  set_error(String(oss));
                  return false;
               }
            }
         }
         
// Now, finally, compare the current coordinate from the two 
// CoordinateSystems except on the specified axes. Leave it
// this function to set the error message

         if (!coordinate(i).near(cSys.coordinate(i),excludeAxes,tol)) {
           set_error(coordinate(i).errorMessage());
           return false;
         }
      }
   }
   return true;
}


bool CoordinateSystem::nearPixel  (const CoordinateSystem& other, 
                                   double tol) const
{
   if (this->type() != other.type()) {
      set_error("Comparison is not with another CoordinateSystem");
      return false;
   }
//
   const CoordinateSystem& cSys1 = *this;
   const CoordinateSystem& cSys2 = dynamic_cast<const CoordinateSystem&>(other);
//
   const uint32_t nPixelAxes1 = cSys1.nPixelAxes();
   const uint32_t nPixelAxes2 = cSys2.nPixelAxes();
//
   if (nPixelAxes1 != nPixelAxes2) {
      set_error("The CoordinateSystems have different numbers of pixel axes");
      return false;
   }
//
   const uint32_t nPixelAxes = nPixelAxes1;
   int32_t coord1, axisInCoord1;
   int32_t coord2, axisInCoord2;
   for (uint32_t i=0; i<nPixelAxes; i++) {
      cSys1.findPixelAxis (coord1, axisInCoord1, i);
      cSys2.findPixelAxis (coord2, axisInCoord2, i);
      AlwaysAssert(coord1>=0, AipsError);
      AlwaysAssert(coord2>=0, AipsError);
//      
      const Coordinate& c1 = cSys1.coordinate(coord1);
      const Coordinate& c2 = cSys2.coordinate(coord2);
      if (c1.type() != c2.type()) {
         ostringstream oss;
         oss << "The coordinate types differ for pixel axis number " << i;
         set_error(String(oss));
         return false;
      }
//
      Vector<int32_t> pixelAxes1 = cSys1.pixelAxes(coord1);
      Vector<int32_t> pixelAxes2 = cSys2.pixelAxes(coord2);
//
      Vector<bool> whichAxes1(pixelAxes1.nelements(), true);
      Vector<bool> whichAxes2(pixelAxes2.nelements(), true);
//     
      for (uint32_t j=0; j<pixelAxes1.nelements(); j++) {
         if (pixelAxes1(j)==-1) whichAxes1(j) = false;
      }
//
      for (uint32_t j=0; j<pixelAxes2.nelements(); j++) {
         if (pixelAxes2(j)==-1) whichAxes2(j) = false;
      }
//
      if (!c1.doNearPixel(c2, whichAxes1, whichAxes2, tol)) {
        set_error(c1.errorMessage());
        return false;
      }
   }
//
   return true;
}




String CoordinateSystem::format(
	String& units, Coordinate::formatType format,
	double worldValue, uint32_t worldAxis,
	bool isAbsolute, bool showAsAbsolute,
	int32_t precision, bool usePrecForMixed
) const {
    AlwaysAssert(worldAxis < nWorldAxes(), AipsError);
// 
    int32_t coord, axis;
    findWorldAxis(coord, axis, worldAxis);
    AlwaysAssert(coord>=0 && axis >= 0, AipsError);
    return coordinates_p[coord]->format(
    	units, format, worldValue, axis,
    	isAbsolute, showAsAbsolute, precision, usePrecForMixed
    );
}

ObsInfo CoordinateSystem::obsInfo() const
{
    return obsinfo_p;
}

void CoordinateSystem::setObsInfo(const ObsInfo &obsinfo)
{
    obsinfo_p = obsinfo;
}

String CoordinateSystem::coordRecordName(uint32_t which) const
{
  // Write each string into a field it's type plus coordinate
  // number, e.g. direction0
  string basename = "unknown";
  switch (coordinates_p[which]->type()) {
  case Coordinate::LINEAR:    basename = "linear"; break;
  case Coordinate::DIRECTION: basename = "direction"; break;
  case Coordinate::SPECTRAL:  basename = "spectral"; break;
  case Coordinate::STOKES:    basename = "stokes"; break;
  case Coordinate::TABULAR:   basename = "tabular"; break;
  case Coordinate::QUALITY:   basename = "quality"; break;
  case Coordinate::COORDSYS:  basename = "coordsys"; break;
  }
  ostringstream onum;
  onum << which;
  return basename + onum.str();
}

bool CoordinateSystem::save(RecordInterface &container,
			    const String &fieldName) const
{
    Record subrec;
    if (container.isDefined(fieldName)) {
       set_error(String("The fieldName is already defined in the supplied record"));
       return false;
    }

// Write the obsinfo

    String error;
    bool ok = obsinfo_p.toRecord(error, subrec);
    if (!ok) {
       set_error (error);
       return false;
    }


// If no coordinates, just run away with the ObsInfo
// in place

    uint32_t nc = coordinates_p.nelements();
    if (nc==0) {
       container.defineRecord(fieldName, subrec);
       return true;
    }

    for (uint32_t i=0; i<nc; i++)
    {
	// Write each string into a field it's type plus coordinate
	// number, e.g. direction0
	String basename = "unknown";
	switch (coordinates_p[i]->type()) {
	case Coordinate::LINEAR:    basename = "linear"; break;
	case Coordinate::DIRECTION: basename = "direction"; break;
	case Coordinate::SPECTRAL:  basename = "spectral"; break;
	case Coordinate::STOKES:    basename = "stokes"; break;
	case Coordinate::QUALITY:   basename = "quality"; break;
	case Coordinate::TABULAR:   basename = "tabular"; break;
	case Coordinate::COORDSYS:  basename = "coordsys"; break;
	}
	ostringstream onum;
	onum << i;
	String num = onum;
	String name = basename + num;
	coordinates_p[i]->save(subrec, name);
	name = String("worldmap") + num;
	subrec.define(name, Vector<int32_t>(world_maps_p[i]->begin(), world_maps_p[i]->end()));
	name = String("worldreplace") + num;
	subrec.define(name, Vector<double>(*world_replacement_values_p[i]));
	name = String("pixelmap") + num;
	subrec.define(name, Vector<int32_t>(pixel_maps_p[i]->begin(), pixel_maps_p[i]->end()));
	name = String("pixelreplace") + num;
	subrec.define(name, Vector<double>(*pixel_replacement_values_p[i]));
    }
//
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
    int32_t nc = 0;                         // num coordinates
    PtrBlock<Coordinate *> coords;
    static const String linear   = "linear";
    static const String direction = "direction";
    static const String spectral = "spectral";
    static const String stokes   = "stokes";
    static const String quality  = "quality";
    static const String tabular  = "tabular";
    static const String coordsys = "coordsys";
    while(true) {
	ostringstream onum;
	onum << nc;
	String num = onum;
	nc++;
	if (subrec.isDefined(linear + num)) {
	    coords.resize(nc);
	    coords[nc-1] = LinearCoordinate::restore(subrec, linear+num);
	}
	else if (subrec.isDefined(direction + num)) {
	    coords.resize(nc);
	    coords[nc-1] = DirectionCoordinate::restore(
	    	subrec, direction+num
	    );
	}
	else if (subrec.isDefined(spectral + num)) {
	    coords.resize(nc);
	    coords[nc-1] = SpectralCoordinate::restore(subrec, spectral+num);
	} else if (subrec.isDefined(stokes + num)) {
	    coords.resize(nc);
	    coords[nc-1] = StokesCoordinate::restore(subrec, stokes+num);
	} else if (subrec.isDefined(quality + num)) {
	    coords.resize(nc);
	    coords[nc-1] = QualityCoordinate::restore(subrec, quality+num);
	} else if (subrec.isDefined(tabular + num)) {
	    coords.resize(nc);
	    coords[nc-1] = TabularCoordinate::restore(subrec, tabular+num);
	} else if (subrec.isDefined(coordsys + num)) {
	    coords.resize(nc);
	    coords[nc-1] = CoordinateSystem::restore(subrec, coordsys+num);
	} else {
	    break;
	}
	AlwaysAssert(coords[nc-1] != 0, AipsError);
    }
    nc = coords.nelements();
//
    retval = new CoordinateSystem;
    int32_t i;
    for (i=0; i<nc; i++) {
	retval->addCoordinate(*(coords[i]));
	delete coords[i];
	coords[i] = 0;
    }
    for (i=0; i<nc; i++) {
//
// Copy values
//
	ostringstream onum;
	onum << i;
	Vector<int32_t> dummy;
	String num(onum), name;
	name = String("worldmap") + num;
	subrec.get(name, dummy);
	*(retval->world_maps_p[i]) = makeBlock(dummy);
	name = String("worldreplace") + num;
	subrec.get(name, *(retval->world_replacement_values_p[i]));
	name = String("pixelmap") + num;
	subrec.get(name, dummy);
	*(retval->pixel_maps_p[i]) = makeBlock(dummy);
	name = String("pixelreplace") + num;
	subrec.get(name, *(retval->pixel_replacement_values_p[i]));
    }
//
// Get the obsinfo
//
    String error;
    bool ok = retval->obsinfo_p.fromRecord(error, subrec);
    AlwaysAssert(ok, AipsError); // Should never happen
//
    return retval;
}


Coordinate *CoordinateSystem::clone() const
{
    return new CoordinateSystem(*this);
}



bool CoordinateSystem::toFITSHeader(RecordInterface &header, 
				    IPosition &shape,
				    bool oneRelative,
				    char prefix, bool writeWCS,
				    bool preferVelocity, 
				    bool opticalVelocity,
				    bool preferWavelength,
				    bool airWavelength) const
{
   FITSCoordinateUtil fcu;
   return fcu.toFITSHeader(header, shape, *this, 
                           oneRelative, prefix,
                           writeWCS, 
			   preferVelocity, opticalVelocity,
			   preferWavelength, airWavelength);
}



bool CoordinateSystem::fromFITSHeader (int32_t& stokesFITSValue, 
                                       CoordinateSystem& cSysOut, 
                                       RecordInterface& recHeader,
                                       const Vector<String>& header,
                                       const IPosition& shape, 
                                       uint32_t which)

{
   FITSCoordinateUtil fcu;
   return fcu.fromFITSHeader (stokesFITSValue, cSysOut, recHeader, header,
                              shape, which);
}



void CoordinateSystem::makeWorldAbsoluteMany (Matrix<double>& world) const
{
   makeWorldAbsRelMany (world, true);
}

void CoordinateSystem::makeWorldRelativeMany (Matrix<double>& world) const
{
   makeWorldAbsRelMany (world, false);
}

void CoordinateSystem::makePixelAbsoluteMany (Matrix<double>& pixel) const
{
   makePixelAbsRelMany (pixel, true);
}

void CoordinateSystem::makePixelRelativeMany (Matrix<double>& pixel) const
{
   makePixelAbsRelMany (pixel, false);
}



void CoordinateSystem::makeWorldAbsRelMany (Matrix<double>& world, bool toAbs) const
{
    const uint32_t nTransforms = world.ncolumn();

// Loop over coordinates

    uint32_t i, k;
    int32_t where;
//
    const uint32_t nCoords = coordinates_p.nelements();
    for (k=0; k<nCoords; k++) {

// Load

	const uint32_t nWorldAxes = world_maps_p[k]->nelements();
        Matrix<double> worldTmp(nWorldAxes,nTransforms);
	for (i=0; i<nWorldAxes; i++) {
	    where = world_maps_p[k]->operator[](i);
	    if (where >= 0) {
                worldTmp.row(i) = world.row(where);
	    } else {
		worldTmp.row(i) = world_replacement_values_p[k]->operator()(i);
	    }
	}

// Do conversion using Coordinate specific implementation

        if (toAbs) {
  	   coordinates_p[k]->makeWorldAbsoluteMany(worldTmp);
        } else {
  	   coordinates_p[k]->makeWorldRelativeMany(worldTmp);
        }

// Unload

	for (i=0; i<nWorldAxes; i++) {
	    where = world_maps_p[k]->operator[](i);
	    if (where >= 0) {
		world.row(where) = worldTmp.row(i);
            }
	}
    }
}


void CoordinateSystem::makePixelAbsRelMany (Matrix<double>& pixel, bool toAbs) const
{
    const uint32_t nTransforms = pixel.ncolumn();

// Loop over coordinates

    uint32_t i, k;
    int32_t where;
//
    const uint32_t nCoords = coordinates_p.nelements();
    for (k=0; k<nCoords; k++) {

// Load

	const uint32_t nPixelAxes = pixel_maps_p[k]->nelements();
        Matrix<double> pixelTmp(nPixelAxes,nTransforms);
	for (i=0; i<nPixelAxes; i++) {
	    where = pixel_maps_p[k]->operator[](i);
	    if (where >= 0) {
                pixelTmp.row(i) = pixel.row(where);
	    } else {
		pixelTmp.row(i) = pixel_replacement_values_p[k]->operator()(i);
	    }
	}

// Do conversion using Coordinate specific implementation

        if (toAbs) {
  	   coordinates_p[k]->makePixelAbsoluteMany(pixelTmp);
        } else {
  	   coordinates_p[k]->makePixelRelativeMany(pixelTmp);
        }

// Unload

	for (i=0; i<nPixelAxes; i++) {
	    where = pixel_maps_p[k]->operator[](i);
	    if (where >= 0) {
		pixel.row(where) = pixelTmp.row(i);
            }
	}
    }
}




Coordinate* CoordinateSystem::makeFourierCoordinate (const Vector<bool>& axes,
                                                     const Vector<int32_t>& shape) const
{
   LogIO os(LogOrigin(_class, __FUNCTION__, WHERE));
//
   if (axes.nelements() != nPixelAxes()) {  
      throw (AipsError("Invalid number of specified pixel axes"));
   } 
   if (axes.nelements()==0) {
      throw (AipsError("There are no pixel axes in this CoordinateSystem"));
   }
//
   if (allEQ(axes,false)) {
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
   uint32_t nReplaced = 0;
   const uint32_t nCoord = nCoordinates();
   for (uint32_t i=0; i<nCoord; i++) {

// Are there some axes true for this coordinate and are their
// world/pixel axes not removed ?

      if (checkAxesInThisCoordinate(axes, i)) {

// Find the coordinate-based axes and shape vectors

         nReplaced++;
         Vector<int32_t> coordSysAxes = pixelAxes(i);
         Vector<bool> coordAxes(coordSysAxes.nelements(),false);
         Vector<int32_t> coordShape(coordAxes.nelements(),0);
//
         for (uint32_t j=0; j<coordSysAxes.nelements(); j++) {
            if (axes(coordSysAxes(j))) coordAxes(j) = true;
            coordShape(j) = shape(coordSysAxes(j));        
         }

// Make Fourier coordinate

         const Coordinate& coord = coordinate(i);
         Coordinate* pC2 = coord.makeFourierCoordinate(coordAxes, coordShape);

// Replace in CS.  Note we don't change any pixel/world axis mappings
// or removal lists in this step.

         pCS->replaceCoordinate(*pC2, i);
         delete pC2;
      }
   }
//
   pCS = 0;
   return pC;
}


bool CoordinateSystem::checkAxesInThisCoordinate(const Vector<bool>& axes, uint32_t which) const
//
// 1) See if this coordinate has any axes to be FTd
// 2) Make sure they are all good.
//
{
   LogIO os(LogOrigin(_class, __FUNCTION__, WHERE));
//
   bool wantIt = false;

// Loop over pixel axes in the coordinatesystem

   int32_t coord, axisInCoord, worldAxis;
   for (uint32_t i=0; i<axes.nelements(); i++) {

// For axes the user wants to FT, find the coordinate

      if (axes(i)) {
         findPixelAxis(coord, axisInCoord, i);

// It should not be possible for the pixel axis to be missing,
// because that means the user gave a wrong length vector
// for "axes" in makeFourierCoordinate and that has already been
// checked

         if (coord<0) {
            ostringstream oss;
            oss << "Pixel axis " << axes(i) << " has been removed" << endl;
            os << String(oss) << LogIO::EXCEPTION;
         }

// Is it this coordinate ?

         if (coord==int32_t(which)) {
            wantIt = true;

// If the world axis has been removed, issue a warning.  It doesn't 
// actually matter to the Coordinate that is doing the FT (doesn't
// know the CS has removed one of its world axes).  Possibly better for
// the user to remain ignorant on this one !

            worldAxis = pixelAxisToWorldAxis(i);
            if (worldAxis<0) {
//               ostringstream oss;
//               oss << "World axis for pixel axis " << axes(i) << " has been removed";
//               os << LogIO::WARN << String(oss) << endl;
//               os << LogIO::WARN << "This does not affect the Fourier Transform" << LogIO::POST;
            }
         }
      }
   }
   return wantIt;
}




Vector<String> CoordinateSystem::list (LogIO& os, 
                                       MDoppler::Types doppler,
                                       const IPosition& latticeShape,
                                       const IPosition& tileShape,
                                       bool postLocally) const
{
   LogSinkInterface& lsi = os.localSink();
   uint32_t n = lsi.nelements();
   int32_t iStart  =  0;
   if (n>0) iStart = n - 1;

   os << LogIO::NORMAL << endl;

// List DirectionCoordinate type from the first DirectionCoordinate we find

   listDirectionSystem(os);

// List rest frequency and reference frame from the first spectral axis we find

   listFrequencySystem(os, doppler);

// Pointing center
 
   listPointingCenter(os);

// List telescope, observer, date

   os << "Telescope           : " << obsinfo_p.telescope() << endl;
   os << "Observer            : " << obsinfo_p.observer() << endl;
//
   MEpoch epoch = obsinfo_p.obsDate();
   MEpoch defEpoch = ObsInfo::defaultObsDate();
   if (epoch.getValue().getDay() != defEpoch.getValue().getDay()) { 
      MVTime time = MVTime(epoch.getValue());
      os << "Date observation    : " << time.string(MVTime::YMD, 12) << endl;
   } else {
      os << "Date observation    : " << "UNKNOWN" << endl;
   }
   if (obsinfo_p.isTelescopePositionSet()) {
      os << "Telescope position: " << obsinfo_p.telescopePositionString() << endl;
   }
   os << endl;

// Determine the widths for all the fields that we want to list

   bool doShape = tileShape.nelements()>0 && 
                  latticeShape.nelements()>0 &&
                  tileShape.nelements()==latticeShape.nelements();
   uint32_t widthName, widthProj, widthShape, widthTile, widthRefValue;
   uint32_t widthRefPixel, widthInc, widthUnits, totWidth, widthCoordType;
   uint32_t widthAxis, widthCoordNumber;

   String nameName, nameProj, nameShape, nameTile, nameRefValue;
   String nameRefPixel, nameInc, nameUnits, nameCoordType, nameAxis;
   String nameCoordNumber;
   
   int32_t precRefValSci, precRefValFloat, precRefValRADEC;
   int32_t precRefPixFloat, precIncSci;
   getFieldWidths (os, widthAxis, widthCoordType, widthCoordNumber, widthName, 
                   widthProj, widthShape, widthTile, widthRefValue, widthRefPixel, widthInc,
                   widthUnits, precRefValSci, precRefValFloat, precRefValRADEC, precRefPixFloat,
                   precIncSci, nameAxis, nameCoordType, nameCoordNumber, nameName, nameProj, nameShape, 
                   nameTile, nameRefValue, nameRefPixel, nameInc, 
                   nameUnits, doppler, latticeShape,
                   tileShape);

// Write headers

   os.output().fill(' ');
   os.output().setf(ios::left, ios::adjustfield);

   os.output().width(widthAxis);
   os << nameAxis;

   os.output().width(widthCoordNumber);
   os << nameCoordNumber;

   os.output().width(widthCoordType);
   os << nameCoordType;

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

   totWidth = widthAxis + widthCoordType + widthCoordNumber + widthName + widthProj + widthShape + 
              widthTile + widthRefValue + widthRefPixel + widthInc + widthUnits;
   os.output().fill('-');
   os.output().width(totWidth);
   os.output().setf(ios::right, ios::adjustfield);
   os << " " << endl;
   os.output() << setfill(' ');


// Loop over the pixel axes in the CS.

   int32_t axisInCoordinate, coordinate;
   for (uint32_t pixelAxis=0; pixelAxis<nPixelAxes(); pixelAxis++) {

// Find coordinate number for this pixel axis. A pixel axis is guarenteed to have 
// a world axis.  A world axis might not have a pixel axis.
 
      findPixelAxis(coordinate, axisInCoordinate, pixelAxis);

// List it

      const Coordinate& c = CoordinateSystem::coordinate(coordinate);
      Coordinate* pc = c.clone();      
      listHeader(os, pc, widthAxis, widthCoordType, widthCoordNumber, widthName, 
                 widthProj, widthShape, widthTile, 
                 widthRefValue, widthRefPixel, widthInc, widthUnits,
                 false, coordinate, axisInCoordinate, pixelAxis, 
                 precRefValSci, precRefValFloat, precRefValRADEC, 
                 precRefPixFloat, precIncSci, latticeShape, tileShape);

// If the axis is spectral, we might like to see it as
// velocity as well as frequency.  Since the listing is row
// based, we have to do it like this.  Urk.

      if (pc->type() == Coordinate::SPECTRAL) {
         listVelocity (os, pc, widthAxis, widthCoordType, widthCoordNumber, widthName, 
                       widthProj, widthShape, widthTile, 
                       widthRefValue, widthRefPixel, widthInc, widthUnits,
                       false, axisInCoordinate, pixelAxis, doppler,
                       precRefValSci, precRefValFloat, precRefValRADEC, 
                       precRefPixFloat, precIncSci);

      }

// If we have listed all of the pixel axes from this coordinate,  then
// see if there are any world axes without pixel axes and list them

      Vector<int32_t> pixelAxes = this->pixelAxes(coordinate);
      Vector<int32_t> worldAxes = this->worldAxes(coordinate);
      int32_t maxPixAxis  = max(pixelAxes);
      if (int32_t(pixelAxis) == maxPixAxis) {
         for (uint32_t axis=0; axis<worldAxes.nelements(); axis++) {
            if (pixelAxes(axis)<0 && worldAxes(axis) >= 0) {
               listHeader(os, pc, widthAxis, widthCoordType, widthCoordNumber, widthName, 
                          widthProj, widthShape, widthTile, 
                          widthRefValue, widthRefPixel, widthInc, widthUnits,
                          false, coordinate, axis, -1, 
                          precRefValSci, precRefValFloat, precRefValRADEC, 
                          precRefPixFloat, precIncSci, latticeShape, tileShape);
//
               if (pc->type() == Coordinate::SPECTRAL) {
                  listVelocity (os, pc, widthAxis, widthCoordType, widthCoordNumber, widthName, 
                                widthProj, widthShape, widthTile, 
                                widthRefValue, widthRefPixel, widthInc, widthUnits,
                                false, axis, -1, doppler,
                                precRefValSci, precRefValFloat, precRefValRADEC, 
                                precRefPixFloat, precIncSci);
               }
            }
         }
      }
//
      delete pc;
   }
   os << endl;

// Post it

   if (postLocally) {
      os.postLocally();
   } else {
      os.post();
   }
//

   n = lsi.nelements();
   Vector<String> messages(n-iStart);
   if (postLocally) {
      for (uint32_t i=iStart; i<n; i++) {
         messages(i) = lsi.getMessage(i);
      }
   }
   return messages;
}

void CoordinateSystem::getFieldWidths (LogIO& os, uint32_t& widthAxis, uint32_t& widthCoordType, uint32_t& widthCoordNumber,
                                       uint32_t& widthName, uint32_t& widthProj,  
                                       uint32_t& widthShape, uint32_t& widthTile, uint32_t& widthRefValue, 
                                       uint32_t& widthRefPixel, uint32_t& widthInc, uint32_t& widthUnits,  
                                       int32_t& precRefValSci, int32_t& precRefValFloat, int32_t& precRefValRADEC, 
                                       int32_t& precRefPixFloat, int32_t& precIncSci, String& nameAxis, 
                                       String& nameCoordType, String& nameCoordNumber, String& nameName, String& nameProj,
                                       String& nameShape, String& nameTile, String& nameRefValue,
                                       String& nameRefPixel, String& nameInc, String& nameUnits,
                                       MDoppler::Types doppler,
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
   bool doShape = tileShape.nelements()>0 && 
                  latticeShape.nelements()>0 &&
                  tileShape.nelements()==latticeShape.nelements();

// Header names for fields

   nameAxis = "Axis";
   nameCoordType = "Type";
   nameCoordNumber = "Coord";
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
   widthRefPixel = widthInc = widthUnits = widthCoordType = widthAxis = 0;
   widthCoordNumber = 0;

// Loop over number of world axes

   int32_t pixelAxis;
   uint32_t worldAxis;
   int32_t coordinate, axisInCoordinate;
   for (worldAxis=0; worldAxis<nWorldAxes(); worldAxis++) {

// Find coordinate number for this pixel axis
 
      findWorldAxis(coordinate, axisInCoordinate, worldAxis);
      pixelAxis = worldAxisToPixelAxis(worldAxis);

// Update widths of fields

      const Coordinate& c = CoordinateSystem::coordinate(coordinate);
      Coordinate* pc = c.clone();
      listHeader (os, pc,  widthAxis, widthCoordType, widthCoordNumber, widthName, 
                  widthProj, widthShape, widthTile, 
                  widthRefValue, widthRefPixel, widthInc, widthUnits,
                  true, coordinate, axisInCoordinate, pixelAxis,
                  precRefValSci, precRefValFloat,
                  precRefValRADEC, precRefPixFloat, precIncSci, 
                  latticeShape, tileShape);
//
      if (pc->type() == Coordinate::SPECTRAL) {
         listVelocity (os, pc, widthAxis, widthCoordType, widthCoordNumber, widthName, widthProj, widthShape, 
                       widthTile, widthRefValue, widthRefPixel, widthInc, widthUnits,
                       true, axisInCoordinate, pixelAxis, doppler,
                       precRefValSci, precRefValFloat, precRefValRADEC, 
                       precRefPixFloat, precIncSci);
      }
//
      delete pc;
   }


// Compare with header widths.  We only list the coordinate type
// if we are not listing the shape 

   widthAxis = max(nameAxis.length(), widthAxis) + 1;
   widthCoordType = max(nameCoordType.length(), widthCoordType) + 1;
   widthCoordNumber = max(nameCoordNumber.length(), widthCoordNumber) + 1;
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


void CoordinateSystem::listHeader (LogIO& os,  Coordinate* pc, uint32_t& widthAxis, uint32_t& widthCoordType, 
                                   uint32_t& widthCoordNumber, uint32_t& widthName,  uint32_t& widthProj, 
                                   uint32_t& widthShape, uint32_t& widthTile, 
                                   uint32_t& widthRefValue,  uint32_t& widthRefPixel, uint32_t& widthInc,  
                                   uint32_t& widthUnits,  bool findWidths, 
                                   int32_t coordinate, int32_t axisInCoordinate,  
                                   int32_t pixelAxis, int32_t precRefValSci, 
                                   int32_t precRefValFloat, int32_t precRefValRADEC, int32_t precRefPixFloat, 
                                   int32_t, const IPosition& latticeShape, const IPosition& tileShape) const
//
// List all the good stuff
//
//  Input:
//     os               The LogIO to write to
//     pc               Pointer to the coordinate
//     coordinate       Coordinate number
//     axisIncoordinate The axis number in this coordinate 
//     pixelAxis        The axis in the image for this axis in this coordinate
//           
{

// Clear flags

   if (!findWidths) clearFlags(os);

// Axis number

   String string;
   {
     ostringstream oss;
     if (pixelAxis != -1) {
       //oss << pixelAxis + 1;
        oss << pixelAxis;
     } else {
        oss << "..";
     }
     string = String(oss);
     if (findWidths) {
        widthAxis = max(widthAxis, string.length());
     } else {
        os.output().setf(ios::left, ios::adjustfield);
        os.output().width(widthAxis);
        os << string;
     }
   }

// Coordinate number

   {
     ostringstream oss;
     //oss << coordinate + 1;
     oss << coordinate;
     string = String(oss);
     if (findWidths) {
        widthCoordNumber = max(widthCoordNumber, string.length());
     } else {
        os.output().setf(ios::left, ios::adjustfield);
        os.output().width(widthCoordNumber);
        os << string;
     }
   }

// Coordinate type

   string = Coordinate::typeToString(pc->type());
   if (findWidths) {
      widthCoordType = max(widthCoordType, string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthCoordType);
      os << string;
   }

// Axis name

   string = pc->worldAxisNames()(axisInCoordinate);
   if (pc->type() == Coordinate::SPECTRAL) {
      SpectralCoordinate* sc = dynamic_cast<SpectralCoordinate*>(pc);
      if (sc->isTabular()) {
	 string += " (tab)";
      }
   }
   if (findWidths) {
      widthName = max(widthName, string.length());
   } else {
      os.output().setf(ios::left, ios::adjustfield);
      os.output().width(widthName);
      os << string;
   }


// Projection

   if (pc->type() == Coordinate::DIRECTION) {
      DirectionCoordinate* dc = dynamic_cast<DirectionCoordinate*>(pc);
      string = dc->isNCP() ? "NCP" : dc->projection().name();
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

   bool doShape = tileShape.nelements()>0 && 
                  latticeShape.nelements()>0 &&
                  tileShape.nelements()==latticeShape.nelements();
   if (doShape) {
      if (pixelAxis != -1) {
         ostringstream oss;
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
         ostringstream oss;
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

   String refValListUnits;
   {
      Coordinate::formatType form;
      int32_t prec;
//
      if (pc->type() == Coordinate::STOKES) {
         StokesCoordinate* sc = dynamic_cast<StokesCoordinate*>(pc);
//
         Vector<double> world(1);
         Vector<double> pixel(1);
         String sName;
         form = Coordinate::DEFAULT;

         if (pixelAxis != -1) {
            const uint32_t nPixels = sc->stokes().nelements();
            for (uint32_t i=0; i<nPixels; i++) {
               pixel(0) = double(i);
               bool ok = sc->toWorld(world, pixel);
               String temp;
               if (ok) {
                  temp = sc->format(refValListUnits, form, world(0), 
                                    axisInCoordinate, true, true, -1);
               } else {
                  temp = "?";
               }
               if (i>0) {
                  sName += String(" ") + temp;
               } else {
                  sName += temp;
               }
            }
         } else {
            pixel(0) = (*pixel_replacement_values_p[coordinate])[axisInCoordinate];
            bool ok = sc->toWorld(world, pixel);
            if (ok) {
               sName = sc->format(refValListUnits, form, world(0), 
                                  axisInCoordinate, true, true, -1);
            } else {
               sName = "?";
            }
         }
         string = sName;
      } else if (pc->type() == Coordinate::QUALITY) {
    	  QualityCoordinate* qc = dynamic_cast<QualityCoordinate*>(pc);
    //
    	  Vector<double> world(1);
    	  Vector<double> pixel(1);
    	  String sName;
    	  form = Coordinate::DEFAULT;

    	  if (pixelAxis != -1) {
    		  const uint32_t nPixels = qc->quality().nelements();
    		  for (uint32_t i=0; i<nPixels; i++) {
    			  pixel(0) = double(i);
    			  bool ok = qc->toWorld(world, pixel);
    			  String temp;
    			  if (ok) {
    				  temp = qc->format(refValListUnits, form, world(0),
    						  axisInCoordinate, true, true, -1);
    			  } else {
    				  temp = "?";
    			  }
    			  if (i>0) {
    				  sName += String(" ") + temp;
                  } else {
                	  sName += temp;
                  }
    		  }
    	  } else {
    		  pixel(0) = (*pixel_replacement_values_p[coordinate])[axisInCoordinate];
    		  bool ok = qc->toWorld(world, pixel);
    		  if (ok) {
    			  sName = qc->format(refValListUnits, form, world(0),
    					  axisInCoordinate, true, true, -1);
    		  } else {
    			  sName = "?";
    		  }
    	  }
    	  string = sName;
      } else {
         form = Coordinate::DEFAULT;
         pc->getPrecision(prec, form, true, precRefValSci, 
                          precRefValFloat, precRefValRADEC);
         if (pixelAxis != -1) {
            string = pc->format(refValListUnits, form, 
                                pc->referenceValue()(axisInCoordinate),
                                axisInCoordinate, true, true, prec);
         } else {
            Vector<double> world;
            pc->toWorld(world, (*pixel_replacement_values_p[coordinate]));
            string = pc->format(refValListUnits, form, 
                                world(axisInCoordinate),
                                axisInCoordinate, true, true, prec);
         }
      }
   }
   if (findWidths) {
      widthRefValue = max(widthRefValue,string.length());
   } else {
      os.output().width(widthRefValue);
      os << string;
   }

// Reference pixel

   if (pc->type() != Coordinate::STOKES && (pc->type()!= Coordinate::QUALITY)) {
      ostringstream oss;
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(precRefPixFloat);
      if (pixelAxis != -1) {
	oss << pc->referencePixel()(axisInCoordinate); // + 1.0;
      } else {
	oss << (*pixel_replacement_values_p[coordinate])[axisInCoordinate]; // + 1.0;
      }
      string = String(oss);
      if (findWidths) {
         widthRefPixel = max(widthRefPixel,string.length());
      } else {
         os.output().width(widthRefPixel);
         os << string;
      }
   }


// Increment

   String incUnits;
   if (pc->type() != Coordinate::STOKES && (pc->type()!= Coordinate::QUALITY)) {
      if (pixelAxis != -1) {
         Coordinate::formatType form;
         int32_t prec;
         form = Coordinate::SCIENTIFIC;
         pc->getPrecision(prec, form, false, precRefValSci, 
                          precRefValFloat, precRefValRADEC);
         string = pc->format(incUnits, form, 
                             pc->increment()(axisInCoordinate),
                             axisInCoordinate, false, false, prec);
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

// Units

   if ((pc->type()!= Coordinate::STOKES) && (pc->type()!= Coordinate::QUALITY)) {
      if (pixelAxis != -1) {
         string = " " + incUnits;
      } else {
         string = " " + refValListUnits;
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


void CoordinateSystem::listVelocity (LogIO& os,  Coordinate* pc, uint32_t widthAxis, 
                                    uint32_t widthCoordType, uint32_t widthCoordNumber, 
                                    uint32_t& widthName, uint32_t widthProj,
                                    uint32_t widthShape, uint32_t widthTile, uint32_t& widthRefValue, 
                                    uint32_t widthRefPixel, uint32_t& widthInc,  uint32_t& widthUnits, 
                                    bool findWidths, int32_t axisInCoordinate, 
                                    int32_t pixelAxis, MDoppler::Types doppler,
                                    int32_t precRefValSci, int32_t precRefValFloat, 
                                    int32_t precRefValRADEC, int32_t precRefPixFloat,
                                    int32_t precIncSci) const
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

// Caste to get SpectralCoordinate 

    SpectralCoordinate* sc0 = dynamic_cast<SpectralCoordinate*>(pc);
    SpectralCoordinate sc(*sc0);

// If the rest freq is non-positive, out we go

    double restFreq = sc.restFrequency();
    if (restFreq <=0.0) return;

// Axis number

   String string;
   if (!findWidths) {
      os.output().width(widthAxis);
      string = " ";
      os << string;
   }

// Coordinate number

   if (!findWidths) {
      os.output().width(widthCoordNumber);
      string = " ";
      os << string;
   }

// Coordinate type

   if (!findWidths) {
      os.output().width(widthCoordType);
      string = " ";
      os << string;
   }

// Axis name

   string = "Velocity";
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
         string = " ";
         os << string;
      }

// Tile shape

      if (!findWidths) {   
         os.output().width(widthTile);
         string = " ";
         os << string;
      }
   }

// Remember units

   Vector<String> oldUnits(sc.nWorldAxes());
   oldUnits = sc.worldAxisUnits();
   Vector<String> units(sc.nWorldAxes());

// Convert reference pixel it to a velocity and format 

   Coordinate::formatType form;
   String velUnits("km/s");
   int32_t prec;
   form = Coordinate::DEFAULT;
   sc.getPrecision(prec, form, true, precRefValSci, 
                    precRefValFloat, precRefValRADEC);
//
   String empty;
   sc.setVelocity (empty, doppler);
   string = sc.format(velUnits, form, 
                      sc.referenceValue()(axisInCoordinate),
                      axisInCoordinate, true, true, prec);
   if (findWidths) {
      widthRefValue = max(widthRefValue,string.length());
   } else {
      os.output().width(widthRefValue);
      os << string;
   }

// Reference pixel

   if (pixelAxis != -1) {
      ostringstream oss;
      oss.setf(ios::fixed, ios::floatfield);
      oss.precision(precRefPixFloat);
      oss << sc.referencePixel()(axisInCoordinate); // + 1.0;
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
     double velocityInc;
     if (!velocityIncrement(velocityInc, sc, doppler, velUnits)) {
        string = "Fail";
     } else {
        ostringstream oss;
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



bool CoordinateSystem::velocityIncrement(double& velocityInc, SpectralCoordinate& sc,
                                         MDoppler::Types doppler, const String& velUnits) const
{

// DO this the hard way for now until Wim gives me spectralMachine

   if (sc.nWorldAxes() != 1) return false;
   double refPix = sc.referencePixel()(0);

// Find world values at refPix +/- 0.5 and take difference

   double pixel;
   pixel = refPix + 0.5;
   Quantum<double> velocity1;
   sc.setVelocity (velUnits, doppler);
   if (!sc.pixelToVelocity(velocity1, pixel)) return false;
//
   pixel = refPix - 0.5;
   Quantum<double> velocity2;
   if (!sc.pixelToVelocity(velocity2, pixel)) return false;

// Return increment
   
   velocityInc = velocity1.getValue() - velocity2.getValue();

   return true;
}




void CoordinateSystem::listDirectionSystem(LogIO& os) const
{
   int32_t afterCoord = -1;
   int32_t ic = findCoordinate(Coordinate::DIRECTION, afterCoord);
   if (ic >= 0) {
      const DirectionCoordinate& coord = directionCoordinate(uint32_t(ic));
      MDirection::Types type = coord.directionType();
      MDirection::Types conversionType;
      coord.getReferenceConversion(conversionType);
//
      if (type==conversionType) {
         os << "Direction reference : " << MDirection::showType(type) << endl;
      } else {
         os << "Direction reference : " << MDirection::showType(type) << 
               " (-> " << MDirection::showType(conversionType) << ")" << endl;
      }
   }
}



void CoordinateSystem::listFrequencySystem(LogIO& os, MDoppler::Types doppler) const
{
   int32_t afterCoord = -1;
   int32_t ic = findCoordinate(Coordinate::SPECTRAL, afterCoord);
   if (ic >= 0) {
      const SpectralCoordinate& coord = spectralCoordinate(uint32_t(ic));
      MFrequency::Types type = coord.frequencySystem();
      MFrequency::Types conversionType;
      MEpoch epoch;
      MDirection direction;
      MPosition position;
      coord.getReferenceConversion(conversionType, epoch, position, direction);
//
      if (type==conversionType) {
         os << "Spectral  reference : " << MFrequency::showType(type) << endl;
      } else {
         os << "Spectral  reference : " << MFrequency::showType(type) << 
               " (-> " << MFrequency::showType(conversionType) << ")" << endl;
      }
//
      os << "Velocity  type      : " << MDoppler::showType(doppler) << endl;
//
      String str = coord.formatRestFrequencies();
      if (!str.empty()) os << str << endl;
   }
}


void CoordinateSystem::listPointingCenter (LogIO& os) const
{
   int32_t afterCoord = -1;
   int32_t iC = findCoordinate(Coordinate::DIRECTION, afterCoord);
   if (iC >= 0) {
      if (!obsinfo_p.isPointingCenterInitial()) {
         int32_t prec;
         Coordinate::formatType form(Coordinate::DEFAULT);
         coordinates_p[iC]->getPrecision(prec, form, true, 6, 6, 6);
//
         MVDirection pc = obsinfo_p.pointingCenter();
         Quantum<double> qLon = pc.getLong(Unit(String("deg")));
         Quantum<double> qLat = pc.getLat(Unit(String("deg")));
//
         String listUnits;
         String lon = coordinates_p[iC]->formatQuantity(listUnits, form, qLon,
                                                        0, true, true, prec);
         String lat  = coordinates_p[iC]->formatQuantity(listUnits, form, qLat,
                                                        1, true, true, prec);
//
         ostringstream oss;
         oss << "Pointing center     :  " << lon << "  " << lat;
         os << String(oss) << endl;
      }
   }
}


StokesCoordinate CoordinateSystem::stokesSubImage(const StokesCoordinate& sc, int32_t originShift, int32_t pixincFac,
                                                  int32_t newShape) const
{
   const Vector<int32_t>& values = sc.stokes();
   const int32_t nValues = values.nelements();
//
   int32_t start = originShift;
   if (start < 0 || start > nValues-1) {
      throw(AipsError("Illegal origin shift"));
   }
//
   Vector<int32_t> newStokes(nValues);   
   int32_t j = start;
   int32_t n = 0;
   while (j <= nValues-1) {
      newStokes(n) = values(j);
      n++;
      j += pixincFac;
   }
   
// If shape given, use it
   
   if (newShape>0) {
      if (newShape>n) {
         throw(AipsError("New shape is invalid"));
      }
//
      newStokes.resize(newShape, true);
   } else {
      newStokes.resize(n, true);
   }
//  
   StokesCoordinate scOut(sc);
   scOut.setStokes(newStokes);
   return scOut;
}

QualityCoordinate CoordinateSystem::qualitySubImage(const QualityCoordinate& qc, int32_t originShift,
		int32_t pixincFac,
		int32_t newShape) const
{
   const Vector<int32_t>& values = qc.quality();
   const int32_t nValues = values.nelements();
//
   int32_t start = originShift;
   if (start < 0 || start > nValues-1) {
      throw(AipsError("Illegal origin shift"));
   }
//
   Vector<int32_t> newQuality(nValues);
   int32_t j = start;
   int32_t n = 0;
   while (j <= nValues-1) {
	   newQuality(n) = values(j);
      n++;
      j += pixincFac;
   }

// If shape given, use it

   if (newShape>0) {
      if (newShape>n) {
         throw(AipsError("New shape is invalid"));
      }
//
      newQuality.resize(newShape, true);
   } else {
	  newQuality.resize(n, true);
   }
//
   QualityCoordinate qcOut(qc);
   qcOut.setQuality(newQuality);
   return qcOut;
}


bool CoordinateSystem::setWorldMixRanges (const IPosition& shape)
{
   AlwaysAssert(shape.nelements()==nPixelAxes(), AipsError);
//
   for (uint32_t i=0; i<nCoordinates(); i++) {
      Vector<int32_t> pA = pixelAxes(i);
      Vector<int32_t> wA = worldAxes(i);
      IPosition shape2(coordinates_p[i]->nPixelAxes());
      for (uint32_t j=0; j<shape2.nelements(); j++) {
         if (pA(j) != -1) {
            shape2(j) = shape(pA(j));
         } else {
            shape2(j) = -1;          // Pixel axis removed
         }             
      }

// Set range for this coordinate. If both pixel and world
// axis removed, use reference pixel for centre location

      if (!coordinates_p[i]->setWorldMixRanges (shape2)) {
         set_error(coordinates_p[i]->errorMessage());
         return false;
      }

// If there is a removed pixel axis, but not world axis
// we can be cleverer.   We need to use the removed pixel coordinate
// value as the centre value. The DC knows nothing about the removal,
// its the CS that knows this.

      if (coordinates_p[i]->type()==Coordinate::DIRECTION) {
         DirectionCoordinate* dC = dynamic_cast<DirectionCoordinate*>(coordinates_p[i]);
         Vector<double> pixel(dC->referencePixel().copy());
         Vector<bool> which(dC->nWorldAxes(), false);
         bool doit = false;
         for (uint32_t j=0; j<pixel.nelements(); j++) {
            if (pA(j)==-1 && wA(j)>=0) {
               pixel(j) = pixel_replacement_values_p[i]->operator()(j);
               which(j) = true;
               doit = true;
            }
         }
//
         if (doit) {
            Vector<double> world;
            dC->toWorld(world, pixel);
            dC->setWorldMixRanges(which, world);
         }
      }
   }
//cerr <<  "min, max = " << worldMixMin() << worldMixMax() << endl;
   return true;
}


void CoordinateSystem::setDefaultWorldMixRanges ()
{
   for (uint32_t i=0; i<nCoordinates(); i++) {
      coordinates_p[i]->setDefaultWorldMixRanges ();
   }
}

Vector<double> CoordinateSystem::worldMixMin () const
//
// Could speed up by holding min/max in a private variable
//
{
   Vector<double> wm(nWorldAxes());
   for (uint32_t i=0; i<nWorldAxes(); i++) {
      int32_t coord, coordAxis;
      findWorldAxis(coord, coordAxis, i);
      Vector<double> tmp = coordinates_p[coord]->worldMixMin();
      wm(i) = tmp(coordAxis);
   }
   return wm;
}

Vector<double> CoordinateSystem::worldMixMax () const
//
// Could speed up by holding min/max in a private variable
//
{
   Vector<double> wm(nWorldAxes());
   for (uint32_t i=0; i<nWorldAxes(); i++) {
      int32_t coord, coordAxis;
      findWorldAxis(coord, coordAxis, i);
      Vector<double> tmp = coordinates_p[coord]->worldMixMax();
      wm(i) = tmp(coordAxis);
   }
   return wm;
}


void CoordinateSystem::cleanUpSpecCoord (PtrBlock<SpectralCoordinate*>&  in, 
                                         PtrBlock<SpectralCoordinate*>&  out)
{
   for (uint32_t i=0; i<in.nelements(); i++) {
      if (in[i]) {
         delete in[i];
         in[i] = 0;
      }
   }
   for (uint32_t i=0; i<out.nelements(); i++) {
      if (out[i]) {
         delete out[i];
         out[i] = 0;
      }
   }
}



CoordinateSystem CoordinateSystem::stripRemovedAxes (const CoordinateSystem& cSys) const
{
    CoordinateSystem cSysOut;
//
    bool noWorld, noPixel;

// Loop over coordinates

    uint32_t j = 0;
    for (uint32_t i=0; i<cSys.nCoordinates(); i++) {
       const Coordinate& coord = cSys.coordinate(i); 
//
       const Vector<int32_t>& worldAxes = cSys.worldAxes(i);
       const Vector<int32_t>& pixelAxes = cSys.pixelAxes(i);
       noWorld = allEQ(worldAxes, -1);
       noPixel = allEQ(pixelAxes, -1);
// 
       if (!noWorld || !noPixel) {
   
// This coordinate has some pixel or world axes left, so hang onto it
       
          cSysOut.addCoordinate(coord);

// We must remove, in the output CS, the same world/pixel
// axes that have been removed in the input CS
  
          Vector<double> refVal = coord.referenceValue();
          Vector<double> refPix = coord.referencePixel();
          const Vector<int32_t>& worldAxesOut = cSysOut.worldAxes(j);
          const Vector<int32_t>& pixelAxesOut = cSysOut.pixelAxes(j);
          for (uint32_t k=worldAxes.nelements(); k>0;) {
             k--;
//
             if (worldAxes(k) == -1) {
                cSysOut.removeWorldAxis(worldAxesOut(k), refVal(k));    // Assumes worldAxes in ascending order
             }
          }
          for (uint32_t k=worldAxes.nelements(); k>0;) {
             k--;
//
             if (pixelAxes(k) == -1) {
                cSysOut.removePixelAxis(pixelAxesOut(k), refPix(k));    // Assumes pixelAxes in ascending order
             }
          }
//
          j++;
       }
    }
//
   return cSysOut;
}


/*
bool CoordinateSystem::checkWorldReplacementAxis(int32_t& coordinate,
                                                 int32_t& axisInCoordinate,
                                                 uint32_t axis) const
{

// Find number of (unremoved) world axes

    const uint32_t nC = nCoordinates();
    uint32_t nWorld = 0;
    for (uint32_t i=0; i<nC; i++) {
       nWorld += world_maps_p[i]->nelements();
    }   

// Check axis

    if (axis >= nWorld) {
       ostringstream oss;
       oss << "Illegal removal world axis number (" << axis << "), max is ("
           << nWorld << ")" << endl;
       set_error (String(oss));
       return false;
    }

// Find coordinate and axis in coordinate

    coordinate = -1;
    axisInCoordinate = -1;
    int32_t value;
    for (uint32_t i=0; i<nC; i++) {
	const uint32_t na = world_maps_p[i]->nelements();
	for (uint32_t j=0; j<na; j++) {
           value = world_maps_p[i]->operator[](j);       // We stored -1 * (axis + 1) when we removed it
           if (value < 0) {
              value = -1 * (value + 1);            
              if (value == int32_t(axis)) {
		coordinate = i;
		axisInCoordinate = j;
                break;
              }
           }
	}
    }

// Check

    if (coordinate==-1 || axisInCoordinate==-1) {
       ostringstream oss;
       oss << "Cannot find world axis " << axis << endl;
       set_error (String(oss));
       return false;
    }
//
    return true;
}



bool CoordinateSystem::checkPixelReplacementAxis(int32_t& coordinate,
                                                 int32_t& axisInCoordinate,
                                                 uint32_t axis) const
{

// Find number of (unremoved) pixel axes

    const uint32_t nC = nCoordinates();
    uint32_t nPixel = 0;
    for (uint32_t i=0; i<nC; i++) {
       nPixel += pixel_maps_p[i]->nelements();
    }   

// Check axis

    if (axis >= nPixel) {
       ostringstream oss;
       oss << "Illegal removal pixel axis number (" << axis << "), max is ("
           << nPixel << ")" << endl;
       set_error (String(oss));
       return false;
    }

// Find coordinate and axis in coordinate for removed axis

    coordinate = -1;
    axisInCoordinate = -1;
    int32_t value;
    for (uint32_t i=0; i<nC; i++) {
	const uint32_t na = pixel_maps_p[i]->nelements();
	for (uint32_t j=0; j<na; j++) {
           value = pixel_maps_p[i]->operator[](j);       // We stored -1 * (axis + 1) when we removed it
           if (value < 0) {
              value = -1 * (value + 1);            
              if (value == int32_t(axis)) {
                 coordinate = i;
                 axisInCoordinate = j;
                 break;
              }
           }
	}
    }

// Check 

    if (coordinate==-1 || axisInCoordinate==-1) {
       ostringstream oss;
       oss << "Cannot find removed pixel axis " << axis << endl;
       set_error (String(oss));
       return false;
    }
//
    return true;
}

*/


bool CoordinateSystem::mapOne(Vector<int32_t>& worldAxisMap,
                              Vector<int32_t>& worldAxisTranspose,
                              Vector<bool>& refChange,
                              const CoordinateSystem& cSys1,
                              const CoordinateSystem& cSys2,
                              const uint32_t coord1,
                              const uint32_t coord2) const
//
// Update the world axis mappings from one coordinate system to another.  
//
{

// Make tests on specific coordinate types here. We already
// know that the two cSys are the same coordinate type 
// (e.g. DIRECTION)

   bool refDiff = false;
   if (cSys2.coordinate(coord2).type() == Coordinate::DIRECTION) {
      if (cSys1.directionCoordinate(coord1).directionType() != 
          cSys2.directionCoordinate(coord2).directionType()) {
         refDiff = true;
      }
   } else if (cSys2.coordinate(coord2).type() == Coordinate::SPECTRAL) {
      if (cSys1.spectralCoordinate(coord1).frequencySystem() != 
         cSys2.spectralCoordinate(coord2).frequencySystem()) {
         refDiff = true;
      }
   }

// How many world and pixel axes for these coordinates

   uint32_t nWorld1 = cSys1.worldAxes(coord1).nelements();
   uint32_t nWorld2 = cSys2.worldAxes(coord2).nelements();
   uint32_t nPixel1 = cSys1.pixelAxes(coord1).nelements();
   uint32_t nPixel2 = cSys2.pixelAxes(coord2).nelements();

// These tests should never fail

   if (nWorld1 != nWorld2) return false;
   if (nPixel1 != nPixel2) return false;

// Find their world  and pixel axes

   Vector<int32_t> world1 = cSys1.worldAxes(coord1);
   Vector<int32_t> pixel1 = cSys1.pixelAxes(coord1);
   Vector<int32_t> world2 = cSys2.worldAxes(coord2);
   Vector<int32_t> pixel2 = cSys2.pixelAxes(coord2);
   const Vector<String>& units1 = cSys1.coordinate(coord1).worldAxisUnits();
   const Vector<String>& units2 = cSys2.coordinate(coord2).worldAxisUnits();

// Compare quantities for the world axes.  

   for (uint32_t j=0; j<nWorld2; j++) {
      if (world2(j) != -1) {
         if (world1(j) != -1) {

// Compare intrinsic axis units.  Should never fail.

            if (Unit(units1(j)) != Unit(units2(j))) return false;

// Set the world axis maps

            worldAxisMap(world2(j)) = world1(j);
            worldAxisTranspose(world1(j)) = world2(j);
            refChange(world1(j)) = refDiff;
         } else {

// The world axis is missing in cSys1 and present in cSys2.  

           return false;
         }

// The world axis has been removed in cSys2 so we aren't interested in it
     
      }
   }
//
   return true;
}




void CoordinateSystem::deleteTemps (const uint32_t which)
{
   delete world_maps_p[which]; 
   world_maps_p[which] = 0;
//
   delete world_tmps_p[which]; 
   world_tmps_p[which] = 0;
//
   delete world_replacement_values_p[which]; 
   world_replacement_values_p[which] = 0;
//
   delete pixel_maps_p[which]; 
   pixel_maps_p[which] = 0;
//
   delete pixel_tmps_p[which]; 
   pixel_tmps_p[which] = 0;
//
   delete pixel_replacement_values_p[which]; 
   pixel_replacement_values_p[which] = 0;
//
   delete worldAxes_tmps_p[which]; 
   worldAxes_tmps_p[which] = 0;
//
   delete pixelAxes_tmps_p[which]; 
   pixelAxes_tmps_p[which] = 0;
//
   delete worldOut_tmps_p[which]; 
   worldOut_tmps_p[which] = 0;
//
   delete pixelOut_tmps_p[which]; 
   pixelOut_tmps_p[which] = 0;
//
   delete worldMin_tmps_p[which]; 
   worldMin_tmps_p[which] = 0;
//
   delete worldMax_tmps_p[which]; 
   worldMax_tmps_p[which] = 0;
}

bool CoordinateSystem::hasSpectralAxis() const {
    int32_t spectralCoordNum = findCoordinate(Coordinate::SPECTRAL);
    return (spectralCoordNum >= 0 && spectralCoordNum < (int32_t)nCoordinates());
}

int32_t CoordinateSystem::spectralCoordinateNumber() const {
    return findCoordinate(Coordinate::SPECTRAL);
}

int32_t CoordinateSystem::spectralAxisNumber(bool doWorld) const {
    if (! hasSpectralAxis()) {
        return -1;
    }
    int32_t specIndex = findCoordinate(Coordinate::SPECTRAL);
    if (doWorld) {
      return worldAxes(specIndex)[0];
    }
    return pixelAxes(specIndex)[0];
}


bool CoordinateSystem::hasPolarizationCoordinate() const {
    int32_t polarizationCoordNum = findCoordinate(Coordinate::STOKES);
    return (
        polarizationCoordNum >= 0
        && polarizationCoordNum < (int32_t)nCoordinates()
    );
}

int32_t CoordinateSystem::polarizationCoordinateNumber() const {
    // don't do hasPolarizationAxis check or you will go down an infinite recursion path :)
    return findCoordinate(Coordinate::STOKES);
}

int32_t CoordinateSystem::polarizationAxisNumber(bool doWorld) const {
    if (! hasPolarizationCoordinate()) {
        return -1;
    }
    if (doWorld) {
      return worldAxes(polarizationCoordinateNumber())[0];
    }
    return pixelAxes(polarizationCoordinateNumber())[0];
}

bool CoordinateSystem::hasQualityAxis() const {
    int32_t qualityCoordNum = findCoordinate(Coordinate::QUALITY);
    return (
    		qualityCoordNum >= 0
        && qualityCoordNum < (int32_t)nCoordinates()
    );
}

int32_t CoordinateSystem::qualityAxisNumber() const {
    if (! hasQualityAxis()) {
        return -1;
    }
    return pixelAxes(qualityCoordinateNumber())[0];
}

int32_t CoordinateSystem::qualityCoordinateNumber() const {
    // don't do hasQualityAxis check or you will go down an infinite recursion path :)
    return findCoordinate(Coordinate::QUALITY);
}

int32_t CoordinateSystem::qualityPixelNumber(const String& qualityString) const{
    if (! hasQualityAxis()) {
        return -1;
    }
    int32_t qualCoordNum = findCoordinate(Coordinate::QUALITY);
    QualityCoordinate qualityCoord = qualityCoordinate(qualCoordNum);
    int32_t qualityPix = -1;
    qualityCoord.toPixel(qualityPix, Quality::type(qualityString));
    if (qualityPix < 0) {
        return -1;
    }
    return qualityPix;
}

String CoordinateSystem::qualityAtPixel(const uint32_t pixel) const {
    if (! hasQualityAxis()) {
         return "";
    }
    int32_t qualCoordNum = qualityCoordinateNumber();
    QualityCoordinate qualityCoord = qualityCoordinate(qualCoordNum);
    int32_t quality = qualityCoord.quality()[pixel];
    Quality::QualityTypes qualityType = Quality::type(quality);
    return Quality::name(qualityType);
}

int32_t CoordinateSystem::stokesPixelNumber(const String& stokesString) const {
    if (! hasPolarizationCoordinate()) {
        return -1;
    }
    int32_t polCoordNum = findCoordinate(Coordinate::STOKES);
    StokesCoordinate stokesCoord = stokesCoordinate(polCoordNum);
    int32_t stokesPix = -1;
    stokesCoord.toPixel(stokesPix, Stokes::type(stokesString));
    if (stokesPix < 0) {
        return -1;
    }
    return stokesPix;
}

String CoordinateSystem::stokesAtPixel(const uint32_t pixel) const {
    if (! hasPolarizationCoordinate()) {
         return "";
    }
    int32_t polCoordNum = polarizationCoordinateNumber();
    StokesCoordinate stokesCoord = stokesCoordinate(polCoordNum);
    int32_t stokes = stokesCoord.stokes()[pixel];
    Stokes::StokesTypes stokesType = Stokes::type(stokes);
    return Stokes::name(stokesType);
}

int32_t CoordinateSystem::directionCoordinateNumber() const {
    // don't do a hasDirectionCoordinate() check or you will go down an infinite recursion path
    return findCoordinate(Coordinate::DIRECTION);
}

bool CoordinateSystem::hasDirectionCoordinate() const {
    int32_t directionCoordNum = directionCoordinateNumber();
    return (
        directionCoordNum >= 0
        && directionCoordNum < (int32_t)nCoordinates()
    );
}

Vector<int32_t> CoordinateSystem::directionAxesNumbers() const {
    if (! hasDirectionCoordinate()) {
      return Vector<int32_t>();
    }
    return pixelAxes(directionCoordinateNumber());
}

int32_t CoordinateSystem::linearCoordinateNumber() const {
    // don't do a hasLinearCoordinate() check or you will go down an infinite recursion path
    return findCoordinate(Coordinate::LINEAR);
}

bool CoordinateSystem::hasLinearCoordinate() const {
    int32_t linearCoordNum = linearCoordinateNumber();
    return (
        linearCoordNum >= 0
        && linearCoordNum < (int32_t)nCoordinates()
    );
}

Vector<int32_t> CoordinateSystem::linearAxesNumbers() const {
    if (! hasLinearCoordinate()) {
      return Vector<int32_t>();
    }
    return pixelAxes(linearCoordinateNumber());
}

void CoordinateSystem::_initFriendlyAxisMap() {
        std::lock_guard<std::mutex> lock(_mapInitMutex);
	if (_friendlyAxisMap.size() == 0) {
		_friendlyAxisMap["velocity"] = "spectral";
		_friendlyAxisMap["frequency"] = "spectral";
		_friendlyAxisMap["right ascension"] = "ra";
	}
}

Vector<int32_t> CoordinateSystem::getWorldAxesOrder(
	Vector<String>& myNames, bool requireAll,
	bool allowFriendlyNames
) const {
	LogIO os(LogOrigin(_class, __FUNCTION__, WHERE));
	if (allowFriendlyNames) {
		_initFriendlyAxisMap();
	}
	uint32_t naxes = nWorldAxes();
	uint32_t raxes = myNames.size();
	if (requireAll && raxes != naxes) {
		os << "Image has " << naxes << " axes but " << raxes
			<< " were given. Number of given axes must match the number of image axes"
			<< LogIO::EXCEPTION;
	}
	Vector<String> axisNames = worldAxisNames();
	_downcase(axisNames);
	_downcase(myNames);
	Vector<int32_t> myorder(raxes);

	Vector<String> matchMap(naxes, "");
	for (uint32_t i=0; i<myNames.size(); i++) {
		String name = myNames[i];
		vector<String> matchedNames(0);
		vector<uint32_t> matchedNumbers(0);
		for (uint32_t j=0; j<axisNames.size(); j++) {
			if (
				axisNames[j].startsWith(name)
				|| (
					allowFriendlyNames
					&& _friendlyAxisMap.find(axisNames[j]) != _friendlyAxisMap.end()
					&& _friendlyAxisMap[axisNames[j]].startsWith(name)
				)
			) {
				matchedNames.push_back(axisNames[j]);
				matchedNumbers.push_back(j);
			}
		}
		if(matchedNames.size() == 0) {
			os << "No axis matches requested axis " << name
				<< ". Image axis names are " << axisNames
				<< LogIO::EXCEPTION;
		}
		else if (matchedNames.size() > 1) {
			os << "Multiple axes " << matchedNames << " match requested axis "
				<< name << LogIO::EXCEPTION;
		}
		uint32_t axisIndex = matchedNumbers[0];
		if (matchMap[axisIndex].empty()) {
			myorder[i] = axisIndex;
			matchMap[axisIndex] = name;
		}
		else {
			os << "Ambiguous axis specification. Both " << matchMap[axisIndex]
			    << " and " << name << " match image axis name "
			    << matchedNames[0] << LogIO::EXCEPTION;
		}
	}
	return myorder;
}

bool CoordinateSystem::isDirectionAbscissaLongitude() const {
	ThrowIf(
		! hasDirectionCoordinate(),
		"Coordinate system has no direction coordinate"
	);
	Vector<int32_t> dirPixelAxes = directionAxesNumbers();
	ThrowIf(
		dirPixelAxes(0) == -1 || dirPixelAxes(1) == -1,
		"The pixel axes for the DirectionCoordinate have been removed"
	);
	return dirPixelAxes(0) < dirPixelAxes(1);
}

void CoordinateSystem::setSpectralConversion (
	const String frequencySystem
) {
	String err;
	ThrowIf(
		! setSpectralConversion(err, frequencySystem),
		err
	);
}

bool CoordinateSystem::setSpectralConversion (
	String& errorMsg, const String frequencySystem
) {
	if (! hasSpectralAxis()) {
		return true;
	}
	if (! hasDirectionCoordinate()) {
		errorMsg = String("No DirectionCoordinate; cannot set Spectral conversion layer");
		return false;
	}
	MFrequency::Types ctype;
	if (!MFrequency::getType(ctype, frequencySystem)) {
		errorMsg = String("invalid frequency system " + frequencySystem);
		return false;
	}

	SpectralCoordinate coord = spectralCoordinate();
	MFrequency::Types oldctype;
	MEpoch epoch;
	MPosition position;
	MDirection direction;
	coord.getReferenceConversion(oldctype, epoch, position, direction);
	if (ctype == oldctype) {
		return true;
	}
	const DirectionCoordinate& dCoord = directionCoordinate();
	const Vector<double>& rp = dCoord.referencePixel();
	if (!dCoord.toWorld(direction, rp)) {
		errorMsg = dCoord.errorMessage();
		return false;
	}

	const ObsInfo& oi = obsInfo();
	String telescope = oi.telescope();
	if (! MeasTable::Observatory(position, telescope)) {
		errorMsg = String("Cannot find observatory; cannot set Spectral conversion layer");
		return false;
	}
	epoch = oi.obsDate();
	double t = epoch.getValue().get();
	if (t <= 0.0) {
		errorMsg = String("Epoch not valid; cannot set Spectral conversion layer");
		return false;
	}
	coord.setReferenceConversion(ctype, epoch, position, direction);
	replaceCoordinate(coord, this->spectralCoordinateNumber());
	return true;
}

bool CoordinateSystem::setRestFrequency (
	String& errorMsg, const Quantity& freq
) {
	double value = freq.getValue();
	if (value < 0.0) {
		errorMsg = "The rest frequency/wavelength is below zero!";
		return false;
	}
	else if (isNaN(value)) {
		errorMsg = "The rest frequency/wavelength is NaN!";
		return false;
	}
	else if (isInf(value)) {
		errorMsg = "The rest frequency/wavelength is InF!";
		return false;
	}
	static Unit HZ(String("GHz"));
	static Unit M(String("m"));
	Unit t(freq.getUnit());
	if (t != HZ && t!= M) {
		errorMsg = "Illegal spectral unit " + freq.getUnit();
		return false;
	}
	if (! hasSpectralAxis()) {
		return true;
	}
	SpectralCoordinate sCoord = spectralCoordinate();
	Unit oldUnit(sCoord.worldAxisUnits()[0]);

	MVFrequency newFreq = MVFrequency(freq);
	double newValue = newFreq.get(oldUnit).getValue();

	if (isNaN(newValue)) {
		errorMsg = "The new rest frequency/wavelength is NaN!";
		return false;
	}
	else if (isInf(newValue)) {
		errorMsg = "The new rest frequency/wavelength is InF!";
		return false;
	}

	if (!sCoord.setRestFrequency(newValue)) {
		errorMsg = sCoord.errorMessage();
		return false;
	}
	this->replaceCoordinate(sCoord, spectralCoordinateNumber());
	return true;
}

} //# NAMESPACE CASACORE - END
