//# CoordinateSystem.h: Interconvert pixel and image coordinates.
//# Copyright (C) 1997
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
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>


#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Utilities/Assert.h>
#include <aips/Measures/Unit.h>
#include <aips/Measures/UnitMap.h>

#include <aips/Containers/Record.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>

#include <strstream.h>


CoordinateSystem::CoordinateSystem()
    : coordinates_p(0), 
      world_maps_p(0), world_tmps_p(0), world_replacement_values_p(0),
      pixel_maps_p(0), pixel_tmps_p(0), pixel_replacement_values_p(0)
      
{
    // Nothing
}

void CoordinateSystem::copy(const CoordinateSystem &other)
{
    if (this == &other) {
	return;
    }

    clear();

    coordinates_p = other.coordinates_p;
    const uInt n = coordinates_p.nelements();

    for (uInt i=0; i < n; i++) {
	coordinates_p[i] = coordinates_p[i]->clone();
	AlwaysAssert(coordinates_p[i], AipsError);
    }

    world_maps_p.resize(n);
    world_tmps_p.resize(n);
    world_replacement_values_p.resize(n);
    pixel_maps_p.resize(n);
    pixel_tmps_p.resize(n);
    pixel_replacement_values_p.resize(n);

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
    }
}

CoordinateSystem::CoordinateSystem(const CoordinateSystem &other)
    : coordinates_p(0), 
      world_maps_p(0), world_tmps_p(0), world_replacement_values_p(0),
      pixel_maps_p(0), pixel_tmps_p(0), pixel_replacement_values_p(0)
      
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

    // coordinates_p
    const uInt n = coordinates_p.nelements(); // "before" n, index of new coord
    coordinates_p.resize(n+1);
    coordinates_p[n] = coord.clone();
    AlwaysAssert(coordinates_p[n] != 0, AipsError);

    // world_maps_p
    world_maps_p.resize(n+1);
    world_maps_p[n] = new Block<Int>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_maps_p[n], AipsError);
    for (uInt i=0; i < world_maps_p[n]->nelements(); i++) {
	world_maps_p[n]->operator[](i) = oldWorldAxes + i;
    }

    // world_tmps_p
    world_tmps_p.resize(n+1);
    world_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_tmps_p[n], AipsError);

    // world_replacement_values_p
    world_replacement_values_p.resize(n+1);
    world_replacement_values_p[n] = 
	new Vector<Double>(coordinates_p[n]->nWorldAxes());
    AlwaysAssert(world_replacement_values_p[n], AipsError);

    // pixel_maps_p
    pixel_maps_p.resize(n+1);
    pixel_maps_p[n] = new Block<Int>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_maps_p[n], AipsError);
    for (i=0; i < pixel_maps_p[n]->nelements(); i++) {
	pixel_maps_p[n]->operator[](i) = oldPixelAxes + i;
    }

    // pixel_tmps_p
    pixel_tmps_p.resize(n+1);
    pixel_tmps_p[n] = new Vector<Double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_tmps_p[n], AipsError);

    // pixel_replacement_values_p
    pixel_replacement_values_p.resize(n+1);
    pixel_replacement_values_p[n] = 
	new Vector<Double>(coordinates_p[n]->nPixelAxes());
    AlwaysAssert(pixel_replacement_values_p[n], AipsError);
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
    for (uInt i=0; i<found.nelements(); i++) {
	Int which = newWorldOrder(i);
	AlwaysAssert(which >=0 && which < nw && !found[which], AipsError);
	found[which] = True;
    }
    found.resize(np);
    found = False;
    for (i=0; i<found.nelements(); i++) {
	Int which = newPixelOrder(i);
	AlwaysAssert(which >=0 && which < np && !found[which], AipsError);
	found[which] = True;
    }

    PtrBlock<Block<Int> *> newWorldMaps(nc);
    PtrBlock<Block<Int> *> newPixelMaps(nc);
    newWorldMaps.set((Block<Int> *)0);
    newPixelMaps.set((Block<Int> *)0);

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

void CoordinateSystem::removeWorldAxis(uInt axis, Double replacement) 
{
    AlwaysAssert(axis <= nWorldAxes(), AipsError);

    const uInt nc = nCoordinates();

    Int coord, caxis;
    findWorldAxis(coord, caxis, axis);
    world_replacement_values_p[coord]->operator()(caxis) = replacement;
    world_maps_p[coord]->operator[](caxis) = -1;

    for (uInt i=0; i<nc; i++) {
	for (uInt j=0; j<world_maps_p[i]->nelements(); j++) {
	    if (world_maps_p[i]->operator[](j) > axis) {
		world_maps_p[i]->operator[](j)--;
	    }
	}
    }
}

void CoordinateSystem::removePixelAxis(uInt axis, Double replacement) 
{
    AlwaysAssert(axis <= nPixelAxes(), AipsError);

    const uInt nc = nCoordinates();

    Int coord, caxis;
    findPixelAxis(coord, caxis, axis);
    pixel_replacement_values_p[coord]->operator()(caxis) = replacement;
    pixel_maps_p[coord]->operator[](caxis) = -1;

    for (uInt i=0; i<nc; i++) {
	for (uInt j=0; j<pixel_maps_p[i]->nelements(); j++) {
	    if (pixel_maps_p[i]->operator[](j) > axis) {
		pixel_maps_p[i]->operator[](j)--;
	    }
	}
    }
}

CoordinateSystem CoordinateSystem::subImage(const Vector<Int> &originShift,
					    const Vector<Int> &pixinc) const
{
    AlwaysAssert(originShift.nelements() == nPixelAxes() &&
                 pixinc.nelements() == nPixelAxes(), AipsError);

    // We could get rid of this assumption by multiplying by accounting for the PC
    // matrix as well as cdelt, or going through group-by-group, but it doesn't
    // seem necessary now, or maybe ever.
    AlwaysAssert(originShift.nelements() == pixinc.nelements(), AipsError);

    uInt n = nPixelAxes();

    CoordinateSystem coords = *this;
    Vector<Double> crpix = coords.referencePixel();
    Vector<Double> cdelt = coords.increment();

    // Not efficient, but easy and this code shouldn't be called often
    for (uInt i=0; i<n; i++) {
        AlwaysAssert(pixinc(i) >= 1, AipsError);
        crpix(i) -= originShift(i);
        crpix(i) /= pixinc(i);
        cdelt(i) *= pixinc(i);
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

const Coordinate &CoordinateSystem::coordinate(uInt which) const
{
    AlwaysAssert(which <= nCoordinates(), AipsError);
    return *(coordinates_p[which]);
}

const LinearCoordinate &CoordinateSystem::linearCoordinate(uInt which) const
{
    AlwaysAssert(which <= nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::LINEAR, AipsError);
    return (const LinearCoordinate &)(*(coordinates_p[which]));
}

const DirectionCoordinate &CoordinateSystem::directionCoordinate(uInt which) const
{
    AlwaysAssert(which <= nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::DIRECTION, AipsError);
    return (const DirectionCoordinate &)(*(coordinates_p[which]));
}

const SpectralCoordinate &CoordinateSystem::spectralCoordinate(uInt which) const
{
    AlwaysAssert(which <= nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::SPECTRAL, AipsError);
    return (const SpectralCoordinate &)(*(coordinates_p[which]));
}

const StokesCoordinate &CoordinateSystem::stokesCoordinate(uInt which) const
{
    AlwaysAssert(which <= nCoordinates() && 
		 coordinates_p[which]->type() == Coordinate::STOKES, AipsError);
    return (const StokesCoordinate &)(*(coordinates_p[which]));
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
	    if (world_maps_p[i]->operator[](j) == orig) {
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
	    if (pixel_maps_p[i]->operator[](j) == orig) {
		coordinate = i;
		axisInCoordinate = j;
		return;
	    }
	}
    }
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
	if (coord == whichCoord) {
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
	if (coord == whichCoord) {
	    retval(axis) = i;
	}
    }
    return retval;
}

Coordinate::Type CoordinateSystem::type() const
{
    return Coordinate::COORDSYS;
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
    // This is neede so we can write into some temporaries
    CoordinateSystem *This = (CoordinateSystem *)this;

    const uInt nc = coordinates_p.nelements();

    Bool ok = True;
    for (uInt i=0; i<nc; i++) {
	// For each coordinate, putt the appropriate pixel or replacement values in the
	// pixel temporary, call the coordinates own toWorld, and then copy the output
	// values from the world temporary to the world coordinate
	const uInt npa = pixel_maps_p[i]->nelements();
	for (uInt j=0; j<npa; j++) {
	    Int where = pixel_maps_p[i]->operator[](j);
	    if (where >= 0) {
		This->pixel_tmps_p[i]->operator()(j) = pixel(where);
	    } else {
		This->pixel_tmps_p[i]->operator()(j) = 
		    pixel_replacement_values_p[i]->operator()(j);
	    }
	}
	// cout << "world pixel map: " << *(world_maps_p[i]) << " " <<
	// *(pixel_maps_p[i]) << endl;
	// cout << "toWorld # " << i << "pix=" << pixel_tmps_p[i]->ac() << endl;
	ok = ToBool(ok && coordinates_p[i]->toWorld(
			    *(This->world_tmps_p[i]), *(pixel_tmps_p[i])));
	// cout << "toWorld # " << i << "wld=" << world_tmps_p[i]->ac() << endl;
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

// Move out of function in case it causes problems with our exception emulation.
    static Vector<Double> pix;
Bool CoordinateSystem::toWorld(Vector<Double> &world, 
			       const IPosition &pixel) const
{
    if (pix.nelements() != pixel.nelements()) {
	pix.resize(pixel.nelements());
    }

    uInt n = pixel.nelements();
    for (uInt i=0; i<n; i++) {
	pix(i) = pixel(i)*1.0;
    }

    return toWorld(world, pix);
}

Bool CoordinateSystem::toPixel(Vector<Double> &pixel, 
			       const Vector<Double> &world) const
{
    // This is neede so we can write into some temporaries
    CoordinateSystem *This = (CoordinateSystem *)this;

    const uInt nc = coordinates_p.nelements();

    Bool ok = True;
    for (uInt i=0; i<nc; i++) {
	// For each coordinate, putt the appropriate world or replacement values in the
	// world temporary, call the coordinates own toPixel, and then copy the output
	// values from the pixel temporary to the pixel coordinate
	const uInt nwra = world_maps_p[i]->nelements();
	for (uInt j=0; j<nwra; j++) {
	    Int where = world_maps_p[i]->operator[](j);
	    if (where >= 0) {
		This->world_tmps_p[i]->operator()(j) = world(where);
	    } else {
		This->world_tmps_p[i]->operator()(j) = 
		    world_replacement_values_p[i]->operator()(j);
	    }
	}
	ok = ToBool(ok && coordinates_p[i]->toPixel(
			    *(This->pixel_tmps_p[i]), *(world_tmps_p[i])));
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
    const uInt nc = nCoordinates();
    Bool ok = True;

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

Bool CoordinateSystem::setWorldAxisUnits(const Vector<String> &units,
					 Bool adjust)
{
    const uInt nc = nCoordinates();
    Bool ok = True;

    for (uInt i=0; i<nc; i++) {
	Vector<String> tmp = coordinates_p[i]->worldAxisUnits();
	uInt na = tmp.nelements();
	for (uInt j=0; j<na; j++) {
	    Int which = world_maps_p[i]->operator[](j);
	    if (which >= 0) {
		tmp(j) = units(which);
	    }
	}
	ok = ToBool(coordinates_p[i]->setWorldAxisUnits(tmp,adjust) && ok);
    }

    return ok;
}

Bool CoordinateSystem::setReferencePixel(const Vector<Double> &refPix)
{
    const uInt nc = nCoordinates();
    Bool ok = True;

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
	    for (uInt k=0; j<ncol; k++) {
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
    const uInt nc = nCoordinates();
    Bool ok = True;

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
    const uInt nc = nCoordinates();
    Bool ok = True;

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

String CoordinateSystem::format(Double worldValue, uInt worldAxis, 
				Int sigDigits) const
{
    AlwaysAssert(worldAxis < nWorldAxes(), AipsError);

    Int coord, axis;
    findWorldAxis(coord, axis, worldAxis);

    // Should never fail
    AlwaysAssert(coord>=0 && axis >= 0, AipsError);

    return coordinate(coord).format(worldValue, axis, sigDigits);
}


Bool CoordinateSystem::save(RecordInterface &container,
			    const String &fieldName) const
{
    Record subrec;
    if (container.isDefined(fieldName)) {
	return False;
    }

    uInt nc = coordinates_p.nelements();
    for (uInt i=0; i<nc; i++)
    {
	// Write eaach string into a field it's type plus coordinate number, e.g. direction0
	String basename = "unknown";
	switch (coordinates_p[i]->type()) {
	case Coordinate::LINEAR:    basename = "linear"; break;
	case Coordinate::DIRECTION: basename = "direction"; break;
	case Coordinate::SPECTRAL:  basename = "spectral"; break;
	case Coordinate::STOKES:    basename = "stokes"; break;
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
    container.defineRecord(fieldName, subrec);

    return True;
}

CoordinateSystem *CoordinateSystem::restore(const RecordInterface &container,
					   const String &fieldName)
{
    CoordinateSystem *retval = 0;

    if (!container.isDefined(fieldName)) {
	return retval;
    }

    Record subrec(container.asRecord(fieldName));
    uInt nfields = subrec.nfields();
    PtrBlock<Coordinate *> tmp;

    Int nc = 0; // num coordinates
    PtrBlock<Coordinate *> coords;
    String linear = "linear";
    String direction = "direction";
    String spectral = "spectral";
    String stokes = "stokes";
    String coordsys = "coordsys";
    while(1) {
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
	} else if (subrec.isDefined(coordsys + num)) {
	    coords.resize(nc);
	    coords[nc - 1] = CoordinateSystem::restore(subrec, coordsys+num);
	} else {
	    break;
	}
	AlwaysAssert(coords[nc-1] != 0, AipsError);
    }
    nc = coords.nelements();

    retval = new CoordinateSystem;
    for (uInt i=0; i<nc; i++) {
	retval->addCoordinate(*(coords[i]));
	delete coords[i];
	coords[i] = 0;
    }
    for (i=0; i<nc; i++) {
	retval->world_tmps_p[i] = 
	    new Vector<Double>(retval->coordinates_p[i]->nWorldAxes());
	AlwaysAssert(retval->world_tmps_p[i], AipsError);
	retval->pixel_tmps_p[i] = 
	    new Vector<Double>(retval->coordinates_p[i]->nPixelAxes());
	AlwaysAssert(retval->pixel_tmps_p[i], AipsError);
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

    return retval;
}


Coordinate *CoordinateSystem::clone() const
{
    return new CoordinateSystem(*this);
}

Bool CoordinateSystem::toFITSHeader(RecordInterface &header, Bool oneRelative,
				    char prefix) const
{
    LogIO os(LogOrigin("CoordinateSystem", "toFITSHeader", WHERE));

    os << LogIO::NORMAL <<
      "AIPS++ writes the DRAFT WCS coordinate convention. This will probably\n"
      "not cause any problems, except perhaps for the NCP projection." <<
	LogIO::POST;

    // ********** Validation

    const uInt n = nWorldAxes();

    if (nWorldAxes() != nPixelAxes()) {
	os << LogIO::SEVERE << "nWorldAxes(" << nWorldAxes() <<
	    ") != nPixelAxes(" << nPixelAxes() << ")";
	return False; // required for FITS
    }

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

    // ********** Canonicalize units and find sky axes
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

    for (uInt i=0; i<n ; i++) {
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

    // ********** Generate keywords

    // crval
    Vector<Double> crval = coordsys.referenceValue();

    // crpix
    Vector<Double> crpix = coordsys.referencePixel().ac() + offset;
    
    // cdelt
    Vector<Double> cdelt = coordsys.increment().ac();

    // ctype
    Vector<String> ctype = coordsys.worldAxisNames();
    for (i=0; i < n; i++) {
	if (i == longAxis) {
	    const DirectionCoordinate &dc = coordsys.directionCoordinate(skyCoord);
	    String name = dc.axisNames(dc.directionType(), True)(0);
	    while (name.length() < 4) {
		name += "-";
	    }
	    name = name + "-" + dc.projection().name();
	    ctype(i) = name.chars();
	} else if (i == latAxis) {
	    const DirectionCoordinate &dc = coordsys.directionCoordinate(skyCoord);
	    String name = dc.axisNames(dc.directionType(), True)(1);
	    while (name.length() < 4) {
		name += "-";
	    }
	    name = name + "-" + dc.projection().name();
	    ctype(i) = name.chars();
	} else if (i == specAxis) {
	    ctype(i) = "FREQ    ";
	} else if (i == stokesAxis) {
	    ctype(i) = "STOKES  ";
	} else {
	    ctype(i).upcase();
	    if (ctype(i).length() > 8) {
		ctype(i) = ctype(i).at(0,8);
	    }
	    while (ctype(i).length() < 8) {
		ctype(i) += " ";
	    }
	}
    }
    
    // cunit
    Vector<String> cunit = coordsys.worldAxisUnits();
    for (i=0; i<n; i++) {
	cunit(i).upcase();
	if (cunit(i).length() > 8) {
	    cunit(i) = cunit(i).at(0,8);
	}
	while (cunit(i).length() < 8) {
	    cunit(i) += " ";
	}
    }

    // projp
    Vector<Double> projp;
    if (skyCoord >= 0) {
	projp = coordsys.directionCoordinate(skyCoord).projection().parameters();
    }

    // pc
    Matrix<Double> pc = linearTransform();

    // crota: Greisen and Calabretta "Converting Previous Formats"
    Vector<Double> crota(n);
    crota = 0;
    if (longAxis >= 0 && latAxis >= 0) {
	Double rholong = atan2(pc(latAxis, longAxis)*C::pi/180.0,
			pc(longAxis, longAxis)*C::pi/180.0)*180.0/C::pi;
	Double rholat = atan2(-pc(longAxis, latAxis)*C::pi/180.0,
			pc(latAxis, latAxis)*C::pi/180.0)*180.0/C::pi;
	crota(latAxis) = (rholong + rholat)/2.0;
	if (!near(rholong, rholat)) {
	    os << LogIO::SEVERE << sprefix + "rota is not very accurate. PC matrix"
		" is not a pure rotation." << LogIO::POST;
	}
    }

    // Special stokes handling
    if (stokesCoord >= 0) {
	Vector<Int> stokes(coordsys.stokesCoordinate(stokesCoord).stokes());
	Int inc = 1;
	Bool inorder = True;
	if (stokes.nelements() > 1) {
	    inc = Stokes::FITSValue(Stokes::StokesTypes(stokes(1))) - 
		Stokes::FITSValue(Stokes::StokesTypes(stokes(0)));
	    for (uInt i=2; i<stokes.nelements(); i++) {
		if ((Stokes::FITSValue(Stokes::StokesTypes(stokes(i))) - 
		     Stokes::FITSValue(Stokes::StokesTypes(stokes(i-1)))) != inc) {
		    inorder = False;
		}
	    }
	}
	if (inorder) {
	    crval(stokesAxis) = Stokes::FITSValue(Stokes::StokesTypes(stokes(0)));
	    crpix(stokesAxis) = 1;
	    cdelt(stokesAxis) = inc;
	} else {
	    // !inorder
	    crval(stokesAxis) = Stokes::FITSValue(Stokes::StokesTypes(stokes(0))) + 200;
	    crpix(stokesAxis) = 1;
	    cdelt(stokesAxis) = 1;
	}
    }

    // Actually write the header
    header.define("pc", pc);
    header.define(sprefix + "type", ctype);
    header.define(sprefix + "rval", crval);
    header.define(sprefix + "delt", cdelt);
    header.define(sprefix + "rota", crota);
    header.define(sprefix + "rpix", crpix);
    header.define(sprefix + "unit", cunit);
    if (projp.nelements() > 0) {
	header.define("projp", projp);
	for (uInt i=0; i<projp.nelements(); i++) {
	    if (! nearAbs(projp(i), 0.0)) {
		os << LogIO::NORMAL << "PROJPn not all zero.\nOld FITS readers"
		    " will not interpret the coordinates correctly." <<
		    LogIO::POST;
		break;
	    }
	}
    }

    return True;
}

Bool CoordinateSystem::fromFITSHeader(CoordinateSystem &coordsys, 
				      const RecordInterface &header,
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

    Vector<Double> cdelt, crval, crpix;
    Vector<String> ctype, cunit;
    Matrix<Double> pc;
    Bool haveUnit = False;
    Int rotationAxis = -1;
    try {
	header.get(sprefix + "rval", crval);
	header.get(sprefix + "rpix", crpix);
	crpix.ac() -= offset;
	header.get(sprefix + "delt", cdelt);
	header.get(sprefix + "type", ctype);

	// Units are optional
	if (header.isDefined(sprefix + "unit")) {
	    header.get(sprefix + "unit", cunit);
	    UnitMap::addFITS();
	    haveUnit = True;
	}

	// PC and/or CROTA is optional. We prefer PC if it is defined.
	if (header.isDefined("pc")) {
	    if (header.isDefined(sprefix + "rota")) {
		os << "Ignoring redundant " << sprefix << "rota in favour of "
		    "pc matrix." << LogIO::NORMAL << LogIO::POST;
	    }
	    header.get("pc", pc);
	} else if (header.isDefined(sprefix + "rota")) {
	    Vector<Double> crota;
	    header.get(sprefix + "rota", crota);
	    // Turn crota into PC matrix
	    pc.resize(crota.nelements(), crota.nelements());
	    pc = 0.0;
	    pc.diagonal() = 1.0;
	    // We can only handle one non-zero angle
	    for (uInt i=0; i<crota.nelements(); i++) {
		if (!near(crota(i), 0.0)) {
		    if (rotationAxis >= 0) {
			os << LogIO::SEVERE << "Can only convert one non-zero"
			    " angle from " << sprefix << 
			    "rota to pc matrix. Using the first." <<
			    LogIO::POST;
		    } else {
			rotationAxis = i;
		    }
		}
	    }
	    if (rotationAxis >= 0 && pc.nrow() > 1) { // can't rotate 1D!
		if (rotationAxis > 0) {
		    pc(rotationAxis-1,rotationAxis-1) =
			pc(rotationAxis,rotationAxis) = 
			cos(crota(rotationAxis)*C::pi/180.0);
		    pc(rotationAxis-1,rotationAxis)=
			-sin(crota(rotationAxis)*C::pi/180.0);
		    pc(rotationAxis,rotationAxis-1)=
			sin(crota(rotationAxis)*C::pi/180.0);
		} else {
		    os << LogIO::NORMAL << "Unusual to rotate about first"
			" axis." << LogIO::POST;
		    pc(rotationAxis+1,rotationAxis+1) =
			pc(rotationAxis,rotationAxis) = 
			cos(crota(rotationAxis)*C::pi/180.0);
		    // Might be backwards?
		    pc(rotationAxis+1,rotationAxis)=
			-sin(crota(rotationAxis)*C::pi/180.0);
		    pc(rotationAxis,rotationAxis+1)=
			sin(crota(rotationAxis)*C::pi/180.0);
		}
	    }
	} else {
	    // Pure diagonal PC matrix
	    pc.resize(ctype.nelements(), ctype.nelements());
	    pc = 0.0;
	    pc.diagonal() = 1.0;
	}
	
    } catch (AipsError x) {
	os << LogIO::SEVERE << "Error retrieving *rval, *rpix, *delt, *type "
	    "from header";
	return False;
    } end_try;

    const uInt n = ctype.nelements();

    if (crval.nelements() != n || crpix.nelements() != n || 
	cdelt.nelements() != n || pc.nrow() != n || pc.ncolumn() != n ||
	(cunit.nelements() > 0 && cunit.nelements() != n)) {
	os << LogIO::SEVERE << "Inconsistent number of axes in header";
	return False;
    }

    // OK, find out what standard axes we have.
    Int longAxis=-1, latAxis=-1, stokesAxis=-1, specAxis=-1;
    for (uInt i=0; i<n; i++) {
	if (ctype(i).contains("RA") || ctype(i).contains("LON")) {
	    if (longAxis >= 0) {
		os << LogIO::SEVERE << "More than one longitude axis is "
		    "present in header!";
		return False;
	    }
	    longAxis = i;
	} else if (ctype(i).contains("DEC") || ctype(i).contains("LAT")) {
	    if (latAxis >= 0) {
		os << LogIO::SEVERE << "More than one latitude axis is "
		    "present in header!";
		return False; // we already have a latitude axis!
	    }
	    latAxis = i;
	} else if (ctype(i).contains("STOKES")) {
	    stokesAxis = i;
	} else if (ctype(i).contains("FREQ")) {
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
    for (uInt j=0; j<n; j++) {
	for (i=0; i<n; i++) {
	    if (i == j) {
		continue;
	    } else {
		if (!near(pc(i,j), 0.0)) {
		    if (rotationAxis < 0 || (i == longAxis && j == latAxis) ||
			(i == latAxis  && j == longAxis)) {
			continue;
		    } else {
			os << LogIO::SEVERE << sprefix + "rota may only" <<
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
	proj.gsub(Regex("^.*-"), "");  // Get rid of, e.g., RA---
	String proj2 = ctype(latAxis);
	proj2.gsub(Regex("^.*-"), "");
	if (proj != proj2) {
	    os << LogIO::SEVERE << "Longitude and latitude axes have different"
		" projections (" << proj << "!=" << proj2 << ")";
	    return False;
	}

	// OK, let's make our Direction coordinate and add it to the
	// coordinate system. We'll worry about transposing later. FITS
	// should always be degrees, but if the units are set we'll honor
	// them.

	// First, work out what the projection actually is.
	// Special case NCP - now SIN with 
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
	    os << LogIO::SEVERE << "Error forming projection, maybe the "
		"wrong number of parameters\n(" << x.getMesg() << ")" << 
		LogIO::POST;
	    return False;
	} end_try;

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
		if (near(epoch, 1950.0)) {
		    radecsys = MDirection::B1950;
		} else if (near(epoch, 2000.0)) {
		    radecsys = MDirection::J2000;
		}
	    } else if (header.isDefined("equinox") && 
		       (header.dataType("equinox") == TpDouble ||
			header.dataType("equinox") == TpDouble ||
			header.dataType("equinox") == TpInt)) {
		Double epoch = header.asdouble("equinox");
		if (near(epoch, 1950.0)) {
		    radecsys = MDirection::B1950;
		} else if (near(epoch, 2000.0)) {
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
	DirectionCoordinate dir(radecsys,
				projn,
				crval(longAxis)*toRadX,	crval(latAxis)*toRadY,
				cdelt(longAxis)*toRadX, cdelt(latAxis)*toRadY,
				dirpc,
				crpix(longAxis), crpix(latAxis));
	coordsys.addCoordinate(dir);
    }

    // STOKES
    if (stokesAxis >= 0) {
	Vector<Int> stokes(4); // at most 4 stokes
	// Must be stokes.nelements() since the default switch might resize
	// the vector.
	for (uInt i=0; i<stokes.nelements(); i++) {
	    Double tmp = crval(stokesAxis) + 
		(i - crpix(stokesAxis))*cdelt(stokesAxis);
	    if (tmp >= 0) {
		stokes(i) = Int(floor(tmp + 0.01));
	    } else {
		stokes(i) = Int(floor(tmp - 0.01));
	    }
	    switch (stokes(i)) {
	    case 1: stokes(i) = Stokes::I; break;
	    case 2: stokes(i) = Stokes::Q; break;
	    case 3: stokes(i) = Stokes::U; break;
	    case 4: stokes(i) = Stokes::V; break;
	    case -1: stokes(i) = Stokes::RR; break;
	    case -2: stokes(i) = Stokes::LL; break;
	    case -3: stokes(i) = Stokes::RL; break;
	    case -4: stokes(i) = Stokes::LR; break;
	    case -5: stokes(i) = Stokes::XX; break;
	    case -6: stokes(i) = Stokes::YY; break;
	    case -7: stokes(i) = Stokes::XY; break;
	    case -8: stokes(i) = Stokes::YX; break;
	    default:
		os << LogIO::NORMAL << "There are at most " << i << " known "
		    "Stokes values on the Stokes axis." << LogIO::POST;
		stokes.resize(i, True);
	    }
	}
	try {
	    StokesCoordinate sc(stokes);
	    coordsys.addCoordinate(sc);
	} catch (AipsError x) {
	    os << "Error forming stokes axis : " << x.getMesg() <<
		LogIO::SEVERE << LogIO::POST;
	    return False;
	} end_try;
    }

    // SPECTRAL
    if (specAxis >= 0) {
	Double toHz = 1.0; // assume Hz unless we're told otherwise
	if (cunit.nelements() > 0) {
	    Unit frequ = cunit(specAxis);
	    Unit hz = "Hz";
	    toHz = frequ.getValue().getFac()/hz.getValue().getFac();
	}
	os << LogIO::NORMAL << "Assuming frequencies are topocentric. Gripe if"
	    " this is a problem." << LogIO::POST;
	SpectralCoordinate sc(MFrequency::TOPO,
			      crval(specAxis)*toHz,
			      cdelt(specAxis)*toHz, 
			      crpix(specAxis));
	coordsys.addCoordinate(sc);
    }
    
    // Remaining axes are LINEAR
    uInt nlin = n;
    if (longAxis >= 0) {nlin--;}
    if (latAxis >= 0) {nlin--;}
    if (specAxis >= 0) {nlin--;}
    if (stokesAxis >= 0) {nlin--;}
    if (nlin > 0) {
	os << LogIO::NORMAL << "Assuming no rotation (etc) in linear axes." <<
	    LogIO::POST;
	Matrix<Double> linpc(nlin, nlin); linpc = 0; linpc.diagonal() = 1.0;
	Vector<Double> lincrpix(nlin), lincdelt(nlin), lincrval(nlin);
	Vector<String> linctype(nlin), lincunit(nlin);
	Int where_i = 0;
	for (uInt i=0; i<n; i++) {
	    if (i != longAxis && i != latAxis && i != stokesAxis &&
		i != specAxis) {
		lincrpix(where_i) = crpix(i);
		lincrval(where_i) = crval(i);
		lincdelt(where_i) = cdelt(i);
		linctype(where_i) = ctype(i);
		if (cunit.nelements() > 0) {
		    lincunit(where_i) = cunit(i);
		}
		where_i++;
	    }
	}
	Int where_j = 0;
	for (uInt j=0; j<n; j++) {
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
    if (longAxis >= 0) {nspecial++;}
    if (latAxis >= 0) {nspecial++;}
    if (stokesAxis >= 0) {nspecial++;}
    if (specAxis >= 0) {nspecial++;}
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
    coordsys.transpose(order, order);
    
    return True;
}
