//# TiledShape.cc: Define the shape and tile shape
//# Copyright (C) 1997,2000,2001
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


#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TiledShape::TiledShape()
: itsTileDefined (True)
{}

TiledShape::TiledShape (const IPosition& shape)
: itsShape       (shape),
  itsTileDefined (False)
{
    uInt n = shape.nelements();
    for (uInt i=0; i<n; i++) {
	if (shape(i) <= 0) {
	    throw (AipsError ("TiledShape: shape has to be > 0"));
	}
    }
}

TiledShape::TiledShape (const IPosition& shape, const IPosition& tileShape)
: itsShape       (shape),
  itsTileShape   (tileShape),
  itsTileDefined (True)
{
    uInt n = shape.nelements();
    if (tileShape.nelements() != n) {
	throw (AipsError
                    ("TiledShape: #elements in shape and tileShape differ"));
    }
    for (uInt i=0; i<n; i++) {
	if (tileShape(i) <= 0) {
	    throw (AipsError ("TiledShape: tileShape has to be > 0"));
	}
	if (shape(i) < tileShape(i)) {
	    throw (AipsError ("TiledShape: shape has to be >= tileShape"));
	}
    }
}

TiledShape::TiledShape (const TiledShape& that)
: itsShape       (that.itsShape),
  itsTileShape   (that.itsTileShape),
  itsTileDefined (that.itsTileDefined)
{}

TiledShape::~TiledShape()
{}

TiledShape& TiledShape::operator= (const TiledShape& that)
{
    if (this != &that) {
	itsShape.resize (that.itsShape.nelements());
	itsShape       = that.itsShape;
	itsTileShape.resize (that.itsTileShape.nelements());
	itsTileShape   = that.itsTileShape;
	itsTileDefined = that.itsTileDefined;
    }
    return *this;
}


IPosition TiledShape::defaultTileShape (uInt nrPixelsPerTile,
					Double tolerance) const
{
    uInt n = itsShape.nelements();
    Vector<Double> tol(n);
    tol = tolerance;
    Vector<double> weight(n);
    weight = double(1);
    return defaultTileShape (nrPixelsPerTile, tol, weight);
}

IPosition TiledShape::defaultTileShape (uInt nrPixelsPerTile,
					const Vector<Double>& tolerance,
					const Vector<Double>& weight) const
{
    uInt nrdim = itsShape.nelements();
    if (tolerance.nelements() != nrdim  ||  weight.nelements() != nrdim) {
	throw (AipsError ("TiledShape::defaultTileShape: nelements mismatch"));
    }
    double nrLeft = nrPixelsPerTile;
    Vector<double> tmpShape(nrdim);
    IPosition tileShape(nrdim, 0);
    uInt i;
    Int j;
    // Iterate until the tile shape is set nicely.
    // This is needed to prevent tile shape dimensions from underflow
    // or overflow.
    while (True) {
	double prod = 1;
	uInt n = 0;
	for (i=0; i<nrdim; i++) {
	    if (tileShape(i) == 0) {
		prod *= itsShape(i) * weight(i);
		n++;
	    }
	}
	// Exit if nothing left.
	if (n == 0) {
	    break;
	}
	double factor = pow (nrLeft / prod, double(1) / n);
	double maxDiff = 0;
	double diff;
	Int maxIndex = -1;
	// Calculate the tile shape for the remaining dimensions.
	// Determine the greatest difference in case of underflow/overflow.
	// (note that the reciproke is used, thus in fact the minimum matters).
	// That tile dimension will be set and the iteration starts again.
	for (i=0; i<nrdim; i++) {
	    if (tileShape(i) == 0) {
		diff = itsShape(i) * weight(i) * factor;
		tmpShape(i) = diff;
		if (diff > 1) {
		    diff = itsShape(i) / diff;
		}
		if (maxIndex < 0  ||  diff < maxDiff) {
		    maxDiff  = diff;
		    maxIndex = i;
		}
	    }
	}
	// If there is no underflow/overflow we can copy the dimensions
	// and exit.
	if (maxDiff >= 1) {
	    for (i=0; i<nrdim; i++) {
		if (tileShape(i) == 0) {
		    tileShape(i) = Int(tmpShape(i) + 0.5);   // round-off
		}
	    }
	    break;
	}
	// Set the dimension with the greatest difference.
	if (tmpShape(maxIndex) < 1) {
	    tileShape(maxIndex) = 1;
	}else{
	    tileShape(maxIndex) = itsShape(maxIndex);
	    nrLeft /= tileShape(maxIndex);
	}
    }
    // Return the found tile shape when fitting exactly.
    Bool isFit = True;
    Double size = 1;
    for (i=0; i<nrdim; i++) {
	if (itsShape(i) % tileShape(i) != 0) {
	    isFit = False;
	}
	size *= itsShape(i);
    }
    if (isFit) {
	return tileShape;
    }
    // When the cube shape <= 4* the maximum tile size, return that.
    if (size <= 4*nrPixelsPerTile) {
	return itsShape;
    }

    // We have to do a bit more to find a nice tile shape.
    // Use the tolerance to find the tile shape boundaries to search.
    IPosition bestShape (tileShape);
    IPosition minShape (nrdim);
    IPosition maxShape (nrdim);
    Double cubeSpace = 1;
    for (i=0; i<nrdim; i++) {
	minShape(i) = Int (tileShape(i) * tolerance(i));
	maxShape(i) = Int (tileShape(i) / tolerance(i) + 0.5);
	if (minShape(i) > maxShape(i)) {
	    Int sav = minShape(i);
	    minShape(i) = maxShape(i);
	    maxShape(i) = sav;
	}
	if (minShape(i) < 1) {
	    minShape(i) = 1;
	}
	if (maxShape(i) > itsShape(i)) {
	    maxShape(i) = itsShape(i);
	}
	cubeSpace *= itsShape(i);
    }
    // Find the shapes on each axis that will be tried.
    Block<uInt> nval(nrdim, uInt(0));
    PtrBlock<Block<Int>*> values(nrdim);
    for (i=0; i<nrdim; i++) {
	values[i] = new Block<Int> (maxShape(i) - minShape(i) + 1);
	// First find exactly fitting shapes.
	for (j=minShape(i); j<=maxShape(i); j++) {
	    if (itsShape(i) % j == 0) {
		(*values[i])[nval[i]] = j;
		nval[i]++;
	    }
	}
	// If none available, use all possible shapes within half the range..
	if (nval[i] == 0) {
	    for (j=(tileShape(i)+minShape(i))/2;
		     j<=(tileShape(i)+maxShape(i))/2; j++) {
		(*values[i])[nval[i]] = j;
		nval[i]++;
	    }
	}
    }
    // Now calculate the cost for all the possibilities.
    // Take the one with the lowest cost.
    Block<uInt> ndone (nrdim, uInt(0));
    IPosition tshape (nrdim);
    for (i=0; i<nrdim; i++) {
	tshape(i) = (*values[i])[0];
    }
    Double minCost = 1000000;
    while (True) {
	Int totalSize = 1;
	Double totalSpace = 1;
	Double costAxes = 0;
	for (i=0; i<nrdim; i++) {
	    totalSize *= tshape(i);
	    Int ntile = (itsShape(i) + tshape(i) - 1) / tshape(i);
	    totalSpace *= ntile * tshape(i);
	    costAxes += abs(tileShape(i) - tshape(i)) / double(tileShape(i));
	}
	Double waste = (totalSpace - cubeSpace) / cubeSpace;
	Double diff  = abs(double(totalSize) -
					   nrPixelsPerTile) / nrPixelsPerTile;
	Double cost = (costAxes + 10*waste + diff);
	if (cost < minCost) {
	    bestShape = tshape;
	    minCost = cost;
	}
///	cout << cost << " " << costAxes << " " << waste << " "
///	     << diff << " " << tshape << endl;
	for (i=0; i<nrdim; i++) {
	    if (++ndone[i] < nval[i]) {
		tshape(i) = (*values[i])[ndone[i]];
		break;
	    }
	    ndone[i] = 0;
	    tshape(i) = (*values[i])[0];
	}
	if (i == nrdim) {
	    break;
	}
    }
    // Optimize the tile shape by recalculating tile length for the same
    // number of tiles.
    for (i=0; i<nrdim; i++) {
	delete values[i];
	uInt nrtile = (itsShape(i) + bestShape(i) - 1) / bestShape(i);
	bestShape(i) = (itsShape(i) + nrtile - 1) / nrtile;
    }
    return bestShape;
}

} //# NAMESPACE CASACORE - END

