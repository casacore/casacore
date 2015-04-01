//# TiledShape.h: Define the shape and tile shape
//# Copyright (C) 1997,1998,1999,2001
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

#ifndef LATTICES_TILEDSHAPE_H
#define LATTICES_TILEDSHAPE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Vector;


// <summary>
// Define the shape and tile shape
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tTiledShape.cc">
// </reviewed>

// <prerequisite>
//  <li> <linkto class=IPosition>IPosition</linkto>
// </prerequisite>

// <etymology>
// TiledShape defines the shape and tile shape of a tiled array.
// </etymology>

// <synopsis> 
// TiledShape is a class defining the shape and optionally the tile
// shape of a lattice. It is used in the constructors of
// <linkto class=PagedArray>PagedArray</linkto> and
// <linkto class=PagedImage>PagedImage</linkto>.
// <p>
// In principle it serves as a place holder for the lattice shape and
// tile shape. The functions <src>shape</src> and <src>tileShape</src>
// can be used to retrieve the shapes.
// However, when the tile shape is not given, the function
// <src>tileShape</src> calculates a default tile shape using the
// given maximum tile size in pixel elements. The default tile shape
// is calculated in such a way that the sizes of its axes
// are proportional to the sizes of the lattice axes. Per axis it is
// tried as much as possible to fit an integral number of tiles
// in the lattice.
// <br>In this way getting the tile shape is completely transparent.
// </synopsis>

// <example>
// <srcblock>
// // Do not explicitly define a tile shape.
// // This results in a default tile shape (of 32,32,32).
// TiledShape shape(IPosition(3,128,128,128));
// cout << shape.shape() << ' ' << shape.tileShape() << endl;
//
// // Use with an explicitly given tile shape.
// TiledShape shape(IPosition(3,128,128,128), IPosition(3,64,32,8));
// cout << shape.shape() << ' ' << shape.tileShape() << endl;
// </srcblock>
// </example>

// <motivation>
// Classes <src>PagedArray</src> and <src>PagedImage</src> contained
// several duplicated constructors to be able to pass a tile shape.
// This class makes it possible to have only one constructor
// instead of two. Furthermore it contains the logic to check if the
// shapes are conforming and the logic to calculate a default tile shape.
// </motivation>


class TiledShape
{
public:
    // Default constructor has empty shape and tile shape.
    TiledShape();

    // Use the given shape.
    // No tile shape is given, so function <src>tileShape</src>
    // will calculate it using the size of a tile.
    TiledShape (const IPosition& shape);

    // Use the given shape and tile shape.
    // Both shapes must be conforming (i.e. have same number of elements).
    TiledShape (const IPosition& shape, const IPosition& tileShape);

    // Copy constructor (copy semantics).
    TiledShape (const TiledShape& that);

    ~TiledShape();

    // Assignment (copy semantics).
    TiledShape& operator= (const TiledShape& that);

    // Is the tile shape defined?
    Bool isTileShapeDefined() const;

    // Return the shape.
    const IPosition& shape() const;

    // Return the tile shape.
    // When the tile shape is undefined, the default tile shape will be
    // calculated using the given tile size and tolerance.
    // <br> The tolerance is used to determine the boundaries where
    // it is tried to fit an integral number of tiles.
    IPosition tileShape (uInt nrPixelsPerTile = 32768,
			 Double tolerance = 0.5) const;

    // Derive the default tile shape from the shape for the given
    // number of pixels per tile. It is tried to get the same number
    // of tiles for each dimension.
    // When a weight vector is given, the number of tiles for a dimension
    // is proportional to the weight.
    // <br>After the initial guess it tries to optimize it by trying to
    // waste as little space as possible, while trying to keep as close as
    // possible to the initial guess. The given tolerance (possibly per axis)
    // gives the minimum and maximum possible length of a tile axis
    // (minimum = initial_guess*tolerance; maximum = initial_guess/tolerance).
    // The heuristic is such that a tile axis length dividing the cube length
    // exactly is always favoured.
    // The test program <src>tTiledShape</src> can be used to see how
    // the algorithm works out for a given shape and tile size.
    // <group>
    IPosition defaultTileShape (uInt nrPixelsPerTile, Double tolerance) const;
    IPosition defaultTileShape (uInt nrPixelsPerTile,
				const Vector<Double>& tolerance,
				const Vector<Double>& weight) const;
    // </group>

private:
    IPosition itsShape;
    IPosition itsTileShape;
    Bool      itsTileDefined;
};


inline Bool TiledShape::isTileShapeDefined() const
{
    return itsTileDefined;
}
inline const IPosition& TiledShape::shape() const
{
    return itsShape;
}
inline IPosition TiledShape::tileShape (uInt nrPixelsPerTile,
					Double tolerance) const
{
    return (itsTileDefined  ?  itsTileShape :
	                       defaultTileShape (nrPixelsPerTile, tolerance));
}



} //# NAMESPACE CASACORE - END

#endif
