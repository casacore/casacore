//# TiledShape.h: Define the shape and tile shape
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
//# $Id$

#if !defined (AIPS_TILEDSHAPE_H)
#define AIPS_TILEDSHAPE_H

//# Includes
#include <aips/Lattices/IPosition.h>

//# Forward Declarations
template<class T> class Vector;


// <summary>
// Define the shape and tile shape
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <linkto class=IPosition>IPosition</linkto>
// </prerequisite>

// <etymology>
// TiledShape defines the shape and tile shape of a tiled array.
// </etymology>

// <synopsis> 
// TiledShape is "logically" a Vector<Int> constrained so that it's origin
// is zero-based, and in fact that used to be the way it was implemented.
// It was split out into a separate class to make the inheritance from
// Arrays simpler (since Arrays use TiledShapes). The
// template instantiation mechanism is complicated enough that this
// simplification was felt to be a good idea.
// <p>
// TiledShape objects are normally used to index into, and define the shapes
// of, multi-dimensional arrays. For example, if you have a 5 dimensional
// array, you need an TiledShape of length 5 to index into the array (or
// to define its shape, etc.).
// <p>
// Unlike Vectors, TiledShapes always use copy semantics.
// <srcblock>
// TiledShape ip1(5);                         // An TiledShape of length 5
// ip1(0) = 11; ip1(1) = 5; ... ip1(4) = 6;  // Indices 0-based
// TiledShape ip2(ip1);                       // Copy constructor; a COPY
// </srcblock>
//
// Binary operations must take place either with a conformnat (same size)
// TiledShape or with an integer, which behaves as if it was an TiledShape
// of the same size (i.e., length). All the usual binary arithmetic
// operations are available, as well as logical operations, which return
// Booleans. These all operate "element-by-element".
// <p>
// All non-inlined member functions of TiledShape check invariants if the
// preprocessor symbol AIPS_DEBUG is defined.
// That is, the member functions check that ok() is true (constructors
// check after construction, other functions on entry to the function).
// If these tests fail, an AipsError exception is thrown; its message
// contains the line number and source file of the failure (it is thrown
// by the lAssert macro defined in aips/Assert.h).
//
// <example>
// <srcblock>
// TiledShape blc(5), trc(5,1,2,3,4,5);
// blc = 0;            // OR TiledShape blc(5,0);
// //...
// if (blc > trc) {
//    TiledShape tmp;
//    tmp = trc;       // Swap
//    trc = blc;
//    blc = tmp;
// }
// //...
// trc += 5;           // make the box 5 larger in all dimensions
// </srcblock>
// </example>


class TiledShape
{
public:
    // Use the given shape.
    // No tile shape is given, so function <src>tileShape</src>
    // will calculate it using the size of a tile.
    TiledShape (const IPosition& shape);

    // Use the given shape and tile shape.
    TiledShape (const IPosition& shape, const IPosition& tileShape);

    // Copy constructor (copy semantics).
    TiledShape (const TiledShape& that);

    ~TiledShape();

    // Assignment (copy semantics).
    TiledShape& operator= (const TiledShape& that);

    // Is the tile shape defined.
    Bool isTileShapeDefined() const;

    // Return the shape.
    const IPosition& shape() const;

    // Return the tile shape.
    // When the tile shape is undefined, the default tile shape will be
    // calculated using the given tile size and tolerance.
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


#endif
