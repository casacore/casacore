//# LCPolygon.h: Define a 2-dimensional region by a polygon
//# Copyright (C) 1998
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

#if !defined(AIPS_LCPOLYGON_H)
#define AIPS_LCPOLYGON_H

//# Includes
#include <trial/Lattices/LCRegionFixed.h>
#include <aips/Arrays/Vector.h>


// <summary>
// Define a 2-dimensional region by a polygon.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCPolygon class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to define a 2-dimensional region by means
// an ordered collection of points with straight lines connecting
// adjacent points. The last points can be equal to the first one.
// If not, an extra point gets added to get the closing line.
// <p>
// The polygon can be as complex as one likes. E.g. it is possible to
// have a rectangle with an inner rectangle to exclude interior points.
// <p>
// The points defining the polygon do not need to coincide with pixel points.
// At least one point of the polygon must be within the lattice space.
// Points may be outside the lattice meaining that only part of the
// polygon surface is actually used.
// </synopsis> 

// <example>
// <srcblock>
//    // A simple (tilted) square.
//    Vector<Float> x(4), y(4);
//    x(0)=3; y(0)=3;
//    x(1)=6; y(1)=6;
//    x(2)=3; y(2)=9;
//    x(3)=0; y(3)=6;
//    LCPolygon region(x, y, IPosition(2,128,128));
//
//    // A rectangle with an inner region to exclude interior points.
//    // Note that the last point is equal to the first point, thus
//    // the last line is given explicitly.
//    Vector<Float> x(11), y(11);
//    x(0)=3; y(0)=3;
//    x(1)=9; y(1)=3;
//    x(2)=9; y(2)=8;
//    x(3)=3; y(3)=8;
//    x(4)=3; y(4)=3;
//    x(5)=5; y(5)=5;
//    x(6)=8; y(6)=4;
//    x(7)=7; y(7)=7;
//    x(8)=5; y(8)=7;
//    x(9)=5; y(9)=5;
//    x(10)=3; y(10)=3;
//    LCPolygon region(x, y, IPosition(2,128,128));
// </srcblock>
// </example>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>

class LCPolygon: public LCRegionFixed
{
public:
    LCPolygon();

    // Construct from the given x and y values.
    // The latticeShape must define a 2-dimensional lattice.
    // <br>LCPolygon can be used for an N-dimensional lattice by making
    // another lattice representing any 2 axes from the original lattice.
    // <group>
    LCPolygon (const Vector<Float>& x, const Vector<Float>& y,
	       const IPosition& latticeShape);
    LCPolygon (const Vector<Double>& x, const Vector<Double>& y,
	       const IPosition& latticeShape);
    // </group>

    // Copy constructor (partly reference semantics).
    LCPolygon (const LCPolygon& other);

    virtual ~LCPolygon();

    // Assignment (copy semantics).
    LCPolygon& operator= (const LCPolygon& other);

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Construct another LCPolygon (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Get the X-values.
    const Vector<Float>& x() const;

    // Get the Y-values.
    const Vector<Float>& y() const;

    // Get the class name (to store in the record).
    static String className();

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCPolygon* fromRecord (const TableRecord&,
				  const String& tablename);

private:
    // Make the bounding box.
    void defineBox();

    // Define the mask to indicate which elements are inside the polygon.
    void defineMask();

    
    Vector<Float> itsX;
    Vector<Float> itsY;
};


inline const Vector<Float>& LCPolygon::x() const
{
    return itsX;
}
inline const Vector<Float>& LCPolygon::y() const
{
    return itsY;
}


#endif
