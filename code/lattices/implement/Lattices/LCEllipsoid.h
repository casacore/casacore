//# LCEllipsoid.h: Define an N-dimensional ellipsoidal region of interest
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

#if !defined(AIPS_LCELLIPSOID_H)
#define AIPS_LCELLIPSOID_H

//# Includes
#include <trial/Lattices/LCRegionFixed.h>
#include <aips/Arrays/Vector.h>


// <summary>
// Define an N-dimensional ellipsoidal region of interest.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCEllipsoid class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to define an N-dimensional ellipsoidal region
// of interest, which includes the border. A separate constructor exists
// to define the special case of an N-dimensional sphere.
// <br>
// The center and the radii of the ellipsoid do not need to be pixel aligned.
// The center of the ellipsoid must be inside the lattice.
// The current implementation only supports ellipsoids with axes parallel
// to the lattice axes.
// <p>
// It can only be used for a lattice of any dimensionality as long as the
// dimensionality of the (hyper-)ellipsoid matches the dimensionality of
// the lattice.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// <li> Arguments to have ellipsoid axes not parallel to lattice axes.
// </todo>


class LCEllipsoid: public LCRegionFixed
{
public:
    LCEllipsoid();

    // Construct an N-dimensional sphere with the given center and
    // radius (in pixels). The center is pixel-aligned.
    LCEllipsoid (const IPosition& center, Float radius,
		 const IPosition& latticeShape);

    // Construct an N-dimensional sphere with the given center and
    // radius (in pixels). The center does not need to be pixel-aligned.
    // <group>
    LCEllipsoid (const Vector<Float>& center, Float radius,
		 const IPosition& latticeShape);
    LCEllipsoid (const Vector<Double>& center, Double radius,
		 const IPosition& latticeShape);
    // </group>

    // Construct an N-dimensional ellipsoid with the given center and
    // radii (in pixels). The center does not need to be pixel-aligned.
    // (the radii are half the length of the axes of the ellipsoid).
    // <group>
    LCEllipsoid (const Vector<Float>& center, const Vector<Float>& radii,
		 const IPosition& latticeShape);
    LCEllipsoid (const Vector<Double>& center, const Vector<Double>& radii,
		 const IPosition& latticeShape);
    // </group>

    // Copy constructor (reference semantics).
    LCEllipsoid (const LCEllipsoid& other);

    virtual ~LCEllipsoid();

    // Assignment (copy semantics).
    LCEllipsoid& operator= (const LCEllipsoid& other);

    // Comparison
    virtual Bool operator== (const LCRegion& other) const;

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Construct another LCBox (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Get the center.
    const Vector<Float>& center() const;

    // Get the radii.
    const Vector<Float>& radii() const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns className()
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCEllipsoid* fromRecord (const TableRecord&,
				    const String& tableName);

private:
    // Fill the itsCenter vector from an IPosition.
    void fillCenter (const IPosition& center);

    // Make the bounding box from center, radii, and shape.
    static Slicer makeBox (const Vector<Float>& center,
			   const Vector<Float>& radii,
			   const IPosition& latticeShape);

    // Define the mask to indicate which elements are inside the ellipsoid.
    void defineMask();


    Vector<Float> itsCenter;
    Vector<Float> itsRadii;
};


inline const Vector<Float>& LCEllipsoid::center() const
{
    return itsCenter;
}
inline const Vector<Float>& LCEllipsoid::radii() const
{
    return itsRadii;
}


#endif
