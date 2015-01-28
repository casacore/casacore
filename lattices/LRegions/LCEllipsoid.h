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

#ifndef LATTICES_LCELLIPSOID_H
#define LATTICES_LCELLIPSOID_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegionFixed.h>
#include <casacore/casa/Arrays/Vector.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// to the lattice axes except in the case of a 2-D ellipse for which a
// constructor is provided for specifying the angle between the x-axis
// and major axis of the ellipse.
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
// <li> Arguments to have ellipsoid axes not parallel to lattice axes for
// dimensions greater than 2. This is a nontrivial problem because of the
// complexity of the rotation matrices involved.
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

    // Construct a two dimensional ellipse with theta being the angle from
    // the x-axis to the major axis of the ellipse in radians.
    LCEllipsoid (
    	const Float xcenter, const Float ycenter,
    	const Float majorAxis, const Float minorAxis,
    	const Float theta, const IPosition& latticeShape
    );

    // Copy constructor (reference semantics).
    LCEllipsoid (const LCEllipsoid& other);

    virtual ~LCEllipsoid();

    // Assignment (copy semantics).
    LCEllipsoid& operator= (const LCEllipsoid& other);

    // Comparison
    virtual Bool operator== (const LCRegion& other) const;

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Get the center.
    const Vector<Float>& center() const;

    // Get the radii.
    const Vector<Float>& radii() const;

    // Get the angle of the major axis of the ellipse relative to the x-axis
    // 2-D only, throws exception if ellipse is not 2-D.
    const Float& theta() const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns className()
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCEllipsoid* fromRecord (const TableRecord&,
				    const String& tableName);

protected:
    // Construct another LCBox (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

private:
    // Fill the itsCenter vector from an IPosition.
    void fillCenter (const IPosition& center);

    // Make the bounding box from center, radii, and shape.
    Slicer makeBox (const Vector<Float>& center,
			   const Vector<Float>& radii,
			   const IPosition& latticeShape);

    Slicer _makeBox2D (
    	const Vector<Float>& center,
    	const Vector<Float>& radii,
    	const IPosition& latticeShape
    );

    // Define the mask to indicate which elements are inside the ellipsoid.
    void defineMask();

    //for 2-D ellipse with non-zero theta
    void _defineMask2D();


    Vector<Float> itsCenter;
    Vector<Float> itsRadii;
    // small offset to guard against roundoff error
    Vector<Float> _epsilon;
    // for 2-D case only
    Float _theta;
};


inline const Vector<Float>& LCEllipsoid::center() const
{
    return itsCenter;
}
inline const Vector<Float>& LCEllipsoid::radii() const
{
    return itsRadii;
}



} //# NAMESPACE CASACORE - END

#endif
