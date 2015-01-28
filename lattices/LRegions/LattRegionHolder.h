//# LattRegionHolder.h: Class to hold a region of interest in an image
//# Copyright (C) 1999,2001
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

#ifndef LATTICES_LATTREGIONHOLDER_H
#define LATTICES_LATTREGIONHOLDER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class CoordinateSystem;
class IPosition;
class LCRegion;
class LCSlicer;
class WCRegion;
class String;
class TableRecord;


// <summary>
// Class to hold a region of interest in an image.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCSlicer>LCSlicer</linkto>
//   <li> <linkto class=WCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The only purpose of LattRegionHolder is to have a single object for
// the various kinds of regions. It can hold a
// <linkto class=LCRegion>LCRegion</linkto>, and
// <linkto class=LCSlicer>LCSlicer</linkto>.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// It was felt that making an abstract base class LatticeRegion for
// LCRegion and WCRegion would create undesirable dependencies of
// module Lattices on module Coordinates. E.g. it would be impossible
// to have a function toWCRegion.
// Therefore the container class LattRegionHolder is chosen, from which
// the container <linkto class=ImageRegion>ImageRegion</linkto> is derived.
// </motivation>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>


class LattRegionHolder
{
public:
    // Construct from a region based on lattice coordinates.
    LattRegionHolder (const LCRegion&);

    // Construct from a slicer based on lattice coordinates.
    LattRegionHolder (const LCSlicer&);

    // Similar constructors as above, but using a pointer.
    // It takes over the pointer, so the user should not delete the
    // object. It is deleted by the LattRegionHolder destructor.
    // <group>
    explicit LattRegionHolder (LCRegion*);
    explicit LattRegionHolder (LCSlicer*);
    // </group>

    // Copy constructor (copy semantics).
    LattRegionHolder (const LattRegionHolder& other);

    virtual ~LattRegionHolder();

    // Assignment (copy semantics).
    LattRegionHolder& operator= (const LattRegionHolder& other);

    // Clone the object.
    virtual LattRegionHolder* clone() const;

    // Comparison
    // <group>
    virtual Bool operator==(const LattRegionHolder& other) const;
    Bool operator!=(const LattRegionHolder& other) const;
    // </group>

    // Test if the underlying region is an LCRegion, etc.
    // <group>
    Bool isLCRegion() const;
    Bool isLCSlicer() const;
    virtual Bool isWCRegion() const;
    // </group>

    // Get the region as a pointer to a LCRegion, LCSlicer, or WCRegion.
    // An exception is thrown if the region is not the correct type.
    // Functions <src>isWCRegion()</src>, etc. can be used to test the type.
    // <group>
    const LCRegion* asLCRegionPtr() const;
    const LCSlicer* asLCSlicerPtr() const;
    virtual const WCRegion* asWCRegionPtr() const;
    // </group>

    // Get the dimensionality.
    uInt ndim() const;

    // Convert to a LatticeRegion using the given shape.
    LatticeRegion toLatticeRegion (const IPosition& shape) const;

    // Convert to a LatticeRegion using the given coordinate system
    // (with reference pixel) and shape.
    // It will also make the region complete (absolute and non-fractional).
    virtual LatticeRegion toLatticeRegion (const CoordinateSystem& cSys,
					   const IPosition& shape) const;

    // Form a compound from this and the other region.
    // <group>
    virtual LattRegionHolder* makeUnion (const LattRegionHolder& other) const;
    virtual LattRegionHolder* makeIntersection
                                        (const LattRegionHolder& other) const;
    virtual LattRegionHolder* makeDifference
                                        (const LattRegionHolder& other) const;
    virtual LattRegionHolder* makeComplement() const;
    // </group>

protected:
    // Construct for the given dimensionality (for derived classes).
    explicit LattRegionHolder (uInt ndim);

private:
    LCRegion*   itsLC;
    LCSlicer*   itsSlicer;
    uInt        itsNdim;
};


inline Bool LattRegionHolder::isLCRegion() const
{
    return  (itsLC != 0);
}
inline Bool LattRegionHolder::isLCSlicer() const
{
    return  (itsSlicer != 0);
}
inline Bool LattRegionHolder::operator!= (const LattRegionHolder& other) const
{
    return  (! operator== (other));
}
inline uInt LattRegionHolder::ndim() const
{
    return itsNdim;
}



} //# NAMESPACE CASACORE - END

#endif
