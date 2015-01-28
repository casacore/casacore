//# LCUnion.h: Make the union of 2 or more regions
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

#ifndef LATTICES_LCUNION_H
#define LATTICES_LCUNION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegionMulti.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Make the union of 2 or more regions.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCUnion class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to extend a LCRegion along straight lines to
// other dimensions. E.g. a circle in the xy-plane can be extended to
// a cylinder in the xyz-space.
// includes the union border.
// It can only be used for a lattice of any dimensionality as long as the
// dimensionality of the (hyper-)union matches the dimensionality of
// the lattice.
// <p>
// The center of the union must be inside the lattice
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// <li> Expand along (slanted) cone lines
// </todo>

class LCUnion: public LCRegionMulti
{
public:
    LCUnion();

    // Construct the union of the given regions.
    LCUnion (const LCRegion& region1, const LCRegion& region2);

    // Construct from multiple regions.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    // <group>
    LCUnion (Bool takeOver, const LCRegion* region1,
	     const LCRegion* region2 = 0,
	     const LCRegion* region3 = 0,
	     const LCRegion* region4 = 0,
	     const LCRegion* region5 = 0,
	     const LCRegion* region6 = 0,
	     const LCRegion* region7 = 0,
	     const LCRegion* region8 = 0,
	     const LCRegion* region9 = 0,
	     const LCRegion* region10 = 0);
    LCUnion (Bool takeOver, const PtrBlock<const LCRegion*>& regions);
    // </group>

    // Copy constructor (copy semantics).
    LCUnion (const LCUnion& other);

    virtual ~LCUnion();

    // Assignment (copy semantics).
    LCUnion& operator= (const LCUnion& other);

    // Comparison
    virtual Bool operator== (const LCRegion& other) const;

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns className()
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCUnion* fromRecord (const TableRecord&,
				const String& tableName);

protected:
    // Construct another LCRegion (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box and mask.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Do the actual getting of the mask.
    virtual void multiGetSlice (Array<Bool>& buffer, const Slicer& section);

private:
    // Make the bounding box and determine the offsets.
    void defineBox();
};



} //# NAMESPACE CASACORE - END

#endif
