//# LCRegionFixed.h: Abstract base class to define a fixed region
//# Copyright (C) 1998,2000
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

#ifndef LATTICES_LCREGIONFIXED_H
#define LATTICES_LCREGIONFIXED_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegionSingle.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Abstract base class to define a fixed region.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Slicer>Slicer</linkto>
// </prerequisite>

// <synopsis> 
// The LCRegion class is the abstract base class for various types
// of LCRegion's (e.g. LCRegionEllipsoid, LCRegionBox).
// It contains the minimal bounding box of the region and, if needed,
// a mask with the same shape as the bounding box. A mask element
// is true if the element is inside the box.
// <p>
// Each LCRegion object must be able to convert itself to and from a record.
// In that way they can be made persistent (in for example a Table).
// <p>
// The LCRegion can be used in several Lattices and Images classes and
// functions to limit the area to operate on.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// The Slicer class is too limited as a region, because it can only
// describe a rectangular region. Specialized classes are needed to
// describe arbitrary regions. They need a base class to combine them.
// </motivation>

//# <todo asof="1997/11/11">
//# <li>
//# </todo>

class LCRegionFixed : public LCRegionSingle
{
public:
    LCRegionFixed();

    // Construct with the lattice shape only.
    LCRegionFixed (const IPosition& latticeShape);

    // Copy constructor (copy semantics).
    LCRegionFixed (const LCRegionFixed& other);

    // Destructor
    virtual ~LCRegionFixed();

    // Comparison. Mask is not checked. Use the 
    // LCRegionSingle::masksEqual function as well if 
    // you want to check the masks
    virtual Bool operator== (const LCRegion& other) const;

 protected:
    // Assignment (copy semantics) is only useful for derived classes.
    LCRegionFixed& operator= (const LCRegionFixed& other);

    // Set the mask.
    void setMask (const Array<Bool>& mask);

private:
    ArrayLattice<Bool> itsMask;
};



} //# NAMESPACE CASACORE - END

#endif
