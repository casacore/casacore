//# LCRegionSingle.h: Abstract base class to define a single region
//# Copyright (C) 1998,1999,2003
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

#ifndef LATTICES_LCREGIONSINGLE_H
#define LATTICES_LCREGIONSINGLE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegion.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Abstract base class to define a single region.
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
// Each LCRegion object must be able to convert itself to and from a Record.
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

class LCRegionSingle : public LCRegion
{
public:
    LCRegionSingle();

    // Construct with the lattice shape only.
    LCRegionSingle (const IPosition& latticeShape);

    // Copy constructor (copy semantics).
    LCRegionSingle (const LCRegionSingle& other);

    virtual ~LCRegionSingle();

    // Does the region have a mask?
    virtual Bool hasMask() const;

    // Get the mask (as an array).
    const Array<Bool> maskArray() const;

    // Is the mask of this region the same as the mask of the other
    Bool masksEqual (const LCRegion& other) const;

    // The following "put" functions are described in detail in class
    // <linkto class=Lattice>Lattice</linkto>.
    // They'll throw an exception is no mask is available or if
    // the mask is not writable.
    // <group>
    virtual void set (const Bool& value);
    virtual void apply (Bool (*function)(Bool));
    virtual void apply (Bool (*function)(const Bool&));
    virtual void apply (const Functional<Bool,Bool>& function);
    virtual void putAt (const Bool& value, const IPosition& where);
    virtual void copyData (const Lattice<Bool>& from);
    // </group>

protected:
    // Assignment (copy semantics) is only useful for derived classes.
    LCRegionSingle& operator= (const LCRegionSingle& other);

    // Set the pointer to the mask in the derived class.
    void setMaskPtr (Lattice<Bool>& mask);
    
    // Do the actual getting of the mask.
    virtual Bool doGetSlice (Array<Bool>& buffer, const Slicer& section);

    // Do the actual putting of the mask. Only possible if region is writable.
    virtual void doPutSlice (const Array<Bool>& sourceBuffer,
			     const IPosition& where,
			     const IPosition& stride);

    // Get the best cursor shape.
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

    // Make an iterator.
    // When the underlying region has a mask, an iterator for that region
    // is returned. Otherwise the standard iterator is returned.
    virtual LatticeIterInterface<Bool>* makeIter
				(const LatticeNavigator& navigator,
				 Bool useRef) const;

private:
    Bool           itsHasMask;
    Lattice<Bool>* itsMaskPtr;
};



} //# NAMESPACE CASACORE - END

#endif
