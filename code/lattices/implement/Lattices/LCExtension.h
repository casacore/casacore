//# LCExtension.h: Extend an LCRegion along straight lines to other dimensions
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

#if !defined(AIPS_LCEXTENSION_H)
#define AIPS_LCEXTENSION_H

//# Includes
#include <trial/Lattices/LCRegionMulti.h>


// <summary>
// Extend an LCRegion along straight lines to other dimensions
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCExtension class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to extend a LCRegion along straight lines to
// other dimensions. E.g. a circle in the xy-plane can be extended to
// a cylinder in the xyz-space.
// It can be used for a lattice of any dimensionality as long as the
// dimensionality of the (hyper-)extension matches the dimensionality of
// the lattice.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// <li> Extend along (slanted) cone lines
// </todo>

class LCExtension: public LCRegionMulti
{
public:
    LCExtension();

    // Extend the given region along the <src>extendAxes</src>
    // from <src>extendBlc</src> to <src>extendTrc</src> inclusive.
    // The sum of the dimensionality of the region and the length of the
    // <src>extend</src> vectors must be equal to the lattice dimensionality.
    // Blc/trc defaults to the entire lattices for the <src>extendAxes</src>.
    // <group>
    LCExtension (const LCRegion& region,
		 const IPosition& extendAxes,
		 const IPosition& latticeShape);
    LCExtension (const LCRegion& region,
		 const IPosition& extendAxes,
		 const IPosition& extendBlc,
		 const IPosition& extendTrc,
		 const IPosition& latticeShape);
    // </group>

    // Copy constructor (copy semantics).
    LCExtension (const LCExtension& other);

    virtual ~LCExtension();

    // Assignment (copy semantics).
    LCExtension& operator= (const LCExtension& other);

    // Comparison
    // <group>
    virtual Bool operator== (const LCRegion& other) const;
    virtual Bool operator!= (const LCRegion& other) const;
    // </group>
 
    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Get the original region.
    const LCRegion& region() const;
    
     // Get the new axes.
    const IPosition& axes() const;

    // Get the blc of the new axes.
    const IPosition& blc() const;

    // Get the trc of the new axes.
    const IPosition& trc() const;

    // Construct another LCRegion (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box and mask.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns the class name.
    virtual String type() const;
 
    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCExtension* fromRecord (const TableRecord&,
				    const String& tableName);

protected:
    // Do the actual getting of the mask.
    virtual void multiGetSlice (Array<Bool>& buffer, const Slicer& section);

    // This function is needed here because the niceCursorShape of the
    // contributing region does not make any sense (other dimensionality). 
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

private:
    // Construct from multiple regions given as a Block..
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    LCExtension (Bool takeOver, const PtrBlock<const LCRegion*>& regions);

    // Make the bounding box and determine the offsets..
    void defineBox();

    IPosition itsExtendAxes;
    IPosition itsRegionAxes;
    IPosition itsBlc;
    IPosition itsTrc;
};


inline const LCRegion& LCExtension::region() const
{
    return *(regions()[0]);
}
inline const IPosition& LCExtension::axes() const
{
    return itsExtendAxes;
}
inline const IPosition& LCExtension::blc() const
{
    return itsBlc;
}
inline const IPosition& LCExtension::trc() const
{
    return itsTrc;
}


#endif
