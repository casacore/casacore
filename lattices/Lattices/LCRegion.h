//# LCRegion.h: Abstract base class to define a region of interest in lattice coordinates
//# Copyright (C) 1998,1999,2000,2001
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

#ifndef LATTICES_LCREGION_H
#define LATTICES_LCREGION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TableRecord;
class RecordInterface;


// <summary>
// Abstract base class to define a region of interest in lattice coordinates.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Slicer>Slicer</linkto>
// </prerequisite>

// <synopsis> 
// The LCRegion class is the abstract base class for various types
// of LCRegion's (e.g. <linkto class=LCEllipsoid>LCEllipsoid</linkto>,
// <linkto class=LCBox>LCBox</linkto>).
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

class LCRegion : public Lattice<Bool>
{
public:
    LCRegion();

    // Construct with the lattice shape only.
    LCRegion (const IPosition& latticeShape);

    // Copy constructor (copy semantics).
    LCRegion (const LCRegion& other);

    virtual ~LCRegion();

    // Equality 
    virtual Bool operator== (const LCRegion& other) const;

    // Non-equality.  Be careful, do not use this anywhere in the derived
    // class structure.  You must use, e.g.,
    // <src>if (! LCRegion::operator== (...))</src>
    // rather than <src>if (LCRegion::operator!= (...))</src> as the
    // latter will invoke an infinite loop.  It is ok to use when applying
    // to a concrete class object.
    Bool operator!= (const LCRegion& other) const;

    // Make a copy of the derived object.
    // <group>
    virtual Lattice<Bool>* clone() const;
    virtual LCRegion* cloneRegion() const = 0;
    // </group>

    // Handle deletion of the region by deleting possible tables.
    // The default implementation does nothing.
    virtual void handleDelete();

    // Handle renaming the region by renaming possible tables.
    // The default implementation does nothing.
    virtual void handleRename (const String& newName, Bool overwrite);

    // Region type.  Returns className() of derived class.
    virtual String type() const = 0;

    // Get or set the comment.
    // <group>
    const String& comment() const;
    void setComment (const String& comment);
    // </group>

    // Does the region have a mask?
    virtual Bool hasMask() const = 0;

    // Construct another LCRegion (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box and mask.
    // A positive translation value indicates "to right".
    // <group>
    LCRegion* translate (const IPosition& translateVector) const;
    LCRegion* translate (const IPosition& translateVector,
			 const IPosition& newLatticeShape) const;
    LCRegion* translate (const Vector<Float>& translateVector) const;
    LCRegion* translate (const Vector<Float>& translateVector,
			 const IPosition& newLatticeShape) const;
    // </group>

    // Give the full lattice shape.
    const IPosition& latticeShape() const;

    // Give the bounding box.
    const Slicer& boundingBox() const;

    // Expand a slicer or position in the region to the full lattice.
    // This converts the positions in the region to positions
    // in the entire lattice.
    // <group>
    Slicer expand (const Slicer& slicer) const;
    IPosition expand (const IPosition& index) const;
    // </group>

    // Convert the (derived) object to a record.
    // The record can be used to make the object persistent.
    // The <src>tableName</src> argument can be used by derived
    // classes (e.g. LCPagedMask) to put very large objects.
    virtual TableRecord toRecord (const String& tableName) const = 0;

    // Convert correct object from a record.
    static LCRegion* fromRecord (const TableRecord&,
				 const String& tableName);

    // Return the dimensionality of the region.
    virtual uInt ndim() const;

    // Return the shape of the region (i.e. of its bounding box).
    virtual IPosition shape() const;

    // Usually the lattice (i.e. the region mask) is not writable.
    virtual Bool isWritable() const;

    // Regions can usually not be put; i.e. no putSlice, etc. can be
    // done on their masks.
    // Hence LCRegion throws by default an exception for the
    // following functions.
    // <group>
    virtual void doPutSlice (const Array<Bool>& sourceBuffer,
			     const IPosition& where,
			     const IPosition& stride);
    virtual void set (const Bool& value);
    virtual void apply (Bool (*function)(Bool));
    virtual void apply (Bool (*function)(const Bool&));
    virtual void apply (const Functional<Bool,Bool>& function);
    virtual void putAt (const Bool& value, const IPosition& where);
    virtual void copyData (const Lattice<Bool>& from);
    // </group>

protected:
    // Assignment (copy semantics) is only useful for derived classes.
    LCRegion& operator= (const LCRegion& other);

    // Sometimes it is inconvenient for a derived class to set the bounding
    // box in the constructor. So it can be set explicitly.
    // It fills in the possibly undefined Slicer values.
    // It may even be needed to set the lattice shape.
    // <group>
    void setBoundingBox (const Slicer& boundingBox);
    void setShapeAndBoundingBox (const IPosition& latticeShape,
				 const Slicer& boundingBox);
    // </group>

    // Do the actual translate in a derived class.
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const = 0;

    // Define the type and class name in the record.
    void defineRecordFields (RecordInterface& record,
			     const String& className) const;

private:
    IPosition itsShape;
    Slicer    itsBoundingBox;
    String    itsComment;
};


inline const Slicer& LCRegion::boundingBox() const
{
    return itsBoundingBox;
}
inline const IPosition& LCRegion::latticeShape() const
{
    return itsShape;
}
inline LCRegion* LCRegion::translate (const IPosition& translateVector) const
{
    return translate (translateVector, itsShape);
}
inline LCRegion* LCRegion::translate (const Vector<Float>& translateVector)
                                                                        const
{
    return translate (translateVector, itsShape);
}
inline const String& LCRegion::comment() const
{
    return itsComment;
}
inline void LCRegion::setComment (const String& comment)
{
    itsComment = comment;
}

inline Bool LCRegion::operator!= (const LCRegion& other) const
//
// Watch out !  You must not, in the derived class structure,
// invoke LCRegion::operator!=  If you do,  you will be stuck 
// in a time warp, as this will just fetch the 
// operator== of the concrete class and you start all over again.
// You must use always use !LCRegion::operator==.  It is ok in application
// code using the concrete class to say
// if (x != y) where x and y are, say, LCBoxes.
{
   return (!operator==(other));
}




} //# NAMESPACE CASACORE - END

#endif
