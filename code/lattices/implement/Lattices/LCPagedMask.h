//# LCPagedMask.h: Class to define a rectangular mask as a region
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

#if !defined(AIPS_LCPAGEDMASK_H)
#define AIPS_LCPAGEDMASK_H

//# Includes
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/PagedArray.h>
#include <aips/Lattices/Slicer.h>


// <summary>
// Class to define a rectangular mask as a region
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCPagedMask class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// </todo>

class LCPagedMask: public LCBox
{
public:
    LCPagedMask();

    // Construct a PagedMask object for (part of) a lattice.
    // The default mask shape is the lattice shape.
    // The slicer may not contain a stride.
    // <group>
    LCPagedMask (const TiledShape& latticeShape, const String& tableName);
    LCPagedMask (const TiledShape& maskShape, const String& tableName,
		 const IPosition& blc, const IPosition& latticeShape);
    // </group>

    // Copy constructor (copy semantics).
    LCPagedMask (const LCPagedMask& other);

    // Destructor
    virtual ~LCPagedMask();

    // Assignment (reference semantics).
    LCPagedMask& operator= (const LCPagedMask& other);

    // Comparison
    // <group>
    virtual Bool operator==(const LCRegion& other) const;
    virtual Bool operator!=(const LCRegion& other) const;
    // </group>

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Construct another LCPagedMask (for e.g. another lattice) by moving
    // this one. However, the mask values are not copied across.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Get the class name (to store in the record).
    static String className();

    // Region type. Returns class name
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCPagedMask* fromRecord (const TableRecord&,
				    const String& tablename);

    // An LCPagedMask is writable if the underlying PagedArray is.
    virtual Bool isWritable() const;

private:
    // Create the object from a record (for an existing mask).
    LCPagedMask (PagedArray<Bool>& mask,
		 const IPosition& blc,
		 const IPosition& latticeShape);


    PagedArray<Bool> itsMask;
};


#endif
