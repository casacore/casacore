//# LCMask.h: Class to define a rectangular mask as a region
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

#if !defined(AIPS_LCMASK_H)
#define AIPS_LCMASK_H

//# Includes
#include <trial/Lattices/LCBox.h>
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
// The LCMask class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to define a rectangular region of interest.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// </todo>

class LCMask: public LCBox
{
public:
    LCMask();

    // Construct from the Slicer defining the mask.
    // The slicer may not contain a stride.
    LCMask (const IPosition& blc, const Array<Bool>& mask,
	    const IPosition& latticeShape);

    // Copy constructor (copy semantics).
    LCMask (const LCMask& other);

    virtual ~LCMask();

    // Assignment (copy semantics).
    LCMask& operator= (const LCMask& other);

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Construct another LCMask (for e.g. another lattice) by moving
    // this one. It recalculates the bounding mask.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Get the class name (to store in the record).
    static String className();

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCMask* fromRecord (const TableRecord&,
			       const String& tablename);

    // An LCMask can be written.
    virtual Bool isWritable() const;
};


#endif
