//# LCBox.h: Class to define a rectangular box of interest
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

#if !defined(AIPS_LCBOX_H)
#define AIPS_LCBOX_H

//# Includes
#include <trial/Lattices/LCRegionFixed.h>
#include <aips/Lattices/Slicer.h>


// <summary>
// Class to define a rectangular box of interest.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCBox class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to define a rectangular region of interest.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// </todo>

class LCBox: public LCRegionFixed
{
public:
    LCBox();

    // Construct from the Slicer defining the box.
    // The slicer may not contain a stride.
    LCBox (const Slicer& box, const IPosition& latticeShape);

    // Construct from the IPosition's defining the bottom-left and
    // top-right corner of the box.
    LCBox (const IPosition& blc, const IPosition& trc,
	   const IPosition& latticeShape);

    // Copy constructor (copy semantics).
    LCBox (const LCBox& other);

    virtual ~LCBox();

    // Assignment (copy semantics).
    LCBox& operator= (const LCBox& other);

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Construct another LCBox (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Get the class name (to store in the record).
    static String className();

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCBox* fromRecord (const TableRecord&,
			      const String& tablename);

private:
    // Make a box from the blc,trc such that it does not exceed the
    // lattice boundaries.
    void setSlicerBox (const IPosition& blc, const IPosition& trc);
};


#endif
