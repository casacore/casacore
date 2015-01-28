//# LCPixelSet.h: Class to define a rectangular set of pixels as a region
//# Copyright (C) 1998,1999
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

#ifndef LATTICES_LCPIXELSET_H
#define LATTICES_LCPIXELSET_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Slicer.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// The LCPixelSet class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to define a rectangular region of interest.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// </todo>

class LCPixelSet: public LCRegionFixed
{
public:
    LCPixelSet();

    // Construct from the box defining the position of the mask.
    // The shape of the region and mask must be the same.
    LCPixelSet (const Array<Bool>& mask, const LCBox& region);

    // Copy constructor (copy semantics).
    LCPixelSet (const LCPixelSet& other);

    virtual ~LCPixelSet();

    // Assignment (copy semantics).
    LCPixelSet& operator= (const LCPixelSet& other);

    // Comparison
    virtual Bool operator== (const LCRegion& other) const;
 
    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns className().
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCPixelSet* fromRecord (const TableRecord&,
				   const String& tablename);

protected:
    // Construct another LCPixelSet (for e.g. another lattice) by moving
    // this one. It recalculates the bounding mask.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

private:
    LCBox itsBox;
};



} //# NAMESPACE CASACORE - END

#endif
