//# LCBox.h: Class to define a rectangular box of interest
//# Copyright (C) 1997,1998,1999
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

#ifndef LATTICES_LCBOX_H
#define LATTICES_LCBOX_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegionFixed.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Vector.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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

    // Construct a box for the full lattice shape.
    explicit LCBox (const IPosition& latticeShape);

    // Construct from the Slicer defining the box.
    // The slicer may not contain a stride.
    LCBox (const Slicer& box, const IPosition& latticeShape);

    // Construct from the IPosition's defining the bottom-left and
    // top-right corner of the box.
    LCBox (const IPosition& blc, const IPosition& trc,
	   const IPosition& latticeShape);

    // Construct from the Vector's defining the bottom-left and
    // top-right corner of the box.
    // <group>
    LCBox (const Vector<Float>& blc, const Vector<Float>& trc,
	   const IPosition& latticeShape);
    LCBox (const Vector<Double>& blc, const Vector<Double>& trc,
	   const IPosition& latticeShape);
    // </group>

    // Copy constructor (reference semantics).
    LCBox (const LCBox& other);

    virtual ~LCBox();

    // Assignment (copy semantics).
    LCBox& operator= (const LCBox& other);

    // Comparison.  Mask not checked. Use function 
    // LRegionSingle::maskEqual  to do this
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
    static LCBox* fromRecord (const TableRecord&,
			      const String& tablename);

    // Get the box blc
    Vector<Float> blc() const;

    // Get the box trc
    Vector<Float> trc() const;

// Verify a box specification.  Illegal (inlcuding blc > trc) or
// unspecified values are  given 0 (blc) shape (trc) or
// unity (inc).  Returns <src>True</src> if any of the blc/trc/inc 
// are changed from their input values, else returns <src>False</src>
   static Bool verify (IPosition& blc, IPosition& trc,
                       IPosition& inc, const IPosition& shape);


protected:
    // Construct another LCBox (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

private:
    // Make a box from the blc,trc such that it does not exceed the
    // lattice boundaries.
    void setSlicerBox (const IPosition& blc, const IPosition& trc);

    // Fill the blc and trc vector from IPositions.
    void fillBlcTrc();


    //# Variables
    Vector<Float> itsBlc;
    Vector<Float> itsTrc;
};


inline Vector<Float> LCBox::blc() const
{
    return itsBlc;
}
inline Vector<Float> LCBox::trc() const
{
    return itsTrc;
}



} //# NAMESPACE CASACORE - END

#endif
