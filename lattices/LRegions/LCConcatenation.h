//# LCConcatenation.h: Combine multiple LCRegion's into a new dimension
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

#ifndef LATTICES_LCCONCATENATION_H
#define LATTICES_LCCONCATENATION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegionMulti.h>
#include <casacore/lattices/LRegions/LCBox.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Combine multiple LCRegion's into a new dimension.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCConcatenation class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to combine multiple LCRegion's and to add a
// dimension on them. The range (beginning and end) in that new
// dimension have to be specified using an
// <linkto class=LCBox>LCBox</linkto> object.
// When the LCBox is complete, it will be checked if the given number
// of regions matches the length of the given range (so it could only
// be done after the makeComplete call).
// Using a fractional box does not make much sense, because it results
// in a varying length range when used with varying shaped lattices.
// However, one can use it if wanted.
// <br>
// LCConcatenation can be seen as a mixture of the classes
// <linkto class=LCUnion>LCUnion</linkto> and
// <linkto class=LCExtension>LCExtension</linkto>. Like LCUnion it
// combines regions and like LCExtension it increases the dimensionality
// for the new region (be it with only 1).
// <br>
// E.g. One can define a different polygon in the RA-DEC plane of each
// channel. LCConcatenation makes it possible to combine the polygons
// to one 3D region in the RA-DEC-Freq cube.
// </synopsis> 

// <example>
// This example combines <src>n</src> (relative) circles 
// given in the x-z plane along the y-axis.
// In this example the regions used are circles with the same centers,
// but it is also  possible to combine differently shaped regions.
// Note that LCConcatenation takes over the pointers to the individual regions,
// so they do not need to be deleted (the LCConcatenation destructor does it).
// <srcblock>
// IPosition center (2,10,20);
// PtrBlock<LCRegion*> cirPtr(n);
// for (i=0; i<n; i++) {
//   // Each circle has a different radius.
//   cirPtr(i) = new LCEllipsoid cir1 (center, 1 + i%(n/2));
// }
// // Construct the concatenation for a range (given as a relative box).
// // Extend along the y-axis (axis numbers start counting at 0!).
// // Take over the region pointers.
// LCConcatenation region (True, cirPtr, 1, LCBox(n/2-n, n/2-1));
// </srcblock>
// </example>

//# <todo asof="1997/11/11">
//# <li> 
//# </todo>

class LCConcatenation: public LCRegionMulti
{
public:
    LCConcatenation();

    // Combine the given regions.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    // The extend range has to be given as a 1-dimensional box.
    // The default range is the entire axis.
    // <group>
    LCConcatenation (Bool takeOver, const PtrBlock<const LCRegion*>& regions,
		     Int extendAxis);
    LCConcatenation (Bool takeOver, const PtrBlock<const LCRegion*>& regions,
		     Int extendAxis, const LCBox& extendRange);
    // </group>

    // Copy constructor (copy semantics).
    LCConcatenation (const LCConcatenation& other);

    virtual ~LCConcatenation();

    // Assignment (copy semantics).
    LCConcatenation& operator= (const LCConcatenation& other);

    // Comparison
    virtual Bool operator== (const LCRegion& other) const;
 
    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

     // Get the extend axis.
    Int extendAxis() const;

    // Get the extend box.
    const LCBox& extendBox() const;

    // Get the class name (to store in the record).
    static String className();

    // Get the region type.  Returns the class name.
    virtual String type() const;
 
    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCConcatenation* fromRecord (const TableRecord&,
					const String& tableName);

protected:
    // Construct another LCRegion (for e.g. another lattice) by moving
    // this one. It recalculates the bounding box and mask.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

    // Do the actual getting of the mask.
    virtual void multiGetSlice (Array<Bool>& buffer, const Slicer& section);

    // This function is needed here because the niceCursorShape of the
    // contributing region does not make any sense (other dimensionality). 
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

private:
    // Fill the object.
    // <group>
    void fillRegionAxes();
    void fill();
    // </group>

    Int       itsExtendAxis;
    IPosition itsRegionAxes;
    LCBox     itsExtendBox;
};


inline Int LCConcatenation::extendAxis() const
{
    return itsExtendAxis;
}
inline const LCBox& LCConcatenation::extendBox() const
{
    return itsExtendBox;
}



} //# NAMESPACE CASACORE - END

#endif
