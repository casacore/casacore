//# LCRegionMulti.h: Make the intersection of 2 or more regions
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

#ifndef LATTICES_LCREGIONMULTI_H
#define LATTICES_LCREGIONMULTI_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Make the intersection of 2 or more regions.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCRegionMulti class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// It makes it possible to extend a LCRegion along straight lines to
// other dimensions. E.g. a circle in the xy-plane can be extended to
// a cylinder in the xyz-space.
// includes the intersection border.
// It can only be used for a lattice of any dimensionality as long as the
// dimensionality of the (hyper-)intersection matches the dimensionality of
// the lattice.
// <p>
// The center of the intersection must be inside the lattice
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// <li>
// </todo>

class LCRegionMulti: public LCRegion
{
public:
    LCRegionMulti();

    // Construct from 2 regions.
    LCRegionMulti (const LCRegion& region1, const LCRegion& region2);

    // Construct from multiple regions.
    LCRegionMulti (Bool takeOver, const LCRegion* region1,
		   const LCRegion* region2 = 0,
		   const LCRegion* region3 = 0,
		   const LCRegion* region4 = 0,
		   const LCRegion* region5 = 0,
		   const LCRegion* region6 = 0,
		   const LCRegion* region7 = 0,
		   const LCRegion* region8 = 0,
		   const LCRegion* region9 = 0,
		   const LCRegion* region10 = 0);

    // Construct from multiple regions given as a Block.
    // When <src>takeOver</src> is True, the destructor will delete the
    // given regions. Otherwise a copy of the regions is made.
    LCRegionMulti (Bool takeOver, const PtrBlock<const LCRegion*>& regions);

    // Copy constructor (copy semantics).
    LCRegionMulti (const LCRegionMulti& other);

    virtual ~LCRegionMulti();

    // Assignment (copy semantics).
    LCRegionMulti& operator= (const LCRegionMulti& other);

    // Comparison 
    virtual Bool operator== (const LCRegion& other) const;

    // Does the region have a mask?
    virtual Bool hasMask() const;

protected:
    // Store the contributing regions in a record.
    TableRecord makeRecord (const String& tableName) const;

    // Retrieve the contributing objects from the record.
    static void unmakeRecord (PtrBlock<const LCRegion*>&,
			      const TableRecord&,
			      const String& tableName);

    // Translate all regions.
    void multiTranslate (PtrBlock<const LCRegion*>&,
			 const Vector<Float>& translateVector,
			 const IPosition& newLatticeShape) const;

    // Determine if all regions have mask (used by LCIntersection).
    void fillHasMask();

    // Find which area of the section and region are needed.
    // False is returned if no part of the region is included in the section.
    Bool findAreas (IPosition& bufStart, IPosition& bufEnd,
		    IPosition& regStart, IPosition& regEnd,
		    const Slicer& section, uInt regNr) const;

    // Get the contributing regions.
    const PtrBlock<const LCRegion*>& regions() const;
    
protected:
    // Construct from lattice shape and region pointer, which is
    // taken over.
    // Primarily meant for LCExtension.
    LCRegionMulti (const LCRegion* region, const IPosition& latticeShape);

    // Do the actual getting of an array of values.
    virtual Bool doGetSlice (Array<Bool>& buffer, const Slicer& section);

    // Get the values from the class derived from Multi.
    // It is called when there is a mask. Note that it is not sure
    // whether the buffer has the correct size.
    virtual void multiGetSlice (Array<Bool>& buffer,
				const Slicer& section) = 0;

    // Get the best cursor shape.
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

private:
    // Check if the regions are correct.
    // If needed, make a copy of the region objects.
    void init (Bool takeOver);

    //# >=0 means this region has a mask.
    //# Its value gives the region with the biggest mask.
    Int itsHasMask;
    PtrBlock<const LCRegion*> itsRegions;
};


inline const PtrBlock<const LCRegion*>& LCRegionMulti::regions() const
{
    return itsRegions;
}



} //# NAMESPACE CASACORE - END

#endif
