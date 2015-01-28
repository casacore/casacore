//# LELRegion.h:  Class to hold a region as a LEL node
//# Copyright (C) 1999
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

#ifndef LATTICES_LELREGION_H
#define LATTICES_LELREGION_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELInterface.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LattRegionHolder;


// <summary>
// Class to hold a region as a LEL node
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice</linkto>
//   <li> <linkto class="LatticeExpr"> LatticeExpr</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
// </prerequisite>

// <etymology>
// This derived LEL letter class handles regions.
// </etymology>

// <synopsis>
// This LEL letter class is derived from LELInterface.  It
// is used to construct LEL objects from regions.
// The internal region is an <linkto class=ImageRegion>ImageRegion</linkto>
// object, thus the region can be of any type.
// With operator [] a region is applied to an image (expression).
// At that stage possible world coordinates are converted to lattice
// coordinates.
// <p>
// The attributes of a LELRegion object define an empty shape,
// because in general the shape of a region is only known after
// it is applied to an image.
// <p>
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 

// <motivation>
// We needed to be able to handle regions in a LEL expression.
// </motivation>

//# <todo asof="1998/01/21">
//# </todo>


class LELRegion : public LELInterface<Bool>
{
public: 
// Constructor.
    LELRegion (const LattRegionHolder& region);

// Constructor. It takes over the pointer.
    LELRegion (LattRegionHolder* region);

// Destructor.
    ~LELRegion();

// Get a pointer to the region object.
    const LattRegionHolder& region() const
	{ return *region_p; }

// Getting region data cannot be done (throws an exception).
    virtual void eval(LELArray<Bool>&, const Slicer&) const;

// Getting region data cannot be done (throws an exception).
    virtual LELScalar<Bool> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
    virtual Bool prepareScalarExpr();

// Get class name
    virtual String className() const;

// Form a compound from the regions.
// <group>
    static LELRegion* makeUnion (const LELInterface<Bool>& left,
				 const LELInterface<Bool>& right);
    static LELRegion* makeIntersection (const LELInterface<Bool>& left,
					const LELInterface<Bool>& right);
    static LELRegion* makeDifference (const LELInterface<Bool>& left,
				      const LELInterface<Bool>& right);
    static LELRegion* makeComplement (const LELInterface<Bool>& expr);
// </group>

private:
// Get the LattRegionHolder after checking that the expression is a region.
    static const LattRegionHolder& region (const LELInterface<Bool>& expr);

// Check if both regions have the same type (pixel or world) and if
// no LCSlicer type of region is used.
    static void checkTypes (const LattRegionHolder& left,
			    const LattRegionHolder& right);

// Member variables.
    LattRegionHolder* region_p;
};




// <summary>
// Class to convert a region to a boolean node
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="LELRegion"> Lattice</linkto>
//   <li> <linkto class="LatticeExprNode"> LatticeExprNode</linkto>
//   <li> <linkto class="LELInterface"> LELInterface</linkto>
// </prerequisite>

// <etymology>
// This derived LEL letter class handles a region as a boolean lattice.
// </etymology>

// <synopsis>
// This class makes it possible to handle a region as a true
// boolean lattice without the need to apply the region to an image.
// It means that it is only possible if the region has absolute
// lattice coordinates.
// <p>
// A description of the implementation details of the LEL classes can
// be found in
// <a href="../notes/216.html">Note 216</a>
// </synopsis> 

// <motivation>
// It is useful to be able to handle a mask as a boolean lattice.
// </motivation>

//# <todo asof="1998/01/21">
//# </todo>


class LELRegionAsBool : public LELInterface<Bool>
{
public: 
// Constructor.
    LELRegionAsBool (const LELRegion& region);

// Destructor.
    ~LELRegionAsBool();

// Get region data.
    virtual void eval(LELArray<Bool>& result,
		      const Slicer& section) const;

// Getting region data as a scalar cannot be done (throws an exception).
    virtual LELScalar<Bool> getScalar() const;

// Do further preparations (e.g. optimization) on the expression.
    virtual Bool prepareScalarExpr();

// Get class name
    virtual String className() const;

private:
// Member variables.
    LatticeRegion region_p;
};




} //# NAMESPACE CASACORE - END

#endif
