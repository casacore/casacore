//# LELRegion.cc:  Class to hold a region as a LEL node
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


#include <casacore/lattices/LEL/LELRegion.h>
#include <casacore/lattices/LRegions/LattRegionHolder.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

LELRegion::LELRegion (const LattRegionHolder& region)
: region_p (region.clone())
{
   setAttr (LELAttribute(region.ndim()));
}

LELRegion::LELRegion (LattRegionHolder* region)
: region_p (region)
{
   setAttr (LELAttribute(region->ndim()));
}

LELRegion::~LELRegion()
{
    delete region_p;
}

void LELRegion::eval(LELArray<Bool>&, 
		     const Slicer&) const
{
    throw (AipsError ("LELRegion:eval - cannot be used"));
}

LELScalar<Bool> LELRegion::getScalar() const
{
   throw (AipsError ("LELRegion::getScalar - cannot be used"));
   return False;
}

Bool LELRegion::prepareScalarExpr()
{
    return False;
}

String LELRegion::className() const
{
    return "LELRegion";
}


LELRegion* LELRegion::makeUnion (const LELInterface<Bool>& left,
				 const LELInterface<Bool>& right)
{
    const LattRegionHolder& r1 = LELRegion::region (left);
    const LattRegionHolder& r2 = LELRegion::region (right);
    // Assure that both types are the same and that no LCSlicer is used.
    checkTypes (r1, r2);
    // Return the union of both regions.
    return new LELRegion (r1.makeUnion (r2));
}

LELRegion* LELRegion::makeIntersection (const LELInterface<Bool>& left,
					const LELInterface<Bool>& right)
{
    const LattRegionHolder& r1 = LELRegion::region (left);
    const LattRegionHolder& r2 = LELRegion::region (right);
    // Assure that both types are the same and that no LCSlicer is used.
    checkTypes (r1, r2);
    // Return the intersection of both regions.
    return new LELRegion (r1.makeIntersection (r2));
}

LELRegion* LELRegion::makeDifference (const LELInterface<Bool>& left,
				      const LELInterface<Bool>& right)
{
    const LattRegionHolder& r1 = LELRegion::region (left);
    const LattRegionHolder& r2 = LELRegion::region (right);
    // Assure that both types are the same and that no LCSlicer is used.
    checkTypes (r1, r2);
    // Return the difference of both regions.
    return new LELRegion (r1.makeDifference (r2));
}

LELRegion* LELRegion::makeComplement (const LELInterface<Bool>& expr)
{
    const LattRegionHolder& r1 = LELRegion::region (expr);
    // Assure that both types are the same and that no LCSlicer is used.
    checkTypes (r1, r1);
    // Return the complement of the region.
    return new LELRegion (r1.makeComplement());
}


const LattRegionHolder& LELRegion::region (const LELInterface<Bool>& expr)
{
    AlwaysAssert (expr.className() == "LELRegion", AipsError);
    return ((const LELRegion&)expr).region();
}

void LELRegion::checkTypes (const LattRegionHolder& left,
			    const LattRegionHolder& right)
{
    if (left.isLCRegion()  &&  right.isLCRegion()) {
	return;
    }
    if (left.isWCRegion()  &&  right.isWCRegion()) {
	return;
    }
    if (left.isLCSlicer()  ||  right.isLCSlicer()) {
	throw (AipsError ("LELRegion::checkTypes - "
			  "a compound of Slicer objects is not possible"));
    }
    throw (AipsError ("LELRegion::checkTypes - "
		      "in a compound both regions must have the same "
		      "type of coordinates (pixel or world)"));
}




LELRegionAsBool::LELRegionAsBool (const LELRegion& region)
{
   const LattRegionHolder& reg = region.region();
   if (! reg.isLCRegion()) {
      throw (AipsError ("LELRegionAsBool cannot handle "
			"a region in world coordinates"));
   }
   region_p = LatticeRegion (*(reg.asLCRegionPtr()));
   setAttr(LELAttribute(False, 
			region_p.shape(), region_p.niceCursorShape(),
			region_p.lelCoordinates()));
}

LELRegionAsBool::~LELRegionAsBool()
{}

void LELRegionAsBool::eval(LELArray<Bool>& result, 
			   const Slicer& section) const
{
   Array<Bool> tmp = region_p.getSlice (section);
   result.value().reference(tmp);
}

LELScalar<Bool> LELRegionAsBool::getScalar() const
{
   throw (AipsError ("LELRegionAsBool::getScalar - cannot be used"));
   return False;
}

Bool LELRegionAsBool::prepareScalarExpr()
{
    return False;
}

String LELRegionAsBool::className() const
{
    return "LELRegionAsBool";
}

} //# NAMESPACE CASACORE - END

