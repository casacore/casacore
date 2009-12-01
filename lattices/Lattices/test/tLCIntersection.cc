//# tLCIntersection.cc:  mechanical test of the LCIntersection class
//# Copyright (C) 1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <lattices/Lattices/LCIntersection.h>
#include <lattices/Lattices/LCBox.h>
#include <lattices/Lattices/LCEllipsoid.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/IPosition.h>
#include <tables/Tables/TableRecord.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>


#include <casa/namespace.h>
void doIt (const IPosition& latticeShape,
	   const IPosition& start,
	   const IPosition& end,
	   const IPosition& center,
	   Float radius)
{
    uInt ndim = latticeShape.nelements();
    LCBox box (start, end, latticeShape);
    LCEllipsoid cir (center, radius, latticeShape);
    LCIntersection inters (box, cir);
    AlwaysAssertExit (inters.hasMask());
    AlwaysAssertExit (! inters.isWritable());
    cout << inters.hasMask() << ' ' << endl;
    cout << inters.boundingBox().start() << inters.boundingBox().end()
	 << inters.boundingBox().length() << inters.latticeShape() << endl;
    Array<Bool> mask;
    inters.getSlice (mask, IPosition(ndim,0), inters.boundingBox().length(),
		     IPosition(ndim,1));
    cout << mask << endl;

    LCIntersection inters1 (False, &box); 
    AlwaysAssertExit (! inters1.hasMask());
    AlwaysAssertExit (! inters1.isWritable());
    Array<Bool> mask1;
    inters1.getSlice (mask1, IPosition(ndim,0), inters1.boundingBox().length(),
		     IPosition(ndim,1));
    AlwaysAssertExit (allEQ(mask1, True));
   
    {
	// Test cloning.
        LCRegion* interscop = inters.cloneRegion();
	AlwaysAssertExit (inters.hasMask() == interscop->hasMask());
	AlwaysAssertExit (inters.boundingBox().start() ==
			  interscop->boundingBox().start());
	AlwaysAssertExit (inters.boundingBox().end() ==
			  interscop->boundingBox().end());
	AlwaysAssertExit (inters.boundingBox().stride() ==
			  interscop->boundingBox().stride());
	AlwaysAssertExit (inters.boundingBox().length() ==
			  interscop->boundingBox().length());
	Array<Bool> arr;
	interscop->getSlice (arr, IPosition(ndim,0),
			     inters.boundingBox().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete interscop;
    }
    {
	// Test persistency.
	LCRegion* interscop = (LCRegion::fromRecord (inters.toRecord(""), ""));
	AlwaysAssertExit (inters.hasMask() == interscop->hasMask());
	AlwaysAssertExit (inters.boundingBox().start() ==
			  interscop->boundingBox().start());
	AlwaysAssertExit (inters.boundingBox().end() ==
			  interscop->boundingBox().end());
	AlwaysAssertExit (inters.boundingBox().stride() ==
			  interscop->boundingBox().stride());
	AlwaysAssertExit (inters.boundingBox().length() ==
			  interscop->boundingBox().length());
	Array<Bool> arr;
	interscop->getSlice (arr, IPosition(ndim,0),
			     inters.boundingBox().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete interscop;
    }
    {
    // Test ordered equality.
       LCIntersection inters1(box, cir);
       LCIntersection inters2(inters1);
       AlwaysAssertExit (inters1 == inters2);
    }
    {
    // Test unordered equality.
       LCIntersection inters1(box, cir);
       LCIntersection inters2(cir, box);
       AlwaysAssertExit (inters1 == inters2);
    }   
}


int main()
{
    try {
	doIt (IPosition (2,11,20),
	      IPosition (2,3,4), IPosition (2,7,8),
	      IPosition (2,5,10), 5.);
	try {
	    doIt (IPosition (2,10,20),
		  IPosition (2,3,4), IPosition (2,7,8),
		  IPosition (2,4,16), 5.);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
