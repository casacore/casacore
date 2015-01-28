//# tLCConcatenation.cc:  mechanical test of the LCConcatenation class
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

#include <casacore/lattices/LRegions/LCConcatenation.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCEllipsoid.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt (const IPosition& latticeShape,
	   const IPosition& start,
	   const IPosition& end,
	   const IPosition& center,
	   Float radius)
{
    uInt ndim = 1 + latticeShape.nelements();
    LCBox box (start, end, latticeShape);
    LCEllipsoid cir (center, radius, latticeShape);
    LCEllipsoid cir1 (center, radius-1, latticeShape);
    PtrBlock<const LCRegion*> ptrs(3);
    ptrs[0] = &cir;
    ptrs[1] = &box;
    ptrs[2] = &cir1;
    // Extend along the last axis
    LCConcatenation inters (False, ptrs, ndim-1);

    AlwaysAssertExit (inters.hasMask());
    AlwaysAssertExit (! inters.isWritable());
    cout << inters.hasMask() << ' ' << endl;
    cout << inters.boundingBox().start() << inters.boundingBox().end()
	 << inters.boundingBox().length() << inters.latticeShape() << endl;
    Array<Bool> mask;
    inters.getSlice (mask, IPosition(ndim,0), inters.boundingBox().length(),
		     IPosition(ndim,1));
    cout << mask << endl;
    Array<Bool> mask1;
    ptrs.resize (1, True, True);
    ptrs[0] = &box;
    LCConcatenation inters1 (False, ptrs, start.nelements()); 
    AlwaysAssertExit (inters1.hasMask());
    AlwaysAssertExit (! inters1.isWritable());
    inters1.getSlice (mask1, IPosition(ndim,0), inters1.boundingBox().length(),
		     IPosition(ndim,1));
    AlwaysAssertExit (allEQ(mask1, True));

    // Test slicing in various ways.
    // This is also a test for LCRegionMulti::findAreas.
    {
	IPosition start(ndim,0);
	IPosition end(inters.boundingBox().length() - 1);
	IPosition inc(ndim,2);
	inters.getSlice (mask1, Slicer(start, end, inc, Slicer::endIsLast));
	AlwaysAssertExit (allEQ(mask1, mask(start, end, inc)));
	IPosition start2(start+5);
	start2(ndim-1) = 0;
	inters.getSlice (mask1, Slicer(start2, end, inc, Slicer::endIsLast));
	AlwaysAssertExit (allEQ(mask1, mask(start2, end, inc)));
	IPosition end2(end-5);
	end2(ndim-1) = end(ndim-1);
	inters.getSlice (mask1, Slicer(start, end2, inc, Slicer::endIsLast));
	AlwaysAssertExit (allEQ(mask1, mask(start, end2, inc)));
	IPosition inc2(inc+10);
	inc2(ndim-1) = 1;
	inters.getSlice (mask1, Slicer(start, end, inc2, Slicer::endIsLast));
	AlwaysAssertExit (allEQ(mask1, mask(start, end, inc2)));
	inc2 = inc+8;
	inc2(ndim-1) = 1;
	inters.getSlice (mask1, Slicer(start, end, inc2, Slicer::endIsLast));
	AlwaysAssertExit (allEQ(mask1, mask(start, end, inc2)));
    }

    {
	// Test cloning.
        LCRegion* interscop = inters.cloneRegion();
	AlwaysAssertExit (inters.hasMask() == interscop->hasMask());
	AlwaysAssertExit (inters.boundingBox().start()
			  == interscop->boundingBox().start());
	AlwaysAssertExit (inters.boundingBox().end()
			  == interscop->boundingBox().end());
	AlwaysAssertExit (inters.boundingBox().stride()
			  == interscop->boundingBox().stride());
	AlwaysAssertExit (inters.boundingBox().length()
			  == interscop->boundingBox().length());
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
	AlwaysAssertExit (inters.boundingBox().start()
			  == interscop->boundingBox().start());
	AlwaysAssertExit (inters.boundingBox().end()
			  == interscop->boundingBox().end());
	AlwaysAssertExit (inters.boundingBox().stride()
			  == interscop->boundingBox().stride());
	AlwaysAssertExit (inters.boundingBox().length()
			  == interscop->boundingBox().length());
	Array<Bool> arr;
	interscop->getSlice (arr, IPosition(ndim,0),
			     inters.boundingBox().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete interscop;
    }
    {
    // Test ordered equality. 
	PtrBlock<const LCRegion*> ptrs(2);
	ptrs[0] = &box;
	ptrs[1] = &cir;
	LCConcatenation union1 (False, ptrs, start.nelements());
	LCConcatenation union2 (False, ptrs, start.nelements());
	AlwaysAssertExit (union1 == union2);
    }
    {
    // Test unordered equality. 
	PtrBlock<const LCRegion*> ptrs(2);
	ptrs[0] = &box;
	ptrs[1] = &cir;
	LCConcatenation union1 (False, ptrs, start.nelements());
	ptrs[0] = &cir;
	ptrs[1] = &box;
	LCConcatenation union2 (False, ptrs, start.nelements());
	AlwaysAssertExit (union1 == union2);
    }
}


int main()
{
    try {
	doIt (IPosition (2,11,20),
	      IPosition (2,3,4), IPosition (2,7,8),
	      IPosition (2,5,10), 5.);
	doIt (IPosition (2,10,20),
	      IPosition (2,3,4), IPosition (2,7,8),
	      IPosition (2,4,16), 5.);
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;
}
