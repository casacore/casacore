//# tLCDifference.cc:  mechanical test of the LCDifference class
//# Copyright (C) 1998
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

#include <trial/Lattices/LCDifference.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LCEllipsoid.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


void doIt (const IPosition& latticeShape,
	   const IPosition& start,
	   const IPosition& end,
	   const IPosition& center,
	   Float radius)
{
    uInt ndim = latticeShape.nelements();
    LCBox box (start, end, latticeShape);
    LCEllipsoid cir (center, radius, latticeShape);
    LCDifference inters (box, cir);
    AlwaysAssertExit (inters.hasMask());
    AlwaysAssertExit (! inters.isWritable());
    cout << inters.hasMask() << ' ' << endl;
    cout << inters.box().start() << inters.box().end()
	 << inters.box().length() << inters.latticeShape() << endl;
    Array<Bool> mask;
    inters.getSlice (mask, IPosition(ndim,0), inters.box().length(),
		     IPosition(ndim,1));
    cout << mask << endl;
    {
      LCDifference inters (cir, box);
      AlwaysAssertExit (inters.hasMask());
      AlwaysAssertExit (! inters.isWritable());
      cout << inters.hasMask() << ' ' << endl;
      cout << inters.box().start() << inters.box().end()
	   << inters.box().length() << inters.latticeShape() << endl;
      Array<Bool> mask;
      inters.getSlice (mask, IPosition(ndim,0), inters.box().length(),
		       IPosition(ndim,1));
      cout << mask << endl;
    }
    {
	// Test cloning.
        LCRegion* interscop = inters.cloneRegion();
	AlwaysAssertExit (inters.hasMask() == interscop->hasMask());
	AlwaysAssertExit (inters.box().start() == interscop->box().start());
	AlwaysAssertExit (inters.box().end() == interscop->box().end());
	AlwaysAssertExit (inters.box().stride() == interscop->box().stride());
	AlwaysAssertExit (inters.box().length() == interscop->box().length());
	Array<Bool> arr;
	interscop->getSlice (arr, IPosition(ndim,0), inters.box().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete interscop;
    }
    {
	// Test persistency.
	LCRegion* interscop = (LCRegion::fromRecord (inters.toRecord(""), ""));
	AlwaysAssertExit (inters.hasMask() == interscop->hasMask());
	AlwaysAssertExit (inters.box().start() == interscop->box().start());
	AlwaysAssertExit (inters.box().end() == interscop->box().end());
	AlwaysAssertExit (inters.box().stride() == interscop->box().stride());
	AlwaysAssertExit (inters.box().length() == interscop->box().length());
	Array<Bool> arr;
	interscop->getSlice (arr, IPosition(ndim,0), inters.box().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete interscop;
    }
    {   
    // Test ordered equality.
       LCDifference diff1(box, cir);
       LCDifference diff2(diff1);
       AlwaysAssertExit (diff1 == diff2);
    }
    {
    // Test unordered equality.  The bounding boxes will differ.
       LCDifference diff1(box, cir);
       LCDifference diff2(cir, box);
       AlwaysAssertExit (diff1 != diff2);
    }
}


main()
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
	exit(1);
    } end_try;
    cout << "OK" << endl; 
    exit(0);
}
