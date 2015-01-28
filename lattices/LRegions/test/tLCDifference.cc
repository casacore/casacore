//# tLCDifference.cc:  mechanical test of the LCDifference class
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

#include <casacore/lattices/LRegions/LCDifference.h>
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
    uInt ndim = latticeShape.nelements();
    LCBox box (start, end, latticeShape);
    LCEllipsoid cir (center, radius, latticeShape);
    LCDifference inters (box, cir);
    AlwaysAssertExit (inters.hasMask());
    AlwaysAssertExit (! inters.isWritable());
    cout << inters.hasMask() << ' ' << endl;
    cout << inters.boundingBox().start() << inters.boundingBox().end()
	 << inters.boundingBox().length() << inters.latticeShape() << endl;
    Array<Bool> mask;
    inters.getSlice (mask, IPosition(ndim,0), inters.boundingBox().length(),
		     IPosition(ndim,1));
    cout << mask << endl;
    {
      LCDifference inters (cir, box);
      AlwaysAssertExit (inters.hasMask());
      AlwaysAssertExit (! inters.isWritable());
      cout << inters.hasMask() << ' ' << endl;
      cout << inters.boundingBox().start() << inters.boundingBox().end()
	   << inters.boundingBox().length() << inters.latticeShape() << endl;
      Array<Bool> mask;
      inters.getSlice (mask, IPosition(ndim,0), inters.boundingBox().length(),
		       IPosition(ndim,1));
      cout << mask << endl;
    }
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
