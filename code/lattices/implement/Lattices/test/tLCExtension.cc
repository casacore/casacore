//# tLCExtension.cc: Test program for LCExtension class
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

#include <trial/Lattices/LCExtension.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LCPolygon.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


void doIt (const LCRegion& region,
	   const IPosition& axes,
	   const IPosition& blc,
	   const IPosition& trc,
	   const IPosition& latticeShape)
{
    try {
	LCExtension prism (region, axes, LCBox(blc, trc, latticeShape));
	AlwaysAssertExit (prism.hasMask() == region.hasMask());
	AlwaysAssertExit (! prism.isWritable());
	Array<Bool> regmask;
	uInt ndimr = region.boundingBox().ndim();
        uInt ndim = ndimr + latticeShape.nelements();
	((LCRegion&)region).getSlice (regmask, IPosition(ndimr,0),
				      region.boundingBox().length(),
				      IPosition(ndimr,1));
	cout << regmask << endl;
	Array<Bool> mask;
	prism.getSlice (mask, IPosition(ndim,0), prism.boundingBox().length(),
			IPosition(ndim,1));
	cout << mask << endl;
	cout << prism.hasMask() << ' ' << endl;
	cout << prism.boundingBox().start() << prism.boundingBox().end()
	     << prism.boundingBox().length() << prism.latticeShape() << endl;
	cout << prism.extendAxes() << prism.extendBox().blc().ac()
	     << prism.extendBox().trc().ac() << endl;
      {
	// Test cloning.
	LCRegion* prismcop = prism.cloneRegion();
	AlwaysAssertExit (prism.hasMask() == prismcop->hasMask());
	AlwaysAssertExit (prism.boundingBox().start() ==
			  prismcop->boundingBox().start());
	AlwaysAssertExit (prism.boundingBox().end() ==
			  prismcop->boundingBox().end());
	AlwaysAssertExit (prism.boundingBox().stride() ==
			  prismcop->boundingBox().stride());
	AlwaysAssertExit (prism.boundingBox().length() ==
			  prismcop->boundingBox().length());
	Array<Bool> arr;
	prismcop->getSlice (arr, IPosition(ndim,0),
			    prism.boundingBox().length(),
			    IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete prismcop;
      }
      {
	// Test persistency.
	LCRegion* prismcop = LCRegion::fromRecord (prism.toRecord(""), "");
	AlwaysAssertExit (prism.hasMask() == prismcop->hasMask());
	AlwaysAssertExit (prism.boundingBox().start() ==
			  prismcop->boundingBox().start());
	AlwaysAssertExit (prism.boundingBox().end() ==
			  prismcop->boundingBox().end());
	AlwaysAssertExit (prism.boundingBox().stride() ==
			  prismcop->boundingBox().stride());
	AlwaysAssertExit (prism.boundingBox().length() ==
			  prismcop->boundingBox().length());
	Array<Bool> arr;
	prismcop->getSlice (arr, IPosition(ndim,0),
			    prism.boundingBox().length(),
			    IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete prismcop;
      }
      {
        // Test ordered equality.
        LCExtension prism2(prism);
        AlwaysAssertExit (prism2 == prism);
      }
      {
        // Test unordered equality.
        LCExtension prism2 (region, axes, LCBox(blc-1, trc, latticeShape));
        AlwaysAssertExit (prism2 != prism);
      }
    } catch (AipsError x) {
	cout << x.getMesg() << endl;
    } end_try;
}


main()
{
    try {
	// A simple box (having no mask).
	LCBox box (IPosition(2,1,4), IPosition(2,5,6), IPosition(2,12,14));
	// A cross-like figure.
	Vector<Float> x(4), y(4);
	x(0)=3; y(0)=3;
	x(1)=9; y(1)=3;
	x(2)=3; y(2)=8;
	x(3)=9; y(3)=8;
	LCPolygon polygon(x, y, IPosition(2,12,14));
	
	doIt (box, IPosition(1,1), IPosition(1,2), IPosition(1,3),
	      IPosition(1,20));
	doIt (polygon, IPosition(1,2), IPosition(1,2), IPosition(1,3),
	      IPosition(1,4));
	doIt (polygon, IPosition(1,1), IPosition(1,2), IPosition(1,3),
	      IPosition(1,20));
	doIt (polygon, IPosition(2,0,2), IPosition(2,2,0), IPosition(2,3,2),
	      IPosition(2,20,10));
	// Trc outside lattice, is silently adjusted
	doIt (polygon, IPosition(1,2), IPosition(1,2), IPosition(1,5),
	      IPosition(1,3));
	// Error; no extendaxes
	doIt (polygon, IPosition(), IPosition(1,2), IPosition(1,3),
	      IPosition(1,20));
	// Error; #extendAxes mismatches blc/trc
	doIt (polygon, IPosition(2,1,2), IPosition(1,2), IPosition(1,3),
	      IPosition(1,20));
	// Error; incorrect order of extendAxes
	doIt (polygon, IPosition(2,1), IPosition(2,2), IPosition(2,3),
	      IPosition(2,20));
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	exit(1);
    } end_try;
    cout << "OK" << endl;
    exit(0);
}
