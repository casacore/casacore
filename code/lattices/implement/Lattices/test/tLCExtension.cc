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
        uInt ndim = latticeShape.nelements();
	LCExtension prism (region, axes, blc, trc, latticeShape);
	AlwaysAssertExit (prism.hasMask() == region.hasMask());
	AlwaysAssertExit (! prism.isWritable());
	Array<Bool> regmask;
	uInt ndimr = region.box().ndim();
	((LCRegion&)region).getSlice (regmask, IPosition(ndimr,0),
				      region.box().length(),
				      IPosition(ndimr,1));
	cout << regmask << endl;
	Array<Bool> mask;
	prism.getSlice (mask, IPosition(ndim,0), prism.box().length(),
			IPosition(ndim,1));
	cout << mask << endl;
	cout << prism.hasMask() << ' ' << endl;
	cout << prism.box().start() << prism.box().end()
	     << prism.box().length() << prism.latticeShape() << endl;
	cout << prism.axes() << prism.blc() << prism.trc() << endl;
      {
	// Test cloning.
	LCRegion* prismcop = prism.cloneRegion();
	AlwaysAssertExit (prism.hasMask() == prismcop->hasMask());
	AlwaysAssertExit (prism.box().start() == prismcop->box().start());
	AlwaysAssertExit (prism.box().end() == prismcop->box().end());
	AlwaysAssertExit (prism.box().stride() == prismcop->box().stride());
	AlwaysAssertExit (prism.box().length() == prismcop->box().length());
	Array<Bool> arr;
	prismcop->getSlice (arr, IPosition(ndim,0), prism.box().length(),
			    IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete prismcop;
      }
      {
	// Test persistency.
	LCRegion* prismcop = LCRegion::fromRecord (prism.toRecord(""), "");
	AlwaysAssertExit (prism.hasMask() == prismcop->hasMask());
	AlwaysAssertExit (prism.box().start() == prismcop->box().start());
	AlwaysAssertExit (prism.box().end() == prismcop->box().end());
	AlwaysAssertExit (prism.box().stride() == prismcop->box().stride());
	AlwaysAssertExit (prism.box().length() == prismcop->box().length());
	Array<Bool> arr;
	prismcop->getSlice (arr, IPosition(ndim,0), prism.box().length(),
			    IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete prismcop;
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
	      IPosition(3,12,20,14));
	doIt (polygon, IPosition(1,2), IPosition(1,2), IPosition(1,3),
	      IPosition(3,12,14,4));
	doIt (polygon, IPosition(1,1), IPosition(1,2), IPosition(1,3),
	      IPosition(3,12,20,14));
	doIt (polygon, IPosition(2,0,2), IPosition(2,2,0), IPosition(2,3,2),
	      IPosition(4,20,12,10,14));
	// Error; trc outside lattice.
	doIt (polygon, IPosition(1,2), IPosition(1,2), IPosition(1,3),
	      IPosition(3,12,14,3));
	// Error; lattice axes lengths mismatch (15 should be 14).
	doIt (polygon, IPosition(1,1), IPosition(1,2), IPosition(1,3),
	      IPosition(3,12,20,15));
	// Error; trc-length mismatches blc-length.
	doIt (polygon, IPosition(1,1), IPosition(1,2), IPosition(2,3),
	      IPosition(3,12,20,14));
	// Error; lattice should have 3 dimensions.
	doIt (polygon, IPosition(1,1), IPosition(1,2), IPosition(1,3),
	      IPosition(4,12,20,14,3));
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    cout << "OK" << endl;
    return 0;
}
