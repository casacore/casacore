//# tLCComplement.cc:  mechanical test of the LCComplement class
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

#include <trial/Lattices/LCComplement.h>
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
    LCEllipsoid cir (center, radius, latticeShape);
    LCBox box (start, end, latticeShape);
    LCComplement compl (cir);
    AlwaysAssertExit (compl.hasMask());
    AlwaysAssertExit (! compl.isWritable());
    cout << compl.hasMask() << ' ' << endl;
    cout << compl.box().start() << compl.box().end()
	 << compl.box().length() << compl.latticeShape() << endl;
    Array<Bool> mask;
    compl.getSlice (mask, IPosition(ndim,0), compl.box().length(),
		     IPosition(ndim,1));
    cout << mask << endl;

    LCComplement compl1 (box); 
    AlwaysAssertExit (compl1.hasMask());
    AlwaysAssertExit (! compl1.isWritable());
    Array<Bool> mask1;
    compl1.getSlice (mask1, IPosition(ndim,0), compl1.box().length(),
		     IPosition(ndim,1));
    cout << mask1 << endl;
   
    {
	// Test cloning.
        LCRegion* complcop = compl.cloneRegion();
	AlwaysAssertExit (compl.hasMask() == complcop->hasMask());
	AlwaysAssertExit (compl.box().start() == complcop->box().start());
	AlwaysAssertExit (compl.box().end() == complcop->box().end());
	AlwaysAssertExit (compl.box().stride() == complcop->box().stride());
	AlwaysAssertExit (compl.box().length() == complcop->box().length());
	Array<Bool> arr;
	complcop->getSlice (arr, IPosition(ndim,0), compl.box().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete complcop;
    }
    {
	// Test persistency.
	LCRegion* complcop = (LCRegion::fromRecord (compl.toRecord(""), ""));
	AlwaysAssertExit (compl.hasMask() == complcop->hasMask());
	AlwaysAssertExit (compl.box().start() == complcop->box().start());
	AlwaysAssertExit (compl.box().end() == complcop->box().end());
	AlwaysAssertExit (compl.box().stride() == complcop->box().stride());
	AlwaysAssertExit (compl.box().length() == complcop->box().length());
	Array<Bool> arr;
	complcop->getSlice (arr, IPosition(ndim,0), compl.box().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete complcop;
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
	return 1;
    } end_try;
    cout << "OK" << endl;
    return 0;
}
