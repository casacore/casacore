//# tLCComplement.cc:  mechanical test of the LCComplement class
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <casacore/lattices/LRegions/LCComplement.h>
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
    LCEllipsoid cir (center, radius, latticeShape);
    LCBox box (start, end, latticeShape);
    LCComplement compl0 (cir);
    AlwaysAssertExit (compl0.hasMask());
    AlwaysAssertExit (! compl0.isWritable());
    cout << compl0.hasMask() << ' ' << endl;
    cout << compl0.boundingBox().start() << compl0.boundingBox().end()
	 << compl0.boundingBox().length() << compl0.latticeShape() << endl;
    Array<Bool> mask;
    compl0.getSlice (mask, IPosition(ndim,0), compl0.boundingBox().length(),
		     IPosition(ndim,1));
    cout << mask << endl;

    LCComplement compl1 (box); 
    AlwaysAssertExit (compl1.hasMask());
    AlwaysAssertExit (! compl1.isWritable());
    Array<Bool> mask1;
    compl1.getSlice (mask1, IPosition(ndim,0), compl1.boundingBox().length(),
		     IPosition(ndim,1));
    cout << mask1 << endl;
   
    {
	// Test cloning.
        LCRegion* complcop = compl0.cloneRegion();
	AlwaysAssertExit (compl0.hasMask() == complcop->hasMask());
	AlwaysAssertExit (compl0.boundingBox().start() ==
			  complcop->boundingBox().start());
	AlwaysAssertExit (compl0.boundingBox().end() ==
			  complcop->boundingBox().end());
	AlwaysAssertExit (compl0.boundingBox().stride() ==
			  complcop->boundingBox().stride());
	AlwaysAssertExit (compl0.boundingBox().length() ==
			  complcop->boundingBox().length());
	Array<Bool> arr;
	complcop->getSlice (arr, IPosition(ndim,0),
			    compl0.boundingBox().length(),
			     IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete complcop;
    }
    {
	// Test persistency.
	LCRegion* complcop = (LCRegion::fromRecord (compl0.toRecord(""), ""));
	AlwaysAssertExit (compl0.hasMask() == complcop->hasMask());
	AlwaysAssertExit (compl0.boundingBox().start() ==
			  complcop->boundingBox().start());
	AlwaysAssertExit (compl0.boundingBox().end() ==
			  complcop->boundingBox().end());
	AlwaysAssertExit (compl0.boundingBox().stride() ==
			  complcop->boundingBox().stride());
	AlwaysAssertExit (compl0.boundingBox().length() ==
			  complcop->boundingBox().length());
	Array<Bool> arr;
	complcop->getSlice (arr, IPosition(ndim,0),
			    compl0.boundingBox().length(),
			    IPosition(ndim,1));
	AlwaysAssertExit (allEQ (arr, mask));
	delete complcop;
    }
    {   
    // Test equality.
       LCComplement comp1(box);
       LCComplement comp2(comp1);
       AlwaysAssertExit (comp2 == comp1);
       LCComplement comp3(cir);
       AlwaysAssertExit (comp3 != comp1);
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
