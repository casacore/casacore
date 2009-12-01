//# tLatticeExprNode3.cc:  Test program for regions in LatticeExprNode
//# Copyright (C) 1999,2000,2001
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

#include <lattices/Lattices/LatticeExprNode.h>
#include <lattices/Lattices/ArrayLattice.h>
#include <lattices/Lattices/SubLattice.h>
#include <lattices/Lattices/LELArray.h>
#include <lattices/Lattices/LCBox.h>
#include <lattices/Lattices/LCEllipsoid.h>
#include <lattices/Lattices/LCUnion.h>
#include <lattices/Lattices/LCIntersection.h>
#include <lattices/Lattices/LCDifference.h>
#include <lattices/Lattices/LCComplement.h>
#include <lattices/Lattices/LCPixelSet.h>
#include <casa/Arrays/Slicer.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>


#include <casa/namespace.h>
Bool checkFloat (const LatticeExprNode& expr, 
		 const SubLattice<Float>& lat,
		 const LCRegion& region)
{
    SubLattice<Float> sublat (lat, region);
    Array<Float> result;
    result = sublat.get();
    Array<Bool> mask;
    if (sublat.isMasked()) {
      mask = sublat.getMask();
    }
    // Test if result is no scalar.
    if (expr.isScalar()) {
        cout << "   result should be an array" << endl;
	return False;
    }
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return False;
    }
    // Get the result (value and optional mask).
    LELArray<Float> arr(shape);
    IPosition origin(shape);
    origin = 0;
    Slicer slice(origin, shape);
    expr.eval (arr, slice);
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return False;
    }
    // Check if there is a mask if it should be.
    if (sublat.isMasked() != arr.isMasked()) {
	cout << "   mismatch in arr.isMasked" << endl;
	return False;
    }
    // If masked, test if matches.
    // If entire mask is false, the values can be anything (thus not checked).
    if (arr.isMasked()) {
        if (! allEQ (arr.mask(), mask)) {
	    cout << "   expected mask " << mask << endl;
	    cout << "             got " << arr.mask() << endl;
	    return False;
	}
    }
    return True;
}


Bool doIt (const SubLattice<Float>& aF, const LCRegion& region1,
	   const LCRegion& region2)
{
    Bool ok = True;
    LatticeExprNode node(aF);
    if (!checkFloat (node[region1], aF, region1)) ok = False;
    if (!checkFloat (node[region2], aF, region2)) ok = False;
    if (!checkFloat (node[region1 && region2], aF,
		     LCIntersection(region1,region2))) ok = False;
    if (!checkFloat (node[region1 || region2], aF,
		     LCUnion(region1,region2))) ok = False;
    if (!checkFloat (node[region1 - region2], aF,
		     LCDifference(region1,region2))) ok = False;
    if (!checkFloat (node[!region1], aF, LCComplement(region1))) ok = False;

    return ok;
}


int main()
{
    Bool ok = True;
    try {
	IPosition shape(2,5,5);
	Array<Float> arra(shape);
	indgen (arra, Float(1), Float(1));
	ArrayLattice<Float> aF(arra);
	LCBox box1(shape);
	LCBox box2(IPosition(2,0,0), shape-2, shape);
	LCEllipsoid cir1(IPosition(2,2,2),3,shape);
	LCEllipsoid cir2(IPosition(2,1,3),3,shape);
	if (!doIt(SubLattice<Float>(aF), box1, box2)) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF), cir1, box1)) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF), box2, cir2)) {
	    ok = False;
	}

	Array<Bool> mat1(shape);
	mat1 = True;
	mat1(IPosition(2,1,0)) = False;
	LCPixelSet mask1 (mat1, LCBox(shape));
	SubLattice<Float> sublat (aF, mask1);
	if (!doIt(sublat, box1, box2)) {
	    ok = False;
	}
	if (!doIt(sublat, cir1, box1)) {
	    ok = False;
	}
	if (!doIt(sublat, box2, cir2)) {
	    ok = False;
	}
    } catch (AipsError x) {
	cout << "Caught exception: " << x.getMesg() << endl;
	ok = False;
    } 
    if (ok) {
	cout << "OK" << endl;
	return 0;
    }
    return 1;
}
