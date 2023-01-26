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

#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCEllipsoid.h>
#include <casacore/lattices/LRegions/LCUnion.h>
#include <casacore/lattices/LRegions/LCIntersection.h>
#include <casacore/lattices/LRegions/LCDifference.h>
#include <casacore/lattices/LRegions/LCComplement.h>
#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
bool checkFloat (const LatticeExprNode& expr, 
		 const SubLattice<float>& lat,
		 const LCRegion& region)
{
    SubLattice<float> sublat (lat, region);
    Array<float> result;
    result = sublat.get();
    Array<bool> mask;
    if (sublat.isMasked()) {
      mask = sublat.getMask();
    }
    // Test if result is no scalar.
    if (expr.isScalar()) {
        cout << "   result should be an array" << endl;
	return false;
    }
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return false;
    }
    // Get the result (value and optional mask).
    LELArray<float> arr(shape);
    IPosition origin(shape);
    origin = 0;
    Slicer slice(origin, shape);
    expr.eval (arr, slice);
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return false;
    }
    // Check if there is a mask if it should be.
    if (sublat.isMasked() != arr.isMasked()) {
	cout << "   mismatch in arr.isMasked" << endl;
	return false;
    }
    // If masked, test if matches.
    // If entire mask is false, the values can be anything (thus not checked).
    if (arr.isMasked()) {
        if (! allEQ (arr.mask(), mask)) {
	    cout << "   expected mask " << mask << endl;
	    cout << "             got " << arr.mask() << endl;
	    return false;
	}
    }
    return true;
}


bool doIt (const SubLattice<float>& aF, const LCRegion& region1,
	   const LCRegion& region2)
{
    bool ok = true;
    LatticeExprNode node(aF);
    if (!checkFloat (node[region1], aF, region1)) ok = false;
    if (!checkFloat (node[region2], aF, region2)) ok = false;
    if (!checkFloat (node[region1 && region2], aF,
		     LCIntersection(region1,region2))) ok = false;
    if (!checkFloat (node[region1 || region2], aF,
		     LCUnion(region1,region2))) ok = false;
    if (!checkFloat (node[region1 - region2], aF,
		     LCDifference(region1,region2))) ok = false;
    if (!checkFloat (node[!region1], aF, LCComplement(region1))) ok = false;

    return ok;
}


int main()
{
    bool ok = true;
    try {
	IPosition shape(2,5,5);
	Array<float> arra(shape);
	indgen (arra, float(1), float(1));
	ArrayLattice<float> aF(arra);
	LCBox box1(shape);
	LCBox box2(IPosition(2,0,0), shape-2, shape);
	LCEllipsoid cir1(IPosition(2,2,2),3,shape);
	LCEllipsoid cir2(IPosition(2,1,3),3,shape);
	if (!doIt(SubLattice<float>(aF), box1, box2)) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF), cir1, box1)) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF), box2, cir2)) {
	    ok = false;
	}

	Array<bool> mat1(shape);
	mat1 = true;
	mat1(IPosition(2,1,0)) = false;
	LCPixelSet mask1 (mat1, LCBox(shape));
	SubLattice<float> sublat (aF, mask1);
	if (!doIt(sublat, box1, box2)) {
	    ok = false;
	}
	if (!doIt(sublat, cir1, box1)) {
	    ok = false;
	}
	if (!doIt(sublat, box2, cir2)) {
	    ok = false;
	}
    } catch (std::exception& x) {
	cout << "Caught exception: " << x.what() << endl;
	ok = false;
    } 
    if (ok) {
	cout << "OK" << endl;
	return 0;
    }
    return 1;
}
