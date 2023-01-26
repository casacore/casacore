//# tLatticeExprNode2.cc:  Test program for masks in LatticeExprNode
//# Copyright (C) 1998,1999,2000,2001
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
		 const Array<float>& result,
		 float scalarResult,
		 bool isInvalid,
		 const Array<bool>& mask)
{
    // Test if result is indeed a scalar.
    // If so, test if invalid if it should be.
    // If not invalid, test if result matches.
    if (result.nelements() == 0) {
       if (! expr.isScalar()) {
	   cout << "   expected scalar result" << endl;
	   return false;
       }
       if (isInvalid != expr.isInvalidScalar()) {
	   cout << "   mismatch in invalid; expected " << isInvalid << endl;
	   return false;
       }
       if (!isInvalid) {
	   if (expr.getFloat() != scalarResult) {
	       cout << "   expected value " << scalarResult << endl;
	       cout << "       got scalar " << expr.getFloat() << endl;
	       return false;
	   }
       }
       return true;
    }
    // The result is an array.
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return false;
    }
    // Get the result (value and optional mask).
    LELArray<float> arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval (arr, region);
    // Check if there is a mask if it should be.
    if ((mask.nelements()==0) == arr.isMasked()) {
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
	} else if (allEQ (mask, false)) {
	    return true;
	}
    }
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return false;
    }
    return true;
}

bool checkComplex (const LatticeExprNode& expr, 
		   const Array<Complex>& result,
		   Complex scalarResult,
		   bool isInvalid,
		   const Array<bool>& mask)
{
    // Test if result is indeed a scalar.
    // If so, test if invalid if it should be.
    // If not invalid, test if result matches.
    if (result.nelements() == 0) {
       if (! expr.isScalar()) {
	   cout << "   expected scalar result" << endl;
	   return false;
       }
       if (isInvalid != expr.isInvalidScalar()) {
	   cout << "   mismatch in invalid; expected " << isInvalid << endl;
	   return false;
       }
       if (!isInvalid) {
	   if (expr.getComplex() != scalarResult) {
	       cout << "   expected value " << scalarResult << endl;
	       cout << "       got scalar " << expr.getComplex() << endl;
	       return false;
	   }
       }
       return true;
    }
    // The result is an array.
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return false;
    }
    // Get the result (value and optional mask).
    LELArray<Complex> arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval (arr, region);
    // Check if there is a mask if it should be.
    if ((mask.nelements()==0) == arr.isMasked()) {
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
	} else if (allEQ (mask, false)) {
	    return true;
	}
    }
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return false;
    }
    return true;
}

bool checkBool (const LatticeExprNode& expr, 
		const Array<bool>& result,
		bool scalarResult,
		bool isInvalid,
		const Array<bool>& mask,
		bool orand)
{
    // Test if result is indeed a scalar.
    // If so, test if invalid if it should be.
    // If not invalid, test if result matches.
    if (result.nelements() == 0) {
       if (! expr.isScalar()) {
	   cout << "   expected scalar result" << endl;
	   return false;
       }
       if (isInvalid != expr.isInvalidScalar()) {
	   cout << "   mismatch in invalid; expected " << isInvalid << endl;
	   return false;
       }
       if (!isInvalid) {
	   if (expr.getBool() != scalarResult) {
	       cout << "   expected value " << scalarResult << endl;
	       cout << "       got scalar " << expr.getBool() << endl;
	       return false;
	   }
       }
       return true;
    }
    // The result is an array.
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return false;
    }
    // Get the result (value and optional mask).
    LELArray<bool> arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval (arr, region);
    // Check if there is a mask if it should be.
    // When an or/and was done, it can be the case that no mask is
    // present if entire mask is true.
    if ((mask.nelements()==0) == arr.isMasked()) {
	if (!orand  ||  mask.nelements() == 0) {
	    cout << "   mismatch in arr.isMasked" << endl;
	    return false;
	} else if (!allEQ(mask, true)) {
	    cout << "   expected or/and mask " << mask << endl;
	    return false;
	}
    }
    // If masked, test if matches.
    // If entire mask is false, the values can be anything (thus not checked).
    if (arr.isMasked()) {
        if (! allEQ (arr.mask(), mask)) {
	    cout << "   expected mask " << mask << endl;
	    cout << "             got " << arr.mask() << endl;
	    return false;
	} else if (allEQ (mask, false)) {
	    return true;
	}
    }
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return false;
    }
    return true;
}


bool doIt (const SubLattice<float>& aF, const SubLattice<float>& bF)
{
    Array<bool> lmask;
    Array<float> emptyFArr;
    Array<bool> emptyBArr;
    Array<Complex> emptyCArr;
    Array<bool> emptyMask;
    Array<float> arra;
    arra = aF.get();
    Array<float> arrb;
    arrb = bF.get();
    Array<bool> aMask = aF.getMask().copy();
    Array<bool> bMask = bF.getMask().copy();
    Array<bool> mask = aMask && bMask;
    Array<bool> asMask = aMask.copy();
    Array<bool> bsMask = bMask.copy();
    Array<bool> abMask = aMask || bMask;
    bool aInvalid = false;
    bool bInvalid = false;
    if (allEQ(aMask,false)) {
	aInvalid = true;
	bsMask = false;
    } else if (! bF.isMasked()) {
        bsMask.reference (emptyMask);
    }
    if (allEQ(bMask,false)) {
	bInvalid = true;
	asMask = false;
    } else if (! aF.isMasked()) {
        asMask.reference (emptyMask);
    }
    bool invalid = (aInvalid || bInvalid);
    if (!aF.isMasked() && !bF.isMasked()) {
	mask.reference (emptyMask);
	abMask.reference (emptyMask);
    }
    if (!aF.isMasked()) {
	aMask.reference (emptyMask);
    }
    if (!bF.isMasked()) {
	bMask.reference (emptyMask);
    }
    bool ok = true;

    if (!checkFloat(aF+bF, arra+arrb,
		    0, false, mask)) ok = false;
    if (!checkFloat(aF+min(bF), arra+min(arrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(max(aF)+bF, arrb+max(arra),
		    0, false, bsMask)) ok = false;
    if (!checkFloat(min(aF)+max(bF), emptyFArr,
		    min(arra)+max(arrb), invalid, emptyMask)) ok = false;

    if (!checkFloat(aF-bF, arra-arrb,
		    0, false, mask)) ok = false;
    if (!checkFloat(aF-min(bF), arra-min(arrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(max(aF)-bF, max(arra)-arrb,
		    0, false, bsMask)) ok = false;
    if (!checkFloat(min(aF)-max(bF), emptyFArr,
		    min(arra)-max(arrb), invalid, emptyMask)) ok = false;

    if (!checkFloat(aF*bF, arra*arrb,
		    0, false, mask)) ok = false;
    if (!checkFloat(aF*min(bF), arra*min(arrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(max(aF)*bF, arrb*max(arra),
		    0, false, bsMask)) ok = false;
    if (!checkFloat(min(aF)*max(bF), emptyFArr,
		    min(arra)*max(arrb), invalid, emptyMask)) ok = false;

    if (!checkFloat(aF/bF, arra/arrb,
		    0, false, mask)) ok = false;
    if (!checkFloat(aF/min(bF), arra/min(arrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(max(aF)/bF, max(arra)/arrb,
		    0, false, bsMask)) ok = false;
    if (!checkFloat(min(aF)/max(bF), emptyFArr,
		    min(arra)/max(arrb), invalid, emptyMask)) ok = false;

    Array<float> maxarra(arra.shape());
    maxarra = max(arra);
    Array<float> minarrb(arrb.shape());
    minarrb = min(arrb);
    if (!checkFloat(atan2(aF,bF), atan2(arra,arrb),
		    0, false, mask)) ok = false;
    if (!checkFloat(atan2(aF,min(bF)), atan2(arra,minarrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(atan2(max(aF),bF), atan2(maxarra,arrb),
		    0, false, bsMask)) ok = false;
    if (!checkFloat(atan2(min(aF),max(bF)), emptyFArr,
		    atan2(min(arra),max(arrb)), invalid, emptyMask)) ok = false;

    if (!checkFloat(pow(aF,bF), pow(arra,arrb),
		    0, false, mask)) ok = false;
    if (!checkFloat(pow(aF,min(bF)), pow(arra,minarrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(pow(max(aF),bF), pow(maxarra,arrb),
		    0, false, bsMask)) ok = false;
    if (!checkFloat(pow(min(aF),max(bF)), emptyFArr,
		    pow(min(arra),max(arrb)), invalid, emptyMask)) ok = false;

    if (!checkFloat(fmod(aF,bF), fmod(arra,arrb),
		    0, false, mask)) ok = false;
    if (!checkFloat(fmod(aF,min(bF)), fmod(arra,minarrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(fmod(max(aF),bF), fmod(maxarra,arrb),
		    0, false, bsMask)) ok = false;
    if (!checkFloat(fmod(min(aF),max(bF)), emptyFArr,
		    fmod(min(arra),max(arrb)), invalid, emptyMask)) ok = false;

       // SGI needs LatticeExprNode(SubLattice) for min(SubLattice, SubLattice) and
       //  max(SubLattice, SubLattice), It's scoping problem with -LANG:std
    if (!checkFloat((min(LatticeExprNode(aF),LatticeExprNode(bF))), min(arra,arrb),
		    0, false, static_cast<Array<bool> >(mask))) ok = false;
    if (!checkFloat(min(aF,min(bF)), min(arra,minarrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(min(max(aF),bF), min(maxarra,arrb),
		    0, false, bsMask)) ok = false;
    if (!checkFloat(min(min(aF),max(bF)), emptyFArr,
		    min(min(arra),max(arrb)), invalid, emptyMask)) ok = false;

    if (!checkFloat(max(LatticeExprNode(aF),LatticeExprNode(bF)), max(arra,arrb),
		    0, false, mask)) ok = false;
    if (!checkFloat(max(aF,min(bF)), max(arra,minarrb),
		    0, false, asMask)) ok = false;
    if (!checkFloat(max(max(aF),bF), max(maxarra,arrb),
		    0, false, bsMask)) ok = false;
    if (!checkFloat(max(min(aF),max(bF)), emptyFArr,
		    max(min(arra),max(arrb)), invalid, emptyMask)) ok = false;

    Array<Complex> arrc(arra.shape());
    bool delc, dela, delb;
    uint32_t nr = arrc.nelements();
    Complex* cptr = arrc.getStorage(delc);
    const float* aptr = arra.getStorage(dela);
    const float* bptr = arrb.getStorage(delb);
    for (uint32_t i=0; i<nr; i++) {
       cptr[i] = Complex(aptr[i], bptr[i]);
    }
    if (!checkComplex(formComplex(aF,bF), arrc,
		      0, false, mask)) ok = false;
    for (uint32_t i=0; i<nr; i++) {
       cptr[i] = Complex(aptr[i], min(arrb));
    }
    if (!checkComplex(formComplex(aF,min(bF)), arrc,
		      0, false, asMask)) ok = false;
    for (uint32_t i=0; i<nr; i++) {
       cptr[i] = Complex(max(arra), bptr[i]);
    }
    if (!checkComplex(formComplex(max(aF),bF), arrc,
		      0, false, bsMask)) ok = false;
    Array<Complex> arrc2;
    if (!checkComplex(formComplex(min(aF),max(bF)), arrc2,
		      Complex(min(arra),max(arrb)),
		      invalid, emptyMask)) ok = false;

    // Test comparison operators.
    if (!checkBool (aF==bF, arra==arrb,
		    0, false, mask, false)) ok = false;
    if (!checkBool (aF==min(bF), arra==min(arrb),
		    0, false, asMask, false)) ok = false;
    if (!checkBool (max(aF)==bF, arrb==max(arra),
		    0, false, bsMask, false)) ok = false;
    if (!checkBool (min(aF)==max(bF), emptyBArr,
		    min(arra)==max(arrb), invalid, emptyMask, false)) ok = false;

    if (!checkBool (LatticeExprNode(aF)!=bF, arra!=arrb,
		    0, false, mask, false)) ok = false;
    if (!checkBool (aF!=min(bF), arra!=min(arrb),
		    0, false, asMask, false)) ok = false;
    if (!checkBool (max(aF)!=bF, arrb!=max(arra),
		    0, false, bsMask, false)) ok = false;
    if (!checkBool (min(aF)!=max(bF), emptyBArr,
		    min(arra)!=max(arrb), invalid, emptyMask, false)) ok = false;

    if (!checkBool (aF<bF, arra<arrb,
		    0, false, mask, false)) ok = false;
    if (!checkBool (aF<min(bF), arra<min(arrb),
		    0, false, asMask, false)) ok = false;
    if (!checkBool (max(aF)<bF, max(arra)<arrb,
		    0, false, bsMask, false)) ok = false;
    if (!checkBool (min(aF)<max(bF), emptyBArr,
		    min(arra)<max(arrb), invalid, emptyMask, false)) ok = false;

    if (!checkBool (LatticeExprNode(aF)<=bF, arra<=arrb,
		    0, false, mask, false)) ok = false;
    if (!checkBool (aF<=min(bF), arra<=min(arrb),
		    0, false, asMask, false)) ok = false;
    if (!checkBool (max(aF)<=bF, max(arra)<=arrb,
		    0, false, bsMask, false)) ok = false;
    if (!checkBool (min(aF)<=max(bF), emptyBArr,
		    min(arra)<=max(arrb), invalid, emptyMask, false)) ok = false;

    if (!checkBool (LatticeExprNode(aF)>bF, arra>arrb,
		    0, false, mask, false)) ok = false;
    if (!checkBool (aF>min(bF), arra>min(arrb),
		    0, false, asMask, false)) ok = false;
    if (!checkBool (max(aF)>bF, max(arra)>arrb,
		    0, false, bsMask, false)) ok = false;
    if (!checkBool (min(aF)>max(bF), emptyBArr,
		    min(arra)>max(arrb), invalid, emptyMask, false)) ok = false;

    if (!checkBool (LatticeExprNode(aF)>=bF, arra>=arrb,
		    0, false, mask, false)) ok = false;
    if (!checkBool (aF>=min(bF), arra>=min(arrb),
		    0, false, asMask, false)) ok = false;
    if (!checkBool (max(aF)>=bF, max(arra)>=arrb,
		    0, false, bsMask, false)) ok = false;
    if (!checkBool (min(aF)>=max(bF), emptyBArr,
		    min(arra)>=max(arrb), invalid, emptyMask, false)) ok = false;

    // Test anding of array and array.
    // This is already tested more extensively in tLatticeExprNode.
    if (!checkBool (aF==aF && bF==bF, arra==arra && arrb==arrb,
		    0, false, mask, true)) ok = false;

    // Test anding of array and scalar.
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (aF==aF && min(bF)==0, arra==arra && min(arrb)==0,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF && min(bF)==0,
		    arra!=arra && min(arrb)==0,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (aF==aF && min(bF)!=0, arra==arra && min(arrb)!=0,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF && min(bF)!=0,
		    arra!=arra && min(arrb)!=0,
		    0, false, lmask, true)) ok = false;

    // Test anding of scalar and array.
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)==0 && bF==bF, max(arra)==0 && arrb==arrb,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)==0 && LatticeExprNode(bF)!=bF,
		    max(arra)==0 && arrb!=arrb,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)!=0 && LatticeExprNode(bF)==bF,
		    max(arra)!=0 && arrb==arrb,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)!=0 && LatticeExprNode(bF)!=bF,
		    max(arra)!=0 && arrb!=arrb,
		    0, false, lmask, true)) ok = false;

    // Test anding of scalar and scalar.
    invalid =  (!((min(arra)==0 && !aInvalid) ||
	                (max(arrb)==0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)!=0 && max(bF)!=0, emptyBArr,
		    min(arra)!=0 && max(arrb)!=0, invalid,
		    emptyMask, true)) ok = false;
    invalid =  (!((min(arra)==0 && !aInvalid) ||
	                (max(arrb)!=0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)!=0 && max(bF)==0, emptyBArr,
		    min(arra)!=0 && max(arrb)==0, invalid,
		    emptyMask, true)) ok = false;
    invalid =  (!((min(arra)!=0 && !aInvalid) ||
	                (max(arrb)==0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)==0 && max(bF)!=0, emptyBArr,
		    min(arra)==0 && max(arrb)!=0, invalid,
		    emptyMask, true)) ok = false;
    invalid =  (!((min(arra)!=0 && !aInvalid) ||
	                (max(arrb)!=0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)==0 && max(bF)==0, emptyBArr,
		    min(arra)==0 && max(arrb)==0, invalid,
		    emptyMask, true)) ok = false;


    // Test oring of array and array.
    // This is already tested more extensively in tLatticeExprNode.
    if (!checkBool (LatticeExprNode(aF)==aF || LatticeExprNode(bF)==bF,
		    arra==arra || arrb==arrb,
		    0, false, abMask, true)) ok = false;

    // Test oring of array and scalar.
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)==aF || min(bF)==0,
		    arra==arra || min(arrb)==0,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF || min(bF)==0,
		    arra!=arra || min(arrb)==0,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)==aF || min(bF)!=0,
		    arra==arra || min(arrb)!=0,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF || min(bF)!=0,
		    arra!=arra || min(arrb)!=0,
		    0, false, lmask, true)) ok = false;

    // Test oring of scalar and array.
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)==0 || LatticeExprNode(bF)==bF,
		    max(arra)==0 || arrb==arrb,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)==0 || LatticeExprNode(bF)!=bF,
		    max(arra)==0 || arrb!=arrb,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)!=0 || LatticeExprNode(bF)==bF,
		    max(arra)!=0 || arrb==arrb,
		    0, false, lmask, true)) ok = false;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)!=0 || LatticeExprNode(bF)!=bF,
		    max(arra)!=0 || arrb!=arrb,
		    0, false, lmask, true)) ok = false;

    // Test oring of scalar and scalar.
    invalid =  ((min(arra)==0 && bInvalid) ||
	              (max(arrb)==0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)!=0 || max(bF)!=0, emptyBArr,
		    min(arra)!=0 || max(arrb)!=0, invalid,
		    emptyMask, true)) ok = false;
    invalid =  ((min(arra)==0 && bInvalid) ||
	              (max(arrb)!=0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)!=0 || max(bF)==0, emptyBArr,
		    min(arra)!=0 || max(arrb)==0, invalid,
		    emptyMask, true)) ok = false;
    invalid =  ((min(arra)!=0 && bInvalid) ||
	              (max(arrb)==0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)==0 || max(bF)!=0, emptyBArr,
		    min(arra)==0 || max(arrb)!=0, invalid,
		    emptyMask, true)) ok = false;
    invalid =  ((min(arra)!=0 && bInvalid) ||
	              (max(arrb)!=0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)==0 || max(bF)==0, emptyBArr,
		    min(arra)==0 || max(arrb)==0, invalid,
		    emptyMask, true)) ok = false;

    // Test the iif function in all possible ways.
    // First with a scalar condition.
    Array<bool> iifMask;
    if (aInvalid || min(arra)==0) {
	iifMask.reference (aMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)==0,aF,bF), min(arra)==0?arra:arrb,
		     0, false, iifMask)) ok = false;
    if (aInvalid || min(arra)!=0) {
	iifMask.reference (aMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)!=0,aF,bF), min(arra)!=0?arra:arrb,
		     0, false, iifMask)) ok = false;

    if (aInvalid) {
	iifMask.reference (aMask);
    } else if (min(arra)==0) {
	iifMask.reference (emptyMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)==0,max(aF),bF), min(arra)==0?maxarra:arrb,
		     0, false, iifMask)) ok = false;
    if (aInvalid) {
	iifMask.reference (aMask);
    } else if (min(arra)!=0) {
	iifMask.reference (emptyMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)!=0,max(aF),bF), min(arra)!=0?maxarra:arrb,
		     0, false, iifMask)) ok = false;

    if (aInvalid || min(arra)==0) {
	iifMask.reference (aMask);
    } else if (bInvalid) {
	iifMask.reference (bMask);
    } else {
	iifMask.reference (emptyMask);
    }
    if (!checkFloat (iif(min(aF)==0,aF,min(bF)), min(arra)==0?arra:minarrb,
		     0, false, iifMask)) ok = false;
    if (aInvalid || min(arra)!=0) {
	iifMask.reference (aMask);
    } else if (bInvalid) {
	iifMask.reference (bMask);
    } else {
	iifMask.reference (emptyMask);
    }
    if (!checkFloat (iif(min(aF)!=0,aF,min(bF)), min(arra)!=0?arra:minarrb,
		     0, false, iifMask)) ok = false;

    if (!checkFloat (iif(min(aF)==0,max(aF),min(bF)), emptyFArr,
		     min(arra)==0?max(arra):min(arrb),
		     (aInvalid || (min(arra)!=0 && bInvalid)),
		     emptyMask)) ok = false;
    if (!checkFloat (iif(min(aF)!=0,max(aF),min(bF)), emptyFArr,
		     min(arra)!=0?max(arra):min(arrb),
		     (aInvalid || (min(arra)==0 && bInvalid)),
		     emptyMask)) ok = false;

    // Now test iif with an array condition.
    // Note that aF is always filled with positive values,
    // so we can be sure that the result is not a mix of arra and arrb.
    Array<bool> aiMask(aMask.copy());
    if (!aF.isMasked() && bF.isMasked()) {
	aiMask.resize (arra.shape());
	aiMask = true;
    }
    if (!checkFloat (iif(aF<0,aF,bF), arrb,
		     0, false, mask)) ok = false;
    if (!checkFloat (iif(aF>=0,aF,bF), arra,
		     0, false, aiMask)) ok = false;
    if (!checkFloat (iif(aF<0,max(aF),bF), arrb,
		     0, false, mask)) ok = false;
    if (aInvalid) {
	aiMask = false;
    }
    if (!checkFloat (iif(aF>=0,max(aF),bF), maxarra,
		     0, false, aiMask)) ok = false;
    if (!checkFloat (iif(aF<0,aF,min(bF)), minarrb,
		     0, false, asMask)) ok = false;
    if (!bInvalid) {
	aiMask.reference (aMask);
    }
    if (!checkFloat (iif(aF>=0,aF,min(bF)), arra,
		     0, false, aiMask)) ok = false;
    if (!checkFloat (iif(aF<0,max(aF),min(bF)), minarrb,
		     0, false, asMask)) ok = false;
    if (!checkFloat (iif(aF>=0,max(aF),min(bF)), maxarra,
		     0, false, aiMask)) ok = false;

    return ok;
}


int main() {
    bool ok = true;
    try {
	IPosition shape(2,2,2);
	Array<float> arra(shape);
	Array<float> arrb(shape);
	// Make sure to fill the arrays with positive values,
	// otherwise some iif tests in doIt will fail.
	indgen (arra, float(1), float(1));
	indgen (arrb, float(11), float(1));
	ArrayLattice<float> aF(arra);
	ArrayLattice<float> bF(arrb);
	Array<bool> mat1(shape);
	Array<bool> mat2(shape);
	mat1 = true;
	mat1(IPosition(2,1,0)) = false;
	mat2 = false;
	LCBox box(shape);
	LCPixelSet mask1 (mat1, box);
	LCPixelSet mask2 (mat2, box);
	if (!doIt(SubLattice<float>(aF), SubLattice<float>(bF))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF), SubLattice<float>(bF,mask1))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF,mask1), SubLattice<float>(bF))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF,mask1), SubLattice<float>(bF,mask1))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF), SubLattice<float>(bF,mask2))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF,mask2), SubLattice<float>(bF))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF,mask2), SubLattice<float>(bF,mask2))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF,mask1), SubLattice<float>(bF,mask2))) {
	    ok = false;
	}
	if (!doIt(SubLattice<float>(aF,mask2), SubLattice<float>(bF,mask1))) {
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
