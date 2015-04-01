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
//#
//# $Id$

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
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
Bool checkFloat (const LatticeExprNode& expr, 
		 const Array<Float>& result,
		 Float scalarResult,
		 Bool isInvalid,
		 const Array<Bool>& mask)
{
    // Test if result is indeed a scalar.
    // If so, test if invalid if it should be.
    // If not invalid, test if result matches.
    if (result.nelements() == 0) {
       if (! expr.isScalar()) {
	   cout << "   expected scalar result" << endl;
	   return False;
       }
       if (isInvalid != expr.isInvalidScalar()) {
	   cout << "   mismatch in invalid; expected " << isInvalid << endl;
	   return False;
       }
       if (!isInvalid) {
	   if (expr.getFloat() != scalarResult) {
	       cout << "   expected value " << scalarResult << endl;
	       cout << "       got scalar " << expr.getFloat() << endl;
	       return False;
	   }
       }
       return True;
    }
    // The result is an array.
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return False;
    }
    // Get the result (value and optional mask).
    LELArray<Float> arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval (arr, region);
    // Check if there is a mask if it should be.
    if ((mask.nelements()==0) == arr.isMasked()) {
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
	} else if (allEQ (mask, False)) {
	    return True;
	}
    }
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return False;
    }
    return True;
}

Bool checkComplex (const LatticeExprNode& expr, 
		   const Array<Complex>& result,
		   Complex scalarResult,
		   Bool isInvalid,
		   const Array<Bool>& mask)
{
    // Test if result is indeed a scalar.
    // If so, test if invalid if it should be.
    // If not invalid, test if result matches.
    if (result.nelements() == 0) {
       if (! expr.isScalar()) {
	   cout << "   expected scalar result" << endl;
	   return False;
       }
       if (isInvalid != expr.isInvalidScalar()) {
	   cout << "   mismatch in invalid; expected " << isInvalid << endl;
	   return False;
       }
       if (!isInvalid) {
	   if (expr.getComplex() != scalarResult) {
	       cout << "   expected value " << scalarResult << endl;
	       cout << "       got scalar " << expr.getComplex() << endl;
	       return False;
	   }
       }
       return True;
    }
    // The result is an array.
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return False;
    }
    // Get the result (value and optional mask).
    LELArray<Complex> arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval (arr, region);
    // Check if there is a mask if it should be.
    if ((mask.nelements()==0) == arr.isMasked()) {
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
	} else if (allEQ (mask, False)) {
	    return True;
	}
    }
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return False;
    }
    return True;
}

Bool checkBool (const LatticeExprNode& expr, 
		const Array<Bool>& result,
		Bool scalarResult,
		Bool isInvalid,
		const Array<Bool>& mask,
		Bool orand)
{
    // Test if result is indeed a scalar.
    // If so, test if invalid if it should be.
    // If not invalid, test if result matches.
    if (result.nelements() == 0) {
       if (! expr.isScalar()) {
	   cout << "   expected scalar result" << endl;
	   return False;
       }
       if (isInvalid != expr.isInvalidScalar()) {
	   cout << "   mismatch in invalid; expected " << isInvalid << endl;
	   return False;
       }
       if (!isInvalid) {
	   if (expr.getBool() != scalarResult) {
	       cout << "   expected value " << scalarResult << endl;
	       cout << "       got scalar " << expr.getBool() << endl;
	       return False;
	   }
       }
       return True;
    }
    // The result is an array.
    // Test if the shape matches.
    IPosition shape = result.shape();
    if (expr.shape() != shape) {
	cout << "   mismatch in result shape" << endl;
	return False;
    }
    // Get the result (value and optional mask).
    LELArray<Bool> arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval (arr, region);
    // Check if there is a mask if it should be.
    // When an or/and was done, it can be the case that no mask is
    // present if entire mask is true.
    if ((mask.nelements()==0) == arr.isMasked()) {
	if (!orand  ||  mask.nelements() == 0) {
	    cout << "   mismatch in arr.isMasked" << endl;
	    return False;
	} else if (!allEQ(mask, True)) {
	    cout << "   expected or/and mask " << mask << endl;
	    return False;
	}
    }
    // If masked, test if matches.
    // If entire mask is false, the values can be anything (thus not checked).
    if (arr.isMasked()) {
        if (! allEQ (arr.mask(), mask)) {
	    cout << "   expected mask " << mask << endl;
	    cout << "             got " << arr.mask() << endl;
	    return False;
	} else if (allEQ (mask, False)) {
	    return True;
	}
    }
    // Check if the values match.
    if (! allEQ (arr.value(), result)) {
	cout << "   expected value " << result << endl;
	cout << "        got array  " << arr.value() << endl;
	return False;
    }
    return True;
}


Bool doIt (const SubLattice<Float>& aF, const SubLattice<Float>& bF)
{
    Array<Bool> lmask;
    Array<Float> emptyFArr;
    Array<Bool> emptyBArr;
    Array<Complex> emptyCArr;
    Array<Bool> emptyMask;
    Array<Float> arra;
    arra = aF.get();
    Array<Float> arrb;
    arrb = bF.get();
    Array<Bool> aMask = aF.getMask().copy();
    Array<Bool> bMask = bF.getMask().copy();
    Array<Bool> mask = aMask && bMask;
    Array<Bool> asMask = aMask.copy();
    Array<Bool> bsMask = bMask.copy();
    Array<Bool> abMask = aMask || bMask;
    Bool aInvalid = False;
    Bool bInvalid = False;
    if (allEQ(aMask,False)) {
	aInvalid = True;
	bsMask = False;
    } else if (! bF.isMasked()) {
        bsMask.reference (emptyMask);
    }
    if (allEQ(bMask,False)) {
	bInvalid = True;
	asMask = False;
    } else if (! aF.isMasked()) {
        asMask.reference (emptyMask);
    }
    Bool invalid = (aInvalid || bInvalid);
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
    Bool ok = True;

    if (!checkFloat(aF+bF, arra+arrb,
		    0, False, mask)) ok = False;
    if (!checkFloat(aF+min(bF), arra+min(arrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(max(aF)+bF, arrb+max(arra),
		    0, False, bsMask)) ok = False;
    if (!checkFloat(min(aF)+max(bF), emptyFArr,
		    min(arra)+max(arrb), invalid, emptyMask)) ok = False;

    if (!checkFloat(aF-bF, arra-arrb,
		    0, False, mask)) ok = False;
    if (!checkFloat(aF-min(bF), arra-min(arrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(max(aF)-bF, max(arra)-arrb,
		    0, False, bsMask)) ok = False;
    if (!checkFloat(min(aF)-max(bF), emptyFArr,
		    min(arra)-max(arrb), invalid, emptyMask)) ok = False;

    if (!checkFloat(aF*bF, arra*arrb,
		    0, False, mask)) ok = False;
    if (!checkFloat(aF*min(bF), arra*min(arrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(max(aF)*bF, arrb*max(arra),
		    0, False, bsMask)) ok = False;
    if (!checkFloat(min(aF)*max(bF), emptyFArr,
		    min(arra)*max(arrb), invalid, emptyMask)) ok = False;

    if (!checkFloat(aF/bF, arra/arrb,
		    0, False, mask)) ok = False;
    if (!checkFloat(aF/min(bF), arra/min(arrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(max(aF)/bF, max(arra)/arrb,
		    0, False, bsMask)) ok = False;
    if (!checkFloat(min(aF)/max(bF), emptyFArr,
		    min(arra)/max(arrb), invalid, emptyMask)) ok = False;

    Array<Float> maxarra(arra.shape());
    maxarra = max(arra);
    Array<Float> minarrb(arrb.shape());
    minarrb = min(arrb);
    if (!checkFloat(atan2(aF,bF), atan2(arra,arrb),
		    0, False, mask)) ok = False;
    if (!checkFloat(atan2(aF,min(bF)), atan2(arra,minarrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(atan2(max(aF),bF), atan2(maxarra,arrb),
		    0, False, bsMask)) ok = False;
    if (!checkFloat(atan2(min(aF),max(bF)), emptyFArr,
		    atan2(min(arra),max(arrb)), invalid, emptyMask)) ok = False;

    if (!checkFloat(pow(aF,bF), pow(arra,arrb),
		    0, False, mask)) ok = False;
    if (!checkFloat(pow(aF,min(bF)), pow(arra,minarrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(pow(max(aF),bF), pow(maxarra,arrb),
		    0, False, bsMask)) ok = False;
    if (!checkFloat(pow(min(aF),max(bF)), emptyFArr,
		    pow(min(arra),max(arrb)), invalid, emptyMask)) ok = False;

    if (!checkFloat(fmod(aF,bF), fmod(arra,arrb),
		    0, False, mask)) ok = False;
    if (!checkFloat(fmod(aF,min(bF)), fmod(arra,minarrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(fmod(max(aF),bF), fmod(maxarra,arrb),
		    0, False, bsMask)) ok = False;
    if (!checkFloat(fmod(min(aF),max(bF)), emptyFArr,
		    fmod(min(arra),max(arrb)), invalid, emptyMask)) ok = False;

       // SGI needs LatticeExprNode(SubLattice) for min(SubLattice, SubLattice) and
       //  max(SubLattice, SubLattice), It's scoping problem with -LANG:std
    if (!checkFloat((min(LatticeExprNode(aF),LatticeExprNode(bF))), min(arra,arrb),
		    0, False, static_cast<Array<Bool> >(mask))) ok = False;
    if (!checkFloat(min(aF,min(bF)), min(arra,minarrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(min(max(aF),bF), min(maxarra,arrb),
		    0, False, bsMask)) ok = False;
    if (!checkFloat(min(min(aF),max(bF)), emptyFArr,
		    min(min(arra),max(arrb)), invalid, emptyMask)) ok = False;

    if (!checkFloat(max(LatticeExprNode(aF),LatticeExprNode(bF)), max(arra,arrb),
		    0, False, mask)) ok = False;
    if (!checkFloat(max(aF,min(bF)), max(arra,minarrb),
		    0, False, asMask)) ok = False;
    if (!checkFloat(max(max(aF),bF), max(maxarra,arrb),
		    0, False, bsMask)) ok = False;
    if (!checkFloat(max(min(aF),max(bF)), emptyFArr,
		    max(min(arra),max(arrb)), invalid, emptyMask)) ok = False;

    Array<Complex> arrc(arra.shape());
    Bool delc, dela, delb;
    uInt nr = arrc.nelements();
    Complex* cptr = arrc.getStorage(delc);
    const Float* aptr = arra.getStorage(dela);
    const Float* bptr = arrb.getStorage(delb);
    for (uInt i=0; i<nr; i++) {
       cptr[i] = Complex(aptr[i], bptr[i]);
    }
    if (!checkComplex(formComplex(aF,bF), arrc,
		      0, False, mask)) ok = False;
    for (uInt i=0; i<nr; i++) {
       cptr[i] = Complex(aptr[i], min(arrb));
    }
    if (!checkComplex(formComplex(aF,min(bF)), arrc,
		      0, False, asMask)) ok = False;
    for (uInt i=0; i<nr; i++) {
       cptr[i] = Complex(max(arra), bptr[i]);
    }
    if (!checkComplex(formComplex(max(aF),bF), arrc,
		      0, False, bsMask)) ok = False;
    Array<Complex> arrc2;
    if (!checkComplex(formComplex(min(aF),max(bF)), arrc2,
		      Complex(min(arra),max(arrb)),
		      invalid, emptyMask)) ok = False;

    // Test comparison operators.
    if (!checkBool (aF==bF, arra==arrb,
		    0, False, mask, False)) ok = False;
    if (!checkBool (aF==min(bF), arra==min(arrb),
		    0, False, asMask, False)) ok = False;
    if (!checkBool (max(aF)==bF, arrb==max(arra),
		    0, False, bsMask, False)) ok = False;
    if (!checkBool (min(aF)==max(bF), emptyBArr,
		    min(arra)==max(arrb), invalid, emptyMask, False)) ok = False;

    if (!checkBool (LatticeExprNode(aF)!=bF, arra!=arrb,
		    0, False, mask, False)) ok = False;
    if (!checkBool (aF!=min(bF), arra!=min(arrb),
		    0, False, asMask, False)) ok = False;
    if (!checkBool (max(aF)!=bF, arrb!=max(arra),
		    0, False, bsMask, False)) ok = False;
    if (!checkBool (min(aF)!=max(bF), emptyBArr,
		    min(arra)!=max(arrb), invalid, emptyMask, False)) ok = False;

    if (!checkBool (aF<bF, arra<arrb,
		    0, False, mask, False)) ok = False;
    if (!checkBool (aF<min(bF), arra<min(arrb),
		    0, False, asMask, False)) ok = False;
    if (!checkBool (max(aF)<bF, max(arra)<arrb,
		    0, False, bsMask, False)) ok = False;
    if (!checkBool (min(aF)<max(bF), emptyBArr,
		    min(arra)<max(arrb), invalid, emptyMask, False)) ok = False;

    if (!checkBool (LatticeExprNode(aF)<=bF, arra<=arrb,
		    0, False, mask, False)) ok = False;
    if (!checkBool (aF<=min(bF), arra<=min(arrb),
		    0, False, asMask, False)) ok = False;
    if (!checkBool (max(aF)<=bF, max(arra)<=arrb,
		    0, False, bsMask, False)) ok = False;
    if (!checkBool (min(aF)<=max(bF), emptyBArr,
		    min(arra)<=max(arrb), invalid, emptyMask, False)) ok = False;

    if (!checkBool (LatticeExprNode(aF)>bF, arra>arrb,
		    0, False, mask, False)) ok = False;
    if (!checkBool (aF>min(bF), arra>min(arrb),
		    0, False, asMask, False)) ok = False;
    if (!checkBool (max(aF)>bF, max(arra)>arrb,
		    0, False, bsMask, False)) ok = False;
    if (!checkBool (min(aF)>max(bF), emptyBArr,
		    min(arra)>max(arrb), invalid, emptyMask, False)) ok = False;

    if (!checkBool (LatticeExprNode(aF)>=bF, arra>=arrb,
		    0, False, mask, False)) ok = False;
    if (!checkBool (aF>=min(bF), arra>=min(arrb),
		    0, False, asMask, False)) ok = False;
    if (!checkBool (max(aF)>=bF, max(arra)>=arrb,
		    0, False, bsMask, False)) ok = False;
    if (!checkBool (min(aF)>=max(bF), emptyBArr,
		    min(arra)>=max(arrb), invalid, emptyMask, False)) ok = False;

    // Test anding of array and array.
    // This is already tested more extensively in tLatticeExprNode.
    if (!checkBool (aF==aF && bF==bF, arra==arra && arrb==arrb,
		    0, False, mask, True)) ok = False;

    // Test anding of array and scalar.
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (aF==aF && min(bF)==0, arra==arra && min(arrb)==0,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF && min(bF)==0,
		    arra!=arra && min(arrb)==0,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (aF==aF && min(bF)!=0, arra==arra && min(arrb)!=0,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF && min(bF)!=0,
		    arra!=arra && min(arrb)!=0,
		    0, False, lmask, True)) ok = False;

    // Test anding of scalar and array.
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)==0 && bF==bF, max(arra)==0 && arrb==arrb,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)==0 && LatticeExprNode(bF)!=bF,
		    max(arra)==0 && arrb!=arrb,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)!=0 && LatticeExprNode(bF)==bF,
		    max(arra)!=0 && arrb==arrb,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)!=0 && LatticeExprNode(bF)!=bF,
		    max(arra)!=0 && arrb!=arrb,
		    0, False, lmask, True)) ok = False;

    // Test anding of scalar and scalar.
    invalid =  (!((min(arra)==0 && !aInvalid) ||
	                (max(arrb)==0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)!=0 && max(bF)!=0, emptyBArr,
		    min(arra)!=0 && max(arrb)!=0, invalid,
		    emptyMask, True)) ok = False;
    invalid =  (!((min(arra)==0 && !aInvalid) ||
	                (max(arrb)!=0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)!=0 && max(bF)==0, emptyBArr,
		    min(arra)!=0 && max(arrb)==0, invalid,
		    emptyMask, True)) ok = False;
    invalid =  (!((min(arra)!=0 && !aInvalid) ||
	                (max(arrb)==0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)==0 && max(bF)!=0, emptyBArr,
		    min(arra)==0 && max(arrb)!=0, invalid,
		    emptyMask, True)) ok = False;
    invalid =  (!((min(arra)!=0 && !aInvalid) ||
	                (max(arrb)!=0 && !bInvalid) ||
		        (!aInvalid && !bInvalid)));
    if (!checkBool (min(aF)==0 && max(bF)==0, emptyBArr,
		    min(arra)==0 && max(arrb)==0, invalid,
		    emptyMask, True)) ok = False;


    // Test oring of array and array.
    // This is already tested more extensively in tLatticeExprNode.
    if (!checkBool (LatticeExprNode(aF)==aF || LatticeExprNode(bF)==bF,
		    arra==arra || arrb==arrb,
		    0, False, abMask, True)) ok = False;

    // Test oring of array and scalar.
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)==aF || min(bF)==0,
		    arra==arra || min(arrb)==0,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)!=0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF || min(bF)==0,
		    arra!=arra || min(arrb)==0,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (aMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)==aF || min(bF)!=0,
		    arra==arra || min(arrb)!=0,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (asMask.nelements() > 0) {
	if (bInvalid || min(arrb)==0) {
	    lmask.reference (asMask);
	}
    }
    if (!checkBool (LatticeExprNode(aF)!=aF || min(bF)!=0,
		    arra!=arra || min(arrb)!=0,
		    0, False, lmask, True)) ok = False;

    // Test oring of scalar and array.
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)==0 || LatticeExprNode(bF)==bF,
		    max(arra)==0 || arrb==arrb,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)!=0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)==0 || LatticeExprNode(bF)!=bF,
		    max(arra)==0 || arrb!=arrb,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bMask);
	}
    }
    if (!checkBool (max(aF)!=0 || LatticeExprNode(bF)==bF,
		    max(arra)!=0 || arrb==arrb,
		    0, False, lmask, True)) ok = False;
    lmask.reference (emptyMask);
    if (bsMask.nelements() > 0) {
	if (aInvalid || max(arra)==0) {
	    lmask.reference (bsMask);
	}
    }
    if (!checkBool (max(aF)!=0 || LatticeExprNode(bF)!=bF,
		    max(arra)!=0 || arrb!=arrb,
		    0, False, lmask, True)) ok = False;

    // Test oring of scalar and scalar.
    invalid =  ((min(arra)==0 && bInvalid) ||
	              (max(arrb)==0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)!=0 || max(bF)!=0, emptyBArr,
		    min(arra)!=0 || max(arrb)!=0, invalid,
		    emptyMask, True)) ok = False;
    invalid =  ((min(arra)==0 && bInvalid) ||
	              (max(arrb)!=0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)!=0 || max(bF)==0, emptyBArr,
		    min(arra)!=0 || max(arrb)==0, invalid,
		    emptyMask, True)) ok = False;
    invalid =  ((min(arra)!=0 && bInvalid) ||
	              (max(arrb)==0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)==0 || max(bF)!=0, emptyBArr,
		    min(arra)==0 || max(arrb)!=0, invalid,
		    emptyMask, True)) ok = False;
    invalid =  ((min(arra)!=0 && bInvalid) ||
	              (max(arrb)!=0 && aInvalid) ||
		      (aInvalid && bInvalid));
    if (!checkBool (min(aF)==0 || max(bF)==0, emptyBArr,
		    min(arra)==0 || max(arrb)==0, invalid,
		    emptyMask, True)) ok = False;

    // Test the iif function in all possible ways.
    // First with a scalar condition.
    Array<Bool> iifMask;
    if (aInvalid || min(arra)==0) {
	iifMask.reference (aMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)==0,aF,bF), min(arra)==0?arra:arrb,
		     0, False, iifMask)) ok = False;
    if (aInvalid || min(arra)!=0) {
	iifMask.reference (aMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)!=0,aF,bF), min(arra)!=0?arra:arrb,
		     0, False, iifMask)) ok = False;

    if (aInvalid) {
	iifMask.reference (aMask);
    } else if (min(arra)==0) {
	iifMask.reference (emptyMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)==0,max(aF),bF), min(arra)==0?maxarra:arrb,
		     0, False, iifMask)) ok = False;
    if (aInvalid) {
	iifMask.reference (aMask);
    } else if (min(arra)!=0) {
	iifMask.reference (emptyMask);
    } else {
	iifMask.reference (bMask);
    }
    if (!checkFloat (iif(min(aF)!=0,max(aF),bF), min(arra)!=0?maxarra:arrb,
		     0, False, iifMask)) ok = False;

    if (aInvalid || min(arra)==0) {
	iifMask.reference (aMask);
    } else if (bInvalid) {
	iifMask.reference (bMask);
    } else {
	iifMask.reference (emptyMask);
    }
    if (!checkFloat (iif(min(aF)==0,aF,min(bF)), min(arra)==0?arra:minarrb,
		     0, False, iifMask)) ok = False;
    if (aInvalid || min(arra)!=0) {
	iifMask.reference (aMask);
    } else if (bInvalid) {
	iifMask.reference (bMask);
    } else {
	iifMask.reference (emptyMask);
    }
    if (!checkFloat (iif(min(aF)!=0,aF,min(bF)), min(arra)!=0?arra:minarrb,
		     0, False, iifMask)) ok = False;

    if (!checkFloat (iif(min(aF)==0,max(aF),min(bF)), emptyFArr,
		     min(arra)==0?max(arra):min(arrb),
		     (aInvalid || (min(arra)!=0 && bInvalid)),
		     emptyMask)) ok = False;
    if (!checkFloat (iif(min(aF)!=0,max(aF),min(bF)), emptyFArr,
		     min(arra)!=0?max(arra):min(arrb),
		     (aInvalid || (min(arra)==0 && bInvalid)),
		     emptyMask)) ok = False;

    // Now test iif with an array condition.
    // Note that aF is always filled with positive values,
    // so we can be sure that the result is not a mix of arra and arrb.
    Array<Bool> aiMask(aMask.copy());
    if (!aF.isMasked() && bF.isMasked()) {
	aiMask.resize (arra.shape());
	aiMask = True;
    }
    if (!checkFloat (iif(aF<0,aF,bF), arrb,
		     0, False, mask)) ok = False;
    if (!checkFloat (iif(aF>=0,aF,bF), arra,
		     0, False, aiMask)) ok = False;
    if (!checkFloat (iif(aF<0,max(aF),bF), arrb,
		     0, False, mask)) ok = False;
    if (aInvalid) {
	aiMask = False;
    }
    if (!checkFloat (iif(aF>=0,max(aF),bF), maxarra,
		     0, False, aiMask)) ok = False;
    if (!checkFloat (iif(aF<0,aF,min(bF)), minarrb,
		     0, False, asMask)) ok = False;
    if (!bInvalid) {
	aiMask.reference (aMask);
    }
    if (!checkFloat (iif(aF>=0,aF,min(bF)), arra,
		     0, False, aiMask)) ok = False;
    if (!checkFloat (iif(aF<0,max(aF),min(bF)), minarrb,
		     0, False, asMask)) ok = False;
    if (!checkFloat (iif(aF>=0,max(aF),min(bF)), maxarra,
		     0, False, aiMask)) ok = False;

    return ok;
}


int main() {
    Bool ok = True;
    try {
	IPosition shape(2,2,2);
	Array<Float> arra(shape);
	Array<Float> arrb(shape);
	// Make sure to fill the arrays with positive values,
	// otherwise some iif tests in doIt will fail.
	indgen (arra, Float(1), Float(1));
	indgen (arrb, Float(11), Float(1));
	ArrayLattice<Float> aF(arra);
	ArrayLattice<Float> bF(arrb);
	Array<Bool> mat1(shape);
	Array<Bool> mat2(shape);
	mat1 = True;
	mat1(IPosition(2,1,0)) = False;
	mat2 = False;
	LCBox box(shape);
	LCPixelSet mask1 (mat1, box);
	LCPixelSet mask2 (mat2, box);
	if (!doIt(SubLattice<Float>(aF), SubLattice<Float>(bF))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF), SubLattice<Float>(bF,mask1))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF,mask1), SubLattice<Float>(bF))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF,mask1), SubLattice<Float>(bF,mask1))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF), SubLattice<Float>(bF,mask2))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF,mask2), SubLattice<Float>(bF))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF,mask2), SubLattice<Float>(bF,mask2))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF,mask1), SubLattice<Float>(bF,mask2))) {
	    ok = False;
	}
	if (!doIt(SubLattice<Float>(aF,mask2), SubLattice<Float>(bF,mask1))) {
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
