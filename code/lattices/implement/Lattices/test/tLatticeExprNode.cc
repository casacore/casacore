//# tLatticeExprNode.cc:  Basic test program for LEL classes
//# Copyright (C) 1997,1998,1999
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

#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LELAttribute.h>
#include <trial/Lattices/LELBinary.h>
#include <trial/Lattices/LELConvert.h>
#include <trial/Lattices/LELFunction.h>
#include <trial/Lattices/LELLattice.h>
#include <trial/Lattices/LELUnary.h>
#include <trial/Lattices/LELArray.h>

#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LCMask.h>
#include <trial/Lattices/LCBox.h>
#include <aips/Lattices/Slicer.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Utilities/DataType.h>

#include <iostream.h>

Bool compareScalarFloat  (const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const Float bFVal,
                          const IPosition shape);
Bool compareScalarDouble (const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const Double bDVal,
                          const IPosition shape);
Bool compareScalarComplex(const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const Complex bCVal,
                          const IPosition shape);
Bool compareScalarDComplex(const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const DComplex bDCVal,
                          const IPosition shape);
Bool compareScalarBool   (const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const Bool bBVal,
                          const IPosition shape);

Bool checkFloat (const LatticeExprNode& expr,
                 const Float result,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
		 const Bool undefinedScalar);

Bool checkDouble (const LatticeExprNode& expr,
                  const Double result,
                  const IPosition& shape,
                  const Bool shouldBeScalar,
		  const Bool undefinedScalar);

Bool checkComplex (const LatticeExprNode& expr,
                   const Complex result,
                   const IPosition& shape,
                   const Bool shouldBeScalar,
		   const Bool undefinedScalar);

Bool checkDComplex (const LatticeExprNode& expr,
                    const DComplex result,
                    const IPosition& shape,
                    const Bool shouldBeScalar,
		    const Bool undefinedScalar);

Bool checkBool (const LatticeExprNode& expr,
                const Bool result,
                const IPosition& shape,
                const Bool shouldBeScalar,
		const Bool undefinedScalar);

Bool checkMask (const LatticeExprNode& expr,
		Bool hasMask,
		const Array<Bool>& result);

Bool doIt (const MaskedLattice<Float>& aF,
	   const MaskedLattice<Float>& bF,
	   const MaskedLattice<Float>& cF,
	   const MaskedLattice<Double>& bD,
	   const MaskedLattice<Double>& cD,
	   const MaskedLattice<Complex>& bC,
	   const MaskedLattice<Complex>& cC,
	   const MaskedLattice<DComplex>& bDC,
	   const MaskedLattice<DComplex>& cDC,
	   const MaskedLattice<Bool>& aB,
	   const MaskedLattice<Bool>& bB,
	   const MaskedLattice<Bool>& cB,
	   Float bFVal, Float cFVal,
	   Double bDVal, Double cDVal,
	   Complex bCVal, Complex cCVal,
	   DComplex bDCVal, DComplex cDCVal,
	   Bool aBVal, Bool bBVal, Bool cBVal,
	   uInt nb)
{

    Bool ok = True;
    IPosition shape = aF.shape();

//
//************************************************************************
//
// Test constructors, get*, eval, shape, dataType, isScalar functions
//
    cout << endl << "Constant contructors, get*, eval, shape, dataType, isScalar" << endl;
    cout << "LatticeExprNode (constant T) " << endl;
    {
      cout << "  Int " << endl;
      Int bIVal = Int(bFVal);
      LatticeExprNode expr(bIVal);
      if (!checkFloat (expr, Float(bIVal), shape, True, False)) ok = False;
    }

    {
      cout << "  Float" << endl;
      LatticeExprNode expr(bFVal);
      if (!checkFloat (expr, bFVal, shape, True, False)) ok = False;
    }

    {
      cout << "  Double" << endl;
      LatticeExprNode expr(bDVal);
      if (!checkDouble(expr, bDVal, shape, True, False)) ok = False;
    }

    {
      cout << "  Complex" << endl;
      LatticeExprNode expr(bCVal);
      if (!checkComplex(expr, bCVal, shape, True, False)) ok = False;
    }

    {
      cout << "  DComplex" << endl;
      LatticeExprNode expr(bDCVal);
      if (!checkDComplex(expr, bDCVal, shape, True, False)) ok = False;
    }
    {
      cout << "  Bool" << endl;
      LatticeExprNode expr(bBVal);
      if (!checkBool(expr, bBVal, shape, True, False)) ok = False;
    }
//
//************************************************************************
//
// Test LELInterface constructors
//
// Assume non-LELInterface constructor gives the right result
// and compare
//

   cout << endl << "LELInterface constructors" << endl;
   cout << "LatticeExprNode(CountedPtr<LELInterface<T> >&) " << endl;
   {
      cout << "  Float" << endl;
      CountedPtr<LELInterface<Float> > pExpr = new LELUnaryConst<Float>(bFVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bFVal);
      if (!compareScalarFloat (expr, expr2, bFVal, shape)) ok = False;
   }
   {
      cout << "  Double" << endl;
      CountedPtr<LELInterface<Double> > pExpr = new LELUnaryConst<Double>(bDVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bDVal);
      if (!compareScalarDouble (expr, expr2, bDVal, shape)) ok = False;
   }
   {
      cout << "  Complex" << endl;
      CountedPtr<LELInterface<Complex> > pExpr = new LELUnaryConst<Complex>(bCVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bCVal);
      if (!compareScalarComplex (expr, expr2, bCVal, shape)) ok = False;
   }
   {
      cout << "  DComplex" << endl;
      CountedPtr<LELInterface<DComplex> > pExpr = new LELUnaryConst<DComplex>(bDCVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bDCVal);
      if (!compareScalarDComplex (expr, expr2, bDCVal, shape)) ok = False;
   }
   {
      cout << "  Bool" << endl;
      CountedPtr<LELInterface<Bool> > pExpr = new LELUnaryConst<Bool>(bBVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bBVal);
      if (!compareScalarBool (expr, expr2, bBVal, shape)) ok = False;
   }


//
//************************************************************************
//
// Test assignment and copy constructor
//

   cout << endl;
   {
      cout << "Copy constructor " << endl;
      LatticeExprNode expr(bDVal);
      LatticeExprNode expr2(expr);
      if (!compareScalarDouble (expr, expr2, bDVal, shape)) ok = False;
   }
   {
      cout << "Assignment " << endl;
      LatticeExprNode expr(bDVal);
      LatticeExprNode expr2; 
      expr2 = expr;
      if (!compareScalarDouble (expr, expr2, bDVal, shape)) ok = False;
   }

//
//************************************************************************
//
// Check unary operators.    
//
    cout << endl << "Unary operator +" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkFloat (expr2, bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkDouble (expr2, bDVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkComplex(expr2, bCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkDComplex(expr2, bDCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = +expr1;
      if (!checkFloat (expr2, bFVal, shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
	  
   }
   {
      cout << "  Double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = +expr1;
      if (!checkDouble (expr2, bDVal, shape, False, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = +expr1;
      if (!checkComplex(expr2, bCVal, shape, False, False)) ok = False;
   }
   {
      cout << "  DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = +expr1;
      if (!checkDComplex(expr2, bDCVal, shape, False, False)) ok = False;
   }
   cout << "Unary operator -" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = -expr1;
      if (!checkFloat (expr2, -bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = -expr1;
      if (!checkComplex (expr2, -bCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = -expr1;
      if (!checkFloat (expr2, -bFVal, shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = -expr1;
      if (!checkComplex (expr2, -bCVal, shape, False, False)) ok = False;
   }
   cout << "Unary operator !" << endl;
   {
      cout << "  Bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2 = !expr1;
      if (!checkBool(expr2, ToBool(!bBVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = !expr1;
      if (!checkBool(expr2, ToBool(!bBVal), shape, False, False)) ok = False;
   }
//
//************************************************************************
//
// Check binary operators.    
//
   cout << endl << "Binary operator +" << endl;
   {
      cout << " Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkFloat (expr3, bFVal+cFVal, shape, True, False)) ok = False;
   }
   {
      cout << " Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDouble(expr3, bDVal+cDVal, shape, True, False)) ok = False;
   }
   {
      cout << " Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkComplex (expr3, bCVal+cCVal, shape, True, False)) ok = False;
   }
   {
      cout << " DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2(cDCVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDComplex (expr3, bDCVal+cDCVal, shape, True, False)) ok = False;
   }
   {
      cout << " Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkFloat (expr3, bFVal+cFVal, shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   {
      cout << " Double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDouble(expr3, bDVal+cDVal, shape, False, False)) ok = False;
   }
   {
      cout << " Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkComplex (expr3, bCVal+cCVal, shape, False, False)) ok = False;
   }
   {
      cout << " DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2(cDC);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDComplex (expr3, bDCVal+cDCVal, shape, False, False)) ok = False;
   }
   cout << "Binary operator -" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkFloat (expr3, bFVal-cFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkComplex (expr3, bCVal-cCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkFloat (expr3, bFVal-cFVal, shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkComplex (expr3, bCVal-cCVal, shape, False, False)) ok = False;
   }
    cout << "Binary operator *" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkFloat (expr3, bFVal*cFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkComplex(expr3, bCVal*cCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkFloat (expr3, bFVal*cFVal, shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkComplex(expr3, bCVal*cCVal, shape, False, False)) ok = False;
   }
   cout << "Binary operator /" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkFloat (expr3, bFVal/cFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkComplex (expr3, bCVal/cCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkFloat (expr3, bFVal/cFVal, shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkComplex (expr3, bCVal/cCVal, shape, False, False)) ok = False;
   }
    cout << "Binary operator ==" << endl;
   {
      cout << "  Bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, ToBool(bBVal==cBVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, True, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, ToBool(bFVal==cFVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, True, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, ToBool(bCVal==cCVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, True, shape, True, False)) ok = False;
   }
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, ToBool(bBVal==cBVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bB.isMasked() || cB.isMasked()),
		      bB.getMask() && cB.getMask())) ok = False;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
      if (!checkMask (expr4, bB.isMasked(),
		      bB.getMask())) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, ToBool(bFVal==cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(bC);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, ToBool(bCVal==bCVal), shape, False, False)) ok = False;
   }
   cout << "Binary operator !=" << endl;
   {
      cout << "  Bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, ToBool(bBVal!=cBVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, False, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, ToBool(bFVal!=cFVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, False, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, ToBool(bCVal!=cCVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, False, shape, True, False)) ok = False;
   }
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, ToBool(bBVal!=cBVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bB.isMasked() || cB.isMasked()),
		      bB.getMask() && cB.getMask())) ok = False;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
      if (!checkMask (expr4, bB.isMasked(),
		      bB.getMask())) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, ToBool(bFVal!=cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, ToBool(bCVal!=cCVal), shape, False, False)) ok = False;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
   }
   cout << "Binary operator >" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, ToBool(bFVal>cFVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, ToBool(bCVal>cCVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, ToBool(bFVal>cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, ToBool(bCVal>cCVal), shape, False, False)) ok = False;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
   }
   cout << "Binary operator >=" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, ToBool(bFVal>=cFVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, ToBool(bCVal>=cCVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, ToBool(bFVal>=cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, ToBool(bCVal>=cCVal), shape, False, False)) ok = False;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
   }
   cout << "Binary operator <" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, ToBool(bFVal<cFVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, ToBool(bCVal<cCVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, ToBool(bFVal<cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, ToBool(bCVal<cCVal), shape, False, False)) ok = False;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, False, shape, False, False)) ok = False;
   }
   cout << "Binary operator <=" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, ToBool(bFVal<=cFVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, ToBool(bCVal<=cCVal), shape, True, False)) ok = False;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, ToBool(bFVal<=cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, ToBool(bCVal<=cCVal), shape, False, False)) ok = False;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, True, shape, False, False)) ok = False;
   }
   cout << "Binary operator &&" << endl;
   {
      cout << "  Bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1&&expr2);
      if (!checkBool (expr3, ToBool(bBVal&&cBVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1&&expr2);
      if (!checkBool (expr3, ToBool(bBVal&&cBVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bB.isMasked() || cB.isMasked()),
		      (!bB.get() && bB.getMask()) ||
		      (!cB.get() && cB.getMask()) ||
		      (bB.getMask() && cB.getMask()))) ok = False;
   }
   cout << "Binary operator ||" << endl;
   {
      cout << "  Bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1||expr2);
      if (!checkBool (expr3, ToBool(bBVal||cBVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1||expr2);
      if (!checkBool (expr3, ToBool(bBVal||cBVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bB.isMasked() || cB.isMasked()),
		      (bB.get() && bB.getMask()) ||
		      (cB.get() && cB.getMask()) ||
		      (bB.getMask() && cB.getMask()))) ok = False;
   }

   cout << "operator []" << endl;
   {
       cout << "  Bool Array" << endl;
       LatticeExprNode expr1(cB);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkBool (expr3, cBVal, shape, False, False)) ok = False;
       if (!checkMask (expr3, True,
		       cB.getMask() && bB.get() && bB.getMask())) ok = False;
   }
   {
       cout << "  Float Array" << endl;
       LatticeExprNode expr1(cF);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkFloat (expr3, cFVal, shape, False, False)) ok = False;
       if (!checkMask (expr3, True,
		       cF.getMask() && bB.get() && bB.getMask())) ok = False;
   }
   {
       cout << "  Double Array" << endl;
       LatticeExprNode expr1(cD);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkDouble (expr3, cDVal, shape, False, False)) ok = False;
       if (!checkMask (expr3, True,
		       cD.getMask() && bB.get() && bB.getMask())) ok = False;
   }
   {
       cout << "  Complex Array" << endl;
       LatticeExprNode expr1(cC);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkComplex (expr3, cCVal, shape, False, False)) ok = False;
       if (!checkMask (expr3, True,
		       cC.getMask() && bB.get() && bB.getMask())) ok = False;
   }
   {
       cout << "  DComplex Array" << endl;
       LatticeExprNode expr1(cDC);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkDComplex (expr3, cDCVal, shape, False, False)) ok = False;
       if (!checkMask (expr3, True,
		       cDC.getMask() && bB.get() && bB.getMask())) ok = False;
   }
//
//************************************************************************
//
// Check 1D functions
//
    cout << endl << "1-argument functions " << endl;
    cout << "sin" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkFloat (expr2, sin(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDouble (expr2, sin(bDVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkComplex(expr2, sin(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDComplex(expr2, sin(bDCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkFloat (expr2, sin(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDouble (expr2, sin(bDVal), shape, False, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkComplex(expr2, sin(bCVal), shape, False, False)) ok = False;
   }
   {
      cout << "  DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDComplex(expr2, sin(bDCVal), shape, False, False)) ok = False;
   }
    cout << "sinh" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkFloat (expr2, sinh(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkComplex(expr2, sinh(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkFloat (expr2, sinh(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkComplex(expr2, sinh(bCVal), shape, False, False)) ok = False;
   }
    cout << "asin" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = asin(expr1);
      if (!checkFloat (expr2, asin(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = asin(expr1);
      if (!checkFloat (expr2, asin(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "cos" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkFloat (expr2, cos(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkComplex(expr2, cos(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkFloat (expr2, cos(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkComplex(expr2, cos(bCVal), shape, False, False)) ok = False;
   }
    cout << "cosh" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkFloat (expr2, cosh(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkComplex(expr2, cosh(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkFloat (expr2, cosh(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkComplex(expr2, cosh(bCVal), shape, False, False)) ok = False;
   }
    cout << "acos" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = acos(expr1);
      if (!checkFloat (expr2, acos(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = acos(expr1);
      if (!checkFloat (expr2, acos(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "tan" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = tan(expr1);
      if (!checkFloat (expr2, tan(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = tan(expr1);
      if (!checkFloat (expr2, tan(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "tanh" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = tanh(expr1);
      if (!checkFloat (expr2, tanh(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = tanh(expr1);
      if (!checkFloat (expr2, tanh(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "atan" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = atan(expr1);
      if (!checkFloat (expr2, atan(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = atan(expr1);
      if (!checkFloat (expr2, atan(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "exp" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkFloat (expr2, exp(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkComplex(expr2, exp(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkFloat (expr2, exp(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkComplex(expr2, exp(bCVal), shape, False, False)) ok = False;
   }
    cout << "log" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = log(expr1);
      if (!checkFloat (expr2, log(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = log(expr1);
      if (!checkComplex(expr2, log(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = log(expr1);
      if (!checkFloat (expr2, log(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = log(expr1);
      if (!checkComplex(expr2, log(bCVal), shape, False, False)) ok = False;
   }
    cout << "log10" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkFloat (expr2, log10(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkComplex(expr2, log10(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkFloat (expr2, log10(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkComplex(expr2, log10(bCVal), shape, False, False)) ok = False;
   }
    cout << "sqrt" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkFloat (expr2, sqrt(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkComplex(expr2, sqrt(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkFloat (expr2, sqrt(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkComplex(expr2, sqrt(bCVal), shape, False, False)) ok = False;
   }
    cout << "ceil" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = ceil(expr1);
      if (!checkFloat (expr2, ceil(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = ceil(expr1);
      if (!checkFloat (expr2, ceil(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "floor" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = floor(expr1);
      if (!checkFloat (expr2, floor(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = floor(expr1);
      if (!checkFloat (expr2, floor(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "conj" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = conj(expr1);
      if (!checkComplex(expr2, conj(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = conj(expr1);
      if (!checkComplex(expr2, conj(bCVal), shape, False, False)) ok = False;
   }
    cout << "complex" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = complex(expr1,expr2);
      if (!checkComplex(expr3, Complex(bFVal,cFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr3 = complex(real(expr1),imag(expr1));
      if (!checkComplex(expr3, bCVal, shape, False, False)) ok = False;
   }
   {
      cout << "  Float Array,Scalar" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr3 = complex(real(expr1),min(imag(expr1)));
      if (!checkComplex(expr3, bCVal, shape, False, False)) ok = False;
   }
   {
      cout << "  Float Scalar,Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr3 = complex(min(real(expr1)),imag(expr1));
      if (!checkComplex(expr3, bCVal, shape, False, False)) ok = False;
   }
    cout << "abs" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat (expr2, abs(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat(expr2, abs(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat (expr2, abs(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat(expr2, abs(bCVal), shape, False, False)) ok = False;
   }
    cout << "arg" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = arg(expr1);
      if (!checkFloat(expr2, arg(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = arg(expr1);
      if (!checkFloat(expr2, arg(bCVal), shape, False, False)) ok = False;
   }
    cout << "real" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = real(expr1);
      if (!checkFloat(expr2, real(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = real(expr1);
      if (!checkFloat(expr2, real(bCVal), shape, False, False)) ok = False;
   }
    cout << "imag" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = imag(expr1);
      if (!checkFloat(expr2, imag(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = imag(expr1);
      if (!checkFloat(expr2, imag(bCVal), shape, False, False)) ok = False;
   }
    cout << "min" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = min(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = min(expr1);
      if (!checkComplex(expr2, bCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = min(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, ToBool(nb==0))) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = min(expr1);
      if (!checkComplex(expr2, bCVal, shape, True, ToBool(nb==0))) ok = False;
   }
    cout << "max" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = max(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = max(expr1);
      if (!checkComplex(expr2, bCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = max(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, ToBool(nb==0))) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = max(expr1);
      if (!checkComplex(expr2, bCVal, shape, True, ToBool(nb==0))) ok = False;
   }
    cout << "mean" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkComplex(expr2, bCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, ToBool(nb==0))) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkComplex(expr2, bCVal, shape, True, ToBool(nb==0))) ok = False;
   }
    cout << "sum" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkComplex(expr2, bCVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkFloat (expr2, nb*bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkComplex(expr2, nb*bCVal, shape, True, False)) ok = False;
   }
    cout << "nelements" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, 1.0, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, 1.0, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, Double(nb), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, Double(nb), shape, True, False)) ok = False;
   }
    cout << "length" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = length(expr1,Float(1));
      if (!checkFloat(expr2, 1.0, shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = length(expr1,Double(1));
      if (!checkFloat(expr2, 1.0, shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = length(expr1,1);
      if (!checkFloat(expr2, shape(0), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = length(expr1,2);
      if (!checkFloat(expr2, shape(1), shape, True, False)) ok = False;
   }
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = length(expr1,3);
      if (!checkFloat(expr2, 1, shape, True, False)) ok = False;
   }
    cout << "any" << endl;
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = any(expr1);
      if (!checkBool(expr2, ToBool(nb>0 && bBVal), shape, True, False)) ok = False;
   }
    cout << "all" << endl;
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = all(expr1);
      if (!checkBool(expr2, ToBool(nb==0 || bBVal), shape, True, False)) ok = False;
   }
   cout << "ntrue" << endl;
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = ntrue(expr1);
      Double result;
      if (bBVal) {
         result = nb;
      } else {
         result = 0.0;
      }
      if (!checkDouble(expr2, result, shape, True, False)) ok = False;
   }
   cout << "nfalse" << endl;
   {
      cout << "  Bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = nfalse(expr1);
      Double result;
      if (!bBVal) {
         result = nb;
      } else {
         result = 0.0;
      }
      if (!checkDouble(expr2, result, shape, True, False)) ok = False;
   }


//
//************************************************************************
//
// Check 2D functions
//

    cout << endl << "2-argument functions " << endl;    
    cout << "atan2" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = atan2(expr1,expr2);
      if (!checkFloat (expr3, atan2(bFVal,cFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = atan2(expr1,expr2);
      if (!checkFloat (expr3, atan2(bFVal,cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
    cout << "pow" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkFloat (expr3, pow(bFVal,cFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkComplex(expr3, pow(bCVal,cCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkFloat (expr3, pow(bFVal,cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkComplex(expr3, pow(bCVal,cCVal), shape, False, False)) ok = False;
   }
    cout << "fmod" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = fmod(expr1,expr2);
      if (!checkFloat (expr3, fmod(bFVal,cFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = fmod(expr1,expr2);
      if (!checkFloat (expr3, fmod(bFVal,cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
    cout << "min" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = min(expr1,expr2);
      if (!checkFloat (expr3, min(bFVal,cFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = min(expr1,expr2);
      if (!checkFloat (expr3, min(bFVal,cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
    cout << "max" << endl;
   {
      cout << "  Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = max(expr1,expr2);
      if (!checkFloat (expr3, max(bFVal,cFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = max(expr1,expr2);
      if (!checkFloat (expr3, max(bFVal,cFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   cout << "amp" << endl;
   {
      cout << " Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = amp(expr1,expr2);
      Float result = sqrt(bFVal*bFVal+cFVal*cFVal);
      if (!checkFloat (expr3, result, shape, True, False)) ok = False;
   }
   {
      cout << " Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = amp(expr1,expr2);
      Complex result = sqrt(bCVal*bCVal+cCVal*cCVal);
      if (!checkComplex (expr3, result, shape, True, False)) ok = False;
   }
   {
      cout << " Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = amp(expr1,expr2);
      Float result = sqrt(bFVal*bFVal+cFVal*cFVal);
      if (!checkFloat (expr3, result, shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   {
      cout << " Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = amp(expr1,expr2);
      Complex result = sqrt(bCVal*bCVal+cCVal*cCVal);
      if (!checkComplex (expr3, result, shape, False, False)) ok = False;
   }
   cout << "pa" << endl;
   {
      cout << " Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = pa(expr1,expr2);
      Float result = 90.0/C::pi*atan2(bFVal,cFVal);
      if (!checkFloat (expr3, result, shape, True, False)) ok = False;
   }
   {
      cout << " Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = pa(expr1,expr2);
      Float result = 90.0/C::pi*atan2(bFVal,cFVal);
      if (!checkFloat (expr3, result, shape, False, False)) ok = False;
      if (!checkMask (expr3, ToBool(bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = False;
   }
   {
      cout << " Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = pa(expr1,expr2);
      Double result = 90.0/C::pi*atan2(bDVal,cDVal);
      if (!checkDouble (expr3, result, shape, True, False)) ok = False;
   }
   {
      cout << " Double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = pa(expr1,expr2);
      Double result = 90.0/C::pi*atan2(bDVal,cDVal);
      if (!checkDouble (expr3, result, shape, False, False)) ok = False;
   }


//
//************************************************************************
//
// Check 3D functions
//

    cout << endl << "3-argument functions " << endl;    
    cout << "iif" << endl;
   {
      cout << "  Float Scalar,Scalar,Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkFloat (expr3, bFVal, shape, True, False)) ok = False;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkFloat (expr4, cFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  Double Scalar,Array,Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, False, False)) ok = False;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, False, False)) ok = False;
   }
   {
      cout << "  Complex Scalar,Scalar,Array" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkComplex (expr3, bCVal, shape, False, False)) ok = False;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkComplex (expr4, cCVal, shape, False, False)) ok = False;
   }
   {
      cout << "  DComplex Scalar,Array,Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2(cDC);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkDComplex (expr3, bDCVal, shape, False, False)) ok = False;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkDComplex (expr4, cDCVal, shape, False, False)) ok = False;
   }
   {
      cout << "  Float/Double Array,Scalar,Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, False, False)) ok = False;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, False, False)) ok = False;
   }
   {
      cout << "  Complex/Double Array,Scalar,Array" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDComplex (expr3, bDCVal, shape, False, False)) ok = False;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDComplex (expr4, DComplex(cDVal,0), shape, False, False)) ok = False;
   }
   {
      cout << "  Double Array,Array,Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, False, False)) ok = False;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, False, False)) ok = False;
   }
   {
      cout << "  Double Array,Array,Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, False, False)) ok = False;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, False, False)) ok = False;
   }


//
//************************************************************************
//
// Test conversion functions.  The to* functions call
// the public members makeFloat, makeDouble, makeComplex, 
// and makeDComplex so we don't have to test them again 
// explicitly
//

    cout << endl << "Conversion functions" << endl;
    cout << "toFloat" << endl;
   {
      cout << "  from Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, bFVal, shape, True, False)) ok = False;
   }
   {
      cout << "  from Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, Float(bDVal), shape, True, False)) ok = False;
   }
   {
      cout << "  from Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, bFVal, shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  from Double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, Float(bDVal), shape, False, False)) ok = False;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
    cout << "toDouble" << endl;
   {
      cout << "  from Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, Double(bFVal), shape, True, False)) ok = False;
   }
   {
      cout << "  from Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, bDVal, shape, True, False)) ok = False;
   }
   {
      cout << "  from Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, Double(bFVal), shape, False, False)) ok = False;
   }
   {
      cout << "  from Double Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, bDVal, shape, False, False)) ok = False;
   }
    cout << "toComplex" << endl;
   {
      cout << "  from Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bFVal,0.0), shape, True, False)) ok = False;
   }
   {
      cout << "  from Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDVal,0.0), shape, True, False)) ok = False;
   }
   {
      cout << "  from Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  from DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  from Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bFVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from Double Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, bCVal, shape, False, False)) ok = False;
   }
   {
      cout << "  from DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDCVal), shape, False, False)) ok = False;
   }
    cout << "toDComplex" << endl;
   {
      cout << "  from Float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bFVal,0.0), shape, True, False)) ok = False;
   }
   {
      cout << "  from Double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDVal,0.0), shape, True, False)) ok = False;
   }
   {
      cout << "  from Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      DComplex result;
      result = bCVal;
      if (!checkDComplex(expr2, result, shape, True, False)) ok = False;
   }
   {
      cout << "  from DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDCVal), shape, True, False)) ok = False;
   }
   {
      cout << "  from Float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bFVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from Double Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from DComplex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = toDComplex(expr1);
      DComplex result;
      result = bCVal;
      if (!checkDComplex(expr2, result, shape, False, False)) ok = False;
   }
   {
      cout << "  from DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDCVal), shape, False, False)) ok = False;
   }

//
//************************************************************************
//
// Check casting functions.  These return a LatticeExpr<T>.  Now
// since a LatticeExpr isA Lattice, we can use the LatticeExprNode(Lattice<T>)
// constrcutors to convert it back to a LatticeExprNode and hence test it for validity
// However, this never returns a scalar so we can't test scalar functionality
//

   cout << endl << "Casting operators" << endl;
   cout << "LatticeExpr<Float>()" << endl;
   {
      cout << "  from Float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<Float> expr2 = (LatticeExpr<Float>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkFloat (expr3, Float(bFVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   {
      cout << "  from Double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<Float> expr2 = (LatticeExpr<Float>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkFloat (expr3, Float(bDVal), shape, False, False)) ok = False;
      if (!checkMask (expr3, bF.isMasked(),
		      bF.getMask())) ok = False;
   }
   cout << "LatticeExpr<Double>()" << endl;
   {
      cout << "  from Float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<Double> expr2 = (LatticeExpr<Double>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDouble(expr3, Double(bFVal), shape, False, False)) ok = False;
   }
   {
      cout << "  from Double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<Double> expr2 = (LatticeExpr<Double>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDouble(expr3, Double(bDVal), shape, False, False)) ok = False;
   }
   cout << "LatticeExpr<Complex>()" << endl;
   {
      cout << "  from Float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bFVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from Double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bDVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from Complex" << endl;
      LatticeExprNode expr1(bC);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bCVal), shape, False, False)) ok = False;
   }
   {
      cout << "  from DComplex" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bDCVal), shape, False, False)) ok = False;
   }
   cout << "LatticeExpr<DComplex>()" << endl;
   {
      cout << "  from Float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDComplex(expr3, DComplex(bFVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from Double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDComplex(expr3, DComplex(bDVal,0.0), shape, False, False)) ok = False;
   }
   {
      cout << "  from Complex" << endl;
      LatticeExprNode expr1(bC);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      DComplex result;
      result = bCVal;
      if (!checkDComplex(expr3, result, shape, False, False)) ok = False;
   }
   {
      cout << "  from DComplex" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDComplex(expr3, DComplex(bDCVal), shape, False, False)) ok = False;
   }
   cout << "LatticeExpr<Bool>()" << endl;
   {
      cout << "  from Bool" << endl;
      LatticeExprNode expr1(bB);
      LatticeExpr<Bool> expr2 = (LatticeExpr<Bool>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkBool(expr3, bBVal, shape, False, False)) ok = False;
   }

   return ok;
}



Bool compareScalarFloat (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const Float bVal,
                         const IPosition shape)
{
      LELArray<Float> Arr(shape);
      LELArray<Float> Arr2(shape);
      // Test LELArray copy constructor and assignment.
      LELArray<Float> Arrt(Arr);
      Arrt = Arr2;
      Float result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      Bool ok = True;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = False;
      }
      if (expr.getFloat() != expr2.getFloat()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getFloat() 
              << ", " << expr2.getFloat() << endl;
         ok = False;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = False;
      }
      if (expr.dataType() != expr2.dataType()) {
         cout << "   Data type should be " << TpFloat << endl;
         cout << "   Data types are      " << expr.dataType() 
              << ", " << expr2.dataType() << endl;
      }
      if (expr.shape() != expr2.shape()) {
         cout << "   Shape should be " << shape << endl;
         cout << "   Shapes are      " << expr.shape() 
              << ", " << expr2.shape() << endl;
         ok = False;
      }
      return ok;
}

Bool compareScalarDouble (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const Double bVal,
                         const IPosition shape)
{
      LELArray<Double> Arr(shape);
      LELArray<Double> Arr2(shape);
      Double result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      Bool ok = True;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = False;
      }
      if (expr.getDouble() != expr2.getDouble()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getDouble() 
              << ", " << expr2.getDouble() << endl;
         ok = False;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = False;
      }
      if (expr.dataType() != expr2.dataType()) {
         cout << "   Data type should be " << TpDouble << endl;
         cout << "   Data types are      " << expr.dataType() 
              << ", " << expr2.dataType() << endl;
      }
      if (expr.shape() != expr2.shape()) {
         cout << "   Shape should be " << shape << endl;
         cout << "   Shapes are      " << expr.shape() 
              << ", " << expr2.shape() << endl;
         ok = False;
      }
      return ok;
}


Bool compareScalarComplex (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const Complex bVal,
                         const IPosition shape)
{
      LELArray<Complex> Arr(shape);
      LELArray<Complex> Arr2(shape);
      Complex result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      Bool ok = True;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = False;
      }
      if (expr.getComplex() != expr2.getComplex()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getComplex() 
              << ", " << expr2.getComplex() << endl;
         ok = False;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = False;
      }
      if (expr.dataType() != expr2.dataType()) {
         cout << "   Data type should be " << TpComplex << endl;
         cout << "   Data types are      " << expr.dataType() 
              << ", " << expr2.dataType() << endl;
      }
      if (expr.shape() != expr2.shape()) {
         cout << "   Shape should be " << shape << endl;
         cout << "   Shapes are      " << expr.shape() 
              << ", " << expr2.shape() << endl;
         ok = False;
      }
      return ok;
}


Bool compareScalarDComplex (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const DComplex bVal,
                         const IPosition shape)
{
      LELArray<DComplex> Arr(shape);
      LELArray<DComplex> Arr2(shape);
      DComplex result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      Bool ok = True;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = False;
      }
      if (expr.getDComplex() != expr2.getDComplex()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getDComplex() 
              << ", " << expr2.getDComplex() << endl;
         ok = False;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = False;
      }
      if (expr.dataType() != expr2.dataType()) {
         cout << "   Data type should be " << TpDComplex << endl;
         cout << "   Data types are      " << expr.dataType() 
              << ", " << expr2.dataType() << endl;
      }
      if (expr.shape() != expr2.shape()) {
         cout << "   Shape should be " << shape << endl;
         cout << "   Shapes are      " << expr.shape() 
              << ", " << expr2.shape() << endl;
         ok = False;
      }
      return ok;
}




Bool compareScalarBool (const LatticeExprNode expr,
			const LatticeExprNode expr2,
			const Bool bVal,
			const IPosition shape)
{
      LELArray<Bool> Arr(shape);
      LELArray<Bool> Arr2(shape);
      Bool result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      Bool ok = True;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = False;
      }
      if (expr.getBool() != expr2.getBool()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getBool() 
              << ", " << expr2.getBool() << endl;
         ok = False;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = False;
      }
      if (expr.dataType() != expr2.dataType()) {
         cout << "   Data type should be " << TpBool << endl;
         cout << "   Data types are      " << expr.dataType() 
              << ", " << expr2.dataType() << endl;
      }
      if (expr.shape() != expr2.shape()) {
         cout << "   Shape should be " << shape << endl;
         cout << "   Shapes are      " << expr.shape() 
              << ", " << expr2.shape() << endl;
         ok = False;
      }
      return ok;
}




Bool checkFloat (const LatticeExprNode& expr,
                 const Float result,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
		 const Bool undefinedScalar)
{
    Bool ok = True;  
    Float result2;
    LELArray<Float> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = False;
    }
    if (!shouldBeScalar && expr.isScalar()) {
       cout << "   result should be array" << endl;
       ok = False;
    }
    if (expr.dataType() != TpFloat) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpFloat << endl;
       ok = False;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getFloat();
	if (result2 != result) {
	  cout << "   result should be " << result << endl;
	  cout << "   Scalar result is " << result2  << endl;
	  ok = False;
	}
	expr.eval(result2);
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = False;
	}
      }
      expr.eval(Arr, region);
      if (! allEQ (Arr.value(), result)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = False;
      }
    }
    return ok;
}


Bool checkDouble (const LatticeExprNode& expr,
		  const Double result,
		  const IPosition& shape,
		  const Bool shouldBeScalar,
		  const Bool undefinedScalar)
{
    Bool ok = True;  
    Double result2;
    LELArray<Double> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = False;
    }
    if (expr.dataType() != TpDouble) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpDouble << endl;
       ok = False;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getDouble();
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2  << endl;
          ok = False;
	}
	expr.eval(result2);
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = False;
	}
      }
      expr.eval(Arr, region);
      if (! allEQ (Arr.value(), result)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = False;
      }
    }
    return ok;
}


Bool checkComplex (const LatticeExprNode& expr,
		   const Complex result,
		   const IPosition& shape,
		   const Bool shouldBeScalar,
		   const Bool undefinedScalar)
{
    Bool ok = True;  
    Complex result2;
    LELArray<Complex> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = False;
    }
    if (expr.dataType() != TpComplex) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpComplex << endl;
       ok = False;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getComplex();
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2  << endl;
          ok = False;
	}
	expr.eval(result2);
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = False;
	}
      }
      expr.eval(Arr, region);
      if (! allEQ (Arr.value(), result)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = False;
      }
    }
    return ok;
}


Bool checkDComplex (const LatticeExprNode& expr,
		    const DComplex result,
		    const IPosition& shape,
		    const Bool shouldBeScalar,
		    const Bool undefinedScalar)
{
    Bool ok = True;  
    DComplex result2;
    LELArray<DComplex> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = False;
    }
    if (expr.dataType() != TpDComplex) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpDComplex << endl;
       ok = False;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getDComplex();
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2  << endl;
          ok = False;
	}
	expr.eval(result2);
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = False;
	}
      }
      expr.eval(Arr, region);
      if (! allEQ (Arr.value(), result)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = False;
      }
    }
    return ok;
}



Bool checkBool (const LatticeExprNode& expr,
		const Bool result,
		const IPosition& shape,
		const Bool shouldBeScalar,
		const Bool undefinedScalar)
{
    Bool ok = True;  
    Bool result2;
    LELArray<Bool> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = False;
    }
    if (expr.dataType() != TpBool) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpBool << endl;
       ok = False;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = False;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getBool();
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2  << endl;
          ok = False;
	}
	expr.eval(result2);
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = False;
	}
      }
      expr.eval(Arr, region);
      if (! allEQ (Arr.value(), result)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = False;
      }
    }
    return ok;
}

Bool checkMask (const LatticeExprNode& expr,
		Bool hasMask,
		const Array<Bool>& result)
{
    if (hasMask != expr.isMasked()) {
        cout << "   expr should have " << hasMask << " a mask" << endl;
        return False;
    }
    IPosition origin(result.shape()); origin = 0;
    Slicer region(origin, result.shape());
    Array<Bool> mask;
    Bool isMasked;
    switch (expr.dataType()) {
    case TpBool:
      {
        LELArray<Bool> arr(result.shape());
	expr.eval (arr, region);
	isMasked = arr.isMasked();
	if (isMasked) mask = arr.mask();
	break;
      }
    case TpFloat:
      {
        LELArray<Float> arr(result.shape());
	expr.eval (arr, region);
	isMasked = arr.isMasked();
	if (isMasked) mask = arr.mask();
	break;
      }
    case TpDouble:
      {
        LELArray<Double> arr(result.shape());
	expr.eval (arr, region);
	isMasked = arr.isMasked();
	if (isMasked) mask = arr.mask();
	break;
      }
    case TpComplex:
      {
        LELArray<Complex> arr(result.shape());
	expr.eval (arr, region);
	isMasked = arr.isMasked();
	if (isMasked) mask = arr.mask();
	break;
      }
    case TpDComplex:
      {
        LELArray<DComplex> arr(result.shape());
	expr.eval (arr, region);
	isMasked = arr.isMasked();
	if (isMasked) mask = arr.mask();
	break;
      }
    default:
        cout << "Unknown data type in checkMask" << endl;
	return False;
    }
    if (hasMask != isMasked) {
        cout << "   result should have " << hasMask << " a mask" << endl;
        return False;
    }
    if (hasMask) {
        if (anyNE (result, mask)) {
	    cout << "   result should have mask " << result << endl;
	    cout << "              but has mask " << mask << endl;
	    return False;
	}
    }
    return True;
}


main (int argc, char *argv[])
{
 try {
    Input inp(1);
    inp.Version(" ");
    inp.Create("nx", "2", "Number of pixels along the x-axis", "int");
    inp.Create("ny", "2", "Number of pixels along the y-axis", "int");
    inp.ReadArguments(argc, argv);

    const uInt nx=inp.GetInt("nx");
    const uInt ny=inp.GetInt("ny");

//
// The use of these tiny ArrayLattices means this test program
// does not computationally stress the classes.  Here we just
// test them logically.  See other test programs for stress tests
//

    IPosition shape(2,nx,ny);

// Bool Lattices

    ArrayLattice<Bool> aB(shape);
    ArrayLattice<Bool> bB(shape);
    ArrayLattice<Bool> cB(shape);
    Bool aBVal = True;
    aB.set(aBVal);
    Bool bBVal = False;
    bB.set(bBVal);
    Bool cBVal = True;
    cB.set(cBVal);



// FLoat Lattices

    ArrayLattice<Float> aF(shape);
    ArrayLattice<Float> bF(shape);
    ArrayLattice<Float> cF(shape);
    Float aFVal = 0.0;
    aF.set(aFVal);
    Float bFVal = 1.0;
    bF.set(1.0);
    Float cFVal = 2.0;
    cF.set(cFVal);


// Double Lattices

    ArrayLattice<Double> aD(shape);
    ArrayLattice<Double> bD(shape);
    ArrayLattice<Double> cD(shape);
    Double aDVal = 0.0;
    aD.set(aDVal);
    Double bDVal = 1.0;
    bD.set(1.0);
    Double cDVal = 2.0;
    cD.set(cDVal);


// Complex Lattices

    ArrayLattice<Complex> aC(shape);
    ArrayLattice<Complex> bC(shape);
    ArrayLattice<Complex> cC(shape);
    ArrayLattice<Complex> dC(shape);
    Complex aCVal = Complex(0.0,10.0);
    aC.set(aCVal);
    Complex bCVal = Complex(1.0,21.0);
    bC.set(bCVal);
    Complex cCVal = Complex(2.0,32.0);
    cC.set(cCVal);
    Complex dCVal = Complex(3.0,43.0);
    dC.set(dCVal);


// DComplex Lattices

    ArrayLattice<DComplex> aDC(shape);
    ArrayLattice<DComplex> bDC(shape);
    ArrayLattice<DComplex> cDC(shape);
    ArrayLattice<DComplex> dDC(shape);
    DComplex aDCVal = DComplex(0.0,10.0);
    aDC.set(aDCVal);
    DComplex bDCVal = DComplex(1.0,21.0);
    bDC.set(bDCVal);
    DComplex cDCVal = DComplex(2.0,32.0);
    cDC.set(cDCVal);
    DComplex dDCVal = DComplex(3.0,43.0);
    dDC.set(dDCVal);

    // Define some array masks.
    Matrix<Bool> mat1(shape);
    Matrix<Bool> mat2(shape);
    Matrix<Bool> mat3(shape);
    mat1 = True;
    mat2 = True;
    mat3 = True;
    mat1(0,0) = False;
    mat2(1,0) = False;
    mat3(0,1) = False;
    LCBox box(shape);
    LCMask mask1 (mat1, box);
    LCMask mask2 (mat2, box);
    LCMask mask3 (mat3, box);
    
    Bool ok = True;
    if (!doIt (SubLattice<Float>(aF),
	       SubLattice<Float>(bF),
	       SubLattice<Float>(cF),
	       SubLattice<Double>(bD),
	       SubLattice<Double>(cD),
	       SubLattice<Complex>(bC),
	       SubLattice<Complex>(cC),
	       SubLattice<DComplex>(bDC),
	       SubLattice<DComplex>(cDC),
	       SubLattice<Bool>(aB),
	       SubLattice<Bool>(bB),
	       SubLattice<Bool>(cB),
	       bFVal,cFVal,bDVal,cDVal,bCVal,cCVal,
	       bDCVal,cDCVal,aBVal,bBVal,cBVal,
	       4)) {
	ok = False;
    }
    if (!doIt (SubLattice<Float>(aF,mask1),
	       SubLattice<Float>(bF,mask2),
	       SubLattice<Float>(cF,mask3),
	       SubLattice<Double>(bD,mask2),
	       SubLattice<Double>(cD,mask3),
	       SubLattice<Complex>(bC,mask2),
	       SubLattice<Complex>(cC,mask3),
	       SubLattice<DComplex>(bDC,mask2),
	       SubLattice<DComplex>(cDC,mask3),
	       SubLattice<Bool>(aB,mask1),
	       SubLattice<Bool>(bB,mask2),
	       SubLattice<Bool>(cB,mask3),
	       bFVal,cFVal,bDVal,cDVal,bCVal,cCVal,
	       bDCVal,cDCVal,aBVal,bBVal,cBVal,
	       3)) {
	ok = False;
    }
    mat2 = False;
    mask2 = LCMask (mat2, box);
    if (!doIt (SubLattice<Float>(aF,mask1),
	       SubLattice<Float>(bF,mask2),
	       SubLattice<Float>(cF,mask3),
	       SubLattice<Double>(bD,mask2),
	       SubLattice<Double>(cD,mask3),
	       SubLattice<Complex>(bC,mask2),
	       SubLattice<Complex>(cC,mask3),
	       SubLattice<DComplex>(bDC,mask2),
	       SubLattice<DComplex>(cDC,mask3),
	       SubLattice<Bool>(aB,mask1),
	       SubLattice<Bool>(bB,mask2),
	       SubLattice<Bool>(cB,mask3),
	       bFVal,cFVal,bDVal,cDVal,bCVal,cCVal,
	       bDCVal,cDCVal,aBVal,bBVal,cBVal,
	       0)) {
	ok = False;
    }

    if (ok) {
	cout << endl << "ok" << endl;
    } else {
	exit(1);
    }

 } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    return 1;
 } end_try;
 
 exit(0);
}
