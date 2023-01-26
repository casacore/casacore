//# tLatticeExprNode.cc:  Basic test program for LEL classes
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LEL/LELAttribute.h>
#include <casacore/lattices/LEL/LELBinary.h>
#include <casacore/lattices/LEL/LELConvert.h>
#include <casacore/lattices/LEL/LELFunction.h>
#include <casacore/lattices/LEL/LELLattice.h>
#include <casacore/lattices/LEL/LELUnary.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/Lattices/RebinLattice.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/casa/Arrays/Slicer.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/DataType.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
bool checkInfo (const LatticeExprNode& expr,
		const IPosition& shape,
		const bool shouldBeScalar,
		const bool undefinedScalar,
		const DataType dtype,
		const bool emptyShape=false);
bool compareScalarFloat  (const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const float bFVal,
                          const IPosition shape);
bool compareScalarDouble (const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const double bDVal,
                          const IPosition shape);
bool compareScalarComplex(const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const Complex bCVal,
                          const IPosition shape);
bool compareScalarDComplex(const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const DComplex bDCVal,
                          const IPosition shape);
bool compareScalarBool   (const LatticeExprNode expr,
                          const LatticeExprNode expr2,
                          const bool bBVal,
                          const IPosition shape);

bool checkFloat (const LatticeExprNode& expr,
                 const float result,
                 const IPosition& shape,
                 const bool shouldBeScalar,
		 const bool undefinedScalar);
bool checkFloatRepl (const LatticeExprNode& expr,
		     const float result,
		     const IPosition& shape,
		     const Array<float>& replArray,
		     float replScalar,
		     bool isReplScalar);

bool checkDouble (const LatticeExprNode& expr,
                  const double result,
                  const IPosition& shape,
                  const bool shouldBeScalar,
		  const bool undefinedScalar);

bool checkComplex (const LatticeExprNode& expr,
                   const Complex result,
                   const IPosition& shape,
                   const bool shouldBeScalar,
		   const bool undefinedScalar);

bool checkDComplex (const LatticeExprNode& expr,
                    const DComplex result,
                    const IPosition& shape,
                    const bool shouldBeScalar,
		    const bool undefinedScalar);

bool checkBool (const LatticeExprNode& expr,
                const bool result,
                const IPosition& shape,
                const bool shouldBeScalar,
		const bool undefinedScalar,
		const bool emptyShape=false);
bool checkBoolRepl (const LatticeExprNode& expr,
		    const bool result,
		    const IPosition& shape,
		    const Array<bool>& replArray,
		    bool replScalar,
		    bool isReplScalar);


bool checkMask (const LatticeExprNode& expr,
		bool hasMask,
		const Array<bool>& result);

bool doIt (const MaskedLattice<float>& aF,
	   const MaskedLattice<float>& bF,
	   const MaskedLattice<float>& cF,
	   const MaskedLattice<double>& bD,
	   const MaskedLattice<double>& cD,
	   const MaskedLattice<Complex>& bC,
	   const MaskedLattice<Complex>& cC,
	   const MaskedLattice<DComplex>& bDC,
	   const MaskedLattice<DComplex>& cDC,
	   const MaskedLattice<bool>& aB,
	   const MaskedLattice<bool>& bB,
	   const MaskedLattice<bool>& cB,
	   float bFVal, float cFVal,
	   double bDVal, double cDVal,
	   Complex bCVal, Complex cCVal,
	   DComplex bDCVal, DComplex cDCVal,
	   bool aBVal, bool bBVal, bool cBVal,
	   uint32_t nb)
{

    bool ok = true;
    IPosition shape = aF.shape();

//
//************************************************************************
//
// Test constructors, get*, eval, shape, dataType, isScalar functions
//
    cout << endl << "Constant contructors, get*, eval, shape, dataType, isScalar" << endl;
    cout << "LatticeExprNode (constant T) " << endl;
    {
      cout << "  int32_t " << endl;
      int32_t bIVal = int32_t(bFVal);
      LatticeExprNode expr(bIVal);
      if (!checkFloat (expr, float(bIVal), shape, true, false)) ok = false;
    }

    {
      cout << "  float" << endl;
      LatticeExprNode expr(bFVal);
      if (!checkFloat (expr, bFVal, shape, true, false)) ok = false;
    }

    {
      cout << "  double" << endl;
      LatticeExprNode expr(bDVal);
      if (!checkDouble(expr, bDVal, shape, true, false)) ok = false;
    }

    {
      cout << "  Complex" << endl;
      LatticeExprNode expr(bCVal);
      if (!checkComplex(expr, bCVal, shape, true, false)) ok = false;
    }

    {
      cout << "  DComplex" << endl;
      LatticeExprNode expr(bDCVal);
      if (!checkDComplex(expr, bDCVal, shape, true, false)) ok = false;
    }
    {
      cout << "  bool" << endl;
      LatticeExprNode expr(bBVal);
      if (!checkBool(expr, bBVal, shape, true, false)) ok = false;
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
      cout << "  float" << endl;
      CountedPtr<LELInterface<float> > pExpr = new LELUnaryConst<float>(bFVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bFVal);
      if (!compareScalarFloat (expr, expr2, bFVal, shape)) ok = false;
   }
   {
      cout << "  double" << endl;
      CountedPtr<LELInterface<double> > pExpr = new LELUnaryConst<double>(bDVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bDVal);
      if (!compareScalarDouble (expr, expr2, bDVal, shape)) ok = false;
   }
   {
      cout << "  Complex" << endl;
      CountedPtr<LELInterface<Complex> > pExpr = new LELUnaryConst<Complex>(bCVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bCVal);
      if (!compareScalarComplex (expr, expr2, bCVal, shape)) ok = false;
   }
   {
      cout << "  DComplex" << endl;
      CountedPtr<LELInterface<DComplex> > pExpr = new LELUnaryConst<DComplex>(bDCVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bDCVal);
      if (!compareScalarDComplex (expr, expr2, bDCVal, shape)) ok = false;
   }
   {
      cout << "  bool" << endl;
      CountedPtr<LELInterface<bool> > pExpr = new LELUnaryConst<bool>(bBVal);
      LatticeExprNode expr(pExpr);
      LatticeExprNode expr2(bBVal);
      if (!compareScalarBool (expr, expr2, bBVal, shape)) ok = false;
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
      if (!compareScalarDouble (expr, expr2, bDVal, shape)) ok = false;
   }
   {
      cout << "Assignment " << endl;
      LatticeExprNode expr(bDVal);
      LatticeExprNode expr2; 
      expr2 = expr;
      if (!compareScalarDouble (expr, expr2, bDVal, shape)) ok = false;
   }

//
//************************************************************************
//
// Check unary operators.    
//
    cout << endl << "Unary operator +" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkDouble (expr2, bDVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkComplex(expr2, bCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = +expr1;
      if (!checkDComplex(expr2, bDCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = +expr1;
      if (!checkFloat (expr2, bFVal, shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
	  
   }
   {
      cout << "  double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = +expr1;
      if (!checkDouble (expr2, bDVal, shape, false, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = +expr1;
      if (!checkComplex(expr2, bCVal, shape, false, false)) ok = false;
   }
   {
      cout << "  DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = +expr1;
      if (!checkDComplex(expr2, bDCVal, shape, false, false)) ok = false;
   }
   cout << "Unary operator -" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = -expr1;
      if (!checkFloat (expr2, -bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = -expr1;
      if (!checkComplex (expr2, -bCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = -expr1;
      if (!checkFloat (expr2, -bFVal, shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = -expr1;
      if (!checkComplex (expr2, -bCVal, shape, false, false)) ok = false;
   }
   cout << "Unary operator !" << endl;
   {
      cout << "  bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2 = !expr1;
      if (!checkBool(expr2, (!bBVal), shape, true, false)) ok = false;
   }
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = !expr1;
      if (!checkBool(expr2, (!bBVal), shape, false, false)) ok = false;
   }
//
//************************************************************************
//
// Check binary operators.    
//
   cout << endl << "Binary operator +" << endl;
   {
      cout << " float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkFloat (expr3, bFVal+cFVal, shape, true, false)) ok = false;
   }
   {
      cout << " double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDouble(expr3, bDVal+cDVal, shape, true, false)) ok = false;
   }
   {
      cout << " Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkComplex (expr3, bCVal+cCVal, shape, true, false)) ok = false;
   }
   {
      cout << " DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2(cDCVal);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDComplex (expr3, bDCVal+cDCVal, shape, true, false)) ok = false;
   }
   {
      cout << " float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkFloat (expr3, bFVal+cFVal, shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   {
      cout << " double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDouble(expr3, bDVal+cDVal, shape, false, false)) ok = false;
   }
   {
      cout << " Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkComplex (expr3, bCVal+cCVal, shape, false, false)) ok = false;
   }
   {
      cout << " DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2(cDC);
      LatticeExprNode expr3 = expr1+expr2;
      if (!checkDComplex (expr3, bDCVal+cDCVal, shape, false, false)) ok = false;
   }
   cout << "Binary operator -" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkFloat (expr3, bFVal-cFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkComplex (expr3, bCVal-cCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkFloat (expr3, bFVal-cFVal, shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1-expr2;
      if (!checkComplex (expr3, bCVal-cCVal, shape, false, false)) ok = false;
   }
    cout << "Binary operator *" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkFloat (expr3, bFVal*cFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkComplex(expr3, bCVal*cCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkFloat (expr3, bFVal*cFVal, shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1*expr2;
      if (!checkComplex(expr3, bCVal*cCVal, shape, false, false)) ok = false;
   }
   cout << "Binary operator /" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkFloat (expr3, bFVal/cFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkComplex (expr3, bCVal/cCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkFloat (expr3, bFVal/cFVal, shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = expr1/expr2;
      if (!checkComplex (expr3, bCVal/cCVal, shape, false, false)) ok = false;
   }
    cout << "Binary operator ==" << endl;
   {
      cout << "  bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, (bBVal==cBVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, true, shape, true, false)) ok = false;
   }
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, (bFVal==cFVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, true, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, (bCVal==cCVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, true, shape, true, false)) ok = false;
   }
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, (bBVal==cBVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bB.isMasked() || cB.isMasked()),
		      bB.getMask() && cB.getMask())) ok = false;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, true, shape, false, false)) ok = false;
      if (!checkMask (expr4, bB.isMasked(),
		      bB.getMask())) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, (bFVal==cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
      LatticeExprNode expr4(expr1==expr1);
      if (!checkBool (expr4, true, shape, false, false)) ok = false;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(bC);
      LatticeExprNode expr3(expr1==expr2);
      if (!checkBool (expr3, (bCVal==bCVal), shape, false, false)) ok = false;
   }
   cout << "Binary operator !=" << endl;
   {
      cout << "  bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, (bBVal!=cBVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, false, shape, true, false)) ok = false;
   }
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, (bFVal!=cFVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, false, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, (bCVal!=cCVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, false, shape, true, false)) ok = false;
   }
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, (bBVal!=cBVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bB.isMasked() || cB.isMasked()),
		      bB.getMask() && cB.getMask())) ok = false;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, false, shape, false, false)) ok = false;
      if (!checkMask (expr4, bB.isMasked(),
		      bB.getMask())) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, (bFVal!=cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, false, shape, false, false)) ok = false;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1!=expr2);
      if (!checkBool (expr3, (bCVal!=cCVal), shape, false, false)) ok = false;
      LatticeExprNode expr4(expr1!=expr1);
      if (!checkBool (expr4, false, shape, false, false)) ok = false;
   }
   cout << "Binary operator >" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, (bFVal>cFVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, false, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, (bCVal>cCVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, false, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, (bFVal>cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, false, shape, false, false)) ok = false;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1>expr2);
      if (!checkBool (expr3, (bCVal>cCVal), shape, false, false)) ok = false;
      LatticeExprNode expr4(expr1>expr1);
      if (!checkBool (expr4, false, shape, false, false)) ok = false;
   }
   cout << "Binary operator >=" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, (bFVal>=cFVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, true, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, (bCVal>=cCVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, true, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, (bFVal>=cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, true, shape, false, false)) ok = false;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1>=expr2);
      if (!checkBool (expr3, (bCVal>=cCVal), shape, false, false)) ok = false;
      LatticeExprNode expr4(expr1>=expr1);
      if (!checkBool (expr4, true, shape, false, false)) ok = false;
   }
   cout << "Binary operator <" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, (bFVal<cFVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, false, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, (bCVal<cCVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, false, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, (bFVal<cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, false, shape, false, false)) ok = false;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1<expr2);
      if (!checkBool (expr3, (bCVal<cCVal), shape, false, false)) ok = false;
      LatticeExprNode expr4(expr1<expr1);
      if (!checkBool (expr4, false, shape, false, false)) ok = false;
   }
   cout << "Binary operator <=" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, (bFVal<=cFVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, true, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, (bCVal<=cCVal), shape, true, false)) ok = false;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, true, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, (bFVal<=cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, true, shape, false, false)) ok = false;
      if (!checkMask (expr4, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3(expr1<=expr2);
      if (!checkBool (expr3, (bCVal<=cCVal), shape, false, false)) ok = false;
      LatticeExprNode expr4(expr1<=expr1);
      if (!checkBool (expr4, true, shape, false, false)) ok = false;
   }
   cout << "Binary operator &&" << endl;
   {
      cout << "  bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1&&expr2);
      if (!checkBool (expr3, (bBVal&&cBVal), shape, true, false)) ok = false;
   }
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1&&expr2);
      if (!checkBool (expr3, (bBVal&&cBVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bB.isMasked() || cB.isMasked()),
		      (!bB.get() && bB.getMask()) ||
		      (!cB.get() && cB.getMask()) ||
		      (bB.getMask() && cB.getMask()))) ok = false;
   }
   cout << "Binary operator ||" << endl;
   {
      cout << "  bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3(expr1||expr2);
      if (!checkBool (expr3, (bBVal||cBVal), shape, true, false)) ok = false;
   }
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3(expr1||expr2);
      if (!checkBool (expr3, (bBVal||cBVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bB.isMasked() || cB.isMasked()),
		      (bB.get() && bB.getMask()) ||
		      (cB.get() && cB.getMask()) ||
		      (bB.getMask() && cB.getMask()))) ok = false;
   }

   cout << "operator []" << endl;
   {
       cout << "  bool Array" << endl;
       LatticeExprNode expr1(cB);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkBool (expr3, cBVal, shape, false, false)) ok = false;
       if (!checkMask (expr3, true,
		       cB.getMask() && bB.get() && bB.getMask())) ok = false;
   }
   {
       cout << "  float Array" << endl;
       LatticeExprNode expr1(cF);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkFloat (expr3, cFVal, shape, false, false)) ok = false;
       if (!checkMask (expr3, true,
		       cF.getMask() && bB.get() && bB.getMask())) ok = false;
   }
   {
       cout << "  double Array" << endl;
       LatticeExprNode expr1(cD);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkDouble (expr3, cDVal, shape, false, false)) ok = false;
       if (!checkMask (expr3, true,
		       cD.getMask() && bB.get() && bB.getMask())) ok = false;
   }
   {
       cout << "  Complex Array" << endl;
       LatticeExprNode expr1(cC);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkComplex (expr3, cCVal, shape, false, false)) ok = false;
       if (!checkMask (expr3, true,
		       cC.getMask() && bB.get() && bB.getMask())) ok = false;
   }
   {
       cout << "  DComplex Array" << endl;
       LatticeExprNode expr1(cDC);
       LatticeExprNode expr2(bB);
       LatticeExprNode expr3(expr1[expr2]);
       if (!checkDComplex (expr3, cDCVal, shape, false, false)) ok = false;
       if (!checkMask (expr3, true,
		       cDC.getMask() && bB.get() && bB.getMask())) ok = false;
   }
//
//************************************************************************
//
// Check 1D functions
//
    cout << endl << "1-argument functions " << endl;
    cout << "sin" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkFloat (expr2, sin(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDouble (expr2, sin(bDVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkComplex(expr2, sin(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDComplex(expr2, sin(bDCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkFloat (expr2, sin(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDouble (expr2, sin(bDVal), shape, false, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkComplex(expr2, sin(bCVal), shape, false, false)) ok = false;
   }
   {
      cout << "  DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = sin(expr1);
      if (!checkDComplex(expr2, sin(bDCVal), shape, false, false)) ok = false;
   }
    cout << "sinh" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkFloat (expr2, sinh(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkComplex(expr2, sinh(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkFloat (expr2, sinh(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sinh(expr1);
      if (!checkComplex(expr2, sinh(bCVal), shape, false, false)) ok = false;
   }
    cout << "asin" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = asin(expr1);
      if (!checkFloat (expr2, asin(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = asin(expr1);
      if (!checkFloat (expr2, asin(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "cos" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkFloat (expr2, cos(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkComplex(expr2, cos(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkFloat (expr2, cos(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = cos(expr1);
      if (!checkComplex(expr2, cos(bCVal), shape, false, false)) ok = false;
   }
    cout << "cosh" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkFloat (expr2, cosh(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkComplex(expr2, cosh(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkFloat (expr2, cosh(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = cosh(expr1);
      if (!checkComplex(expr2, cosh(bCVal), shape, false, false)) ok = false;
   }
    cout << "acos" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = acos(expr1);
      if (!checkFloat (expr2, acos(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = acos(expr1);
      if (!checkFloat (expr2, acos(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "tan" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = tan(expr1);
      if (!checkFloat (expr2, tan(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = tan(expr1);
      if (!checkFloat (expr2, tan(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "tanh" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = tanh(expr1);
      if (!checkFloat (expr2, tanh(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = tanh(expr1);
      if (!checkFloat (expr2, tanh(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "atan" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = atan(expr1);
      if (!checkFloat (expr2, atan(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = atan(expr1);
      if (!checkFloat (expr2, atan(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "exp" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkFloat (expr2, exp(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkComplex(expr2, exp(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkFloat (expr2, exp(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = exp(expr1);
      if (!checkComplex(expr2, exp(bCVal), shape, false, false)) ok = false;
   }
    cout << "log" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = log(expr1);
      if (!checkFloat (expr2, log(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = log(expr1);
      if (!checkComplex(expr2, log(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = log(expr1);
      if (!checkFloat (expr2, log(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = log(expr1);
      if (!checkComplex(expr2, log(bCVal), shape, false, false)) ok = false;
   }
    cout << "log10" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkFloat (expr2, log10(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkComplex(expr2, log10(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkFloat (expr2, log10(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = log10(expr1);
      if (!checkComplex(expr2, log10(bCVal), shape, false, false)) ok = false;
   }
    cout << "sqrt" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkFloat (expr2, sqrt(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkComplex(expr2, sqrt(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkFloat (expr2, sqrt(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sqrt(expr1);
      if (!checkComplex(expr2, sqrt(bCVal), shape, false, false)) ok = false;
   }
    cout << "ceil" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = ceil(expr1);
      if (!checkFloat (expr2, ceil(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = ceil(expr1);
      if (!checkFloat (expr2, ceil(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "floor" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = floor(expr1);
      if (!checkFloat (expr2, floor(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = floor(expr1);
      if (!checkFloat (expr2, floor(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "conj" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = conj(expr1);
      if (!checkComplex(expr2, conj(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = conj(expr1);
      if (!checkComplex(expr2, conj(bCVal), shape, false, false)) ok = false;
   }
    cout << "complex" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = formComplex(expr1,expr2);
      if (!checkComplex(expr3, Complex(bFVal,cFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr3 = formComplex(real(expr1),imag(expr1));
      if (!checkComplex(expr3, bCVal, shape, false, false)) ok = false;
   }
   {
      cout << "  float Array,Scalar" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr3 = formComplex(real(expr1),min(imag(expr1)));
      if (!checkComplex(expr3, bCVal, shape, false, false)) ok = false;
   }
   {
      cout << "  float Scalar,Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr3 = formComplex(min(real(expr1)),imag(expr1));
      if (!checkComplex(expr3, bCVal, shape, false, false)) ok = false;
   }
    cout << "abs" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat (expr2, abs(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat(expr2, abs(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat (expr2, abs(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = abs(expr1);
      if (!checkFloat(expr2, abs(bCVal), shape, false, false)) ok = false;
   }
    cout << "arg" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = arg(expr1);
      if (!checkFloat(expr2, arg(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = arg(expr1);
      if (!checkFloat(expr2, arg(bCVal), shape, false, false)) ok = false;
   }
    cout << "real" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = real(expr1);
      if (!checkFloat(expr2, real(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = real(expr1);
      if (!checkFloat(expr2, real(bCVal), shape, false, false)) ok = false;
   }
    cout << "imag" << endl;
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = imag(expr1);
      if (!checkFloat(expr2, imag(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = imag(expr1);
      if (!checkFloat(expr2, imag(bCVal), shape, false, false)) ok = false;
   }
    cout << "min" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = min(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = min(expr1);
      if (!checkComplex(expr2, bCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = min(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, (nb==0))) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = min(expr1);
      if (!checkComplex(expr2, bCVal, shape, true, (nb==0))) ok = false;
   }
    cout << "max" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = max(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = max(expr1);
      if (!checkComplex(expr2, bCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = max(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, (nb==0))) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = max(expr1);
      if (!checkComplex(expr2, bCVal, shape, true, (nb==0))) ok = false;
   }
    cout << "sign" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sign(abs(expr1)+1);
      if (!checkFloat (expr2, float(1), shape, true, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sign(expr1-expr1);
      if (!checkFloat (expr2, float(0), shape, true, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sign(-abs(expr1)-1);
      if (!checkFloat (expr2, float(-1), shape, true, false)) ok = false;
   }
   {
      cout << "  double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = sign(abs(expr1)+1);
      if (!checkFloat (expr2, float(1), shape, true, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = sign(expr1-expr1);
      if (!checkFloat (expr2, float(0), shape, true, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = sign(-abs(expr1)-1);
      if (!checkFloat (expr2, float(-1), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sign(abs(expr1)+1);
      if (!checkFloat (expr2, float(1), shape, false, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sign(expr1-expr1);
      if (!checkFloat (expr2, float(0), shape, false, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sign(-abs(expr1)-1);
      if (!checkFloat (expr2, float(-1), shape, false, false)) ok = false;
   }
   {
      cout << "  double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = sign(abs(expr1)+1);
      if (!checkFloat (expr2, float(1), shape, false, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = sign(expr1-expr1);
      if (!checkFloat (expr2, float(0), shape, false, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = sign(-abs(expr1)-1);
      if (!checkFloat (expr2, float(-1), shape, false, false)) ok = false;
   }
    cout << "round" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = round(floor(abs(expr1)) + float(10.499));
      if (!checkFloat (expr2, 10+floor(abs(bFVal)), shape,
		       true, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = round(floor(abs(expr1)) + float(10.501));
      if (!checkFloat (expr2, 11+floor(abs(bFVal)), shape,
		       true, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = round(-floor(abs(expr1)) - float(10.499));
      if (!checkFloat (expr2, -(10+floor(abs(bFVal))), shape, 
		       true, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = round(-floor(abs(expr1)) - float(10.501));
      if (!checkFloat (expr2, -(11+floor(abs(bFVal))), shape,
		       true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = round(floor(abs(expr1)) + float(10.499));
      if (!checkFloat (expr2, 10+floor(abs(bFVal)), shape,
		       false, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = round(floor(abs(expr1)) + float(10.501));
      if (!checkFloat (expr2, 11+floor(abs(bFVal)), shape, 
		       false, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = round(-floor(abs(expr1)) - float(10.499));
      if (!checkFloat (expr2, -(10+floor(abs(bFVal))), shape,
		       false, false)) ok = false;
   }
   {
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = round(-floor(abs(expr1)) - float(10.501));
      if (!checkFloat (expr2, -(11+floor(abs(bFVal))), shape,
		       false, false)) ok = false;
   }
    cout << "median" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = median(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = median(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, (nb==0))) ok = false;
   }
    cout << "fractile" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = fractile(expr1, 0.5);
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = fractile(expr1, 0.5);
      if (!checkDouble (expr2, bDVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = fractile(expr1, 0.5);
      if (!checkFloat (expr2, bFVal, shape, true, (nb==0))) ok = false;
   }
   {
      cout << "  double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = fractile(expr1, 0.5);
      if (!checkDouble (expr2, bDVal, shape, true, (nb==0))) ok = false;
   }
    cout << "fractileRange 2" << endl;
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = fractileRange(expr1, 0.5);
      if (!checkFloat (expr2, 0., shape, true, (nb==0))) ok = false;
   }
   {
      cout << "  double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = fractileRange(expr1, 0.5);
      if (!checkDouble (expr2, 0., shape, true, (nb==0))) ok = false;
   }
    cout << "fractileRange 3" << endl;
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = fractileRange(expr1, 0.5, 0.5);
      if (!checkFloat (expr2, 0., shape, true, (nb==0))) ok = false;
   }
   {
      cout << "  double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = fractileRange(expr1, 0.5, 0.5);
      if (!checkDouble (expr2, 0., shape, true, (nb==0))) ok = false;
   }
    cout << "mean" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkComplex(expr2, bCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, (nb==0))) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = mean(expr1);
      if (!checkComplex(expr2, bCVal, shape, true, (nb==0))) ok = false;
   }
    cout << "variance" << endl;
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = variance(expr1);
      if (!checkFloat (expr2, 0.0, shape, true, (nb==0))) ok = false;
   }
    cout << "stddev" << endl;
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = stddev(expr1);
      if (!checkFloat (expr2, 0.0, shape, true, (nb==0))) ok = false;
   }
    cout << "avdev" << endl;
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = avdev(expr1);
      if (!checkFloat (expr2, 0.0, shape, true, (nb==0))) ok = false;
   }
    cout << "sum" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkComplex(expr2, bCVal, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkFloat (expr2, nb*bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = sum(expr1);
      if (!checkComplex(expr2, float(nb)*bCVal, shape, true, false)) ok = false;
   }
    cout << "nelements" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, 1.0, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, 1.0, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, double(nb), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = nelements(expr1);
      if (!checkDouble(expr2, double(nb), shape, true, false)) ok = false;
   }
    cout << "ndim" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = ndim(expr1);
      if (!checkFloat(expr2, 0.0, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = ndim(expr1);
      if (!checkFloat(expr2, 0.0, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = ndim(expr1);
      if (!checkFloat(expr2, shape.nelements(), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = ndim(expr1);
      if (!checkFloat(expr2, shape.nelements(), shape, true, false)) ok = false;
   }
    cout << "length" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = length(expr1,float(0));
      if (!checkFloat(expr2, 1.0, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = length(expr1,double(0));
      if (!checkFloat(expr2, 1.0, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = length(expr1,0);
      if (!checkFloat(expr2, shape(0), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = length(expr1,1);
      if (!checkFloat(expr2, shape(1), shape, true, false)) ok = false;
   }
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = length(expr1,2);
      if (!checkFloat(expr2, 1, shape, true, false)) ok = false;
   }
    cout << "any" << endl;
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = any(expr1);
      if (!checkBool(expr2, (nb>0 && bBVal), shape, true, false)) ok = false;
   }
    cout << "all" << endl;
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = all(expr1);
      if (!checkBool(expr2, (nb==0 || bBVal), shape, true, false)) ok = false;
   }
    cout << "ntrue" << endl;
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = ntrue(expr1);
      double result;
      if (bBVal) {
         result = nb;
      } else {
         result = 0.0;
      }
      if (!checkDouble(expr2, result, shape, true, false)) ok = false;
   }
    cout << "nfalse" << endl;
   {
      cout << "  bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2 = nfalse(expr1);
      double result;
      if (!bBVal) {
         result = nb;
      } else {
         result = 0.0;
      }
      if (!checkDouble(expr2, result, shape, true, false)) ok = false;
   }
    cout << "isNaN" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = isNaN(expr1);
      if (!checkBool(expr2, false, shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = isNaN(expr1);
      if (!checkBool(expr2, false, shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = isNaN(expr1);
      if (!checkBool(expr2, false, shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = isNaN(expr1);
      if (!checkBool(expr2, false, shape, false, false)) ok = false;
      if (!checkMask (expr2, bC.isMasked(),
		      bC.getMask())) ok = false;
   }
    cout << "indexin" << endl;     
   {
      Vector<bool> flags(2,true);
      LatticeExprNode expr1(0);
      LatticeExprNode expr2((ArrayLattice<bool>(flags)));
      LatticeExprNode expr = indexin(expr1,expr2);
      if (!checkBool (expr, true, IPosition(2,2,4), false, false, true)) ok = false;
   }
   {
      Vector<bool> flags(2,true);
      LatticeExprNode expr1(1);
      LatticeExprNode expr2((ArrayLattice<bool>(flags)));
      LatticeExprNode expr = indexin(expr1,expr2);
      if (!checkBool (expr, true, IPosition(2,10,2), false, false, true)) ok = false;
   }
   {
      Vector<bool> flags(3,false);
      LatticeExprNode expr1(0);
      LatticeExprNode expr2((ArrayLattice<bool>(flags)));
      LatticeExprNode expr = indexin(expr1,expr2);
      if (!checkBool (expr, false, IPosition(2,6,2), false, false, true)) ok = false;
      if (!checkBool (expr, false, IPosition(2,2,6), false, false, true)) ok = false;
   }


//
//************************************************************************
//
// Check 2D functions
//

    cout << endl << "2-argument functions " << endl;    
    cout << "atan2" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = atan2(expr1,expr2);
      if (!checkFloat (expr3, atan2(bFVal,cFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = atan2(expr1,expr2);
      if (!checkFloat (expr3, atan2(bFVal,cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
    cout << "pow" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkFloat (expr3, pow(bFVal,cFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkComplex(expr3, pow(bCVal,cCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkFloat (expr3, pow(bFVal,cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   {
      cout << "  Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = pow(expr1,expr2);
      if (!checkComplex(expr3, pow(bCVal,cCVal), shape, false, false)) ok = false;
   }
    cout << "fmod" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = fmod(expr1,expr2);
      if (!checkFloat (expr3, fmod(bFVal,cFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = fmod(expr1,expr2);
      if (!checkFloat (expr3, fmod(bFVal,cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
    cout << "min" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = min(expr1,expr2);
      if (!checkFloat (expr3, min(bFVal,cFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = min(expr1,expr2);
      if (!checkFloat (expr3, min(bFVal,cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
    cout << "max" << endl;
   {
      cout << "  float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = max(expr1,expr2);
      if (!checkFloat (expr3, max(bFVal,cFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = max(expr1,expr2);
      if (!checkFloat (expr3, max(bFVal,cFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   cout << "amp" << endl;
   {
      cout << " float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = amp(expr1,expr2);
      float result = sqrt(bFVal*bFVal+cFVal*cFVal);
      if (!checkFloat (expr3, result, shape, true, false)) ok = false;
   }
   {
      cout << " Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cCVal);
      LatticeExprNode expr3 = amp(expr1,expr2);
      Complex result = sqrt(bCVal*bCVal+cCVal*cCVal);
      if (!checkComplex (expr3, result, shape, true, false)) ok = false;
   }
   {
      cout << " float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = amp(expr1,expr2);
      float result = sqrt(bFVal*bFVal+cFVal*cFVal);
      if (!checkFloat (expr3, result, shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   {
      cout << " Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = amp(expr1,expr2);
      Complex result = sqrt(bCVal*bCVal+cCVal*cCVal);
      if (!checkComplex (expr3, result, shape, false, false)) ok = false;
   }
   cout << "pa" << endl;
   {
      cout << " float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = pa(expr1,expr2);
      float result = 90.0/C::pi*atan2(bFVal,cFVal);
      if (!checkFloat (expr3, result, shape, true, false)) ok = false;
   }
   {
      cout << " float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = pa(expr1,expr2);
      float result = 90.0/C::pi*atan2(bFVal,cFVal);
      if (!checkFloat (expr3, result, shape, false, false)) ok = false;
      if (!checkMask (expr3, (bF.isMasked() || cF.isMasked()),
		      bF.getMask() && cF.getMask())) ok = false;
   }
   {
      cout << " double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = pa(expr1,expr2);
      double result = 90.0/C::pi*atan2(bDVal,cDVal);
      if (!checkDouble (expr3, result, shape, true, false)) ok = false;
   }
   {
      cout << " double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = pa(expr1,expr2);
      double result = 90.0/C::pi*atan2(bDVal,cDVal);
      if (!checkDouble (expr3, result, shape, false, false)) ok = false;
   }
   cout << "mask" << endl;
   {
      cout << " float Scalar 1" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr3 = mask(expr1);
      if (! checkInfo (expr3, shape, true, false, TpBool)) {
	ok = false;
      } else {
	if (expr3.getBool() != true) {
	  cout << "  expected true; result is " << expr3.getBool() << endl;
	  ok = false;
	}
      }
   }
   {
      cout << " float Scalar 2" << endl;
      LatticeExprNode expr1(min(bF));
      LatticeExprNode expr3 = mask(expr1);
      if (! checkInfo (expr3, shape, true, false, TpBool)) {
	ok = false;
      } else {
	bool result = (nb!=0);
	if (expr3.getBool() != result) {
	  cout << "  expected " << result
	       << "; result is " << expr3.getBool() << endl;
	  ok = false;
	}
      }
   }
   {
      cout << " float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr3 = mask(expr1);
      if (! checkInfo (expr3, shape, false, false, TpBool)) {
	ok = false;
      } else {
	LELArray<bool> result(shape);
	Slicer section(IPosition(shape.nelements(), 0), shape);
	expr3.eval (result, section);
	if (! allEQ(result.value(), bF.getMask())) {
	  cout << "  expected " << bF.getMask()
	       << " result is " << result.value() << endl;
	  ok = false;
	}
      }
      if (!checkMask (expr3, false, bF.getMask())) ok = false;
   }
   {
      cout << " bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr3 = mask(expr1);
      if (! checkInfo (expr3, shape, true, false, TpBool)) {
	ok = false;
      } else {
	if (expr3.getBool() != true) {
	  cout << "  expected true; result is " << expr3.getBool() << endl;
	  ok = false;
	}
      }
   }
   {
      cout << " bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr3 = mask(expr1);
      if (! checkInfo (expr3, shape, false, false, TpBool)) {
	ok = false;
      } else {
	LELArray<bool> result(shape);
	Slicer section(IPosition(shape.nelements(), 0), shape);
	expr3.eval (result, section);
	if (! allEQ(result.value(), bB.getMask())) {
	  cout << "  expected " << bB.getMask()
	       << " result is " << result.value() << endl;
	  ok = false;
	}
      }
      if (!checkMask (expr3, false, bB.getMask())) ok = false;
   }
   cout << "value" << endl;
   {
      cout << " float Scalar 1" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr3 = value(expr1);
      if (!checkFloat (expr3, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << " float Scalar 2" << endl;
      LatticeExprNode expr1(min(bF));
      LatticeExprNode expr3 = value(expr1);
      if (!checkFloat (expr3, bFVal, shape, true, (nb==0))) ok = false;
   }
   {
      cout << " float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr3 = value(expr1);
      if (!checkFloat (expr3, bFVal, shape, false, false)) ok = false;
      if (!checkMask (expr3, false, bF.getMask())) ok = false;
   }
   {
      cout << " bool Scalar" << endl;
      LatticeExprNode expr1(bBVal);
      LatticeExprNode expr3 = value(expr1);
      if (!checkBool (expr3, bBVal, shape, true, false)) ok = false;
   }
   {
      cout << " bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr3 = value(expr1);
      if (!checkBool (expr3, bBVal, shape, false, false)) ok = false;
      if (!checkMask (expr3, false, bB.getMask())) ok = false;
   }
   cout << "replace" << endl;
   {
      cout << " float Scalar" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = replace(expr1,expr2);
      if (!checkFloatRepl (expr3, bFVal, shape, cF.get(), cFVal, true))
                                                           ok = false;
      if (!checkMask (expr3, bF.isMasked(), bF.getMask())) ok = false;
   }
   {
      cout << " float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2(cF);
      LatticeExprNode expr3 = replace(expr1,expr2);
      if (!checkFloatRepl (expr3, bFVal, shape, cF.get(), cFVal, false))
                                                           ok = false;
      if (!checkMask (expr3, bF.isMasked(), bF.getMask())) ok = false;
   }
   {
      cout << " bool Scalar" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3 = replace(expr1,expr2);
      if (!checkBoolRepl (expr3, bBVal, shape, cB.get(), cBVal, true))
                                                           ok = false;
      if (!checkMask (expr3, bB.isMasked(), bB.getMask())) ok = false;
   }
   {
      cout << " bool Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3 = replace(expr1,expr2);
      if (!checkBoolRepl (expr3, bBVal, shape, cB.get(), cBVal, false))
                                                           ok = false;
      if (!checkMask (expr3, bB.isMasked(), bB.getMask())) ok = false;
   }
   {
      cout << "Rebin" << endl;
      IPosition shapeIn(2, 10, 20);
      IPosition binfac(2, 2, 2);
      LatticeExprNode nodeBin(binfac);
//
      IPosition binI(shapeIn.nelements());
      for (uint32_t i=0; i<binI.nelements(); i++) {
	binI[i] = 2;
      }
//
      {
         cout << "  float" << endl;
         ArrayLattice<float> lat(shapeIn);
         lat.set(1.0);
         SubLattice<float> mLat(lat);
         RebinLattice<float> rL(mLat, binI);
         IPosition shapeBin = rL.shape();
//
         LatticeExprNode expr = rebin(mLat,nodeBin);
         const Array<float>& data = expr.getArrayFloat();
         AlwaysAssert(data.shape().isEqual(shapeBin), AipsError);
         AlwaysAssert(allNear(data, float(1.0), float(1.0e-6)), AipsError);
         AlwaysAssert(allNear(data, rL.get(), 1.0e-6), AipsError);
//
         bool hasMask = rL.isMasked();
         checkMask (expr, hasMask, rL.getMask());
      }
      {
         cout << "  double" << endl;
         ArrayLattice<double> lat(shapeIn);
         lat.set(1.0);
         SubLattice<double> mLat(lat);
         RebinLattice<double> rL(mLat, binI);
         IPosition shapeBin = rL.shape();
//
         LatticeExprNode expr = rebin(mLat,nodeBin);
         const Array<double>& data = expr.getArrayDouble();
         AlwaysAssert(data.shape().isEqual(shapeBin), AipsError);
         AlwaysAssert(allNear(data, double(1.0), double(1.0e-6)), AipsError);
         AlwaysAssert(allNear(data, rL.get(), 1.0e-6), AipsError);
//
         bool hasMask = rL.isMasked();
         checkMask (expr, hasMask, rL.getMask());
      }
      {
         cout << "  Complex" << endl;
         ArrayLattice<Complex> lat(shapeIn);
         Complex val(1.0, 1.0);
         lat.set(val);
         SubLattice<Complex> mLat(lat);
         RebinLattice<Complex> rL(mLat, binI);
         IPosition shapeBin = rL.shape();
//
         LatticeExprNode expr = rebin(mLat,nodeBin);
         const Array<Complex>& data = expr.getArrayComplex();
         AlwaysAssert(data.shape().isEqual(shapeBin), AipsError);
         AlwaysAssert(allNear(data, val, 1.0e-6), AipsError);
         AlwaysAssert(allNear(data, rL.get(), 1.0e-6), AipsError);
//
         bool hasMask = rL.isMasked();
         checkMask (expr, hasMask, rL.getMask());
      }
      {
         cout << "  DComplex" << endl;
         ArrayLattice<DComplex> lat(shapeIn);
         DComplex val(1.0, 1.0);
         lat.set(val);
         SubLattice<DComplex> mLat(lat);
         RebinLattice<DComplex> rL(mLat, binI);
         IPosition shapeBin = rL.shape();
//
         LatticeExprNode expr = rebin(mLat,nodeBin);
         const Array<DComplex>& data = expr.getArrayDComplex();
         AlwaysAssert(data.shape().isEqual(shapeBin), AipsError);
         AlwaysAssert(allNear(data, val, 1.0e-6), AipsError);
         AlwaysAssert(allNear(data, rL.get(), 1.0e-6), AipsError);
//
         bool hasMask = rL.isMasked();
         checkMask (expr, hasMask, rL.getMask());
      }
   }        
      

//
//************************************************************************
//
// Check 3D functions
//

    cout << endl << "3-argument functions " << endl;    
    cout << "iif" << endl;
   {
      cout << "  float Scalar,Scalar,Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cFVal);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkFloat (expr3, bFVal, shape, true, false)) ok = false;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkFloat (expr4, cFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  double Scalar,Array,Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, false, false)) ok = false;
   }
   {
      cout << "  Complex Scalar,Scalar,Array" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cC);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkComplex (expr3, bCVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkComplex (expr4, cCVal, shape, false, false)) ok = false;
   }
   {
      cout << "  DComplex Scalar,Array,Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2(cDC);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkDComplex (expr3, bDCVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkDComplex (expr4, cDCVal, shape, false, false)) ok = false;
   }
   {
      cout << "  bool Scalar,Array,Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3 = iif(aBVal,expr1,expr2);
      if (!checkBool (expr3, bBVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bBVal,expr1,expr2);
      if (!checkBool (expr4, cBVal, shape, false, false)) ok = false;
   }
   {
      cout << "  float/double Array,Scalar,Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, false, false)) ok = false;
   }
   {
      cout << "  Complex/double Array,Scalar,Array" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDComplex (expr3, bDCVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDComplex (expr4, DComplex(cDVal,0), shape, false, false)) ok = false;
   }
   {
      cout << "  double Array,Array,Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cDVal);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, false, false)) ok = false;
   }
   {
      cout << "  double Array,Array,Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2(cD);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkDouble (expr3, bDVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkDouble (expr4, cDVal, shape, false, false)) ok = false;
   }
   {
      cout << "  bool Array,Array,Scalar" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cBVal);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkBool (expr3, bBVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkBool (expr4, cBVal, shape, false, false)) ok = false;
   }
   {
      cout << "  bool Array,Array,Array" << endl;
      LatticeExprNode expr1(bB);
      LatticeExprNode expr2(cB);
      LatticeExprNode expr3 = iif(aB,expr1,expr2);
      if (!checkBool (expr3, bBVal, shape, false, false)) ok = false;
      LatticeExprNode expr4 = iif(bB,expr1,expr2);
      if (!checkBool (expr4, cBVal, shape, false, false)) ok = false;
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
      cout << "  from float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, bFVal, shape, true, false)) ok = false;
   }
   {
      cout << "  from double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, float(bDVal), shape, true, false)) ok = false;
   }
   {
      cout << "  from float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, bFVal, shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  from double Array" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toFloat(expr1);
      if (!checkFloat (expr2, float(bDVal), shape, false, false)) ok = false;
      if (!checkMask (expr2, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
    cout << "toDouble" << endl;
   {
      cout << "  from float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, double(bFVal), shape, true, false)) ok = false;
   }
   {
      cout << "  from double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, bDVal, shape, true, false)) ok = false;
   }
   {
      cout << "  from float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, double(bFVal), shape, false, false)) ok = false;
   }
   {
      cout << "  from double Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toDouble(expr1);
      if (!checkDouble(expr2, bDVal, shape, false, false)) ok = false;
   }
    cout << "toComplex" << endl;
   {
      cout << "  from float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bFVal,0.0), shape, true, false)) ok = false;
   }
   {
      cout << "  from double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDVal,0.0), shape, true, false)) ok = false;
   }
   {
      cout << "  from Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  from DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  from float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bFVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from double Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from Complex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, bCVal, shape, false, false)) ok = false;
   }
   {
      cout << "  from DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = toComplex(expr1);
      if (!checkComplex(expr2, Complex(bDCVal), shape, false, false)) ok = false;
   }
    cout << "toDComplex" << endl;
   {
      cout << "  from float Scalar" << endl;
      LatticeExprNode expr1(bFVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bFVal,0.0), shape, true, false)) ok = false;
   }
   {
      cout << "  from double Scalar" << endl;
      LatticeExprNode expr1(bDVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDVal,0.0), shape, true, false)) ok = false;
   }
   {
      cout << "  from Complex Scalar" << endl;
      LatticeExprNode expr1(bCVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      DComplex result;
      result = bCVal;
      if (!checkDComplex(expr2, result, shape, true, false)) ok = false;
   }
   {
      cout << "  from DComplex Scalar" << endl;
      LatticeExprNode expr1(bDCVal);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDCVal), shape, true, false)) ok = false;
   }
   {
      cout << "  from float Array" << endl;
      LatticeExprNode expr1(bF);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bFVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from double Scalar" << endl;
      LatticeExprNode expr1(bD);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from DComplex Array" << endl;
      LatticeExprNode expr1(bC);
      LatticeExprNode expr2 = toDComplex(expr1);
      DComplex result;
      result = bCVal;
      if (!checkDComplex(expr2, result, shape, false, false)) ok = false;
   }
   {
      cout << "  from DComplex Array" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExprNode expr2 = toDComplex(expr1);
      if (!checkDComplex(expr2, DComplex(bDCVal), shape, false, false)) ok = false;
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
   cout << "LatticeExpr<float>()" << endl;
   {
      cout << "  from float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<float> expr2 = (LatticeExpr<float>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkFloat (expr3, float(bFVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   {
      cout << "  from double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<float> expr2 = (LatticeExpr<float>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkFloat (expr3, float(bDVal), shape, false, false)) ok = false;
      if (!checkMask (expr3, bF.isMasked(),
		      bF.getMask())) ok = false;
   }
   cout << "LatticeExpr<double>()" << endl;
   {
      cout << "  from float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<double> expr2 = (LatticeExpr<double>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDouble(expr3, double(bFVal), shape, false, false)) ok = false;
   }
   {
      cout << "  from double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<double> expr2 = (LatticeExpr<double>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDouble(expr3, double(bDVal), shape, false, false)) ok = false;
   }
   cout << "LatticeExpr<Complex>()" << endl;
   {
      cout << "  from float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bFVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bDVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from Complex" << endl;
      LatticeExprNode expr1(bC);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bCVal), shape, false, false)) ok = false;
   }
   {
      cout << "  from DComplex" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExpr<Complex> expr2 = (LatticeExpr<Complex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkComplex(expr3, Complex(bDCVal), shape, false, false)) ok = false;
   }
   cout << "LatticeExpr<DComplex>()" << endl;
   {
      cout << "  from float" << endl;
      LatticeExprNode expr1(bF);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDComplex(expr3, DComplex(bFVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from double" << endl;
      LatticeExprNode expr1(bD);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDComplex(expr3, DComplex(bDVal,0.0), shape, false, false)) ok = false;
   }
   {
      cout << "  from Complex" << endl;
      LatticeExprNode expr1(bC);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      DComplex result;
      result = bCVal;
      if (!checkDComplex(expr3, result, shape, false, false)) ok = false;
   }
   {
      cout << "  from DComplex" << endl;
      LatticeExprNode expr1(bDC);
      LatticeExpr<DComplex> expr2 = (LatticeExpr<DComplex>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkDComplex(expr3, DComplex(bDCVal), shape, false, false)) ok = false;
   }
   cout << "LatticeExpr<bool>()" << endl;
   {
      cout << "  from bool" << endl;
      LatticeExprNode expr1(bB);
      LatticeExpr<bool> expr2 = (LatticeExpr<bool>)expr1;
      LatticeExprNode expr3(expr2);
      if (!checkBool(expr3, bBVal, shape, false, false)) ok = false;
   }

   return ok;
}



bool compareScalarFloat (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const float bVal,
                         const IPosition shape)
{
      LELArray<float> Arr(shape);
      LELArray<float> Arr2(shape);
      // Test LELArray copy constructor and assignment.
      LELArray<float> Arrt(Arr);
      Arrt = Arr2;
      float result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      bool ok = true;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = false;
      }
      if (expr.getFloat() != expr2.getFloat()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getFloat() 
              << ", " << expr2.getFloat() << endl;
         ok = false;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = false;
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
         ok = false;
      }
      return ok;
}

bool compareScalarDouble (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const double bVal,
                         const IPosition shape)
{
      LELArray<double> Arr(shape);
      LELArray<double> Arr2(shape);
      double result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      bool ok = true;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = false;
      }
      if (expr.getDouble() != expr2.getDouble()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getDouble() 
              << ", " << expr2.getDouble() << endl;
         ok = false;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = false;
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
         ok = false;
      }
      return ok;
}


bool compareScalarComplex (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const Complex bVal,
                         const IPosition shape)
{
      LELArray<Complex> Arr(shape);
      LELArray<Complex> Arr2(shape);
      Complex result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      bool ok = true;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = false;
      }
      if (expr.getComplex() != expr2.getComplex()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getComplex() 
              << ", " << expr2.getComplex() << endl;
         ok = false;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = false;
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
         ok = false;
      }
      return ok;
}


bool compareScalarDComplex (const LatticeExprNode expr,
                         const LatticeExprNode expr2,
                         const DComplex bVal,
                         const IPosition shape)
{
      LELArray<DComplex> Arr(shape);
      LELArray<DComplex> Arr2(shape);
      DComplex result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      bool ok = true;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = false;
      }
      if (expr.getDComplex() != expr2.getDComplex()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getDComplex() 
              << ", " << expr2.getDComplex() << endl;
         ok = false;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = false;
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
         ok = false;
      }
      return ok;
}




bool compareScalarBool (const LatticeExprNode expr,
			const LatticeExprNode expr2,
			const bool bVal,
			const IPosition shape)
{
      LELArray<bool> Arr(shape);
      LELArray<bool> Arr2(shape);
      bool result, result2;
      IPosition origin(shape); origin = 0;
      Slicer region(origin, shape);
      bool ok = true;

      if (expr.isScalar() != expr2.isScalar()) {
         cout << "   result should be a scalar" << endl;
         cout << "   results are " << expr.isScalar() 
              << ", " << expr2.isScalar() << endl;
         ok = false;
      }
      if (expr.getBool() != expr2.getBool()) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << expr.getBool() 
              << ", " << expr2.getBool() << endl;
         ok = false;
      }
      expr.eval(result);
      expr2.eval(result2);
      if (result != result2) {
         cout << "   result should be " << bVal << endl;
         cout << "   Scalar results are " << result
              << ", " << result2  << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      expr2.eval(Arr2, region);
      if (! allEQ (Arr.value(), Arr2.value())) {
         cout << "   result should be " << bVal << endl;
         cout << "   Array results are  " << Arr.value()(origin) 
              << ", " << Arr2.value()(origin) << endl;
         ok = false;
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
         ok = false;
      }
      return ok;
}




bool checkInfo (const LatticeExprNode& expr,
		const IPosition& shape,
		const bool shouldBeScalar,
		const bool undefinedScalar,
		const DataType dtype,
		const bool emptyShape)
{
    bool ok = true;  
    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = false;
    }
    if (!shouldBeScalar && expr.isScalar()) {
       cout << "   result should be array" << endl;
       ok = false;
    }
    if (expr.dataType() != dtype) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << dtype << endl;
       ok = false;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = false;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (emptyShape) {
	  if (!expr.shape().isEqual(IPosition())) {
	     cout << "   Shape should be empty" << endl;
	     cout << "   Shape is        " << expr.shape() << endl;
	  }
       } else {
	  if (!expr.shape().isEqual(shape)) {
	     cout << "   Shape should be " << shape << endl;
	     cout << "   Shape is        " << expr.shape() << endl;
	     ok = false;
	  }
       }
    }
    return ok;
}

bool checkFloat (const LatticeExprNode& expr,
                 const float result,
                 const IPosition& shape,
                 const bool shouldBeScalar,
		 const bool undefinedScalar)
{
    bool ok = checkInfo (expr, shape, shouldBeScalar,
			 undefinedScalar, TpFloat);
    float result2;
    LELArray<float> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getFloat();
	if (!near(result2, result)) {
	  cout << "   result should be " << result << endl;
	  cout << "   Scalar result is " << result2  << endl;
	  ok = false;
	}
	expr.eval(result2);
	if (!near(result2, result)) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = false;
	}
      }
      expr.eval(Arr, region);
      if (! allNear (Arr.value(), result, 1.e-06)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = false;
      }
    }
    return ok;
}

bool checkFloatRepl (const LatticeExprNode& expr,
		     const float result,
		     const IPosition& shape,
		     const Array<float>& replArray,
		     float replScalar,
		     bool isReplScalar)
{
    bool ok = checkInfo (expr, shape, false, false, TpFloat);
    LELArray<float> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval(Arr, region);
    if (! Arr.isMasked()) {
       if (! allNear (Arr.value(), result,1.e-06)) {
	  cout << "   result should be " << result << endl;
	  cout << "   Array result is  " << Arr.value() << endl;
	  ok = false;
       }
    } else {
       bool delres, delmask, delrepl;
       const bool* mask = Arr.mask().getStorage (delmask);
       const float* res = Arr.value().getStorage (delres);
       const float* repl = 0;
       if (!isReplScalar) {
	  repl = replArray.getStorage (delrepl);
       }
       uint32_t n = Arr.value().nelements();
       for (uint32_t i=0; i<n; i++) {
	  if (! mask[i]) {
	     if (!isReplScalar) {
	        replScalar = repl[i];
	     }
	     if (!near(res[i], replScalar, 1e-06)) {
	        cout << "   result " << i << " should be " << replScalar << endl;
		cout << "   Replace result is " << res[i] << endl;
		ok = false;
	     }
	  } else {
	     if (!near(res[i], result, 1e-06)) {
	        cout << "   result " << i << " should be " << result << endl;
		cout << "   Replace result is " << res[i] << endl;
		ok = false;
	     }
	  }
       }
       Arr.mask().freeStorage (mask, delmask);
       Arr.value().freeStorage (res, delres);
       if (!isReplScalar) {
	  replArray.freeStorage (repl, delrepl);
       }
    }
    return ok;
}


bool checkDouble (const LatticeExprNode& expr,
		  const double result,
		  const IPosition& shape,
		  const bool shouldBeScalar,
		  const bool undefinedScalar)
{
    bool ok = true;  
    double result2;
    LELArray<double> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = false;
    }
    if (expr.dataType() != TpDouble) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpDouble << endl;
       ok = false;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = false;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = false;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getDouble();
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2  << endl;
          ok = false;
	}
	expr.eval(result2);
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = false;
	}
      }
      expr.eval(Arr, region);
      if (! allEQ (Arr.value(), result)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = false;
      }
    }
    return ok;
}


bool checkComplex (const LatticeExprNode& expr,
		   const Complex result,
		   const IPosition& shape,
		   const bool shouldBeScalar,
		   const bool undefinedScalar)
{
    bool ok = true;  
    Complex result2;
    LELArray<Complex> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = false;
    }
    if (expr.dataType() != TpComplex) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpComplex << endl;
       ok = false;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = false;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = false;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getComplex();
	if (!near (result2, result)) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2  << endl;
          ok = false;
	}
	expr.eval(result2);
	if (!near (result2, result)) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = false;
	}
      }
      expr.eval(Arr, region);
      if (! allNear (Arr.value(), result, 1.0e-5)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = false;
      }
    }
    return ok;
}


bool checkDComplex (const LatticeExprNode& expr,
		    const DComplex result,
		    const IPosition& shape,
		    const bool shouldBeScalar,
		    const bool undefinedScalar)
{
    bool ok = true;  
    DComplex result2;
    LELArray<DComplex> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);

    if (shouldBeScalar && !expr.isScalar()) {
       cout << "   result should be scalar" << endl;
       ok = false;
    }
    if (expr.dataType() != TpDComplex) {
       cout << "   Data type is        " << expr.dataType() << endl;
       cout << "   Data type should be " << TpDComplex << endl;
       ok = false;
    }
    if (expr.isScalar()) {
       if (!expr.shape().isEqual(IPosition())) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = false;
       }
       if (undefinedScalar != expr.isInvalidScalar()) {
	  cout << "   Incorrect (in)valid scalar result" << endl;
       }
    } else {
       if (!expr.shape().isEqual(shape)) {
          cout << "   Shape should be " << shape << endl;
          cout << "   Shape is        " << expr.shape() << endl;
          ok = false;
       }
    }
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getDComplex();
	if (!near (result2, result)) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2  << endl;
          ok = false;
	}
	expr.eval(result2);
	if (!near (result2, result)) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = false;
	}
      }
      expr.eval(Arr, region);
      if (! allNear (Arr.value(), result, 1.0e-13)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = false;
      }
    }
    return ok;
}


bool checkBool (const LatticeExprNode& expr,
		const bool result,
		const IPosition& shape,
		const bool shouldBeScalar,
		const bool undefinedScalar,
		const bool emptyShape)
{
    bool ok = checkInfo (expr, shape, shouldBeScalar,
			 undefinedScalar, TpBool, emptyShape);
    bool result2;
    LELArray<bool> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    if (! expr.isInvalidScalar()) {
      if (expr.isScalar()) {
	result2 = expr.getBool();
	if (result2 != result) {
	  cout << "   result should be " << result << endl;
	  cout << "   Scalar result is " << result2  << endl;
	  ok = false;
	}
	expr.eval(result2);
	if (result2 != result) {
          cout << "   result should be " << result << endl;
          cout << "   Scalar result is " << result2 << endl;
          ok = false;
	}
      }
      expr.eval(Arr, region);
      if (! allEQ (Arr.value(), result)) {
	cout << "   result should be " << result << endl;
	cout << "   Array result is  " << Arr.value() << endl;
	ok = false;
      }
    }
    return ok;
}

bool checkBoolRepl (const LatticeExprNode& expr,
		    const bool result,
		    const IPosition& shape,
		    const Array<bool>& replArray,
		    bool replScalar,
		    bool isReplScalar)
{
    bool ok = checkInfo (expr, shape, false, false, TpBool);
    LELArray<bool> Arr(shape);
    IPosition origin(shape); origin = 0;
    Slicer region(origin, shape);
    expr.eval(Arr, region);
    if (! Arr.isMasked()) {
       if (! allEQ (Arr.value(), result)) {
	  cout << "   result should be " << result << endl;
	  cout << "   Array result is  " << Arr.value() << endl;
	  ok = false;
       }
    } else {
       bool delres, delmask, delrepl;
       const bool* mask = Arr.mask().getStorage (delmask);
       const bool* res = Arr.value().getStorage (delres);
       const bool* repl = 0;
       if (!isReplScalar) {
	  repl = replArray.getStorage (delrepl);
       }
       uint32_t n = Arr.value().nelements();
       for (uint32_t i=0; i<n; i++) {
	  if (! mask[i]) {
	     if (!isReplScalar) {
	        replScalar = repl[i];
	     }
	     if (res[i] != replScalar) {
	        cout << "   result " << i << " should be " << replScalar << endl;
		cout << "   Replace result is " << res[i] << endl;
		ok = false;
	     }
	  } else {
	     if (res[i] != result) {
	        cout << "   result " << i << " should be " << result << endl;
		cout << "   Replace result is " << res[i] << endl;
		ok = false;
	     }
	  }
       }
       Arr.mask().freeStorage (mask, delmask);
       Arr.value().freeStorage (res, delres);
       if (!isReplScalar) {
	  replArray.freeStorage (repl, delrepl);
       }
    }
    return ok;
}


bool checkMask (const LatticeExprNode& expr,
		bool hasMask,
		const Array<bool>& result)
{
    if (hasMask != expr.isMasked()) {
        cout << "   expr should have " << hasMask << " a mask" << endl;
        return false;
    }
    IPosition origin(result.shape()); origin = 0;
    Slicer region(origin, result.shape());
    Array<bool> mask;
    bool isMasked;
    switch (expr.dataType()) {
    case TpBool:
      {
        LELArray<bool> arr(result.shape());
	expr.eval (arr, region);
	isMasked = arr.isMasked();
	if (isMasked) mask = arr.mask();
	break;
      }
    case TpFloat:
      {
        LELArray<float> arr(result.shape());
	expr.eval (arr, region);
	isMasked = arr.isMasked();
	if (isMasked) mask = arr.mask();
	break;
      }
    case TpDouble:
      {
        LELArray<double> arr(result.shape());
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
	return false;
    }
    if (hasMask != isMasked) {
        cout << "   result should have " << hasMask << " a mask" << endl;
        return false;
    }
    if (hasMask) {
        if (anyNE (result, mask)) {
	    cout << "   result should have mask " << result << endl;
	    cout << "              but has mask " << mask << endl;
	    return false;
	}
    }
    return true;
}


int main (int argc, const char* argv[])
{
  bool ok = true;
  try {
    cout << ">>>" << endl;
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "2", "Number of pixels along the x-axis", "int");
    inp.create("ny", "2", "Number of pixels along the y-axis", "int");
    inp.readArguments(argc, argv);
    cout << "<<<" << endl;

    const uint32_t nx=inp.getInt("nx");
    const uint32_t ny=inp.getInt("ny");

//
// The use of these tiny ArrayLattices means this test program
// does not computationally stress the classes.  Here we just
// test them logically.  See other test programs for stress tests
//

    IPosition shape(2,nx,ny);

// bool Lattices

    ArrayLattice<bool> aB(shape);
    ArrayLattice<bool> bB(shape);
    ArrayLattice<bool> cB(shape);
    bool aBVal = true;
    aB.set(aBVal);
    bool bBVal = false;
    bB.set(bBVal);
    bool cBVal = true;
    cB.set(cBVal);



// FLoat Lattices

    ArrayLattice<float> aF(shape);
    ArrayLattice<float> bF(shape);
    ArrayLattice<float> cF(shape);
    float aFVal = 0.0;
    aF.set(aFVal);
    float bFVal = 1.0;
    bF.set(1.0);
    float cFVal = 2.0;
    cF.set(cFVal);


// double Lattices

    ArrayLattice<double> aD(shape);
    ArrayLattice<double> bD(shape);
    ArrayLattice<double> cD(shape);
    double aDVal = 0.0;
    aD.set(aDVal);
    double bDVal = 1.0;
    bD.set(1.0);
    double cDVal = 2.0;
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
    Matrix<bool> mat1(shape);
    Matrix<bool> mat2(shape);
    Matrix<bool> mat3(shape);
    mat1 = true;
    mat2 = true;
    mat3 = true;
    mat1(0,0) = false;
    mat2(1,0) = false;
    mat3(0,1) = false;
    LCBox box(shape);
    LCPixelSet mask1 (mat1, box);
    LCPixelSet mask2 (mat2, box);
    LCPixelSet mask3 (mat3, box);
    
    if (!doIt (SubLattice<float>(aF),
	       SubLattice<float>(bF),
	       SubLattice<float>(cF),
	       SubLattice<double>(bD),
	       SubLattice<double>(cD),
	       SubLattice<Complex>(bC),
	       SubLattice<Complex>(cC),
	       SubLattice<DComplex>(bDC),
	       SubLattice<DComplex>(cDC),
	       SubLattice<bool>(aB),
	       SubLattice<bool>(bB),
	       SubLattice<bool>(cB),
	       bFVal,cFVal,bDVal,cDVal,bCVal,cCVal,
	       bDCVal,cDCVal,aBVal,bBVal,cBVal,
	       4)) {
	ok = false;
    }
    if (!doIt (SubLattice<float>(aF,mask1),
	       SubLattice<float>(bF,mask2),
	       SubLattice<float>(cF,mask3),
	       SubLattice<double>(bD,mask2),
	       SubLattice<double>(cD,mask3),
	       SubLattice<Complex>(bC,mask2),
	       SubLattice<Complex>(cC,mask3),
	       SubLattice<DComplex>(bDC,mask2),
	       SubLattice<DComplex>(cDC,mask3),
	       SubLattice<bool>(aB,mask1),
	       SubLattice<bool>(bB,mask2),
	       SubLattice<bool>(cB,mask3),
	       bFVal,cFVal,bDVal,cDVal,bCVal,cCVal,
	       bDCVal,cDCVal,aBVal,bBVal,cBVal,
	       3)) {
	ok = false;
    }
    mat2 = false;
    mask2 = LCPixelSet (mat2, box);
    if (!doIt (SubLattice<float>(aF,mask1),
	       SubLattice<float>(bF,mask2),
	       SubLattice<float>(cF,mask3),
	       SubLattice<double>(bD,mask2),
	       SubLattice<double>(cD,mask3),
	       SubLattice<Complex>(bC,mask2),
	       SubLattice<Complex>(cC,mask3),
	       SubLattice<DComplex>(bDC,mask2),
	       SubLattice<DComplex>(cDC,mask3),
	       SubLattice<bool>(aB,mask1),
	       SubLattice<bool>(bB,mask2),
	       SubLattice<bool>(cB,mask3),
	       bFVal,cFVal,bDVal,cDVal,bCVal,cCVal,
	       bDCVal,cDCVal,aBVal,bBVal,cBVal,
	       0)) {
	ok = false;
    }


 } catch (std::exception& x) {
    cerr << "aipserror: error " << x.what() << endl;
    ok = false;
 } 
 
  if (!ok) {
    return 1;
  }
  cout << endl << "ok" << endl;
  return 0;
}
