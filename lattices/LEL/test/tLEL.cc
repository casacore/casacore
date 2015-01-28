//# tLEL.cc:  Tests the LEL* classes directly
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
//#
//# $Id$


#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LEL/LELAttribute.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/LEL/LELScalar.h>
#include <casacore/lattices/LEL/LELBinary.h>
#include <casacore/lattices/LEL/LELConvert.h>
#include <casacore/lattices/LEL/LELFunction.h>
#include <casacore/lattices/LEL/LELLattice.h>
#include <casacore/lattices/LEL/LELUnary.h>
#include <casacore/lattices/LEL/LELLattCoord.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
Bool checkAttribute (const LELAttribute& attr,
		     const Bool isMasked,
                     const Bool isScalar,
                     const IPosition& shape,
                     const IPosition& tileShape,
                     const LELCoordinates& lattCoord);

Bool checkFloat (LELInterface<Float>& expr, 
                 const Float Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress);

Bool checkDouble (LELInterface<Double>& expr, 
                 const Double Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress);

Bool checkComplex (LELInterface<Complex>& expr, 
                 const Complex& Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress);

Bool checkDComplex (LELInterface<DComplex>& expr, 
                 const DComplex& Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress);

Bool checkBool (LELInterface<Bool>& expr, 
                 const Bool Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress,
		 const Bool emptyShape=False);


int main (int argc, const char* argv[])
{
 try {
    cout << ">>>" << endl;
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "2", "Number of pixels along the x-axis", "int");
    inp.create("ny", "2", "Number of pixels along the y-axis", "int");
    inp.create("sup", "False", "Suppress expected exception messages", "Bool");
    inp.readArguments(argc, argv);
    cout << "<<<" << endl;

    const uInt nx=inp.getInt("nx");
    const uInt ny=inp.getInt("ny");
    const Bool suppress =inp.getBool("sup");

//
// The use of these tiny ArrayLattices means this test program
// does not computationally stress the classes.  Here we just
// test them logically.  See other test programs for stress tests
//

    IPosition shape(2,nx,ny);

// Bool Lattices

    Bool BResult;
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

    Array<Float> FArr(shape);
    Float FResult;
    ArrayLattice<Float> aF(shape);
    ArrayLattice<Float> bF(shape);
    ArrayLattice<Float> cF(shape);
    ArrayLattice<Float> nanF(shape);
    Float aFVal = 0.0;
    aF.set(aFVal);
    Float bFVal = 2.0;
    bF.set(bFVal);
    Float cFVal = 3.0;
    cF.set(cFVal);
    Float nanFVal;
    setNaN(nanFVal);
    nanF.set(nanFVal);


// Double Lattices

    Array<Double> DArr(shape);
    Double DResult;
    ArrayLattice<Double> aD(shape);
    ArrayLattice<Double> bD(shape);
    ArrayLattice<Double> cD(shape);
    Double aDVal = 0.0;
    aD.set(aDVal);
    Double bDVal = 2.0;
    bD.set(bDVal);
    Double cDVal = 3.0;
    cD.set(cDVal);


// Complex Lattices

    Array<Complex> CArr(shape);
    Complex CResult;
    ArrayLattice<Complex> aC(shape);
    ArrayLattice<Complex> bC(shape);
    ArrayLattice<Complex> cC(shape);
    Complex aCVal = Complex(0.0,0.0);
    aC.set(aCVal);
    Complex bCVal = Complex(2.0,2.0);
    bC.set(bCVal);
    Complex cCVal = Complex(3.0,3.0);
    cC.set(cCVal);


// DComplex Lattices

    Array<DComplex> DCArr(shape);
    DComplex DCResult;
    ArrayLattice<DComplex> aDC(shape);
    ArrayLattice<DComplex> bDC(shape);
    ArrayLattice<DComplex> cDC(shape);
    DComplex aDCVal = DComplex(0.0,0.0);
    aDC.set(aDCVal);
    DComplex bDCVal = DComplex(2.0,2.0);
    bDC.set(bDCVal);
    DComplex cDCVal = DComplex(3.0,3.0);
    cDC.set(cDCVal);

    Bool ok = True;


//************************************************************************
// 
// LELAttribute
//
  {
    cout << "LELAttribute" << endl;

// First a scalar attribute

    const IPosition nullIPos = IPosition();
    LELAttribute attr1;
    LELCoordinates lattCoord2 (new LELLattCoord());
    if (!checkAttribute(attr1, False, True, nullIPos, nullIPos,
			lattCoord2)) ok = False;

// Now a non-scalar one; this only tests null LELCoordinates

    Bool isScalar2 = False;
    IPosition shape2 = shape;
    IPosition tileShape2 = shape;
    LELAttribute attr2(True, shape2, tileShape2, lattCoord2);
    if (!checkAttribute(attr2, True, isScalar2, shape2, tileShape2,
			lattCoord2)) ok = False;

    LELAttribute attr3 = attr2;
    if (!checkAttribute(attr3, attr2.isMasked(), attr2.isScalar(),
			attr2.shape(), attr2.tileShape(),
                        attr2.coordinates())) {
      cout << "   Assignment failed" << endl;
      ok = False;
    }    

// Result of scalar and non-scalar is non-scalar

    LELAttribute attr4(attr1, attr2);
    if (!checkAttribute(attr4, attr2.isMasked(), attr2.isScalar(),
			attr2.shape(), attr2.tileShape(),
                        attr2.coordinates())) {
      cout << "   binary constructor failed" << endl;
      ok = False;
    }    

  }       
       

//************************************************************************
//
// LELLattice
//
  {
    cout << endl << "LELLattice<Float> " << endl;
    LELLattice<Float> expr(bF);
    FResult = bFVal;
    if (!checkFloat (expr, FResult, String("LELLattice"), shape, False, suppress)) ok = False;
  }
  {
    cout << "LELLattice<Double> " << endl;
    LELLattice<Double> expr(bD);
    DResult = bDVal;
    if (!checkDouble(expr, DResult, String("LELLattice"), shape, False, suppress)) ok = False;
  }
  {
    cout << "LELLattice<Complex> " << endl;
    LELLattice<Complex> expr(bC);
    CResult = bCVal;
    if (!checkComplex(expr, CResult, String("LELLattice"), shape, False, suppress)) ok = False;
  }
  {
    cout << "LELLattice<DComplex> " << endl;
    LELLattice<DComplex> expr(bDC);
    DCResult = bDCVal;
    if (!checkDComplex(expr, DCResult, String("LELLattice"), shape, False, suppress)) ok = False;
  }
  {
    cout << "LELLattice<Bool> " << endl;
    LELLattice<Bool> expr(bB);
    BResult = bBVal;
    if (!checkBool(expr, BResult, String("LELLattice"), shape, False, suppress)) ok = False;
  }

//************************************************************************
//
// LELUnaryConst
//
  {

    cout << endl << "LELUnaryConst<Float>" << endl;
    LELUnaryConst<Float> expr(aFVal);
    if (!checkFloat (expr, aFVal, String("LELUnaryConst"), shape, True, suppress)) ok = False;

  }
  {
    cout << "LELUnaryConst<Double>" << endl;
    LELUnaryConst<Double> expr(aDVal);
    if (!checkDouble(expr, aDVal, String("LELUnaryConst"), shape, True, suppress)) ok = False;
  }
  {
    cout << "LELUnaryConst<Complex>" << endl;
    
    LELUnaryConst<Complex> expr(aCVal);
    if (!checkComplex(expr, aCVal, String("LELUnaryConst"), shape, True, suppress)) ok = False;
  }
  {
    cout << "LELUnaryConst<DComplex>" << endl;
    LELUnaryConst<DComplex> expr(aDCVal);
    if (!checkDComplex(expr, aDCVal, String("LELUnaryConst"), shape, True, suppress)) ok = False;
  }
  {
    cout << "LELUnaryConst<Bool>" << endl;
    LELUnaryConst<Bool> expr(aBVal);
    if (!checkBool(expr, aBVal, String("LELUnaryConst"), shape, True, suppress)) ok = False;
  }

//
//************************************************************************
//
// LELUnary
//
   cout << endl << "LELUnary<Float>" << endl;
  {
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    LELUnary<Float> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkFloat (expr, -bFVal, String("LELUnary"), shape, False, suppress)) ok = False;
  }

   cout << "LELUnary<Double>" << endl;
  {

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    CountedPtr<LELInterface<Double> > pExpr = new LELLattice<Double>(bD);
    LELUnary<Double> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkDouble(expr, -bDVal, String("LELUnary"), shape, False, suppress)) ok = False;
  }


   cout << "LELUnary<Complex>" << endl;
  {

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    CountedPtr<LELInterface<Complex> > pExpr = new LELLattice<Complex>(bC);
    LELUnary<Complex> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkComplex(expr, -bCVal, String("LELUnary"), shape, False, suppress)) ok = False;
  }


   cout << "LELUnary<DComplex>" << endl;
  {

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    CountedPtr<LELInterface<DComplex> > pExpr = new LELLattice<DComplex>(bDC);
    LELUnary<DComplex> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkDComplex(expr, -bDCVal, String("LELUnary"), shape, False, suppress)) ok = False;
  }

//************************************************************************
//
// LELUnaryBool
//
  {
    cout << endl << "LELUnaryBool" << endl;

    {
      cout << "   Operator !" << endl;     
      CountedPtr<LELInterface<Bool> > pExpr = new LELLattice<Bool>(aB);
      LELUnaryBool expr(LELUnaryEnums::NOT, pExpr);
      if (!checkBool(expr, (!aBVal), String("LELUnaryBool"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinary<Float>
//
  {
    cout << endl << "LELBinary<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExprLeft = new LELLattice<Float>(bF);
    CountedPtr<LELInterface<Float> > pExprRight = new LELLattice<Float>(cF);

    {
    cout << "   Operator +" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::ADD, pExprLeft, pExprRight);
    FResult = bFVal + cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    FResult = bFVal - cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    FResult = bFVal * cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    FResult = bFVal / cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }
  }
//
//
//************************************************************************
//
// LELBinary<Double>
//
  {
    cout << endl << "LELBinary<Double>" << endl;
    CountedPtr<LELInterface<Double> > pExprLeft = new LELLattice<Double>(bD);
    CountedPtr<LELInterface<Double> > pExprRight = new LELLattice<Double>(cD);

    {
    cout << "   Operator +" << endl;     
    LELBinary<Double> expr(LELBinaryEnums::ADD, pExprLeft, pExprRight);
    DResult = bDVal + cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<Double> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    DResult = bDVal - cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<Double> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    DResult = bDVal * cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<Double> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    DResult = bDVal / cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinary<Complex>
//
  {
    cout << endl << "LELBinary<Complex>" << endl;
    CountedPtr<LELInterface<Complex> > pExprLeft = new LELLattice<Complex>(bC);
    CountedPtr<LELInterface<Complex> > pExprRight = new LELLattice<Complex>(cC);

    {
    cout << "   Operator +" << endl;     
    LELBinary<Complex> expr(LELBinaryEnums::ADD, pExprLeft, pExprRight);
    CResult = bCVal + cCVal;
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<Complex> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    CResult = bCVal - cCVal;
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<Complex> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    CResult = bCVal * cCVal;
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<Complex> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    CResult = bCVal / cCVal;
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinary<DComplex>
//
  {
    cout << endl << "LELBinary<DComplex>" << endl;
    CountedPtr<LELInterface<DComplex> > pExprLeft = new LELLattice<DComplex>(bDC);
    CountedPtr<LELInterface<DComplex> > pExprRight = new LELLattice<DComplex>(cDC);

    {
    cout << "   Operator +" << endl;     
    LELBinary<DComplex> expr(LELBinaryEnums::ADD, pExprLeft, pExprRight);
    DCResult = bDCVal + cDCVal;
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<DComplex> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    DCResult = bDCVal - cDCVal;
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<DComplex> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    DCResult = bDCVal * cDCVal;
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<DComplex> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    DCResult = bDCVal / cDCVal;
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinaryCmp<Float>
//
  {
    cout << endl << "LELBinaryCmp<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExprLeft = new LELLattice<Float>(bF);
    CountedPtr<LELInterface<Float> > pExprRight = new LELLattice<Float>(cF);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bFVal==cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bFVal!=cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bFVal>cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bFVal>=cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinaryCmp<Double>
//
  {
    cout << endl << "LELBinaryCmp<Double>" << endl;
    CountedPtr<LELInterface<Double> > pExprLeft = new LELLattice<Double>(bD);
    CountedPtr<LELInterface<Double> > pExprRight = new LELLattice<Double>(cD);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryCmp<Double> expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bDVal==cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<Double> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bDVal!=cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<Double> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bDVal>cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<Double> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bDVal>=cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinaryCmp<Complex>
//
  {
    cout << endl << "LELBinaryCmp<Complex>" << endl;
    CountedPtr<LELInterface<Complex> > pExprLeft = new LELLattice<Complex>(bC);
    CountedPtr<LELInterface<Complex> > pExprRight = new LELLattice<Complex>(cC);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryCmp<Complex> expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bCVal==cCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<Complex> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bCVal!=cCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<Complex> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bCVal>cCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<Complex> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bCVal>=cCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinaryCmp<DComplex>
//
  {
    cout << endl << "LELBinaryCmp<DComplex>" << endl;
    CountedPtr<LELInterface<DComplex> > pExprLeft = new LELLattice<DComplex>(bDC);
    CountedPtr<LELInterface<DComplex> > pExprRight = new LELLattice<DComplex>(cDC);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryCmp<DComplex> expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bDCVal==cDCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<DComplex> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bDCVal!=cDCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<DComplex> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bDCVal>cDCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<DComplex> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bDCVal>=cDCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELBinaryBool
//
  {
    cout << endl << "LELBinaryBool" << endl;
    CountedPtr<LELInterface<Bool> > pExprLeft = new LELLattice<Bool>(bB);
    CountedPtr<LELInterface<Bool> > pExprRight = new LELLattice<Bool>(cB);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryBool expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bBVal==cBVal);
    if (!checkBool(expr, BResult, String("LELBinaryBool"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryBool expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bBVal!=cBVal);
    if (!checkBool(expr, BResult, String("LELBinaryBool"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Operator &&" << endl;     
    LELBinaryBool expr(LELBinaryEnums::AND, pExprLeft, pExprRight);
    BResult = (bBVal&&cBVal);
    if (!checkBool(expr, BResult, String("LELBinaryBool"), shape, False, suppress)) ok = False;
    }
  }

//
//************************************************************************
//
// LELFunction1D<Float>
//
  {
    cout << endl << "LELFunction1D<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);

    {
    cout << "   Function sin" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SIN, pExpr);
    FResult = sin(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SINH, pExpr);
    FResult = sinh(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::COS, pExpr);
    FResult = cos(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::COSH, pExpr);
    FResult = cosh(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::EXP, pExpr);
    FResult = exp(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::LOG, pExpr);
    FResult = log(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::LOG10, pExpr);
    FResult = log10(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SQRT, pExpr);
    FResult = sqrt(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::MIN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = min(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::MAX1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = max(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function median" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::MEDIAN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = median(FArr);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function mean" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::MEAN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = mean(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function sum" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SUM, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = sum(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }
  }

//
//
//************************************************************************
//
// LELFunction1D<Double>
//
  {
    cout << endl << "LELFunction1D<Double>" << endl;
    CountedPtr<LELInterface<Double> > pExpr = new LELLattice<Double>(bD);

    {
    cout << "   Function sin" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::SIN, pExpr);
    DResult = sin(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::SINH, pExpr);
    DResult = sinh(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::COS, pExpr);
    DResult = cos(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::COSH, pExpr);
    DResult = cosh(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::EXP, pExpr);
    DResult = exp(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::LOG, pExpr);
    DResult = log(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::LOG10, pExpr);
    DResult = log10(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::SQRT, pExpr);
    DResult = sqrt(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::MIN1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = min(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::MAX1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = max(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function median" << endl;     
    LELFunctionReal1D<Double> expr(LELFunctionEnums::MEDIAN1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = median(DArr);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function mean" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::MEAN1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = mean(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function sum" << endl;     
    LELFunction1D<Double> expr(LELFunctionEnums::SUM, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = sum(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }
  }

//
//
//************************************************************************
//
// LELFunction1D<Complex>
//
  {
    cout << endl << "LELFunction1D<Complex>" << endl;
    CountedPtr<LELInterface<Complex> > pExpr = new LELLattice<Complex>(bC);

    {
    cout << "   Function sin" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::SIN, pExpr);
    CResult = sin(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::SINH, pExpr);
    CResult = sinh(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::COS, pExpr);
    CResult = cos(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::COSH, pExpr);
    CResult = cosh(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::EXP, pExpr);
    CResult = exp(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::LOG, pExpr);
    CResult = log(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::LOG10, pExpr);
    CResult = log10(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::SQRT, pExpr);
    CResult = sqrt(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::MIN1D, pExpr);
    bC.getSlice(CArr, IPosition(CArr.ndim(),0), 
                CArr.shape(), IPosition(CArr.ndim(),1));
    CResult = min(CArr);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::MAX1D, pExpr);
    bC.getSlice(CArr, IPosition(CArr.ndim(),0), 
                CArr.shape(), IPosition(CArr.ndim(),1));
    CResult = max(CArr);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }
  }

//
//
//************************************************************************
//
// LELFunction1D<DComplex>
//
  {
    cout << endl << "LELFunction1D<DComplex>" << endl;
    CountedPtr<LELInterface<DComplex> > pExpr = new LELLattice<DComplex>(bDC);

    {
    cout << "   Function sin" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::SIN, pExpr);
    DCResult = sin(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::SINH, pExpr);
    DCResult = sinh(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::COS, pExpr);
    DCResult = cos(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::COSH, pExpr);
    DCResult = cosh(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::EXP, pExpr);
    DCResult = exp(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::LOG, pExpr);
    DCResult = log(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::LOG10, pExpr);
    DCResult = log10(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::SQRT, pExpr);
    DCResult = sqrt(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::MIN1D, pExpr);
    bDC.getSlice(DCArr, IPosition(DCArr.ndim(),0), 
                DCArr.shape(), IPosition(DCArr.ndim(),1));
    DCResult = min(DCArr);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::MAX1D, pExpr);
    bDC.getSlice(DCArr, IPosition(DCArr.ndim(),0), 
                DCArr.shape(), IPosition(DCArr.ndim(),1));
    DCResult = max(DCArr);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, True, suppress)) ok = False;
    }

  }

//
//************************************************************************
//
// LELFunctionND<Float>
//
  {
    cout << endl << "LELFunctionND<Float>" << endl;


    cout << "   Function iif" << endl;     
    {

    cout << "     Scalar, scalar, scalar" << endl;

    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<Float> expr1(LELFunctionEnums::IIF, arga);
    FResult = bFVal;
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Float> expr2(LELFunctionEnums::IIF, arga);
    FResult = cFVal;
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cF);
    LELFunctionND<Float> expr1(LELFunctionEnums::IIF, arga);
    FResult = bFVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Float> expr2(LELFunctionEnums::IIF, arga);
    FResult = cFVal;
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bF);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<Float> expr1(LELFunctionEnums::IIF, arga);
    FResult = bFVal;
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Float> expr2(LELFunctionEnums::IIF, arga);
    FResult = cFVal;
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }

    {

    cout << "     Array, scalar, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<Float> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Float> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, Array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bF);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<Float> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Float> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cF);
    LELFunctionND<Float> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Float> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }

  }


//
//************************************************************************
//
// LELFunctionND<Double>
//
  {
    cout << endl << "LELFunctionND<Double>" << endl;


    cout << "   Function iif" << endl;     
    {

    cout << "     Scalar, scalar, scalar" << endl;

    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<Double> expr1(LELFunctionEnums::IIF, arga);
    DResult = bDVal;
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Double> expr2(LELFunctionEnums::IIF, arga);
    DResult = cDVal;
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cD);
    LELFunctionND<Double> expr1(LELFunctionEnums::IIF, arga);
    DResult = bDVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Double> expr2(LELFunctionEnums::IIF, arga);
    DResult = cDVal;
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bD);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<Double> expr1(LELFunctionEnums::IIF, arga);
    DResult = bDVal;
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Double> expr2(LELFunctionEnums::IIF, arga);
    DResult = cDVal;
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }

    {

    cout << "     Array, scalar, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<Double> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Double> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, Array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bD);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<Double> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Double> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cD);
    LELFunctionND<Double> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Double> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
  }


//
//************************************************************************
//
// LELFunctionND<Complex>
//
  {
    cout << endl << "LELFunctionND<Complex>" << endl;


    cout << "   Function iif" << endl;     
    {

    cout << "     Scalar, scalar, scalar" << endl;

    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bCVal);
    arga[2] = LatticeExprNode(cCVal);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    CResult = bCVal;
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    CResult = cCVal;
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bCVal);
    arga[2] = LatticeExprNode(cC);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    CResult = bCVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    CResult = cCVal;
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bC);
    arga[2] = LatticeExprNode(cCVal);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    CResult = bCVal;
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    CResult = cCVal;
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }

    {

    cout << "     Array, scalar, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bCVal);
    arga[2] = LatticeExprNode(cCVal);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, Array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bC);
    arga[2] = LatticeExprNode(cCVal);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bCVal);
    arga[2] = LatticeExprNode(cC);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
  }



//
//************************************************************************
//
// LELFunctionND<DComplex>
//
  {
    cout << endl << "LELFunctionND<DComplex>" << endl;


    cout << "   Function iif" << endl;     
    {

    cout << "     Scalar, scalar, scalar" << endl;

    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bDCVal);
    arga[2] = LatticeExprNode(cDCVal);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    DCResult = bDCVal;
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    DCResult = cDCVal;
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, True, suppress)) ok = False;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bDCVal);
    arga[2] = LatticeExprNode(cDC);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    DCResult = bDCVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    DCResult = cDCVal;
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(True);
    arga[1] = LatticeExprNode(bDC);
    arga[2] = LatticeExprNode(cDCVal);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    DCResult = bDCVal;
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(False);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    DCResult = cDCVal;
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }

    {

    cout << "     Array, scalar, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bDCVal);
    arga[2] = LatticeExprNode(cDCVal);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, Array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bDC);
    arga[2] = LatticeExprNode(cDCVal);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
    {

    cout << "     Array, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bDCVal);
    arga[2] = LatticeExprNode(cDC);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, False, suppress)) ok = False;

    }
  }



//
//************************************************************************
//
// LELFunctionReal1D<Float>
//
  {
    cout << endl << "LELFunctionReal1D<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);
    CountedPtr<LELInterface<Float> > pExpra = new LELLattice<Float>(aF);

    {
    cout << "   Function asin" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::ASIN, pExpra);
    FResult = asin(aFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function acos" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::ACOS, pExpra);
    FResult = acos(aFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Function tan" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::TAN, pExpr);
    FResult = tan(bFVal);    
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function tanh" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::TANH, pExpr);
    FResult = tanh(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function ceil" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::CEIL, pExpr);
    FResult = ceil(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function floor" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::FLOOR, pExpr);
    FResult = floor(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }
  }

//
//************************************************************************
//
// LELFunctionReal1D<Double>
//
  {
    cout << endl << "LELFunctionReal1D<Double>" << endl;
    CountedPtr<LELInterface<Double> > pExpr = new LELLattice<Double>(bD);
    CountedPtr<LELInterface<Double> > pExpra = new LELLattice<Double>(aD);

    {
    cout << "   Function asin" << endl;     
    LELFunctionReal1D<Double> expr(LELFunctionEnums::ASIN, pExpra);
    DResult = asin(aDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function acos" << endl;     
    LELFunctionReal1D<Double> expr(LELFunctionEnums::ACOS, pExpra);
    DResult = acos(aDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Function tan" << endl;     
    LELFunctionReal1D<Double> expr(LELFunctionEnums::TAN, pExpr);
    DResult = tan(bDVal);    
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function tanh" << endl;     
    LELFunctionReal1D<Double> expr(LELFunctionEnums::TANH, pExpr);
    DResult = tanh(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function ceil" << endl;     
    LELFunctionReal1D<Double> expr(LELFunctionEnums::CEIL, pExpr);
    DResult = ceil(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function floor" << endl;     
    LELFunctionReal1D<Double> expr(LELFunctionEnums::FLOOR, pExpr);
    DResult = floor(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELFunctionFloat
//
  {
    cout << endl << "LELFunctionFloat" << endl;

    Block<LatticeExprNode> arga(2);
    arga[0] = LatticeExprNode(bF);
    arga[1] = LatticeExprNode(cF);


    {
    cout << "   Function min" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::MIN, arga);
    FResult = min(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Function max" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::MAX, arga);
    FResult = max(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function pow" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::POW, arga);
    FResult = pow(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function atan2" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ATAN2, arga);
    FResult = atan2(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function fmod" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::FMOD, arga);
    FResult = fmod(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bC);

    {
    cout << "   Function abs" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ABS, argb);
    FResult = abs(bCVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function arg" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ARG, argb);
    FResult = Float(arg(bCVal));
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function real" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::REAL, argb);
    FResult = real(bCVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }

    {
      cout << "   Function imag" << endl;
    LELFunctionFloat expr(LELFunctionEnums::IMAG, argb);
    FResult = imag(bCVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function fractile" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bF);
    arg[1] = LatticeExprNode(Float(0.5));
    LELFunctionFloat expr(LELFunctionEnums::FRACTILE1D, arg);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = fractile(FArr, 0.5);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function fractilerange 2" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bF);
    arg[1] = LatticeExprNode(Float(0.2));
    LELFunctionFloat expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = fractile(FArr, 0.8) - fractile(FArr, 0.2);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function fractilerange 3" << endl;
    Block<LatticeExprNode> arg(3);
    arg[0] = LatticeExprNode(bF);
    arg[1] = LatticeExprNode(Float(0.2));
    arg[2] = LatticeExprNode(Float(0.7));
    LELFunctionFloat expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = fractile(FArr, 0.7) - fractile(FArr, 0.2);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, True, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELFunctionDouble
//
  {
    cout << endl << "LELFunctionDouble" << endl;

    Block<LatticeExprNode> arga(2);
    arga[0] = LatticeExprNode(bD);
    arga[1] = LatticeExprNode(cD);

    {
    cout << "   Function min" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::MIN, arga);
    DResult = min(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }


    {
    cout << "   Function max" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::MAX, arga);
    DResult = max(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function pow" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::POW, arga);
    DResult = pow(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function atan2" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::ATAN2, arga);
    DResult = atan2(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function fmod" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::FMOD, arga);
    DResult = fmod(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bDC);

    {
    cout << "   Function abs" << endl;     
    DResult = abs(bDCVal);
    LELFunctionDouble expr(LELFunctionEnums::ABS, argb);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function arg" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::ARG, argb);
    DResult = Double(arg(bDCVal));
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function real" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::REAL, argb);
    DResult = real(bDCVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }

    {
    cout << "   Function imag" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::IMAG, argb);
    DResult = imag(bDCVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, False, suppress)) ok = False;
    }


    Block<LatticeExprNode> argc(1);
    argc[0] = LatticeExprNode(bB);
    {
    cout << "   Function ntrue" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::NTRUE, argc);
    if (bBVal) {
      DResult = shape.product();
    } else {
      DResult = 0.0;
    }
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, True, suppress)) ok = False;
    }


    {
    cout << "   Function nfalse" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::NFALSE, argc);
    if (!bBVal) {
      DResult = shape.product();
    } else {
      DResult = 0.0;
    }
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function nelements" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::NELEM, argc);
    DResult = shape.product();
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function fractile" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bD);
    arg[1] = LatticeExprNode(Float(0.5));
    LELFunctionDouble expr(LELFunctionEnums::FRACTILE1D, arg);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = fractile(DArr, 0.5);
    if (!checkDouble (expr, DResult, String("LELFunctionDouble"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function fractilerange 2" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bD);
    arg[1] = LatticeExprNode(Float(0.2));
    LELFunctionDouble expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = fractile(DArr, 0.8) - fractile(DArr, 0.2);
    if (!checkDouble (expr, DResult, String("LELFunctionDouble"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function fractilerange 3" << endl;
    Block<LatticeExprNode> arg(3);
    arg[0] = LatticeExprNode(bD);
    arg[1] = LatticeExprNode(Float(0.2));
    arg[2] = LatticeExprNode(Float(0.7));
    LELFunctionDouble expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = fractile(DArr, 0.7) - fractile(DArr, 0.2);
    if (!checkDouble (expr, DResult, String("LELFunctionDouble"), shape, True, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELFunctionComplex
//
  {
    cout << endl << "LELFunctionComplex" << endl;

    Block<LatticeExprNode> arga(2);
    arga[0] = LatticeExprNode(bC);
    arga[1] = LatticeExprNode(cC);


    {
    cout << "   Function pow" << endl;     
    LELFunctionComplex expr(LELFunctionEnums::POW, arga);
    CResult = pow(bCVal,cCVal);
    if (!checkComplex(expr, CResult, String("LELFunctionComplex"), shape, False, suppress)) ok = False;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bC);

    {
    cout << "   Function conj" << endl;     
    LELFunctionComplex expr(LELFunctionEnums::CONJ, argb);
    CResult = conj(bCVal);
    if (!checkComplex(expr, CResult, String("LELFunctionComplex"), shape, False, suppress)) ok = False;
    }

  }
//
//************************************************************************
//
// LELFunctionDComplex
//
  {
    cout << endl << "LELFunctionDComplex" << endl;

    Block<LatticeExprNode> arga(2);
    arga[0] = LatticeExprNode(bDC);
    arga[1] = LatticeExprNode(cDC);


    {
    cout << "   Function pow" << endl;     
    LELFunctionDComplex expr(LELFunctionEnums::POW, arga);
    DCResult = pow(bDCVal,cDCVal);
    if (!checkDComplex(expr, DCResult, String("LELFunctionDComplex"), shape, False, suppress)) ok = False;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bDC);

    {
    cout << "   Function conj" << endl;     
    LELFunctionDComplex expr(LELFunctionEnums::CONJ, argb);
    DCResult = conj(bDCVal);
    if (!checkDComplex(expr, DCResult, String("LELFunctionDComplex"), shape, False, suppress)) ok = False;
    }
  }
//
//************************************************************************
//
// LELFunctionBool
//
  {
    cout << endl << "LELFunctionBool" << endl;

    Block<LatticeExprNode> arga(1);
    arga[0] = LatticeExprNode(bB);


    {
    cout << "   Function all" << endl;     
    LELFunctionBool expr(LELFunctionEnums::ALL, arga);
    BResult = bBVal;
    if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, True, suppress)) ok = False;
    }

    {
    cout << "   Function any" << endl;     
    LELFunctionBool expr(LELFunctionEnums::ANY, arga);
    BResult = bBVal;
    if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, True, suppress)) ok = False;
    }

    {
    Block<LatticeExprNode> argb(1);
    cout << "   Function isNaN" << endl;     
    {
      argb[0] = LatticeExprNode(bF);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = False;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, False, suppress)) ok = False;
    }
    {
      argb[0] = LatticeExprNode(nanF);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = True;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, False, suppress)) ok = False;
    }
    {
      argb[0] = LatticeExprNode(bD);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = False;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, False, suppress)) ok = False;
    }
    {
      argb[0] = LatticeExprNode(bC);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = False;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, False, suppress)) ok = False;
    }
    {
      argb[0] = LatticeExprNode(bDC);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = False;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, False, suppress)) ok = False;
    }
    }

    {
    Block<LatticeExprNode> argb(2);
    cout << "   Function indexin" << endl;     
    {
      Vector<Bool> flags(2,True);
      argb[0] = LatticeExprNode(0);
      argb[1] = LatticeExprNode(ArrayLattice<Bool>(flags));
      LELFunctionBool expr(LELFunctionEnums::INDEXIN, argb);
      BResult = True;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,2,4), False, suppress, True)) ok = False;
    }
    {
      Vector<Bool> flags(2,True);
      argb[0] = LatticeExprNode(1);
      argb[1] = LatticeExprNode(ArrayLattice<Bool>(flags));
      LELFunctionBool expr(LELFunctionEnums::INDEXIN, argb);
      BResult = True;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,10,2), False, suppress, True)) ok = False;
    }
    {
      Vector<Bool> flags(3,False);
      argb[0] = LatticeExprNode(0);
      argb[1] = LatticeExprNode(ArrayLattice<Bool>(flags));
      LELFunctionBool expr(LELFunctionEnums::INDEXIN, argb);
      BResult = False;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,6,2), False, suppress, True)) ok = False;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,2,6), False, suppress, True)) ok = False;
    }
    }

  }
//
//************************************************************************
//
// LELConvert
//
  {

    {
    cout << endl << "LELConvert<Float,Double> " << endl;
    CountedPtr<LELInterface<Double> > pExpr = new LELLattice<Double>(bD);
    LELConvert<Float,Double> expr(pExpr);
    FResult = Float(bDVal);
    if (!checkFloat (expr, FResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }

    {
    cout << "LELConvert<Double,Float> " << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);
    LELConvert<Double,Float> expr(pExpr);
    DResult = Double(bFVal);
    if (!checkDouble(expr, DResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }

    {
    cout << "LELConvert<Complex,DComplex> " << endl;
    CountedPtr<LELInterface<DComplex> > pExpr = new LELLattice<DComplex>(bDC);
    LELConvert<Complex,DComplex> expr(pExpr);
    CResult = Complex(bDCVal.real(), bDCVal.imag());
    if (!checkComplex(expr, CResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }

    {
    cout << "LELConvert<DComplex,Complex> " << endl;
    CountedPtr<LELInterface<Complex> > pExpr = new LELLattice<Complex>(bC);
    LELConvert<DComplex,Complex> expr(pExpr);
    DCResult = bCVal;
    if (!checkDComplex(expr, DCResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }


    {
    cout << "LELConvert<Complex,Float> " << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);
    LELConvert<Complex,Float> expr(pExpr);
    CResult = Complex(bFVal,0.0);
    if (!checkComplex(expr, CResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }

    {
    cout << "LELConvert<Complex,Double> " << endl;
    CountedPtr<LELInterface<Double> > pExpr = new LELLattice<Double>(bD);
    LELConvert<Complex,Double> expr(pExpr);
    CResult = Complex(bDVal,0.0);
    if (!checkComplex(expr, CResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }

    {
    cout << "LELConvert<DComplex,Float> " << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);
    LELConvert<DComplex,Float> expr(pExpr);
    DCResult = DComplex(bFVal,0.0);
    if (!checkDComplex(expr, DCResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }

    {
    cout << "LELConvert<DComplex,Double> " << endl;
    CountedPtr<LELInterface<Double> > pExpr = new LELLattice<Double>(bD);
    LELConvert<DComplex,Double> expr(pExpr);
    DCResult = DComplex(bDVal,0.0);
    if (!checkDComplex(expr, DCResult, String("LELConvert"), shape, False, suppress)) ok = False;
    }
  }


  if (!ok) {
    cout << "not ok" << endl;
     return 1;
  } else {
    cout << endl << "ok" << endl;
  }

 } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    return 1;
 } 
 
   return 0;
}


Bool checkFloat (LELInterface<Float>& expr, 
                 const Float Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress)
{
    LELArray<Float> Arr(shape);
    Bool ok = True;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = False;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = False;
      }
      try {
        expr.eval(Arr, region);
      } catch (AipsError x) {
        if (!suppress) cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = False;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = False;
      }
      try {
       expr.getScalar();
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



Bool checkDouble (LELInterface<Double>& expr, 
                 const Double Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress)
{
    LELArray<Double> Arr(shape);
    Bool ok = True;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = False;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = False;
      }
      try {
        expr.eval(Arr, region);
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = False;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = False;
      }
      try {
       expr.getScalar();
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



Bool checkComplex (LELInterface<Complex>& expr, 
                 const Complex& Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress)
{
    LELArray<Complex> Arr(shape);
    Bool ok = True;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = False;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = False;
      }
      try {
        expr.eval(Arr, region);
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = False;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = False;
      }
      try {
       expr.getScalar();
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



Bool checkDComplex (LELInterface<DComplex>& expr, 
                 const DComplex& Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress)
{
    LELArray<DComplex> Arr(shape);
    Bool ok = True;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = False;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = False;
      }
      try {
        expr.eval(Arr, region);
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = False;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = False;
      }
      try {
       expr.getScalar();
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



Bool checkBool (LELInterface<Bool>& expr, 
                 const Bool Result,
                 const String& name,
                 const IPosition& shape,
                 const Bool shouldBeScalar,
                 const Bool suppress,
		 const Bool emptyShape)
{
    LELArray<Bool> Arr(shape);
    Bool ok = True;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = False;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = False;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = False;
      }
      try {
        expr.eval(Arr, region);
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = False;
      }
      if (emptyShape) {
	if (expr.shape() != IPosition()) {
	  cout << "   Expression has no empty shape" << endl;
	  ok = False;
	}
      } else {
	if (expr.shape() != shape) {
	  cout << "   Expression has wrong shape" << endl;
	  ok = False;
	}
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = False;
      }
      try {
       expr.getScalar();
      } catch (AipsError x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.getMesg() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}
Bool checkAttribute (const LELAttribute& attr,
		     const Bool isMasked,
                     const Bool isScalar,
                     const IPosition& shape,
                     const IPosition& tileShape,
                     const LELCoordinates& lattCoord)
{
   Bool ok = True;
   if (attr.isMasked() != isMasked) {
      cout << "   isMasked function failed" << endl;
      ok = False;
   }
   if (attr.isScalar() != isScalar) {
      cout << "   isScalar function failed" << endl;
      ok = False;
   }
   if (attr.shape() != shape) {
      cout << "   shape function failed" << endl;
      ok = False;
   }    
   if (attr.tileShape() != tileShape) {
      cout << "   tileShape function failed" << endl;
      ok = False;
   }    
   if (attr.coordinates().compare(lattCoord) != 0) {
      cout << "   coordinates function failed" << endl;
      ok = False;
   }
   return ok;
}

