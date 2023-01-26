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
bool checkAttribute (const LELAttribute& attr,
		     const bool isMasked,
                     const bool isScalar,
                     const IPosition& shape,
                     const IPosition& tileShape,
                     const LELCoordinates& lattCoord);

bool checkFloat (LELInterface<float>& expr, 
                 const float Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress);

bool checkDouble (LELInterface<double>& expr, 
                 const double Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress);

bool checkComplex (LELInterface<Complex>& expr, 
                 const Complex& Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress);

bool checkDComplex (LELInterface<DComplex>& expr, 
                 const DComplex& Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress);

bool checkBool (LELInterface<bool>& expr, 
                 const bool Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress,
		 const bool emptyShape=false);


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

    const uint32_t nx=inp.getInt("nx");
    const uint32_t ny=inp.getInt("ny");
    const bool suppress =inp.getBool("sup");

//
// The use of these tiny ArrayLattices means this test program
// does not computationally stress the classes.  Here we just
// test them logically.  See other test programs for stress tests
//

    IPosition shape(2,nx,ny);

// bool Lattices

    bool BResult;
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

    Array<float> FArr(shape);
    float FResult;
    ArrayLattice<float> aF(shape);
    ArrayLattice<float> bF(shape);
    ArrayLattice<float> cF(shape);
    ArrayLattice<float> nanF(shape);
    float aFVal = 0.0;
    aF.set(aFVal);
    float bFVal = 2.0;
    bF.set(bFVal);
    float cFVal = 3.0;
    cF.set(cFVal);
    float nanFVal;
    setNaN(nanFVal);
    nanF.set(nanFVal);


// double Lattices

    Array<double> DArr(shape);
    double DResult;
    ArrayLattice<double> aD(shape);
    ArrayLattice<double> bD(shape);
    ArrayLattice<double> cD(shape);
    double aDVal = 0.0;
    aD.set(aDVal);
    double bDVal = 2.0;
    bD.set(bDVal);
    double cDVal = 3.0;
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

    bool ok = true;


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
    if (!checkAttribute(attr1, false, true, nullIPos, nullIPos,
			lattCoord2)) ok = false;

// Now a non-scalar one; this only tests null LELCoordinates

    bool isScalar2 = false;
    IPosition shape2 = shape;
    IPosition tileShape2 = shape;
    LELAttribute attr2(true, shape2, tileShape2, lattCoord2);
    if (!checkAttribute(attr2, true, isScalar2, shape2, tileShape2,
			lattCoord2)) ok = false;

    LELAttribute attr3 = attr2;
    if (!checkAttribute(attr3, attr2.isMasked(), attr2.isScalar(),
			attr2.shape(), attr2.tileShape(),
                        attr2.coordinates())) {
      cout << "   Assignment failed" << endl;
      ok = false;
    }    

// Result of scalar and non-scalar is non-scalar

    LELAttribute attr4(attr1, attr2);
    if (!checkAttribute(attr4, attr2.isMasked(), attr2.isScalar(),
			attr2.shape(), attr2.tileShape(),
                        attr2.coordinates())) {
      cout << "   binary constructor failed" << endl;
      ok = false;
    }    

  }       
       

//************************************************************************
//
// LELLattice
//
  {
    cout << endl << "LELLattice<float> " << endl;
    LELLattice<float> expr(bF);
    FResult = bFVal;
    if (!checkFloat (expr, FResult, String("LELLattice"), shape, false, suppress)) ok = false;
  }
  {
    cout << "LELLattice<double> " << endl;
    LELLattice<double> expr(bD);
    DResult = bDVal;
    if (!checkDouble(expr, DResult, String("LELLattice"), shape, false, suppress)) ok = false;
  }
  {
    cout << "LELLattice<Complex> " << endl;
    LELLattice<Complex> expr(bC);
    CResult = bCVal;
    if (!checkComplex(expr, CResult, String("LELLattice"), shape, false, suppress)) ok = false;
  }
  {
    cout << "LELLattice<DComplex> " << endl;
    LELLattice<DComplex> expr(bDC);
    DCResult = bDCVal;
    if (!checkDComplex(expr, DCResult, String("LELLattice"), shape, false, suppress)) ok = false;
  }
  {
    cout << "LELLattice<bool> " << endl;
    LELLattice<bool> expr(bB);
    BResult = bBVal;
    if (!checkBool(expr, BResult, String("LELLattice"), shape, false, suppress)) ok = false;
  }

//************************************************************************
//
// LELUnaryConst
//
  {

    cout << endl << "LELUnaryConst<float>" << endl;
    LELUnaryConst<float> expr(aFVal);
    if (!checkFloat (expr, aFVal, String("LELUnaryConst"), shape, true, suppress)) ok = false;

  }
  {
    cout << "LELUnaryConst<double>" << endl;
    LELUnaryConst<double> expr(aDVal);
    if (!checkDouble(expr, aDVal, String("LELUnaryConst"), shape, true, suppress)) ok = false;
  }
  {
    cout << "LELUnaryConst<Complex>" << endl;
    
    LELUnaryConst<Complex> expr(aCVal);
    if (!checkComplex(expr, aCVal, String("LELUnaryConst"), shape, true, suppress)) ok = false;
  }
  {
    cout << "LELUnaryConst<DComplex>" << endl;
    LELUnaryConst<DComplex> expr(aDCVal);
    if (!checkDComplex(expr, aDCVal, String("LELUnaryConst"), shape, true, suppress)) ok = false;
  }
  {
    cout << "LELUnaryConst<bool>" << endl;
    LELUnaryConst<bool> expr(aBVal);
    if (!checkBool(expr, aBVal, String("LELUnaryConst"), shape, true, suppress)) ok = false;
  }

//
//************************************************************************
//
// LELUnary
//
   cout << endl << "LELUnary<float>" << endl;
  {
    CountedPtr<LELInterface<float> > pExpr = new LELLattice<float>(bF);

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    LELUnary<float> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkFloat (expr, -bFVal, String("LELUnary"), shape, false, suppress)) ok = false;
  }

   cout << "LELUnary<double>" << endl;
  {

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    CountedPtr<LELInterface<double> > pExpr = new LELLattice<double>(bD);
    LELUnary<double> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkDouble(expr, -bDVal, String("LELUnary"), shape, false, suppress)) ok = false;
  }


   cout << "LELUnary<Complex>" << endl;
  {

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    CountedPtr<LELInterface<Complex> > pExpr = new LELLattice<Complex>(bC);
    LELUnary<Complex> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkComplex(expr, -bCVal, String("LELUnary"), shape, false, suppress)) ok = false;
  }


   cout << "LELUnary<DComplex>" << endl;
  {

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    CountedPtr<LELInterface<DComplex> > pExpr = new LELLattice<DComplex>(bDC);
    LELUnary<DComplex> expr(LELUnaryEnums::MINUS, pExpr);
    if (!checkDComplex(expr, -bDCVal, String("LELUnary"), shape, false, suppress)) ok = false;
  }

//************************************************************************
//
// LELUnaryBool
//
  {
    cout << endl << "LELUnaryBool" << endl;

    {
      cout << "   Operator !" << endl;     
      CountedPtr<LELInterface<bool> > pExpr = new LELLattice<bool>(aB);
      LELUnaryBool expr(LELUnaryEnums::NOT, pExpr);
      if (!checkBool(expr, (!aBVal), String("LELUnaryBool"), shape, false, suppress)) ok = false;
    }
  }
//
//************************************************************************
//
// LELBinary<float>
//
  {
    cout << endl << "LELBinary<float>" << endl;
    CountedPtr<LELInterface<float> > pExprLeft = new LELLattice<float>(bF);
    CountedPtr<LELInterface<float> > pExprRight = new LELLattice<float>(cF);

    {
    cout << "   Operator +" << endl;     
    LELBinary<float> expr(LELBinaryEnums::ADD, pExprLeft, pExprRight);
    FResult = bFVal + cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<float> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    FResult = bFVal - cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<float> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    FResult = bFVal * cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<float> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    FResult = bFVal / cFVal;
    if (!checkFloat (expr, FResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }
  }
//
//
//************************************************************************
//
// LELBinary<double>
//
  {
    cout << endl << "LELBinary<double>" << endl;
    CountedPtr<LELInterface<double> > pExprLeft = new LELLattice<double>(bD);
    CountedPtr<LELInterface<double> > pExprRight = new LELLattice<double>(cD);

    {
    cout << "   Operator +" << endl;     
    LELBinary<double> expr(LELBinaryEnums::ADD, pExprLeft, pExprRight);
    DResult = bDVal + cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<double> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    DResult = bDVal - cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<double> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    DResult = bDVal * cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<double> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    DResult = bDVal / cDVal;
    if (!checkDouble (expr, DResult, String("LELBinary"), shape, false, suppress)) ok = false;
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
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<Complex> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    CResult = bCVal - cCVal;
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<Complex> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    CResult = bCVal * cCVal;
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<Complex> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    CResult = bCVal / cCVal;
    if (!checkComplex (expr, CResult, String("LELBinary"), shape, false, suppress)) ok = false;
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
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<DComplex> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    DCResult = bDCVal - cDCVal;
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<DComplex> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    DCResult = bDCVal * cDCVal;
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<DComplex> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    DCResult = bDCVal / cDCVal;
    if (!checkDComplex (expr, DCResult, String("LELBinary"), shape, false, suppress)) ok = false;
    }
  }
//
//************************************************************************
//
// LELBinaryCmp<float>
//
  {
    cout << endl << "LELBinaryCmp<float>" << endl;
    CountedPtr<LELInterface<float> > pExprLeft = new LELLattice<float>(bF);
    CountedPtr<LELInterface<float> > pExprRight = new LELLattice<float>(cF);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryCmp<float> expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bFVal==cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<float> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bFVal!=cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<float> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bFVal>cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<float> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bFVal>=cFVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }
  }
//
//************************************************************************
//
// LELBinaryCmp<double>
//
  {
    cout << endl << "LELBinaryCmp<double>" << endl;
    CountedPtr<LELInterface<double> > pExprLeft = new LELLattice<double>(bD);
    CountedPtr<LELInterface<double> > pExprRight = new LELLattice<double>(cD);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryCmp<double> expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bDVal==cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<double> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bDVal!=cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<double> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bDVal>cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<double> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bDVal>=cDVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
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
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<Complex> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bCVal!=cCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<Complex> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bCVal>cCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<Complex> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bCVal>=cCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
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
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<DComplex> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bDCVal!=cDCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<DComplex> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    BResult = (bDCVal>cDCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<DComplex> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    BResult = (bDCVal>=cDCVal);
    if (!checkBool(expr, BResult, String("LELBinaryCmp"), shape, false, suppress)) ok = false;
    }
  }
//
//************************************************************************
//
// LELBinaryBool
//
  {
    cout << endl << "LELBinaryBool" << endl;
    CountedPtr<LELInterface<bool> > pExprLeft = new LELLattice<bool>(bB);
    CountedPtr<LELInterface<bool> > pExprRight = new LELLattice<bool>(cB);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryBool expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    BResult = (bBVal==cBVal);
    if (!checkBool(expr, BResult, String("LELBinaryBool"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryBool expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    BResult = (bBVal!=cBVal);
    if (!checkBool(expr, BResult, String("LELBinaryBool"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Operator &&" << endl;     
    LELBinaryBool expr(LELBinaryEnums::AND, pExprLeft, pExprRight);
    BResult = (bBVal&&cBVal);
    if (!checkBool(expr, BResult, String("LELBinaryBool"), shape, false, suppress)) ok = false;
    }
  }

//
//************************************************************************
//
// LELFunction1D<float>
//
  {
    cout << endl << "LELFunction1D<float>" << endl;
    CountedPtr<LELInterface<float> > pExpr = new LELLattice<float>(bF);

    {
    cout << "   Function sin" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::SIN, pExpr);
    FResult = sin(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::SINH, pExpr);
    FResult = sinh(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::COS, pExpr);
    FResult = cos(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::COSH, pExpr);
    FResult = cosh(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::EXP, pExpr);
    FResult = exp(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::LOG, pExpr);
    FResult = log(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::LOG10, pExpr);
    FResult = log10(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::SQRT, pExpr);
    FResult = sqrt(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::MIN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = min(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::MAX1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = max(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function median" << endl;     
    LELFunctionReal1D<float> expr(LELFunctionEnums::MEDIAN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = median(FArr);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function mean" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::MEAN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = mean(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function sum" << endl;     
    LELFunction1D<float> expr(LELFunctionEnums::SUM, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = sum(FArr);
    if (!checkFloat (expr, FResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }
  }

//
//
//************************************************************************
//
// LELFunction1D<double>
//
  {
    cout << endl << "LELFunction1D<double>" << endl;
    CountedPtr<LELInterface<double> > pExpr = new LELLattice<double>(bD);

    {
    cout << "   Function sin" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::SIN, pExpr);
    DResult = sin(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::SINH, pExpr);
    DResult = sinh(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::COS, pExpr);
    DResult = cos(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::COSH, pExpr);
    DResult = cosh(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::EXP, pExpr);
    DResult = exp(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::LOG, pExpr);
    DResult = log(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::LOG10, pExpr);
    DResult = log10(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::SQRT, pExpr);
    DResult = sqrt(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::MIN1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = min(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::MAX1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = max(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function median" << endl;     
    LELFunctionReal1D<double> expr(LELFunctionEnums::MEDIAN1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = median(DArr);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function mean" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::MEAN1D, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = mean(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function sum" << endl;     
    LELFunction1D<double> expr(LELFunctionEnums::SUM, pExpr);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = sum(DArr);
    if (!checkDouble (expr, DResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
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
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::SINH, pExpr);
    CResult = sinh(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::COS, pExpr);
    CResult = cos(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::COSH, pExpr);
    CResult = cosh(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::EXP, pExpr);
    CResult = exp(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::LOG, pExpr);
    CResult = log(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::LOG10, pExpr);
    CResult = log10(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::SQRT, pExpr);
    CResult = sqrt(bCVal);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::MIN1D, pExpr);
    bC.getSlice(CArr, IPosition(CArr.ndim(),0), 
                CArr.shape(), IPosition(CArr.ndim(),1));
    CResult = min(CArr);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<Complex> expr(LELFunctionEnums::MAX1D, pExpr);
    bC.getSlice(CArr, IPosition(CArr.ndim(),0), 
                CArr.shape(), IPosition(CArr.ndim(),1));
    CResult = max(CArr);
    if (!checkComplex (expr, CResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
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
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::SINH, pExpr);
    DCResult = sinh(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::COS, pExpr);
    DCResult = cos(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::COSH, pExpr);
    DCResult = cosh(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function exp" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::EXP, pExpr);
    DCResult = exp(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::LOG, pExpr);
    DCResult = log(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function log10" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::LOG10, pExpr);
    DCResult = log10(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::SQRT, pExpr);
    DCResult = sqrt(bDCVal);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function min" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::MIN1D, pExpr);
    bDC.getSlice(DCArr, IPosition(DCArr.ndim(),0), 
                DCArr.shape(), IPosition(DCArr.ndim(),1));
    DCResult = min(DCArr);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<DComplex> expr(LELFunctionEnums::MAX1D, pExpr);
    bDC.getSlice(DCArr, IPosition(DCArr.ndim(),0), 
                DCArr.shape(), IPosition(DCArr.ndim(),1));
    DCResult = max(DCArr);
    if (!checkDComplex (expr, DCResult, String("LELFunction1D"), shape, true, suppress)) ok = false;
    }

  }

//
//************************************************************************
//
// LELFunctionND<float>
//
  {
    cout << endl << "LELFunctionND<float>" << endl;


    cout << "   Function iif" << endl;     
    {

    cout << "     Scalar, scalar, scalar" << endl;

    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<float> expr1(LELFunctionEnums::IIF, arga);
    FResult = bFVal;
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<float> expr2(LELFunctionEnums::IIF, arga);
    FResult = cFVal;
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cF);
    LELFunctionND<float> expr1(LELFunctionEnums::IIF, arga);
    FResult = bFVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<float> expr2(LELFunctionEnums::IIF, arga);
    FResult = cFVal;
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bF);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<float> expr1(LELFunctionEnums::IIF, arga);
    FResult = bFVal;
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<float> expr2(LELFunctionEnums::IIF, arga);
    FResult = cFVal;
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }

    {

    cout << "     Array, scalar, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<float> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<float> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Array, Array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bF);
    arga[2] = LatticeExprNode(cFVal);
    LELFunctionND<float> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<float> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Array, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bFVal);
    arga[2] = LatticeExprNode(cF);
    LELFunctionND<float> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr1, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<float> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      FResult = bFVal;
    } else {
      FResult = cFVal;
    }
    if (!checkFloat (expr2, FResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }

  }


//
//************************************************************************
//
// LELFunctionND<double>
//
  {
    cout << endl << "LELFunctionND<double>" << endl;


    cout << "   Function iif" << endl;     
    {

    cout << "     Scalar, scalar, scalar" << endl;

    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<double> expr1(LELFunctionEnums::IIF, arga);
    DResult = bDVal;
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<double> expr2(LELFunctionEnums::IIF, arga);
    DResult = cDVal;
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cD);
    LELFunctionND<double> expr1(LELFunctionEnums::IIF, arga);
    DResult = bDVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<double> expr2(LELFunctionEnums::IIF, arga);
    DResult = cDVal;
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bD);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<double> expr1(LELFunctionEnums::IIF, arga);
    DResult = bDVal;
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<double> expr2(LELFunctionEnums::IIF, arga);
    DResult = cDVal;
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }

    {

    cout << "     Array, scalar, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<double> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<double> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Array, Array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bD);
    arga[2] = LatticeExprNode(cDVal);
    LELFunctionND<double> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<double> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Array, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(aB);
    arga[1] = LatticeExprNode(bDVal);
    arga[2] = LatticeExprNode(cD);
    LELFunctionND<double> expr1(LELFunctionEnums::IIF, arga);
    if (aBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr1, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<double> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DResult = bDVal;
    } else {
      DResult = cDVal;
    }
    if (!checkDouble (expr2, DResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bCVal);
    arga[2] = LatticeExprNode(cCVal);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    CResult = bCVal;
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    CResult = cCVal;
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bCVal);
    arga[2] = LatticeExprNode(cC);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    CResult = bCVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    CResult = cCVal;
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bC);
    arga[2] = LatticeExprNode(cCVal);
    LELFunctionND<Complex> expr1(LELFunctionEnums::IIF, arga);
    CResult = bCVal;
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    CResult = cCVal;
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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
    if (!checkComplex (expr1, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<Complex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      CResult = bCVal;
    } else {
      CResult = cCVal;
    }
    if (!checkComplex (expr2, CResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bDCVal);
    arga[2] = LatticeExprNode(cDCVal);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    DCResult = bDCVal;
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    DCResult = cDCVal;
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, true, suppress)) ok = false;

    }

    {

    cout << "     Scalar, scalar, array" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bDCVal);
    arga[2] = LatticeExprNode(cDC);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    DCResult = bDCVal;

// Although the conditional is scalar, the result is still an array
// because one of the evaluation expressions is an array

    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    DCResult = cDCVal;
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
    {

    cout << "     Scalar, array, scalar" << endl;
    Block<LatticeExprNode> arga(3);

    arga[0] = LatticeExprNode(true);
    arga[1] = LatticeExprNode(bDC);
    arga[2] = LatticeExprNode(cDCVal);
    LELFunctionND<DComplex> expr1(LELFunctionEnums::IIF, arga);
    DCResult = bDCVal;
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(false);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    DCResult = cDCVal;
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

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
    if (!checkDComplex (expr1, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    arga[0] = LatticeExprNode(bB);
    LELFunctionND<DComplex> expr2(LELFunctionEnums::IIF, arga);
    if (bBVal) {
      DCResult = bDCVal;
    } else {
      DCResult = cDCVal;
    }
    if (!checkDComplex (expr2, DCResult, String("LELFunctionND"), shape, false, suppress)) ok = false;

    }
  }



//
//************************************************************************
//
// LELFunctionReal1D<float>
//
  {
    cout << endl << "LELFunctionReal1D<float>" << endl;
    CountedPtr<LELInterface<float> > pExpr = new LELLattice<float>(bF);
    CountedPtr<LELInterface<float> > pExpra = new LELLattice<float>(aF);

    {
    cout << "   Function asin" << endl;     
    LELFunctionReal1D<float> expr(LELFunctionEnums::ASIN, pExpra);
    FResult = asin(aFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function acos" << endl;     
    LELFunctionReal1D<float> expr(LELFunctionEnums::ACOS, pExpra);
    FResult = acos(aFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Function tan" << endl;     
    LELFunctionReal1D<float> expr(LELFunctionEnums::TAN, pExpr);
    FResult = tan(bFVal);    
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function tanh" << endl;     
    LELFunctionReal1D<float> expr(LELFunctionEnums::TANH, pExpr);
    FResult = tanh(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function ceil" << endl;     
    LELFunctionReal1D<float> expr(LELFunctionEnums::CEIL, pExpr);
    FResult = ceil(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function floor" << endl;     
    LELFunctionReal1D<float> expr(LELFunctionEnums::FLOOR, pExpr);
    FResult = floor(bFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }
  }

//
//************************************************************************
//
// LELFunctionReal1D<double>
//
  {
    cout << endl << "LELFunctionReal1D<double>" << endl;
    CountedPtr<LELInterface<double> > pExpr = new LELLattice<double>(bD);
    CountedPtr<LELInterface<double> > pExpra = new LELLattice<double>(aD);

    {
    cout << "   Function asin" << endl;     
    LELFunctionReal1D<double> expr(LELFunctionEnums::ASIN, pExpra);
    DResult = asin(aDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function acos" << endl;     
    LELFunctionReal1D<double> expr(LELFunctionEnums::ACOS, pExpra);
    DResult = acos(aDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Function tan" << endl;     
    LELFunctionReal1D<double> expr(LELFunctionEnums::TAN, pExpr);
    DResult = tan(bDVal);    
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function tanh" << endl;     
    LELFunctionReal1D<double> expr(LELFunctionEnums::TANH, pExpr);
    DResult = tanh(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function ceil" << endl;     
    LELFunctionReal1D<double> expr(LELFunctionEnums::CEIL, pExpr);
    DResult = ceil(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function floor" << endl;     
    LELFunctionReal1D<double> expr(LELFunctionEnums::FLOOR, pExpr);
    DResult = floor(bDVal);
    if (!checkDouble (expr, DResult, String("LELFunctionReal1D"), shape, false, suppress)) ok = false;
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
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Function max" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::MAX, arga);
    FResult = max(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function pow" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::POW, arga);
    FResult = pow(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function atan2" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ATAN2, arga);
    FResult = atan2(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function fmod" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::FMOD, arga);
    FResult = fmod(bFVal,cFVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bC);

    {
    cout << "   Function abs" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ABS, argb);
    FResult = abs(bCVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function arg" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ARG, argb);
    FResult = float(arg(bCVal));
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function real" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::REAL, argb);
    FResult = real(bCVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }

    {
      cout << "   Function imag" << endl;
    LELFunctionFloat expr(LELFunctionEnums::IMAG, argb);
    FResult = imag(bCVal);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function fractile" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bF);
    arg[1] = LatticeExprNode(float(0.5));
    LELFunctionFloat expr(LELFunctionEnums::FRACTILE1D, arg);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = fractile(FArr, 0.5);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function fractilerange 2" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bF);
    arg[1] = LatticeExprNode(float(0.2));
    LELFunctionFloat expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = fractile(FArr, 0.8) - fractile(FArr, 0.2);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function fractilerange 3" << endl;
    Block<LatticeExprNode> arg(3);
    arg[0] = LatticeExprNode(bF);
    arg[1] = LatticeExprNode(float(0.2));
    arg[2] = LatticeExprNode(float(0.7));
    LELFunctionFloat expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = fractile(FArr, 0.7) - fractile(FArr, 0.2);
    if (!checkFloat (expr, FResult, String("LELFunctionFloat"), shape, true, suppress)) ok = false;
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
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }


    {
    cout << "   Function max" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::MAX, arga);
    DResult = max(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function pow" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::POW, arga);
    DResult = pow(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function atan2" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::ATAN2, arga);
    DResult = atan2(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function fmod" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::FMOD, arga);
    DResult = fmod(bDVal,cDVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bDC);

    {
    cout << "   Function abs" << endl;     
    DResult = abs(bDCVal);
    LELFunctionDouble expr(LELFunctionEnums::ABS, argb);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function arg" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::ARG, argb);
    DResult = double(arg(bDCVal));
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function real" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::REAL, argb);
    DResult = real(bDCVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
    }

    {
    cout << "   Function imag" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::IMAG, argb);
    DResult = imag(bDCVal);
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, false, suppress)) ok = false;
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
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, true, suppress)) ok = false;
    }


    {
    cout << "   Function nfalse" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::NFALSE, argc);
    if (!bBVal) {
      DResult = shape.product();
    } else {
      DResult = 0.0;
    }
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function nelements" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::NELEM, argc);
    DResult = shape.product();
    if (!checkDouble(expr, DResult, String("LELFunctionDouble"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function fractile" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bD);
    arg[1] = LatticeExprNode(float(0.5));
    LELFunctionDouble expr(LELFunctionEnums::FRACTILE1D, arg);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = fractile(DArr, 0.5);
    if (!checkDouble (expr, DResult, String("LELFunctionDouble"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function fractilerange 2" << endl;
    Block<LatticeExprNode> arg(2);
    arg[0] = LatticeExprNode(bD);
    arg[1] = LatticeExprNode(float(0.2));
    LELFunctionDouble expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = fractile(DArr, 0.8) - fractile(DArr, 0.2);
    if (!checkDouble (expr, DResult, String("LELFunctionDouble"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function fractilerange 3" << endl;
    Block<LatticeExprNode> arg(3);
    arg[0] = LatticeExprNode(bD);
    arg[1] = LatticeExprNode(float(0.2));
    arg[2] = LatticeExprNode(float(0.7));
    LELFunctionDouble expr(LELFunctionEnums::FRACTILERANGE1D, arg);
    bD.getSlice(DArr, IPosition(DArr.ndim(),0), 
                DArr.shape(), IPosition(DArr.ndim(),1));
    DResult = fractile(DArr, 0.7) - fractile(DArr, 0.2);
    if (!checkDouble (expr, DResult, String("LELFunctionDouble"), shape, true, suppress)) ok = false;
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
    if (!checkComplex(expr, CResult, String("LELFunctionComplex"), shape, false, suppress)) ok = false;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bC);

    {
    cout << "   Function conj" << endl;     
    LELFunctionComplex expr(LELFunctionEnums::CONJ, argb);
    CResult = conj(bCVal);
    if (!checkComplex(expr, CResult, String("LELFunctionComplex"), shape, false, suppress)) ok = false;
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
    if (!checkDComplex(expr, DCResult, String("LELFunctionDComplex"), shape, false, suppress)) ok = false;
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bDC);

    {
    cout << "   Function conj" << endl;     
    LELFunctionDComplex expr(LELFunctionEnums::CONJ, argb);
    DCResult = conj(bDCVal);
    if (!checkDComplex(expr, DCResult, String("LELFunctionDComplex"), shape, false, suppress)) ok = false;
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
    if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, true, suppress)) ok = false;
    }

    {
    cout << "   Function any" << endl;     
    LELFunctionBool expr(LELFunctionEnums::ANY, arga);
    BResult = bBVal;
    if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, true, suppress)) ok = false;
    }

    {
    Block<LatticeExprNode> argb(1);
    cout << "   Function isNaN" << endl;     
    {
      argb[0] = LatticeExprNode(bF);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = false;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, false, suppress)) ok = false;
    }
    {
      argb[0] = LatticeExprNode(nanF);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = true;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, false, suppress)) ok = false;
    }
    {
      argb[0] = LatticeExprNode(bD);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = false;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, false, suppress)) ok = false;
    }
    {
      argb[0] = LatticeExprNode(bC);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = false;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, false, suppress)) ok = false;
    }
    {
      argb[0] = LatticeExprNode(bDC);
      LELFunctionBool expr(LELFunctionEnums::ISNAN, argb);
      BResult = false;
      if (!checkBool(expr, BResult, String("LELFunctionBool"), shape, false, suppress)) ok = false;
    }
    }

    {
    Block<LatticeExprNode> argb(2);
    cout << "   Function indexin" << endl;     
    {
      Vector<bool> flags(2,true);
      argb[0] = LatticeExprNode(0);
      argb[1] = LatticeExprNode(ArrayLattice<bool>(flags));
      LELFunctionBool expr(LELFunctionEnums::INDEXIN, argb);
      BResult = true;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,2,4), false, suppress, true)) ok = false;
    }
    {
      Vector<bool> flags(2,true);
      argb[0] = LatticeExprNode(1);
      argb[1] = LatticeExprNode(ArrayLattice<bool>(flags));
      LELFunctionBool expr(LELFunctionEnums::INDEXIN, argb);
      BResult = true;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,10,2), false, suppress, true)) ok = false;
    }
    {
      Vector<bool> flags(3,false);
      argb[0] = LatticeExprNode(0);
      argb[1] = LatticeExprNode(ArrayLattice<bool>(flags));
      LELFunctionBool expr(LELFunctionEnums::INDEXIN, argb);
      BResult = false;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,6,2), false, suppress, true)) ok = false;
      if (!checkBool (expr, BResult, "LELFunctionBool", IPosition(2,2,6), false, suppress, true)) ok = false;
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
    cout << endl << "LELConvert<float,double> " << endl;
    CountedPtr<LELInterface<double> > pExpr = new LELLattice<double>(bD);
    LELConvert<float,double> expr(pExpr);
    FResult = float(bDVal);
    if (!checkFloat (expr, FResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }

    {
    cout << "LELConvert<double,float> " << endl;
    CountedPtr<LELInterface<float> > pExpr = new LELLattice<float>(bF);
    LELConvert<double,float> expr(pExpr);
    DResult = double(bFVal);
    if (!checkDouble(expr, DResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }

    {
    cout << "LELConvert<Complex,DComplex> " << endl;
    CountedPtr<LELInterface<DComplex> > pExpr = new LELLattice<DComplex>(bDC);
    LELConvert<Complex,DComplex> expr(pExpr);
    CResult = Complex(bDCVal.real(), bDCVal.imag());
    if (!checkComplex(expr, CResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }

    {
    cout << "LELConvert<DComplex,Complex> " << endl;
    CountedPtr<LELInterface<Complex> > pExpr = new LELLattice<Complex>(bC);
    LELConvert<DComplex,Complex> expr(pExpr);
    DCResult = bCVal;
    if (!checkDComplex(expr, DCResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }


    {
    cout << "LELConvert<Complex,float> " << endl;
    CountedPtr<LELInterface<float> > pExpr = new LELLattice<float>(bF);
    LELConvert<Complex,float> expr(pExpr);
    CResult = Complex(bFVal,0.0);
    if (!checkComplex(expr, CResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }

    {
    cout << "LELConvert<Complex,double> " << endl;
    CountedPtr<LELInterface<double> > pExpr = new LELLattice<double>(bD);
    LELConvert<Complex,double> expr(pExpr);
    CResult = Complex(bDVal,0.0);
    if (!checkComplex(expr, CResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }

    {
    cout << "LELConvert<DComplex,float> " << endl;
    CountedPtr<LELInterface<float> > pExpr = new LELLattice<float>(bF);
    LELConvert<DComplex,float> expr(pExpr);
    DCResult = DComplex(bFVal,0.0);
    if (!checkDComplex(expr, DCResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }

    {
    cout << "LELConvert<DComplex,double> " << endl;
    CountedPtr<LELInterface<double> > pExpr = new LELLattice<double>(bD);
    LELConvert<DComplex,double> expr(pExpr);
    DCResult = DComplex(bDVal,0.0);
    if (!checkDComplex(expr, DCResult, String("LELConvert"), shape, false, suppress)) ok = false;
    }
  }


  if (!ok) {
    cout << "not ok" << endl;
     return 1;
  } else {
    cout << endl << "ok" << endl;
  }

 } catch (std::exception& x) {
    cerr << "aipserror: error " << x.what() << endl;
    return 1;
 } 
 
   return 0;
}


bool checkFloat (LELInterface<float>& expr, 
                 const float Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress)
{
    LELArray<float> Arr(shape);
    bool ok = true;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = false;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = false;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = false;
      }
      try {
        expr.eval(Arr, region);
      } catch (std::exception& x) {
        if (!suppress) cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = false;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = false;
      }
      try {
       expr.getScalar();
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



bool checkDouble (LELInterface<double>& expr, 
                 const double Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress)
{
    LELArray<double> Arr(shape);
    bool ok = true;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = false;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = false;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = false;
      }
      try {
        expr.eval(Arr, region);
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = false;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = false;
      }
      try {
       expr.getScalar();
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



bool checkComplex (LELInterface<Complex>& expr, 
                 const Complex& Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress)
{
    LELArray<Complex> Arr(shape);
    bool ok = true;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = false;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = false;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = false;
      }
      try {
        expr.eval(Arr, region);
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = false;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = false;
      }
      try {
       expr.getScalar();
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



bool checkDComplex (LELInterface<DComplex>& expr, 
                 const DComplex& Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress)
{
    LELArray<DComplex> Arr(shape);
    bool ok = true;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = false;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = false;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = false;
      }
      try {
        expr.eval(Arr, region);
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = false;
      }
      if (expr.shape() != shape) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = false;
      }
      try {
       expr.getScalar();
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}



bool checkBool (LELInterface<bool>& expr, 
                 const bool Result,
                 const String& name,
                 const IPosition& shape,
                 const bool shouldBeScalar,
                 const bool suppress,
		 const bool emptyShape)
{
    LELArray<bool> Arr(shape);
    bool ok = true;
    IPosition origin(2,0,0);
    Slicer region(origin, shape);


    if (expr.className() != name) {
       cout << "   Class name is wrong" << endl;
       ok = false;
    }

    if (shouldBeScalar) {
      if (!expr.isScalar()) {
         cout << "   Expression is not a scalar but should be" << endl;
         ok = false;
      }
      if (expr.shape() != IPosition()) {
         cout << "   Expression has wrong shape" << endl;
         ok = false;
      }
      if (expr.getScalar().value() != Result) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << expr.getScalar().value() << endl;
         ok = false;
      }
      try {
        expr.eval(Arr, region);
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    } else {
      if (expr.isScalar()) {
         cout << "   Expression is a scalar but shouldn't be" << endl;
         ok = false;
      }
      if (emptyShape) {
	if (expr.shape() != IPosition()) {
	  cout << "   Expression has no empty shape" << endl;
	  ok = false;
	}
      } else {
	if (expr.shape() != shape) {
	  cout << "   Expression has wrong shape" << endl;
	  ok = false;
	}
      }
      expr.eval(Arr, region);
      if (!allEQ (Arr.value(), Result)) {
         cout << "   Result should be " << Result << endl;
         cout << "   Result is        " << Arr.value()(origin) << endl;
         ok = false;
      }
      try {
       expr.getScalar();
      } catch (std::exception& x) {
       if (!suppress)  cout << "      Caught expected exception; message is: " << x.what() << endl;
      } 
    }
    expr.prepareScalarExpr();
 
    return ok;
}
bool checkAttribute (const LELAttribute& attr,
		     const bool isMasked,
                     const bool isScalar,
                     const IPosition& shape,
                     const IPosition& tileShape,
                     const LELCoordinates& lattCoord)
{
   bool ok = true;
   if (attr.isMasked() != isMasked) {
      cout << "   isMasked function failed" << endl;
      ok = false;
   }
   if (attr.isScalar() != isScalar) {
      cout << "   isScalar function failed" << endl;
      ok = false;
   }
   if (attr.shape() != shape) {
      cout << "   shape function failed" << endl;
      ok = false;
   }    
   if (attr.tileShape() != tileShape) {
      cout << "   tileShape function failed" << endl;
      ok = false;
   }    
   if (attr.coordinates().compare(lattCoord) != 0) {
      cout << "   coordinates function failed" << endl;
      ok = false;
   }
   return ok;
}

