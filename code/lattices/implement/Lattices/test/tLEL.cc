//# tLEL.cc:  Tests the LEL* classes directly
//# Copyright (C) 1997
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

#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/PixelBox.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Complex.h>

#include <iostream.h>


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

    Array<Bool> BArr(shape);
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
    Float aFVal = 0.0;
    aF.set(aFVal);
    Float bFVal = 1.0;
    bF.set(1.0);
    Float cFVal = 2.0;
    cF.set(cFVal);


// Double Lattices

    Array<Double> DArr(shape);
    Double DResult;
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

    Array<Complex> CArr(shape);
    Complex CResult;
    ArrayLattice<Complex> aC(shape);
    ArrayLattice<Complex> bC(shape);
    ArrayLattice<Complex> cC(shape);
    Complex aCVal = Complex(0.0,0.0);
    aC.set(aCVal);
    Complex bCVal = Complex(1.0,1.0);
    bC.set(bCVal);
    Complex cCVal = Complex(2.0,2.0);
    cC.set(cCVal);


// DComplex Lattices

    Array<DComplex> DCArr(shape);
    DComplex DCResult;
    ArrayLattice<DComplex> aDC(shape);
    ArrayLattice<DComplex> bDC(shape);
    ArrayLattice<DComplex> cDC(shape);
    DComplex aDCVal = DComplex(0.0,0.0);
    aDC.set(aDCVal);
    DComplex bDCVal = DComplex(1.0,1.0);
    bDC.set(bDCVal);
    DComplex cDCVal = DComplex(2.0,2.0);
    cDC.set(cDCVal);


    IPosition origin(2,0,0);
    PixelBox region(origin, shape-1, shape);
    Bool ok = True;


//************************************************************************
// 
// LELAttribute
//
  {
    cout << "LELAttribute" << endl;
    Bool isScalar1 = True;
    IPosition shape1 = IPosition();
    LELAttribute attr1(isScalar1, shape1);
    if (attr1.isScalar() != isScalar1) {
      cout << "   isScalar function failed" << endl;
      ok = False;
    }
    if (attr1.shape() != shape1) {
      cout << "   shape function failed" << endl;
      ok = False;
    }    
    Bool isScalar2 = False;
    IPosition shape2 = shape;
    LELAttribute attr2(isScalar2, shape2);

    LELAttribute attr3 = attr2;
    if (attr3.isScalar()!=attr2.isScalar() || attr3.shape() != attr2.shape()) {
      cout << "   Assignment failed" << endl;
      ok = False;
    }    

// Result of scalar and non-scalar is non-scalar

    LELAttribute attr4(attr1, attr2);
    if (attr4.isScalar() || attr4.shape() != attr2.shape()) {
      cout << "   double constructor failed" << endl;
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
    expr.eval(FArr, region);
    FResult = bFVal;
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELLattice")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
  }

//************************************************************************
//
// LELUnaryConst
//
  {

    cout << endl << "LELUnaryConst<Float>" << endl;
    
    LELUnaryConst<Float> expr(aFVal);
    FResult = aFVal;
    if (expr.getScalar() != FResult) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELUnaryConst")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
  }
//
//************************************************************************
//
// LELUnary
//
  {
    cout << endl << "LELUnary<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);

// Note that operator+ is not actually implemented in LELUnary because it
// wouldn't do anything !  It is implemented in LatticeExprNode though

    cout << "   Operator -" << endl;     
    LELUnary<Float> expr(LELUnaryEnums::MINUS, pExpr);
    expr.eval(FArr, region);
    FResult = -bFVal; 
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELUnary")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
  }
//************************************************************************
//
// LELUnaryBool
//
  {
    cout << endl << "LELUnaryBool" << endl;
    CountedPtr<LELInterface<Bool> > pExpr = new LELLattice<Bool>(aB);

    cout << "   Operator !" << endl;     
    LELUnaryBool expr(LELUnaryEnums::NOT, pExpr);
    expr.eval(BArr, region);
    BResult = ToBool(!aBVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELUnaryBool")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
  }
//
//************************************************************************
//
// LELBinary
//
  {
    cout << endl << "LELBinary<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExprLeft = new LELLattice<Float>(bF);
    CountedPtr<LELInterface<Float> > pExprRight = new LELLattice<Float>(cF);

    {
    cout << "   Operator +" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::ADD, pExprLeft, pExprRight);
    expr.eval(FArr, region);
    FResult = bFVal + cFVal;
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinary")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Operator -" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::SUBTRACT, pExprLeft, pExprRight);
    expr.eval(FArr, region);
    FResult = bFVal - cFVal;
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinary")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Operator *" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::MULTIPLY, pExprLeft, pExprRight);
    expr.eval(FArr, region);
    FResult = bFVal * cFVal;
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinary")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Operator /" << endl;     
    LELBinary<Float> expr(LELBinaryEnums::DIVIDE, pExprLeft, pExprRight);
    expr.eval(FArr, region);
    FResult = bFVal / cFVal;
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinary")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }
  }
//
//************************************************************************
//
// LELBinaryCmp
//
  {
    cout << endl << "LELBinaryCmp<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExprLeft = new LELLattice<Float>(bF);
    CountedPtr<LELInterface<Float> > pExprRight = new LELLattice<Float>(cF);

    {
    cout << "   Operator ==" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::EQ, pExprLeft, pExprRight);
    expr.eval(BArr, region);
    BResult = ToBool(bFVal==cFVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinaryCmp")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    expr.eval(BArr, region);
    BResult = ToBool(bFVal!=cFVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinaryCmp")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Operator >" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::GT, pExprLeft, pExprRight);
    expr.eval(BArr, region);
    BResult = ToBool(bFVal>cFVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinaryCmp")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Operator >=" << endl;     
    LELBinaryCmp<Float> expr(LELBinaryEnums::GE, pExprLeft, pExprRight);
    expr.eval(BArr, region);
    BResult = ToBool(bFVal>=cFVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinaryCmp")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
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
    expr.eval(BArr, region);
    BResult = ToBool(bBVal==cBVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinaryBool")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Operator !=" << endl;     
    LELBinaryBool expr(LELBinaryEnums::NE, pExprLeft, pExprRight);
    expr.eval(BArr, region);
    BResult = ToBool(bBVal!=cBVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinaryBool")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Operator &&" << endl;     
    LELBinaryBool expr(LELBinaryEnums::AND, pExprLeft, pExprRight);
    expr.eval(BArr, region);
    BResult = ToBool(bBVal&&cBVal);
    if (! allEQ (BArr, BResult)) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << BArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELBinaryBool")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }
  }

//
//************************************************************************
//
// LELFunction1D
//
  {
    cout << endl << "LELFunction1D<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);

    {
    cout << "   Function sin" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SIN, pExpr);
    expr.eval(FArr, region);
    FResult = sin(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function sinh" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SINH, pExpr);
    expr.eval(FArr, region);
    FResult = sinh(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function cos" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::COS, pExpr);
    expr.eval(FArr, region);
    FResult = cos(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function cosh" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::COSH, pExpr);
    expr.eval(FArr, region);
    FResult = cosh(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function exp" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::EXP, pExpr);
    expr.eval(FArr, region);
    FResult = exp(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function log" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::LOG, pExpr);
    expr.eval(FArr, region);
    FResult = log(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function log10" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::LOG10, pExpr);
    expr.eval(FArr, region);
    FResult = log10(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function sqrt" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SQRT, pExpr);
    expr.eval(FArr, region);
    FResult = sqrt(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function min" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::MIN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = min(FArr);
    if (expr.getScalar() != FResult) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function max" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::MAX1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = max(FArr);
    if (expr.getScalar() != FResult) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function mean" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::MEAN1D, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = mean(FArr);
    if (expr.getScalar() != FResult) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function sum" << endl;     
    LELFunction1D<Float> expr(LELFunctionEnums::SUM, pExpr);
    bF.getSlice(FArr, IPosition(FArr.ndim(),0), 
                FArr.shape(), IPosition(FArr.ndim(),1));
    FResult = sum(FArr);
    if (expr.getScalar() != FResult) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunction1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }
  }

//
//************************************************************************
//
// LELFunctionReal1D
//
  {
    cout << endl << "LELFunctionReal1D<Float>" << endl;
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);

    {
    cout << "   Function asin" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::ASIN, pExpr);
    expr.eval(FArr, region);
    FResult = asin(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionReal1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function acos" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::ACOS, pExpr);
    expr.eval(FArr, region);
    FResult = acos(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionReal1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function tan" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::TAN, pExpr);
    expr.eval(FArr, region);
    FResult = tan(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionReal1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function tanh" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::TANH, pExpr);
    expr.eval(FArr, region);
    FResult = tanh(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionReal1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function ceil" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::CEIL, pExpr);
    expr.eval(FArr, region);
    FResult = ceil(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionReal1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function floor" << endl;     
    LELFunctionReal1D<Float> expr(LELFunctionEnums::FLOOR, pExpr);
    expr.eval(FArr, region);
    FResult = floor(bFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionReal1D")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
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
    expr.eval(FArr, region);
    FResult = min(bFVal,cFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function max" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::MAX, arga);
    expr.eval(FArr, region);
    FResult = max(bFVal,cFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function pow" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::POW, arga);
    expr.eval(FArr, region);
    FResult = pow(bFVal,cFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function atan2" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ATAN2, arga);
    expr.eval(FArr, region);
    FResult = atan2(bFVal,cFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function fmod" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::FMOD, arga);
    expr.eval(FArr, region);
    FResult = fmod(bFVal,cFVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bC);

    {
    cout << "   Function abs" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ABS, argb);
    expr.eval(FArr, region);
    FResult = abs(bCVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function arg" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::ARG, argb);
    expr.eval(FArr, region);
    FResult = Float(arg(bCVal));
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function real" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::REAL, argb);
    expr.eval(FArr, region);
    FResult = real(bCVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function imag" << endl;     
    LELFunctionFloat expr(LELFunctionEnums::IMAG, argb);
    expr.eval(FArr, region);
    FResult = imag(bCVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionFloat")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
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
    expr.eval(DArr, region);
    DResult = min(bFVal,cFVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function max" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::MAX, arga);
    expr.eval(DArr, region);
    DResult = max(bFVal,cFVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function pow" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::POW, arga);
    expr.eval(DArr, region);
    DResult = pow(bFVal,cFVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function atan2" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::ATAN2, arga);
    expr.eval(DArr, region);
    DResult = atan2(bFVal,cFVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function fmod" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::FMOD, arga);
    expr.eval(DArr, region);
    DResult = fmod(bFVal,cFVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bDC);

    {
    cout << "   Function abs" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::ABS, argb);
    expr.eval(DArr, region);
    DResult = abs(bDCVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function arg" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::ARG, argb);
    expr.eval(DArr, region);
    DResult = Double(arg(bDCVal));
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function real" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::REAL, argb);
    expr.eval(DArr, region);
    DResult = real(bDCVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function imag" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::IMAG, argb);
    expr.eval(DArr, region);
    DResult = imag(bDCVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
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
    if (expr.getScalar() != DResult) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << expr.getScalar()  << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << "   Function nfalse" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::NFALSE, argc);
    if (!bBVal) {
      DResult = shape.product();
    } else {
      DResult = 0.0;
    }
    if (expr.getScalar() != DResult) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function nelements" << endl;     
    LELFunctionDouble expr(LELFunctionEnums::NELEM, argc);
    DResult = shape.product();
    if (expr.getScalar() != DResult) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << expr.getScalar()  << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDouble")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
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
    expr.eval(CArr, region);
    CResult = pow(bCVal,cCVal);
    if (! allEQ (CArr, CResult)) {
       cout << "   Result should be " << CResult << endl;
       cout << "   Result is        " << CArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionComplex")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bC);

    {
    cout << "   Function conj" << endl;     
    LELFunctionComplex expr(LELFunctionEnums::CONJ, argb);
    expr.eval(CArr, region);
    CResult = conj(bCVal);
    if (! allEQ (CArr, CResult)) {
       cout << "   Result should be " << CResult << endl;
       cout << "   Result is        " << CArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionComplex")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
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
    expr.eval(DCArr, region);
    DCResult = pow(bDCVal,cDCVal);
    if (! allEQ (DCArr, DCResult)) {
       cout << "   Result should be " << DCResult << endl;
       cout << "   Result is        " << DCArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDComplex")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    Block<LatticeExprNode> argb(1);
    argb[0] = LatticeExprNode(bDC);

    {
    cout << "   Function conj" << endl;     
    LELFunctionDComplex expr(LELFunctionEnums::CONJ, argb);
    expr.eval(DCArr, region);
    DCResult = conj(bDCVal);
    if (! allEQ (DCArr, DCResult)) {
       cout << "   Result should be " << DCResult << endl;
       cout << "   Result is        " << DCArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionDComplex")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
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
    if (expr.getScalar() != BResult) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionBool")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << "   Function any" << endl;     
    LELFunctionBool expr(LELFunctionEnums::ANY, arga);
    BResult = bBVal;
    if (expr.getScalar() != BResult) {
       cout << "   Result should be " << BResult << endl;
       cout << "   Result is        " << expr.getScalar() << endl;
       ok = False;
    }
    if (expr.className()  != String("LELFunctionBool")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (!expr.isScalar()) {
       cout << "   Expression is a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != IPosition()) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
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
    expr.eval(FArr, region);
    FResult = Float(bDVal);
    if (! allEQ (FArr, FResult)) {
       cout << "   Result should be " << FResult << endl;
       cout << "   Result is        " << FArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELConvert")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }

    {
    cout << endl << "LELConvert<Double,Float> " << endl;
    
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);
    LELConvert<Double,Float> expr(pExpr);
    expr.eval(DArr, region);
    DResult = Double(bFVal);
    if (! allEQ (DArr, DResult)) {
       cout << "   Result should be " << DResult << endl;
       cout << "   Result is        " << DArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELConvert")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }


    {
    cout << endl << "LELConvert<Complex,Float> " << endl;
    
    CountedPtr<LELInterface<Float> > pExpr = new LELLattice<Float>(bF);
    LELConvert<Complex,Float> expr(pExpr);
    expr.eval(CArr, region);
    CResult = Complex(bFVal,0.0);
    if (! allEQ (CArr, CResult)) {
       cout << "   Result should be " << CResult << endl;
       cout << "   Result is        " << CArr.ac()(origin) << endl;
       ok = False;
    }
    if (expr.className()  != String("LELConvert")) {
       cout << "   Class name is wrong" << endl;
       ok = False;
    }

// From LELInterface

    if (expr.isScalar()) {
       cout << "   Expression is not a scalar" << endl;
       ok = False;
    }
    if (expr.shape() != shape) {
       cout << "   Expression has wrong shape" << endl;
       ok = False;
    }
    }
  }


  if (!ok) {
     return 1;
  } else {
    cout << endl << "ok" << endl;
  }

 } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    return 1;
 } end_try;
 
 return 0;
}
