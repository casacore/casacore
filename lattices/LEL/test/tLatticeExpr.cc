//# tLatticeExpr.cc:  
//# Copyright (C) 1997,1999,2000,2001
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
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/COWPtr.h>

#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
bool checkFloat(Lattice<float>& expr, 
                const float result,
                const IPosition shape,
                const bool supress);

bool checkDouble(Lattice<double>& expr, 
                const double result,
                const IPosition shape,
                const bool supress);

bool checkComplex(Lattice<Complex>& expr, 
                const Complex result,
                const IPosition shape,
                const bool supress);

bool checkDComplex(Lattice<DComplex>& expr, 
                const DComplex result,
                const IPosition shape,
                const bool supress);

bool checkBool(Lattice<bool>& expr, 
                const bool result,
                const IPosition shape,
                const bool supress);


int main (int argc, const char* argv[])
{
 try {
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "2", "Number of pixels along the x-axis", "int");
    inp.create("ny", "2", "Number of pixels along the y-axis", "int");
    inp.create("sup", "False", "Supress expected exceptions messages", "Bool");
    inp.readArguments(argc, argv);

    const uint32_t nx=inp.getInt("nx");
    const uint32_t ny=inp.getInt("ny");
    const bool supress=inp.getBool("sup");

    IPosition shape(2,nx,ny);
    bool ok = true;
 
// bool Lattices
    
    ArrayLattice<bool> aB(shape);
    bool aBVal = true;
    aB.set(aBVal);

    
// FLoat Lattices
    
    ArrayLattice<float> aF(shape);   
    float aFVal = 2.0;
    aF.set(aFVal);
    
    
// double Lattices
 
    ArrayLattice<double> aD(shape);
    double aDVal = 2.0;
    aD.set(aDVal);  
  
// Complex Lattices
 
    ArrayLattice<Complex> aC(shape);
    Complex aCVal = Complex(2.0,2.0);
    aC.set(aCVal);
      
    
// DComplex Lattices
     
    ArrayLattice<DComplex> aDC(shape);
    DComplex aDCVal = DComplex(2.0,2.0);
    aDC.set(aDCVal);


//
// <float>
// 
     {
       cout << "Float" << endl;
       LatticeExprNode node(aF);
       LatticeExpr<float> expr(node);
       if (!checkFloat(expr, aFVal, shape, supress)) ok = false;

       LatticeExpr<float> expr2(expr);
       if (!checkFloat(expr2, aFVal, shape, supress)) ok = false;
 
       LatticeExpr<float> expr3;
       expr3 = expr;      
       if (!checkFloat(expr2, aFVal, shape, supress)) ok = false;

       Lattice<float>* pExpr;
       pExpr = expr.clone();
       if (!checkFloat(*pExpr, aFVal, shape, supress)) ok = false;
       delete pExpr;
     }

//
// <double>
// 
     {
       cout << "Double" << endl;
       LatticeExprNode node(aD);
       LatticeExpr<double> expr(node);
       if (!checkDouble(expr, aDVal, shape, supress)) ok = false;

       LatticeExpr<double> expr2(expr);
       if (!checkDouble(expr2, aDVal, shape, supress)) ok = false;
 
       LatticeExpr<double> expr3;
       expr3 = expr;      
       if (!checkDouble(expr2, aDVal, shape, supress)) ok = false;

       Lattice<double>* pExpr;
       pExpr = expr.clone();
       if (!checkDouble(*pExpr, aDVal, shape, supress)) ok = false;
       delete pExpr;
     }

//
// <Complex>
// 
     {
       cout << "Complex" << endl;
       LatticeExprNode node(aC);
       LatticeExpr<Complex> expr(node);
       if (!checkComplex(expr, aCVal, shape, supress)) ok = false;

       LatticeExpr<Complex> expr2(expr);
       if (!checkComplex(expr2, aCVal, shape, supress)) ok = false;
 
       LatticeExpr<Complex> expr3;
       expr3 = expr;      
       if (!checkComplex(expr2, aCVal, shape, supress)) ok = false;

       Lattice<Complex>* pExpr;
       pExpr = expr.clone();
       if (!checkComplex(*pExpr, aCVal, shape, supress)) ok = false;
       delete pExpr;
     }

//
// <DComplex>
// 
     {
       cout << "DComplex" << endl;
       LatticeExprNode node(aDC);
       LatticeExpr<DComplex> expr(node);
       if (!checkDComplex(expr, aDCVal, shape, supress)) ok = false;

       LatticeExpr<DComplex> expr2(expr);
       if (!checkDComplex(expr2, aDCVal, shape, supress)) ok = false;
 
       LatticeExpr<DComplex> expr3;
       expr3 = expr;      
       if (!checkDComplex(expr2, aDCVal, shape, supress)) ok = false;

       Lattice<DComplex>* pExpr;
       pExpr = expr.clone();
       if (!checkDComplex(*pExpr, aDCVal, shape, supress)) ok = false;
       delete pExpr;
     }

//
// <bool>
// 
     {
       cout << "Bool" << endl;
       LatticeExprNode node(aB);
       LatticeExpr<bool> expr(node);
       if (!checkBool(expr, aBVal, shape, supress)) ok = false;

       LatticeExpr<bool> expr2(expr);
       if (!checkBool(expr2, aBVal, shape, supress)) ok = false;
 
       LatticeExpr<bool> expr3;
       expr3 = expr;      
       if (!checkBool(expr2, aBVal, shape, supress)) ok = false;

       Lattice<bool>* pExpr;
       pExpr = expr.clone();
       if (!checkBool(*pExpr, aBVal, shape, supress)) ok = false;
       delete pExpr;
     }



  cout << endl;
  if (!ok) {
     cout << "not ok" << endl;
     return 1;
  } else {
     cout << "ok" << endl;
  }

 } catch (std::exception& x) {
    cerr << "aipserror: error " << x.what() << endl;
    return 1;
 } 
 
 return 0;

}


bool checkFloat(Lattice<float>& expr, 
                const float result,
                const IPosition shape,
                const bool supress)
{
   bool ok = true;
   Array<float> outArr(shape);  
   ArrayLattice<float> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = false;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = false;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }


   COWPtr<Array<float> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   return ok;
}


bool checkDouble(Lattice<double>& expr, 
                const double result,
                const IPosition shape,
                const bool supress)
{
   bool ok = true;
   Array<double> outArr(shape);  
   ArrayLattice<double> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = false;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = false;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }


   COWPtr<Array<double> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   return ok;
}


bool checkComplex(Lattice<Complex>& expr, 
                const Complex result,
                const IPosition shape,
                const bool supress)
{
   bool ok = true;
   Array<Complex> outArr(shape);  
   ArrayLattice<Complex> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = false;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = false;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }


   COWPtr<Array<Complex> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   return ok;
}



bool checkDComplex(Lattice<DComplex>& expr, 
                const DComplex result,
                const IPosition shape,
                const bool supress)
{
   bool ok = true;
   Array<DComplex> outArr(shape);  
   ArrayLattice<DComplex> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = false;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = false;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }


   COWPtr<Array<DComplex> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   return ok;
}




bool checkBool(Lattice<bool>& expr, 
                const bool result,
                const IPosition shape,
                const bool supress)
{
   bool ok = true;
   Array<bool> outArr(shape);  
   ArrayLattice<bool> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = false;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = false;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (std::exception& x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.what() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }


   COWPtr<Array<bool> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = false;
   }

   return ok;
}



