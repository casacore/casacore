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
//#
//# $Id$

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
Bool checkFloat(Lattice<Float>& expr, 
                const Float result,
                const IPosition shape,
                const Bool supress);

Bool checkDouble(Lattice<Double>& expr, 
                const Double result,
                const IPosition shape,
                const Bool supress);

Bool checkComplex(Lattice<Complex>& expr, 
                const Complex result,
                const IPosition shape,
                const Bool supress);

Bool checkDComplex(Lattice<DComplex>& expr, 
                const DComplex result,
                const IPosition shape,
                const Bool supress);

Bool checkBool(Lattice<Bool>& expr, 
                const Bool result,
                const IPosition shape,
                const Bool supress);


int main (int argc, const char* argv[])
{
 try {
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "2", "Number of pixels along the x-axis", "int");
    inp.create("ny", "2", "Number of pixels along the y-axis", "int");
    inp.create("sup", "False", "Supress expected exceptions messages", "Bool");
    inp.readArguments(argc, argv);

    const uInt nx=inp.getInt("nx");
    const uInt ny=inp.getInt("ny");
    const Bool supress=inp.getBool("sup");

    IPosition shape(2,nx,ny);
    Bool ok = True;
 
// Bool Lattices
    
    ArrayLattice<Bool> aB(shape);
    Bool aBVal = True;
    aB.set(aBVal);

    
// FLoat Lattices
    
    ArrayLattice<Float> aF(shape);   
    Float aFVal = 2.0;
    aF.set(aFVal);
    
    
// Double Lattices
 
    ArrayLattice<Double> aD(shape);
    Double aDVal = 2.0;
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
// <Float>
// 
     {
       cout << "Float" << endl;
       LatticeExprNode node(aF);
       LatticeExpr<Float> expr(node);
       if (!checkFloat(expr, aFVal, shape, supress)) ok = False;

       LatticeExpr<Float> expr2(expr);
       if (!checkFloat(expr2, aFVal, shape, supress)) ok = False;
 
       LatticeExpr<Float> expr3;
       expr3 = expr;      
       if (!checkFloat(expr2, aFVal, shape, supress)) ok = False;

       Lattice<Float>* pExpr;
       pExpr = expr.clone();
       if (!checkFloat(*pExpr, aFVal, shape, supress)) ok = False;
       delete pExpr;
     }

//
// <Double>
// 
     {
       cout << "Double" << endl;
       LatticeExprNode node(aD);
       LatticeExpr<Double> expr(node);
       if (!checkDouble(expr, aDVal, shape, supress)) ok = False;

       LatticeExpr<Double> expr2(expr);
       if (!checkDouble(expr2, aDVal, shape, supress)) ok = False;
 
       LatticeExpr<Double> expr3;
       expr3 = expr;      
       if (!checkDouble(expr2, aDVal, shape, supress)) ok = False;

       Lattice<Double>* pExpr;
       pExpr = expr.clone();
       if (!checkDouble(*pExpr, aDVal, shape, supress)) ok = False;
       delete pExpr;
     }

//
// <Complex>
// 
     {
       cout << "Complex" << endl;
       LatticeExprNode node(aC);
       LatticeExpr<Complex> expr(node);
       if (!checkComplex(expr, aCVal, shape, supress)) ok = False;

       LatticeExpr<Complex> expr2(expr);
       if (!checkComplex(expr2, aCVal, shape, supress)) ok = False;
 
       LatticeExpr<Complex> expr3;
       expr3 = expr;      
       if (!checkComplex(expr2, aCVal, shape, supress)) ok = False;

       Lattice<Complex>* pExpr;
       pExpr = expr.clone();
       if (!checkComplex(*pExpr, aCVal, shape, supress)) ok = False;
       delete pExpr;
     }

//
// <DComplex>
// 
     {
       cout << "DComplex" << endl;
       LatticeExprNode node(aDC);
       LatticeExpr<DComplex> expr(node);
       if (!checkDComplex(expr, aDCVal, shape, supress)) ok = False;

       LatticeExpr<DComplex> expr2(expr);
       if (!checkDComplex(expr2, aDCVal, shape, supress)) ok = False;
 
       LatticeExpr<DComplex> expr3;
       expr3 = expr;      
       if (!checkDComplex(expr2, aDCVal, shape, supress)) ok = False;

       Lattice<DComplex>* pExpr;
       pExpr = expr.clone();
       if (!checkDComplex(*pExpr, aDCVal, shape, supress)) ok = False;
       delete pExpr;
     }

//
// <Bool>
// 
     {
       cout << "Bool" << endl;
       LatticeExprNode node(aB);
       LatticeExpr<Bool> expr(node);
       if (!checkBool(expr, aBVal, shape, supress)) ok = False;

       LatticeExpr<Bool> expr2(expr);
       if (!checkBool(expr2, aBVal, shape, supress)) ok = False;
 
       LatticeExpr<Bool> expr3;
       expr3 = expr;      
       if (!checkBool(expr2, aBVal, shape, supress)) ok = False;

       Lattice<Bool>* pExpr;
       pExpr = expr.clone();
       if (!checkBool(*pExpr, aBVal, shape, supress)) ok = False;
       delete pExpr;
     }



  cout << endl;
  if (!ok) {
     cout << "not ok" << endl;
     return 1;
  } else {
     cout << "ok" << endl;
  }

 } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    return 1;
 } 
 
 return 0;

}


Bool checkFloat(Lattice<Float>& expr, 
                const Float result,
                const IPosition shape,
                const Bool supress)
{
   Bool ok = True;
   Array<Float> outArr(shape);  
   ArrayLattice<Float> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = False;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = False;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }


   COWPtr<Array<Float> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   return ok;
}


Bool checkDouble(Lattice<Double>& expr, 
                const Double result,
                const IPosition shape,
                const Bool supress)
{
   Bool ok = True;
   Array<Double> outArr(shape);  
   ArrayLattice<Double> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = False;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = False;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }


   COWPtr<Array<Double> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   return ok;
}


Bool checkComplex(Lattice<Complex>& expr, 
                const Complex result,
                const IPosition shape,
                const Bool supress)
{
   Bool ok = True;
   Array<Complex> outArr(shape);  
   ArrayLattice<Complex> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = False;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = False;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }


   COWPtr<Array<Complex> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   return ok;
}



Bool checkDComplex(Lattice<DComplex>& expr, 
                const DComplex result,
                const IPosition shape,
                const Bool supress)
{
   Bool ok = True;
   Array<DComplex> outArr(shape);  
   ArrayLattice<DComplex> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = False;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = False;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }


   COWPtr<Array<DComplex> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   return ok;
}




Bool checkBool(Lattice<Bool>& expr, 
                const Bool result,
                const IPosition shape,
                const Bool supress)
{
   Bool ok = True;
   Array<Bool> outArr(shape);  
   ArrayLattice<Bool> outLat(shape);
   IPosition origin(shape); origin = 0;
   IPosition stride(outArr.ndim(),1);
          
   if (expr.shape() != shape) {
      cout << "   Shape should be " << shape << endl;
      cout << "   Shape is " << expr.shape()  << endl;
      ok = False;
   }
   if (expr.isWritable()) {
      cout << "   LatticeExpr should not be writable" << endl;
      ok = False;
   }

   try {
     expr.putSlice(outArr, origin, stride);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 
   try {
     expr.putSlice(outArr, origin);
   } catch (AipsError x) {
     if (!supress)  cout << "   Caught expected exception; message is: " << x.getMesg() << endl;
   } 

   outLat.copyData(expr);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   Slicer slicer(origin, shape, stride);
   expr.getSlice(outArr, slicer);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }


   COWPtr<Array<Bool> > moo;
   expr.getSlice(moo, origin, shape, stride);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.getSlice(moo, slicer);
   outArr.reference(moo.rwRef());
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   expr.copyDataTo(outLat);
   outLat.getSlice(outArr, origin, shape, stride);
   if (!allEQ (outArr, result)) {
	cout << "   Result should be " << result << endl;
	cout << "   Result is " << outArr(origin) << endl;
	ok = False;
   }

   return ok;
}



