//# LELFunction.cc:  this defines non-templated classes in LELFunction.h
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
#include <trial/Lattices/ArrayLattice.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Inputs/Input.h>
#include <aips/Exceptions/Error.h>
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

    ArrayLattice<Double> a(IPosition (2,nx,ny));
    ArrayLattice<Double> b(IPosition (2,nx,ny));
    ArrayLattice<Double> c(IPosition (2,nx,ny));
    ArrayLattice<Double> d(IPosition (2,nx,ny));
    ArrayLattice<Double> e(IPosition (2,nx,ny));
    ArrayLattice<Double> f(IPosition (2,nx,ny));
    ArrayLattice<Double> g(IPosition (2,nx,ny));
    ArrayLattice<Double> h(IPosition (2,nx,ny));
    ArrayLattice<Double> i(IPosition (2,nx,ny));
    ArrayLattice<Bool> aBool(IPosition (2,nx,ny));
    ArrayLattice<Bool> bBool(IPosition (2,nx,ny));


    Double aVal = 0.0;
    a.set(aVal);
    Double bVal = 1.0;
    b.set(1.0);
    Double cVal = 2.0;
    c.set(cVal);
    Double dVal = 3.0;
    d.set(dVal);
    Double eVal = 4.0;
    e.set(eVal);
    Double fVal = 5.0;
    f.set(fVal);
    Double gVal = 6.0;
    g.set(gVal);
    Double hVal = 7.0;
    h.set(hVal);
    Double iVal = 0.0;
    i.set(iVal);
    Bool aBoolVal = False;
    aBool.set(aBoolVal);
    Bool bBoolVal = False;
    bBool.set(bBoolVal);

    Array<Double> aArr(a.shape());
    Array<Double> bArr(b.shape());
    Array<Double> cArr(c.shape());
    Array<Double> dArr(d.shape());
    Array<Double> eArr(e.shape());
    Array<Double> fArr(f.shape());
    Array<Double> gArr(g.shape());
    Array<Double> hArr(h.shape());
    Array<Double> iArr(i.shape());

    Array<Bool> aBoolArr(a.shape());
    Array<Bool> bBoolArr(b.shape());

    Bool foundError = False;

  {
    cout << endl;
    cout << "Expr:  a = 1" << endl;
    LatticeExpr<Double> expr(LatticeExprNode(1.0));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = 1.0;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = b" << endl;
    a.copyData(b);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = bVal;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result  << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = sin(c)" << endl;
    LatticeExpr<Double> expr(sin(c));
    a.copyData(sin(c));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = sin(cVal);
    if (! allEQ (aArr, sin(cVal))) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }

  {
    cout << "Expr:  a = c+2" << endl;
    a.copyData(c+2);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = cVal+2;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = b+(c+d)" << endl;
    LatticeExpr<Double> expr(b+(c+d));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = bVal + cVal + dVal;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = 3.5*b + cos(c)-10/min(c,d)*-e*log(b)-C::pi" << endl;

    a.copyData( (3.5*b) + (cos(c)) - (10/min(c,d)*(-e)*log(b)) - (C::pi) );
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    b.getSlice(bArr, IPosition(bArr.ndim(),0), 
	       bArr.shape(), IPosition(bArr.ndim(),1));
    c.getSlice(cArr, IPosition(cArr.ndim(),0), 
	       cArr.shape(), IPosition(cArr.ndim(),1));
    d.getSlice(dArr, IPosition(dArr.ndim(),0), 
	       dArr.shape(), IPosition(dArr.ndim(),1));
    e.getSlice(eArr, IPosition(eArr.ndim(),0), 
	       eArr.shape(), IPosition(eArr.ndim(),1));
    Double result = 3.5*bVal + cos(cVal) -
	            10/min(cVal,dVal)*-eVal*log(bVal) - C::pi;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
    if (! allEQ (bArr, bVal)) {
	cout << "b is " << bArr.ac() << " and should be " << bVal << endl;
	foundError = True;
    }
    if (! allEQ (cArr, cVal)) {
	cout << "c is " << cArr.ac() << " and should be " << cVal << endl;
	foundError = True;
    }
    if (! allEQ (dArr, dVal)) {
	cout << "d is " << dArr.ac() << " and should be " << dVal << endl;
	foundError = True;
    }
    if (! allEQ (eArr, eVal)) {
	cout << "e is " << eArr.ac() << " and should be " << eVal << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = (b+c-d/2.0*-b) + pi" << endl;

    a.copyData((b+c-d/2.0*-b)+C::pi);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = (bVal + cVal - dVal / 2*-bVal) + C::pi;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = pow(c,d)" << endl;

    a.copyData(pow(c,d));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = pow(cVal,dVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = fmod(c*2,d)" << endl;

    a.copyData(fmod(c*2,d));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = fmod(cVal*2,dVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = pow(c,2.3)" << endl;

    a.copyData(pow(c,2.3));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = pow(cVal,2.3);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = cos(b)" << endl;

    a.copyData(cos(b));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = cos(bVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = cos(sin(b))" << endl;

    a.copyData (cos(sin(b)));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = cos(sin(bVal));
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = ntrue(b>=1)" << endl;

    a.copyData(ntrue(b>=1));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = b.shape().product();
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  aBool = !bBool" << endl;

    aBool.copyData(LatticeExpr<Bool>(!bBool));
    aBool.getSlice(aBoolArr, IPosition(aBoolArr.ndim(),0), 
		   aBoolArr.shape(), IPosition(aBoolArr.ndim(),1));
    Bool result = ToBool(!bBoolVal);
    if (! allEQ (aBoolArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aBoolArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = sum(e)/nelements(b) + min(c) + max(c, mean(c+d))" << endl;

    a.copyData(sum(e)/nelements(b) + min(c) + max(c, mean(c+d)));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = eVal+cVal+(cVal+dVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = min(a+10,5)" << endl;

    a.copyData(min(a+10,5));
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = 5;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }

  cout << endl;
  if (foundError) {
     return 1;
  }

 } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    return 1;
 } end_try;
 
 return 0;
}
