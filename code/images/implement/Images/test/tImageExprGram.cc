//# tImageExprGram.cc: Test program for image expression parser
//# Copyright (C) 1998,1999
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

#include <trial/Images/ImageExprParse.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/ArrayLattice.h>
#include <aips/Tables/Table.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Containers/Block.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Inputs/Input.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>


main (int argc, char *argv[])
{
 Bool foundError = False;

 try {
    Input inp(1);
    inp.Version(" ");
    inp.Create("nx", "2", "Number of pixels along the x-axis", "int");
    inp.Create("ny", "2", "Number of pixels along the y-axis", "int");
    inp.ReadArguments(argc, argv);

    const uInt nx=inp.GetInt("nx");
    const uInt ny=inp.GetInt("ny");

    Double aVal = 0.0;
    Double bVal = 1.0;
    Double cVal = 2.0;
    Double dVal = 3.0;
    Double eVal = 4.0;
    Double fVal = 5.0;
    Double gVal = 6.0;
    Double hVal = 7.0;
    Bool aBoolVal = False;
    Bool bBoolVal = False;
    ArrayLattice<Double> a(IPosition (2,nx,ny));
    ArrayLattice<Bool> aBool(IPosition (2,nx,ny));
    a.set(aVal);
    aBool.set(aBoolVal);
    {
	PagedArray<Double> b(IPosition (2,nx,ny), "b");
	PagedArray<Double> c(IPosition (2,nx,ny), "c");
	PagedArray<Double> d(IPosition (2,nx,ny), "d");
	PagedArray<Double> e(IPosition (2,nx,ny), "e");
	PagedArray<Double> f(IPosition (2,nx,ny), "f");
	PagedArray<Double> g(IPosition (2,nx,ny), "g");
	PagedArray<Double> h(IPosition (2,nx,ny), "h");
	PagedArray<Bool> bBool(IPosition (2,nx,ny), "bBool");
	
	b.set(bVal);
	c.set(cVal);
	d.set(dVal);
	e.set(eVal);
	f.set(fVal);
	g.set(gVal);
	h.set(hVal);
	bBool.set(bBoolVal);
    }

    Array<Double> aArr(a.shape());
    Array<Bool> aBoolArr(aBool.shape());

  {
    cout << endl;
    cout << "Expr:  a = 1" << endl;
    LatticeExpr<Double> expr (ImageExprParse::command ("1.0"));
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
    LatticeExpr<Double> expr(ImageExprParse::command ("b"));
    a.copyData(expr);
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
    cout << "Expr:  a = sin(\\c)" << endl;
    LatticeExpr<Double> expr(ImageExprParse::command ("sin(\\c)"));
    a.copyData(expr);
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
    cout << "Expr:  a = 'c'+2" << endl;
    LatticeExpr<Double> expr(ImageExprParse::command ("'c'+2"));
    a.copyData(expr);
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
    cout << "Expr:  a = b+('c'+d)     (using $n notation)" << endl;
    PagedArray<Double> b("b");
    PagedArray<Double> c("c");
    PagedArray<Double> d("d");
    Block<LatticeExprNode> temps(3);
    temps[0] = LatticeExprNode(b);
    temps[1] = LatticeExprNode(c);
    temps[2] = LatticeExprNode(d);
    LatticeExpr<Double> expr(ImageExprParse::command ("$1+($2+$3)", temps));
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
    cout << "Expr:  a = 3.5*b + cos('c')-10/min('c',d)*-e*log(b)-pi()" << endl;

    LatticeExpr<Double> expr( ImageExprParse::command
               ("(3.5*b) + (cos('c')) - (10/min('c',d)*(-e)*log(b)) - (pi()) "));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = 3.5*bVal + cos(cVal) -
	            10/min(cVal,dVal)*-eVal*log(bVal) - C::pi;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = (b+'c'-d/2.0*-b) + pi()" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command ("(b+'c'-d/2.0*-b)+pi()"));
    a.copyData(expr);
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
    cout << "Expr:  a = pow('c',d)" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command ("pow('c',d)"));
    a.copyData(expr);
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
    cout << "Expr:  a = fmod('c'*2,d)" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command ("fmod('c'*2,d)"));
    a.copyData(expr);
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
    cout << "Expr:  a = pow('c',2.3)" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command ("pow('c',2.3)"));
    a.copyData(expr);
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

    LatticeExpr<Double> expr(ImageExprParse::command ("cos(b)"));
    a.copyData(expr);
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

    LatticeExpr<Double> expr (ImageExprParse::command ("cos(sin(b))"));
    a.copyData(expr);
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

    LatticeExpr<Double> expr(ImageExprParse::command ("ntrue(b>=1)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = a.shape().product();
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  aBool = T||F" << endl;

    LatticeExpr<Bool> expr(ImageExprParse::command ("T||F"));
    aBool.copyData(expr);
    aBool.getSlice(aBoolArr, IPosition(aBoolArr.ndim(),0), 
		   aBoolArr.shape(), IPosition(aBoolArr.ndim(),1));
    Bool result = ToBool(True||False);
    if (! allEQ (aBoolArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aBoolArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  aBool = !bBool" << endl;

    LatticeExpr<Bool> expr(ImageExprParse::command ("!bBool"));
    aBool.copyData(expr);
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
    cout << "Expr:  a = sum(e)/nelements(b) + min('c') + max('c', mean('c'+d))" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command
	          ("sum(e)/nelements(b) + min('c') + max('c', mean('c'+d))"));
    a.copyData(expr);
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
    cout << "Expr:  a = sum(e[bBool])" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command
	          ("sum(e[bBool])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = (bBoolVal  ?  aArr.nelements()*eVal : 0);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    cout << "Expr:  a = sum((e + sum(f[b>0]*c)/d)[!bBool])" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command
	          ("sum((e + sum(f[b>0]*c)/d)[!bBool])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       aArr.shape(), IPosition(aArr.ndim(),1));
    Double result = (bVal>0  ?  aArr.nelements()*fVal*cVal/dVal : 0);
    result = (bBoolVal  ?  0 : aArr.nelements()*(eVal+result));
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr.ac() << endl;
	foundError = True;
    }
  }
  {
    {
	PagedArray<Double> ap (a.shape(), "a");
	ap.copyData (a);
    }
    cout << "Expr:  a = min(a+10,5)" << endl;

    LatticeExpr<Double> expr(ImageExprParse::command ("min(a+10,5)"));
    a.copyData(expr);
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

 } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    foundError = True;
 } end_try;

 // Delete all created tables (if they exist).
 Table tab; 
 if (Table::isReadable("a")) tab =  Table ("a", Table::Delete);
 if (Table::isReadable("b")) tab =  Table ("b", Table::Delete);
 if (Table::isReadable("c")) tab =  Table ("c", Table::Delete);
 if (Table::isReadable("d")) tab =  Table ("d", Table::Delete);
 if (Table::isReadable("e")) tab =  Table ("e", Table::Delete);
 if (Table::isReadable("f")) tab =  Table ("f", Table::Delete);
 if (Table::isReadable("g")) tab =  Table ("g", Table::Delete);
 if (Table::isReadable("h")) tab =  Table ("h", Table::Delete);
 if (Table::isReadable("bBool")) tab =  Table ("bBool", Table::Delete);

  if (foundError) {
     return 1;
  }
 return 0;
}
