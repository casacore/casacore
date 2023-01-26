//# tImageExprGram.cc: Test program for image expression parser
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/Lattices/PagedArray.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCPixelSet.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
//# This function simulates the same function in DOImage2.cc.
String substituteOID (Block<LatticeExprNode>& nodes,
		      String& exprName,
		      const String& expr)
{
  nodes.resize (0, false, true);
  exprName = expr;
  return expr;
}
void makeRegionBlock (PtrBlock<const ImageRegion*>& regions,
		      const Record&,
		      LogIO&)
{
   for (uint32_t j=0; j<regions.nelements(); j++) {
      delete regions[j];
   }
   regions.resize (0, true, true);
}

//# This function is a copy of the expr function in DOImage2.cc, which
//# failed mysteriously on sneffels (RedHat Linux 5.2) in April 1999.
void doExpr (const String& expr, const Record& regions)
{
  LogIO os(LogOrigin("image", "expr(const String& expr)", WHERE));

// Get LatticeExprNode (tree) from parser
// Convert the GlishRecord containing regions to a
// PtrBlock<const ImageRegion*>.

  if (expr.empty()) {
    os << "You must specify an expression" << LogIO::EXCEPTION;
  }
  Block<LatticeExprNode> temps;
  String exprName;
  String newexpr = substituteOID (temps, exprName, expr);
  PtrBlock<const ImageRegion*> tempRegs;
  makeRegionBlock (tempRegs, regions, os);
  LatticeExprNode node = ImageExprParse::command (newexpr, temps, tempRegs);
  
// Delete the ImageRegions (by using an empty GlishRecord).
  
  makeRegionBlock (tempRegs, Record(), os);
  
// Make the ImageExpr object.  It will throw an exception if there
// are no true coordinates
  
  LatticeExpr<float> latEx(node); 
  ImageInterface<float>* pImage = new ImageExpr<float>(latEx, exprName);
  if (pImage==0) {
    os << "Failed to create PagedImage" << LogIO::EXCEPTION;
  }
}


int main (int argc, const char* argv[])
{
 bool foundError = false;

 try {
    cout << ">>>" << endl;
    Input inp(1);
    inp.version("1.0");
    inp.create("nx", "10", "Number of pixels along the x-axis", "int");
    inp.create("ny", "10", "Number of pixels along the y-axis", "int");
    inp.readArguments(argc, argv);
    cout << "<<<" << endl;

    const uint32_t nx=inp.getInt("nx");
    const uint32_t ny=inp.getInt("ny");

    double aVal = 0.0;
    double bVal = 1.0;
    double cVal = 2.0;
    double dVal = 3.0;
    double eVal = 4.0;
    double fVal = 5.0;
    double gVal = 6.0;
    double hVal = 7.0;
    bool aBoolVal = false;
    bool bBoolVal = false;
    IPosition shape(2, nx, ny);
    TiledShape tshp(shape, IPosition(2,(nx+1)/2, (ny+1)/2));
    PagedArray<double> a(tshp, "paa");
    PagedArray<bool> aBool(tshp, "paab");
    a.set(aVal);
    aBool.set(aBoolVal);
    {
	PagedArray<double> b(tshp, "b");
	PagedArray<double> c(tshp, "c");
	PagedArray<double> d(tshp, "d");
	PagedArray<double> e(tshp, "e");
	PagedArray<double> f(tshp, "f");
	PagedArray<double> g(tshp, "g");
	PagedArray<double> h(tshp, "h");
	PagedArray<bool> bBool(tshp, "bBool");
	PagedArray<double> kpa(tshp, "kpa");
	
	b.set(bVal);
	c.set(cVal);
	d.set(dVal);
	e.set(eVal);
	f.set(fVal);
	g.set(gVal);
	h.set(hVal);
	bBool.set(bBoolVal);
	Array<double> arr(shape);
	indgen(arr);
	kpa.put(arr);
    }

    Array<double> aArr(shape);
    Array<bool> aBoolArr(shape);

  {
    cout << endl;
    try {
      doExpr ("xxx", Record());
    } catch (std::exception& x) {
      cout << x.what() << endl;
    } 
    try {
      LatticeExpr<double> expr (ImageExprParse::command ("b/a1"));
    } catch (std::exception& x) {
      cout << x.what() << endl;
    } 
    try {
      LatticeExpr<double> expr (ImageExprParse::command ("a1/b"));
    } catch (std::exception& x) {
      cout << x.what() << endl;
    } 
    try {
      LatticeExpr<double> expr (ImageExprParse::command ("b/b*"));
    } catch (std::exception& x) {
      cout << x.what() << endl;
    } 
    try {
      LatticeExpr<double> expr (ImageExprParse::command ("min(b,b,b)"));
    } catch (std::exception& x) {
      cout << x.what() << endl;
    } 
  }
  {
    cout << endl;
    cout << "Expr:  a = 1" << endl;
    LatticeExpr<double> expr (ImageExprParse::command ("1.0"));
    cout << "Images used: " << ImageExprParse::getImageNames() << endl;
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = 1.0;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = b" << endl;
    LatticeExpr<double> expr(ImageExprParse::command ("b"));
    cout << "Images used: " << ImageExprParse::getImageNames() << endl;
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = bVal;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result  << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = sin(\\c)" << endl;
    LatticeExpr<double> expr(ImageExprParse::command ("sin(\\c)"));
    cout << "Images used: " << ImageExprParse::getImageNames() << endl;
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = sin(cVal);
    if (! allEQ (aArr, sin(cVal))) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }

  {
    cout << "Expr:  a = 'c'+2" << endl;
    LatticeExpr<double> expr(ImageExprParse::command ("'c'+2"));
    cout << "Images used: " << ImageExprParse::getImageNames() << endl;
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = cVal+2;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = b+('c'+d)[region]     (using $n notation)" << endl;
    PagedArray<double> b("b");
    PagedArray<double> c("c");
    PagedArray<double> d("d");
    Block<LatticeExprNode> temps(3);
    temps[0] = LatticeExprNode(b);
    temps[1] = LatticeExprNode(c);
    temps[2] = LatticeExprNode(d);
    PtrBlock<const ImageRegion*> regions(1);
    regions[0] = new ImageRegion(LCBox(shape));
    LatticeExpr<double> expr(ImageExprParse::command
         ("$1 + ($2 + $3)[$R1]",
						      temps, regions));
    delete regions[0];
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = bVal + cVal + dVal;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = nelements(b[$region]" << endl;
    Block<LatticeExprNode> temps(0);
    PtrBlock<const ImageRegion*> regions(1);
    Matrix<bool> mask(shape-1);
    mask = false;
    mask(0,0) = true;
    regions[0] = new ImageRegion(LCPixelSet(mask,LCBox(IPosition(2,0),
						       shape-2, shape)));
    LatticeExpr<double> expr(ImageExprParse::command ("nelements(b[$R1])",
						      temps, regions));
    delete regions[0];
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = 1;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = nelements(b[$region1 || $region2) - "
	    "length(b[$region1],0)" << endl;
    Block<LatticeExprNode> temps(0);
    PtrBlock<const ImageRegion*> regions(2);
    Matrix<bool> mask1(shape-1);
    Matrix<bool> mask2(shape-1);
    mask1 = false;
    mask2 = false;
    mask1(0,0) = true;
    mask2(shape-2) = true;
    regions[0] = new ImageRegion(LCPixelSet(mask1,LCBox(IPosition(2,0),
							shape-2, shape)));
    regions[1] = new ImageRegion(LCPixelSet(mask2,LCBox(IPosition(2,0),
							shape-2, shape)));
    LatticeExpr<double> expr(ImageExprParse::command
			     ("nelements(b[$R1 || $R2]) - "
			      "length(b[$R1],0)",
			      temps, regions));
    cout << "Images used: " << ImageExprParse::getImageNames() << endl;
    delete regions[0];
    delete regions[1];
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = 2 - (shape(0)-1);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = 3.5*b + cos('c')-10/min('c',d)*-e*log(b)-pi()" << endl;

    LatticeExpr<double> expr( ImageExprParse::command
               ("(3.5*b) + (cos('c')) - (10/min('c',d)*(-e)*log(b)) - (pi()) "));
    cout << "Images used: " << ImageExprParse::getImageNames() << endl;
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = 3.5*bVal + cos(cVal) -
	            10/min(cVal,dVal)*-eVal*log(bVal) - C::pi;
    if (! allNear (aArr, result, 1.0e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = (b+'c'-d/2.0*-b) + pi()" << endl;

    LatticeExpr<double> expr(ImageExprParse::command ("(b+'c'-d/2.0*-b)+pi()"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = (bVal + cVal - dVal / 2*-bVal) + C::pi;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = pow('c',d)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command ("pow('c',d)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = pow(cVal,dVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = fmod('c'*2,d)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command ("fmod('c'*2,d)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = fmod(cVal*2,dVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = pow('c',2.3)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command ("pow('c',2.3)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = pow(cVal,2.3);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = cos(b)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command ("cos(b)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = cos(bVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = cos(sin(b))" << endl;

    LatticeExpr<double> expr (ImageExprParse::command ("cos(sin(b))"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = cos(sin(bVal));
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = ntrue(b>=1)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command ("ntrue(b>=1)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = shape.product();
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  aBool = T||F" << endl;

    LatticeExpr<bool> expr(ImageExprParse::command ("T||F"));
    aBool.copyData(expr);
    aBool.getSlice(aBoolArr, IPosition(aBoolArr.ndim(),0), 
		   shape, IPosition(aBoolArr.ndim(),1));
    bool result = (true||false);
    if (! allEQ (aBoolArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aBoolArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  aBool = !bBool" << endl;

    LatticeExpr<bool> expr(ImageExprParse::command ("!bBool"));
    aBool.copyData(expr);
    aBool.getSlice(aBoolArr, IPosition(aBoolArr.ndim(),0), 
		   shape, IPosition(aBoolArr.ndim(),1));
    bool result = (!bBoolVal);
    if (! allEQ (aBoolArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aBoolArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = sum(e)/nelements(b) + min('c') + max('c', mean('c'+d))" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("sum(e)/nelements(b) + min('c') + max('c', mean('c'+d))"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = eVal+cVal+(cVal+dVal);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = sum(e[bBool])" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("sum(e[bBool])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = (bBoolVal  ?  aArr.nelements()*eVal : 0);
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = sum((e + sum(f[b>0]*c)/d)[!bBool])" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("sum((e + sum(f[b>0]*c)/d)[!bBool])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = (bVal>0  ?  aArr.nelements()*fVal*cVal/dVal : 0);
    result = (bBoolVal  ?  0 : aArr.nelements()*(eVal+result));
    if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = median(b)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("median(b)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = bVal;
    if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = fractile(b,0.2)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("fractile(b,0.2)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = bVal;
    if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = fractilerange(b,0.2)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("fractilerange(b,0.2)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = 0.0;
    if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = fractilerange(b,0.2,0.6)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("fractilerange(b,0.2,0.6)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = 0.0;
    if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    {
	PagedArray<double> ap (tshp, "a");
	ap.copyData (a);
    }
    cout << "Expr:  a = min(a+10,5)" << endl;

    LatticeExpr<double> expr(ImageExprParse::command ("min(a+10,5)"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = 5;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }
  {
    cout << "Expr:  a = b+sum(kpa[indexin(0,[0:1,7,3:6:2])])" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("b+sum(kpa[indexin(0,[0:1,7,3:6:2])])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    if (shape(0) > 7) {
      int32_t n = shape(1);
      double result = 1 + (0+1+7+3+5)*n + 5*shape(0)*n*(n-1)/2;
      if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
      }
    }
  }
  {
    cout << "Expr:  a = b+sum(kpa[indexnotin(1,[0:1,9,3:6:3,9])])" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("b+sum(kpa[indexnotin(1,[0:1,9,3:6:3,9])])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    if (shape(1) > 9) {
      int32_t n = shape(0);
      double result = 1 + (0+1+9+3+6)*n*n + 5*n*(n-1)/2;
      n *= shape(1);
      result = n*(n-1)/2 - result + 2*1;
      if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
      }
    }
  }
  {
    cout << "Expr:  a = b+sum(kpa[index1 in [0:1,9,3:6:3,9]])" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("b+sum(kpa[index1 in [0:1,9,3:6:3,9]])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    if (shape(1) > 9) {
      int32_t n = shape(0);
      double result = 1 + (0+1+9+3+6)*n*n + 5*n*(n-1)/2;
      if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
      }
    }
  }
  {
    cout << "Expr:  a = b+sum(kpa[index1 not in [0:1,9,3:6:3,9]])" << endl;

    LatticeExpr<double> expr(ImageExprParse::command
	          ("b+sum(kpa[index1 not in [0:1,9,3:6:3,9]])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    if (shape(1) > 9) {
      int32_t n = shape(0);
      double result = 1 + (0+1+9+3+6)*n*n + 5*n*(n-1)/2;
      n *= shape(1);
      result = n*(n-1)/2 - result + 2*1;
      if (! allNear (aArr, result, 1e-10)) {
	cout << "Result should be " << result << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
      }
    }
  }
  {
    cout << "Expr:  a = rebin(b,[1,2/2])" << endl;
    LatticeExpr<double> expr(ImageExprParse::command ("rebin(b,[1,2/2])"));
    a.copyData(expr);
    a.getSlice(aArr, IPosition(aArr.ndim(),0), 
	       shape, IPosition(aArr.ndim(),1));
    double result = bVal;
    if (! allEQ (aArr, result)) {
	cout << "Result should be " << result  << endl;
	cout << "Result is " << aArr << endl;
	foundError = true;
    }
  }

  cout << endl;

 } catch (std::exception& x) {
    cerr << "aipserror: error " << x.what() << endl;
    foundError = true;
 } 

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
 if (Table::isReadable("kpa")) tab =  Table ("kpa", Table::Delete);
 if (Table::isReadable("bBool")) tab =  Table ("bBool", Table::Delete);
 if (Table::isReadable("paa")) tab =  Table ("paa", Table::Delete);
 if (Table::isReadable("paab")) tab =  Table ("paab", Table::Delete);

  if (foundError) {
     return 1;
  }
 return 0;
}
