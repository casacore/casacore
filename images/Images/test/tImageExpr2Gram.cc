//# tImageExpr2Gram.cc: Test program for WC regions in image expression parser
//# Copyright (C) 1999,2000,2001
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

#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/images/Regions/WCBox.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main (int argc, const char* argv[])
{
  bool foundError = false;

  try {
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "10", "Number of pixels along the x-axis", "int");
    inp.create("ny", "10", "Number of pixels along the y-axis", "int");
    inp.readArguments(argc, argv);
    
    const uint32_t nx=inp.getInt("nx");
    const uint32_t ny=inp.getInt("ny");
    IPosition shape(2, nx, ny);
    Slicer section(IPosition(2,0), shape);
    Array<float> arr(shape);
    indgen (arr);
    Array<float> arrm1, arrm2;
    arrm1 = arr;
    arrm2 = arr;
    Array<bool> m1;
    Array<bool> m2;
    {    
      PagedImage<float> image (shape,
			       CoordinateUtil::defaultCoords2D(),
			       "tImageExpr2Gram_tmp.img");
      image.put (arr);
      
      // Define 2 masks for the image and make the first one the default.
      ImageRegion maskreg1 = image.makeMask ("mask1", true, true);
      ImageRegion maskreg2 = image.makeMask ("mask2", true, false);
      LCRegion& mask1 = maskreg1.asMask();
      LCRegion& mask2 = maskreg2.asMask();
      Matrix<bool> mask(shape);
      mask = true;
      mask(0,0) = false;
      arrm1(IPosition(2,0,0)) = -1;
      mask1.put (mask);
      m1 = mask;
      mask = true;
      mask(0,1) = false;
      mask(1,1) = false;
      arrm2(IPosition(2,0,1)) = -1;
      arrm2(IPosition(2,1,1)) = -1;
      mask2.put (mask);
      m2 = mask;
    }
    PagedImage<float> image ("tImageExpr2Gram_tmp.img");
    Block<LatticeExprNode> temps(1);
    temps[0] = LatticeExprNode(image);

    PtrBlock<const ImageRegion*> tempRegs(1);
    tempRegs[0] = new ImageRegion (WCBox(LCBox(shape), image.coordinates()));

    {
      cout << endl;
      cout << "Expr:  $1" << endl;
      LatticeExpr<float> expr (ImageExprParse::command
			       ("$1", temps, tempRegs));
      Array<float> result;
      expr.get (result);
      if (! allEQ (result, arr)) {
	cout << "Result should be " << arr << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  $1[$region || $region && $region]" << endl;
      LatticeExpr<float> expr (ImageExprParse::command
			     ("$1[$R1 || $r1 && $R1]",
			      temps, tempRegs));
      Array<float> result;
      expr.get (result);
      if (! allEQ (result, arr)) {
	cout << "Result should be " << arr << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements($1)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements($1)", temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-1) {
	cout << "Result should be " << shape.product()-1 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  ndim('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("ndim('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      float result = expr.getFloat();
      if (result != shape.nelements()) {
	cout << "Result should be " << shape.nelements() << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  ndim($R1)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("ndim($R1)",
			     temps, tempRegs));
      float result = expr.getFloat();
      if (result != shape.nelements()) {
	cout << "Result should be " << shape.nelements() << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  any('tImageExpr2Gram_tmp.img::mask2')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("any('tImageExpr2Gram_tmp.img::mask2')",
			     temps, tempRegs));
      bool result = expr.getBool();
      if (!result) {
	cout << "Result should be " << true << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  all('tImageExpr2Gram_tmp.img::mask2')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("all('tImageExpr2Gram_tmp.img::mask2')",
			     temps, tempRegs));
      bool result = expr.getBool();
      if (result) {
	cout << "Result should be " << false << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  ntrue('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("ntrue('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-1) {
	cout << "Result should be " << shape.product()-1 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nfalse('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nfalse('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != 1) {
	cout << "Result should be " << 1 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()) {
	cout << "Result should be " << shape.product() << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  'tImageExpr2Gram_tmp.img::mask2' == tImageExpr2Gram_tmp.img::mask2" << endl;
      LatticeExprNode expr (ImageExprParse::command
	  ("'tImageExpr2Gram_tmp.img::mask2' == tImageExpr2Gram_tmp.img::mask2",
	   temps, tempRegs));
      LELArray<bool> result(shape);
      expr.eval (result, section);
      if (! allEQ (result.value(), true)) {
	cout << "Result should be " << m2 << endl;
	cout << "Result is " << result.value() << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  'tImageExpr2Gram_tmp.img::mask2' && "
	      "tImageExpr2Gram_tmp.img::mask1 && "
	      "(tImageExpr2Gram_tmp.img::mask2=="
	      "tImageExpr2Gram_tmp.img::mask2)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("'tImageExpr2Gram_tmp.img::mask2' &&"
			     "tImageExpr2Gram_tmp.img::mask1 && "
			     "(tImageExpr2Gram_tmp.img::mask1=="
			     "tImageExpr2Gram_tmp.img::mask2)",
			     temps, tempRegs));
      LELArray<bool> result(shape);
      expr.eval (result, section);
      if (! allEQ (result.value(), m1&&m2)) {
	cout << "Result should be " << false << endl;
	cout << "Result is " << result.value() << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  iif ('tImageExpr2Gram_tmp.img::mask2', "
 	      "tImageExpr2Gram_tmp.img,-1)"
	   << endl;
      LatticeExpr<float> expr (ImageExprParse::command
			       ("iif ('tImageExpr2Gram_tmp.img::mask2', "
				"tImageExpr2Gram_tmp.img,-1)",
				temps, tempRegs));
      Array<float> result;
      expr.get (result);
      if (! allEQ (result, arrm2)) {
	cout << "Result should be " << arr << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements('tImageExpr2Gram_tmp.img')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements('tImageExpr2Gram_tmp.img')",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-1) {
	cout << "Result should be " << shape.product()-1 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements('tImageExpr2Gram_tmp.img:nomask')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements('tImageExpr2Gram_tmp.img:nomask')",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()) {
	cout << "Result should be " << shape.product() << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img:mask2)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img:mask2)",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-2) {
	cout << "Result should be " << shape.product()-2 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img:mask2)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img:mask2)",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-2) {
	cout << "Result should be " << shape.product()-2 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img[mask2])" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img[mask2])",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-3) {
	cout << "Result should be " << shape.product()-3 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img[::mask2])" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img[::mask2])",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-3) {
	cout << "Result should be " << shape.product()-3 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img"
	      "[tImageExpr2Gram_tmp.img::mask2])" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img"
			     "[tImageExpr2Gram_tmp.img::mask2])",
			     temps, tempRegs));
      double result = expr.getDouble();
      if (result != shape.product()-3) {
	cout << "Result should be " << shape.product()-3 << endl;
	cout << "Result is " << result << endl;
	foundError = true;
      }
    }

    for (uint32_t i=0; i<tempRegs.nelements(); i++) {
      delete tempRegs[i];
    }

  } catch (std::exception& x) {
    cerr << "aipserror: error " << x.what() << endl;
    foundError = true;
  } 

  if (foundError) {
    return 1;
  }
  return 0;
}
