//# tImageExpr2Gram.cc: Test program for WC regions in image expression parser
//# Copyright (C) 1999,2000
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

#include <trial/Images/PagedImage.h>
#include <trial/Images/ImageExprParse.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/WCBox.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LCPagedMask.h>
#include <trial/Lattices/LELArray.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/IPosition.h>
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
    inp.version(" ");
    inp.create("nx", "10", "Number of pixels along the x-axis", "int");
    inp.create("ny", "10", "Number of pixels along the y-axis", "int");
    inp.readArguments(argc, argv);
    
    const uInt nx=inp.getInt("nx");
    const uInt ny=inp.getInt("ny");
    IPosition shape(2, nx, ny);
    Slicer section(IPosition(2,0), shape);
    Array<Float> arr(shape);
    indgen (arr);
    Array<Float> arrm1, arrm2;
    arrm1 = arr;
    arrm2 = arr;
    Array<Bool> m1;
    Array<Bool> m2;
    {    
      PagedImage<Float> image (shape,
			       CoordinateUtil::defaultCoords2D(),
			       "tImageExpr2Gram_tmp.img");
      image.put (arr);
      
      // Define 2 masks for the image and make the first one the default.
      LCPagedMask mask1 = RegionHandler::makeMask (image, "mask1");
      LCPagedMask mask2 = RegionHandler::makeMask (image, "mask2");
      Matrix<Bool> mask(shape);
      mask = True;
      mask(0,0) = False;
      arrm1(IPosition(2,0,0)) = -1;
      mask1.put (mask);
      m1 = mask;
      mask = True;
      mask(0,1) = False;
      mask(1,1) = False;
      arrm2(IPosition(2,0,1)) = -1;
      arrm2(IPosition(2,1,1)) = -1;
      mask2.put (mask);
      m2 = mask;
      image.defineRegion ("mask1", mask1, RegionHandler::Masks);
      image.defineRegion ("mask2", mask2, RegionHandler::Masks);
      image.setDefaultMask ("mask1");
    }
    PagedImage<Float> image ("tImageExpr2Gram_tmp.img");
    Block<LatticeExprNode> temps(1);
    temps[0] = LatticeExprNode(image);

    PtrBlock<const ImageRegion*> tempRegs(1);
    tempRegs[0] = new ImageRegion (WCBox(LCBox(shape), image.coordinates()));

    {
      cout << endl;
      cout << "Expr:  $1" << endl;
      LatticeExpr<Float> expr (ImageExprParse::command
			       ("$OBJ#1#O", temps, tempRegs));
      Array<Float> result;
      expr.get (result);
      if (! allEQ (result, arr)) {
	cout << "Result should be " << arr << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  $1[$region || $region && $region]" << endl;
      LatticeExpr<Float> expr (ImageExprParse::command
			     ("$OBJ#1#O[$REGION#1 || $REGION#1 && $REGION#1]",
			      temps, tempRegs));
      Array<Float> result;
      expr.get (result);
      if (! allEQ (result, arr)) {
	cout << "Result should be " << arr << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements($1)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements($OBJ#1#O)", temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()-1) {
	cout << "Result should be " << shape.product()-1 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  ndim('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("ndim('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      Float result = expr.getFloat();
      if (result != shape.nelements()) {
	cout << "Result should be " << shape.nelements() << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  ndim($REGION#1)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("ndim($REGION#1)",
			     temps, tempRegs));
      Float result = expr.getFloat();
      if (result != shape.nelements()) {
	cout << "Result should be " << shape.nelements() << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  any('tImageExpr2Gram_tmp.img::mask2')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("any('tImageExpr2Gram_tmp.img::mask2')",
			     temps, tempRegs));
      Bool result = expr.getBool();
      if (!result) {
	cout << "Result should be " << True << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  all('tImageExpr2Gram_tmp.img::mask2')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("all('tImageExpr2Gram_tmp.img::mask2')",
			     temps, tempRegs));
      Bool result = expr.getBool();
      if (result) {
	cout << "Result should be " << False << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  ntrue('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("ntrue('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()-1) {
	cout << "Result should be " << shape.product()-1 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nfalse('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nfalse('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != 1) {
	cout << "Result should be " << 1 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements('tImageExpr2Gram_tmp.img::mask1')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements('tImageExpr2Gram_tmp.img::mask1')",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()) {
	cout << "Result should be " << shape.product() << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  'tImageExpr2Gram_tmp.img::mask2' == tImageExpr2Gram_tmp.img::mask2" << endl;
      LatticeExprNode expr (ImageExprParse::command
	  ("'tImageExpr2Gram_tmp.img::mask2' == tImageExpr2Gram_tmp.img::mask2",
	   temps, tempRegs));
      LELArray<Bool> result(shape);
      expr.eval (result, section);
      if (! allEQ (result.value(), True)) {
	cout << "Result should be " << m2 << endl;
	cout << "Result is " << result.value() << endl;
	foundError = True;
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
      LELArray<Bool> result(shape);
      expr.eval (result, section);
      if (! allEQ (result.value(), m1&&m2)) {
	cout << "Result should be " << False << endl;
	cout << "Result is " << result.value() << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  iif ('tImageExpr2Gram_tmp.img::mask2', "
 	      "tImageExpr2Gram_tmp.img,-1)"
	   << endl;
      LatticeExpr<Float> expr (ImageExprParse::command
			       ("iif ('tImageExpr2Gram_tmp.img::mask2', "
				"tImageExpr2Gram_tmp.img,-1)",
				temps, tempRegs));
      Array<Float> result;
      expr.get (result);
      if (! allEQ (result, arrm2)) {
	cout << "Result should be " << arr << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements('tImageExpr2Gram_tmp.img')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements('tImageExpr2Gram_tmp.img')",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()-1) {
	cout << "Result should be " << shape.product()-1 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements('tImageExpr2Gram_tmp.img:nomask')" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements('tImageExpr2Gram_tmp.img:nomask')",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()) {
	cout << "Result should be " << shape.product() << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img:mask2)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img:mask2)",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()-2) {
	cout << "Result should be " << shape.product()-2 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img:mask2)" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img:mask2)",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()-2) {
	cout << "Result should be " << shape.product()-2 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img[mask2])" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img[mask2])",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()-3) {
	cout << "Result should be " << shape.product()-3 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  nelements(tImageExpr2Gram_tmp.img[::mask2])" << endl;
      LatticeExprNode expr (ImageExprParse::command
			    ("nelements(tImageExpr2Gram_tmp.img[::mask2])",
			     temps, tempRegs));
      Double result = expr.getDouble();
      if (result != shape.product()-3) {
	cout << "Result should be " << shape.product()-3 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
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
      Double result = expr.getDouble();
      if (result != shape.product()-3) {
	cout << "Result should be " << shape.product()-3 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }

    for (uInt i=0; i<tempRegs.nelements(); i++) {
      delete tempRegs[i];
    }

  } catch (AipsError x) {
    cerr << "aipserror: error " << x.getMesg() << endl;
    foundError = True;
  } 

  if (foundError) {
    return 1;
  }
  return 0;
}
