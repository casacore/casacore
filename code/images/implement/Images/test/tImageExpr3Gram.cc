//# tImageExpr3Gram.cc: Test program for lattice expression with mixed dimensionalities
//# Copyright (C) 2001
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
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Inputs/Input.h>
#include <aips/Exceptions/Error.h>
#include <aips/iostream.h>


int main (int argc, char *argv[])
{
  Bool foundError = False;

  try {
    Input inp(1);
    inp.version(" ");
    inp.create("nx", "10", "Number of pixels along the x-axis", "int");
    inp.create("ny", "10", "Number of pixels along the y-axis", "int");
    inp.create("nz", "10", "Number of pixels along the z-axis", "int");
    inp.readArguments(argc, argv);
    
    const uInt nx=inp.getInt("nx");
    const uInt ny=inp.getInt("ny");
    const uInt nz=inp.getInt("nz");
    IPosition shape2(2, nx, ny);
    IPosition shape3(3, nx, ny, nz);
    Array<Float> arr2(shape2);
    indgen (arr2);
    Array<Float> arr3(shape3);
    indgen (arr3);
    Array<Float> arr2a(shape3);
    {
      Array<Float> arr2b(IPosition(3,nx,ny,1));
      indgen (arr2b);
      for (uInt i=0; i<nz; i++) {
	arr2a(IPosition(3,0,0,i), IPosition(3,nx-1,ny-1,i)) = arr2b;
      }
    }

    {    
      PagedImage<Float> image2 (shape2,
				CoordinateUtil::defaultCoords2D(),
				"tImageExpr3Gram_tmp.img2");
      image2.put (arr2);
      PagedImage<Float> image3 (shape3,
				CoordinateUtil::defaultCoords3D(),
				"tImageExpr3Gram_tmp.img3");
      image3.put (arr3);
      
    }
    {
      cout << endl;
      cout << "Expr:  image3-image2" << endl;
      LatticeExpr<Float> expr (ImageExprParse::command
		    ("tImageExpr3Gram_tmp.img3 - tImageExpr3Gram_tmp.img2"));
      Array<Float> result;
      expr.get (result);
      if (! allEQ (result, arr3-arr2a)) {
	cout << "Result should be " << arr3-arr2a << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
    }
    {
      cout << endl;
      cout << "Expr:  image2-image3" << endl;
      LatticeExpr<Float> expr (ImageExprParse::command
		    ("tImageExpr3Gram_tmp.img2 - tImageExpr3Gram_tmp.img3"));
      Array<Float> result;
      expr.get (result);
      if (! allEQ (result, arr2a-arr3)) {
	cout << "Result should be " << arr2a-arr3 << endl;
	cout << "Result is " << result << endl;
	foundError = True;
      }
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
