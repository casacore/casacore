//# tImageDecomposer.cc: 
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#include <aips/iostream.h>
#include <aips/stdlib.h>
#include <aips/math.h>
#include <aips/aips.h>

#include <trial/Images/ImageInterface.h>

#include <aips/Lattices/TiledShape.h>
#include <trial/Images/TempImage.h>
#include <trial/Lattices/LatticeStatistics.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Functionals/Gaussian2D.h> 
#include <trial/Functionals/Gaussian3D.h>
#include <trial/Images/ImageDecomposer.h>
#include <trial/Images/PagedImage.h>
#include <aips/Inputs/Input.h>


int main(int argc, char** argv)
{  
   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

//   String root = Aipsrc::aipsRoot();
//   String name = root + "/data/demo/Images/test_image";
   inputs.create("in", "", "Input image name");
   inputs.create("threshold", "-1.0", "Threshold");
   inputs.create("shape", "128,128", "Image shape");
   inputs.create("tol", "0.1", "Chi square tolerance");
   inputs.create("ncontours", "0", "Number of contours");
   inputs.create("vary", "False", "Vary contours");
//
   inputs.readArguments(argc, argv);
   const String in = inputs.getString("in");
   const Block<Int> shapeB(inputs.getIntArray("shape"));
   IPosition shape(shapeB.nelements());
   for (uInt i=0; i<shape.nelements(); i++) shape(i) = shapeB[i];
   Int nContours = inputs.getInt("ncontours");
   Double threshold = inputs.getDouble("threshold");
   const Bool varyContours = inputs.getBool("vary");
   const Double chiSqTol = inputs.getDouble("tol");
//
  PagedImage<Float> imageIn(in);
  if (threshold <= 0.0) {
     LatticeStatistics<float> stats(imageIn);
     Array<Float> out;
     Bool ok = stats.getSigma (out, True);
     cerr << "out = " << out << endl;
     threshold = 3.0 * out(IPosition(imageIn.ndim(),0));
  }
  cerr << "Threshold = " << threshold << endl;
//
//
  try {
    if (nContours==0) {
      cerr << "Simple decomposition" << endl;
      ImageDecomposer<Float> pmap(imageIn);
      pmap.decomposeImage(threshold, 0.1); 
//    pmap.display();
      pmap.printComponents();
    }
  }  catch (AipsError decompositionerror)  {
    String errorMsg = decompositionerror.getMesg();
    cout << errorMsg << endl;
    return 1;
  }
//
  if (nContours > 2) {
     cerr << endl << "Contouring decomposition" << endl;
     try  {
       ImageDecomposer<Float> pmap(imageIn);
       pmap.decomposeImage(threshold, nContours, chiSqTol, varyContours);
//       pmap.display();
       pmap.printComponents();
     }  catch (AipsError decompositionError)  {
       String errorMsg = decompositionError.getMesg();
       cout << errorMsg << endl;
       return 1;
     }
   } 

  return 0;

}

