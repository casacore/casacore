//# tImagePolarimetry.cc: test ImagePolarimetry class
//# Copyright (C) 1996,1997,1998,1999
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
// 
//
#include <trial/Images/ImagePolarimetry.h>

#include <aips/aips.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/TempImage.h>
#include <trial/Images/ImageExpr.h>
#include <trial/Images/ImageSummary.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Mathematics/Math.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Utilities/String.h>

#include <iostream.h>


main (int argc, char **argv)
{
try {

   Input inputs(1);
   inputs.version ("$Revision$");


// Get inputs

   inputs.create("debias", "False", "Debias");
   inputs.readArguments(argc, argv);

   const Bool debias = inputs.getBool("debias");
   LogOrigin or("tImagePolarimetry", "main()", WHERE);
   LogIO os(or);
 

// Check image name and get image data type. 

   ImageInterface<Float>* pIm = 0;
   IPosition shape;
   shape = IPosition(4,10,10,4,10);
   pIm = new TempImage<Float>(shape, CoordinateUtil::defaultCoords4D());
//
   ImagePolarimetry pol(*pIm);
   AlwaysAssert(pol.shape()==shape,AipsError);
   AlwaysAssert(pol.coordinates().near(&(pIm->coordinates())),AipsError);
   AlwaysAssert(pol.isMasked()==pIm->isMasked(),AipsError);
   AlwaysAssert(pol.stokesShape()==IPosition(4,10,10,1,10),AipsError);
   {
      ImageExpr<Float> ie = pol.stokesI();
   }
   {
      ImageExpr<Float> ie = pol.stokesQ();
   }
   {
      ImageExpr<Float> ie = pol.stokesU();
   }
   {
      ImageExpr<Float> ie = pol.stokesV();
   }
   {
      ImageExpr<Float> ie = pol.linPolInt(debias, 0.0);
   }
   {
      ImageExpr<Float> ie = pol.totPolInt(debias, 0.0);
   }
   {
      ImageExpr<Float> ie = pol.linPolPosAng();
   }
   {
      ImageExpr<Float> ie = pol.fracLinPol(debias, 0.0);
   }
   {
      ImageExpr<Float> ie = pol.fracTotPol(debias, 0.0);
   }
   delete pIm;

}   catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     exit(1);
} end_try;
  cout << "ok" << endl;
  exit(0);
}

