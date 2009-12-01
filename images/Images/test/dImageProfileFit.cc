//# dImageProfileFit.cc: 
//# Copyright (C) 1996,1997,1999,2000,2001,2002,2003,2004
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
//# $Id: 
//
#include <images/Images/ImageProfileFit.h>

#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/Inputs/Input.h>
#include <casa/Logging.h>
#include <tables/Tables/Table.h>
#include <casa/BasicSL/String.h>
#include <casa/Containers/Record.h>
#include <casa/Utilities/Assert.h>

#include <images/Images/PagedImage.h>
#include <casa/iostream.h>



#include <casa/namespace.h>
int main (int argc, const char* argv[])
{

try {

   Input inputs(1);

// Get inputs

   String name = "test_image.im";
   inputs.create("in", name, "Input file name");
   inputs.create("out", "dImageProfileFit_tmp", "Output root image name");
   inputs.create("axis", "2", "axis to fit (default is 2)");
   inputs.readArguments(argc, argv);
//
   const String in = inputs.getString("in");
   const String out = inputs.getString("out");
   Int axis = inputs.getInt("axis");
//
   if (in.empty()) {
      cout << "You must give an input image name" << endl;
      return 1;
   }
   if (out.empty()) {
      cout << "You must give an output image root name" << endl;
      return 1;
   }
// 
   DataType imageType = imagePixelType(in);
   if (imageType!=TpFloat) {
      cout << "The image must be of type Float" << endl;
      return 1;
   }

// Construct images

   PagedImage<Float> inImage(in);
   if (axis > Int(inImage.ndim())) {
      cout << "Axis must be <=" << inImage.ndim() << endl;
      return 1;
   }
//
   String outFitName(out+String(".fit"));
   PagedImage<Float>* pOutFit = new PagedImage<Float>(TiledShape(inImage.shape()),
                            inImage.coordinates(), outFitName);

   String outResidName(out+String(".residual"));
   PagedImage<Float>* pOutResid = new PagedImage<Float>(TiledShape(inImage.shape()),
                              inImage.coordinates(), outResidName);
// Make fitter

   ImageProfileFit fitter;

// Set Data

   fitter.setData (inImage, axis, False);

// Initial estimate

   Int nMax = 1;
   Bool ok = fitter.estimate(nMax);
   AlwaysAssert(ok, AipsError);
// Fit

   Record rec;
   Bool xAbs = True;
   String xUnit ("pix");
   String doppler("RADIO");
   // For the time being use fillRecord=False, otherwise an exception is
   // thrown by ImageProfileFit::getElements that it cannot handle polynomials.
   ////   fitter.fit (True, rec, xAbs, xUnit, doppler, pOutFit, pOutResid, 2);
   fitter.fit (False, rec, xAbs, xUnit, doppler, pOutFit, pOutResid, 2);
//
   delete pOutFit;
   delete pOutResid;
   Table::deleteTable (outFitName, True);
   Table::deleteTable (outResidName, True);

} catch (AipsError x) {
   cerr << "aipserror: error " << x.getMesg() << endl;
   return 1;
} 

   return 0;
}

