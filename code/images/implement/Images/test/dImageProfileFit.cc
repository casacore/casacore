//# tImageProfileFit.cc: 
//# Copyright (C) 1996,1997,1999,2000,2001,2002,2003
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
#include <trial/Images/ImageProfileFit.h>

#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Utilities/String.h>
#include <aips/Containers/Record.h>


#include <trial/Images/PagedImage.h>
#include <aips/iostream.h>



main (int argc, char **argv)
{

try {

   Input inputs(1);

// Get inputs

   String root = Aipsrc::aipsRoot();
   String name = root + "/data/demo/Images/test_image";
   inputs.create("in", name, "Input file name");
   inputs.create("out", "dImageProfileFit_output", "Output root image name");
   inputs.create("axis", "2", "axis to fit (default is 2)");
   inputs.readArguments(argc, argv);
//
   const String in = inputs.getString("in");
   const String out = inputs.getString("out");
   Int axis = inputs.getInt("axis");
//
   if (in.empty()) {
      cout << "You must give an input image name" << endl;
      exit(1);
   }
   if (out.empty()) {
      cout << "You must give an output image root name" << endl;
      exit(1);
   }
// 
   DataType imageType = imagePixelType(in);
   if (imageType!=TpFloat) {
      cout << "The image must be of type Float" << endl;
      exit(1);
   }

// Construct images

   PagedImage<Float> inImage(in);
   if (axis > Int(inImage.ndim())) {
      cout << "Axis must be <=" << inImage.ndim() << endl;
      exit(1);
   }
   PagedImage<Float>* pOutFit = new PagedImage<Float>(TiledShape(inImage.shape()),
                            inImage.coordinates(), out+String(".fit"));

   PagedImage<Float>* pOutResid = new PagedImage<Float>(TiledShape(inImage.shape()),
                              inImage.coordinates(), out+String(".residual"));
// Make fitter

   ImageProfileFit fitter;

// Set Data

   fitter.setData (inImage, axis, False);

// Initial estimate

   Int nMax = 1;
   Bool ok = fitter.estimate(nMax);

// Fit

   Record rec;
   Bool xAbs = True;
   String xUnit ("pix");
   String doppler("RADIO");
   fitter.fit (True, rec, xAbs, xUnit, doppler, pOutFit, pOutResid);
//
   delete pOutFit;
   delete pOutResid;

} catch (AipsError x) {
   cerr << "aipserror: error " << x.getMesg() << endl;
   return 1;
} 

   exit(0);
}

