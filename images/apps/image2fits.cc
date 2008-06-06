//# image2fits.cc: Program to convert an image to FITS format
//# Copyright (C) 2008
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes
#include <casa/aips.h>
#include <casa/Inputs.h>
#include <images/Images/ImageExpr.h>
#include <images/Images/ImageExprParse.h>
#include <images/Images/ImageFITSConverter.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

int main (Int argc, const char* argv[])
{
  try {
    // enable input in no-prompt mode
    Input inputs(1);
    // define the input structure
    inputs.version("20080214GvD");
    inputs.create ("in", "",
		   "Name of input Image or image expression",
		   "string");
    inputs.create ("out", "",
		   "Name of output FITS file",
		   "string");
    // Fill the input structure from the command line.
    inputs.readArguments (argc, argv);

    // Get and check the input specification.
    String imgin (inputs.getString("in"));
    if (imgin == "") {
      throw AipsError(" an input Image or expression must be given");
    }
    cout << "The input image is: " << imgin << endl;
    // Get the fits file name.
    String ffout(inputs.getString("out"));
    if (ffout == "") {
      throw AipsError(" an output FITS file name must be given");
    }

    String error;
    LatticeExpr<Float> lat (ImageExprParse::command(imgin));
    ImageExpr<Float> img (lat, imgin);
    // Now write the fits file.
    Bool res = ImageFITSConverter::ImageToFITS (error, img, ffout);
    if (!res) {
      throw AipsError(error);
    }
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    return 1;
  } 
  cout << "image2fits normally ended" << endl;
  return 0;
}
