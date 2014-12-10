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
#include <casacore/casa/aips.h>
#include <casacore/casa/Inputs.h>
#include <casacore/images/Images/ImageOpener.h>
#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/images/Images/MIRIADImage.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main (int argc, const char* argv[])
{
  try {
    // Register the FITS and Miriad image types.
    casacore::FITSImage::registerOpenFunction();
    casacore::MIRIADImage::registerOpenFunction();

    // enable input in no-prompt mode
    Input inputs(1);
    // define the input structure
    inputs.version("20090915GvD");
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

    // First try to open as a normal image.
    ImageInterface<Float>* img = 0;
    String error;
    Bool res = True;
    LatticeBase* lattice = ImageOpener::openImage (imgin);
    if (lattice) {
      // Succeeded to open as an image.
      if (lattice->dataType() == TpFloat) {
        img = dynamic_cast<ImageInterface<Float>*>(lattice);
      } else {
        // If no datatype float, convert to it using LEL.
        // Enclose the name in quotes.
        imgin = "float('" + imgin + "')";
        delete lattice;
      }
    }
    if (img == 0) {
      // Try to interpret it as a LEL expression.
      LatticeExpr<Float> lat (ImageExprParse::command(imgin));
      img = new ImageExpr<Float> (lat, imgin);
    }
    // Now write the fits file.
    res = ImageFITSConverter::ImageToFITS (error, *img, ffout);
    delete img;
    if (!res) {
      throw AipsError(error);
    }
  } catch (std::exception& x) {
    cout << x.what() << endl;
    return 1;
  } 
  cout << "image2fits normally ended" << endl;
  return 0;
}
