//# imagecalc.cc: Calculate an output image from an image expression
//# Copyright (C) 2008
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

#include <casacore/casa/Inputs/Input.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/HDF5Image.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/images/Images/MIRIADImage.h>
#include <casacore/images/Images/ImageProxy.h>
#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Images/ImageExprParse.h>

using namespace casacore;

int main(int argc, const char* argv[]) 
{
  try {
    // Register the FITS and Miriad image types.
    casacore::FITSImage::registerOpenFunction();
    casacore::MIRIADImage::registerOpenFunction();

    // Read the input parameters.
    Input inputs(1);
    inputs.version("20080710GvD");
    inputs.create("in", "", "Input image or image expression", "string");
    inputs.create("out", "", "Output image name (optional)", "string");
    inputs.create("hdf5", "F", "output image in HDF5 format?", "bool");
    inputs.readArguments(argc, argv);

    // Get and check the input specification.
    String imgin (inputs.getString("in"));
    if (imgin.empty()) {
      throw AipsError(" an input Image or expression must be given");
    }
    // Get the output file name.
    String outName(inputs.getString("out"));
    if (outName.empty()) {
      outName = "/tmp/image.out";
    }
    Bool hdf5 = inputs.getBool("hdf5");
    if (hdf5  &&  !HDF5Object::hasHDF5Support()) {
      cerr << "Support for HDF5 has not been compiled in; revert to PagedImage"
           << endl;
      hdf5 = False;
    }

    LatticeExprNode node(ImageExprParse::command(imgin));
    if (node.isScalar()) {
      if (node.dataType() == TpBool) {
	cout << "bool result = " << node.getBool() << endl;
      } else if (node.dataType() == TpFloat) {
	cout << "float result = " << node.getFloat() << endl;
      } else if (node.dataType() == TpDouble) {
	cout << "double result = " << node.getDouble() << endl;
      } else if (node.dataType() == TpComplex) {
	cout << "complex result = " << node.getComplex() << endl;
      } else {
	cout << "dcomplex result = " << node.getDComplex() << endl;
      }
    } else {
      cout << "Copying '" << imgin << "' to '" << outName << "'" << endl;
      ImageProxy img(imgin, String(), vector<ImageProxy>());
      img.saveAs (outName, True, hdf5, True);
    }
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
