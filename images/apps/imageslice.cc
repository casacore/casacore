//# imageslice: extract a subimage using pixel regions
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

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Inputs/Input.h>
#include <casa/Arrays/IPosition.h>

#include <images/Images/ImageOpener.h>
#include <images/Images/FITSImage.h>
#include <images/Images/MIRIADImage.h>
#include <images/Images/SubImage.h>
#include <images/Images/ImageFITSConverter.h>
#include <images/Images/ImageUtilities.h>
#include <images/Images/HDF5Image.h>

using namespace casa;

int main(int argc, const char* argv[]) {

  try {
    Input inputs(1);
    inputs.create("in", "", "Input image name", "string");
    inputs.create("out", "sliced_<in>", "Output image name", "string");
    inputs.create("outregion", "", "Output image region, specify start/end pairs for each axis and use -1 to use the input image shape", "Block<Int>");
    inputs.readArguments(argc, argv);

    const String in = inputs.getString("in");
    if ( in.empty() ) {
      cout << "Please specify input image name" << endl;
      exit(1);
    }

    String out = inputs.getString("out");
    if ( out.empty() ) {
      out = "sliced_"+in;
    }
    Bool outisfits = downcase(out).after(out.size()-6) == ".fits";

    const Block<Int> outregion = inputs.getIntArray("outregion");

    Bool inisfits = downcase(in).after(in.size()-6) == ".fits";

    FITSImage::registerOpenFunction();
    MIRIADImage::registerOpenFunction();
    LatticeBase* pLatt = ImageOpener::openImage(in);
    ImageInterface<Float>* pImage = dynamic_cast<ImageInterface<Float>*>(pLatt);
    if (!pImage) {
      cout << "The input image must have data type Float" << endl;
      exit(1);
    }

    IPosition imshape = pImage->shape();

    if (outregion.nelements() != pImage->shape().nelements()*2) {
      cout << "Please specify all start/end pairs for all axes" << endl;
      cout << "The shape of the image is " << imshape << endl;
      exit(1);
    }
    
    IPosition start(imshape); start = 0;
    IPosition end(imshape);end-=1;
    for (uInt i=0; i < outregion.nelements(); ++i) {
      if ( outregion[i]  > -1 ) {
        if ( i%2  == 0 ) {        
          start(i/2) = outregion[i];
        } else {
          end(i/2) = outregion[i];
        }
      }
    }
    Slicer slice(start, end, Slicer::endIsLast);
    SubImage<Float> subim(*pImage, slice);
    
    if (outisfits) {
      String errMsg;
      Bool ok = ImageFITSConverter::ImageToFITS(errMsg, subim, out,
                                                128, False, False);
    } else {
      ImageInterface<Float>* pim = 0;
      if (dynamic_cast<HDF5Image<Float>*>(pImage) != 0) {
	pim = new HDF5Image<Float> (subim.shape(),
				    subim.coordinates(), out);
      }
      if (pim == 0) {
	pim = new PagedImage<Float> (subim.shape(), 
				     subim.coordinates(), out);
      }
      pim->copyData(subim);
      ImageUtilities::copyMiscellaneous(*pim, subim);
      delete pim;
    }

    delete pImage;
  } catch (const AipsError &x) {
    cerr << "Exception caught:" << endl;
    cerr << x.getMesg() << endl;
  } 
}
