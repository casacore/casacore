//# casacore_regrid.cc: regrid and image to the new coordsys
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

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Arrays/IPosition.h>

#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageOpener.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/images/Images/MIRIADImage.h>
#include <casacore/images/Images/HDF5Image.h>
#include <casacore/images/Images/ImageUtilities.h>
#include <casacore/images/Images/ImageRegrid.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>

//#include <casacore/casa/Logging/LogIO.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/Quanta/Quantum.h>
//#include <casacore/casa/Quanta/MVAngle.h>


using namespace casacore;

int main(int argc, const char* argv[]) {

  try {
    Input inputs(1);
    inputs.version("$Id$");
    inputs.create("in", "", "Input image name");
    inputs.create("out", "", "Output image name");
    inputs.create("decimate", "10", "decimation factor");
    inputs.create("dirref", "GALACTIC", "MDirection type");
    inputs.create("projection", "AIT", "Projection");
    inputs.create("shape", "", "Output image shape", "Block<Int>");
    inputs.create("refval", "", "New center of the image in degrees (reference value)", 
		  "Block<Double>");
    inputs.create("interpolation", "linear", "Interpolation method (linear, nearest, cubic,lanczos)");
    inputs.readArguments(argc, argv);

    const String in = inputs.getString("in");
    if ( in.empty() ) {
      cerr << "Please specify input image name" << endl;
      exit(1);
    }
    String out = inputs.getString("out");
    if ( out.empty() ) {
      out = "regridded_"+in;
      cout << "No output name give using '" << out << "'." << endl;
    }
    Bool outisfits = downcase(out).after(out.size()-6) == ".fits";

    const Int decimate = inputs.getInt("decimate");
    const String dirref = inputs.getString("dirref");
    Block<Int> outshape = inputs.getIntArray("shape");
    const Block<Double> refval = inputs.getDoubleArray("refval");
    if (refval.size() != 2) {
      cerr << "Please specify valid reference value e.g. refval=0.0,0.0" << endl;
      exit(1);
    }
    const String proj = inputs.getString("projection");
    const String interpolation = inputs.getString("interpolation");

    FITSImage::registerOpenFunction();
    MIRIADImage::registerOpenFunction();
    LatticeBase* pLatt = ImageOpener::openImage(in);
    ImageInterface<Float>* pImage = dynamic_cast<ImageInterface<Float>*>(pLatt);
    if (!pImage) {
      cout << "The input image must have data type Float" << endl;
      exit(1);
    }
    
    Vector<uInt> itsAxes;
    ImageInterface<Float>* itsImage = pImage;
    ImageRegrid<Float> itsIr;
    ImageInterface<Float>* itsTmp;
    Interpolate2D::Method itsMethod = Interpolate2D::stringToMethod(interpolation);
    itsIr.disableReferenceConversions(False);
    itsIr.showDebugInfo(0);
    Int itsDecimate = decimate;
    String itsProj = proj;
    String itsMDir = dirref;
    
    //Bool changeRefFrame = False;
    //changeRefFrame = (itsProj != "" || itsMDir != "");
    CoordinateSystem csys(itsImage->coordinates());
    Int dircoordNo = 
        itsImage->coordinates().findCoordinate(Coordinate::DIRECTION, -1);

    DirectionCoordinate                                                 \
      dirCoordFrom(itsImage->coordinates().directionCoordinate(dircoordNo));
    
    Projection project(dirCoordFrom.projection());
    if (itsProj != "") {
      project = Projection(Projection::type(itsProj));
    }
    MDirection::Types mdirt = dirCoordFrom.directionType();
    if (itsMDir != "") {
      MDirection::getType(mdirt, itsMDir);
    }
    Vector<String> unitsFrom = dirCoordFrom.worldAxisUnits();
    Vector<String> radUnits(2);
    radUnits = String("rad");
    if (!dirCoordFrom.setWorldAxisUnits(radUnits)) {
      cerr << "Failed to set radian units for DirectionCoordinate" << endl;
      delete itsImage;
      exit(1);
    }
    // HARDCODED
    IPosition shapeOut = itsImage->shape();
    Vector<Int> pAx, wAx;
    CoordinateUtil::findDirectionAxes(pAx, wAx, dircoordNo, csys);
    //use output shape if valid
    if (outshape.size() == 2) {
      shapeOut[pAx(0)] = outshape[0];
      shapeOut[pAx(1)] =  outshape[1];
    } else {
      cout << "Output shape not specified, using input shape " <<  shapeOut[pAx(0)]
	   << "," << shapeOut[pAx(1)] << "." << endl;
    }

    Vector<Double> refPixFrom = dirCoordFrom.referencePixel();
    Vector<Double> incrFrom = dirCoordFrom.increment();
    Vector<Double> refValFrom = dirCoordFrom.referenceValue();
    
    refValFrom[0] = Quantity(refval[0], "deg").getValue("rad");
    refValFrom[1] = Quantity(refval[1], "deg").getValue("rad");
    refPixFrom[0]= Double(outshape[0])/2.0;
    refPixFrom[1]= Double(outshape[1])/2.0;
    
    DirectionCoordinate dirCoordTo(mdirt, 
                                   project,
                                   refValFrom(0), refValFrom(1),
                                   incrFrom(0), incrFrom(1),
                                   dirCoordFrom.linearTransform(),
                                   refPixFrom(0), refPixFrom(1));
    
    csys.replaceCoordinate(dirCoordTo, dircoordNo);
    IPosition outAxes(2, pAx(0), pAx(1));
    itsTmp = new TempImage<Float>(shapeOut,csys);
    cout << "Regridding image..." << endl; 
    itsIr.regrid(*itsTmp, itsMethod, outAxes, *itsImage, False, 
                 itsDecimate, False);
    cout << "Writing " << out << "..." << endl;
    if (outisfits) {
      String errMsg;
      Bool res = ImageFITSConverter::ImageToFITS(errMsg, *itsTmp, out);
      if (!res) {
	cerr << errMsg << endl;
      }
    } else {
      ImageInterface<Float>* pim = 0;
      if (dynamic_cast<HDF5Image<Float>*>(pImage) != 0) {
        pim = new HDF5Image<Float> (itsTmp->shape(),
                                    itsTmp->coordinates(), out);
      }
      if (pim == 0) {
        pim = new PagedImage<Float>(itsTmp->shape(), 
				    itsTmp->coordinates(), out);
      }
      pim->copyData(*itsTmp);
      ImageUtilities::copyMiscellaneous(*pim, *itsTmp);
      delete pim;
    }

    delete itsTmp;
  } catch (const AipsError &x) {
    cerr << "Exception caught:" << endl;
    cerr << x.getMesg() << endl;
  } 
}
