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

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Regex.h>
#include <casa/OS/Timer.h>
#include <casa/Containers/Record.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Inputs/Input.h>
#include <casa/Arrays/IPosition.h>

#include <images/Images/PagedImage.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/FITSImage.h>
#include <images/Images/ImageRegrid.h>
#include <images/Images/TempImage.h>
#include <images/Images/ImageRegion.h>
#include <images/Images/SubImage.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <lattices/Lattices/LatticeStatistics.h>

#include <casa/Logging/LogIO.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MDoppler.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MeasTable.h>
#include <measures/Measures/MeasureHolder.h>
#include <measures/Measures/Stokes.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/MVEpoch.h>
#include <casa/Quanta/QuantumHolder.h>

using namespace casa;

int main(int argc, const char* argv[]) {

  try {
    Input inputs(1);
    inputs.create("in", "GASS_11.integrated", "Input image name");
    inputs.create("out", "GASS_11.integrated.regridded", "Output image name");
    inputs.create("decimate", "10", "decimation factor");
    inputs.create("dirref", "GALACTIC", "MDirection type");
    inputs.create("projection", "AIT", "Projection");
    inputs.create("shape", "2400,2200", "Output image shape", "Block<Int>");
    inputs.create("refval", "300.0,0.0", "Reference value", "Block<Double>");
    inputs.readArguments(argc, argv);

    const String in = inputs.getString("in");
    const String out = inputs.getString("out");
    const Int decimate = inputs.getInt("decimate");
    const String dirref = inputs.getString("dirref");
    const Block<Int> outshape = inputs.getIntArray("shape");
    const Block<Double> refval = inputs.getDoubleArray("refval");
    const String proj = inputs.getString("projection");

    Bool isfits = downcase(in).after(in.size()-6) == ".fits";    

    ImageInterface<Float>* pImage = 0;
    if (isfits) {
      cout << "Trying to load FITS Image \"" << in << "\"" << endl;
      pImage = new FITSImage(in);
    } else {
      cout << "Trying to load AIPS++ Image \"" << in << "\"" << endl;
      pImage = new PagedImage<Float>(in);
    }
    
    Vector<uInt> itsAxes;
    ImageInterface<Float>* itsImage = pImage;
    ImageRegrid<Float> itsIr;
    PagedImage<Float>* itsTmp;
    Interpolate2D::Method itsMethod = Interpolate2D::stringToMethod("linear");
    itsIr.disableReferenceConversions(False);
    itsIr.showDebugInfo(0);
    Int itsDecimate = decimate;
    String itsProj = proj;
    String itsMDir = dirref;
    
    Bool changeRefFrame = False;
    changeRefFrame = (itsProj != "" || itsMDir != "");
    CoordinateSystem csys(itsImage->coordinates());
    Int dircoordNo = 
        itsImage->coordinates().findCoordinate(Coordinate::DIRECTION, -1);
    if (!changeRefFrame) {
      //fail
    }
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
    }
    //Vector<Double> refValFrom = dirCoordFrom.referenceValue();
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
    Vector<Int> pAx, wAx;
    CoordinateUtil::findDirectionAxes(pAx, wAx, dircoordNo, csys);
    // HARDCODED
    IPosition outAxes(2, pAx(0), pAx(1));
    IPosition shapeOut = itsImage->shape();
    shapeOut[pAx(0)] = outshape[0];
    shapeOut[pAx(1)] =  outshape[1];
    //IPosition shapeOut(2,outshape[0],outshape[1]);
    itsTmp = new PagedImage<Float>(shapeOut,csys, out);
    cout << "Regridding image..." << endl; 
    itsIr.regrid(*itsTmp, itsMethod, outAxes, *itsImage, False, 
                 itsDecimate, False);
    delete itsTmp;
  } catch (const AipsError &x) {
    cerr << "Exception caught:" << endl;
    cerr << x.getMesg() << endl;
  } 
}
