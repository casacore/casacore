//# tLimbDarkenedDiskShape.cc: Test programs for the LimbDarkenedDiskShape class
//# Copyright (C) 1999,2000,2001
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
//# $Id: tLimbDarkenedDiskShape.cc 23184 2013-03-11 22:00:47Z tak.tsutsumi $

#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/LimbDarkenedDiskShape.h>
#include <components/ComponentModels/TwoSidedShape.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MeasureHolder.h>
#include <casa/Quanta/Euler.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Quanta/RotMatrix.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {
#ifndef HAVE_GSL
  return 3;    // not tested
#endif
  try {
    TwoSidedShape* shapePtr = 0;
    {
      // Create a LDisk component at the default direction
      const LimbDarkenedDiskShape defDisk;
      AlwaysAssert(defDisk.ok(), AipsError);
      AlwaysAssert(defDisk.type() == ComponentType::LDISK, AipsError);
      AlwaysAssert(defDisk.isSymmetric() == True, AipsError);
      const MVAngle pixelSize(Quantity(1.0,"''"));
      // Sample the Disk at the Maximum and a bit more ands a bit less than
      // half an arc-min on either side.
      const Double peak = square(pixelSize.radian())/
	(C::pi*square(Quantity(1, "arcmin").getValue("rad")));
      {
	MVDirection sampleDirVal(Quantity(0,"deg"), 
				 Quantity(90, "deg") - Quantity(.5001, "'"));
	MDirection sampleDir(sampleDirVal, MDirection::J2000);
 	AlwaysAssert(near(defDisk.sample(sampleDir, pixelSize,pixelSize), 0.0),
 		     AipsError);
      }
      {
	MVDirection sampleDirVal(Quantity(0,"deg"), 
				 Quantity(90, "deg") - Quantity(.4999, "'"));
	MDirection sampleDir(sampleDirVal, MDirection::J2000);
 	AlwaysAssert(near(defDisk.sample(sampleDir,pixelSize,pixelSize), peak),
 		     AipsError);
      }
      {
	MVDirection sampleDirVal(Quantity(90,"deg"), 
				 Quantity(90, "deg") - Quantity(.5001, "'"));
	MDirection sampleDir(sampleDirVal, MDirection::J2000);
 	AlwaysAssert(near(defDisk.sample(sampleDir,pixelSize,pixelSize), 0.0), 
 		     AipsError);
      }
      {
	const MVAngle halfPix = pixelSize.radian()/2.0;
	MVDirection sampleDirVal(Quantity(90,"deg"), 
				 Quantity(90, "deg") - Quantity(.4999, "'"));
	MDirection sampleDir(sampleDirVal, MDirection::J2000);
 	AlwaysAssert(near(defDisk.sample(sampleDir,halfPix,pixelSize),
			  peak/2.0), AipsError);
      }
      cout << "Passed the default disk shape test" << endl;
    }
    {
      // Create a limbdarken disk shape at a defined non-J2000 direction
      const MVDirection mvd(Quantity(0,"deg"), Quantity(0, "deg"));
      const MDirection dir(mvd, MDirection::J2000);
      const Quantity majorAxis(4, "deg");
      const Quantity minorAxis(2, "deg");
      const Quantity pa(1, "deg");
      const Float nfactor = 0.0;
      const MVAngle pixelSize(Quantity(1.0,"''"));
      const Double peak = square(pixelSize.radian())/
	(C::pi*majorAxis.getValue("rad")*minorAxis.getValue("rad"));
      LimbDarkenedDiskShape ds(dir, majorAxis, minorAxis, pa, nfactor);
      Vector<MDirection::MVType> dirs(8);
      dirs(0) = MVDirection(Quantity(0,"deg"), Quantity(-2.001, "deg"));
      dirs(1) = MVDirection(Quantity(0,"deg"), Quantity(-1.999, "deg"));
      dirs(2) = MVDirection(Quantity(0,"deg"), Quantity(2.001, "deg"));
      dirs(3) = MVDirection(Quantity(0,"deg"), Quantity(1.999, "deg"));
      dirs(4) = MVDirection(Quantity(-1.001,"deg"), Quantity(0.0, "deg"));
      dirs(5) = MVDirection(Quantity(-0.999,"deg"), Quantity(0.0, "deg"));
      dirs(6) = MVDirection(Quantity(1.001,"deg"), Quantity(0.0, "deg"));
      dirs(7) = MVDirection(Quantity(0.999,"deg"), Quantity(0.0, "deg"));
      Vector<Double> results(8, -1.0);
      ds.sample(results, dirs, MeasRef<MDirection>(MDirection::J2000),
		pixelSize, pixelSize);
      AlwaysAssert(nearAbs(results(0), 0.0), AipsError);
      AlwaysAssert(near(results(1), peak), AipsError);
      AlwaysAssert(nearAbs(results(2), 0.0), AipsError);
      AlwaysAssert(near(results(3), peak), AipsError);
      AlwaysAssert(nearAbs(results(4), 0.0), AipsError);
      AlwaysAssert(near(results(5), peak), AipsError);
      AlwaysAssert(nearAbs(results(6), 0.0), AipsError);
      AlwaysAssert(near(results(7), peak), AipsError);
	
      // check that the PA goes the right way!
      {
	MVDirection sampleDirVal(Quantity(0.003, "deg"), 
				 Quantity(1.999, "deg"));
	MDirection sampleDir(sampleDirVal, MDirection::J2000);
 	AlwaysAssert(near(ds.sample(sampleDir, pixelSize, pixelSize), peak), 
 		     AipsError);
      }
      {
	MVDirection sampleDirVal(Quantity(-0.003, "deg"),
				 Quantity(1.999, "deg"));
	MDirection sampleDir(sampleDirVal, MDirection::J2000);
 	AlwaysAssert(near(ds.sample(sampleDir, pixelSize, pixelSize), 0.0), 
 		     AipsError);
      }
      cout << "Passed the arbitrary disk shape test" << endl;

      // test the copy semantics
      LimbDarkenedDiskShape otherds(ds);
      LimbDarkenedDiskShape assignedds;
      assignedds = otherds;
      shapePtr = (TwoSidedShape*) ds.clone();
      ds.setWidthInRad(1., 0.5, .1);
      shapePtr->setWidth(Quantity(1000, "mas"), Quantity(0.5, "arcsec"), 
 			 Quantity(-10, "deg"));
      otherds.setWidth(Quantity(5, "deg"), 0.5, Quantity(1, "rad"));
      {
  	AlwaysAssert(near(assignedds.majorAxis().getValue("deg"), 4.0),
 		     AipsError);
 	AlwaysAssert(assignedds.majorAxis().getFullUnit().getName() == "deg",
 		     AipsError);
  	AlwaysAssert(near(assignedds.minorAxis().getValue("deg"), 2.0),
 		     AipsError);
 	AlwaysAssert(assignedds.minorAxis().getFullUnit().getName() == "deg",
 		     AipsError);
  	AlwaysAssert(near(assignedds.positionAngle().getValue("deg"), 1.0),
 		     AipsError);
 	AlwaysAssert(assignedds.positionAngle().getFullUnit().getName() =="deg",
 		     AipsError);
      }
      {
 	AlwaysAssert(near(otherds.majorAxis().getValue("deg"), 5.0),
		     AipsError);
	AlwaysAssert(otherds.majorAxis().getFullUnit().getName() == "deg",
		     AipsError);
 	AlwaysAssert(near(otherds.axialRatio(), 0.5), AipsError);
 	AlwaysAssert(near(otherds.TwoSidedShape::axialRatio(), 0.5), AipsError);
	AlwaysAssert(otherds.minorAxis().getFullUnit().getName() == "deg",
		     AipsError);
 	AlwaysAssert(near(otherds.positionAngle().getValue("rad"), 1.0),
		     AipsError);
	AlwaysAssert(otherds.positionAngle().getFullUnit().getName() =="rad",
		     AipsError);
      }
      {
 	AlwaysAssert(near(ds.majorAxisInRad(), 1.0), AipsError);
 	AlwaysAssert(near(ds.minorAxisInRad(), 0.5), AipsError);
 	AlwaysAssert(near(ds.positionAngleInRad(), 0.1), AipsError);
	AlwaysAssert(ds.majorAxis().getFullUnit().getName() == "deg",
		     AipsError);
	AlwaysAssert(ds.minorAxis().getFullUnit().getName() == "deg",
		     AipsError);
	AlwaysAssert(ds.positionAngle().getFullUnit().getName() =="deg",
		     AipsError);
      }
      {
 	AlwaysAssert(near(shapePtr->majorAxis().getValue("mas"), 1000.0),
		     AipsError);
	AlwaysAssert(shapePtr->majorAxis().getFullUnit().getName() == "mas",
		     AipsError);
 	AlwaysAssert(near(shapePtr->minorAxis().getValue("arcsec"), 0.5),
		     AipsError);
	AlwaysAssert(shapePtr->minorAxis().getFullUnit().getName() == "arcsec",
		     AipsError);
 	AlwaysAssert(near(shapePtr->positionAngle().getValue("deg"), -10.0),
		     AipsError);
	AlwaysAssert(shapePtr->positionAngle().getFullUnit().getName() =="deg",
		     AipsError);
	AlwaysAssert(shapePtr->type() == ComponentType::LDISK, AipsError);
	AlwaysAssert(shapePtr->isSymmetric() == True, AipsError);
      }
      cout << "Passed the copy semantics test" << endl;
    }
    {
      MDirection dir(MVDirection(0.0, 0.0), MDirection::J2000);
      // n=0.0 -> uniform disk case
      LimbDarkenedDiskShape ds(dir, Quantity(1, "deg"), 0.5, Quantity(90, "deg"),Float(0.0));
      //cerr<<"ds.majorAxisInRad()="<<ds.majorAxisInRad()<<endl;
      //cerr<<"ds.minorAxisInRad()="<<ds.minorAxisInRad()<<endl;
 
      Vector<Double> uvw(3,0.0);
      Double freq = 1E6;
      AlwaysAssert(near(ds.visibility(uvw, freq).real(), 1.0), AipsError);
      AlwaysAssert(near(ds.visibility(uvw, freq).imag(), 0.0), AipsError);
      uvw(1) = 3.8317 / C::pi * C::c / freq / ds.minorAxisInRad();
      cerr<<"uvw(0)="<<uvw(0)<<" uvw(1)="<<uvw(1)<<endl;
      cerr<<"ds.vis="<<ds.visibility(uvw,freq).real()<<endl;
      AlwaysAssert(nearAbs(ds.visibility(uvw, freq).real(), 0.0, 2E-6),
		   AipsError);
      AlwaysAssert(near(ds.visibility(uvw, freq).imag(), 0.0), AipsError);
      uvw(0) = uvw(1)/2; uvw(1) = 0.0;
      AlwaysAssert(nearAbs(ds.visibility(uvw, freq).real(), 0.0, 2E-6),
		   AipsError);
      AlwaysAssert(near(ds.visibility(uvw, freq).imag(), 0.0), AipsError);
      
      Matrix<Double> uvws(3, 2);
      uvws = 0.0; 
      uvws(1,0) = -uvw(0)*2;
      uvws(0,1) = -uvw(0);
      Vector<DComplex> results(2, DComplex(10.0, 10.0));
      ds.visibility(results, uvws, freq);
      AlwaysAssert(nearAbs(results(0).real(), 0.0, 2E-6), AipsError);
      AlwaysAssert(near(results(0).imag(), 0.0), AipsError);
      AlwaysAssert(nearAbs(results(1).real(), 0.0, 2E-6), AipsError);
      AlwaysAssert(near(results(1).imag(), 0.0), AipsError);

      cout << "Passed the visibility test" << endl;
    }
    delete shapePtr;
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  }
  cout << "OK" << endl;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tDiskShape"
// End: 
