//# tSkyCompRep.cc:
//# Copyright (C) 1998
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

#include <trial/ComponentModels/SkyCompRep.h>
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/Stokes.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <trial/ComponentModels/PointCompRep.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Images/PagedImage.h>

int main() {
  try {
    SkyCompRep * skyPtr = new PointCompRep;
    Vector<Double> flux(4);
    flux(0) = 2.0;
    flux(1) = 0.5;
    flux(2) = 0.2;
    flux(3) = 0.1;
    skyPtr -> setFlux(Quantum<Vector<Double> >(flux, "Jy"));
    
    MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
    MDirection coord00(ra0dec0, MDirection::J2000);
    skyPtr -> setDirection(coord00);

    // Test the project function
    const uInt imSize = 6;
    {
      CoordinateSystem coords = CoordinateUtil::defaultCoords2D();
      PagedImage<Float> image(IPosition(2,imSize,imSize), 
				coords, "tSkyCompRep_tmp.image");
      image.set(0.0f);
      
      skyPtr -> SkyCompRep::project(image);
      IPosition ptPos(2,2,1);
      AlwaysAssert(near(image.getAt(ptPos), flux(0), C::flt_epsilon),
		   AipsError);
      image.putAt(0.0f, ptPos);
      for (uInt y = 0; y < imSize; y++) {
	ptPos(1) = y;
	for (uInt x = 0; x < imSize; x++) {
	  ptPos(0) = x;
	  AlwaysAssert(near(image.getAt(ptPos), 0.0f), AipsError);
	}
      }
      image.table().markForDelete();
    }
    const uInt nChan = 3;
    {
      CoordinateSystem coords = CoordinateUtil::defaultCoords3D();
      {
	Vector<Int> whichStokes(1);
	whichStokes = Stokes::U;
	coords.addCoordinate(StokesCoordinate(whichStokes));
      }
      PagedImage<Float> image(IPosition(4, imSize, imSize, nChan, 1), 
			      coords, "tSkyCompRep_tmp.image");
      image.set(0.0f);
      
      skyPtr -> SkyCompRep::project(image);
      IPosition ptPos(4,2,1,0,0);
      for (uInt f1 = 0; f1 < nChan; f1++) {
	ptPos(2) = f1;
	AlwaysAssert(near(image.getAt(ptPos), flux(2), C::flt_epsilon), 
		     AipsError);
	image.putAt(0.0f, ptPos);
      }
      for (uInt f = 0; f < nChan; f++) {
 	ptPos(2) = f;
	for (uInt y = 0; y < imSize; y++) {
	  ptPos(1) = y;
	  for (uInt x = 0; x < imSize; x++) {
	    ptPos(0) = x;
	    AlwaysAssert(near(image.getAt(ptPos), 0.0f), AipsError);
	  }
	}
      }
      image.table().markForDelete();
    }
    {
      CoordinateSystem coords = CoordinateUtil::defaultCoords2D();
      {
	Vector<Int> whichStokes(3);
	whichStokes(0) = Stokes::I;
	whichStokes(1) = Stokes::Q;
	whichStokes(2) = Stokes::V;
	coords.addCoordinate(StokesCoordinate(whichStokes));
      }
      PagedImage<Float> image(IPosition(3, imSize, imSize, 3), 
			      coords, "tSkyCompRep_tmp.image");
      image.set(0.0f);
      
      skyPtr -> SkyCompRep::project(image);
      IPosition ptPos(3,2,1,0);
      uInt sf;
      for (uInt s1 = 0; s1 < 3; s1++) {
 	ptPos(2) = s1;
	if (s1 == 2) {
	  sf = 3;
	} else {
	  sf = s1;
	}
 	AlwaysAssert(near(image.getAt(ptPos), flux(sf), C::flt_epsilon), 
 		     AipsError);
 	image.putAt(0.0f, ptPos);
      }
      for (uInt s = 0; s < 3; s++) {
  	ptPos(2) = s;
 	for (uInt y = 0; y < imSize; y++) {
 	  ptPos(1) = y;
 	  for (uInt x = 0; x < imSize; x++) {
 	    ptPos(0) = x;
 	    AlwaysAssert(near(image.getAt(ptPos), 0.0f), AipsError);
 	  }
 	}
      }
      image.table().markForDelete();
    }
    {
      CoordinateSystem coords = CoordinateUtil::defaultCoords4D();
      PagedImage<Float> image(IPosition(4, imSize, imSize, 4, nChan), 
			      coords, "tSkyCompRep_tmp.image");
      image.set(0.0f);
      
      skyPtr -> SkyCompRep::project(image);
      IPosition ptPos(4,2,1,0,0);
      for (uInt s1 = 0; s1 < 4; s1++) {
 	ptPos(2) = s1;
	for (uInt f1 = 0; f1 < nChan; f1++) {
	  ptPos(3) = f1;
	  AlwaysAssert(near(image.getAt(ptPos), flux(s1), C::flt_epsilon), 
		       AipsError);
	  image.putAt(0.0f, ptPos);
	}
      }
      for (uInt f = 0; f < nChan; f++) {
	ptPos(3) = f;
	for (uInt s = 0; s < 4; s++) {
	  ptPos(2) = s;
	  for (uInt y = 0; y < imSize; y++) {
	    ptPos(1) = y;
	    for (uInt x = 0; x < imSize; x++) {
	      ptPos(0) = x;
	      AlwaysAssert(near(image.getAt(ptPos), 0.0f), AipsError);
	    }
	  }
  	}
      }
      image.table().markForDelete();
    }
    // test the label functions
    {
      skyPtr -> SkyCompRep::setLabel(String("A dummy label"));
      String label;
      skyPtr -> SkyCompRep::label(label);
      AlwaysAssert(label == "", AipsError);
    }
    // test the ok function
    {
      AlwaysAssert(skyPtr -> SkyCompRep::ok() == True, AipsError);
    }
    // test the setFluxLinear & fluxLinear functions
    {
      Quantum<Vector<DComplex> > fluxLinear;
      {
	Vector<DComplex> valLinear(4);
	valLinear(0) = DComplex(1.5,0.0);
	valLinear(1) = DComplex(0.1,0.05);
	valLinear(2) = DComplex(0.1,-0.05);
	valLinear(3) = DComplex(0.5,0.0);
	fluxLinear.setValue(valLinear);
	fluxLinear.setUnit("WU");
      }
      skyPtr -> setFluxLinear(fluxLinear);

      Quantum<Vector<Double> > fluxStokes;
      skyPtr -> flux(fluxStokes);
      // The tolerances should be increased when the conversions are done in
      // double precision
      AlwaysAssert(near(fluxStokes.getValue("Jy")(0), 0.01, C::flt_epsilon), 
		   AipsError); 
      AlwaysAssert(near(fluxStokes.getValue("Jy")(1), 0.005, C::flt_epsilon), 
		   AipsError); 
      AlwaysAssert(near(fluxStokes.getValue("Jy")(2), 0.001, C::flt_epsilon),
		   AipsError); 
      AlwaysAssert(near(fluxStokes.getValue("Jy")(3), 0.0005, C::flt_epsilon),
		   AipsError); 

      skyPtr -> fluxLinear(fluxLinear);
      AlwaysAssert(near(fluxLinear.getValue("WU")(0), DComplex(1.5, 0.0), 
			C::flt_epsilon), AipsError); 
      AlwaysAssert(near(fluxLinear.getValue("WU")(1), DComplex(0.1, 0.05),
			C::flt_epsilon), AipsError); 
      AlwaysAssert(near(fluxLinear.getValue("WU")(2), DComplex(0.1, -0.05), 
			C::flt_epsilon), AipsError); 
      AlwaysAssert(near(fluxLinear.getValue("WU")(3), DComplex(0.5, 0.0),
			C::flt_epsilon), AipsError); 
    }
    // test the setFluxCircular & fluxCircular functions
    {
      Quantum<Vector<DComplex> > fluxCircular;
      {
	Vector<DComplex> valCircular(4);
	valCircular(0) = DComplex(1.5,0.0);
	valCircular(1) = DComplex(0.1,0.05);
	valCircular(2) = DComplex(0.1,-0.05);
	valCircular(3) = DComplex(0.5,0.0);
	fluxCircular.setValue(valCircular);
	fluxCircular.setUnit("WU");
      }
      skyPtr -> setFluxCircular(fluxCircular);

      Quantum<Vector<Double> > fluxStokes;
      skyPtr -> flux(fluxStokes);
      // The tolerances should be increased when the conversions are done in
      // double precision
      AlwaysAssert(near(fluxStokes.getValue("Jy")(0), 0.01, C::flt_epsilon), 
		   AipsError); 
      AlwaysAssert(near(fluxStokes.getValue("Jy")(1), 0.001, C::flt_epsilon), 
		   AipsError); 
      AlwaysAssert(near(fluxStokes.getValue("Jy")(2), 0.0005, C::flt_epsilon),
		   AipsError); 
      AlwaysAssert(near(fluxStokes.getValue("Jy")(3), 0.005, C::flt_epsilon),
		   AipsError); 

      skyPtr -> fluxCircular(fluxCircular);
      AlwaysAssert(near(fluxCircular.getValue("WU")(0), DComplex(1.5, 0.0), 
			C::flt_epsilon), AipsError); 
      AlwaysAssert(near(fluxCircular.getValue("WU")(1), DComplex(0.1, 0.05),
			C::flt_epsilon), AipsError); 
      AlwaysAssert(near(fluxCircular.getValue("WU")(2), DComplex(0.1, -0.05), 
			C::flt_epsilon), AipsError); 
      AlwaysAssert(near(fluxCircular.getValue("WU")(3), DComplex(0.5, 0.0),
			C::flt_epsilon), AipsError); 
    }
    delete skyPtr;
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tSkyCompRep"
// End: 
