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
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/PointCompRep.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Images/PagedImage.h>

int main() {
  try {
    SkyCompRep * skyPtr = new PointCompRep;
    Vector<Double> flux;
    {
      Flux<Double> compFlux(2.0, 0.5, 0.1, 0.1);
      compFlux.setUnit("Jy");
      compFlux.value(flux);
      skyPtr -> flux() = compFlux;
    }
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
