//# tGaussianCompRep.cc:  this defines tGaussianCompRep.cc
//# Copyright (C) 1996,1997
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
#include <trial/ComponentModels/GaussianCompRep.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Images/PagedImage.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/Euler.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Measures/Stokes.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>

int main() {
  try {
    {
      // Create a Gaussian component at the default position
      GaussianCompRep defGaussian;
      const MVAngle pixelSize(Quantity(1.0,"''"));
      // Sample the Gaussian at the Maximum and half an arc-min on either side.
      MVDirection sampleDirVal(Quantity(0,"deg"), 
			       Quantity(90, "deg") - Quantity(.5, "'"));
      MDirection sampleDir(sampleDirVal, MDirection::J2000);
      RotMatrix rotDec(Euler(Quantity(0.5, "'").get().getValue(), 2u));
      
      // This is not exact. To be exact I should do a integration over the
      // pixel area. Instead I set the pixel size to be something small enough!
      const Double peak = 4. * 3600 * pow(180.,2.) * C::ln2 * pow(C::pi,-3.0) *
	pixelSize.radian() * pixelSize.radian();
      Vector<Double> expectedSample(4);
      Vector<Double> actualSample(4);
      expectedSample = 0.0; expectedSample(0) = peak*0.5;
      defGaussian.sample(actualSample, sampleDir, pixelSize);
      AlwaysAssert(allNear(actualSample.ac(), expectedSample.ac(), 1E-10),
		   AipsError);
      sampleDirVal *= rotDec;
      sampleDir.set(sampleDirVal);
      defGaussian.sample(actualSample, sampleDir, pixelSize);
      expectedSample(0) = peak;
      AlwaysAssert(allNear(actualSample.ac(), expectedSample.ac(), 1E-10),
		   AipsError);
      sampleDirVal *= rotDec;
      sampleDir.set(sampleDirVal);
      expectedSample(0) = peak*0.5;
      defGaussian.sample(actualSample, sampleDir, pixelSize);
      AlwaysAssert(allNear(actualSample.ac(), expectedSample.ac(), 1E-10),
		   AipsError);
      cout << "Passed the default Gaussian component test" << endl;
    }
    {
      // Create a Gaussian component at a defined non-J2000 position
      MVDirection dir1934(Quantity(293.5,"deg"), Quantity(-63.7, "deg"));
      MDirection coord1934J2000(dir1934, MDirection::J2000);
      Vector<Double> flux1934(4);
      flux1934 = 0.0; flux1934(0) = 6.3;
      MVAngle majorAxis(Quantity(2E-3, "''"));
      MVAngle minorAxis(Quantity(2E-3, "''"));
      GaussianCompRep J1934(flux1934, coord1934J2000, majorAxis, minorAxis,
			    MVAngle());
      // Create a direction that is 1 mas away from the pole
      MVDirection sampleDir(Quantity(0,"deg"),
 			    Quantity(90, "deg") - Quantity(1.E-3, "''"));
      // And now make another rotater that can rotate this point about the pole
      // in steps of say 40 degrees
      RotMatrix rotater(Euler(Quantity(40, "deg").get().getValue(), uInt(3)));
   
      // Create a rotation matrix that can rotate the pole down to the
      // component. 
      RotMatrix pole2src(Euler(Quantity(-153.7,"deg").get().getValue(), 2u,
 			       Quantity(-293.5,"deg").get().getValue(), 3u));
      MVDirection pole;
      // Sample at a set of MDirections equidistant from the position of the
      // component. All these points should have the same flux (in Jy/pixel)
      // of half the maximum. 
      const MVAngle pixelSize(Quantity(1.0,"''"));
      MDirection sampledDirection;
      Vector<Double> sampledFlux(4);
      Vector<Double> peak(4);
      peak = flux1934.ac() * 4. * pow(180. *60. *60. *1000. /2. ,2.) 
	                   * C::ln2 * pow(C::pi,-3.) 
	                   * pixelSize.radian() * pixelSize.radian();
      for (uInt i = 0; i < 6; i++){
 	sampledDirection = MDirection(sampleDir*pole2src, MDirection::J2000);
	J1934.sample(sampledFlux, sampledDirection, pixelSize);
	// Precision is lost because of the subtraction done in the
	// MVPosition::separation member function
 	AlwaysAssert(allNear(sampledFlux.ac(), peak.ac()*0.5, 1E-6), AipsError);
 	sampleDir *= rotater;
      }
      cout << "Passed the arbitrary Gaussian component test" << endl;
    }
    {
      // Create a Gaussian component at a defined non-J2000 position
      Vector<Double> initialFlux(4);
      initialFlux = 2.0;
      const MDirection initialPosition(MVDirection(1.0), MDirection::B1950);
      const MVAngle initialMajorAxis(MVAngle(Quantity(13, "''")));
      const Double initialAxialRatio = 0.1;
      const MVAngle initialPA(MVAngle(Quantity(10, "deg")));
   
      GaussianCompRep B1934(initialFlux, initialPosition, initialMajorAxis,
			    initialAxialRatio, initialPA);
      Vector<Double> componentFlux(4);
      B1934.flux(componentFlux);
      AlwaysAssert(allNear(initialFlux.ac(), componentFlux.ac(), 1E-10),
		   AipsError);
   
      // Set and verify  the flux of the Gaussian component.
      Vector<Double> flux1934(4);
      flux1934 = 0.0; flux1934(0) = 6.3;
      B1934.setFlux(flux1934);
      B1934.flux(componentFlux);
      AlwaysAssert(allNear(flux1934.ac(), componentFlux.ac(), 1E-10),
		   AipsError);
   
      // Set and verify the position of the Gaussian component. It is
      // internally converted to a J2000 reference frame
      MVDirection dir1934(Quantity(293.5,"deg"),Quantity(-63.8,"deg"));
      MDirection coord1934B1950(dir1934, MDirection::B1950);
      MDirection coord1934J2000 = coord1934B1950;
      B1934.position(coord1934J2000);
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
 		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue().near(
  	   MDirection::Convert(initialPosition,MDirection::J2000)().getValue()),
 		   AipsError);
      B1934.setPosition(coord1934B1950);
      B1934.position(coord1934J2000);
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
 		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue().near( 
  	   MDirection::Convert(coord1934B1950,MDirection::J2000)().getValue()),
		   AipsError);
      // Set and verify the width of the Gaussian component. 
      MVAngle majorAxis;
      B1934.majorAxis(majorAxis);
      AlwaysAssert(near(majorAxis.radian(), initialMajorAxis.radian(), 1E-10), 
 		   AipsError);
      MVAngle minorAxis;
      B1934.minorAxis(minorAxis);
      AlwaysAssert(near(minorAxis.radian(), 
			initialMajorAxis.radian()*initialAxialRatio, 1E-10), 
 		   AipsError);
      Double axialRatio;
      B1934.axialRatio(axialRatio);
      AlwaysAssert(near(axialRatio, initialAxialRatio, 1E-10), AipsError);
      MVAngle pa;
      B1934.positionAngle(pa);
      AlwaysAssert(near(pa.radian(), initialPA.radian(), 1E-10), AipsError);
   
      MVAngle compMajorAxis(Quantity(4, "''" ));
      MVAngle compMinorAxis(Quantity(2, "''" ));
      MVAngle compPA(Quantity(45, "deg" ));
      B1934.setWidth(compMajorAxis, compMinorAxis, compPA);
      B1934.width(majorAxis, minorAxis, pa);
      AlwaysAssert(near(majorAxis.radian(), compMajorAxis.radian(), 1E-10), 
		   AipsError);
      AlwaysAssert(near(minorAxis.radian(), compMinorAxis.radian(), 1E-10), 
		   AipsError);
      AlwaysAssert(near(pa.radian(), compPA.radian(), 1E-10), AipsError);
   
      compMajorAxis = Quantity(8, "''");
      compPA = Quantity(30, "deg");
      Double compAxialRatio = .5;
      B1934.setWidth(compMajorAxis, compAxialRatio, compPA);
      B1934.width(majorAxis, axialRatio, pa);
      AlwaysAssert(near(majorAxis.radian(), compMajorAxis.radian(), 1E-10), 
		   AipsError);
      AlwaysAssert(near(axialRatio, compAxialRatio, 1E-10), AipsError);
      AlwaysAssert(near(pa.radian(), compPA.radian(), 1E-10), AipsError);
   
      // Check this is a Gaussian component
      AlwaysAssert(B1934.type() == ComponentType::GAUSSIAN, AipsError);
      AlwaysAssert(ComponentType::name(B1934.type()).matches("Gaussian") == 1, 
 		   AipsError);

      // Check the parameters interface
      AlwaysAssert(B1934.nParameters() == 3, AipsError);
      Vector<Double> parms(3);
      B1934.parameters(parms);
      AlwaysAssert(near(parms(0), compMajorAxis.radian(), 1E-10), AipsError);
      AlwaysAssert(near(parms(1), compMajorAxis.radian()*compAxialRatio,1E-10),
		   AipsError);
      AlwaysAssert(near(parms(2), compPA.radian(), 1E-10), AipsError);
      parms(0) = Quantity(4, "''").getValue("rad");
      parms(1) = Quantity(2, "''").getValue("rad");;
      parms(2) = Quantity(45.0, "deg").getValue("rad");
      B1934.setParameters(parms);
      parms = 0.0;
      B1934.parameters(parms);
      AlwaysAssert(near(parms(0), Quantity(4, "''").getValue("rad"), 1E-10), 
		   AipsError);
      AlwaysAssert(near(parms(1), Quantity(2, "''").getValue("rad"), 1E-10), 
		   AipsError);
      AlwaysAssert(near(parms(2), Quantity(45, "deg").getValue("rad"), 1E-10), 
		   AipsError);
      parms.resize(1);
      try {
	B1934.setParameters(parms);
  	throw(AipsError("GaussianCompRep incorrectly accepted a too small "
  			"Parameter Vector"));
      }
      catch (AipsError x) {
 	if (!x.getMesg().contains("newParms.nelements() == nParameters()")) {
 	  cerr << x.getMesg() << endl;
 	  cout << "FAIL" << endl;
 	  return 1;
 	}
      }
      end_try;
      cout << "Passed the set/get parameters test for Gaussian components"
 	   << endl;
      SkyCompRep * compRepPtr = B1934.clone();
      AlwaysAssert(compRepPtr->type() == ComponentType::GAUSSIAN, AipsError);
      flux1934 = 0.0;
      compRepPtr->flux(flux1934) ;
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      GaussianCompRep copiedComp(*((GaussianCompRep *) compRepPtr));
      flux1934 = 0.0;
      copiedComp.setFlux(flux1934);
      compRepPtr->flux(flux1934) ;
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      copiedComp.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 0.0), AipsError);
      copiedComp = B1934;
      copiedComp.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      flux1934 = 0.0;
      copiedComp.setFlux(flux1934);
      B1934.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      copiedComp.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 0.0), AipsError);
      AlwaysAssert(B1934.ok(), AipsError);
      AlwaysAssert(copiedComp.SkyCompRep::ok(), AipsError);
      AlwaysAssert(compRepPtr->ok(), AipsError);
      cout << "Passed the copy and assignment tests" 
  	   << endl;
    }
    {
      const uInt imSize = 6;
      const uInt nPol = 2;
      const uInt nFreq = 1;
      CoordinateSystem coords(defaultCoords2D());
      {
	Vector<Int> pols(nPol);
	pols(0) = Stokes::I;
	pols(1) = Stokes::U;
	StokesCoordinate polAxis(pols);
	coords.addCoordinate(polAxis);
      }
      addFreqAxis(coords);
      PagedImage<Float> image(IPosition(4,imSize,imSize,nPol,nFreq), 
 			      coords, "tGaussianCompRep_tmp.image");
      image.set(0.0f);
      GaussianCompRep defComp;
   
      MVAngle majorAxis(Quantity(2., "'"));
      MVAngle minorAxis = majorAxis;
      MVAngle pa(Quantity(0., "deg"));
      defComp.setWidth(majorAxis, minorAxis, pa);
      Vector<Double> flux(4);
      flux(0) = 1.0;
      flux(1) = 0.5;
      flux(2) = 0.2;
      flux(3) = 0.1;
      defComp.setFlux(flux);
   
      MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
      MDirection coord00(ra0dec0, MDirection::J2000);
      defComp.setPosition(coord00);
      defComp.project(image);
      Double pixelSize = Quantity(1.0, "'").getValue("rad");
      Float peak = 60.*60.* pow(180.,2.) * C::ln2 * pow(C::pi,-3.0) 
	* pixelSize * pixelSize;
      AlwaysAssert(near(image(IPosition(4, 2, 1, 0, 0)), peak), AipsError);
      AlwaysAssert(near(image(IPosition(4, 2, 0, 0, 0)),peak*0.5f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 2, 2, 0, 0)),peak*0.5f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 1, 1, 0, 0)),peak*0.5f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 3, 1, 0, 0)),peak*0.5f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 2, 1, 1, 0)),peak*0.2f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 2, 0, 1, 0)),peak*0.1f), AipsError);
   
      majorAxis = Quantity(10., "'");
      minorAxis = Quantity(2., "'");
      pa = Quantity(-1.0*atan(3.0/4.0), "rad");
      defComp.setWidth(majorAxis, minorAxis, pa);
      image.set(0.0f);
      defComp.project(image);
      peak = image(IPosition(4,2,1,0,0));
      AlwaysAssert(near(image(IPosition(4, 5, 5, 0, 0)), peak*0.5f),
		   AipsError);
      image.table().markForDelete();
      cout << "Passed the projection to an image test" << endl;
    }
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
// compile-command: "gmake OPTLIB=1 tGaussianCompRep"
// End: 
