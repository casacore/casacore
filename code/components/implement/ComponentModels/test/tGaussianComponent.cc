//# tGaussianComponent.cc:  this defines tGaussianComponent.cc
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

#include <trial/ComponentModels/GaussianComponent.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Images/PagedImage.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <trial/MeasurementEquations/StokesUtil.h>

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogSink.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/Euler.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Utilities/String.h>

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> gnu_tGaussianComponent_bug;
#endif

int main() {
  try {
    Bool anyFailures = False;
    {
      // Create a Gaussian component at the default position
      GaussianComponent defGaussian;
      // Sample the Gaussian at the Maximum at half a degree on either side.
      MVDirection sampleDir(Quantity(0,"deg"), Quantity(89.5, "deg"));
      RotMatrix rotDec(Euler(Quantity(0.5, "deg").get().getValue(), uInt(2)));
      
      const Float peak = 4. * pow(180.,2.) * C::ln2 * pow(C::pi,-3.0);
      const StokesVector polPeak(peak, 0.f,0.f,0.f);
      
      AlwaysAssert(near(defGaussian(sampleDir), polPeak*0.5f, 1E-6),AipsError);
      sampleDir *= rotDec;
      AlwaysAssert(near(defGaussian(sampleDir), polPeak, 1E-6), AipsError);
      sampleDir *= rotDec;
      AlwaysAssert(near(defGaussian(sampleDir), polPeak*0.5f, 1E-6),AipsError);
      cout << "Passed the default Gaussian component test" << endl;
    }
    {
      // Create a Gaussian component at a defined non-J2000 position
      MVDirection dir1934(Quantity(293.5,"deg"), Quantity(-63.7, "deg"));
      MDirection coord1934J2000(dir1934, MDirection::J2000);
      StokesVector flux1934(6.3f,0.0f,0.0f,0.0f);
      Vector<MVAngle> width(2);
      width = MVAngle(Quantity(2E-3, "''" ));
      GaussianComponent J1934(flux1934, coord1934J2000, width, MVAngle());
      // Create a direction that is 1 mas away from the pole
      MVDirection sampleDir(Quantity(0,"deg"),
			    Quantity(90, "deg") - Quantity(1.E-3, "''"));
      // And now make another rotater that can rotate this point about the pole
      // in steps of say 40 degrees
      RotMatrix rotater(Euler(Quantity(40, "deg").get().getValue(), uInt(3)));
      
      // Create a rotation matrix that can rotate the pole down to the
      // component. 
      RotMatrix pole2src(Euler(Quantity(-153.7,"deg").get().getValue(),uInt(2),
			       Quantity(-293.5,"deg").get().getValue(),uInt(3)
			       ));
      MVDirection pole;
      // Create a vector of MDirections equidistant from the position of the
      // component. All these points should have the same flux (in Jy/pixel)
      // of half the maximum. 
      Vector<MDirection> directions(6);
      for (uInt i = 0; i < 6; i++){
	directions(i) = MDirection(sampleDir*pole2src, MDirection::J2000);
	sampleDir *= rotater;
      }
      Vector<StokesVector> fluxes;
      fluxes = J1934(directions);
      const StokesVector peak = flux1934 * 4.0f 
	* pow(180.0f*60.0f*60.0f*1000.0f/2.0f,2.0f) 
	* C::ln2 * pow(C::pi,-3.0f);
      for (uInt j = 0; j < 6; j++)
	AlwaysAssert(near(fluxes(j), peak*0.5f, 1E-5), AipsError);
      cout << "Passed the arbitrary Gaussian component test" << endl;
    }
    {
      // Create an arbitrary Gaussian component
      GaussianComponent B1934(StokesVector(2.0f), 
			      MDirection(MVDirection(1.0), MDirection::B1950), 
			      MVAngle(Quantity(13, "''")), 
			      0.1f, 
			      MVAngle(Quantity(10, "deg")));
      
      // Set and verify  the flux of the Gaussian component.
      StokesVector flux1934(6.3f, 0.3f, 0.2f, 0.1f);
      B1934.setFlux(flux1934);
      
      AlwaysAssert(near(B1934.flux(), flux1934), AipsError);
      
      // Set and verify the position of the Gaussian component. It is
      // internally converted to a J2000 reference frame
      MVDirection dir1934(Quantity(293.5,"deg"),Quantity(-63.8,"deg"));
      MDirection coord1934B1950(dir1934, MDirection::B1950);
      B1934.setPosition(coord1934B1950);
      MDirection coord1934J2000 = B1934.position();
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue().near(
 	   MDirection::Convert(coord1934B1950,MDirection::J2000)().getValue()),
		   AipsError);
      // Set and verify the width of the Gaussian component. 
      Vector<MVAngle> width(2);
      width(0) = MVAngle(Quantity(4, "''" ));
      width(1) = MVAngle(Quantity(2, "''" ));
      B1934.setWidth(width);
      Vector<MVAngle> newWidth(2);
      newWidth = B1934.width();
      
      AlwaysAssert(near(newWidth(0).radian(),width(0).radian(), 1E-7), 
		   AipsError);
      AlwaysAssert(near(newWidth(1).radian(),width(1).radian(), 1E-7), 
		   AipsError);

      // Set and verify the axial ratio
      B1934.setAxialRatio(0.5f);
      AlwaysAssert(near(B1934.axialRatio(), 0.5f), AipsError);
      
      // Set and verify the position Angle
      B1934.setPA(MVAngle(Quantity(45.0, "deg")));
      AlwaysAssert(near(B1934.PA().circle(), 0.125, 1E-6), AipsError);
      
      // Check this is a Gaussian component
      AlwaysAssert(B1934.type().matches("Gaussian") == 1, AipsError);
      cout << "Passed the set/get parameters test for Gaussian components"
	   << endl;
    }
    {
      const uInt imSize = 6;
      const uInt nPol = 4;
      const uInt nFreq = 1;
      PagedImage<Float> image(IPosition(4,imSize,imSize,nPol,nFreq), 
			      defaultCoords4D(), 
			      "tGaussianComponent_tmp.image");
      image.set(0.0f);
      GaussianComponent defComp;
      
      Vector<MVAngle> width(2);
      width = MVAngle(Quantity(2, "'" ));
      defComp.setWidth(width);
      StokesVector flux(1.0f, 0.5f, 0.1f, 0.0f);
      defComp.setFlux(flux);
      
      MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
      MDirection coord00(ra0dec0, MDirection::J2000);
      defComp.setPosition(coord00);
      defComp(image);
//       const Float peak = 60.*60.* pow(180.,2.) * C::ln2 * pow(C::pi,-3.0);
//       AlwaysAssert(near(image(IPosition(4, 2, 1, 0, 0)),peak), AipsError);
//       AlwaysAssert(near(image(IPosition(4, 2, 0, 0, 0)),peak*0.5f), AipsError);
//       AlwaysAssert(near(image(IPosition(4, 2, 2, 0, 0)),peak*0.5f), AipsError);
//       AlwaysAssert(near(image(IPosition(4, 1, 1, 0, 0)),peak*0.5f), AipsError);
//       AlwaysAssert(near(image(IPosition(4, 3, 1, 0, 0)),peak*0.5f), AipsError);
//       AlwaysAssert(near(image(IPosition(4, 2, 1, 1, 0)),peak*0.5f), AipsError);
//       AlwaysAssert(near(image(IPosition(4, 2, 0, 1, 0)),peak*0.25f),AipsError);
//       AlwaysAssert(near(image(IPosition(4, 2, 1, 2, 0)),peak*0.1f), AipsError);
//       AlwaysAssert(near(image(IPosition(4, 1, 1, 2, 0)),peak*0.05f),AipsError);
//       AlwaysAssert(near(image(IPosition(4, 2, 1, 3, 0)),peak*0.0f), AipsError);
      
//       cout << "Passed the projection to an image test" << endl;
    }
//     {
//       const uInt imSize = 6;
//       const uInt nPol = 1;
//       CoordinateSystem coords(defaultCoords2D());
//       addIAxis(coords);
//       PagedImage<Float> image(IPosition(3,imSize,imSize,nPol), 
// 			      coords, 
// 			      String("tGaussianComponent_tmp.image"));
//       PagedImage<Float> psf(IPosition(2,4), defaultCoords2D(), 
// 			    String("tGaussianComponentPsf_tmp.image"));
//       image.set(Float(0)); psf.set(Float(0));
//       psf(IPosition(2, 2, 2)) = Float(1);
//       psf(IPosition(2, 1, 2)) = Float(.5);
//       psf(IPosition(2, 3, 2)) = Float(.5);
//       psf(IPosition(2, 2, 1)) = Float(.5);
//       psf(IPosition(2, 2, 3)) = Float(.5);
      
//       GaussianComponent defComp;
//       MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
//       MDirection coord00(ra0dec0, MDirection::J2000);
//       defComp.setPosition(coord00);
//       Vector<MVAngle> width(2); 
//       width = MVAngle(Quantity(2, "''" ));
//       defComp.setWidth(width);
//       StokesVector flux(1.0f, 0.5f, 0.1f, 0.0f);
//       defComp.setFlux(flux);
//       const Float peak=60.*60.*60.*60.*pow(180.,2.) * C::ln2 * pow(C::pi,-3.0);
      
//       defComp(image, psf);

//       AlwaysAssert(near(image(IPosition(3, 2, 1, 0)), peak), AipsError);
//       image(IPosition(3, 2, 1, 0)) = Float(0);
//       AlwaysAssert(near(image(IPosition(3, 1, 1, 0)), peak*0.5f), AipsError);
//       image(IPosition(3, 1, 1, 0)) = Float(0);
//       AlwaysAssert(near(image(IPosition(3, 3, 1, 0)), peak*0.5f), AipsError);
//       image(IPosition(3, 3, 1, 0)) = Float(0);
//       AlwaysAssert(near(image(IPosition(3, 2, 0, 0)), peak*0.5f), AipsError);
//       image(IPosition(3, 2, 0, 0)) = Float(0);
//       AlwaysAssert(near(image(IPosition(3, 2, 2, 0)), peak*0.5f), AipsError);
//       image(IPosition(3, 2, 2, 0)) = Float(0);

//       cout << "Passed the projection to an image (with convolution) test" 
// 	   << endl;
//   }
  if (anyFailures) {
    cout << "FAIL" << endl;
    return 1;
  }
  else {
    cout << "OK" << endl;
    return 0;
  }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    return 1;
  } end_try;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tGaussianComponent"
// End: 
