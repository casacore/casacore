//# tPointComponent.cc:  this defines tPointComponent.cc
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

#include <aips/aips.h>
#include <trial/ComponentModels/PointComponent.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Images/PagedImage.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <trial/MeasurementEquations/StokesUtil.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> gpp_complist_bug1;
#endif

int main() {
  try {
    {
      // Create a point component at the default position
      MVDirection defMVdir;
      MDirection defDirJ2000(defMVdir);
      MDirection defDirB1950(defMVdir, MDirection::B1950);
      PointComponent defPoint;
      StokesVector flux(1.0f, 0.0f, 0.0f, 0.0f);
      StokesVector zero(0.0f);
      AlwaysAssert(near(defPoint(defDirJ2000), flux), AipsError);
      AlwaysAssert(near(defPoint(defDirB1950), zero), AipsError);
      cout << "Passed the default Point component test" << endl;
    }
    {
      // Create a point component at a defined non-J2000 position
      MVDirection dir1934(Quantity(293.5,"deg"),
			  Quantity(-63, "deg") + Quantity(43, "'"));
      MDirection coord1934J2000(dir1934, MDirection::J2000);
      MDirection coord1934B1950(dir1934, MDirection::B1950);
      StokesVector flux1934(6.3f, 0.0f, 0.0f, 0.0f);
      PointComponent B1934(flux1934, coord1934B1950);
      Vector<MDirection> directions(2);
      directions(0) = coord1934B1950;
      directions(1) = coord1934J2000;
      Vector<StokesVector> fluxes(2);
      fluxes = B1934(directions);
      AlwaysAssert(near(fluxes(0), flux1934), AipsError);
      AlwaysAssert(near(fluxes(1), StokesVector(0.0f)), AipsError);
      cout << "Passed the arbitrary Point component test" << endl;
    }
    {
      // Create a default point component
      PointComponent B1934;
      // Set and verify  the flux of the point component.
      StokesVector flux1934(6.3f, 0.0f, 0.0f, 0.0f);
      B1934.setFlux(flux1934);
      AlwaysAssert(near(B1934.flux(), flux1934), AipsError);

      // Set and verify the position of the point component. It is internally
      // converted to a J2000 reference frame
      MVDirection dir1934(Quantity(293.5,"deg"),Quantity(-63.8,"deg"));
      MDirection coord1934B1950(dir1934, MDirection::B1950);
      B1934.setPosition(coord1934B1950);
      MDirection coord1934J2000 = B1934.position();
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue().near(MDirection::Convert(
                   coord1934B1950,MDirection::J2000)().getValue()),AipsError);
      
      // Check this is a point component
      AlwaysAssert(B1934.type().matches("Point") == 1, AipsError);
      cout << "Passed the set/get parameters test for point components" 
	   << endl;
    }
    {
    uInt nx=6, ny=nx;
    uInt nPol = 4;
    uInt nFreq = 2;
    PagedImage<Float> image(IPosition(4,nx,ny,nPol,nFreq), defaultCoords4D(), 
			    "tPointComponent_tmp.image");
    image.set(0.0f);
    PointComponent defComp;
    MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
    MDirection coord00(ra0dec0, MDirection::J2000);
    defComp.setPosition(coord00);
    defComp(image);
    AlwaysAssert(near(image(IPosition(4, 2, 1, 0, 0)), 1.0f), AipsError);
    image(IPosition(4, 2, 1, 0, 0)) = 0.0f;
    AlwaysAssert(near(image(IPosition(4, 2, 1, 0, 1)), 1.0f), AipsError);
    image(IPosition(4, 2, 1, 0, 1)) = 0.0f;

    for (uInt f = 0; f < nFreq; f++)
      for (uInt p = 0; p < nPol; p++)
 	for (uInt i = 0; i < 6; i++)
 	  for (uInt j = 0; j < 6; j++)
 	    AlwaysAssert(near(image(IPosition(4, i, j, p, f)), 0.0f), 
			 AipsError);
    cout << "Passed the projection to an image test" << endl;
    }
    {
      uInt nx=6, ny=nx;
      uInt nPol = 1;
      CoordinateSystem imcoord(defaultCoords2D());
      addIAxis(imcoord);
      PagedImage<Float> image(IPosition(3,nx, ny, nPol), imcoord, 
			      String("test.image"));
      PagedImage<Float> psf(IPosition(2,4), defaultCoords2D(), 
			    String("psf.image"));
      image.set(Float(0)); psf.set(Float(0));
      psf(IPosition(2, 2, 2)) = Float(1);
      psf(IPosition(2, 1, 2)) = Float(.5);
      psf(IPosition(2, 3, 2)) = Float(.5);
      psf(IPosition(2, 2, 1)) = Float(.5);
      psf(IPosition(2, 2, 3)) = Float(.5);
    
      PointComponent defComp;
      MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
      MDirection coord00(ra0dec0, MDirection::J2000);
      defComp.setPosition(coord00);
      defComp(image, psf);

      AlwaysAssert(near(image(IPosition(3, 2, 1, 0)), 1.0f), AipsError);
      image(IPosition(3, 2, 1, 0)) = 0.0f;
      AlwaysAssert(near(image(IPosition(3, 1, 1, 0)), 0.5f), AipsError);
      image(IPosition(3, 1, 1, 0)) = 0.0f;
      AlwaysAssert(near(image(IPosition(3, 3, 1, 0)), 0.5f), AipsError);
      image(IPosition(3, 3, 1, 0)) = 0.0f;
      AlwaysAssert(near(image(IPosition(3, 2, 0, 0)), 0.5f), AipsError);
      image(IPosition(3, 2, 0, 0)) = 0.0f;
      AlwaysAssert(near(image(IPosition(3, 2, 2, 0)), 0.5f), AipsError);
      image(IPosition(3, 2, 2, 0)) = 0.0f;

      for (uInt i = 0; i < 6; i++)
        for (uInt j = 0; j < 6; j++)
	  AlwaysAssert(nearAbs(image(IPosition(3, i, j, 0)), 0.0f), AipsError);

      cout << "Passed the projection to an image (with convolution) test" 
	   << endl;
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
// compile-command: "gmake OPTLIB=1 tPointComponent"
// End: 
