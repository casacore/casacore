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
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/PointComponent.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Images/PagedImage.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>

int main() {
  try {
    {
      // Create a point component at the default position
      MVDirection defMVdir;
      MDirection defDirJ2000(defMVdir);
      MDirection defDirB1950(defMVdir, MDirection::B1950);
      PointComponent defPoint;
      Vector<Double> sampledFlux(4);
      Vector<Double> expectedFlux(4); 
      expectedFlux = 0.0; expectedFlux(0) = 1.0;
      defPoint.sample(sampledFlux, defDirJ2000);
      AlwaysAssert(allNear(sampledFlux.ac(), expectedFlux.ac(), 1E-12), 
 		   AipsError);
      defPoint.sample(sampledFlux, defDirB1950);
      expectedFlux = 0.0;
      AlwaysAssert(allNear(sampledFlux.ac(), expectedFlux.ac(), 1E-12), 
 		   AipsError);
      cout << "Passed the default Point component test" << endl;
    }
    {
      // Create a point component at a defined non-J2000 position
      MVDirection dir1934(Quantity(293.5,"deg"),
 			  Quantity(-63, "deg") + Quantity(43, "'"));
      MDirection coord1934J2000(dir1934, MDirection::J2000);
      MDirection coord1934B1950(dir1934, MDirection::B1950);
      Vector<Double> flux1934(4); flux1934 = 0.0; flux1934(0) = 6.3;
      PointComponent B1934(flux1934, coord1934B1950);
      Vector<Double> sampledFlux(4);
      B1934.sample(sampledFlux, coord1934B1950);
      AlwaysAssert(allNear(sampledFlux.ac(), flux1934.ac(), 1E-12), 
 		   AipsError);
      B1934.sample(sampledFlux, coord1934J2000);
      AlwaysAssert(allNear(sampledFlux.ac(), flux1934.ac()*0.0, 1E-12),
 		   AipsError);
       cout << "Passed the arbitrary Point component test" << endl;
    }
    {
      // Create a default point component
      PointComponent B1934;
      // Set and verify  the flux of the point component.
      Vector<Double> flux1934(4); flux1934 = 0.0; flux1934(0) = 6.3;
      B1934.setFlux(flux1934);
      Vector<Double> pointFlux(4);
      B1934.flux(pointFlux);
      AlwaysAssert(allNear(pointFlux.ac(), flux1934.ac(), 1E-12), 
  		   AipsError);
      
      // Set and verify the position of the point component. It is internally
      // converted to a J2000 reference frame
      MVDirection dir1934(Quantity(293.5,"deg"),Quantity(-63.8,"deg"));
      MDirection coord1934B1950(dir1934, MDirection::B1950);
      B1934.setPosition(coord1934B1950);
      MDirection coord1934J2000;
      B1934.position(coord1934J2000);
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
  		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue().near(MDirection::Convert(
 		   coord1934B1950,MDirection::J2000)().getValue()),AipsError);
      
      // Check this is a point component
      AlwaysAssert(B1934.type() == ComponentType::POINT, AipsError);
      AlwaysAssert(ComponentType::name(B1934.type()).matches("Point") == 1, 
 		   AipsError);
      // Check the parameters interface (there are no parameters!)
      AlwaysAssert(B1934.nParameters() == 0, AipsError);
      Vector<Double> parms;
      B1934.setParameters(parms);
      B1934.parameters(parms);
      parms.resize(1);
      try {
 	B1934.setParameters(parms);
  	throw(AipsError("PointCompRep incorrectly accepted a non-zero "
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
      try {
	B1934.parameters(parms);
	throw(AipsError("PointCompRep incorrectly used a non-zero "
 			"Parameter Vector"));
      }
      catch (AipsError x) {
 	if (!x.getMesg().contains("compParms.nelements() == nParameters()")) {
 	  cerr << x.getMesg() << endl;
 	  cout << "FAIL" << endl;
 	  return 1;
 	}
      }
      end_try;
      cout << "Passed the set/get parameters test for point components" 
  	   << endl;
      PointComponent compCopy = B1934.copy();
      AlwaysAssert(compCopy.type() == ComponentType::POINT, AipsError);
      flux1934 = 0.0;
      B1934.setFlux(flux1934);
      B1934.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 0.0), AipsError);
      compCopy.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      compCopy = B1934.copy();
      compCopy.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 0.0), AipsError);
      flux1934(0) = 6.3;
      B1934.setFlux(flux1934);
      compCopy.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 0.0), AipsError);
      B1934.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      PointComponent compRef = B1934;
      compRef.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      flux1934 = 0.0;
      B1934.setFlux(flux1934);
      flux1934(0) = 6.3;
      compRef.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 0.0), AipsError);
      flux1934(0) = 6.3;
      compCopy.setFlux(flux1934);
      compRef = compCopy;
      flux1934(0) = 0.0;
      compRef.flux(flux1934);
      AlwaysAssert(near(flux1934(0), 6.3), AipsError);
      AlwaysAssert(B1934.ok(), AipsError);
      AlwaysAssert(compCopy.ok(), AipsError);
      AlwaysAssert(compRef.ok(), AipsError);
      cout << "Passed the copy and assignment tests" 
  	   << endl;
    }
    {
      uInt nx=6, ny=nx;
      uInt nFreq = 2;
      PagedImage<Float> image(IPosition(3,nx,ny,nFreq), 
 			      defaultCoords3D(),
 			      "tPointComponent_tmp.image");
      image.set(0.0f);
      PointCompRep defComp;
      MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
      MDirection coord00(ra0dec0, MDirection::J2000);
      defComp.setPosition(coord00);
      defComp.project(image);
      AlwaysAssert(near(image(IPosition(3, 2, 1, 0)), 1.0f), AipsError);
      image(IPosition(3, 2, 1, 0)) = 0.0f;
      AlwaysAssert(near(image(IPosition(3, 2, 1, 1)), 1.0f), AipsError);
      image(IPosition(3, 2, 1, 1)) = 0.0f;
      
      for (uInt f = 0; f < nFreq; f++)
	for (uInt i = 0; i < nx; i++)
	  for (uInt j = 0; j < ny; j++)
	    AlwaysAssert(near(image(IPosition(3, i, j, f)), 0.0f), 
			 AipsError);
      image.table().rename("junk.image", Table::Scratch);
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
// compile-command: "gmake OPTLIB=1 tPointComponent"
// End: 
