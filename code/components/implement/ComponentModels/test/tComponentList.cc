//# tComponentList.cc:  this defines tComponentList.cc
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
#include <trial/ComponentModels/ComponentList.h>
#include <trial/ComponentModels/PointComponent.h>
#include <trial/ComponentModels/GaussianComponent.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Images/PagedImage.h>
#include <trial/MeasurementEquations/StokesVector.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/Assert.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>


int main() {
  try {
    {
      // Test all the constructors and ways of putting data into the lists.
      ComponentList model;

      GaussianComponent defaultGauss;
      model.addComp(defaultGauss);
      model.addComp(new PointComponent(StokesVector(2.0f,0.0f,0.1f,0.1f), 
				       MDirection(Quantity(10, "deg"),
						  Quantity(87, "deg"), 
						  MDirection::J2000)));
      CountedPtr<SkyComponent> countedPtr(new GaussianComponent());
      model.addComp(countedPtr);
      GaussianComponent * 
	ptrGauss = new GaussianComponent(StokesVector(4.0f,2.0f,0.1f,0.1f), 
					 MDirection(Quantity(30, "deg"),
						    Quantity(89, "deg"),
						    MDirection::J2000),
					 MVAngle(Quantity(0.5, "deg")),
					 0.5f,
					 MVAngle(Quantity(60, "deg")));
      model.insertComp(ptrGauss);
      // reset the flux of the first component. Reference semantics means
      // this changes the value in the list also.
      defaultGauss.setFlux(StokesVector(5.0f, 1.0f, 0.1f, 0.05f));
      // So the list should now contain 4 elements with I fluxes of
      // 5, 2, 4, 1
      model.gotoStart();
      AlwaysAssert(near(model.getComp()->flux()(0), 5.0f), AipsError);
      model.nextComp();
      AlwaysAssert(near(model.getComp()->flux()(0), 2.0f), AipsError);
      model.nextComp();
      AlwaysAssert(near(model.getComp()->flux()(0), 4.0f), AipsError);
      model.nextComp();
      AlwaysAssert(near(model.getComp()->flux()(0), 1.0f), AipsError);
      model.prevComp();
      AlwaysAssert(model.curPosition() == 3, AipsError);
      // Try my hardest to induce a memory leak by extracting a component,
      // deleting it from the list and adding it to another list.
      model.gotoEnd();
      CountedPtr<SkyComponent> countedPtr1(model.getComp());
      model.removeComp();
      ComponentList model1(countedPtr1);
      AlwaysAssert(model.nComponents() == 3, AipsError);
      AlwaysAssert(model.curPosition() == 3, AipsError);
      AlwaysAssert(model1.nComponents() == 1, AipsError);
      AlwaysAssert(model1.curPosition() == 1, AipsError);
      AlwaysAssert(near(model1.getComp()->flux()(0), 1.0f), AipsError);
      // try a few of the other constructors and functions. 
      ComponentList model2(new PointComponent());
      model2.insertComp(defaultGauss);
      AlwaysAssert(model2.curPosition() == 1, AipsError);
      AlwaysAssert(model2.getComp()->type().matches("Gaussian"), AipsError);
      model2.gotoPosition(2);
      AlwaysAssert(model2.curPosition() == 2, AipsError);
      AlwaysAssert(model2.getComp()->type().matches("Point"), AipsError);
      AlwaysAssert(near(model2.getComp()->flux()(1), 0.0f), AipsError);
      ComponentList model3(defaultGauss);
      AlwaysAssert(model3.getComp()->type().matches("Gaussian"), AipsError);
      cout << "Passed the Constructors, insertion, deletion and traversal tests" 
	   << endl;
    }
    {
      ComponentList model(new PointComponent());
      MDirection otherDir(Quantity(10, "deg"),Quantity(87, "deg"), 
 			  MDirection::J2000);
      model.addComp(new PointComponent(StokesVector(2.0f,0.0f,0.1f,0.1f), 
 				       otherDir));
      MVDirection NPoleDir;
      MDirection NPole(NPoleDir, MDirection::J2000);
      AlwaysAssert(near(model(NPole)(0), 1.0f), AipsError);
      AlwaysAssert(near(model(otherDir)(2), 0.1f), AipsError);
      MDirection thirdDir(Quantity(40, "deg"), Quantity(89, "deg"), 
			  MDirection::J2000);
      AlwaysAssert(near(model(thirdDir)(0), 0.0f), AipsError);

      Vector<MDirection> directions(3);
      directions(0) = NPole;
      directions(1) = otherDir;
      directions(2) = thirdDir;
      Vector<StokesVector> samples(3);
      samples = model(directions);
      AlwaysAssert(near(samples(0)(1), 0.0f), AipsError);
      AlwaysAssert(near(samples(1)(0), 2.0f), AipsError);
      AlwaysAssert(near(samples(2)(1), 0.0f), AipsError); 
      while (model.nComponents() > 0)
	model.removeComp();

      const uInt imSize = 30;
      const uInt nPol = 4;
      const uInt nFreq = 1;
      CoordinateSystem imCoords = defaultCoords2D();
      imCoords.setIncrement(imCoords.increment().ac()/60.0);
      addIQUVAxis(imCoords);
      addFreqAxis(imCoords);
      PagedImage<Float> image(IPosition(4,imSize,imSize,nPol,nFreq), 
			      imCoords, 
			      String("tComponentList_tmp.image"));
      image.set(0.0f);
      GaussianComponent gcomp;
      model.addComp(gcomp);
      Vector<MVAngle> width(2); 
      width(0) = MVAngle(Quantity(10., "''" ));
      width(1) = MVAngle(Quantity(2., "''" ));
      gcomp.setWidth(width);
      gcomp.setPA(MVAngle(Quantity(atan(6./8.), "rad")));
      StokesVector flux(1.0f, 0.5f, 0.1f, 0.0f);
      model.getComp()->setFlux(flux);
      MVDirection ra0dec0(Quantity(4, "''"), Quantity(2, "''"));
      MDirection coord00(ra0dec0, MDirection::J2000);
      model.getComp()->setPosition(coord00);

      model.addComp(new PointComponent());
      MVDirection ra1dec1(Quantity(23, "''"), Quantity(17, "''"));
      MDirection coord11(ra1dec1, MDirection::J2000);
      model.getComp()->setPosition(coord11);
      model.getComp()->setFlux(StokesVector(1E5, 4.E4, 2.E4, 1.E4));

      model(image);
      const Float peak = 4/(width(0).radian()*width(1).radian())*C::ln2/C::pi;
      AlwaysAssert(near(image(IPosition(4, 4, 2, 0, 0)), peak), AipsError);
      AlwaysAssert(near(image(IPosition(4, 1, 6, 0, 0)), peak/2), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 17, 0, 0)), 1E5f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 17, 3, 0)), 1E4f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 18, 0, 0)), 0.0f), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 18, 2, 0)), 0.0f), AipsError);
      
      PagedImage<Float> psf(IPosition(2,4), defaultCoords2D(), 
			    String("tComponentListPsf_tmp.image"));
      image.set(0.0f); psf.set(0.0f);
      psf(IPosition(2, 2, 2)) = Float(1);
      psf(IPosition(2, 1, 2)) = Float(.5);
      psf(IPosition(2, 3, 2)) = Float(.5);
      psf(IPosition(2, 2, 1)) = Float(.5);
      psf(IPosition(2, 2, 3)) = Float(.5);
      
      model(image, psf);
      AlwaysAssert(!near(image(IPosition(4, 4, 2, 0, 0)), peak), AipsError);
      AlwaysAssert(!near(image(IPosition(4, 1, 6, 0, 0)), peak/2), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 17, 0, 0)), 1E5f, 1E-3), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 17, 3, 0)), 1E4f, 1E-7), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 18, 0, 0)), 5E4f, 1E-2), AipsError);
      AlwaysAssert(near(image(IPosition(4, 23, 18, 2, 0)), 1E4f, 1E-4), AipsError);
      
      cout << "Passed the sampling & projection tests" << endl;
    }
    {
      // Create a model. This is just a copy of the first part of this code.
      //####################################################################
      // There is a serious problem in the ComponentList class that can be
      // demonstrated by swapping the next two lines. What happens when they
      // are swapped is that the defaultGauss object is destroyed by the
      // GaussianComponent destructor, which is called before the destructor
      // for the ComponentList. So the destructor for the ComponentList
      // (which tries to save the component to disk), is left with a null
      // pointer, and no way to save the destroyed component to disk.
      // One way to solve this problem is to use copy semantics. 
      //####################################################################
      GaussianComponent defaultGauss;
      ComponentList model;

      model.addComp(defaultGauss);
      model.addComp(new PointComponent(StokesVector(2.0f,0.0f,0.1f,0.1f), 
				       MDirection(Quantity(10, "deg"),
						  Quantity(87, "deg"), 
						  MDirection::J2000)));
      CountedPtr<SkyComponent> countedPtr(new GaussianComponent());
      model.addComp(countedPtr);
      GaussianComponent * 
	ptrGauss = new GaussianComponent(StokesVector(4.0f,2.0f,0.1f,0.1f), 
					 MDirection(Quantity(30, "deg"),
						    Quantity(89, "deg"),
						    MDirection::J2000),
					 MVAngle(Quantity(0.5, "deg")),
					 0.5f,
					 MVAngle(Quantity(60, "deg")));
      model.insertComp(ptrGauss);
      defaultGauss.setFlux(StokesVector(5.0f, 1.0f, 0.1f, 0.05f));
      
      model.setListName("tComponentList_tmp.table");
      
    }
    { 
      ComponentList model("tComponentList_tmp.table", True);
      model.setListName("tComponentListCopy_tmp.table");
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
// compile-command: "gmake OPTLIB=1 tComponentList"
// End: 
