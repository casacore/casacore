//# tComponentList.cc:  this defines tComponentList.cc
//# Copyright (C) 1996,1997,1998,1999
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
#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/GaussianShape.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Images/PagedImage.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


int main() {
  try {

/*
    {
      // Test all the constructors and ways of putting data into the lists.
      ComponentList model;
      GaussianShape defaultGauss;
      model.add(defaultGauss);
      {
	Flux<Double> flux(2.0, 0.0, 0.1, 0.01);
	PointShape otherPoint(flux, MDirection(Quantity(10, "deg"),
						   Quantity(87,"deg"),
						   MDirection::J2000));
	model.add(otherPoint);
      }
      {
	SkyComponent SkyCompGauss(ComponentType::GAUSSIAN);
	model.add(SkyCompGauss);
	{
	  Flux<Double> flux(4.0, 2.0, 0.1, 0.1);
	  SkyCompGauss.flux() = flux;
	}
	{
	  MDirection newDirection(Quantity(30, "deg"), Quantity(89, "deg"),
				  MDirection::J2000);
	  SkyCompGauss.setDirection(newDirection);
	}
	{
	  Vector<Double> newParameters(3);
	  newParameters(0) = Quantity(30, "'").getValue("rad"); 
	  newParameters(1) = Quantity(15, "'").getValue("rad");
	  newParameters(2) = Quantity(60, "deg").getValue("rad");
	  SkyCompGauss.setParameters(newParameters);
	}
      }
      {
	SkyComponent SkyCompPoint;
	model.add(SkyCompPoint);
      }
      // reset the flux of the first component. Reference semantics means
      // this changes the value in the list also.
      {
	Flux<Double> flux(5.0, 1.0, 0.1, 0.05);
	defaultGauss.flux() = flux;
      }
      // So the list should now contain 4 elements with I fluxes of
      // 5, 2, 4, 1
      AlwaysAssert(model.nelements() == 4, AipsError);
      AlwaysAssert(near(model.component(0).flux().value(0), 5.0), AipsError);
      AlwaysAssert(near(model.component(1).flux().value(0), 2.0), AipsError);
      AlwaysAssert(near(model.component(2).flux().value(0), 4.0), AipsError);
      AlwaysAssert(near(model.component(3).flux().value(0), 1.0), AipsError);
      // Try my hardest to induce a memory leak by extracting a component,
      // deleting it from the list and adding it to another list.
      ComponentList model1;
      model1.add(model.component(0));
      AlwaysAssert(model1.nelements() == 1, AipsError);
      model.remove(0);
      AlwaysAssert(model.nelements() == 3, AipsError);
      model.remove(2);
      AlwaysAssert(model.nelements() == 2, AipsError);
      AlwaysAssert(near(model.component(0).flux().value(0), 2.0), AipsError);
      AlwaysAssert(near(model.component(1).flux().value(0), 4.0), AipsError);
      AlwaysAssert(near(model1.component(0).flux().value(0), 5.0), AipsError);
      
      AlwaysAssert(model.component(1).shape() == ComponentType::GAUSSIAN,
 		   AipsError);
      GaussianShape gaussComp(model.component(1));
      Double ratio;
      gaussComp.axialRatio(ratio);
      AlwaysAssert(near(ratio, 0.5), AipsError);
      cout << "Passed the constructors, insertion and deletion tests" 
 	   << endl;
    }
    {
      ComponentList crux;
      MDirection sampleDirection1, sampleDirection2, sampleDirection3;
      {
  	Flux<Double> flux(0.235, 0.0, 0.0, 0.235);
 	MDirection direction(Quantity(12.0/24*360, "deg")
			     + Quantity(26.0/24*360, "'") 
			     + Quantity(35.9/24*360, "''"), 
			     Quantity(-63, "deg") 
			     - Quantity(5, "'")
			     - Quantity(56, "''"), MDirection::J2000);
 	PointShape alpha1Crux(flux, direction);
 	crux.add(alpha1Crux);
      }
      {
 	Flux<Double> flux(0.147, 0.147, 0.0, 0.0);
 	MDirection direction(Quantity(12.0/24*360, "deg")
			     + Quantity(26.0/24*360, "'")
			     + Quantity(36.5/24*360, "''"), 
			     Quantity(-63, "deg") 
			     - Quantity(5, "'")
			     - Quantity(58, "''"), MDirection::J2000);
 	PointShape alpha2Crux(flux, direction);
 	crux.add(alpha2Crux);
      }
      {
 	Flux<Double> flux(0.318, 0.0, 0.318, 0.0);
 	MDirection direction(Quantity(12.0/24*360, "deg")
			     + Quantity(47.0/24*360, "'")
			     + Quantity(43.3/24*360, "''"), 
			     Quantity(-59, "deg")
			     - Quantity(41, "'")
			     - Quantity(19, "''"), MDirection::J2000);
 	PointShape betaCrux(flux, direction);
 	crux.add(betaCrux);
      }
      {
 	Flux<Double> flux(.225);
 	MDirection direction(Quantity(12.0/24*360, "deg")
			     + Quantity(31.0/24*360, "'")
			     + Quantity(16.7/24*360, "''"), 
			     Quantity(-57, "deg")
			     - Quantity(4, "'")
			     - Quantity(51, "''"), MDirection::J2000);
 	//	MVAngle width(Quantity(35, "mas"));
 	MVAngle width(Quantity(10, "'"));
 	MVAngle pa(Quantity(0, "deg"));
 	GaussianShape gammaCrux(flux, direction, width, 1.0, pa);
	crux.add(gammaCrux);
	sampleDirection1 = direction;
      }
      {
 	Flux<Double> flux(0.0769);
 	MDirection direction(Quantity(12.0/24*360, "deg")
			     + Quantity(15.0/24*360, "'")
			     + Quantity(8.6/24*360, "''"), 
			     Quantity(-58, "deg")
			     - Quantity(44, "'")
			     - Quantity(56, "''"), MDirection::J2000);
 	PointShape deltaCrux(flux, direction);
 	crux.add(deltaCrux);
 	sampleDirection2 = direction;
      }
      {
 	Flux<Double> flux(0.0373);
 	MDirection direction(Quantity(12.0/24*360, "deg")
			     + Quantity(21.0/24*360, "'")
			     + Quantity(21.5/24*360, "''"),
			     Quantity(-60, "deg")
			     - Quantity(24, "'")
			     - Quantity(4, "''"), MDirection::J2000);
 	PointShape epsilonCrux(flux, direction);
 	crux.add(epsilonCrux);
 	sampleDirection3 = direction;
      }
      const MVAngle pixelSize(Quantity(1.0, "'"));
      Vector<Double> sampledFlux(4);
      crux.sample(sampledFlux, sampleDirection1, pixelSize);
      AlwaysAssert(!near(sampledFlux(0), 0.0, 1E-2), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, C::dbl_epsilon), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);
      crux.sample(sampledFlux, sampleDirection2, pixelSize);
      AlwaysAssert(near(sampledFlux(0), 0.0769, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);
      crux.sample(sampledFlux, sampleDirection3, pixelSize);
      AlwaysAssert(near(sampledFlux(0), 0.0373, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);

      const uInt imSize = 84;
      const uInt nPol = 4;
      const uInt nFreq = 1;
      CoordinateSystem imCoords;
      {
 	DirectionCoordinate dirCoord(CoordinateUtil::defaultCoords2D()
				     .directionCoordinate(0));
 	Vector<String> units(2); units = "deg";
  	dirCoord.setWorldAxisUnits(units);
  	Vector<Double> refValue(2);
  	refValue(0) = 12.5/24*360.0;
  	refValue(1) = -60.0;
  	dirCoord.setReferenceValue(refValue);
  	Vector<Double> inc(2); inc = .1;
  	dirCoord.setIncrement(inc);
  	Vector<Double> refPixel(2);
  	refPixel = Double(imSize+1)/2.0;
  	dirCoord.setReferencePixel(refPixel);
 	imCoords.addCoordinate(dirCoord);
      }
      CoordinateUtil::addIQUVAxis(imCoords);
      CoordinateUtil::addFreqAxis(imCoords);
      PagedImage<Float> image(IPosition(4,imSize,imSize,nPol,nFreq), 
  			      imCoords, "tComponentList_tmp.image");
      image.set(0.0f);
      crux.project(image);
      AlwaysAssert(near(image(IPosition(4, 39, 12, 0, 0)), 0.235f, 1E-5), 
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 12, 1, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 12, 2, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 12, 3, 0)), 0.235f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 0, 0)), 0.147f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 1, 0)), 0.147f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 2, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 3, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 0, 0)), 0.318f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 1, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 2, 0)), 0.318f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 3, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 0, 0)), 0.0f, 1E-5), 
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 1, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 2, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 3, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(!near(image(IPosition(4, 44, 73, 0, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 44, 73, 1, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 44, 73, 2, 0)), 0.0f, 1E-5),
		   AipsError);
      AlwaysAssert(near(image(IPosition(4, 44, 73, 3, 0)), 0.0f, 1E-5),
		   AipsError);

      cout << "Passed the sampling & projection tests" << endl;
      crux.rename("tComponentList_tmp.model", Table::New);
      image.table().markForDelete();
    }
    {
      // Create a model by reading the previous one from disk
      ComponentList model("tComponentList_tmp.model");
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).shape() == ComponentType::POINT,
		   AipsError);
      AlwaysAssert(model.component(3).shape() == ComponentType::GAUSSIAN,
		   AipsError);
      model.rename("tComponentList_tmp.modelRenamed");
    }
    {
      // Check that the Table has been renamed and not copied
      AlwaysAssert(Table::isWritable("tComponentList_tmp.model") == False,
 		   AipsError);
      AlwaysAssert(Table::isReadable("tComponentList_tmp.model") == False,
 		   AipsError);

      // Open (readonly) the renamed model and copy it so we can mess with.
      const ComponentList model("tComponentList_tmp.modelRenamed", True);
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).shape() == ComponentType::POINT,
 		   AipsError);
      AlwaysAssert(model.component(3).shape() == ComponentType::GAUSSIAN, 
 		   AipsError);
      ComponentList RO_Model(model);
      try {
 	RO_Model.remove(0);
      } catch (AipsError x) {
	if (!x.getMesg().contains("ROFlag == False")) {
	  cout << x.getMesg() << endl << "FAIL" << endl;
	  return 1;
 	}
      } end_try;
      ComponentList RW_Model(model.copy());
      Flux<Double> newFlux(1.0);
      RW_Model.component(1).flux() = newFlux;
      // I cannot use       
      // RO_Model.component(1).flux(ROflux); here because the component(i)
      // function uses the non-const version which checks the RO flag. So I
      // recommend if opening a Model file readonly to always use
      // const ComponentList model(...)
      AlwaysAssert(allNear(RW_Model.component(1).flux().value().ac(), 
			   newFlux.value().ac(), C::dbl_epsilon),
 		   AipsError);
      AlwaysAssert(!allNear(model.component(1).flux().value().ac(), 
			    newFlux.value().ac(), C::dbl_epsilon),
 		   AipsError);
      RW_Model.remove(0);
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).shape() == ComponentType::POINT, 
  		   AipsError);
      AlwaysAssert(model.component(3).shape() == ComponentType::GAUSSIAN, 
  		   AipsError);
      AlwaysAssert(RW_Model.nelements() == 5, AipsError);
      AlwaysAssert(RW_Model.component(1).shape() == ComponentType::POINT, 
 		   AipsError);
      AlwaysAssert(RW_Model.component(2).shape() == ComponentType::GAUSSIAN,
   		   AipsError);
      RW_Model.rename("tComponentList_tmp.modelCopied");
      cout << "Passed the Table reading/writing tests" << endl;
    }
    {
      ComponentList original("tComponentList_tmp.modelRenamed");
      ComponentList modified("tComponentList_tmp.modelCopied", False);
      AlwaysAssert(original.nelements() == 6, AipsError);
      AlwaysAssert(original.component(2).shape() == ComponentType::POINT, 
  		   AipsError);
      AlwaysAssert(original.component(3).shape() == ComponentType::GAUSSIAN, 
  		   AipsError);
      AlwaysAssert(modified.nelements() == 5, AipsError);
      AlwaysAssert(modified.component(1).shape() == ComponentType::POINT, 
 		   AipsError);
      AlwaysAssert(modified.component(2).shape() == ComponentType::GAUSSIAN,
   		   AipsError);
      Flux<Double> expectedFlux(1.0);
      AlwaysAssert(!allNear(original.component(0).flux().value().ac(),
			    expectedFlux.value().ac(), C::dbl_epsilon), 
		   AipsError);
      AlwaysAssert(!allNear(original.component(1).flux().value().ac(),
			    expectedFlux.value().ac(), C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(allNear(modified.component(0).flux().value().ac(),
			   expectedFlux.value().ac(), C::dbl_epsilon),
		   AipsError);
      ComponentList newList(modified);
      newList.remove(0);
      expectedFlux.setValue(2.0);
      newList.component(0).flux() = expectedFlux;
      AlwaysAssert(!allNear(modified.component(0).flux().value().ac(), 
			    expectedFlux.value().ac(), C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(allNear(modified.component(1).flux().value().ac(),
			   expectedFlux.value().ac(), C::dbl_epsilon),
		   AipsError);
      newList = original;
      AlwaysAssert(newList.nelements() == 6, AipsError);
      newList.remove(0);
      AlwaysAssert(original.nelements() == 6, AipsError);
      AlwaysAssert(newList.nelements() == 5, AipsError);
      newList.component(0).flux() = expectedFlux;
      AlwaysAssert(!allNear(original.component(0).flux().value().ac(),
			    expectedFlux.value().ac(), C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(allNear(original.component(1).flux().value().ac(),
			   expectedFlux.value().ac(), C::dbl_epsilon),
		   AipsError);
      cout << "Passed the copy and assignment tests" << endl;
      original.rename("junk.model", Table::Scratch);
      modified.rename("more_junk.model", Table::Scratch);
    }

*/
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
