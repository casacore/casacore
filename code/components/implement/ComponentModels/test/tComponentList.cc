//# tComponentList.cc:  this defines tComponentList.cc
//# Copyright (C) 1996,1997,1998,1999,2000
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
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <trial/ComponentModels/Flux.h>
// #include <trial/Coordinates/CoordinateUtil.h>
// #include <trial/Coordinates/CoordinateSystem.h>
// #include <trial/Coordinates/DirectionCoordinate.h>
// #include <trial/Images/PagedImage.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
// #include <aips/Arrays/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

int main() {
  try {
    {
      // Test all the constructors and ways of putting data into the lists.
      ComponentList model;
      SkyComponent defaultGauss(ComponentType::GAUSSIAN);
      model.add(defaultGauss);
      {
 	SkyComponent otherPoint(ComponentType::POINT);
	otherPoint.flux() = Flux<Double>(2.0, 0.0, 0.1, 0.01);
	otherPoint.shape().setRefDirection(MDirection(Quantity(10, "deg"),
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
 	  SkyCompGauss.shape().setRefDirection(newDirection);
 	}
 	{
 	  Vector<Double> newParameters(3);
 	  newParameters(0) = Quantity(30, "'").getValue("rad"); 
 	  newParameters(1) = Quantity(15, "'").getValue("rad");
 	  newParameters(2) = Quantity(60, "deg").getValue("rad");
 	  SkyCompGauss.shape().parameters(newParameters);
 	}
      }
      {
 	model.add(SkyComponent());
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
      AlwaysAssert(model.component(1).shape().type() 
		   == ComponentType::GAUSSIAN, AipsError);
//       GaussianComponent gaussComp(model.component(1));
//       Double ratio;
//       gaussComp.axialRatio(ratio);
//       AlwaysAssert(near(ratio, 0.5), AipsError);
//       cout << "Passed the constructors, insertion and deletion tests" 
//  	   << endl;
    }
    {
      ComponentList crux;
      MDirection sampleDirection1, sampleDirection2, sampleDirection3;
      {
  	SkyComponent alpha1Crux(ComponentType::POINT);
	alpha1Crux.flux() = Flux<Double>(0.235, 0.0, 0.0, 0.235);
  	const MDirection direction((MVAngle(Quantity(12, "h")).get()
				    + MVAngle(Quantity(26, "min")).get() 
				    + MVAngle(Quantity(35.9, "s")).get()), 
				   (Quantity(-63, "deg") 
				    - Quantity(5, "'")
				    - Quantity(56, "''")),
				   MDirection::J2000);
	alpha1Crux.shape().setRefDirection(direction);
 	alpha1Crux.label() = "Alpha1 Crux";
  	crux.add(alpha1Crux);
      }
      {
  	SkyComponent alpha2Crux(ComponentType::POINT);
  	alpha2Crux.flux() = Flux<Double>(0.147, 0.147, 0.0, 0.0);
  	const MDirection direction((MVAngle(Quantity(12, "h")).get()
				    + MVAngle(Quantity(26, "min")).get()
				    + MVAngle(Quantity(36.5, "s")).get()), 
				   (Quantity(-63, "deg") 
				    - Quantity(5, "'")
				    - Quantity(58, "''")),
				   MDirection::J2000);
	alpha2Crux.shape().setRefDirection(direction);
 	alpha2Crux.label() = "Alpha2 Crux";
  	crux.add(alpha2Crux);
      }
      {
  	SkyComponent betaCrux(ComponentType::POINT);
  	betaCrux.flux() = Flux<Double>(0.318, 0.0, 0.318, 0.0);
  	const MDirection direction((MVAngle(Quantity(12, "h")).get()
				    + MVAngle(Quantity(47, "min")).get()
				    + MVAngle(Quantity(43.3, "s")).get()), 
				   (Quantity(-59, "deg")
				    - Quantity(41, "'")
				    - Quantity(19, "''")),
				   MDirection::J2000);
	betaCrux.shape().setRefDirection(direction);
 	betaCrux.label() = "Beta Crux";
  	crux.add(betaCrux);
      }
      {
	SkyComponent gammaCrux(ComponentType::GAUSSIAN);
 	gammaCrux.flux() = Flux<Double>(.225);
 	const MDirection direction((MVAngle(Quantity(12, "h")).get()
				    + MVAngle(Quantity(31, "min")).get()
				    + MVAngle(Quantity(16.7, "s")).get()), 
				   (Quantity(-57, "deg")
				    - Quantity(4, "'")
				    - Quantity(51, "''")),
				   MDirection::J2000);
	gammaCrux.shape().setRefDirection(direction);
 	//	const MVAngle width(Quantity(35, "mas"));
 	const MVAngle width(Quantity(10, "'"));
 	const MVAngle pa(Quantity(0, "deg"));
 	Vector<Double> shapeParms(gammaCrux.shape().nParameters());
 	shapeParms(0) = shapeParms(1) = width.radian();
 	shapeParms(2) = pa.radian();
 	gammaCrux.shape().parameters(shapeParms);
	gammaCrux.label() = "Gamma Crux";
	crux.add(gammaCrux);
	sampleDirection1 = direction;
      }
      {
 	SkyComponent deltaCrux(ComponentType::POINT,
			       ComponentType::SPECTRAL_INDEX);
 	const Flux<Double> flux(0.0769);
 	deltaCrux.flux() = flux;
 	const MDirection direction((MVAngle(Quantity(12, "h")).get()
				    + MVAngle(Quantity(15, "min")).get()
				    + MVAngle(Quantity(8.6, "s")).get()), 
				   (Quantity(-58, "deg")
				    - Quantity(44, "'")
				    - Quantity(56, "''")),
				   MDirection::J2000);
 	deltaCrux.shape().setRefDirection(direction);
	deltaCrux.spectrum().setRefFrequency(MFrequency(Quantity(1, "GHz")));
	Vector<Double> specParms(1, 0.5);
	deltaCrux.spectrum().parameters(specParms);
	deltaCrux.label() = "Delta Crux";
 	crux.add(deltaCrux);
 	sampleDirection2 = direction;
      }
      {
	SkyComponent epsilonCrux(ComponentType::POINT);
 	epsilonCrux.flux() = Flux<Double>(0.0373);
 	const MDirection direction((MVAngle(Quantity(12, "h")).get()
				    + MVAngle(Quantity(21, "min")).get()
				    + MVAngle(Quantity(21.5, "s")).get()),
				   (Quantity(-60, "deg")
				    - Quantity(24, "'")
				    - Quantity(4, "''")),
				   MDirection::J2000);
 	epsilonCrux.shape().setRefDirection(direction);
	epsilonCrux.label() = "Epsilon Crux";
 	crux.add(epsilonCrux);
 	sampleDirection3 = direction;
      }
      const MVAngle pixelSize(Quantity(1.0, "'"));
      const MVAngle halfSize(pixelSize.radian()/2.0);
      Vector<Double> sampledFlux(4);
      const MFrequency defaultFreq;
      crux.sample(sampleDirection1, pixelSize, pixelSize, defaultFreq)
	.value(sampledFlux);
      AlwaysAssert(near(sampledFlux(0), 0.1985720, 1E-6), AipsError);
      crux.sample(sampleDirection1, pixelSize, halfSize, defaultFreq)
	.value(sampledFlux);
      AlwaysAssert(near(sampledFlux(0), 0.099286, 1E-6), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, C::dbl_epsilon), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);
      crux.sample(sampleDirection2, pixelSize, pixelSize, defaultFreq)
	.value(sampledFlux);;
      AlwaysAssert(near(sampledFlux(0), 0.0769, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);
      crux.sample(sampleDirection3, pixelSize, halfSize, defaultFreq)
	.value(sampledFlux);
      AlwaysAssert(near(sampledFlux(0), 0.0373, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);

//       const uInt imSize = 84;
//       const uInt nPol = 4;
//       const uInt nFreq = 1;
//       CoordinateSystem imCoords;
//       {
//  	DirectionCoordinate dirCoord(CoordinateUtil::defaultCoords2D()
// 				     .directionCoordinate(0));
//  	Vector<String> units(2); units = "deg";
//   	dirCoord.setWorldAxisUnits(units);
//   	Vector<Double> refValue(2);
//   	refValue(0) = 12.5/24*360.0;
//   	refValue(1) = -60.0;
//   	dirCoord.setReferenceValue(refValue);
//   	Vector<Double> inc(2); inc = .1;
//   	dirCoord.setIncrement(inc);
//   	Vector<Double> refPixel(2);
//   	refPixel = Double(imSize+1)/2.0;
//   	dirCoord.setReferencePixel(refPixel);
//  	imCoords.addCoordinate(dirCoord);
//       }
//       CoordinateUtil::addIQUVAxis(imCoords);
//       CoordinateUtil::addFreqAxis(imCoords);
//       PagedImage<Float> image(IPosition(4,imSize,imSize,nPol,nFreq), 
//   			      imCoords, "tComponentList_tmp.image");
//       image.set(0.0f);
//       crux.project(image);
//       AlwaysAssert(near(image(IPosition(4, 39, 12, 0, 0)), 0.235f, 1E-5), 
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 39, 12, 1, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 39, 12, 2, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 39, 12, 3, 0)), 0.235f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 39, 11, 0, 0)), 0.147f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 39, 11, 1, 0)), 0.147f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 39, 11, 2, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 39, 11, 3, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 65, 45, 0, 0)), 0.318f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 65, 45, 1, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 65, 45, 2, 0)), 0.318f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 65, 45, 3, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 9, 11, 0, 0)), 0.0f, 1E-5), 
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 9, 11, 1, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 9, 11, 2, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 9, 11, 3, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(!near(image(IPosition(4, 44, 73, 0, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 44, 73, 1, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 44, 73, 2, 0)), 0.0f, 1E-5),
// 		   AipsError);
//       AlwaysAssert(near(image(IPosition(4, 44, 73, 3, 0)), 0.0f, 1E-5),
// 		   AipsError);

       cout << "Passed the sampling & projection tests" << endl;
       crux.rename("tComponentList_tmp.model", Table::New);
       //       image.table().markForDelete();
    }
     {
       // Create a model by reading the previous one from disk
       ComponentList model("tComponentList_tmp.model");
       AlwaysAssert(model.nelements() == 6, AipsError);
       AlwaysAssert(model.component(2).shape().type() == 
		    ComponentType::POINT, AipsError);
       AlwaysAssert(model.component(3).shape().type() == 
		    ComponentType::GAUSSIAN, AipsError);
       model.rename("tComponentList_tmp_renamed.model");
       model.component(1).flux().convertUnit("WU");
       model.component(2).flux().convertUnit("WU");
       model.component(3).flux().convertUnit("WU");
       model.component(0).label() = "Component 1";
       model.component(2).flux().convertPol(ComponentType::LINEAR);
       model.component(3).flux().convertPol(ComponentType::CIRCULAR);
     }
    {
      // Check that the Table has been renamed and not copied
      AlwaysAssert(Table::isWritable("tComponentList_tmp.model") == False,
  		   AipsError);
      AlwaysAssert(Table::isReadable("tComponentList_tmp.model") == False,
  		   AipsError);

      // Open (readonly) the renamed model and copy it so we can mess with.
      const ComponentList model("tComponentList_tmp_renamed.model", True);
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).shape().type() == ComponentType::POINT,
  		   AipsError);
      AlwaysAssert(model.component(3).shape().type() == 
		   ComponentType::GAUSSIAN, AipsError);
      ComponentList RO_Model(model);
      try {
  	RO_Model.remove(0);
      } catch (AipsError x) {
 	if (!x.getMesg().contains("ROFlag == False")) {
 	  cout << x.getMesg() << endl << "FAIL" << endl;
 	  return 1;
  	}
      }
      ComponentList RW_Model(model.copy());
      Flux<Double> newFlux(1.0);
      RW_Model.component(1).flux() = newFlux;
      // I cannot use       
      // RO_Model.component(1).flux(); here because the component(i)
      // function uses the non-const version which checks the RO flag. So I
      // recommend if opening a Model file readonly to always use
      // const ComponentList model(...)
      AlwaysAssert(allNear(RW_Model.component(1).flux().value(), 
			   newFlux.value(), C::dbl_epsilon),
 		   AipsError);
      AlwaysAssert(!allNear(model.component(1).flux().value(), 
			    newFlux.value(), C::dbl_epsilon),
 		   AipsError);
      RW_Model.remove(0);
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).shape().type() == ComponentType::POINT, 
  		   AipsError);
      AlwaysAssert(model.component(3).shape().type() == 
		   ComponentType::GAUSSIAN, AipsError);
      AlwaysAssert(RW_Model.nelements() == 5, AipsError);
      AlwaysAssert(RW_Model.component(1).shape().type() == 
		   ComponentType::POINT, AipsError);
      AlwaysAssert(RW_Model.component(2).shape().type() == 
		   ComponentType::GAUSSIAN, AipsError);
      RW_Model.rename("tComponentList_tmp_copied.model");
      cout << "Passed the Table reading/writing tests" << endl;
    }
    {
      ComponentList original("tComponentList_tmp_renamed.model");
      ComponentList modified("tComponentList_tmp_copied.model", False);
      AlwaysAssert(original.nelements() == 6, AipsError);
      AlwaysAssert(original.component(2).shape().type() == 
		   ComponentType::POINT, AipsError);
      AlwaysAssert(original.component(3).shape().type() == 
		   ComponentType::GAUSSIAN, AipsError);
      AlwaysAssert(modified.nelements() == 5, AipsError);
      AlwaysAssert(modified.component(1).shape().type() == 
		   ComponentType::POINT, AipsError);
      AlwaysAssert(modified.component(2).shape().type() == 
		   ComponentType::GAUSSIAN, AipsError);
      Flux<Double> expectedFlux(1.0);
      AlwaysAssert(!allNear(original.component(0).flux().value(),
			    expectedFlux.value(), C::dbl_epsilon), 
		   AipsError);
      AlwaysAssert(!allNear(original.component(1).flux().value(),
			    expectedFlux.value(), C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(allNear(modified.component(0).flux().value(),
			   expectedFlux.value(), C::dbl_epsilon),
		   AipsError);
      ComponentList newList(modified);
      newList.remove(0);
      expectedFlux.setValue(2.0);
      newList.component(0).flux() = expectedFlux;
      AlwaysAssert(!allNear(modified.component(0).flux().value(), 
			    expectedFlux.value(), C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(allNear(modified.component(1).flux().value(),
			   expectedFlux.value(), C::dbl_epsilon),
		   AipsError);
      newList = original;
      AlwaysAssert(newList.nelements() == 6, AipsError);
      newList.remove(0);
      AlwaysAssert(original.nelements() == 6, AipsError);
      AlwaysAssert(newList.nelements() == 5, AipsError);
      newList.component(0).flux() = expectedFlux;
      AlwaysAssert(!allNear(original.component(0).flux().value(),
			    expectedFlux.value(), C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(allNear(original.component(1).flux().value(),
			   expectedFlux.value(), C::dbl_epsilon),
		   AipsError);
      cout << "Passed the copy and assignment tests" << endl;
      //      original.rename("junk.model", Table::Scratch);
      // modified.rename("more_junk.model", Table::Scratch);
    }
  } 
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tComponentList"
// End: 
