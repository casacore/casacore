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
#include <trial/ComponentModels/SkyComponent.h>
#include <trial/ComponentModels/ComponentType.h>
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
#include <aips/Measures/Quantum.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Utilities/Assert.h>
#include <iostream.h>


int main() {
  try {
    {
      // Test all the constructors and ways of putting data into the lists.
      ComponentList model;
      GaussianComponent defaultGauss;
      model.add(defaultGauss);
      {
	Vector<Double> pFlux(4);
	pFlux(0) = 2.0; pFlux(1) = 0.0; pFlux(2) = 0.1; pFlux(3) = 0.01;
	PointComponent otherPoint(pFlux, MDirection(Quantity(10, "deg"),
						    Quantity(87,"deg"),
						    MDirection::J2000));
	model.add(otherPoint);
      }
      {
	SkyComponent SkyCompGauss(ComponentType::GAUSSIAN);
	model.add(SkyCompGauss);
	{
	  Vector<Double> newFlux(4);
	  newFlux(0) = 4.0; newFlux(1) = 2.0;
	  newFlux(2) = 0.1; newFlux(3) = 0.1;
	  SkyCompGauss.setFlux(newFlux);
	}
	{
	  MDirection newPosition(Quantity(30, "deg"), Quantity(89, "deg"),
				 MDirection::J2000);
	  SkyCompGauss.setPosition(newPosition);
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
	Vector<Double> newFlux(4);
	newFlux(0) = 5.0; newFlux(1) = 1.0;
	newFlux(2) = 0.1; newFlux(3) = 0.05;
	defaultGauss.setFlux(newFlux);
      }
      
//       {
// 	cout << "List has " << model.nelements() << " elements" << endl;
// 	Vector<Double> flux(4);
// 	MDirection position;
// 	for (uInt n = 0; n < model.nelements(); n++){
// 	  SkyComponent tmpComp = model.component(n);
// 	  tmpComp.flux(flux);
// 	  tmpComp.position(position);
// 	  cout << n << ": " << ComponentType::name(tmpComp.type())
// 	       << " Flux:" << flux 
// 	       << position
// 	       << endl;
// 	}
//       }
      // So the list should now contain 4 elements with I fluxes of
      // 5, 2, 4, 1
      AlwaysAssert(model.nelements() == 4, AipsError);
      Vector<Double> compFlux(4);
      model.component(0).flux(compFlux);
      AlwaysAssert(near(compFlux(0), 5.0), AipsError);
      model.component(1).flux(compFlux);
      AlwaysAssert(near(compFlux(0), 2.0), AipsError);
      model.component(2).flux(compFlux);
      AlwaysAssert(near(compFlux(0), 4.0), AipsError);
      model.component(3).flux(compFlux);
      AlwaysAssert(near(compFlux(0), 1.0), AipsError);
      // Try my hardest to induce a memory leak by extracting a component,
      // deleting it from the list and adding it to another list.

      ComponentList model1;
      model1.add(model.component(0));
      AlwaysAssert(model1.nelements() == 1, AipsError);
      model.remove(0);
      AlwaysAssert(model.nelements() == 3, AipsError);
      model.remove(2);
      AlwaysAssert(model.nelements() == 2, AipsError);
      model.component(0).flux(compFlux);
      AlwaysAssert(near(compFlux(0), 2.0), AipsError);
      model.component(1).flux(compFlux);
      AlwaysAssert(near(compFlux(0), 4.0), AipsError);
      model1.component(0).flux(compFlux);
      AlwaysAssert(near(compFlux(0), 5.0), AipsError);
      
      AlwaysAssert(model.component(1).type() == ComponentType::GAUSSIAN,
		   AipsError);
      GaussianComponent gaussComp(model.component(1));
      Double ratio;
      gaussComp.axialRatio(ratio);
      AlwaysAssert(near(ratio, 0.5), AipsError);
      cout << "Passed the constructors, insertion and deletion tests" 
	   << endl;
    }
    {
      ComponentList crux;
      MDirection samplePosition1, samplePosition2, samplePosition3;
      {
 	Vector<Double> flux(4);
	flux = 0.0; flux(0) = 0.235; flux(3) = flux(0);
	MDirection position(Quantity(12.0/24*360, "deg")
 			    + Quantity(26.0/24*360, "'") 
			    + Quantity(35.9/24*360, "''"), 
			    Quantity(-63, "deg") 
			    - Quantity(5, "'")
			    - Quantity(56, "''"), MDirection::J2000);
	PointComponent alpha1Crux(flux, position);
	crux.add(alpha1Crux);
      }
      {
	Vector<Double> flux(4);
	flux = 0.0; flux(0) = 0.147; flux(1) = flux(0);
	MDirection position(Quantity(12.0/24*360, "deg")
			    + Quantity(26.0/24*360, "'")
			    + Quantity(36.5/24*360, "''"), 
			    Quantity(-63, "deg") 
			    - Quantity(5, "'")
			    - Quantity(58, "''"), MDirection::J2000);
	PointComponent alpha2Crux(flux, position);
	crux.add(alpha2Crux);
      }
      {
	Vector<Double> flux(4);
	flux = 0.0; flux(0) = 0.318; flux(2) = flux(0);
	MDirection position(Quantity(12.0/24*360, "deg")
			    + Quantity(47.0/24*360, "'")
			    + Quantity(43.3/24*360, "''"), 
			    Quantity(-59, "deg")
			    - Quantity(41, "'")
			    - Quantity(19, "''"), MDirection::J2000);
	PointComponent betaCrux(flux, position);
	crux.add(betaCrux);
      }
      {
	Vector<Double> flux(4);
	flux = 0.0; flux(0) = 0.225;
	MDirection position(Quantity(12.0/24*360, "deg")
			    + Quantity(31.0/24*360, "'")
			    + Quantity(16.7/24*360, "''"), 
			    Quantity(-57, "deg")
			    - Quantity(4, "'")
			    - Quantity(51, "''"), MDirection::J2000);
	//	MVAngle width(Quantity(35, "mas"));
	MVAngle width(Quantity(10, "'"));
	MVAngle pa(Quantity(0, "deg"));
	GaussianComponent gammaCrux(flux, position, width, 1.0, pa);
	crux.add(gammaCrux);
	samplePosition1 = position;
      }
      {
	Vector<Double> flux(4);
	flux = 0.0; flux(0) = 0.0769;
	MDirection position(Quantity(12.0/24*360, "deg")
			    + Quantity(15.0/24*360, "'")
			    + Quantity(8.6/24*360, "''"), 
			    Quantity(-58, "deg")
			    - Quantity(44, "'")
			    - Quantity(56, "''"), MDirection::J2000);
	PointComponent deltaCrux(flux, position);
	crux.add(deltaCrux);
	samplePosition2 = position;
      }
      {
	Vector<Double> flux(4);
	flux = 0.0; flux(0) = 0.0373;
	MDirection position(Quantity(12.0/24*360, "deg")
			    + Quantity(21.0/24*360, "'")
			    + Quantity(21.5/24*360, "''"),
			    Quantity(-60, "deg")
			    - Quantity(24, "'")
			    - Quantity(4, "''"), MDirection::J2000);
	PointComponent epsilonCrux(flux, position);
	crux.add(epsilonCrux);
	samplePosition3 = position;
      }
      const MVAngle pixelSize(Quantity(1.0, "'"));
      Vector<Double> sampledFlux(4);
      crux.sample(sampledFlux, samplePosition1, pixelSize);
      AlwaysAssert(!near(sampledFlux(0), 0.0, 1E-2), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);
      crux.sample(sampledFlux, samplePosition2, pixelSize);
      AlwaysAssert(near(sampledFlux(0), 0.0769, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);
      crux.sample(sampledFlux, samplePosition3, pixelSize);
      AlwaysAssert(near(sampledFlux(0), 0.0373, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(1), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(2), 0.0, 1E-10), AipsError);
      AlwaysAssert(near(sampledFlux(3), 0.0, 1E-10), AipsError);

      const uInt imSize = 84;
      const uInt nPol = 4;
      const uInt nFreq = 1;
      CoordinateSystem imCoords;
      {
 	DirectionCoordinate dirCoord(CoordinateUtil::defaultCoords2D().directionCoordinate(0));
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
      AlwaysAssert(near(image(IPosition(4, 39, 12, 0, 0)), 0.235f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 12, 1, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 12, 2, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 12, 3, 0)), 0.235f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 0, 0)), 0.147f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 1, 0)), 0.147f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 2, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 39, 11, 3, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 0, 0)), 0.318f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 1, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 2, 0)), 0.318f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 65, 45, 3, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 0, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 1, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 2, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 9, 11, 3, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(!near(image(IPosition(4, 44, 73, 0, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 44, 73, 1, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 44, 73, 2, 0)), 0.0f, 1E-5), AipsError);
      AlwaysAssert(near(image(IPosition(4, 44, 73, 3, 0)), 0.0f, 1E-5), AipsError);
   
      cout << "Passed the sampling & projection tests" << endl;
      crux.rename("tComponentList_tmp.model", Table::New);
      image.table().markForDelete();
    }
    {
      // Create a model by reading the previous one from disk
      ComponentList model("tComponentList_tmp.model");
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).type() == ComponentType::POINT, AipsError);
      AlwaysAssert(model.component(3).type() == ComponentType::GAUSSIAN, AipsError);
      model.rename("tComponentListRenamed_tmp.model");
    }
    {
      // Check that the Table has been renamed and not copied
      AlwaysAssert(Table::isWritable("tComponentList_tmp.model") == False,
		   AipsError);
      AlwaysAssert(Table::isReadable("tComponentList_tmp.model") == False,
		   AipsError);

      // Open (readonly) the renamed model and copy it so we can mess with.
      const ComponentList model("tComponentListRenamed_tmp.model", True);
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).type() == ComponentType::POINT,
		   AipsError);
      AlwaysAssert(model.component(3).type() == ComponentType::GAUSSIAN, 
		   AipsError);
      ComponentList RO_Model(model);
      try {
	RO_Model.remove(0);
      } catch (AipsError x) {
        if (!x.getMesg().contains("theROFlag == False")) {
          cout << x.getMesg() << endl << "FAIL" << endl;
          return 1;
	}
      } end_try;
      ComponentList RW_Model(model.copy());
      Vector<Double> newFlux(4);
      newFlux = 0.0; newFlux(0) = 1.0;
      RW_Model.component(1).setFlux(newFlux);
      Vector<Double> ROflux(4), RWflux(4);
      // I cannot use       
      // RO_Model.component(1).flux(ROflux); here because the component(i)
      // function uses the non-const version which checks the RO flag. So I
      // recommend if opening a Model file readonly to always use
      // const ComponentList model(...)
      model.component(1).flux(ROflux);
      RW_Model.component(1).flux(RWflux);
      AlwaysAssert(allNear(RWflux.ac(), newFlux.ac(), 1E-10), AipsError);
      AlwaysAssert(!allNear(ROflux.ac(), newFlux.ac(), 1E-10), AipsError);
      RW_Model.remove(0);
      AlwaysAssert(model.nelements() == 6, AipsError);
      AlwaysAssert(model.component(2).type() == ComponentType::POINT, 
 		   AipsError);
      AlwaysAssert(model.component(3).type() == ComponentType::GAUSSIAN, 
 		   AipsError);
      AlwaysAssert(RW_Model.nelements() == 5, AipsError);
      AlwaysAssert(RW_Model.component(1).type() == ComponentType::POINT, 
		   AipsError);
      AlwaysAssert(RW_Model.component(2).type() == ComponentType::GAUSSIAN,
  		   AipsError);
      RW_Model.rename("tComponentListCopied_tmp.model");
      cout << "Passed the Table reading/writing tests" << endl;
    }
    {
      ComponentList original("tComponentListRenamed_tmp.model");
      ComponentList modified("tComponentListCopied_tmp.model", False);
      AlwaysAssert(original.nelements() == 6, AipsError);
      AlwaysAssert(original.component(2).type() == ComponentType::POINT, 
 		   AipsError);
      AlwaysAssert(original.component(3).type() == ComponentType::GAUSSIAN, 
 		   AipsError);
      AlwaysAssert(modified.nelements() == 5, AipsError);
      AlwaysAssert(modified.component(1).type() == ComponentType::POINT, 
		   AipsError);
      AlwaysAssert(modified.component(2).type() == ComponentType::GAUSSIAN,
  		   AipsError);
      Vector<Double> expectedFlux(4);
      expectedFlux = 0.0; expectedFlux(0) = 1.0;
      Vector<Double> oFlux(4), mFlux(4);
      original.component(0).flux(oFlux);
      AlwaysAssert(!allNear(oFlux.ac(), expectedFlux.ac(), 1E-10), AipsError);
      original.component(1).flux(oFlux);
      AlwaysAssert(!allNear(oFlux.ac(), expectedFlux.ac(), 1E-10), AipsError);
      modified.component(0).flux(mFlux);
      AlwaysAssert(allNear(mFlux.ac(), expectedFlux.ac(), 1E-10), AipsError);
      ComponentList newList(modified);
      newList.remove(0);
      expectedFlux = 2.0;
      newList.component(0).setFlux(expectedFlux);
      modified.component(0).flux(mFlux);
      AlwaysAssert(!allNear(mFlux.ac(), expectedFlux.ac(), 1E-10), AipsError);
      modified.component(1).flux(mFlux);
      AlwaysAssert(allNear(mFlux.ac(), expectedFlux.ac(), 1E-10), AipsError);
      newList = original;
      AlwaysAssert(newList.nelements() == 6, AipsError);
      newList.remove(0);
      AlwaysAssert(original.nelements() == 6, AipsError);
      AlwaysAssert(newList.nelements() == 5, AipsError);
      newList.component(0).setFlux(expectedFlux);
      original.component(0).flux(oFlux);
      AlwaysAssert(!allNear(oFlux.ac(), expectedFlux.ac(), 1E-10), AipsError);
      original.component(1).flux(oFlux);
      AlwaysAssert(allNear(oFlux.ac(), expectedFlux.ac(), 1E-10), AipsError);
      cout << "Passed the copy and assignment tests" << endl;
      original.rename("junk.model", Table::Scratch);
      modified.rename("more_junk.model", Table::Scratch);
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
