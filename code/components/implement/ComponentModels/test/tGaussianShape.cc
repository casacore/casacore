//# tGaussianCompRep.cc:  this defines tGaussianCompRep.cc
//# Copyright (C) 1996,1997,1998
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
#include <trial/ComponentModels/GaussianCompRep.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Images/PagedImage.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Glish/GlishValue.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Quanta/Euler.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/RotMatrix.h>
#include <aips/Measures/Stokes.h>
#include <aips/Tables/Table.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> 
        gpp_measconvert_mdirection_mvdirection_mcdirection;
#endif

int main() {
  try {
    {
      // Create a Gaussian component at the default direction
      const GaussianCompRep defGaussian;
      const MVAngle pixelSize(Quantity(1.0,"''"));
      // Sample the Gaussian at the Maximum and half an arc-min on either side.
      MVDirection sampleDirVal(Quantity(0,"deg"), 
			       Quantity(90, "deg") - Quantity(.5, "'"));
      MDirection sampleDir(sampleDirVal, MDirection::J2000);
      const RotMatrix rotDec(Euler(Quantity(0.5, "'").getValue("rad"), 2u));
   
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
      // Create a Gaussian component at a defined non-J2000 direction
      const MVDirection dir1934(Quantity(293.5,"deg"), Quantity(-63.7, "deg"));
      const MDirection coord1934J2000(dir1934, MDirection::J2000);
      Flux<Double> flux1934(6.3, 0.0, 0.0, 0.0);
      const MVAngle majorAxis(Quantity(2E-3, "''"));
      const MVAngle minorAxis(Quantity(2E-3, "''"));
      const GaussianCompRep J1934(flux1934, coord1934J2000, majorAxis, 
				  minorAxis, MVAngle());
      // Create a direction that is 1 mas away from the pole
      MVDirection sampleDir(Quantity(0,"deg"),
 			    Quantity(90, "deg") - Quantity(1, "mas"));
      // And now make another rotater that can rotate this point about the pole
      // in steps of say 40 degrees
      const RotMatrix rotater(Euler(Quantity(40, "deg").getValue("rad"), 3u));

      // Create a rotation matrix that can rotate the pole down to the
      // component. 
      const RotMatrix pole2src(Euler(Quantity(-153.7,"deg").getValue("rad"), 
				     2u, 
				     Quantity(-293.5,"deg").getValue("rad"),
				     3u));
      const MVDirection pole;
      // Sample at a set of MDirections equidistant from the direction of the
      // component. All these points should have the same flux (in Jy/pixel)
      // of half the maximum. 
      const MVAngle pixelSize(Quantity(1.0,"''"));
      MDirection sampledDirection;
      Vector<Double> sampledFlux(4);
      Double peak;
      peak = flux1934.value(0).re * 4. * pow(180. * 60. * 60. * 1000. /2. , 2.)
	* C::ln2 * pow(C::pi,-3.) 
	* pixelSize.radian() * pixelSize.radian();
      for (uInt i = 0; i < 6; i++){
 	sampledDirection = MDirection(sampleDir*pole2src, MDirection::J2000);
	J1934.sample(sampledFlux, sampledDirection, pixelSize);
	// Precision is lost because of the subtraction done in the
	// MVPosition::separation member function
 	AlwaysAssert(near(sampledFlux(0), peak*0.5, 1E-6), 
		     AipsError);
  	AlwaysAssert(near(sampledFlux(1), 0.0, C::dbl_epsilon), 
 		     AipsError);
  	AlwaysAssert(near(sampledFlux(2), 0.0, C::dbl_epsilon), 
 		     AipsError);
  	AlwaysAssert(near(sampledFlux(3), 0.0, C::dbl_epsilon), 
 		     AipsError);
 	sampleDir *= rotater;
      }
      cout << "Passed the arbitrary Gaussian component test" << endl;
    }
    {
      // Create a Gaussian component at a defined non-J2000 direction
      Flux<Double> initialFlux(2.0, 2.0, 2.0, 2.0);
      Vector<Double> initialVal;
      initialFlux.value(initialVal);
      const MDirection initialPosition(MVDirection(1.0), MDirection::B1950);
      const MVAngle initialMajorAxis(MVAngle(Quantity(13, "''")));
      const Double initialAxialRatio = 0.1;
      const MVAngle initialPA(MVAngle(Quantity(10, "deg")));
      GaussianCompRep B1934(initialFlux, initialPosition, initialMajorAxis,
  			    initialAxialRatio, initialPA);
      Vector<Double> componentFluxVal;
      B1934.flux().value(componentFluxVal);
      AlwaysAssert(allNear(initialVal.ac(), componentFluxVal.ac(),
			   C::dbl_epsilon), AipsError);

      // Set and verify  the flux of the Gaussian component.
      Flux<Double> flux1934(6.3*200, 0.0, 0.0, 0.0);
      flux1934.setUnit("WU");
      B1934.flux() = flux1934;
      B1934.flux().convertUnit("Jy");
      AlwaysAssert(near(B1934.flux().value(0), DComplex(6.3, 0.0), 
 			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(B1934.flux().value(1), DComplex(0.0, 0.0), 
 			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(B1934.flux().value(2), DComplex(0.0, 0.0), 
 			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(B1934.flux().value(3), DComplex(0.0, 0.0), 
 			C::dbl_epsilon), AipsError);

      // Set and verify the direction of the Gaussian component. It is
      // internally converted to a J2000 reference frame
      const MVDirection dir1934(Quantity(293.5,"deg"),Quantity(-63.8,"deg"));
      const MDirection coord1934B1950(dir1934, MDirection::B1950);
      MDirection coord1934J2000 = coord1934B1950;
      B1934.direction(coord1934J2000);
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
  		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue()
 		   .near(MDirection::Convert(initialPosition,MDirection::J2000)
 			 ().getValue()), AipsError);
      B1934.setDirection(coord1934B1950);
      B1934.direction(coord1934J2000);
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
  		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue()
 		   .near(MDirection::Convert(coord1934B1950,MDirection::J2000)
 			 ().getValue()), AipsError);
      // Set and verify the width of the Gaussian component. 
      MVAngle majorAxis;
      B1934.majorAxis(majorAxis);
      AlwaysAssert(near(majorAxis.radian(), initialMajorAxis.radian(),
			C::dbl_epsilon), AipsError);
      MVAngle minorAxis;
      B1934.minorAxis(minorAxis);
      AlwaysAssert(near(minorAxis.radian(), 
			initialMajorAxis.radian()*initialAxialRatio,
			C::dbl_epsilon), AipsError);
      Double axialRatio;
      B1934.axialRatio(axialRatio);
      AlwaysAssert(near(axialRatio, initialAxialRatio, C::dbl_epsilon),
		   AipsError);
      MVAngle pa;
      B1934.positionAngle(pa);
      AlwaysAssert(near(pa.radian(), initialPA.radian(),
			C::dbl_epsilon), AipsError);

      MVAngle compMajorAxis(Quantity(4, "''" ));
      const MVAngle compMinorAxis(Quantity(2, "''" ));
      MVAngle compPA(Quantity(45, "deg" ));
      B1934.setWidth(compMajorAxis, compMinorAxis, compPA);
      B1934.width(majorAxis, minorAxis, pa);
      AlwaysAssert(near(majorAxis.radian(), compMajorAxis.radian(),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(minorAxis.radian(), compMinorAxis.radian(),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(pa.radian(), compPA.radian(), C::dbl_epsilon),
		   AipsError);
      
      compMajorAxis = Quantity(8, "''");
      compPA = Quantity(30, "deg");
      Double compAxialRatio = .5;
      B1934.setWidth(compMajorAxis, compAxialRatio, compPA);
      B1934.width(majorAxis, axialRatio, pa);
      AlwaysAssert(near(majorAxis.radian(), compMajorAxis.radian(),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(axialRatio, compAxialRatio, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(pa.radian(), compPA.radian(), C::dbl_epsilon),
		   AipsError);

      // Check this is a Gaussian component
      AlwaysAssert(B1934.shape() == ComponentType::GAUSSIAN, AipsError);
      AlwaysAssert(ComponentType::name(B1934.shape()).matches("Gaussian") == 1,
 		   AipsError);

      // Check the parameters interface
      AlwaysAssert(B1934.nParameters() == 3, AipsError);
      Vector<Double> parms(3);
      B1934.parameters(parms);
      AlwaysAssert(near(parms(0), compMajorAxis.radian(), C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(near(parms(1), compMajorAxis.radian()*compAxialRatio,
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(parms(2), compPA.radian(), C::dbl_epsilon), AipsError);
      parms(0) = Quantity(4, "''").getValue("rad");
      parms(1) = Quantity(2, "''").getValue("rad");;
      parms(2) = Quantity(45.0, "deg").getValue("rad");
      B1934.setParameters(parms);
      parms = 0.0;
      B1934.parameters(parms);
      AlwaysAssert(near(parms(0), Quantity(4, "''").getValue("rad"),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(parms(1), Quantity(2, "''").getValue("rad"),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(parms(2), Quantity(45, "deg").getValue("rad"),
			C::dbl_epsilon), AipsError);
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
      String errorMessage("");
      GlishRecord compRec;
      AlwaysAssert(B1934.toRecord(errorMessage, compRec) == True, AipsError);
      AlwaysAssert(errorMessage == "", AipsError);
      {
 	AlwaysAssert(compRec.exists("flux"), AipsError);
 	AlwaysAssert(compRec.get("flux").type() == GlishValue::RECORD, 
 		     AipsError);
 	GlishRecord fluxRec(compRec.get("flux"));
 	AlwaysAssert(fluxRec.exists("value"), AipsError);
 	AlwaysAssert(fluxRec.get("value").type() == GlishValue::ARRAY, 
 		     AipsError);
 	GlishArray fluxVal(fluxRec.get("value"));
 	AlwaysAssert(fluxVal.elementType() != GlishArray::STRING, AipsError);
 	AlwaysAssert(fluxVal.shape().isEqual(IPosition(1,4)), AipsError);
 	Vector<Double> value(4);
 	fluxVal.get(value);
 	AlwaysAssert(near(value(0), 6.3, C::dbl_epsilon), AipsError);
 	AlwaysAssert(near(value(1), 0.0, C::dbl_epsilon), AipsError);
 	AlwaysAssert(near(value(2), 0.0, C::dbl_epsilon), AipsError);
 	AlwaysAssert(near(value(3), 0.0, C::dbl_epsilon), AipsError);
 	AlwaysAssert(fluxRec.exists("unit"), AipsError);
 	AlwaysAssert(fluxRec.get("unit").type() == GlishValue::ARRAY, 
 		     AipsError);
 	GlishArray fluxUnit(fluxRec.get("unit"));
 	AlwaysAssert(fluxUnit.elementType() == GlishArray::STRING, AipsError);
 	AlwaysAssert(fluxUnit.shape().isEqual(IPosition(1,1)), AipsError);
 	String unit;
 	fluxUnit.get(unit);
 	AlwaysAssert(unit == "Jy", AipsError);
 	AlwaysAssert(fluxRec.exists("polarisation"), AipsError);
 	AlwaysAssert(fluxRec.get("polarisation").type() == GlishValue::ARRAY, 
 		     AipsError);
 	GlishArray fluxPol(fluxRec.get("polarisation"));
 	AlwaysAssert(fluxPol.elementType() == GlishArray::STRING, AipsError);
 	AlwaysAssert(fluxPol.shape().isEqual(IPosition(1,1)), AipsError);
 	String pol;
 	fluxPol.get(pol);
 	AlwaysAssert(pol == "Stokes", AipsError);
      }
      {
	AlwaysAssert(compRec.exists("direction"), AipsError);
	AlwaysAssert(compRec.get("direction").type() == GlishValue::RECORD, 
		     AipsError);
	GlishRecord dirRec(compRec.get("direction"));
	AlwaysAssert(dirRec.exists("refer"), AipsError);
	AlwaysAssert(dirRec.get("refer").type() == GlishValue::ARRAY, 
		     AipsError);
	GlishArray referVal(dirRec.get("refer"));
	AlwaysAssert(referVal.elementType() == GlishArray::STRING, AipsError);
	AlwaysAssert(referVal.shape().isEqual(IPosition(1,1)), AipsError);
	String refer;
	referVal.get(refer);
	AlwaysAssert(refer == "J2000", AipsError);

	AlwaysAssert(dirRec.exists("type"), AipsError);
	AlwaysAssert(dirRec.get("type").type() == GlishValue::ARRAY, 
		     AipsError);
	GlishArray typeVal(dirRec.get("type"));
	AlwaysAssert(typeVal.elementType() == GlishArray::STRING, AipsError);
	AlwaysAssert(typeVal.shape().isEqual(IPosition(1,1)), AipsError);
	String type;
	typeVal.get(type);
	AlwaysAssert(type == "direction", AipsError);
  
	AlwaysAssert(dirRec.exists("m0"), AipsError);
	AlwaysAssert(dirRec.get("m0").type() == GlishValue::RECORD, 
		     AipsError);
	{
	  GlishRecord mRec(dirRec.get("m0"));
	  AlwaysAssert(mRec.exists("value"), AipsError);
	  AlwaysAssert(mRec.get("value").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mVal(mRec.get("value"));
	  AlwaysAssert(mVal.elementType() != GlishArray::STRING, AipsError);
	  AlwaysAssert(mVal.shape().isEqual(IPosition(1,1)), AipsError);
	  Double value;
	  mVal.get(value);
	  AlwaysAssert(near(value, -65.3439, 1E-6), AipsError);
	  AlwaysAssert(mRec.exists("unit"), AipsError);
	  AlwaysAssert(mRec.get("unit").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mUnit(mRec.get("unit"));
	  AlwaysAssert(mUnit.elementType() == GlishArray::STRING, AipsError);
	  AlwaysAssert(mUnit.shape().isEqual(IPosition(1,1)), AipsError);
	  String unit;
	  mUnit.get(unit);
	  AlwaysAssert(unit == "deg", AipsError);
	}
	AlwaysAssert(dirRec.exists("m1"), AipsError);
	AlwaysAssert(dirRec.get("m1").type() == GlishValue::RECORD, 
		     AipsError);
	{
	  GlishRecord mRec(dirRec.get("m1"));
	  AlwaysAssert(mRec.exists("value"), AipsError);
	  AlwaysAssert(mRec.get("value").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mVal(mRec.get("value"));
	  AlwaysAssert(mVal.elementType() != GlishArray::STRING, AipsError);
	  AlwaysAssert(mVal.shape().isEqual(IPosition(1,1)), AipsError);
	  Double value;
	  mVal.get(value);
	  AlwaysAssert(near(value, -63.6864, 1E-6), AipsError);
	  AlwaysAssert(mRec.exists("unit"), AipsError);
	  AlwaysAssert(mRec.get("unit").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mUnit(mRec.get("unit"));
	  AlwaysAssert(mUnit.elementType() == GlishArray::STRING, AipsError);
	  AlwaysAssert(mUnit.shape().isEqual(IPosition(1,1)), AipsError);
	  String unit;
	  mUnit.get(unit);
	  AlwaysAssert(unit == "deg", AipsError);
	}
      }
      {
	AlwaysAssert(compRec.exists("shape"), AipsError);
	AlwaysAssert(compRec.get("shape").type() == GlishValue::RECORD, 
		     AipsError);
	GlishRecord shapeRec(compRec.get("shape"));
	AlwaysAssert(shapeRec.exists("type"), AipsError);
	AlwaysAssert(shapeRec.get("type").type() == GlishValue::ARRAY, 
		     AipsError);
	GlishArray typeVal(shapeRec.get("type"));
	AlwaysAssert(typeVal.elementType() == GlishArray::STRING, AipsError);
	AlwaysAssert(typeVal.shape().isEqual(IPosition(1,1)), AipsError);
	String type;
	typeVal.get(type);
	AlwaysAssert(type == "gaussian", AipsError);

	AlwaysAssert(shapeRec.exists("majoraxis"), AipsError);
	AlwaysAssert(shapeRec.get("majoraxis").type() == GlishValue::RECORD, 
		     AipsError);
	{
	  GlishRecord mRec(shapeRec.get("majoraxis"));
	  AlwaysAssert(mRec.exists("value"), AipsError);
	  AlwaysAssert(mRec.get("value").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mVal(mRec.get("value"));
	  AlwaysAssert(mVal.elementType() != GlishArray::STRING, AipsError);
	  AlwaysAssert(mVal.shape().isEqual(IPosition(1,1)), AipsError);
	  Double value;
	  mVal.get(value);
	  AlwaysAssert(near(value, 0.0666667, 1E-6), AipsError);
	  AlwaysAssert(mRec.exists("unit"), AipsError);
	  AlwaysAssert(mRec.get("unit").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mUnit(mRec.get("unit"));
	  AlwaysAssert(mUnit.elementType() == GlishArray::STRING, AipsError);
	  AlwaysAssert(mUnit.shape().isEqual(IPosition(1,1)), AipsError);
	  String unit;
	  mUnit.get(unit);
	  AlwaysAssert(unit == "'", AipsError);
	}
	AlwaysAssert(shapeRec.exists("minoraxis"), AipsError);
	AlwaysAssert(shapeRec.get("minoraxis").type() == GlishValue::RECORD, 
		     AipsError);
	{
	  GlishRecord mRec(shapeRec.get("minoraxis"));
	  AlwaysAssert(mRec.exists("value"), AipsError);
	  AlwaysAssert(mRec.get("value").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mVal(mRec.get("value"));
	  AlwaysAssert(mVal.elementType() != GlishArray::STRING, AipsError);
	  AlwaysAssert(mVal.shape().isEqual(IPosition(1,1)), AipsError);
	  Double value;
	  mVal.get(value);
	  AlwaysAssert(near(value, 0.0333333, 1E-6), AipsError);
	  AlwaysAssert(mRec.exists("unit"), AipsError);
	  AlwaysAssert(mRec.get("unit").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mUnit(mRec.get("unit"));
	  AlwaysAssert(mUnit.elementType() == GlishArray::STRING, AipsError);
	  AlwaysAssert(mUnit.shape().isEqual(IPosition(1,1)), AipsError);
	  String unit;
	  mUnit.get(unit);
	  AlwaysAssert(unit == "'", AipsError);
	}
	AlwaysAssert(shapeRec.exists("positionangle"), AipsError);
	AlwaysAssert(shapeRec.get("positionangle").type() ==GlishValue::RECORD,
		     AipsError);
 	{
 	  GlishRecord mRec(shapeRec.get("positionangle"));
	  AlwaysAssert(mRec.exists("value"), AipsError);
	  AlwaysAssert(mRec.get("value").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mVal(mRec.get("value"));
	  AlwaysAssert(mVal.elementType() != GlishArray::STRING, AipsError);
	  AlwaysAssert(mVal.shape().isEqual(IPosition(1,1)), AipsError);
	  Double value;
	  mVal.get(value);
	  AlwaysAssert(near(value, 45.0, C::dbl_epsilon), AipsError);
	  AlwaysAssert(mRec.exists("unit"), AipsError);
	  AlwaysAssert(mRec.get("unit").type() == GlishValue::ARRAY, 
		       AipsError);
	  GlishArray mUnit(mRec.get("unit"));
	  AlwaysAssert(mUnit.elementType() == GlishArray::STRING, AipsError);
	  AlwaysAssert(mUnit.shape().isEqual(IPosition(1,1)), AipsError);
	  String unit;
	  mUnit.get(unit);
	  AlwaysAssert(unit == "deg", AipsError);
	}
      }
      GaussianCompRep newComp;
      AlwaysAssert(newComp.fromRecord(errorMessage, compRec) == True,
		   AipsError);
      AlwaysAssert(errorMessage == "", AipsError);
      AlwaysAssert(near(flux1934.value(0), newComp.flux().value(0),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(flux1934.value(1), newComp.flux().value(1),
			C::dbl_epsilon), AipsError);
      newComp.direction(coord1934J2000);
      AlwaysAssert(coord1934J2000.getValue()
 		   .near(MDirection::Convert(coord1934B1950,MDirection::J2000)
 			 ().getValue()), AipsError);
      newComp.width(majorAxis, minorAxis, pa);
      parms.resize(3);
      newComp.parameters(parms);
      AlwaysAssert(near(parms(0), Quantity(4, "''").getValue("rad"),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(parms(1), Quantity(2, "''").getValue("rad"),
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(parms(2), Quantity(45, "deg").getValue("rad"),
			C::dbl_epsilon), AipsError);
      cout << "Passed the to/from GlishRecord test for Gaussian components"
  	   << endl;

      const SkyCompRep * compRepPtr = B1934.clone();
      AlwaysAssert(compRepPtr->shape() == ComponentType::GAUSSIAN, AipsError);
      flux1934 = 0.0;
      AlwaysAssert(near(compRepPtr->flux().value(0).re, 6.3), AipsError);
      GaussianCompRep copiedComp(*((GaussianCompRep *) compRepPtr));
      copiedComp.flux().setValue(0.0);
      AlwaysAssert(near(compRepPtr->flux().value(0), 6.3), AipsError);
      AlwaysAssert(near(copiedComp.flux().value(0), 0.0), AipsError);
      copiedComp = B1934;
      AlwaysAssert(near(copiedComp.flux().value(0), 6.3), AipsError);
      copiedComp.flux().setValue(0.0);
      AlwaysAssert(near(B1934.flux().value(0), 6.3), AipsError);
      AlwaysAssert(near(copiedComp.flux().value(0), 0.0), AipsError);
      AlwaysAssert(B1934.ok(), AipsError);
      AlwaysAssert(copiedComp.SkyCompRep::ok(), AipsError);
      AlwaysAssert(compRepPtr->ok(), AipsError);
      cout << "Passed the copy and assignment tests" << endl;
    }
    {
      const uInt imSize = 6;
      const uInt nPol = 2;
      const uInt nFreq = 1;
      CoordinateSystem coords(CoordinateUtil::defaultCoords2D());
      {
  	Vector<Int> pols(nPol);
  	pols(0) = Stokes::I;
  	pols(1) = Stokes::U;
  	StokesCoordinate polAxis(pols);
  	coords.addCoordinate(polAxis);
      }
      CoordinateUtil::addFreqAxis(coords);
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
      defComp.flux().setValue(flux);

      MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
      MDirection coord00(ra0dec0, MDirection::J2000);
      defComp.setDirection(coord00);
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
    {
      // Create a Gaussian component at the default direction
      GaussianCompRep comp;
      Vector<Double> flux(4);
      flux(0) = 1;
      flux(1) = 0.1;
      flux(2) = 0.01;
      flux(3) = 0.001;
      comp.flux().setValue(flux);
      Vector<Double> uvw(3, 0.0);
      Double freq = C::c;
      Vector<Double> realVis;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac(), C::dbl_epsilon),AipsError);
      Vector<DComplex> vis = comp.visibility(uvw, freq).value();
      Vector<DComplex> expectedVis(4);
      for (uInt s = 0; s < 4; s++) {
 	expectedVis(s).re = flux(s);
 	expectedVis(s).im = 0.0;
      }
      AlwaysAssert(allNear(vis.ac(), expectedVis.ac(), C::dbl_epsilon),
  		   AipsError);
      uvw(0) = (4.0*C::ln2/C::pi)/((1.0/60.0) * C::pi/180.0)/2.0;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac()/2.0, 2*C::dbl_epsilon),
 		   AipsError);
      uvw(1) = uvw(0); uvw(0) = 0.0;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac()/2.0, 2*C::dbl_epsilon),
 		   AipsError);
      uvw(2) = uvw(1); uvw(1) = 0.0;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac(), C::dbl_epsilon),
 		   AipsError);
      const MVAngle compMajorAxis(Quantity(1, "'" ));
      const MVAngle compMinorAxis(Quantity(30, "''" ));
      MVAngle compPA(Quantity(0, "deg" ));
      comp.setWidth(compMajorAxis, compMinorAxis, compPA);
      uvw(1) = uvw(2); uvw(2) = 0.0;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac()/2.0, 2*C::dbl_epsilon),
 		   AipsError);
      uvw(0) = uvw(1)*2; uvw(1) = 0.0;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac()/2.0, 2*C::dbl_epsilon),
 		   AipsError);
      compPA = Quantity(30, "deg");
      comp.setWidth(compMajorAxis, compMinorAxis, compPA);
      uvw(1)  = .5 * uvw(0);
      uvw(0)  *= C::sqrt3/2.0;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac()/2.0, 2*C::dbl_epsilon),
 		   AipsError);
      freq *= 2.0;
      uvw(0) /= -2.0;
      uvw(1) /= -2.0;
      comp.visibility(uvw, freq).value(realVis);
      AlwaysAssert(allNear(realVis.ac(), flux.ac()/2.0, 2*C::dbl_epsilon),
 		   AipsError);
      cout << "Passed the Fourier transform test" << endl;
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
