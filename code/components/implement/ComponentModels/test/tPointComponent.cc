//# tPointComponent.cc:  this defines tPointComponent.cc
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
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/PointComponent.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Images/PagedImage.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishValue.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/Quantum.h>
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
      // Create a point component at the default direction
      const MVDirection defMVdir;
      const MDirection defDirJ2000(defMVdir);
      const MDirection defDirB1950(defMVdir, MDirection::B1950);
      const PointComponent defPoint;
      Vector<Double> sampledFlux(4);
      Vector<Double> expectedFlux(4); 
      expectedFlux = 0.0; expectedFlux(0) = 1.0;
      const MVAngle tol = Quantity(1E-3, "mas");
      defPoint.sample(sampledFlux, defDirJ2000, tol);
      AlwaysAssert(allNear(sampledFlux.ac(), expectedFlux.ac(),C::dbl_epsilon),
 		   AipsError);
      defPoint.sample(sampledFlux, defDirB1950, tol);
      expectedFlux = 0.0;
      AlwaysAssert(allNear(sampledFlux.ac(), expectedFlux.ac(),C::dbl_epsilon),
 		   AipsError);
      cout << "Passed the default Point component test" << endl;
    }
    {
      // Create a point component at a defined non-J2000 direction
      const MVDirection dir1934(Quantity(293.5,"deg"),
  				Quantity(-63, "deg") + Quantity(43, "'"));
      const MDirection coord1934J2000(dir1934, MDirection::J2000);
      const MDirection coord1934B1950(dir1934, MDirection::B1950);
      const Flux<Double> flux1934(6.3, 0.0, 0.0, 0.0);
      const PointComponent B1934(flux1934, coord1934B1950);
      const Vector<DComplex> fluxVal(flux1934.value());
      Vector<Double> sampledFlux(4);
      const MVAngle tol = Quantity(1E-3, "mas");
      B1934.sample(sampledFlux, coord1934B1950, tol);
      for (uInt p = 0; p < 4; p++) {
	AlwaysAssert(near(sampledFlux(p), fluxVal(p).re, C::dbl_epsilon),
		     AipsError);
      }
      B1934.sample(sampledFlux, coord1934J2000, tol);
      for (uInt q = 0; q < 4; q++) {
	AlwaysAssert(near(sampledFlux(q), 0.0, C::dbl_epsilon),
		     AipsError);
      }
      cout << "Passed the arbitrary Point component test" << endl;
    }
    {
      // Create a default point component
      PointComponent B1934;
      // Set and verify  the flux of the point component.
      Flux<Double> flux1934(6.3*200, 0.0, 0.0, 0.0);
      flux1934.setUnit("WU");
      B1934.flux() = flux1934;
      Vector<Double> setFlux(4); flux1934.value(setFlux);
      Vector<Double> curFlux(4); B1934.flux().value(curFlux);
      AlwaysAssert(allNear(curFlux.ac(), setFlux.ac(), C::dbl_epsilon), 
 		   AipsError);

      // Set and verify the direction of the point component. It is internally
      // converted to a J2000 reference frame
      const MVDirection dir1934(Quantity(293.5,"deg"),Quantity(-63.8,"deg"));
      const MDirection coord1934B1950(dir1934, MDirection::B1950);
      B1934.setDirection(coord1934B1950);
      MDirection coord1934J2000;
      B1934.direction(coord1934J2000);
      AlwaysAssert(coord1934J2000.getRef().getType() == MDirection::J2000,
   		   AipsError); 
      AlwaysAssert(coord1934J2000.getValue()
  		   .near(MDirection::Convert(coord1934B1950,MDirection::J2000)
  			 ().getValue()),AipsError);

      // Check the label functions
      B1934.setLabel("1934-638");
      String label;
      B1934.label(label);
      AlwaysAssert(label == "1934-638", AipsError);

      // Check this is a point component
      AlwaysAssert(B1934.shape() == ComponentType::POINT, AipsError);
      AlwaysAssert(ComponentType::name(B1934.shape()).matches("Point") == 1, 
 		   AipsError);
      // Check the parameters interface (there are no parameters!)
      AlwaysAssert(B1934.nParameters() == 0, AipsError);
      Vector<Double> parms;
      B1934.setParameters(parms);
      B1934.parameters(parms);
      parms.resize(1);
      try {
 	B1934.setParameters(parms);
  	throw(AipsError("PointComponent incorrectly accepted a non-zero "
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
	throw(AipsError("PointComponent incorrectly used a non-zero "
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
      B1934.flux().convertUnit("Jy");
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
	AlwaysAssert(type == "point", AipsError);
      }
      PointComponent newComp;
      AlwaysAssert(newComp.fromRecord(errorMessage, compRec) == True,
		   AipsError);
      AlwaysAssert(errorMessage == "", AipsError);
      AlwaysAssert(near(flux1934.value(0).re, newComp.flux().value(0).re,
			C::dbl_epsilon), AipsError);
      AlwaysAssert(near(flux1934.value(1).re, newComp.flux().value(1).re,
			C::dbl_epsilon), AipsError);
      newComp.direction(coord1934J2000);
      AlwaysAssert(coord1934J2000.getValue()
 		   .near(MDirection::Convert(coord1934B1950,MDirection::J2000)
 			 ().getValue()), AipsError);
      cout << "Passed the to/from GlishRecord test for Point components"
  	   << endl;

      PointComponent compCopy = B1934.copy();
      AlwaysAssert(compCopy.shape() == ComponentType::POINT, AipsError);
      B1934.flux().setValue(0.0);
      AlwaysAssert(near(B1934.flux().value(0), 0.0), AipsError);
      AlwaysAssert(near(compCopy.flux().value(0), 6.3), AipsError);
      compCopy = B1934.copy();
      AlwaysAssert(near(compCopy.flux().value(0), 0.0), AipsError);
      B1934.flux().setValue(6.3);
      AlwaysAssert(near(compCopy.flux().value(0), 0.0), AipsError);
      AlwaysAssert(near(B1934.flux().value(0), 6.3), AipsError);
      PointComponent compRef = B1934;
      AlwaysAssert(near(compRef.flux().value(0), 6.3), AipsError);
      B1934.flux().setValue(0.0);
      AlwaysAssert(near(compRef.flux().value(0), 0.0), AipsError);
      compCopy.flux().setValue(6.3);
      compRef = compCopy;
      AlwaysAssert(near(compRef.flux().value(0), 6.3), AipsError);
      AlwaysAssert(B1934.ok(), AipsError);
      AlwaysAssert(compCopy.ok(), AipsError);
      AlwaysAssert(compRef.ok(), AipsError);
      cout << "Passed the copy and assignment tests" << endl;
    }
    {
      const uInt nx=6, ny=nx;
      PagedImage<Float> image(IPosition(2,nx,ny), 
 			      CoordinateUtil::defaultCoords2D(),
 			      "tPointComponent_tmp.image");
      image.set(0.0f);
      PointComponent defComp;
      const MVDirection ra0dec0(Quantity(2, "'"), Quantity(1, "'"));
      const MDirection coord00(ra0dec0, MDirection::J2000);
      defComp.setDirection(coord00);
      defComp.project(image);
      AlwaysAssert(near(image(IPosition(2, 2, 1)), 1.0f), AipsError);
      image.putAt(0.0f, IPosition(2, 2, 1));
   
      for (uInt i = 0; i < nx; i++)
	for (uInt j = 0; j < ny; j++)
	  AlwaysAssert(near(image(IPosition(2, i, j)), 0.0f), 
		       AipsError);
      image.table().markForDelete();
      cout << "Passed the projection to a 4-D image test" << endl;
    }
    {
      PointComponent defPoint;
      Vector<Double> uvw(3);
      uvw = 0.0;
      Double freq = 1.0;
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(0),
			DComplex(1.0, 0.0)), AipsError);
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(1),
			DComplex(0.0, 0.0)), AipsError);
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(2),
			DComplex(0.0, 0.0)), AipsError);
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(3),
			DComplex(0.0, 0.0)), AipsError);
      uvw = 1.0;
      freq = 2.0;
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(0),
			DComplex(1.0, 0.0)), AipsError);
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(1),
			DComplex(0.0, 0.0)), AipsError);
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(2),
			DComplex(0.0, 0.0)), AipsError);
      AlwaysAssert(near(defPoint.visibility(uvw, freq).value(3),
			DComplex(0.0, 0.0)), AipsError);
      cout << "Passed the Fourier Transform test" << endl;
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
