//# tPointShape.cc:  test program for PointShape and componentShape classes
//# Copyright (C) 1999,2000,2001
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

#include <casa/aips.h>
#include <components/ComponentModels/PointShape.h>
#include <components/ComponentModels/ComponentType.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Exceptions/Error.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordFieldId.h>
#include <measures/Measures/MeasureHolder.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MDirection.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {
  try {
    ComponentShape* shapePtr = 0;
    {
      // Create a default point shape 
      const PointShape defPoint;
      AlwaysAssert(defPoint.type() == ComponentType::POINT, AipsError);
      AlwaysAssert(defPoint.ident() == "Point", AipsError);
      AlwaysAssert(defPoint.isSymmetric() == True, AipsError);
      
      const MVDirection defMVdir;
      const MDirection dirJ2000(defMVdir);
      const MDirection dirB1950(defMVdir, MDirection::B1950);
      const MVAngle tol = Quantity(1E-3, "mas");
      AlwaysAssert(near(defPoint.sample(dirJ2000, tol, tol), 1.0, 
			C::dbl_epsilon), AipsError);
      AlwaysAssert(nearAbs(defPoint.sample(dirB1950, tol, tol), 0.0,
 			   C::dbl_min), AipsError);
      cout << "Passed the default Point component test" << endl;
    }
    {
      // Create a point component at a defined non-J2000 direction
      PointShape pc;
      {
	AlwaysAssert(pc.refDirection().getRef().getType() == MDirection::J2000,
		     AipsError);
	const Vector<Double> v = pc.refDirection().getValue().get();
	AlwaysAssert(nearAbs(v(0), 0.0), AipsError);
	AlwaysAssert(near(v(1), C::pi_2), AipsError);
      }
      {
	const MVDirection dir1934(Quantity(180,"deg"), Quantity(45, "deg"));
	const MDirection coord1934(dir1934, MDirection::B1950);
	pc = PointShape(coord1934);
      }
      {
	AlwaysAssert(pc.refDirection().getRef().getType() == MDirection::B1950,
		     AipsError);
	const Vector<Double> v = pc.refDirection().getValue().get();
	AlwaysAssert(nearAbs(v(0), C::pi), AipsError);
	AlwaysAssert(near(v(1), C::pi/4.0), AipsError);
      }
      cout << "Passed the arbitrary Point component test" << endl;
      // test the copy semantics
      PointShape otherpc(pc);
      shapePtr = pc.clone();
      pc.setRefDirection(MDirection(MVDirection(Quantity(0,"deg"), 
						Quantity(0, "deg")),
				    MDirection::AZEL));
      pc.setRefDirectionError(Quantum<Double>(1.0, "deg"),
 			      Quantum<Double>(2.0, "arcsec"));
      shapePtr->setRefDirection(MDirection(MVDirection(Quantity(90,"deg"), 
						       Quantity(45, "deg")),
					   MDirection::SUPERGAL));
      shapePtr->setRefDirectionError(Quantum<Double>(3.0, "rad"),
 				     Quantum<Double>(4.0, "arcmin"));
      {
	AlwaysAssert(pc.refDirection().getRef().getType() == MDirection::AZEL,
		     AipsError);
	const Vector<Double> v = pc.refDirection().getValue().get();
	AlwaysAssert(near(v(0), 0.0), AipsError);
	AlwaysAssert(near(v(1), 0.0), AipsError);
	AlwaysAssert(near(pc.refDirectionErrorLat().getValue(), 1.0),
		     AipsError);
	AlwaysAssert(near(pc.refDirectionErrorLong().getValue(), 2.0),
		     AipsError);
	AlwaysAssert(pc.refDirectionErrorLat().getUnit() == "deg", AipsError);
	AlwaysAssert(pc.refDirectionErrorLong().getUnit() == "arcsec", 
		     AipsError);
	AlwaysAssert(pc.ok(), AipsError);
      }
      {
	AlwaysAssert(shapePtr->refDirection().getRef().getType() 
		     == MDirection::SUPERGAL, AipsError);
	const Vector<Double> v = shapePtr->refDirection().getValue().get();
	AlwaysAssert(near(v(0), C::pi_2), AipsError);
	AlwaysAssert(near(v(1), C::pi_4), AipsError);
	AlwaysAssert(near(shapePtr->refDirectionErrorLat().getValue(), 3.0),
		     AipsError);
	AlwaysAssert(near(shapePtr->refDirectionErrorLong().getValue(), 4.0),
		     AipsError);
	AlwaysAssert(shapePtr->refDirectionErrorLat().getUnit() == "rad",
		     AipsError);
	AlwaysAssert(shapePtr->ok(), AipsError);
	AlwaysAssert(shapePtr->type() == ComponentType::POINT, AipsError);
      }
      {
	AlwaysAssert(otherpc.refDirection().getRef().getType() 
		     == MDirection::B1950, AipsError);
	const Vector<Double> v = otherpc.refDirection().getValue().get();
	AlwaysAssert(near(v(0), C::pi), AipsError);
	AlwaysAssert(near(v(1), C::pi_4), AipsError);
	AlwaysAssert(near(otherpc.refDirectionErrorLat().getValue(), 0.0),
		     AipsError);
	AlwaysAssert(near(otherpc.refDirectionErrorLong().getValue(), 0.0),
		     AipsError);
	AlwaysAssert(otherpc.refDirectionErrorLat().getUnit() == "rad", 
		     AipsError);
	AlwaysAssert(otherpc.refDirectionErrorLong().getUnit() == "rad", 
		     AipsError);
	AlwaysAssert(otherpc.ok(), AipsError);
      }
      cout << "Passed the copy semantics test" << endl;
    }
    {
      const MVAngle pixSize(Quantity(1, "mas"));
      Vector<MDirection::MVType> dirs(2);
      dirs(0) = MVDirection(Quantity(0,"deg"), Quantity(0, "deg"));
      dirs(1) = MVDirection(Quantity(90,"deg"), Quantity(45, "deg"));
      MDirection::Ref ref(shapePtr->refDirection().getRef());
      Vector<Double> scales(2, -1.0);
      shapePtr->sample(scales, dirs, ref, pixSize, pixSize);
      AlwaysAssert(nearAbs(scales(0), 0.0), AipsError);
      AlwaysAssert(near(scales(1), 1.0), AipsError);
      scales = -1.0;
      shapePtr->ComponentShape::sample(scales, dirs, ref, pixSize, pixSize);
      AlwaysAssert(nearAbs(scales(0), 0.0), AipsError);
      AlwaysAssert(near(scales(1), 1.0), AipsError);
      
      Vector<Double> uvw(3,0.0);
      Double freq = 1E6;
      AlwaysAssert(near(shapePtr->visibility(uvw, freq).real(), 1.0),
		   AipsError);
      AlwaysAssert(near(shapePtr->visibility(uvw, freq).imag(), 0.0),
		   AipsError);
      Matrix<Double> uvws(3, 2);
      uvws = 0.0; uvws(1,1) = 1.0;
      freq = 1;
      Vector<DComplex> results(2, DComplex(10.0, 10.0));
      shapePtr->visibility(results, uvws, freq);
      AlwaysAssert(near(results(0).real(), 1.0), AipsError);
      AlwaysAssert(near(results(0).imag(), 0.0), AipsError);
      AlwaysAssert(near(results(1).real(), 1.0), AipsError);
      AlwaysAssert(near(results(1).imag(), 0.0), AipsError);
      results = DComplex(10.0, 10.0);
      shapePtr->ComponentShape::visibility(results, uvws, freq);
      AlwaysAssert(near(results(0).real(), 1.0), AipsError);
      AlwaysAssert(near(results(0).imag(), 0.0), AipsError);
      AlwaysAssert(near(results(1).real(), 1.0), AipsError);
      AlwaysAssert(near(results(1).imag(), 0.0), AipsError);
      cout << "Passed the sample/visibility test" << endl;
    }
    {
      AlwaysAssert(shapePtr->nParameters() == 0, AipsError);
      Vector<Double> v = shapePtr->parameters().copy();
      AlwaysAssert(v.nelements() == 0, AipsError);
      shapePtr->setParameters(v);
      Vector<Double> e = shapePtr->errors().copy();
      AlwaysAssert(e.nelements() == 0, AipsError);
      shapePtr->setErrors(e);
#if defined(AIPS_DEBUG)
      v.resize(1);
      try{
	shapePtr->setParameters(v);
	throw(AipsError("Incorrect parameter vector exception NOT thrown"));
      }
      catch (AipsError x) {
	if(!x.getMesg().contains("newParms.nelements() == nParameters()")) {
	  throw;
	}
      }
      e.resize(1);
      try{
	shapePtr->setErrors(e);
	throw(AipsError("Incorrect parameter vector exception NOT thrown"));
      }
      catch (AipsError x) {
	if(!x.getMesg().contains("newErrors.nelements() == nParameters()")) {
	  throw;
	}
      }
#endif
      cout << "Passed the parameter interface test" << endl;
    }
    {
      String errorMsg;
      Record rec;
      shapePtr->toRecord(errorMsg, rec);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      AlwaysAssert(rec.isDefined("type"), AipsError);
      AlwaysAssert(rec.isDefined("direction"), AipsError);
      String type;
      rec.get(RecordFieldId("type"), type);
      AlwaysAssert(type == "Point", AipsError);
      Record dirRec = rec.asRecord(RecordFieldId("direction"));
      MeasureHolder mh;
      mh.fromRecord(errorMsg, dirRec);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      AlwaysAssert(mh.isMDirection(), AipsError);
      Record emptyRec;
      shapePtr->convertUnit(errorMsg, emptyRec);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      emptyRec.define(RecordFieldId("type"), "poiNT");
      AlwaysAssert(ComponentShape::getType(errorMsg, emptyRec) ==
		   ComponentType::POINT, AipsError);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      AlwaysAssert(shapePtr->fromRecord(errorMsg, emptyRec) == True,
		   AipsError);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      emptyRec.defineRecord(RecordFieldId("direction"), dirRec);
      PointShape p;
      errorMsg = "";
      p.fromRecord(errorMsg, emptyRec);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      AlwaysAssert(p.ok(), AipsError);
      cout << "Passed the record handling test" << endl;
      
    }
    delete shapePtr;
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tPointShape"
// End: 
