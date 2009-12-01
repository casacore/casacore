//# tGaussianShape.cc: Test program for the GaussianShape & TwoSidedShape classes
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/GaussianShape.h>
#include <components/ComponentModels/TwoSidedShape.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MeasureHolder.h>
#include <casa/Quanta/Euler.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Quanta/RotMatrix.h>
#include <casa/Utilities/DataType.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
void pixelWorldConversion ();

Bool pixelReflection (const TwoSidedShape& shp, const DirectionCoordinate& dCoord, 
                     Double tol=1.0e-5);


int main() {
  try {
    TwoSidedShape* shapePtr = 0;
    {
      // Create a Gaussian shape at the default direction
      const GaussianShape defGaussian;
      AlwaysAssert(defGaussian.ok(), AipsError);
      AlwaysAssert(defGaussian.type() == ComponentType::GAUSSIAN, AipsError);
      AlwaysAssert(defGaussian.isSymmetric() == True, AipsError);
      const MVAngle pixelSize(Quantity(1.0,"''"));
      // Sample the Gaussian at the Maximum and half an arc-min on either side.
      MVDirection sampleDirVal(Quantity(0,"deg"), 
  			       Quantity(90, "deg") - Quantity(.5, "'"));
      MDirection sampleDir(sampleDirVal, MDirection::J2000);
      const RotMatrix rotDec((Euler(Quantity(0.5, "'").getValue("rad"), 2u)));
   
      // This is not exact. To be exact I should do a integration over the
      // pixel area. Instead I set the pixel size to be something small enough!
      const Double peak = 4. * 3600 * pow(180.,2.) * C::ln2 * pow(C::pi,-3.0) *
  	square(pixelSize.radian());
      AlwaysAssert(near(defGaussian.sample(sampleDir, pixelSize, pixelSize), 
			peak*0.5, 1E-11), AipsError);
      sampleDirVal *= rotDec;
      sampleDir.set(sampleDirVal);
      AlwaysAssert(near(defGaussian.sample(sampleDir, pixelSize, pixelSize), 
			peak, 1E-12), AipsError);
      sampleDirVal *= rotDec;
      sampleDir.set(sampleDirVal);
      const MVAngle halfPix = pixelSize.radian()/2.0;
      AlwaysAssert(near(defGaussian.sample(sampleDir, pixelSize, halfPix), 
			peak*0.25, 1E-11), AipsError);
      cout << "Passed the default Gaussian shape test" << endl;
    }
    {
      // Create a Gaussian shape at a defined non-J2000 direction
      const MVDirection dir1934(Quantity(293.5,"deg"), Quantity(-63.7, "deg"));
      const MDirection coord1934J2000(dir1934, MDirection::J2000);
      const Quantity majorAxis(2, "'");
      const Quantity minorAxis(2, "'");
      const Quantity pa(10, "deg");
      GaussianShape J1934(coord1934J2000, majorAxis, minorAxis, pa);
      // Create a direction that is 1 arc-min away from the pole
      MVDirection sampleDir(Quantity(0,"deg"),
   			    Quantity(90, "deg") - Quantity(1, "'"));
      // And now make another rotater that can rotate this point about the pole
      // in steps of say 40 degrees
      const RotMatrix rotater((Euler(Quantity(40, "deg").getValue("rad"),
				     3u)));

      // Create a rotation matrix that can rotate the pole down to the
      // component. 
      const RotMatrix pole2src((Euler(Quantity(-153.7,"deg").getValue("rad"),
				      2u, 
				      Quantity(-293.5,"deg").getValue("rad"),
				      3u)));
      // Sample at a set of MDirections equidistant from the direction of the
      // component. All these points should have the same flux of half the
      // maximum.
      const MVAngle pixelSize(Quantity(1.0,"''"));
      MDirection sampledDirection;
      const Double peak = 4. * square(180. * 60. /2.) * C::ln2 * 
	pow(C::pi,-3.) * square(pixelSize.radian());
      for (uInt i = 0; i < 6; i++){
	sampledDirection = MDirection(sampleDir*pole2src, MDirection::J2000);
 	AlwaysAssert(near(J1934.sample(sampledDirection, pixelSize,pixelSize), 
			  peak*0.5, 1E-11), AipsError);
   	sampleDir *= rotater;
       }
      const MVDirection pole;
      sampledDirection = MDirection(pole*pole2src, MDirection::J2000);
      AlwaysAssert(near(J1934.sample(sampledDirection, pixelSize, pixelSize),
			peak), AipsError);
      cout << "Passed the arbitrary Gaussian shape test" << endl;
      // test the copy semantics
      GaussianShape othergc(J1934);
      GaussianShape assignedgc;
      assignedgc = othergc;
      shapePtr = dynamic_cast<TwoSidedShape*>(J1934.clone());
      J1934.setWidthInRad(1., 0.5, .1);
      shapePtr->setWidth(Quantity(1000, "mas"), Quantity(0.5, "arcsec"), 
			 Quantity(-10, "deg"));
      othergc.setWidth(Quantity(5, "deg"), 0.5, Quantity(1, "rad"));
      {
 	AlwaysAssert(near(assignedgc.majorAxis().getValue("arcmin"), 2.0),
		     AipsError);
	AlwaysAssert(assignedgc.majorAxis().getFullUnit().getName() == "'",
		     AipsError);
 	AlwaysAssert(near(assignedgc.minorAxis().getValue("arcmin"), 2.0),
		     AipsError);
	AlwaysAssert(assignedgc.minorAxis().getFullUnit().getName() == "'",
		     AipsError);
 	AlwaysAssert(near(assignedgc.positionAngle().getValue("deg"), 10.0),
		     AipsError);
	AlwaysAssert(assignedgc.positionAngle().getFullUnit().getName() =="deg",
		     AipsError);
      }
      {
 	AlwaysAssert(near(othergc.majorAxis().getValue("deg"), 5.0),
		     AipsError);
	AlwaysAssert(othergc.majorAxis().getFullUnit().getName() == "deg",
		     AipsError);
 	AlwaysAssert(near(othergc.axialRatio(), 0.5), AipsError);
 	AlwaysAssert(near(othergc.TwoSidedShape::axialRatio(), 0.5), AipsError);
	AlwaysAssert(othergc.minorAxis().getFullUnit().getName() == "deg",
		     AipsError);
 	AlwaysAssert(near(othergc.positionAngle().getValue("rad"), 1.0),
		     AipsError);
	AlwaysAssert(othergc.positionAngle().getFullUnit().getName() =="rad",
		     AipsError);
      }
      {
 	AlwaysAssert(near(J1934.majorAxisInRad(), 1.0), AipsError);
 	AlwaysAssert(near(J1934.minorAxisInRad(), 0.5), AipsError);
 	AlwaysAssert(near(J1934.positionAngleInRad(), 0.1), AipsError);
	AlwaysAssert(J1934.majorAxis().getFullUnit().getName() == "'",
		     AipsError);
	AlwaysAssert(J1934.minorAxis().getFullUnit().getName() == "'",
		     AipsError);
	AlwaysAssert(J1934.positionAngle().getFullUnit().getName() =="deg",
		     AipsError);
      }
      {
 	AlwaysAssert(near(shapePtr->majorAxis().getValue("mas"), 1000.0),
		     AipsError);
	AlwaysAssert(shapePtr->majorAxis().getFullUnit().getName() == "mas",
		     AipsError);
 	AlwaysAssert(near(shapePtr->minorAxis().getValue("arcsec"), 0.5),
		     AipsError);
	AlwaysAssert(shapePtr->minorAxis().getFullUnit().getName() == "arcsec",
		     AipsError);
 	AlwaysAssert(near(shapePtr->positionAngle().getValue("deg"), 170.0),
		     AipsError);
	AlwaysAssert(shapePtr->positionAngle().getFullUnit().getName() =="deg",
		     AipsError);
	AlwaysAssert(shapePtr->type() == ComponentType::GAUSSIAN, AipsError);
	AlwaysAssert(shapePtr->isSymmetric() == True, AipsError);
      }
      cout << "Passed the copy semantics test" << endl;
    }
    {
      MDirection dir(MVDirection(0.0, 0.0), MDirection::J2000);
      GaussianShape gc(dir, Quantity(1, "deg"), 0.5, Quantity(0, "deg"));

      const MVAngle pixSize(Quantity(1, "mas"));
      Vector<MDirection::MVType> dirs(5);
      dirs(0) = MVDirection(Quantity(0,"deg"), Quantity(0, "deg"));
      dirs(1) = MVDirection(Quantity(0.25,"deg"), Quantity(0, "deg"));
      dirs(2) = MVDirection(Quantity(-0.25,"deg"), Quantity(0, "deg"));
      dirs(3) = MVDirection(Quantity(0.,"deg"), Quantity(0.5, "deg"));
      dirs(4) = MVDirection(Quantity(0.,"deg"), Quantity(-0.5, "deg"));
      MDirection::Ref ref(shapePtr->refDirection().getRef());
      Vector<Double> scales(5, -1.0);
      gc.sample(scales, dirs, ref, pixSize, pixSize);
      const Double peak = 2. * square(6.*60) * C::ln2 * 
	pow(C::pi,-3.) * square(pixSize.radian());
      AlwaysAssert(near(scales(0), peak), AipsError);
      AlwaysAssert(near(scales(1), peak/2), AipsError);
      AlwaysAssert(near(scales(2), peak/2), AipsError);
      AlwaysAssert(near(scales(3), peak/2), AipsError);
      AlwaysAssert(near(scales(4), peak/2), AipsError);
      scales = -1.0;
      gc.TwoSidedShape::sample(scales, dirs, ref, pixSize, pixSize);
      AlwaysAssert(near(scales(0), peak), AipsError);
      AlwaysAssert(near(scales(1), peak/2), AipsError);
      AlwaysAssert(near(scales(2), peak/2), AipsError);
      AlwaysAssert(near(scales(3), peak/2), AipsError);
      AlwaysAssert(near(scales(4), peak/2), AipsError);

      Vector<Double> uvw(3,0.0);
      Double freq = 1E6;
      AlwaysAssert(near(gc.visibility(uvw, freq).real(), 1.0), AipsError);
      AlwaysAssert(near(gc.visibility(uvw, freq).imag(), 0.0), AipsError);
      uvw(0) = 4*C::ln2/square(C::pi)*180.*C::c/freq;
      AlwaysAssert(near(gc.visibility(uvw, freq).real(), 0.5), AipsError);
      AlwaysAssert(near(gc.visibility(uvw, freq).imag(), 0.0), AipsError);
      uvw(1) = uvw(0)/2; uvw(0) = 0.0;
      AlwaysAssert(near(gc.visibility(uvw, freq).real(), 0.5), AipsError);
      AlwaysAssert(near(gc.visibility(uvw, freq).imag(), 0.0), AipsError);
      
      Matrix<Double> uvws(3, 2);
      uvws = 0.0; 
      uvws(0,0) = -uvw(1)*2;
      uvws(1,1) = -uvw(1);
      Vector<DComplex> results(2, DComplex(10.0, 10.0));
      gc.visibility(results, uvws, freq);
      AlwaysAssert(near(results(0).real(), 0.5), AipsError);
      AlwaysAssert(near(results(0).imag(), 0.0), AipsError);
      AlwaysAssert(near(results(1).real(), 0.5), AipsError);
      AlwaysAssert(near(results(1).imag(), 0.0), AipsError);
      results = DComplex(10.,10.);
      gc.TwoSidedShape::visibility(results, uvws, freq);
      AlwaysAssert(near(results(0).real(), 0.5), AipsError);
      AlwaysAssert(near(results(0).imag(), 0.0), AipsError);
      AlwaysAssert(near(results(1).real(), 0.5), AipsError);
      AlwaysAssert(near(results(1).imag(), 0.0), AipsError);

      cout << "Passed the sample/visibility test" << endl;
    }
    {
      AlwaysAssert(shapePtr->nParameters() == 3, AipsError);
      Vector<Double> v = shapePtr->parameters().copy();
      AlwaysAssert(near(v(0), 1.0/60/60/180.*C::pi), AipsError);
      AlwaysAssert(near(v(1), 0.5/60/60/180.*C::pi), AipsError);
      AlwaysAssert(near(v(2), 170.0/180.*C::pi), AipsError);
      v(0) = 1.0/180*C::pi;
      v(1) = 0.1/180*C::pi;
      v(2) = 10.0/180*C::pi;
      shapePtr->setParameters(v);
      AlwaysAssert(near(shapePtr->majorAxis().getValue("deg"), 1.0), AipsError);
      AlwaysAssert(near(shapePtr->minorAxis().getValue("deg"), 0.1), AipsError);
      AlwaysAssert(near(shapePtr->positionAngle().getValue("deg"), 10.0), 
 		   AipsError);
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
      try{
  	v = shapePtr->parameters();
  	throw(AipsError("Incorrect parameter vector exception NOT thrown"));
      }
      catch (AipsError x) {
  	if(!x.getMesg().contains("compParms.nelements() == nParameters()")) {
  	  throw;
  	}
      }
#endif
      cout << "Passed the parameter interface test" << endl;
    }
    {
      String errorMsg;
      Record rec;
      AlwaysAssert(shapePtr->toRecord(errorMsg, rec), AipsError);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      AlwaysAssert(rec.isDefined("type"), AipsError);
      AlwaysAssert(rec.isDefined("direction"), AipsError);
      const RecordFieldId direction("direction");
      AlwaysAssert(rec.dataType(direction) == TpRecord, AipsError);
      const Record dRec = rec.asRecord(direction);
      AlwaysAssert(dRec.isDefined("error"), AipsError);
      const RecordFieldId error("error");
      AlwaysAssert(dRec.dataType(error) == TpRecord, AipsError);
      const Record eRec = dRec.asRecord(error);
      AlwaysAssert(eRec.isDefined("latitude"), AipsError);
      AlwaysAssert(eRec.isDefined("longitude"), AipsError);
      AlwaysAssert(rec.isDefined("majoraxis"), AipsError);
      AlwaysAssert(rec.isDefined("majoraxiserror"), AipsError);
      AlwaysAssert(rec.isDefined("minoraxis"), AipsError);
      AlwaysAssert(rec.isDefined("minoraxiserror"), AipsError);
      AlwaysAssert(rec.isDefined("positionangle"), AipsError);
      AlwaysAssert(rec.isDefined("positionangleerror"), AipsError);
      String type;
      rec.get(RecordFieldId("type"), type);
      AlwaysAssert(type == "Gaussian", AipsError);
      {
	Record qRec = rec.asRecord(RecordFieldId("majoraxis"));
	QuantumHolder qh;
	qh.fromRecord(errorMsg, qRec);
	AlwaysAssert(errorMsg.length() == 0, AipsError);
	AlwaysAssert(qh.isQuantity(), AipsError);
	Quantum<Double> q = qh.asQuantity();
	AlwaysAssert(near(q.getValue("deg"), 1.0), AipsError);
	AlwaysAssert(q.getFullUnit().getName() == "mas", AipsError);
      }
      {
	Record qRec = rec.asRecord(RecordFieldId("minoraxis"));
	QuantumHolder qh;
	qh.fromRecord(errorMsg, qRec);
	AlwaysAssert(errorMsg.length() == 0, AipsError);
	AlwaysAssert(qh.isQuantity(), AipsError);
	Quantum<Double> q = qh.asQuantity();
	AlwaysAssert(near(q.getValue("deg"), 0.1), AipsError);
	AlwaysAssert(q.getFullUnit().getName() == "arcsec", AipsError);
      }
      {
	Record qRec = rec.asRecord(RecordFieldId("positionangle"));
	QuantumHolder qh;
	qh.fromRecord(errorMsg, qRec);
	AlwaysAssert(errorMsg.length() == 0, AipsError);
	AlwaysAssert(qh.isQuantity(), AipsError);
	Quantum<Double> q = qh.asQuantity();
	AlwaysAssert(near(q.getValue("deg"), 10.), AipsError);
	AlwaysAssert(q.getFullUnit().getName() == "deg", AipsError);
      }
      {
 	Record unitRec;
	unitRec.define(RecordFieldId("majoraxis"), "deg");
	unitRec.define(RecordFieldId("minoraxis"), "arcmin");
	unitRec.define(RecordFieldId("positionangle"), "rad");
	AlwaysAssert(shapePtr->convertUnit(errorMsg, unitRec), AipsError);
	AlwaysAssert(errorMsg.length() == 0, AipsError);
      }
      AlwaysAssert(shapePtr->toRecord(errorMsg, rec), AipsError);
      {
	Record qRec = rec.asRecord(RecordFieldId("majoraxis"));
	QuantumHolder qh;
	qh.fromRecord(errorMsg, qRec);
	Quantum<Double> q = qh.asQuantity();
	AlwaysAssert(near(q.getValue("deg"), 1.0), AipsError);
	AlwaysAssert(q.getFullUnit().getName() == "deg", AipsError);
      }
      {
	Record qRec = rec.asRecord(RecordFieldId("minoraxis"));
	QuantumHolder qh;
	qh.fromRecord(errorMsg, qRec);
	Quantum<Double> q = qh.asQuantity();
	AlwaysAssert(near(q.getValue("deg"), 0.1), AipsError);
	AlwaysAssert(q.getFullUnit().getName() == "arcmin", AipsError);
      }
      {
	Record qRec = rec.asRecord(RecordFieldId("positionangle"));
	QuantumHolder qh;
	qh.fromRecord(errorMsg, qRec);
	Quantum<Double> q = qh.asQuantity();
	AlwaysAssert(near(q.getValue("deg"), 10.), AipsError);
	AlwaysAssert(q.getFullUnit().getName() == "rad", AipsError);
      }
      Record shapeRec;
      {
	QuantumHolder qh(Quantum<Double>(3, "arcmin"));
	Record rec;
	qh.toRecord(errorMsg, rec);
	shapeRec.defineRecord(RecordFieldId("majoraxis"), rec);
	qh = QuantumHolder(Quantum<Double>(1, "arcmin"));
	qh.toRecord(errorMsg, rec);
	shapeRec.defineRecord(RecordFieldId("majoraxiserror"), rec);
      }
      {
	QuantumHolder qh(Quantum<Double>(20, "arcsec"));
	Record rec;
	qh.toRecord(errorMsg, rec);
	shapeRec.defineRecord(RecordFieldId("minoraxis"), rec);
	qh = QuantumHolder(Quantum<Double>(5, "arcsec"));
	qh.toRecord(errorMsg, rec);
	shapeRec.defineRecord(RecordFieldId("minoraxiserror"), rec);
      }
      {
	QuantumHolder qh(Quantum<Double>(2, "deg"));
	Record rec;
	qh.toRecord(errorMsg, rec);
	shapeRec.defineRecord(RecordFieldId("positionangle"), rec);
	qh = QuantumHolder(Quantum<Double>(.5, "deg"));
	qh.toRecord(errorMsg, rec);
	shapeRec.defineRecord(RecordFieldId("positionangleerror"), rec);
      }
      {
	MVDirection val(Quantity(2, "rad"), Quantity(1, "rad"));
	MeasureHolder mh(MDirection(val, MDirection::B1950));
	Record rec;
	mh.toRecord(errorMsg, rec);
	shapeRec.defineRecord(RecordFieldId("direction"), rec);
      }
      shapeRec.define(RecordFieldId("type"), "gAUssian");
      AlwaysAssert(shapePtr->fromRecord(errorMsg, shapeRec), AipsError);
      AlwaysAssert(errorMsg.length() == 0, AipsError);
      AlwaysAssert(near(shapePtr->majorAxis().getValue("'"), 3.), AipsError);
      AlwaysAssert(shapePtr->majorAxis().getFullUnit().getName() =="arcmin",
		   AipsError);
      AlwaysAssert(near(shapePtr->minorAxis().getValue("''"), 20.), AipsError);
      AlwaysAssert(shapePtr->minorAxis().getFullUnit().getName() =="arcsec",
		   AipsError);
      AlwaysAssert(near(shapePtr->positionAngle().getValue("deg"), 2.0), 
		   AipsError);
      
      AlwaysAssert(shapePtr->positionAngle().getFullUnit().getName() =="deg",
		   AipsError);
      AlwaysAssert(shapePtr->type() == ComponentType::GAUSSIAN, AipsError);
      Vector<Double> v = shapePtr->refDirection().getValue().get();
      AlwaysAssert(near(v(0), 2.), AipsError);
      AlwaysAssert(near(v(1), 1.), AipsError);
      cout << "Passed the record handling test" << endl;
    }
    delete shapePtr;

// pixel<->world conversions

   pixelWorldConversion();

  } catch (AipsError x) {
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
}



void pixelWorldConversion ()
{
         
// Make DirectionCoordinate
    cout << "bbb1" << endl;
       
      Matrix<Double> xform(2,2);
      xform = 0.0; xform.diagonal() = 1.0;
      Vector<Double> incr(2), rp(2), rv(2);
      incr(0) = -3.8785096e-05;
      incr(1) = 5.817764e-05;
      rp(0) = 55.0;
      rp(1) = 37.0;
      rv(0) = 0.0;
      rv(1) = 0.0;
    cout << "bbb2" << endl;
      DirectionCoordinate dirCoord(MDirection::J2000, Projection::SIN,
                                   rv(0), rv(1),
                                   incr(0), incr(1),   
                                   xform, rp(0), rp(1)); 
    cout << "bbb3" << endl;
// Now shape
         
      Quantum<Double> majorAxis(72.0, String("arcsec"));
      Quantum<Double> minorAxis(60.0, String("arcsec"));
      Quantum<Double> pa(111.0, String("deg"));
    cout << "bbb4" << endl;
       
// Try reflection test for a few directions

      {
         cerr << "0 0" << endl;
         MDirection dir(MVDirection(0.0, 0.0), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         pixelReflection(gc, dirCoord);
      }
      {
         cerr << "2*inc" << endl;
         MDirection dir(MVDirection(2*incr(0), 2*incr(1)), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa); 
         pixelReflection(gc, dirCoord);
      }
      {
         cerr << "10*inc" << endl;
         MDirection dir(MVDirection(10*incr(0), 10*incr(1)), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         pixelReflection(gc, dirCoord);
      }   
      {
         cerr << "100*inc" << endl;
         MDirection dir(MVDirection(100*incr(0), 100*incr(1)), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         pixelReflection(gc, dirCoord, 1.0e-3);
      }
      {
         cerr << "-0.1 0.1" << endl;
         MDirection dir(MVDirection(-0.1, 0.1), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         pixelReflection(gc, dirCoord, 1.e-2);
      }   
      {
         cerr << "0.1 -0.1" << endl;
         MDirection dir(MVDirection(0.1, -0.1), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         pixelReflection(gc, dirCoord, 1.0e-2);
      }
      {
         cerr << "-0.1 -0.1" << endl;
         MDirection dir(MVDirection(-0.1, -0.1), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         pixelReflection(gc, dirCoord, 1.0e-2);
      }  
      {
         cerr << "0.1 0.1" << endl;
         MDirection dir(MVDirection(0.1, 0.1), MDirection::J2000);
         GaussianShape gc(dir, majorAxis, minorAxis, pa);
         pixelReflection(gc, dirCoord, 1.0e-2);
      }
}


Bool pixelReflection (const TwoSidedShape& shp, const DirectionCoordinate& dirCoord, Double tol)
{
   Vector<Double> pars1 = shp.toPixel(dirCoord);
   AlwaysAssert(pars1.nelements()==5, AipsError);
//
   GaussianShape shp2;
   shp2.fromPixel (pars1, dirCoord);
// 
   Vector<Double> pars2 = shp2.toPixel(dirCoord);
/*
   cerr << pars1 << endl;
   cerr << pars2 << endl;
*/
   AlwaysAssert(allNear(pars1, pars2, tol), AipsError);
   return True;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tGaussianShape"
// End:

