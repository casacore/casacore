//# SkyCompRep.cc:  this defines SkyCompRep
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

#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/ArrayLattice.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Glish/GlishValue.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/QLogical.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/Stokes.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <strstream.h>
//#include <iostream.h>

SkyCompRep::~SkyCompRep()
{
}

void SkyCompRep::project(ImageInterface<Float> & image) const {
  const CoordinateSystem coords = image.coordinates();
  const IPosition imageShape = image.shape();
  const uInt naxis = imageShape.nelements();
  
  // I currently REQUIRE that the image has one direction coordinate (only).
  // All other coordinates (ie. polarization and frequency) are optional. 
  const Vector<uInt> dirAxes = CoordinateUtil::findDirectionAxes(coords);
  AlwaysAssert(dirAxes.nelements() != 0, AipsError);
  const uInt nPixAxes = dirAxes.nelements();
  Vector<Double> pixelCoord(nPixAxes); pixelCoord = 0.0;
  Vector<Double> worldCoord(2);

  const DirectionCoordinate dirCoord = 
    coords.directionCoordinate(coords.findCoordinate(Coordinate::DIRECTION));
  MDirection pixelDir(MVDirection(0.0), dirCoord.directionType());
  Vector<Quantum<Double> > dirVal(2);
  MVAngle pixelSize;
  {
    Vector<String> units = dirCoord.worldAxisUnits();
    dirVal(0).setUnit(units(0));
    dirVal(1).setUnit(units(1));
    Vector<Double> inc = dirCoord.increment();
    Quantum<Double> inc0(abs(inc(0)), units(0));
    Quantum<Double> inc1(abs(inc(1)), units(1));
    AlwaysAssert(near(inc0, inc1), AipsError);
    pixelSize = MVAngle(inc0);
  }
  
  // Setup an iterator to step through the image in chunks that can fit into
  // memory. Go to a bit of effort to make the chunck size as large as
  // possible but still minimize the number of tiles in the cache.
  IPosition elementShape = imageShape;
  IPosition chunckShape = imageShape;
  uInt axis;
  {
    const IPosition tileShape(image.niceCursorShape(image.maxPixels()));
    for (uInt k = 0; k < nPixAxes; k++) {
      axis = dirAxes(k);
      elementShape(axis) = 1;
      chunckShape(axis) = tileShape(axis);
    }
  }

  // Check if there is a Stokes Axes and if so which polarizations. Otherwise
  // only grid the I polarisation.
  Vector<Int> stokes; // Vector stating which polarisations is on each plane
  // Find which axis is the stokes pixel axis
  const Int polAxis = CoordinateUtil::findStokesAxis(stokes, coords);  
  const uInt nStokes = stokes.nelements(); 
  if (polAxis >= 0)
    AlwaysAssert(imageShape(polAxis) == Int(nStokes), AipsError);
  for (uInt p = 0; p < nStokes; p++)
    AlwaysAssert(stokes(p) == Stokes::I || stokes(p) == Stokes::Q ||
		 stokes(p) == Stokes::U || stokes(p) == Stokes::V, 
		 AipsError);

  Block<IPosition> blc;
  Block<IPosition> trc;
  if (nStokes > 1) {
    blc.resize(nStokes);
    blc = IPosition(naxis,0);
    trc.resize(nStokes);
    trc = elementShape - 1;
    for (uInt p = 0; p < nStokes; p++) {
      blc[p](polAxis) = p;
      trc[p](polAxis) = p;
    }
  }

  LatticeIterator<Float> chunkIter(image, chunckShape);
  Vector<Double> pixelVal(4);
  IPosition chunkOrigin(naxis), elementPosition(naxis);
  for (chunkIter.reset(); !chunkIter.atEnd(); chunkIter++) {
    ArrayLattice<Float> array(chunkIter.cursor());
    LatticeIterator<Float> elementIter(array, elementShape);
    chunkOrigin = chunkIter.position();
    for (elementIter.reset(); !elementIter.atEnd(); elementIter++) {
      elementPosition = elementIter.position();
      for (uInt k = 0; k < nPixAxes; k++) {
	axis = dirAxes(k);
	pixelCoord(k) = elementPosition(axis) + chunkOrigin(axis);
      }
      if (dirCoord.toWorld(worldCoord, pixelCoord) == False) {
// I am not sure what to do here.
//  	cerr << " SkyCompRep::Pixel at " << pixelCoord 
//  	     << " cannot be projected" << endl;
      }
      else {
	dirVal(0).setValue(worldCoord(0));
	dirVal(1).setValue(worldCoord(1));
	pixelDir.set(MVDirection(dirVal));
	sample(pixelVal, pixelDir, pixelSize);
	if (nStokes == 1) {
	  switch (stokes(0)) {
	  case Stokes::I:
	    elementIter.cursor() += Float(pixelVal(0)); break;
	  case Stokes::Q:
	    elementIter.cursor() += Float(pixelVal(1)); break;
	  case Stokes::U:
	    elementIter.cursor() += Float(pixelVal(2)); break;
	  case Stokes::V:
	    elementIter.cursor() += Float(pixelVal(3)); break;
	  }
	}
	else if (elementShape.product() == Int(nStokes))
	  for (uInt p = 0; p < nStokes; p++) {
	    switch (stokes(p)) {
	    case Stokes::I:
	      elementIter.cursor()(blc[p]) += Float(pixelVal(0)); break;
	    case Stokes::Q:
	      elementIter.cursor()(blc[p]) += Float(pixelVal(1)); break;
	    case Stokes::U:
	      elementIter.cursor()(blc[p]) += Float(pixelVal(2)); break;
	    case Stokes::V:
	      elementIter.cursor()(blc[p]) += Float(pixelVal(3)); break;
	    }
	  }
	else
	for (uInt p = 0; p < nStokes; p++) {
	  switch (stokes(p)) {
	  case Stokes::I:
	    elementIter.cursor()(blc[p], trc[p]).ac() += Float(pixelVal(0));
	    break;
	  case Stokes::Q:
	    elementIter.cursor()(blc[p], trc[p]).ac() += Float(pixelVal(1));
	    break;
	  case Stokes::U:
	    elementIter.cursor()(blc[p], trc[p]).ac() += Float(pixelVal(2));
	    break;
	  case Stokes::V:
	    elementIter.cursor()(blc[p], trc[p]).ac() += Float(pixelVal(3));
	    break;
	  }
	}
	elementIter.writeCursor();
      }
    }
    chunkIter.writeCursor();
  }
}

Bool SkyCompRep::ok() const {
  return True;
}

void SkyCompRep::fromRecord(Quantum<Double> & quantity, String & errorMessage, 
			    const GlishRecord & record) {
  // First extract the value for this quantum.
  if (!record.exists("value"))
    errorMessage += "\nThe record does not have a 'value' field";
  else {
    if (record.get("value").type() != GlishValue::ARRAY)
      errorMessage += "\nThe 'value' field cannot be a record";
    else {
      GlishArray valField(record.get("value"));
      if (valField.elementType() == GlishArray::STRING)
 	errorMessage += "\nThe 'value' field cannot be a string";
      else {
	const IPosition shape = valField.shape();
	if (shape.product() != 1)
 	  errorMessage += "\nThe 'value' field can only have one element";
	else {
	  Double val;
	  if (valField.get(val) == False)
	    errorMessage += "\nCould not read the 'value' field "
	      "for an unknown reason";
	  else
	    quantity.setValue(val);
	}
      }
    }
  }
  // Now extract the corresponding unit
  if (!record.exists("unit"))
    errorMessage += "\nThe record does not have a 'unit' field";
  else {
    if (record.get("unit").type() != GlishValue::ARRAY)
      errorMessage += "\nThe 'unit' field cannot be a record";
    else {
      GlishArray unitField(record.get("unit"));
      if (unitField.elementType() != GlishArray::STRING)
 	errorMessage += "\nThe 'unit' field must be a string";
      else {
	const IPosition shape = unitField.shape();
	if (shape.product() > 1)
 	  errorMessage += "\nThe 'unit' field"
	    " can only have at most one element";
	else {
	  if (shape.product() == 0) 
	    quantity.setUnit("");
	  else { // shape.product() == 1
	    String unit;
	    if (unitField.get(unit) == False)
	      errorMessage += "\nCould not read the 'unit' field"
		" for an unknown reason";
	    else
	      quantity.setUnit(unit);
	  }
	}
      }
    }
  }
}

void SkyCompRep::toRecord(GlishRecord & record, 
			  const Quantum<Double> & quantity) {
  record.add("value", quantity.getValue());
  record.add("unit", quantity.getUnit());
}

void SkyCompRep::fromRecord(MDirection & direction, String & errorMessage, 
			    const GlishRecord & record) {
// MDirection DO_componentlist::
// makeMDirection(String & errorMessage, const GlishRecord & posRec) {
  if (!record.exists("type"))
    errorMessage += "\nThe record does not have a 'type' field";
  else {
    if (record.get("type").type() != GlishValue::ARRAY)
      errorMessage += "\nThe 'type' field cannot be a record";
    else {
      GlishArray typeField(record.get("type"));
      if (typeField.elementType() != GlishArray::STRING)
	errorMessage += "\nThe 'type' field must be a string";
      else {
	if (typeField.shape().product() != 1)
	  errorMessage += "\nThe 'type' field can only have one element";
	else {
	  String measureType;
	  if (typeField.get(measureType) == False)
	    errorMessage += "\nCould not read the 'type' field "
	      "for an unknown reason";
	  else {
	    measureType.downcase();
	    if (measureType != "direction")
	      errorMessage += "\nThe 'type' field MUST be of type direction."
		" The actual value is "+ measureType;
	  }
	}
      }
    }
  }
  if (!record.exists("refer"))
    errorMessage += "\nThe record does not have a 'refer' field";
  else {
    if (record.get("refer").type() != GlishValue::ARRAY)
      errorMessage += "\nThe 'refer' field cannot be a record";
    else {
      GlishArray referField(record.get("refer"));
      if (referField.elementType() != GlishArray::STRING)
	errorMessage += "\nThe 'refer' field must be a string";
      else {
	if (referField.shape().product() != 1)
	  errorMessage += "\nThe 'refer' field can only have one element";
	else {
	  String referType;
	  if (referField.get(referType) == False)
	    errorMessage += "\nCould not read the 'refer' field"
	      " for an unknown reason";
	  else {
	    referType.downcase();
	    MDirection::Ref refEnum;
	    if (direction.giveMe(referType, refEnum) == False)
	    errorMessage += "\nCould not translate the 'refer' field value"
	      " of " + referType + " into a known reference frame";
	    else
	      direction.set(refEnum);
	  }
	}
      }
    }
  }
  String dirErrors = "";
  if (!(record.exists("m0") && record.exists("m1")))
    dirErrors += "\nThe record does not have 'm0' and 'm1' fields";
  else {
    if (record.get("m0").type() != GlishValue::RECORD || 
	record.get("m1").type() != GlishValue::RECORD)
      dirErrors += "\nBoth the 'm0' and 'm1' fields"
	", in the position record, must be a records";
    else {
      GlishRecord mRec = record.get("m0");
      String qErrors = "";
      Quantum<Double> ra;
      fromRecord(ra, qErrors, mRec);
      if (qErrors != "") {
	dirErrors += "\nThe following errors occured in reading the m0 record";
	dirErrors += qErrors;
	dirErrors += "\n";
	qErrors = "";
      }
      mRec = record.get("m1");
      Quantum<Double> dec;
      fromRecord(dec, qErrors, mRec);
      if (qErrors != "") {
	dirErrors += "\nThe following errors occured in reading the m1 record";
	dirErrors += qErrors;
	dirErrors += "\n";
      }
      if (ra.check(UnitVal::ANGLE) == True &&
	  dec.check(UnitVal::ANGLE) == True)
	direction.set(MVDirection(ra, dec));
      else 
	dirErrors += "\nAt least one of the 'm0' and 'm1' fields"
	  ", does not have angular units";
    }
  }
  if (dirErrors != "") {
    if (!(record.exists("ev0") && 
	  record.exists("ev1") && record.exists("ev2"))) {
      dirErrors += "\nThe record does not have 'ev0', 'ev1' and 'ev2' fields";
    }
    else {
      if (record.get("ev0").type() != GlishValue::RECORD || 
	  record.get("ev1").type() != GlishValue::RECORD ||
	  record.get("ev1").type() != GlishValue::RECORD)
	dirErrors += "\nAll of the 'ev0', 'ev1' and 'ev2' fields"
	  " must be a records";
      else {
	GlishRecord evRec(record.get("ev0"));
	String qErrors = "";
	Vector<Quantum<Double> > lmn(3);
	fromRecord(lmn(0), qErrors, evRec);
	if (qErrors != "") {
	  dirErrors += "\nThe following errors occured in reading the"
	    " ev0 record";
	  dirErrors += qErrors;
	  dirErrors += "\n";
	  qErrors = "";
	}
	evRec = record.get("ev1");
	fromRecord(lmn(1), qErrors, evRec);
	if (qErrors != "") {
	  dirErrors += "\nThe following errors occured in reading the"
	    " ev1 record";
	  dirErrors += qErrors;
	  dirErrors += "\n";
	  qErrors = "";
	}
	evRec = record.get("ev2");
	fromRecord(lmn(2), qErrors, evRec);
	if (qErrors != "") {
	  dirErrors += "\nThe following errors occured in reading the"
	    " ev2 record";
	  dirErrors += qErrors;
	  dirErrors += "\n";
	  qErrors = "";
	}
 	if (lmn(0).check(UnitVal::NODIM) &&
 	    lmn(1).check(UnitVal::NODIM) &&
 	    lmn(2).check(UnitVal::NODIM)) {
 	  direction.set(MVDirection(lmn));
	  dirErrors = "";
	}
 	else
 	  dirErrors += "\nAt least one of the 'ev0', 'ev1' or 'ev2' fields"
 	    ", is not dimensionless";
      }
    }
  }
  if (dirErrors != "")
    errorMessage += dirErrors;
}

void SkyCompRep::toRecord(GlishRecord & record, 
			  const MDirection & direction) {
  record.add("type", "direction");
  {
    const String refFrame = MDirection::showType(direction.getRef().getType());
    record.add("refer", refFrame);
  }
  {
    const Quantum<Vector<Double> > raDec = direction.getAngle("deg");
    const Vector<Double> raDecValue = raDec.getValue();
    AlwaysAssert(raDecValue.nelements() == 2, AipsError)
    const String raDecUnit = raDec.getUnit();
    GlishRecord m;
    m.add("value", raDecValue(0));
    m.add("unit", raDecUnit);
    record.add("m0", m);
    m.add("value", raDecValue(1));
    record.add("m1", m);
  }
  {
    const Vector<Double> dirCosValue = direction.getValue().getValue();
    AlwaysAssert(dirCosValue.nelements() == 3, AipsError)
    GlishRecord ev;
    ev.add("value", dirCosValue(0));
    ev.add("unit", "");
    record.add("ev0", ev);
    ev.add("value", dirCosValue(1));
    record.add("ev1", ev);
    ev.add("value", dirCosValue(2));
    record.add("ev2", ev);
  }
}

void SkyCompRep::readFlux(Vector<Double> & flux, String & errorMessage,
			  const GlishRecord & record) {
  AlwaysAssert(flux.nelements() == 0 || 
	       flux.nelements() == 4, AipsError);
  if (!record.exists("flux"))
    errorMessage += "\nThe record does not have a 'flux' field";
  else {
    if (record.get("flux").type() != GlishValue::ARRAY)
      errorMessage += "\nThe 'flux' field cannot be a record";
    else {
      const GlishArray fluxField = record.get("flux");
      if (fluxField.elementType() == GlishArray::STRING)
	errorMessage += "\nThe 'flux' field cannot be a string";
      else {
	const IPosition shape = fluxField.shape();
	if (shape.nelements() != 1 || shape.product() != 4)
	  errorMessage += "\nThe 'flux' field"
	    " must be a vector with 4 elements";
	else {
	  if (fluxField.get(flux.ac()) == False)
	    errorMessage += "\nCould not read the 'flux' field"
	      " for an unknown reason";
	}
      }
    }
  }
}

void SkyCompRep::addFlux(GlishRecord & record) const {
  Vector<Double> compFlux(4);
  flux(compFlux);
  record.add("flux", GlishArray(compFlux.ac()));
}

void SkyCompRep::readParameters(Vector<Double> & parameters, 
				String & errorMessage,
				const GlishRecord & record) const {
  AlwaysAssert(parameters.nelements() == 0 || 
	       parameters.nelements() == nParameters(), AipsError);
  if (!record.exists("parameters"))
    errorMessage += "\nThe record does not have a 'parameters' field";
  else {
    if (record.get("parameters").type() != GlishValue::ARRAY)
      errorMessage += "\nThe 'parameters' field cannot be a record";
    else {
      const GlishArray parmField = record.get("parameters");
      if (parmField.elementType() == GlishArray::STRING)
	errorMessage += "\nThe 'parameters' field cannot be a string";
      else {
	const IPosition shape = parmField.shape();
	if (shape.nelements() != 1 || shape.product() != Int(nParameters())) {
	  ostrstream buffer; buffer << nParameters();
	  errorMessage += 
	    String("\nThe 'parameters' field must be a vector with ") +
	    buffer + String(" elements");
	}
	else {
	  if (parmField.get(parameters.ac()) == False)
	    errorMessage += "\nCould not read the 'parameters' field"
	      " for an unknown reason";
	}
      }
    }
  }
}

void SkyCompRep::addParameters(GlishRecord & record) const {
  Vector<Double> parms(nParameters());
  parameters(parms);
  record.add("parameters", GlishArray(parms.ac()));
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 SkyCompRep"
// End: 
