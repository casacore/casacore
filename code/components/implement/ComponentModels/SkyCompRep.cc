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
#include <trial/Tasking/MeasureParameterAccessor.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/Exceptions/Error.h>
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Glish/GlishValue.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/UnitVal.h>
#include <aips/Measures/Unit.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/QLogical.h>
#include <aips/Measures/Quantum.h>
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
    const IPosition tileShape(image.niceCursorShape());
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
    ArrayLattice<Float> array(chunkIter.rwCursor());
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
	    elementIter.rwCursor() += Float(pixelVal(0)); break;
	  case Stokes::Q:
	    elementIter.rwCursor() += Float(pixelVal(1)); break;
	  case Stokes::U:
	    elementIter.rwCursor() += Float(pixelVal(2)); break;
	  case Stokes::V:
	    elementIter.rwCursor() += Float(pixelVal(3)); break;
	  }
	}
	else if (elementShape.product() == Int(nStokes))
	  for (uInt p = 0; p < nStokes; p++) {
	    switch (stokes(p)) {
	    case Stokes::I:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(0)); break;
	    case Stokes::Q:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(1)); break;
	    case Stokes::U:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(2)); break;
	    case Stokes::V:
	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(3)); break;
	    }
	  }
	else
	for (uInt p = 0; p < nStokes; p++) {
	  switch (stokes(p)) {
	  case Stokes::I:
	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(0));
	    break;
	  case Stokes::Q:
	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(1));
	    break;
	  case Stokes::U:
	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(2));
	    break;
	  case Stokes::V:
	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(3));
	    break;
	  }
	}
      }
    }
  }
}

void SkyCompRep::setLabel(const String & newLabel) {
  // Use newLabel for something to suppress a compiler warning
  if (newLabel == "") {
  }
}

void SkyCompRep::label(String & compLabel) const {
  compLabel = "";
}

Bool SkyCompRep::ok() const {
  return True;
}

// void SkyCompRep::fromRecord(Quantum<Double> & quantity, String & errorMessage, 
// 			    const GlishRecord & record) {
//   // First extract the value for this quantum.
//   if (!record.exists("value"))
//     errorMessage += "\nThe record does not have a 'value' field";
//   else {
//     if (record.get("value").type() != GlishValue::ARRAY)
//       errorMessage += "\nThe 'value' field cannot be a record";
//     else {
//       GlishArray valField(record.get("value"));
//       if (valField.elementType() == GlishArray::STRING)
//  	errorMessage += "\nThe 'value' field cannot be a string";
//       else {
// 	const IPosition shape = valField.shape();
// 	if (shape.product() != 1)
//  	  errorMessage += "\nThe 'value' field can only have one element";
// 	else {
// 	  Double val;
// 	  if (valField.get(val) == False)
// 	    errorMessage += "\nCould not read the 'value' field "
// 	      "for an unknown reason";
// 	  else
// 	    quantity.setValue(val);
// 	}
//       }
//     }
//   }
//   // Now extract the corresponding unit
//   if (!record.exists("unit"))
//     errorMessage += "\nThe record does not have a 'unit' field";
//   else {
//     if (record.get("unit").type() != GlishValue::ARRAY)
//       errorMessage += "\nThe 'unit' field cannot be a record";
//     else {
//       GlishArray unitField(record.get("unit"));
//       if (unitField.elementType() != GlishArray::STRING)
//  	errorMessage += "\nThe 'unit' field must be a string";
//       else {
// 	const IPosition shape = unitField.shape();
// 	if (shape.product() > 1)
//  	  errorMessage += "\nThe 'unit' field"
// 	    " can only have at most one element";
// 	else {
// 	  if (shape.product() == 0) 
// 	    quantity.setUnit("");
// 	  else { // shape.product() == 1
// 	    String unit;
// 	    if (unitField.get(unit) == False)
// 	      errorMessage += "\nCould not read the 'unit' field"
// 		" for an unknown reason";
// 	    else
// 	      quantity.setUnit(unit);
// 	  }
// 	}
//       }
//     }
//   }
// }

void SkyCompRep::toRecord(GlishRecord & record, 
 			  const Quantum<Double> & quantity) {
  record.add("value", quantity.getValue());
  record.add("unit", quantity.getUnit());
}

Bool SkyCompRep::readDir(String & errorMessage, const GlishRecord & record) {
  // The GlishRecord parameter should really be const but the ParameterAccessor
  // needs a non-const one for an unknown reason
  MeasureParameterAccessor<MDirection> mpa(String("direction"),
					   ParameterSet::In, 
					   (GlishRecord *) &record);
  if (mpa.copyIn(errorMessage) == False) return False;
  setDirection(mpa());
  return True;
}

void SkyCompRep::addDir(GlishRecord & record) const {
  GlishRecord dirRec;
  dirRec.add("type", "direction");
  MDirection compDir;
  direction(compDir);
  {
    const String refFrame = MDirection::showType(compDir.getRef().getType());
    dirRec.add("refer", refFrame);
  }
  {
    const Quantum<Vector<Double> > raDec = compDir.getAngle("deg");
    const Vector<Double> raDecValue = raDec.getValue();
    AlwaysAssert(raDecValue.nelements() == 2, AipsError)
    const String raDecUnit = raDec.getUnit();
    GlishRecord m;
    m.add("value", raDecValue(0));
    m.add("unit", raDecUnit);
    dirRec.add("m0", m);
    m.add("value", raDecValue(1));
    dirRec.add("m1", m);
  }
  record.add("direction", dirRec);
}

Bool SkyCompRep::readFlux(String & errorMessage, const GlishRecord & record) {
  if (record.exists("flux") == False) {
    errorMessage += "\nThe component record does not have a 'flux' field";
    return False;
  }
  if (record.get("flux").type() != GlishValue::RECORD) {
    errorMessage += "\nThe 'flux' field must be a record";
    return False;
  }
  Quantum<Vector<Double> > flux;
  const GlishRecord fluxRec = record.get("flux");
  {
    if (fluxRec.exists("value") == False) {
      errorMessage += "\nThe 'flux' record must have a 'value' field";
      return False;
    }
    if (fluxRec.get("value").type() != GlishValue::ARRAY) {
      errorMessage += "\nThe 'value' field cannot be a record";
      return False;
    }
    const GlishArray valueField = fluxRec.get("value");
    if (valueField.elementType() == GlishArray::STRING) {
      errorMessage += "\nThe 'value' field cannot be a string";
      return False;
    }
    const IPosition shape = valueField.shape();
    if (shape.nelements() != 1 || shape.product() != 4) {
      errorMessage += String("\nThe 'value' field in the flux record ") + 
	String("must contain a vector with 4 elements");
      return False;
    }
    Vector<Double> fluxVal(4);
    if (valueField.get(fluxVal.ac()) == False) {
      errorMessage += String("\nCould not read the 'value' field ") + 
	String("in the flux record for an unknown reason");
      return False;
    }
    flux.setValue(fluxVal);
  }
  {
    if (fluxRec.exists("unit") == False) {
      errorMessage += "\nThe 'flux' record must have a 'unit' field";
      return False;
    }
    if (fluxRec.get("unit").type() != GlishValue::ARRAY) {
      errorMessage += "\nThe 'unit' field cannot be a record";
      return False;
    }
    const GlishArray unitField = fluxRec.get("unit");
    if (unitField.elementType() != GlishArray::STRING) {
      errorMessage += "\nThe 'unit' field must be a string";
      return False;
    }
    if (unitField.shape().product() != 1) {
      errorMessage += String("\nThe 'unit' field cannot be an array ");
      return False;
    }
    String unitVal;
    if (unitField.get(unitVal) == False) {
      errorMessage += String("\nCould not read the 'unit' field ") + 
	String("in the flux record for an unknown reason");
      return False;
    }
    const Unit fluxUnits(unitVal);
    const Unit jy("Jy");
    if (fluxUnits != jy) {
      errorMessage += String("\nThe flux units have the wrong dimensions. ") +
	String("\nThey must be the same as the Jansky.");
      return False;
    }
    flux.setUnit(fluxUnits);
  }
  setFlux(flux);
  return True;
}

void SkyCompRep::addFlux(GlishRecord & record) const {
  Quantum<Vector<Double> > compFlux(Vector<Double>(4), "Jy");
  flux(compFlux);
  GlishRecord fluxRec;
  fluxRec.add("value", GlishArray(compFlux.getValue("Jy").ac()));
  fluxRec.add("unit", "Jy");
  record.add("flux", fluxRec);
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
