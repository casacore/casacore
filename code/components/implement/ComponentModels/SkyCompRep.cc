//# SkyCompRep.cc:  this defines SkyCompRep
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

#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/ComponentModels/Flux.h>
#include <trial/Coordinates/Coordinate.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/ArrayLattice.h>
#include <trial/Lattices/LatticeIterator.h>
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
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/QLogical.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/Stokes.h>
#include <aips/Measures/Unit.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <strstream.h>
#include <iostream.h>
#include <iomanip.h>

SkyCompRep::~SkyCompRep() {
}

ComponentType::Shape SkyCompRep::shape() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::POINT;
}

void SkyCompRep::setDirection(const MDirection & newDirection) {
  DebugAssert(ok(), AipsError);
  // Use newDirection for something to suppress a compiler warning
  if (&newDirection == 0) {
  }
}
  
void SkyCompRep::direction(MDirection & compDirection) const {
  DebugAssert(ok(), AipsError);
  // Use compDirection for something to suppress a compiler warning
  if (&compDirection == 0) {
  }
}

uInt SkyCompRep::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 0;
}

void SkyCompRep::setParameters(const Vector<Double> & newParms) {
  DebugAssert(newParms.nelements() == nParameters(), AipsError);
}

void SkyCompRep::parameters(Vector<Double> & compParms) const {
  DebugAssert(compParms.nelements() == nParameters(), AipsError);
}

ComponentType::SpectralShape SkyCompRep::spectralShape() const {
  DebugAssert(ok(), AipsError);
  return SpectralModel::spectralShape();
}

void SkyCompRep::setRefFrequency(const MFrequency & newRefFreq) {
  // Use newRefFreq for something to suppress a compiler warning
  SpectralModel::setRefFrequency(newRefFreq);
  DebugAssert(ok(), AipsError);
}
  
const MFrequency & SkyCompRep::refFrequency() const {
  DebugAssert(ok(), AipsError);
  // Use refFreq for something to suppress a compiler warning
  return SpectralModel::refFrequency();
}

Double SkyCompRep::scale(const MFrequency & sampleFreq) const {
  DebugAssert(ok(), AipsError);
  return SpectralModel::scale(sampleFreq);
}

Flux<Double> SkyCompRep::sample(const MFrequency & sampleFreq) const {
  DebugAssert(ok(), AipsError);
  Flux<Double> scaledFlux(flux().copy());
  scaledFlux.scaleValue(SpectralModel::scale(sampleFreq));
  return scaledFlux;
}

uInt SkyCompRep::nSpectralParameters() const {
  DebugAssert(ok(), AipsError);
  return SpectralModel::nSpectralParameters();
}

void SkyCompRep::setSpectralParameters(const Vector<Double> & newParms) {
  SpectralModel::setSpectralParameters(newParms);
  DebugAssert(ok(), AipsError);
}

void SkyCompRep::spectralParameters(Vector<Double> & compParms) const {
  DebugAssert(ok(), AipsError);
  SpectralModel::spectralParameters(compParms);
}

void SkyCompRep::setLabel(const String & newLabel) {
  DebugAssert(ok(), AipsError);
  // Use newLabel for something to suppress a compiler warning
  if (&newLabel == 0) {
  }
}

void SkyCompRep::label(String & compLabel) const {
  DebugAssert(ok(), AipsError);
  // Use compLabel for something to suppress a compiler warning
  if (&compLabel == 0) {
  }
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
      if (!dirCoord.toWorld(worldCoord, pixelCoord)) {
// I am not sure what to do here, probably this message should be logged.
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

Bool SkyCompRep::ok() const {
  return True;
}

// Use a QuantumParameterAccessor for this function and complain to Wim about
// this function not being available in the Measures module!
void SkyCompRep::toRecord(GlishRecord & record, 
 			  const Quantum<Double> & quantity) {
  record.add("value", quantity.getValue());
  record.add("unit", quantity.getUnit());
}

// Use functions in the measures class for this.
Bool SkyCompRep::readDir(String & errorMessage, const GlishRecord & record) {
  // The GlishRecord parameter should really be const but the ParameterAccessor
  // needs a non-const one for an unknown reason
  MeasureParameterAccessor<MDirection> mpa(String("direction"),
					   ParameterSet::In, 
					   (GlishRecord *) &record);
  if (!mpa.copyIn(errorMessage)) return False;
  setDirection(mpa());
  return True;
}

// Use functions in the measures class for this or at the bare minumum use a
// MeasureParameterAccessor<MDirection> class.
Bool SkyCompRep::addDir(String & errorMessage, GlishRecord & record) const {
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
  if (errorMessage == ""); // Suppress compiler warning about unused variable
  return True;
}

// Move into the Flux<T> class.
Bool SkyCompRep::readFlux(String & errorMessage, const GlishRecord & record) {
  if (!record.exists("flux")) {
    errorMessage += "\nThe component record does not have a 'flux' field";
    return False;
  }
  if (record.get("flux").type() != GlishValue::RECORD) {
    errorMessage += "\nThe 'flux' field must be a record";
    return False;
  }
  Flux<Double> & thisFlux = flux();
  const GlishRecord fluxRec = record.get("flux");
  {
    if (!fluxRec.exists("polarisation")) {
      // FIX THIS UP LATER
//      errorMessage += "\nThe 'flux' record must have a 'polarisation' field";
//      return False;
      thisFlux.setPol(ComponentType::STOKES);
    } else {
      if (fluxRec.get("polarisation").type() != GlishValue::ARRAY) {
	errorMessage += "\nThe 'polarisation' field cannot be a record";
	return False;
      }
      const GlishArray polField = fluxRec.get("polarisation");
      if (polField.elementType() != GlishArray::STRING) {
	errorMessage += "\nThe 'polarisation' field must be a string";
	return False;
      }
      // Maybe the polarisation field should contain ["I", "Q", "U", "V"]. This
      // is harder to parse but more flexible for the future.
      if (polField.shape().product() != 1) {
	errorMessage += String("\nThe 'polarisation' field cannot be an array ");
	return False;
      }
      String polVal;
      if (!polField.get(polVal)) {
	errorMessage += String("\nCould not read the 'polarisation' field ") + 
	  String("in the flux record for an unknown reason");
	return False;
      }
      const ComponentType::Polarisation pol(ComponentType::polarisation(polVal));
      if (pol == ComponentType::UNKNOWN_POLARISATION) {
	errorMessage += String("\nThe polarisation type is not known. ") +
	  String("\nCommon values are 'Stokes', 'Linear' & 'Circular'");
	return False;
      }
      thisFlux.setPol(pol);
    }
  }
  {
    if (!fluxRec.exists("value")) {
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
    // This code needs to be consolidated when the GlishArray bug is resolved.
    if (thisFlux.pol() == ComponentType::STOKES) {
      Vector<Double> fluxVal(4);
      if (!valueField.get(fluxVal.ac())) {
	errorMessage += String("\nCould not read the 'value' field ") + 
	  String("in the flux record for an unknown reason");
	return False;
      }
      thisFlux.setValue(fluxVal);
    } else {
      Vector<DComplex> fluxVal(4);
      if (!valueField.get(fluxVal.ac())) {
	errorMessage += String("\nCould not read the 'value' field ") + 
	  String("in the flux record for an unknown reason");
	return False;
      }
      thisFlux.setValue(fluxVal);
    }
  }
  {
    if (!fluxRec.exists("unit")) {
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
    if (!unitField.get(unitVal)) {
      errorMessage += String("\nCould not read the 'unit' field ") + 
	String("in the flux record for an unknown reason");
      return False;
    }
    thisFlux.setUnit(Unit(unitVal));
  }
  return True;
}

// Move into the Flux<T> class.
Bool SkyCompRep::addFlux(String & errorMessage, GlishRecord & record) const {
  const Flux<Double> & thisFlux = flux();
  GlishRecord fluxRec;
  if (thisFlux.pol() == ComponentType::STOKES) {
    Flux<Double> fluxCopy = thisFlux;
    Vector<Double> fluxVal(4);
    fluxCopy.value(fluxVal);
    fluxRec.add("value", GlishArray(fluxVal.ac()));
    fluxRec.add("polarisation", ComponentType::name(ComponentType::STOKES));
  } else {
    fluxRec.add("value", GlishArray(thisFlux.value().ac()));
    fluxRec.add("polarisation", ComponentType::name(thisFlux.pol()));
  }
  fluxRec.add("unit", thisFlux.unit().getName());
  record.add("flux", fluxRec);
  if (errorMessage == ""); // Suppress compiler warning about unused variable
  return True;
}

Bool SkyCompRep::readLabel(String & errorMessage, const GlishRecord & record) {
  String labelVal("");
  if (record.exists("label")) {
    if (record.get("label").type() != GlishValue::ARRAY) {
      errorMessage += "\nThe 'label' field cannot be a record";
      return False;
    }
    const GlishArray labelField = record.get("label");
    if (labelField.elementType() != GlishArray::STRING) {
      errorMessage += "\nThe 'label' field must be a string";
      return False;
    }
    if (labelField.nelements() != 1) {
      errorMessage += String("\nThe 'label' field cannot be an array");
      return False;
    }
    if (!labelField.get(labelVal)) {
      errorMessage += String("\nCould not read the 'field' field ") + 
	String("for an unknown reason");
      return False;
    }
  }
  setLabel(labelVal);
  return True;
}

Bool SkyCompRep::addLabel(String & errorMessage, GlishRecord & record) const {
  String thisLabel;
  label(thisLabel);
  if (errorMessage == ""); // Suppress compiler warning about unused variable
  record.add("label", thisLabel);
  return True;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 SkyCompRep"
// End: 
