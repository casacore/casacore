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

#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ConstantSpectrum.h>
#include <trial/ComponentModels/GaussianShape.h>
#include <trial/ComponentModels/PointShape.h>
#include <trial/ComponentModels/SkyCompRep.h>
#include <trial/ComponentModels/SpectralIndex.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/Record.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/Quantum.h>
#include <aips/Measures/Unit.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>

SkyCompRep::SkyCompRep() 
  :itsShapePtr(new PointShape),
   itsSpectrumPtr(new ConstantSpectrum),
   itsFlux(),
   itsLabel()
{
  DebugAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const ComponentType::Shape & shape)
  :itsShapePtr((ComponentShape *) 0),
   itsSpectrumPtr(new ConstantSpectrum),
   itsFlux(),
   itsLabel()
{
  initShape(shape);
  DebugAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const ComponentType::Shape & shape,
		       const ComponentType::SpectralShape & spectrum)
  :itsShapePtr((ComponentShape *) 0),
   itsSpectrumPtr((SpectralModel *) 0),
   itsFlux(),
   itsLabel()
{
  initShape(shape);
  initSpectrum(spectrum);
  DebugAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const Flux<Double> & flux,
		       const ComponentShape & shape, 
		       const SpectralModel & spectrum)
  :itsShapePtr(shape.clone()),
   itsSpectrumPtr(spectrum.clone()),
   itsFlux(flux.copy()),
   itsLabel()
{
  DebugAssert(ok(), AipsError);
}

SkyCompRep::SkyCompRep(const SkyCompRep & other) 
  :itsShapePtr(other.itsShapePtr->clone()),
   itsSpectrumPtr(other.itsSpectrumPtr->clone()),
   itsFlux(other.itsFlux.copy()),
   itsLabel(other.itsLabel)
{
  DebugAssert(ok(), AipsError);
}

SkyCompRep::~SkyCompRep() {
  DebugAssert(ok(), AipsError);
}

SkyCompRep & SkyCompRep::operator=(const SkyCompRep & other) {
  if (this != &other) {
    itsShapePtr = other.itsShapePtr->clone();
    itsSpectrumPtr = other.itsSpectrumPtr->clone();
    itsFlux = other.itsFlux.copy();
    itsLabel = other.itsLabel;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

const Flux<Double> & SkyCompRep::flux() const {
  DebugAssert(ok(), AipsError);
  return itsFlux;
}

Flux<Double> & SkyCompRep::flux() {
  DebugAssert(ok(), AipsError);
  return itsFlux;
}

const ComponentShape & SkyCompRep::shape() const {
  DebugAssert(ok(), AipsError);
  return *itsShapePtr;
}

ComponentShape & SkyCompRep::shape() {
  DebugAssert(ok(), AipsError);
  return *itsShapePtr;
}

void SkyCompRep::setShape(const ComponentShape & newShape) {
  DebugAssert(ok(), AipsError);
  itsShapePtr = newShape.clone();
}

SpectralModel & SkyCompRep::spectrum() {
  DebugAssert(ok(), AipsError);
  return *itsSpectrumPtr;
}

const SpectralModel & SkyCompRep::spectrum() const {
  DebugAssert(ok(), AipsError);
  return *itsSpectrumPtr;
}

void SkyCompRep::setSpectrum(const SpectralModel & newSpectrum) {
  DebugAssert(ok(), AipsError);
  itsSpectrumPtr = newSpectrum.clone();
}

String & SkyCompRep::label() {
  DebugAssert(ok(), AipsError);
  return itsLabel;
}

const String & SkyCompRep::label() const {
  DebugAssert(ok(), AipsError);
  return itsLabel;
}

Flux<Double> SkyCompRep::sample(const MDirection & direction, 
				const MVAngle & pixelSize, 
				const MFrequency & centerFrequency) const {
  DebugAssert(ok(), AipsError);
  Flux<Double> flux = itsFlux.copy();
  itsShapePtr->sample(flux, direction, pixelSize);
  itsSpectrumPtr->sample(flux, centerFrequency);
  return flux;
}

Flux<Double> SkyCompRep::visibility(const Vector<Double> & uvw,
				    const Double & frequency) const {
  DebugAssert(ok(), AipsError);
  Flux<Double> flux = itsFlux.copy();
  itsShapePtr->visibility(flux, uvw, frequency);
  // I should scale by the frequency here also but I need to consult with Tim
  // first.
  return flux;
}

Bool SkyCompRep::fromRecord(String & errorMessage,
			    const RecordInterface & record) {
  {
    const String fluxString("flux");
    if (record.isDefined(fluxString)) {
      const RecordFieldId flux(fluxString);
      if (record.dataType(flux) != TpRecord) {
	errorMessage += "The 'flux' field must be a record\n";
	return False;
      }
      const Record & fluxRec = record.asRecord(flux);
      if (!itsFlux.fromRecord(errorMessage, fluxRec)) {
	errorMessage += "Problem parsing the 'flux' field\n";
	return False;
      }
    } else {
      LogIO logErr(LogOrigin("SkyCompRep", "fromRecord()"));
      logErr << LogIO::WARN 
	     << "The component does not have a 'flux' field." << endl
	     << "The default is 1.0 Jy in I and 0.0 in Q, U & V"
	     << LogIO::POST;
      itsFlux = Flux<Double>(1);
    }
  }
  {
    const String shapeString("shape");
    if (record.isDefined(shapeString)) {
      const RecordFieldId shape(shapeString);
      if (record.dataType(shape) != TpRecord) {
	errorMessage += "\nThe 'shape' field must be a record";
	return False;
      }      
      const Record & shapeRec = record.asRecord(shape);
      const ComponentType::Shape recShape = 
	ComponentShape::getType(errorMessage, shapeRec);
      const ComponentType::Shape thisShape = itsShapePtr->type();
      if (recShape != thisShape) {
	errorMessage += String("The shape record specifies a ") + 
	  ComponentType::name(recShape) + String(" shape\n") +
	  String("which cannot be assigned to a component with a ") +
	  ComponentType::name(thisShape) + String(" shape");
	return False;
      }
      if (!itsShapePtr->fromRecord(errorMessage, shapeRec)) {
	errorMessage += "Problem parsing the 'shape' field\n";
	return False;
      }
    } else {
      LogIO logErr(LogOrigin("SkyCompRep", "fromRecord()"));
      logErr << LogIO::WARN 
	     << "The component does not have a 'shape' field." << endl
	     << "The default is a point component at the J2000 north pole"
	     << LogIO::POST;
      const Unit deg("deg");
      itsShapePtr = new PointShape(MDirection(Quantum<Double>(0.0, deg),
					      Quantum<Double>(90.0, deg),
					      MDirection::J2000));
    }
  }
  {
    const String spectrumString("spectrum");
    if (record.isDefined(spectrumString)) {
      const RecordFieldId spectrum(spectrumString);
      if (record.dataType(spectrum) != TpRecord) {
	errorMessage += "\nThe 'spectrum' field must be a record";
	return False;
      }      
      const Record & spectrumRec = record.asRecord(spectrum);
      const ComponentType::SpectralShape recShape = 
	SpectralModel::getType(errorMessage, spectrumRec);
      const ComponentType::SpectralShape thisShape = itsSpectrumPtr->type();
      if (recShape != thisShape) {
	errorMessage += String("The spectrum record specifies a ") + 
	  ComponentType::name(recShape) + String(" spectrum\n") +
	  String("which cannot be assigned to a component with a") +
	  ComponentType::name(thisShape) + String(" spectrum");
	return False;
      }
      if (!itsSpectrumPtr->fromRecord(errorMessage, spectrumRec)) {
	return False;
      }
    } else {
      LogIO logErr(LogOrigin("SkyCompRep", "fromRecord()"));
      logErr << LogIO::WARN 
	     << "The component does not have a 'spectrum' field." << endl
	     << "The default is a constant spectrum"
	     << LogIO::POST;
      itsSpectrumPtr = new ConstantSpectrum;
    }
  }
  {
    const String labelString("label");
    if (record.isDefined(labelString)) {
      const RecordFieldId label(labelString);
      if (record.dataType(label) != TpString) {
	errorMessage += "\nThe 'label' field must be a string";
	return False;
      }      
      if (record.shape(label) != IPosition(1,1)) {
	errorMessage += "\nThe 'label' field must have only 1 element";
	return False;
      } 
      itsLabel = record.asString(label);
    }
  }
  return True;
}

Bool SkyCompRep::toRecord(String & errorMessage, 
			  RecordInterface & record) const {
  {
    Record fluxRec;
    if (!itsFlux.toRecord(errorMessage, fluxRec)) {
      return False;
    }
    record.defineRecord(RecordFieldId("flux"), fluxRec);
  }
  {
    Record shapeRec;
    if (!itsShapePtr->toRecord(errorMessage, shapeRec)) {
      return False;
    }
    record.defineRecord(RecordFieldId("shape"), shapeRec);
  }
  {
    Record spectrumRec;
    if (!itsSpectrumPtr->toRecord(errorMessage, spectrumRec)) {
      return False;
    }
    record.defineRecord(RecordFieldId("spectrum"), spectrumRec);
  }
  record.define(RecordFieldId("label"), itsLabel);
  DebugAssert(ok(), AipsError);
  return True;
}

Bool SkyCompRep::ok() const {
  if (itsShapePtr == (ComponentShape *) 0) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "Shape pointer is null"
           << LogIO::POST;
    return False;
  }
  if (itsShapePtr->ok() == False) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The component shape is not ok"
           << LogIO::POST;
    return False;
  }
  if (itsSpectrumPtr == (SpectralModel *) 0) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "Spectrum pointer is null"
           << LogIO::POST;
    return False;
  }
  if (itsSpectrumPtr->ok() == False) {
    LogIO logErr(LogOrigin("SkyCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The component spectrum is not ok"
           << LogIO::POST;
    return False;
  }
  return True;
}

void SkyCompRep::initShape(ComponentType::Shape shape) {
  switch (shape) {
  case ComponentType::POINT: 
    itsShapePtr = new PointShape;
    break;
  case ComponentType::GAUSSIAN:
    itsShapePtr = new GaussianShape;
    break;
  default:
    throw(AipsError(String("SkyCompRep::SkyCompRep(shape,spectrum) - ") + 
		    String("Cannot construct a SkyCompRep with an ")+ 
		    ComponentType::name(shape) + String(" shape")));
  };
}

void SkyCompRep::initSpectrum(ComponentType::SpectralShape spectrum) {
  switch (spectrum) {
  case ComponentType::CONSTANT_SPECTRUM: 
    itsSpectrumPtr = new ConstantSpectrum;
    break;
  case ComponentType::SPECTRAL_INDEX:
    itsSpectrumPtr = new SpectralIndex;
    break;
  default:
    throw(AipsError(String("SkyCompRep::SkyCompRep(shape,spectrum) - ") + 
		    String("Cannot construct a SkyCompRep with an ") + 
		    ComponentType::name(spectrum) + String(" spectrum")));
  };
}

void SkyCompRep::project(ImageInterface<Float> & image) const {
//   const CoordinateSystem coords = image.coordinates();
//   const IPosition imageShape = image.shape();
//   const uInt naxis = imageShape.nelements();
  
//   // I currently REQUIRE that the image has one direction coordinate (only).
//   // All other coordinates (ie. polarization and frequency) are optional. 
//   const Vector<uInt> dirAxes = CoordinateUtil::findDirectionAxes(coords);
//   AlwaysAssert(dirAxes.nelements() != 0, AipsError);
//   const uInt nPixAxes = dirAxes.nelements();
//   Vector<Double> pixelCoord(nPixAxes); pixelCoord = 0.0;
//   Vector<Double> worldCoord(2);

//   const DirectionCoordinate dirCoord = 
//     coords.directionCoordinate(coords.findCoordinate(Coordinate::DIRECTION));
//   MDirection pixelDir(MVDirection(0.0), dirCoord.directionType());
//   Vector<Quantum<Double> > dirVal(2);
//   MVAngle pixelSize;
//   {
//     Vector<String> units = dirCoord.worldAxisUnits();
//     dirVal(0).setUnit(units(0));
//     dirVal(1).setUnit(units(1));
//     Vector<Double> inc = dirCoord.increment();
//     Quantum<Double> inc0(abs(inc(0)), units(0));
//     Quantum<Double> inc1(abs(inc(1)), units(1));
//     AlwaysAssert(near(inc0, inc1), AipsError);
//     pixelSize = MVAngle(inc0);
//   }
  
//   // Setup an iterator to step through the image in chunks that can fit into
//   // memory. Go to a bit of effort to make the chunck size as large as
//   // possible but still minimize the number of tiles in the cache.
//   IPosition elementShape = imageShape;
//   IPosition chunckShape = imageShape;
//   uInt axis;
//   {
//     const IPosition tileShape(image.niceCursorShape());
//     for (uInt k = 0; k < nPixAxes; k++) {
//       axis = dirAxes(k);
//       elementShape(axis) = 1;
//       chunckShape(axis) = tileShape(axis);
//     }
//   }

//   // Check if there is a Stokes Axes and if so which polarizations. Otherwise
//   // only grid the I polarisation.
//   Vector<Int> stokes; // Vector stating which polarisations is on each plane
//   // Find which axis is the stokes pixel axis
//   const Int polAxis = CoordinateUtil::findStokesAxis(stokes, coords);  
//   const uInt nStokes = stokes.nelements(); 
//   if (polAxis >= 0)
//     AlwaysAssert(imageShape(polAxis) == Int(nStokes), AipsError);
//   for (uInt p = 0; p < nStokes; p++)
//     AlwaysAssert(stokes(p) == Stokes::I || stokes(p) == Stokes::Q ||
// 		 stokes(p) == Stokes::U || stokes(p) == Stokes::V, 
// 		 AipsError);

//   Block<IPosition> blc;
//   Block<IPosition> trc;
//   if (nStokes > 1) {
//     blc.resize(nStokes);
//     blc = IPosition(naxis,0);
//     trc.resize(nStokes);
//     trc = elementShape - 1;
//     for (uInt p = 0; p < nStokes; p++) {
//       blc[p](polAxis) = p;
//       trc[p](polAxis) = p;
//     }
//   }

//   LatticeIterator<Float> chunkIter(image, chunckShape);
//   Vector<Double> pixelVal(4);
//   IPosition chunkOrigin(naxis), elementPosition(naxis);
//   for (chunkIter.reset(); !chunkIter.atEnd(); chunkIter++) {
//     ArrayLattice<Float> array(chunkIter.rwCursor());
//     LatticeIterator<Float> elementIter(array, elementShape);
//     chunkOrigin = chunkIter.position();
//     for (elementIter.reset(); !elementIter.atEnd(); elementIter++) {
//       elementPosition = elementIter.position();
//       for (uInt k = 0; k < nPixAxes; k++) {
// 	axis = dirAxes(k);
// 	pixelCoord(k) = elementPosition(axis) + chunkOrigin(axis);
//       }
//       if (!dirCoord.toWorld(worldCoord, pixelCoord)) {
// // I am not sure what to do here, probably this message should be logged.
// //  	cerr << " SkyCompRep::Pixel at " << pixelCoord 
// //  	     << " cannot be projected" << endl;
//       }
//       else {
// 	dirVal(0).setValue(worldCoord(0));
// 	dirVal(1).setValue(worldCoord(1));
// 	pixelDir.set(MVDirection(dirVal));
// 	sample(pixelVal, pixelDir, pixelSize);
// 	if (nStokes == 1) {
// 	  switch (stokes(0)) {
// 	  case Stokes::I:
// 	    elementIter.rwCursor() += Float(pixelVal(0)); break;
// 	  case Stokes::Q:
// 	    elementIter.rwCursor() += Float(pixelVal(1)); break;
// 	  case Stokes::U:
// 	    elementIter.rwCursor() += Float(pixelVal(2)); break;
// 	  case Stokes::V:
// 	    elementIter.rwCursor() += Float(pixelVal(3)); break;
// 	  }
// 	}
// 	else if (elementShape.product() == Int(nStokes))
// 	  for (uInt p = 0; p < nStokes; p++) {
// 	    switch (stokes(p)) {
// 	    case Stokes::I:
// 	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(0)); break;
// 	    case Stokes::Q:
// 	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(1)); break;
// 	    case Stokes::U:
// 	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(2)); break;
// 	    case Stokes::V:
// 	      elementIter.rwCursor()(blc[p]) += Float(pixelVal(3)); break;
// 	    }
// 	  }
// 	else
// 	for (uInt p = 0; p < nStokes; p++) {
// 	  switch (stokes(p)) {
// 	  case Stokes::I:
// 	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(0));
// 	    break;
// 	  case Stokes::Q:
// 	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(1));
// 	    break;
// 	  case Stokes::U:
// 	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(2));
// 	    break;
// 	  case Stokes::V:
// 	    elementIter.rwCursor()(blc[p], trc[p]).ac() += Float(pixelVal(3));
// 	    break;
// 	  }
// 	}
//       }
//     }
//   }
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 SkyCompRep"
// End: 
