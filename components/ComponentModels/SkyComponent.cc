//# SkyComponent.cc:  this defines SkyComponent
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
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

#include <casa/Quanta/QMath.h>
#include <components/ComponentModels/SkyComponent.h>
#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/SkyCompRep.h>
#include <components/ComponentModels/SpectralModel.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Exceptions/Error.h>
#include <casa/iomanip.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Utilities/Precision.h>

#include <measures/Measures/MDirection.h>
#include <measures/Measures/MFrequency.h>

#include <casa/Quanta/MVAngle.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>

#include <iomanip>

namespace casa { //# NAMESPACE CASA - BEGIN

SkyComponent::SkyComponent()
  :itsCompPtr(new SkyCompRep) 
{}

SkyComponent::SkyComponent(const ComponentType::Shape& shape)   
  :itsCompPtr(new SkyCompRep(shape))
{}

SkyComponent::SkyComponent(const ComponentType::Shape& shape,
			   const ComponentType::SpectralShape& spectralModel) 
  :itsCompPtr(new SkyCompRep(shape, spectralModel))
{}

SkyComponent::SkyComponent(const Flux<Double>& flux,
			   const ComponentShape& shape, 
			   const SpectralModel& spectrum)
  :itsCompPtr(new SkyCompRep(flux, shape, spectrum))
{}

SkyComponent::SkyComponent(const SkyComponent& other) 
  :SkyCompBase(other),
   itsCompPtr (other.itsCompPtr)
{}

SkyComponent::~SkyComponent() {}

SkyComponent& SkyComponent::operator=(const SkyComponent& other) {
  if (this != &other)
    itsCompPtr = other.itsCompPtr;
  return *this;
}

Flux<Double>& SkyComponent::flux() {
  return itsCompPtr->flux();
}

const Flux<Double>& SkyComponent::flux() const {
  return itsCompPtr->flux();
}

const ComponentShape& SkyComponent::shape() const {
  return itsCompPtr->shape();
}

ComponentShape& SkyComponent::shape() {
  return itsCompPtr->shape();
}

void SkyComponent::setShape(const ComponentShape& newShape) {
  itsCompPtr->setShape(newShape);
}

const SpectralModel& SkyComponent::spectrum() const {
  return itsCompPtr->spectrum();
}

SpectralModel& SkyComponent::spectrum() {
  return itsCompPtr->spectrum();
}

void SkyComponent::setSpectrum(const SpectralModel& newSpectrum) {
  itsCompPtr->setSpectrum(newSpectrum);
}

String& SkyComponent::label() {
  return itsCompPtr->label();
}

const String& SkyComponent::label() const {
  return itsCompPtr->label();
}

Vector<Double>& SkyComponent::optionalParameters() {
   return itsCompPtr->optionalParameters();
}

const Vector<Double>& SkyComponent::optionalParameters() const {
   return itsCompPtr->optionalParameters();
}

Bool SkyComponent::isPhysical() const {
  return itsCompPtr->isPhysical();
}

Flux<Double> SkyComponent::sample(const MDirection& direction, 
			      const MVAngle& pixelLatSize, 
			      const MVAngle& pixelLongSize, 
			      const MFrequency& centerFrequency) const {
  return itsCompPtr->sample(direction, pixelLatSize, pixelLongSize, 
			    centerFrequency);
}

void SkyComponent::sample(Cube<Double>& samples, const Unit& reqUnit,
			  const Vector<MVDirection>& directions, 
			  const MeasRef<MDirection>& dirRef, 
			  const MVAngle& pixelLatSize, 
			  const MVAngle& pixelLongSize, 
			  const Vector<MVFrequency>& frequencies,
			  const MeasRef<MFrequency>& freqRef) const {
  itsCompPtr->sample(samples, reqUnit,
		     directions, dirRef, pixelLatSize, pixelLongSize,
		     frequencies, freqRef);
}

Flux<Double> SkyComponent::visibility(const Vector<Double>& uvw,
 				      const Double& frequency) const {
  return itsCompPtr->visibility(uvw, frequency);
}

void SkyComponent::visibility(Cube<DComplex>& visibilities,
			      const Matrix<Double>& uvws,
			      const Vector<Double>& frequencies) const {
  itsCompPtr->visibility(visibilities, uvws, frequencies);
}

Bool SkyComponent::fromRecord(String& errorMessage, 
 			      const RecordInterface& record) {
  return itsCompPtr->fromRecord(errorMessage, record);
}

Bool SkyComponent::toRecord(String& errorMessage,
 			    RecordInterface& record) const {
  return itsCompPtr->toRecord(errorMessage, record);
}

SkyComponent SkyComponent::copy() const {
  SkyComponent newComp(flux().copy(), shape(), spectrum());
  newComp.label() = label();
  newComp.optionalParameters() = optionalParameters();
  return newComp;
}

void SkyComponent::fromPixel (Double& fluxRatio, const Vector<Double>& parameters,
                              const Unit& brightnessUnitIn,
                              const GaussianBeam& restoringBeam,
                              const CoordinateSystem& cSys,
                              ComponentType::Shape componentShape,
                              Stokes::StokesTypes stokes)
{
   itsCompPtr->fromPixel(fluxRatio, parameters, brightnessUnitIn, restoringBeam,
                         cSys, componentShape, stokes);
}

Vector<Double> SkyComponent::toPixel (const Unit& brightnessUnitIn,
                                      const GaussianBeam& restoringBeam,
                                      const CoordinateSystem& cSys,
                                      Stokes::StokesTypes stokes) const
{
   return itsCompPtr->toPixel(brightnessUnitIn, restoringBeam,
                              cSys, stokes);
}




Bool SkyComponent::ok() const {
  if (itsCompPtr.null()) {
    LogIO logErr(LogOrigin("SkyComponent", "ok()"));
    logErr << LogIO::SEVERE << "Internal pointer is not pointing to anything"
           << LogIO::POST;
    return False;
  }
  if (!itsCompPtr->ok()) {
    LogIO logErr(LogOrigin("SkyComponent", "ok()"));
    logErr << LogIO::SEVERE << "Component representation is not ok"
           << LogIO::POST;
    return False;
  }
  return True;
}

String SkyComponent::summarize(
	const DirectionCoordinate *const &dc, Bool longErrOnGreatCircle
) const {
	ostringstream ldpar;
	if (shape().type()==ComponentType::LDISK) {
		ldpar << " (limb-darkening exponent: "<<optionalParameters()(0) <<" )"<<endl;
	}
	ostringstream summary;
	summary << "SUMMARY OF COMPONENT " << label() << endl;
	summary << "Shape: " << shape().ident() << ldpar.str() <<endl;
	const Flux<Double> myFlux = flux();
	Quantum<Vector<std::complex<double> > > fluxValue;
	myFlux.value(fluxValue);
	summary << "Flux density: " << fluxValue << " +/- " << myFlux.errors() << endl;
	summary << "Spectral model: " << spectrum().ident() << endl;
	summary << "Position: " <<  positionToString(dc, longErrOnGreatCircle) << endl;
	summary << "Size: " << endl << shape().sizeToString() << endl;
	return summary.str();
}

String SkyComponent::positionToString(
	const DirectionCoordinate *const &dc ,Bool longErrOnGreatCircle
) const {
	// FIXME essentially cut and paste of Gareth's python code. Needs work.
	ostringstream position;
	MDirection mdir = shape().refDirection();

	Quantity lat = mdir.getValue().getLat("rad");
	String latString = MVAngle(lat).string(MVAngle::ANGLE_CLEAN, 8);

	Quantity longitude = mdir.getValue().getLong("rad");
	String longString = MVTime(longitude).string(MVTime::TIME, 9);

	Quantity dLat = shape().refDirectionErrorLat();
	dLat.convert("rad");

	Quantity dLong = shape().refDirectionErrorLong();
	dLong.convert("rad");

	// Add error estimates to ra/dec strings if an error is given (either >0)

	uInt precision = 1;
	if ( dLong.getValue() != 0 || dLat.getValue() != 0 ) {
		dLong.convert("s");
		dLat.convert("arcsec");
		Double drasec  = roundDouble(dLong.getValue());
		Double ddecarcsec = roundDouble(dLat.getValue());
		Vector<Double> dravec(2), ddecvec(2);
		dravec.set(drasec);
		ddecvec.set(ddecarcsec);
		precision = precisionForValueErrorPairs(dravec,ddecvec);
		longString = MVTime(longitude).string(MVTime::TIME, 6+precision);
		latString =  MVAngle(lat).string(MVAngle::ANGLE, 6+precision);
	}
	std::pair<String, String> labels = _axisLabels(dc);
	position << "Position ---" << endl;
	position << "       --- " << labels.first << "   " << longString;
	if (dLong.getValue() == 0) {
		position << " (fixed)" << endl;
	}
	else {
		Quantity timeError = longErrOnGreatCircle ? dLong/cos(lat) : dLong;
		position << " +/- " << std::fixed
			<< setprecision(precision) << timeError << " ("
			<< dLong.getValue("arcsec") << " arcsec"
			<< (longErrOnGreatCircle ? " along great circle" : "")
			<< ")" << endl;
	}
	position << "       --- " << labels.second << " " << latString;
	if (dLat.getValue() == 0) {
		position << " (fixed)" << endl;
	}
	else {
		position << " +/- " << dLat << endl;
	}
	if (dc) {
		const Vector<String> units = dc->worldAxisUnits();
		Vector<Double> world(dc->nWorldAxes(), 0), pixel(dc->nPixelAxes(), 0);
		world[0] = longitude.getValue(units[0]);
		world[1] = lat.getValue(units[1]);
		// TODO do the pixel computations in another method
		if (dc->toPixel(pixel, world)) {
			Vector<Double> increment = dc->increment();
			Double longPixErr = dLong.getValue() == 0
				? 0 : abs(dLong.getValue(units[0])/increment[0]);
			Double latPixErr = dLat.getValue() == 0
				? 0 : abs(dLat.getValue(units[1])/increment[1]);
			Vector<Double> longPix(2), latPix(2);
			longPix.set(roundDouble(longPixErr));
			latPix.set(roundDouble(latPixErr));
			precision = precisionForValueErrorPairs(longPix, latPix);
			position << std::fixed <<  setprecision(precision);
			position << "       --- " << labels.first << " " << pixel[0];
			if (dLong.getValue() == 0) {
				position << " (fixed)" << endl;
			}
			else {
				position << " +/- " << longPixErr << " pixels" << endl;
			}
			position << "       --- " << labels.second << " " << pixel[1];
			if (dLat.getValue() == 0) {
				position << " (fixed)" << endl;
			}
			else {
				position << " +/- " << latPixErr << " pixels" << endl;
			}
		}
		else {
			position << "unable to determine position in pixels:" << dc->errorMessage() << endl;
		}
	}
	return position.str();
}

std::pair<String, String> SkyComponent::_axisLabels(
	const DirectionCoordinate *const &dc
) {
	std::pair<String, String> labels;
	if (dc) {
		Vector<String> names = dc->worldAxisNames();
		for (uInt i=0; i<2; i++) {
			String label;
			names[i].downcase();
			if (names[i] == "right ascension") {
				label = "ra:";
			}
			else if (names[i] == "declination") {
				label = "dec:";
			}
			else if (names[i] == "longitude") {
				label = "long:";
			}
			else if (names[i] == "latitude") {
				label = "lat:";
			}
			else {
				label = names[i] + ":";
			}
			if (i == 0) {
				labels.first = label;
			}
			else {
				labels.second = label;
			}
		}
	}
	else {
		labels.first  = "long:";
		labels.second = "lat: ";
	}
	typedef std::string::size_type size_type;
	size_type f = labels.first.size();
	size_type s = labels.second.size();
	if (f > s) {
		size_type d = f - s;
		for (size_type i=0; i<d ;i++) {
			labels.second += " ";
		}
	}
	else if (f < s) {
		size_type d = s - f;
		for (size_type i=0; i<d ;i++) {
			labels.first += " ";
		}
	}
	return labels;
}

}

