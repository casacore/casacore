//# SpectralIndex.cc:
//# Copyright (C) 1998
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

#include <trial/ComponentModels/SpectralIndex.h>
#include <trial/ComponentModels/Flux.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MCFrequency.h>
#include <aips/Measures/MVFrequency.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>

#ifdef __GNUG__
typedef MeasConvert<MFrequency,MVFrequency,MCFrequency> 
  gpp_measconvert_mfrequency_mvfrequency_mcfrequency;
#endif

SpectralIndex::SpectralIndex()
  :itsRefFreq(Quantum<Double>(1, "GHz"), MFrequency::LSR),
   itsIndex(4, 0.0),
   itsRefFrame((MFrequency::Types) itsRefFreq.getRef().getType()),
   itsNu0(itsRefFreq.getValue().getValue()),
   itsIonly(True),
   itsIindex(itsIndex(0)),
   itsQindex(itsIndex(1)),
   itsUindex(itsIndex(2)),
   itsVindex(itsIndex(3)),
   itsUnit("GHz")
{
  DebugAssert(ok(), AipsError);
}

SpectralIndex::SpectralIndex(const MFrequency & refFreq, Double exponent)
  :itsRefFreq(refFreq),
   itsIndex(4, 0.0),
   itsRefFrame((MFrequency::Types) itsRefFreq.getRef().getType()),
   itsNu0(itsRefFreq.getValue().getValue()),
   itsIonly(True),
   itsIindex(itsIndex(0)),
   itsQindex(itsIndex(1)),
   itsUindex(itsIndex(2)),
   itsVindex(itsIndex(3)),
   itsUnit("GHz")
{
  itsIindex = exponent;
  DebugAssert(ok(), AipsError);
}

SpectralIndex::SpectralIndex(const MFrequency & refFreq,
			     const Vector<Double> & exponents)
  :itsRefFreq(refFreq),
   itsIndex(exponents),
   itsRefFrame((MFrequency::Types) itsRefFreq.getRef().getType()),
   itsNu0(itsRefFreq.getValue().getValue()),
   itsIonly(False),
   itsIindex(itsIndex(0)),
   itsQindex(itsIndex(1)),
   itsUindex(itsIndex(2)),
   itsVindex(itsIndex(3)),
   itsUnit("GHz")
{
  DebugAssert(exponents.nelements() == 4, AipsError);
  itsIonly = isIonly();
  DebugAssert(ok(), AipsError);
}

SpectralIndex::SpectralIndex(const SpectralIndex & other) 
  :itsRefFreq(other.itsRefFreq),
   itsIndex(other.itsIndex.copy()),
   itsRefFrame(other.itsRefFrame),
   itsNu0(other.itsNu0),
   itsIonly(other.itsIonly),
   itsIindex(itsIndex(0)),
   itsQindex(itsIndex(1)),
   itsUindex(itsIndex(2)),
   itsVindex(itsIndex(3)),
   itsUnit(other.itsUnit)
{
  DebugAssert(ok(), AipsError);
}

SpectralIndex::~SpectralIndex() {
  DebugAssert(ok(), AipsError);
}

SpectralIndex & SpectralIndex::operator=(const SpectralIndex & other) {
  if (this != &other) {
    itsRefFreq = other.itsRefFreq;
    itsIndex = other.itsIndex;
    itsRefFrame = other.itsRefFrame;
    itsNu0 = other.itsNu0;
    itsIonly = other.itsIonly;
    itsIindex = itsIndex(0);
    itsQindex = itsIndex(1);
    itsUindex = itsIndex(2);
    itsVindex = itsIndex(3);
    itsUnit = other.itsUnit;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::SpectralShape SpectralIndex::type() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::SPECTRAL_INDEX;
}

const MFrequency & SpectralIndex::refFrequency() const {
  DebugAssert(ok(), AipsError);
  return itsRefFreq;
}

void SpectralIndex::setRefFrequency(const MFrequency & newRefFrequency) {
  itsRefFreq = newRefFrequency;
  itsRefFrame = (MFrequency::Types) itsRefFreq.getRef().getType();
  itsNu0 = itsRefFreq.getValue().getValue();
  DebugAssert(ok(), AipsError);
}

const Unit & SpectralIndex::frequencyUnit() const {  
  DebugAssert(ok(), AipsError);
  return itsUnit;
}

void SpectralIndex::convertFrequencyUnit(const Unit & freqUnit) {
  itsUnit = freqUnit;
  DebugAssert(ok(), AipsError);
}

const Double & SpectralIndex::index(const Stokes::StokesTypes which) const {
  DebugAssert(ok(), AipsError);
  switch (which) {
  case Stokes::I: 
    return itsIindex;
    break;
  case Stokes::Q:
    return itsQindex;
    break;
  case Stokes::U:
    return itsUindex;
    break;
  case Stokes::V:
    return itsVindex;
    break;
  default:
    throw(AipsError(String("SpectralIndex::index(Stokes::StokesTypes) - ") + 
		    String("Can only provide the spectral index for ")+ 
		    String("Stokes I,Q,U or V polarisations")));
  };
  return itsIindex;
}

void SpectralIndex::setIndex(const Double & newIndex, 
			     const Stokes::StokesTypes which) {
  switch (which) {
  case Stokes::I: 
    itsIindex = newIndex;
    break;
  case Stokes::Q:
    itsQindex = newIndex;
    break;
  case Stokes::U:
    itsUindex = newIndex;
    break;
  case Stokes::V:
    itsVindex = newIndex;
    break;
  default:
    throw(AipsError(String("SpectralIndex::setIndex") +
		    String("(Double,Stokes::StokesTypes) - ") +
		    String("Can only set the spectral index for ")+ 
		    String("Stokes I,Q,U or V polarisations")));
  };
  itsIonly = isIonly();
  DebugAssert(ok(), AipsError);
}

Vector<Double> SpectralIndex::indices() const {
  DebugAssert(ok(), AipsError);
  return itsIndex.copy();
}

void SpectralIndex::setIndices(const Vector<Double> & newIndices) {
  DebugAssert(newIndices.nelements() == 4, AipsError);
  itsIndex = newIndices;
  itsIonly = isIonly();
  DebugAssert(ok(), AipsError);
}

void SpectralIndex::sample(Flux<Double> & scaledFlux, 
			   const MFrequency & centerFreq) const {
  DebugAssert(ok(), AipsError);
  DebugAssert(!nearAbs(itsNu0, 0.0, C::dbl_epsilon), AipsError);
  Double nu = centerFreq.getValue().getValue();
  if ((MFrequency::Types) centerFreq.getRef().getType() != itsRefFrame) {
    nu = MFrequency::Convert(centerFreq, itsRefFrame)().getValue().getValue();
  }
  scaledFlux.convertPol(ComponentType::STOKES);
  if (!near(nu, itsNu0, C::dbl_epsilon)) {
    const Double freqFactor = nu/itsNu0;
    const Double Iscale = pow(freqFactor, itsIindex);
    if (itsIonly) {
      scaledFlux.scaleValue(Iscale);
    } else {
      const Double Qscale = pow(freqFactor, itsQindex);
      const Double Uscale = pow(freqFactor, itsUindex);
      const Double Vscale = pow(freqFactor, itsVindex);
      scaledFlux.scaleValue(Iscale, Qscale, Uscale, Vscale);
    }
  }
}

SpectralModel * SpectralIndex::clone() const {
  DebugAssert(ok(), AipsError);
  SpectralModel * tmpPtr = new SpectralIndex(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

uInt SpectralIndex::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 4;
}

void SpectralIndex::
setParameters(const Vector<Double> & newSpectralParms) {
  DebugAssert(newSpectralParms.nelements() == nParameters(),AipsError);
  itsIindex = newSpectralParms(0);
  itsQindex = newSpectralParms(1);
  itsUindex = newSpectralParms(2);
  itsVindex = newSpectralParms(3);
  itsIonly = isIonly();
  DebugAssert(ok(), AipsError);
}

void SpectralIndex::parameters(Vector<Double> & spectralParms) const {
  DebugAssert(ok(), AipsError);
  DebugAssert(spectralParms.nelements() == nParameters(),AipsError);
  spectralParms(0) = itsIindex;
  spectralParms(1) = itsQindex;
  spectralParms(2) = itsUindex;
  spectralParms(3) = itsVindex;
}

Bool SpectralIndex::fromRecord(String & errorMessage, 
			       const RecordInterface & record) {
  if (!SpectralModel::readFreq(errorMessage, record)) return False;
  {
    if (!record.isDefined(String("index"))) {
      errorMessage += "The 'spectrum' record must have an 'index' field\n";
      return False;
    }
    const RecordFieldId index("index");
    const IPosition shape(1,4);
    if (record.shape(index) != shape) {
      errorMessage += "The 'index' field must be a vector with 4 elements\n";
      return False;
    }
    Vector<Double> indexVal(shape);
    switch (record.dataType(index)) {
    case TpArrayDouble:
      indexVal = record.asArrayDouble(index);
      break;
    case TpArrayFloat:
      convertArray(indexVal.ac(), record.asArrayFloat(index));
      break;
    case TpArrayInt:
      convertArray(indexVal.ac(), record.asArrayInt(index));
      break;
    default:
      errorMessage += "The 'index' field must be vector of real numbers\n";
      return False;
    }
    setIndices(indexVal);
  }
  DebugAssert(ok(), AipsError);
  return True;
}

Bool SpectralIndex::toRecord(String & errorMessage,
			     RecordInterface & record) const {
  DebugAssert(ok(), AipsError);
  record.define(RecordFieldId("type"), ComponentType::name(type()));
  if (!SpectralModel::addFreq(errorMessage, record)) return False;
  record.define("index", indices());
  return True;
}

Bool SpectralIndex::convertUnit(String & errorMessage,
                                const RecordInterface & record) {
  const String fieldString("index");
  if (!record.isDefined(fieldString)) {
    return True;
  }
  const RecordFieldId field(fieldString);
  if (!((record.dataType(field) == TpString) && 
	(record.shape(field) == IPosition(1,1)) &&
	(record.asString(field) == ""))) {
    errorMessage += "The 'index' field must be an empty string\n";
    errorMessage += "(and not a vector of strings)\n";
    return False;
  }
  return True;
}

Bool SpectralIndex::ok() const {
  if (itsIndex.nelements() != 4) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "Index vector does not have 4 elements"
           << LogIO::POST;
    return False;
  }
  if (itsIndex(0) != itsIindex) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "I index and index vector disagree"
           << LogIO::POST;
    return False;
  }
  if (itsIndex(1) != itsQindex) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "Q index and index vector disagree"
           << LogIO::POST;
    return False;
  }
  if (itsIndex(2) != itsUindex) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "U index and index vector disagree"
           << LogIO::POST;
    return False;
  }
  if (itsIndex(3) != itsVindex) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "V index and index vector disagree"
           << LogIO::POST;
    return False;
  }
  if (itsIonly != isIonly()) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "I only flag incorrectly set"
           << LogIO::POST;
    return False;
  }
  if (itsNu0 != itsRefFreq.getValue().getValue()) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "incorrectly cached reference frequency"
           << LogIO::POST;
    return False;
  }
  if (itsRefFrame != (MFrequency::Types) itsRefFreq.getRef().getType()) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "incorrectly cached reference frame"
           << LogIO::POST;
    return False;
  }
  return True;
}

Bool SpectralIndex::isIonly() const {
  if (nearAbs(itsQindex, 0.0, C::dbl_min) && 
      nearAbs(itsUindex, 0.0, C::dbl_min) &&
      nearAbs(itsVindex, 0.0, C::dbl_min)) {
    return True;
  } else {
    return False;
  }
}

// spectrum := [type = 'discrete',
//              frequency = MFrequency,
//              frequencyoffset[1] = MVFrequency
//              frequencyoffset[2] = MVFrequency
//              scale[1] = [2, 1, 5, 4],
//              scale[2] = [3, 2, 3, 4],
//             ]
// spectrum := [type = 'rotationmeasure',
//              frequency = MFrequency,
//              rotationmeasure = [value = 1.0, unit = 'rad.m^-2']
//             ]
// spectrum := [type = 'gaussian',
//              reference = MFrequency,
//              fwhm = MVFrequency,
//              fluxscale = [1, 0.5, 0.5, 1],
//             ]
// Local Variables: 
// compile-command: "gmake OPTLIB=1 SpectralIndex"
// End: 
