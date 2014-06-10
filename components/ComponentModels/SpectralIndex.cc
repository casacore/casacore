//# SpectralIndex.cc:
//# Copyright (C) 1998,1999,2000,2003
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

#include <components/ComponentModels/SpectralIndex.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Exceptions/Error.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MFrequency.h>
#include <measures/Measures/MCFrequency.h>
#include <measures/Measures/MeasConvert.h>
#include <casa/Quanta/MVFrequency.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/DataType.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

SpectralIndex::SpectralIndex()
  :SpectralModel(),
   itsIndex(0.0),
   itsError(0.0)
{
  DebugAssert(ok(), AipsError);
}

SpectralIndex::SpectralIndex(const MFrequency& refFreq, Double exponent)
  :SpectralModel(refFreq),
   itsIndex(exponent),
   itsError(0.0)
{
  DebugAssert(ok(), AipsError);
}

SpectralIndex::SpectralIndex(const SpectralIndex& other) 
  :SpectralModel(other),
   itsIndex(other.itsIndex),
   itsError(other.itsError)
{
  DebugAssert(ok(), AipsError);
}

SpectralIndex::~SpectralIndex() {
  DebugAssert(ok(), AipsError);
}

SpectralIndex& SpectralIndex::operator=(const SpectralIndex& other) {
  if (this != &other) {
    SpectralModel::operator=(other);
    itsIndex = other.itsIndex;
    itsError = other.itsError;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::SpectralShape SpectralIndex::type() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::SPECTRAL_INDEX;
}

const Double& SpectralIndex::index() const {
   DebugAssert(ok(), AipsError);
   return itsIndex;
}

void SpectralIndex::setIndex(const Double& newIndex) { 
  itsIndex = newIndex;
  DebugAssert(ok(), AipsError);
}

Double SpectralIndex::sample(const MFrequency& centerFreq) const {
  DebugAssert(ok(), AipsError);
  const MFrequency& refFreq(refFrequency());
  const MFrequency::Ref& centerFreqFrame(centerFreq.getRef());
  Double nu0;
  if (centerFreqFrame != refFreq.getRef()) {
    nu0 = MFrequency::Convert(refFreq,centerFreqFrame)().getValue().getValue();
  } else {
    nu0 = refFreq.getValue().getValue();
  }
  if (nu0 <= 0.0) {
    throw(AipsError("SpectralIndex::sample(...) - "
		    "the reference frequency is zero or negative"));
  }
  const Double nu = centerFreq.getValue().getValue();
  return pow(nu/nu0, itsIndex);
}

void SpectralIndex::sample(Vector<Double>& scale, 
			   const Vector<MFrequency::MVType>& frequencies, 
			   const MFrequency::Ref& refFrame) const {
  DebugAssert(ok(), AipsError);
  const uInt nSamples = frequencies.nelements();
  DebugAssert(scale.nelements() == nSamples, AipsError);

  const MFrequency& refFreq(refFrequency());
  Double nu0;
  if (refFrame != refFreq.getRef()) {
    nu0 = MFrequency::Convert(refFreq, refFrame)().getValue().getValue();
  } else {
    nu0 = refFreq.getValue().getValue();
  }
  if (nu0 <= 0.0) {
    throw(AipsError("SpectralIndex::sample(...) - "
		    "the reference frequency is zero or negative"));
  }

  Double nu;
  for (uInt i = 0; i < nSamples; i++) {
    nu = frequencies(i).getValue();
    scale(i) = pow(nu/nu0, itsIndex);
  }
}

SpectralModel* SpectralIndex::clone() const {
  DebugAssert(ok(), AipsError);
  SpectralModel* tmpPtr = new SpectralIndex(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

uInt SpectralIndex::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 1;
}

void SpectralIndex::setParameters(const Vector<Double>& newSpectralParms) {
  DebugAssert(newSpectralParms.nelements() == nParameters(), AipsError);
  itsIndex = newSpectralParms(0);
  DebugAssert(ok(), AipsError);
}

Vector<Double> SpectralIndex::parameters() const {
  DebugAssert(ok(), AipsError);
  return Vector<Double>(1, itsIndex);
}

void SpectralIndex::setErrors(const Vector<Double>& newSpectralErrs) {
  DebugAssert(newSpectralErrs.nelements() == nParameters(), AipsError);
  if (newSpectralErrs(0) < 0.0) {
    LogIO logErr(LogOrigin("SpectralIndex", "setErrors(...)"));
    logErr << "The errors must be non-negative."
	   << LogIO::EXCEPTION;
  }
  itsError = newSpectralErrs(0);
  DebugAssert(ok(), AipsError);
}

Vector<Double> SpectralIndex::errors() const {
  DebugAssert(ok(), AipsError);
  return Vector<Double>(1, itsError);
}

Bool SpectralIndex::fromRecord(String& errorMessage, 
 			       const RecordInterface& record) {
  if (!SpectralModel::fromRecord(errorMessage, record)) return False;
  if (!record.isDefined(String("index"))) {
    errorMessage += "The 'spectrum' record must have an 'index' field\n";
    return False;
  }
//
  {
     const RecordFieldId index("index");
     const IPosition shape(1,1);
     if (record.shape(index) != shape) {
       errorMessage += "The 'index' field must be a scalar\n";
       return False;
     }
     Double indexVal;
     switch (record.dataType(index)) {
     case TpDouble:
     case TpFloat:
     case TpInt:
       indexVal = record.asDouble(index);
       break;
     default:
       errorMessage += "The 'index' field must be a real number\n";
       return False;
     }
     setIndex(indexVal);
  }
//
  {
      Vector<Double> errorVals(1, 0.0);
      if (record.isDefined("error")) {
        const RecordFieldId error("error");
        const IPosition shape(1,1);
        if (record.shape(error) != shape) {
          errorMessage += "The 'error' field must be a scalar\n";
          return False;
        }
        switch (record.dataType(error)) {
        case TpDouble:
        case TpFloat:
        case TpInt:
          errorVals[0] = record.asDouble(error);
          break;
        default:
          errorMessage += "The 'error' field must be a real number\n";
          return False;
        }
     }
//
     setErrors(errorVals);
  }
//
  DebugAssert(ok(), AipsError);
  return True;
}

Bool SpectralIndex::toRecord(String& errorMessage,
 			     RecordInterface& record) const {
  DebugAssert(ok(), AipsError);
  if (!SpectralModel::toRecord(errorMessage, record)) return False;
  record.define("index", index());
  record.define("error", errors()(0));
  return True;
}

Bool SpectralIndex::convertUnit(String& errorMessage,
				const RecordInterface& record) {
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
  if (!SpectralModel::ok()) return False;
  if (refFrequency().getValue().getValue() <= 0.0) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "The reference frequency is zero or negative!" 
           << LogIO::POST;
    return False;
  }
  if (abs(itsIndex) > 100) {
    LogIO logErr(LogOrigin("SpectralIndex", "ok()"));
    logErr << LogIO::SEVERE << "The spectral index is greater than 100!" 
           << LogIO::POST;
    return False;
  }
  return True;
}

// Local Variables: 
// compile-command: "gmake SpectralIndex"
// End: 

} //# NAMESPACE CASA - END

