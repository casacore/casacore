//# ClassFileName.cc:  this defines ClassName, which ...
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
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/MVFrequency.h>
#include <aips/Measures/MCFrequency.h>
#include <trial/Tasking/MeasureParameterAccessor.h>
#include <trial/Measures/DOmeasures.h>
#include <aips/Utilities/String.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Glish/GlishValue.h>
#include <aips/Glish/GlishArray.h>

#ifdef __GNUG__
typedef MeasConvert<MFrequency,MVFrequency,MCFrequency> 
  gpp_measconvert_mfrequency_mvfrequency_mcfrequency;
#endif

SpectralIndex::SpectralIndex()
  :itsRefFreq(),
   itsIndex(0.0),
   itsRefFrame((MFrequency::Types) itsRefFreq.getRef().getType()),
   itsNu0(itsRefFreq.getValue().getValue())
{
  DebugAssert(ok(), AipsError);
}

SpectralIndex::SpectralIndex(const MFrequency & refFreq,
			     const Double & exponent)
  :itsRefFreq(refFreq),
   itsIndex(exponent),
   itsRefFrame((MFrequency::Types) itsRefFreq.getRef().getType()),
   itsNu0(itsRefFreq.getValue().getValue())
{
  DebugAssert(ok(), AipsError);
}

SpectralIndex::SpectralIndex(const SpectralIndex & other) 
  :itsRefFreq(other.itsRefFreq),
   itsIndex(other.itsIndex),
   itsRefFrame(other.itsRefFrame),
   itsNu0(other.itsNu0)
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
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::SpectralShape SpectralIndex::spectralShape() const {
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

const Double & SpectralIndex::index() const {
  DebugAssert(ok(), AipsError);
  return itsIndex;
}

void SpectralIndex::index(Double & index) const {
  DebugAssert(ok(), AipsError);
  index = itsIndex;
}

void SpectralIndex::setIndex(const Double & newIndex) {
  itsIndex = newIndex;
  DebugAssert(ok(), AipsError);
}

Double SpectralIndex::scale(const MFrequency & centerFreq) const {
  DebugAssert(ok(), AipsError);
  Double nu = centerFreq.getValue().getValue();
  DebugAssert(!nearAbs(itsNu0, 0.0, C::dbl_epsilon), AipsError);
  if (centerFreq.getRef().getType() != itsRefFrame) {
    nu = MFrequency::Convert(centerFreq, itsRefFrame)().getValue().getValue();
  }
  return pow(nu/itsNu0, itsIndex);
}

uInt SpectralIndex::nSpectralParameters() const {
  DebugAssert(ok(), AipsError);
  return 1;
}

void SpectralIndex::
setSpectralParameters(const Vector<Double> & newSpectralParms) {
  DebugAssert(newSpectralParms.nelements() == nSpectralParameters(),AipsError);
  itsIndex = newSpectralParms(0);
  DebugAssert(ok(), AipsError);
}

void SpectralIndex::spectralParameters(Vector<Double> & spectralParms) const {
  DebugAssert(ok(), AipsError);
  DebugAssert(spectralParms.nelements() == nSpectralParameters(),AipsError);
  spectralParms(0) = itsIndex;
}

Bool SpectralIndex::fromRecord(String & errorMessage, 
			       const GlishRecord & record) {
  {
    if (!record.exists("reference")) {
      errorMessage += "\nThe 'spectrum' record must have an 'reference' field";
      return False;
    }
    MeasureParameterAccessor<MFrequency> mpa(String("reference"),
					     ParameterSet::In, 
					     (GlishRecord *) &record);
    if (!mpa.copyIn(errorMessage)) return False;
    setRefFrequency(mpa());
  }
  {
    if (!record.exists("index")) {
      errorMessage += "\nThe 'spectrum' record must have an 'index' field";
      return False;
    }
    if (record.get("index").type() != GlishValue::ARRAY) {
      errorMessage += "\nThe 'index' field cannot be a record";
      return False;
    }
    const GlishArray indexField = record.get("index");
    if (indexField.elementType() == GlishArray::STRING) {
      errorMessage += "\nThe 'index' field cannot be a string";
      return False;
    }
    if (indexField.shape().product() != 1) {
      errorMessage += String("\nThe 'index' field cannot be an array ");
      return False;
    }
    Double indexVal;
    if (!indexField.get(indexVal)) {
      errorMessage += String("\nCould not read the 'index' field ") + 
	String("in the shape record for an unknown reason");
      return False;
    }
    setIndex(indexVal);
  }
  DebugAssert(ok(), AipsError);
  return True;
}

Bool SpectralIndex::toRecord(String & errorMessage,
			     GlishRecord & record) const {
  // Use errorMessage for something to suppress a compiler warning
  if (&errorMessage == 0) {
  }
  record.add("type", ComponentType::name(spectralShape()));
  record.add("reference", measures::toRecord(refFrequency()));
  record.add("index", index());
  return True;
}

Bool SpectralIndex::ok() const {
  return True;
}

// spectrum := [type = 'constant']
// spectrum := [type = 'spectralindex',
//              reference = MFrequency,
//              index = Double]
// spectrum := [type = 'discrete',
//              nsamples = Integer (> 0),
//              frequencies = Vector of MFrequencies,
//              fluxes = Vector of fluxes]
// Local Variables: 
// compile-command: "gmake OPTLIB=1 SpectralIndex"
// End: 
