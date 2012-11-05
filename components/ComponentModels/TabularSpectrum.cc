//# TabularSpectrum.cc:
//# Copyright (C) 2010
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

#include <components/ComponentModels/TabularSpectrum.h>
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
#include <measures/Measures/MeasureHolder.h>
#include <casa/Containers/Record.h>
#include <casa/Quanta/MVFrequency.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/DataType.h>
#include <casa/BasicSL/String.h>
#include <scimath/Mathematics/InterpolateArray1D.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TabularSpectrum::TabularSpectrum()
  :SpectralModel(),
   tabFreqVal_p(0),
   flux_p(0), ival_p(0), referenceFreq_p(0.0), maxFreq_p(0.0), minFreq_p(0.0)
{
  freqRef_p=MFrequency::Ref(MFrequency::LSRK);

  DebugAssert(ok(), AipsError);
}

TabularSpectrum::TabularSpectrum(const MFrequency& refFreq,
                                 const Vector<MFrequency::MVType>& freq,
                                 const Vector<Flux<Double> >& flux,
                                 const MFrequency::Ref& refFrame)
  :SpectralModel(refFreq)
{

  Bool stupidTransform = (refFrame.getType() == MFrequency::REST) ||  (refFrame.getType() == MFrequency::N_Types) || (refFreq.getRef().getType() == MFrequency::REST) ||  (refFreq.getRef().getType() == MFrequency::N_Types);

  if (refFrame.getType() != refFreq.getRef().getType() && !stupidTransform) {
    referenceFreq_p = MFrequency::Convert(refFreq, refFrame)().getValue().get("Hz").getValue();
  } else {
    referenceFreq_p = refFreq.getValue().get("Hz").getValue();
  }
  setValues(freq, flux, refFrame);
  DebugAssert(ok(), AipsError);
}

TabularSpectrum::TabularSpectrum(const TabularSpectrum& other) 
  :SpectralModel(other)
{
  operator=(other);
  DebugAssert(ok(), AipsError);
}

TabularSpectrum::~TabularSpectrum() {
  DebugAssert(ok(), AipsError);
}

TabularSpectrum& TabularSpectrum::operator=(const TabularSpectrum& other) {
  if (this != &other) {
    SpectralModel::operator=(other);
    freqRef_p=other.freqRef_p;
    tabFreqVal_p.resize();
    tabFreqVal_p=other.tabFreqVal_p;
    flux_p.resize();
    flux_p=other.flux_p;
    referenceFreq_p=other.referenceFreq_p;
    maxFreq_p=other.maxFreq_p;
    minFreq_p=other.minFreq_p;
    ival_p=other.ival_p;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::SpectralShape TabularSpectrum::type() const {
  return ComponentType::TABULAR_SPECTRUM;
}

void TabularSpectrum::values(Vector<MFrequency::MVType>& freq, Vector<Flux<Double> >& flux) const {
   freq.resize(tabFreqVal_p.nelements());
   flux.resize(flux_p.nelements());
   flux=flux_p;
   for (uInt k=0; k < tabFreqVal_p.nelements(); ++k){
     freq(k)=MVFrequency(Quantity(tabFreqVal_p(k), "Hz"));
   }
}

void TabularSpectrum::setValues(const Vector<MFrequency::MVType>& frequencies, const Vector<Flux<Double> >& flux, const MFrequency::Ref& refFrame) { 
  if(flux.nelements() != frequencies.nelements()){
    throw(AipsError("frequencies length is not equal to flux length in TabularSpectrum::setValues"));
  }

  Bool stupidTransform = (refFrame.getType() == MFrequency::REST) ||  (refFrame.getType() == MFrequency::N_Types) || (refFrequency().getRef().getType() == MFrequency::REST) ||  (refFrequency().getRef().getType() == MFrequency::N_Types);

  if (refFrame.getType() != refFrequency().getRef().getType() && !stupidTransform) {
    referenceFreq_p = MFrequency::Convert(refFrequency(), refFrame)().getValue().get("Hz").getValue();
  } else {
    referenceFreq_p = refFrequency().getValue().get("Hz").getValue();
  }

  freqRef_p=refFrame;
  tabFreqVal_p.resize(frequencies.nelements());
  flux_p.resize();
  flux_p=flux;
  ival_p.resize(frequencies.nelements());
  for (uInt k=0; k < frequencies.nelements(); ++k){
    tabFreqVal_p(k)=frequencies(k).get("Hz").getValue();
    //IQUV
    flux_p(k).convertPol(ComponentType::STOKES);
    ival_p(k)=flux_p(k).value(Stokes::I).getValue();
  }
  maxFreq_p=max(tabFreqVal_p);
  minFreq_p=min(tabFreqVal_p);
}

Double TabularSpectrum::sample(const MFrequency& centerFreq) const {
  const MFrequency& refFreq(refFrequency());
  const MFrequency::Ref& centerFreqFrame(centerFreq.getRef());
  Double nu;

  Bool stupidTransform = (centerFreqFrame.getType() == MFrequency::REST) ||  (centerFreqFrame.getType() == MFrequency::N_Types) || (freqRef_p.getType() == MFrequency::REST) ||  (freqRef_p.getType() == MFrequency::N_Types);
  if (centerFreqFrame.getType() != freqRef_p.getType() && !stupidTransform) {
    nu = MFrequency::Convert(centerFreq, freqRef_p)().getValue().get("Hz").getValue();
  } else {
    nu = refFreq.getValue().get("Hz").getValue();
  }
  if (nu < minFreq_p || nu > maxFreq_p) {
    throw(AipsError("TabularSpectrun::sample(...) - "
		    "the frequency requested out of range"));
  }

  Vector<Double> xout(1, referenceFreq_p);
  Vector<Double> scale(1,0.0);
  InterpolateArray1D<Double, Double>::interpolate(scale, xout, tabFreqVal_p, ival_p, InterpolateArray1D<Double, Double>::linear);
  Double refy=scale[0];
  xout[0]=nu;
  InterpolateArray1D<Double, Double>::interpolate(scale, xout, tabFreqVal_p, ival_p, InterpolateArray1D<Double, Double>::linear);
  

  if(refy !=0.0){
    return scale[0]/refy;
  }
  else if(max(ival_p) !=0.0)
    return scale[0]/max(ival_p);
  
  return 0.0 ;
}

void TabularSpectrum::sample(Vector<Double>& scale, 
			   const Vector<MFrequency::MVType>& frequencies, 
			   const MFrequency::Ref& refFrame) const {
  const uInt nSamples = frequencies.nelements();
  DebugAssert(scale.nelements() == nSamples, AipsError);

  MFrequency::Convert toThisFrame(refFrame, freqRef_p);
  Vector<Double> nu(frequencies.nelements());
  //try frame conversion only if it is not something stupid...
  //if it is then assume the frequencies are fine as is.
  Bool stupidTransform = (refFrame.getType() == MFrequency::REST) ||  (refFrame.getType() == MFrequency::N_Types) || (freqRef_p.getType() == MFrequency::REST) ||  (freqRef_p.getType() == MFrequency::N_Types);
  if ((refFrame.getType() != freqRef_p.getType()) && !stupidTransform) {
    for(uInt k=0; k < nSamples; ++k){
      nu(k) = toThisFrame(frequencies(k).getValue()).getValue().getValue();
    }
  } else {
    for(uInt k=0; k< nSamples; ++k){
      nu(k) = frequencies(k).getValue();
    }
  }
  Vector<Double> xout(1, referenceFreq_p);
  Vector<Double> refVal(1,0.0);
  InterpolateArray1D<Double, Double>::interpolate(refVal, xout, tabFreqVal_p, ival_p, InterpolateArray1D<Double, Double>::linear);
  scale.resize(nSamples);
  InterpolateArray1D<Double, Double>::interpolate(scale, nu, tabFreqVal_p, ival_p, InterpolateArray1D<Double, Double>::linear);
  if(refVal(0) !=0.0){
    for (uInt i = 0; i < nSamples; i++) {
      scale(i) = scale(i)/refVal(0);
    }
  }
  else{
    if(max(scale) != 0.0)
      scale /= max(scale);
  }

}

SpectralModel* TabularSpectrum::clone() const {
  DebugAssert(ok(), AipsError);
  SpectralModel* tmpPtr = new TabularSpectrum(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

uInt TabularSpectrum::nParameters() const {
  return 0;
}

void TabularSpectrum::setParameters(const Vector<Double>& newSpectralParms) {
  AlwaysAssert(newSpectralParms.nelements() == nParameters(), AipsError);
}

Vector<Double> TabularSpectrum::parameters() const {
  return Vector<Double>(0);
}

void TabularSpectrum::setErrors(const Vector<Double>& newSpectralErrs) {
  AlwaysAssert(newSpectralErrs.nelements() == nParameters(), AipsError);
}

Vector<Double> TabularSpectrum::errors() const {
  return Vector<Double>(0);
}

Bool TabularSpectrum::fromRecord(String& errorMessage, 
 			       const RecordInterface& record) {
  if (!SpectralModel::fromRecord(errorMessage, record)) return False;
  //freqRef
  if (!record.isDefined(String("freqRef"))) {
    errorMessage += "The 'TabularSpectrum' record must have an 'freqRef' field\n";
    return False;
  }
  else{
    Record theTmpMF(record.asRecord("freqRef"));
    MeasureHolder mh;
    if(!mh.fromRecord(errorMessage, theTmpMF))
      return False;
    if(mh.isMFrequency())
      freqRef_p=(mh.asMFrequency().getRef());
    else
      return False;
  }


//tabFreqVal
if (!record.isDefined(String("tabFreqVal"))) {
    errorMessage += "The 'TabularSpectrum' record must have an 'tabFreqVal' field\n";
    return False;
  }
  else{
    tabFreqVal_p.resize();
    tabFreqVal_p=Vector<Double> (record.asArrayDouble("tabFreqVal"));
  }
////ival
 if (!record.isDefined(String("ival"))) {
   errorMessage += "The 'TabularSpectrum' record must have an 'ival' field\n";
    return False;
 }
 else{
    ival_p.resize();
    ival_p=Vector<Double> (record.asArrayDouble("ival"));
  }

//referenceFreq
 if (!record.isDefined(String("referenceFreq"))) {
   errorMessage += "The 'TabularSpectrum' record must have an 'referenceFreq' field\n";
    return False;
 }
 else{
   referenceFreq_p=record.asDouble("referenceFreq");
 }
 setRefFrequency(MFrequency(Quantity(referenceFreq_p, "Hz"), freqRef_p));
//maxFreq and minFreq
 if (!record.isDefined(String("maxFreq")) || !record.isDefined(String("minFreq")) ) {
   errorMessage += "The 'TabularSpectrum' record must have a 'maxFreq' and a 'minFreq' field\n";
   return False;
 }
 else{
   maxFreq_p=record.asDouble("maxFreq");
   minFreq_p=record.asDouble("minFreq");
 }

  return True;
}

Bool TabularSpectrum::toRecord(String& errorMessage,
 			     RecordInterface& record) const {
  DebugAssert(ok(), AipsError);
  if (!SpectralModel::toRecord(errorMessage, record)) return False;
  //save frame in a temporary MFrequency object
  MFrequency tmpMF(Quantity(0, "Hz"), freqRef_p);
  MeasureHolder mh(tmpMF);
  Record outRec;
  if(!mh.toRecord(errorMessage, outRec))
    return False;
  record.defineRecord("freqRef",outRec);
  record.define("tabFreqVal", tabFreqVal_p);
  record.define("ival", ival_p);
  record.define("referenceFreq", referenceFreq_p);
  record.define("maxFreq", maxFreq_p);
  record.define("minFreq", minFreq_p);
  return True;
}

Bool TabularSpectrum::convertUnit(String& errorMessage,
				const RecordInterface& record) {
  if (!record.isDefined("freqRef")){
    errorMessage+="Not a tabularSpectrum object";
    return False;
  }
  return True;
}

Bool TabularSpectrum::ok() const {
  if (!SpectralModel::ok()) return False;
  if (refFrequency().getValue().getValue() <= 0.0) {
    LogIO logErr(LogOrigin("TabularSpectrum", "ok()"));
    logErr << LogIO::SEVERE << "The reference frequency is zero or negative!" 
           << LogIO::POST;
    return False;
  }
  return True;
}

// Local Variables: 
// compile-command: "gmake SpectralIndex"
// End: 

} //# NAMESPACE CASA - END

