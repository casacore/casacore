//# Flux.cc:
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

#include <trial/ComponentModels/Flux.h>
#include <aips/Measures/QuantumHolder.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>

template<class T> FluxRep<T>::
FluxRep()
  :itsVal(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  itsVal(0).re = T(1);
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(T i) 
  :itsVal(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  itsVal(0).re = i;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(T i, T q, T u, T v)
  :itsVal(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  itsVal(0).re = i;
  itsVal(1).re = q;
  itsVal(2).re = u;
  itsVal(3).re = v;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Vector<T> & flux)
  :itsVal(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  DebugAssert(flux.nelements() == 4, AipsError);
  for (uInt i = 0 ; i < 4; i++) {
    itsVal(i).re = flux(i);
  }
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Quantum<Vector<T> > & flux)
  :itsVal(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit(flux.getFullUnit())
{
  const Vector<T> & fluxVal(flux.getValue());
  DebugAssert(fluxVal.nelements() == 4, AipsError);
  convertArray(itsVal.ac(), fluxVal.ac());
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const FluxRep<T> & other) 
  :itsVal(other.itsVal.copy()),
   itsPol(other.itsPol),
   itsUnit(other.itsUnit)
{
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
~FluxRep() {
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T> & FluxRep<T>::
operator=(const FluxRep<T> & other) {
  if (this != &other) {
    itsVal = other.itsVal;
    itsPol = other.itsPol;
    itsUnit = other.itsUnit;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

template<class T> const Unit & FluxRep<T>::
unit() const {
  DebugAssert(ok(), AipsError);
  return itsUnit;
}

template<class T> void FluxRep<T>::
unit(Unit & unit) const {
  unit = itsUnit;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setUnit(const Unit & unit) {
  itsUnit = unit;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
convertUnit(const Unit & unit) {
  if (unit.getName() != itsUnit.getName()) {
    T factor = itsUnit.getValue().getFac()/unit.getValue().getFac();
    for (uInt i = 0; i < 4; i++) {
      itsVal(i) *= factor;
    }
    itsUnit = unit;
  }
  DebugAssert(ok(), AipsError);
}

template<class T> ComponentType::Polarisation FluxRep<T>::
pol() const {
  DebugAssert(ok(), AipsError);
  return itsPol;
}

template<class T> void FluxRep<T>::
pol(ComponentType::Polarisation & pol) const {
  pol = itsPol;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setPol(ComponentType::Polarisation pol) {
  itsPol = pol;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
convertPol(ComponentType::Polarisation pol) {
  if (itsPol != pol) {
    switch (pol){
    case ComponentType::STOKES:
      if (itsPol == ComponentType::LINEAR) {
 	Flux<T>::linearToStokes(itsVal, itsVal);
      } else {
 	Flux<T>::circularToStokes(itsVal, itsVal);
      }
      break;
    case ComponentType::LINEAR:
      if (itsPol == ComponentType::STOKES) {
 	Flux<T>::stokesToLinear(itsVal, itsVal);
      } else {
 	Flux<T>::circularToLinear(itsVal, itsVal);
      }
      break;
    case ComponentType::CIRCULAR:
      if (itsPol == ComponentType::STOKES) {
 	Flux<T>::stokesToCircular(itsVal, itsVal);
      } else {
 	Flux<T>::linearToCircular(itsVal, itsVal);
      }
      break;
    default:
      throw(AipsError("FluxRep<T>::convertPol(...) - bad polarisation type"));
    };    
    itsPol = pol;
  }
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
value(Vector<T> & value) {
  const uInt len = value.nelements();
  DebugAssert (len == 4 || len == 0, AipsError);
  if (len == 0) value.resize(4);
  convertPol(ComponentType::STOKES);
  for (uInt s = 0 ; s < 4; s++) {
    value(s) = itsVal(s).re;
  }
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
value(Quantum<Vector<T> > & value) {
  const Unit & curUnit = value.getFullUnit();
  if (curUnit != itsUnit) {
    value.setUnit(itsUnit);
  }
  convertPol(ComponentType::STOKES);
  Vector<T> & newValue = value.getValue();
  if (newValue.nelements() != 4) newValue.resize(4);
  for (uInt s = 0 ; s < 4; s++) {
    newValue(s) = itsVal(s).re;
  }
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(T value) {
  for (uInt i = 0; i < 4; i++) {
    itsVal(i).im = itsVal(i).re = T(0.0);
  }
  itsVal(0).re = value;
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Vector<T> & value) {
  DebugAssert (value.nelements() == 4, AipsError);
  for (uInt i = 0; i < 4; i++) {
    itsVal(i).re = value(i);
    itsVal(i).im = T(0.0);
  }
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Quantum<Vector<T> > & value) {
  DebugAssert (value.getValue().nelements() == 4, AipsError);
  const Vector<T> & val = value.getValue();
  for (uInt s = 0; s < 4; s++) {
    itsVal(s).re = val(s);
    itsVal(s).im = T(0);
  }
  itsUnit = value.getFullUnit();
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
scaleValue(const T & factor) {
  itsVal(0) *= factor;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
scaleValue(const T & factor0, const T & factor1, 
	   const T & factor2, const T & factor3) {
  itsVal(0) *= factor0;
  itsVal(1) *= factor1;
  itsVal(2) *= factor2;
  itsVal(3) *= factor3;
  DebugAssert(ok(), AipsError);
}

template<class T> Bool FluxRep<T>::
fromRecord(String & errorMessage, const RecordInterface & record) {
  {
    const String polarisationString("polarisation");
    if (!record.isDefined(polarisationString)) {
      LogIO logErr(LogOrigin("FluxRep", "fromRecord()"));
      logErr << LogIO::WARN 
	     << "The flux does not have a 'polarisation' field." << endl
	     << "Using the default of Stokes"
	     << LogIO::POST;
      setPol(ComponentType::STOKES);
    } else {
      const RecordFieldId polarisation(polarisationString);
      // Maybe the polarisation field should contain ["I", "Q", "U", "V"]. This
      // is harder to parse but more flexible for the future.
      if (record.dataType(polarisation) != TpString) {
	errorMessage += "The 'polarisation' field must be a record\n";
	return False;
      }      
      if (record.shape(polarisation) != IPosition(1,1)) {
	errorMessage += "The 'polarisation' field must have only 1 element\n";
	return False;
      } 
      const String polVal = record.asString(polarisation);
      const ComponentType::Polarisation 
	newPol(ComponentType::polarisation(polVal));
      if (newPol == ComponentType::UNKNOWN_POLARISATION) {
	errorMessage += String("The polarisation type is not known.\n") +
	  String("Allowed values are 'Stokes', 'Linear' & 'Circular'\n");
	return False;
      }
      setPol(newPol);
    }
  }
  {
    QuantumHolder qh;
    if (!qh.fromRecord(errorMessage, record)) {
      errorMessage += "Could not parse the flux record\n";
      return False;
    }
    if (qh.isScalar() && pol() == ComponentType::STOKES) {
      if (qh.isReal()) {
	const Quantum<Double> & qVal = qh.asQuantumDouble();
	setValue(T(qVal.getValue()));
	setUnit(qVal.getFullUnit());
      } else {
	const Quantum<DComplex> & qVal = qh.asQuantumDComplex();
	const DComplex & val = qVal.getValue();
	if (!nearAbs(val.im, 0.0, NumericTraits<T>::minimum)) {
	  errorMessage += "I value cannot be complex\n";
	  return False;
	}
	setValue(T(val.re));
	setUnit(qVal.getFullUnit());
      }
      LogIO logErr(LogOrigin("FluxRep", "fromRecord()"));
      logErr << LogIO::WARN
	     << "Only the I flux specified. Assuming Q, U, & V are zero\n" 
	     << LogIO::POST;
    } else {
      if (qh.nelements() != 4u) {
	errorMessage += String("Must specify all 4 flux values\n")
	  + String("if the polarisation representation is not Stokes\n");
	return False;
      }
      if (qh.isQuantumVectorDouble()) {
	const Quantum<Vector<Double> > qVal = qh.asQuantumVectorDouble();
	setUnit(qVal.getFullUnit());
	Vector<NumericTraits<T>::ConjugateType> val(4);
	convertArray(val.ac(), qVal.getValue().ac());
	setValue(val);
      } else if (qh.isQuantumVectorDComplex()) {
	const Quantum<Vector<DComplex> > & qVal = qh.asQuantumVectorDComplex();
	setUnit(qVal.getFullUnit());
	Vector<NumericTraits<T>::ConjugateType> val(4);
	convertArray(val.ac(), qVal.getValue().ac());
	setValue(val);
      } else if (qh.isQuantumVectorComplex()) {
	const Quantum<Vector<Complex> > & qVal = qh.asQuantumVectorComplex();
	setUnit(qVal.getFullUnit());
	Vector<NumericTraits<T>::ConjugateType> val(4);
	convertArray(val.ac(), qVal.getValue().ac());
	setValue(val);
      } else if (qh.isQuantumVectorFloat()) {
	const Quantum<Vector<Float> > & qVal = qh.asQuantumVectorFloat();
	setUnit(qVal.getFullUnit());
	Vector<NumericTraits<T>::ConjugateType> val(4);
	convertArray(val.ac(), qVal.getValue().ac());
	setValue(val);
      } else if (qh.isQuantumVectorInt()) {
	const Quantum<Vector<Int> > & qVal = qh.asQuantumVectorInt();
	setUnit(qVal.getFullUnit());
	Vector<NumericTraits<T>::ConjugateType> val(4);
	convertArray(val.ac(), qVal.getValue().ac());
	setValue(val);
      } else {
	errorMessage += "value field must be a real or complex vector\n";
	return False;
      }
    }
  }
  if (unit() != Unit("Jy")) {
    errorMessage += "The dimensions of the units must be same as the Jy\n";
    return False;
  }
  if (!ok()) {
    errorMessage += "Inconsistancies in the FluxRep object\n";
    return False;
  }
  return True;
}

template<class T> Bool FluxRep<T>::
toRecord(String & errorMessage, RecordInterface & record) const {
  if (!ok()) {
    errorMessage += "Inconsistancies in the FluxRep object\n";
    return False;
  }
  QuantumHolder qh;
  if (pol() == ComponentType::STOKES) {
    FluxRep<T> fluxCopy = *this;
    Quantum<Vector<T> > qVal;
    fluxCopy.value(qVal);
    qh = QuantumHolder(qVal);
  } else {
    Quantum<Vector<NumericTraits<T>::ConjugateType> > qVal;
    value(qVal);
    qh = QuantumHolder(qVal);
  }
  if (!qh.toRecord(errorMessage, record)) {
    errorMessage += "Problem generating the flux record\n";
    return False;
  }
  record.define(RecordFieldId("polarisation"), ComponentType::name(pol()));
  return True;
}


template<class T> Bool FluxRep<T>::
ok() const {
  // The LogIO class is only constructed if an Error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (itsVal.nelements() != 4) {
    LogIO logErr(LogOrigin("FluxRep", "ok()"));
    logErr << LogIO::SEVERE << "The flux does not have 4 elements"
 	   << " (corresponding to four polarisations)"
	   << LogIO::POST;
    return False;
  }
  if (itsUnit != Unit("Jy")) {
    LogIO logErr(LogOrigin("FluxRep", "ok()"));
    logErr << LogIO::SEVERE << "The flux units have dimensions that are "
	   << "different from 'Jy'"
	   << LogIO::POST;
    return False;
  }
  return True;
}

template<class T> Flux<T>::
Flux()
  :itsFluxPtr(new FluxRep<T>)
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(T i)
  :itsFluxPtr(new FluxRep<T>(i))
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(T i, T q, T u, T v)
 :itsFluxPtr(new FluxRep<T>(i, q, u, v))
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(const Vector<T> & flux)
  :itsFluxPtr(new FluxRep<T>(flux))
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(const Quantum<Vector<T> > & flux)
  :itsFluxPtr(new FluxRep<T>(flux))
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(const Flux<T> & other) 
  :itsFluxPtr(other.itsFluxPtr)
{ 
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
~Flux() {
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T> & Flux<T>::
operator=(const Flux<T> & other) {
  if (this != &other) {
    itsFluxPtr = other.itsFluxPtr;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

template<class T> Flux<T> Flux<T>::
copy() const {
  DebugAssert(ok(), AipsError);
  Flux<T> newFlux(value(), pol());
  newFlux.setUnit(unit());
  return newFlux;
}

template<class T> const Unit & Flux<T>::
unit() const {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->unit();
}

template<class T> void Flux<T>::
unit(Unit & unit) const {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->unit(unit);
}

template<class T> void Flux<T>::
setUnit(const Unit & unit) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setUnit(unit);
}

template<class T> void Flux<T>::
convertUnit(const Unit & unit) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->convertUnit(unit);
}

template<class T> ComponentType::Polarisation Flux<T>::
pol() const {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->pol();
}

template<class T> void Flux<T>::
pol(ComponentType::Polarisation & pol) const {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->pol(pol);
}

template<class T> void Flux<T>::
setPol(ComponentType::Polarisation pol) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setPol(pol);
}

template<class T> void Flux<T>::
convertPol(ComponentType::Polarisation pol) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->convertPol(pol);
}

template<class T> void Flux<T>::
value(Vector<T> & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
value(Quantum<Vector<T> > & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
setValue(T value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const Vector<T> & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const Quantum<Vector<T> > & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
scaleValue(const T & factor) {
  itsFluxPtr->scaleValue(factor);
  DebugAssert(ok(), AipsError);
}

template<class T> void Flux<T>::
scaleValue(const T & factor0, const T & factor1, 
	   const T & factor2, const T & factor3) {
  itsFluxPtr->scaleValue(factor0, factor1, factor2, factor3);
  DebugAssert(ok(), AipsError);
}

template<class T> Bool Flux<T>::
fromRecord(String & errorMessage, const RecordInterface & record) {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->fromRecord(errorMessage, record);
}

template<class T> Bool Flux<T>::
toRecord(String & errorMessage, RecordInterface & record) const {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->toRecord(errorMessage, record);
}

template<class T> Bool Flux<T>::
ok() const {
  if (itsFluxPtr.null() == True) {
    LogIO logErr(LogOrigin("Flux", "ok()"));
    logErr << LogIO::SEVERE << "Internal pointer is not pointing to anything"
           << LogIO::POST;
    return False;
  }
  if (itsFluxPtr->ok() == False) {
    LogIO logErr(LogOrigin("Flux", "ok()"));
    logErr << LogIO::SEVERE << "Flux representation is not ok"
           << LogIO::POST;
    return False;
  }
  return True;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 Flux"
// End: 
