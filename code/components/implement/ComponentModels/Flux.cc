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
#include <aips/Glish/GlishArray.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Glish/GlishValue.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Mathematics/Complex.h>
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
  for (uInt s = 0; s < 4; s++) {
    itsVal(s).re = fluxVal(s);
  }
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

template<class T> Unit FluxRep<T>::
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
setPol(const ComponentType::Polarisation & pol) {
  itsPol = pol;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
convertPol(const ComponentType::Polarisation & pol) {
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
  uInt len = value.getValue().nelements();
  DebugAssert(len == 4 || len == 0, AipsError);
  convertUnit(value.getFullUnit());
  convertPol(ComponentType::STOKES);
  Vector<T> & newValue = value.getValue();
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
  for (uInt i = 0; i < 4; i++) {
    itsVal(i) *= factor;
  }
  DebugAssert(ok(), AipsError);
}
template<class T> Bool FluxRep<T>::
fromRecord(String & errorMessage, const GlishRecord & record) {
  {
    if (!record.exists("polarisation")) {
      setPol(ComponentType::STOKES);
    } else {
      if (record.get("polarisation").type() != GlishValue::ARRAY) {
	errorMessage += "\nThe 'polarisation' field cannot be a record";
	return False;
      }
      const GlishArray polField = record.get("polarisation");
      if (polField.elementType() != GlishArray::STRING) {
	errorMessage += "\nThe 'polarisation' field must be a string";
	return False;
      }
      // Maybe the polarisation field should contain ["I", "Q", "U", "V"]. This
      // is harder to parse but more flexible for the future.
      if (polField.shape().product() != 1) {
	errorMessage += 
	  String("\nThe 'polarisation' field cannot be an array ");
	return False;
      }
      String polVal;
      if (!polField.get(polVal)) {
	errorMessage += String("\nCould not read the 'polarisation' field ") + 
	  String("in the flux record for an unknown reason");
	return False;
      }
      const ComponentType::Polarisation 
	pol(ComponentType::polarisation(polVal));
      if (pol == ComponentType::UNKNOWN_POLARISATION) {
	errorMessage += String("\nThe polarisation type is not known. ") +
	  String("\nCommon values are 'Stokes', 'Linear' & 'Circular'");
	return False;
      }
      setPol(pol);
    }
  }
  {
    if (!record.exists("value")) {
      errorMessage += "\nThe 'flux' record must have a 'value' field";
      return False;
    }
    if (record.get("value").type() != GlishValue::ARRAY) {
      errorMessage += "\nThe 'value' field cannot be a record";
      return False;
    }
    const GlishArray valueField = record.get("value");
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
    Vector<DComplex> fluxVal(4);
    if (!valueField.get(fluxVal.ac())) {
      errorMessage += String("\nCould not read the 'value' field ") + 
	String("in the flux record for an unknown reason");
      return False;
    }
    setValue(fluxVal);
  }
  {
    if (!record.exists("unit")) {
      errorMessage += "\nThe 'flux' record must have a 'unit' field";
      return False;
    }
    if (record.get("unit").type() != GlishValue::ARRAY) {
      errorMessage += "\nThe 'unit' field cannot be a record";
      return False;
    }
    const GlishArray unitField = record.get("unit");
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
    setUnit(Unit(unitVal));
  }
  return True;
}

template<class T> Bool FluxRep<T>::
toRecord(String & errorMessage, GlishRecord & record) const {
  if (pol() == ComponentType::STOKES) {
    FluxRep<Double> fluxCopy = *this;
    Vector<Double> fluxVal(4);
    fluxCopy.value(fluxVal);
    record.add("value", GlishArray(fluxVal.ac()));
    record.add("polarisation", ComponentType::name(ComponentType::STOKES));
  } else {
    record.add("value", GlishArray(value().ac()));
    record.add("polarisation", ComponentType::name(pol()));
  }
  record.add("unit", unit().getName());
  if (errorMessage == ""); // Suppress compiler warning about unused variable
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
	   << " different from 'Jy'"
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
  Vector<NumericTraits<T>::ConjugateType> thisVal(4);
  value(thisVal);
  Flux<T> newFlux(thisVal, pol());
  newFlux.setUnit(unit());
  return newFlux;
}

template<class T> Unit Flux<T>::
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
setPol(const ComponentType::Polarisation & pol) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setPol(pol);
}

template<class T> void Flux<T>::
convertPol(const ComponentType::Polarisation & pol) {
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
  DebugAssert(ok(), AipsError);
  itsFluxPtr->scaleValue(factor);
}

template<class T> Bool Flux<T>::
fromRecord(String & errorMessage, const GlishRecord & record) {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->fromRecord(errorMessage, record);
}

template<class T> Bool Flux<T>::
toRecord(String & errorMessage, GlishRecord & record) const {
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
