//# Flux.cc:
//# Copyright (C) 1998,1999,2000,2001,2002
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

#include <components/ComponentModels/Flux.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicSL/Constants.h>
#include <measures/Measures/Stokes.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/DataType.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T> Vector<String> FluxRep<T>::_allowedUnits(0);

template<class T> FluxRep<T>::
FluxRep()
  :itsVal(4, typename NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy"),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  itsVal(0) = 1;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(T i) 
  :itsVal(4, typename NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy"),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  itsVal(0) = i;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(T i, T q, T u, T v)
  :itsVal(4, typename NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy"),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  itsVal(0) = i;
  itsVal(1) = q;
  itsVal(2) = u;
  itsVal(3) = v;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(typename NumericTraits<T>::ConjugateType xx,
	typename NumericTraits<T>::ConjugateType xy,
	typename NumericTraits<T>::ConjugateType yx,
	typename NumericTraits<T>::ConjugateType yy,
	ComponentType::Polarisation pol)
  :itsVal(4),
   itsPol(pol),
   itsUnit("Jy"),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  itsVal(0) = xx;
  itsVal(1) = xy;
  itsVal(2) = yx;
  itsVal(3) = yy;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Vector<T>& flux)
  :itsVal(4, typename NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy"),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  DebugAssert(flux.nelements() == 4, AipsError);
  for (uInt i = 0 ; i < 4; i++) {
    itsVal(i) = flux(i);
  }
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Vector<typename NumericTraits<T>::ConjugateType>& flux,
	ComponentType::Polarisation pol)
  :itsVal(flux.copy()),
   itsPol(pol),
   itsUnit("Jy"),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Quantum<Vector<T> >& flux)
  :itsVal(4, typename NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit(flux.getFullUnit()),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  const Vector<T>& fluxVal(flux.getValue());
  DebugAssert(fluxVal.nelements() == 4, AipsError);
  convertArray(itsVal, fluxVal);
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Quantum<Vector<typename NumericTraits<T>::ConjugateType> >& flux,
	ComponentType::Polarisation pol)
  :itsVal(flux.getValue().copy()),
   itsPol(pol),
   itsUnit(flux.getFullUnit()),
   itsErr(4, typename NumericTraits<T>::ConjugateType(0,0))
{
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const FluxRep<T>& other) 
  :itsVal(other.itsVal.copy()),
   itsPol(other.itsPol),
   itsUnit(other.itsUnit),
   itsErr(other.itsErr)
{
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
~FluxRep() {
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>& FluxRep<T>::
operator=(const FluxRep<T>& other) {
  if (this != &other) {
    itsVal = other.itsVal;
    itsPol = other.itsPol;
    itsUnit = other.itsUnit;
    itsErr = other.itsErr;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

template<class T> const Unit& FluxRep<T>::
unit() const {
  DebugAssert(ok(), AipsError);
  return itsUnit;
}

template<class T> void FluxRep<T>::
unit(Unit& unit) const {
  unit = itsUnit;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setUnit(const Unit& unit) {
  itsUnit = unit;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
convertUnit(const Unit& unit) {
  if (unit.getName() != itsUnit.getName()) {
    T factor = itsUnit.getValue().getFac()/unit.getValue().getFac();
    for (uInt i = 0; i < 4; i++) {
      itsVal(i) *= factor;
      itsErr(i) *= factor;
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
pol(ComponentType::Polarisation& pol) const {
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

    setPol(pol);                     // New pol rep label

    if (!allNearAbs(itsErr, typename NumericTraits<T>::ConjugateType(0,0), 
		    C::dbl_epsilon)) {
      LogIO logErr(LogOrigin("FluxRep", "convertPol()"));
      logErr << LogIO::WARN 
	     << "The change in flux representation means the numerical values"
	     << " have changed" << endl
	     << "But the errors in the flux are not being changed "
	     << "and are probably now incorrect"
	     << LogIO::POST;
    }
  }
  DebugAssert(ok(), AipsError);
}

template<class T>
const Vector<typename NumericTraits<T>::ConjugateType>& FluxRep<T>::
value() const {
  DebugAssert(ok(), AipsError);
  return itsVal;
}

template<class T>
const typename NumericTraits<T>::ConjugateType& FluxRep<T>::
value(uInt p) const {
  DebugAssert(p < 4, AipsError);
  DebugAssert(ok(), AipsError);
  return itsVal(p);
}

template<class T> void FluxRep<T>::
value(Vector<T>& value) {
  const uInt len = value.nelements();
  DebugAssert (len == 4 || len == 0, AipsError);
  if (len == 0) value.resize(4);
  convertPol(ComponentType::STOKES);
  for (uInt s = 0 ; s < 4; s++) {
    value(s) = itsVal(s).real();
  }
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
value(Vector<typename NumericTraits<T>::ConjugateType>& value) const {
  DebugAssert(ok(), AipsError);
  DebugAssert (value.nelements() == 4 || value.nelements() == 0, AipsError);
  value = itsVal;
}

template<class T> void FluxRep<T>::
value(Quantum<Vector<T> >& value) {
  const Unit& curUnit = value.getFullUnit();
  if (curUnit != itsUnit) {
    value.setUnit(itsUnit);
  }
  convertPol(ComponentType::STOKES);
  Vector<T>& newValue = value.getValue();
  if (newValue.nelements() != 4) newValue.resize(4);
  for (uInt s = 0 ; s < 4; s++) {
    newValue(s) = itsVal(s).real();
  }
  DebugAssert(ok(), AipsError);
}

template<class T> Quantum<T>  FluxRep<T>::
value (Stokes::StokesTypes stokes, Bool toJy) 
{
   LogIO os(LogOrigin("FluxRep", "value(Stokes::StokesTypes)"));

// Get the vector of values that we are holding
  
   Vector<T> values;
   value(values);
  
// Make a quantum for the result
      
   Quantum<T> value;
   value.setUnit(unit());

// Fish it out
   
   ComponentType::Polarisation cPol = pol();
   if ( (stokes==Stokes::I || stokes==Stokes::Q ||
         stokes==Stokes::U || stokes==Stokes::V)  &&
         cPol==ComponentType::STOKES) {
//
      String error("Failed to extract Flux from SkyComponent because not enough Stokes values");
      if (stokes==Stokes::I) {
         if (values.nelements()<1) os << error << LogIO::EXCEPTION;
         value.setValue(values(0));
      } else if (stokes==Stokes::Q) {
         if (values.nelements()<2) os << error << LogIO::EXCEPTION;
         value.setValue(values(1));
      } else if (stokes==Stokes::U) {
         if (values.nelements()<3) os << error << LogIO::EXCEPTION;
         value.setValue(values(2));
      } else if (stokes==Stokes::V) {
         if (values.nelements()<4) os << error << LogIO::EXCEPTION;
         value.setValue(values(3));
      }
   } else {
      os << "It is not possible currently to extract the flux value" << endl;
      os << "for Stokes type " << Stokes::name(stokes)  << " from the SkyComponent" << LogIO::EXCEPTION;
   }
//
   if (toJy) value.convert(Unit("Jy"));
   return value;
}

template<class T> void FluxRep<T>::
value(Quantum<Vector<typename NumericTraits<T>::ConjugateType> >&
      value) const {
  const Unit& curUnit = value.getFullUnit();
  if (curUnit != itsUnit) {
    value.setUnit(itsUnit);
  }
  Vector<typename NumericTraits<T>::ConjugateType>& newValue = value.getValue();
  if (newValue.nelements() != 4) newValue.resize(4);
  for (uInt s = 0 ; s < 4; s++) {
    newValue(s) = itsVal(s);
  }
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(T value) {
  for (uInt i = 0; i < 4; i++) {
    itsVal(i) = 0.0;
  }
  itsVal(0) = value;
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Vector<T>& value) {
  DebugAssert (value.nelements() == 4, AipsError);
  for (uInt i = 0; i < 4; i++) {
    itsVal(i) = value(i);
  }
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Vector<typename NumericTraits<T>::ConjugateType>& value) {
  DebugAssert (value.nelements() == 4, AipsError);
  itsVal = value;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Quantum<Vector<T> >& value) {
  DebugAssert (value.getValue().nelements() == 4, AipsError);
  const Vector<T>& val = value.getValue();
  for (uInt s = 0; s < 4; s++) {
    itsVal(s) = val(s);
  }
  itsUnit = value.getFullUnit();
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const
	 Quantum<Vector<typename NumericTraits<T>::ConjugateType> >& value,
	 ComponentType::Polarisation pol) {
  DebugAssert (value.getValue().nelements() == 4, AipsError);
  itsVal = value.getValue();
  itsUnit = value.getFullUnit();
  itsPol = pol;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::setValue(
		const Quantum<T>& value, Stokes::StokesTypes stokes
) {
	LogIO os(LogOrigin("FluxRep", "setValue(const Quantum<Double>&, Stokes::StokesTypes)"));
	Vector<T> tmp(4, 0.0);
	String conversionUnit = "Jy";
	if (! value.isConform("Jy")) {
		Bool found = False;
		for (
			Vector<String>::const_iterator iter=_allowedUnits.begin();
			iter != _allowedUnits.end(); iter++
		) {
			if (value.isConform(*iter)) {
				conversionUnit = *iter;
				found = True;
				break;
			}
		}
		if (! found) {
			os << LogIO::EXCEPTION << "The flux units "
				<< value.getFullUnit().getName() << " have dimensions that are "
				<< "different from 'Jy' and are not allowed";
		}
	}
	if (stokes==Stokes::I || stokes==Stokes::Q || stokes==Stokes::U || stokes==Stokes::V) {
		if (stokes==Stokes::I) {
			tmp(0) = value.getValue(Unit(conversionUnit));
		} else if (stokes==Stokes::Q) {
			tmp(1) = value.getValue(Unit(conversionUnit));
		} else if (stokes==Stokes::U) {
			tmp(2) = value.getValue(Unit(conversionUnit));
		} else if (stokes==Stokes::V) {
			tmp(3) = value.getValue(Unit(conversionUnit));
		}
	}
	else {
		os << LogIO::WARN << "Can only properly handle I,Q,U,V presently." << endl;
		os << "The brightness is assumed to be Stokes I"  << LogIO::POST;
		tmp(0) = value.getValue(Unit("Jy"));
	}
	setValue(tmp);
}

template<class T> void FluxRep<T>::
scaleValue(const T& factor) {
  itsVal(0) *= factor;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
scaleValue(const T& factor0, const T& factor1, 
	   const T& factor2, const T& factor3) {
  itsVal(0) *= factor0;
  itsVal(1) *= factor1;
  itsVal(2) *= factor2;
  itsVal(3) *= factor3;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
scaleValue(const typename  NumericTraits<T>::ConjugateType& factor) {
  itsVal(0) *= factor;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
scaleValue(const typename NumericTraits<T>::ConjugateType& factor0,
	   const typename NumericTraits<T>::ConjugateType& factor1,
	   const typename NumericTraits<T>::ConjugateType& factor2,
	   const typename NumericTraits<T>::ConjugateType& factor3) {
  itsVal(0) *= factor0;
  itsVal(1) *= factor1;
  itsVal(2) *= factor2;
  itsVal(3) *= factor3;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setErrors(const typename NumericTraits<T>::ConjugateType& error0,
	  const typename NumericTraits<T>::ConjugateType& error1,
	  const typename NumericTraits<T>::ConjugateType& error2,
	  const typename NumericTraits<T>::ConjugateType& error3) {
  itsErr(0) = error0;
  itsErr(1) = error1;
  itsErr(2) = error2;
  itsErr(3) = error3;
}

template<class T> void FluxRep<T>::setErrors(
	const Vector<typename NumericTraits<T>::ConjugateType>& errors
) {
	itsErr = errors;
}


template<class T> const
Vector<typename NumericTraits<T>::ConjugateType>& FluxRep<T>::errors() const {
  return itsErr;
}

template<class T> Bool FluxRep<T>::
fromRecord(String& errorMessage, const RecordInterface& record) {
  ComponentType::Polarisation newPol = ComponentType::UNKNOWN_POLARISATION;
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
      newPol = ComponentType::polarisation(polVal);
      if (newPol == ComponentType::UNKNOWN_POLARISATION) {
	errorMessage += String("The polarisation type is not known.\n") +
	  String("Allowed values are 'Stokes', 'Linear' & 'Circular'\n");
	return False;
      }
    }
  }
  setPol(newPol);
  {
    QuantumHolder qh;
    if (!qh.fromRecord(errorMessage, record)) {
      errorMessage += "Could not parse the flux record\n";
      return False;
    }
    if (qh.isScalar() && pol() == ComponentType::STOKES) {
      if (qh.isReal()) {
	const Quantum<Double>& qVal = qh.asQuantumDouble();
	setValue(qVal.getValue());
	setUnit(qVal.getFullUnit());
      } else {
	const Quantum<DComplex>& qVal = qh.asQuantumDComplex();
	const DComplex& val = qVal.getValue();
	if (!nearAbs(val.imag(), 0.0, NumericTraits<T>::minimum)) {
	  errorMessage += "I value cannot be complex\n";
	  return False;
	}
	setValue(T(val.real()));
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
	Vector<typename NumericTraits<T>::ConjugateType> val(4);
	convertArray(val, qVal.getValue());
	setValue(val);
      } else if (qh.isQuantumVectorDComplex()) {
	const Quantum<Vector<DComplex> >& qVal = qh.asQuantumVectorDComplex();
	setUnit(qVal.getFullUnit());
	Vector<typename NumericTraits<T>::ConjugateType> val(4);
	convertArray(val, qVal.getValue());
	setValue(val);
      } else if (qh.isQuantumVectorComplex()) {
	const Quantum<Vector<Complex> >& qVal = qh.asQuantumVectorComplex();
	setUnit(qVal.getFullUnit());
	Vector<typename NumericTraits<T>::ConjugateType> val(4);
	convertArray(val, qVal.getValue());
	setValue(val);
      } else if (qh.isQuantumVectorFloat()) {
	const Quantum<Vector<Float> >& qVal = qh.asQuantumVectorFloat();
	setUnit(qVal.getFullUnit());
	Vector<typename NumericTraits<T>::ConjugateType> val(4);
	convertArray(val, qVal.getValue());
	setValue(val);
      } else if (qh.isQuantumVectorInt()) {
	const Quantum<Vector<Int> >& qVal = qh.asQuantumVectorInt();
	setUnit(qVal.getFullUnit());
	Vector<typename NumericTraits<T>::ConjugateType> val(4);
	convertArray(val, qVal.getValue());
	setValue(val);
      } else {
	errorMessage += "value field must be a real or complex vector\n";
	return False;
      }
    }
  }
  {
    const String errorString("error");
    if (record.isDefined(errorString)) {
      const RecordFieldId error(errorString);
      Vector<DComplex> err(4, DComplex(0.0, 0.0));
      if  (record.shape(error) != IPosition(1,4)) {
 	errorMessage += 
 	  "The 'error' field must be a vector with 4 elements\n";
 	return False;
      }
      if (record.dataType(error) == TpArrayDouble && 
	  newPol == ComponentType::STOKES) {
	convertArray(err, record.asArrayDouble(error));
      } else if (record.dataType(error) == TpArrayDComplex) {
	err = record.asArrayDComplex(error);
      } else {
 	errorMessage += 
 	  "The 'error' field must be a complex vector with 4 elements\n";
 	return False;
      }
      setErrors(err(0), err(1), err(2), err(3));
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
toRecord(String& errorMessage, RecordInterface& record) const {
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
    Quantum<Vector<typename NumericTraits<T>::ConjugateType> > qVal;
    value(qVal);
    qh = QuantumHolder(qVal);
  }
  if (!qh.toRecord(errorMessage, record)) {
    errorMessage += "Problem generating the flux record\n";
    return False;
  }
  record.define(RecordFieldId("polarisation"), ComponentType::name(pol()));
  if (pol() != ComponentType::STOKES) {
    record.define(RecordFieldId("error"), errors());
  } else {
    Vector<T> realErr(4);
    real(realErr, errors());
    record.define(RecordFieldId("error"), realErr);
  }
  return True;
}


template<class T> Bool FluxRep<T>::ok() const {
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
	if (itsUnit == Unit("Jy")) {
		return True;
	}
	Bool found = False;
	for (Vector<String>::const_iterator iter=_allowedUnits.begin(); iter!=_allowedUnits.end(); iter++) {
		if (itsUnit == Unit(*iter)) {
			found = True;
			break;
		}
	}
	if (! found) {
		LogIO logErr(LogOrigin("FluxRep", "ok()"));
		logErr << LogIO::SEVERE << "The flux units have dimensions that are "
				<< "different from 'Jy'"
				<< LogIO::POST;
		return False;
	}
	return True;
}

template<class T> void FluxRep<T>::setAllowedUnits(const Vector<String>& allowedUnits) {
	_allowedUnits = allowedUnits;
}

template<class T> void FluxRep<T>::clearAllowedUnits() {
	_allowedUnits.resize(0);
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
Flux(typename NumericTraits<T>::ConjugateType xx,
     typename NumericTraits<T>::ConjugateType xy,
     typename NumericTraits<T>::ConjugateType yx,
     typename NumericTraits<T>::ConjugateType yy, 
     ComponentType::Polarisation pol)  
  :itsFluxPtr(new FluxRep<T>(xx, xy, yx, yy, pol))
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(const Vector<T>& flux)
  :itsFluxPtr(new FluxRep<T>(flux))
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(const Vector<typename NumericTraits<T>::ConjugateType>& flux,
     ComponentType::Polarisation pol)
  :itsFluxPtr(new FluxRep<T>(flux, pol))
{
  DebugAssert(ok(), AipsError);
}
  
template<class T> Flux<T>::
Flux(const Quantum<Vector<T> >& flux)
  :itsFluxPtr(new FluxRep<T>(flux))
{
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
Flux(const Flux<T>& other) 
  :itsFluxPtr(other.itsFluxPtr)
{ 
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>::
~Flux() {
  DebugAssert(ok(), AipsError);
}

template<class T> Flux<T>& Flux<T>::
operator=(const Flux<T>& other) {
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

template<class T> const Unit& Flux<T>::
unit() const {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->unit();
}

template<class T> void Flux<T>::
unit(Unit& unit) const {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->unit(unit);
}

template<class T> void Flux<T>::
setUnit(const Unit& unit) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setUnit(unit);
}

template<class T> void Flux<T>::
convertUnit(const Unit& unit) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->convertUnit(unit);
}

template<class T> ComponentType::Polarisation Flux<T>::
pol() const {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->pol();
}

template<class T> void Flux<T>::
pol(ComponentType::Polarisation& pol) const {
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

template<class T> const
Vector<typename NumericTraits<T>::ConjugateType>& Flux<T>::value() const {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->value();
}

template<class T> const typename NumericTraits<T>::ConjugateType& Flux<T>::
value(uInt p) const {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->value(p);
}

template<class T> void Flux<T>::
value(Vector<T>& value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
value(Vector<typename NumericTraits<T>::ConjugateType>& value) const {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
value(Quantum<Vector<T> >& value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
value(Quantum<Vector<typename NumericTraits<T>::ConjugateType> >&
      value) const {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> Quantum<T>  Flux<T>::
value (Stokes::StokesTypes stokes, Bool toJy) 
{
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->value(stokes, toJy);
}



template<class T> void Flux<T>::
setValue(T value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const Vector<T>& value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const Vector<typename NumericTraits<T>::ConjugateType>& value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const Quantum<Vector<T> >& value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const
	 Quantum<Vector<typename NumericTraits<T>::ConjugateType> >& value,
	 ComponentType::Polarisation pol) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value, pol);
}

template<class T> void Flux<T>::
setValue(const Quantum<T>& value, Stokes::StokesTypes stokes)
{
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value, stokes);
}

template<class T> void Flux<T>::
scaleValue(const T& factor) {
  itsFluxPtr->scaleValue(factor);
  DebugAssert(ok(), AipsError);
}

template<class T> void Flux<T>::
scaleValue(const T& factor0, const T& factor1, 
	   const T& factor2, const T& factor3) {
  itsFluxPtr->scaleValue(factor0, factor1, factor2, factor3);
  DebugAssert(ok(), AipsError);
}

template<class T> void Flux<T>::
scaleValue(const typename NumericTraits<T>::ConjugateType& factor) {
  itsFluxPtr->scaleValue(factor);
  DebugAssert(ok(), AipsError);
}

template<class T> void Flux<T>::
scaleValue(const typename NumericTraits<T>::ConjugateType& factor0,
	   const typename NumericTraits<T>::ConjugateType& factor1,
	   const typename NumericTraits<T>::ConjugateType& factor2,
	   const typename NumericTraits<T>::ConjugateType& factor3) {
  itsFluxPtr->scaleValue(factor0, factor1, factor2, factor3);
  DebugAssert(ok(), AipsError);
}

template<class T> void Flux<T>::
setErrors(const typename NumericTraits<T>::ConjugateType& error0,
	  const typename NumericTraits<T>::ConjugateType& error1,
	  const typename NumericTraits<T>::ConjugateType& error2,
	  const typename NumericTraits<T>::ConjugateType& error3) {
  itsFluxPtr->setErrors(error0, error1, error2, error3);
  DebugAssert(ok(), AipsError);
}

template<class T> void Flux<T>::setErrors(
	const Vector<typename NumericTraits<T>::ConjugateType>& errors
) {
	itsFluxPtr->setErrors(errors);
	DebugAssert(ok(), AipsError);
}


template<class T> const
Vector<typename NumericTraits<T>::ConjugateType>& Flux<T>::errors() const {
  return itsFluxPtr->errors();
}

template<class T> Bool Flux<T>::
fromRecord(String& errorMessage, const RecordInterface& record) {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->fromRecord(errorMessage, record);
}

template<class T> Bool Flux<T>::
toRecord(String& errorMessage, RecordInterface& record) const {
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

template<class T> void Flux<T>::
stokesToCircular(Vector<typename NumericTraits<T>::ConjugateType>& out, 
		 const Vector<T>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T i = in(0);
  const T q = in(1);
  const T u = in(2);
  const T v = in(3);
  out(0) = typename NumericTraits<T>::ConjugateType(i + v);
  out(1) = typename NumericTraits<T>::ConjugateType(q, u);
  out(2) = typename NumericTraits<T>::ConjugateType(q, -u);
  out(3) = typename NumericTraits<T>::ConjugateType(i - v);
}

template<class T> void Flux<T>::
stokesToCircular(Vector<typename NumericTraits<T>::ConjugateType>& out, 
		 const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const typename NumericTraits<T>::ConjugateType i = in(0);
  const typename NumericTraits<T>::ConjugateType q = in(1);
  const typename NumericTraits<T>::ConjugateType& u = in(2);
  const typename NumericTraits<T>::ConjugateType ju(-u.imag(), u.real());
  const typename NumericTraits<T>::ConjugateType v = in(3);
  out(0) = i + v;
  out(1) = q + ju;
  out(2) = q - ju;
  out(3) = i - v;
}

template<class T> void Flux<T>::
circularToStokes(Vector<T>& out,
		 const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T rr = in(0).real();
  const typename NumericTraits<T>::ConjugateType rl = in(1);
  const typename NumericTraits<T>::ConjugateType lr = in(2);
  const T ll = in(3).real();
  const T two(2);
  out(0) = (rr + ll)/two;
  out(1) = (rl.real() + lr.real())/two;
  out(2) = (rl.imag() - lr.imag())/two;
  out(3) = (rr - ll)/two;
}

template<class T> void Flux<T>::
circularToStokes(Vector<typename NumericTraits<T>::ConjugateType>& out,
		 const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const typename NumericTraits<T>::ConjugateType rr = in(0);
  const typename NumericTraits<T>::ConjugateType rl = in(1);
  const typename NumericTraits<T>::ConjugateType lr = in(2);
  const typename NumericTraits<T>::ConjugateType ll = in(3);
  const T two(2);
  out(0) = (rr + ll)/two;
  out(1) = (rl + lr)/two;
  out(2) = typename NumericTraits<T>::ConjugateType((rl.imag()-lr.imag())/two,
					   (lr.real()-rl.real())/two);
  out(3) = (rr - ll)/two;
}

template<class T> void Flux<T>::
stokesToLinear(Vector<typename NumericTraits<T>::ConjugateType>& out, 
	       const Vector<T>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T i = in(0);
  const T q = in(1);
  const T u = in(2);
  const T v = in(3);
  out(0) = typename NumericTraits<T>::ConjugateType(i + q);
  out(1) = typename NumericTraits<T>::ConjugateType(u, v);
  out(2) = typename NumericTraits<T>::ConjugateType(u, -v);
  out(3) = typename NumericTraits<T>::ConjugateType(i - q);
}

template<class T> void Flux<T>::
stokesToLinear(Vector<typename NumericTraits<T>::ConjugateType>& out, 
	       const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const typename NumericTraits<T>::ConjugateType i = in(0);
  const typename NumericTraits<T>::ConjugateType q = in(1);
  const typename NumericTraits<T>::ConjugateType u = in(2);
  const typename NumericTraits<T>::ConjugateType& v = in(3);
  const typename NumericTraits<T>::ConjugateType jv(-v.imag(), v.real());
  out(0) = i + q;
  out(1) = u + jv;
  out(2) = u - jv;
  out(3) = i - q;
}

template<class T> void Flux<T>::
linearToStokes(Vector<T>& out, 
	       const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T xx = in(0).real();
  const typename NumericTraits<T>::ConjugateType xy = in(1);
  const typename NumericTraits<T>::ConjugateType yx = in(2);
  const T yy = in(3).real();
  const T two(2);
  out(0) = (xx + yy)/two;
  out(1) = (xx - yy)/two;
  out(2) = (xy.real() + xy.real())/two;
  out(3) = (xy.imag() - yx.imag())/two;
}

template<class T> void Flux<T>::
linearToStokes(Vector<typename NumericTraits<T>::ConjugateType>& out, 
	       const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const typename NumericTraits<T>::ConjugateType xx = in(0);
  const typename NumericTraits<T>::ConjugateType xy = in(1);
  const typename NumericTraits<T>::ConjugateType yx = in(2);
  const typename NumericTraits<T>::ConjugateType yy = in(3);
  const T two(2);
  out(0) = (xx + yy)/two;
  out(1) = (xx - yy)/two;
  out(2) = (xy + yx)/two;
  out(3) = typename NumericTraits<T>::ConjugateType((xy.imag()-yx.imag())/two,
					   (yx.real()-xy.real())/two);
}

template<class T> void Flux<T>::
linearToCircular(Vector<typename NumericTraits<T>::ConjugateType>& out, 
		 const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const typename NumericTraits<T>::ConjugateType xx = in(0);
  const typename NumericTraits<T>::ConjugateType& xy = in(1);
  const typename NumericTraits<T>::ConjugateType jxy(-xy.imag(), xy.real());
  const typename NumericTraits<T>::ConjugateType& yx = in(2);
  const typename NumericTraits<T>::ConjugateType jyx(-yx.imag(), yx.real());
  const typename NumericTraits<T>::ConjugateType yy = in(3);
  const T two(2);
  out(0) = (xx - jxy + jyx + yy)/two;
  out(1) = (xx + jxy + jyx - yy)/two;
  out(2) = (xx - jxy - jyx - yy)/two;
  out(3) = (xx + jxy - jyx + yy)/two;
}

template<class T> void Flux<T>::
circularToLinear(Vector<typename NumericTraits<T>::ConjugateType>& out, 
		 const Vector<typename NumericTraits<T>::ConjugateType>& in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const typename NumericTraits<T>::ConjugateType rr = in(0);
  const typename NumericTraits<T>::ConjugateType rl = in(1);
  const typename NumericTraits<T>::ConjugateType lr = in(2);
  const typename NumericTraits<T>::ConjugateType ll = in(3);
  const T two(2);
  out(0) = (rr + rl + lr + ll)/two;
  out(1) = typename NumericTraits<T>::ConjugateType(
		     (-rr.imag() + rl.imag() - lr.imag() + ll.imag())/two,
                     ( rr.real() - rl.real() + lr.real() - ll.real())/two);
  out(2) = typename NumericTraits<T>::ConjugateType(
		     ( rr.imag() + rl.imag() - lr.imag() - ll.imag())/two,
                     (-rr.real() - rl.real() + lr.real() + ll.real())/two);
  out(3) = (rr - rl - lr + ll)/two;
}
// Local Variables: 
// compile-command: "gmake Flux"
// End: 

} //# NAMESPACE CASA - END

