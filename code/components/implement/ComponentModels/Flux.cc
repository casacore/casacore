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
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Measures/Quantum.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>

template<class T> FluxRep<T>::
FluxRep()
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  itsFlux(0).re = T(1);
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(T i) 
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  itsFlux(0).re = i;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(T i, T q, T u, T v)
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  itsFlux(0).re = i;
  itsFlux(1).re = q;
  itsFlux(2).re = u;
  itsFlux(3).re = v;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(NumericTraits<T>::ConjugateType xx, NumericTraits<T>::ConjugateType xy,
	NumericTraits<T>::ConjugateType yx, NumericTraits<T>::ConjugateType yy,
	ComponentType::Polarisation pol)
  :itsFlux(4),
   itsPol(pol),
   itsUnit("Jy") 
{
  itsFlux(0) = xx;
  itsFlux(1) = xy;
  itsFlux(2) = yx;
  itsFlux(3) = yy;
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Vector<T> & flux)
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit("Jy")
{
  DebugAssert(flux.nelements() == 4, AipsError);
  for (uInt i = 0 ; i < 4; i++) {
    itsFlux(i).re = flux(i);
  }
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Vector<NumericTraits<T>::ConjugateType> & flux,
	const ComponentType::Polarisation & pol)
  :itsFlux(flux.copy()),
   itsPol(pol),
   itsUnit("Jy")
{
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Quantum<Vector<T> > & flux)
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsPol(ComponentType::STOKES),
   itsUnit(flux.getFullUnit())
{
  const Vector<T> & fluxVal(flux.getValue());
  DebugAssert(fluxVal.nelements() == 4, AipsError);
  for (uInt s = 0; s < 4; s++) {
    itsFlux(s).re = fluxVal(s);
  }
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
	const ComponentType::Polarisation & pol)
  :itsFlux(flux.getValue().copy()),
   itsPol(pol),
   itsUnit(flux.getFullUnit())
{
  DebugAssert(ok(), AipsError);
}

template<class T> FluxRep<T>::
FluxRep(const FluxRep<T> & other) 
  :itsFlux(other.itsFlux.copy()),
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
    itsFlux = other.itsFlux;
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
    T factor = unit.getValue().getFac()/itsUnit.getValue().getFac();
    for (uInt i = 0; i < 4; i++) {
      itsFlux(i).re *= factor;
      itsFlux(i).im *= factor;
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
 	Flux<T>::linearToStokes(itsFlux, itsFlux);
      } else {
 	Flux<T>::circularToStokes(itsFlux, itsFlux);
      }
      break;
    case ComponentType::LINEAR:
      if (itsPol == ComponentType::STOKES) {
 	Flux<T>::stokesToLinear(itsFlux, itsFlux);
      } else {
 	Flux<T>::circularToLinear(itsFlux, itsFlux);
      }
      break;
    case ComponentType::CIRCULAR:
      if (itsPol == ComponentType::STOKES) {
 	Flux<T>::stokesToCircular(itsFlux, itsFlux);
      } else {
 	Flux<T>::linearToCircular(itsFlux, itsFlux);
      }
      break;
    };    
  }
  DebugAssert(ok(), AipsError);
}

template<class T> T FluxRep<T>::
value() {
  convertPol(ComponentType::STOKES);
  DebugAssert(ok(), AipsError);
  return itsFlux(0).re;
}

template<class T> void FluxRep<T>::
value(Vector<T> & value) {
  const uInt len = value.nelements();
  DebugAssert (len == 4 || len == 0, AipsError);
  if (len == 0) value.resize(4);
  convertPol(ComponentType::STOKES);
  for (uInt s = 0 ; s < 4; s++) {
    value(s) = itsFlux(s).re;
  }
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
value(Vector<NumericTraits<T>::ConjugateType> & value) const {
  DebugAssert(ok(), AipsError);
  uInt len = value.nelements();
  DebugAssert (len == 4 || len == 0, AipsError);
  value = itsFlux;
}

template<class T> void FluxRep<T>::
value(Quantum<Vector<T> > & value) {
  uInt len = value.getValue().nelements();
  DebugAssert(len == 4 || len == 0, AipsError);
  convertUnit(value.getFullUnit());
  convertPol(ComponentType::STOKES);
  Vector<T> & newValue = value.getValue();
  for (uInt s = 0 ; s < 4; s++) {
    newValue(s) = itsFlux(s).re;
  }
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
     const ComponentType::Polarisation & pol) {
  uInt len = value.getValue().nelements();
  DebugAssert(len == 4 || len == 0, AipsError);
  convertUnit(value.getFullUnit());
  convertPol(pol);
  value.setValue(itsFlux);
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(T value) {
  for (uInt i = 0; i < 4; i++) {
    itsFlux(i).im = itsFlux(i).re = T(0.0);
  }
  itsFlux(0).re = value;
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Vector<T> & value) {
  DebugAssert (value.nelements() == 4, AipsError);
  for (uInt i = 0; i < 4; i++) {
    itsFlux(i).re = value(i);
    itsFlux(i).im = T(0.0);
  }
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Vector<NumericTraits<T>::ConjugateType> & value) {
  DebugAssert (value.nelements() == 4, AipsError);
  itsFlux = value;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Quantum<Vector<T> > & value) {
  DebugAssert (value.getValue().nelements() == 4, AipsError);
  const Vector<T> & val = value.getValue();
  for (uInt s = 0; s < 4; s++) {
    itsFlux(s).re = val(s);
    itsFlux(s).im = T(0);
  }
  itsUnit = value.getFullUnit();
  itsPol = ComponentType::STOKES;
  DebugAssert(ok(), AipsError);
}

template<class T> void FluxRep<T>::
setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
	const ComponentType::Polarisation & pol) {
  DebugAssert (value.getValue().nelements() == 4, AipsError);
  itsFlux = value.getValue();
  itsUnit = value.getFullUnit();
  itsPol = pol;
  DebugAssert(ok(), AipsError);
}

template<class T> Bool FluxRep<T>::
ok() const {
  // The LogIO class is only constructed if an Error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (itsFlux.nelements() != 4) {
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
Flux(NumericTraits<T>::ConjugateType xx, NumericTraits<T>::ConjugateType xy,
     NumericTraits<T>::ConjugateType yx, NumericTraits<T>::ConjugateType yy, 
     ComponentType::Polarisation pol)  
  :itsFluxPtr(new FluxRep<T>(xx, xy, yx, yy, pol))
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
Flux(const Vector<NumericTraits<T>::ConjugateType> & flux,
     const ComponentType::Polarisation & pol)
  :itsFluxPtr(new FluxRep<T>(flux, pol))
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
Flux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
     const ComponentType::Polarisation & pol)
  :itsFluxPtr(new FluxRep<T>(flux, pol))
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
  Flux<T> newFlux;
  {
    Vector<NumericTraits<T>::ConjugateType> thisVal(4);
    value(thisVal);
    newFlux.setValue(thisVal, pol());
  }
  {
    Unit thisUnit(unit());
    newFlux.setUnit(thisUnit);
  }
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

template<class T> T Flux<T>::
value() {
  DebugAssert(ok(), AipsError);
  return itsFluxPtr->value();
}

template<class T> void Flux<T>::
value(Vector<T> & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
value(Vector<NumericTraits<T>::ConjugateType> & value) const {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
value(Quantum<Vector<T> > & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value);
}

template<class T> void Flux<T>::
value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
     const ComponentType::Polarisation & pol) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->value(value, pol);
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
setValue(const Vector<NumericTraits<T>::ConjugateType> & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const Quantum<Vector<T> > & value) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value);
}

template<class T> void Flux<T>::
setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
	const ComponentType::Polarisation & pol) {
  DebugAssert(ok(), AipsError);
  itsFluxPtr->setValue(value, pol);
}

template<class T> void Flux<T>::
stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
		 const Vector<T> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T i = in(0);
  const T q = in(1);
  const T u = in(2);
  const T v = in(3);
  out(0).re = i + v; out(0).im = T(0);
  out(1).re = q;     out(1).im = u;
  out(2).re = q;     out(2).im = -u;
  out(3).re = i - v; out(3).im = T(0);
}

template<class T> void Flux<T>::
stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
		 const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const NumericTraits<T>::ConjugateType i = in(0);
  const NumericTraits<T>::ConjugateType q = in(1);
  const NumericTraits<T>::ConjugateType & u = in(2);
  const NumericTraits<T>::ConjugateType ju(-u.im, u.re);
  const NumericTraits<T>::ConjugateType v = in(3);
  out(0) = i + v;
  out(1) = q + ju;
  out(2) = q - ju;
  out(3) = i - v;
}

template<class T> void Flux<T>::
circularToStokes(Vector<T> & out,
		 const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T rr = in(0).re;
  const NumericTraits<T>::ConjugateType rl = in(1);
  const NumericTraits<T>::ConjugateType lr = in(2);
  const T ll = in(3).re;
  out(0) = (rr + ll)/T(2);
  out(1) = (rl.re + lr.re)/T(2);
  out(2) = (rl.im - lr.im)/T(2);
  out(3) = (rr - ll)/T(2);
}

template<class T> void Flux<T>::
circularToStokes(Vector<NumericTraits<T>::ConjugateType> & out,
		 const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const NumericTraits<T>::ConjugateType rr = in(0);
  const NumericTraits<T>::ConjugateType rl = in(1);
  const NumericTraits<T>::ConjugateType lr = in(2);
  const NumericTraits<T>::ConjugateType ll = in(3);
  out(0) = (rr + ll)/T(2);
  out(1) = (rl + lr)/T(2);
  NumericTraits<T>::ConjugateType & u = out(2);
  u.re = (rl.im-lr.im)/T(2);
  u.im = (lr.re-rl.re)/T(2);
  out(3) = (rr - ll)/T(2);
}

template<class T> void Flux<T>::
stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
	       const Vector<T> & in){
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T i = in(0);
  const T q = in(1);
  const T u = in(2);
  const T v = in(3);
  out(0).re = i + q; out(0).im = T(0);
  out(1).re = u;     out(1).im = v;
  out(2).re = u;     out(2).im = -v;
  out(3).re = i - q; out(3).im = T(0);
}

template<class T> void Flux<T>::
stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
	       const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const NumericTraits<T>::ConjugateType i = in(0);
  const NumericTraits<T>::ConjugateType q = in(1);
  const NumericTraits<T>::ConjugateType u = in(2);
  const NumericTraits<T>::ConjugateType & v = in(3);
  const NumericTraits<T>::ConjugateType jv(-v.im, v.re);
  out(0) = i + q;
  out(1) = u + jv;
  out(2) = u - jv;
  out(3) = i - q;
}

template<class T> void Flux<T>::
linearToStokes(Vector<T> & out, 
	       const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const T xx = in(0).re;
  const NumericTraits<T>::ConjugateType xy = in(1);
  const NumericTraits<T>::ConjugateType yx = in(2);
  const T yy = in(3).re;
  out(0) = (xx + yy)/T(2);
  out(1) = (xx - yy)/T(2);
  out(2) = (xy.re + xy.re)/T(2);
  out(3) = (xy.im - yx.im)/T(2);
}

template<class T> void Flux<T>::
linearToStokes(Vector<NumericTraits<T>::ConjugateType> & out, 
	       const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const NumericTraits<T>::ConjugateType xx = in(0);
  const NumericTraits<T>::ConjugateType xy = in(1);
  const NumericTraits<T>::ConjugateType yx = in(2);
  const NumericTraits<T>::ConjugateType yy = in(3);
  out(0) = (xx + yy)/T(2);
  out(1) = (xx - yy)/T(2);
  out(2) = (xy + yx)/T(2);
  NumericTraits<T>::ConjugateType & v = out(3);
  v.re = (-xy.re-yx.im)/T(2);
  v.im = (yx.re-xy.im)/T(2);
}

template<class T> void Flux<T>::
linearToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
		 const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const NumericTraits<T>::ConjugateType xx = in(0);
  const NumericTraits<T>::ConjugateType & xy = in(1);
  const NumericTraits<T>::ConjugateType jxy(-xy.im, xy.re);
  const NumericTraits<T>::ConjugateType & yx = in(2);
  const NumericTraits<T>::ConjugateType jyx(-yx.im, yx.re);
  const NumericTraits<T>::ConjugateType yy = in(3);
  out(0) = (xx - jxy + jyx + yy)/T(2);
  out(1) = (xx + jxy + jyx - yy)/T(2);
  out(2) = (xx - jxy - jyx - yy)/T(2);
  out(3) = (xx + jxy - jyx + yy)/T(2);
}

template<class T> void Flux<T>::
circularToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
		 const Vector<NumericTraits<T>::ConjugateType> & in) {
  DebugAssert(in.nelements() == 4, AipsError);
  DebugAssert(out.nelements() == 4, AipsError);
  const NumericTraits<T>::ConjugateType rr = in(0);
  const NumericTraits<T>::ConjugateType rl = in(1);
  const NumericTraits<T>::ConjugateType lr = in(2);
  const NumericTraits<T>::ConjugateType ll = in(3);
  out(0) = (rr + rl + lr + ll)/T(2);
  out(1).re = (-rr.im + rl.im - lr.im + ll.im)/T(2);
  out(1).im = ( rr.re - rl.re + lr.re - ll.re)/T(2);
  out(2).re = (-rr.im - rl.im + lr.im + ll.im)/T(2);
  out(2).im = (-rr.re - rl.re + lr.re + ll.re)/T(2);
  out(3) = (rr - rl - lr + ll)/T(2);
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
