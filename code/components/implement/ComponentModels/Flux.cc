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

#include <trial/ComponentModels/ComponentFlux.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>
#include <aips/Measures/Quantum.h>

template<class T> ComponentFlux<T>::
ComponentFlux()
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsRep(FluxEnums::STOKES),
   itsUnit("Jy")
{
  itsFlux(0).re = T(1);
}

template<class T> ComponentFlux<T>::
ComponentFlux(T i) 
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsRep(FluxEnums::STOKES),
   itsUnit("Jy")
{
  itsFlux(0).re = i;
}

template<class T> ComponentFlux<T>::
ComponentFlux(T i, T q, T u, T v)
  :itsFlux(4),
   itsRep(FluxEnums::STOKES),
   itsUnit("Jy")
{
  itsFlux(0) = i;
  itsFlux(1) = q;
  itsFlux(2) = u;
  itsFlux(3) = v;
}

template<class T> ComponentFlux<T>::
ComponentFlux(NumericTraits<T>::ConjugateType xx,
 	      NumericTraits<T>::ConjugateType xy,
 	      NumericTraits<T>::ConjugateType yx,
 	      NumericTraits<T>::ConjugateType yy, 
	      FluxEnums::PolType whichRep)
  :itsFlux(4),
   itsRep(whichRep),
   itsUnit("Jy") 
{
  itsFlux(0) = xx;
  itsFlux(1) = xy;
  itsFlux(2) = yx;
  itsFlux(3) = yy;
}

template<class T> ComponentFlux<T>::
ComponentFlux(const Vector<T> & flux)
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsRep(FluxEnums::STOKES),
   itsUnit("Jy")
{
  AlwaysAssert(flux.nelements() == 4, AipsError);
  for (uInt i = 0 ; i < 4; i++) {
    itsFlux(i).re = flux(i);
  }
}

template<class T> ComponentFlux<T>::
ComponentFlux(const Vector<T> & flux, const Unit & unit) 
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsRep(FluxEnums::STOKES),
   itsUnit(unit)
{
  AlwaysAssert(itsUnit == Unit("Jy"), AipsError);
  AlwaysAssert(flux.nelements() == 4, AipsError);
  for (uInt i = 0 ; i < 4; i++) {
    itsFlux(i).re = flux(i);
  }
}

template<class T> ComponentFlux<T>::
ComponentFlux(const Quantum<Vector<T> > & flux)
  :itsFlux(4, NumericTraits<T>::ConjugateType(0,0)),
   itsRep(FluxEnums::STOKES),
   itsUnit(flux.getFullUnit())
{
  AlwaysAssert(itsUnit == Unit("Jy"), AipsError);
  const Vector<T> & fluxVal(flux.getValue());
  AlwaysAssert(fluxVal.nelements() == 4, AipsError);
  for (uInt i = 0 ; i < 4; i++) {
    itsFlux(i).re = fluxVal(i);
  }
}

template<class T> ComponentFlux<T>::
ComponentFlux(const Vector<NumericTraits<T>::ConjugateType> & flux,
	      const FluxEnums::PolType & rep) 
  :itsFlux(flux.copy()),
   itsRep(rep),
   itsUnit("Jy")
{
  AlwaysAssert(itsFlux.nelements() == 4, AipsError);
  AlwaysAssert(itsUnit == Unit("Jy"), AipsError);
}

template<class T> ComponentFlux<T>::
ComponentFlux(const Vector<NumericTraits<T>::ConjugateType> & flux,
	      const Unit & unit, const FluxEnums::PolType & rep) 
  :itsFlux(flux.copy()),
   itsRep(rep),
   itsUnit(unit)
{
  AlwaysAssert(itsFlux.nelements() == 4, AipsError);
  AlwaysAssert(itsUnit == Unit("Jy"), AipsError);
}


template<class T> ComponentFlux<T>::
ComponentFlux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
	      const FluxEnums::PolType & rep) 
  :itsFlux(flux.getValue().copy()),
   itsRep(rep),
   itsUnit(flux.getFullUnit())
{
  AlwaysAssert(itsFlux.nelements() == 4, AipsError);
  AlwaysAssert(itsUnit == Unit("Jy"), AipsError);
}

template<class T> ComponentFlux<T>::
ComponentFlux(const ComponentFlux<T> & other) 
  :itsFlux(other.itsFlux.copy()),
   itsRep(other.itsRep),
   itsUnit(other.itsUnit)
{
}

template<class T> ComponentFlux<T>::
~ComponentFlux() {
}

template<class T> ComponentFlux<T> & ComponentFlux<T>::
operator=(const ComponentFlux<T> & other) {
  if (this != &other) {
    itsFlux = other.itsFlux;
    itsRep = other.itsRep;
    itsUnit = other.itsUnit;
  }
  return *this;
}

template<class T> Unit ComponentFlux<T>::
unit() const {
  return itsUnit;
}

template<class T> void ComponentFlux<T>::
unit(Unit & unit) const {
  unit = itsUnit;
}

template<class T> void ComponentFlux<T>::
setUnit(const Unit & unit) {
  AlwaysAssert(unit == Unit("Jy"), AipsError);
  itsUnit = unit;
}

template<class T> void ComponentFlux<T>::
convertUnit(const Unit & unit) {
  AlwaysAssert(unit == Unit("Jy"), AipsError);
  T factor = unit.getValue().getFac()/itsUnit.getValue().getFac();
  for (uInt i = 0; i < 4; i++) {
    itsFlux(i).re *= factor;
    itsFlux(i).im *= factor;
  }
  itsUnit = unit;
}

template<class T> FluxEnums::PolType ComponentFlux<T>::
rep() const {
  return itsRep;
}

template<class T> void ComponentFlux<T>::
rep(FluxEnums::PolType & rep) const {
  rep = itsRep;
}

template<class T> void ComponentFlux<T>::
setRep(const FluxEnums::PolType & rep) {
  itsRep = rep;
}

template<class T> void ComponentFlux<T>::
convertRep(const FluxEnums::PolType & rep) {
  if (itsRep != rep) {
    switch (rep){
    case FluxEnums::STOKES:
      if (itsRep == FluxEnums::LINEAR) {
// 	linearToStokes(itsFlux, itsFlux);
      } else {
 	circularToStokes(itsFlux, itsFlux);
      }
      break;
    case FluxEnums::LINEAR:
      if (itsRep == FluxEnums::STOKES) {
// 	stokesToLinear(itsFlux, itsFlux);
      } else {
// 	circularToLinear(itsFlux, itsFlux);
      }
      break;
    case FluxEnums::CIRCULAR:
      if (itsRep == FluxEnums::STOKES) {
 	stokesToCircular(itsFlux, itsFlux);
      } else {
// 	linearToCircular(itsFlux, itsFlux);
      }
      break;
    };    
  }
}

template<class T> T ComponentFlux<T>::
flux() {
  convertRep(FluxEnums::STOKES);
  return itsFlux(0).re;
}

template<class T> void ComponentFlux<T>::
flux(Vector<T> & value) {
  const uInt len = value.nelements();
  AlwaysAssert (len == 4 || len == 0, AipsError);
  if (len == 0) value.resize(4);
  convertRep(FluxEnums::STOKES);
  for (uInt i = 0 ; i < 4; i++) {
    value(i) = itsFlux(i).re;
  }
}

template<class T> void ComponentFlux<T>::
setFlux(T value) {
  for (uInt i = 0; i < 4; i++) {
    itsFlux(i).im = itsFlux(i).re = T(0.0);
  }
  itsFlux(0).re = value;
  itsRep = FluxEnums::STOKES;
}

template<class T> void ComponentFlux<T>::
setFlux(const Vector<T> & value) {
  AlwaysAssert (value.nelements() == 4, AipsError);
  for (uInt i = 0; i < 4; i++) {
    itsFlux(i).re = value(i);
    itsFlux(i).im = T(0.0);
  }
  itsRep = FluxEnums::STOKES;
}

template<class T> void ComponentFlux<T>::
stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
		 const Vector<NumericTraits<T>::ConjugateType> & in) {
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

template<class T> void ComponentFlux<T>::
circularToStokes(Vector<NumericTraits<T>::ConjugateType> & out,
		 const Vector<NumericTraits<T>::ConjugateType> & in) {
  const NumericTraits<T>::ConjugateType rr = in(0);
  const NumericTraits<T>::ConjugateType rl = in(1);
  const NumericTraits<T>::ConjugateType lr = in(2);
  const NumericTraits<T>::ConjugateType ll = in(3);
  
  out(0) = (rr + ll)/2.0;
  out(1) = (rl + lr)/2.0;
  NumericTraits<T>::ConjugateType & u = out(2);
  u.re = (rl.im-lr.im)/2.0;
  u.im = (lr.re-rl.re)/2.0;
  out(3) = (rr - ll)/2.0;
}


// Local Variables: 
// compile-command: "gmake OPTLIB=1 ComponentFlux"
// End: 
