//# <ClassFileName.h>: this defines <ClassName>, which ...
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
//#
//# $Id$

#if !defined(AIPS_COMPONENTFLUX_H)
#define AIPS_COMPONENTFLUX_H

#include <aips/Measures/Unit.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/NumericTraits.h>

#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Measures/Quantum.h>

// template <class Qtype> class Quantum;

// <summary>Contains a flux, its units and its polarisations</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// This class was needed to contain the flux in the ComponentModels class. It
// centralises a lot of code that would otherwise be duplicated. It may be
// replaced by a Flux Measure in the future.
// </motivation>
//
// <templating arg=T>
//    <li>
//    <li>
// </templating>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class FluxEnums {
public:
  enum PolType {
    STOKES,
    LINEAR,
    CIRCULAR
  };
};


template<class T> class ComponentFlux
{
public:

  // Assume I = 1, Q=U=V=0, Stokes representation, units are Jy.
  ComponentFlux();

  // Q=U=V=0, Stokes representation, units are Jy.
  ComponentFlux(T i);

  // Stokes representation, units are Jy.
  ComponentFlux(T i, T q, T u, T v);

  // units are Jy.
  ComponentFlux(NumericTraits<T>::ConjugateType xx, 
  		NumericTraits<T>::ConjugateType xy,
  		NumericTraits<T>::ConjugateType yx,
  		NumericTraits<T>::ConjugateType yy, 
 		FluxEnums::PolType rep)  
    :itsFlux(4),
     itsRep(rep),
     itsUnit("Jy") 
    {
      itsFlux(0) = xx;
      itsFlux(1) = xy;
      itsFlux(2) = yx;
      itsFlux(3) = yy;
    };

  // Stokes representation, units are Jy.
  ComponentFlux(const Vector<T> & flux);

  // Stokes representation
  // <group>
  ComponentFlux(const Quantum<Vector<T> > & flux);
  // </group>

  // units are Jy.
  ComponentFlux(const Vector<NumericTraits<T>::ConjugateType> & flux,
  		const FluxEnums::PolType & rep)
    :itsFlux(flux.copy()),
     itsRep(rep),
     itsUnit("Jy")
    {
      AlwaysAssert(itsFlux.nelements() == 4, AipsError);
    };
  
  // Fully Specified
  ComponentFlux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
 		const FluxEnums::PolType & rep)
    :itsFlux(flux.getValue().copy()),
     itsRep(rep),
     itsUnit(flux.getFullUnit())
    {
      AlwaysAssert(itsFlux.nelements() == 4, AipsError);
      AlwaysAssert(itsUnit == Unit("Jy"), AipsError);
    };

  // The copy constructor uses copy semantics.
  ComponentFlux(const ComponentFlux<T> & other);

  // The destructor is trivial
  ~ComponentFlux();

  // The assignment operator uses copy semantics.
  ComponentFlux<T> & operator=(const ComponentFlux<T> & other);

  // get the default units
  Unit unit() const;
  void unit(Unit & unit) const;
  // set the default units
  void setUnit(const Unit & unit);

  // get the default polarisation representation
  FluxEnums::PolType rep() const;
  void rep(FluxEnums::PolType & rep) const;
  // set the default polarisation representation
  void setRep(const FluxEnums::PolType & rep);

  // get the flux assuming ...
  // <group>
  // user wants I flux only
  T flux();
  // Stokes representation & current unit
  void flux(Vector<T> & value);
  // current unit
  void flux(Vector<NumericTraits<T>::ConjugateType> & value, 
 	    const FluxEnums::PolType & rep) {
    uInt len = value.nelements();
    AlwaysAssert (len == 4 || len == 0, AipsError);
    convertRep(rep);
    value = itsFlux;
  };
  // Stokes rep.
  void flux(Quantum<Vector<T> > & value);
  // Don't assume anything
  void flux(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
  	    const FluxEnums::PolType & rep) {
    uInt len = value.getValue().nelements();
    AlwaysAssert(len == 4 || len == 0, AipsError);
    convertUnit(value.getFullUnit());
    convertRep(rep);
    value.setValue(itsFlux);
  };
  // </group>

  // Set the current flux assuming for the unspecified values ...
  // <group>
  // User specifies I only and Q=U=V=0 and the current unit
  void setFlux(T value); 
  // a Stokes rep and the current unit
  void setFlux(const Vector<T> & value); 
  // the current unit
  void setFlux(const Vector<NumericTraits<T>::ConjugateType> & value, 
	       const FluxEnums::PolType & rep) {
    AlwaysAssert (value.nelements() == 4, AipsError);
    itsFlux = value;
    itsRep = rep;
  };
  // a Stokes rep
  void setFlux(const Quantum<Vector<T> > & value);
  // Nothing. Flux is fully specified.
  void setFlux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
 	       const FluxEnums::PolType & rep) {
    AlwaysAssert (value.getValue().nelements() == 4, AipsError);
    itsFlux = value.getValue();
    itsUnit = value.getFullUnit();
    itsRep = rep;
  };

  // Functions for converting a 4 element complex vector between
  // different representations.
  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
    			       const Vector<T> & in) {
    const T i = in(0);
    const T q = in(1);
    const T u = in(2);
    const T v = in(3);
    out(0).re = i + v; out(0).im = T(0);
    out(1).re = q;     out(1).im = u;
    out(2).re = q;     out(2).im = -u;
    out(3).re = i - v; out(3).im = T(0);
  };

  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
    const NumericTraits<T>::ConjugateType i = in(0);
    const NumericTraits<T>::ConjugateType q = in(1);
    const NumericTraits<T>::ConjugateType & u = in(2);
    const NumericTraits<T>::ConjugateType ju(-u.im, u.re);
    const NumericTraits<T>::ConjugateType v = in(3);
    out(0) = i + v;
    out(1) = q + ju;
    out(2) = q - ju;
    out(3) = i - v;
  };

  static void circularToStokes(Vector<T> & out,
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
    const T rr = in(0).re;
    const NumericTraits<T>::ConjugateType rl = in(1);
    const NumericTraits<T>::ConjugateType lr = in(2);
    const T ll = in(3).re;
    out(0) = (rr + ll)/T(2);
    out(1) = (rl.re + lr.re)/T(2);
    out(2) = (rl.im - lr.im)/T(2);
    out(3) = (rr - ll)/T(2);
  };

  static void circularToStokes(Vector<NumericTraits<T>::ConjugateType> & out,
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };

  static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
  			     const Vector<T> & in){
    const T i = in(0);
    const T q = in(1);
    const T u = in(2);
    const T v = in(3);
    out(0).re = i + q; out(0).im = T(0);
    out(1).re = u;     out(1).im = v;
    out(2).re = u;     out(2).im = -v;
    out(3).re = i - q; out(3).im = T(0);
  };

  static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
  			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in) {
    const NumericTraits<T>::ConjugateType i = in(0);
    const NumericTraits<T>::ConjugateType q = in(1);
    const NumericTraits<T>::ConjugateType u = in(2);
    const NumericTraits<T>::ConjugateType & v = in(3);
    const NumericTraits<T>::ConjugateType jv(-v.im, v.re);
    out(0) = i + q;
    out(1) = u + jv;
    out(2) = u - jv;
    out(3) = i - q;
  };

  static void linearToStokes(Vector<T> & out, 
 			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in) {
    const T xx = in(0).re;
    const NumericTraits<T>::ConjugateType xy = in(1);
    const NumericTraits<T>::ConjugateType yx = in(2);
    const T yy = in(3).re;
    out(0) = (xx + yy)/T(2);
    out(1) = (xx - yy)/T(2);
    out(2) = (xy.re + xy.re)/T(2);
    out(3) = (xy.im - yx.im)/T(2);
  };

  static void linearToStokes(Vector<NumericTraits<T>::ConjugateType> & out, 
 			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in) {
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
  };

  static void linearToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };

  static void circularToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };

  // set the default units and convert the internal flux
  void convertUnit(const Unit & unit);

private:
  // set the default polarisation representation and convert the internal flux
  void convertRep(const FluxEnums::PolType & rep);

  Vector<NumericTraits<T>::ConjugateType> itsFlux;
  FluxEnums::PolType itsRep;
  Unit itsUnit;
};

#endif
