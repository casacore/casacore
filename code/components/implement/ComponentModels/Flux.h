//# Flux.h:
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

#if !defined(AIPS_FLUX_H)
#define AIPS_FLUX_H

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/NumericTraits.h>
#include <aips/Measures/Unit.h>
#include <aips/Utilities/CountedPtr.h>
#include <trial/ComponentModels/ComponentType.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Measures/Quantum.h>
#include <aips/Utilities/Assert.h>

class GlishRecord;

// <summary>A class that represents the Flux (copy semantics)</summary>

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

template<class T> class FluxRep
{
public:

  // Assume I = 1, Q=U=V=0, Stokes representation, units are Jy.
  FluxRep();

  // Q=U=V=0, Stokes representation, units are Jy.
  FluxRep(T i);

  // Stokes representation, units are Jy.
  FluxRep(T i, T q, T u, T v);

  // units are Jy.
  FluxRep(NumericTraits<T>::ConjugateType xx,
	  NumericTraits<T>::ConjugateType xy,
	  NumericTraits<T>::ConjugateType yx,
	  NumericTraits<T>::ConjugateType yy, ComponentType::Polarisation pol)
    :itsVal(4),
     itsPol(pol),
     itsUnit("Jy") 
    {
      itsVal(0) = xx;
      itsVal(1) = xy;
      itsVal(2) = yx;
      itsVal(3) = yy;
      DebugAssert(ok(), AipsError);
    };
  
  // Stokes representation, units are Jy.
  FluxRep(const Vector<T> & flux);

  // units are Jy.
  FluxRep(const Vector<NumericTraits<T>::ConjugateType> & flux,
  		const ComponentType::Polarisation & pol)
    :itsVal(flux.copy()),
     itsPol(pol),
     itsUnit("Jy")
    {
      DebugAssert(ok(), AipsError);
    };
  
  // Stokes representation
  FluxRep(const Quantum<Vector<T> > & flux);

  // Fully Specified
  FluxRep(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
	  const ComponentType::Polarisation & pol)
    :itsVal(flux.getValue().copy()),
     itsPol(pol),
     itsUnit(flux.getFullUnit())
    {
      DebugAssert(ok(), AipsError);
    };

  // The copy constructor uses copy semantics.
  FluxRep(const FluxRep<T> & other);

  // The destructor is trivial
  ~FluxRep();

  // The assignment operator uses copy semantics.
  FluxRep<T> & operator=(const FluxRep<T> & other);

  // get the default units
  Unit unit() const;
  void unit(Unit & unit) const;
  // set the default units
  void setUnit(const Unit & unit);
  // set the default units and convert the internal flux
  void convertUnit(const Unit & unit);

  // get the default polarisation representation
  ComponentType::Polarisation pol() const;
  void pol(ComponentType::Polarisation & pol) const;
  // set the default polarisation representation
  void setPol(const ComponentType::Polarisation & pol);
  // set the default polarisation representation and convert the internal flux
  void convertPol(const ComponentType::Polarisation & pol);

  // get the flux value assuming ...
  // <group>
  // the current units and polarisation
  const Vector<NumericTraits<T>::ConjugateType> & value() const {
    DebugAssert(ok(), AipsError);
    return itsVal;
  };

  // the current units and polarisation only return the specified polarisation.
  const NumericTraits<T>::ConjugateType & value(uInt p) const {
    DebugAssert(p < 4, AipsError);
    DebugAssert(ok(), AipsError);
    return itsVal(p);
  };

  // Stokes representation & current unit
  void value(Vector<T> & value);
  // current unit and pol
  void value(Vector<NumericTraits<T>::ConjugateType> & value) const {
    DebugAssert(ok(), AipsError);
    uInt len = value.nelements();
    DebugAssert (len == 4 || len == 0, AipsError);
    value = itsVal;
  };
  // Stokes pol.
  void value(Quantum<Vector<T> > & value);
  // Don't assume anything
  void value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
  	    const ComponentType::Polarisation & pol) {
    uInt len = value.getValue().nelements();
    DebugAssert(len == 4 || len == 0, AipsError);
    convertUnit(value.getFullUnit());
    convertPol(pol);
    value.setValue(itsVal);
    DebugAssert(ok(), AipsError);
  };
  // </group>

  // Set the current flux assuming for the unspecified values ...
  // <group>
  // User specifies I only and Q=U=V=0 and the current unit
  void setValue(T value); 
  // a Stokes pol and the current unit
  void setValue(const Vector<T> & value); 
  // the current unit and pol
  void setValue(const Vector<NumericTraits<T>::ConjugateType> & value) {
    DebugAssert (value.nelements() == 4, AipsError);
    itsVal = value;
    DebugAssert(ok(), AipsError);
  };
  // a Stokes pol
  void setValue(const Quantum<Vector<T> > & value);
  // Nothing. Flux is fully specified.
  void setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> >& value,
 	       const ComponentType::Polarisation & pol) {
    DebugAssert (value.getValue().nelements() == 4, AipsError);
    itsVal = value.getValue();
    itsUnit = value.getFullUnit();
    itsPol = pol;
    DebugAssert(ok(), AipsError);
  };
  // </group>

  // Scale the Flux value by the specified amount
  // <group>
  void scaleValue(const T & factor);
  void scaleValue(const NumericTraits<T>::ConjugateType & factor) {
    itsVal.ac() *= factor;
    DebugAssert(ok(), AipsError);
  };
  void scaleValue(const Vector<NumericTraits<T>::ConjugateType> & factor) {
    DebugAssert (factor.nelements() == 4, AipsError);
    itsVal.ac() *= factor.ac();
    DebugAssert(ok(), AipsError);
  };
  // </group>

  // This functions convert between a glish record and a FluxRep object. These
  // functions define how the FluxRep is represented in glish.  They return
  // False if the glish record is malformed and append an error message to the
  // supplied string giving the reason.  
  // <group>
  Bool fromRecord(String & errorMessage, const GlishRecord & record);
  Bool toRecord(String & errorMessage, GlishRecord & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  Bool ok() const;

private:
  Vector<NumericTraits<T>::ConjugateType> itsVal;
  ComponentType::Polarisation itsPol;
  Unit itsUnit;
};

// <summary>A class that represents the Flux (reference semantics)</summary>

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

template<class T> class Flux
{
public:

  // Assume I = 1, Q=U=V=0, Stokes representation, units are Jy.
  Flux();

  // Q=U=V=0, Stokes representation, units are Jy.
  Flux(T i);

  // Stokes representation, units are Jy.
  Flux(T i, T q, T u, T v);

  // units are Jy.
  Flux(NumericTraits<T>::ConjugateType xx, NumericTraits<T>::ConjugateType xy,
       NumericTraits<T>::ConjugateType yx, NumericTraits<T>::ConjugateType yy, 
       ComponentType::Polarisation pol)  
    :itsFluxPtr(new FluxRep<T>(xx, xy, yx, yy, pol))
    {
      DebugAssert(ok(), AipsError);
    };
  // Stokes representation, units are Jy.
  Flux(const Vector<T> & flux);

  // units are Jy.
  Flux(const Vector<NumericTraits<T>::ConjugateType> & flux,
       const ComponentType::Polarisation & pol)
    :itsFluxPtr(new FluxRep<T>(flux, pol))
    {
      DebugAssert(ok(), AipsError);
    };
  
  // Stokes representation
  Flux(const Quantum<Vector<T> > & flux);

  // Fully Specified
  Flux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
       const ComponentType::Polarisation & pol);

  // The copy constructor uses copy semantics.
  Flux(const Flux<T> & other);

  // The destructor is trivial
  ~Flux();

  // The assignment operator uses copy semantics.
  Flux<T> & operator=(const Flux<T> & other);

  // Return a distinct copy of this flux. As both the assignment operator
  // and the copy constructor use reference semantics this is the only way to
  // get a real copy.
  Flux<T> copy() const;

  // get the default units
  Unit unit() const;
  void unit(Unit & unit) const;
  // set the default units
  void setUnit(const Unit & unit);
  // set the default units and convert the internal flux
  void convertUnit(const Unit & unit);

  // get the default polarisation representation
  ComponentType::Polarisation pol() const;
  void pol(ComponentType::Polarisation & pol) const;
  // set the default polarisation representation
  void setPol(const ComponentType::Polarisation & pol);
  // set the default polarisation representation and convert the internal flux
  void convertPol(const ComponentType::Polarisation & pol);

  // get the flux value assuming ...
  // <group>
  // user wants I flux only
  // the current units and polarisation
  const Vector<NumericTraits<T>::ConjugateType> & value() const {
    DebugAssert(ok(), AipsError);
    return itsFluxPtr->value();
  };

  // the current units and polarisation only return the specified polarisation.
  const NumericTraits<T>::ConjugateType & value(uInt p) const {
    DebugAssert(ok(), AipsError);
    return itsFluxPtr->value(p);
  };

  // Stokes representation & current unit
  void value(Vector<T> & value);
  // current unit and pol
  void value(Vector<NumericTraits<T>::ConjugateType> & value) const {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->value(value);
  };
  // Stokes pol.
  void value(Quantum<Vector<T> > & value);
  // Don't assume anything
  void value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
	     const ComponentType::Polarisation & pol) {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->value(value, pol);
  };
  // </group>

  // Set the current flux assuming for the unspecified values ...
  // <group>
  // User specifies I only and Q=U=V=0 and the current unit
  void setValue(T value); 
  // a Stokes pol and the current unit
  void setValue(const Vector<T> & value); 
  // the current unit and pol
  void setValue(const Vector<NumericTraits<T>::ConjugateType> & value) {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->setValue(value);
  };
  // a Stokes pol
  void setValue(const Quantum<Vector<T> > & value);
  // Nothing. Flux is fully specified.
  void setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> >& value,
		const ComponentType::Polarisation & pol) {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->setValue(value, pol);
  };
  // </group>

  // Functions for converting a 4 element complex vector between
  // different representations.
  // <group>
  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
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
  };

  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };

  static void circularToStokes(Vector<T> & out,
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };

  static void circularToStokes(Vector<NumericTraits<T>::ConjugateType> & out,
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };

  static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
  			     const Vector<T> & in) {
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
  };

  static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
  			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in) {
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
  };

  static void linearToStokes(Vector<T> & out, 
 			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in) {
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
  };

  static void linearToStokes(Vector<NumericTraits<T>::ConjugateType> & out, 
 			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in) {
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
  };

  static void linearToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };

  static void circularToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in) {
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
  };
  // </group>

  // Scale the Flux value by the specified amount
  // <group>
  void scaleValue(const T & factor);
  void scaleValue(const NumericTraits<T>::ConjugateType & factor) {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->scaleValue(factor);
  };
  void scaleValue(const Vector<NumericTraits<T>::ConjugateType> & factor) {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->scaleValue(factor);
  };
  // </group>

  // This functions convert between a glish record and a Flux object and define
  // how the Flux is represented in glish.  They return False if the glish
  // record is malformed and append an error message to the supplied string
  // giving the reason.
  // <group>
  Bool fromRecord(String & errorMessage, const GlishRecord & record);
  Bool toRecord(String & errorMessage, GlishRecord & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  Bool ok() const;

private:
  CountedPtr<FluxRep<T> > itsFluxPtr;
};

#endif
