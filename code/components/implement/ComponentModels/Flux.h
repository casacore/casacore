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

//#! Create an include 'guard', containing your class name in the all
//#! upper case format implied below.  This prevents multiple inclusion
//#! of this header file during pre-processing.
//#!
//#! Note that the leading "AIPS_" identifies the package to which your class
//#! belongs.  Other packages include dish, vlbi, nfra, synthesis, atnf...
//#! If you are contributing a new class to one of these packages, be
//#! sure to replace "AIPS_" with (for instance) "DISH_" or "ATNF_".

#if !defined(AIPS_COMPONENTFLUX_H)
#define AIPS_COMPONENTFLUX_H

#include <aips/Measures/Unit.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/NumericTraits.h>

template <class Qtype> class Quantum;

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
 		FluxEnums::PolType whichRep=FluxEnums::LINEAR);

  // Stokes representation, units are Jy.
  ComponentFlux(const Vector<T> & flux);

  // Stokes representation
  // <group>
  ComponentFlux(const Vector<T> & flux, const Unit & unit);
  ComponentFlux(const Quantum<Vector<T> > & flux);
  // </group>

  // units are Jy.
  ComponentFlux(const Vector<NumericTraits<T>::ConjugateType> & flux,
  		const FluxEnums::PolType & rep);

  // Fully Specified
  ComponentFlux(const Vector<NumericTraits<T>::ConjugateType> & flux,
 		const Unit & unit, const FluxEnums::PolType & rep);
  ComponentFlux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
 		const FluxEnums::PolType & rep);

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
  // set the default units and convert the internal flux
  void convertUnit(const Unit & unit);

  // get the default polarisation representation
  FluxEnums::PolType rep() const;
  void rep(FluxEnums::PolType & rep) const;
  // set the default polarisation representation
  void setRep(const FluxEnums::PolType & rep);
  // set the default polarisation representation and convert the internal flux
  void convertRep(const FluxEnums::PolType & rep);

  // get the flux assuming ...
  // <group>
  // user wants I flux only
  T flux();
  // Stokes representation & current unit
  void flux(Vector<T> & value);
  // current representation & unit
//   void flux(Vector<NumericTraits<T>::ConjugateType> & value) const;
  // current unit
//   void flux(Vector<NumericTraits<T>::ConjugateType> & value, 
// 	    const FluxEnums::PolType & rep) const;
  // Stokes rep.
//   void flux(Quantum<Vector<T> > & value) const;
  // current rep.
//   void flux(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value) const;
  // Don't assume anything
//   void flux(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
// 	    const FluxEnums::PolType & rep) const;
  // </group>

  // Set the current flux assuming for the unspecified values ...
  // <group>
  // User specifies I only and Q=U=V=0
  void setFlux(T value); 
  // a Stokes rep and the current unit
  void setFlux(const Vector<T> & value); 
  // the current rep and unit
//   void setFlux(const Vector<NumericTraits<T>::ConjugateType> & value);
  // the current unit
//   void setFlux(const Vector<NumericTraits<T>::ConjugateType> & value, 
// 	       const FluxEnums::PolType & rep);
  // a Stokes rep
  void setFlux(const Quantum<Vector<T> > & value);
  // the current rep
//   void setFlux(const Quantum<Vector<NumericTraits<T>::ConjugateType> >& value);
  // Nothing. Flux is fully specified.
//   void setFlux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
// 	       const FluxEnums::PolType & rep);

  // Functions for converting a 4 element complex vector between
  // different representations.
  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in);
//  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
  // 			       const Vector<T> & in);
   static void circularToStokes(Vector<NumericTraits<T>::ConjugateType> & out,
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in);
//   static void circularToStokes(Vector<T> & out,
// 			       const Vector<NumericTraits<T>::ConjugateType> &
// 			       in);
//   static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
//  			     const Vector<NumericTraits<T>::ConjugateType> &
// 			     in);
//   static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
//  			     const Vector<T> & in);
//   static void linearToStokes(Vector<NumericTraits<T>::ConjugateType> & out, 
// 			     const Vector<NumericTraits<T>::ConjugateType> &
// 			     in)
//   static void linearToStokes(Vector<T> & out, 
// 			     const Vector<NumericTraits<T>::ConjugateType> &
// 			     in)
//   static void linearToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
// 			       const Vector<NumericTraits<T>::ConjugateType> &
// 			       in);
//   static void circularToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
// 			       const Vector<NumericTraits<T>::ConjugateType> &
// 			       in);
private:
  Vector<NumericTraits<T>::ConjugateType> itsFlux;
  FluxEnums::PolType itsRep;
  Unit itsUnit;
};

#endif
