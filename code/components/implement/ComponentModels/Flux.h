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
#include <aips/Utilities/CountedPtr.h>
#include <aips/Measures/Unit.h>
#include <aips/Arrays/Vector.h>
#include <aips/Mathematics/NumericTraits.h>
#include <trial/ComponentModels/ComponentType.h>

template <class Qtype> class Quantum;

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
	  NumericTraits<T>::ConjugateType yy, ComponentType::Polarisation pol);

  // Stokes representation, units are Jy.
  FluxRep(const Vector<T> & flux);

  // units are Jy.
  FluxRep(const Vector<NumericTraits<T>::ConjugateType> & flux,
  		const ComponentType::Polarisation & pol);
  
  // Stokes representation
  FluxRep(const Quantum<Vector<T> > & flux);

  // Fully Specified
  FluxRep(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
	  const ComponentType::Polarisation & pol);

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
  // user wants I flux only
  T value();
  // Stokes representation & current unit
  void value(Vector<T> & value);
  // current unit and pol
  void value(Vector<NumericTraits<T>::ConjugateType> & value) const;
  // Stokes pol.
  void value(Quantum<Vector<T> > & value);
  // Don't assume anything
  void value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
  	    const ComponentType::Polarisation & pol);
  // </group>

  // Set the current flux assuming for the unspecified values ...
  // <group>
  // User specifies I only and Q=U=V=0 and the current unit
  void setValue(T value); 
  // a Stokes pol and the current unit
  void setValue(const Vector<T> & value); 
  // the current unit and pol
  void setValue(const Vector<NumericTraits<T>::ConjugateType> & value);
  
  // a Stokes pol
  void setValue(const Quantum<Vector<T> > & value);
  // Nothing. Flux is fully specified.
  void setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> >& value,
 	       const ComponentType::Polarisation & pol);
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  Bool ok() const;

private:
  Vector<NumericTraits<T>::ConjugateType> itsFlux;
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
       ComponentType::Polarisation pol);

  // Stokes representation, units are Jy.
  Flux(const Vector<T> & flux);

  // units are Jy.
  Flux(const Vector<NumericTraits<T>::ConjugateType> & flux,
       const ComponentType::Polarisation & pol);
  
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
  T value();
  // Stokes representation & current unit
  void value(Vector<T> & value);
  // current unit and pol
  void value(Vector<NumericTraits<T>::ConjugateType> & value) const;
  // Stokes pol.
  void value(Quantum<Vector<T> > & value);
  // Don't assume anything
  void value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value,
	     const ComponentType::Polarisation & pol);
  // </group>

  // Set the current flux assuming for the unspecified values ...
  // <group>
  // User specifies I only and Q=U=V=0 and the current unit
  void setValue(T value); 
  // a Stokes pol and the current unit
  void setValue(const Vector<T> & value); 
  // the current unit and pol
  void setValue(const Vector<NumericTraits<T>::ConjugateType> & value);
  
  // a Stokes pol
  void setValue(const Quantum<Vector<T> > & value);
  // Nothing. Flux is fully specified.
  void setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> >& value,
		const ComponentType::Polarisation & pol);
  // </group>

  // Functions for converting a 4 element complex vector between
  // different representations.
  // <group>
  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
    			       const Vector<T> & in);

  static void stokesToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in);

  static void circularToStokes(Vector<T> & out,
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in);

  static void circularToStokes(Vector<NumericTraits<T>::ConjugateType> & out,
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in);

  static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
  			     const Vector<T> & in);

  static void stokesToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
  			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in);

  static void linearToStokes(Vector<T> & out, 
 			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in);

  static void linearToStokes(Vector<NumericTraits<T>::ConjugateType> & out, 
 			     const Vector<NumericTraits<T>::ConjugateType> &
 			     in);

  static void linearToCircular(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in);

  static void circularToLinear(Vector<NumericTraits<T>::ConjugateType> & out, 
 			       const Vector<NumericTraits<T>::ConjugateType> &
 			       in);
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  Bool ok() const;

private:
  CountedPtr<FluxRep<T> > itsFluxPtr;
};

#endif
