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

class RecordInterface;

// <summary>A class that represents the Flux (copy semantics)</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Vector">Vector</linkto>
//   <li> <linkto class="Unit">Unit</linkto>
// </prerequisite>
//
// <etymology>
// This class actually contains the flux values as distinct from the Flux class
// which is a pointer to this class. Hence this class name is a shortening of
// "Flux Representation". The Flux class is probably of more general user
// utility as its reference semantics can cut down on the number of temporary
// objects that get constructed.
// </etymology>
//
// <synopsis>
// This class encapsulates three quantities that are needed to more completely
// represent the polarised flux of a component. These quantities are:
// <dl>
// <dt> Flux values.
// <dd> These are four numbers which are the numerical value of the flux in all
//      four polarisations. The values can be represented in either single or
//      double precision depending on the template type.
// <dt> Flux units.
// <dd> These are the units for the flux values. The units must have dimensions
//      of W/m^2/Hz and are represented using the
//      <linkto class="Unit">Unit</linkto> class. The most common unit is "Jy".
// <dt> Polarisation representation.
// <dd> This describes how the polarised flux is represented. It can be one of
//      the following:
//      <dl compact><dt>Stokes<dd>
//      The flux is representing using the Stokes I, Q, U, V components
//      respectively.
//      <dt>Linear<dd>
//      The flux is representing using the XX,XY,YX,YY correlation products
//      resp. X and Y is the signal from linear feeds at zero parallactic
//      angle.
//      <dt>Circular<dd>
//      The flux is representing using the RR,RL,LR,LL correlation products
//      resp. R and L is the signal from right and left handed circular feeds.
//      </dl>
// </dl>
// This class is templated but only two templated types are valid. These are:
// <ul> 
// <li> T = Float
// <li> T = Double
// </ul>
// The template type defines the precision of the Flux Values.
//
// This class uses functions in the Flux class which convert between the
// different polarisation representations. They assume (until a decision has
// been made on this) that the total intensity is the average of the linear or
// circular correlation products. ie., <src>I = (RR+LL)/2 = (XX+YY)/2</src>.
//
// In order to represent the Flux using the circular or linear representations
// the Flux values need to be complex (eg.,<src>XY = I + iV, YX = I - iV</src>)
// This class does not require, or check a number of constraints such as
// <src>XY = conj(YX)</src> and hence it is possible to define a flux using a
// linear or circular representation that cannot be completely represented
// using the Stokes representation. Because this class stores the flux values
// as complex numbers there is no loss of accuracy when converting between
// different polarisation representations. But it discards the imaginary
// component of the flux when externally representing the flux using with a
// Stokes representation (eg., when calling the <src>value(Vector<T> &)</src>
// function).
//
// Because this class using Complex numbers with a precision that depends on
// the template type many of the function arguments are of type
// <src>NumericTraits<T></src>. This simply a type that maps to Complex if T is
// Float and DComplex if T is a Double. Because of problems with the the gnu
// compiler functions which use this type as an argument MUST be
// inline. Hopefully this problem will go away sometime.
// </synopsis>
//
// <example>
// The following example creates a FluxRep object using a Stokes representation
// and converts it to "WU" (Westerbork Units). After printing out the converted
// I flux it converts the Flux to a linear representation and prints out a
// Vector with the [XX,XY,YX,YY] values (still in "WU")
// <srcblock>
// FluxRep<Double> flux(1.0, 0.0, 0.0, 0.1); // I = 1.0, V = 0.1
// flux.convertUnit("WU");
// cout << "The I flux (in WU is)" << flux.value(0) << endl;
// flux.convertPol(ComponentType::LINEAR);
// cout << "The XX,XY,YX,YY flux (in WU is)" << flux.value().ac() << endl;
// </srcblock>
// </example>
//
// <motivation>
// This class was needed to contain the flux in the ComponentModels module and
// centralizes a lot of code that would otherwise be duplicated in disparate
// places. 
// </motivation>
//
// <thrown>
//    <li> AipsError, When the Vectors are not of length 4 or when indices are
//          greater than 4
// </thrown>
//
// <todo asof="1998/03/11">
//   <li> get this class reviewed.
// </todo>

template<class T> class FluxRep
{
public:

  // The default constructor makes an object with <src>I = 1, Q=U=V=0</src>,
  // a Stokes representation, and units of "Jy".
  FluxRep();

  // This constructor makes an object where I is specified and
  // <src>Q=U=V=0</src>. It assumes a Stokes representation, and units of "Jy".
  FluxRep(T i);

  // This constructor makes an object where I,Q,U,V are all specified. It
  // assumes a Stokes representation, and units of "Jy".
  FluxRep(T i, T q, T u, T v);

  // This constructor makes an object where the flux values and polarisation
  // representation are specified. It assumes the units are "Jy".
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
  
  // This constructor makes an object where I,Q,U,V are all specified by a
  // Vector that must have four elements. It assumes a Stokes representation,
  // and units of "Jy".
  FluxRep(const Vector<T> & flux);

  // This constructor makes an object where the flux values are all specified
  // by a Vector that must have four elements. The polarisation representation
  // must also be specified. It assumes the units are "Jy".
  FluxRep(const Vector<NumericTraits<T>::ConjugateType> & flux,
	  ComponentType::Polarisation pol)
    :itsVal(flux.copy()),
     itsPol(pol),
     itsUnit("Jy")
    {
      DebugAssert(ok(), AipsError);
    };
  
  // This constructor makes an object where the flux values are all specified
  // by a Quantum<Vector> that must have four elements.  The Quantum must have
  // units that are dimensionally equivalent to the "Jy" and these are the
  // units of the FluxRep object. A Stokes polarisation representation is
  // assumed.
  FluxRep(const Quantum<Vector<T> > & flux);

  // This constructor makes an object where the flux values are all specified
  // by a Quantum<Vector> that must have four elements. The Quantum must have
  // units that are dimensionally equivalent to the "Jy" and these are the
  // units of the FluxRep object. The polarisation representation must also be
  // specified.
  FluxRep(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
	  ComponentType::Polarisation pol)
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

  // These two functions return the current units
  // <group>
  const Unit & unit() const;
  void unit(Unit & unit) const;
  // </group>

  // This function sets the current unit. It does NOT convert the flux values
  // to correspond to the new unit. The new unit must be dimensionally
  // equivalent to the "Jy".
  void setUnit(const Unit & unit);

  // This function sets the current units to the supplied value and
  // additionally converts the internal flux values to the correspond to the
  // new unit.
  void convertUnit(const Unit & unit);

  // These two functions return the current polarisation representation.
  // <group>
  ComponentType::Polarisation pol() const;
  void pol(ComponentType::Polarisation & pol) const;
  // </group>

  // This function sets the current polarisation representation. It does NOT
  // convert the flux values.
  void setPol(ComponentType::Polarisation pol);

  // This function sets the current polarisation representation to the supplied
  // value and additionally converts the internal flux values to the correspond
  // to the new polarisation representation.
  void convertPol(ComponentType::Polarisation pol);

  // This function returns the flux values. The polarisation representation and
  // units are in whatever is current.
  const Vector<NumericTraits<T>::ConjugateType> & value() const {
    DebugAssert(ok(), AipsError);
    return itsVal;
  };

  // This function returns the specified component of the flux values.
  // The polarisation representation and units are in whatever is current.
  const NumericTraits<T>::ConjugateType & value(uInt p) const {
    DebugAssert(p < 4, AipsError);
    DebugAssert(ok(), AipsError);
    return itsVal(p);
  };

  // This function returns the flux values after converting it to the Stokes
  // representation. The units of the returned Vector are the current units.
  void value(Vector<T> & value);

  // This function returns the flux values. The polarisation representation and
  // units are in whatever is current.
  void value(Vector<NumericTraits<T>::ConjugateType> & value) const {
    DebugAssert(ok(), AipsError);
    uInt len = value.nelements();
    DebugAssert (len == 4 || len == 0, AipsError);
    value = itsVal;
  };

  // This function returns the flux values after converting it to the Stokes
  // representation. The units of the returned Quantum are the current units of
  // the FluxRep object. The length of the Vector in the Quantum will be
  // resized to 4 elements if it is not already that length.
  void value(Quantum<Vector<T> > & value);

  // This function returns the flux values. The units of the returned Quantum
  // are the current units of the FluxRep object. Similarly the polarisation
  // representation of the returned Quantum is the current polarisation
  // representation. The length of the Vector in the Quantum will be resized to
  // 4 elements if it is not already that length.
  void value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value) const {
    const Unit & curUnit = value.getFullUnit();
    if (curUnit != itsUnit) {
      value.setUnit(itsUnit);
    }
    Vector<NumericTraits<T>::ConjugateType> & newValue = value.getValue();
    if (newValue.nelements() != 4) newValue.resize(4);
    for (uInt s = 0 ; s < 4; s++) {
      newValue(s) = itsVal(s);
    }
    DebugAssert(ok(), AipsError);
  };

  // This function sets the Flux values assuming the supplied value represents
  // the Stokes I flux in the current units. The other Stokes parameters are
  // set to zero.
  void setValue(T value);
 
  // This function sets the Flux values assuming the supplied values represent
  // the flux in the Stokes representation and is in the current units. The
  // Vector must have four elements.
  void setValue(const Vector<T> & value); 

  // This function sets the Flux values assuming the supplied values represent
  // the flux in the current representation and units. The Vector must have
  // four elements.
  void setValue(const Vector<NumericTraits<T>::ConjugateType> & value) {
    DebugAssert (value.nelements() == 4, AipsError);
    itsVal = value;
    DebugAssert(ok(), AipsError);
  };

  // This function sets the flux values and units assuming the supplied values
  // represent the flux in the Stokes representation. The units of the Quantum
  // must be dimensionally equivalent to the "Jy" and the Vector must have four
  // elements.
  void setValue(const Quantum<Vector<T> > & value);

  // This function sets the flux values, units and polarisation assuming the
  // supplied values represent the flux in the specified representation. The
  // units of the Quantum must be dimensionally equivalent to the "Jy" and the
  // Vector must have four elements.
  void setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> >& value,
		ComponentType::Polarisation pol) {
    DebugAssert (value.getValue().nelements() == 4, AipsError);
    itsVal = value.getValue();
    itsUnit = value.getFullUnit();
    itsPol = pol;
    DebugAssert(ok(), AipsError);
  };

  // Scale the Flux value by the specified amount. These functions multiply the
  // flux values irrespective of the current polarisation representation. If
  // only one scale factor is supplied then only the first component of the
  // flux is scaled.
  // <group>
  void scaleValue(const T & factor);
  void scaleValue(const T & factor0, const T & factor1,
		  const T & factor2, const T & factor3);
  void scaleValue(const NumericTraits<T>::ConjugateType & factor) {
    itsVal(0) *= factor;
    DebugAssert(ok(), AipsError);
  };
  void scaleValue(const NumericTraits<T>::ConjugateType & factor0,
		  const NumericTraits<T>::ConjugateType & factor1,
		  const NumericTraits<T>::ConjugateType & factor2,
		  const NumericTraits<T>::ConjugateType & factor3) {
    itsVal(0) *= factor0;
    itsVal(1) *= factor1;
    itsVal(2) *= factor2;
    itsVal(3) *= factor3;
    DebugAssert(ok(), AipsError);
  };
  // </group>

  // This functions convert between a RecordInterface and a FluxRep object and
  // define how the FluxRep is represented in glish.  They return False if the
  // RecordInterface is malformed and append an error message to the supplied
  // string giving the reason.
  // <group>
  Bool fromRecord(String & errorMessage, const RecordInterface & record);
  Bool toRecord(String & errorMessage, RecordInterface & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistent values. Returns True if everything is fine
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
//   <li> <linkto class="Vector">Vector</linkto>
//   <li> <linkto class="Unit">Unit</linkto>
//   <li> <linkto class="FluxRep">FluxRep</linkto>
// </prerequisite>
//
// <etymology>
// The Flux class is used to represent the flux of a component on the sky
// </etymology>
//
// <synopsis>

// This class is nearly identical to the <linkto
// class="FluxRep">FluxRep</linkto> class and the reader is referred to the
// documentation of this class for a general explanation of this class.  Most
// of the functions in this class just call the functions in the FluxRep class.

// There are two important differences between the Flux class and the FluxRep
// class. 
// <ul>
// <li> This class uses reference semantics rather than copy semantics. This
// aids in cutting down on the number of temporary objects that need to be
// constructed. An example of this is illustrated in the examples section
// below.
// <li> This class contains functions for converting between different
// polarisation representations. 
// </ul>
// The functions for converting between different polarisation representations
// require that the supplied and returned vector have all four polarisations.
// In the Stokes representation the order of the elements is I,Q,U,V, in the
// linear representation it is XX,XY,YX,YY, and in the circular representation
// it is RR,RL,LR,LL.
//
// These functions will correctly convert between Linear/Circular
// representations and the Stokes representation even if the linear or circular
// polarisation cannot represent a physically realisable polarisation (eg if
// <src>XY != conj(YX)</src>). In these cases the stokes representation will
// have an imaginary component and be complex. When converting the complex
// Stokes representation to a real one the imaginary components are simply
// discarded.
// </synopsis>
//
// <example>
// The function in this example calculates the total flux of all the
// components in a list. It accumulates the flux in a Vector after ensuring
// that the flux is in the appropriate units and Polarisation. It then returns
// the sum as a Flux object. Because this class uses reference semantics the
// returned object is passed by reference and hence this is a relatively cheap
// operation.
// <srcblock>
// Flux<Double> totalFlux(ComponentList & list) {
//   Vector<DComplex> sum(4, DComplex(0.0, 0.0));
//   for (uInt i = 0; i < list.nelements(); i++) {
//     list.component(i).flux().convertPol(ComponentType::STOKES);
//     list.component(i).flux().convertUnit("Jy");
//     sum.ac() += list.component(i).flux().value().ac()
//   }
//   return Flux<Double>(value, ComponentType::STOKES);
// }
// </srcblock>
// </example>
//
// <motivation>
// This class was needed to contain the flux in the ComponentModels class. It
// centralizes a lot of code that would otherwise be duplicated. The reference
// semantics further simplify the interface that the component classes use.
// </motivation>
//
// <thrown>
//    <li> AipsError, When the Vectors are not of length 4 or when indices are
//          greater than 4
// </thrown>
//
// <todo asof="1998/03/11">
//   <li> get this class reviewed.
// </todo>

template<class T> class Flux
{
public:
  // The default constructor makes an object with <src>I = 1, Q=U=V=0</src>,
  // a Stokes representation, and units of "Jy".
  Flux();

  // The default constructor makes an object with <src>I = 1, Q=U=V=0</src>,
  // a Stokes representation, and units of "Jy".
   Flux(T i);

  // This constructor makes an object where I,Q,U,V are all specified. It
  // assumes a Stokes representation, and units of "Jy".
  Flux(T i, T q, T u, T v);

  // This constructor makes an object where the flux values and polarisation
  // representation are specified. It assumes the units are "Jy".
  Flux(NumericTraits<T>::ConjugateType xx, NumericTraits<T>::ConjugateType xy,
       NumericTraits<T>::ConjugateType yx, NumericTraits<T>::ConjugateType yy, 
       ComponentType::Polarisation pol)  
    :itsFluxPtr(new FluxRep<T>(xx, xy, yx, yy, pol))
    {
      DebugAssert(ok(), AipsError);
    };

  // This constructor makes an object where I,Q,U,V are all specified by a
  // Vector that must have four elements. It assumes a Stokes representation,
  // and units of "Jy".
  Flux(const Vector<T> & flux);

  // This constructor makes an object where the flux values are all specified
  // by a Vector that must have four elements. The polarisation representation
  // must also be specified. It assumes the units are "Jy".
  Flux(const Vector<NumericTraits<T>::ConjugateType> & flux,
       ComponentType::Polarisation pol)
    :itsFluxPtr(new FluxRep<T>(flux, pol))
    {
      DebugAssert(ok(), AipsError);
    };
  
  // This constructor makes an object where the flux values are all specified
  // by a Quantum<Vector> that must have four elements.  The Quantum must have
  // units that are dimensionally equivalent to the "Jy" and these are the
  // units of the FluxRep object. A Stokes polarisation representation is
  // assumed.
  Flux(const Quantum<Vector<T> > & flux);

  // This constructor makes an object where the flux values are all specified
  // by a Quantum<Vector> that must have four elements. The Quantum must have
  // units that are dimensionally equivalent to the "Jy" and these are the
  // units of the FluxRep object. The polarisation representation must also be
  // specified.
  Flux(const Quantum<Vector<NumericTraits<T>::ConjugateType> > & flux,
       ComponentType::Polarisation pol);

  // The copy constructor uses reference semantics.
  Flux(const Flux<T> & other);

  // The destructor is trivial
  ~Flux();

  // The assignment operator uses reference semantics.
  Flux<T> & operator=(const Flux<T> & other);

  // Return a distinct copy of this flux. As both the assignment operator
  // and the copy constructor use reference semantics this is the only way to
  // get a real copy.
  Flux<T> copy() const;

  // These two functions return the current units.
  // <group>
  const Unit & unit() const;
  void unit(Unit & unit) const;
  // </group>

  // This function sets the current unit. It does NOT convert the flux values
  // to correspond to the new unit. The new unit must be dimensionally
  // equivalent to the "Jy".
  void setUnit(const Unit & unit);

  // This function sets the current units to the supplied value and
  // additionally converts the internal flux values to the correspond to the
  // new unit.
  void convertUnit(const Unit & unit);

  // These two functions return the current polarisation representation.
  // <group>
  ComponentType::Polarisation pol() const;
  void pol(ComponentType::Polarisation & pol) const;
  // </group>

  // This function sets the current polarisation representation. It does NOT
  // convert the flux values.
  void setPol(ComponentType::Polarisation pol);

  // This function sets the current polarisation representation to the supplied
  // value and additionally converts the internal flux values to the correspond
  // to the new polarisation representation.
  void convertPol(ComponentType::Polarisation pol);

  // This function returns the flux values. The polarisation representation and
  // units are in whatever is current.
  const Vector<NumericTraits<T>::ConjugateType> & value() const {
    DebugAssert(ok(), AipsError);
    return itsFluxPtr->value();
  };

  // This function returns the specified component of the flux values.
  // The polarisation representation and units are in whatever is current.
  const NumericTraits<T>::ConjugateType & value(uInt p) const {
    DebugAssert(ok(), AipsError);
    return itsFluxPtr->value(p);
  };

  // This function returns the flux values after converting it to the Stokes
  // representation. The units of the returned Vector are the current units.
  void value(Vector<T> & value);

  // This function returns the flux values. The polarisation representation and
  // units are in whatever is current.
  void value(Vector<NumericTraits<T>::ConjugateType> & value) const {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->value(value);
  };

  // This function returns the flux values after converting it to the Stokes
  // representation. The units of the returned Quantum are the current units of
  // the FluxRep object. The length of the Vector in the Quantum will be
  // resized to 4 elements if it is not already that length.
  void value(Quantum<Vector<T> > & value);

  // This function returns the flux values. The units of the returned Quantum
  // are the current units of the FluxRep object. Similarly the polarisation
  // representation of the returned Quantum is the current polarisation
  // representation. The length of the Vector in the Quantum will be resized to
  // 4 elements if it is not already that length.
  void value(Quantum<Vector<NumericTraits<T>::ConjugateType> > & value) const {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->value(value);
  };

  // This function sets the Flux values assuming the supplied value represents
  // the Stokes I flux in the current units. The other Stokes parameters are
  // set to zero.
  void setValue(T value); 

  // This function sets the Flux values assuming the supplied values represent
  // the flux in the Stokes representation and is in the current units. The
  // Vector must have four elements.
  void setValue(const Vector<T> & value); 

  // This function sets the Flux values assuming the supplied values represent
  // the flux in the current representation and units. The Vector must have
  // four elements.
  void setValue(const Vector<NumericTraits<T>::ConjugateType> & value) {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->setValue(value);
  };

  // This function sets the flux values and units assuming the supplied values
  // represent the flux in the Stokes representation. The units of the Quantum
  // must be dimensionally equivalent to the "Jy" and the Vector must have four
  // elements.
  void setValue(const Quantum<Vector<T> > & value);

  // This function sets the flux values, units and polarisation assuming the
  // supplied values represent the flux in the specified representation. The
  // units of the Quantum must be dimensionally equivalent to the "Jy" and the
  // Vector must have four elements.
  void setValue(const Quantum<Vector<NumericTraits<T>::ConjugateType> >& value,
		ComponentType::Polarisation pol) {
    DebugAssert(ok(), AipsError);
    itsFluxPtr->setValue(value, pol);
  };

  // Scale the Flux value by the specified amount. These functions multiply the
  // flux values irrespective of the current polarisation representation. If
  // only one scale factor is supplied then only the first component of the
  // flux is scaled.
  // <group>
  void scaleValue(const T & factor);
  void scaleValue(const T & factor0, const T & factor1,
		  const T & factor2, const T & factor3);
  void scaleValue(const NumericTraits<T>::ConjugateType & factor) {
    itsFluxPtr->scaleValue(factor);
    DebugAssert(ok(), AipsError);
  };
  void scaleValue(const NumericTraits<T>::ConjugateType & factor0,
		  const NumericTraits<T>::ConjugateType & factor1,
		  const NumericTraits<T>::ConjugateType & factor2,
		  const NumericTraits<T>::ConjugateType & factor3) {
    itsFluxPtr->scaleValue(factor0, factor1, factor2, factor3);
    DebugAssert(ok(), AipsError);
  };
  // </group>

  // This functions convert between a RecordInterface and a Flux object and
  // define how the Flux is represented in glish.  They return False if the
  // RecordInterface is malformed and append an error message to the supplied
  // string giving the reason.
  // <group>
  Bool fromRecord(String & errorMessage, const RecordInterface & record);
  Bool toRecord(String & errorMessage, RecordInterface & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistent values. Returns True if everything is fine
  // otherwise returns False.
  Bool ok() const;

  // This function converts between a Vector in Stokes representation and one
  // in Circular representation.
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

  // This function converts between a Vector in Stokes representation and one
  // in Circular representation. The imaginary components of the Stokes vector
  // are NOT ignored.
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

  // This function converts between a Vector in Circular representation and one
  // in Stokes representation. The imaginary components of the Stokes vector
  // are discarded.
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

  // This function converts between a Vector in Circular representation and one
  // in Stokes representation. The imaginary components of the Stokes vector
  // are NOT ignored.
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

  // This function converts between a Vector in Stokes representation and one
  // in Linear representation.
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

  // This function converts between a Vector in Stokes representation and one
  // in Linear representation. The imaginary components of the Stokes vector
  // are NOT ignored.
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

  // This function converts between a Vector in Linear representation and one
  // in Stokes representation. The imaginary components of the Stokes vector
  // are discarded.
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

  // This function converts between a Vector in Linear representation and one
  // in Stokes representation. The imaginary components of the Stokes vector
  // are NOT ignored.
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
    v.re = (xy.im-yx.im)/T(2);
    v.im = (yx.re-xy.re)/T(2);
  };

  // This function converts between a Vector in Linear representation and one
  // in Circular representation.
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

  // This function converts between a Vector in Circular representation and one
  // in Linear representation.
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

private:
  CountedPtr<FluxRep<T> > itsFluxPtr;
};

#endif
