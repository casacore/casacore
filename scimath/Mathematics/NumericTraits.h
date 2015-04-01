//# NumericTraits.h: Defines relationships between numeric data types 
//# Copyright (C) 1996,1997,1998,2000,2002
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

#ifndef SCIMATH_NUMERICTRAITS_H
#define SCIMATH_NUMERICTRAITS_H

//# Include files
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//
// <summary> Relationships between numeric data types </summary>
// <use visibility=export>
//
// <reviewed reviewer="nkilleen" date="1996/12/12" tests="tConvolver,tFFTServer">
// </reviewed>
//
// <etymology>
// A trait is a characteristic feature. NumericTraits defines relationships
// between and characteristics of Numeric data types.
// </etymology>
//
// <synopsis>

// This templated class contains a number of typedefs and definitions that
// describe the relationship between different numeric data types and there
// characteristics. Its use is in templated classes either where the use of one
// type implictly implies the use of a corresponding one or where a
// characteristic value differs with templating argument.  Use of this class
// often avoids the need for double templating.
//
// Currently this class defines the following relationships:
// <dl> 
// <dt> <src>value_type</src>
// <dd> The template type itself. The name <src>value_type</src> is the
//	C++ standard (e.g. <src>DComplex::value_type</src> equals
//	<src>double</src>)
// <dt> <src>BaseType</src>
// <dd> The numeric base type. I.e. <src>Double</src> for <src>Double</src>
//	and <src>DComplex</src>; <src>Float</src> for <src>Float</src> and
//	<src>Complex</src>
// <dt> <src>ConjugateType</src>
// <dd> The corresponding complex type for a real type, and real type
//	for a complex type. It is the type of the result if a Fourier
//	Transform was to be done.
// <dt> <src>PrecisionType</src>
// <dd> The Type of the next higher numerical precision. I.e. <src>Double</src>
//	or <src>DComplex</src>
// </dl>
// 
// And the following characteristics:
// <dl> 
// <dt> <src>epsilon</src>
// <dd> A Double containing the smallest value such that 1+epsilon is different
//      from 1.
// <dt> <src>minimum</src>
// <dd> A Double containing the smallest positive representable number,
//      excluding denormalised numbers.
// <dt> <src>maximum</src>
// <dd> A Double containing the largest representable number.
// <dt> <src>size()</src>
// <dd> The number of numeric values in the templated entity. It will be
//	2 for complex numbers; 1 for real numbers.
// <dt> <src>setImag(T &other, const BaseType &val)</src>
// <dd> Set an imaginary part (for complex numbers) or a NOP (for reals).
// <dt> <src>getValue(T &other, const uInt n)</src>
// <dd> Get the <src>n%size()-th</src> value in the argument.
//	For complex numbers the sequence is real, imaginary part.
// <dt> <src>setValue(T &other, const BaseType &val, const uInt n)</src>
// <dd> Set the <src>n%size()-th</src> value in the argument.
//	For complex numbers the sequence is real, imaginary part.
// </dl>
//
// For complex numbers these values are applicable to the real or imaginary
// components separately.
// 
// The use of this class is best illustrated in a number of examples.
//
// A default template declaration is required by the C++ standard.
// It should never be used, except through the specialisations.
// The default types for ConjugateType and PrecisionType are deliberatly set to
// a non-numeric type to further discourage the use of the non-specialized
// class defined below. It also helps when using this class with the Sun native
// compiler.
// <note role=warning> The specialized instantiations seem to have a name with
// an appended code. This is only for cxx2html reasons. The name is in all
// cases <src>NumericTraits</src>
// </note>
//
// This class is implemented as a number of specialisations for the
// following data types.
// <ul> 
// <li> Float
// <li> Double
// <li> Complex
// <li> DComplex
// </ul>
// This class should not be used with other template types and does nothing
// except return its template type if it is used. ie. <br>
// <src>NumericTraits<ArbitraryType>\::ConjugateType</src> returns 
//   <src>Char</src> and <br>
// <src>NumericTraits<ArbitraryType>\::PrecisionType</src> returns 
//   <src>Char</src><br>
// <src>NumericTraits<ArbitraryType>\::epsilon</src> is undefined<br>
// <src>NumericTraits<ArbitraryType>\::minimum</src> is undefined<br>
// <src>NumericTraits<ArbitraryType>\::maximum</src> is undefined
// </synopsis>
//
// <example>
// <h4>Example 1:</h4>
// Suppose you are writing a templated class that needs to do Fourier
// Transforms. The FFTServer class can do FFT's of Float or Double data
// types, but you need to doubly template it on the conjugate data type. To
// avoid having the conjugate data type appear as a template in the class
// you are writing you can use the ConjugateType typedef.
// <srcblock> 
// template<class T> class myClass {
// private:
// FFTServer<T, NumericTraits<T>::ConjugateType> server;
// }
// </srcblock>
// The ConjugateType transforms
// <ul>
// <li> Float -> Complex
// <li> Double -> DComplex
// <li> Complex -> Float
// <li> DComplex -> Double
// </ul>
//
// <h4>Example 2:</h4>
// Suppose you have a templated numerical integrator class. Because the
// individual samples can be negative it is possible to add two numbers
// of nearly equal magnitude but opposite sign and lose precision
// considerably. One way to combat this is to make the accumulator variable
// the next higher precision numerical type. The PrecisionType typedef
// defines what type this is
// <srcblock> 
// template<class T> class Integrator {
// private:
// NumericTraits<T>::PrecisionType accumulator;
// }
// </srcblock>
// The PrecisionType transforms
// <ul>
// <li> Float -> Double
// <li> Double -> Double
// <li> Complex -> DComplex
// <li> DComplex -> DComplex
// </ul>

// <h4>Example 3:</h4>
// Suppose you have a templated class that needs to use the <src>allNear</src>
// functions from
// <linkto group="ArrayMath.h#Array mathematical operations">ArrayMath</linkto>
// to determine if a templated Array is near
// one. The tolerance argument to the allNear function will depend on the
// template type and this is not known until the template is instantiated. The
// epsilon trait can be used to supply this value.

// <srcblock> 
// template<class T> void myClass<T>::myFunction(Array<T> & aArray) {
//   if (allNear(aArray, T(1), NumericTraits<T>::epsilon))
//     return;
// // Do something
// }
// </srcblock>

// <dl>
// <dt><src>NumericTraits<T>\::epsilon</src> 
// <dd> is FLT_EPSILON for Float and Complex types and DBL_EPSILON for Double
// and DComplex data types.
// <dt><src>NumericTraits<T>\::minimum</src>
// <dd>is FLT_MIN for Float and complex Types and DBL_MIN for Double and
// DComplex data types.
// <dt><src>NumericTraits<T>\::maximum</src>
// <dd>is FLT_MAX for Float and complex Types and DBL_MAX for Double and
// DComplex data types.
// </dl>
// See the <linkto class=Dummy_Constants_class">C class/namespace</linkto>
// for the values of these variables.
// </example>
//
// <motivation>
// This is a nice way to make the Convolver class singly templated (as it
// should be), even though the FFTServer it contains is doubly templated. 
// </motivation>
//
// <thrown>
// <li> This class does not throw any exceptions
// </thrown>
//
// <todo asof="2002/06/25">
// <li> Nothing (I hope!)
// </todo>
//

template <class T> class NumericTraits {
public:
  // Template argument
  typedef T    value_type;
  // Numeric type
  typedef Char BaseType;
  // Conjugate (<src>real<->complex</src>) type
  typedef Char ConjugateType; 
  // Higher precision type (<src>Float->Double</src>)
  typedef Char PrecisionType;      
  // Relevant minimum and maximum numbers
  // <group>
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
  // </group>
  // Number of relevant numeric values
  static uInt  size() { return 0; }
  // Set the imaginary part of a complex value only (a NOP for reals)
  static void setImag(T &, const BaseType &) {;}
  // Get the <src>n%size()-th</src> numeric value
  static BaseType getValue(const T &, const uInt) { return 0; }
  // Set the <src>n%size()-th</src> numeric value
  static void setValue(T &, const BaseType &, const uInt) {;}
};

#if defined NumericTraits_F
#undef NumericTraits_F
#endif
#define NumericTraits_F NumericTraits

// <summary>NumericTraits specialization for Float</summary>

template <> class NumericTraits_F<Float> {
public:
  // Template argument
  typedef Float    value_type;
  // Numeric type
  typedef Float	   BaseType;
  // Conjugate (<src>real<->complex</src>) type
  typedef Complex  ConjugateType; 
  // Higher precision type (<src>Float->Double</src>)
  typedef Double   PrecisionType;      
  // Relevant minimum and maximum numbers
  // <group>
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
  // </group>
  // Number of relevant numeric values
  static uInt  size() { return 1; }
  // Set the imaginary part of a complex value only (a NOP for reals)
  static void setImag(value_type &, const BaseType &) {;}
  // Get the <src>n%size()-th</src> numeric value
  static BaseType getValue(const value_type &other, const uInt) {
    return other; }
  // Set the <src>n%size()-th</src> numeric value
  static void setValue(value_type &other, const BaseType &val, const uInt) {
    other = val; }
};

#undef NumericTraits_F

#if defined NumericTraits_D
#undef NumericTraits_D
#endif
#define NumericTraits_D NumericTraits

// <summary>NumericTraits specialization for Double</summary>

template <> class NumericTraits_D<Double> {
public:
  // Template argument
  typedef Double    value_type;
  // Numeric type
  typedef Double    BaseType;
  // Conjugate (<src>real<->complex</src>) type
  typedef DComplex  ConjugateType; 
  // Higher precision type (<src>Float->Double</src>)
  typedef Double    PrecisionType;      
  // Relevant minimum and maximum numbers
  // <group>
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
  // </group>
  // Number of relevant numeric values
  static uInt  size() { return 1; }
  // Set the imaginary part of a complex value only (a NOP for reals)
  static void setImag(value_type &, const BaseType &) {;}
  // Get the <src>n%size()-th</src> numeric value
  static BaseType getValue(const value_type &other, const uInt) {
    return other; }
  // Set the <src>n%size()-th</src> numeric value
  static void setValue(value_type &other, const BaseType &val, const uInt) {
    other = val; }
};

#undef NumericTraits_D

#if defined NumericTraits_C
#undef NumericTraits_C
#endif
#define NumericTraits_C NumericTraits

// <summary>NumericTraits specialization for Complex</summary>

template <> class NumericTraits_C<Complex> {
public:
  // Template argument
  typedef Complex    value_type;
  // Numeric type
  typedef Float      BaseType;
  // Conjugate (<src>real<->complex</src>) type
  typedef Float      ConjugateType; 
  // Higher precision type (<src>Float->Double</src>)
  typedef DComplex   PrecisionType;      
  // Relevant minimum and maximum numbers
  // <group>
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
  // </group>
  // Number of relevant numeric values
  static uInt  size() { return 2; }
  // Set the imaginary part of a complex value only (a NOP for reals)
  static void setImag(value_type &other, const BaseType &val) {
    other = value_type(other.real(), val); }
  // Get the <src>n%size()-th</src> numeric value
  static BaseType getValue(const value_type &other, const uInt n) {
    return ((n%2 == 0) ? other.real() : other.imag()); }
  // Set the <src>n%size()-th</src> numeric value
  static void setValue(value_type &other, const BaseType &val, const uInt n) {
    other = (n%2 == 0) ? value_type(val, other.imag()) :
      value_type(other.real(), val); }
};

#undef NumericTraits_C

#if defined NumericTraits_DC
#undef NumericTraits_DC
#endif
#define NumericTraits_DC NumericTraits

// <summary>NumericTraits specialization for DComplex</summary>

template <> class NumericTraits_DC<DComplex> {
public:
  // Template argument
  typedef DComplex    value_type;
  // Numeric type
  typedef Double      BaseType;
  // Conjugate (<src>real<->complex</src>) type
  typedef Double      ConjugateType; 
  // Higher precision type (<src>Float->Double</src>)
  typedef DComplex   PrecisionType;      
  // Relevant minimum and maximum numbers
  // <group>
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
  // </group>
  // Number of relevant numeric values
  static uInt  size() { return 2; }
  // Set the imaginary part of a complex value only (a NOP for reals)
  static void setImag(value_type &other, const BaseType &val) {
    other = value_type(other.real(), val); }
  // Get the <src>n%size()-th</src> numeric value
  static BaseType getValue(const value_type &other, const uInt n) {
    return ((n%2 == 0) ? other.real() : other.imag()); }
  // Set the <src>n%size()-th</src> numeric value
  static void setValue(value_type &other, const BaseType &val, const uInt n) {
    other = (n%2 == 0) ? value_type(val, other.imag()) :
      value_type(other.real(), val); }
};

#undef NumericTraits_DC


} //# NAMESPACE CASACORE - END

#endif
