//# NumericTraits.h: Defines relationships between numeric data types 
//# Copyright (C) 1996,1997,1998
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

#if !defined(AIPS_NUMERICTRAITS_H)
#define AIPS_NUMERICTRAITS_H

#include <aips/aips.h>

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
// <dt> <src>ConjugateType</src>
// <dd>      the Type of the result if a Fourier Transform was to be done.
// <dt> <src>PrecisionType</src>
// <dd>      a Type of the next higher numerical precision. 
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
// </dl>
//
// For complex numbers these values are applicable to the real or imaginary
// components separately.
// 
// The use of this class is best illustrated in a number of examples.
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
// <dt><src>NumericTraits<T>::epsilon</src> 
// <dd> is FLT_EPSILON for Float and Complex types and DBL_EPSILON for Double
// and DComplex data types.
// <dt><src>NumericTraits<T>::minimum</src>
// <dd>is FLT_MIN for Float and complex Types and DBL_MIN for Double and
// DComplex data types.
// <dt><src>NumericTraits<T>::maximum</src>
// <dd>is FLT_MAX for Float and complex Types and DBL_MAX for Double and
// DComplex data types.
// </dl>
// See the <linkto class="C">Constants</linkto> class for the
// values of these variables.
// </example>
//
// <motivation>
// This is a nice way to make the Convolver class singly templated (as it
// should be), even though the FFTServer it contains is doubly templated. 
// </motivation>
//
// <templating arg=T>
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
// <src>NumericTraits<ArbitraryType>::ConjugateType</src> returns 
//   <src>ArbitraryType</src> and <br>
// <src>NumericTraits<ArbitraryType>::PrecisionType</src> returns 
//   <src>ArbitraryType</src><br>
// <src>NumericTraits<ArbitraryType>::epsilon</src> is undefined
// <src>NumericTraits<ArbitraryType>::minimum</src> is undefined
// <src>NumericTraits<ArbitraryType>::maximum</src> is undefined
// </templating>
//
// <thrown>
// This class does not throw any exceptions
// </thrown>
//
// <todo asof="1996/11/09">
// Nothing (I hope!)
// </todo>
//

// A default template class is required by the C++ standard (Annotated C++
// reference Manual by Ellis & Stroustrup pg. 348). It should never be used,
// except through the specialisations in 
// <linkto file="NumericTraits2.h#NumericTraits">NumericTraits2.h</linkto>. 
// The default types for ConjugateType and PrecisionType are deliberatly set to
// a non-numeric type to further discourage the use of the non-specialised
// class defined below. It also helps when using this class with the Sun native
// compiler.

template <class T> class NumericTraits {
public:
  typedef Char ConjugateType; 
  typedef Char PrecisionType;      
  static const Double & epsilon;
  static const Double & minimum;
  static const Double & maximum;
};

//# These specialisations are in a seperate file so that cxx2html 
//# (Nov-96 version) will generate correct documentation for this
//# class. cxx2html gets confused when there are class definitions that
//# differ only in the template type in the same file.

#include <aips/Mathematics/NumericTraits2.h>

#endif
