//# NumericTraits.h: Defines relationships between numeric data types 
//# Copyright (C) 1996,1997
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
#if !defined(AIPS_NUMERICTRAITS_H)
#define AIPS_NUMERICTRAITS_H

#include <aips/aips.h>

//
// <summary> Relationships between numeric data types </summary>
// <use visibility=export>
//
// <reviewed reviewer="nkilleen" date="1996/12/12" tests="tConvolver">
// </reviewed>
//
// <etymology>
// A trait is a characteristic feature. NumericTraits defines relationships
// between Numeric data types.
// </etymology>
//
// <synopsis>
// This templated class contains a number of typedefs that describe the
// relationship between different numeric data types. Its use is in
// templated classes where the use of one type implictly implies the use of
// a corresponding one.  Use of this class avoids the need for double
// templating.
//
// Curently this class defines the following relationships:
// <ul> 
// <li> <src>ConjugateType</src> - the Type of the result if a Fourier
//      Transform was to be done.
// <li> <src>PrecisionType</src> - a Type of the next higher numerical
//      precision. 
// </ul>
// 
// 
// The use of this class is best illustrated in a number of examples.
// </synopsis>
//
// <example>
// eg 1. Suppose you are writing a templated class that needs to do Fourier
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
// eg 2. Suppose you have a templated numerical integrator class. Because the
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
//   <src>ArbitraryType</src>
// </templating>
//
// <thrown>
// This class only contains typedefs and hence cannot throw any exceptions
// </thrown>
//
// <todo asof="1996/11/09">
// Nothing (I hope!)
// </todo>
//

// A default template class is required by the C++ standard (Annotated C++
// reference Manual by Ellis & Stroustrup pg. 348). It should never be used,
// except through the specialisations in 
// <linkto file="NumericTraits2.h#NumericTraits">NumericTraits2.h</linkto>

template <class T> class NumericTraits {
public:
  typedef T ConjugateType; 
  typedef T PrecisionType;      
};

//# These specialisations are in a seperate file so that cxx2html 
//# (Nov-96 version) will generate correct documentation for this
//# class. cxx2html gets confused when there are class definitions that
//# differ only in the template type in the same file.

#include <aips/Mathematics/NumericTraits2.h>

#endif
