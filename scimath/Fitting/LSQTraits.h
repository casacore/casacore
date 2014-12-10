//# LSQTraits.h: Typing support classes for LSQ classes
//# Copyright (C) 2004
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
#ifndef SCIMATH_LSQTRAITS_H
#define SCIMATH_LSQTRAITS_H

//# Includes
#include <casacore/casa/aips.h>
#include <complex>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> Typing support classes for LSQ classes </summary>
// <reviewed reviewer="Wim Brouw" date="2004/04/01" tests="tLSQFit, tLSQaips"
//	 demos="">
// </reviewed>

// <synopsis>
// The following classes are used in detremining the type of iterator
// presented to the LSQFit class. They are for a large part based on ideas by 
// Alexandrescu(2001), 'Modern C++ design'.
// </synopsis>

// <example>
// See <linkto class="LSQFit">LSQFit</linkto> class, especially the
// <src>LSQFit2.cc</src>
//  defintion file.
// </example>

// <motivation>
// To ease the interface to Fitting (and probably other) classes, by producing
// a framework that can be used with Casacore containers.
// </motivation>

// <todo asof="2004/04/01">
//  <li> Add more type info if and when needed
// </todo>

// Type of real numeric class indicator
class LSQReal {
  typedef LSQReal value_type;
};

// <summary> Type of complex numeric class indicator </summary>
class LSQComplex {
  typedef LSQComplex value_type;
};

// <summary> Non relevant class indicator </summary>
class LSQNull {};

// <summary> Determine if pointer type </summary>
template <class T>
class LSQType {
 private:
  template <class U> struct PointerTraits {
    enum { result = False };
    typedef LSQNull Pointee;
  };
  template <class U> struct PointerTraits<U*> {
    enum { result = True };
    typedef U Pointee;
  };
 public:
  enum { isPointer = PointerTraits<T>::result };
  typedef typename PointerTraits<T>::Pointee Pointee;
};

// <summary> Traits for numeric classes used </summary>
template <class T>
class LSQTraits {
 public:
  // Defining type
  typedef T    value_type;
  // Numeric base type
  typedef Char base;
  // Numeric type
  typedef Char num_type;
  // Number of basic numeric type elements
  enum { size = 0 };
};

#if defined LSQTraits_F
#undef LSQTraits_F
#endif
#define LSQTraits_F LSQTraits
// <summary>LSQTraits specialization for Float</summary>
template <> class LSQTraits_F<Float> {
 public:
  typedef Float        value_type;
  typedef Float        base;
  typedef LSQReal      num_type;
  enum { size = 1 };
};
#undef LSQTraits_F

#if defined LSQTraits_D
#undef LSQTraits_D
#endif
#define LSQTraits_D LSQTraits
// <summary>LSQTraits specialization for Double</summary>
template <> class LSQTraits_D<Double> {
 public:
  typedef Double       value_type;
  typedef Double       base;
  typedef LSQReal      num_type;
  enum { size = 1 };
};
#undef LSQTraits_D

#if defined LSQTraits_CD
#undef LSQTraits_CD
#endif
#define LSQTraits_CD LSQTraits
// <summary>LSQTraits specialization for DComplex </summary>
template <> class LSQTraits_CD<std::complex<Double> > {
 public:
  typedef std::complex<Double>    value_type;
  typedef Double                  base;
  typedef LSQComplex              num_type;
  enum { size = 2 };
};
#undef LSQTraits_CD

#if defined LSQTraits_CF
#undef LSQTraits_CF
#endif
#define LSQTraits_CF LSQTraits
// <summary>LSQTraits specialization for Complex </summary>
template <> class LSQTraits_CF<std::complex<Float> > {
 public:
  typedef std::complex<Float>    value_type;
  typedef Float                  base;
  typedef LSQComplex             num_type;
  enum { size = 2 };
};
#undef LSQTraits_CF


} //# NAMESPACE CASACORE - END

#endif
