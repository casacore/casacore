//# KaiserBFunction.h: A one dimensional Kaiser-Bessel function
//# Copyright (C) 2002,2005
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

#ifndef SCIMATH_KAISERBFUNCTION_H
#define SCIMATH_KAISERBFUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/KaiserBParam.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional Kaiser-Bessel function
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="KaiserBParam">KaiserBParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A Kaiser-Bessel function
// </etymology>

// <synopsis> 
// A <src>Kaiser-Bessel</src> is described by a height, a center, a width
// (halfwidth) and a parameter.
// The parameters are enumerated by HEIGHT, CENTER, WIDTH, KBPAR. They have
// default values of (1, 0, 1, 2.5).
// </synopsis> 
//
// <example>
// <srcblock>
//    KaiserBFunction<Double> sf;
//    sf(0);            // = 1.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
// </thrown>
//

template<class T> class KaiserBFunction : public KaiserBParam<T>
{
public:
  //# Constructors
  // Constructs the KaiserBFunction, Defaults:
  // height=1, center=0, width=1, kbpar=2.5.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  KaiserBFunction() : KaiserBParam<T>() {}
  explicit KaiserBFunction(const T &kbpar) :
    KaiserBParam<T>(kbpar) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  KaiserBFunction(const KaiserBFunction<T> &other) : KaiserBParam<T>(other) {}
  template <class W>
    KaiserBFunction(const KaiserBFunction<W> &other) : KaiserBParam<T>(other) {}
  // </group>


  // Copy assignment (deep copy)
  KaiserBFunction<T> &operator=(const KaiserBFunction<T> &other) {
    KaiserBParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~KaiserBFunction() {}

  //# Operators    
  // Evaluate the KaiserB at <src>x</src>.
  // If a vector is used as the argument only its first element is used.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer. 
  // <group>
  virtual Function<T> *clone() const { return new KaiserBFunction<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new KaiserBFunction<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new KaiserBFunction<typename FunctionTraits<T>::BaseType>(*this); }

  // </group>

  //# Make members of parent classes known.
protected:
  using KaiserBParam<T>::param_p;
public:
  using KaiserBParam<T>::KBPAR;
  using KaiserBParam<T>::CENTER;
  using KaiserBParam<T>::WIDTH;
  using KaiserBParam<T>::HEIGHT;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/KaiserBFunction.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
