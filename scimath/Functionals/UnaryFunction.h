//# UnaryFunction.h: A one dimensional unary function
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

#ifndef SCIMATH_UNARYFUNCTION_H
#define SCIMATH_UNARYFUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/UnaryParam.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional unary function
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="UnaryParam">UnaryParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A 1-dimensional unary hat.
// </etymology>

// <synopsis> 
// A <src>Unary</src> is described by a height, a center and a width
// (halfwidth). The value is:
// <srcblock>
//      height          (|x-center| <  width)
//   0.5height          (|x-center| == width)
//      0               (|x-center| >  width)
// </srcblock>
// The parameters are enumerated by HEIGHT, CENTER and WIDTH. They have
// default values of (1, 0, 1).
// </synopsis> 
//
// <example>
// <srcblock>
//    UnaryFunction<Double> sf(5.0, 25.0, 7);
//    sf(25);            // = 5.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
// </thrown>
//

template<class T> class UnaryFunction : public UnaryParam<T>
{
public:
  //# Constructors
  // Constructs the UnaryFunction, Defaults:
  // height=1, center=0, width=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  UnaryFunction() : UnaryParam<T>() {}
  explicit UnaryFunction(const T &height) :
    UnaryParam<T>(height) {}
  UnaryFunction(const T &height, const T &center) :
    UnaryParam<T>(height, center) {}
  UnaryFunction(const T &height, const T &center, const T &width) :
    UnaryParam<T>(height, center, width) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  UnaryFunction(const UnaryFunction<T> &other) : UnaryParam<T>(other) {}
  template <class W>
  UnaryFunction(const UnaryFunction<W> &other) : UnaryParam<T>(other) {}
  // </group>
  // Copy assignment (deep copy)
  UnaryFunction<T> &operator=(const UnaryFunction<T> &other) {
    UnaryParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~UnaryFunction() {}

  //# Operators    
  // Evaluate the Unary at <src>x</src>.
  // If a vector is used as the argument only its first element is used.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer. 
  // <group>
  virtual Function<T> *clone() const { return new UnaryFunction<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new UnaryFunction<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new UnaryFunction<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using UnaryParam<T>::param_p;
public:
  using UnaryParam<T>::nparameters;
  using UnaryParam<T>::CENTER;
  using UnaryParam<T>::WIDTH;
  using UnaryParam<T>::HEIGHT;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/UnaryFunction.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
