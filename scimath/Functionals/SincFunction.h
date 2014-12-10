//# SincFunction.h: A one dimensional sin(x)/x
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

#ifndef SCIMATH_SINCFUNCTION_H
#define SCIMATH_SINCFUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/SincParam.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional sin(x)/x
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SincParam">SincParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A 1-dimensional sinc function.
// </etymology>

// <synopsis> 
// A <src>Sinc</src> is described by a height, a center and a width
// (halfwidth). The value is:
// <srcblock>
//      let y = (x-center)/width
//      height                              (x == center)
//      height*sin(pi*y)/(pi*y)             (x |= center)
// </srcblock>
// The parameters are enumerated by HEIGHT, CENTER and WIDTH. They have
// default values of (1, 0, 1).
// </synopsis> 
//
// <example>
// <srcblock>
//    SincFunction<Double> sf(5.0, 25.0, 7);
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

template<class T> class SincFunction : public SincParam<T>
{
public:
  //# Constructors
  // Constructs the SincFunction, Defaults:
  // height=1, center=0, width=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  SincFunction() : SincParam<T>() {}
  explicit SincFunction(const T &height) :
    SincParam<T>(height) {}
  SincFunction(const T &height, const T &center) :
    SincParam<T>(height, center) {}
  SincFunction(const T &height, const T &center, const T &width) :
    SincParam<T>(height, center, width) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  SincFunction(const SincFunction<T> &other) : SincParam<T>(other) {}
  template <class W>
    SincFunction(const SincFunction<W> &other) : SincParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  SincFunction<T> &operator=(const SincFunction<T> &other) {
    SincParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~SincFunction() {}

  //# Operators    
  // Evaluate the Sinc at <src>x</src>.
  // If a vector is used as the argument only its first element is used.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer. 
  // <group>
  virtual Function<T> *clone() const { return new SincFunction<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new SincFunction<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new SincFunction<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using SincParam<T>::param_p;
public:
  using SincParam<T>::nparameters;
  using SincParam<T>::CENTER;
  using SincParam<T>::WIDTH;
  using SincParam<T>::HEIGHT;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/SincFunction.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
