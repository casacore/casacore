//# DiracDFunction.h: A one dimensional Dirac delta function
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

#ifndef SCIMATH_DIRACDFUNCTION_H
#define SCIMATH_DIRACDFUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/DiracDParam.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional Dirac delta function
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="DiracDParam">DiracDParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A 1-dimensional Dirac delta.
// </etymology>

// <synopsis> 
// A <src>DiracD</src> is described by a height, a center and a width
// (halfwidth). The value is:
// <srcblock>
//      height          (|x-center| == 0.0)
//      0               (|x-center| != 0.0)
// </srcblock>
// The parameters are enumerated by HEIGHT, CENTER and WIDTH. They have
// default values of (1, 0).
// </synopsis> 
//
// <example>
// <srcblock>
//    DiracDFunction<Double> sf(5.0, 25.0);
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

template<class T> class DiracDFunction : public DiracDParam<T>
{
public:
  //# Constructors
  // Constructs the DiracDFunction, Defaults:
  // height=1, center=0.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  DiracDFunction() : DiracDParam<T>() {}
  explicit DiracDFunction(const T &height) :
    DiracDParam<T>(height) {}
  DiracDFunction(const T &height, const T &center) :
    DiracDParam<T>(height, center) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  DiracDFunction(const DiracDFunction<T> &other) : DiracDParam<T>(other) {}
  template <class W>
  DiracDFunction(const DiracDFunction<W> &other) : DiracDParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  DiracDFunction<T> &operator=(const DiracDFunction<T> &other) {
    DiracDParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~DiracDFunction() {}

  //# Operators    
  // Evaluate the DiracD at <src>x</src>.
  // If a vector is used as the argument only its first element is used.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer. 
  // <group>
  virtual Function<T> *clone() const { return new DiracDFunction<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new DiracDFunction<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new DiracDFunction<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using DiracDParam<T>::param_p;
public:
  using DiracDParam<T>::nparameters;
  using DiracDParam<T>::HEIGHT;
  using DiracDParam<T>::CENTER;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/DiracDFunction.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
