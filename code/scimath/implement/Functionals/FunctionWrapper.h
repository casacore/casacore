//# NQFunctionWrapper.h: Construct function objects from C++ functions 
//# Copyright (C) 2001
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

#if !defined(AIPS_NQFUNCTIONWRAPPER_H)
#define AIPS_NQFUNCTIONWRAPPER_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/NQWrapperParam.h>

//# Forward declarations
template <class T> class Vector;
template <class T> class NQWrapperBase;

// <summary> Construct nD function objects from C++ functions 
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="1996/02/22" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> class
//   <li> <linkto class="FunctionParam">FunctionParam</linkto>
// </prerequisite>
//
// <synopsis>
// This class is provided so that user can quickly construct a function
// object from a C++ function pointer without having to write a function
// class. The constructor constructs a function object from a function 
// pointer, and an optional parameter list.
// Parameters are necessary if
// the function has to be used in a functional fitting process (see
// <linkto class=LQGenericL2Fit>LQGenericL2Fit</linkto>).
//
// The general function signature is <src>f(x;p)</src>, where <src>x</src>
// represents the <em>arguments</em>, and <src>p</src> the parameters.
// The allowed signatures of the function include all combinations of
// arguments and parameters, and are:
// <ul>
//  <li> <src>f()</src>  no arguments e.g. random number or constant
//  <li> <src>f(x)</src>  1-dimensional, e.g. <src>sin(x)</src>
//  <li> <src>f(Vectorx)</src>  n-dimensional, e.g. <src>sin(x+2y)</src>
// </ul>
//
// </synopsis>
//
// <example>
// <srcblock>
// Float func(const Vector<Float>& x) {return x(0)*x(1);}        // x*y
// // Convert C++ functions to Functionals
// NQFunctionWrapper<Float> Func(func,2);
// </srcblock>
//

template <class T>
class NQFunctionWrapper : public NQWrapperParam<T> {
 public:
  //# Constructors
  // Default constructor, to enable arrays
  NQFunctionWrapper();
  // A function with no parameters and no arguments.
  NQFunctionWrapper(T(*f)());
  // A function with parameter and no arguments
  // (Note value of isPar irrelevant)
  NQFunctionWrapper(T(*f)( const T&), const Bool isPar);
  // A function with parameters and no arguments.
  // (Note value of isPar irrelevant)
  NQFunctionWrapper(T(*f)(const Vector<T>&), const Bool isPar);
  // Construct a  1-dimensional function with no parameters.
  NQFunctionWrapper(T(*f)(const T&));
  // Construct a  1-dimensional function with parameter.
  NQFunctionWrapper(T(*f)(const T&, const T&), const T &par);
  // Construct a  1-dimensional function with parameters.
  NQFunctionWrapper(T(*f)(const T&, const Vector<T>&),
		    const Vector<T> &par);
  // Construct an n-dimensional  function with no parameters.
  NQFunctionWrapper(T(*f)(const Vector<T>&), const Int dim=1);
  // Construct an n-dimensional  function with parameter.
  NQFunctionWrapper(T(*f)(const Vector<T>&, const T&),
		    const T &par, const uInt dim=1);
  // Construct an n-dimensional  function with parameters.
  NQFunctionWrapper(T(*f)(const Vector<T>&, const Vector<T>&),
		    const Vector<T> &par, const uInt dim=1);
  // Copy constructor (deep copy)
  NQFunctionWrapper(const NQFunctionWrapper<T> &other);
  // Copy assignment (deep copy)
  NQFunctionWrapper<T> &operator=(const NQFunctionWrapper<T> &other);

  // Destructor
  virtual ~NQFunctionWrapper() {};

  //# Operators    
  // Evaluate the function at <src>x</src>.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>

  //# Member functions
  // Get the dimensionality
  virtual uInt ndim() const { return (doit_p ? doit_p->ndim() : 0); };
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<T> *clone() const {
    return new NQFunctionWrapper<T>(*this); };
  // </group>

 protected:
  //# Data
  // The function aid object
  NQWrapperBase<T> *doit_p;
};

#endif
