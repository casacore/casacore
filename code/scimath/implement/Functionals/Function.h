//# Function.h: Numerical functional interface class
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

#if !defined(AIPS_FUNCTION__H)
#define AIPS_FUNCTION__H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/Functional.h>
#include <trial/Functionals/FunctionParam.h>

//# Forward declarations
template <class T> class Vector;

// <summary> Numerical functional interface class
// </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Functional">Functional</linkto>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto>
// </prerequisite>
//
// <synopsis>
// A <src>Function</src> is used for classes which map an n-dimensional a
// scalar or n-dimensional Vector of type <src>T</src> into a <src>T>/src>.
// The object also has zero or more parameters which can be masked
// if necessary, and be used in the <src>Fitting</src> module, and, implicitly,
// in the <linkto class=AutoDiff>AutoDiff</linkto> differentiation module.
//
// The parameter interface is provided by the
// <linkto class="FunctionParam"><src>FunctionParam</src></linkto> class.
//
// The function calls implemented are:
// <ul>
// <li> <src>operator()()</src>
// <li> <src>operator()(const T &x)</src>
// <li> <src>operator()(const Vector<T> &x)</src>
// <li> <src>operator()(Function::FunctionArg x)</src>
// </ul>
// These calls are (in debug mode) tested for correct number of arguments,
// after which they call a <src>T eval(FunctionArg x) const = 0</src> to
// be implemented in derived classes. The derived class should also implement 
// an <src>uInt ndim() const</src>.
//
// </synopsis>
//
// <motivation>
// A function of more than one variable was required for a function which
// represents the sky brightness. Adjustable parameters were required for
// non-linear least squares fitting.
// </motivation>
//
// <templating arg=T>
//    <li> Besides the requirements set by the
//    <linkto class="Functional">Functional</linkto> base class, it must be
//    possible to form a <src>Vector<T></src>.
// </templating>
//
// <todo asof="2001/08/29">
//   <li> Should the clone() functions return a reference-counted pointer
//   instead of a raw pointer?
//   <li> At some point, we may want to implement a letter-envelope class,
//   implement function arithmetic, etc.
// </todo>

template<class T> class Function : public Functional<T, T>,
  public Functional<Vector<T>, T>,
  public FunctionParam<T> {

 public:
  //# Typedefs
  typedef const T* FunctionArg;

  //# Constructors
  // Constructors for FunctionParam
  // <group>
  Function() : FunctionParam<T>() {};
  explicit Function(const uInt n) : FunctionParam<T>(n) {};
  explicit Function(const Vector<T> &in) : FunctionParam<T>(in) {};
  Function(const FunctionParam<T> &other) : FunctionParam<T>(other) {};
  // </group>

  // Destructor
  virtual ~Function();
  
  // Returns the number of dimensions of function
  virtual uInt ndim() const = 0;

  //# Operators
  // Evaluate this function object at <src>x</src>. The length of <src>
  // x</src> must be greater than or equal to as <src>ndim()</src>.
  // <group>
  virtual T operator()(const Vector<T> &x) const;
  virtual T operator()(const T &x) const;
  virtual T operator()() const;
  virtual T operator()(FunctionArg x) const { return this->eval(x); };
  // </group>
  // Evaluate the function object
  virtual T eval(FunctionArg x) const = 0;

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting the returned pointer.
  virtual Function<T> *clone() const = 0;
};

#endif
