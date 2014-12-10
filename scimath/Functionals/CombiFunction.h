//# CombiFunction.h: Form a linear combination of Functions
//# Copyright (C) 2001,2002,2005
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
//#
//# $Id$

#ifndef SCIMATH_COMBIFUNCTION_H
#define SCIMATH_COMBIFUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/CombiParam.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>
// Form a linear combination of function objects.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tCombiFunction" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>
//
// <synopsis>
// Given N function objects, the class describes a linear combination of the 
// form:
// <srcblock>
// f(x) = a(0)*f(0)(x) + a(1)*f(1)(x) + ... + a(N-1)*f(N-1)(x)
// </srcblock>
// where a = {a(n)} are parameters. If the combi function is used in
// a functional fitting process (see
// <linkto class=LinearFit>LinearFit</linkto>) these parameters canm be
// solved for. In all aspects they behave as
// <linkto class=FunctionParam>FunctionParam</linkto> values.
//
// Member functions are added with the <src>addFunction()</src> method.
//
// <note role=tip>
// Check <linkto class=CompoundFunction>CompoundFunction</linkto> class
// for a combination of functions behaving as one object. </note>
// </synopsis>
//
// <example>
// In the following example a second order polynomial is built from 3 separate
// polynomials.
// <srcblock>
// Polynomial<Double> constant(0); 
// Polynomial<Double> linear(1); 
// Polynomial<Double> square(2);
// 
// constant.setCoefficient(0, 1.0);   // 1
// linear.setCoefficient(1, 1.0);     // x
// square[2] = 1.0;     // x^2
// 
// CombiFunction<Double> combination;
// 
// // form function, e0 + e1*x + e2*x^2
// combination.addFunction(constant);
// combination.addFunction(linear);
// combination.addFunction(square);
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
// <li> AipsError in debug mode if incorrect function index
// </thrown>
//
// <motivation>
// This class was created to allow specialization of the evaluation in
// a simple way.
// </motivation>
//
// <todo asof="2001/10/22">
// <li> Nothing I know of
// </todo>

template <class T> class CombiFunction : public CombiParam<T> {
 public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  CombiFunction() : CombiParam<T>() {}
  // Make this object a (deep) copy of other.
  // <group>
  CombiFunction(const CombiFunction<T> &other) :
    CombiParam<T>(other) {}
  CombiFunction(const CombiFunction<T> &other, Bool) :
    CombiParam<T>(other, True) {}
  template <class W>
    CombiFunction(const CombiFunction<W> &other) : CombiParam<T>(other) {}
  template <class W>
    CombiFunction(const CombiFunction<W> &other, Bool) :
    CombiParam<T>(other, True) {}
  // </group>
  // Make this object a (deep) copy of other.
  CombiFunction<T> &operator=(const CombiFunction<T> &other) {
    CombiParam<T>::operator=(other); return *this; }

  // Destructor
  virtual ~CombiFunction() {}

  //# Operators
  // Evaluate the function at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new CombiFunction<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new CombiFunction<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new CombiFunction<typename FunctionTraits<T>::BaseType>
      (*this, True); }
  // </group>

  //# Make members of parent classes known.
public:
  using CombiParam<T>::nparameters;
};

#define CombiFunction_PS CombiFunction

// <summary> Partial specialization of CombiFunction for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>CombiFunction_PS</src> is only for cxx2html
// documentation problems. Use <src>CombiFunction</src> in your code.</note>
// </synopsis>

template <class T> class CombiFunction_PS<AutoDiff<T> > :
public CombiParam<AutoDiff<T> > {
 public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  CombiFunction_PS() : CombiParam<AutoDiff<T> >() {}
  // Make this object a (deep) copy of other.
  // <group>
  CombiFunction_PS(const CombiFunction_PS<AutoDiff<T> > &other) :
    CombiParam<AutoDiff<T> >(other) {}
  template <class W>
    CombiFunction_PS(const CombiFunction_PS<W> &other) :
    CombiParam<AutoDiff<T> >(other) {}
  // </group>
  // Make this object a (deep) copy of other.
  CombiFunction_PS<AutoDiff<T> > &
    operator=(const CombiFunction_PS<AutoDiff<T> > &other) {
    CombiParam<AutoDiff<T> >::operator=(other); return *this; }

  // Destructor
  virtual ~CombiFunction_PS() {}

  //# Operators
  // Evaluate the function and its derivatives at <src>x</src> <em>wrt</em>
  // to the coefficients.
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new CombiFunction_PS<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new CombiFunction<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new CombiFunction<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this, True); }
  // </group>

  //# Make members of parent classes known.
public:
  using CombiParam<AutoDiff<T> >::nparameters;
};

#undef CombiFunction_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/CombiFunction.tcc>
#include <casacore/scimath/Functionals/Combi2Function.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
