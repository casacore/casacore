//# NQCombiFunction.h: Form a linear combination of Functions
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
//#
//# $Id$

#if !defined(AIPS_NQCOMBIFUNCTION_H)
#define AIPS_NQCOMBIFUNCTION_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/NQCombiParam.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffMath.h>

//# Forward declarations

// <summary>
// Form a linear combination of function objects.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tNQCombiFunction" demos="">
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
// <linkto class=LQLinearFit>LQLinearFit</linkto>) these parameters canm be
// solved for. In all aspects they behave as
// <linkto class=FunctionParam>FunctionParam</linkto> values.
//
// Member functions are added with the <src>addFunction()</src> method.
//
// <note role=tip>
// Check <linkto class=NQCompoundFunction>NQCompoundFunction</linkto> class
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
// NQCombiFunction<Double> combination;
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

template <class T> class NQCombiFunction : public NQCombiParam<T> {
 public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  NQCombiFunction() : NQCombiParam<T>() {};
  // Make this object a (deep) copy of other.
  NQCombiFunction(const NQCombiFunction<T> &other) :
    NQCombiParam<T>(other) {};
  // Make this object a (deep) copy of other.
  NQCombiFunction<T> &operator=(const NQCombiFunction<T> &other) {
    NQCombiParam<T>::operator=(other); return *this; };

  // Destructor
  virtual ~NQCombiFunction() {};

  //# Operators
  // Evaluate the function at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  virtual Function<T> *clone() const { return new NQCombiFunction<T>(*this); };

private:

};

#define NQCombiFunction_PS NQCombiFunction

// <summary> Partial specialization of NQCombiFunction for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>NQPolynomial_PS</src> is only for cxx2html
// documentation problems. Use <src>NQPolynomial</src> in your code.</note>
// </synopsis>

template <class T> class NQCombiFunction_PS<AutoDiff<T> > :
public NQCombiParam<AutoDiff<T> > {
 public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  NQCombiFunction_PS() : NQCombiParam<AutoDiff<T> >() {};
  // Make this object a (deep) copy of other.
  NQCombiFunction_PS(const NQCombiFunction_PS<AutoDiff<T> > &other) :
    NQCombiParam<AutoDiff<T> >(other) {};
  // Make this object a (deep) copy of other.
  NQCombiFunction_PS<AutoDiff<T> > &
    operator=(const NQCombiFunction_PS<AutoDiff<T> > &other) {
    NQCombiParam<AutoDiff<T> >::operator=(other); return *this; };

  // Destructor
  virtual ~NQCombiFunction_PS() {};

  //# Operators
  // Evaluate the function and its derivatives at <src>x</src> <em>wrt</em>
  // to the coefficients.
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  virtual Function<AutoDiff<T> > *clone() const {
    return new NQCombiFunction_PS<AutoDiff<T> >(*this); };

};

#undef NQCombiFunction_PS

#endif
