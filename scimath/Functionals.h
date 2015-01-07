//# Functionals.h: A module that represents various function-like classes.
//# Copyright (C) 1995,1996,1998,1999,2001,2002
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


#ifndef SCIMATH_FUNCTIONALS_H
#define SCIMATH_FUNCTIONALS_H

//# Base classes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicMath/Functional.h>
#include <casacore/scimath/Functionals/FunctionTraits.h>
#include <casacore/scimath/Functionals/FunctionParam.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Functionals/Function1D.h>

//# Combination methods
#include <casacore/scimath/Functionals/FunctionWrapper.h>
#include <casacore/scimath/Functionals/CombiFunction.h>
#include <casacore/scimath/Functionals/CompoundFunction.h>

//# remainder will be removed
#include <casacore/scimath/Functionals/SampledFunctional.h>

//# 1-D Functions
#include <casacore/scimath/Functionals/Interpolate1D.h>
#include <casacore/scimath/Functionals/ArraySampledFunctional.h>
#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>A module that represents various function-like classes.</summary>

// <reviewed reviewer="tcornwel" date="1996/02/13" demos=""></reviewed>

// <etymology>
// The term <src>Functional</src> was chosen to roughly follow the usage in
// Barton and Nackman's <em>Scientific and Engineering C++</em>.
// Functional classes map a Domain object into a Range object, rather like a
// mathematical <src>function</src>. They use <src>operator()</src>,
// so they look much like single argument C++ <src>functions</src>.
// </etymology>
//
// <synopsis>
// <src>Functionals</src> and their derived classes map an input
// <src>Domain</src> object into an output <src>Range</src> object using the 
// <src>operator()</src>.
// Often the input and output types are numeric, but it can be of any type. 
// <srcblock>
// class Offspring : public Functional<List<Parents>, List<Children> > {
//  public:
//    List<Children> operator()(List<Parents>);
// };
// </srcblock>
// would be a legal Functional.
// 
// The <src>Functions</src> and their derived classes map, again using the
// <src>operator()</src>, numeric value(s) into a numeric value. Since they are
// numeric, the <src>Domain</src> and <src>Range</src> base type can be of type
// <src>AutoDiff<T></src> (where <src>T</src> is numeric base type) or one
// of its derivations, in which case the value and its derivatives will be
// calculated.
// 
// <note role=warning> In the current version the <src>Domain</src> and 
// <src>Range</src> are the same for Functions </note>
//
// The basic classes are:
// <dl>
// <dt> <linkto class=Functional><src>Functional<Domain, Range></src></linkto>
// <dd>
// A base class that maps a <src>Domain</src> object into a <src>Range</src>
// object using the <src>Range operator(const Domain &)</src>. All
// information necessary to convert the <src>Domain</src> into a
// <src>Range</src> will be available in the class
// or in the input information. No variable class state (<em>parameters</em>)
// are available.
// 
// <dt> <linkto class=FunctionParam><src>FunctionParam<T></src></linkto>
// <dd> A helper base class that acts as a container for <em>parameters</em>
// (<em>state</em>) used in <src>Function</src> classes. The class contains
// a list of parameters, and a list of flags associated with the parameters.
// Methods to set and obtain the parameters (using <src>operator[]</src>)
// and their flags (using methods <src>mask()</src>) are available. The flags
// can e.g. be used to indicate to <src>Fitting</src> routines if a certain
// parameter has to be updated ('fitted') or not.
// <note role=tip>
// The FunctionParam class does not assume anything about the uses of the
// class, but leaves that to the final users. This means that a lot of
// copying between intermediate and final users is not necessary
// (like between a Gaussian fitter with fixed parameters
// and the Fitting routines: the Gaussian fitter just sets a flag to False, and
// let the Fitting worry about what to do internally).
// </note>
// 
// <dt> <linkto class=Function><src>Function<T></src></linkto>
// <dd> Base class for function objects with zero or more parameters (i.e.
// Functionals with state).
// All parameters should be of the same type <em>T</em> as the <src>
// Function<T></src>. <src>Function</src> objects are specifically geared
// towards use in the <linkto module=Fitting>Fitting</linkto> classes, but
// can be used anywhere where the value (and/or derivatives) of functions
// are needed.
//
// The <src>Function<T></src> class is derived from <src>Functional</src>
// and contains a <src>FunctionParam<T></src> object.
// The parameters act as state for the function
// (e.g. a width for a Gaussian). A function object is called using the
// <src>T operator(const T&)</src> (<em>ndim=1</em>), or the
// <src>T operator(const Vector<T>&)</src> (all values of <em>ndim</em>), or
// <src>T operator(const T&, const T&)</src> (for <em>ndim=2</em> only).
// If the template argument is <src>AutoDiff<T></src>, the parameters and the
// returned value will be <src>AutoDiff<T></src>; the arguments of the
// <src>operator()</src> will be of type <src>T</src>. The returned value
// of the function will be the function value at <em>x</em> (and the
// derivatives w.r.t. the non-masked parameters) Using <src>AutoDiffA<T></src>
// the derivatives can be calculated w.r.t. parameters and/or arguments, see
// <linkto class=AutoDiff>AutoDiff</linkto> and <linkto class=FunctionTraits>
// FunctionTraits</linkto> for details. 
// 
// <note role=tip>
// A <src>Function1D</src> is provided for 1-dimensional function objects
// </note>
// </dl>
// 
//  Actual functional classes:
// <dl>
// <dt> e.g. <linkto
// class=Gaussian1D><src>Gaussian1D<T></src></linkto>
// <dd> An actual function object will be derived from 
// <src>Function<T></src>. The minimum functionality of a Function
// object will be support for the <src>operator()</src> methods (through a
// single, hidden, <src>eval()</src> method); for the manipulation of the
// associated parameters (using <src>operator[index]</src> and
// <src>mask(index)</src>) and some administrative aids (<src>ndim()</src>,
// <src>nparameters()</src> and the like.
//
// In most cases it is advantageous to have a special parameter handling
// class (e.g. <src>Gaussian1DParam</src>), to separate the (template
// independent) parameter handling from the possible specialization of
// the <src>eval()</src> method, and to more easily incorporate
// special parameter handling (e.g. using <em>flux</em> rather than amplitude
// of a Gaussian). All of this is transparent to the end-user.
// </dl>
// Combinatory Function objects are provided to easily combine and create
// function objects:
// <dl> 
// <dt> <linkto class=CompoundFunction>CompoundFunction</linkto>
// <dd> creates
// a new, compound, function object from one or more other function objects
// (including compounds...). The new function will have the sum of the 
// parameters of the input functions as the new parameters (i.e the compound
// function created from a 1-dimensional Gaussian (with 3 parameters) and a 
// third-order polynomial (with 4 parameters) will have 7 parameters).
// <dt> <linkto class=CombiFunction>CombiFunction</linkto> 
// <dd> creates
// a (linear) combination of a number of input functions. The number of
// parameters of the newly created function will be equal to the number of
// input functions (i.e. the combi 
// function created from a 1-dimensional Gaussian (with 3 parameters) and a 
// third-order polynomial (with 4 parameters) will have 2 parameters). The
// function will be <src>param0*gauss(x) + param1*poly(x)</src>
// <dt> <linkto class=FunctionWrapper>FunctionWrapper</linkto>
// <dd> will take
// a global function (or by the use of the <em>STL</em> function adapters
// <src>mem_fun*</src> also member functions) of any dimension, and with
// any number of parameters. The function is assumed to be called as
// <src>f(x, p)</src>, and is wrapped like 
// <src>FunctionWrapper(&func, param&, ndim)</src> (see example). 
//   
// </dl>
//
// </synopsis>

// <example>
// A function to find a bracketed root by bisection could be written
// as follows:
// <srcblock>
//    template <class Domain, class Range> 
//      Domain findRoot(const Functional<Domain,Range> &func, Domain left, 
//                      Domain right, Domain tol) {
//          Range fr = func(right);
//          Range fl = func(left);
//          Range sign = fr > 0 ? 1 : -1 ;
//          AlwaysAssertExit(fl*fr < 0.0 && right > left);
//          while (right - left > tol) {
//              Domain mid = (left + right) / 2;
//              Range fmid = func(mid);
//              if (sign*fmid > 0.0) right = mid;
//              else left = mid;
//          };
//          return (left + right)/2;
//      }
// </srcblock>
// Since Function1D is derived from Functional, the
// above function will also work with classes derived from Function1D. To
// behave sensibly, the Domain and Range types should be real, <em>i.e.</em>,
// Float or Double.
//
// To calculate the value of a polynomial
// <srcblock>2 + 4x<sup>2</sup> + 6x<sup>4</sup></srcblock>
// at <src>x=5.1</src>:
// <srcblock>
//      Polynomial<Double> pol(4);
//	pol[0] = 2; pol[2] = 4; pol[4] = 6;
//	cout << "Polynomial value at 5.1: " << pol(5.1) << endl;
// </srcblock>
//
// Create a simple function (1-dimensional) with 2 parameters (A and B):
// <srcblock>
//	Double myf(const Double x, const Vector<Double> p) {
//	  return p[0]*sin(p[1]*x); }
// </srcblock>
// make it into a function object for initial parameters 2 and pi:
// <srcblock>
//	Vector<Double> p(2);
//	p[0] = 2; p[1] = C::pi;
//	FunctionWrapper<Double> f0(myf, p, 2);
// </srcblock>
// Make the first parameter 3:
// <srcblock>
//	f0[0] = 3;
// </srcblock>
// (for the global function you have to change <src>p[0]</src>).
// Calculate the value of the function:
// <srcblock>
//	cout << "The value " << f0(3) << " should be 1.5 times the value " <<
//	  myf(3) << endl;
// </srcblock>
// A function object could be created as:
// <srcblock>
//	template<class T> class objf : public Function<T> {
//	 public:
//	   objf() : Function<T>(2) {};		// 2 parameters
//	   objf(const objf<T> &other) : Function<T>(other) {};
//	   virtual ~objf() {};
//	   // The actual method called for the evaluation operator():
//	   virtual T eval(typename Function<T>::FunctionArg x) const {
//	     return param_p[0] * sin(param_p[1] * x[0]); };
//	   // Return a copy of function (used for combination e.g.)
//	   virtual Function<T> *clone() const {
//	     return new objf<T>(*this); };
//     };
// </srcblock>
// Which can be called as:
// <srcblock>
//	objf<Double> f1;
//	f1[0] = 2; f1[1] = C::pi;
//	cout << "The value " << myf(3) << " should be equal to the value " <<
//	  f1(3) << endl;
// </srcblock>
// </example>

// <motivation>
// The immediate motivations for this module were:
// <ol>
//    <li> To represent functions which are used in linear and non-linear least
//         squares fitting
// </ol>
// </motivation>

// <todo asof="2001/12/30">
//   <li> It could be convenient to have a letter/envelope class, and to 
//        define ``function arithmetic.''
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif

