//# Functionals.h: A module that represents various function-like classes.
//# Copyright (C) 1995,1996,1998,1999,2001
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


#if !defined (AIPS_FUNCTIONALS_H)
#define AIPS_FUNCTIONALS_H

//# Base classes
#include <aips/Functionals/Functional.h>
#include <aips/Functionals/Parameterized.h>
#include <aips/Functionals/Function1D.h>
#include <aips/Functionals/FunctionND.h>
#include <aips/Functionals/SampledFunctional.h>
#include <aips/Functionals/FuncWithDerivs.h>

//# 1-D Functions
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Functionals/Polynomial.h>
#include <aips/Functionals/Interpolate1D.h>
#include <aips/Functionals/ArraySampledFunctional.h>
#include <aips/Functionals/ScalarSampledFunctional.h>

//# ND Functions
#include <aips/Functionals/SumFunction.h>
#include <aips/Functionals/Gaussian2D.h>
#include <aips/Functionals/GaussianND.h>

// <module>
//
// <summary>A module that represents various function-like classes.</summary>

// <reviewed reviewer="tcornwel" date="1996/02/13" demos=""></reviewed>

// <etymology>
// The term ``Functional'' was chosen to roughly follow the usage in Barton and
// Nackman's ``Scientific and Engineering C++.'' Functional classes map a
// Domain object into a Range object, rather like a mathematical
// ``function''. They use <src>operator()</src>, so they look much like single
// argument C++ ``functions''.
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
// </srcblock>
// would be a legal Functional.
// 
// The <src>Functions</src> and their derived classes map, again using the
// <src>operator()</src>, numeric value(s) into a numeric value. Since they are
// numeric, the <src>Domain</src> and <src>Range</src> base type can be of type
// <src>AutoDiff<T></src> (where <src>T</src> is numeric base type), in which
// case the value and its derivatives will be calculated.
// 
// <note role=warning> The following description will be valid shortly.
// Immediate need should be fulfilled with the old description. </note>
//
// The basic classes are:
// <dl>
// <dt> <linkto class=Functional><src>Functional<Domain, Range></src></linkto>
// <dd>
// A base class that maps a <src>Domain</src> object into a <src>Range</src>
// object using the <src>operator()</src>. All information necessary to convert
// the <src>Domain</src> into a <src>Range</src> will be available in the class
// or in the input information. No variable class state (<em>parameters</em>)
// are available.
// <note role=warning>
// This behaviour is identical to the original behaviour.
// </note>
// 
// <dt> <linkto class=FunctionParam><src>FunctionParam<T></src></linkto>
// <dd> A base class that acts as a container for <em>parameters</em> to be used
// in <src>Function</src> classes. The class contains a list of parameters, and
// a list of flags associated with the parameters. methods to set and obtain the
// parameters and their flags are available. The flags can e.g. be used to
// indicate to <src>Fitting</src> routines if a certain parameter has to be
// updated or not.
// <note role=warning>
// The FunctionParam class is different from the original Parameterized class: it
// does not assume anything about the uses of the class, but leaves that to the
// final users. This means that a lot of copying between intermediate and final
// users is not necessary (like between a Gaussian fitter with fixed parameters
// and the Fitting routines: the Gaussian fitter just sets a flag to False, and
// let the Fitting worry about what to do internally. Access through an
// operator[] will be added.<br>
// Status: system changed to use new methods; still called Parameterized
// </note>
// 
// <dt> <linkto class=Function><src>Function<T></src></linkto>
// <dd> Base class for function objects with parameters.
// All parameters shoudl be of the same type <em>T</em> as the <src>
// Function</src>. <src>Function</src> objects are specifically geared
// towards use in the <linkto module=Fitting>Fitting</linkto> classes, but
// can be used anywhere where the value (or derivatives) of functions
// are needed.
//
// The <src>Function<T></src> class is derived from <src>Functional</src>
// and contains and is derived from <src>FunctionParam<T></src>.
// The parameters act as state for the function
// (e.g. a width for a Gaussian). A function object is called using the
// <src>operator()</src>, and will return the value of the function (and its
// derivatives if the template argument is <src>AutoDiff<T></src> rather than
// <src>T</src>). 
// 
// <note role=warning>
// Function will replace FunctionND. A Function1D could be derived if necessary
// (as could a Function2D etc).<br>
// </note>
// 
// <dt> Actual functional classes (e.g. <linkto
// class=Gaussian1D><src>Gaussian1D<T></src></linkto>) 
// <dd> An actual function object will be derived from the
// <src>Function<T></src>. The minimum functionality will be an
// <src>eval()</src> method to calculate the function value, using the provided
// arguments and parameters. In some cases it will be advantageous to have
// special parameter handling (e.g. using <em>flux</em> rather than amplitude of
// a Gaussian). It is strongly suggested to separate the parameter handling from
// the actual calculation into a <src>*Param</src> class (see e.g. <linkto
// class=Gaussian1DParam><src>Gaussian1DParam<T></src></linkto>), to minimize
// the amount of work necessary to make specialization implementations of the
// calculations for e.g. Complex or AutoDiff versions.<br>
// <note role=tip>
// A special implementation of the one dimensional Gaussian using the
// new classes is provided in the <linkto class=G1D>G1D</linkto> class.
// </note>
//   
// </dl>
//
// <note role=warning> The following is the old description </note>
// The fundamental Functional classes are as follows:

// <ul>
// <li>
//  <linkto class="Functional">Functional</linkto>:
//  This base class represents functions that map a <src>Domain</src> object 
//  into a <src>Range</src> object via <src>operator()</src> function.
// <li>
//   <linkto class="FunctionND">FunctionND</linkto>: 
//   This base class is used for N-dimensional functions, that is it maps a 
//   <linkto class="Vector">Vector<Domain></linkto> into a <src>Range</src> 
//   object via the operator() function.
// <li> 
//   <linkto class="Function1D">Function1D</linkto>: 
//   This class is much like <src>FunctionND</src>, however it is strictly for
//   one-dimensional functions.
// <li> 
//   <linkto class="SampledFunctional">SampledFunctional</linkto>: 
//   This is the base class for discrete, regularly sampled one-dimensional 
//   functions. It inherits from <src>Functional<uInt, Range></src>.
// <li> 
//   <linkto class="FuncWithDerivs">FuncWithDerivs</linkto>:
//   This is the base class for functions which can calculate their
//   derivatives
// <li> 
//   <linkto class="Parameterized">Parameterized</linkto>:
//   This base class provides the interface for adjustable parameters for
//    <src>Function1D</src> and <src>FunctionND</src>.
// </ul>
//
// Presently, the following one-dimensional classes are implemented:
// <ul>
// <li>
//   <linkto class="Polynomial">Polynomial<T></linkto>: 
//   A 1-Dimensional polynomial. The polynomial coefficients are its
//   adjustable parameters.
// <li> 
//   <linkto class="Gaussian1D">Gaussian1D<T></linkto>:
//   A 1-dimensional Gaussian. The height, center, and width are the
//   parameters of the Gaussian.
// <li> 
//   <linkto class="Interpolate1D">Interpolate1D<T></linkto>:
//   A 1-dimensional function that will interpolate using a variety of
//   algorithms between a supplied set of data points.
// <li> 
//   <linkto class="ScalarSampledFunctional">ScalarSampledFunctional</linkto>:
//   This is for discrete, regularly sampled one dimensional functions that
//   return a scalar value.
// <li> 
//   <linkto class="ArraySampledFunctional">ArraySampledFunctional</linkto>:
//   This is for discrete, regularly sampled one dimensional functions that
//   return an <linkto class="Array">Array</linkto> of values.
// </ul>
//
// Presently, the following multi-dimensional classes are implemented:
// <ul>
// <li>
//   <linkto class="SumFunction">SumFunction<T></linkto>: 
//   A function which is made up of the sum of other Functions.
// <li> 
//   <linkto class="GaussianND">GaussianND<T></linkto>:
//   A N-dimensional Gaussian. A statistical description of the parameters
//   in terms on variance and co-variance is used.
// <li> 
//   <linkto class="Gaussian2D">Gaussian2D<T></linkto>:
//   A 2-dimensional Gaussian. The parameters are the height (or alternatively
//   the integrated flux), the major axis width, minor axis width, position
//   angle and position (x,y) of the centre.
// </ul>
// </synopsis>

// <example>
// In old system:<br>
// A function to find a bracketed root by bisection could be written as follows:
// <srcblock>
//    template<class Domain, class Range> 
//      Domain findRoot(const Functional<Domain,Range> &func, Domain left, 
//                      Domain right, Domain tol)
//      {
//          Range fr = func(right);
//          Range fl = func(left);
//          Range sign = fr > 0 ? 1 : -1 ;
//          AlwaysAssertExit(fl*fr < 0.0 && right > left);
//          while (right - left > tol) {
//              Domain mid = (left + right) / 2;
//              Range fmid = func(mid);
//              if (sign*fmid > 0.0)
//                  right = mid;
//              else
//                  left = mid;
//          }
//          return (left + right)/2;
//      }
// </srcblock>
// Since Function1D is derived from Functional, the
// above function will also work with classes derived from Function1D. To
// behave sensibly, the Domain and Range types should be real, <e>i.e.</e>,
// Float or Double.  (This example should likely be made a real function in the
// system!)
//
// In new system see <linkto class=Function>Function</linkto> class for an
// extensive example.
// </example>

// <motivation>
// The immediate motivations for this module were:
// <ol>
//    <li> To represent functions which are used in linear and non-linear least
//         squares fitting (1-Dimensional).
//    <li> For functions which model the sky brightness, and the Fourier
//    transform of the sky brightness. (Not currently (early 1996) in use).
// </ol>
// </motivation>

// <todo asof="1995/08/30">
//   <li> REPLACE THE MATHFUNC CLASSES!!
//   <li> It would be very convenient to have a letter/envelope class, and to 
//        define ``function arithmetic.''
// </todo>

// </module>

#endif

