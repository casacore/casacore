//# NQCompoundFunction.h: Sum of a collection of Functions
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

#if !defined(AIPS_NQCOMPOUNDFUNCTION_H)
#define AIPS_NQCOMPOUNDFUNCTION_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/NQCompoundParam.h>
#include <aips/Functionals/Function.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffMath.h>

//# Forward declarations

// <summary>
// Sum of a collection of Functions which behaves as one Function object.
// </summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tNQCompoundFunction" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>
//
// <synopsis>
// This class takes an arbitrary number of Function objects, and generates
// a new, single function object. The parameters of the compound object
// are the union of all the parameters in the input objects.
//
// When NQCompoundFunction is evaluated, the result is the sum of 
// all the individual function values.
//
// Member functions are added with the <src>addFunction()</src> method.
//
// <note role=tip>
// Check <linkto class=NQCompoundFunction>NQCombiFunction</linkto> class
// for a simple linear combination of function objects </note>
// </synopsis>
//
// <example>
// Suppose for some reason we wanted the sum of <src>x^2</src> plus a gaussian.
// We could form it as follows:
// <srcblock>
//    NQPolynomial<Float> x2(2);
//    x[2] = 1.0; 					 // x^2
//    NQGaussian1D<Float> gauss(1.0, 0.0, 1.0);          // e^{-x^2}
//    NQCompoundParam<Float> sum;                        // sum == 0.0
//    sum.addFunction(x2);                               // sum == x^2
//    sum.addFunction(gauss);                            // sum == x^2+e^{-x^2}
//    sum(2.0);                                          // == 4 + e^-4
//    NQCompoundParam[0] = 2.0;                          // sum ==2x^2+e^{-x^2}
//    sum(2.0);                                          // == 8 + e^-4
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
// <li> AipsError if dimensions of functions added different
// </thrown>

// <motivation>
// This class was created to allow a non-linear least squares fitter to fit a 
// (potentially) arbitrary number of functions (typically Gaussians).
// </motivation>
//
// <todo asof="2001/10/22">
//   <li> Nothing I know of
// </todo>

template <class T> class NQCompoundFunction : public NQCompoundParam<T> {
 public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  NQCompoundFunction() : NQCompoundParam<T>() {};
  // Make this object a (deep) copy of other.
  NQCompoundFunction(const NQCompoundFunction<T> &other) :
    NQCompoundParam<T>(other) {};
  // Make this object a (deep) copy of other.
  NQCompoundFunction<T> &operator=(const NQCompoundFunction<T> &other) {
    NQCompoundParam<T>::operator=(other); return *this; };
  
  // Destructor
  virtual ~NQCompoundFunction() {};
  
  //# Operators
  // Evaluate the function at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  virtual Function<T> *clone() const { return new NQCompoundFunction<T>(*this); };
  
private:
  //# Member functions
  // Copy the local parameters to/from general block
  // <group>
  void toParam_p() const;
  void fromParam_p() const;
  // </group>

};

#define NQCompoundFunction_PS NQCompoundFunction

// <summary> Partial <src>AutoDiff</src> specialization of NQCompoundFunction
// </summary>

// <synopsis>
// <note role=warning> The name <src>NQCompoundFunction_PS</src> is only
// for cxx2html documentation problems. Use
// <src>NQCompoundFunction</src> in your code.</note>
// </synopsis>

template <class T> class NQCompoundFunction_PS<AutoDiff<T> > :
public NQCompoundParam<AutoDiff<T> > {
 public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  NQCompoundFunction_PS() : NQCompoundParam<AutoDiff<T> >() {};
  // Make this object a (deep) copy of other.
  NQCompoundFunction_PS(const NQCompoundFunction_PS<AutoDiff<T> > &other) :
    NQCompoundParam<AutoDiff<T> >(other) {};
  // Make this object a (deep) copy of other.
  NQCompoundFunction_PS<AutoDiff<T> > &
    operator=(const NQCompoundFunction_PS<AutoDiff<T> > &other) {
    NQCompoundParam<AutoDiff<T> >::operator=(other); return *this; };

  // Destructor
  virtual ~NQCompoundFunction_PS() {};

  //# Operators
  // Evaluate the function and its derivatives at <src>x</src> <em>wrt</em>
  // to the coefficients.
  virtual AutoDiff<T>
    eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  virtual Function<AutoDiff<T> > *clone() const {
    return new NQCompoundFunction<AutoDiff<T> >(*this); };

 private:
  //# Member functions
  // Copy the local parameters to/from general block
  // <group>
  void toParam_p() const;
  void fromParam_p() const;
  // </group>

};

#undef NQCompoundFunction_PS

#endif
