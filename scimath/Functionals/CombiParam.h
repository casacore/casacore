//# CombiParam.h: Parameters for a linear combination of Functions
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

#ifndef SCIMATH_COMBIPARAM_H
#define SCIMATH_COMBIPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/scimath/Functionals/Function.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>
// Parameters for a linear combination of function objects.
// </summary>
//
// <use visibility=local>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tCombiFunction" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="CombiFunction">CombiFunction</linkto> class
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
// CombiParam<Double> combination;
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
// <li> AipsError if dimensions of functions added different
// </thrown>

// <motivation>
// This class was created to allow specialization of the evaluation in
// a simple way.
// </motivation>
//
// <todo asof="2001/10/22">
// <li> Nothing I know of
// </todo>

template <class T> class CombiParam : public Function<T>
{
public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  CombiParam();
  // Make this object a (deep) copy of other.
  // <group>
  CombiParam(const CombiParam<T> &other);
  CombiParam(const CombiParam<T> &other, Bool) :
    Function<T>(other), ndim_p(other.ndim_p),
    functionPtr_p(other.functionPtr_p.nelements()) { 
    for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
      functionPtr_p[i] = (*(other.functionPtr_p[i])).clone();
    }
  }
  template <class W>
    CombiParam(const CombiParam<W> &other) :
    Function<T>(other), ndim_p(other.ndim()),
    functionPtr_p(other.nFunctions()) { 
    for (uInt i=0; i<nFunctions(); ++i) {
      functionPtr_p[i] = other.function(i).cloneAD();
    }
  }
  template <class W>
    CombiParam(const CombiParam<W> &other, Bool) :
    Function<T>(other), ndim_p(other.ndim()),
    functionPtr_p(other.nFunctions()) { 
    for (uInt i=0; i<nFunctions(); ++i) {
      functionPtr_p[i] = other.function(i).cloneNonAD();
    }
  }
  // </group>
  // Make this object a (deep) copy of other.
  CombiParam<T> &operator=(const CombiParam<T> &other);
  // Destructor
  virtual ~CombiParam();

  //# Operators
  
  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("combi");
    return x; }

  // Add a function.  All functions must have the same <src>ndim()</src>
  // as the first one.  Returns the (zero relative) number (<src>i</src>)
  // of the function just added.
  // The default initial parameter value (<src>a(i)</src>) is
  // initialized to 1. The parameter mask is set <src>True</src>.
  uInt addFunction(const Function<T> &newFunction);

  // Return the total number of functions.  The number is equal to the
  // number of functions that have been added.  
  uInt nFunctions() const { return nparameters(); }

  // Return a reference to a specific Function in the combination.
  // <group>
  const Function<T> &function(uInt which) const {
    DebugAssert(nFunctions() > which, AipsError);
    return *(functionPtr_p[which]); }
  const Function<T> &function(uInt which) {
    DebugAssert(nFunctions() > which, AipsError);
    return *(functionPtr_p[which]); }
  // </group>

  // Returns the dimension of functions in the linear combination
  virtual uInt ndim() const { return ndim_p; }

protected:
  //# Data
  // Number of dimensions of underlying functions
  uInt ndim_p;
  
  // Pointer to each added function
  PtrBlock<Function<T> *> functionPtr_p;

  //# Make members of parent classes known.
public:
  using Function<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/CombiParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
