//# CompoundParam.h: Parameters for sum of parameterized Functions
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

#ifndef SCIMATH_COMPOUNDPARAM_H
#define SCIMATH_COMPOUNDPARAM_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Parameters for sum of parameterized Functions
// </summary>

// <use visibility=local>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tCompoundFunction" 
// demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>
//
// <synopsis>
// This class takes an arbitrary number of Function objects, and generates
// a new, single function object. The parameters of the compound object
// are the union of all the parameters in the input objects.
//
// When CompoundFunction is evaluated, the result is the sum of 
// all the individual function values.
//
// Note that any Function object (including another Compound object) can be
// part of a compound object. 
// </synopsis>
//
// <example>
// Suppose for some reason we wanted the sum of <src>x^2</src> plus a gaussian.
// We could form it as follows:
// <srcblock>
//    Polynomial<Float> x2(2);
//    x[2] = 1.0; 					 // x^2
//    Gaussian1D<Float> gauss(1.0, 0.0, 1.0);          // e^{-x^2}
//    CompoundParam<Float> sum;                        // sum == 0.0
//    sum.addFunction(x2);                               // sum == x^2
//    sum.addFunction(gauss);                            // sum == x^2+e^{-x^2}
//    sum(2.0);                                          // == 4 + e^-4
//    CompoundParam[0] = 2.0;                          // sum ==2x^2+e^{-x^2}
//    sum(2.0);                                          // == 8 + e^-4
//    // Set the height of the gaussian
//    sum[parameterOffset[1] + Gaussian1D<Float>::HEIGHT] = 2.5;
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
// (potentially) arbitrary number of functions (typically gaussians).
// </motivation>
//
// <todo asof="2001/10/22">
//   <li> Nothing I know of
// </todo>

template<class T> class CompoundParam : public Function<T>
{
public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  CompoundParam();
  // Make this object a (deep) copy of other.
  // <group>
  CompoundParam(const CompoundParam<T> &other);
  CompoundParam(const CompoundParam<T> &other, Bool) :
    Function<T>(other), ndim_p(other.ndim_p),
    functionPtr_p(other.functionPtr_p.nelements()),
    paroff_p(other.paroff_p.nelements()),
    funpar_p(other.funpar_p.nelements()), 
    locpar_p(other.locpar_p.nelements()) { 
    for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
      functionPtr_p[i] = other.functionPtr_p[i]->clone();
      paroff_p[i] = other.paroff_p[i];
    }
    for (uInt i=0; i<funpar_p.nelements(); ++i) {
      funpar_p[i] = other.funpar_p[i];
      locpar_p[i] = other.locpar_p[i];
    }
  }
  template <class W>
    CompoundParam(const CompoundParam<W> &other) :
    Function<T>(other), ndim_p(other.ndim()),
    functionPtr_p(other.nFunctions()),
    paroff_p(other.nFunctions()),
    funpar_p(other.nparameters()), 
    locpar_p(other.nparameters()) { 
    for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
      functionPtr_p[i] = other.function(i).cloneAD();
      paroff_p[i] = other.parameterOffset(i);
    }
    for (uInt i=0; i<funpar_p.nelements(); ++i) {
      funpar_p[i] = other.parameterFunction(i);
      locpar_p[i] = other.parameterLocation(i);
    }
  }
  template <class W>
    CompoundParam(const CompoundParam<W> &other, Bool) :
    Function<T>(other), ndim_p(other.ndim()),
    functionPtr_p(other.nFunctions()),
    paroff_p(other.nFunctions()),
    funpar_p(other.nparameters()), 
    locpar_p(other.nparameters()) { 
    for (uInt i=0; i<functionPtr_p.nelements(); ++i) {
      functionPtr_p[i] = other.function(i).cloneNonAD();
      paroff_p[i] = other.parameterOffset(i);
    }
    for (uInt i=0; i<funpar_p.nelements(); ++i) {
      funpar_p[i] = other.parameterFunction(i);
      locpar_p[i] = other.parameterLocation(i);
    }
  }
  CompoundParam<T> &operator=(const CompoundParam<T> &other);
  // </group>
  
  virtual ~CompoundParam();

  //# Operators
  
  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("compound");
    return x; }
  
  // Add a function to the sum. All functions must have the same 
  // <src>ndim()</src> as the first one. Returns the (zero relative) number 
  // of the function just added.
  uInt addFunction(const Function<T> &newFunction);
  
  // Return the number of functions in the sum.
  uInt nFunctions() const { return functionPtr_p.nelements(); }
  
  // Return a reference to a specific Function.
  // <group>
  const Function<T> &function(uInt which) const {
    DebugAssert(nFunctions() > which, AipsError);
    return *(functionPtr_p[which]); }
  // </group>
  // Get the offset in function parameterlist for function which
  uInt parameterOffset(uInt which) const {
    DebugAssert(nFunctions() > which, AipsError); return paroff_p[which]; }
  // Get the function number belonging to parameter list element which
  uInt parameterFunction(uInt which) const {
    DebugAssert(nparameters() > which, AipsError);
    return funpar_p[which];
  }
  // Return locpar
  uInt parameterLocation(uInt which) const {
    DebugAssert(nparameters() > which, AipsError);
    return locpar_p[which];
  }
  // Returns the dimension of functions in the linear combination
  virtual uInt ndim() const { return ndim_p; }

private:
  //# Data
  // Number of dimensions of underlying functions
  uInt ndim_p;

protected:
  //# Data
  // Pointer to each added function
  PtrBlock<Function<T> *> functionPtr_p;
  // Index of offset for each function to its parameters in general list
  Block<uInt> paroff_p;
  // Index of function belonging to parameter
  Block<uInt> funpar_p;
  // Index of local parameter
  Block<uInt> locpar_p;

  //# Make members of parent classes known.
protected:
  using Function<T>::parset_p;
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/CompoundParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
