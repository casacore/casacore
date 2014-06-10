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

#ifndef SCIMATH_POISSONFUNCTION_H
#define SCIMATH_POISSONFUNCTION_H

//# Includes
#include <casa/aips.h>
#include <scimath/Functionals/PoissonParam.h>
#include <scimath/Functionals/Function.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward declarations

// <summary> A one dimensional Poisson function
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFunctionHolder" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="PoissonParam">PoissonParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A 1-dimensional Poisson.
// </etymology>

// <synopsis> 
// A <src>Poisson</src> is described by lambda.
// The parameters are enumerated by LAMBDA. They have
// default values of 1.
// </synopsis> 
//
// <example>
// <srcblock>
//    PoissonFunction<Double> sf(5.0);
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

template<class T> class PoissonFunction : public PoissonParam<T>
{
public:
  //# Constructors
  // Constructs the PoissonFunction, Defaults:
  // lambda=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  PoissonFunction() : PoissonParam<T>() {}
  explicit PoissonFunction(const T &lambda) :
    PoissonParam<T>(lambda) {}
  PoissonFunction( const T& lambda, const T& height ):
	  PoissonParam<T>(lambda,height){}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  PoissonFunction(const PoissonFunction<T> &other) : PoissonParam<T>(other) {}
  template <class W>
  PoissonFunction(const PoissonFunction<W> &other) : PoissonParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  PoissonFunction<T> &operator=(const PoissonFunction<T> &other) {
    PoissonParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~PoissonFunction() {}

  //# Operators    
  // Evaluate the Poisson at <src>x</src>.
  // If a vector is used as the argument only its first element is used.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer. 
  // <group>
  virtual Function<T> *clone() const { return new PoissonFunction<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new PoissonFunction<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new PoissonFunction<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using PoissonParam<T>::param_p;
public:
  using PoissonParam<T>::nparameters;
  using PoissonParam<T>::LAMBDA;
  using PoissonParam<T>::HEIGHT;
};



#define PoissonFunction_PS PoissonFunction

// <summary> Partial specialization of PoissonFunction for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>PoissonFunction_PS</src> is only for cxx2html
// documentation problems. Use <src>PoissonFunction</src> in your code.</note>
// </synopsis>

template <class T> class PoissonFunction_PS<AutoDiff<T> > :
public PoissonParam<AutoDiff<T> >
{
public:
  //# Constructors
  // Constructs one dimensional Poisson.
  // <group>
  PoissonFunction_PS() : PoissonParam<AutoDiff<T> >() {}
  explicit PoissonFunction_PS(const AutoDiff<T> &lambda) :
    PoissonParam<AutoDiff<T> >(lambda) {}
  PoissonFunction_PS( const AutoDiff<T> & lambda, const AutoDiff<T>& height):
	  PoissonParam<AutoDiff<T> >(lambda,height){}

  // </group>

  // Copy constructor (deep copy)
  // <group>
  PoissonFunction_PS(const PoissonFunction_PS &other) :
    PoissonParam<AutoDiff<T> >(other) {}
  template <class W>
  PoissonFunction_PS(const PoissonFunction_PS<W> &other) :
    PoissonParam<AutoDiff<T> >(other) {}
  // </group>

  // Copy assignment (deep copy)
  PoissonFunction_PS<AutoDiff<T> > &
    operator=(const PoissonFunction_PS<AutoDiff<T> > &other) {
    PoissonFunction<AutoDiff<T> >::operator=(other); return *this; }

  // Destructor
  virtual ~PoissonFunction_PS() {}

  //# Operators
  // Evaluate the Poisson and its derivatives at <src>x</src>.
  // <group>
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible
  // for deleting this pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new PoissonFunction<AutoDiff<T> >(*this);
  }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new PoissonFunction<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this);
  }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new PoissonFunction<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this);
  }
  // </group>

  //# Make members of parent classes known.
protected:
  using PoissonParam<AutoDiff<T> >::param_p;
public:
  using PoissonParam<AutoDiff<T> >::LAMBDA;
  using PoissonParam<AutoDiff<T> >::HEIGHT;
};

#undef PoissonFunction_PS


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <scimath/Functionals/PoissonFunction.tcc>
#include <scimath/Functionals/PoissonFunction2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
