//# HyperPlane.h: Form a hyper plane function
//# Copyright (C) 2001,2002,2004,2005
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

#ifndef SCIMATH_CONSTANT_H
#define SCIMATH_CONSTANT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/ConstantNDParam.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> A constant function.
// </summary>
//
// <use visibility=export>
// <reviewed reviewer="" date="" tests="tConstant" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis>
// This class represents a constant function in a space
// of arbitrary dimension
// f(x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>) = constant
// where x<sub>i</sub>
// are independent arguments and m is the number of dimensions of the space.
//
//
// Since the <src>Constant</src> is a <src>Function</src>, the derivatives
// can be obtained as well (and are in fact 0 of course).
//
// The parameter interface (see 
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// This class is in general used implicitly by the <src>Constant</src>
// class only.
// </synopsis>
//
// <example>
// <srcblock>
// // form the constant function in 4-D space
//  Constant<Double> constant(4); // 4-dim hyperplane
//  constant.parameters()[0] = 22;
//  // Evaluate at x0=5, x3=7
//  Vector<Double> x(4);
//  x=0; x[0]=5; x[3]=7;
//  cout << "constant value: " << constant(x) << endl;
//  constant value: 22
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to address incorrect
//	   coefficients
// </thrown>

// <motivation>
// This class was created because HyperPlane does not support a constant
// offset and modifying that class really required an interface change
// (ie that the constant offset be at the beginning of the parameter vector
// and that the parameter vector increase by one) so rather than breaking
// any code that already used HyperPlane I simply made a trivial Constant
// class.
// </motivation>
//
// <todo asof="2011/07/01">
//  <li> Nothing I know of
// </todo>

template<class T> class ConstantND : public ConstantNDParam<T>
{
public:
  //# Constructors
  // Construct a constant in an a space of dimensionality <src>m</src>.  By
  // default, the constant value is initialised to zero, and <src>m=0</src>
  explicit ConstantND(const uInt m=0) : ConstantNDParam<T>(m) {;};
  // Copy constructor/assignment (deep copy)
  // <group>
  ConstantND(const ConstantND<T> &other) : ConstantNDParam<T>(other) {};
  template <class W>
    ConstantND(const ConstantND<W> &other) : ConstantNDParam<T>(other) {}
  ConstantND<T> &operator=(const ConstantND<T> &other) {
    ConstantNDParam<T>::operator=(other); return *this; };
  // </group>

  // Destructor
  virtual ~ConstantND() {};

  //# Operators    
  // Evaluate the hyper plane function at
  // (x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>).
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new ConstantND<T>(*this); };
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new ConstantND<typename FunctionTraits<T>::DiffType>(*this); };
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new ConstantND<typename FunctionTraits<T>::BaseType>(*this); };
  // </group>

  //# Make members of parent classes known.
protected:
  using ConstantNDParam<T>::param_p;
public:
  using ConstantNDParam<T>::nparameters;
};


#define ConstantND_PS ConstantND

// <summary> Partial specialization of ConstantND for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>HyperPlane_PS</src> is only for cxx2html
// documentation problems. Use <src>HyperPlane</src> in your code.</note>
// </synopsis>

template <class T> class ConstantND_PS<AutoDiff<T> > :
public ConstantNDParam<AutoDiff<T> >
{
public:
  //# Construct
  // Constructors a constant in a space of dimensionality <src>m</src>.  By
  // default, the coefficients are initialized to zero, and <src>m=0</src>
  explicit ConstantND_PS(const uInt m=0) :
    ConstantNDParam<AutoDiff<T> >(m) {};
  // Copy constructor/assignment (deep copy)
  // <group>
  ConstantND_PS(const ConstantND_PS<AutoDiff<T> > &other) :
    ConstantNDParam<AutoDiff<T> >(other) {};
  template <class W>
    ConstantND_PS(const ConstantND_PS<W> &other) :
    ConstantNDParam<AutoDiff<T> >(other) {}
  ConstantND_PS<AutoDiff<T> > &
    operator=(const ConstantND_PS<AutoDiff<T> > &other) {
    ConstantNDParam<AutoDiff<T> >::operator=(other); return *this; };
  // </group>
  
  // Destructor
  virtual ~ConstantND() {};
  
  //# Operators    
  // Evaluate the constant function at
  // (x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>).
  virtual AutoDiff<T> 
    eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new ConstantND_PS<AutoDiff<T> >(*this); };
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new ConstantND<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); };
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new ConstantND<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); };
  // </group>

  //# Make members of parent classes known.
protected:
  using ConstantNDParam<AutoDiff<T> >::param_p;
public:
  using ConstantNDParam<AutoDiff<T> >::nparameters;
};

#undef ConstantND_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/ConstantND.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
