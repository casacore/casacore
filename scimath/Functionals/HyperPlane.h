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

#ifndef SCIMATH_HYPERPLANE_H
#define SCIMATH_HYPERPLANE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/HyperPlaneParam.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> A hyper plane function.
// </summary>
//
// <use visibility=export>
// <reviewed reviewer="wbrouw" date="2004/05/25" tests="tHyperPlane" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis>
// This class forms a function of the form
// f(x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>) =
// p<sub>0</sub>*x<sub>0</sub> + p<sub>1</sub>*x<sub>1</sub> + ...
// + p<sub>m-1</sub>*x<sub>m-1</sub>, 
// where p<sub>i</sub> are coefficients (parameters) and x<sub>i</sub>
// are independent arguments.
//
// f(x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>) represents a hyper plane
// of dimension <src>m</src>.
//
// Since the <src>HyperPlane</src> is a <src>Function</src>, the derivatives
// can be obtained as well. 
//
// The parameter interface (see 
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// This class is in general used implicitly by the <src>HyperPlane</src>
// class only.
// </synopsis>
//
// <example>
// <srcblock>
// // form the hyper plane function of this form: 
// // 6*x0 + 2*x3 
//  HyperPlane<Double> hyper(4); // 4-dim hyperplane
//  hyper.parameters()[0] = 6;   
//  hyper.parameters()[3] = 2;
//  // Evaluate at x0=5, x3=7
//  Vector<Double> x(4);
//  x=0; x[0]=5; x[3]=7;
//  cout << "Hypervalue: " << hyper(x) << endl;
//  Hypervalue: 44
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
// This class was created to allow the creation of linear constraint functions
// for the use of linear least-squares fit.
// </motivation>
//
// <todo asof="2004/05/25>
//  <li> Nothing I know of
// </todo>

template<class T> class HyperPlane : public HyperPlaneParam<T>
{
public:
  //# Constructors
  // Construct an m-dimensional hyper plane which has m parameters.  By 
  // default, the coefficients are initialised to zero, and <src>m=0</src>
  explicit HyperPlane(const uInt m=0) : HyperPlaneParam<T>(m) {;}
  // Copy constructor/assignment (deep copy)
  // <group>
  HyperPlane(const HyperPlane<T> &other) : HyperPlaneParam<T>(other) {}
  template <class W>
    HyperPlane(const HyperPlane<W> &other) : HyperPlaneParam<T>(other) {}
  HyperPlane<T> &operator=(const HyperPlane<T> &other) {
    HyperPlaneParam<T>::operator=(other); return *this; }
  // </group>

  // Destructor
  virtual ~HyperPlane() {}

  //# Operators    
  // Evaluate the hyper plane function at
  // (x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>).
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new HyperPlane<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new HyperPlane<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new HyperPlane<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using HyperPlaneParam<T>::param_p;
public:
  using HyperPlaneParam<T>::nparameters;
};

#define HyperPlane_PS HyperPlane

// <summary> Partial specialization of HyperPlane for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>HyperPlane_PS</src> is only for cxx2html
// documentation problems. Use <src>HyperPlane</src> in your code.</note>
// </synopsis>

template <class T> class HyperPlane_PS<AutoDiff<T> > : 
public HyperPlaneParam<AutoDiff<T> >
{
public:
  //# Construct
  // Constructors an m-dimensional hyper plane which has m parameters.  By 
  // default, the coefficients are initialized to zero, and <src>m=0</src>
  explicit HyperPlane_PS(const uInt m=0) :
    HyperPlaneParam<AutoDiff<T> >(m) {}
  // Copy constructor/assignment (deep copy)
  // <group>
  HyperPlane_PS(const HyperPlane_PS<AutoDiff<T> > &other) :
    HyperPlaneParam<AutoDiff<T> >(other) {}
  template <class W>
    HyperPlane_PS(const HyperPlane_PS<W> &other) :
    HyperPlaneParam<AutoDiff<T> >(other) {}
  HyperPlane_PS<AutoDiff<T> > &
    operator=(const HyperPlane_PS<AutoDiff<T> > &other) {
    HyperPlaneParam<AutoDiff<T> >::operator=(other); return *this; }
  // </group>
  
  // Destructor
  virtual ~HyperPlane() {}
  
  //# Operators    
  // Evaluate the hyper plane function at
  // (x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>).
  virtual AutoDiff<T> 
    eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new HyperPlane_PS<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new HyperPlane<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new HyperPlane<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using HyperPlaneParam<AutoDiff<T> >::param_p;
public:
  using HyperPlaneParam<AutoDiff<T> >::nparameters;
};

#undef HyperPlane_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/HyperPlane.tcc>
#include <casacore/scimath/Functionals/HyperPlane2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
