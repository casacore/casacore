//# NQHyperPlane.h: Form a hyper plane function
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

#if !defined(AIPS_NQHYPERPLANE_H)
#define AIPS_NQHYPERPLANE_H

//# Includes
#include <aips/aips.h>
#include <aips/Functionals/NQHyperPlaneParam.h>
#include <aips/Functionals/Function.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffMath.h>

// <summary> A hyper plane function.
// </summary>
//
// <use visibility=export>
// <reviewed reviewer="" date="1996/8/14" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <synopsis>
// This class forms a function of the form
// <srcblock>
// f(x0,x1,...,xm-1) = e0*x0 + e1*x1 + ... + em-1*xm-1 + c
// </srcblock>
// where e = {ei} and c are coefficients and x = {xi} are indepedent 
// varaibles.  f(x0,x1,...,xm-1) = 0 represents a hyper plane.
// The coefficients can or cannot be adjusted depending on corresponding 
// masks' values.<p>
//
// In this class the coefficients {ei} plus c are the "available" parameters.
// They can be access through access functions 
// inherited from parameterized class.
// </synopsis>
//
// <example>
// // form the hyper plane function of this form: 
// // 6*x0 + 2*x3 + 5 = 0
// NQHyperPlane<Double> hyper(4);
// hyper.setCoefficient(0,6);   
// hyper.setCoefficient(3,2);
// hyper.setCoefficient(4,5);
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to address incorrect
//		coefficients
// </thrown>

// <motivation>
// This class was created to allow the creation of linear constraint functions
// for the use of linear least-squares fit.
// </motivation>
//
// <todo asof="2001/10/25>
//  <li> Look at providing special class with <src>c=0</src> (plane through
// origin)
// </todo>

template<class T> class NQHyperPlane : public NQHyperPlaneParam<T> {
 public:
  //# Constructors
  // Construct an m-dimensional hyper plane which has m+1 coefficients.  By 
  // default, the coefficients are initialized to zero, and <src>m=0</src>
  explicit NQHyperPlane(const uInt m=0) : NQHyperPlaneParam<T>(m) {};
  // Copy constructor/assignment (deep copy)
  // <group>
  NQHyperPlane(const NQHyperPlane<T> &other) : NQHyperPlaneParam<T>(other) {};
  NQHyperPlane<T> &operator=(const NQHyperPlane<T> &other) {
    NQHyperPlaneParam<T>::operator=(other); return *this; };
  // </group>

  // Destructor
  virtual ~NQHyperPlane() {};

  //# Operators    
  // Evaluate the hyper plane function at <src>(x0,x1,...,xm-1)</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  virtual Function<T> *clone() const { return new NQHyperPlane<T>(*this); };

 private:

};

#define NQHyperPlane_PS NQHyperPlane

// <summary> Partial specialization of NQHyperPlane for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>NQHyperPlane_PS</src> is only for cxx2html
// documentation problems. Use <src>NQHyperPlane</src> in your code.</note>
// </synopsis>

template <class T> class NQHyperPlane_PS<AutoDiff<T> > : 
public NQHyperPlaneParam<AutoDiff<T> > {
 public:
  //# Construct
  // Constructors an m-dimensional hyper plane which has m+1 coefficients.  By 
  // default, the coefficients are initialized to zero, and <src>m=0</src>
  explicit NQHyperPlane_PS(const uInt m=0) :
    NQHyperPlaneParam<AutoDiff<T> >(m) {};
  // Copy constructor/assignment (deep copy)
  // <group>
  NQHyperPlane_PS(const NQHyperPlane_PS<AutoDiff<T> > &other) :
    NQHyperPlaneParam<AutoDiff<T> >(other) {};
  NQHyperPlane_PS<AutoDiff<T> > &
    operator=(const NQHyperPlane_PS<AutoDiff<T> > &other) {
    NQHyperPlaneParam<AutoDiff<T> >::operator=(other); return *this; };
  // </group>

  // Destructor
  virtual ~NQHyperPlane() {};

  //# Operators    
  // Evaluate the hyper plane function at <src>(x0,x1,...,xm-1)</src>.
  virtual AutoDiff<T> 
    eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  virtual Function<AutoDiff<T> > *clone() const {
    return new NQHyperPlane_PS<AutoDiff<T> >(*this); };

 private:

};

#undef NQHyperPlane_PS

#endif
