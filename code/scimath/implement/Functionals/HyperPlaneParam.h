//# NQHyperPlaneParam.h: Parameters For a hyper plane function
//# Copyright (C) 2001,2002
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

#if !defined(AIPS_NQHYPERPLANEPARAM_H)
#define AIPS_NQHYPERPLANEPARAM_H


#include <aips/aips.h>
#include <aips/Functionals/Function.h>

// <summary> Parameters for a hyper plane function.
// </summary>
//
// <use visibility=local>
// <reviewed reviewer="" date="1996/8/14" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>
//
// <synopsis>
// This class forms a function of the form
// <srcblock>
// f(x0,x1,...,xm-1) = e0*x0 + e1*x1 + ... + em-1*xm-1 + c
// </srcblock>
// where e = {ei} and c are coefficients and x = {xi} are independent 
// variables.  f(x0,x1,...,xm-1) = 0 represents a hyper plane.
// The coefficients can or cannot be adjusted depending on corresponding 
// masks' values.<p>
// In this class the coefficients {<src>ei</src>} plus c are the
// <em>available</em> parameters.
// They can be accessed through access functions 
// inherited <linkto class=Function>Function</linkto>
// class.
// </synopsis>
//
// <example>
// // form the hyper plane function of this form: 
// // 6*x0 + 2*x3 + 5 = 0
// NQHyperPlaneParam<Double> hyper(4);
// hyper.setCoefficient(0,6);   
// hyper.setCoefficient(3,2);
// hyper.setCoefficient(4,5);
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

// <motivation>
// This class was created to allow the creation of linear constraint functions
// for the use of linear least-squares fit.
// </motivation>

// <todo asof="2001/10/22">
//  <li> Nothing I know of
// </todo>

template<class T> class NQHyperPlaneParam : public Function<T> {
 public:
  //# Constructors
  // Construct an m dimensional hyper plane which has m+1 coefficients.  By 
  // default, the coefficients are initialized to zero. The default plane has
  // <src>m=0</src>
  // <group>
  explicit NQHyperPlaneParam(const uInt m=0);
  // </group>

  // Copy constructor (deep copy)
  NQHyperPlaneParam(const NQHyperPlaneParam &other);

  // Copy assignment (deep copy)
  NQHyperPlaneParam<T> &operator=(const NQHyperPlaneParam<T> &other);

  // Destructor
  virtual ~NQHyperPlaneParam();

  //# Operators    
    
  //# Member functions
  // What is the order (<em>m</em>of the HyperPlane, i.e. the dimension
  uInt order() const { return param_p.nelements() - 1; };
  // What is the dimension of the parameter list
  virtual uInt ndim() const { return param_p.nelements(); };

};

#endif
