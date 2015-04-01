//# HyperPlaneParam.h: Parameters For a hyper plane function
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

#ifndef SCIMATH_HYPERPLANEPARAM_H
#define SCIMATH_HYPERPLANEPARAM_H


#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Parameter handling for a hyper plane function.
// </summary>
//
// <use visibility=local>
// <reviewed reviewer="wbrouw" date="2004/05/25" tests="tHyperPlane" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function">Function</linkto> class
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
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

// <motivation>
// This class was created to facilitate linear constraint functions
// for the use of least-squares fits.
// </motivation>

// <todo asof="2004/05/22">
//  <li> Nothing I know of
// </todo>

template<class T> class HyperPlaneParam : public Function<T>
{
public:
  //# Constructors
  // Construct an m-dimensional hyper plane which has m parameters.  By 
  // default, the coefficients are initialized to zero. The default plane has
  // <src>m=0</src>
  // <group>
  explicit HyperPlaneParam(uInt m=0);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  HyperPlaneParam(const HyperPlaneParam &other);
  template <class W>
    HyperPlaneParam(const HyperPlaneParam<W> &other) :
    Function<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  HyperPlaneParam<T> &operator=(const HyperPlaneParam<T> &other);

  // Destructor
  virtual ~HyperPlaneParam();

  //# Operators    
  // Comparisons.  
  // HyperPlanes are equal if they are of the same order and have the same
  // parameters
  // <group>
  Bool operator==(const HyperPlaneParam<T> &other) const {
    return (this->param_p == other.param_p); }
  Bool operator!=(const HyperPlaneParam<T> &other) const {
    return (this->param_p != other.param_p); }
  // </group>
    
  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("hyperplane");
    return x; }

  // What is the dimension of the parameter list
  virtual uInt ndim() const { return this->param_p.nelements(); }

  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/HyperPlaneParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
