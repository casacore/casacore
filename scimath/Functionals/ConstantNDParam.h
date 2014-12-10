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

#ifndef SCIMATH_CONSTANTNDPARAM_H
#define SCIMATH_CONSTANTNDPARAM_H


#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Parameter handling for a constant function in a space of arbitrary dimensionality.
// </summary>
//
// <use visibility=local>
// <reviewed reviewer="" date="" tests="tConstant" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>
//
// <synopsis>
// This class forms a function of the form
// f(x<sub>0</sub>,x<sub>1</sub>,..,x<sub>m-1</sub>) = constant
// in an m-dimensional parameter space where
// x<sub>i</sub> are independent arguments.
//
// Since the <src>Constant</src> is a <src>Function</src>, the derivatives
// can be obtained as well (and in fact are 0 of course).
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
//  form a constant function in 4-D space
//  Constant<Double> constant(4); // constant in 4-D param space
//  constant.parameters()[0] = 22;
//  // Evaluate at x0=5, x3=7
//  Vector<Double> x(4);
//  x=0; x[0]=5; x[3]=7;
//  cout << "constant value: " << constant(x) << endl;
//  constant value: 22
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
// This class was created because HyperPlane does not support a constant
// offset and modifying that class really required an interface change
// (ie that the constant offset be at the beginning of the parameter vector
// and that the parameter vector increase by one) so rather than breaking
// any code that already used HyperPlane I simply made a trivial Constant
// class.
// </motivation>

// <todo asof="2011/07/01">
//  <li> Nothing I know of
// </todo>

template<class T> class ConstantNDParam : public Function<T>
{
public:
  //# Constructors
  // Construct a constant in m-dimensional space.  By
  // default, the constant value is initialized to zero.
  // <group>
  explicit ConstantNDParam(uInt m=0);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  ConstantNDParam(const ConstantNDParam &other);
  template <class W>
    ConstantNDParam(const ConstantNDParam<W> &other) :
    Function<T>(other), _ndim(other.ndim()) {}
  // </group>

  // Copy assignment (deep copy)
  ConstantNDParam<T> &operator=(const ConstantNDParam<T> &other);

  // Destructor
  virtual ~ConstantNDParam();

  //# Operators    
  // Comparisons.  
  // HyperPlanes are equal if they are of the same order and have the same
  // parameters
  // <group>
  Bool operator==(const ConstantNDParam<T> &other) const {
    return (this->param_p == other.param_p); };
  Bool operator!=(const ConstantNDParam<T> &other) const {
    return (this->param_p != other.param_p); };
  // </group>
    
  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("constant");
    return x; };

  // What is the dimension of the parameter list
  virtual uInt ndim() const { return _ndim; };

  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
private:
  uInt _ndim;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/ConstantNDParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
