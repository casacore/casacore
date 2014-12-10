//# CompiledFunction.h: Form a linear combination of Functions
//# Copyright (C) 2002,2004,2005
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

#ifndef SCIMATH_COMPILEDFUNCTION_H
#define SCIMATH_COMPILEDFUNCTION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/CompiledParam.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>
// Form a linear combination of function objects.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tFuncExpression" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>
//
// <synopsis>
// Given a string describing an expression
// (see <linkto class=FuncExpression>FuncExpression</linkto> class for
// details of the expression), the <src>CompiledFunction</src>class wraps
// this expression as a 
// Function (see <linkto class=Function>Function</linkto> class) which can
// be used in all places where functions can be used (e.g. see
// <linkto module=Fitting>Fitting</linkto>).
//
// The <linkto class=CompiledFunction>CompiledParam</linkto> class takes
// care of the parameter interface.
// </synopsis>
//
// <example>
// In the following example a Gaussian profile with three parameters
// (height, center and halfwidth) is specified and its value and
// derivatives with respect to the parameters are calculated at
// <src>x=[1.9,2,2.1]</src>.
// <srcblock>
// // the Gaussian
// CompiledFunction<Double> prof;
// prof.setFunction("p0*exp(-((x-p1)/p2)^2)");
// prof[0] = 2;				// the height
// prof[1] = 1.5;			// the center
// prof[2] = 1;				// the width
// Vector<Double> x(3);
// x[0] = 1.9; x[1] = 2.0; x[2] = 2.1;
// for (uInt i=0; i<3; ++i) {
//   cout << "Gaussian at x=" << x[i] << ": " << prof(x[i]) << endl;
// }
// // Calculate automatic derivatives of same function:
// CompiledFunction<AutoDiff<Double> > profad;
// profad.setFunction("p0*exp(-((x-p1)/p2)^2)");
// // Set the parameters (note the specification of the number of
// // derivatives and which derivative the parameter is)
// profad[0] = AutoDiff<Double>(2,  3,0);	// the height
// profad[1] = AutoDiff<Double>(1.5,3,1);	// the center
// profad[2] = AutoDiff<Double>(1,  3,2);      	// the width
// for (uInt i=0; i<3; ++i) {
//   cout << "Gaussian at x=" << x[i] << ": " << profad(x[i]) << endl;
// }
// cout << "Value (x=2): " << profad(x[1]).value() << endl;
// cout << "Derivatives: " << profad(x[1]).derivatives() << endl;
// cout << "Derivative1: " << profad(x[1]).derivatives()[1] << endl;
// </srcblock>
// will produce the output:
// <srcblock>
//	Gaussian at x=1.9: 1.70429
//	Gaussian at x=2: 1.5576
//	Gaussian at x=2.1: 1.39535
//	Gaussian at x=1.9: (1.70429, [0.852144, 1.36343, 0.545372])
//	Gaussian at x=2: (1.5576, [0.778801, 1.5576, 0.778801])
//	Gaussian at x=2.1: (1.39535, [0.697676, 1.67442, 1.00465])
//	Value (x=2): 1.5576
//	Derivatives: [0.778801, 1.5576, 0.778801]
//	Derivative1: 1.5576
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and functions.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
// </thrown>
//
// <motivation>
// This class was created to allow specialization of the function evaluation in
// a simple way.
// </motivation>
//
// <todo asof="2002/04/29">
// <li> Nothing I know of
// </todo>

template <class T> class CompiledFunction : public CompiledParam<T> {
 public:
  //# Constructors
  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.
  CompiledFunction() : CompiledParam<T>() {}
  // Make this object a (deep) copy of other.
  // <group>
  CompiledFunction(const CompiledFunction<T> &other) :
    CompiledParam<T>(other) {}
  template <class W>
    CompiledFunction(const CompiledFunction<W> &other) :
    CompiledParam<T>(other) {}
  // </group>
  // Make this object a (deep) copy of other.
  CompiledFunction<T> &operator=(const CompiledFunction<T> &other) {
    CompiledParam<T>::operator=(other); return *this; }

  // Destructor
  virtual ~CompiledFunction() {}

  //# Operators
  // Evaluate the function at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const {
    return new CompiledFunction<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new CompiledFunction<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new CompiledFunction<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>
  
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/CompiledFunction.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
