//# CompiledFunction.h: Form a linear combination of Functions
//# Copyright (C) 2002
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

#if !defined(AIPS_COMPILEDFUNCTION_H)
#define AIPS_COMPILEDFUNCTION_H

//# Includes
#include <aips/aips.h>
#include <trial/Functionals/CompiledParam.h>

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
// derivatives with respect to the parameters are calculated at <src>x=2</src>.
// <srcblock>
// // the Gaussian
// CompiledFunction<Double> prof("p0*exp(-((x-p1)/p2)^2)");
// prof[0] = 2;				// the height
// prof[1] = 1.5;			// the center
// prof[2] = 1;				// the width
// Vector<Double> x(3);
// X[0] = 1.9; x[1] = 2.0; x[2] = 2.1;
// cout << "Gaussian at x=" << x << ": " << prof(x) << endl;
// // and an automatic derivative one:
// CompiledFunction<AutoDiff<Double> > profad("p0*exp(-((x-p1)/p2)^2)");
// cout << "Gaussian at x=" << x << ": " << profad(x) << endl;
// </srcblock>
// will produce the output:
// <srcblock>
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
  CompiledFunction() : CompiledParam<T>() {};
  // Make this object a (deep) copy of other.
  CompiledFunction(const CompiledFunction<T> &other) :
    CompiledParam<T>(other) {};
  // Make this object a (deep) copy of other.
  CompiledFunction<T> &operator=(const CompiledFunction<T> &other) {
    CompiledParam<T>::operator=(other); return *this; };

  // Destructor
  virtual ~CompiledFunction() {};

  //# Operators
  // Evaluate the function at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;
  
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible for
  // deleting the pointer.
  // <group>
  virtual Function<T> *clone() const { return new CompiledFunction<T>(*this); };
  // </group>

};

#endif
