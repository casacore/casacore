//# tCombiFunction.cc: Test the CombiFunction class
//# Copyright (C) 2001,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/scimath/Functionals/CombiFunction.h>

#include <casacore/scimath/Functionals/Polynomial.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main() {
  int i;
  // construct a linear combination of functions: a(0)+a(1)*x+a(2)*x^2    
  Polynomial<Double> constant(0); 
  Polynomial<Double> linear(1); 
  Polynomial<Double> square(2);
  constant[0] = 1.0;   // 1
  linear[1] =  1.0;     // x
  square[2] =  1.0;     // x^2


  // The default constructor -- no functions, no parameters, nothing, the
  // function operator returns a 0.  
  CombiFunction<Double> combination;

  // Add a function.  All functions must have the same <src>ndim()</src>
  // as the first one.  Returns the (zero relative) number of the function 
  // just added.  In the meantime, the coefficient a(i) which is also the
  // ith available parameter, and the mask for the "available parameter" are 
  // initialized with "one" and "True," respectively.
  combination.addFunction(constant);
  combination.addFunction(linear);
  combination.addFunction(square);

  // Make this object a copy of other.
  //CombiFunction(const CombiFunction<T, T> &other);
  CombiFunction<Double> comb2(combination);
  
  // Make this object a copy of other.
  //CombiFunction<T> &operator=(const CombiFunction<T> &other);
  comb2 = combination;

  // Return the total number of coefficients.  The number is equal to the
  // number of functins that have been added.  
  // uInt nCoefficients() const;
  cout << "n: " <<
    combination.nFunctions() << ", " <<
    combination.nparameters() << ", " <<
    comb2.nFunctions() << ", " << endl;
  AlwaysAssertExit(combination.nFunctions() == 3 && 
		   combination.nparameters() == comb2.nFunctions());

  // Return the total number of functions.  The number is equal to the
  // number of functins that have been added.  
  //uInt nFunctions() const;
  AlwaysAssertExit(combination.nFunctions() == 3 && 
		   combination.nFunctions() == comb2.nFunctions());

  Vector<Double> v(3);

  // Set the value of a coefficient. 
  // f(x) = 10 + 11*x + 12*x^2
  for (i = 0; i < 3; i++) {
    combination[i] = i+10;
    AlwaysAssertExit(combination[i] == Double(i+10));
    v(i) = i+10;
  }

  // Set all coefficients at once. 
  combination.parameters().setParameters(v);
  AlwaysAssertExit(allEQ(combination.parameters().getParameters(), v));
  
  // Return a reference to a specific Function in the combination.
  // f(x) = 10 + 11*x + 12*x^2
  AlwaysAssertExit((combination.function(0))(10) == Double(1));
  AlwaysAssertExit((combination.function(1))(10) == Double(10));
  AlwaysAssertExit((combination.function(2))(10) == Double(100));

  // Evaluate the linear combination at <src>x</src>. 
  //virtual T operator()(const Vector<T> &x) const;
  // Evaluate the linear combination at <src>x</src>.
  // This operator is used when combination is 1D
  //virtual T operator()(const T &x) const;
  // f(x) = 10 + 11*x + 12*x^2
  v.resize(1);
  v(0) = 5;
  AlwaysAssertExit((combination(v) - Double(36365)) < 1.e-6);
  AlwaysAssertExit((combination(5) - Double(36365)) < 1.e-6);

  // Returns the dimension of functions in the linear combination
  //virtual uInt ndim() const;
  AlwaysAssertExit(combination.ndim() == 1);

  cout << "OK" << endl;
  return 0;
}
