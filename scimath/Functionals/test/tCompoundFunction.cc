//# tCompoundFunction: Test the CompoundFunction class
//# Copyright (C) 1995,1996,1999,2001,2002
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


#include <casacore/scimath/Functionals/CompoundFunction.h>

#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>

#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {

  //     CompoundFunction();
  CompoundFunction<Double> sumfunc;
  AlwaysAssertExit(sumfunc.nparameters() == 0 && sumfunc(-11.0) == 0.0);

  //     uInt addFunction(const Function<T> &newFunction);
  Polynomial<Double> poly(2); poly[2] = 1.0; 	     // x^2
  Gaussian1D<Double> gauss(1.0, 0.0, sqrt(log(16.0)));   // e^{-x^2}
  sumfunc.addFunction(poly);
  sumfunc.addFunction(gauss);                             // x^2 + e^{-x^2}

  //   T operator()(const T &x) const;
  AlwaysAssertExit(near(sumfunc(2.0), 2.0*2.0 + 1.0/C::e/C::e/C::e/C::e));
  Double xvec = 1.0;
  AlwaysAssertExit(near(sumfunc(xvec), 1.0*1.0 + 1.0/C::e));

  //   uInt nparameters()
  AlwaysAssertExit(sumfunc.nparameters() == 6);
  
  //     CompoundFunction(const CompoundFunction<T> &other);
  //     operator=(const CompoundFunction<T> &other);
  CompoundFunction<Double> f2(sumfunc);
  CompoundFunction<Double> f3;
  f3 = sumfunc;

  //     void setParameter(uInt which, const T &val);
  AlwaysAssertExit(allEQ(sumfunc.parameters().getParameters(),
			 f2.parameters().getParameters()) &&
		   allEQ(sumfunc.parameters().getParameters(), 
			 f3.parameters().getParameters()));

  //     uInt nFunctions() const { return functions_p.nelements(); }
  AlwaysAssertExit(sumfunc.nFunctions() == 2 && f2.nFunctions() == 2 &&
		   f3.nFunctions() == 2);

  //     const Function<T> *function(uInt which) const
  //     Function<T> *function(uInt which);
  const CompoundFunction<Double> sfref = sumfunc;
  AlwaysAssertExit( (sumfunc.function(0))(3.0) == 9.0);
  AlwaysAssertExit( near((sfref.function(1))(-1.0), 1.0/C::e ));
  //     T getparameter(uInt which) const;
  AlwaysAssertExit(sumfunc[0] == 0.0 &&
		   sumfunc[1] == 0.0 &&
		   sumfunc[2] == 1.0 &&
		   sumfunc[3] == 1.0 &&
		   sumfunc[4] == 0.0 &&
		   near(sumfunc[5] , sqrt(log(16.0))));
  
  //     virtual void setParameter(uInt which, const T &val);
  sumfunc[4] = 2.0;
  AlwaysAssertExit(near(sumfunc(3.0), 3.0*3.0 + 1.0/C::e));
  
  //     virtual Function<T> *cloneFunction() const;
  //     ~CompoundFunction();
  Function<Double> *fptr = sumfunc.clone();
  AlwaysAssertExit(allEQ(sumfunc.parameters().getParameters(), 
			  fptr->parameters().getParameters()));
  delete fptr;
  
  cout << "OK" << endl;
  return 0;
}
