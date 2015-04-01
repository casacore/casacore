//# tSampledFunctional.cc:
//# Copyright (C) 1996,1999,2002
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
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/scimath/Functionals/ArraySampledFunctional.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  Bool anyFailures = False;
  {
    Bool Failed = False;
    uInt i;
    Vector<Float> v(10); indgen(v);
    ScalarSampledFunctional<Float> fv(v);
    for (i = 0; i < fv.nelements(); i++)
      if (!near(fv(i), Float(i)))
	Failed = True;

    // Check the assignment operator and copy constructor 
    // for const ScalarSampledFunctionals use copy semantics
    const ScalarSampledFunctional<Float> cfv(v);
    ScalarSampledFunctional<Float> cfv1(cfv), cfv2;
    cfv2 = cfv;
    v(0) = 100.0f; 
    if (!near(cfv(0), 100.0f))
      Failed = True;
    for (i = 1; i < cfv.nelements(); i++)
      if (!near(cfv(i), Float(i)))
	Failed = True;
    for (i = 0; i < cfv1.nelements(); i++)
      if (!near(cfv1(i), Float(i)))
 	Failed = True;
    for (i = 0; i < cfv2.nelements(); i++)
      if (!near(cfv2(i), Float(i)))
 	Failed = True;

    // Check the copy constructor uses reference sematics
    ScalarSampledFunctional<Float> fv1(fv);
    for (i = 1; i < fv.nelements(); i++)
      if (!near(fv1(i), Float(i)))
	Failed = True;
    if (!near(fv(0),100.0f))
      Failed = True;
    if (!near(fv1(0),100.0f))
      Failed = True;

    // Check the assignment operator uses reference sematics
    ScalarSampledFunctional<Float> fv2;
    fv2 = fv1;
    for (i = 1; i < fv.nelements(); i++)
      if (!near(fv1(i), Float(i)))
	Failed = True;
    if (!near(fv1(0),100.0f))
      Failed = True;

    // The block constructor uses copy semantics
    Block<Float> b(10);
    for (i = 0; i < fv.nelements(); i++)
      b[i] = Float(i);
    ScalarSampledFunctional<Float> fb(b);
    b = 0.0f;
    for (i = 0; i < fb.nelements(); i++)
      if (!near(fb(i), Float(i)))
	Failed = True;

    if (Failed) {
      cout << "Failed"; 
      anyFailures = True;
    }
    else
      cout << "Passed";
    cout << " the ScalarSampledFunctional test" << endl;
  }
  {
    Bool Failed = False;
    uInt i;
    Array<Double> a(IPosition(4,2,3,10,1)); indgen(a);
    ArraySampledFunctional<Array<Double> > f(a);
    Matrix<Double> m;
    for (i = 0; i < f.nelements(); i++) {
      m = f(i);
      if (!near(m(0,0), Double(6*i+0)))
	Failed = True;
      if (!near(m(1,0), Double(6*i+1)))
	Failed = True;
      if (!near(m(0,1), Double(6*i+2)))
	Failed = True;
      if (!near(m(1,1), Double(6*i+3)))
	Failed = True;
      if (!near(m(0,2), Double(6*i+4)))
	Failed = True;
      if (!near(m(1,2), Double(6*i+5)))
	Failed = True;
      //      cout << "     f(" << i << ") = " << m << endl;;
    }
    ArraySampledFunctional<Array<Double> > f1(f);
    a(IPosition(4,0,0,0,0)) = 100.0;

    ArraySampledFunctional<Array<Double> > f2;
    f2 = f1;
    for (i = 1; i < f2.nelements(); i++) {
      m = f2(i);
      if (!near(m(0,0), Double(6*i+0)))
	Failed = True;
      if (!near(m(1,0), Double(6*i+1)))
	Failed = True;
      if (!near(m(0,1), Double(6*i+2)))
	Failed = True;
      if (!near(m(1,1), Double(6*i+3)))
	Failed = True;
      if (!near(m(0,2), Double(6*i+4)))
	Failed = True;
      if (!near(m(1,2), Double(6*i+5)))
	Failed = True;
    }
    m = f2(0);
    if (!near(m(0,0), 100.0))
      Failed = True;

    if (Failed) {
      cout << "Failed"; 
      anyFailures = True;
    }
    else
      cout << "Passed";
    cout << " the ArraySampledFunctional test" << endl;
  }
  if (anyFailures) {
    cout << "FAIL" << endl;
    return 1;
  }
  else {
    cout << "OK" << endl;
    return 0;
  }
}
