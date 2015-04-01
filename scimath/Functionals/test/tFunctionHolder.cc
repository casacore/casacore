//# tFunctionHolder.cc: Test the one-dimensional scaled polynomial class
//# Copyright (C) 2002
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

#include <casacore/scimath/Functionals/FunctionHolder.h>

#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/scimath/Functionals/UnaryFunction.h>
#include <casacore/scimath/Functionals/DiracDFunction.h>
#include <casacore/scimath/Functionals/GNoiseFunction.h>
#include <casacore/scimath/Functionals/KaiserBFunction.h>
#include <casacore/scimath/Functionals/SincFunction.h>
#include <casacore/scimath/Functionals/FunctionHolder.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// Make near zero zero
Double Y(const Double in) {
  return (abs(in)<1e-15 ? Double(0.0) : in);
}

AutoDiff<Double> Y(AutoDiff<Double> in) {
  in.value() = Y(in.value());
  for (uInt i=0; i<in.nDerivatives(); ++i) {
    in.deriv(i) = Y(in.deriv(i));
  }
  return in;
}


int main() {
  cout << "---------------- test FunctionHolder ---------------" << endl;
  cout << "------------------------ Unary ---------------------" << endl;
  {
    UnaryFunction<Double> fn(5, 7, 3);
    for (Double x=3; x<11.2; x+=1.0) cout << "x=" << Y(x) << ": " <<
				       Y(fn(x)) << endl;
    UnaryFunction<AutoDiff<Double> > fnd;
    fnd[0] = AutoDiff<Double>(5, 3, 0);
    fnd[1] = AutoDiff<Double>(7, 3, 1);
    fnd[2] = AutoDiff<Double>(3, 3, 2);
    for (Double x=3; x<11.2; x+=1.0) cout << "x=" << Y(x) << ": " <<
				       Y(fnd(x)) << endl;
  }
  cout << "------------------------ Dirac delta ----------------" << endl;
  {
    DiracDFunction<Double> fn(5, 7);
    for (Double x=6; x<8.2; x+=1.0) cout << "x=" << Y(x) << ": " <<
				       Y(fn(x)) << endl;
    DiracDFunction<AutoDiff<Double> > fnd;
    fnd[0] = AutoDiff<Double>(5, 2, 0);
    fnd[1] = AutoDiff<Double>(7, 2, 1);
    for (Double x=6; x<8.2; x+=1.0) cout << "x=" << Y(x) << ": " <<
				      Y(fnd(x)) << endl;
  }
  cout << "------------------------ Normal noise ---------------" << endl;
  {
    GNoiseFunction<Double> fn(0, 2.0);
    for (uInt i=0; i<10; ++i) cout << fn() << endl;
    GNoiseFunction<AutoDiff<Double> > fnd;
    for (uInt i=0; i<10; ++i) cout << fnd() << endl;
  }
  cout << "------------------------ Kaiser-Bessel --------------" << endl;
  {
    KaiserBFunction<Double> fn;
    for (Double x=-1.2; x<1.21; x+=0.2) cout << "x=" << Y(x) << ": " <<
					  Y(fn(x)) << endl;
    for (Double x=6; x<8.2; x+=1.0) cout << "x=" << Y(x) << ": " <<
				      Y(fn(x)) << endl;
    KaiserBFunction<AutoDiff<Double> > fnd;
    fnd[0] = AutoDiff<Double>(1, 4, 0);
    fnd[1] = AutoDiff<Double>(0, 4, 1);
    fnd[2] = AutoDiff<Double>(1, 4, 2);
    fnd[3] = AutoDiff<Double>(2.5, 4, 2);
    for (Double x=-1.2; x<1.21; x+=0.2) cout << "x=" << Y(x) << ": " <<
					  Y(fnd(x)) << endl;
  }
  cout << "------------------------ sinc -----------------------" << endl;
  {
    SincFunction<Double> fn;
    for (Double x=-1.2; x<1.21; x+=0.2) cout << "x=" << Y(x) << ": " <<
					  Y(fn(x)) << endl;
    SincFunction<AutoDiff<Double> > fnd;
    fnd[0] = AutoDiff<Double>(1, 3, 0);
    fnd[1] = AutoDiff<Double>(0, 3, 1);
    fnd[2] = AutoDiff<Double>(1, 3, 2);
    for (Double x=-1.19999; x<1.21; x+=0.2) cout << "x=" << Y(x) << ": " <<
					      Y(fnd(x)) << endl;
  }
  cout << "------------------------ Gaussian1D -----------------" << endl;
  {
    Gaussian1D<Double> fn;
    FunctionHolder<Double> fh(fn);
    for (Double x=-1.2; x<1.21; x+=0.2) cout << "x=" << Y(x) << ": " <<
					  fh.asFunction()(x) << endl;
    Gaussian1D<AutoDiff<Double> > fnd;
    fnd[0] = AutoDiff<Double>(1, 3, 0);
    fnd[1] = AutoDiff<Double>(0, 3, 1);
    fnd[2] = AutoDiff<Double>(1, 3, 2);
    ///    FunctionHolder<AutoDiff<Double> > fhd(fnd);
    for (Double x=-1.2; x<1.21; x+=0.2) cout << "x=" << Y(x) << ": " <<
    ///					  fhd.asFunction()(x) << endl;
					  Y(fnd(x)) << endl;
  }

  cout << "-----------------------------------------------------" << endl;
  cout << "OK" << endl;
  return 0;
}
