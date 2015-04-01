//# dFunction.cc: test program for functional (AutoDiff) timing
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
//

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// 3 flavours of calculating a Gaussian
// The inputs are the parameters and the x value

Double a0(const Vector<Double> &par, const Double x0) {
  return par(0)*exp(-(x0-par(1))*(x0-par(1))/par(2)/par(2));
}

Double a1(const Vector<Double> &par, const Double x0) {
  Double g = (x0-par(1))/par(2);
  return par(0)*exp(-g*g);
}

Double a2(const Vector<Double> &par, const Double x0) {
  Double y(x0);
  y -= par(1);
  y /= par(2);
  y *= y;
  y *= -1.0;
  y = exp(y);
  y *= par(0);
  return y;
}

AutoDiff<Double> a0(const Vector<AutoDiff<Double> > &par, const Double x0) {
  return par(0)*exp(-(x0-par(1))*(x0-par(1))/par(2)/par(2));
}

AutoDiff<Double> a1(const Vector<AutoDiff<Double> > &par, const Double x0) {
  AutoDiff<Double> g = (x0-par(1))/par(2);
  return par(0)*exp(-g*g);
}

AutoDiff<Double> a2(const Vector<AutoDiff<Double> > &par, const Double x0) {
  AutoDiff<Double> y(x0);
  y -= par(1);
  y /= par(2);
  y *= y;
  y *= -1.0;
  y = exp(y);
  y *= par(0);
  return y;
}

AutoDiff<Double> a0(const Vector<AutoDiff<Double> > &par,
		    const AutoDiff<Double> x0) {
  return par(0)*exp(-(x0-par(1))*(x0-par(1))/par(2)/par(2));
}

AutoDiff<Double> a1(const Vector<AutoDiff<Double> > &par,
		    const AutoDiff<Double> x0) {
  AutoDiff<Double> g = (x0-par(1))/par(2);
  return par(0)*exp(-g*g);
}

AutoDiff<Double> a2(const Vector<AutoDiff<Double> > &par,
		    const AutoDiff<Double> x0) {
  AutoDiff<Double> y(x0);
  y -= par(1);
  y /= par(2);
  y *= y;
  y *= -1.0;
  y = exp(y);
  y *= par(0);
  return y;
}

// Manual derivatives

Double mv(Vector<Double> &res, const Vector<Double> &par, const Double x) {
  res.resize(par.nelements());
  Double nm = (x-par(1))/par(2);
  Double e = exp(-nm*nm);
  Double val = par(0)*e;
  res(0) = e;
  res(1) = 2.0*val*nm/par(2);
  res(2) = res(1)*nm;
  return val;
}

int main(int argc, const char* argv[])
{      
  // Inputs
  cout << ">>>" << endl;
  Input inputs(1);
  inputs.version("$Id$");
  inputs.create("n", "100000", "n"); 
  inputs.readArguments(argc, argv);
  Int N = inputs.getInt("n"); 
  cout << "<<<" << endl;

  cout << "N = " << N << endl;

  // Parameters
  Vector<Double> par(3);
  par(0) = 1000;
  par(1) = 2;
  par(2) = 3;
  Double x = 4;
  AutoDiff<Double> xa(4, 3);
  Vector<AutoDiff<Double> > para(3);
  para(0) = AutoDiff<Double>(1000, 3, 0);
  para(1) = AutoDiff<Double>(2, 3, 1);
  para(2) = AutoDiff<Double>(3, 3, 2);
  Vector<Double> va(3);

  // Check results
  cout << "Values (g0: 1 line; g1: 2 lines; g2: RPN type):" << endl;
  cout << "g0: " << a0(par, x) << endl;
  cout << "g1: " << a1(par, x) << endl;
  cout << "g2: " << a2(par, x) << endl;
  cout << "Manual (mv): " << mv(va, par, x);
  cout << " " << va << endl;

  Gaussian1D<Double> g1d((Double(par(0))),
			   (Double(par(1))),
			   (Double(par(2))/(1.0/sqrt(log(16.0)))));

  // Autoderivatives
  cout << "AutoDiff (a0): " << a0(para, x) << endl;
  cout << "AutoDiff (a1): " << a1(para, x) << endl;
  cout << "AutoDiff (a2): " << a2(para, x) << endl;

  cout << "--------- Values original (OPTLIB  N=1000000/100000) ---------" <<
    "\ng0:        3.91 real        3.91 user           0 system"
    "\ng1:        2.77 real        2.77 user           0 system"
    "\ng2:         2.9 real        2.89 user           0 system"
    "\nmd:       18.06 real       18.05 user           0 system"
    "\nmv:        6.47 real        6.44 user           0 system"
    "\nao:        0.99 real        0.99 user           0 system"
    "\na0:        16.6 real        16.6 user           0 system"
    "\na1:        13.6 real        13.4 user           0 system"
    "\na2:        6.72 real        6.32 user           0 system" << endl;
  cout << "--------- Values original (OPT=1 N=1000000/100000) -----------" <<
    "\ng0:        1.32 real        1.31 user           0 system"
    "\ng1:        0.82 real        0.82 user           0 system"
    "\ng2:        0.83 real        0.83 user           0 system"
    "\nmd:       11.74 real       11.74 user           0 system"
    "\nmv:        1.21 real        1.22 user           0 system"
    "\nao:        0.93 real        0.93 user           0 system"
    "\na0:        6.81 real        6.78 user           0 system"
    "\na1:        5.27 real        5.27 user           0 system"
    "\na2:        1.88 real        1.88 user           0 system" << endl;
  cout << "--------- Function access; Rep (OPT=1 N=100000) ----" <<
    "\na0:        7.54 real        7.48 user           0 system"
    "\na1:        5.81 real         5.8 user           0 system"
    "\na2:        1.86 real        1.86 user           0 system" << endl;
  cout << "--------- Public access; Rep (OPT=1 N=100000) ------" <<
    "\na0:        6.49 real        6.18 user           0 system"
    "\na1:        4.84 real        4.63 user           0 system"
    "\na2:        1.52 real        1.43 user           0 system" << endl;
  cout << "--------- Pool simple        (OPT=1 N=100000) ------" <<
    "\na0:        5.83 real        5.83 user           0 system"
    "\na1:        4.58 real        4.58 user           0 system"
    "\na2:        1.61 real        1.61 user           0 system" << endl;
  cout << "--------- Pool no copy of temp (OPT=1 N=100000) ----" <<
    "\na0:        3.59 real        3.58 user           0 system"
    "\na1:        2.65 real        2.65 user           0 system"
    "\na2:        1.38 real        1.35 user           0 system" << endl;
  cout << "--------- Pool as g0 etc (OPT=1 N=100000) ----------" <<
    "\na0:        2.29 real        2.27 user           0 system"
    "\na1:        1.84 real        1.82 user           0 system"
    "\na2:        1.35 real        1.33 user           0 system" << endl;
  cout << "--------- Revamp; vector g0 etc (OPT=1 N=100000) ----------" <<
    "\na0:        1.49 real        1.49 user           0 system"
    "\na1:        1.09 real        1.09 user           0 system"
    "\na2:        0.77 real        0.77 user           0 system" << endl;
  
  // Loop values
  cout << "\nTiming values:" << endl;
  Timer tim;

  tim.mark();
  for (Int i=0; i<N; i++) {
    Double res = a0(par, x);
    res *= 1.0;
  }
  cout << "g0: ";
  tim.show();

  tim.mark();
  for (Int i=0; i<N; i++) {
    Double res = a1(par, x);
    res *= 1.0;
  }
  cout << "g1: ";
  tim.show();

  tim.mark();
  for (Int i=0; i<N; i++) {
    Double res = a2(par, x);
    res *= 1.0;
  }
  cout << "g2: ";
  tim.show();

  tim.mark();
  for (Int i=0; i<N; i++) {
    Double res = mv(va, par, x);
    res *= 1.0;
  }
  cout << "mv: ";
  tim.show();

  tim.mark();
  for (Int i=0; i<N; i++) {
    g1d(x);
  }
  cout << "ao: ";
  tim.show();

  ///  N /= 10;
  AutoDiff<Double> resa;

  for (uInt j=0; j<4; j++) {
    ///  for (uInt j=3; j<4; j++) {
    cout << endl << "--------- " << j << " derivatives: ";
    Vector<AutoDiff<Double> > para(3);
    if (j == 0) {
      para(0) = AutoDiff<Double>(1000);
      para(1) = AutoDiff<Double>(2);
      para(2) = AutoDiff<Double>(3);
    } else if (j==1) {
      para(0) = AutoDiff<Double>(1000, j, 0);
      para(1) = AutoDiff<Double>(2, j);
      para(2) = AutoDiff<Double>(3, j);
    } else if (j==2) {
      para(0) = AutoDiff<Double>(1000, j, 0);
      para(1) = AutoDiff<Double>(2, j, 1);
      para(2) = AutoDiff<Double>(3, j);
    } else {
      para(0) = AutoDiff<Double>(1000, 3, 0);
      para(1) = AutoDiff<Double>(2, 3, 1);
      para(2) = AutoDiff<Double>(3, 3, 2);
    }
    cout << "N = " << N << " (at x=Double)" << endl;

    tim.mark();
    for (Int i=0; i<N; i++) resa = a0(para, x);
    cout << "a0: "; tim.show();
    tim.mark();
    for (Int i=0; i<N; i++) resa = a1(para, x);
    cout << "a1: "; tim.show();
    tim.mark();
    for (Int i=0; i<N; i++) resa = a2(para, x);
    cout << "a2: "; tim.show();

    if (j == 3) { ///
      cout << "N = " << N << " (at x=AutoDiff)" << endl;
      AutoDiff<Double> xa(4, j);
      
      tim.mark();
      for (Int i=0; i<N; i++) resa = a0(para, xa);
      cout << "a0: "; tim.show();
      tim.mark();
      for (Int i=0; i<N; i++) resa = a1(para, xa);
      cout << "a1: "; tim.show();
      tim.mark();
      for (Int i=0; i<N; i++) resa = a2(para, xa);
      cout << "a2: "; tim.show();
    } ///
  }

}
