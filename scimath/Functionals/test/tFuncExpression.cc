//# tFuncExpression.cc: This program test the functional run-time expressions
//# Copyright (C) 2000,2002
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

//# Includes
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/scimath/Functionals/FuncExpression.h>
#include <casacore/scimath/Functionals/FuncExprData.h>
#include <casacore/scimath/Functionals/CompiledFunction.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {

  try {
    cout << "------------ test functional expressions ------------" << endl;

    cout << "--- Check base -----" << endl;
    // Make an operator base
    FuncExprData base;
    cout << base << endl;

    cout << "--- Check expression syntax ----" << endl;
    const uInt n=27;
    String exprlist[n] = {
      String("+-(25*30+2)--(75+2)"),
      String("1+2-3"),
      String("1+2/3"),
      String("2^2^3"),
      String("1+(2+3"),
      String("1"),
      String(""),
      String("pi"),
      String("sin(1)"),
      String("cop(1)"),
      String("pi(2)+1.5"),
      String("sin(1,2)"),
      String("2*p"),
      String("2*p1"),
      String("2*p[1]"),
      String("2*x0"),
      String("sin(sin(2))"),
      String("x==0"),
      String("(x==0)+1"),
      String("(x==0)+1"),
      String("((x==0) * 1)+((x!=0) * sin(x+(x==0)*1)/(x+(1)))"),
      String("1+(1==2)?5:8+20"),
      String("1+(2==2)?5:(8+20)"),
      String("1+((1==2)?5:8)+20"),
      String("1+((2==2)?5:(8+20))"),
      String("erf(1)"),
      String("erfc(1)")
    };
    for (uInt i=0; i<n; ++i) {
      FuncExpression expr;
      String myexpr = exprlist[i];
      cout << "Expression: '" << myexpr << "'" << endl; 
      if (!expr.create(myexpr)) {
	cout << expr.errorMessage() << endl;
      }
      cout << expr;
      Double res;
      cout << "Value: ";
      if (!expr.exec(res)) {
	cout << expr.errorMessage() << endl;
      } else cout << res << endl;
      cout << "----------------------------------------------------" << endl;
    }
    for (uInt i=0; i<n; ++i) {
      CompiledFunction<Double> expr;
      String myexpr = exprlist[i];
      cout << "Expression: '" << myexpr << "'" << endl; 
      if (!expr.setFunction(myexpr)) {
	cout << expr.errorMessage() << endl;
      }
      if (expr.nparameters() > 0) expr[0] = 1.5;
      if (expr.nparameters() > 1) expr[1] = 2.5;
      cout << "Value(3.5, 0): ";
      cout << expr(3.5) << ", " << expr(0.0) << endl;
      cout << "----------------------------------------------------" << endl;
    }
    for (uInt i=0; i<n; ++i) {
      CompiledFunction<AutoDiff<Double> > expr;
      String myexpr = exprlist[i];
      cout << "Expression: '" << myexpr << "'" << endl; 
      if (!expr.setFunction(myexpr)) {
	cout << expr.errorMessage() << endl;
      }
      if (expr.nparameters() > 0) {
	expr[0] = AutoDiff<Double>(1.5, expr.nparameters(), 0);
      }
      if (expr.nparameters() > 1) {
	expr[1] = AutoDiff<Double>(2.5, expr.nparameters(), 1);
      }
      cout << "Value(3.5, 0): ";
      cout << expr(3.5) << ", " << expr(0.0) << endl;
      cout << "----------------------------------------------------" << endl;
    }
  }  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  }
}
