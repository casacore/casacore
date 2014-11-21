//# tExprUnitNode.cc: Test program for unit handling in selection expressions
//# Copyright (C) 2009
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

#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for unit handling in selection expressions.
// </summary>

Bool foundError = False;


void checkScaBool (const String& str, const TableExprId& exprid,
		   const TableExprNode& expr,
		   const Bool& value)
{
  cout << "checkScaBool " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpBool);
  AlwaysAssertExit (expr.unit().getName().empty());
  Bool val;
  expr.get (exprid, val);
  if (val != value) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkScaInt (const String& str, const TableExprId& exprid,
                  const TableExprNode& expr,
                  const Int& value)
{
  cout << "checkScaInt " << str << endl;
  AlwaysAssertExit (expr.dataType() == TpInt);
  AlwaysAssertExit (expr.unit().getName().empty());
  Int64 val;
  expr.get (exprid, val);
  if (val != value) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

void checkScaDouble (const String& str, const TableExprId& exprid,
		     const TableExprNode& expr,
		     const Double& value,
                     const String& unit)
{
  cout << "checkScaDouble " << str << ' ' << expr.unit().getName() << endl;
  AlwaysAssertExit (expr.dataType() == TpDouble);
  AlwaysAssertExit (expr.unit().getName() == unit);
  Double val;
  expr.get (exprid, val);
  if (!near (val,  value, 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected " << value << endl;
  }
}

#define checkFailure(STR,EXPR)\
{\
  bool failed = False;\
  try {\
    TableExprNode n(EXPR);\
  } catch (std::exception&) {\
    failed = True;\
  }\
  if (!failed) {\
    cout << STR << ": was expected to fail, but did not" << endl;\
  }\
}


void doIt()
{
  TableExprNode e30(30);
  e30.useUnit("deg");
  TableExprNode e1(2);
  checkScaInt ("int 2", 0, e1, 2);
  TableExprNode e2 = e1.useUnit (Unit("cm"));
  checkScaDouble ("2 cm 1", 0, e1, 2, "cm");
  checkScaDouble ("2 cm 2", 0, e2, 2, "cm");
  TableExprNode e3 = e2.useUnit (Unit("m"));
  checkScaDouble ("2 cm 3", 0, e2, 2, "cm");
  checkScaDouble ("2 cm m", 0, e3, 0.02, "m");
  // Math
  checkScaDouble ("1+e3", 0, 1+e3, 1.02, "m");
  checkScaDouble ("e2+e3", 0, e2+e3, 4, "cm");
  checkScaDouble ("e3-2*e2", 0, e3-2*e2, -0.02, "m");
  checkScaDouble ("e2*e3", 0, e2*e3, 0.04, "cm.m");
  checkScaDouble ("e3*e2", 0, e3*e2, 0.04, "m.cm");
  checkScaDouble ("e3/e2", 0, e3/e2, 0.01, "m/(cm)");
  checkScaDouble ("e3%e2", 0, e3%e2, 0.02, "m");
  // Comparison
  checkScaBool ("e3*e2==e2*e3", 0, e3*e2==e2*e3, True);
  checkScaBool ("e2==e3", 0, e2+0.00001==e3, False);
  checkScaBool ("e2>=e3", 0, e2+0.00001>=e3, True);
  checkScaBool ("e2>e3", 0, e2+0.00001>e3, True);
  checkScaBool ("e2<=e3", 0, e2-0.00001<=e3, True);
  checkScaBool ("e2<e3", 0, e2-0.00001<e3, True);
  checkScaBool ("e2!=e3", 0, e2+0.00001!=e3, True);
  // Functions
  checkScaBool ("near2(e2,e3)", 0, near(e2,e3), True);
  checkScaBool ("near3(e2,e3)", 0, near(e2,e3,1e-10), True);
  checkScaBool ("nearAbs2(e2,e3)", 0, nearAbs(e2,e3), True);
  checkScaBool ("nearAbs3(e2,e3)", 0, nearAbs(e2,e3,1e-10), True);
  checkScaDouble ("min(e2,e3)", 0, min(e2,e3), 2, "cm");
  checkScaDouble ("max(e3,e2)", 0, max(e3,e2), 0.02, "m");
  checkScaDouble ("sin(30)", 0, sin(e30), 0.5, "");
  checkScaDouble ("cos(30)", 0, cos(e30), 0.5*sqrt(3.), "");
  checkScaDouble ("tan(30)", 0, tan(e30), 1./sqrt(3.), "");
  checkScaDouble ("sinh(e2)", 0, sinh(e2), (exp(2)-exp(-2))/2, "");
  checkScaDouble ("cosh(e2)", 0, cosh(e2), (exp(2)+exp(-2))/2, "");
  checkScaDouble ("tanh(e2)", 0, tanh(e2), (exp(4)-1)/(exp(4)+1), "");
  checkScaDouble ("asin(sin(30))", 0, asin(sin(e30)), C::pi/6, "rad");
  checkScaDouble ("acos(cos(30))", 0, acos(cos(e30)), C::pi/6, "rad");
  checkScaDouble ("atan(tan(30))", 0, atan(tan(e30)), C::pi/6, "rad");
  checkScaDouble ("atan2(1,1)", 0, atan2(TableExprNode(1),1), C::pi/4, "rad");
  checkScaDouble ("square(e2)", 0, square(e2), 4, "(cm)2");
  checkScaDouble ("cube(e2)", 0, cube(e2), 8, "(cm)3");
  // sqrt returns basic SI units.
  checkScaDouble ("sqrt(square(e2))", 0, sqrt(square(e2)), 0.02, "m");
  checkScaDouble ("sqrt(e2*e2)", 0, sqrt(e2*e2), 0.02, "m");
  checkScaDouble ("pow(e2,2)", 0, pow(e2,2), 4, "");
  checkScaDouble ("log(e2)", 0, log(e2), log(2.), "");
  checkScaDouble ("log10(e2)", 0, log10(e2), log10(2.), "");
  checkScaDouble ("exp(e2)", 0, exp(e2), exp(2.), "");
  checkScaDouble ("norm(e2)", 0, norm(e2), 4., "(cm)2");
  checkScaDouble ("abs(e2)", 0, abs(e2), 2., "cm");
  checkScaDouble ("real(e2)", 0, real(e2), 2., "cm");
  checkScaDouble ("imag(e2)", 0, imag(e2), 0., "cm");
  checkScaDouble ("arg(e2)", 0, arg(e2), 0., "rad");
  checkScaDouble ("conj(e2)", 0, conj(e2), 2., "cm");
  checkScaDouble ("round(e2)", 0, round(e2), 2., "cm");
  checkScaDouble ("floor(e2)", 0, floor(e2), 2., "cm");
  checkScaDouble ("ceil(e2)", 0, ceil(e2), 2., "cm");
  checkScaDouble ("sign(e2)", 0, sign(e2), 1, "");
  checkScaInt ("integer(e2)", 0, integer(e2), 2);
  checkScaDouble ("sum(e2)", 0, sum(e2), 2., "cm");
  checkScaDouble ("product(e2)", 0, product(e2), 2., "");
  checkScaDouble ("sumSquare(e2)", 0, sumSquare(e2), 4., "(cm)2");
  checkScaDouble ("min(e2)", 0, min(e2), 2., "cm");
  checkScaDouble ("max(e2)", 0, max(e2), 2., "cm");
  checkScaDouble ("mean(e2)", 0, mean(e2), 2., "cm");
  checkScaDouble ("variance(e2)", 0, variance(e2), 0., "(cm)2");
  checkScaDouble ("stddev(e2)", 0, stddev(e2), 0., "cm");
  checkScaDouble ("avdev(e2)", 0, avdev(e2), 0., "cm");
  checkScaDouble ("rms(e2)", 0, rms(e2), 2., "cm");
  checkScaDouble ("median(e2)", 0, median(e2), 2., "cm");
  checkScaBool ("isNaN(e2)", 0, isNaN(e2), False);
  checkScaInt ("ndim(e2)", 0, ndim(e2), 0);
  checkScaDouble ("time", 0, time("17Apr09/12:00:00"), C::pi, "rad");
  checkScaDouble ("time", 0, time("1Jan08/12:00:00"), C::pi, "rad");
  checkScaDouble ("date-date", 0, date("17Apr2009")-"1Apr2009", 16., "d"); 
  checkScaDouble ("iif(T,e2,e3+1)", 0, iif(True,e2,e3+1), 2., "cm"); 
  checkScaDouble ("iif(F,e2,e3+1)", 0, iif(False,e2,e3+1), 102., "cm"); 
  checkScaDouble ("iif(T,e3,e2+1)", 0, iif(True,e3,e2+1), 0.02, "m"); 
  checkScaDouble ("iif(F,e3,e2+1)", 0, iif(False,e3,e2+1), 0.03, "m"); 

  TableExprNode s2(3);
  s2.useUnit ("rad");
  checkFailure ("e2+s2", e2+s2);
  checkFailure ("e2-s2", e2-s2);
  checkFailure ("sin(e2)", sin(e2));
  checkFailure ("cos(e2)", cos(e2));
  checkFailure ("tan(e2)", tan(e2));
}


int main()
{
  try {
    doIt();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    return 1;
  }
  if (foundError) {
    cout << "Some unexpected results were found" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
