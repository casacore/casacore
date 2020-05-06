//# tTableGramFunc.cc: This program tests TaQL functions
//# Copyright (C) 2018
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

#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for all TaQL functions.
// They are tested in two ways:
// - The correct way where it is checked if the result is correct and has the
//   correct data type and unit.
// - Various incorrect ways where it is checked if the expected exception is thrown.
// The functions are called for scalars and arrays and combinations where applicable.
// At the end it prints the total number of tests done.
// </summary>

int ntest=0;


int checkScaBool (const String& func, const String& arg, Bool expResult)
{
  ntest++;
  String comm = "using style python calc " + func + '(' + arg + ')';
  try {
    TaQLResult result = tableCommand (comm);
    TableExprNode node = result.node();
    if (node.dataType() != TpBool) {
      cout << "Bool error in evaluating: " + comm << endl;
      cout << " expected data type Bool, found "
           << ValType::getTypeStr(node.dataType()) << endl;
      return 1;
    }
    if (node.getBool(0) != expResult  ||  node.unit().getName() != "") {
      cout << "Bool error in evaluating: " + comm << endl;
      cout << " expected " << expResult << endl;
      cout << " found    " << node.getBool(0) << ' '
           << node.unit().getName() << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm << endl;
    return 1;
  }
  return 0;
}

int checkScaInt (const String& func, const String& arg, Int expResult)
{
  ntest++;
  String comm = "using style python calc " + func + '(' + arg + ')';
  try {
    TaQLResult result = tableCommand (comm);
    TableExprNode node = result.node();
    if (node.dataType() != TpInt64) {
      cout << "Int error in evaluating: " + comm << endl;
      cout << " expected data type Int, found "
           << ValType::getTypeStr(node.dataType()) << endl;
      return 1;
    }
    if (node.getInt(0) != expResult  ||  node.unit().getName() != "") {
      cout << "Int error in evaluating: " + comm << endl;
      cout << " expected " << expResult << endl;
      cout << " found    " << node.getInt(0) << ' '
           << node.unit().getName() << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm << endl;
    return 1;
  }
  return 0;
}

int checkScaDouble (const String& func, const String& arg, Double expResult,
                    const String& unit = String(), double tol=1e-5)
{
  ntest++;
  String comm = "using style python calc " + func + '(' + arg + ')';
  try {
    TaQLResult result = tableCommand (comm);
    TableExprNode node = result.node();
    if (node.dataType() != TpDouble) {
      cout << "Double error in evaluating: " + comm << endl;
      cout << " expected data type Double, found "
           << ValType::getTypeStr(node.dataType()) << endl;
      return 1;
    }
    if (!near(node.getDouble(0), expResult, tol)  ||  node.unit().getName() != unit) {
      cout << "Double error in evaluating: " + comm << endl;
      cout << " expected " << expResult << endl;
      cout << " found    " << node.getDouble(0) << ' '
           << node.unit().getName() << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm << endl;
    return 1;
  }
  return 0;
}

int checkScaDComplex (const String& func, const String& arg, DComplex expResult)
{
  ntest++;
  String comm = "using style python calc " + func + '(' + arg + ')';
  try {
    TaQLResult result = tableCommand (comm);
    TableExprNode node = result.node();
    if (node.dataType() != TpDComplex) {
      cout << "DComplex error in evaluating: " + comm << endl;
      cout << " expected data type DComplex, found "
           << ValType::getTypeStr(node.dataType()) << endl;
      return 1;
    }
    if (!near(node.getDComplex(0), expResult)  ||  node.unit().getName() != "") {
      cout << "DComplex error in evaluating: " + comm << endl;
      cout << " expected " << expResult << endl;
      cout << " found    " << node.getDComplex(0) << ' '
           << node.unit().getName() << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm << endl;
    return 1;
  }
  return 0;
}

int checkScaDateTime (const String& func, const String& arg, MVTime expResult)
{
  ntest++;
  String comm = "using style python calc " + func + '(' + arg + ')';
  try {
    TaQLResult result = tableCommand (comm);
    TableExprNode node = result.node();
    if (node.dataType() != TpQuantity) {
      cout << "DateTime error in evaluating: " + comm << endl;
      cout << " expected data type Quantity, found "
           << ValType::getTypeStr(node.dataType()) << endl;
      return 1;
    }
    if (!(node.getDate(0) == expResult)  ||  node.unit().getName() != "") {
      MVTime::setFormat (MVTime::YMD);
      cout << "DateTime error in evaluating: " + comm << endl;
      cout << " expected " << expResult << endl;
      cout << " found    " << node.getDate(0) << ' '
           << node.unit().getName() << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm << endl;
    return 1;
  }
  return 0;
}

int checkScaString (const String& func, const String& arg, String expResult)
{
  ntest++;
  String comm = "using style python calc " + func + '(' + arg + ')';
  try {
    TaQLResult result = tableCommand (comm);
    TableExprNode node = result.node();
    if (node.dataType() != TpString) {
      cout << "String error in evaluating: " + comm << endl;
      cout << " expected data type String, found "
           << ValType::getTypeStr(node.dataType()) << endl;
      return 1;
    }
    if (node.getString(0) != expResult  ||  node.unit().getName() != "") {
      cout << "String error in evaluating: " + comm << endl;
      cout << " expected " << expResult << endl;
      cout << " found    " << node.getString(0) << ' '
           << node.unit().getName() << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm << endl;
    return 1;
  }
  return 0;
}

int checkArrBool (const String& func, const String& arg, const String& expResult)
{
  ntest++;
  String comm1 = "using style python calc " + func + '(' + arg + ')';
  String comm2 = "using style python calc " + expResult;
  try {
    TaQLResult result1 = tableCommand (comm1);
    TableExprNode node1 = result1.node();
    TaQLResult result2 = tableCommand (comm2);
    TableExprNode node2 = result2.node();
    if (node1.dataType() != TpBool) {
      cout << "Bool Array error in evaluating: " + comm1 << endl;
      cout << " expected data type Bool, found "
           << ValType::getTypeStr(node1.dataType()) << endl;
      return 1;
    }
    if (! allEQ (node1.getArrayBool(0), node2.getArrayBool(0))) {
      cout << "Bool Array error in evaluating: " + comm1 << endl;
      cout << " expected " << node2.getArrayBool(0) << endl;
      cout << " found    " << node1.getArrayBool(0) << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm1 << endl;
    return 1;
  }
  return 0;
}

int checkArrInt (const String& func, const String& arg, const String& expResult)
{
  ntest++;
  String comm1 = "using style python calc " + func + '(' + arg + ')';
  String comm2 = "using style python calc " + expResult;
  try {
    TaQLResult result1 = tableCommand (comm1);
    TableExprNode node1 = result1.node();
    TaQLResult result2 = tableCommand (comm2);
    TableExprNode node2 = result2.node();
    if (node1.dataType() != TpInt64) {
      cout << "Int Array error in evaluating: " + comm1 << endl;
      cout << " expected data type Int, found "
           << ValType::getTypeStr(node1.dataType()) << endl;
      return 1;
    }
    if (! allEQ (node1.getArrayInt(0), node2.getArrayInt(0))) {
      cout << "Int Array error in evaluating: " + comm1 << endl;
      cout << " expected " << node2.getArrayInt(0) << endl;
      cout << " found    " << node1.getArrayInt(0) << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm1 << endl;
    return 1;
  }
  return 0;
}

int checkArrDouble (const String& func, const String& arg, const String& expResult)
{
  ntest++;
  String comm1 = "using style python calc " + func + '(' + arg + ')';
  String comm2 = "using style python calc " + expResult;
  try {
    TaQLResult result1 = tableCommand (comm1);
    TableExprNode node1 = result1.node();
    TaQLResult result2 = tableCommand (comm2);
    TableExprNode node2 = result2.node();
    if (node1.dataType() != TpDouble) {
      cout << "Double Array error in evaluating: " + comm1 << endl;
      cout << " expected data type Double, found "
           << ValType::getTypeStr(node1.dataType()) << endl;
      return 1;
    }
    if (! allNear (node1.getArrayDouble(0), node2.getArrayDouble(0), 1e-5)) {
      cout << "Double Array error in evaluating: " + comm1 << endl;
      cout << " expected " << node2.getArrayDouble(0) << endl;
      cout << " found    " << node1.getArrayDouble(0) << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm1 << endl;
    return 1;
  }
  return 0;
}

int checkArrDComplex (const String& func, const String& arg, const String& expResult)
{
  ntest++;
  String comm1 = "using style python calc " + func + '(' + arg + ')';
  String comm2 = "using style python calc " + expResult;
  try {
    TaQLResult result1 = tableCommand (comm1);
    TableExprNode node1 = result1.node();
    TaQLResult result2 = tableCommand (comm2);
    TableExprNode node2 = result2.node();
    if (node1.dataType() != TpDComplex) {
      cout << "DComplex Array error in evaluating: " + comm1 << endl;
      cout << " expected data type DComplex, found "
           << ValType::getTypeStr(node1.dataType()) << endl;
      return 1;
    }
    if (! allNear (node1.getArrayDComplex(0), node2.getArrayDComplex(0), 1e-10)) {
      cout << "DComplex Array error in evaluating: " + comm1 << endl;
      cout << " expected " << node2.getArrayDComplex(0) << endl;
      cout << " found    " << node1.getArrayDComplex(0) << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm1 << endl;
    return 1;
  }
  return 0;
}

int checkArrString (const String& func, const String& arg, const String& expResult)
{
  ntest++;
  String comm1 = "using style python calc " + func + '(' + arg + ')';
  String comm2 = "using style python calc " + expResult;
  try {
    TaQLResult result1 = tableCommand (comm1);
    TableExprNode node1 = result1.node();
    TaQLResult result2 = tableCommand (comm2);
    TableExprNode node2 = result2.node();
    if (node1.dataType() != TpString) {
      cout << "String Array error in evaluating: " + comm1 << endl;
      cout << " expected data type String, found "
           << ValType::getTypeStr(node1.dataType()) << endl;
      return 1;
    }
    if (! allEQ (node1.getArrayString(0), node2.getArrayString(0))) {
      cout << "String Array error in evaluating: " + comm1 << endl;
      cout << " expected " << node2.getArrayString(0) << endl;
      cout << " found    " << node1.getArrayString(0) << endl;
      return 1;
    }
  } catch (const AipsError& x) {
    cout << x.getMesg() << endl;
    cout << "    Unexpected exception in: " << comm1 << endl;
    return 1;
  }
  return 0;
}

int checkArrDouble1 (const String& func, const String& arg1, const String& arg2)
{
  // Check an array function by comparing with its scalar counterpart.
  // E.g., cos([1,2]) and [cos(1),cos(2)]
  return checkArrDouble (func, '[' + arg1 + ',' + arg2 + ']',
                         '[' + func + '(' + arg1 + ")," + func + '(' + arg2 + ")]");
}

int checkArrDComplex1 (const String& func, const String& arg1, const String& arg2)
{
  return checkArrDComplex (func, '[' + arg1 + ',' + arg2 + ']',
                           '[' + func + '(' + arg1 + ")," + func + '(' + arg2 + ")]");
}

int checkArrInt2 (const String& func, const String& arg1, const String& arg2,
                  const String& arg3)
{
  // Check an array function with 2 arguments by comparing with its scalar counterpart.
  int nfail = 0;
  // arr-arr
  nfail += checkArrInt (func, '[' + arg1 + ',' + arg2 + "],[" + arg2 + ',' + arg3 + ']',
                        '[' + func + '(' + arg1 + ',' + arg2 + "),"
                        + func + '(' + arg2 + ',' + arg3 + ")]");
  // sca-arr
  nfail += checkArrInt (func, arg1 + ",[" + arg2 + ',' + arg3 + ']',
                        '[' + func + '(' + arg1 + ',' + arg2 + "),"
                        + func + '(' + arg1 + ',' + arg3 + ")]");
  // arr-sca
  nfail += checkArrInt (func, '[' + arg1 + ',' + arg2 + "]," + arg3,
                        '[' + func + '(' + arg1 + ',' + arg3 + "),"
                        + func + '(' + arg2 + ',' + arg3 + ")]");
  return nfail;
}

int checkArrDouble2 (const String& func, const String& arg1, const String& arg2,
                     const String& arg3)
{
  // Check an array function with 2 arguments by comparing with its scalar counterpart.
  int nfail = 0;
  // arr-arr
  nfail += checkArrDouble (func, '[' + arg1 + ',' + arg2 + "],[" + arg2 + ',' + arg3 + ']',
                           '[' + func + '(' + arg1 + ',' + arg2 + "),"
                           + func + '(' + arg2 + ',' + arg3 + ")]");
  // sca-arr
  nfail += checkArrDouble (func, arg1 + ",[" + arg2 + ',' + arg3 + ']',
                           '[' + func + '(' + arg1 + ',' + arg2 + "),"
                           + func + '(' + arg1 + ',' + arg3 + ")]");
  // arr-sca
  nfail += checkArrDouble (func, '[' + arg1 + ',' + arg2 + "]," + arg3,
                           '[' + func + '(' + arg1 + ',' + arg3 + "),"
                           + func + '(' + arg2 + ',' + arg3 + ")]");
  return nfail;
}

int checkExcp (const String& func, const String& arg,
               const String& msgPart=String())
{
  ntest++;
  String comm = "using style python calc " + func + '(' + arg + ')';
  try {
    tableCommand (comm);
  } catch (const AipsError& x) {
    if (msgPart.empty()) {
      cout << x.getMesg() << endl;
    } else if (x.getMesg().find (msgPart) == String::npos) {
      cout << x.getMesg() << endl;
      cout << "    Expected another exception in: " << comm << endl;
      return 1;
    }
    return 0;
  }
  cout << "Expected exception in: " << comm << endl;
  return 1;
}


int testScaBool()
{
  cout<<"  testing scalar Bool functions ..."<<endl;
  int nfail = 0;
  nfail += checkScaBool ("near", "2., 2.0000001, 1e-5", True);
  nfail += checkScaBool ("near", "2., 2.0000001", False);
  nfail += checkScaBool ("near", "2+3i, 2.0000001+3i", False);
  nfail += checkScaBool ("near", "2+3i, 2+3.0000001i", False);
  nfail += checkScaBool ("near", "2+3i, 2.0000001+3.0000001i, 1e-5", True);
  nfail += checkScaBool ("near", "2, 2+3i", False);
  nfail += checkScaBool ("nearAbs", "2, 3", False);
  nfail += checkScaBool ("nearAbs", "2, 3, 2", True);
  nfail += checkScaBool ("nearAbs", "2+3i, 2.0000001+3.0000001i", False);
  nfail += checkScaBool ("nearAbs", "2+3i, 2.0000001+3.0000001i, 1e-5", True);
  nfail += checkScaBool ("isnan", "2", False);
  nfail += checkScaBool ("isnan", "0/0", True);
  nfail += checkScaBool ("isinf", "2", False);
  nfail += checkScaBool ("isinf", "2/0", True);
  nfail += checkScaBool ("isfinite", "2", True);
  nfail += checkScaBool ("isnan", "2.", False);
  nfail += checkScaBool ("isinf", "2.", False);
  nfail += checkScaBool ("isfinite", "2.", True);
  nfail += checkScaBool ("isfinite", "2./0", False);
  nfail += checkScaBool ("isnan", "2i", False);
  nfail += checkScaBool ("isinf", "2i", False);
  nfail += checkScaBool ("isfinite", "2j", True);
  nfail += checkScaBool ("isfinite", "0j/0", False);
  nfail += checkScaBool ("isnull", "nullarray(1)", True);
  nfail += checkScaBool ("isnull", "array(1, [0])", False);
  nfail += checkScaBool ("iif", "T,F,T", False);
  nfail += checkScaBool ("iif", "F,F,T", True);
  nfail += checkScaBool ("bool", "F", False);
  nfail += checkScaBool ("bool", "T", True);
  nfail += checkScaBool ("bool", "0", False);
  nfail += checkScaBool ("bool", "-1", True);
  nfail += checkScaBool ("bool", "0.", False);
  nfail += checkScaBool ("bool", "2.5", True);
  nfail += checkScaBool ("bool", "0+0i", False);
  nfail += checkScaBool ("bool", "2.5+1j", True);
  nfail += checkScaBool ("bool", "date()", True);
  nfail += checkScaBool ("bool", "' F '", False);
  nfail += checkScaBool ("bool", "'y'", True);
  nfail += checkScaBool ("bool", "'0'", False);
  nfail += checkScaBool ("bool", "'-1'", True);
  return nfail;
}

int testScaInt()
{
  cout<<"  testing scalar Int functions ..."<<endl;
  int nfail = 0;
  nfail += checkScaInt ("sqr", "4", 16);
  nfail += checkScaInt ("cube", "4", 64);
  nfail += checkScaInt ("min", "-1,3", -1);
  nfail += checkScaInt ("max", "-6, -4", -4);
  nfail += checkScaInt ("norm", "-6", 36);
  nfail += checkScaInt ("abs", "-6", 6);
  nfail += checkScaInt ("int", "F", 0);
  nfail += checkScaInt ("int", "T", 1);
  nfail += checkScaInt ("int", "2", 2);
  nfail += checkScaInt ("int", "2.5", 2);
  nfail += checkScaInt ("integer", "'-3.7'", -3);
  nfail += checkScaInt ("sign", "-3", -1);
  nfail += checkScaInt ("sign", "0", 0);
  nfail += checkScaInt ("sign", "3", 1);
  nfail += checkScaInt ("sign", "-3", -1);
  nfail += checkScaInt ("sign", "0", 0);
  nfail += checkScaInt ("round", "3", 3);
  nfail += checkScaInt ("round", "-3", -3);
  nfail += checkScaInt ("floor", "3", 3);
  nfail += checkScaInt ("ceil", "3", 3);
  nfail += checkScaInt ("fmod", "8,3", 2);
  nfail += checkScaInt ("fmod", "-8,3", -2);
  nfail += checkScaInt ("fmod", "8,-3", 2);
  nfail += checkScaInt ("fmod", "-8,-3", -2);
  nfail += checkScaInt ("ndim", "1", 0);
  nfail += checkScaInt ("ndim", "[T]", 1);
  nfail += checkScaInt ("ndim", "[['a']]", 2);
  nfail += checkScaInt ("ndim", "nullarray(1)", 0);
  nfail += checkScaInt ("nelements", "1.", 1);
  nfail += checkScaInt ("nelements", "[[1+0i,2],[3,4]]", 4);
  nfail += checkScaInt ("nelements", "nullarray(1)", 0);
  nfail += checkScaInt ("strlength", "''", 0);
  nfail += checkScaInt ("strlength", "' a '", 3);
  nfail += checkScaInt ("rownr", "", 0);
  nfail += checkScaInt ("rowid", "", 0);
  nfail += checkScaInt ("iif", "T,2,3", 2);
  nfail += checkScaInt ("iif", "F,2,3", 3);
  return nfail;
}

int testScaDouble()
{
  cout<<"  testing scalar Double functions ..."<<endl;
  int nfail = 0;
  nfail += checkScaDouble ("pi", "", C::pi);
  nfail += checkScaDouble ("e", "", C::e);
  nfail += checkScaDouble ("c", "", C::c, "m/s");
  nfail += checkScaDouble ("sin", "0.5", sin(0.5));
  nfail += checkScaDouble ("sinh", "2", sinh(2));
  nfail += checkScaDouble ("cos", "0.6", cos(0.6));
  nfail += checkScaDouble ("cosh", "1.5", cosh(1.5));
  nfail += checkScaDouble ("exp", "0.5", sqrt(C::e));
  nfail += checkScaDouble ("log", "e()*e()", 2.);
  nfail += checkScaDouble ("log10", "0.1", -1.);
  nfail += checkScaDouble ("sqr", "-4.5", 4.5*4.5);
  nfail += checkScaDouble ("sqrt", "4", 2.);
  nfail += checkScaDouble ("sqrt", "4.", 2.);
  nfail += checkScaDouble ("pow", "1.6, 1.5", 1.6*sqrt(1.6));
  nfail += checkScaDouble ("conj", "0.1", 0.1);
  nfail += checkScaDouble ("square", "-4.5", 4.5*4.5);
  nfail += checkScaDouble ("cube", "-1.4", -1.4*1.4*1.4);
  nfail += checkScaDouble ("min", "-1.4,-5", -5.);
  nfail += checkScaDouble ("max", "1,2.", 2.);
  nfail += checkScaDouble ("max", "1.,2", 2.);
  nfail += checkScaDouble ("norm", "-6.1", 6.1*6.1);
  nfail += checkScaDouble ("abs", "-6.5", 6.5);
  nfail += checkScaDouble ("arg", "6", 0., "rad");
  nfail += checkScaDouble ("arg", "-6", C::pi, "rad");
  nfail += checkScaDouble ("real", "F", 0.);
  nfail += checkScaDouble ("real", "T", 1.);
  nfail += checkScaDouble ("real", "2", 2.);
  nfail += checkScaDouble ("real", "2.5", 2.5);
  nfail += checkScaDouble ("real", "'-2.7-1i'", -2.7);
  nfail += checkScaDouble ("real", "'-3.7'", -3.7);
  nfail += checkScaDouble ("imag", "2", 0.);
  nfail += checkScaDouble ("imag", "2.5", 0);
  nfail += checkScaDouble ("imag", "-2.7-1i", -1.);
  nfail += checkScaDouble ("asin", "0.5", asin(0.5), "rad");
  nfail += checkScaDouble ("acos", "0.65", acos(0.65), "rad");
  nfail += checkScaDouble ("atan", "0.3", atan(0.3), "rad");
  nfail += checkScaDouble ("atan2", "0.5,0.3", atan2(0.5,0.3), "rad");
  nfail += checkScaDouble ("tan", "1.2", tan(1.2));
  nfail += checkScaDouble ("tanh", "-1.2", tanh(-1.2));
  nfail += checkScaDouble ("round", "3.1", 3);
  nfail += checkScaDouble ("round", "3.5", 4);
  nfail += checkScaDouble ("round", "-3.1", -3);
  nfail += checkScaDouble ("round", "-3.5", -4);
  nfail += checkScaDouble ("floor", "3.1", 3);
  nfail += checkScaDouble ("floor", "-3.1", -4);
  nfail += checkScaDouble ("ceil", "3.1", 4);
  nfail += checkScaDouble ("ceil", "-3.1", -3);
  nfail += checkScaDouble ("fmod", "8.1,3.2", 1.7);
  nfail += checkScaDouble ("fmod", "-8.1,3.2", -1.7);
  nfail += checkScaDouble ("fmod", "8.1,-3.2", 1.7);
  nfail += checkScaDouble ("fmod", "-8.1,-3.2", -1.7);
  nfail += checkScaDouble ("rand", "", 0.5, "", 2);    // result is between 0 and 1
  nfail += checkScaDouble ("iif", "T, 2.1 deg, 3.1", 2.1, "deg");
  nfail += checkScaDouble ("iif", "F, 2.1 deg, 3.1", 3.1, "deg");
  nfail += checkScaDouble ("iif", "T, 2, 3.1", 2);
  nfail += checkScaDouble ("iif", "F, 2, 3.1", 3.1);
  nfail += checkScaDouble ("angdist", "[[34.3deg, 45deg]], [34.2deg, 47deg]", 0.0349276, "rad");
  nfail += checkScaDouble ("angdistx", "[325.7deg, -45deg], [-34.2deg, -47deg]", 0.0349276, "rad");
  nfail += checkScaDouble ("normangle", "325.7deg", -0.598648, "rad");
  nfail += checkScaDouble ("normangle", "3", 3.0, "rad");
  nfail += checkScaDouble ("variance", "[1,2]", 0.25);
  nfail += checkScaDouble ("stddev", "[1,2.]", sqrt(0.25));
  nfail += checkScaDouble ("variance", "[1+1i,2+3i]", 1.25);
  nfail += checkScaDouble ("stddev", "[1+1i,2+3i]", sqrt(1.25));
  nfail += checkScaDouble ("samplevariance", "[1,2]", 0.5);
  nfail += checkScaDouble ("samplestddev", "[1,2.]", sqrt(0.5));
  nfail += checkScaDouble ("samplevariance", "[1+1i,2+3i]", 2.5);
  nfail += checkScaDouble ("samplestddev", "[1+1i,2+3i]", sqrt(2.5));
  return nfail;
}

int testScaDComplex()
{
  cout<<"  testing scalar DComplex functions ..."<<endl;
  int nfail = 0;
  nfail += checkScaDComplex ("sin", "0.5+0.6i", sin(DComplex(0.5,0.6)));
  nfail += checkScaDComplex ("sinh", "2+2.1i", sinh(DComplex(2,2.1)));
  nfail += checkScaDComplex ("cos", "0.6+0.4i", cos(DComplex(0.6,0.4)));
  nfail += checkScaDComplex ("cosh", "1.5+0.6i", cosh(DComplex(1.5,0.6)));
  nfail += checkScaDComplex ("exp", "0.5+2i", exp(DComplex(0.5,2)));
  nfail += checkScaDComplex ("log", "1+3i", log(DComplex(1,3)));
  nfail += checkScaDComplex ("log10", "0.1-3i", log10(DComplex(0.1,-3)));
  nfail += checkScaDComplex ("sqrt", "8i", DComplex(2,2));
  nfail += checkScaDComplex ("pow", "1.6+2i, 1.5", DComplex(1.6,2)*sqrt(DComplex(1.6,2)));
  nfail += checkScaDComplex ("conj", "1+2i", DComplex(1,-2));
  nfail += checkScaDComplex ("sqr", "-4.5+3i", DComplex(-4.5,3) * DComplex(-4.5,3));
  nfail += checkScaDComplex ("cube", "1.4+1i", DComplex(1.4,1)*DComplex(1.4,1)*DComplex(1.4,1));
  nfail += checkScaDComplex ("min", "-1.4+1i, -5+8i", DComplex(-1.4,1));
  nfail += checkScaDComplex ("max", "-1.4+1i, -5+8i", DComplex(-5,8));
  nfail += checkScaDouble ("norm", "-5+8i", norm(DComplex(-5,8)));
  nfail += checkScaDouble ("abs", "-5+8i", abs(DComplex(-5,8)));
  nfail += checkScaDouble ("arg", "-5+8i", arg(DComplex(-5,8)), "rad");
  nfail += checkScaDComplex ("complex", "-1.4,1", DComplex(-1.4,1));
  nfail += checkScaDComplex ("complex", "'-1.4+10j'", DComplex(-1.4,10));
  nfail += checkScaDComplex ("complex", "'-1.4'", DComplex(-1.4,0));
  nfail += checkScaDComplex ("iif", "T,2+2i,3+4i", DComplex(2,2));
  nfail += checkScaDComplex ("iif", "F,2+2i,3+4j", DComplex(3,4));
  nfail += checkScaDComplex ("iif", "F,2,3+4j", DComplex(3,4));
  nfail += checkScaDComplex ("iif", "T,2,3+4j", DComplex(2,0));
  nfail += checkScaDComplex ("iif", "F,2.1,3+4j", DComplex(3,4));
  nfail += checkScaDComplex ("iif", "T,2.1,3+4j", DComplex(2.1,0));
  return nfail;
}

int testScaDateTime()
{
  cout<<"  testing scalar DateTime functions ..."<<endl;
  int nfail = 0;
  // Most functions work with a date/time as such and a date/time string.
  nfail += checkScaDateTime ("datetime", "'12Feb2017/12:00:00'", MVTime(2017,2,12,0.5));
  nfail += checkScaDateTime ("mjdtodate", "57796.5", MVTime(2017,2,12,0.5));
  nfail += checkScaDouble ("mjd", "'12Feb2017/12:00:00'", 57796.5, "d");
  nfail += checkScaDouble ("mjd", "12Feb2017/12:00:00", 57796.5, "d");
  nfail += checkScaDateTime ("date", "'12Feb2017/12:0:0'", MVTime(2017,2,12,0));
  nfail += checkScaDateTime ("date", "12Feb2017/12:0:0", MVTime(2017,2,12,0));
  nfail += checkScaDouble ("time", "'12Feb2017/12:0:0'", C::pi, "rad");
  nfail += checkScaDouble ("time", "12Feb2017/12:0:0", C::pi, "rad");
  nfail += checkScaInt ("year", "'12-Feb-17/12:0:0'", 2017);
  nfail += checkScaInt ("year", "12-Feb-17/12:0:0", 2017);
  nfail += checkScaInt ("month", "'12Feb2017/12:0:0'", 2);
  nfail += checkScaInt ("month", "12Feb2017/12:0:0", 2);
  nfail += checkScaInt ("day", "'12Feb2017/12:0:0'", 12);
  nfail += checkScaInt ("day", "12Feb2017/12:0:0", 12);
  nfail += checkScaString ("cmonth", "'12Feb2017/12:0:0'", "Feb");
  nfail += checkScaString ("cmonth", "12Feb2017/12:0:0", "Feb");
  nfail += checkScaInt ("weekday", "'11-Sep-18/12:0:0'", 2);
  nfail += checkScaInt ("dow", "11-Sep-18/12:0:0", 2);
  nfail += checkScaString ("cweekday", "'11-Sep-18/12:0:0'", "Tue");
  nfail += checkScaString ("cdow", "11-Sep-18/12:0:0", "Tue");
  nfail += checkScaInt ("week", "'11-Sep-18/12:0:0'", 37);
  nfail += checkScaInt ("week", "11-Sep-18/12:0:0", 37);
  nfail += checkScaString ("cdatetime", "'11-Sep-18/12:0:0'", "2018/09/11/12:00:00.000");
  nfail += checkScaString ("ctod", "11-Sep-18/12:0:0", "2018/09/11/12:00:00.000");
  nfail += checkScaString ("cdate", "'11-Sep-18/12:0:0'", "11-Sep-2018");
  nfail += checkScaString ("cdate", "11-Sep-18/12:0:0", "11-Sep-2018");
  nfail += checkScaString ("ctime", "'11-Sep-18/12:0:0'", "12:00:00.000");
  nfail += checkScaString ("ctime", "11-Sep-18/12:0:0", "12:00:00.000");
  // Also check some functions without argument which use current date/time.
  // The double comparison has 1e-5 accuracy which is fine enough for the slight
  // differences between the current date/time.
  // Note that functions like month can fail if the test is done on the edge of the month.
  nfail += checkScaDouble ("mjd(datetime", ")", MVTime(Time()), "d");
  nfail += checkScaInt ("month", "", Time().month());
  nfail += checkScaInt ("year", "", Time().year());
  // Also test with a datetime argument.
  nfail += checkScaInt ("month(datetime", ")", Time().month());
  nfail += checkScaInt ("year(datetime", ")", Time().year());
  nfail += checkScaDateTime ("iif", "T, 10-Sep-18/12:0:0, 12-sep-18/0:0", MVTime(2018,9,10,0.5));
  nfail += checkScaDateTime ("iif", "F, 10-Sep-18/12:0:0, 12-sep-18/0:0", MVTime(2018,9,12,0.));
  return nfail;
}

int testScaString()
{
  cout<<"  testing scalar String functions ..."<<endl;
  int nfail = 0;
  nfail += checkScaString ("upcase", "'abCD'", "ABCD");
  nfail += checkScaString ("upcase", "''", "");
  nfail += checkScaString ("downcase", "'abCD'", "abcd");
  nfail += checkScaString ("downcase", "''", "");
  nfail += checkScaString ("capitalize", "'abCD'", "Abcd");
  nfail += checkScaString ("capitalize", "'ab_CD'", "Ab_Cd");
  nfail += checkScaString ("capitalize", "''", "");
  nfail += checkScaString ("reversestring", "'ab_CD'", "DC_ba");
  nfail += checkScaString ("sreverse", "''", "");
  nfail += checkScaString ("trim", "'ab'", "ab");
  nfail += checkScaString ("trim", "'  a b  '", "a b");
  nfail += checkScaString ("trim", "''", "");
  nfail += checkScaString ("ltrim", "'ab'", "ab");
  nfail += checkScaString ("ltrim", "'  a b  '", "a b  ");
  nfail += checkScaString ("ltrim", "''", "");
  nfail += checkScaString ("rtrim", "'ab'", "ab");
  nfail += checkScaString ("rtrim", "'  a b  '", "  a b");
  nfail += checkScaString ("rtrim", "''", "");
  nfail += checkScaString ("substr", "'0123456789', 7", "789");
  nfail += checkScaString ("substr", "'0123456789', 7, 5", "789");
  nfail += checkScaString ("substr", "'0123456789', -4, 2", "67");
  nfail += checkScaString ("substr", "'0123456789', -11, 3", "012");
  nfail += checkScaString ("substr", "'', 10, 2", "");
  nfail += checkScaString ("replace", "'', 'a', 'b'", "");
  nfail += checkScaString ("replace", "'a123ab', 'a', 'xyz'", "xyz123xyzb");
  nfail += checkScaString ("replace", "'a123ab', 'a'", "123b");
  // Check the pretty print functions.
  nfail += checkScaString ("string", "'a123ab', 'x%10sx'", "x    a123abx");
  nfail += checkScaString ("str", "'a123ab', 'x %4s x'", "x a123ab x");
  nfail += checkScaString ("str", "'a123ab', 4", "a123");
  nfail += checkScaString ("str", "12.34567, 5", "12.3457");
  nfail += checkScaString ("str", "12.34567, 10", "   12.3457");
  nfail += checkScaString ("str", "12.34567, 10.5", "    12.346");
  nfail += checkScaString ("str", "-12.34567, 10.5", "   -12.346");
  nfail += checkScaString ("str", "12.34567, .6", "12.3457");
  nfail += checkScaString ("str", "'a123ab', 4", "a123");
  nfail += checkScaString ("string", "-3, 'x%4dx'", "x  -3x");
  nfail += checkScaString ("string", "-3, 'x%04dx'", "x-003x");
  nfail += checkScaString ("string", "-3.14, 'x%8.3fx'", "x  -3.140x");
  nfail += checkScaString ("string", "-3.14+2.5i, '(%8.3f,%8.3f)'", "(  -3.140,   2.500)");
  nfail += checkScaString ("str", "3h2m4.16, '%5.3f'", "0.794");    // in radians
  nfail += checkScaString ("str", "3h2m4.36, 'TIME'", "03:02:04");
  nfail += checkScaString ("str", "3h2m4.36, 'TIME|DIG2'", "+03:02:04");
  nfail += checkScaString ("str", "13h2m4.36, 'TIME|DIG2|8'", "-10:57:55.64");
  nfail += checkScaString ("str", "3h2m4.36, 'TIME|10'", "03:02:04.3600");
  nfail += checkScaString ("str", "3h2m4.36, 'ANGLE|10'", "+045.31.05.4000");
  nfail += checkScaString ("str", "11sep18/3h2m4.36, 'dmy|USE_SPACE'", "11-Sep-2018 03:02:04");
  nfail += checkScaString ("str", "11sep18 3h2m4.36, 'yMD|no_time'", "2018/09/11");
  nfail += checkScaString ("str", "11sep18T3h2m4.36Z, 'FITS'", "2018-09-11T03:02:04");
  nfail += checkScaString ("str", "11sep18 3h2m4.36, 'ISO|9'", "2018-09-11 03:02:04.360Z");
  nfail += checkScaString ("hms", "3h2m4.16", "03h02m04.160");
  nfail += checkScaString ("dms", "3h2m4.16", "+045d31m02.400");
  nfail += checkScaString ("iif", "T, 'abc', '1234'", "abc");
  nfail += checkScaString ("iif", "F, 'abc', '1234'", "1234");
  return nfail;
}

int testRegex()
{
  cout<<"  testing Regex functions ..."<<endl;
  int nfail = 0;
  nfail += checkScaBool ("'abcde' = pattern", "'ab*'", True);
  nfail += checkScaBool ("'fabcde' = pattern", "'ab*'", False);
  nfail += checkScaBool ("'abcde' = regex", "'ab*'", False);
  nfail += checkScaBool ("'abcde' = regex", "'ab.*'", True);
  nfail += checkScaBool ("'abcde' = sqlpattern", "'ab%'", True);
  nfail += checkScaBool ("'fabcde' = sqlpattern", "'ab%'", False);
  nfail += checkScaString ("replace", "'', pattern('*'), 'b'", "");
  nfail += checkScaString ("replace", "'a123ab', pattern('*'), 'xyz'", "xyz");
  nfail += checkScaString ("replace", "'a123ab', pattern('a'), '..'", "..123..b");
  nfail += checkScaString ("replace", "'a123ab', pattern('a')", "123b");
  return nfail;
}

int testCone()
{
  cout<<"  testing Cone functions ..."<<endl;
  int nfail = 0;
  // Note angdist 1,2 to 1.1,2.1 is 0.1100137372 rad
  // Note angdist 1,2 to 0.9,1.9 is 0.1065205744 rad
  nfail += checkScaBool ("anycone", "[1,2], [1.1,2.1,0.11]", False);
  nfail += checkScaBool ("anycone", "[1,2], [1.1,2.1], 0.110014", True);
  nfail += checkScaInt ("findcone", "[1,2], [1.1,2.1,0.11]", -1);
  nfail += checkScaInt ("findcone", "[1,2], [1.1,2.1], 0.110014", 0);
  nfail += checkScaBool ("cones", "[1,2], [1.1,2.1,0.11]", False);
  nfail += checkScaBool ("cones", "[1,2], [1.1,2.1], 0.110014", True);
  nfail += checkScaBool ("anycone", "[1,2], [1.1,2.1,0.11, 1.1,2.1,0.12]", True);
  nfail += checkScaBool ("anycone", "[1,2], [1.1,2.1],[0.11,0.12]", True);
  nfail += checkScaBool ("anycone", "[1,2], [1.1,2.1],[0.11,0.10]", False);
  nfail += checkScaInt ("findcone", "[1,2], [1.1,2.1,0.11, 1.1,2.1,0.12]", 1);
  nfail += checkScaInt ("findcone", "[1,2], [1.1,2.1, 0.9,1.9], [0.11,0.12]", 1);
  nfail += checkArrInt ("findcone", "[1,2,3,4], [1.1,2.1, 0.9,1.9], [0.11,0.12]", "[1,-1]");
  nfail += checkArrBool ("cones", "[1,2], [1.1,2.1,0.11, 1.1,2.1,0.12]", "[[F,T]]");
  nfail += checkArrBool ("cones", "[1,2], [1.1,2.1, 0.9,1.9], [0.11,0.12]", "[[[F,T],[T,T]]]");
  nfail += checkArrBool ("cones", "[1,2,3,4], [1.1,2.1, 0.9,1.9], [0.11,0.12]",
                         "[[[F,T],[T,T]],[[F,F],[F,F]]]");
  return nfail;
}

int testArrBool()
{
  cout<<"  testing array Bool functions ..."<<endl;
  int nfail = 0;
  nfail += checkArrBool ("near", "-1., [3., -1.]", "[F,T]");
  nfail += checkArrBool ("near", "[1.,-1], 1.", "[T,F]");
  nfail += checkArrBool ("near", "[1.,-1], [1.,-2.]", "[T,F]");
  nfail += checkArrBool ("near", "-1., [3., -1.009], 1e-2", "[F,T]");
  nfail += checkArrBool ("near", "[3., -1.009], -1, 1e-2", "[F,T]");
  nfail += checkArrBool ("near", "[1.,-0.3], [1.009,-0.309], 1e-2", "[T,F]");
  nfail += checkArrBool ("near", "-1., [3.+0i, -1.]", "[F,T]");
  nfail += checkArrBool ("near", "[1.+2i,-1], 1.+2i", "[T,F]");
  nfail += checkArrBool ("near", "[1.,-1+2i], [1.+0i,-2.]", "[T,F]");
  nfail += checkArrBool ("near", "-1., [3.+2i, -1.009], 1e-2", "[F,T]");
  nfail += checkArrBool ("near", "[3.+2i, -1.009], -1, 1e-2", "[F,T]");
  nfail += checkArrBool ("near", "[1.,-1+0i], [1.009,-1.1], 1e-2", "[T,F]");
  nfail += checkArrBool ("nearAbs", "-1., [3., -1.]", "[F,T]");
  nfail += checkArrBool ("nearabs", "[1.,-1], 1.", "[T,F]");
  nfail += checkArrBool ("nearabs", "[1.,-1], [1.,-2.]", "[T,F]");
  nfail += checkArrBool ("nearabs", "-1., [3., -1.009], 1e-2", "[F,T]");
  nfail += checkArrBool ("nearabs", "[3., -1.009], -1, 1e-2", "[F,T]");
  nfail += checkArrBool ("nearabs", "[1.,-0.3], [1.009,-0.309], 1e-2", "[T,T]");
  nfail += checkArrBool ("nearabs", "-1., [3.+0i, -1.]", "[F,T]");
  nfail += checkArrBool ("nearabs", "[1.+2i,-1], 1.+2i", "[T,F]");
  nfail += checkArrBool ("nearabs", "[1.,-1+2i], [1.+0i,-2.]", "[T,F]");
  nfail += checkArrBool ("nearabs", "-1., [3.+2i, -1.009], 1e-2", "[F,T]");
  nfail += checkArrBool ("nearabs", "[3.+2i, -1.009], -1, 1e-2", "[F,T]");
  nfail += checkArrBool ("nearabs", "[1.,-1+0i], [1.009,-1.1], 1e-2", "[T,F]");
  nfail += checkArrBool ("bool", "[F,T]", "[false,true]");
  nfail += checkArrBool ("bool", "[0,1,-3]", "[F,T,T]");
  nfail += checkArrBool ("bool", "[0.,1.,-3]", "[F,T,T]");
  nfail += checkArrBool ("bool", "[2.5, 0+0i]", "[T,F]");
  nfail += checkArrBool ("bool", "[2mar76, date()]", "[T,T]");
  nfail += checkArrBool ("bool", "['-','0','n','nO','f','faLse','', 'nx']", "[F,F,F,F,F,F,F,T]");
  return nfail;
}

int testArrInt()
{
  cout<<"  testing array Int functions ..."<<endl;
  int nfail = 0;
  nfail += checkArrInt2 ("min", "-1", "3", "1");
  nfail += checkArrInt2 ("max", "-1", "3", "1");
  nfail += checkArrInt ("array", "[[1,2,3]], 2,3", "[[1,2,3],[1,2,3]]");
  nfail += checkArrInt ("array", "[[1,2,3]], 1,3,2", "[[[1,2],[3,1],[2,3]]]");
  nfail += checkArrInt ("transpose", "[[1,2,3],[4,5,6],[7,8,9]]", "[[1,4,7],[2,5,8],[3,6,9]]");
  nfail += checkArrInt ("transpose", "[[[1,2],[4,5],[7,8]]], [2,0]",
                        "[[[1,4,7]],[[2,5,8]]]");      // axes get 2,0,1 -> shape 3,1,2
  nfail += checkArrInt ("areverse", "[[1,2,3],[4,5,6],[7,8,9]]", "[[9,8,7],[6,5,4],[3,2,1]]");
  nfail += checkArrInt ("areverse", "[[1,2,3],[4,5,6],[7,8,9]],0,1", "[[9,8,7],[6,5,4],[3,2,1]]");
  nfail += checkArrInt ("areverse", "[[1,2,3],[4,5,6],[7,8,9]], [1]", "[[3,2,1],[6,5,4],[9,8,7]]");
  nfail += checkArrInt ("areverse", "[[1,2,3],[4,5,6],[7,8,9]], 0", "[[7,8,9],[4,5,6],[1,2,3]]");
  nfail += checkArrInt ("resize", "[[1,2,3],[4,5,6]], [3,2]", "[[1,2],[4,5],[0,0]]");
  nfail += checkArrInt ("resize", "[[1,2,3],[4,5,6]], [2,6], 0", "[[1,1,2,2,3,3],[4,4,5,5,6,6]]");
  nfail += checkArrInt ("resize", "[[1,2,3],[4,5,6]], [2,6], 1", "[[1,2,3,1,2,3],[4,5,6,4,5,6]]");
  nfail += checkArrInt ("sums", "[[1,2,3],[4,5,6]], 1", "[6,15]");
  nfail += checkArrInt ("sums", "[[1,2,3],[4,5,6]], 0", "[5,7,9]");
  nfail += checkArrInt ("products", "[[1,2,3],[4,5,6]], 0", "[4,10,18]");
  nfail += checkArrInt ("sumsqrs", "[[1,2,3],[4,5,6]], 0", "[17,29,45]");
  nfail += checkArrInt ("mins", "[[[1,2,3],[4,5,6]]], 1", "[[1,2,3]]");
  nfail += checkArrInt ("maxs", "[[[1,2,3],[4,5,6]]], 0", "[[1,2,3],[4,5,6]]");
  nfail += checkArrInt ("shape", "[1,2]", "[2]");
  nfail += checkArrInt ("shape", "[[date(),date()]]", "[1,2]");
  return nfail;
}

int testArrDouble()
{
  cout<<"  testing array Double functions ..."<<endl;
  int nfail = 0;
  nfail += checkArrDouble1 ("sin", "1.1rad", "34deg");
  nfail += checkArrDouble1 ("cos", "1.1rad", "34deg");
  nfail += checkArrDouble1 ("tan", "1.1rad", "34deg");
  nfail += checkArrDouble1 ("sinh", "1.1", "34");
  nfail += checkArrDouble1 ("cosh", "1.1", "34");
  nfail += checkArrDouble1 ("tanh", "1.1", "34");
  nfail += checkArrDouble1 ("asin", "0.5", "-0.6");
  nfail += checkArrDouble1 ("acos", "0.5", "-0.6");
  nfail += checkArrDouble1 ("atan", "0.5", "-0.6");
  nfail += checkArrDouble2 ("atan2", "-0.9", "0.45", "0.9");
  nfail += checkArrDouble2 ("min", "-1.", "3.", "1.");
  nfail += checkArrDouble2 ("max", "-1.", "3.", "1.");
  nfail += checkArrDouble ("sums", "[[1.1,2,3],[4,5,6]], 1", "[6.1,15]");
  nfail += checkArrDouble ("sums", "[[1.1,2,3],[4,5,6]], 0", "[5.1,7,9]");
  nfail += checkArrDouble ("products", "[[1,2,3],[4,5.,6]], 0", "[4,10,18]");
  nfail += checkArrDouble ("sumsqrs", "[[1,2,3],[4,5,6.]], 0", "[17,29,45]");
  nfail += checkArrDouble ("mins", "[[[1,2,3],[4,5,6.]]], 1", "[[1,2,3]]");
  nfail += checkArrDouble ("maxs", "[[[1,2,3],[4,5,6.]]], 0", "[[1,2,3],[4,5,6]]");
  nfail += checkArrDouble ("means", "[[1,2,3],[4,5,6.]], 0", "[2.5,3.5,4.5]");
  nfail += checkArrDouble ("variances", "[[1,2,3],[4,5,7.]], 1", "[2./3,14./9]");
  nfail += checkArrDouble ("stddevs", "[[1,2,3],[4,5,7.]], 1", "[sqrt(2./3),sqrt(14./9)]");
  nfail += checkArrDouble ("samplevariances", "[[1,2,3],[4,5,7.]], 1", "[1,7./3]");
  nfail += checkArrDouble ("samplestddevs", "[[1,2,3],[4,5,7.]], 1", "[1,sqrt(7./3)]");
  nfail += checkArrDouble ("avdevs", "[[1,2,3],[4,5,7.]], 0", "[1.5,1.5,2]");
  nfail += checkArrDouble ("rmss", "[[1,2,3],[4,5,7.]], 1", "[sqrt(14./3), sqrt(90./3)]");
  nfail += checkArrDouble ("medians", "[[1,2,3],[4,5,6.]], 1", "[2,5]");
  nfail += checkArrDouble ("fractiles", "[[1,2,3],[4,5,6.]], 0.5, [1]", "[2,5]");
  nfail += checkArrDouble ("boxedsum", "[[1.1,2,3],[4,5,6]], [2,2]", "[[12.1,9]]");
  nfail += checkArrDouble ("angdist", "[34.3deg, 45deg, 34.2deg,45deg], [0.5,0.6,0.7,0.8]",
                           "[0.200206,0.0738066154]");
  nfail += checkArrDouble ("angdistx", "[34.3deg, 45deg, 34.2deg,45deg], [0.5,0.6,0.7,0.8]",
                           "[[0.2002061307,0.1997055276],[0.0726069829,0.0738066154]]");
  nfail += checkArrDouble ("normangle", "[-12:12:2]",
                           "[0.5663706,2.5663706,-1.7168147, 0.2831853,2.2831853,-2.0000000,"
                           "0.0000000, 2.0000000,-2.2831853,-0.2831853,1.7168147,-2.5663706]");
  return nfail;
}

int testArrDComplex()
{
  cout<<"  testing array DComplex functions ..."<<endl;
  int nfail = 0;
  return nfail;
}

int testArrDateTime()
{
  cout<<"  testing array DateTime functions ..."<<endl;
  int nfail = 0;
  return nfail;
}

int testArrString()
{
  cout<<"  testing array String functions ..."<<endl;
  int nfail = 0;
  nfail += checkArrString ("hdms", "[3d2m4.16,-3h2m4.16]",
                           "['00h12m08.277', '-045d31m02.400']");
  return nfail;
}

int testMaskedArr()
{
  cout<<"  testing masked array functions ..."<<endl;
  int nfail = 0;
  // Check with a mask.
  nfail += checkArrInt ("arraydata", "marray([[1,2,3],[4,5,6]],[[T,F,F],[F,T,F]])",
                        "[[1,2,3],[4,5,6]]");
  nfail += checkArrInt ("arraydata", "marray(1,T)", "[1]");
  nfail += checkArrBool ("arraymask", "marray([1,2],[1,2]>1)", "[F,T]");
  nfail += checkArrInt ("flatten", "marray([[1,2,3],[4,5,6]],[[T,F,F],[F,T,F]])", "[2,3,4,6]");
  nfail += checkArrInt ("flatten", "[[1,2,3],[4,5,6]][[[T,F,F],[F,T,F]]]", "[2,3,4,6]");
  nfail += checkArrBool ("arraymask", "negatemask(marray([1,2],[1,2]>1))", "[T,F]");
  nfail += checkArrInt ("arraydata", "replacemasked([[1,2,3],[4,5,6]][[[T,F,F],[F,T,F]]], 10)",
                        "[[10,2,3],[4,10,6]]");
  nfail += checkArrInt ("arraydata", "replacemasked([[1,2,3],[4,5,6]][[[T,F,F],[F,T,F]]], [[-1,-2,-3],[-4,-5,-6]])",
                        "[[-1,2,3],[4,-5,6]]");
  nfail += checkArrInt ("arraydata", "replaceunmasked([[1,2,3],[4,5,6]][[[T,F,F],[F,T,F]]], 10)",
                        "[[1,10,10],[10,5,10]]");
  nfail += checkArrInt ("arraydata", "replaceunmasked([[1,2,3],[4,5,6]][[[T,F,F],[F,T,F]]], [[-1,-2,-3],[-4,-5,-6]])",
                        "[[1,-2,-3],[-4,5,-6]]");
  // Check without a mask.
  nfail += checkArrInt ("arraydata", "[[1,2,3],[4,5,6]]", "[[1,2,3],[4,5,6]]");
  nfail += checkArrBool ("arraymask", "[1,2]", "[F,F]");
  nfail += checkArrBool ("arraymask", "negatemask([1,2])", "[T,T]");
  nfail += checkArrBool ("arraymask", "negatemask(2)", "[T]");
  nfail += checkArrInt ("flatten", "[[1,2,3],[4,5,6]]", "[1,2,3,4,5,6]");
  nfail += checkArrInt ("arraydata", "replacemasked([[1,2,3],[4,5,6]], 10)",
                        "[[1,2,3],[4,5,6]]");
  nfail += checkArrInt ("arraydata", "replacemasked([[1,2,3],[4,5,6]], [[-1,-2,-3],[-4,-5,-6]])",
                        "[[1,2,3],[4,5,6]]");
  nfail += checkArrInt ("arraydata", "replaceunmasked([[1,2,3],[4,5,6]], 10)",
                        "[[10,10,10],[10,10,10]]");
  nfail += checkArrInt ("arraydata", "replaceunmasked([[1,2,3],[4,5,6]], [[-1,-2,-3],[-4,-5,-6]])",
                        "[[-1,-2,-3],[-4,-5,-6]]");
  // Check with a scalar argument.
  nfail += checkArrInt ("arraydata", "1", "[1]");
  nfail += checkArrInt ("arraydata", "marray(1, True)", "[1]");
  nfail += checkArrBool ("arraymask", "marray(1, True)", "[T]");
  nfail += checkArrBool ("arraymask", "1", "[F]");
  nfail += checkArrBool ("arraymask", "negatemask(1)", "[T]");
  nfail += checkArrInt ("arraydata", "replacemasked(1, 2)", "[1]");
  nfail += checkArrInt ("arraydata", "replaceunmasked(1, 2)", "[2]");
  nfail += checkArrInt ("flatten", "1", "[1]");
  return nfail;
}

int testLessArg()
{
  cout<<"  testing functions with too few arguments ..."<<endl;
  int nfail = 0;
  nfail += checkExcp ("NEAR", "1", "- too few ");
  nfail += checkExcp ("NearAbs", "", "- too few ");
  nfail += checkExcp ("sin", "", "- too few ");
  nfail += checkExcp ("sinh", "", "- too few ");
  nfail += checkExcp ("cos", "", "- too few ");
  nfail += checkExcp ("cosh", "", "- too few ");
  nfail += checkExcp ("exp", "", "- too few ");
  nfail += checkExcp ("log", "", "- too few ");
  nfail += checkExcp ("log10", "", "- too few ");
  nfail += checkExcp ("sqrt", "", "- too few ");
  nfail += checkExcp ("pow", "2", "- too few ");
  nfail += checkExcp ("conj", "", "- too few ");
  nfail += checkExcp ("sqr", "", "- too few ");
  nfail += checkExcp ("cube", "", "- too few ");
  nfail += checkExcp ("min", "", "- too few ");
  nfail += checkExcp ("max", "", "- too few ");
  nfail += checkExcp ("norm", "", "- too few ");
  nfail += checkExcp ("abs", "", "- too few ");
  nfail += checkExcp ("arg", "", "- too few ");
  nfail += checkExcp ("real", "", "- too few ");
  nfail += checkExcp ("imag", "", "- too few ");
  nfail += checkExcp ("int", "", "- too few ");
  nfail += checkExcp ("sign", "", "- too few ");
  nfail += checkExcp ("round", "", "- too few ");
  nfail += checkExcp ("ceil", "", "- too few ");
  nfail += checkExcp ("floor", "", "- too few ");
  nfail += checkExcp ("fmod", "", "- too few ");
  nfail += checkExcp ("complex", "-1.4", "- too few ");
  nfail += checkExcp ("array", "1", "- too few ");
  nfail += checkExcp ("transpose", "", "- too few ");
  nfail += checkExcp ("reversearray", "", "- too few ");
  nfail += checkExcp ("diagonal", "", "- too few ");
  nfail += checkExcp ("resize", "1", "- too few ");
  nfail += checkExcp ("sums", "1", "- too few ");
  nfail += checkExcp ("products", "1", "- too few ");
  nfail += checkExcp ("sumsqrs", "1", "- too few ");
  nfail += checkExcp ("mins", "1", "- too few ");
  nfail += checkExcp ("maxs", "1", "- too few ");
  nfail += checkExcp ("means", "1", "- too few ");
  nfail += checkExcp ("variances", "1", "- too few ");
  nfail += checkExcp ("samplevariances", "1", "- too few ");
  nfail += checkExcp ("stddevs", "1", "- too few ");
  nfail += checkExcp ("samplestddevs", "1", "- too few ");
  nfail += checkExcp ("avdevs", "1", "- too few ");
  nfail += checkExcp ("rmss", "1", "- too few ");
  nfail += checkExcp ("medians", "1", "- too few ");
  nfail += checkExcp ("fractiles", "1,0.5", "- too few ");
  nfail += checkExcp ("alls", "1", "- too few ");
  nfail += checkExcp ("anys", "1", "- too few ");
  nfail += checkExcp ("ntrues", "1", "- too few ");
  nfail += checkExcp ("nfalses", "1", "- too few ");
  nfail += checkExcp ("runningsum", "1", "- too few ");
  nfail += checkExcp ("runningproduct", "1", "- too few ");
  nfail += checkExcp ("runningsumsqr", "1", "- too few ");
  nfail += checkExcp ("runningmin", "1", "- too few ");
  nfail += checkExcp ("runningmax", "1", "- too few ");
  nfail += checkExcp ("runningmean", "1", "- too few ");
  nfail += checkExcp ("runningvariance", "1", "- too few ");
  nfail += checkExcp ("runningsamplevariance", "1", "- too few ");
  nfail += checkExcp ("runningstddev", "1", "- too few ");
  nfail += checkExcp ("runningsamplestddev", "1", "- too few ");
  nfail += checkExcp ("runningavdev", "1", "- too few ");
  nfail += checkExcp ("runningrms", "1", "- too few ");
  nfail += checkExcp ("runningmedian", "1", "- too few ");
  nfail += checkExcp ("runningfractile", "1,0.5", "- too few ");
  nfail += checkExcp ("runningall", "1", "- too few ");
  nfail += checkExcp ("runningany", "1", "- too few ");
  nfail += checkExcp ("runningntrue", "1", "- too few ");
  nfail += checkExcp ("runningnfalse", "1", "- too few ");
  nfail += checkExcp ("boxedsum", "1", "- too few ");
  nfail += checkExcp ("boxedproduct", "1", "- too few ");
  nfail += checkExcp ("boxedsumsqr", "1", "- too few ");
  nfail += checkExcp ("boxedmin", "1", "- too few ");
  nfail += checkExcp ("boxedmax", "1", "- too few ");
  nfail += checkExcp ("boxedmean", "1", "- too few ");
  nfail += checkExcp ("boxedvariance", "1", "- too few ");
  nfail += checkExcp ("boxedsamplevariance", "1", "- too few ");
  nfail += checkExcp ("boxedstddev", "1", "- too few ");
  nfail += checkExcp ("boxedsamplestddev", "1", "- too few ");
  nfail += checkExcp ("boxedavdev", "1", "- too few ");
  nfail += checkExcp ("boxedrms", "1", "- too few ");
  nfail += checkExcp ("boxedmedian", "1", "- too few ");
  nfail += checkExcp ("boxedfractile", "1,0.5", "- too few ");
  nfail += checkExcp ("boxedall", "1", "- too few ");
  nfail += checkExcp ("boxedany", "1", "- too few ");
  nfail += checkExcp ("boxedntrue", "1", "- too few ");
  nfail += checkExcp ("boxednfalse", "1", "- too few ");
  nfail += checkExcp ("isnan", "", "- too few ");
  nfail += checkExcp ("isinf", "", "- too few ");
  nfail += checkExcp ("isdefined", "", "- too few ");
  nfail += checkExcp ("isnull", "", "- too few ");
  nfail += checkExcp ("iscolumn", "", "- too few ");
  nfail += checkExcp ("iskeyword", "", "- too few ");
  nfail += checkExcp ("ndim", "", "- too few ");
  nfail += checkExcp ("nelements", "", "- too few ");
  nfail += checkExcp ("shape", "", "- too few ");
  nfail += checkExcp ("substr", "1", "- too few ");
  nfail += checkExcp ("replace", "1", "- too few ");
  nfail += checkExcp ("pattern", "", "- too few ");
  nfail += checkExcp ("regex", "", "- too few ");
  nfail += checkExcp ("sqlpattern", "", "- too few ");
  nfail += checkExcp ("mjdtodate", "", "- too few ");
  nfail += checkExcp ("str", "", "- too few ");
  nfail += checkExcp ("hms", "", "- too few ");
  nfail += checkExcp ("dms", "", "- too few ");
  nfail += checkExcp ("hdms", "", "- too few ");
  nfail += checkExcp ("iif", "1,2", "- too few ");
  nfail += checkExcp ("angdist", "1", "- too few ");
  nfail += checkExcp ("angdistx", "1", "- too few ");
  nfail += checkExcp ("anycone", "1", "- too few ");
  nfail += checkExcp ("findcone", "1", "- too few ");
  nfail += checkExcp ("cones", "1", "- too few ");
  nfail += checkExcp ("bool", "", "- too few ");
  nfail += checkExcp ("marray", "1", "- too few ");
  nfail += checkExcp ("arraydata", "", "- too few ");
  nfail += checkExcp ("arraymask", "", "- too few ");
  nfail += checkExcp ("negatemask", "", "- too few ");
  nfail += checkExcp ("replacemasked", "1", "- too few ");
  nfail += checkExcp ("replaceunmasked", "1", "- too few ");
  nfail += checkExcp ("flatten", "", "- too few ");
  nfail += checkExcp ("gfirst", "", "- too few ");
  nfail += checkExcp ("glast", "", "- too few ");
  nfail += checkExcp ("gmin", "", "- too few ");
  nfail += checkExcp ("gmax", "", "- too few ");
  nfail += checkExcp ("gsum", "", "- too few ");
  nfail += checkExcp ("gproduct", "", "- too few ");
  nfail += checkExcp ("gsumsqr", "", "- too few ");
  nfail += checkExcp ("gmean", "", "- too few ");
  nfail += checkExcp ("gvariance", "", "- too few ");
  nfail += checkExcp ("gsamplevariance", "", "- too few ");
  nfail += checkExcp ("gstddev", "", "- too few ");
  nfail += checkExcp ("gsamplestddev", "", "- too few ");
  nfail += checkExcp ("grms", "", "- too few ");
  nfail += checkExcp ("gany", "", "- too few ");
  nfail += checkExcp ("gall", "", "- too few ");
  nfail += checkExcp ("gntrue", "", "- too few ");
  nfail += checkExcp ("gnfalse", "", "- too few ");
  nfail += checkExcp ("gmins", "", "- too few ");
  nfail += checkExcp ("gmaxs", "", "- too few ");
  nfail += checkExcp ("gsums", "", "- too few ");
  nfail += checkExcp ("gproducts", "", "- too few ");
  nfail += checkExcp ("gsumsqrs", "", "- too few ");
  nfail += checkExcp ("gmeans", "", "- too few ");
  nfail += checkExcp ("gvariances", "", "- too few ");
  nfail += checkExcp ("gsamplevariances", "", "- too few ");
  nfail += checkExcp ("gstddevs", "", "- too few ");
  nfail += checkExcp ("gsamplestddevs", "", "- too few ");
  nfail += checkExcp ("grmss", "", "- too few ");
  nfail += checkExcp ("ganys", "", "- too few ");
  nfail += checkExcp ("galls", "", "- too few ");
  nfail += checkExcp ("gntrues", "", "- too few ");
  nfail += checkExcp ("gnfalses", "", "- too few ");
  nfail += checkExcp ("ghist", "1,2,3", "- too few ");
  nfail += checkExcp ("gaggr", "", "- too few ");
  nfail += checkExcp ("gmedian", "", "- too few ");
  nfail += checkExcp ("gfractile", "1", "- too few ");
  return nfail;
}

int testMoreArg()
{
  cout<<"  testing functions with too many arguments ..."<<endl;
  int nfail = 0;
  nfail += checkExcp ("pi", "1", "- too many ");
  nfail += checkExcp ("e", "1", "- too many ");
  nfail += checkExcp ("c", "1", "- too many ");
  nfail += checkExcp ("NEAR", "1,2,3,4", "- too many ");
  nfail += checkExcp ("NearAbs", "1,2,3,4", "- too many ");
  nfail += checkExcp ("sin", "1,2", "- too many ");
  nfail += checkExcp ("sinh", "2,3", "- too many ");
  nfail += checkExcp ("cos", "3,4", "- too many ");
  nfail += checkExcp ("cosh", "4,5", "- too many ");
  nfail += checkExcp ("exp", "1,2", "- too many ");
  nfail += checkExcp ("log", "1,2", "- too many ");
  nfail += checkExcp ("log10", "2,3", "- too many ");
  nfail += checkExcp ("sqrt", "1,2", "- too many ");
  nfail += checkExcp ("pow", "2,3,4", "- too many ");
  nfail += checkExcp ("conj", "2,4", "- too many ");
  nfail += checkExcp ("sqr", "2,4", "- too many ");
  nfail += checkExcp ("cube", "2,4", "- too many ");
  nfail += checkExcp ("min", "2,4,3", "- too many ");
  nfail += checkExcp ("max", "2,4,3", "- too many ");
  nfail += checkExcp ("norm", "2,4", "- too many ");
  nfail += checkExcp ("amplitude", "4,3", "- too many ");
  nfail += checkExcp ("phase", "2,3", "- too many ");
  nfail += checkExcp ("real", "2,4", "- too many ");
  nfail += checkExcp ("imag", "2,4", "- too many ");
  nfail += checkExcp ("integer", "2,3", "- too many ");
  nfail += checkExcp ("sign", "2,3", "- too many ");
  nfail += checkExcp ("round", "2,3", "- too many ");
  nfail += checkExcp ("ceil", "2,3", "- too many ");
  nfail += checkExcp ("floor", "2,3", "- too many ");
  nfail += checkExcp ("fmod", "2,3,4", "- too many ");
  nfail += checkExcp ("complex", "-1.4,4,5", "- too many ");
  nfail += checkExcp ("resize", "1,2,3,4", "- too many ");
  nfail += checkExcp ("isnan", "1,2", "- too many ");
  nfail += checkExcp ("isinf", "1,2", "- too many ");
  nfail += checkExcp ("isfinite", "1,2", "- too many ");
  nfail += checkExcp ("isdefined", "1,2", "- too many ");
  nfail += checkExcp ("isnull", "1,2", "- too many ");
  nfail += checkExcp ("iscolumn", "1,2", "- too many ");
  nfail += checkExcp ("iskeyword", "1,2", "- too many ");
  nfail += checkExcp ("ndim", "1,2", "- too many ");
  nfail += checkExcp ("nelements", "1,2", "- too many ");
  nfail += checkExcp ("shape", "1,2", "- too many ");
  nfail += checkExcp ("substr", "1,2,3,4", "- too many ");
  nfail += checkExcp ("replace", "1,2,3,4", "- too many ");
  nfail += checkExcp ("pattern", "1,2", "- too many ");
  nfail += checkExcp ("regex", "1,2", "- too many ");
  nfail += checkExcp ("sqlpattern", "1,2", "- too many ");
  nfail += checkExcp ("mjdtodate", "1,2", "- too many ");
  nfail += checkExcp ("datetime", "1,2", "- too many ");
  nfail += checkExcp ("mjd", "1,2", "- too many ");
  nfail += checkExcp ("date", "1,2", "- too many ");
  nfail += checkExcp ("time", "1,2", "- too many ");
  nfail += checkExcp ("year", "1,2", "- too many ");
  nfail += checkExcp ("month", "1,2", "- too many ");
  nfail += checkExcp ("day", "1,2", "- too many ");
  nfail += checkExcp ("cmonth", "1,2", "- too many ");
  nfail += checkExcp ("weekday", "1,2", "- too many ");
  nfail += checkExcp ("cweekday", "1,2", "- too many ");
  nfail += checkExcp ("week", "1,2", "- too many ");
  nfail += checkExcp ("cdatetime", "1,2", "- too many ");
  nfail += checkExcp ("cdate", "1,2", "- too many ");
  nfail += checkExcp ("ctime", "1,2", "- too many ");
  nfail += checkExcp ("str", "1,2,3", "- too many ");
  nfail += checkExcp ("hms", "1,2", "- too many ");
  nfail += checkExcp ("dms", "1,2", "- too many ");
  nfail += checkExcp ("hdms", "1,2", "- too many ");
  nfail += checkExcp ("iif", "1,2,3,4", "- too many ");
  nfail += checkExcp ("angdist", "1,2,3", "- too many ");
  nfail += checkExcp ("angdistx", "1,2,3", "- too many ");
  nfail += checkExcp ("anycone", "1,2,3,4", "- too many ");
  nfail += checkExcp ("findcone", "1,2,3,4", "- too many ");
  nfail += checkExcp ("cones", "1,2,3,4", "- too many ");
  nfail += checkExcp ("bool", "1,2", "- too many ");
  nfail += checkExcp ("marray", "1,2,3", "- too many ");
  nfail += checkExcp ("arraydata", "1,2", "- too many ");
  nfail += checkExcp ("arraymask", "1,2", "- too many ");
  nfail += checkExcp ("negatemask", "1,2", "- too many ");
  nfail += checkExcp ("replacemasked", "1,2,3", "- too many ");
  nfail += checkExcp ("replaceunmasked", "1,2,3", "- too many ");
  nfail += checkExcp ("flatten", "1,2", "- too many ");
  nfail += checkExcp ("gcount", "1,2", "- too many ");
  nfail += checkExcp ("gfirst", "1,2", "- too many ");
  nfail += checkExcp ("glast", "1,2", "- too many ");
  nfail += checkExcp ("gmin", "1,2", "- too many ");
  nfail += checkExcp ("gmax", "1,2", "- too many ");
  nfail += checkExcp ("gsum", "1,2", "- too many ");
  nfail += checkExcp ("gproduct", "1,2", "- too many ");
  nfail += checkExcp ("gsumsqr", "1,2", "- too many ");
  nfail += checkExcp ("gmean", "1,2", "- too many ");
  nfail += checkExcp ("gvariance", "1,2", "- too many ");
  nfail += checkExcp ("gsamplevariance", "1,2", "- too many ");
  nfail += checkExcp ("gstddev", "1,2", "- too many ");
  nfail += checkExcp ("gsamplestddev", "1,2", "- too many ");
  nfail += checkExcp ("grms", "1,2", "- too many ");
  nfail += checkExcp ("gany", "1,2", "- too many ");
  nfail += checkExcp ("gall", "1,2", "- too many ");
  nfail += checkExcp ("gntrue", "1,2", "- too many ");
  nfail += checkExcp ("gnfalse", "1,2", "- too many ");
  nfail += checkExcp ("gmins", "[1],2", "- too many ");
  nfail += checkExcp ("gmaxs", "[1],2", "- too many ");
  nfail += checkExcp ("gsums", "[1],2", "- too many ");
  nfail += checkExcp ("gproducts", "[1],2", "- too many ");
  nfail += checkExcp ("gsumsqrs", "[1],2", "- too many ");
  nfail += checkExcp ("gmeans", "[1],2", "- too many ");
  nfail += checkExcp ("gvariances", "[1],2", "- too many ");
  nfail += checkExcp ("gsamplevariances", "[1],2", "- too many ");
  nfail += checkExcp ("gstddevs", "[1],2", "- too many ");
  nfail += checkExcp ("gsamplestddevs", "[1],2", "- too many ");
  nfail += checkExcp ("grmss", "[1],2", "- too many ");
  nfail += checkExcp ("ganys", "[1],2", "- too many ");
  nfail += checkExcp ("galls", "[1],2", "- too many ");
  nfail += checkExcp ("gntrues", "[1],2", "- too many ");
  nfail += checkExcp ("gnfalses", "[1],2", "- too many ");
  nfail += checkExcp ("ghist", "[1],2,3,4,5", "- too many ");
  nfail += checkExcp ("gaggr", "[1],2", "- too many ");
  nfail += checkExcp ("gmedian", "[1],2", "- too many ");
  nfail += checkExcp ("gfractile", "[1],2,3", "- too many ");
  nfail += checkExcp ("growid", "2", "- too many ");
  nfail += checkExcp ("rand", "2", "- too many ");
  nfail += checkExcp ("rownumber", "2", "- too many ");
  nfail += checkExcp ("rowid", "2", "- too many ");
  return nfail;
}

int testInvScaArg()
{
  cout<<"  testing scalar functions with invalid arguments ..."<<endl;
  int nfail = 0;
  nfail += checkExcp ("near", "F,T", "- invalid operand data type");
  nfail += checkExcp ("near", "'4', '5'", "- invalid operand data type");
  nfail += checkExcp ("near", "date(), date()", "- invalid operand data type");
  nfail += checkExcp ("nearAbs", "True,False", "- invalid operand data type");
  nfail += checkExcp ("nearAbs", "'4', '5'", "- invalid operand data type");
  nfail += checkExcp ("nearAbs", "date(), date()", "- invalid operand data type");
  nfail += checkExcp ("sin", "T", "- invalid operand data type");
  nfail += checkExcp ("sin", "''", "- invalid operand data type");
  nfail += checkExcp ("sin", "date()", "- invalid operand data type");
  nfail += checkExcp ("sinh", "T", "- invalid operand data type");
  nfail += checkExcp ("sinh", "''", "- invalid operand data type");
  nfail += checkExcp ("sinh", "date()", "- invalid operand data type");
  nfail += checkExcp ("cos", "T", "- invalid operand data type");
  nfail += checkExcp ("cos", "''", "- invalid operand data type");
  nfail += checkExcp ("cos", "date()", "- invalid operand data type");
  nfail += checkExcp ("cosh", "T", "- invalid operand data type");
  nfail += checkExcp ("cosh", "''", "- invalid operand data type");
  nfail += checkExcp ("cosh", "date()", "- invalid operand data type");
  nfail += checkExcp ("exp", "F", "- invalid operand data type");
  nfail += checkExcp ("exp", "''", "- invalid operand data type");
  nfail += checkExcp ("exp", "date()", "- invalid operand data type");
  nfail += checkExcp ("log", "T", "- invalid operand data type");
  nfail += checkExcp ("log", "''", "- invalid operand data type");
  nfail += checkExcp ("log", "date()", "- invalid operand data type");
  nfail += checkExcp ("log10", "T", "- invalid operand data type");
  nfail += checkExcp ("log10", "''", "- invalid operand data type");
  nfail += checkExcp ("log10", "date()", "- invalid operand data type");
  nfail += checkExcp ("sqrt", "True", "- invalid operand data type");
  nfail += checkExcp ("sqrt", "'4'", "- invalid operand data type");
  nfail += checkExcp ("sqrt", "date()", "- invalid operand data type");
  nfail += checkExcp ("pow", "2,T", "- invalid operand data type");
  nfail += checkExcp ("pow", "T,2", "- invalid operand data type");
  nfail += checkExcp ("pow", "'',2", "- invalid operand data type");
  nfail += checkExcp ("pow", "2,''", "- invalid operand data type");
  nfail += checkExcp ("pow", "date(), 2", "- invalid operand data type");
  nfail += checkExcp ("conj", "T", "- invalid operand data type");
  nfail += checkExcp ("conj", "''", "- invalid operand data type");
  nfail += checkExcp ("conj", "date()", "- invalid operand data type");
  nfail += checkExcp ("sqr", "T", "- invalid operand data type");
  nfail += checkExcp ("sqr", "''", "- invalid operand data type");
  nfail += checkExcp ("sqr", "date()", "- invalid operand data type");
  nfail += checkExcp ("cube", "T", "- invalid operand data type");
  nfail += checkExcp ("cube", "''", "- invalid operand data type");
  nfail += checkExcp ("cube", "date()", "- invalid operand data type");
  nfail += checkExcp ("min", "T,F", "- invalid operand data type");
  nfail += checkExcp ("min", "'',3", "- invalid operand data type");
  nfail += checkExcp ("min", "date(),date()", "- invalid operand data type");
  nfail += checkExcp ("max", "4,T", "- invalid operand data type");
  nfail += checkExcp ("max", "'','a'", "- invalid operand data type");
  nfail += checkExcp ("max", "date(),date()", "- invalid operand data type");
  nfail += checkExcp ("norm", "T", "- invalid operand data type");
  nfail += checkExcp ("norm", "'2+3i'", "- invalid operand data type");
  nfail += checkExcp ("norm", "date()", "- invalid operand data type");
  nfail += checkExcp ("abs", "False", "- invalid operand data type");
  nfail += checkExcp ("abs", "'2+3i'", "- invalid operand data type");
  nfail += checkExcp ("abs", "date()", "- invalid operand data type");
  nfail += checkExcp ("arg", "F", "- invalid operand data type");
  nfail += checkExcp ("arg", "'2+3i'", "- invalid operand data type");
  nfail += checkExcp ("arg", "date()", "- invalid operand data type");
  nfail += checkExcp ("imag", "F", "- invalid operand data type");
  nfail += checkExcp ("imag", "'-3.7'", "- invalid operand data type");
  nfail += checkExcp ("imag", "date()", "- invalid operand data type");
  nfail += checkExcp ("int", "2+3i", "- invalid operand data type");
  nfail += checkExcp ("int", "date()", "- invalid operand data type");
  nfail += checkExcp ("isnan", "T", "- invalid operand data type");
  nfail += checkExcp ("isnan", "''", "- invalid operand data type");
  nfail += checkExcp ("isnan", "date()", "- invalid operand data type");
  nfail += checkExcp ("isinf", "T", "- invalid operand data type");
  nfail += checkExcp ("isinf", "''", "- invalid operand data type");
  nfail += checkExcp ("isinf", "date()", "- invalid operand data type");
  nfail += checkExcp ("isfinite", "T", "- invalid operand data type");
  nfail += checkExcp ("isfinite", "''", "- invalid operand data type");
  nfail += checkExcp ("isfinite", "date()", "- invalid operand data type");
  nfail += checkExcp ("isdefined", "COL", "Shorthand  has not been defined");
  nfail += checkExcp ("isnull", "COL", "Shorthand  has not been defined");
  nfail += checkExcp ("iscolumn", "COL", "Shorthand  has not been defined");
  nfail += checkExcp ("iskeyword", "COL", "Shorthand  has not been defined");
  nfail += checkExcp ("strlength", "1", "- invalid operand data type");
  nfail += checkExcp ("len", "2.", "- invalid operand data type");
  nfail += checkExcp ("strlength", "3i", "- invalid operand data type");
  nfail += checkExcp ("strlength", "date()", "- invalid operand data type");
  nfail += checkExcp ("upcase", "1", "- invalid operand data type");
  nfail += checkExcp ("upper", "2.", "- invalid operand data type");
  nfail += checkExcp ("upcase", "3i", "- invalid operand data type");
  nfail += checkExcp ("upcase", "date()", "- invalid operand data type");
  nfail += checkExcp ("downcase", "1", "- invalid operand data type");
  nfail += checkExcp ("lower", "2.", "- invalid operand data type");
  nfail += checkExcp ("downcase", "3i", "- invalid operand data type");
  nfail += checkExcp ("downcase", "date()", "- invalid operand data type");
  nfail += checkExcp ("capitalize", "1", "- invalid operand data type");
  nfail += checkExcp ("capitalize", "2.", "- invalid operand data type");
  nfail += checkExcp ("capitalize", "3i", "- invalid operand data type");
  nfail += checkExcp ("capitalize", "date()", "- invalid operand data type");
  nfail += checkExcp ("reversestring", "1", "- invalid operand data type");
  nfail += checkExcp ("reversestring", "2.", "- invalid operand data type");
  nfail += checkExcp ("reversestring", "3i", "- invalid operand data type");
  nfail += checkExcp ("reversestring", "date()", "- invalid operand data type");
  nfail += checkExcp ("trim", "1", "- invalid operand data type");
  nfail += checkExcp ("trim", "2.", "- invalid operand data type");
  nfail += checkExcp ("trim", "3i", "- invalid operand data type");
  nfail += checkExcp ("trim", "date()", "- invalid operand data type");
  nfail += checkExcp ("ltrim", "1", "- invalid operand data type");
  nfail += checkExcp ("ltrim", "2.", "- invalid operand data type");
  nfail += checkExcp ("ltrim", "3i", "- invalid operand data type");
  nfail += checkExcp ("ltrim", "date()", "- invalid operand data type");
  nfail += checkExcp ("rtrim", "1", "- invalid operand data type");
  nfail += checkExcp ("rtrim", "2.", "- invalid operand data type");
  nfail += checkExcp ("rtrim", "3i", "- invalid operand data type");
  nfail += checkExcp ("rtrim", "date()", "- invalid operand data type");
  nfail += checkExcp ("substr", "1, 1", "SUBSTR has to be a string");
  nfail += checkExcp ("substr", "2., 1", "SUBSTR has to be a string");
  nfail += checkExcp ("substr", "3i, 1", "SUBSTR has to be a string");
  nfail += checkExcp ("substr", "date(), 1", "SUBSTR has to be a string");
  nfail += checkExcp ("substr", "'', 1.", "SUBSTR have to be integer scalars");
  nfail += checkExcp ("substr", "'', 1, 2.", "SUBSTR have to be integer scalars");
  nfail += checkExcp ("replace", "1, 1", "1st argument of function REPLACE");
  nfail += checkExcp ("replace", "2., 1", "1st argument of function REPLACE");
  nfail += checkExcp ("replace", "3i, 1", "1st argument of function REPLACE");
  nfail += checkExcp ("replace", "date(), 1", "1st argument of function REPLACE");
  nfail += checkExcp ("replace", "'', 1.", "REPLACE has to be a string or regex");
  nfail += checkExcp ("replace", "'', '', 2.", "REPLACE has to be a string scalar");
  nfail += checkExcp ("pattern", "1", "- invalid operand data type");
  nfail += checkExcp ("pattern", "2.", "- invalid operand data type");
  nfail += checkExcp ("pattern", "3i", "- invalid operand data type");
  nfail += checkExcp ("pattern", "date()", "- invalid operand data type");
  nfail += checkExcp ("pattern", "['s1','s2']", " has to have a scalar argument");
  nfail += checkExcp ("regex", "1", "- invalid operand data type");
  nfail += checkExcp ("regex", "2.", "- invalid operand data type");
  nfail += checkExcp ("regex", "3i", "- invalid operand data type");
  nfail += checkExcp ("regex", "date()", "- invalid operand data type");
  nfail += checkExcp ("regex", "['s1','s2']", " has to have a scalar argument");
  nfail += checkExcp ("sqlpattern", "1", "- invalid operand data type");
  nfail += checkExcp ("sqlpattern", "2.", "- invalid operand data type");
  nfail += checkExcp ("sqlpattern", "3i", "- invalid operand data type");
  nfail += checkExcp ("sqlpattern", "date()", "- invalid operand data type");
  nfail += checkExcp ("sqlpattern", "['s1','s2']", " has to have a scalar argument");
  nfail += checkExcp ("datetime", "1", "- invalid operand data type");
  nfail += checkExcp ("datetime", "1.", "- invalid operand data type");
  nfail += checkExcp ("datetime", "1i", "- invalid operand data type");
  nfail += checkExcp ("datetime", "date()", "- invalid operand data type");
  nfail += checkExcp ("mjdtodate", "1i", "- invalid operand data type");
  nfail += checkExcp ("mjdtodate", "date()", "- invalid operand data type");
  nfail += checkExcp ("mjdtodate", "'1'", "- invalid operand data type");
  nfail += checkExcp ("mjd", "1i", "- invalid operand data type");
  nfail += checkExcp ("date", "1i", "- invalid operand data type");
  nfail += checkExcp ("time", "1i", "- invalid operand data type");
  nfail += checkExcp ("year", "1i", "- invalid operand data type");
  nfail += checkExcp ("month", "1i", "- invalid operand data type");
  nfail += checkExcp ("day", "1i", "- invalid operand data type");
  nfail += checkExcp ("cmonth", "1i", "- invalid operand data type");
  nfail += checkExcp ("weekday", "1i", "- invalid operand data type");
  nfail += checkExcp ("cweekday", "1i", "- invalid operand data type");
  nfail += checkExcp ("week", "1i", "- invalid operand data type");
  nfail += checkExcp ("cdatetime", "1i", "- invalid operand data type");
  nfail += checkExcp ("cdate", "1i", "- invalid operand data type");
  nfail += checkExcp ("ctime", "1i", "- invalid operand data type");
  nfail += checkExcp ("str", "1i, 2i", "- 2nd argument of function STRING");
  nfail += checkExcp ("str", "1i, date()", "- 2nd argument of function STRING");
  nfail += checkExcp ("hms", "2i", "- invalid operand data type");
  nfail += checkExcp ("hms", "date()", "- invalid operand data type");
  nfail += checkExcp ("hms", "''", "- invalid operand data type");
  nfail += checkExcp ("dms", "2i", "- invalid operand data type");
  nfail += checkExcp ("dms", "date()", "- invalid operand data type");
  nfail += checkExcp ("dms", "''", "- invalid operand data type");
  nfail += checkExcp ("hdms", "[2i]", "- invalid operand data type");
  nfail += checkExcp ("hdms", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("hdms", "['']", "- invalid operand data type");
  nfail += checkExcp ("iif", "2,3,4", "- invalid operand data type");
  nfail += checkExcp ("iif", "2.,3,4", "- invalid operand data type");
  nfail += checkExcp ("iif", "2+2i,3,4", "- invalid operand data type");
  nfail += checkExcp ("iif", "date(),3,4", "- invalid operand data type");
  nfail += checkExcp ("iif", "'2',3,4", "- invalid operand data type");
  nfail += checkExcp ("iif", "T,3,'4'", "- invalid operand data type");
  nfail += checkExcp ("iif", "T,date(),'3'", "- invalid operand data type");
  nfail += checkExcp ("angdist", "[1+0i,2],[2,3]", "- invalid operand data type");
  nfail += checkExcp ("angdist", "[''],['']", "- invalid operand data type");
  nfail += checkExcp ("angdist", "[date()],[date()]", "- invalid operand data type");
  nfail += checkExcp ("angdistx", "[1+0i,2],[2,3]", "- invalid operand data type");
  nfail += checkExcp ("angdistx", "[''],['']", "- invalid operand data type");
  nfail += checkExcp ("angdistx", "[date()],[date()]", "- invalid operand data type");
  nfail += checkExcp ("anycone", "1i, 1", " must be double arrays");
  nfail += checkExcp ("anycone", "date(), 1", " must be double arrays");
  nfail += checkExcp ("anycone", "'', 1", " must be double arrays");
  nfail += checkExcp ("anycone", "1, 1i", " must be double arrays");
  nfail += checkExcp ("anycone", "1, date()", " must be double arrays");
  nfail += checkExcp ("anycone", "1, ''", " must be double arrays");
  nfail += checkExcp ("anycone", "1, 1, 1i", " must be double arrays");
  nfail += checkExcp ("anycone", "1, 1, date()", " must be double arrays");
  nfail += checkExcp ("anycone", "1, 1, ''", " must be double arrays");
  nfail += checkExcp ("anycone", "1, 1", " must be double arrays");
  nfail += checkExcp ("anycone", "[1,1], 1", " must be double arrays");
  nfail += checkExcp ("anycone", "[1,1,1], [1,1]", " must have multiple of 2 values");
  nfail += checkExcp ("anycone", "[1,1], [1,1]", " must have multiple of 3 values");
  nfail += checkExcp ("anycone", "[1,1], [1,1]", " must have multiple of 3 values");
  nfail += checkExcp ("anycone", "[1,1], [1,1,1], 1", " must have multiple of 2 values");
  nfail += checkExcp ("findcone", "1i, 1", " must be double arrays");
  nfail += checkExcp ("findcone", "date(), 1", " must be double arrays");
  nfail += checkExcp ("findcone", "'', 1", " must be double arrays");
  nfail += checkExcp ("findcone", "1, 1i", " must be double arrays");
  nfail += checkExcp ("findcone", "1, date()", " must be double arrays");
  nfail += checkExcp ("findcone", "1, ''", " must be double arrays");
  nfail += checkExcp ("findcone", "1, 1, 1i", " must be double arrays");
  nfail += checkExcp ("findcone", "1, 1, date()", " must be double arrays");
  nfail += checkExcp ("findcone", "1, 1, ''", " must be double arrays");
  nfail += checkExcp ("findcone", "1, 1", " must be double arrays");
  nfail += checkExcp ("findcone", "[1,1], 1", " must be double arrays");
  nfail += checkExcp ("findcone", "[1,1,1], [1,1]", " must have multiple of 2 values");
  nfail += checkExcp ("findcone", "[1,1], [1,1]", " must have multiple of 3 values");
  nfail += checkExcp ("findcone", "[1,1], [1,1]", " must have multiple of 3 values");
  nfail += checkExcp ("findcone", "[1,1], [1,1,1], 1", " must have multiple of 2 values");
  nfail += checkExcp ("cones", "1i, 1", " must be double arrays");
  nfail += checkExcp ("cones", "date(), 1", " must be double arrays");
  nfail += checkExcp ("cones", "'', 1", " must be double arrays");
  nfail += checkExcp ("cones", "1, 1i", " must be double arrays");
  nfail += checkExcp ("cones", "1, date()", " must be double arrays");
  nfail += checkExcp ("cones", "1, ''", " must be double arrays");
  nfail += checkExcp ("cones", "1, 1, 1i", " must be double arrays");
  nfail += checkExcp ("cones", "1, 1, date()", " must be double arrays");
  nfail += checkExcp ("cones", "1, 1, ''", " must be double arrays");
  nfail += checkExcp ("cones", "1, 1", " must be double arrays");
  nfail += checkExcp ("cones", "[1,1], 1", " must be double arrays");
  nfail += checkExcp ("cones", "[1,1,1], [1,1]", " must have multiple of 2 values");
  nfail += checkExcp ("cones", "[1,1], [1,1]", " must have multiple of 3 values");
  nfail += checkExcp ("cones", "[1,1], [1,1]", " must have multiple of 3 values");
  nfail += checkExcp ("cones", "[1,1], [1,1,1], 1", " must have multiple of 2 values");
  nfail += checkExcp ("bool", "regex('a')", "- invalid operand data type");
  nfail += checkExcp ("marray", "[1], 1", " of marray function must be bool");
  nfail += checkExcp ("marray", "1, [T,F]", " AlwaysAssert itsShape.isEqual (mask.shape())");
  nfail += checkExcp ("marray", "[1,2], T", " AlwaysAssert itsShape.isEqual (mask.shape())");
  nfail += checkExcp ("replacemasked", "1, '2'", " invalid operand data type");
  nfail += checkExcp ("replaceunmasked", "1, '2'", " invalid operand data type");
  nfail += checkExcp ("replacemasked", "1, [3,4]", " array shapes mismatch");
  nfail += checkExcp ("replaceunmasked", "1, [3,4]", " array shapes mismatch");
  nfail += checkExcp ("gmin", "T", "- invalid operand data type");
  nfail += checkExcp ("gmin", "1i", "- invalid operand data type");
  nfail += checkExcp ("gmin", "date()", "- invalid operand data type");
  nfail += checkExcp ("gmin", "''", "- invalid operand data type");
  nfail += checkExcp ("gmax", "T", "- invalid operand data type");
  nfail += checkExcp ("gmax", "1i", "- invalid operand data type");
  nfail += checkExcp ("gmax", "date()", "- invalid operand data type");
  nfail += checkExcp ("gmax", "''", "- invalid operand data type");
  nfail += checkExcp ("gsum", "T", "- invalid operand data type");
  nfail += checkExcp ("gsum", "date()", "- invalid operand data type");
  nfail += checkExcp ("gsum", "''", "- invalid operand data type");
  nfail += checkExcp ("gproduct", "T", "- invalid operand data type");
  nfail += checkExcp ("gproduct", "date()", "- invalid operand data type");
  nfail += checkExcp ("gproduct", "''", "- invalid operand data type");
  nfail += checkExcp ("gsumsqr", "T", "- invalid operand data type");
  nfail += checkExcp ("gsumsqr", "date()", "- invalid operand data type");
  nfail += checkExcp ("gsumsqr", "''", "- invalid operand data type");
  nfail += checkExcp ("gmean", "T", "- invalid operand data type");
  nfail += checkExcp ("gmean", "date()", "- invalid operand data type");
  nfail += checkExcp ("gmean", "''", "- invalid operand data type");
  nfail += checkExcp ("gvariance", "T", "- invalid operand data type");
  nfail += checkExcp ("gvariance", "date()", "- invalid operand data type");
  nfail += checkExcp ("gvariance", "''", "- invalid operand data type");
  nfail += checkExcp ("gsamplevariance", "T", "- invalid operand data type");
  nfail += checkExcp ("gsamplevariance", "date()", "- invalid operand data type");
  nfail += checkExcp ("gsamplevariance", "''", "- invalid operand data type");
  nfail += checkExcp ("gstddev", "T", "- invalid operand data type");
  nfail += checkExcp ("gstddev", "date()", "- invalid operand data type");
  nfail += checkExcp ("gstddev", "''", "- invalid operand data type");
  nfail += checkExcp ("gsamplestddev", "T", "- invalid operand data type");
  nfail += checkExcp ("gsamplestddev", "date()", "- invalid operand data type");
  nfail += checkExcp ("gsamplestddev", "''", "- invalid operand data type");
  nfail += checkExcp ("grms", "T", "- invalid operand data type");
  nfail += checkExcp ("grms", "1i", "- invalid operand data type");
  nfail += checkExcp ("grms", "date()", "- invalid operand data type");
  nfail += checkExcp ("grms", "''", "- invalid operand data type");
  nfail += checkExcp ("gany", "1", "- invalid operand data type");
  nfail += checkExcp ("gany", "1.", "- invalid operand data type");
  nfail += checkExcp ("gany", "1i", "- invalid operand data type");
  nfail += checkExcp ("gany", "date()", "- invalid operand data type");
  nfail += checkExcp ("gany", "''", "- invalid operand data type");
  nfail += checkExcp ("gall", "1", "- invalid operand data type");
  nfail += checkExcp ("gall", "1.", "- invalid operand data type");
  nfail += checkExcp ("gall", "1i", "- invalid operand data type");
  nfail += checkExcp ("gall", "date()", "- invalid operand data type");
  nfail += checkExcp ("gall", "''", "- invalid operand data type");
  nfail += checkExcp ("gntrue", "1", "- invalid operand data type");
  nfail += checkExcp ("gntrue", "1.", "- invalid operand data type");
  nfail += checkExcp ("gntrue", "1i", "- invalid operand data type");
  nfail += checkExcp ("gntrue", "date()", "- invalid operand data type");
  nfail += checkExcp ("gntrue", "''", "- invalid operand data type");
  nfail += checkExcp ("gnfalse", "1", "- invalid operand data type");
  nfail += checkExcp ("gnfalse", "1.", "- invalid operand data type");
  nfail += checkExcp ("gnfalse", "1i", "- invalid operand data type");
  nfail += checkExcp ("gnfalse", "date()", "- invalid operand data type");
  nfail += checkExcp ("gnfalse", "''", "- invalid operand data type");
  nfail += checkExcp ("gmins", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gmins", "[1i]", "- invalid operand data type");
  nfail += checkExcp ("gmins", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gmins", "['']", "- invalid operand data type");
  nfail += checkExcp ("gmaxs", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gmaxs", "[1i]", "- invalid operand data type");
  nfail += checkExcp ("gmaxs", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gmaxs", "['']", "- invalid operand data type");
  nfail += checkExcp ("gsums", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gsums", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gsums", "['']", "- invalid operand data type");
  nfail += checkExcp ("gproducts", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gproducts", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gproducts", "['']", "- invalid operand data type");
  nfail += checkExcp ("gsumsqrs", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gsumsqrs", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gsumsqrs", "['']", "- invalid operand data type");
  nfail += checkExcp ("gmeans", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gmeans", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gmeans", "['']", "- invalid operand data type");
  nfail += checkExcp ("gvariances", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gvariances", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gvariances", "['']", "- invalid operand data type");
  nfail += checkExcp ("gsamplevariances", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gsamplevariances", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gsamplevariances", "['']", "- invalid operand data type");
  nfail += checkExcp ("gstddevs", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gstddevs", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gstddevs", "['']", "- invalid operand data type");
  nfail += checkExcp ("gsamplestddevs", "[T]", "- invalid operand data type");
  nfail += checkExcp ("gsamplestddevs", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gsamplestddevs", "['']", "- invalid operand data type");
  nfail += checkExcp ("grmss", "[T]", "- invalid operand data type");
  nfail += checkExcp ("grmss", "[1i]", "- invalid operand data type");
  nfail += checkExcp ("grmss", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("grmss", "['']", "- invalid operand data type");
  nfail += checkExcp ("ganys", "[1]", "- invalid operand data type");
  nfail += checkExcp ("ganys", "[1.]", "- invalid operand data type");
  nfail += checkExcp ("ganys", "[1i]", "- invalid operand data type");
  nfail += checkExcp ("ganys", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("ganys", "['']", "- invalid operand data type");
  nfail += checkExcp ("galls", "[1]", "- invalid operand data type");
  nfail += checkExcp ("galls", "[1.]", "- invalid operand data type");
  nfail += checkExcp ("galls", "[1i]", "- invalid operand data type");
  nfail += checkExcp ("galls", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("galls", "['']", "- invalid operand data type");
  nfail += checkExcp ("gntrues", "[1]", "- invalid operand data type");
  nfail += checkExcp ("gntrues", "[1.]", "- invalid operand data type");
  nfail += checkExcp ("gntrues", "[1i]", "- invalid operand data type");
  nfail += checkExcp ("gntrues", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gntrues", "['']", "- invalid operand data type");
  nfail += checkExcp ("gnfalses", "[1]", "- invalid operand data type");
  nfail += checkExcp ("gnfalses", "[1.]", "- invalid operand data type");
  nfail += checkExcp ("gnfalses", "[1i]", "- invalid operand data type");
  nfail += checkExcp ("gnfalses", "[date()]", "- invalid operand data type");
  nfail += checkExcp ("gnfalses", "['']", "- invalid operand data type");
  nfail += checkExcp ("gcount", "2", "only use in SELECT or HAVING");
  nfail += checkExcp ("ghist", "2,3,4,5", "only use in SELECT or HAVING");
  nfail += checkExcp ("gaggr", "2", "only use in SELECT or HAVING");
  nfail += checkExcp ("gmedian", "2", "only use in SELECT or HAVING");
  nfail += checkExcp ("gfractile", "2,3", "only use in SELECT or HAVING");
  return nfail;
}

int testNoArrArg()
{
  cout<<"  testing array functions with invalid arguments ..."<<endl;
  int nfail = 0;
  nfail += checkExcp ("transpose", "1,1", " has to be an array");
  nfail += checkExcp ("areverse", "1,1", " has to be an array");
  nfail += checkExcp ("diagonal", "1,1", " has to be an array");
  nfail += checkExcp ("resize", "1,1", " has to be an array");
  nfail += checkExcp ("sums", "1,1", " has to be an array");
  nfail += checkExcp ("products", "1,1", " has to be an array");
  nfail += checkExcp ("sumsqrs", "1,1", " has to be an array");
  nfail += checkExcp ("mins", "1,1", " has to be an array");
  nfail += checkExcp ("maxs", "1,1", " has to be an array");
  nfail += checkExcp ("means", "1,1", " has to be an array");
  nfail += checkExcp ("variances", "1,1", " has to be an array");
  nfail += checkExcp ("samplevariances", "1,1", " has to be an array");
  nfail += checkExcp ("stddevs", "1,1", " has to be an array");
  nfail += checkExcp ("samplestddevs", "1,1", " has to be an array");
  nfail += checkExcp ("avdevs", "1,1", " has to be an array");
  nfail += checkExcp ("rmss", "1,1", " has to be an array");
  nfail += checkExcp ("medians", "1,1", " has to be an array");
  nfail += checkExcp ("fractiles", "1,0.5,1", " has to be an array");
  nfail += checkExcp ("alls", "1,1", " has to be an array");
  nfail += checkExcp ("anys", "1,1", " has to be an array");
  nfail += checkExcp ("ntrues", "1,1", " has to be an array");
  nfail += checkExcp ("nfalses", "1,1", " has to be an array");
  nfail += checkExcp ("runningsum", "1,1", " has to be an array");
  nfail += checkExcp ("runningproduct", "1,1", " has to be an array");
  nfail += checkExcp ("runningsumsqr", "1,1", " has to be an array");
  nfail += checkExcp ("runningmin", "1,1", " has to be an array");
  nfail += checkExcp ("runningmax", "1,1", " has to be an array");
  nfail += checkExcp ("runningmean", "1,1", " has to be an array");
  nfail += checkExcp ("runningvariance", "1,1", " has to be an array");
  nfail += checkExcp ("runningsamplevariance", "1,1", " has to be an array");
  nfail += checkExcp ("runningstddev", "1,1", " has to be an array");
  nfail += checkExcp ("runningsamplestddev", "1,1", " has to be an array");
  nfail += checkExcp ("runningavdev", "1,1", " has to be an array");
  nfail += checkExcp ("runningrms", "1,1", " has to be an array");
  nfail += checkExcp ("runningmedian", "1,1", " has to be an array");
  nfail += checkExcp ("runningfractile", "1,0.5,1", " has to be an array");
  nfail += checkExcp ("runningall", "1,1", " has to be an array");
  nfail += checkExcp ("runningany", "1,1", " has to be an array");
  nfail += checkExcp ("runningntrue", "1,1", " has to be an array");
  nfail += checkExcp ("runningnfalse", "1,1", " has to be an array");
  nfail += checkExcp ("boxedsum", "1,1", " has to be an array");
  nfail += checkExcp ("boxedproduct", "1,1", " has to be an array");
  nfail += checkExcp ("boxedsumsqr", "1,1", " has to be an array");
  nfail += checkExcp ("boxedmin", "1,1", " has to be an array");
  nfail += checkExcp ("boxedmax", "1,1", " has to be an array");
  nfail += checkExcp ("boxedmean", "1,1", " has to be an array");
  nfail += checkExcp ("boxedvariance", "1,1", " has to be an array");
  nfail += checkExcp ("boxedsamplevariance", "1,1", " has to be an array");
  nfail += checkExcp ("boxedstddev", "1,1", " has to be an array");
  nfail += checkExcp ("boxedsamplestddev", "1,1", " has to be an array");
  nfail += checkExcp ("boxedavdev", "1,1", " has to be an array");
  nfail += checkExcp ("boxedrms", "1,1", " has to be an array");
  nfail += checkExcp ("boxedmedian", "1,1", " has to be an array");
  nfail += checkExcp ("boxedfractile", "1,0.5,1", " has to be an array");
  nfail += checkExcp ("boxedall", "1,1", " has to be an array");
  nfail += checkExcp ("boxedany", "1,1", " has to be an array");
  nfail += checkExcp ("boxedntrue", "1,1", " has to be an array");
  nfail += checkExcp ("boxednfalse", "1,1", " has to be an array");
  nfail += checkExcp ("array", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("transpose", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("areverse", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("diagonal", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("sums", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("products", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("sumsqrs", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("mins", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("maxs", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("means", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("variances", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("samplevariances", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("stddevs", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("samplestddevs", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("avdevs", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("rmss", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("medians", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("fractiles", "1,0.5,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("alls", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("anys", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("ntrues", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("nfalses", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningsum", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningproduct", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningsumsqr", "1,[1],1", "are not one or more scalars or ");
  nfail += checkExcp ("runningmin", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningmax", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningmean", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningvariance", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningsamplevariance", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningstddev", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningsamplestddev", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningavdev", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningrms", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningmedian", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningfractile", "1,0.5,[1],1", "are not one or more scalars or ");
  nfail += checkExcp ("runningall", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningany", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningntrue", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("runningnfalse", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedsum", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedproduct", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedsumsqr", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedmin", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedmax", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedmean", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedvariance", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedsamplevariance", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedstddev", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedsamplestddev", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedavdev", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedrms", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedmedian", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedfractile", "1,0.5,[1],[2]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedall", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedany", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxedntrue", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("boxednfalse", "1,1,[1]", "are not one or more scalars or ");
  nfail += checkExcp ("hdms", "13h14m15", " has to be an array");
  nfail += checkExcp ("ANGDIST", "13h14m15, [0,0]", " have to be arrays");
  nfail += checkExcp ("ANGDIST", "[0,0], 13h14m15", " have to be arrays");
  nfail += checkExcp ("ANGDIST", "[0,0], [0,0,0]", " multiple of 2 values");
  nfail += checkExcp ("ANGDIST", "[0], [0,0]", " multiple of 2 values");
  nfail += checkExcp ("ANGDIST", "[0,0,0,0,0,0], [0,0,0,0]", " have equal length");
  nfail += checkExcp ("ANGDISTx", "13h14m15, [0,0]", " have to be arrays");
  nfail += checkExcp ("ANGDISTx", "[0,0], 13h14m15", " have to be arrays");
  nfail += checkExcp ("ANGDISTx", "[0,0,0,0], [0,0,0]", " multiple of 2 values");
  nfail += checkExcp ("ANGDISTx", "[0], [0,0]", " multiple of 2 values");
  nfail += checkExcp ("gmins", "2", " has to be an array");
  nfail += checkExcp ("gmaxs", "2", " has to be an array");
  nfail += checkExcp ("gsums", "2", " has to be an array");
  nfail += checkExcp ("gproducts", "2", " has to be an array");
  nfail += checkExcp ("gsumsqrs", "2", " has to be an array");
  nfail += checkExcp ("gmeans", "2", " has to be an array");
  nfail += checkExcp ("gvariances", "2", " has to be an array");
  nfail += checkExcp ("gsamplevariances", "2", " has to be an array");
  nfail += checkExcp ("gstddevs", "2", " has to be an array");
  nfail += checkExcp ("gsamplestddevs", "2", " has to be an array");
  nfail += checkExcp ("grmss", "2", " has to be an array");
  nfail += checkExcp ("ganys", "2", " has to be an array");
  nfail += checkExcp ("galls", "2", " has to be an array");
  nfail += checkExcp ("gntrues", "2", " has to be an array");
  nfail += checkExcp ("gnfalses", "2", " has to be an array");
  nfail += checkExcp ("angdist", "2,3", " have to be arrays");
  nfail += checkExcp ("angdistx", "2,3", " have to be arrays");
  nfail += checkExcp ("near", "[1.,-1], 1., [1e-2, 1e-3]", " has to be a scalar");
  return nfail;
}

int testInvUnit()
{
  int nfail = 0;
  nfail += checkExcp ("sin", "2kg", " Units rad and kg do not conform");
  nfail += checkExcp ("cos", "2m", " Units rad and m do not conform");
  nfail += checkExcp ("tan", "2kg", " Units rad and kg do not conform");
  nfail += checkExcp ("sqrt", "2s", " Illegal unit dimensions for root");
  nfail += checkExcp ("mjdtodate", "2 kg", " Units d and kg do not conform");
  nfail += checkExcp ("hms", "2 kg", " Units rad and kg do not conform");
  nfail += checkExcp ("dms", "2 kg", " Units rad and kg do not conform");
  nfail += checkExcp ("hdms", "[2,3] kg", " Units rad and kg do not conform");
  nfail += checkExcp ("angdist", "[2,2]kg, [3,3]", " Units rad and kg do not conform");
  nfail += checkExcp ("angdistx", "[2,2]kg, [3,3]", " Units rad and kg do not conform");
  nfail += checkExcp ("anycone", "[2,2], [3,3,2]kg", " Units rad and kg do not conform");
  nfail += checkExcp ("anycone", "[2,2], [3,3], 2kg", " Units rad and kg do not conform");
  nfail += checkExcp ("findcone", "[2,2], [3,3,2]kg", " Units rad and kg do not conform");
  nfail += checkExcp ("findcone", "[2,2], [3,3], 2kg", " Units rad and kg do not conform");
  nfail += checkExcp ("cones", "[2,2]m, [3,3,2]", " Units rad and m do not conform");
  nfail += checkExcp ("cones", "[2,2], [3,3], 2kg", " Units rad and kg do not conform");
  return nfail;
}

int main()
{
  int nfail = 0;
  try {
    nfail += testScaBool();
    nfail += testScaInt();
    nfail += testScaDouble();
    nfail += testScaDComplex();
    nfail += testScaDateTime();
    nfail += testScaString();
    nfail += testRegex();
    nfail += testCone();
    nfail += testArrBool();
    nfail += testArrInt();
    nfail += testArrDouble();
    nfail += testArrDComplex();
    nfail += testArrString();
    nfail += testMaskedArr();
    nfail += testLessArg();
    nfail += testMoreArg();
    nfail += testInvScaArg();
    nfail += testNoArrArg();
    nfail += testInvUnit();
  } catch (const AipsError& x) {
    cout << "\nCaught an exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << ntest << " tests done of which " << nfail << " failed" << endl;
  AlwaysAssertExit (nfail==0);
  return 0;               // successfully executed
}
