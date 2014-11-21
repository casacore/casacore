//# tExprNode.cc: Test program for the ExprNodeSet selection classes
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
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the ExprNodeSet selection classes.
// </summary>


void checkMatchBool (const TableExprNodeSetElem& tset, Bool val, Bool result)
{
  cout << "checkMatchBool " << val << ' ' << result << endl;
  Bool res = False;
  tset.matchBool (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchInt (const TableExprNodeSetElem& tset, Int64 val, Bool result)
{
  cout << "checkMatchInt " << val << ' ' << result << endl;
  Bool res = False;
  tset.matchInt (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDouble (const TableExprNodeSetElem& tset, Double val, Bool result)
{
  cout << "checkMatchDouble " << val << ' ' << result << endl;
  Bool res = False;
  tset.matchDouble (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDComplex (const TableExprNodeSetElem& tset, const DComplex& val,
                         Bool result)
{
  cout << "checkMatchDComplex " << val << ' ' << result << endl;
  Bool res = False;
  tset.matchDComplex (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchString (const TableExprNodeSetElem& tset, const String& val,
                       Bool result)
{
  cout << "checkMatchString " << val << ' ' << result << endl;
  Bool res = False;
  tset.matchString (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDate (const TableExprNodeSetElem& tset, MVTime val, Bool result)
{
  cout << "checkMatchDate " << Double(val) << ' ' << result << endl;
  Bool res = False;
  tset.matchDate (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}


void doDiscreteBool()
{
  TableExprNode st(True);
  bool failed= False;
  try {
    TableExprNodeSetElem tset(&st, 0, 0, False);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = True;
  }
  AlwaysAssertExit (failed);
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTBool);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Bool> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allEQ(Vector<Bool>(1,True), vec));
    checkMatchBool (tset, True, True);
    checkMatchBool (tset, False, False);
  }
}

void doDiscreteInt()
{
  TableExprNode st(1);
  TableExprNode end(99);
  TableExprNode incr(2);
  {
    TableExprNodeSetElem tset(&st, &end, &incr, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTInt);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() != 0);
    Vector<Int64> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    Vector<Int64> exp(50);
    indgen (exp, Int64(1), Int64(2));
    AlwaysAssertExit (allEQ(exp, vec));
    checkMatchInt (tset, 1, True);
    checkMatchInt (tset, 2, False);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() != 0);
    Vector<Int64> vec(51);
    vec[0] = -3;
    vec[1] = -1;
    uInt cnt=2;
    tset.fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(51);
    indgen (exp, Int64(-3), Int64(2));
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Int64> vec(98);
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(98);
    indgen (exp, Int64(1));
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Int64> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allEQ(Vector<Int64>(1,1), vec));
  }
  {
    TableExprNodeSetElem tset(0, &end, 0, False);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() == 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Int64> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(100);
    indgen (exp);
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(0, 0, 0, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() == 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Int64> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allEQ(Vector<Int64>(1,0), vec));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Int64> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allEQ(Vector<Int64>(1,1), vec));
    checkMatchInt (tset, 1, True);
    checkMatchInt (tset, 2, False);
  }
}

void doDiscreteDouble()
{
  TableExprNode st(1);
  st.useUnit ("dm");
  TableExprNode end(1.001);
  end.useUnit ("m");
  TableExprNode incr(2);
  incr.useUnit ("cm");
  {
    TableExprNodeSetElem tset(&st, &end, &incr, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDouble);
    AlwaysAssertExit (tset.unit() == "dm");
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() != 0);
    Vector<Double> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Double> exp(46);
    indgen (exp, 1., 0.2);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
    checkMatchDouble (tset, 3., True);
    checkMatchDouble (tset, 3.1, False);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() != 0);
    Vector<Double> vec(25);
    vec[0] = 0.6;
    vec[1] = 0.8;
    uInt cnt=2;
    tset.fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Double> exp(48);
    indgen (exp, 0.6, 0.2);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Double> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Double> exp(10);
    indgen (exp, 1., 1.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Double> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allNear(Vector<Double>(1, 1.), vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<Double> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allNear(Vector<Double>(1, 1.), vec, 1e-13));
    checkMatchDouble (tset, 1., True);
    checkMatchDouble (tset, -2., False);
  }
}

void doDiscreteDComplex()
{
  TableExprNode st(DComplex(1,2));
  bool failed= False;
  try {
    TableExprNodeSetElem tset(&st, 0, 0, False);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = True;
  }
  AlwaysAssertExit (failed);
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTComplex);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<DComplex> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allEQ(Vector<DComplex>(1,DComplex(1,2)), vec));
    checkMatchDComplex (tset, DComplex(1,2), True);
    checkMatchDComplex (tset, DComplex(1,2.1), False);
  }
}

void doDiscreteString()
{
  TableExprNode st("abcd");
  bool failed= False;
  try {
    TableExprNodeSetElem tset(&st, 0, 0, False);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = True;
  }
  AlwaysAssertExit (failed);
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTString);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<String> vec;
    uInt cnt=0;
    tset.fillVector (vec, cnt, 0);
    AlwaysAssertExit (allEQ(Vector<String>(1,"abcd"), vec));
    checkMatchString (tset, "abcd", True);
    checkMatchString (tset, "abc", False);
    checkMatchString (tset, "abcde", False);
  }
}

void doDiscreteDate()
{
  TableExprNode st(datetime("5Apr09/12:"));
  TableExprNode end(datetime("7May09/12:"));
  TableExprNode incr(7);
  {
    TableExprNodeSetElem tset(&st, &end, &incr, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDate);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() != 0);
    Vector<MVTime> vect;
    uInt cnt=0;
    tset.fillVector (vect, cnt, 0);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    Vector<Double> exp(5);
    indgen (exp, 54926.5, 7.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
    checkMatchDate (tset, 54926.5, True);
    checkMatchDate (tset, 3.1, False);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, True);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDate);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() != 0);
    Vector<MVTime> vect;
    uInt cnt=0;
    tset.fillVector (vect, cnt, 0);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    Vector<Double> exp(5);
    indgen (exp, 54926.5, 7.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() != 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<MVTime> vect;
    uInt cnt=0;
    tset.fillVector (vect, cnt, 0);
    vect.resize (cnt, True);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    Vector<Double> exp(32);
    indgen (exp, 54926.5);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, True);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<MVTime> vect;
    uInt cnt=0;
    tset.fillVector (vect, cnt, 0);
    vect.resize (cnt, True);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    AlwaysAssertExit (allNear(Vector<Double>(1, 54926.5), vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (tset.isDiscrete());
    AlwaysAssertExit (tset.isSingle());
    AlwaysAssertExit (tset.start() != 0);
    AlwaysAssertExit (tset.end() == 0);
    AlwaysAssertExit (tset.increment() == 0);
    Vector<MVTime> vect;
    uInt cnt=0;
    tset.fillVector (vect, cnt, 0);
    vect.resize (cnt, True);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    AlwaysAssertExit (allNear(Vector<Double>(1, 54926.5), vec, 1e-13));
    checkMatchDate (tset, 54926.5, True);
    checkMatchDate (tset, 54926.6, False);
  }
}

void doIntervalBool()
{
  TableExprNode st(True);
  TableExprNode end(False);
  bool failed= False;
  try {
    TableExprNodeSetElem tset(False, st, end, False);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = True;
  }
  AlwaysAssertExit (failed);
}

void doIntervalInt()
{
  {
    TableExprNode st(1);
    TableExprNode end(99);
    TableExprNodeSetElem tset(False, st, end, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDouble);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
  }
  {
    TableExprNode st(1);
    TableExprNode end(99.);
    TableExprNodeSetElem tset(False, st, end, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDouble);
  }
}

void doIntervalDouble()
{
  TableExprNode st(1);
  st.useUnit ("m");
  TableExprNode end(989.5);
  end.useUnit ("cm");
  {
    TableExprNodeSetElem tset(False, st, end, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDouble);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (!tset.isLeftClosed());
    AlwaysAssertExit (!tset.isRightClosed());
    checkMatchDouble (tset, -1., False);
    checkMatchDouble (tset, 1., False);
    checkMatchDouble (tset, 9., True);
    checkMatchDouble (tset, 9.9, False);   // unit is m
    checkMatchDouble (tset, 10., False);
  }
  {
    TableExprNodeSetElem tset(True, st, end, True);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDouble);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.isLeftClosed());
    AlwaysAssertExit (tset.isRightClosed());
    checkMatchDouble (tset, -1., False);
    checkMatchDouble (tset, 1., True);
    checkMatchDouble (tset, 9., True);
    checkMatchDouble (tset, 9.9, False);
    checkMatchDouble (tset, 10., False);
  }
  {
    TableExprNodeSetElem tset(True, st);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDouble);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (tset.isLeftClosed());
    AlwaysAssertExit (!tset.isRightClosed());
    checkMatchDouble (tset, -1., False);
    checkMatchDouble (tset, 1., True);
    checkMatchDouble (tset, 9., True);
    checkMatchDouble (tset, 9.9, True);
    checkMatchDouble (tset, 10., True);
  }
  {
    TableExprNodeSetElem tset(end, True);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDouble);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    AlwaysAssertExit (!tset.isLeftClosed());
    AlwaysAssertExit (tset.isRightClosed());
    checkMatchDouble (tset, -1., True);
    checkMatchDouble (tset, 1., True);
    checkMatchDouble (tset, 989., True);
    checkMatchDouble (tset, 990, False);     // unit is cm
    checkMatchDouble (tset, 1000., False);
  }
}

void doIntervalDComplex()
{
  TableExprNode st(1);
  TableExprNode end(DComplex(1,0));
  bool failed= False;
  try {
    TableExprNodeSetElem tset(False, st, end, False);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = True;
  }
  AlwaysAssertExit (failed);
}

void doIntervalString()
{
  TableExprNode st("abcd");
  TableExprNode end("cde");
  {
    TableExprNodeSetElem tset(False, st, end, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTString);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchString (tset, "abc", False);
    checkMatchString (tset, "abcd", False);
    checkMatchString (tset, "abcde", True);
    checkMatchString (tset, "cde", False);
    checkMatchString (tset, "cdef", False);
  }
  {
    TableExprNodeSetElem tset(True, st, end, True);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTString);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchString (tset, "abc", False);
    checkMatchString (tset, "abcd", True);
    checkMatchString (tset, "abcde", True);
    checkMatchString (tset, "cde", True);
    checkMatchString (tset, "cdef", False);
  }
  {
    TableExprNodeSetElem tset(False, st);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTString);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchString (tset, "abc", False);
    checkMatchString (tset, "abcd", False);
    checkMatchString (tset, "abcde", True);
    checkMatchString (tset, "cde", True);
    checkMatchString (tset, "cdef", True);
  }
  {
    TableExprNodeSetElem tset(end, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTString);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchString (tset, "abc", True);
    checkMatchString (tset, "abcd", True);
    checkMatchString (tset, "abcde", True);
    checkMatchString (tset, "cde", False);
    checkMatchString (tset, "cdef", False);
  }
}

void doIntervalDate()
{
  TableExprNode st(datetime("5Apr09/12:"));
  TableExprNode end(datetime("7May09/12:"));
  {
    TableExprNodeSetElem tset(False, st, end, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDate);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchDate (tset, 54926.0, False);
    checkMatchDate (tset, 54926.5, False);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, False);
    checkMatchDate (tset, 54959.0, False);
  }
  {
    TableExprNodeSetElem tset(True, st, end, True);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDate);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchDate (tset, 54926.0, False);
    checkMatchDate (tset, 54926.5, True);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, True);
    checkMatchDate (tset, 54959.0, False);
  }
  {
    TableExprNodeSetElem tset(False, st);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDate);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchDate (tset, 54926.0, False);
    checkMatchDate (tset, 54926.5, False);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, True);
    checkMatchDate (tset, 54959.0, True);
  }
  {
    TableExprNodeSetElem tset(end, False);
    AlwaysAssertExit (tset.dataType() == TableExprNodeRep::NTDate);
    AlwaysAssertExit (!tset.isDiscrete());
    AlwaysAssertExit (!tset.isSingle());
    checkMatchDate (tset, 54926.0, True);
    checkMatchDate (tset, 54926.5, True);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, False);
    checkMatchDate (tset, 54959.0, False);
  }
}

void doSetBool()
{
  TableExprNodeSetElem elem((TableExprNode(True)));
  TableExprNodeSet set;
  set.add (elem);
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasBool (0, True));
  AlwaysAssertExit (!set.hasBool (0, False));
}

void doSetInt()
{
  TableExprNodeSetElem elem((TableExprNode(1)));
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasInt (0, 1));
  AlwaysAssertExit (!set.hasInt (0, 2));
  TableExprNode st(3);
  TableExprNode end(10);
  TableExprNode incr(3);
  set.add (TableExprNodeSetElem(&st, &end, &incr, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasInt (0, 1));
  AlwaysAssertExit (!set.hasInt (0, 2));
  AlwaysAssertExit (set.hasInt (0, 3));
  AlwaysAssertExit (!set.hasInt (0, 4));
  set.add (TableExprNodeSetElem(&st, 0, 0, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.nelements() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasInt (0, 1));
  AlwaysAssertExit (!set.hasInt (0, 2));
  AlwaysAssertExit (set.hasInt (0, 3));
  AlwaysAssertExit (set.hasInt (0, 4));
}

void doSetDouble()
{
  TableExprNodeSetElem elem((TableExprNode(1.)));
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasDouble (0, 1));
  AlwaysAssertExit (!set.hasDouble (0, 2));
  TableExprNode st(3.);
  TableExprNode end(10);
  TableExprNode incr(3);
  set.add (TableExprNodeSetElem(&st, &end, &incr, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasDouble (0, 1));
  AlwaysAssertExit (!set.hasDouble (0, 2));
  AlwaysAssertExit (set.hasDouble (0, 3));
  AlwaysAssertExit (!set.hasDouble (0, 4));
  set.add (TableExprNodeSetElem(&st, 0, 0, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.nelements() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasDouble (0, 1));
  AlwaysAssertExit (!set.hasDouble (0, 2));
  AlwaysAssertExit (set.hasDouble (0, 3));
  AlwaysAssertExit (set.hasDouble (0, 4));
}

void doSetDComplex()
{
  TableExprNodeSetElem elem((TableExprNode(DComplex(1,2))));
  TableExprNodeSet set;
  set.add (elem);
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasDComplex (0, DComplex(1,2)));
  AlwaysAssertExit (!set.hasDComplex (0, DComplex(1,3)));
}

void doSetString()
{
  TableExprNodeSetElem elem((TableExprNode("ger")));
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasString (0, "ger"));
  AlwaysAssertExit (!set.hasString (0, "Ger"));
  TableExprNode st("ger1");
  TableExprNode end("ger9");
  set.add (TableExprNodeSetElem(True, st, end, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (!set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.nelements() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasString (0, "ger"));
  AlwaysAssertExit (!set.hasString (0, "Ger"));
  AlwaysAssertExit (set.hasString (0, "ger1"));
  AlwaysAssertExit (!set.hasString (0, "ger99"));
  set.add (TableExprNodeSetElem(True, st));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (!set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.nelements() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasString (0, "ger"));
  AlwaysAssertExit (!set.hasString (0, "Ger"));
  AlwaysAssertExit (set.hasString (0, "ger1"));
  AlwaysAssertExit (set.hasString (0, "ger99"));
}

void doSetDate()
{
  TableExprNodeSetElem elem((TableExprNode(1.)));
  TableExprNodeSet set;
  set.add (elem);
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 1);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasDouble (0, 1));
  AlwaysAssertExit (!set.hasDouble (0, 2));
  TableExprNode st(3.);
  TableExprNode end(10);
  TableExprNode incr(3);
  set.add (TableExprNodeSetElem(&st, &end, &incr, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.nelements() == 2);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasDouble (0, 1));
  AlwaysAssertExit (!set.hasDouble (0, 2));
  AlwaysAssertExit (set.hasDouble (0, 3));
  AlwaysAssertExit (!set.hasDouble (0, 4));
  set.add (TableExprNodeSetElem(&st, 0, 0, True));
  AlwaysAssertExit (!set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (!set.isBounded());
  AlwaysAssertExit (set.nelements() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasDouble (0, 1));
  AlwaysAssertExit (!set.hasDouble (0, 2));
  AlwaysAssertExit (set.hasDouble (0, 3));
  AlwaysAssertExit (set.hasDouble (0, 4));
}

void doIPosition()
{
  TableExprNodeSet set(IPosition(3,4,5,6));
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTInt);
  AlwaysAssertExit (set.nelements() == 3);
  AlwaysAssertExit (!set.hasArrays());
  AlwaysAssertExit (set.hasInt (0, 4));
  AlwaysAssertExit (set.hasInt (0, 5));
  AlwaysAssertExit (set.hasInt (0, 6));
  AlwaysAssertExit (!set.hasInt (0, 3));
  // Form an index node.
  TableExprNodeIndex inx(set);
  AlwaysAssertExit (inx.isSingle());
  AlwaysAssertExit (inx.getSlicer(0).start() == IPosition(3,4,5,6));
}

void doSlicer()
{
  {
    Slicer sl(IPosition(2,1,2), IPosition(2,10,12), IPosition(2,2,3),
              Slicer::endIsLast);
    TableExprNodeSet set(sl);
    AlwaysAssertExit (!set.isSingle());
    AlwaysAssertExit (set.isDiscrete());
    AlwaysAssertExit (set.isBounded());
    AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTInt);
    AlwaysAssertExit (set.nelements() == 2);
    AlwaysAssertExit (!set.hasArrays());
    AlwaysAssertExit (set[0].start()->getInt(0) == 1);
    AlwaysAssertExit (set[0].end()->getInt(0) == 10);
    AlwaysAssertExit (set[0].increment()->getInt(0) == 2);
    AlwaysAssertExit (set[1].start()->getInt(0) == 2);
    AlwaysAssertExit (set[1].end()->getInt(0) == 12);
    AlwaysAssertExit (set[1].increment()->getInt(0) == 3);
    // Form an index node.
    TableExprNodeIndex inx(set);
    AlwaysAssertExit (!inx.isSingle());
    AlwaysAssertExit (inx.getSlicer(0).start() == IPosition(2,1,2));
    AlwaysAssertExit (inx.getSlicer(0).end() == IPosition(2,10,12));
    AlwaysAssertExit (inx.getSlicer(0).stride() == IPosition(2,2,3));
  }
  {
    Slicer sl(IPosition(2,Slicer::MimicSource,2),
              IPosition(2,10,Slicer::MimicSource),
              Slicer::endIsLast);

    TableExprNodeSet set(sl);
    AlwaysAssertExit (!set.isSingle());
    AlwaysAssertExit (set.isDiscrete());
    AlwaysAssertExit (set.isBounded());
    AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTInt);
    AlwaysAssertExit (set.nelements() == 2);
    AlwaysAssertExit (!set.hasArrays());
    AlwaysAssertExit (set[0].start() == 0);
    cout << set[0].end()->getInt(0) << endl;
    AlwaysAssertExit (set[0].end()->getInt(0) == 10);
    AlwaysAssertExit (set[0].increment()->getInt(0) == 1);
    AlwaysAssertExit (set[1].start()->getInt(0) == 2);
    AlwaysAssertExit (set[1].end() == 0);
    AlwaysAssertExit (set[1].increment()->getInt(0) == 1);
  }
}

void doEmpty()
{
  TableExprNodeSet set;
  AlwaysAssertExit (set.isSingle());
  AlwaysAssertExit (set.isDiscrete());
  AlwaysAssertExit (set.isBounded());
  AlwaysAssertExit (set.dataType() == TableExprNodeRep::NTNumeric);
  AlwaysAssertExit (set.nelements() == 0);
  AlwaysAssertExit (!set.hasArrays());
}


int main()
{
  try {
    doDiscreteBool();
    doDiscreteInt();
    doDiscreteDouble();
    doDiscreteDComplex();
    doDiscreteString();
    doDiscreteDate();
    doIntervalBool();
    doIntervalInt();
    doIntervalDouble();
    doIntervalDComplex();
    doIntervalString();
    doIntervalDate();
    doSetBool();
    doSetInt();
    doSetDouble();
    doSetDComplex();
    doSetString();
    doSetDate();
    doIPosition();
    doSlicer();
    doEmpty();
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
