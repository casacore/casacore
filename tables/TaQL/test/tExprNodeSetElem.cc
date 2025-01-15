//# tExprNodeSet.cc: Test program for the ExprNodeSetElem selection classes
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/tables/TaQL/ExprNodeSetElem.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the ExprNodeSetElem selection classes.
// </summary>


void checkMatchBool (const TableExprNodeSetElem& tset, Bool val, Bool result)
{
  cout << "checkMatchBool " << val << ' ' << result << endl;
  Bool res = False;
  tset.getElem()->matchBool (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = False;
  newElem->matchBool (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchInt (const TableExprNodeSetElem& tset, Int64 val, Bool result)
{
  cout << "checkMatchInt " << val << ' ' << result << endl;
  Bool res = False;
  tset.getElem()->matchInt (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = False;
  newElem->matchInt (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDouble (const TableExprNodeSetElem& tset, Double val, Bool result)
{
  cout << "checkMatchDouble " << val << ' ' << result << endl;
  Bool res = False;
  tset.getElem()->matchDouble (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = False;
  newElem->matchDouble (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDComplex (const TableExprNodeSetElem& tset, const DComplex& val,
                         Bool result)
{
  cout << "checkMatchDComplex " << val << ' ' << result << endl;
  Bool res = False;
  tset.getElem()->matchDComplex (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = False;
  newElem->matchDComplex (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchString (const TableExprNodeSetElem& tset, const String& val,
                       Bool result)
{
  cout << "checkMatchString " << val << ' ' << result << endl;
  Bool res = False;
  tset.getElem()->matchString (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = False;
  newElem->matchString (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDate (const TableExprNodeSetElem& tset, MVTime val, Bool result)
{
  cout << "checkMatchDate " << Double(val) << ' ' << result << endl;
  Bool res = False;
  tset.getElem()->matchDate (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = False;
  newElem->matchDate (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

Bool testAttr (const TableExprNodeSetElem& elem,
               TableExprNodeRep::NodeDataType dt,
               Bool hasStart, Bool hasEnd, Bool hasIncrement,
               Bool isDiscrete, Bool isSingle, const String& unit=String(),
               Bool isLeftClosed=False, Bool isRightClosed=False,
               Bool isMidWidth=False)
{
  Bool res = (elem.getElem()->dataType() == dt  &&
              Bool(elem.getElem()->start()) == hasStart  &&
              Bool(elem.getElem()->end()) == hasEnd  &&
              Bool(elem.getElem()->increment()) == hasIncrement  &&
              elem.getElem()->isDiscrete() == isDiscrete  &&
              elem.getElem()->isSingle() == isSingle  &&
              elem.getElem()->unit() == unit  &&
              elem.getElem()->isLeftClosed() == isLeftClosed  &&
              elem.getElem()->isRightClosed() == isRightClosed  &&
              elem.getElem()->isMidWidth() == isMidWidth  &&
              elem.getElem()->isConstant());
  // Evaluate the element and test if attributes still match.
  // Note that mid-width is evaluated as a normal interval.
  TENSEBShPtr newElem(elem.getElem()->evaluate (TableExprId(0)));
  Bool res1 = (newElem->dataType() == dt  &&
               Bool(newElem->start()) == hasStart  &&
               Bool(newElem->end()) == hasEnd  &&
               Bool(newElem->increment()) == hasIncrement  &&
               newElem->isDiscrete() == isDiscrete  &&
               newElem->isSingle() == isSingle  &&
               newElem->unit()== unit  &&
               newElem->isLeftClosed() == isLeftClosed  &&
               newElem->isRightClosed() == isRightClosed  &&
               newElem->isMidWidth() == False  &&
               newElem->isConstant());
  return res && res1;
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTBool,
                                True, False, False,
                                True, True));
    Vector<Bool> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
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
  TableExprNode stn(-1);
  TableExprNode endn(-99);
  TableExprNode incrn(-2);
  {
    TableExprNodeSetElem tset(&st, &end, &incr, False);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                True, True, True,
                                True, False));
    Vector<Int64> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(50);
    indgen (exp, Int64(1), Int64(2));
    AlwaysAssertExit (allEQ(exp, vec));
    checkMatchInt (tset, 1, True);
    checkMatchInt (tset, 2, False);
  }
  {
    TableExprNodeSetElem tset(&stn, &endn, &incrn, False);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                True, True, True,
                                True, False));
    Vector<Int64> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(50);
    indgen (exp, Int64(-1), Int64(-2));
    AlwaysAssertExit (allEQ(exp, vec));
    checkMatchInt (tset, -7, True);
    checkMatchInt (tset, -10, False);
  }
  {
    TableExprNodeSetElem tset(&stn, 0, &incrn, False);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                True, False, True,
                                True, False));
    Vector<Int64> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    AlwaysAssertExit (cnt == 1  &&  vec[0] == -1);
    checkMatchInt (tset, -1, True);
    checkMatchInt (tset, -2, False);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                True, True, True,
                                True, False));
    Vector<Int64> vec(51);
    vec[0] = -3;
    vec[1] = -1;
    Int64 cnt=2;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(51);
    indgen (exp, Int64(-3), Int64(2));
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                True, True, False,
                                True, False));
    Vector<Int64> vec(98);
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(98);
    indgen (exp, Int64(1));
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                True, False, False,
                                True, False));
    Vector<Int64> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    AlwaysAssertExit (allEQ(Vector<Int64>(), vec));
  }
  {
    TableExprNodeSetElem tset(0, &end, 0, False);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                False, True, False,
                                True, False));
    Vector<Int64> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Int64> exp(100);
    indgen (exp);
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(0, 0, 0, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                False, False, False,
                                True, False));
    Vector<Int64> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    AlwaysAssertExit (allEQ(Vector<Int64>(), vec));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                True, False, False,
                                True, True));
    Vector<Int64> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
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
  TableExprNode stn(100.);
  TableExprNode endn(10.);
  TableExprNode incrn(-2.5);
  {
    TableExprNodeSetElem tset(&st, &end, &incr, False);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, True,
                                True, False, "dm"));
    Vector<Double> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Double> exp(46);
    indgen (exp, 1., 0.2);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
    checkMatchDouble (tset, 3., True);
    checkMatchDouble (tset, 3.1, False);
  }
  {
    TableExprNodeSetElem tset(&stn, &endn, &incrn, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, True,
                                True, False));
    Vector<Double> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Double> exp(36);
    indgen (exp, 100., -2.5);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
    checkMatchDouble (tset, 100., True);
    checkMatchDouble (tset, 12.51, False);
    checkMatchDouble (tset, 12.5, True);
    checkMatchDouble (tset, 10., False);
  }
  {
    TableExprNodeSetElem tset(&stn, 0, &incrn, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, False, True,
                                True, False));
    Vector<Double> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    AlwaysAssertExit (cnt == 0);
    checkMatchDouble (tset, 100., True);
    checkMatchDouble (tset, -100., True);
    checkMatchDouble (tset, -102., False);
    checkMatchDouble (tset, 102.5, False);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, True,
                                True, False, "dm"));
    Vector<Double> vec(25);
    vec[0] = 0.6;
    vec[1] = 0.8;
    Int64 cnt=2;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Double> exp(48);
    indgen (exp, 0.6, 0.2);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, False,
                                True, False, "dm"));
    Vector<Double> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    Vector<Double> exp(10);
    indgen (exp, 1., 1.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, False, False,
                                True, False, "dm"));
    Vector<Double> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
    AlwaysAssertExit (allNear(Vector<Double>(), vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, False, False,
                                True, True, "dm"));
    Vector<Double> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTComplex,
                                True, False, False,
                                True, True));
    Vector<DComplex> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                True, False, False,
                                True, True));
    Vector<String> vec;
    Int64 cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, True);
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, True, True,
                                True, False));
    Vector<MVTime> vect;
    Int64 cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, True);
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, True, True,
                                True, False));
    Vector<MVTime> vect;
    Int64 cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, True);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    Vector<Double> exp(5);
    indgen (exp, 54926.5, 7.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, True, False,
                                True, False));
    Vector<MVTime> vect;
    Int64 cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, True);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    Vector<Double> exp(32);
    indgen (exp, 54926.5);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, False, False,
                                True, False));
    Vector<MVTime> vect;
    Int64 cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, True);
    Vector<Double> vec(vect.size());
    convertArray (vec, vect);
    AlwaysAssertExit (allNear(Vector<Double>(), vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, False, False,
                                True, True));
    Vector<MVTime> vect;
    Int64 cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, False,
                                False, False, "", False, False));
  }
  {
    TableExprNode st(1);
    TableExprNode end(99.);
    TableExprNodeSetElem tset(False, st, end, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, False,
                                False, False, "", False, True));
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, False,
                                False, False, "m", False, False));
    checkMatchDouble (tset, -1., False);
    checkMatchDouble (tset, 1., False);
    checkMatchDouble (tset, 9., True);
    checkMatchDouble (tset, 9.9, False);   // unit is m
    checkMatchDouble (tset, 10., False);
  }
  {
    TableExprNodeSetElem tset(True, st, end, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, False,
                                False, False, "m", True, True));
    checkMatchDouble (tset, -1., False);
    checkMatchDouble (tset, 1., True);
    checkMatchDouble (tset, 9., True);
    checkMatchDouble (tset, 9.9, False);
    checkMatchDouble (tset, 10., False);
  }
  {
    TableExprNodeSetElem tset(True, st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, False, False,
                                False, False, "m", True, False));
    checkMatchDouble (tset, -1., False);
    checkMatchDouble (tset, 1., True);
    checkMatchDouble (tset, 9., True);
    checkMatchDouble (tset, 9.9, True);
    checkMatchDouble (tset, 10., True);
  }
  {
    TableExprNodeSetElem tset(end, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                False, True, False,
                                False, False, "cm", False, True));
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                True, True, False,
                                False, False, "", False, False));
    checkMatchString (tset, "abc", False);
    checkMatchString (tset, "abcd", False);
    checkMatchString (tset, "abcde", True);
    checkMatchString (tset, "cde", False);
    checkMatchString (tset, "cdef", False);
  }
  {
    TableExprNodeSetElem tset(True, st, end, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                True, True, False,
                                False, False, "", True, True));
    checkMatchString (tset, "abc", False);
    checkMatchString (tset, "abcd", True);
    checkMatchString (tset, "abcde", True);
    checkMatchString (tset, "cde", True);
    checkMatchString (tset, "cdef", False);
  }
  {
    TableExprNodeSetElem tset(False, st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                True, False, False,
                                False, False, "", False, False));
    checkMatchString (tset, "abc", False);
    checkMatchString (tset, "abcd", False);
    checkMatchString (tset, "abcde", True);
    checkMatchString (tset, "cde", True);
    checkMatchString (tset, "cdef", True);
  }
  {
    TableExprNodeSetElem tset(end, False);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                False, True, False,
                                False, False, "", False, False));
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
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, True, False,
                                False, False, "", False, False));
    checkMatchDate (tset, 54926.0, False);
    checkMatchDate (tset, 54926.5, False);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, False);
    checkMatchDate (tset, 54959.0, False);
  }
  {
    TableExprNodeSetElem tset(True, st, end, True);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, True, False,
                                False, False, "", True, True));
    checkMatchDate (tset, 54926.0, False);
    checkMatchDate (tset, 54926.5, True);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, True);
    checkMatchDate (tset, 54959.0, False);
  }
  {
    TableExprNodeSetElem tset(False, st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, False, False,
                                False, False, "", False, False));
    checkMatchDate (tset, 54926.0, False);
    checkMatchDate (tset, 54926.5, False);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, True);
    checkMatchDate (tset, 54959.0, True);
  }
  {
    TableExprNodeSetElem tset(end, False);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                False, True, False,
                                False, False, "", False, False));
    checkMatchDate (tset, 54926.0, True);
    checkMatchDate (tset, 54926.5, True);
    checkMatchDate (tset, 54940.0, True);
    checkMatchDate (tset, 54958.5, False);
    checkMatchDate (tset, 54959.0, False);
  }
}

void doMidWidthDouble()
{
  TableExprNode mid(1);
  mid.useUnit ("m");
  TableExprNode width(200);
  width.useUnit ("cm");
  {
    TableExprNodeSetElem tset(mid, width);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                True, True, False,
                                False, False, "m", True, True, True));
    checkMatchDouble (tset, -0.005, False);
    checkMatchDouble (tset, 0., True);
    checkMatchDouble (tset, 1., True);
    checkMatchDouble (tset, 2., True);   // unit is m
    checkMatchDouble (tset, 2.005, False);
  }
}

void doMidWidthDate()
{
  TableExprNode mid(datetime("5Apr09/12:"));   // is MJD 54926.5
  TableExprNode width(12);
  width.useUnit ("h");
  {
    TableExprNodeSetElem tset(mid, width);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                True, True, False,
                                False, False, "", True, True, True));
    checkMatchDate (tset, 54926.24, False);
    checkMatchDate (tset, 54926.25, True);
    checkMatchDate (tset, 54926.5, True);
    checkMatchDate (tset, 54926.75, True);
    checkMatchDate (tset, 54926.76, False);
  }
}


#define tryError(CMD) \
  { \
    Bool ok = True;                            \
    try { \
      CMD; \
      ok = False; \
    } catch (const std::exception& x) { \
      cout << "Expected: " << x.what() << endl; \
    } \
    AlwaysAssertExit (ok); \
  }

void doErrors()
{
  cout << "Trying erroneous commands ..." << endl;
  Vector<Int> vec({1,2});
  TableExprNode nullNode;
  TableExprNode boolNode(True);
  TableExprNode arrNode(vec);
  TableExprNode intNode(3);
  intNode.useUnit("m");
  TableExprNode dblNode(3.);
  dblNode.useUnit("Hz");
  TableExprNode cmplNode((Complex()));
  TableExprNode strNode("aa");
  TableExprNode timeNode((MVTime()));
  tryError (TableExprNodeSetElemDiscrete(arrNode, nullNode, nullNode));
  tryError (TableExprNodeSetElemDiscrete(nullNode, arrNode, nullNode));
  tryError (TableExprNodeSetElemDiscrete(intNode, nullNode, arrNode));
  tryError (TableExprNodeSetElemDiscrete(strNode, nullNode, nullNode));
  tryError (TableExprNodeSetElemDiscrete(nullNode, strNode, nullNode));
  tryError (TableExprNodeSetElemDiscrete(nullNode, intNode, strNode));
  tryError (TableExprNodeSetElemDiscrete(intNode, timeNode, nullNode));
  tryError (TableExprNodeSetElemDiscrete(nullNode, nullNode, timeNode));
  tryError (TableExprNodeSetElemDiscrete(intNode, dblNode, nullNode));
  tryError (TableExprNodeSetElemCont(True, arrNode));
  tryError (TableExprNodeSetElemCont(arrNode, True));
  tryError (TableExprNodeSetElemCont(True, intNode, arrNode, True));
  tryError (TableExprNodeSetElemCont(True, boolNode));
  tryError (TableExprNodeSetElemCont(cmplNode, True));
  tryError (TableExprNodeSetElemCont(True, intNode, timeNode, True));
  tryError (TableExprNodeSetElemCont(True, intNode, dblNode, True));
  tryError (TableExprNodeSetElemMidWidth(intNode, strNode));
  tryError (TableExprNodeSetElemMidWidth(strNode, intNode));
  tryError (TableExprNodeSetElemMidWidth(intNode, nullNode));
  tryError (TableExprNodeSetElemMidWidth(nullNode, intNode));
  tryError (TableExprNodeSetElemMidWidth(dblNode, intNode));
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
    doMidWidthDouble();
    doMidWidthDate();
    doErrors();
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
