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
//#        Internet email: aips2-request@nrao.edu.
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


void checkMatchBool (const TableExprNodeSetElem& tset, bool val, bool result)
{
  cout << "checkMatchBool " << val << ' ' << result << endl;
  bool res = false;
  tset.getElem()->matchBool (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = false;
  newElem->matchBool (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchInt (const TableExprNodeSetElem& tset, int64_t val, bool result)
{
  cout << "checkMatchInt " << val << ' ' << result << endl;
  bool res = false;
  tset.getElem()->matchInt (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = false;
  newElem->matchInt (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDouble (const TableExprNodeSetElem& tset, double val, bool result)
{
  cout << "checkMatchDouble " << val << ' ' << result << endl;
  bool res = false;
  tset.getElem()->matchDouble (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = false;
  newElem->matchDouble (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDComplex (const TableExprNodeSetElem& tset, const DComplex& val,
                         bool result)
{
  cout << "checkMatchDComplex " << val << ' ' << result << endl;
  bool res = false;
  tset.getElem()->matchDComplex (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = false;
  newElem->matchDComplex (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchString (const TableExprNodeSetElem& tset, const String& val,
                       bool result)
{
  cout << "checkMatchString " << val << ' ' << result << endl;
  bool res = false;
  tset.getElem()->matchString (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = false;
  newElem->matchString (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

void checkMatchDate (const TableExprNodeSetElem& tset, MVTime val, bool result)
{
  cout << "checkMatchDate " << double(val) << ' ' << result << endl;
  bool res = false;
  tset.getElem()->matchDate (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
  // Evaluate the element and test if result still matches.
  TENSEBShPtr newElem (tset.getElem()->evaluate (TableExprId(0)));
  res = false;
  newElem->matchDate (&res, &val, 1, 0);
  AlwaysAssertExit (res == result);
}

bool testAttr (const TableExprNodeSetElem& elem,
               TableExprNodeRep::NodeDataType dt,
               bool hasStart, bool hasEnd, bool hasIncrement,
               bool isDiscrete, bool isSingle, const String& unit=String(),
               bool isLeftClosed=false, bool isRightClosed=false,
               bool isMidWidth=false)
{
  bool res = (elem.getElem()->dataType() == dt  &&
              bool(elem.getElem()->start()) == hasStart  &&
              bool(elem.getElem()->end()) == hasEnd  &&
              bool(elem.getElem()->increment()) == hasIncrement  &&
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
  bool res1 = (newElem->dataType() == dt  &&
               bool(newElem->start()) == hasStart  &&
               bool(newElem->end()) == hasEnd  &&
               bool(newElem->increment()) == hasIncrement  &&
               newElem->isDiscrete() == isDiscrete  &&
               newElem->isSingle() == isSingle  &&
               newElem->unit()== unit  &&
               newElem->isLeftClosed() == isLeftClosed  &&
               newElem->isRightClosed() == isRightClosed  &&
               newElem->isMidWidth() == false  &&
               newElem->isConstant());
  return res && res1;
}

void doDiscreteBool()
{
  TableExprNode st(true);
  bool failed= false;
  try {
    TableExprNodeSetElem tset(&st, 0, 0, false);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = true;
  }
  AlwaysAssertExit (failed);
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTBool,
                                true, false, false,
                                true, true));
    Vector<bool> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allEQ(Vector<bool>(1,true), vec));
    checkMatchBool (tset, true, true);
    checkMatchBool (tset, false, false);
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
    TableExprNodeSetElem tset(&st, &end, &incr, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                true, true, true,
                                true, false));
    Vector<int64_t> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<int64_t> exp(50);
    indgen (exp, int64_t(1), int64_t(2));
    AlwaysAssertExit (allEQ(exp, vec));
    checkMatchInt (tset, 1, true);
    checkMatchInt (tset, 2, false);
  }
  {
    TableExprNodeSetElem tset(&stn, &endn, &incrn, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                true, true, true,
                                true, false));
    Vector<int64_t> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<int64_t> exp(50);
    indgen (exp, int64_t(-1), int64_t(-2));
    AlwaysAssertExit (allEQ(exp, vec));
    checkMatchInt (tset, -7, true);
    checkMatchInt (tset, -10, false);
  }
  {
    TableExprNodeSetElem tset(&stn, 0, &incrn, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                true, false, true,
                                true, false));
    Vector<int64_t> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    AlwaysAssertExit (cnt == 1  &&  vec[0] == -1);
    checkMatchInt (tset, -1, true);
    checkMatchInt (tset, -2, false);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                true, true, true,
                                true, false));
    Vector<int64_t> vec(51);
    vec[0] = -3;
    vec[1] = -1;
    int64_t cnt=2;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<int64_t> exp(51);
    indgen (exp, int64_t(-3), int64_t(2));
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                true, true, false,
                                true, false));
    Vector<int64_t> vec(98);
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<int64_t> exp(98);
    indgen (exp, int64_t(1));
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                true, false, false,
                                true, false));
    Vector<int64_t> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allEQ(Vector<int64_t>(), vec));
  }
  {
    TableExprNodeSetElem tset(0, &end, 0, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                false, true, false,
                                true, false));
    Vector<int64_t> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<int64_t> exp(100);
    indgen (exp);
    AlwaysAssertExit (allEQ(exp, vec));
  }
  {
    TableExprNodeSetElem tset(0, 0, 0, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                false, false, false,
                                true, false));
    Vector<int64_t> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allEQ(Vector<int64_t>(), vec));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTInt,
                                true, false, false,
                                true, true));
    Vector<int64_t> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allEQ(Vector<int64_t>(1,1), vec));
    checkMatchInt (tset, 1, true);
    checkMatchInt (tset, 2, false);
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
    TableExprNodeSetElem tset(&st, &end, &incr, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, true,
                                true, false, "dm"));
    Vector<double> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<double> exp(46);
    indgen (exp, 1., 0.2);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
    checkMatchDouble (tset, 3., true);
    checkMatchDouble (tset, 3.1, false);
  }
  {
    TableExprNodeSetElem tset(&stn, &endn, &incrn, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, true,
                                true, false));
    Vector<double> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<double> exp(36);
    indgen (exp, 100., -2.5);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
    checkMatchDouble (tset, 100., true);
    checkMatchDouble (tset, 12.51, false);
    checkMatchDouble (tset, 12.5, true);
    checkMatchDouble (tset, 10., false);
  }
  {
    TableExprNodeSetElem tset(&stn, 0, &incrn, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, false, true,
                                true, false));
    Vector<double> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    AlwaysAssertExit (cnt == 0);
    checkMatchDouble (tset, 100., true);
    checkMatchDouble (tset, -100., true);
    checkMatchDouble (tset, -102., false);
    checkMatchDouble (tset, 102.5, false);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, true,
                                true, false, "dm"));
    Vector<double> vec(25);
    vec[0] = 0.6;
    vec[1] = 0.8;
    int64_t cnt=2;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<double> exp(48);
    indgen (exp, 0.6, 0.2);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, false,
                                true, false, "dm"));
    Vector<double> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    Vector<double> exp(10);
    indgen (exp, 1., 1.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, false, false,
                                true, false, "dm"));
    Vector<double> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allNear(Vector<double>(), vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, false, false,
                                true, true, "dm"));
    Vector<double> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allNear(Vector<double>(1, 1.), vec, 1e-13));
    checkMatchDouble (tset, 1., true);
    checkMatchDouble (tset, -2., false);
  }
}

void doDiscreteDComplex()
{
  TableExprNode st(DComplex(1,2));
  bool failed= false;
  try {
    TableExprNodeSetElem tset(&st, 0, 0, false);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = true;
  }
  AlwaysAssertExit (failed);
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTComplex,
                                true, false, false,
                                true, true));
    Vector<DComplex> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allEQ(Vector<DComplex>(1,DComplex(1,2)), vec));
    checkMatchDComplex (tset, DComplex(1,2), true);
    checkMatchDComplex (tset, DComplex(1,2.1), false);
  }
}

void doDiscreteString()
{
  TableExprNode st("abcd");
  bool failed= false;
  try {
    TableExprNodeSetElem tset(&st, 0, 0, false);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = true;
  }
  AlwaysAssertExit (failed);
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                true, false, false,
                                true, true));
    Vector<String> vec;
    int64_t cnt=0;
    tset.getElem()->fillVector (vec, cnt, 0);
    vec.resize (cnt, true);
    AlwaysAssertExit (allEQ(Vector<String>(1,"abcd"), vec));
    checkMatchString (tset, "abcd", true);
    checkMatchString (tset, "abc", false);
    checkMatchString (tset, "abcde", false);
  }
}

void doDiscreteDate()
{
  TableExprNode st(datetime("5Apr09/12:"));
  TableExprNode end(datetime("7May09/12:"));
  TableExprNode incr(7);
  {
    TableExprNodeSetElem tset(&st, &end, &incr, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, true, true,
                                true, false));
    Vector<MVTime> vect;
    int64_t cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, true);
    Vector<double> vec(vect.size());
    convertArray (vec, vect);
    Vector<double> exp(5);
    indgen (exp, 54926.5, 7.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
    checkMatchDate (tset, 54926.5, true);
    checkMatchDate (tset, 3.1, false);
  }
  {
    TableExprNodeSetElem tset(&st, &end, &incr, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, true, true,
                                true, false));
    Vector<MVTime> vect;
    int64_t cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, true);
    Vector<double> vec(vect.size());
    convertArray (vec, vect);
    Vector<double> exp(5);
    indgen (exp, 54926.5, 7.);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, &end, 0, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, true, false,
                                true, false));
    Vector<MVTime> vect;
    int64_t cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, true);
    Vector<double> vec(vect.size());
    convertArray (vec, vect);
    Vector<double> exp(32);
    indgen (exp, 54926.5);
    AlwaysAssertExit (allNear(exp, vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(&st, 0, 0, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, false, false,
                                true, false));
    Vector<MVTime> vect;
    int64_t cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, true);
    Vector<double> vec(vect.size());
    convertArray (vec, vect);
    AlwaysAssertExit (allNear(Vector<double>(), vec, 1e-13));
  }
  {
    TableExprNodeSetElem tset(st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, false, false,
                                true, true));
    Vector<MVTime> vect;
    int64_t cnt=0;
    tset.getElem()->fillVector (vect, cnt, 0);
    vect.resize (cnt, true);
    Vector<double> vec(vect.size());
    convertArray (vec, vect);
    AlwaysAssertExit (allNear(Vector<double>(1, 54926.5), vec, 1e-13));
    checkMatchDate (tset, 54926.5, true);
    checkMatchDate (tset, 54926.6, false);
  }
}

void doIntervalBool()
{
  TableExprNode st(true);
  TableExprNode end(false);
  bool failed= false;
  try {
    TableExprNodeSetElem tset(false, st, end, false);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = true;
  }
  AlwaysAssertExit (failed);
}

void doIntervalInt()
{
  {
    TableExprNode st(1);
    TableExprNode end(99);
    TableExprNodeSetElem tset(false, st, end, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, false,
                                false, false, "", false, false));
  }
  {
    TableExprNode st(1);
    TableExprNode end(99.);
    TableExprNodeSetElem tset(false, st, end, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, false,
                                false, false, "", false, true));
  }
}

void doIntervalDouble()
{
  TableExprNode st(1);
  st.useUnit ("m");
  TableExprNode end(989.5);
  end.useUnit ("cm");
  {
    TableExprNodeSetElem tset(false, st, end, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, false,
                                false, false, "m", false, false));
    checkMatchDouble (tset, -1., false);
    checkMatchDouble (tset, 1., false);
    checkMatchDouble (tset, 9., true);
    checkMatchDouble (tset, 9.9, false);   // unit is m
    checkMatchDouble (tset, 10., false);
  }
  {
    TableExprNodeSetElem tset(true, st, end, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, true, false,
                                false, false, "m", true, true));
    checkMatchDouble (tset, -1., false);
    checkMatchDouble (tset, 1., true);
    checkMatchDouble (tset, 9., true);
    checkMatchDouble (tset, 9.9, false);
    checkMatchDouble (tset, 10., false);
  }
  {
    TableExprNodeSetElem tset(true, st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                true, false, false,
                                false, false, "m", true, false));
    checkMatchDouble (tset, -1., false);
    checkMatchDouble (tset, 1., true);
    checkMatchDouble (tset, 9., true);
    checkMatchDouble (tset, 9.9, true);
    checkMatchDouble (tset, 10., true);
  }
  {
    TableExprNodeSetElem tset(end, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDouble,
                                false, true, false,
                                false, false, "cm", false, true));
    checkMatchDouble (tset, -1., true);
    checkMatchDouble (tset, 1., true);
    checkMatchDouble (tset, 989., true);
    checkMatchDouble (tset, 990, false);     // unit is cm
    checkMatchDouble (tset, 1000., false);
  }
}

void doIntervalDComplex()
{
  TableExprNode st(1);
  TableExprNode end(DComplex(1,0));
  bool failed= false;
  try {
    TableExprNodeSetElem tset(false, st, end, false);
  } catch (std::exception& x) {
    cout <<"Expected: " << x.what() << endl;
    failed = true;
  }
  AlwaysAssertExit (failed);
}

void doIntervalString()
{
  TableExprNode st("abcd");
  TableExprNode end("cde");
  {
    TableExprNodeSetElem tset(false, st, end, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                true, true, false,
                                false, false, "", false, false));
    checkMatchString (tset, "abc", false);
    checkMatchString (tset, "abcd", false);
    checkMatchString (tset, "abcde", true);
    checkMatchString (tset, "cde", false);
    checkMatchString (tset, "cdef", false);
  }
  {
    TableExprNodeSetElem tset(true, st, end, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                true, true, false,
                                false, false, "", true, true));
    checkMatchString (tset, "abc", false);
    checkMatchString (tset, "abcd", true);
    checkMatchString (tset, "abcde", true);
    checkMatchString (tset, "cde", true);
    checkMatchString (tset, "cdef", false);
  }
  {
    TableExprNodeSetElem tset(false, st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                true, false, false,
                                false, false, "", false, false));
    checkMatchString (tset, "abc", false);
    checkMatchString (tset, "abcd", false);
    checkMatchString (tset, "abcde", true);
    checkMatchString (tset, "cde", true);
    checkMatchString (tset, "cdef", true);
  }
  {
    TableExprNodeSetElem tset(end, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTString,
                                false, true, false,
                                false, false, "", false, false));
    checkMatchString (tset, "abc", true);
    checkMatchString (tset, "abcd", true);
    checkMatchString (tset, "abcde", true);
    checkMatchString (tset, "cde", false);
    checkMatchString (tset, "cdef", false);
  }
}

void doIntervalDate()
{
  TableExprNode st(datetime("5Apr09/12:"));
  TableExprNode end(datetime("7May09/12:"));
  {
    TableExprNodeSetElem tset(false, st, end, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, true, false,
                                false, false, "", false, false));
    checkMatchDate (tset, 54926.0, false);
    checkMatchDate (tset, 54926.5, false);
    checkMatchDate (tset, 54940.0, true);
    checkMatchDate (tset, 54958.5, false);
    checkMatchDate (tset, 54959.0, false);
  }
  {
    TableExprNodeSetElem tset(true, st, end, true);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, true, false,
                                false, false, "", true, true));
    checkMatchDate (tset, 54926.0, false);
    checkMatchDate (tset, 54926.5, true);
    checkMatchDate (tset, 54940.0, true);
    checkMatchDate (tset, 54958.5, true);
    checkMatchDate (tset, 54959.0, false);
  }
  {
    TableExprNodeSetElem tset(false, st);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                true, false, false,
                                false, false, "", false, false));
    checkMatchDate (tset, 54926.0, false);
    checkMatchDate (tset, 54926.5, false);
    checkMatchDate (tset, 54940.0, true);
    checkMatchDate (tset, 54958.5, true);
    checkMatchDate (tset, 54959.0, true);
  }
  {
    TableExprNodeSetElem tset(end, false);
    AlwaysAssertExit (testAttr (tset, TableExprNodeRep::NTDate,
                                false, true, false,
                                false, false, "", false, false));
    checkMatchDate (tset, 54926.0, true);
    checkMatchDate (tset, 54926.5, true);
    checkMatchDate (tset, 54940.0, true);
    checkMatchDate (tset, 54958.5, false);
    checkMatchDate (tset, 54959.0, false);
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
                                true, true, false,
                                false, false, "m", true, true, true));
    checkMatchDouble (tset, -0.005, false);
    checkMatchDouble (tset, 0., true);
    checkMatchDouble (tset, 1., true);
    checkMatchDouble (tset, 2., true);   // unit is m
    checkMatchDouble (tset, 2.005, false);
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
                                true, true, false,
                                false, false, "", true, true, true));
    checkMatchDate (tset, 54926.24, false);
    checkMatchDate (tset, 54926.25, true);
    checkMatchDate (tset, 54926.5, true);
    checkMatchDate (tset, 54926.75, true);
    checkMatchDate (tset, 54926.76, false);
  }
}


#define tryError(CMD) \
  { \
    bool ok = true;                            \
    try { \
      CMD; \
      ok = false; \
    } catch (const std::exception& x) { \
      cout << "Expected: " << x.what() << endl; \
    } \
    AlwaysAssertExit (ok); \
  }

void doErrors()
{
  cout << "Trying erroneous commands ..." << endl;
  Vector<int32_t> vec({1,2});
  TableExprNode nullNode;
  TableExprNode boolNode(true);
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
  tryError (TableExprNodeSetElemCont(true, arrNode));
  tryError (TableExprNodeSetElemCont(arrNode, true));
  tryError (TableExprNodeSetElemCont(true, intNode, arrNode, true));
  tryError (TableExprNodeSetElemCont(true, boolNode));
  tryError (TableExprNodeSetElemCont(cmplNode, true));
  tryError (TableExprNodeSetElemCont(true, intNode, timeNode, true));
  tryError (TableExprNodeSetElemCont(true, intNode, dblNode, true));
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
