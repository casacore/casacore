//# tExprNodeSetOpt.cc: Test program for the ExprNodeSeOpt classes
//# Copyright (C) 2022
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

#include <casacore/tables/TaQL/ExprNodeSetOpt.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the ExprNodeSetOpt classes.
// </summary>


// Execute the test for all values in the test vector.
template<typename T>
void doTest (TableExprNodeSetOptBase& set,
             const Vector<T>& testVec, const Vector<Int64>& expFind)
{
  TableExprId id(0);
  for (size_t i=0; i<testVec.size(); ++i) {
    AlwaysAssertExit (set.find(testVec[i]) == expFind[i]);
    AlwaysAssertExit (set.contains(id, testVec[i]) == (expFind[i] >= 0));
  }
  // Do the test for the full array.
  MArray<bool> res = set.contains (id, MArray<T>(testVec));
  AlwaysAssertExit (allEQ(res.array(), expFind>=Int64(0)));
}

// Original and transformed set should give the same results.
template<typename T>
void doTestOrig (TableExprNodeSet& orig, TableExprNodeSetOptBase& set,
                 const Vector<T>& testVec)
{
  TableExprId id(0);
  for (T v : testVec) {
    AlwaysAssertExit (orig.contains(id,v) == set.contains(id,v));
  }
}


void doDoubleContSet()
{
  {
    // Test closed-closed intervals
    std::vector<double> st ({1,3,5,7,9,11,13,15,17,19,21});
    std::vector<double> end({2,4,6,8,10,12,14,16,18,20,22});
    TableExprNodeSetOptContSet<Double,std::less_equal<Double>,std::less_equal<Double>>
      set (TableExprNodeSet(), st, end,
           std::less_equal<Double>(), std::less_equal<Double>(), "CC");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<double> vec({-0.5, 1, 1.5, 2, 2.5, 13, 13.5, 14, 14.5, 21, 21.5, 22, 22.5});
    Vector<Int64>  exp({-1,   0, 0,   0, -1,  6,  6,    6,  -1,   10, 10,   10, -1});
    doTest (set, vec, exp);
  }
  {
    // Test open-closed intervals
    std::vector<double> st ({1,3,5,7,9,11,13,15,17,19,21});
    std::vector<double> end({2,4,6,8,10,12,14,16,18,20,22});
    TableExprNodeSetOptContSet<Double,std::less<Double>,std::less_equal<Double>>
      set (TableExprNodeSet(), st, end,
           std::less<Double>(), std::less_equal<Double>(), "OC");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<double> vec({-0.5, 1, 1.5, 2, 2.5, 13, 13.5, 14, 14.5, 21, 21.5, 22, 22.5});
    Vector<Int64>  exp({-1,  -1, 0,   0, -1,  -1, 6,    6,  -1,   -1, 10,   10, -1});
    doTest (set, vec, exp);
  }
  {
    // Test closed-open intervals
    std::vector<double> st ({1,3,5,7,9,11,13,15,17,19,21});
    std::vector<double> end({2,4,6,8,10,12,14,16,18,20,22});
    TableExprNodeSetOptContSet<Double,std::less_equal<Double>,std::less<Double>>
      set (TableExprNodeSet(), st, end,
           std::less_equal<Double>(), std::less<Double>(), "CO");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<double> vec({-0.5, 1, 1.5, 2, 2.5, 13, 13.5, 14, 14.5, 21, 21.5, 22, 22.5});
    Vector<Int64>  exp({-1,   0, 0,  -1, -1,  6,  6,    -1, -1,   10, 10,   -1, -1});
    doTest (set, vec, exp);
  }
  {
    // Test open-open intervals
    std::vector<double> st ({1,3,5,7,9,11,13,15,17,19,21});
    std::vector<double> end({2,4,6,8,10,12,14,16,18,20,22});
    TableExprNodeSetOptContSet<Double,std::less<Double>,std::less<Double>>
      set (TableExprNodeSet(), st, end,
           std::less<Double>(), std::less<Double>(), "OO");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<double> vec({-0.5, 1, 1.5, 2, 2.5, 13, 13.5, 14, 14.5, 21, 21.5, 22, 22.5});
    Vector<Int64>  exp({-1,  -1, 0,  -1, -1,  -1, 6,    -1, -1,   -1, 10,   -1, -1});
    doTest (set, vec, exp);
  }
  {
    // Test intervals with mix of open and closed
    std::vector<double> st  ({1,    19,   3,   21});
    std::vector<double> end ({2,    20,   5,   22});
    std::vector<bool> leftC ({false,false,true,true});
    std::vector<bool> rightC({false,true,true,false});
    TableExprNodeSetOptContSetMixOC<Double> set (TableExprNodeSet(), st, end,
                                                 leftC, rightC);
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<double> vec({-0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 5.5,
                        19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5});
    Vector<Int64> exp ({-1,  -1,  0, -1, -1,  2, 2, 2, -1,
                        -1,  1,   1,  -1,   3,  3,    -1, -1});
    doTest (set, vec, exp);
  }
}

void doStringContSet()
{
  std::vector<String> st ({"a1"});
  std::vector<String> end({"a5"});
  {
    // Test closed-closed intervals
    TableExprNodeSetOptContSet<String,std::less_equal<String>,std::less_equal<String>>
      set (TableExprNodeSet(), st, end,
           std::less_equal<String>(), std::less_equal<String>(), "CC");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<String> vec({"a0", "a1", "a3", "a5", "a6"});
    Vector<Int64>  exp({ -1,   0,    0,    0,    -1});
    doTest (set, vec, exp);
  }
  {
    // Test closed-open intervals
    TableExprNodeSetOptContSet<String,std::less_equal<String>,std::less<String>>
      set (TableExprNodeSet(), st, end,
           std::less_equal<String>(), std::less<String>(), "CO");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<String> vec({"a0", "a1", "a3", "a5", "a6"});
    Vector<Int64>  exp({ -1,   0,    0,    -1,   -1});
    doTest (set, vec, exp);
  }
  {
    // Test open-closed intervals
    TableExprNodeSetOptContSet<String,std::less<String>,std::less_equal<String>>
      set (TableExprNodeSet(), st, end,
           std::less<String>(), std::less_equal<String>(), "OC");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<String> vec({"a0", "a1", "a3", "a5", "a6"});
    Vector<Int64>  exp({ -1,   -1,   0,    0,    -1});
    doTest (set, vec, exp);
  }
  {
    // Test open-open intervals
    TableExprNodeSetOptContSet<String,std::less<String>,std::less<String>>
      set (TableExprNodeSet(), st, end,
           std::less<String>(), std::less<String>(), "OO");
    set.show(cout, 2);
    // Vectors of test values and expected index.
    Vector<String> vec({"a0", "a1", "a3", "a5", "a6"});
    Vector<Int64>  exp({ -1,   -1,   0,    -1,   -1});
    doTest (set, vec, exp);
  }
}

void doIntSet()
{
  Vector<Int64> vecset({1,3,8,10,5});
  TableExprNodeSetOptUSet<Int64> set(TableExprNodeSet(), vecset);
  set.show(cout, 2);
  // Vectors of test values and expected index.
  Vector<Int64> vec({1, 3, 5, 8, 10,  0, 2, 11});
  Vector<Int64> exp({0, 1, 4, 2,  3, -1,-1, -1});
  doTest (set, vec, exp);
}

void doStringSet()
{
  Vector<String> vecset({"a", "d", "b"});
  TableExprNodeSetOptUSet<String> set(TableExprNodeSet(), vecset);
  set.show(cout, 2);
  // Vectors of test values and expected index.
  Vector<String> vec({"aa", "a", "b", "d", "bd", "e"});
  Vector<Int64>  exp({ -1,   0,   2,   1,   -1,  -1});
  doTest (set, vec, exp);
}

void doDoubleTransform()
{
  TableExprNode  st1(1),  st2(6),   st3(25),  st4(30),  st5(33);
  TableExprNode end1(8), end2(20), end3(30), end4(33), end5(34);
  // Test with different leftC and rightC.
  // Also such that the 3rd interval is combined with the 4th,
  // but not 4th with 5th (because end4 and st5 are open).
  {
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(False, st1, end1, False));
    set.add (TableExprNodeSetElem(True, st3, end3, True));
    set.add (TableExprNodeSetElem(False, st2, end2, True));
    set.add (TableExprNodeSetElem(True, st1+1, end2+1, False));
    set.add (TableExprNodeSetElem(False, st4, end4, False));
    set.add (TableExprNodeSetElem(False, st5, end5, True));
    // Vectors of test values and expected index.
    Vector<double> vec({ 0, 1, 2, 6, 19, 21, 23, 25, 26, 31, 33, 33.5, 34, 34.1});
    Vector<Int64>  exp({-1,-1, 0, 0,  0, -1, -1,  1,  1,  1, -1,  2,    2, -1});
    {
      // No combine, thus 6 intervals with different leftC/rightC.
      TENShPtr trSet = TableExprNodeSetOptContSetBase<Double>::transform (set, False);
      TableExprNodeSetOptContSetMixOC<Double>* p =
        dynamic_cast<TableExprNodeSetOptContSetMixOC<Double>*>(trSet.get());
      AlwaysAssertExit (p);
      AlwaysAssertExit (p->size() == 6);
      trSet->show (cout, 0);
      doTestOrig (set, *p, vec);
    }
    // Should result in (1,21) [25,33) (33,34]
    TENShPtr trSet = TableExprNodeSetOptContSetBase<Double>::transform (set);
    TableExprNodeSetOptContSetMixOC<Double>* p =
      dynamic_cast<TableExprNodeSetOptContSetMixOC<Double>*>(trSet.get());
    AlwaysAssertExit (p);
    AlwaysAssertExit (p->size() == 3);
    trSet->show (cout, 0);
    doTest (*p, vec, exp);
    doTestOrig (set, *p, vec);
  }
  // Test with equal leftC and rightC (all combinations).
  for (int i=0; i<4; ++i) {
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(i/2==0, st1, end1, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st3, end3, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st2, end2, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st4, end4, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st5, end5, i%2==0));
    TENShPtr trSet = TableExprNodeSetOptContSetBase<Double>::transform (set);
    TableExprNodeSetOptContSetBase<Double>* p =
      dynamic_cast<TableExprNodeSetOptContSetBase<Double>*>(trSet.get());
    // Results in 2 elements, but 4 when left and right side are open.
    trSet->show (cout, 0);
    AlwaysAssertExit (p->size() == (i==3 ? 4:2));
    // Vectors of test values and expected index (depending on open/closed).
    Int64 i3 = (i==3 ? 2 : 1);
    Int64 i4 = (i==3 ? 3 : 1);
    Int64 l1 = (i/2==0 ? 0 : -1);
    Int64 l2 = (i/2==0 ? 1 : -1);
    Int64 r1 = (i%2==0 ? 0 : -1);
    Int64 r2 = (i!=3   ? 1 : -1);
    Int64 r3 = (i%2==0 ? i3 : -1);
    Vector<double> vec({ 0, 1, 2, 6, 19, 20, 23, 25, 26, 30, 31, 33, 33.5, 34, 34.1});
    Vector<Int64>  exp({-1,l1, 0, 0,  0, r1, -1, l2,  1, r2, i3, r2, i4,   r3, -1});
    doTest (*p, vec, exp);
    doTestOrig (set, *p, vec);
  }
}

void doDateTransform()
{
  TableExprNode st(datetime("5Apr09/12:"));    // MJD 54926.5
  TableExprNode end(datetime("7May09/12:"));   // MJD 54958.5
  TableExprNode width(4);                      // 4 days
  {
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(False, st, end, False));
    TENShPtr trSet = TableExprNodeSetOptContSetBase<Double>::transform (set);
    TableExprNodeSetOptContSetBase<Double>* p =
      dynamic_cast<TableExprNodeSetOptContSetBase<Double>*>(trSet.get());
    AlwaysAssertExit (p);
    AlwaysAssertExit (p->size() == 1);
    trSet->show (cout, 0);
    Vector<double> vec({54926.5, 54926.51, 54934, 54958.49,54958.5});
    Vector<Int64>  exp({   -1,       0,        0,     0,      -1});
    doTest (*p, vec, exp);
    doTestOrig (set, *p, vec);
  }
  {
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(st, width));
    TENShPtr trSet = TableExprNodeSetOptContSetBase<Double>::transform (set);
    TableExprNodeSetOptContSetBase<Double>* p =
      dynamic_cast<TableExprNodeSetOptContSetBase<Double>*>(trSet.get());
    AlwaysAssertExit (p);
    AlwaysAssertExit (p->size() == 1);
    trSet->show (cout, 0);
    Vector<double> vec({54924.49, 54924.5, 54926, 54928.5, 54928.51});
    Vector<Int64>  exp({   -1,        0,       0,     0,      -1});
    doTest (*p, vec, exp);
    doTestOrig (set, *p, vec);
  }
}

int main()
{
  try {
    doDoubleContSet();
    doStringContSet();
    doIntSet();
    doStringSet();
    doDoubleTransform();
    doDateTransform();
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
