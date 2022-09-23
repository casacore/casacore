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
//#        Internet email: aips2-request@nrao.edu.
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



void doDoubleContSet()
{
  std::vector<double> st ({1,3,5,7,9,11,13,15,17,19,21});
  std::vector<double> end({2,4,6,8,10,12,14,16,18,20,22});
  TableExprId id(0);
  {
    // Test closed-closed intervals
    TableExprNodeSetOptContSetCC<Double> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, 0.5));
    AlwaysAssertExit (set.contains (id, 1));
    AlwaysAssertExit (set.contains (id, 1.5));
    AlwaysAssertExit (set.contains (id, 2));
    AlwaysAssertExit (! set.contains (id, 2.5));
    AlwaysAssertExit (set.contains (id, 13));
    AlwaysAssertExit (set.contains (id, 13.5));
    AlwaysAssertExit (set.contains (id, 14));
    AlwaysAssertExit (! set.contains (id, 14.5));
    AlwaysAssertExit (set.contains (id, 21));
    AlwaysAssertExit (set.contains (id, 21.5));
    AlwaysAssertExit (set.contains (id, 22));
    AlwaysAssertExit (! set.contains (id, 22.5));
  }
  {
    // Test open-closed intervals
    TableExprNodeSetOptContSetOC<Double> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, 0.5));
    AlwaysAssertExit (! set.contains (id, 1));
    AlwaysAssertExit (set.contains (id, 1.5));
    AlwaysAssertExit (set.contains (id, 2));
    AlwaysAssertExit (! set.contains (id, 2.5));
    AlwaysAssertExit (! set.contains (id, 13));
    AlwaysAssertExit (set.contains (id, 13.5));
    AlwaysAssertExit (set.contains (id, 14));
    AlwaysAssertExit (! set.contains (id, 14.5));
    AlwaysAssertExit (! set.contains (id, 21));
    AlwaysAssertExit (set.contains (id, 21.5));
    AlwaysAssertExit (set.contains (id, 22));
    AlwaysAssertExit (! set.contains (id, 22.5));
  }
  {
    // Test closed-open intervals
    TableExprNodeSetOptContSetCO<Double> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, 0.5));
    AlwaysAssertExit (set.contains (id, 1));
    AlwaysAssertExit (set.contains (id, 1.5));
    AlwaysAssertExit (! set.contains (id, 2));
    AlwaysAssertExit (! set.contains (id, 2.5));
    AlwaysAssertExit (set.contains (id, 13));
    AlwaysAssertExit (set.contains (id, 13.5));
    AlwaysAssertExit (! set.contains (id, 14));
    AlwaysAssertExit (! set.contains (id, 14.5));
    AlwaysAssertExit (set.contains (id, 21));
    AlwaysAssertExit (set.contains (id, 21.5));
    AlwaysAssertExit (! set.contains (id, 22));
    AlwaysAssertExit (! set.contains (id, 22.5));
  }
  {
    // Test open-open intervals
    TableExprNodeSetOptContSetOO<Double> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, 0.5));
    AlwaysAssertExit (! set.contains (id, 1));
    AlwaysAssertExit (set.contains (id, 1.5));
    AlwaysAssertExit (! set.contains (id, 2));
    AlwaysAssertExit (! set.contains (id, 2.5));
    AlwaysAssertExit (! set.contains (id, 13));
    AlwaysAssertExit (set.contains (id, 13.5));
    AlwaysAssertExit (! set.contains (id, 14));
    AlwaysAssertExit (! set.contains (id, 14.5));
    AlwaysAssertExit (! set.contains (id, 21));
    AlwaysAssertExit (set.contains (id, 21.5));
    AlwaysAssertExit (! set.contains (id, 22));
    AlwaysAssertExit (! set.contains (id, 22.5));
  }
  {
    // Test intervals with mix of open and closed
    std::vector<bool> leftC ({false,true,true,true,true,true,false,true,true,true,true});
    std::vector<bool> rightC({false,true,true,true,true,true,true ,true,true,true,false});
    TableExprNodeSetOptContSet<Double> set (TableExprNodeSet(), st, end, leftC, rightC);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, 0.5));
    AlwaysAssertExit (! set.contains (id, 1));
    AlwaysAssertExit (set.contains (id, 1.5));
    AlwaysAssertExit (! set.contains (id, 2));
    AlwaysAssertExit (! set.contains (id, 2.5));
    AlwaysAssertExit (! set.contains (id, 4.5));
    AlwaysAssertExit (set.contains (id, 5));
    AlwaysAssertExit (set.contains (id, 5.5));
    AlwaysAssertExit (set.contains (id, 6));
    AlwaysAssertExit (! set.contains (id, 6.5));
    AlwaysAssertExit (! set.contains (id, 13));
    AlwaysAssertExit (set.contains (id, 13.5));
    AlwaysAssertExit (set.contains (id, 14));
    AlwaysAssertExit (! set.contains (id, 14.5));
    AlwaysAssertExit (set.contains (id, 21));
    AlwaysAssertExit (set.contains (id, 21.5));
    AlwaysAssertExit (! set.contains (id, 22));
    AlwaysAssertExit (! set.contains (id, 22.5));
    Vector<double> vec({0.5,1,1.5,2,2.5,4.5,5,5.5,6,6.5,13,13.5,14,14.5,21,21.5,22,22.5});
    Vector<bool> exp({false,false,true,false,false,false,true,true,true,
                      false,false,true,true,false,true,true,false,false});
    MArray<bool> res = set.contains (id, MArray<double>(vec));
    AlwaysAssertExit (allEQ(res.array(), exp));
  }
}

void doStringContSet()
{
  std::vector<String> st ({"a1"});
  std::vector<String> end({"a5"});
  TableExprId id(0);
  {
    // Test closed-closed intervals
    TableExprNodeSetOptContSetCC<String> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, "a0"));
    AlwaysAssertExit (set.contains (id, "a1"));
    AlwaysAssertExit (set.contains (id, "a3"));
    AlwaysAssertExit (set.contains (id, "a5"));
    AlwaysAssertExit (! set.contains (id, "a6"));
  }
  {
    // Test closed-open intervals
    TableExprNodeSetOptContSetCO<String> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, "a0"));
    AlwaysAssertExit (set.contains (id, "a1"));
    AlwaysAssertExit (set.contains (id, "a3"));
    AlwaysAssertExit (! set.contains (id, "a5"));
    AlwaysAssertExit (! set.contains (id, "a6"));
  }
  {
    // Test open-closed intervals
    TableExprNodeSetOptContSetOC<String> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, "a0"));
    AlwaysAssertExit (! set.contains (id, "a1"));
    AlwaysAssertExit (set.contains (id, "a3"));
    AlwaysAssertExit (set.contains (id, "a5"));
    AlwaysAssertExit (! set.contains (id, "a6"));
  }
  {
    // Test open-open intervals
    TableExprNodeSetOptContSetOO<String> set (TableExprNodeSet(), st, end);
    set.show(cout, 2);
    AlwaysAssertExit (! set.contains (id, "a0"));
    AlwaysAssertExit (! set.contains (id, "a1"));
    AlwaysAssertExit (set.contains (id, "a3"));
    AlwaysAssertExit (! set.contains (id, "a5"));
    AlwaysAssertExit (! set.contains (id, "a6"));
  }
}

void doIntSet()
{
  TableExprId id(0);
  Vector<Int64> vec({1,3,5,8,10});
  TableExprNodeSetOptIntUSet set(TableExprNodeSet(), vec);
  set.show(cout, 2);
  AlwaysAssertExit (set.contains (id, 1));
  AlwaysAssertExit (set.contains (id, 3));
  AlwaysAssertExit (set.contains (id, 5));
  AlwaysAssertExit (set.contains (id, 8));
  AlwaysAssertExit (! set.contains (id, 0));
  AlwaysAssertExit (! set.contains (id, 2));
  AlwaysAssertExit (! set.contains (id, 11));
}

void doStringSet()
{
  TableExprId id(0);
  Vector<String> vec({"a", "b", "d"});
  TableExprNodeSetOptStringUSet set(TableExprNodeSet(), vec);
  set.show(cout, 2);
  AlwaysAssertExit (set.contains (id, "a"));
  AlwaysAssertExit (set.contains (id, "b"));
  AlwaysAssertExit (set.contains (id, "d"));
  AlwaysAssertExit (! set.contains (id, "c"));
  AlwaysAssertExit (! set.contains (id, "aa"));
}

void doDoubleTransform()
{
  cout << " String std::lowest=" << std::numeric_limits<String>::lowest() << endl;
  cout << " String std::max=" << std::numeric_limits<String>::max() << endl;
  TableExprNode st1(1);
  TableExprNode end1(8);
  TableExprNode st2(6);
  TableExprNode end2(20);
  TableExprNode st3(25);
  TableExprNode end3(30);
  TableExprNode st4(30);
  TableExprNode end4(33);
  TableExprNode st5(33);
  TableExprNode end5(34);
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
    TENShPtr trSet = TableExprNodeSetOptContSet<Double>::transform (set);
    trSet->show (cout, 0);
    TableExprId id(0);
    AlwaysAssertExit (set.contains(id,0.) == trSet->contains(id,0.));
    AlwaysAssertExit (set.contains(id,1.) == trSet->contains(id,1.));
    AlwaysAssertExit (set.contains(id,2.) == trSet->contains(id,2.));
    AlwaysAssertExit (set.contains(id,6.) == trSet->contains(id,6.));
    AlwaysAssertExit (set.contains(id,19.) == trSet->contains(id,19.));
    AlwaysAssertExit (set.contains(id,25.) == trSet->contains(id,25.));
    AlwaysAssertExit (set.contains(id,26.) == trSet->contains(id,26.));
    AlwaysAssertExit (set.contains(id,30.) == trSet->contains(id,30.));
    AlwaysAssertExit (set.contains(id,31.) == trSet->contains(id,31.));
    AlwaysAssertExit (set.contains(id,33.) == trSet->contains(id,33.));
    AlwaysAssertExit (set.contains(id,33.) == False);
  }
  // Test with equal leftC and rightC (all combinations).
  for (int i=0; i<4; ++i) {
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(i/2==0, st1, end1, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st3, end3, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st2, end2, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st1+1, end2+1, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st4, end4, i%2==0));
    set.add (TableExprNodeSetElem(i/2==0, st5, end5, i%2==0));
    TENShPtr trSet = TableExprNodeSetOptContSet<Double>::transform (set);
    trSet->show (cout, 0);
    TableExprId id(0);
    AlwaysAssertExit (set.contains(id,0.) == trSet->contains(id,0.));
    AlwaysAssertExit (set.contains(id,1.) == trSet->contains(id,1.));
    AlwaysAssertExit (set.contains(id,2.) == trSet->contains(id,2.));
    AlwaysAssertExit (set.contains(id,6.) == trSet->contains(id,6.));
    AlwaysAssertExit (set.contains(id,19.) == trSet->contains(id,19.));
    AlwaysAssertExit (set.contains(id,25.) == trSet->contains(id,25.));
    AlwaysAssertExit (set.contains(id,26.) == trSet->contains(id,26.));
    AlwaysAssertExit (set.contains(id,30.) == trSet->contains(id,30.));
    AlwaysAssertExit (set.contains(id,31.) == trSet->contains(id,31.));
    AlwaysAssertExit (set.contains(id,33.) == trSet->contains(id,33.));
    AlwaysAssertExit (set.contains(id,33.) == (i!=3));
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
    TENShPtr trSet = TableExprNodeSetOptContSet<Double>::transform (set);
    trSet->show (cout, 0);
    TableExprId id(0);
    AlwaysAssertExit (set.contains(id,54926.5) == trSet->contains(id,54926.5));
    AlwaysAssertExit (set.contains(id,54926.51) == trSet->contains(id,54926.51));
    AlwaysAssertExit (set.contains(id,54934.) == trSet->contains(id,54934.));
    AlwaysAssertExit (set.contains(id,54958.49) == trSet->contains(id,54958.49));
    AlwaysAssertExit (set.contains(id,54958.5) == trSet->contains(id,54958.5));
  }
  {
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(st, width));
    TENShPtr trSet = TableExprNodeSetOptContSet<Double>::transform (set);
    trSet->show (cout, 0);
    TableExprId id(0);
    AlwaysAssertExit (set.contains(id,54924.5) == trSet->contains(id,54924.5));
    AlwaysAssertExit (set.contains(id,54924.51) == trSet->contains(id,54924.51));
    AlwaysAssertExit (set.contains(id,54926.) == trSet->contains(id,54926.));
    AlwaysAssertExit (set.contains(id,54928.49) == trSet->contains(id,54928.49));
    AlwaysAssertExit (set.contains(id,54928.5) == trSet->contains(id,54928.5));
  }
}

void doDoubleMidWidth()
{
}

void doDateMidWidth()
{
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
    doDoubleMidWidth();
    doDateMidWidth();
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
