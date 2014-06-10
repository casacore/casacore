//# tExprGroupArray.cc: Test program for the grouping aggregate array functions
//# Copyright (C) 2013
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
//# $Id: tExprNode.cc 21156 2011-12-12 07:57:36Z gervandiepen $

#include <casa/Containers/Record.h>
#include <tables/Tables/ExprNode.h>
#include <tables/Tables/ExprNodeSet.h>
#include <tables/Tables/ExprAggrNodeArray.h>
#include <tables/Tables/ExprGroupAggrFunc.h>
#include <tables/Tables/RecordExpr.h>
#include <tables/Tables/TableExprIdAggr.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/MatrixIter.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayPartMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/BasicSL/Constants.h>
#include <casa/Utilities/Assert.h>
#include <casa/stdvector.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// <summary>
// Test program for class TableExprAggrNodeArray.
// </summary>

// Keeps track if errors occurred.
Bool foundError = False;


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

void checkLazy (const TableExprNode& expr,
                const vector<Record>& recs,
                const Array<Bool>& expVal, const String& str)
{
  cout << "Test Bool " << str << endl;
  // Get the aggregation node.
  TableExprAggrNodeArray& aggr = const_cast<TableExprAggrNodeArray&>
    (dynamic_cast<const TableExprAggrNodeArray&>(*expr.getNodeRep()));
  TableExprGroupExprId funcid(0);
  for (uInt i=0; i<recs.size(); ++i) {
    TableExprId id(recs[i]);
    funcid.apply (id);
  }
  funcid.finish();
  CountedPtr<TableExprGroupFuncBase> func = aggr.makeGroupAggrFunc();
  Array<Bool> val = func->getArrayBool(*funcid.getIds());
  if (!allEQ (val, expVal)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected "
         << expVal << endl;
  }
}

void checkLazy (const TableExprNode& expr,
                const vector<Record>& recs,
                const Array<Int64>& expVal, const String& str)
{
  cout << "Test Int " << str << endl;
  // Get the aggregation node.
  TableExprAggrNodeArray& aggr = const_cast<TableExprAggrNodeArray&>
    (dynamic_cast<const TableExprAggrNodeArray&>(*expr.getNodeRep()));
  TableExprGroupExprId funcid(0);
  for (uInt i=0; i<recs.size(); ++i) {
    TableExprId id(recs[i]);
    funcid.apply (id);
  }
  funcid.finish();
  vector<CountedPtr<TableExprGroupFuncSet> > funcSets;
  funcSets.push_back (new TableExprGroupFuncSet());
  CountedPtr<TableExprGroupFuncBase> func = aggr.makeGroupAggrFunc();
  funcSets[0]->add (func);
  Array<Int64> val = func->getArrayInt(*funcid.getIds());
  if (!allEQ (val, expVal)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected "
         << expVal << endl;
  }
  vector<CountedPtr<vector<TableExprId> > > ids(1, funcid.getIds());
  CountedPtr<TableExprGroupResult> groupResult =
    new TableExprGroupResult(funcSets, ids);
  TableExprIdAggr aid(groupResult);
  aid.setRownr (0);
  Array<Int64> val2 = aggr.getArrayInt (aid);
  if (!allEQ (val2, expVal)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected "
         << expVal << endl;
  }
}

void checkLazy (const TableExprNode& expr,
                const vector<Record>& recs,
                const Array<Double>& expVal, const String& str)
{
  cout << "Test Double " << str << endl;
  // Get the aggregation node.
  TableExprAggrNodeArray& aggr = const_cast<TableExprAggrNodeArray&>
    (dynamic_cast<const TableExprAggrNodeArray&>(*expr.getNodeRep()));
  TableExprGroupExprId funcid(0);
  for (uInt i=0; i<recs.size(); ++i) {
    TableExprId id(recs[i]);
    funcid.apply (id);
  }
  funcid.finish();
  CountedPtr<TableExprGroupFuncBase> func = aggr.makeGroupAggrFunc();
  Array<Double> val = func->getArrayDouble(*funcid.getIds());
  if (!allNear (val, expVal, 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected "
         << expVal << endl;
  }
}

void checkLazy (const TableExprNode& expr,
                const vector<Record>& recs,
                const Array<DComplex>& expVal, const String& str)
{
  cout << "Test DComplex " << str << endl;
  // Get the aggregation node.
  TableExprAggrNodeArray& aggr = const_cast<TableExprAggrNodeArray&>
    (dynamic_cast<const TableExprAggrNodeArray&>(*expr.getNodeRep()));
  TableExprGroupExprId funcid(0);
  for (uInt i=0; i<recs.size(); ++i) {
    TableExprId id(recs[i]);
    funcid.apply (id);
  }
  funcid.finish();
  CountedPtr<TableExprGroupFuncBase> func = aggr.makeGroupAggrFunc();
  Array<DComplex> val = func->getArrayDComplex(*funcid.getIds());
  if (!allNear (val, expVal, 1.e-10)) {
    foundError = True;
    cout << str << ": found value " << val << "; expected "
         << expVal << endl;
  }
}


void doBoolArr()
{
  // Define an Array with values.
  Cube<Bool> arr(2,3,4);
  arr = False; arr(0,1,3) = True;  arr(0,2,2) = True;
  // Define records containing equal parts of the array.
  vector<Record> recs(arr.shape()[2]);
  MatrixIterator<Bool> iter(arr);
  int i=0;
  while (!iter.pastEnd()) {
    recs[i++].define ("fld", iter.matrix());
    iter.next();
  }
  // Form the expression node from the record field.
  TableExprNode expr = makeRecordExpr (recs[0], "fld");
  checkLazy (TableExprNode::newFunctionNode(TableExprFuncNode::gaggrFUNC, expr),
             recs, arr, "gaggr");
}

void doIntArr()
{
  // Define an Array with values.
  Cube<Int64> arr(20,30,40);
  indgen (arr);
  // Define records containing equal parts of the array.
  vector<Record> recs(arr.shape()[2]);
  MatrixIterator<Int64> iter(arr);
  int i=0;
  while (!iter.pastEnd()) {
    recs[i++].define ("fld", iter.matrix());
    iter.next();
  }
  // Form the expression node from the record field.
  TableExprNode expr = makeRecordExpr (recs[0], "fld");
  checkLazy (TableExprNode::newFunctionNode(TableExprFuncNode::gaggrFUNC, expr),
             recs, arr, "gaggr");
}

void doDoubleArr()
{
  // Define an Array with values.
  Cube<Double> arr(5,3,1);
  indgen (arr, 10., 2.);
  // Define records containing equal parts of the array.
  vector<Record> recs(arr.shape()[2]);
  MatrixIterator<Double> iter(arr);
  int i=0;
  while (!iter.pastEnd()) {
    recs[i++].define ("fld", iter.matrix());
    iter.next();
  }
  // Form the expression node from the record field.
  TableExprNode expr = makeRecordExpr (recs[0], "fld");
  checkLazy (TableExprNode::newFunctionNode(TableExprFuncNode::gaggrFUNC, expr),
             recs, arr, "gaggr");
}

void doDComplexArr()
{
  // Define an Array with values.
  Cube<DComplex> arr(5,3,8);
  indgen (arr, DComplex(0.1, -0.3), DComplex(-1.3, 3.5));
  // Define records containing equal parts of the array.
  vector<Record> recs(arr.shape()[2]);
  MatrixIterator<DComplex> iter(arr);
  int i=0;
  while (!iter.pastEnd()) {
    recs[i++].define ("fld", iter.matrix());
    iter.next();
  }
  // Form the expression node from the record field.
  TableExprNode expr = makeRecordExpr (recs[0], "fld");
  checkLazy (TableExprNode::newFunctionNode(TableExprFuncNode::gaggrFUNC, expr),
             recs, arr, "gaggr");
}


int main()
{
  try {
    doBoolArr();
    doIntArr();
    doDoubleArr();
    doDComplexArr();
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
