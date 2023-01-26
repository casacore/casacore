//# tExprUDFNode.cc: Test program for class TableExprUDFNode
//# Copyright (C) 2010
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/tables/TaQL/UDFBase.h>
#include <casacore/tables/TaQL/TableExprIdAggr.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/casa/Utilities/Assert.h>

using namespace casacore;

class TestUDF: public UDFBase
{
public:
  TestUDF() {}
  static UDFBase* makeObject (const String&) { return new TestUDF(); }
  virtual void setup (const Table&, const TaQLStyle&)
  {
    AlwaysAssert (operands().size() == 1, AipsError);
    AlwaysAssert (operands()[0]->dataType() == TableExprNodeRep::NTInt, AipsError);
    AlwaysAssert (operands()[0]->valueType() == TableExprNodeRep::VTScalar, AipsError);
    setDataType (TableExprNodeRep::NTBool);
    setNDim (0);   //scalar
  }
  bool getBool (const TableExprId& id) {return operands()[0]->getInt(id) == 1;}
};

class TestUDFAggr: public UDFBase
{
public:
  TestUDFAggr() {}
  static UDFBase* makeObject (const String&) { return new TestUDFAggr(); }
  virtual void setup (const Table&, const TaQLStyle&)
  {
    AlwaysAssert (operands().size() == 1, AipsError);
    AlwaysAssert (operands()[0]->dataType() == TableExprNodeRep::NTInt, AipsError);
    AlwaysAssert (operands()[0]->valueType() == TableExprNodeRep::VTScalar, AipsError);
    setDataType (TableExprNodeRep::NTInt);
    setNDim (0);           // scalar
    setAggregate (true);   // aggregate function
  }
  int64_t getInt (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    const vector<TableExprId>& ids = aid.result().ids(id.rownr());
    int64_t sum3 = 0;
    for (vector<TableExprId>::const_iterator it=ids.begin();
         it!=ids.end(); ++it){
      int64_t v = operands()[0]->getInt(*it);
        sum3 += v*v*v;
    }
    return sum3;
  }
};

void makeTable()
{
  TableDesc td;
  td.addColumn (ScalarColumnDesc<int32_t>("ANTENNA1"));
  SetupNewTable newtab("tExprNodeUDF_tmp.tab", td, Table::New);
  Table tab(newtab);
  ScalarColumn<int32_t> ant1(tab, "ANTENNA1");
  tab.addRow (10);
  for (uint32_t i=0; i<tab.nrow(); ++i) {
    ant1.put (i, i%3);
  }
}

int main()
{
  try {
    UDFBase::registerUDF ("Test.UDF", TestUDF::makeObject);
    UDFBase::registerUDF ("Test.UDFAggr", TestUDFAggr::makeObject);
    makeTable();
    Table tab("tExprNodeUDF_tmp.tab");
    TableExprInfo tabInfo(tab);
    {
      // Test a normal user defined function.
      TableExprNode node1(tab.col("ANTENNA1"));
      TableExprNodeSet set;
      set.add (TableExprNodeSetElem(node1));
      TableExprNode node2(TableExprNode::newUDFNode ("Test.UDF", set, tabInfo));
      Table seltab(tab(node2));
      cout << "selected " << seltab.nrow() << " rows" << endl; 
      AlwaysAssertExit (seltab.nrow() == 3);
      Table seltab2 = tab(tab.col("ANTENNA1")==1);
      Table seltab3 = seltab(seltab.col("ANTENNA1")==1);
      cout << "selected " << seltab2.nrow() <<' '<<seltab3.nrow()<< " rows" << endl;
      AlwaysAssertExit (seltab2.nrow() == 3);
      AlwaysAssertExit (seltab3.nrow() == 3);
    }
    {
      // Test an aggregate user defined function.
      TableExprNode node1(tab.col("ANTENNA1"));
      TableExprNodeSet set;
      set.add (TableExprNodeSetElem(node1));
      TableExprNode node2(TableExprNode::newUDFNode ("Test.UDFAggr", set, tabInfo));
      TableExprNodeRep* rep = const_cast<TableExprNodeRep*>(node2.getRep().get());
      vector<TableExprNodeRep*> aggrNodes = TableExprNodeUtil::getAggrNodes (rep);
      AlwaysAssertExit (aggrNodes.size() == 1);
      AlwaysAssertExit (aggrNodes[0]->isLazyAggregate());
      CountedPtr<vector<TableExprId> > ids(new vector<TableExprId>());
      for (uint32_t i=0; i<tab.nrow(); ++i) {
        ids->push_back (TableExprId(i));
      }
      vector<CountedPtr<vector<TableExprId> > > idVec(1, ids);
      vector<CountedPtr<TableExprGroupFuncSet> > funcVec(1);
      CountedPtr<TableExprGroupResult> res
        (new TableExprGroupResult(funcVec, idVec));
      TableExprIdAggr aid(res);
      aid.setRownr (0);
      int64_t val = node2.getInt(aid);
      cout << "aggregated value=" << val << endl;
      Vector<int32_t> colval (ScalarColumn<int32_t>(tab, "ANTENNA1").getColumn());
      AlwaysAssertExit (val == sum(colval*colval*colval));
    }
  } catch (std::exception& x) {
    cout << "Unexpected exception " << x.what() << endl;
    return 1;
  }
  return 0;
}
