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
//#
//# $Id$

//# Includes
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ExprNode.h>
#include <tables/Tables/ExprNodeSet.h>
#include <tables/Tables/UDFBase.h>
#include <casa/Utilities/Assert.h>

using namespace casa;

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
  Bool getBool (const TableExprId& id) {return operands()[0]->getInt(id) == 1;}
};

void makeTable()
{
  TableDesc td;
  td.addColumn (ScalarColumnDesc<Int>("ANTENNA1"));
  SetupNewTable newtab("tExprNodeUDF_tmp.tab", td, Table::New);
  Table tab(newtab);
  ScalarColumn<Int> ant1(tab, "ANTENNA1");
  tab.addRow (10);
  for (uInt i=0; i<tab.nrow(); ++i) {
    ant1.put (i, i%3);
  }
}

int main()
{
  try {
    UDFBase::registerUDF ("TestUDF", TestUDF::makeObject);
    makeTable();
    Table tab("tExprNodeUDF_tmp.tab");
    TableExprNode node1(tab.col("ANTENNA1"));
    TableExprNodeSet set;
    set.add (TableExprNodeSetElem(node1));
    TableExprNode node2(TableExprNode::newUDFNode ("TestUDF", set, tab));
    Table seltab(tab(node2));
    cout << "selected " << seltab.nrow() << " rows" << endl; 
    AlwaysAssertExit (seltab.nrow() == 3);
    Table seltab2 = tab(tab.col("ANTENNA1")==1);
    Table seltab3 = seltab(seltab.col("ANTENNA1")==1);
    cout << "selected " << seltab2.nrow() <<' '<<seltab3.nrow()<< " rows" << endl;
    AlwaysAssertExit (seltab2.nrow() == 3);
    AlwaysAssertExit (seltab3.nrow() == 3);
  } catch (std::exception& x) {
    cout << "Unexpected exception " << x.what() << endl;
    return 1;
  }
  return 0;
}
