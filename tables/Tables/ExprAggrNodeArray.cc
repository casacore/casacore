//# ExprAggrNodeArray.cc: TaQL node representing an aggregate function
//# Copyright (C) 2013
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
//# $Id: TaQLNodeArray.h 21051 2011-04-20 11:46:29Z gervandiepen $

//# Includes
#include <tables/Tables/ExprAggrNodeArray.h>
#include <tables/Tables/ExprGroupAggrFunc.h>
#include <tables/Tables/ExprGroupAggrFuncArray.h>
#include <tables/Tables/TableExprIdAggr.h>
#include <tables/Tables/TableError.h>


namespace casa { //# NAMESPACE CASA - BEGIN


  TableExprAggrNodeArray::TableExprAggrNodeArray
  (TableExprFuncNode::FunctionType ftype,
   NodeDataType dtype, ValueType vtype,
   const TableExprNodeSet& source, const TaQLStyle& style)
    : TableExprFuncNodeArray (ftype, dtype, vtype, source, style)
  {
    // Always treat an aggregate as a variable expression.
    // Otherwise if might be treated as constant and evaluated immediately
    // which cannot be done.
    exprtype_p = Variable;
  }

  void TableExprAggrNodeArray::getAggrNodes (vector<TableExprNodeRep*>& aggr)
  {
    aggr.push_back (this);
    uInt naggr = aggr.size();
    for (uInt i=0; i<operands().size(); ++i) {
      operands()[i]->getAggrNodes (aggr);
    }
    if (naggr != aggr.size()) {
      throw TableInvExpr ("The argument of an aggregate function cannot use "
                          "an aggregate function");
    }
  }

  // Strictly speaking this function does not need to be implemented,
  // because the default in TableExprNodeRep already returns True.
  // Yet, for clarity it is done here as well.
  Bool TableExprAggrNodeArray::isLazyAggregate() const
  {
    return True;
  }

  CountedPtr<TableExprGroupFuncBase> TableExprAggrNodeArray::makeGroupAggrFunc()
  {
    if (funcType() == TableExprFuncNode::gexpridFUNC) {
      itsFunc = new TableExprGroupExprId(this);
    } else if (funcType() == TableExprFuncNode::gaggrFUNC) {
      itsFunc = new TableExprGroupAggr(this);
    } else if (funcType() == TableExprFuncNode::growidFUNC) {
      itsFunc = new TableExprGroupRowid(this);
    } else {
      throw TableInvExpr ("Array aggregate function " +
                          String::toString(funcType()) +
                          " is unknown");
    }
    return itsFunc;
  }

  Array<Bool> TableExprAggrNodeArray::getArrayBool (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    return itsFunc->getArrayBool (aid.result().ids(id.rownr()));
  }
  Array<Int64> TableExprAggrNodeArray::getArrayInt (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    return itsFunc->getArrayInt (aid.result().ids(id.rownr()));
  }
  Array<Double> TableExprAggrNodeArray::getArrayDouble (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    return itsFunc->getArrayDouble (aid.result().ids(id.rownr()));
  }
  Array<DComplex> TableExprAggrNodeArray::getArrayDComplex (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    return itsFunc->getArrayDComplex (aid.result().ids(id.rownr()));
  }
  Array<String> TableExprAggrNodeArray::getArrayString (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    return itsFunc->getArrayString (aid.result().ids(id.rownr()));
  }
  Array<MVTime> TableExprAggrNodeArray::getArrayDate (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    return itsFunc->getArrayDate (aid.result().ids(id.rownr()));
  }

} //# NAMESPACE CASA - END
