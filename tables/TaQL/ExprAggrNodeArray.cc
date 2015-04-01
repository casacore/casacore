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
#include <casacore/tables/TaQL/ExprAggrNodeArray.h>
#include <casacore/tables/TaQL/ExprGroupAggrFunc.h>
#include <casacore/tables/TaQL/ExprGroupAggrFuncArray.h>
#include <casacore/tables/TaQL/TableExprIdAggr.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


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

  Bool TableExprAggrNodeArray::isLazyAggregate() const
  {
    return (funcType() != TableExprFuncNode::ghistFUNC);
  }

  CountedPtr<TableExprGroupFuncBase> TableExprAggrNodeArray::makeGroupAggrFunc()
  {
    switch (funcType()) {
    case TableExprFuncNode::gexpridFUNC:
      itsFunc = new TableExprGroupExprId(this);
      break;
    case TableExprFuncNode::gaggrFUNC:
      itsFunc = new TableExprGroupAggr(this);
      break;
    case TableExprFuncNode::growidFUNC:
      itsFunc = new TableExprGroupRowid(this);
      break;
    case TableExprFuncNode::ghistFUNC:
      {
        Int64  nbin  = operands()[1]->getInt(0);
        Double start = operands()[2]->getDouble(0);
        Double end   = operands()[3]->getDouble(0);
        if (operands()[0]->valueType() == VTScalar) {
          itsFunc = new TableExprGroupHistScalar (this, nbin, start, end);
        } else {
          if (operands()[0]->dataType() == NTInt) {
            itsFunc = new TableExprGroupHistInt (this, nbin, start, end);
          } else {
            itsFunc = new TableExprGroupHistDouble (this, nbin, start, end);
          }
        }
      }
      break;
    default:
      throw TableInvExpr ("Array aggregate function " +
                          String::toString(funcType()) +
                          " is unknown");
    }
    return itsFunc;
  }

  Array<Bool> TableExprAggrNodeArray::getArrayBool (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayBool (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayBool();
  }
  Array<Int64> TableExprAggrNodeArray::getArrayInt (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayInt (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayInt();
  }
  Array<Double> TableExprAggrNodeArray::getArrayDouble (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayDouble (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayDouble();
  }
  Array<DComplex> TableExprAggrNodeArray::getArrayDComplex (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayDComplex (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayDComplex();
  }
  Array<String> TableExprAggrNodeArray::getArrayString (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayString (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayString();
  }
  Array<MVTime> TableExprAggrNodeArray::getArrayDate (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayDate (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayDate();
  }

} //# NAMESPACE CASACORE - END
