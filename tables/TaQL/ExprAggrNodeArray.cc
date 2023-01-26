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

//# Includes
#include <casacore/tables/TaQL/ExprAggrNodeArray.h>
#include <casacore/tables/TaQL/ExprGroupAggrFunc.h>
#include <casacore/tables/TaQL/ExprGroupAggrFuncArray.h>
#include <casacore/tables/TaQL/TableExprIdAggr.h>
#include <casacore/tables/TaQL/ExprNodeUtil.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  TableExprAggrNodeArray::TableExprAggrNodeArray
  (TableExprFuncNode::FunctionType ftype,
   NodeDataType dtype, ValueType vtype,
   const TableExprNodeSet& source,
   const vector<TENShPtr>& nodes,
   const Block<int32_t>& dtypeOper,
   const TaQLStyle& style)
   : TableExprFuncNodeArray (ftype, dtype, vtype, source,
                             nodes, dtypeOper, style)
  {
    // Always treat an aggregate as a variable expression.
    // Otherwise if might be treated as constant and evaluated immediately
    // which cannot be done.
    exprtype_p = Variable;
  }

  CountedPtr<TableExprGroupFuncBase> TableExprAggrNodeArray::makeGroupAggrFunc()
  {
    // Create a new function object because each FuncSet needs its own one.
    itsFunc = doMakeGroupAggrFunc();
    return itsFunc;
  }

  bool TableExprAggrNodeArray::isAggregate() const
  {
    return true;
  }
  
  bool TableExprAggrNodeArray::isLazyAggregate() const
  {
    return itsFunc->isLazy();
  }

  CountedPtr<TableExprGroupFuncBase> TableExprAggrNodeArray::doMakeGroupAggrFunc()
  {
    if (funcType() == TableExprFuncNode::gexpridFUNC) {
      return new TableExprGroupExprId(this);
    } else if (funcType() == TableExprFuncNode::gaggrFUNC) {
      return new TableExprGroupAggr(this);
    } else if (funcType() == TableExprFuncNode::growidFUNC) {
      return new TableExprGroupRowid(this);
    } else if (funcType() == TableExprFuncNode::ghistFUNC) {
      int64_t  nbin  = operands()[1]->getInt(0);
      double start = operands()[2]->getDouble(0);
      double end   = operands()[3]->getDouble(0);
      if (operands()[0]->valueType() == VTScalar) {
        return new TableExprGroupHistScalar (this, nbin, start, end);
      }
      if (operands()[0]->dataType() == NTInt) {
        return new TableExprGroupHistInt (this, nbin, start, end);
      }
      return new TableExprGroupHistDouble (this, nbin, start, end);
    }
    if (operands()[0]->valueType() == VTScalar) {
      throw TableInvExpr ("Aggregate function " +
                          String::toString(funcType()) +
                          " is unknown for scalar data type " +
                          String::toString(operands()[0]->dataType()));
    }
    // The operand is an array.
    switch (operands()[0]->dataType()) {
    case NTBool:
      switch (funcType()) {
      case TableExprFuncNode::ganysFUNC:
        return new TableExprGroupArrayAnys(this);
      case TableExprFuncNode::gallsFUNC:
        return new TableExprGroupArrayAlls(this);
      case TableExprFuncNode::gntruesFUNC:
        return new TableExprGroupArrayNTrues(this);
      case TableExprFuncNode::gnfalsesFUNC:
        return new TableExprGroupArrayNFalses(this);
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with a bool argument");
      }
    case NTInt:
      switch (funcType()) {
      case TableExprFuncNode::gminsFUNC:
        return new TableExprGroupMinsArrayInt(this);
      case TableExprFuncNode::gmaxsFUNC:
        return new TableExprGroupMaxsArrayInt(this);
      case TableExprFuncNode::gsumsFUNC:
        return new TableExprGroupSumsArrayInt(this);
      case TableExprFuncNode::gproductsFUNC:
        return new TableExprGroupProductsArrayInt(this);
      case TableExprFuncNode::gsumsqrsFUNC:
        return new TableExprGroupSumSqrsArrayInt(this);
      default:
        break;
      }
      // Fall through, so e.g. mean of ints can be done
      CASACORE_FALLTHROUGH;
    case NTDouble:
      switch (funcType()) {
      case TableExprFuncNode::gminsFUNC:
        return new TableExprGroupMinsArrayDouble(this);
      case TableExprFuncNode::gmaxsFUNC:
        return new TableExprGroupMaxsArrayDouble(this);
      case TableExprFuncNode::gsumsFUNC:
        return new TableExprGroupSumsArrayDouble(this);
      case TableExprFuncNode::gproductsFUNC:
        return new TableExprGroupProductsArrayDouble(this);
      case TableExprFuncNode::gsumsqrsFUNC:
        return new TableExprGroupSumSqrsArrayDouble(this);
      case TableExprFuncNode::gmeansFUNC:
        return new TableExprGroupMeansArrayDouble(this);
      case TableExprFuncNode::gvariances0FUNC:
        return new TableExprGroupVariancesArrayDouble(this, 0);
      case TableExprFuncNode::gvariances1FUNC:
        return new TableExprGroupVariancesArrayDouble(this, 1);
      case TableExprFuncNode::gstddevs0FUNC:
        return new TableExprGroupStdDevsArrayDouble(this, 0);
      case TableExprFuncNode::gstddevs1FUNC:
        return new TableExprGroupStdDevsArrayDouble(this, 1);
      case TableExprFuncNode::grmssFUNC:
        return new TableExprGroupRmssArrayDouble(this);
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with an integer/double argument");
      }
    case NTComplex:
      switch (funcType()) {
      case TableExprFuncNode::gsumsFUNC:
        return new TableExprGroupSumsArrayDComplex(this);
      case TableExprFuncNode::gproductsFUNC:
        return new TableExprGroupProductsArrayDComplex(this);
      case TableExprFuncNode::gsumsqrsFUNC:
        return new TableExprGroupSumSqrsArrayDComplex(this);
      case TableExprFuncNode::gmeansFUNC:
        return new TableExprGroupMeansArrayDComplex(this);
      case TableExprFuncNode::gvariances0FUNC:
        return new TableExprGroupVariancesArrayDComplex(this, 0);
      case TableExprFuncNode::gvariances1FUNC:
        return new TableExprGroupVariancesArrayDComplex(this, 1);
      case TableExprFuncNode::gstddevs0FUNC:
        return new TableExprGroupStdDevsArrayDComplex(this, 0);
      case TableExprFuncNode::gstddevs1FUNC:
        return new TableExprGroupStdDevsArrayDComplex(this, 1);
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with a dcomplex argument");
      }
    default:
      break;
    }
    throw TableInvExpr ("Array aggregate function " +
                        String::toString(funcType()) +
                        " is unknown");
  }

  MArray<bool> TableExprAggrNodeArray::getArrayBool (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayBool (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayBool();
  }
  MArray<int64_t> TableExprAggrNodeArray::getArrayInt (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayInt (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayInt();
  }
  MArray<double> TableExprAggrNodeArray::getArrayDouble (const TableExprId& id)
  {
    if (dataType() != NTDouble) {
      return TableExprNodeArray::getArrayDouble (id);
    }
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayDouble (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayDouble();
  }
  MArray<DComplex> TableExprAggrNodeArray::getArrayDComplex (const TableExprId& id)
  {
    if (dataType() != NTComplex) {
      return TableExprNodeArray::getArrayDComplex (id);
    }
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayDComplex (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayDComplex();
  }
  MArray<String> TableExprAggrNodeArray::getArrayString (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayString (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayString();
  }
  MArray<MVTime> TableExprAggrNodeArray::getArrayDate (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayDate (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayDate();
  }

} //# NAMESPACE CASACORE - END
