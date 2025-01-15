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
//#        Internet email: casa-feedback@nrao.edu.
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
   const Block<Int>& dtypeOper,
   const TaQLStyle& style)
   : TableExprFuncNodeArray (ftype, dtype, vtype, source,
                             nodes, dtypeOper, style)
  {
    // Always treat an aggregate as a variable expression.
    // Otherwise if might be treated as constant and evaluated immediately
    // which cannot be done.
    exprtype_p = Variable;
  }

  std::shared_ptr<TableExprGroupFuncBase> TableExprAggrNodeArray::makeGroupAggrFunc()
  {
    // Create a new function object because each FuncSet needs its own one.
    itsFunc = doMakeGroupAggrFunc();
    return itsFunc;
  }

  Bool TableExprAggrNodeArray::isAggregate() const
  {
    return True;
  }
  
  Bool TableExprAggrNodeArray::isLazyAggregate() const
  {
    return itsFunc->isLazy();
  }

  std::shared_ptr<TableExprGroupFuncBase> TableExprAggrNodeArray::doMakeGroupAggrFunc()
  {
    if (funcType() == TableExprFuncNode::gexpridFUNC) {
      return std::make_shared<TableExprGroupExprId>(this);
    } else if (funcType() == TableExprFuncNode::gaggrFUNC) {
      return std::make_shared<TableExprGroupAggr>(this);
    } else if (funcType() == TableExprFuncNode::growidFUNC) {
      return std::make_shared<TableExprGroupRowid>(this);
    } else if (funcType() == TableExprFuncNode::ghistFUNC) {
      Int64  nbin  = operands()[1]->getInt(0);
      Double start = operands()[2]->getDouble(0);
      Double end   = operands()[3]->getDouble(0);
      if (operands()[0]->valueType() == VTScalar) {
        return std::make_shared<TableExprGroupHistScalar>(this, nbin, start, end);
      }
      if (operands()[0]->dataType() == NTInt) {
        return std::make_shared<TableExprGroupHistInt>(this, nbin, start, end);
      }
      return std::make_shared<TableExprGroupHistDouble>(this, nbin, start, end);
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
        return std::make_shared<TableExprGroupArrayAnys>(this);
      case TableExprFuncNode::gallsFUNC:
        return std::make_shared<TableExprGroupArrayAlls>(this);
      case TableExprFuncNode::gntruesFUNC:
        return std::make_shared<TableExprGroupArrayNTrues>(this);
      case TableExprFuncNode::gnfalsesFUNC:
        return std::make_shared<TableExprGroupArrayNFalses>(this);
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with a bool argument");
      }
    case NTInt:
      switch (funcType()) {
      case TableExprFuncNode::gminsFUNC:
        return std::make_shared<TableExprGroupMinsArrayInt>(this);
      case TableExprFuncNode::gmaxsFUNC:
        return std::make_shared<TableExprGroupMaxsArrayInt>(this);
      case TableExprFuncNode::gsumsFUNC:
        return std::make_shared<TableExprGroupSumsArrayInt>(this);
      case TableExprFuncNode::gproductsFUNC:
        return std::make_shared<TableExprGroupProductsArrayInt>(this);
      case TableExprFuncNode::gsumsqrsFUNC:
        return std::make_shared<TableExprGroupSumSqrsArrayInt>(this);
      default:
        break;
      }
      // Fall through, so e.g. mean of ints can be done
      CASACORE_FALLTHROUGH;
    case NTDouble:
      switch (funcType()) {
      case TableExprFuncNode::gminsFUNC:
        return std::make_shared<TableExprGroupMinsArrayDouble>(this);
      case TableExprFuncNode::gmaxsFUNC:
        return std::make_shared<TableExprGroupMaxsArrayDouble>(this);
      case TableExprFuncNode::gsumsFUNC:
        return std::make_shared<TableExprGroupSumsArrayDouble>(this);
      case TableExprFuncNode::gproductsFUNC:
        return std::make_shared<TableExprGroupProductsArrayDouble>(this);
      case TableExprFuncNode::gsumsqrsFUNC:
        return std::make_shared<TableExprGroupSumSqrsArrayDouble>(this);
      case TableExprFuncNode::gmeansFUNC:
        return std::make_shared<TableExprGroupMeansArrayDouble>(this);
      case TableExprFuncNode::gvariances0FUNC:
        return std::make_shared<TableExprGroupVariancesArrayDouble>(this, 0);
      case TableExprFuncNode::gvariances1FUNC:
        return std::make_shared<TableExprGroupVariancesArrayDouble>(this, 1);
      case TableExprFuncNode::gstddevs0FUNC:
        return std::make_shared<TableExprGroupStdDevsArrayDouble>(this, 0);
      case TableExprFuncNode::gstddevs1FUNC:
        return std::make_shared<TableExprGroupStdDevsArrayDouble>(this, 1);
      case TableExprFuncNode::grmssFUNC:
        return std::make_shared<TableExprGroupRmssArrayDouble>(this);
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with an integer/double argument");
      }
    case NTComplex:
      switch (funcType()) {
      case TableExprFuncNode::gsumsFUNC:
        return std::make_shared<TableExprGroupSumsArrayDComplex>(this);
      case TableExprFuncNode::gproductsFUNC:
        return std::make_shared<TableExprGroupProductsArrayDComplex>(this);
      case TableExprFuncNode::gsumsqrsFUNC:
        return std::make_shared<TableExprGroupSumSqrsArrayDComplex>(this);
      case TableExprFuncNode::gmeansFUNC:
        return std::make_shared<TableExprGroupMeansArrayDComplex>(this);
      case TableExprFuncNode::gvariances0FUNC:
        return std::make_shared<TableExprGroupVariancesArrayDComplex>(this, 0);
      case TableExprFuncNode::gvariances1FUNC:
        return std::make_shared<TableExprGroupVariancesArrayDComplex>(this, 1);
      case TableExprFuncNode::gstddevs0FUNC:
        return std::make_shared<TableExprGroupStdDevsArrayDComplex>(this, 0);
      case TableExprFuncNode::gstddevs1FUNC:
        return std::make_shared<TableExprGroupStdDevsArrayDComplex>(this, 1);
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

  MArray<Bool> TableExprAggrNodeArray::getArrayBool (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayBool (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayBool();
  }
  MArray<Int64> TableExprAggrNodeArray::getArrayInt (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getArrayInt (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getArrayInt();
  }
  MArray<Double> TableExprAggrNodeArray::getArrayDouble (const TableExprId& id)
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
