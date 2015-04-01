//# ExprAggrNode.cc: TaQL node representing an aggregate function
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
//# $Id: TaQLNode.h 21051 2011-04-20 11:46:29Z gervandiepen $

//# Includes
#include <casacore/tables/TaQL/ExprAggrNode.h>
#include <casacore/tables/TaQL/ExprGroupAggrFunc.h>
#include <casacore/tables/TaQL/ExprGroupAggrFuncArray.h>
#include <casacore/tables/TaQL/TableExprIdAggr.h>
#include <casacore/tables/Tables/TableError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


  TableExprAggrNode::TableExprAggrNode (FunctionType ftype,
                                        NodeDataType dtype,
                                        ValueType vtype,
                                        const TableExprNodeSet& source)
    : TableExprFuncNode (ftype, dtype, vtype, source)
  {
    // Always treat an aggregate as a variable expression.
    // Otherwise if might be treated as constant and evaluated immediately
    // which cannot be done.
    exprtype_p = Variable;
  }

  TableExprFuncNode::NodeDataType TableExprAggrNode::checkOperands
  (Block<Int>& dtypeOper, ValueType& resVT, FunctionType ftype,
   PtrBlock<TableExprNodeRep*>& nodes)
  {
    resVT = VTScalar;
    switch (ftype) {
    case countallFUNC:
      checkNumOfArg (0, 0, nodes);
      return NTInt;
    case gcountFUNC:
      checkNumOfArg (1, 1, nodes);
      return checkDT (dtypeOper, NTAny, NTInt, nodes);
    case gfirstFUNC:
    case glastFUNC:
      checkNumOfArg (1, 1, nodes);
      resVT = nodes[0]->valueType();
      return checkDT (dtypeOper, NTAny, NTAny, nodes);
    case gexpridFUNC:
      checkNumOfArg (0, 0, nodes);
      return NTInt;
    case gaggrFUNC:
      checkNumOfArg (1, 1, nodes);
      resVT = VTArray;
      return checkDT (dtypeOper, NTAny, NTAny, nodes);
    case ghistFUNC:
      checkNumOfArg (4, 4, nodes);
      if (nodes[1]->dataType() != NTInt) {
        throw TableInvExpr ("2nd argument of function GHIST "
                            "has to be a constant integer scalar");
      }
      for (int i=1; i<4; ++i) {
        if (nodes[i]->valueType() != VTScalar  ||
            ! nodes[i]->isConstant()) {
          throw TableInvExpr ("2nd, 3rd and 4th argument of function GHIST "
                              "have to be constant scalars");
        }
      }
      resVT = VTArray;
      return checkDT (dtypeOper, NTReal, NTInt, nodes);
    case growidFUNC:
      checkNumOfArg (0, 0, nodes);
      resVT = VTArray;
      return checkDT (dtypeOper, NTAny, NTInt, nodes);
    case gminFUNC:
    case gmaxFUNC:
      checkNumOfArg (1, 1, nodes);
      return checkDT (dtypeOper, NTReal, NTReal, nodes);
    case gsumFUNC:
    case gproductFUNC:
    case gsumsqrFUNC:
      checkNumOfArg (1, 1, nodes);
      return checkDT (dtypeOper, NTNumeric, NTNumeric, nodes);
    case gmeanFUNC:
      checkNumOfArg (1, 1, nodes);
      return checkDT (dtypeOper, NTNumeric, NTDouCom, nodes);
    case gvarianceFUNC:
    case gstddevFUNC:
    case grmsFUNC:
    case gmedianFUNC:
      checkNumOfArg (1, 1, nodes);
      return checkDT (dtypeOper, NTReal, NTDouble, nodes);
    case gfractileFUNC:
      checkNumOfArg (2, 2, nodes);
      if (nodes[1]->valueType() != VTScalar  ||
          ! nodes[1]->isConstant()) {
        throw TableInvExpr ("2nd argument of function GFRACTILE "
                            "has to be a constant scalar");
      }
      return checkDT (dtypeOper, NTReal, NTDouble, nodes);
    case ganyFUNC:
    case gallFUNC:
      checkNumOfArg (1, 1, nodes);
      return checkDT (dtypeOper, NTBool, NTBool, nodes);
    case gntrueFUNC:
    case gnfalseFUNC:
      checkNumOfArg (1, 1, nodes);
      return checkDT (dtypeOper, NTBool, NTInt, nodes);
    default:
        throw TableInvExpr ("Unhandled aggregate function " +
                            String::toString(ftype));
    }
  }

  void TableExprAggrNode::getAggrNodes (vector<TableExprNodeRep*>& aggr)
  {
    aggr.push_back (this);
    uInt naggr = aggr.size();
    for (uInt i=0; i<operands_p.size(); ++i) {
      operands()[i]->getAggrNodes (aggr);
    }
    if (naggr != aggr.size()) {
      throw TableInvExpr ("The argument of an aggregate function cannot use "
                          "an aggregate function");
    }
  }

  CountedPtr<TableExprGroupFuncBase> TableExprAggrNode::makeGroupAggrFunc()
  {
    // Create a new function object because each FuncSet needs its own one.
    itsFunc = doMakeGroupAggrFunc();
    return itsFunc;
  }

  Bool TableExprAggrNode::isLazyAggregate() const
  {
    return itsFunc->isLazy();
  }

  TableExprGroupFuncBase* TableExprAggrNode::doMakeGroupAggrFunc()
  {
    if (funcType() == countallFUNC) {
      return new TableExprGroupCountAll(this);
    } else if (funcType() == gcountFUNC) {
      return new TableExprGroupCount(this);
    } else if (funcType() == gfirstFUNC) {
      return new TableExprGroupFirst(this);
    } else if (funcType() == glastFUNC) {
      return new TableExprGroupLast(this);
    } else if (funcType() == gexpridFUNC) {
      return new TableExprGroupExprId(this);
    } else if (funcType() == gaggrFUNC) {
      return new TableExprGroupAggr(this);
    } else if (funcType() == growidFUNC) {
      return new TableExprGroupRowid(this);
    }
    if (operands()[0]->valueType() == VTScalar) {
      switch (operands()[0]->dataType()) {
      case NTBool:
        switch (funcType()) {
        case ganyFUNC:
          return new TableExprGroupAny(this);
        case gallFUNC:
          return new TableExprGroupAll(this);
        case gntrueFUNC:
          return new TableExprGroupNTrue(this);
        case gnfalseFUNC:
          return new TableExprGroupNFalse(this);
        default:
          throw TableInvExpr ("Aggregate function " +
                              String::toString(funcType()) +
                              " cannot be used with a bool argument");
        }
      case NTInt:
        switch (funcType()) {
        case gminFUNC:
          return new TableExprGroupMinInt(this);
        case gmaxFUNC:
          return new TableExprGroupMaxInt(this);
        case gsumFUNC:
          return new TableExprGroupSumInt(this);
        case gproductFUNC:
          return new TableExprGroupProductInt(this);
        case gsumsqrFUNC:
          return new TableExprGroupSumSqrInt(this);
        default:
          break;
        }
        // Fall through, so e.g. mean of ints can be done
      case NTDouble:
        switch (funcType()) {
        case gminFUNC:
          return new TableExprGroupMinDouble(this);
        case gmaxFUNC:
          return new TableExprGroupMaxDouble(this);
        case gsumFUNC:
          return new TableExprGroupSumDouble(this);
        case gproductFUNC:
          return new TableExprGroupProductDouble(this);
        case gsumsqrFUNC:
          return new TableExprGroupSumSqrDouble(this);
        case gmeanFUNC:
          return new TableExprGroupMeanDouble(this);
        case gvarianceFUNC:
          return new TableExprGroupVarianceDouble(this);
        case gstddevFUNC:
          return new TableExprGroupStdDevDouble(this);
        case grmsFUNC:
          return new TableExprGroupRmsDouble(this);
        case gmedianFUNC:
          return new TableExprGroupFractileDouble(this, 0.5);
        case gfractileFUNC:
          return new TableExprGroupFractileDouble
            (this, operands()[1]->getDouble(0));
        case ghistFUNC:
          return new TableExprGroupHistDouble
            (this, operands()[1]->getInt(0),
             operands()[2]->getDouble(0),
             operands()[3]->getDouble(0));
        default:
          throw TableInvExpr ("Aggregate function " +
                              String::toString(funcType()) +
                              " cannot be used with an integer/double"
                              " argument");
        }
      case NTComplex:
        switch (funcType()) {
        case gsumFUNC:
          return new TableExprGroupSumDComplex(this);
        case gproductFUNC:
          return new TableExprGroupProductDComplex(this);
        case gsumsqrFUNC:
          return new TableExprGroupSumSqrDComplex(this);
        case gmeanFUNC:
          return new TableExprGroupMeanDComplex(this);
        default:
          throw TableInvExpr ("Aggregate function " +
                              String::toString(funcType()) +
                              " cannot be used with a dcomplex argument");
        }
      default:
        break;
      }
      throw TableInvExpr ("Aggregate function " +
                          String::toString(funcType()) +
                          " is unknown for scalar data type " + 
                          String::toString(operands()[0]->dataType()));
    }
    // The operand is an array.
    switch (operands()[0]->dataType()) {
    case NTBool:
      switch (funcType()) {
      case ganyFUNC:
        return new TableExprGroupArrayAny(this);
      case gallFUNC:
        return new TableExprGroupArrayAll(this);
      case gntrueFUNC:
        return new TableExprGroupArrayNTrue(this);
      case gnfalseFUNC:
        return new TableExprGroupArrayNFalse(this);
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with a bool argument");
      }
    case NTInt:
      switch (funcType()) {
      case gminFUNC:
        return new TableExprGroupMinArrayInt(this);
      case gmaxFUNC:
        return new TableExprGroupMaxArrayInt(this);
      case gsumFUNC:
        return new TableExprGroupSumArrayInt(this);
      case gproductFUNC:
        return new TableExprGroupProductArrayInt(this);
      case gsumsqrFUNC:
        return new TableExprGroupSumSqrArrayInt(this);
      default:
        break;
      }
      // Fall through, so e.g. mean of ints can be done
    case NTDouble:
      switch (funcType()) {
      case gminFUNC:
        return new TableExprGroupMinArrayDouble(this);
      case gmaxFUNC:
        return new TableExprGroupMaxArrayDouble(this);
      case gsumFUNC:
        return new TableExprGroupSumArrayDouble(this);
      case gproductFUNC:
        return new TableExprGroupProductArrayDouble(this);
      case gsumsqrFUNC:
        return new TableExprGroupSumSqrArrayDouble(this);
      case gmeanFUNC:
        return new TableExprGroupMeanArrayDouble(this);
      case gvarianceFUNC:
        return new TableExprGroupVarianceArrayDouble(this);
      case gstddevFUNC:
        return new TableExprGroupStdDevArrayDouble(this);
      case grmsFUNC:
        return new TableExprGroupRmsArrayDouble(this);
      case gmedianFUNC:
        return new TableExprGroupFractileArrayDouble(this, 0.5);
      case gfractileFUNC:
        return new TableExprGroupFractileArrayDouble
          (this, operands()[1]->getDouble(0));
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with an integer/double argument");
      }
    case NTComplex:
      switch (funcType()) {
      case gsumFUNC:
        return new TableExprGroupSumArrayDComplex(this);
      case gproductFUNC:
        return new TableExprGroupProductArrayDComplex(this);
      case gsumsqrFUNC:
        return new TableExprGroupSumSqrArrayDComplex(this);
      case gmeanFUNC:
        return new TableExprGroupMeanArrayDComplex(this);
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with a dcomplex argument");
      }
    default:
      break;
    }
    throw TableInvExpr ("Aggregate function " +
                        String::toString(funcType()) +
                        " is unknown for array data type " + 
                        String::toString(operands()[0]->dataType()));
  }

  Bool TableExprAggrNode::getBool (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getBool (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getBool();
  }
  Int64 TableExprAggrNode::getInt (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getInt (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getInt();
  }
  Double TableExprAggrNode::getDouble (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getDouble (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getDouble();
  }
  DComplex TableExprAggrNode::getDComplex (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getDComplex (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getDComplex();
  }
  String TableExprAggrNode::getString (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getString (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getString();
  }
  MVTime TableExprAggrNode::getDate (const TableExprId& id)
  {
    const TableExprIdAggr& aid = TableExprIdAggr::cast (id);
    if (itsFunc->isLazy()) {
      return itsFunc->getDate (aid.result().ids(id.rownr()));
    }
    TableExprGroupFuncSet& set = aid.result().funcSet(id.rownr());
    return set.getFuncs()[itsFunc->seqnr()]->getDate();
  }

} //# NAMESPACE CASACORE - END
