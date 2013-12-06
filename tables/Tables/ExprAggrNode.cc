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
#include <tables/Tables/ExprAggrNode.h>
#include <tables/Tables/ExprGroupAggrFunc.h>
#include <tables/Tables/ExprGroupAggrFuncArray.h>
#include <tables/Tables/TableError.h>


namespace casa { //# NAMESPACE CASA - BEGIN


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
    ///    if (ftype != gcountFUNC  &&  !nodes.empty()) {
    ///      if (nodes[0]->valueType() != VTScalar) {
    ///        throw TableInvExpr ("Currently aggregate functions only support "
    ///                            "scalar values");
    ///      }
    ///    }
    resVT = VTScalar;
    switch (ftype) {
    case countallFUNC:
      checkNumOfArg (0, 0, nodes);
      return NTInt;
    case gcountFUNC:
      checkNumOfArg (1, 1, nodes);
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

  void TableExprAggrNode::getAggrNodes (vector<TableExprAggrNode*>& aggr)
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

  TableExprGroupFunc* TableExprAggrNode::makeGroupFunc()
  {
    if (funcType() == gcountFUNC) {
      return new TableExprGroupCount(*this);
    } else if (funcType() == countallFUNC) {
      return new TableExprGroupCountAll();
    }
    if (operands()[0]->valueType() == VTScalar) {
      switch (operands()[0]->dataType()) {
      case NTBool:
        switch (funcType()) {
        case ganyFUNC:
          return new TableExprGroupAny();
        case gallFUNC:
          return new TableExprGroupAll();
        case gntrueFUNC:
          return new TableExprGroupNTrue();
        case gnfalseFUNC:
          return new TableExprGroupNFalse();
        default:
          throw TableInvExpr ("Aggregate function " +
                              String::toString(funcType()) +
                              " cannot be used with a bool argument");
        }
      case NTInt:
        switch (funcType()) {
        case gminFUNC:
          return new TableExprGroupMinInt();
        case gmaxFUNC:
          return new TableExprGroupMaxInt();
        case gsumFUNC:
          return new TableExprGroupSumInt();
        case gproductFUNC:
          return new TableExprGroupProductInt();
        case gsumsqrFUNC:
          return new TableExprGroupSumSqrInt();
        default:
          break;
        }
        // Fall through, so e.g. mean of ints can be done
      case NTDouble:
        switch (funcType()) {
        case gminFUNC:
          return new TableExprGroupMinDouble();
        case gmaxFUNC:
          return new TableExprGroupMaxDouble();
        case gsumFUNC:
          return new TableExprGroupSumDouble();
        case gproductFUNC:
          return new TableExprGroupProductDouble();
        case gsumsqrFUNC:
          return new TableExprGroupSumSqrDouble();
        case gmeanFUNC:
          return new TableExprGroupMeanDouble();
        case gvarianceFUNC:
          return new TableExprGroupVarianceDouble();
        case gstddevFUNC:
          return new TableExprGroupStdDevDouble();
        case grmsFUNC:
          return new TableExprGroupRmsDouble();
        case gmedianFUNC:
          return new TableExprGroupFractileDouble(0.5);
        case gfractileFUNC:
          return new TableExprGroupFractileDouble(operands()[1]->getDouble(0));
        default:
          throw TableInvExpr ("Aggregate function " +
                              String::toString(funcType()) +
                              " cannot be used with an integer/double argument");
        }
      case NTComplex:
        switch (funcType()) {
        case gsumFUNC:
          return new TableExprGroupSumDComplex();
        case gproductFUNC:
          return new TableExprGroupProductDComplex();
        case gsumsqrFUNC:
          return new TableExprGroupSumSqrDComplex();
        case gmeanFUNC:
          return new TableExprGroupMeanDComplex();
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
                          " is unknown for data type " + 
                          String::toString(operands()[0]->dataType()));
    }
    switch (operands()[0]->dataType()) {
    case NTBool:
      switch (funcType()) {
      case ganyFUNC:
        return new TableExprGroupArrAny();
      case gallFUNC:
        return new TableExprGroupArrAll();
      case gntrueFUNC:
        return new TableExprGroupArrNTrue();
      case gnfalseFUNC:
        return new TableExprGroupArrNFalse();
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with a bool argument");
      }
    case NTInt:
      switch (funcType()) {
      case gminFUNC:
        return new TableExprGroupMinArrInt();
      case gmaxFUNC:
        return new TableExprGroupMaxArrInt();
      case gsumFUNC:
        return new TableExprGroupSumArrInt();
      case gproductFUNC:
        return new TableExprGroupProductArrInt();
      case gsumsqrFUNC:
        return new TableExprGroupSumSqrArrInt();
      default:
        break;
      }
      // Fall through, so e.g. mean of ints can be done
    case NTDouble:
      switch (funcType()) {
      case gminFUNC:
        return new TableExprGroupMinArrDouble();
      case gmaxFUNC:
        return new TableExprGroupMaxArrDouble();
      case gsumFUNC:
        return new TableExprGroupSumArrDouble();
      case gproductFUNC:
        return new TableExprGroupProductArrDouble();
      case gsumsqrFUNC:
        return new TableExprGroupSumSqrArrDouble();
      case gmeanFUNC:
        return new TableExprGroupMeanArrDouble();
      case gvarianceFUNC:
        return new TableExprGroupVarianceArrDouble();
      case gstddevFUNC:
        return new TableExprGroupStdDevArrDouble();
      case grmsFUNC:
        return new TableExprGroupRmsArrDouble();
      case gmedianFUNC:
        return new TableExprGroupFractileArrDouble(0.5);
      case gfractileFUNC:
        return new TableExprGroupFractileArrDouble(operands()[1]->getDouble(0));
      default:
        throw TableInvExpr ("Aggregate function " +
                            String::toString(funcType()) +
                            " cannot be used with an integer/double argument");
      }
    case NTComplex:
      switch (funcType()) {
      case gsumFUNC:
        return new TableExprGroupSumArrDComplex();
      case gproductFUNC:
        return new TableExprGroupProductArrDComplex();
      case gsumsqrFUNC:
        return new TableExprGroupSumSqrArrDComplex();
      case gmeanFUNC:
        return new TableExprGroupMeanArrDComplex();
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
                        " is unknown for data type " + 
                        String::toString(operands()[0]->dataType()));
  }

  void TableExprAggrNode::setResult
  (const vector<CountedPtr<TableExprGroupFuncSet> >& funcs,
   uInt funcnr)
  {
    itsResult = &funcs;
    itsFuncNr = funcnr;
  }

  Bool      TableExprAggrNode::getBool (const TableExprId& id)
    { return (*itsResult)[id.seqnr()]->getFuncs()[itsFuncNr]->getBool(); }
  Int64     TableExprAggrNode::getInt      (const TableExprId& id)
    { return (*itsResult)[id.seqnr()]->getFuncs()[itsFuncNr]->getInt(); }
  Double    TableExprAggrNode::getDouble   (const TableExprId& id)
    { return (*itsResult)[id.seqnr()]->getFuncs()[itsFuncNr]->getDouble(); }
  DComplex  TableExprAggrNode::getDComplex (const TableExprId& id)
    { return (*itsResult)[id.seqnr()]->getFuncs()[itsFuncNr]->getDComplex(); }
  String    TableExprAggrNode::getString   (const TableExprId& id)
    { return (*itsResult)[id.seqnr()]->getFuncs()[itsFuncNr]->getString(); }
  MVTime    TableExprAggrNode::getDate     (const TableExprId& id)
    { return (*itsResult)[id.seqnr()]->getFuncs()[itsFuncNr]->getDate(); }

} //# NAMESPACE CASA - END

