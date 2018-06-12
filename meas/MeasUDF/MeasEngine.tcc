//# MeasEngine.tcc: Base class for the TaQL UDF conversion engines
//# Copyright (C) 2018
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

#ifndef MEAS_MEASENGINE_TCC
#define MEAS_MEASENGINE_TCC

//# Includes
#include <casacore/meas/MeasUDF/MeasEngine.h>
#include <casacore/tables/TaQL/ExprDerNode.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>


namespace casacore {

  template <typename M>
  MeasEngine<M>::~MeasEngine()
  {}
  
  template <typename M>
  Bool MeasEngine<M>::handleMeasType (const TENShPtr& operand,
                                      Bool doThrow)
  {
    if (operand->dataType() != TableExprNodeRep::NTString  ||
        operand->valueType() != TableExprNodeRep::VTScalar  ||
        !operand->isConstant()) {
      if (doThrow) {
        throw AipsError (M::showMe() + " type given in a MEAS function "
                         "must be a constant scalar string");
      }
      return False;
    }
    String str = operand->getString(0);
    str.upcase();
    // Let a derived class strip part of the mesaure type (as needed).
    str = stripMeasType (str);
    typename M::Types refType;
    Bool fnd = M::getType (refType, str);
    if (fnd) {
      itsRefType = refType;
    } else if (doThrow) {
      throw AipsError ("Unknown " + M::showMe() + " reference type " + str +
                       " given in a MEAS function");
    }
    return fnd;
  }

  template <typename M>
  void MeasEngine<M>::handleMeasArray (const TENShPtr& operand)
  {
    itsInUnit = operand->unit();
    if ((!operand->isReal())  ||
        (operand->valueType() != TableExprNodeRep::VTScalar  &&
         operand->valueType() != TableExprNodeRep::VTArray)) {
      throw AipsError (M::showMe() + " value given in a MEAS function "
                       "must be a numeric scalar or array");
    }
    // See if the operand has MeasUDF attributes.
    // If so, they define the reference and value type.
    // Note that the same field names are used as in TableMeasures.
    if (operand->attributes().isDefined("MEASINFO")) {
      const Record& measAttr = operand->attributes().subRecord("MEASINFO");
      String type = measAttr.asString("type");
      String ref  = measAttr.asString("Ref");
      Int valueType = measAttr.asInt ("ValueType");
      // Check if type matches.
      if (type != M::showMe()) {
        throw AipsError (M::showMe() + " value expected in a MEAS function, found " + type);
      }
      AlwaysAssert (M::getType(itsRefType, ref), AipsError);
      setValueType (valueType);
      return;
    }
    // Let inherited classes derive attributes as needed.
    deriveAttr (operand->unit(), 0);
    if (operand->isConstant()) {
      handleConstant (operand);
      return;
    }
    // Try if the argument is a column.
    // If found, try to handle it as a TableMeasures column.
    const TableColumn* tabCol = 0;
    Bool directCol = True;
    const TableExprNodeColumn* scaNode =
      dynamic_cast<TableExprNodeColumn*>(operand.get());
    if (scaNode) {
      tabCol = &(scaNode->getColumn());
    } else {
      const TableExprNodeArrayColumn* colNode =
        dynamic_cast<TableExprNodeArrayColumn*>(operand.get());
      if (colNode) {
        tabCol = &(colNode->getColumn());
      } else {
        // The node is an expression, not a column.
        directCol = False;
        // Try if the node is an array part of a column.
        TableExprNodeArrayPart* partNode =
          dynamic_cast<TableExprNodeArrayPart*>(operand.get());
        if (partNode) {
          colNode = partNode->getColumnNode();
          tabCol  = &(colNode->getColumn());
        }
      }
    }
    if (tabCol) {
      // Try if the column contains measures.
      if (scaNode) {
        itsNDim = 0;
      } else {
        itsNDim  = operand->ndim();
        itsShape = operand->shape();
      }
      if (TableMeasDescBase::hasMeasures (*tabCol)) {
        TableMeasColumn measTmp(tabCol->table(),
                                tabCol->columnDesc().name());
        // Check the measure is the correct type.
        AlwaysAssert(measTmp.measDesc().type() == M::showMe(),
                     AipsError);
        // Get and check the node's refType if it is fixed.
        typename M::Types nodeRefType = M::N_Types;
        if (! (measTmp.measDesc().isRefCodeVariable()  ||
               measTmp.measDesc().hasOffset())) {
          uInt refCode = measTmp.measDesc().getRefCode();
          nodeRefType = static_cast<typename M::Types>(refCode);
          if (itsRefType != M::N_Types  &&  nodeRefType != itsRefType) {
            throw AipsError ("MEAS " + M::showMe() + " reference type " +
                             String::toString(itsRefType) +
                             " mismatches type " +
                             String::toString(nodeRefType) + " of column " +
                             tabCol->columnDesc().name());
          }
          itsRefType = nodeRefType;
        }
        // A direct column can directly be accessed using TableMeasures.
        if (directCol) {
          if (scaNode) {
            itsMeasScaCol.attach (tabCol->table(),
                                  tabCol->columnDesc().name());
          } else {
            itsMeasArrCol.attach (tabCol->table(),
                                  tabCol->columnDesc().name());
          }
          return;
        }
        // It is a part, so we cannot use TableMeasures.
        // If the reference type is variable, the user should index the result
        // of the meas.M function.
        if (nodeRefType == M::N_Types) {
            throw AipsError ("Column " + tabCol->columnDesc().name() +
                             ", which has a variable reference frame, "
                             "is used in a MEAS function with slicing. "
                             "The slicing should be done after the function "
                             "like 'meas.direction(arguments)[0:3]'");
        }
      }
    }
    if (itsMeasScaCol.isNull()  &&  itsMeasArrCol.isNull()) {
      if (itsRefType == M::N_Types) {
        throw AipsError("No reference type given for a non-constant MEAS "
                        "function " + M::showMe() + " argument");
      }
      itsExprNode = TableExprNode(operand);
    }
  }

  template<typename M>
  void MeasEngine<M>::handleConstant (const TENShPtr& operand)
  {
    AlwaysAssert (operand->valueType() != TableExprNodeRep::VTSet, AipsError);
    if (itsRefType == M::N_Types) {
      itsRefType = M::DEFAULT;
    }
    TableExprNode node(operand);
    handleValues (node, 0, itsConstants);
  }

  template<typename M>
  Record MeasEngine<M>::makeAttributes (typename M::Types refType,
                                        Int valueType) const
  {
    // This is the opposite of testing attributes in handleMeasArray above.
    Record srec;
    srec.define ("type", M::showMe());
    srec.define ("Ref", M::showType(refType));
    srec.define ("ValueType", valueType);
    Record rec;
    rec.defineRecord ("MEASINFO", srec);
    return rec;
  }

}

#endif
