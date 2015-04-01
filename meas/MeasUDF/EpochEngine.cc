//# EpochEngine.cc: Engine for TaQL UDF Epoch conversions
//# Copyright (C) 2011
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

#include <casacore/meas/MeasUDF/EpochEngine.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
//#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Arrays/ArrayIO.h>

namespace casacore {

  EpochEngine::EpochEngine()
    : itsNDim           (-1),
      itsRefType        (MEpoch::N_Types),
      itsPositionEngine (0)
  {}

  Bool EpochEngine::isConstant() const
  {
    Bool isConst = itsConstants.size() > 0;
    if (isConst && itsPositionEngine) {
      isConst = itsPositionEngine->isConstant();
    }
    return isConst;
  }

  void EpochEngine::handleEpoch (PtrBlock<TableExprNodeRep*>& args,
                                 uInt& argnr)
  {
    // Initialize type to unknown.
    itsRefType = MEpoch::N_Types;
    // Convert a string epoch argument to a date.
    if (args[argnr]->dataType() == TableExprNodeRep::NTString) {
      TableExprNode dNode = datetime (args[argnr]);
      TableExprNodeRep::unlink (args[argnr]);
      args[argnr] = const_cast<TableExprNodeRep*>(dNode.getNodeRep())->link();
    }
    // Check if the value is a date or double.
    if (args[argnr]->dataType() != TableExprNodeRep::NTDouble  &&
        args[argnr]->dataType() != TableExprNodeRep::NTDate) {
      throw AipsError ("Invalid or integer epoch given in a MEAS function");
    }
    // Values can be given as [t1,t2,...],reftype
    uInt nargnr = argnr+1;
    // See if there is a reference type.
    if (args.size() > nargnr  &&
        args[nargnr]->dataType() == TableExprNodeRep::NTString) {
      if (handleEpochType (args[nargnr], False)) {
        nargnr++;
      }
    }
    handleEpochArray (args[argnr]);
    // Skip the arguments handled.
    argnr = nargnr;
    // Determine the output unit, shape, and ndim.
    itsUnit = "s";
    // Fill ndim if unknown and if shape is known.
    if (itsNDim < 0  &&  itsShape.size() > 0) {
      itsNDim = itsShape.size();
    }
    if (itsShape.size() > 0) {
      // time has 1 value
      itsShape[0] = 1;
    }
  }

  Bool EpochEngine::handleEpochType (TableExprNodeRep* operand,
                                     Bool doThrow)
  {
    if (operand->dataType() != TableExprNodeRep::NTString  ||
        operand->valueType() != TableExprNodeRep::VTScalar  ||
        !operand->isConstant()) {
      if (doThrow) {
        throw AipsError ("An epoch type given in a MEAS function "
                         "must be a constant scalar string");
      }
      return False;
    }
    String str = operand->getString(0);
    MEpoch::Types refType;
    Bool fnd = MEpoch::getType (refType, str);
    if (fnd) {
      itsRefType = refType;
    } else if (doThrow) {
      throw AipsError ("Unknown epoch reference type " + str +
                       " given in a MEAS function");
    }
    return fnd;
  }

  void EpochEngine::handleEpochArray (TableExprNodeRep* operand)
  {
    if ((operand->dataType() != TableExprNodeRep::NTDouble  &&
         operand->dataType() != TableExprNodeRep::NTDate)  ||
        (operand->valueType() != TableExprNodeRep::VTScalar  &&
         operand->valueType() != TableExprNodeRep::VTArray)) {
      throw AipsError ("An epoch given in a MEAS function "
                       "must be a double, string, or datetime scalar or array");
    }
    if (operand->isConstant()) {
      handleConstant (operand);
      return;
    }
    // Try if the argument is a column.
    // If found, try to handle it as a TableMeasures column.
    const TableExprNodeArrayColumn* colNode =
      dynamic_cast<TableExprNodeArrayColumn*>(operand);
    Bool directCol = True;
    if (!colNode) {
      // The node is an expression, not a column.
      directCol = False;
      // Try if the node is an array part of a column.
      TableExprNodeArrayPart* partNode =
        dynamic_cast<TableExprNodeArrayPart*>(operand);
      if (partNode) {
        colNode = partNode->getColumnNode();
      }
    }
    if (colNode) {
      // Try if the column contains measures.
      const TableColumn& tabCol = colNode->getColumn();
      itsShape = tabCol.shapeColumn();
      itsNDim  = tabCol.ndimColumn();
      if (TableMeasDescBase::hasMeasures (tabCol)) {
        ArrayMeasColumn<MEpoch> measTmp(tabCol.table(),
                                        tabCol.columnDesc().name());
        // Get and check the node's refType if it is fixed.
        MEpoch::Types nodeRefType = MEpoch::N_Types;
        if (! (measTmp.measDesc().isRefCodeVariable()  ||
               measTmp.measDesc().hasOffset())) {
          uInt refCode = measTmp.measDesc().getRefCode();
          nodeRefType = static_cast<MEpoch::Types>(refCode);
          if (itsRefType != MEpoch::N_Types  &&  nodeRefType != itsRefType) {
            throw AipsError ("Given MEpoch reference type " +
                             String::toString(itsRefType) +
                             " mismatches type " +
                             String::toString(nodeRefType) + " of column " +
                             tabCol.columnDesc().name());
          }
          itsRefType = nodeRefType;
        }
        // A direct column can directly be accessed using TableMeasures.
        if (directCol) {
          itsMeasCol.reference (measTmp);
          return;
        }
        // It is a part, so we cannot use TableMeasures.
        // If the reference type is variable, the user should index the result
        // of the meas.epoch function.
        if (nodeRefType == MEpoch::N_Types) {
            throw AipsError ("Column " + tabCol.columnDesc().name() +
                             ", which has a variable reference frame, "
                             "is used in a MEAS function with slicing. "
                             "The slicing should be done after the function "
                             "like 'meas.epoch('UTC',TIMES)[0:3]'");
        }
      }
    }
    if (itsMeasCol.isNull()) {
      if (itsRefType == MEpoch::N_Types) {
        throw AipsError("No reference type given for a non-constant MEAS "
                        "function epoch argument");
      }
      // Convert expression from date to double if needed.
      itsExprNode = TableExprNode(operand);
      if (operand->dataType() == TableExprNodeRep::NTDate) {
        itsExprNode = mjd(itsExprNode);
      }
    }
  }

  void EpochEngine::handleConstant (TableExprNodeRep* operand)
  {
    // Get unit (default seconds).
    Unit unit = operand->unit();
    if (unit.empty()) {
      unit = "s";
    }
    // Get values (as doubles or dates).
    Array<Double> epochs;
    if (operand->dataType() == TableExprNodeRep::NTDouble) {
      epochs.reference (operand->getDoubleAS(0));
    } else {
      unit = "s";
      Array<MVTime> dates = operand->getDateAS(0);
      epochs.resize (dates.shape());
      for (uInt i=0; i<dates.size(); ++i) {
        epochs.data()[i] = dates.data()[i].second();
      }
    }
    // Use default reference type UTC if not given.
    if (itsRefType == MEpoch::N_Types) {
      itsRefType = MEpoch::UTC;
    }
    Vector<Double> epVec(epochs.reform(IPosition(1,epochs.size())));
    itsConstants.resize (epochs.size());
    for (uInt i=0; i<itsConstants.size(); ++i) {
      itsConstants[i] = MEpoch(Quantity(epVec[i], unit), itsRefType);
    }
  }

  void EpochEngine::setPositionEngine (PositionEngine& engine)
  {
    AlwaysAssert (itsPositionEngine == 0, AipsError);
    itsPositionEngine = &engine;
    uInt ndim = engine.ndim();
    IPosition shape = engine.shape();
    if (ndim > 0  &&  itsNDim > 0) {
      itsNDim += ndim;
    }
    if (!shape.empty()  &&  !itsShape.empty()) {
      itsShape.append (shape);
    }
    // Define the frame part, so it can be reset later.
    itsFrame.set (MPosition());
  }

  void EpochEngine::setConverter (MEpoch::Types toType)
  {
    MEpoch::Ref ref(toType, itsFrame);
    itsConverter = MEpoch::Convert (toType, ref);
  }

  Array<MEpoch> EpochEngine::getEpochs (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasCol.isNull()) {
      return itsMeasCol(id.rownr());
    }
    Array<Double> values = itsExprNode.getDoubleAS(id);
    Array<MEpoch> epochs(values.shape());
    Unit unit = itsExprNode.unit();
    if (unit.empty()) {
      unit = "s";
    }
    Quantity q(0, unit);
    for (uInt i=0; i<values.size(); ++i) {
      q.setValue (values.data()[i]);
      epochs.data()[i] = MEpoch(q, itsRefType);
    }
    return epochs;
  }

  Array<Double> EpochEngine::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MEpoch> res (getEpochs(id));
    // Get positions if given.
    Array<MPosition> pos(IPosition(1,1));
    if (itsPositionEngine) {
      pos.reference (itsPositionEngine->getPositions (id));
    }
    // Convert the epoch to the given type for all positions.
    Array<Double> out;
    if (res.size() > 0  &&  pos.size() > 0) {
      IPosition shape = res.shape();
      if (pos.size() > 1) {
        shape.append (pos.shape());
      }
      out.resize (shape);
      double* outPtr = out.data();
      for (Array<MEpoch>::const_contiter resIter = res.cbegin();
           resIter != res.cend(); ++resIter) {
        itsConverter.setModel (*resIter);
        for (Array<MPosition>::const_contiter posIter = pos.cbegin();
           posIter != pos.cend(); ++posIter) {
          // Convert to desired reference type.
          if (itsPositionEngine) {
            itsFrame.resetPosition (*posIter);
          }
          MEpoch ep = itsConverter();
          // Convert to seconds.
          *outPtr++ = ep.getValue().get() * 24*3600;
        }
      }
    }
    return out;
  }

} //end namespace
