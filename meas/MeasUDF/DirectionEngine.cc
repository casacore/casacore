//# DirectionEngine.cc: Engine for TaQL UDF Direction conversions
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

#include <meas/MeasUDF/DirectionEngine.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ExprUnitNode.h>
#include <tables/Tables/ExprNodeSet.h>
#include <casa/Arrays/ArrayUtil.h>

namespace casa {

  DirectionEngine::DirectionEngine()
    : itsNDim           (-1),
      itsRefType        (MDirection::N_Types),
      itsEpochEngine    (0),
      itsPositionEngine (0)
  {}

  Bool DirectionEngine::isConstant() const
  {
    Bool isConst = itsConstants.size() > 0;
    if (isConst && itsEpochEngine) {
      isConst = itsEpochEngine->isConstant();
    }
    if (isConst && itsPositionEngine) {
      isConst = itsPositionEngine->isConstant();
    }
    return isConst;
  }

  void DirectionEngine::handleDirection (PtrBlock<TableExprNodeRep*>& args,
                                         uInt& argnr)
  {
    // Initialize to unknown reference type.
    itsRefType = MDirection::N_Types;
    // Normally directions must be given in an array, but a single one
    // can be 2 scalars.
    uInt nargnr = argnr+1;
    Bool asScalar = False;
    // A string means that object names (e.g. MOON) are given.
    if (args[argnr]->dataType() == TableExprNodeRep::NTString) {
      handleNames (args[argnr]);
    } else {
      if (args[argnr]->dataType() != TableExprNodeRep::NTDouble) {
        throw AipsError ("Invalid or integer direction given in a MEAS function");
      }
      if (args.size() > nargnr  &&
          args[argnr]->dataType() == TableExprNodeRep::NTDouble  &&
          args[argnr]->valueType() == TableExprNodeRep::VTScalar  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTDouble  &&
          args[nargnr]->valueType() == TableExprNodeRep::VTScalar) {
        asScalar = True;
        nargnr++;
      }
      // See if a reference type is given.
      if (args.size() > nargnr  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTString) {
        handleDirType (args[nargnr]);
        nargnr++;
      }
      // Process as two scalars or as array.
      if (asScalar) {
        handleScalars (args[argnr], args[argnr+1]);
      } else {
        handleDirArray (args[argnr]);
      }
    }
    // Skip the arguments handled.
    argnr = nargnr;
    // Set shape for constants.
    if (itsConstants.size() > 0) {
      if (itsConstants.size() > 1) {
        itsShape = itsConstants.shape();
      }
      itsShape.prepend (IPosition(1,2));
    }
    // Determine the output unit, shape, and ndim.
    itsUnit = "rad";
    // Fill ndim if unknown and if shape is known.
    if (itsNDim < 0  &&  itsShape.size() > 0) {
      itsNDim = itsShape.size();
    }
  }

  void DirectionEngine::handleDirType (TableExprNodeRep* operand)
  {
    if (operand->dataType() != TableExprNodeRep::NTString  ||
        operand->valueType() != TableExprNodeRep::VTScalar  ||
        !operand->isConstant()) {
      throw AipsError ("A direction type given in a MEAS function "
                       "must be a constant scalar string");
    }
    String str = operand->getString(0);
    Bool fnd = MDirection::getType (itsRefType, str);
    if (!fnd) {
      throw AipsError ("Unknown direction reference type " + str +
                       " given in a MEAS function");
    }
  }

  void DirectionEngine::handleScalars (TableExprNodeRep* e1,
                                       TableExprNodeRep* e2)
  {
    if (! (e1->isConstant()  &&  e2->isConstant())) {
      throw AipsError ("Scalar values given as direction a MEAS function "
                       "must be constant values");
    }
    double v1 = e1->getDouble(0);
    double v2 = e2->getDouble(0);
    Unit u1 = e1->unit();
    Unit u2 = e2->unit();
    if (u1.empty()) u1 = "rad";
    if (u2.empty()) u2 = "rad";
    if (itsRefType == MDirection::N_Types) {
      itsRefType = MDirection::J2000;
    }
    itsConstants.resize (IPosition(1,1));
    itsConstants.data()[0] = MDirection(Quantity(v1, u1),
                                        Quantity(v2, u2),
                                        itsRefType);
  }

  void DirectionEngine::handleNames (TableExprNodeRep* operand)
  {
    if (! operand->isConstant()) {
      throw AipsError ("Object names given as directions in a MEAS function "
                       "must be constant values");
    }
    Array<String> names = operand->getStringAS(0).array();
    itsConstants.resize (names.shape());
    for (uInt i=0; i<names.size(); ++i) {
      itsConstants.data()[i] = MDirection::makeMDirection (names.data()[i]);
    }
  }

  void DirectionEngine::handleDirArray (TableExprNodeRep*& operand)
  {
    if (operand->dataType() != TableExprNodeRep::NTDouble  ||
        operand->valueType() != TableExprNodeRep::VTArray) {
      throw AipsError ("A single double argument given as direction in a "
                       "MEAS function must be a double array of values");
    }
    // Set or convert the operands's unit to radian.
    TableExprNodeUnit::adaptUnit (operand, "rad") ;
    // Handle possibly given constants.
    if (operand->isConstant()) {
      handleConstant (operand);
      return;
    }
    // Try if the argument is a column.
    // If so, try to handle it as a TableMeasures column.
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
      const ROTableColumn& tabCol = colNode->getColumn();
      itsShape = tabCol.shapeColumn();
      itsNDim  = tabCol.ndimColumn();
      if (TableMeasDescBase::hasMeasures (tabCol)) {
        ROArrayMeasColumn<MDirection> measTmp(tabCol.table(),
                                              tabCol.columnDesc().name());
        // Get and check the node's refType if it is fixed.
        MDirection::Types nodeRefType = MDirection::N_Types;
        if (! (measTmp.measDesc().isRefCodeVariable()  ||
               measTmp.measDesc().hasOffset())) {
          uInt refCode = measTmp.measDesc().getRefCode();
          nodeRefType = static_cast<MDirection::Types>(refCode);
          if (itsRefType != MDirection::N_Types  &&  nodeRefType != itsRefType) {
            throw AipsError ("Given MDirection reference type " +
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
        // If the reference type is variable, the user should index after
        // the meas.pos function.
        if (nodeRefType == MDirection::N_Types) {
          throw AipsError ("Column " + tabCol.columnDesc().name() +
                           ", which has a variable reference frame, "
                           "is used in a MEAS function with slicing. "
                           "The slicing should be done after the function "
                           "like 'meas.pos('ITRF',DIRECTION)[0:3]'");
        }
      }
    }
    if (itsMeasCol.isNull()) {
      if (itsRefType == MDirection::N_Types) {
        throw AipsError("No reference type given for a non-constant MEAS "
                        "function direction argument");
      }
      itsExprNode = operand;
    }
  }

  void DirectionEngine::handleConstant (TableExprNodeRep* operand)
  {
    AlwaysAssert (operand->valueType() != TableExprNodeRep::VTSet, AipsError);
    if (itsRefType == MDirection::N_Types) {
      itsRefType = MDirection::J2000;
    }
    TableExprNode node(operand);
    handleValues (node, 0, itsConstants);
  }

  void DirectionEngine::handleValues (TableExprNode& operand,
                                      const TableExprId& id,
                                      Array<MDirection>& directions)
  {
    Array<Double> values = operand.getArrayDouble(id).array();
    IPosition shape = values.shape();
    if (shape[0] % 2 != 0) {
      throw AipsError ("Number of values in a direction in a MEAS function "
                       "should be a multiple of 2");
    }
    IPosition dirShape;
    if (shape[0] == 2  &&  shape.size() > 1) {
      dirShape = shape.getLast (shape.size() - 1);
    } else {
      dirShape = shape;
      dirShape[0] /= 2;
    }
    directions.resize (dirShape);
    Quantity q1(0, operand.unit());
    Quantity q2(0, operand.unit());
    Double* valVec = values.data();
    MDirection* dirVec = directions.data();
    for (uInt i=0; i<directions.size(); ++i) {
      q1.setValue (valVec[i*2]);
      q2.setValue (valVec[i*2+1]);
      dirVec[i] = MDirection(q1, q2, itsRefType);
    }
  }

  void DirectionEngine::setEpochEngine (EpochEngine& engine)
  {
    AlwaysAssert (itsEpochEngine == 0, AipsError);
    itsEpochEngine = &engine;
    uInt ndim = engine.ndim();
    IPosition shape = engine.shape();
    if (ndim > 0  &&  itsNDim > 0) {
      itsNDim += ndim;
    }
    if (!shape.empty()  &&  !itsShape.empty()) {
      itsShape.append (shape);
    }
    // Define the frame part, so it can be reset later.
    itsFrame.set (MEpoch());
  }

  void DirectionEngine::setPositionEngine (PositionEngine& engine)
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

  void DirectionEngine::setConverter (MDirection::Types toType)
  {
    MDirection::Ref ref(toType, itsFrame);
    itsConverter = MDirection::Convert (toType, ref);
  }

  Array<MDirection> DirectionEngine::getDirections (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasCol.isNull()) {
      return itsMeasCol(id.rownr());
    }
    Array<MDirection> directions;
    handleValues (itsExprNode, id, directions);
    return directions;
  }

  Array<Double> DirectionEngine::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MDirection> res (getDirections(id));
    // Get epochs and positions if given.
    Array<MEpoch> eps(IPosition(1,1));
    if (itsEpochEngine) {
      Array<MEpoch> arr = itsEpochEngine->getEpochs (id);
      eps.reference (itsEpochEngine->getEpochs (id));
    }
    Array<MPosition> pos(IPosition(1,1));
    if (itsPositionEngine) {
      pos.reference (itsPositionEngine->getPositions (id));
    }
    // Convert the direction to the given type for all epochs and positions.
    Array<Double> out;
    if (res.size() > 0  &&  eps.size() > 0  &&  pos.size() > 0) {
      IPosition shape;
      if (res.size() > 1) {
        shape = res.shape();
      }
      shape.prepend (IPosition(1,2));    // 2 values per MDirection
      if (eps.size() > 1) {
        shape.append (eps.shape());
      }
      if (pos.size() > 1) {
        shape.append (pos.shape());
      }
      out.resize (shape);
      double* outPtr = out.data();
      for (Array<MDirection>::const_contiter resIter = res.cbegin();
           resIter != res.cend(); ++resIter) {
        itsConverter.setModel (*resIter);
        for (Array<MEpoch>::const_contiter epsIter = eps.cbegin();
           epsIter != eps.cend(); ++epsIter) {
          // Convert to desired epoch.
          if (itsEpochEngine) {
            itsFrame.resetEpoch (*epsIter);
          }
          for (Array<MPosition>::const_contiter posIter = pos.cbegin();
               posIter != pos.cend(); ++posIter) {
            // Convert to desired position.
            if (itsPositionEngine) {
              itsFrame.resetPosition (*posIter);
            }
            MDirection mdir = itsConverter();
            // Get as radians.
            Vector<Double> md (mdir.getValue().get());
            *outPtr++ = md[0];
            *outPtr++ = md[1];
          }
        }
      }
    }
    return out;
  }

} //end namespace
