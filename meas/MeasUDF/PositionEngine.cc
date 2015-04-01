//# PositionEngine.cc: Engine for TaQL UDF Position conversions
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

#include <casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/casa/Arrays/ArrayUtil.h>

namespace casacore {

  PositionEngine::PositionEngine()
    : itsNDim      (-1),
      itsRefType   (MPosition::N_Types),
      itsValueType (0)
  {}

  void PositionEngine::handlePosition (Int toValueType,
                                       PtrBlock<TableExprNodeRep*>& args,
                                       uInt& argnr)
  {
    // Types are unknown.
    itsRefType    = MPosition::N_Types;
    itsValueType  = 0;
    uInt nargnr   = argnr+1;
    Bool asScalar = False;
    if (args[argnr]->dataType() == TableExprNodeRep::NTString) {
      // Position is given by observatory name.
      handleObservatory (args[argnr]);
    } else {
      if (args[argnr]->dataType() != TableExprNodeRep::NTDouble) {
        throw AipsError ("Invalid or integer position given in a MEAS function");
      }
    // Normally positions must be given in an array, but a single one
    // can be 2 or 3 scalars.
      if (args.size() > nargnr  &&
          args[argnr]->dataType() == TableExprNodeRep::NTDouble  &&
          args[argnr]->valueType() == TableExprNodeRep::VTScalar  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTDouble  &&
          args[nargnr]->valueType() == TableExprNodeRep::VTScalar) {
        asScalar = True;
        nargnr++;
      }
      // See if there is a node giving height(s).
      TableExprNodeRep* heightNode=0;
      if (args.size() > nargnr  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTDouble) {
        heightNode = args[nargnr];
        nargnr++;
      }
      // See if there is a reference type.
      if (args.size() > nargnr  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTString) {
        handlePosType (args[nargnr]);
        nargnr++;
      }
      // Process as 2 or 3 scalars or as array.
      if (asScalar) {
        handleScalars (args[argnr], args[argnr+1], heightNode);
      } else {
        // Get the position arguments.
        if (heightNode) {
          // For time being, heights can only be given as constants.
          handlePosArray (args[argnr], heightNode);
        } else {
          handlePosArray (args[argnr]);
        }
      }
    }
    // Skip the arguments handled.
    argnr = nargnr;
    // Determine the output unit, shape, and ndim.
    itsOutUnit = "m";
    if (toValueType == 2) {
      itsOutUnit = "rad";          // angles
    }
    if (itsConstants.size() > 0) {
      if (itsConstants.size() > 1) {
        itsShape = itsConstants.shape();
      }
      itsShape.prepend (IPosition(1,3));
    }
    // If returning height, we use one dimension less.
    if (toValueType == 1) {
      if (itsShape.size() > 0) {
        IPosition outShape = itsShape.getLast (itsShape.size() - 1);
        itsShape.resize (outShape.size());
        itsShape = outShape;
      }
    } else if (itsShape.size() > 0) {
      // Set correct nr of output values per position.
      itsShape[0] = toValueType;
    }
    // Fill ndim if unknown and if shape is known.
    if (itsNDim < 0  &&  itsShape.size() > 0) {
      itsNDim = itsShape.size();
    }
  }

  void PositionEngine::handlePosType (TableExprNodeRep* operand)
  {
    if (operand->dataType() != TableExprNodeRep::NTString  ||
        operand->valueType() != TableExprNodeRep::VTScalar  ||
        !operand->isConstant()) {
      throw AipsError ("A position type given in a MEAS function "
                       "must be a constant scalar string");
    }
    // Get value and possible suffix (value type).
    String str = operand->getString(0);
    str.upcase();
    int lens = str.size();
    if (lens > 3  &&  str.substr(lens-3,3) == "XYZ") {
      itsValueType = 3;
      str = str.substr(0,lens-3);
    } else if (lens > 2  &&  str.substr(lens-2,2) == "LL") {
      itsValueType = 2;
      str = str.substr(0,lens-2);
    } else if (lens > 6  &&  str.substr(lens-6,6) == "LONLAT") {
      itsValueType = 2;
      str = str.substr(0,lens-6);
    } else if (lens > 1  &&  str.substr(lens-1,1) == "H") {
      itsValueType = 1;
      str = str.substr(0,lens-1);
    } else if (lens > 6  &&  str.substr(lens-6,6) == "HEIGHT") {
      itsValueType = 1;
      str = str.substr(0,lens-6);
    } else {
      itsValueType = 0;
    }
    // Get reference type.
    if (! MPosition::getType (itsRefType, str)) {
      throw AipsError ("Unknown position reference type and/or suffix " + str +
                       " given in a MEAS function");
    }
  }

  void PositionEngine::makeDefaults (const Unit& unit)
  {
    // Check if the unit is length or angle.
    itsInUnit = unit;
    int valueType = 0;
    if (! itsInUnit.empty()) {
      Quantity q(1., itsInUnit);
      if (q.isConform ("m")) {
        valueType = 3;
      } else if (q.isConform ("rad")) {
        valueType = 2;
      } else {
        throw AipsError ("Invalid unit given for a position value"
                         " in a MEAS function (no length or angle)");
      }
    }
    // Use derived value type if not given.
    if (itsValueType == 0) {
      itsValueType = valueType;
    } else if (valueType > 0  &&  valueType != itsValueType) {
      throw AipsError ("Value unit " + unit.getName() + " mismatches type "
                       " suffix for a position in a MEAS function");
    }
    // Use default reference type if not given.
    if (itsValueType == 3) {
      if (itsRefType == MPosition::N_Types) itsRefType = MPosition::ITRF;
      if (itsInUnit.empty()) itsInUnit = "m";
    } else {
      if (itsRefType == MPosition::N_Types) itsRefType = MPosition::WGS84;
      if (itsInUnit.empty()) itsInUnit = "rad";
    }
    if (itsRefType == MPosition::N_Types  ||
        !(itsValueType == 2  ||  itsValueType == 3)) {
      throw AipsError ("A position in a MEAS function is given "
                       "without a valid reference type or suffix or unit");
    }
  }

  void PositionEngine::handleScalars (TableExprNodeRep* e1,
                                      TableExprNodeRep* e2,
                                      TableExprNodeRep* heightNode)
  {
    if (! (e1->isConstant()  &&  e2->isConstant())  ||
        (heightNode  &&
         ! (heightNode->isConstant()  &&
            heightNode->valueType() == TableExprNodeRep::VTScalar))) {
      throw AipsError ("Scalar values given as position a MEAS function "
                       "must be constant values");
    }
    makeDefaults (e1->unit());
    double vh = 0;
    double v1 = e1->getDouble(0);
    double v2 = e2->getDouble(0);
    Unit uh("m");
    Unit u1 = e1->unit();
    Unit u2 = e2->unit();
    if (u1.empty()) u1 = itsInUnit;
    if (u2.empty()) u2 = itsInUnit;
    itsConstants.resize (IPosition(1,1));
    if (heightNode) {
      vh = heightNode->getDouble(0);
      uh = heightNode->unit();
      if (uh.empty()) uh = "m";
    }
    itsConstants.data()[0] = makePosition(Quantity(vh, uh),
                                          Quantity(v1, u1),
                                          Quantity(v2, u2));
  }

  MPosition PositionEngine::makePosition (const Quantity& qh,
                                          const Quantity& q1,
                                          const Quantity& q2) const
  {
    if (itsValueType == 3) {
      Unit m("m");
      return MPosition (MVPosition(q1.getValue(m), q2.getValue(m),
                                   qh.getValue(m)), itsRefType);
    }
    return MPosition (qh, q1, q2, itsRefType);
  }

  void PositionEngine::handleObservatory (TableExprNodeRep* operand)
  {
    // For the time being the observatory names have to be constants.
    // In the future, it could be a table column.
    if (! operand->isConstant()) {
      throw AipsError ("An observatory name used as position in a MEAS function"
                       " must be a constant string");
    }
    Array<String> names = operand->getStringAS(0);
    itsConstants.resize (names.shape());
    for (uInt i=0; i<names.size(); ++i) {
      if (! MeasTable::Observatory (itsConstants.data()[i], names.data()[i])) {
        throw AipsError ("Observatory '" + names.data()[i] + "' used as a"
                         " position in a MEAS function is unknown");
      }
    }
  }

  void PositionEngine::handlePosArray (TableExprNodeRep*& operand)
  {
    if (operand->dataType() != TableExprNodeRep::NTDouble  ||
        operand->valueType() != TableExprNodeRep::VTArray) {
      throw AipsError ("A single double argument given as position in a "
                       "MEAS function must be a double array of values "
                       "defining x,y,z or lon,lat");
    }
    // Use defaults for reference and value type if not given.
    makeDefaults (operand->unit());
    // Set or convert the operands's unit to radian or meter.
    TableExprNodeUnit::adaptUnit (operand, itsInUnit);
    // Handle possibly given constants.
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
        ArrayMeasColumn<MPosition> measTmp(tabCol.table(),
                                           tabCol.columnDesc().name());
        // Get and check the node's refType if it is fixed.
        MPosition::Types nodeRefType = MPosition::N_Types;
        if (! (measTmp.measDesc().isRefCodeVariable()  ||
               measTmp.measDesc().hasOffset())) {
          uInt refCode = measTmp.measDesc().getRefCode();
          nodeRefType = static_cast<MPosition::Types>(refCode);
          if (itsRefType != MPosition::N_Types  &&  nodeRefType != itsRefType) {
            throw AipsError ("Given MPosition reference type " +
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
        if (nodeRefType == MPosition::N_Types) {
            throw AipsError ("Column " + tabCol.columnDesc().name() +
                             ", which has a variable reference frame, "
                             "is used in a MEAS function with slicing. "
                             "The slicing should be done after the function "
                             "like 'meas.pos('ITRF',POSITION)[0:3]'");
        }
      }
    }
    if (itsMeasCol.isNull()) {
      if (itsRefType == MPosition::N_Types) {
        throw AipsError("No reference type given for a non-constant MEAS "
                        "function position argument");
      }
      itsExprNode = operand;
    }
  }

  void PositionEngine::handlePosArray (TableExprNodeRep* anglesNode,
                                       TableExprNodeRep* heightNode)
  {
    if (anglesNode->dataType() != TableExprNodeRep::NTDouble  ||
        anglesNode->valueType() != TableExprNodeRep::VTArray  ||
        !anglesNode->isConstant()  ||
        heightNode->dataType() != TableExprNodeRep::NTDouble  ||
        heightNode->valueType() != TableExprNodeRep::VTArray  ||
        !heightNode->isConstant()) {
      throw AipsError ("Positions given as angles,heights in a MEAS "
                       "function must be constant double arrays of values");
    }
    if (itsValueType == 3) {
      throw AipsError ("Position reference type suffix in a MEAS function is "
                       "given as xyz, while heights are used");
    }
    Array<Double> angles = anglesNode->getArrayDouble(0);
    if (angles.size() %2 != 0) {
      throw AipsError ("Angles given as position in a MEAS function must "
                       "be a constant double array of multiple of 2 values");
    }
    Array<Double> height = heightNode->getArrayDouble(0);
    if (angles.size() != 2*height.size()) {
      throw AipsError ("Angles and heights given as position in a MEAS "
                       "function have mismatching sizes");
    }
    // Set unit and reference type is undefined.
    Unit aUnit = anglesNode->unit();
    Unit hUnit = heightNode->unit();
    if (aUnit.empty()) aUnit = "rad";
    if (hUnit.empty()) hUnit = "m";
    Vector<Double> aVec(angles.reform(IPosition(1,angles.size())));
    Vector<Double> hVec(height.reform(IPosition(1,height.size())));
    if (itsRefType == MPosition::N_Types) {
      itsRefType = MPosition::WGS84;
    }
    itsConstants.resize (IPosition(1, hVec.size()));
    for (uInt i=0; i<hVec.size(); ++i) {
      itsConstants.data()[i] = MPosition (Quantity(hVec[i], hUnit),
                                          Quantity(aVec[2*i], aUnit),
                                          Quantity(aVec[2*i+1], aUnit),
                                          itsRefType);
    }
  }

  void PositionEngine::handleConstant (TableExprNodeRep* operand)
  {
    AlwaysAssert (operand->valueType() != TableExprNodeRep::VTSet, AipsError);
    TableExprNode node(operand);
    handleValues (node, 0, itsConstants);
  }

  void PositionEngine::handleValues (TableExprNode& operand,
                                     const TableExprId& id,
                                     Array<MPosition>& positions)
  {
    Array<Double> values = operand.getArrayDouble(id);
    const IPosition& shape = values.shape();
    if (shape[0] % itsValueType != 0) {
      throw AipsError ("Number of values in a position in a MEAS function "
                       "should be a multiple of " +
                       String::toString(itsValueType));
    }
    IPosition posShape;
    if (shape[0] == itsValueType  &&  shape.size() > 1) {
      posShape = shape.getLast (shape.size() - 1);
    } else {
      posShape = shape;
      posShape[0] /= itsValueType;
    }
    positions.resize (posShape);
    Quantity qh(0, itsInUnit);
    Quantity q1(0, itsInUnit);
    Quantity q2(0, itsInUnit);
    if (itsValueType != 3) {
      qh = Quantity(0, "m");
    }
    Double* dirVec = values.data();
    MPosition* posVec = positions.data();
    for (uInt i=0; i<positions.size(); ++i) {
      q1.setValue (dirVec[i*itsValueType]);
      q2.setValue (dirVec[i*itsValueType+1]);
      if (itsValueType == 3) {
        qh.setValue (dirVec[i*itsValueType+2]);
      }
      posVec[i] = makePosition(qh, q1, q2);
    }
  }

  Array<MPosition> PositionEngine::getPositions (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasCol.isNull()) {
      return itsMeasCol(id.rownr());
    }
    // Read from expression.
    Array<MPosition> pos;
    handleValues (itsExprNode, id, pos);
    return pos;
  }

  Array<Double> PositionEngine::getArrayDouble (const TableExprId& id,
                                                MPosition::Types toRefType,
                                                Int toValueType)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MPosition> res;
    if (! itsMeasCol.isNull()) {
      res = itsMeasCol.convert (id.rownr(), toRefType);
    } else {
      res.resize (itsConstants.shape());
      for (uInt i=0; i<res.size(); ++i) {
        res.data()[i] = MPosition::Convert (itsConstants.data()[i],
                                            toRefType)();
      }
    }
    Array<Double> out;
    if (res.size() > 0) {
      if (toValueType == 1) {
        out.resize (res.shape());
        Array<MPosition>::const_contiter resIter = res.cbegin();
        for (uInt i=0; i<res.size(); ++i, ++resIter) {
          // Get as height.
          out.data()[i] = resIter->getValue().getLength().getValue();
        }
      } else {
        IPosition shape(1,3);
        if (toValueType == 2) {
          shape[0] = 2;
        }
        if (res.size() > 1) {
          shape.append (res.shape());
        }
        out.resize (shape);
        VectorIterator<Double> outIter(out);
        for (Array<MPosition>::const_contiter resIter = res.cbegin();
             !outIter.pastEnd(); outIter.next(), ++resIter) {
          if (toValueType == 3) {
            // Get as xyz.
            outIter.vector() = resIter->getValue().getValue();
          } else {
            // Get as lon,lat.
            outIter.vector() = resIter->getValue().getAngle().getValue();
          }
        }
      }
    }
    return out;
  }

} //end namespace
