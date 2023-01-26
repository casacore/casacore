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

#include <casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/measures/Measures/MeasTable.h>

namespace casacore {

  PositionEngine::PositionEngine()
    : itsValueType (0)
  {}

  PositionEngine::~PositionEngine()
  {}

  void PositionEngine::handlePosition (int32_t toValueType,
                                       const std::vector<TENShPtr>& args,
                                       uint32_t& argnr)
  {
    // Handle the input position values and possibly type.
    // Set values to unknown, because they might have been set by
    // handleMeasType (called from PositionUDF).
    itsRefType    = MPosition::N_Types;
    itsInUnit     = "";
    itsValueType  = 0;
    uint32_t nargnr   = argnr+1;
    bool asScalar = false;
    if (args[argnr]->dataType() == TableExprNodeRep::NTString) {
      // Position is given by observatory name.
      handleObservatory (args[argnr]);
    } else {
      if (!args[argnr]->isReal()) {
        throw AipsError ("Invalid position given in a MEAS function");
      }
      // Normally positions must be given in an array, but a single constant
      // one can be 1, 2 or 3 scalars.
      TENShPtr node2;
      TENShPtr node3;
      if (args.size() > argnr  &&
          args[argnr]->isReal()  &&
          args[argnr]->valueType() == TableExprNodeRep::VTScalar) {
        asScalar = true;
        if (args.size() > nargnr  &&
            args[nargnr]->isReal()  &&
            args[nargnr]->valueType() == TableExprNodeRep::VTScalar) {
          node2 = args[nargnr];
          nargnr++;
        }
      }
      // See if there is a node giving height(s) (or z in case of scalars).
      if (args.size() > nargnr  &&  args[nargnr]->isReal()) {
        node3 = args[nargnr];
        nargnr++;
      }
      uint32_t nval = nargnr-argnr;
      // See if there is a reference type.
      if (args.size() > nargnr  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTString) {
        handleMeasType (args[nargnr], true);
        nargnr++;
      }
      // Process as scalars or as array.
      if (asScalar) {
        handleScalars (args[argnr], node2, node3, nval);
      } else {
        // Get the position arguments.
        if (node3) {
          // For time being, heights can only be given as constants.
          handlePosArray (args[argnr], node3);
        } else {
          // There is a single argument; handle it.
          handleMeasArray (args[argnr]);
        }
      }
    }
    // Skip the arguments handled.
    argnr = nargnr;
    // Determine the output unit, shape, and ndim.
    if (toValueType == 2) {
      itsOutUnit = "rad";          // angles
    } else if (toValueType > 0) {
      itsOutUnit = "m";            // xyz or height
    }
    adaptForConstant (itsConstants.shape(), abs(toValueType));
  }


  String PositionEngine::stripMeasType (const String& typex)
  {
    itsValueType = 0;
    String type(typex);
    unsigned lens = type.size();
    const char* suffices[] = {"XYZ", "LLH", "LL", "LONLAT", "H", "HEIGHT"};
    const char* units[]    = {"m",   "",    "rad","rad",    "m", "m"};
    int vtypes[]           = {3,     -3,    2,     2,       1,   1};
    for (unsigned i=0; i<sizeof(vtypes)/sizeof(int); ++i) {
      String suf(suffices[i]);
      if (lens > suf.size()  &&  type.substr(lens-suf.size()) == suf) {
        itsValueType = vtypes[i];
        itsInUnit = units[i];
        type = type.substr(0, lens-suf.size());
        break;
      }
    }
    return type;
  }

  void PositionEngine::deriveAttr (const Unit& unit, int32_t nval)
  {
    // This function checks and sets attributes.
    // There are two attributes (itsInUnit and itsValueType) which can be
    // defined or undefined. If defined, it is checked if an attribute
    // matches the other attribute. If undefined, it is set if possible.
    // First set itsValueType if needed.
    if (itsValueType == 0) {
      itsValueType = nval;
    } else if (itsValueType == 2  &&  nval == 3) {
      // If given reftype defines LL, accept an height as well.
      itsValueType = -3;
      itsInUnit    = "";
    } else if (nval != 0  &&  nval != abs(itsValueType)) {
      throw AipsError("The nr of position values in a MEAS function does not "
                      "match the reference type suffix");
    }
    // Set itsInUnit if a value unit is given. Check if it conforms.
    if (! unit.empty()) {
      if (! itsInUnit.empty()) {
        // Check if the unit given in a position value matches.
        if (itsInUnit != unit) {
          throw AipsError("Unit of a position in a MEAS function does not "
                          "match the reference type suffix");
        }
      }
      itsInUnit = unit;
    }
    // Derive itsValueType from unit if needed and possible.
    if (itsValueType == 0  &&  !itsInUnit.empty()) {
      if (itsInUnit == Unit("rad")) {
        itsValueType = 2;    // lon,lat
      } else if (itsInUnit == Unit("m")) {
        itsValueType = 3;    // xyz
      }
    }
    // Derive the unit from itsValueType if needed and possible.
    if (itsInUnit.empty()) {
      if (itsValueType == 2) {
        itsInUnit = "rad";       // must be lon,lat
      } else if (itsValueType > 0) {
        itsInUnit = "m";         // can be h or xyz
      }
    }
    // Check the nr of values against the unit.
    // Set nr of values if undefined.
    if (itsInUnit.empty()) {
      if (itsValueType < 0) {
        // Use rad for lon,lat; height is accessed differently.
        itsInUnit = "rad";
      }
    } else {
      if (itsValueType < 0) {
        throw AipsError("A position in a MEAS function given as llh "
                        "should not have a unit");
      }
      if (itsInUnit == Unit("m")) {
        if (itsValueType == 2) {
          throw AipsError("For unit m 1 or 3 position values should be given "
                          "in a MEAS function");
        }
      } else if (itsInUnit == Unit("rad")) {
        if (itsValueType != 2) {
          throw AipsError("For unit deg 2 position values should be given "
                          "in a MEAS function");
        }
      } else {
        throw AipsError ("Invalid unit given for a position value"
                         " in a MEAS function (no length or angle)");
      }
    }
    // Check if itsValueType is defined.
    if (itsValueType == 0) {
      throw AipsError("The value type of a position in a MEAS function is "
                      "unknown; use a proper unit or reference type suffix");
    }
    // Use default reference type if not given.
    if (itsRefType == MPosition::N_Types)  {
      if (itsValueType == 3) {
        itsRefType = MPosition::ITRF;
      } else {
        itsRefType = MPosition::WGS84;
      }
    }
  }

  void PositionEngine::setValueType (int32_t valueType)
  {
    itsValueType = valueType;
  }

  void PositionEngine::handleScalars (const TENShPtr& e1,
                                      const TENShPtr& e2,
                                      const TENShPtr& e3,
                                      int32_t nval)
  {
    if (! e1->isConstant()  ||
        (e2  &&  ! e2->isConstant())  ||
        (e3  &&  ! (e3->isConstant()  &&
                    e3->valueType() == TableExprNodeRep::VTScalar))) {
      throw AipsError ("Scalar values given as position in a MEAS function "
                       "must be constant values");
    }
    Unit unit = e1->unit();
    if (unit.empty()  &&  e2) {
      unit = e2->unit();
    }
    // Make sure llh is handled correctly if a unit is used.
    if (nval == 3  &&  itsValueType != 2  &&  unit == Unit("rad")) {
      itsValueType = -3;
      unit = "";
    }
    deriveAttr (unit, nval);
    double v1 = e1->getDouble(0);
    double v2 = 0;
    double v3 = 0;
    Unit u1 = e1->unit();
    Unit u2;
    Unit u3;
    if (e2) {
      v2 = e2->getDouble(0);
      u2 = e2->unit();
    }
    if (e3) {
      v3 = e3->getDouble(0);
      u3 = e3->unit();
    }
    if (u1.empty()) u1 = itsInUnit;
    if (u2.empty()) u2 = itsInUnit;
    if (u3.empty()) u3 = "m";
    itsConstants.resize (IPosition(1,1));
    itsConstants.data()[0] = makePosition(Quantity(v1, u1),
                                          Quantity(v2, u2),
                                          Quantity(v3, u3));
  }

  MPosition PositionEngine::makePosition (const Quantity& q1,
                                          const Quantity& q2,
                                          const Quantity& q3) const
  {
    if (itsValueType == 1) {
      // Height; towards pole.
      return MPosition (MVPosition(q1), itsRefType);
    } else if (itsValueType == 3) {
      // xyz.
      Unit m("m");
      return MPosition (MVPosition(q1.getValue(m), q2.getValue(m),
                                   q3.getValue(m)), itsRefType);
    }
    // height,lon,lat
    return MPosition (q3, q1, q2, itsRefType);
  }

  void PositionEngine::handleObservatory (const TENShPtr& operand)
  {
    // For the time being the observatory names have to be constants.
    // In the future, it could be a table column.
    if (! operand->isConstant()) {
      throw AipsError ("An observatory name used as position in a MEAS function"
                       " must be a constant string");
    }
    Array<String> names = operand->getStringAS(0).array();
    itsConstants.resize (names.shape());
    for (uint32_t i=0; i<names.size(); ++i) {
      if (! MeasTable::Observatory (itsConstants.data()[i], names.data()[i])) {
        throw AipsError ("Observatory '" + names.data()[i] + "' used as a"
                         " position in a MEAS function is unknown");
      }
    }
  }

  /*
  void PositionEngine::handlePosArray (const TENShPtr& operand)
  {
    if (!operand->isReal()  ||
        operand->valueType() != TableExprNodeRep::VTArray) {
      throw AipsError ("A single double argument given as position in a "
                       "MEAS function must be a double array of values "
                       "defining x,y,z or lon,lat or height");
    }
    // Use defaults for reference and value type if not given.
    deriveAttr (operand->unit(), 0);
    // Handle possibly given constants.
    if (operand->isConstant()) {
      handleConstant (operand);
      return;
    }
    // Try if the argument is a column.
    // If found, try to handle it as a TableMeasures column.
    const TableExprNodeArrayColumn* colNode =
      dynamic_cast<TableExprNodeArrayColumn*>(operand.get());
    bool directCol = true;
    if (!colNode) {
      // The node is an expression, not a column.
      directCol = false;
      // Try if the node is an array part of a column.
      TableExprNodeArrayPart* partNode =
        dynamic_cast<TableExprNodeArrayPart*>(operand.get());
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
          uint32_t refCode = measTmp.measDesc().getRefCode();
          itsRefType = static_cast<MPosition::Types>(refCode);
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
  */
  
  void PositionEngine::handlePosArray (const TENShPtr& anglesNode,
                                       const TENShPtr& heightNode)
  {
    if (!anglesNode->isReal()  ||
        anglesNode->valueType() != TableExprNodeRep::VTArray  ||
        !anglesNode->isConstant()  ||
        !heightNode->isReal()  ||
        heightNode->valueType() != TableExprNodeRep::VTArray  ||
        !heightNode->isConstant()) {
      throw AipsError ("Positions given as angles,heights in a MEAS "
                       "function must be constant double arrays of values");
    }
    if (itsValueType == 3) {
      throw AipsError ("Position reference type suffix in a MEAS function is "
                       "given as xyz, while heights are used");
    }
    Array<double> angles = anglesNode->getArrayDouble(0).array();
    if (angles.empty()  ||  angles.shape()[0] %2 != 0) {
      throw AipsError ("Angles given as position in a MEAS function must "
                       "be a constant double array of multiple of 2 values");
    }
    Array<double> height = heightNode->getArrayDouble(0).array();
    if (angles.size() != 2*height.size()) {
      throw AipsError ("Angles and heights given as position in a MEAS "
                       "function have mismatching sizes");
    }
    // Set unit and reference type is undefined.
    Unit aUnit = anglesNode->unit();
    Unit hUnit = heightNode->unit();
    if (aUnit.empty()) aUnit = "rad";
    if (hUnit.empty()) hUnit = "m";
    Vector<double> aVec(angles.reform(IPosition(1,angles.size())));
    Vector<double> hVec(height.reform(IPosition(1,height.size())));
    if (itsRefType == MPosition::N_Types) {
      itsRefType = MPosition::WGS84;
    }
    itsConstants.resize (height.shape());
    for (uint32_t i=0; i<hVec.size(); ++i) {
      itsConstants.data()[i] = MPosition (Quantity(hVec[i], hUnit),
                                          Quantity(aVec[2*i], aUnit),
                                          Quantity(aVec[2*i+1], aUnit),
                                          itsRefType);
    }
  }

  void PositionEngine::handleValues (TableExprNode& operand,
                                     const TableExprId& id,
                                     Array<MPosition>& positions)
  {
    Array<double> values = operand.getArrayDouble(id);
    uint32_t nrv = abs(itsValueType);
    const IPosition& shape = values.shape();
    if (shape[0] % nrv != 0) {
      throw AipsError ("Number of values in a position in a MEAS function "
                         "should be a multiple of " +
                       String::toString(nrv));
    }
    IPosition posShape;
    if (shape[0] == nrv  &&  shape.size() > 1) {
      posShape = shape.getLast (shape.size() - 1);
    } else {
      posShape = shape;
      posShape[0] /= nrv;
    }
    positions.resize (posShape);
    Quantity q1(0, itsInUnit);
    Quantity q2(0, itsInUnit);
    Quantity q3(0, itsInUnit);
    if (itsValueType != 1  &&  itsValueType != 3) {
      q3 = Quantity(0, "m");
    }
    bool delIt;
    const double* valVec = values.getStorage (delIt);
    MPosition* posVec = positions.data();
    for (uint32_t i=0; i<positions.size(); ++i) {
      q1.setValue (valVec[i*nrv]);
      if (nrv > 1) {
        q2.setValue (valVec[i*nrv+1]);
        if (nrv == 3) {
          q3.setValue (valVec[i*nrv+2]);
        }
      }
      posVec[i] = makePosition(q1, q2, q3);
    }
    values.freeStorage (valVec, delIt);
  }

  Array<MPosition> PositionEngine::getPositions (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasArrCol.isNull()) {
      return itsMeasArrCol(id.rownr());
    }
    // Read from expression.
    Array<MPosition> pos;
    handleValues (itsExprNode, id, pos);
    return pos;
  }

  Array<double> PositionEngine::getArrayDouble (const TableExprId& id,
                                                MPosition::Types toRefType,
                                                int32_t toValueType)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MPosition> res (getPositions(id));
    Array<double> out;
    if (res.size() > 0) {
      if (toValueType == 1) {
        out.resize (res.shape());
      } else {
        IPosition shape(1,3);
        if (toValueType == 2) {
          shape[0] = 2;
        }
        if (res.size() > 1) {
          shape.append (res.shape());
        }
        out.resize (shape);
      }
      VectorIterator<double> outIter(out);
      Array<MPosition>::const_contiter resIter = res.cbegin();
      for (uint32_t i=0; i<res.size(); ++i, ++resIter) {
        MPosition pos = MPosition::Convert (*resIter, toRefType)();
        if (toValueType == 1) {
          // Get as height.
          out.data()[i] = pos.getValue().getLength().getValue();
        } else if (toValueType == -3) {
          Vector<double> ang = pos.getValue().getAngle().getValue();
          out.data()[i*3]   = ang[0];
          out.data()[i*3+1] = ang[1];
          out.data()[i*3+2] = pos.getValue().getLength().getValue();;
        } else {
          if (toValueType == 3) {
            // Get as xyz.
            outIter.vector() = pos.getValue().getValue();
          } else if (toValueType == 2) {
            // Get as lon,lat.
            outIter.vector() = pos.getValue().getAngle().getValue();
          }
          outIter.next();
        }
      }
    }
    return out;
  }

} //end namespace
