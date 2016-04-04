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

#include <casacore/meas/MeasUDF/DirectionEngine.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
//#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/casa/Arrays/ArrayUtil.h>

namespace casacore {

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
                                         uInt& argnr, Bool riseSet)
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
        throw AipsError("Invalid direction given in a MEAS function");
      }
      if (args.size() > nargnr  &&
          args[argnr]->isReal()  &&
          args[argnr]->valueType() == TableExprNodeRep::VTScalar  &&
          args[nargnr]->isReal()  &&
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
    if (riseSet) {
      itsUnit = "d";
    } else {
      itsUnit = "rad";
    }
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
      throw AipsError ("Scalar values given as direction in a MEAS function "
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
    itsH.resize (names.size());
    for (uInt i=0; i<names.size(); ++i) {
      String name(names.data()[i]);
      name.upcase();
      itsH[i] = 0;
      if (name.substr(0,3) == "SUN") {
        String ext(name.substr(3));
        name = "SUN";
        itsH[i] = -0.833;      // default is -UR
        if (! ext.empty()) {
          if (ext == "-C") {
            itsH[i] = 0;       // centre touches horizon
          } else if (ext == "-U") {
            itsH[i] = -0.25;   // upper edge touches horizon
          } else if (ext == "-L") {
            itsH[i] = 0.25;    // lower edge touches horizon
          } else if (ext == "-CR") {
            itsH[i] = -0.583;  // centre touches horizon (with refraction)
          } else if (ext == "-UR") {
            itsH[i] = -0.833;  // upper edge touches horizon (with refraction)
          } else if (ext == "-LR") {
            itsH[i] = -0.333;  // lower edge touches horizon (with refraction)
          } else if (ext == "-CT") {
            itsH[i] = -6;      // civil twilight darkness
          } else if (ext == "-NT") {
            itsH[i] = -12;     // nautical twilight darkness
          } else if (ext == "-AT") {
            itsH[i] = -15;     // amateur astronomy twilight darkness
          } else if (ext == "-ST") {
            itsH[i] = -18;     // scientific astronomy twilight darkness
          } else {
            throw AipsError("invalid SUN type; use -C, -U, -L, -CR, -UR, -LR,"
                            " -CT, -NT, -AT, -ST");
          }
        }
      } else if (name.substr(0,4) == "MOON") {
        String ext(name.substr(4));
        name = "MOON";
        itsH[i] = -0.833;     // default is -UR
        if (! ext.empty()) {
          if (ext == "-C") {
            itsH[i] = 0;      // centre
           } else if (ext == "-U") {
            itsH[i] = -0.25;  // upper edge touches horizon
          } else if (ext == "-L") {
            itsH[i] = 0.25;   // lower edge touches horizon
          } else if (ext == "-CR") {
            itsH[i] = -0.583;  // centre touches horizon (with refraction)
          } else if (ext == "-UR") {
            itsH[i] = -0.833;  // upper edge touches horizon (with refraction)
          } else if (ext == "-LR") {
            itsH[i] = -0.333;  // lower edge touches horizon (with refraction)
          } else {
            throw AipsError("invalid MOON type; use -C, -U, -L, -CR, -UR, -LR");
          }
        } else {
          name = names.data()[i];   // keep original case
        }
      }
      itsH[i] *= C::pi/180.;
      itsConstants.data()[i] = MDirection::makeMDirection (name);
    }
  }

  void DirectionEngine::handleDirArray (TableExprNodeRep*& operand)
  {
    if (!operand->isReal()  ||
        operand->valueType() != TableExprNodeRep::VTArray) {
      throw AipsError ("A single double argument given as direction in a "
                       "MEAS function must be a double array of values");
    }
    // Set or convert the operand's unit to radian.
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
      const TableColumn& tabCol = colNode->getColumn();
      itsShape = tabCol.shapeColumn();
      itsNDim  = tabCol.ndimColumn();
      if (TableMeasDescBase::hasMeasures (tabCol)) {
        ArrayMeasColumn<MDirection> measTmp(tabCol.table(),
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
    Array<Double> values = operand.getArrayDouble(id);
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

  Array<Double> DirectionEngine::getArrayDouble (const TableExprId& id,
                                                 Bool riseSet)
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
      uInt hIndex = 0;
      for (Array<MDirection>::const_contiter resIter = res.cbegin();
           resIter != res.cend(); ++resIter, ++hIndex) {
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
            if (riseSet) {
              calcRiseSet (*resIter, *posIter, *epsIter,
                           (hIndex<itsH.size() ? itsH[hIndex] : 0),
                           outPtr[0], outPtr[1]);
            } else {
              MDirection mdir = itsConverter();
              // Get angles as radians.
              Vector<Double> md (mdir.getValue().get());
              outPtr[0] = md[0];
              outPtr[1] = md[1];
            }
            outPtr += 2;
          }
        }
      }
    }
    return out;
  }

  void DirectionEngine::calcRiseSet (const MDirection& dir,
                                     const MPosition& pos,
                                     const MEpoch& epoch,
                                     double h,
                                     double& rise, double& set)
  {
    // See http://www.stjarnhimlen.se/comp/riset.html
    double lat = pos.getValue().get()[2];            // latitude
    double start = floor(epoch.getValue().get() + 0.000001);
    // Start of day is the offset for rise and set.
    MEpoch off = MEpoch(Quantity(start, "d"),
                        MEpoch::Types(MEpoch::UTC | MEpoch::RAZE));
    // Use noon in the MeasFrame.
    // Note that for Sun and Moon an iteration can be done using the
    // obtained rise and set time as the new time in the MeasFrame,
    // which makes the calculation more accurate.
    int ab = fillRiseSet (start+0.5, dir, lat, h, off, &rise, &set);
    if (ab > 0) {
      // Always below.
      set = start;
      rise = set + 1;
    } else if (ab < 0) {
      // Always above.
      rise = start;
      set  = rise + 1;
    } else {
      // Note that sometimes Measures has to choose between 2 days
      // due to the 4 minutes difference between earth and sidereal day.
      // For the period between (about) 21-Mar and 21-Sep it chooses wrongly.
      // So adjust rise and set if needed (sidereal day is 236 sec shorter).
      if (rise < start) rise += 1 - 236./86400;
      if (set < start) set += 1 - 236./86400;
      // If set<rise, a planet rises in the evening; so adjust set.
      if (set < rise)  set += 1;
      // Iterate a few times for a better rise and set time.
      for (int i=0; i<2; ++i) {
        fillRiseSet (rise, dir, lat, h, off, &rise, 0);
        if (rise < start) rise += 1 - 236./86400;
        fillRiseSet (set,  dir, lat, h, off, 0, &set);
        if (set < start) set += 1 - 236./86400;
        if (set < rise)  set += 1;
      }
    }
  }

  int DirectionEngine::fillRiseSet (double epoch,
                                    const MDirection& dir,
                                    double lat,
                                    double h,
                                    const MEpoch& off,
                                    double* rise, double* set)
  {
    itsFrame.set (MEpoch(Quantity(epoch, "d"), MEpoch::UTC));
    MDirection::Ref ref2(MDirection::HADEC, itsFrame);
    MDirection hd = MDirection::Convert(MDirection::HADEC, ref2) (dir);
    double dec = hd.getValue().get()[1];
    double ct = (sin(h) - sin(dec)*sin(lat)) / (cos(dec)*cos(lat));
    if (ct >= 1) {
      return 1;
    } else if (ct <= -1) {
      return -1;
    }
    ct = acos(ct);
    // Get RA normalized between 0 and 2pi.
    MDirection::Ref ref1(MDirection::APP, itsFrame);
    MDirection app = MDirection::Convert(MDirection::APP, ref1) (dir);
    double normra = MVAngle(app.getValue().get()[0])(0).radian();
    MEpoch::Ref ref(MEpoch::LAST, itsFrame, off);
    if (rise) {
      double t = normra - ct;
      Quantity tq = MVTime(Quantity(t, "rad")).get();
      MEpoch tr = MEpoch::Convert (MEpoch(tq, ref), MEpoch::UTC)();
      *rise = tr.getValue().get();
    }
    if (set) {
      double t = normra + ct;
      Quantity tq = MVTime(Quantity(t, "rad")).get();
      MEpoch tr = MEpoch::Convert (MEpoch(tq, ref), MEpoch::UTC)();
      *set = tr.getValue().get();
    }
    return 0;
  }

  /*
# From old measures.g:

# Rise/set sidereal time(coord, elev)
#
ct = (sin(el=5deg) - sin(dec)*sin(lat)) / (cos(dec) * cos(lat))
rise = ra - acos(ct)
set  = ra + acos(ct)
    const public.rise := function(crd, ev='5deg') {
      if (!is_measure(crd)) fail('No rise/set coordinates specified');
      if (!is_measure(private.getwhere())) {
        dq.errorgui('Specify where you are in Frame');
        fail('No rise/set Frame->Where specified');
      };
      private.fillnow();
      hd := public.measure(crd, 'hadec');
      c := public.measure(crd, 'app');
      if (!is_measure(hd) || !is_measure(c)) fail('Cannot get HA for rise/set')\
;
      ps := private.getwhere();
      ct := dq.div(dq.sub(dq.sin(ev),
                          dq.mul(dq.sin(hd.m1),
                                 dq.sin(ps.m1))),
                   dq.mul(dq.cos(hd.m1), dq.cos(ps.m1)));
      if (ct.value >= 1) return "below below";
      if (ct.value <= -1) return "above above";
      a := dq.acos(ct);
      return [rise=dq.sub(dq.norm(c.m0, 0), a),
              set=dq.add(dq.norm(c.m0, 0), a)]
    }
#
# Rise/set times(coord, elev)
#
    const public.riseset := function(crd, ev='5deg') {
        a := public.rise(crd, ev);
        if (is_fail(a)) fail;
        if (is_string(a)) {
          return [solved=F,
                 rise=[last=a[1], utc=a[1]],        };
        x := a;
        ofe := public.measure(private.framestack['epoch'], 'utc');
        if (!is_measure(ofe)) ofe := public.epoch('utc', 'today');
        for (i in 1:2) {
          x[i] :=
              public.measure(public.epoch('last',
                                          dq.totime(a[i]),
                                          off=public.epoch('r_utc',
                                                           dq.add(ofe.m0,
                                                                  '0.5d'))),
                             'utc');
        };
        return [solved=T,
               rise=[last=public.epoch('last', dq.totime(a[1])),
                    utc=x[1]],
               set=[last=public.epoch('last', dq.totime(a[2])),
                   utc=x[2]]];
    }
  */

} //end namespace
