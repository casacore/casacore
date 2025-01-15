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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/meas/MeasUDF/DirectionEngine.h>
#include <casacore/meas/MeasUDF/EpochEngine.h>
#include <casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>

namespace casacore {

  DirectionEngine::DirectionEngine()
    : itsEpochEngine    (0),
      itsPositionEngine (0)
  {}

  DirectionEngine::~DirectionEngine()
  {}

  void DirectionEngine::handleDirection (const vector<TENShPtr>& args,
                                         uInt& argnr, Bool riseSet,
                                         Bool asDirCos)
  {
    // Initialize to unknown reference type.
    itsRefType = MDirection::N_Types;
    // Normally directions must be given in an array, but a single one
    // can be 2 or 3 scalars.
    uInt nargnr = argnr+1;
    Bool asScalar = False;
    TENShPtr scalar3;
    // A string means that object names (e.g. MOON) are given.
    if (args[argnr]->dataType() == TableExprNodeRep::NTString) {
      handleNames (args[argnr]);
    } else {
      if (! args[argnr]->isReal()) {
        throw AipsError("Invalid direction given in a MEAS function");
      }
      if (args.size() > nargnr  &&
          args[argnr]->isReal()  &&
          args[argnr]->valueType() == TableExprNodeRep::VTScalar  &&
          args[nargnr]->isReal()  &&
          args[nargnr]->valueType() == TableExprNodeRep::VTScalar) {
        asScalar = True;
        nargnr++;
        // See if given as 3 scalars xyz (direction cosines).
        if (args.size() > nargnr  &&
            args[nargnr]->isReal()  &&
            args[nargnr]->valueType() == TableExprNodeRep::VTScalar) {
          scalar3 = args[nargnr];
          nargnr++;
        }
      }
      // See if a reference type is given.
      if (args.size() > nargnr  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTString) {
        if (handleMeasType (args[nargnr], False)) {
          nargnr++;
        }
      }
      // Process as scalars or as array.
      if (asScalar) {
        handleScalars (args[argnr], args[argnr+1], scalar3);
      } else {
        handleMeasArray (args[argnr]);
        if (itsMeasArrCol.isNull()) {
          // Set or convert the operand's unit to radian.
          TENShPtr operand(args[argnr]);
          TableExprNodeUnit::adaptUnit (operand, "rad");
          itsExprNode = operand;
        }
      }
    }
    // Skip the arguments handled.
    argnr = nargnr;
    // Set shape, etc. for constants.
    adaptForConstant (itsConstants.shape(), asDirCos ? 3:2);
    // Determine the output unit, shape, and ndim.
    if (riseSet) {
      itsOutUnit = "d";
    } else if (!asDirCos) {
      itsOutUnit = "rad";
    }
  }

  void DirectionEngine::handleScalars (const TENShPtr& e1,
                                       const TENShPtr& e2,
                                       const TENShPtr& e3)
  {
    if (! (e1->isConstant()  &&  e2->isConstant())  ||
        (e3  &&  !e3->isConstant())) {
      throw AipsError ("Scalar values given as direction in a MEAS function "
                       "must be constant values");
    }
    double v1 = e1->getDouble(0);
    double v2 = e2->getDouble(0);
    double v3 = 0;
    Unit u1 = e1->unit();
    Unit u2 = e2->unit();
    if (e3) {
      v3 = e3->getDouble(0);
      if (! (u1.empty()  &&  u2.empty()  &&  e3->unit().empty())) {
        throw AipsError ("Directions given as x,y,z in a MEAS function "
                         "cannot have units");
      }
    } else {
      if (u1.empty()) u1 = "rad";
      if (u2.empty()) u2 = "rad";
    }
    if (itsRefType == MDirection::N_Types) {
      itsRefType = MDirection::J2000;             // default reftype
    }
    itsConstants.resize (IPosition(1,1));
    if (e3) {
      itsConstants.data()[0] = MDirection(MVDirection(v1, v2, v3),
                                          itsRefType);
    } else {
      itsConstants.data()[0] = MDirection(Quantity(v1, u1),
                                          Quantity(v2, u2),
                                          itsRefType);
    }
  }

  void DirectionEngine::handleNames (const TENShPtr& operand)
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
        // Determine which part of the sun has to be used for rise/set times.
        // The offset in itsH is first given in degrees.
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
      itsH[i] *= M_PI/180.;
      itsConstants.data()[i] = MDirection::makeMDirection (name);
    }
  }

  void DirectionEngine::handleValues (TableExprNode& operand,
                                      const TableExprId& id,
                                      Array<MDirection>& directions)
  {
    Array<Double> values = operand.getArrayDouble(id);
    IPosition shape = values.shape();
    int nrv = 0;
    Unit unit(operand.unit());
    if (shape[0] % 2 == 0) {
      nrv = 2;
      if (unit.empty()) {
        unit = "rad";
      }
    } else if (shape[0] % 3 == 0) {
      nrv = 3;
      if (! unit.empty()) {
        throw AipsError ("Directions given as x,y,z in a MEAS function "
                         "cannot have units");
      }
    } else {
      throw AipsError ("Number of values in a direction in a MEAS function "
                       "should be a multiple of 2 or 3");
    }
    IPosition dirShape;
    if (shape[0] == nrv  &&  shape.size() > 1) {
      dirShape = shape.getLast (shape.size() - 1);
    } else {
      dirShape = shape;
      dirShape[0] /= nrv;
    }
    directions.resize (dirShape);
    Quantity q1(0, unit);
    Quantity q2(0, unit);
    Bool delIt;
    const Double* valVec = values.getStorage (delIt);
    MDirection* dirVec = directions.data();
    for (uInt i=0; i<directions.size(); ++i) {
      if (nrv == 2) {
        q1.setValue (valVec[i*2]);
        q2.setValue (valVec[i*2+1]);
        dirVec[i] = MDirection(q1, q2, itsRefType);
      } else {
        dirVec[i] = MDirection(MVDirection(valVec[i*3], valVec[i*3+1],
                                           valVec[i*3+2]),
                               itsRefType);
      }
    }
    values.freeStorage (valVec, delIt);
  }

  void DirectionEngine::setEpochEngine (EpochEngine& engine)
  {
    AlwaysAssert (itsEpochEngine == 0, AipsError);
    itsEpochEngine = &engine;
    extendBase (engine, False);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MEpoch());
  }

  void DirectionEngine::setPositionEngine (PositionEngine& engine)
  {
    AlwaysAssert (itsPositionEngine == 0, AipsError);
    itsPositionEngine = &engine;
    extendBase (engine, True);
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
    if (!itsMeasArrCol.isNull()) {
      return itsMeasArrCol(id.rownr());
    }
    Array<MDirection> directions;
    handleValues (itsExprNode, id, directions);
    return directions;
  }

  Array<Double> DirectionEngine::getArrayDouble (const TableExprId& id,
                                                 Bool riseSet, Bool asDirCos)
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
      // 2 or 3 values per MDirection
      IPosition shape(1, asDirCos ? 3:2);
      // Only add the other axes if one of them has multiple values.
      if (res.size() > 1  ||  eps.size() > 1  ||  pos.size() > 1) {
        shape.append (res.shape());
        shape.append (eps.shape());
        shape.append (pos.shape());
      }
      out.resize (shape);
      double* outPtr = out.data();
      for (Array<MPosition>::const_contiter posIter = pos.cbegin();
           posIter != pos.cend(); ++posIter) {
        // Convert to desired position.
        if (itsPositionEngine) {
          itsFrame.resetPosition (*posIter);
        }
        for (Array<MEpoch>::const_contiter epsIter = eps.cbegin();
           epsIter != eps.cend(); ++epsIter) {
          // Convert to desired epoch.
          if (itsEpochEngine) {
            itsFrame.resetEpoch (*epsIter);
          }
          uInt hIndex = 0;
          for (Array<MDirection>::const_contiter resIter = res.cbegin();
               resIter != res.cend(); ++resIter, ++hIndex) {
            if (riseSet) {
              calcRiseSet (*resIter, *posIter, *epsIter,
                           (hIndex<itsH.size() ? itsH[hIndex] : 0),
                           outPtr[0], outPtr[1]);
              outPtr += 2;
            } else {
              itsConverter.setModel (*resIter);
              MDirection mdir = itsConverter();
              if (asDirCos) {
                // Get direction cosines.
                Vector<Double> md (mdir.getValue().getValue());
                *outPtr++ = md[0];
                *outPtr++ = md[1];
                *outPtr++ = md[2];
              } else {
                // Get angles as radians.
                Vector<Double> md (mdir.getValue().get());
                *outPtr++ = md[0];
                *outPtr++ = md[1];
              }
            }
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
