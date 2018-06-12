//# EarthMagneticEngine.cc: Engine for TaQL UDF EarthMagnetic conversions
//# Copyright (C) 2016
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

#include <casacore/meas/MeasUDF/EarthMagneticEngine.h>
#include<casacore/meas/MeasUDF/EpochEngine.h>
#include<casacore/meas/MeasUDF/PositionEngine.h>
#include<casacore/meas/MeasUDF/DirectionEngine.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>

namespace casacore {

  EarthMagneticEngine::EarthMagneticEngine()
    : itsValueType       (0),
      itsToValueType     (0),
      itsAsLOS           (False),
      itsAsLong          (False),
      itsUseModel        (False),
      itsConvertModel    (False),
      itsEpochEngine     (0),
      itsPositionEngine  (0),
      itsDirectionEngine (0)
  {
    itsRefType = MEarthMagnetic::ITRF;
  }

  EarthMagneticEngine::~EarthMagneticEngine()
  {}
  
  void EarthMagneticEngine::handleEarthMagnetic (vector<TENShPtr>& args,
                                                 uInt& argnr)
  {
    // Types are unknown.
    itsRefType    = MEarthMagnetic::ITRF;
    itsValueType  = 0;
    uInt nargnr   = argnr+1;
    Bool asScalar = False;
    if (! args[argnr]->isReal()) {
      throw AipsError ("Non-real EarthMagnetic values given in a MEAS "
                       "function");
    }
    // Normally earthmagnetics must be given in an array, but a single one
    // can be 3 scalars.
    if (args.size() > nargnr+1  &&
        args[argnr]->isReal()  &&
        args[argnr]->valueType() == TableExprNodeRep::VTScalar  &&
        args[nargnr]->isReal()  &&
        args[nargnr]->valueType() == TableExprNodeRep::VTScalar  &&
        args[nargnr+1]->isReal()  &&
        args[nargnr+1]->valueType() == TableExprNodeRep::VTScalar) {
      asScalar = True;
      nargnr += 2;
    }
    // See if there is a reference type.
    if (args.size() > nargnr  &&
        args[nargnr]->dataType() == TableExprNodeRep::NTString) {
      handleMeasType (args[nargnr], False);
      if (itsRefType == MEarthMagnetic::IGRF) {
        throw AipsError ("The 'from' EarthMagnetic reference type given in "
                         "a MEAS function cannot be IGRF; "
                         "use function MEAS.IGRF instead");
      }
      nargnr++;
    }
    // Process as 3 scalars or as array.
    if (asScalar) {
      handleScalars (args[argnr], args[argnr+1], args[argnr+2]);
    } else {
      // Get the EarthMagnetic arguments.
      handleMeasArray (args[argnr]);
    }
    // Skip the arguments handled.
    argnr = nargnr;
  }

  void EarthMagneticEngine::handleHeight (TENShPtr& operand)
  {
    // Heights must be real values with an optional length unit.
    if (! operand->isReal()) {
      throw AipsError("Heights in a MEAS.IGRF function must be real values");
    }
    // Adapt units to meters.
    TableExprNodeUnit::adaptUnit (operand, "m");
    itsExprNode = operand;
    itsIsConst  = operand->isConstant();
  }

  String EarthMagneticEngine::stripMeasType (const String& typex)
  {
    itsValueType = 0;
    String type(typex);
    unsigned lens = type.size();
    const char* suffices[] = {"XYZ", "AL", "ANG", "ANGLES", "LEN", "LENGTH"};
    const char* units[]    = {"nT",  "",   "rad", "rad",    "nT",  "nT"};
    int vtypes[]           = {3,     -3,   2,     2,        1,     1};
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
  
  void EarthMagneticEngine::deriveAttr (const Unit& unit, Int)
  {
    // Check if the unit is length or angle.
    if (unit.empty()) {
      itsInUnit = "rad";
      itsValueType = -3;   // llh as angle and flux
    } else {
      itsInUnit = unit;
      Quantity q(1., itsInUnit);
      if (q.isConform ("T")) {
        itsValueType = 3;  // xyz in Tesla units
      } else if (q.isConform ("rad")) {
        itsValueType = -3; // llh as angle and flux
      } else {
        throw AipsError ("Invalid unit given for an EarthMagnetic value"
                         " in a MEAS function (no fluxdensity or angle)");
      }
    }
  }

  void EarthMagneticEngine::setValueType (Int valueType)
  {
    itsValueType = valueType;
  }

  void EarthMagneticEngine::handleScalars (const TENShPtr& e1,
                                           const TENShPtr& e2,
                                           const TENShPtr& e3)
  {
    if (! (e1->isConstant()  &&  e2->isConstant()  &&  e3->isConstant())) {
      throw AipsError ("Scalar values given as EarthMagnetic in a MEAS "
                       "function must be constant values");
    }
    deriveAttr (e1->unit(), 0);
    double v1 = e1->getDouble(0);
    double v2 = e2->getDouble(0);
    double vh = e3->getDouble(0);
    Unit u1 = e1->unit();
    Unit u2 = e2->unit();
    Unit uh = e3->unit();
    if (u1.empty()) u1 = itsInUnit;
    if (u2.empty()) u2 = itsInUnit;
    if (uh.empty()) uh = "nT";
    itsConstants.resize (IPosition(1,1));
    itsConstants.data()[0] = makeEarthMagnetic(Quantity(vh, uh),
                                               Quantity(v1, u1),
                                               Quantity(v2, u2));
  }

  MEarthMagnetic EarthMagneticEngine::makeEarthMagnetic (const Quantity& qh,
                                                         const Quantity& q1,
                                                         const Quantity& q2) const
  {
    if (itsValueType == 3) {
      Unit m("nT");
      return MEarthMagnetic (MVEarthMagnetic(q1.getValue(m), q2.getValue(m),
                                             qh.getValue(m)), itsRefType);
    }
    return MEarthMagnetic (MVEarthMagnetic(qh, q1, q2), itsRefType);
  }

  void EarthMagneticEngine::handleValues (TableExprNode& operand,
                                          const TableExprId& id,
                                          Array<MEarthMagnetic>& earthMagnetics)
  {
    Array<Double> values;
    values = operand.getArrayDouble(id);
    IPosition shape = values.shape();
    if (shape[0] % 3 != 0) {
      throw AipsError ("Number of values in an EarthMagnetic in a MEAS "
                       "function should be a multiple of 3");
    }
    IPosition emShape;
    if (shape[0] == 3  &&  shape.size() > 1) {
      emShape = shape.getLast (shape.size() - 1);
    } else {
      emShape = shape;
      emShape[0] /= 3;
    }
    earthMagnetics.resize (emShape);
    Quantity qh(0, itsInUnit);
    Quantity q1(0, itsInUnit);
    Quantity q2(0, itsInUnit);
    if (itsValueType == -3) {
      qh = Quantity(0, "nT");
    }
    Bool delIt;
    const Double* valVec = values.getStorage (delIt);
    MEarthMagnetic* emVec = earthMagnetics.data();
    for (uInt i=0; i<earthMagnetics.size(); ++i) {
      q1.setValue (valVec[i*3]);
      q2.setValue (valVec[i*3+1]);
      qh.setValue (valVec[i*3+2]);
      emVec[i] = makeEarthMagnetic(qh, q1, q2);
    }
    values.freeStorage (valVec, delIt);
  }

  void EarthMagneticEngine::setEpochEngine (EpochEngine& engine)
  {
    AlwaysAssert (itsEpochEngine == 0, AipsError);
    itsEpochEngine = &engine;
    extendBase (engine, False);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MEpoch());
  }

  void EarthMagneticEngine::setPositionEngine (PositionEngine& engine)
  {
    AlwaysAssert (itsPositionEngine == 0, AipsError);
    itsPositionEngine = &engine;
    extendBase (engine, True);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MPosition());
  }

  void EarthMagneticEngine::setDirectionEngine (DirectionEngine& engine)
  {
    AlwaysAssert (itsDirectionEngine == 0, AipsError);
    itsDirectionEngine = &engine;
    extendBase (engine, True);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MDirection());
  }

  void EarthMagneticEngine::set (MEarthMagnetic::Types toRefType,
                                 Int toValueType,
                                 Bool asLOS, Bool asLong, Bool useModel)
  {
    itsToValueType = toValueType;
    itsAsLOS       = asLOS;
    itsAsLong      = asLong;
    itsUseModel    = useModel;
    if (itsUseModel  &&  !itsAsLOS  &&  !itsAsLong  && 
        toRefType != MEarthMagnetic::ITRF) {
      // A model result needs to be converted from ITRF to another frame.
      itsConvertModel = True;
    }
    // Determine the output unit, shape, and ndim.
    itsOutUnit = "nT";
    if (itsToValueType == 2  ||  itsAsLong) {
      itsOutUnit = "rad";          // angles
    }
    adaptForConstant (itsConstants.shape(), abs(itsToValueType));
    // Set the to reference type in the converter.
    MEarthMagnetic::Ref ref(toRefType, itsFrame);
    itsConverter = MEarthMagnetic::Convert (toRefType, ref);
  }

  Array<MEarthMagnetic> EarthMagneticEngine::getEarthMagnetics (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasArrCol.isNull()) {
      return itsMeasArrCol(id.rownr());
    }
    // Read from expression.
    Array<MEarthMagnetic> earthMagnetics;
    handleValues (itsExprNode, id, earthMagnetics);
    return earthMagnetics;
  }

  Array<Double> EarthMagneticEngine::getHeights (const TableExprId& id)
  {
    return itsExprNode.getDoubleAS(id).array();
  }

  Array<Double> EarthMagneticEngine::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    // Get epochs and positions if given.
    Array<MEpoch> eps(IPosition(1,1));
    if (itsEpochEngine) {
      eps.reference (itsEpochEngine->getEpochs (id));
    }
    Array<MPosition> pos(IPosition(1,1));
    if (itsPositionEngine) {
      pos.reference (itsPositionEngine->getPositions (id));
    }
    // Get the ems or heights/directions and determine the shape from it.
    IPosition vshape;
    Array<MEarthMagnetic> ems;
    Array<MDirection>     dirs;
    Array<Double>         heights;
    const MEarthMagnetic* ePtr = 0;
    const MDirection*     dPtr = 0;
    const Double*         hPtr = 0;
    size_t nval1 = 1;
    size_t nval2 = 1;
    if (itsUseModel) {
      heights.reference (getHeights (id));
      if (! heights.contiguousStorage()) heights = heights.copy();
      hPtr   = heights.data();
      vshape = heights.shape();
      nval1  = heights.size();
      dirs.reference (itsDirectionEngine->getDirections (id));
      if (! dirs.contiguousStorage()) dirs = dirs.copy();
      dPtr  = dirs.data();
      nval2 = dirs.size();
      if (nval2 > 1) vshape.append (dirs.shape());
    } else {
      ems.reference (getEarthMagnetics(id));
      if (! ems.contiguousStorage()) ems = ems.copy();
      ePtr   = ems.data();
      vshape = ems.shape();
      nval2  = ems.size();
      cout<<"ems="<<ems<<endl;
    }
    // Convert the earthMagnetic to the given type for all values.
    Array<Double> out;
    if (nval1==0 || nval2==0 || eps.empty() || pos.empty()) {
      return out;
    }
    // Determine the output shape.
    IPosition shape;
    // 1, 2 or 3 values per MEarthMagnetic.
    if (itsToValueType > 1) {
      shape = IPosition(1, itsToValueType);   // multiple numbers per value
    }
    if (nval1 > 1  ||  nval2 > 1) {
      shape.append (vshape);
    }
    if (eps.size() > 1) {
      shape.append (eps.shape());
    }
    if (pos.size() > 1) {
      shape.append (pos.shape());
    }
    if (shape.empty()) {
      shape = IPosition(1,1);
    }
    out.resize (shape);
    double* outPtr = out.data();
    // Iterate over all input values.
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
        for (size_t i2=0; i2<nval2; ++i2) {
          if (itsUseModel) {
            EarthMagneticMachine emm(dPtr[i2].getRef(), dPtr[i2].getValue(),
                                     itsFrame);
            // Calculate the values for all heights.
            for (size_t i1=0; i1<nval1; ++i1) {
              emm.calculate (hPtr[i1]);
              if (itsConvertModel) {
                itsConverter.setModel (MEarthMagnetic(emm.getField(),
                                                      MEarthMagnetic::ITRF));
                
                MEarthMagnetic mem = itsConverter();
                copyEM (mem.getValue(), outPtr);
              } else {
                copyLLEM (emm, outPtr);
              }
            }
          } else {
            // Convert an em value.
            itsConverter.setModel (ePtr[i2]);
            MEarthMagnetic mem = itsConverter();
            copyEM (mem.getValue(), outPtr);
          }
        }
      }
    }
    return out;
  }
    
  void EarthMagneticEngine::copyEM (const MVEarthMagnetic& em,
                                    double*& outPtr)
  {
    if (itsToValueType == 1) {
      // Get as length.
      *outPtr++ = em.getLength().getValue();
    } else {
      if (itsToValueType == 3) {
        // Get as xyz in nT.
        Vector<double> vec = em.getValue();
        *outPtr++ = vec[0];
        *outPtr++ = vec[1];
        *outPtr++ = vec[2];
      } else {
        // Get as lon,lat.
        Vector<double> vec = em.getAngle().getValue();
        // Get angles as radians.
        *outPtr++ = vec[0];
        *outPtr++ = vec[1];
      }
    }
  }

  void EarthMagneticEngine::copyLLEM (EarthMagneticMachine& emm,
                                      double*& outPtr)
  {
    if (itsAsLOS == 1) {
      *outPtr++ = emm.getLOSField();
    } else if (itsAsLong) {
      *outPtr++ = emm.getLong();
    } else {
      // Get as EM field.
      copyEM (emm.getField(), outPtr);
    }
  }

} //end namespace
