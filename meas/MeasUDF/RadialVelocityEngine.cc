//# RadialVelocityEngine.cc: Engine for TaQL UDF RadialVelocity conversions
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/meas/MeasUDF/RadialVelocityEngine.h>
#include <casacore/meas/MeasUDF/DopplerEngine.h>
#include <casacore/meas/MeasUDF/EpochEngine.h>
#include <casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/meas/MeasUDF/DirectionEngine.h>
#include <casacore/tables/TaQL/ExprUnitNode.h>

namespace casacore {

  RadialVelocityEngine::RadialVelocityEngine()
    : itsDopplerEngine  (0),
      itsDirectionEngine(0),
      itsEpochEngine    (0),
      itsPositionEngine (0)
  {}

  void RadialVelocityEngine::handleRadialVelocity (vector<TENShPtr>& args,
                                                   uInt& argnr)
  {
    // Initialize type to unknown.
    itsFrame.set (MRadialVelocity());
    itsRefType = MRadialVelocity::N_Types;
    // Check if the value is a double.
    if (!args[argnr]->isReal()) {
      throw AipsError ("Invalid radial velocity given in a MEAS function");
    }
    // Values can be given as [t1,t2,...],reftype
    uInt nargnr = argnr+1;
    // See if there is a reference type.
    if (args.size() > nargnr  &&
        args[nargnr]->dataType() == TableExprNodeRep::NTString) {
      if (handleMeasType (args[nargnr], False)) {
        nargnr++;
      }
    }
    handleMeasArray (args[argnr]);
    // Skip the arguments handled.
    argnr = nargnr;
    // Determine the output unit, shape, and ndim.
    itsOutUnit = "km/s";
    adaptForConstant (itsConstants.shape());
  }

  void RadialVelocityEngine::handleValues (TableExprNode& operand,
                                           const TableExprId& id,
                                           Array<MRadialVelocity>& radVels)
  {
    // Use default reference type LSRK if not given.
    if (itsRefType == MRadialVelocity::N_Types) {
      itsRefType = MRadialVelocity::LSRK;
    }
    // Get values.
    if (itsDopplerEngine) {
      Array<MDoppler> dopplers = itsDopplerEngine->getDopplers(id);
      radVels.resize (dopplers.shape());
      Array<MDoppler>::const_iterator dopIter = dopplers.begin();
      MRadialVelocity* rvVec = radVels.data();
      for (uInt i=0; i<dopplers.size(); ++i, ++dopIter) {
        rvVec[i] = MRadialVelocity::fromDoppler (*dopIter, itsRefType);
      }
      return;
    }
    Array<double> values (operand.getDoubleAS(id).array());
    // Get unit (default km/s).
    Unit unit = operand.unit();
    if (unit.empty()) {
      unit = "km/s";
    }
    radVels.resize (values.shape());
    Quantity q(0, unit);
    Bool delIt;
    const Double* valVec = values.getStorage (delIt);
    MRadialVelocity* rvVec = radVels.data();
    for (uInt i=0; i<radVels.size(); ++i) {
      q.setValue (valVec[i]);
      rvVec[i] = MRadialVelocity(q, MRadialVelocity::Ref(itsRefType, itsFrame));
    }
    values.freeStorage (valVec, delIt);
  }

  void RadialVelocityEngine::setDopplerEngine (DopplerEngine& engine)
  {
    AlwaysAssert (itsDopplerEngine == 0, AipsError);
    itsDopplerEngine = &engine;
    if (engine.isConstant()) {
      handleValues (itsExprNode, 0, itsConstants);
      adaptForConstant (itsConstants.shape());
    } else {
      itsNDim  = engine.ndim();
      itsShape = engine.shape();
    }
    itsOutUnit = "km/s";
  }

  void RadialVelocityEngine::setDirectionEngine (DirectionEngine& engine)
  {
    AlwaysAssert (itsDirectionEngine == 0, AipsError);
    itsDirectionEngine = &engine;
    extendBase (engine, True);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MDirection());
  }

  void RadialVelocityEngine::setEpochEngine (EpochEngine& engine)
  {
    AlwaysAssert (itsEpochEngine == 0, AipsError);
    itsEpochEngine = &engine;
    extendBase (engine, False);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MEpoch());
  }

  void RadialVelocityEngine::setPositionEngine (PositionEngine& engine)
  {
    AlwaysAssert (itsPositionEngine == 0, AipsError);
    itsPositionEngine = &engine;
    extendBase (engine, True);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MPosition());
  }

  void RadialVelocityEngine::setConverter (MRadialVelocity::Types toType)
  {
    MRadialVelocity::Ref ref(toType, itsFrame);
    itsConverter = MRadialVelocity::Convert (toType, ref);
  }

  Array<MRadialVelocity> RadialVelocityEngine::getRadialVelocities (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasScaCol.isNull()) {
      return Vector<MRadialVelocity>(1, itsMeasScaCol(id.rownr()));
    } else if (!itsMeasArrCol.isNull()) {
      return itsMeasArrCol(id.rownr());
    }
    Array<MRadialVelocity> radVels;
    handleValues (itsExprNode, id, radVels);
    return radVels;
  }

  Array<Double> RadialVelocityEngine::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MRadialVelocity> res (getRadialVelocities(id));
    // Get directions, epochs and positions if given.
    Array<MDirection> dir(IPosition(1,1));
    Array<MEpoch> eps(IPosition(1,1));
    Array<MPosition> pos(IPosition(1,1));
    if (itsDirectionEngine) {
      dir.reference (itsDirectionEngine->getDirections (id));
    }
    if (itsEpochEngine) {
      eps.reference (itsEpochEngine->getEpochs (id));
    }
    if (itsPositionEngine) {
      pos.reference (itsPositionEngine->getPositions (id));
    }
    // Convert the radial velocity to the given type for all dir,epoch,pos.
    Array<Double> out;
    if (res.size() > 0  &&  dir.size() > 0  &&  eps.size() > 0  &&  pos.size() > 0) {
      IPosition shape = res.shape();
      // Only add the other axes if one of them has multiple values.
      if (dir.size() > 1  ||  eps.size() > 1  ||  pos.size() > 1) {
        shape.append (dir.shape());
        shape.append (eps.shape());
        shape.append (pos.shape());
      }
      out.resize (shape);
      double* outPtr = out.data();
      for (Array<MRadialVelocity>::const_contiter resIter = res.cbegin();
           resIter != res.cend(); ++resIter) {
        // The frame has to be set in the RadialVelocity.
        MeasRef<MRadialVelocity> mr(resIter->getRef());
        mr.set (itsFrame);
        MRadialVelocity radvel(resIter->getValue(), mr);
        itsConverter.setModel (radvel);
        for (Array<MDirection>::const_contiter dirIter = dir.cbegin();
             dirIter != dir.cend(); ++dirIter) {
          // Convert to desired position.
          if (itsDirectionEngine) {
            itsFrame.resetDirection (*dirIter);
          }
          for (Array<MEpoch>::const_contiter epsIter = eps.cbegin();
               epsIter != eps.cend(); ++epsIter) {
            // Convert to desired epoch.
            if (itsEpochEngine) {
              itsFrame.resetEpoch (*epsIter);
            }
            for (Array<MPosition>::const_contiter posIter = pos.cbegin();
                 posIter != pos.cend(); ++posIter) {
              // Convert to desired reference type.
              if (itsPositionEngine) {
                itsFrame.resetPosition (*posIter);
              }
              MRadialVelocity mf = itsConverter();
              *outPtr++ = mf.getValue().get("km/s").getValue();
            }
          }
        }
      }
    }
    return out;
  }

} //end namespace
