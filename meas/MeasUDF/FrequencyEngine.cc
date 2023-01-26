//# FrequencyEngine.cc: Engine for TaQL UDF Frequency conversions
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

#include <casacore/meas/MeasUDF/FrequencyEngine.h>
#include <casacore/meas/MeasUDF/EpochEngine.h>
#include <casacore/meas/MeasUDF/PositionEngine.h>
#include <casacore/meas/MeasUDF/DirectionEngine.h>
#include <casacore/meas/MeasUDF/DopplerEngine.h>
#include <casacore/meas/MeasUDF/RadialVelocityEngine.h>
#include <casacore/meas/MeasUDF/FrequencyUDF.h>
#include <casacore/measures/Measures/MeasTable.h>

namespace casacore {

  FrequencyEngine::FrequencyEngine()
    : itsEpochEngine    (0),
      itsPositionEngine (0),
      itsDirectionEngine(0),
      itsDopplerEngine  (0),
      itsRadVelEngine   (0)
  {}

  FrequencyEngine::~FrequencyEngine()
  {}

  void FrequencyEngine::handleFrequency (vector<TENShPtr>& args,
                                         uint32_t& argnr)
  {
    // Initialize type to unknown.
    itsRefType = MFrequency::N_Types;
    // Check if the value is a double.
    if (!args[argnr]->isReal()) {
      throw AipsError ("Invalid frequency given in a MEAS function");
    }
    // Values can be given as [t1,t2,...],reftype
    uint32_t nargnr = argnr+1;
    // See if there is a reference type.
    if (args.size() > nargnr  &&
        args[nargnr]->dataType() == TableExprNodeRep::NTString) {
      if (handleMeasType (args[nargnr], false)) {
        nargnr++;
      }
    }
    handleMeasArray (args[argnr]);
    // Skip the arguments handled.
    argnr = nargnr;
    // Determine the output unit, shape, and ndim.
    itsOutUnit = "Hz";
    adaptForConstant (itsConstants.shape());
  }


  void FrequencyEngine::handleValues (TableExprNode& operand,
                                      const TableExprId& id,
                                      Array<MFrequency>& frequencies)
  {
    Array<double> values = operand.getDoubleAS(id).array();
    const IPosition& shape = values.shape();
    frequencies.resize (shape);
    Unit unit = operand.unit();
    if (unit.empty()) {
      unit = "Hz";
    }
    Quantity q(0, unit);
    bool delIt;
    const double* valVec = values.getStorage (delIt);
    MFrequency* freqVec = frequencies.data();
    for (uint32_t i=0; i<frequencies.size(); ++i) {
      q.setValue (valVec[i]);
      freqVec[i] = MFrequency (q, itsRefType);
    }
    values.freeStorage (valVec, delIt);
  }

  void FrequencyEngine::setDopplerEngine (DopplerEngine& engine)
  {
    AlwaysAssert (itsDopplerEngine == 0, AipsError);
    itsDopplerEngine = &engine;
    extendBase (engine, false);
  }

  void FrequencyEngine::setRadVelEngine (RadialVelocityEngine& engine)
  {
    AlwaysAssert (itsRadVelEngine == 0, AipsError);
    itsRadVelEngine = &engine;
    extendBase (engine, false);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MRadialVelocity());
  }

  void FrequencyEngine::setDirectionEngine (DirectionEngine& engine)
  {
    AlwaysAssert (itsDirectionEngine == 0, AipsError);
    itsDirectionEngine = &engine;
    extendBase (engine, true);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MDirection());
    itsRVFrame.set (MDirection());
  }

  void FrequencyEngine::setEpochEngine (EpochEngine& engine)
  {
    AlwaysAssert (itsEpochEngine == 0, AipsError);
    itsEpochEngine = &engine;
    extendBase (engine, false);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MEpoch());
    itsRVFrame.set (MEpoch());
  }

  void FrequencyEngine::setPositionEngine (PositionEngine& engine)
  {
    AlwaysAssert (itsPositionEngine == 0, AipsError);
    itsPositionEngine = &engine;
    extendBase (engine, true);
    // Define the frame part, so it can be reset later.
    itsFrame.set (MPosition());
    itsRVFrame.set (MPosition());
  }

  void FrequencyEngine::setConverter (MFrequency::Types toType)
  {
    MFrequency::Ref ref(toType, itsFrame);
    itsConverter = MFrequency::Convert (toType, ref);
  }

  Array<MFrequency> FrequencyEngine::getFrequencies (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasScaCol.isNull()) {
      return Vector<MFrequency>(1, itsMeasScaCol(id.rownr()));
    } else if (!itsMeasArrCol.isNull()) {
      return itsMeasArrCol(id.rownr());
    }
    Array<MFrequency> freqs;
    handleValues (itsExprNode, id, freqs);
    return freqs;
  }

  Array<double> FrequencyEngine::getArrayDouble (const TableExprId& id, int type)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MFrequency> res (getFrequencies(id));
    // Get directions, epochs, positions, radvels and dopplers if given.
    Array<MDirection> dir(IPosition(1,1));
    Array<MEpoch> eps(IPosition(1,1));
    Array<MPosition> pos(IPosition(1,1));
    Array<MRadialVelocity> rv(IPosition(1,1));
    Array<MDoppler> dop(IPosition(1,1));
    if (itsDirectionEngine) {
      dir.reference (itsDirectionEngine->getDirections (id));
    }
    if (itsEpochEngine) {
      eps.reference (itsEpochEngine->getEpochs (id));
    }
    if (itsPositionEngine) {
      pos.reference (itsPositionEngine->getPositions (id));
    }
    if (itsRadVelEngine) {
      rv.reference (itsRadVelEngine->getRadialVelocities (id));
    }
    if (itsDopplerEngine) {
      dop.reference (itsDopplerEngine->getDopplers (id));
    }
    Array<double> out;
    if (! (res.empty()  ||  dir.empty()  ||  eps.empty()  ||
           pos.empty()  ||  rv.empty()   ||  dop.empty())) {
      IPosition shape;
      // Only add the other axes if one of them has multiple values.
      if (res.size() > 1  ||  dir.size() > 1  ||  eps.size() > 1  ||
          pos.size() > 1  ||  rv.size()  > 1  ||  dop.size() > 1) {
        shape.append (res.shape());
        if (itsDopplerEngine) {
          shape.append (dop.shape());
        } else {
          if (itsRadVelEngine) {
            shape.append (rv.shape());
          }
          shape.append (dir.shape());
          shape.append (eps.shape());
          shape.append (pos.shape());
        }
      } else {
        shape = IPosition(1,1);
      }
      // Convert the frequency to the given type for all radvel/doppler,dir,epoch,pos.
      out.resize (shape);
      double* outPtr = out.data();
      for (Array<MPosition>::const_contiter posIter = pos.cbegin();
           posIter != pos.cend(); ++posIter) {
        if (itsPositionEngine) {
          itsFrame.resetPosition (*posIter);
          itsRVFrame.resetPosition (*posIter);
        }
        for (Array<MEpoch>::const_contiter epsIter = eps.cbegin();
             epsIter != eps.cend(); ++epsIter) {
          if (itsEpochEngine) {
            itsFrame.resetEpoch (*epsIter);
            itsRVFrame.resetEpoch (*epsIter);
          }
          for (Array<MDirection>::const_contiter dirIter = dir.cbegin();
               dirIter != dir.cend(); ++dirIter) {
            if (itsDirectionEngine) {
              itsFrame.resetDirection (*dirIter);
              itsRVFrame.resetDirection (*dirIter);
            }
            for (Array<MRadialVelocity>::const_contiter rvIter = rv.cbegin();
                 rvIter != rv.cend(); ++rvIter) {
              if (itsRadVelEngine) {
                // The frame has to be set in the RadialVelocity.
                MeasRef<MRadialVelocity> mr(rvIter->getRef());
                mr.set (itsRVFrame);
                MRadialVelocity radvel(rvIter->getValue(), mr);
                itsFrame.resetRadialVelocity (radvel);
              }
              for (Array<MDoppler>::const_contiter dopIter = dop.cbegin();
                   dopIter != dop.cend(); ++dopIter) {
                for (Array<MFrequency>::const_contiter resIter = res.cbegin();
                     resIter != res.cend(); ++resIter) {
                  itsConverter.setModel (*resIter);
                  if (itsDopplerEngine) {
                    Vector<double> freqs(1, resIter->getValue().getValue());
                    if (type == FrequencyUDF::SHIFT) {
                      // Shift has to use a BETA Doppler, so convert.
                      // Note that it does the same as MFrequency::fromDoppler.
                      MDoppler tmp = MDoppler::Convert(*dopIter, MDoppler::BETA)();
                      *outPtr++ = tmp.shiftFrequency (freqs)[0];
                    } else {
                      *outPtr++ = resIter->toRest (*dopIter).getValue().getValue();
                    }
                  } else {
                    // Convert frequency to desired reference type.
                    MFrequency mf = itsConverter();
                    *outPtr++ = mf.getValue().getValue();
                  }
                }
              }
            }
          }
        }
      }
    }
    return out;
  }

} //end namespace
