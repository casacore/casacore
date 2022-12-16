//# DopplerEngine.cc: Engine for TaQL UDF Doppler conversions
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

#include <casacore/meas/MeasUDF/DopplerEngine.h>
#include <casacore/meas/MeasUDF/RadialVelocityEngine.h>
#include <casacore/meas/MeasUDF/FrequencyEngine.h>
#include <casacore/measures/Measures/MeasTable.h>

namespace casacore {

  DopplerEngine::~DopplerEngine()
  {}

  void DopplerEngine::handleDoppler (vector<TENShPtr>& args,
                                     uInt& argnr,
                                     Bool allowRadVel, Bool allowFreq)
  {
    // Default type is RADIO.
    itsRefType = MDoppler::RADIO;
    // See if the values are given as radial velocity, frequency or doppler.
    // If given as radial velocity or frequency, a measure type must be given,
    // thus 2 arguments with the 2nd as a string.
    // TODO: use TaQL attributes. so a single argument is possible!!
    BaseEngine* enginePtr = 0;
    Bool restConst = True;
    if (args.size() >= argnr  &&  ! args[argnr]->unit().empty()) {
      if (allowRadVel) {
        try {
          itsRadVelEngine.reset (new RadialVelocityEngine());
          uInt nargnr = argnr;
          itsRadVelEngine->handleRadialVelocity (args, nargnr);
          if (nargnr > argnr) {
            itsType = RADVEL;
            argnr = nargnr;
            // No conversion.
            itsRadVelEngine->setConverter (itsRadVelEngine->refType());
            enginePtr = itsRadVelEngine.get();
          }
        } catch (const AipsError&) {
        }
      }
      if (enginePtr == 0  &&  allowFreq) {
        // No radial velocity; try as frequency (if allowed).
        try {
          itsFreqEngine.reset (new FrequencyEngine());
          uInt nargnr = argnr;
          itsFreqEngine->handleFrequency (args, nargnr);
          if (nargnr > argnr) {
            itsType = FREQ;
            argnr = nargnr;
            enginePtr = itsFreqEngine.get();
          }
        } catch (const AipsError&) {
        }
        // The rest frequency must be given hereafter.
        if (enginePtr) {
          handleRestFreq (args, argnr);
          restConst = ! itsConstRestFreqs.empty();
        }
      }
    }
    if (enginePtr) {
      itsShape = enginePtr->shape();
      itsNDim  = enginePtr->ndim();
      if (enginePtr->isConstant()  &&  restConst) {
        handleValues (itsExprNode, 0, itsConstants);
      }
    } else {
      // No match, so it must be a doppler value.
      itsType = DOPPLER;
      // Check if the value is a double without a unit.
      if (!args[argnr]->isReal()) {
        throw AipsError ("Invalid doppler value given in a MEAS function");
      }
      if (! args[argnr]->unit().empty()) {
        throw AipsError ("A doppler value given in a MEAS function cannot have a unit");
      }
      // Values can be given as [t1,t2,...],reftype
      uInt nargnr = argnr+1;
      // See if there is a reference type.
      if (args.size() > nargnr  &&
          args[nargnr]->dataType() == TableExprNodeRep::NTString) {
        if (handleMeasType (args[nargnr], True)) {
          nargnr++;
        }
      }
      handleMeasArray (args[argnr]);
      argnr = nargnr;
    }
    // Set the output shape, etc.
    adaptForConstant (itsConstants.shape());
  }

  void DopplerEngine::setConverter (MDoppler::Types toType)
  {
    MDoppler::Ref ref(toType);
    itsConverter = MDoppler::Convert (toType, ref);
  }

  Array<MDoppler> DopplerEngine::getDopplers (const TableExprId& id)
  {
    if (itsConstants.size() > 0) {
      return itsConstants;
    }
    if (!itsMeasScaCol.isNull()) {
      return Vector<MDoppler>(1, itsMeasScaCol(id.rownr()));
    } else if (!itsMeasArrCol.isNull()) {
      return itsMeasArrCol(id.rownr());
    }
    Array<MDoppler> dopplers;
    handleValues (itsExprNode, id, dopplers);
    return dopplers;
  }

  void DopplerEngine::handleRestFreq (vector<TENShPtr>& args, uInt& argnr)
  {
    if (args.size() <= argnr) {
      throw AipsError("No rest frequency given after the frequency in MEAS.DOPPLER");
    }
    itsRestFreqNode = args[argnr];
    if (itsRestFreqNode->dataType() == TableExprNodeRep::NTString) {
      // It is the name of a line.
      handleLine (itsRestFreqNode);
      argnr++;
    } else {
      if (! itsRestFreqNode->isReal()) {
        throw AipsError("Rest frequency in MEAS.DOPPLER does not have a real value");
      }
      if (! (itsRestFreqNode->unit().empty()  ||
             itsRestFreqNode->unit() == Unit("Hz"))) {
        throw AipsError("The rest frequency unit in MEAS.DOPPLER is invalid");
      }
      // Handle the rest frequency argument.
      argnr++;
      if (itsRestFreqNode->isConstant()) {
        itsConstRestFreqs = getRestFreqs (0);
      }
    }  
  }

  void DopplerEngine::handleLine (const TENShPtr& operand)
  {
    // For the time being the line names have to be constants.
    // In the future, it could be a table column.
    if (! operand->isConstant()) {
      throw AipsError ("A line name used as frequency in a MEAS function"
                       " must be a constant string");
    }
    Array<String> names = operand->getStringAS(0).array();
    itsConstRestFreqs.resize (names.shape());
    for (uInt i=0; i<names.size(); ++i) {
      MFrequency mfreq;
      if (! MeasTable::Line (mfreq, names.data()[i])) {
        throw AipsError ("Line '" + names.data()[i] + "' used as a"
                         " rest frequency in a MEAS function is unknown");
      }
      itsConstRestFreqs.data()[i] = mfreq.getValue();
    }
  }

  Array<MVFrequency> DopplerEngine::getRestFreqs (const TableExprId& id)
  {
    // Use the constant values if present.
    if (! itsConstRestFreqs.empty()) {
      return itsConstRestFreqs;
    }
    // Get the values.
    Array<Double> values = itsRestFreqNode->getDoubleAS(id).array();
    // Turn them into to MVFrequencies.
    Array<MVFrequency> freqs(values.shape());
    Unit unit = itsRestFreqNode->unit();
    if (unit.empty()) {
      unit = "Hz";
    }
    Quantity q(0, unit);
    for (uInt i=0; i<values.size(); ++i) {
      q.setValue (values.data()[i]);
      freqs.data()[i] = MVFrequency(q);
    }
    return freqs;
  }
  
  void DopplerEngine::handleValues (TableExprNode& operand,
                                    const TableExprId& id,
                                    Array<MDoppler>& dopplers)
  {
    if (itsType == DOPPLER) {
      Array<Double> values = operand.getDoubleAS(id).array();
      dopplers.resize (values.shape());
      Quantity q(0, itsInUnit);
      for (uInt i=0; i<values.size(); ++i) {
        q.setValue (values.data()[i]);
        dopplers.data()[i] = MDoppler(q, itsRefType);
      }
    } else if (itsType == RADVEL) {
      Array<MRadialVelocity> vels = itsRadVelEngine->getRadialVelocities (id);
      dopplers.resize (vels.shape());
      for (uInt i=0; i<vels.size(); ++i) {
        dopplers.data()[i] = vels.data()[i].toDoppler();
      }
    } else {
      Array<MFrequency> freqs = itsFreqEngine->getFrequencies (id);
      Array<MVFrequency> rests = getRestFreqs (id);
      uInt incf = 1;
      uInt incr = 1;
      uInt size = freqs.size();
      if (freqs.size() == 1) {
        incf = 0;
        size = rests.size();
        dopplers.resize (rests.shape());
      } else if (rests.size() == 1) {
        incr = 0;
        dopplers.resize (freqs.shape());
      } else if (freqs.shape().isEqual(rests.shape())) {
        dopplers.resize (freqs.shape());
      } else {
       throw AipsError("Frequencies and rest frequencies in MEAS.DOPPLER must have same shape");
      }
      uInt inxf = 0;
      uInt inxr = 0;
      for (uInt i=0; i<size; ++i, inxf+=incf, inxr+=incr) {
        dopplers.data()[i] = freqs.data()[inxf].toDoppler (rests.data()[inxr]);
      }
    }
  }

  Array<Double> DopplerEngine::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    Array<MDoppler> res (getDopplers(id));
    // Convert the doppler to the given type.
    Array<Double> out;
    if (res.size() > 0) {
      IPosition shape = res.shape();
      out.resize (shape);
      double* outPtr = out.data();
      for (Array<MDoppler>::const_contiter resIter = res.cbegin();
           resIter != res.cend(); ++resIter) {
        itsConverter.setModel (*resIter);
        MDoppler mf = itsConverter();
        *outPtr++ = mf.getValue().getValue();
      }
    }
    return out;
  }

} //end namespace
