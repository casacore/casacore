//# FrequencyUDF.cc: TaQL UDF for Frequency conversions
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

#include <casacore/meas/MeasUDF/FrequencyUDF.h>

namespace casacore {

  FrequencyUDF::FrequencyUDF (FuncType type)
    : itsType    (type),
      itsRefType (MFrequency::DEFAULT)
  {}

  UDFBase* FrequencyUDF::makeFREQ (const String&)
    { return new FrequencyUDF (FREQUENCY); }
  UDFBase* FrequencyUDF::makeREST (const String&)
    { return new FrequencyUDF (REST); }
  UDFBase* FrequencyUDF::makeSHIFT (const String&)
    { return new FrequencyUDF (SHIFT); }

  void FrequencyUDF::setup (const Table&, const TaQLStyle&)
  {
    if (operands().size() < 1) {
      throw AipsError ("No arguments given in a MEAS function");
    }
    // Get the 'to' reference type.
    // Determine the argnr of the epoch.
    Bool needRadVel = False;
    uInt argnr = 0;
    if (itsType == REST) {
      itsRefType = MFrequency::REST;
    } else if (itsType != SHIFT) {
      // A to-type has to be given if not shifting frequencies.
      if (itsEngine.handleMeasType (operands()[0], True)) {
        itsRefType = itsEngine.refType();
        argnr = 1;
        if (itsRefType == MFrequency::REST) {
          itsType = REST;      // Conversion to REST
          needRadVel = True;
        }
      }
    }
    // Get the frequencies (and type).
    if (operands().size() <= argnr) {
      throw AipsError ("No frequency given in a MEAS function");
    }
    itsEngine.handleFrequency (operands(), argnr);
    if (itsEngine.refType() == MFrequency::REST) {
      needRadVel = True;
    }
    Bool useDoppler = False;
    if (itsType == SHIFT) {
      // For shift the output reftype is the same as the input.
      itsRefType = itsEngine.refType();
      // Only get the doppler if shifting.
      if (operands().size() > argnr) {
        itsDopplerEngine.handleDoppler (operands(), argnr, False, False);
        itsEngine.setDopplerEngine (itsDopplerEngine);
        useDoppler = True;
      } else {
        throw AipsError("No doppler given in function MEAS.SHIFTFREQ");
      }
    } else if (itsType == REST  ||  needRadVel) {
      // Get the mandatory radial velocity or doppler for rest frequencies.
      useDoppler = handleRadVelDoppler (argnr, needRadVel);
    }
    // Get all possible other arguments if doppler is not used.
    if (! useDoppler) {
      // Handle possible Direction arguments.
      if (operands().size() > argnr) {
        itsDirectionEngine.handleDirection (operands(), argnr, False, False);
        itsEngine.setDirectionEngine (itsDirectionEngine);
      }
      // Handle possible Epoch arguments.
      if (operands().size() > argnr) {
        itsEpochEngine.handleEpoch (operands(), argnr);
        itsEngine.setEpochEngine (itsEpochEngine);
      }
      // Handle possible Position arguments.
      if (operands().size() > argnr) {
        itsPositionEngine.handlePosition (0, operands(), argnr);
        itsEngine.setPositionEngine (itsPositionEngine);
      }
    }
    if (operands().size() > argnr) {
      throw AipsError ("Too many arguments given in a MEAS function");
    }
    if (itsType != SHIFT) {
      itsEngine.setConverter (itsRefType);
    }
    // Set datatype, shape, unit, etc.
    setDataType (TableExprNodeRep::NTDouble);
    const IPosition& shape = itsEngine.shape();
    if (shape.size() > 0) {
      if (shape.product() == 1) {
        setNDim (0);                  // scalar
      } else {
        setShape (shape);
      }
    } else {
      setNDim (itsEngine.ndim());
    }
    setUnit (itsEngine.unit().getName());
    setConstant (itsEngine.isConstant());
    setAttributes (itsEngine.makeAttributes (itsRefType));
  }

  Bool FrequencyUDF::handleRadVelDoppler (uInt& argnr, Bool mustRadVel)
  {
    // In the REST function a radial velocity or doppler can be used.
    // They can be distinguished by unit or type, so the velocity unit
    // and/or a type is required for radial velocity.
    if (operands().size() > argnr) {
      uInt argnrOld = argnr;
      if (! operands()[argnr]->unit().empty()) {
        try {
          itsRadVelEngine.handleRadialVelocity (operands(), argnr);
          itsEngine.setRadVelEngine (itsRadVelEngine);
          return False;
        } catch (const AipsError&) {
        }
      }
      if (! mustRadVel) {
        argnr = argnrOld;
        try {
          itsDopplerEngine.handleDoppler (operands(), argnr, False, False);
          itsEngine.setDopplerEngine (itsDopplerEngine);
          return True;
        } catch (const AipsError&) {
        }
      }
    }
    if (mustRadVel) {
      throw AipsError("No radial velocity given in MEAS REST conversion");
    }
    throw AipsError("No radial velocity nor doppler given in MEAS.REST function");
  }

  Double FrequencyUDF::getDouble (const TableExprId& id)
  {
    return getArrayDouble(id).array().data()[0];
  }

  MArray<Double> FrequencyUDF::getArrayDouble (const TableExprId& id)
  {
    return MArray<Double>(itsEngine.getArrayDouble (id, itsType));
  }

} //end namespace
