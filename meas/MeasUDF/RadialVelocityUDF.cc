//# RadialVelocityUDF.cc: TaQL UDF for RadialVelocity conversions
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

#include <casacore/meas/MeasUDF/RadialVelocityUDF.h>

namespace casacore {

  RadialVelocityUDF::RadialVelocityUDF()
  {}

  UDFBase* RadialVelocityUDF::makeRADVEL (const String&)
    { return new RadialVelocityUDF(); }

  void RadialVelocityUDF::setup (const Table&, const TaQLStyle&)
  {
    if (operands().size() < 1) {
      throw AipsError ("No arguments given in a MEAS.RADVEL function");
    }
    // Get the 'to' reference type.
    // Determine the argnr of the epoch.
    uint32_t argnr = 0;
    itsEngine.handleMeasType (operands()[0], true);
    itsRefType = itsEngine.refType();
    argnr = 1;
    // Get the radialVelocities.
    if (operands().size() <= argnr) {
      throw AipsError ("No radial velocity given in a MEAS.RADVEL function");
    }
    // First try if givben as doppler values.
    bool asDoppler = tryDoppler (argnr);
    // If not, it must be radialvelocity plus possibly frame info.
    if (! asDoppler) {
      itsEngine.handleRadialVelocity (operands(), argnr);
      // Handle possible Direction arguments.
      if (operands().size() > argnr) {
        itsDirectionEngine.handleDirection (operands(), argnr, false, false);
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
      throw AipsError ("Too many arguments given in a MEAS.RADVEL function");
    }
    itsEngine.setConverter (itsRefType);
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

  bool RadialVelocityUDF::tryDoppler (uint32_t& argnr)
  {
    // Try if a doppler value is given.
    // It is if no unit is given and a possible type is doppler.
    if (operands().size() > argnr  &&
        operands()[argnr]->unit().empty()) {
      uint32_t argnrOld = argnr;
      try {
        itsDopplerEngine.handleDoppler (operands(), argnr, false, false);
        itsEngine.setDopplerEngine (itsDopplerEngine);
        return true;
      } catch (const AipsError&) {
      }
      argnr = argnrOld;
    }
    return false;
  }

  double RadialVelocityUDF::getDouble (const TableExprId& id)
  {
    return getArrayDouble(id).array().data()[0];
  }

  MArray<double> RadialVelocityUDF::getArrayDouble (const TableExprId& id)
  {
    return MArray<double>(itsEngine.getArrayDouble (id));
  }

} //end namespace
