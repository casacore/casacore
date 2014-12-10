//# DirectionUDF.cc: TaQL UDF for Direction conversions
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

#include <casacore/meas/MeasUDF/DirectionUDF.h>

namespace casacore {

  DirectionUDF::DirectionUDF (FuncType type, Bool riseSet)
    : itsType    (type),
      itsRiseSet (riseSet)
  {}

  UDFBase* DirectionUDF::makeDIR (const String&)
    { return new DirectionUDF (DIRECTION); }
  UDFBase* DirectionUDF::makeHADEC (const String&)
    { return new DirectionUDF (HADEC); }
  UDFBase* DirectionUDF::makeAZEL (const String&)
    { return new DirectionUDF (AZEL); }
  UDFBase* DirectionUDF::makeAPP (const String&)
    { return new DirectionUDF (APP); }
  UDFBase* DirectionUDF::makeJ2000 (const String&)
    { return new DirectionUDF (J2000); }
  UDFBase* DirectionUDF::makeB1950 (const String&)
    { return new DirectionUDF (B1950); }
  UDFBase* DirectionUDF::makeECL (const String&)
    { return new DirectionUDF (ECLIPTIC); }
  UDFBase* DirectionUDF::makeGAL (const String&)
    { return new DirectionUDF (GALACTIC); }
  UDFBase* DirectionUDF::makeSGAL (const String&)
    { return new DirectionUDF (SUPERGALACTIC); }
  UDFBase* DirectionUDF::makeRISESET (const String&)
    { return new DirectionUDF (HADEC, True); }

  void DirectionUDF::setup (const Table&, const TaQLStyle&)
  {
    if (operands().size() < 1) {
      throw AipsError ("No arguments given in a MEAS function");
    }
    // Get the 'to' reference type.
    // Determine the argnr of the epoch.
    uInt argnr = 0;
    if (itsType == HADEC) {
      itsRefType = MDirection::HADEC;
    } else if (itsType == AZEL) {
      itsRefType = MDirection::AZEL;
    } else if (itsType == APP) {
      itsRefType = MDirection::APP;
    } else if (itsType == J2000) {
      itsRefType = MDirection::J2000;
    } else if (itsType == B1950) {
      itsRefType = MDirection::B1950;
    } else if (itsType == ECLIPTIC) {
      itsRefType = MDirection::ECLIPTIC;
    } else if (itsType == GALACTIC) {
      itsRefType = MDirection::GALACTIC;
    } else if (itsType == SUPERGALACTIC) {
      itsRefType = MDirection::SUPERGAL;
    } else {
      itsEngine.handleDirType (operands()[0]);
      itsRefType = itsEngine.refType();
      argnr = 1;
    }
    // Get the directions.
    if (operands().size() <= argnr) {
      throw AipsError ("No direction given in a MEAS function");
    }
    itsEngine.handleDirection (operands(), argnr, itsRiseSet);
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
    if (operands().size() > argnr) {
      throw AipsError ("Too many arguments given in a MEAS function");
    }
    itsEngine.setConverter (itsRefType);
    // Set datatype, shape, unit, etc.
    if (itsRiseSet) {
      setDataType (TableExprNodeRep::NTDate);
    } else {
      setDataType (TableExprNodeRep::NTDouble);
    }
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
  }

  Double DirectionUDF::getDouble (const TableExprId& id)
  {
    return getArrayDouble (id).data()[0];
  }

  Array<Double> DirectionUDF::getArrayDouble (const TableExprId& id)
  {
    return itsEngine.getArrayDouble (id, itsRiseSet);
  }
  Array<MVTime> DirectionUDF::getArrayDate (const TableExprId& id)
  {
    Array<Double> res = itsEngine.getArrayDouble (id, itsRiseSet);
    Array<MVTime> dates(res.shape());
    for (uInt i=0; i<res.size(); ++i) {
      dates.data()[i] = MVTime(res.data()[i]);
    }
    return dates;
  }

} //end namespace
