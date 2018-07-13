//# EarthMagneticUDF.cc: TaQL UDF for EarthMagnetic conversions
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

#include <casacore/meas/MeasUDF/EarthMagneticUDF.h>

namespace casacore {

  EarthMagneticUDF::EarthMagneticUDF (FuncType type)
    : itsType      (type),
      itsValueType (0),
      itsRefType   (MEarthMagnetic::ITRF)
  {}

  UDFBase* EarthMagneticUDF::makeEMXYZ (const String&)
    { return new EarthMagneticUDF (EMXYZ); }
  UDFBase* EarthMagneticUDF::makeEMANG (const String&)
    { return new EarthMagneticUDF (EMANG); }
  UDFBase* EarthMagneticUDF::makeEMLEN (const String&)
    { return new EarthMagneticUDF (EMLEN); }
  UDFBase* EarthMagneticUDF::makeIGRFXYZ (const String&)
    { return new EarthMagneticUDF (IGRFXYZ); }
  UDFBase* EarthMagneticUDF::makeIGRFANG (const String&)
    { return new EarthMagneticUDF (IGRFANG); }
  UDFBase* EarthMagneticUDF::makeIGRFLEN (const String&)
    { return new EarthMagneticUDF (IGRFLEN); }
  UDFBase* EarthMagneticUDF::makeIGRFLOS (const String&)
    { return new EarthMagneticUDF (IGRFLOS); }
  UDFBase* EarthMagneticUDF::makeIGRFLONG (const String&)
    { return new EarthMagneticUDF (IGRFLONG); }

  void EarthMagneticUDF::setup (const Table&, const TaQLStyle&)
  {
    if (operands().size() < 1) {
      throw AipsError ("No arguments given in a MEAS.EM function");
    }
    // Get the 'to' value type.
    // Determine the argnr of the earthmagnetic/direction values.
    uInt argnr    = 0;
    Bool asLOS    = False;
    Bool asLong   = False;
    Bool useModel = False;
    if (itsType == EMXYZ) {
      itsValueType = 3;
    } else if (itsType == EMANG) {
      itsValueType = 2;
    } else if (itsType == EMLEN) {
      itsValueType = 1;
    } else if (itsType == IGRFXYZ) {
      itsValueType = 3;
      useModel     = True;
    } else if (itsType == IGRFANG) {
      itsValueType = 2;
      useModel     = True;
    } else if (itsType == IGRFLEN) {
      itsValueType = 1;
      useModel     = True;
    } else if (itsType == IGRFLOS) {
      itsValueType = 1;
      asLOS        = True;
      useModel     = True;
    } else if (itsType == IGRFLONG) {
      itsValueType = 1;
      asLong       = True;
      useModel     = True;
    }
    // Get the to reference type.
    // IGRF means calculating the model and returning in ITRF coordinates.
    if (itsEngine.handleMeasType (operands()[0], False)) {
      itsRefType = itsEngine.refType();
      if (itsRefType == MEarthMagnetic::IGRF) {
        throw AipsError("IGRF cannot be used as to type; "
                        "use function IGRFxxx instead");
      }
      argnr = 1;
      if (itsEngine.valueType() != 0) {
        if (itsValueType != 0) {
          throw AipsError("Valuetype should be given once; "
                          "not in function name and reference type");
        }
        itsValueType = itsEngine.valueType();
      }
    }
    if (itsValueType == 0) {
      itsValueType = 3;        // default is XYZ
    }
    // Get the value arguments.
    if (useModel) {
      // Heights and directions must be given.
      if (operands().size() <= argnr+1) {
        throw AipsError ("No heights and directions given in a MEAS IGRF "
                         "function");
      }
      itsEngine.handleHeight (operands()[argnr]);
      argnr++;
      itsDirectionEngine.handleDirection (operands(), argnr, False, False);
      itsEngine.setDirectionEngine (itsDirectionEngine);
    } else {
      // No model, thus conversions of earthmagnetic values.
      if (operands().size() <= argnr) {
        throw AipsError ("No values given in a MEAS EarthMagnetic function");
      }
      itsEngine.handleEarthMagnetic (operands(), argnr);
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
    if (operands().size() > argnr) {
      throw AipsError ("Too many arguments given in a MEAS EarthMagnetic "
                       "function");
    }
    itsEngine.set (itsRefType, itsValueType, asLOS, asLong, useModel);
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
    setAttributes (itsEngine.makeAttributes (itsRefType, itsValueType));
  }

  Double EarthMagneticUDF::getDouble (const TableExprId& id)
  {
    return itsEngine.getArrayDouble (id).data()[0];
  }

  MArray<Double> EarthMagneticUDF::getArrayDouble (const TableExprId& id)
  {
    return MArray<Double>(itsEngine.getArrayDouble (id));
  }

} //end namespace
