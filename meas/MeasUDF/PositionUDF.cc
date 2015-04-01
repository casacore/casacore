//# PositionUDF.cc: TaQL UDF for Position conversions
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

#include <casacore/meas/MeasUDF/PositionUDF.h>

namespace casacore {

  PositionUDF::PositionUDF (FuncType type)
    : itsType  (type)
  {}

  UDFBase* PositionUDF::makePOS (const String&)
    { return new PositionUDF (POS); }
  UDFBase* PositionUDF::makeITRFXYZ (const String&)
    { return new PositionUDF (ITRFXYZ); }
  UDFBase* PositionUDF::makeITRFLL (const String&)
    { return new PositionUDF (ITRFLL); }
  UDFBase* PositionUDF::makeITRFH (const String&)
    { return new PositionUDF (ITRFH); }
  UDFBase* PositionUDF::makeWGSXYZ (const String&)
    { return new PositionUDF (WGSXYZ); }
  UDFBase* PositionUDF::makeWGSLL (const String&)
    { return new PositionUDF (WGSLL); }
  UDFBase* PositionUDF::makeWGSH (const String&)
    { return new PositionUDF (WGSH); }

  void PositionUDF::setup (const Table&, const TaQLStyle&)
  {
    if (operands().size() < 1) {
      throw AipsError ("No arguments given in MEAS.POS function");
    }
    // Get the reference and value type.
    // Determine the argnr of the position.
    uInt argnr = 0;
    if (itsType == ITRFXYZ) {
      itsRefType   = MPosition::ITRF;
      itsValueType = 3;
    } else if (itsType == ITRFLL) {
      itsRefType   = MPosition::ITRF;
      itsValueType = 2;
    } else if (itsType == ITRFH) {
      itsRefType   = MPosition::ITRF;
      itsValueType = 1;
    } else if (itsType == WGSXYZ) {
      itsRefType   = MPosition::WGS84;
      itsValueType = 3;
    } else if (itsType == WGSLL) {
      itsRefType   = MPosition::WGS84;
      itsValueType = 2;
    } else if (itsType == WGSH) {
      itsRefType   = MPosition::WGS84;
      itsValueType = 1;
    } else {
      itsEngine.handlePosType (operands()[0]);
      itsRefType   = itsEngine.refType();
      itsValueType = itsEngine.valueType();
      argnr = 1;
    }
    // Default value type is xyz.
    if (itsValueType == 0) {
      itsValueType = 3;
    }
    // Get the positions.
    if (operands().size() <= argnr) {
      throw AipsError ("No position given in MEAS.POS function");
    }
    itsEngine.handlePosition (itsValueType, operands(), argnr);
    if (operands().size() > argnr) {
      throw AipsError ("Too many arguments given in MEAS.POS function");
    }
    // Set datatype, shape, unit, etc.
    setDataType (TableExprNodeRep::NTDouble);
    const IPosition& shape = itsEngine.shape();
    if (shape.size() > 0) {
      if (shape.product() == 1) {
        setNDim (0);                  // scalar
      } else {
        setShape (itsEngine.shape());
      }
    } else {
      setNDim (itsEngine.ndim());
    }
    setUnit (itsEngine.unit().getName());
    setConstant (itsEngine.isConstant());
  }

  Double PositionUDF::getDouble (const TableExprId& id)
  {
    return itsEngine.getArrayDouble (id, itsRefType, itsValueType).data()[0];
  }

  Array<Double> PositionUDF::getArrayDouble (const TableExprId& id)
  {
    return itsEngine.getArrayDouble (id, itsRefType, itsValueType);
  }

} //end namespace
