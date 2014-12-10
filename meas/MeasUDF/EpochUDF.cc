//# EpochUDF.cc: TaQL UDF for Epoch conversions
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

#include <casacore/meas/MeasUDF/EpochUDF.h>

namespace casacore {

  EpochUDF::EpochUDF (FuncType type)
    : itsType  (type)
  {}

  UDFBase* EpochUDF::makeEPOCH (const String&)
    { return new EpochUDF (EPOCH); }
  UDFBase* EpochUDF::makeLAST (const String&)
    { return new EpochUDF (LAST); }

  void EpochUDF::setup (const Table&, const TaQLStyle&)
  {
    if (operands().size() < 1) {
      throw AipsError ("No arguments given in a MEAS function");
    }
    // Get the 'to' reference type.
    // Determine the argnr of the epoch.
    uInt argnr = 0;
    if (itsType == LAST) {
      itsRefType = MEpoch::LAST;
    } else {
      itsEngine.handleEpochType (operands()[0], True);
      itsRefType = itsEngine.refType();
      argnr = 1;
    }
    // Get the epochs.
    if (operands().size() <= argnr) {
      throw AipsError ("No epoch given in a MEAS function");
    }
    itsEngine.handleEpoch (operands(), argnr);
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

  Double EpochUDF::getDouble (const TableExprId& id)
  {
    return itsEngine.getArrayDouble (id).data()[0];
  }

  Array<Double> EpochUDF::getArrayDouble (const TableExprId& id)
  {
    return itsEngine.getArrayDouble (id);
  }

} //end namespace
