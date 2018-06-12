//# DopplerUDF.cc: TaQL UDF for Doppler conversions
//# Copyright (C) 2018
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

#include <casacore/meas/MeasUDF/DopplerUDF.h>

namespace casacore {

  DopplerUDF::DopplerUDF()
  {}

  UDFBase* DopplerUDF::makeDOPPLER (const String&)
    { return new DopplerUDF(); }

  void DopplerUDF::setup (const Table&, const TaQLStyle&)
  {
    if (operands().size() < 1) {
      throw AipsError ("No arguments given in a MEAS.DOPPLER function");
    }
    // Get the 'to' reference type.
    itsEngine.handleMeasType (operands()[0], True);
    itsRefType = itsEngine.refType();
    uInt argnr = 1;
    if (operands().size() <= argnr) {
      throw AipsError ("No values given in a MEAS.DOPPLER function");
    }
    itsEngine.handleDoppler (operands(), argnr, True, True);
    if (operands().size() > argnr) {
      throw AipsError ("Too many arguments given in a MEAS.DOPPLER function");
    }
    itsEngine.setConverter (itsRefType);
    // Set datatype, shape, unit, etc.
    setDataType (TableExprNodeRep::NTDouble);
    IPosition shape (itsEngine.shape());
    if (shape.size() > 0) {
      if (shape.product() == 1) {
        setNDim (0);                  // scalar
      } else {
        setShape (shape);
      }
    } else {
      setNDim (itsEngine.ndim());
    }
    setConstant (itsEngine.isConstant());
    setAttributes (itsEngine.makeAttributes (itsRefType));
  }

  Double DopplerUDF::getDouble (const TableExprId& id)
  {
    return getArrayDouble(id).array().data()[0];
  }

  MArray<Double> DopplerUDF::getArrayDouble (const TableExprId& id)
  {
    return MArray<Double>(itsEngine.getArrayDouble (id));
  }

} //end namespace
