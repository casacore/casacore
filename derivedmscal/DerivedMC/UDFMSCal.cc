//# UDFMSCal.cc: TaQL UDF to calculate derived MS values
//# Copyright (C) 2010
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

#include <derivedmscal/DerivedMC/UDFMSCal.h>

namespace casa {

  UDFMSCal::UDFMSCal (ColType type, Int antnr)
    : itsType  (type),
      itsAntNr (antnr)
  {}

  UDFBase* UDFMSCal::makeHA (const String&)
    { return new UDFMSCal (HA, -1); }
  UDFBase* UDFMSCal::makeHA1 (const String&)
    { return new UDFMSCal (HA, 0); }
  UDFBase* UDFMSCal::makeHA2 (const String&)
    { return new UDFMSCal (HA, 1); }
  UDFBase* UDFMSCal::makePA1 (const String&)
    { return new UDFMSCal (PA, 0); }
  UDFBase* UDFMSCal::makePA2 (const String&)
    { return new UDFMSCal (PA, 1); }
  UDFBase* UDFMSCal::makeLAST (const String&)
    { return new UDFMSCal (LAST, -1); }
  UDFBase* UDFMSCal::makeLAST1 (const String&)
    { return new UDFMSCal (LAST, 0); }
  UDFBase* UDFMSCal::makeLAST2 (const String&)
    { return new UDFMSCal (LAST, 1); }
  UDFBase* UDFMSCal::makeAZEL1 (const String&)
    { return new UDFMSCal (AZEL, 0); }
  UDFBase* UDFMSCal::makeAZEL2 (const String&)
    { return new UDFMSCal (AZEL, 1); }
  UDFBase* UDFMSCal::makeUVW (const String&)
    { return new UDFMSCal (UVW, -1); }

  void UDFMSCal::setup (const Table& table, const TaQLStyle&)
  {
    if (table.isNull()) {
      throw AipsError ("UDFMSCal can only be used on a table");
    }
    itsEngine.setTable (table);
    AlwaysAssert (operands().size() == 0, AipsError);
    setDataType (TableExprNodeRep::NTDouble);
    switch (itsType) {
    case HA:
    case PA:
    case LAST:
      setNDim (0);
      break;
    case AZEL:
      setShape (IPosition(1,2));
      break;
    case UVW:
      setShape (IPosition(1,3));
      break;
    }
  }

  Double UDFMSCal::getDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case HA:
      return itsEngine.getHA (itsAntNr, id.rownr());
    case PA:
      return itsEngine.getPA (itsAntNr, id.rownr());
    case LAST:
      return itsEngine.getLAST (itsAntNr, id.rownr());
    default:
      throw AipsError ("UDFMSCal: unexpected getDouble column");
    }
  }

  Array<Double> UDFMSCal::getArrayDouble (const TableExprId& id)
  {
    DebugAssert (id.byRow(), AipsError);
    switch (itsType) {
    case AZEL:
      itsEngine.getAzEl (itsAntNr, id.rownr(), itsTmpAzEl);
      return itsTmpAzEl;
    case UVW:
      itsEngine.getUVWJ2000 (id.rownr(), itsTmpUVW);
      return itsTmpUVW;
    default:
      throw AipsError ("UDFMSCal: unexpected getArrayDouble column");
    }
  }

} //end namespace
