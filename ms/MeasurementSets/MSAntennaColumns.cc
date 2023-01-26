//# MSAntennaColumns.cc:  provides easy access to MeasurementSet columns
//# Copyright (C) 1996,1999,2000
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

#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/ms/MeasurementSets/MSAntenna.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

MSAntennaColumns::MSAntennaColumns()
{
}

MSAntennaColumns::MSAntennaColumns(const MSAntenna& msAntenna)
{
  attach(msAntenna);
}

MSAntennaColumns::~MSAntennaColumns() {}

void MSAntennaColumns::attach(const MSAntenna& msAntenna)
{
  dishDiameter_p.attach(msAntenna, MSAntenna::
			columnName(MSAntenna::DISH_DIAMETER));
  flagRow_p.attach(msAntenna, MSAntenna::
		   columnName(MSAntenna::FLAG_ROW));
  mount_p.attach(msAntenna, MSAntenna::columnName(MSAntenna::MOUNT));
  name_p.attach(msAntenna, MSAntenna::columnName(MSAntenna::NAME));
  offset_p.attach(msAntenna, MSAntenna::columnName(MSAntenna::OFFSET));
  position_p.attach(msAntenna, MSAntenna::
		    columnName(MSAntenna::POSITION));
  station_p.attach(msAntenna, MSAntenna::columnName(MSAntenna::STATION));
  type_p.attach(msAntenna, MSAntenna::columnName(MSAntenna::TYPE));
  offsetMeas_p.attach(msAntenna, MSAntenna::
		      columnName(MSAntenna::OFFSET));
  positionMeas_p.attach(msAntenna, MSAntenna::
			columnName(MSAntenna::POSITION));
  dishDiameterQuant_p.attach(msAntenna, MSAntenna::
			     columnName(MSAntenna::DISH_DIAMETER));
  offsetQuant_p.attach(msAntenna, MSAntenna::
		       columnName(MSAntenna::OFFSET));
  positionQuant_p.attach(msAntenna, MSAntenna::
			 columnName(MSAntenna::POSITION));
  attachOptionalCols(msAntenna);
}

void MSAntennaColumns::attachOptionalCols(const MSAntenna& msAntenna)
{
  const ColumnDescSet& cds=msAntenna.tableDesc().columnDescSet();
  const String& meanOrbit=MSAntenna::columnName(MSAntenna::MEAN_ORBIT);
  if (cds.isDefined(meanOrbit)) meanOrbit_p.attach(msAntenna,meanOrbit);
  const String& orbitId=MSAntenna::columnName(MSAntenna::ORBIT_ID);
  if (cds.isDefined(orbitId)) orbitId_p.attach(msAntenna,orbitId);
  const String& phasedArrayId=MSAntenna::
    columnName(MSAntenna::PHASED_ARRAY_ID);
  if (cds.isDefined(phasedArrayId)) {
    phasedArrayId_p.attach(msAntenna, phasedArrayId);
  }
}


void MSAntennaColumns::setPositionRef(MPosition::Types ref)
{
  positionMeas_p.setDescRefCode(ref);
}

void MSAntennaColumns::setOffsetRef(MPosition::Types ref) 
{
  offsetMeas_p.setDescRefCode(ref);
}


int64_t MSAntennaColumns::
matchAntenna(const MPosition& antennaPos, const Quantum<double>& tolerance,
	     int64_t tryRow) {
  rownr_t r = nrow();
  if (r == 0) return -1;
  // Convert the antenna position to something in m.
  const MPosition::Types refType =
    MPosition::castType(antennaPos.getRef().getType());
  // If the type does not match then throw an exception! If someone is trying
  // to do this then they should be doing the conversions elsewhere and the
  // sooner they know about this error the better.
  if (MPosition::castType(positionMeas().getMeasRef().getType()) != refType) {
    throw(AipsError("MSAntennaColumns::matchAntenna(...) - "
		    " cannot match when reference frames differ"));
  }
  // Convert the tolerance to meters
  const Unit m("m");
  DebugAssert(tolerance.check(m.getValue()), AipsError);
  const double tolInM = tolerance.getValue(m);
  // Convert the position to meters
  const Vector<double>& antPosInM = antennaPos.getValue().getValue();
  // Main matching loop
  if (tryRow >= 0) {
    const rownr_t tr = tryRow;
    if (tr >= r) {
      throw(AipsError("MSAntennaColumns::matchAntenna(...) - "
                      "row " + String::toString(tr) +
                      " you suggest is too big"));
    }
    if (!flagRow()(tr) &&
	matchPosition(tr, antPosInM, tolInM)) {
      return tr;
    }
    if (tr == r-1) r--;
  }
  while (r > 0) {
    r--;
    if (!flagRow()(r) &&
	matchPosition(r, antPosInM, tolInM)) {
      return r;
    }
  }
  return -1;
}

int64_t MSAntennaColumns::matchAntenna(const String& antName,
                                     const MPosition& antennaPos,
                                     const Quantum<double>& tolerance,
                                     int64_t tryRow) {
  return matchAntennaAndStation(antName, "",
				antennaPos, tolerance, tryRow);

}

int64_t MSAntennaColumns::matchAntennaAndStation(const String& antName,
                                               const String& stationName,
                                               const MPosition& antennaPos,
                                               const Quantum<double>& tolerance,
                                               int64_t tryRow) {
  rownr_t r = nrow();
  if (r == 0) return -1;
  // Convert the antenna position to something in m.
  const MPosition::Types refType =
    MPosition::castType(antennaPos.getRef().getType());
  // If the type does not match then throw an exception! If someone is trying
  // to do this then they should be doing the conversions elsewhere and the
  // sooner they know about this error the better.
  if (MPosition::castType(positionMeas().getMeasRef().getType()) != refType) {
    throw(AipsError("MSAntennaColumns::matchAntenna(...) - "
		    " cannot match when reference frames differ"));
  }
  // Convert the tolerance to meters
  const Unit m("m");
  DebugAssert(tolerance.check(m.getValue()), AipsError);
  const double tolInM = tolerance.getValue(m);
  // Convert the position to meters
  const Vector<double>& antPosInM = antennaPos.getValue().getValue();

  // Main matching loop
  if (tryRow >= 0) {
    const rownr_t tr = tryRow;
    if (tr >= r) {
      throw(AipsError("MSAntennaColumns::matchAntenna(...) - "
                      "row " + String::toString(tr) +
                      " you suggest is too big"));
    }
    bool stationMatches = stationName.empty() || matchStation(tr, stationName);
    if (!flagRow()(tr) &&
	stationMatches &&
	matchName(tr, antName) &&
	matchPosition(tr, antPosInM, tolInM)) {
      return tr;
    }
    if (tr == r-1) r--;
  }
  while (r > 0) {
    r--;
    bool stationMatches = stationName.empty() || matchStation(r, stationName);
    if (!flagRow()(r) &&
	stationMatches &&
	matchName(r, antName) &&
	matchPosition(r, antPosInM, tolInM)) {
      return r;
    }
  }
  return -1;
}


bool MSAntennaColumns::matchName(rownr_t row, const String& antName) const {
  DebugAssert(row < nrow(), AipsError);
  return antName.matches(name()(row));
}

bool MSAntennaColumns::matchStation(rownr_t row, const String& stationName) const {
  DebugAssert(row < nrow(), AipsError);
  return stationName.matches(station()(row));
}

bool MSAntennaColumns::
matchPosition(rownr_t row, const Vector<double>& antPosInM,
	      const double tolInM) const {
  DebugAssert(row < nrow(), AipsError);
  DebugAssert(antPosInM.nelements() == 3, AipsError);
  return allNearAbs(position()(row), antPosInM, tolInM);
}

} //# NAMESPACE CASACORE - END

