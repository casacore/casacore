//# NewMSAntennaColumns.cc:  provides easy access to NewMeasurementSet columns
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
//#
//# $Id$

#include <aips/MeasurementSets/NewMSAntennaColumns.h>
#include <aips/MeasurementSets/NewMSAntenna.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>

#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MPosition.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/UnitVal.h>
#include <aips/Utilities/Assert.h>

RONewMSAntennaColumns::RONewMSAntennaColumns(const NewMSAntenna& msAntenna):
  dishDiameter_p(msAntenna, NewMSAntenna::
		 columnName(NewMSAntenna::DISH_DIAMETER)),
  flagRow_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::FLAG_ROW)),
  mount_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::MOUNT)),
  name_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::NAME)),
  offset_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  position_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  station_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::STATION)),
  type_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::TYPE)),
  meanOrbit_p(),
  orbitId_p(),
  phasedArrayId_p(),
  offsetMeas_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionMeas_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  dishDiameterQuant_p(msAntenna, NewMSAntenna::
		      columnName(NewMSAntenna::DISH_DIAMETER)),
  offsetQuant_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionQuant_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::POSITION))
{
  attachOptionalCols(msAntenna);
}

RONewMSAntennaColumns::~RONewMSAntennaColumns() {}

RONewMSAntennaColumns::RONewMSAntennaColumns():
  dishDiameter_p(),
  flagRow_p(),
  mount_p(),
  name_p(),
  offset_p(),
  position_p(),
  station_p(),
  type_p(),
  meanOrbit_p(),
  orbitId_p(),
  phasedArrayId_p(),
  offsetMeas_p(),
  positionMeas_p(),
  dishDiameterQuant_p(),
  offsetQuant_p(),
  positionQuant_p()
{
}

void RONewMSAntennaColumns::attach(const NewMSAntenna& msAntenna)
{
  dishDiameter_p.attach(msAntenna, NewMSAntenna::
			columnName(NewMSAntenna::DISH_DIAMETER));
  flagRow_p.attach(msAntenna, NewMSAntenna::
		   columnName(NewMSAntenna::FLAG_ROW));
  mount_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::MOUNT));
  name_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::NAME));
  offset_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET));
  position_p.attach(msAntenna, NewMSAntenna::
		    columnName(NewMSAntenna::POSITION));
  station_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::STATION));
  type_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::TYPE));
  offsetMeas_p.attach(msAntenna, NewMSAntenna::
		      columnName(NewMSAntenna::OFFSET));
  positionMeas_p.attach(msAntenna, NewMSAntenna::
			columnName(NewMSAntenna::POSITION));
  dishDiameterQuant_p.attach(msAntenna, NewMSAntenna::
			     columnName(NewMSAntenna::DISH_DIAMETER));
  offsetQuant_p.attach(msAntenna, NewMSAntenna::
		       columnName(NewMSAntenna::OFFSET));
  positionQuant_p.attach(msAntenna, NewMSAntenna::
			 columnName(NewMSAntenna::POSITION));
  attachOptionalCols(msAntenna);
}

void RONewMSAntennaColumns::attachOptionalCols(const NewMSAntenna& msAntenna)
{
  const ColumnDescSet& cds=msAntenna.tableDesc().columnDescSet();
  const String& meanOrbit=NewMSAntenna::columnName(NewMSAntenna::MEAN_ORBIT);
  if (cds.isDefined(meanOrbit)) meanOrbit_p.attach(msAntenna,meanOrbit);
  const String& orbitId=NewMSAntenna::columnName(NewMSAntenna::ORBIT_ID);
  if (cds.isDefined(orbitId)) orbitId_p.attach(msAntenna,orbitId);
  const String& phasedArrayId=NewMSAntenna::
    columnName(NewMSAntenna::PHASED_ARRAY_ID);
  if (cds.isDefined(phasedArrayId)) {
    phasedArrayId_p.attach(msAntenna, phasedArrayId);
  }
}

Int RONewMSAntennaColumns::
matchAntenna(const MPosition& antennaPos, const Quantum<Double>& tolerance,
	     Int tryRow) {
  uInt r = nrow();
  if (r == 0) return -1;
  // Convert the antenna position to something in m.
  const MPosition::Types refType =
    MPosition::castType(antennaPos.getRef().getType());
  // If the type does not match then throw an exception! If someone is trying
  // to do this then they should be doing the conversions elsewhere and the
  // sooner they know about this error the better.
  if (MPosition::castType(positionMeas().getMeasRef().getType()) != refType) {
    throw(AipsError("RONewMSAntennaColumns::matchAntenna(...) - "
		    " cannot match when reference frames differ"));
  }
  // Convert the tolerance to meters
  const Unit m("m");
  DebugAssert(tolerance.check(m.getValue()), AipsError);
  const Double tolInM = tolerance.getValue(m);
  // Convert the position to meters
  const Vector<Double>& antPosInM = antennaPos.getValue().getValue();
  // Main matching loop
  if (tryRow >= 0) {
    const uInt tr = tryRow;
    if (tr >= r) {
      throw(AipsError("RONewMSAntennaColumns::matchAntenna(...) - "
                      "the row you suggest is too big"));
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

Int RONewMSAntennaColumns::matchAntenna(const String& antName,
					const MPosition& antennaPos,
					const Quantum<Double>& tolerance,
					Int tryRow) {
  uInt r = nrow();
  if (r == 0) return -1;
  // Convert the antenna position to something in m.
  const MPosition::Types refType =
    MPosition::castType(antennaPos.getRef().getType());
  // If the type does not match then throw an exception! If someone is trying
  // to do this then they should be doing the conversions elsewhere and the
  // sooner they know about this error the better.
  if (MPosition::castType(positionMeas().getMeasRef().getType()) != refType) {
    throw(AipsError("RONewMSAntennaColumns::matchAntenna(...) - "
		    " cannot match when reference frames differ"));
  }
  // Convert the tolerance to meters
  const Unit m("m");
  DebugAssert(tolerance.check(m.getValue()), AipsError);
  const Double tolInM = tolerance.getValue(m);
  // Convert the position to meters
  const Vector<Double>& antPosInM = antennaPos.getValue().getValue();
  // Main matching loop
  if (tryRow >= 0) {
    const uInt tr = tryRow;
    if (tr >= r) {
      throw(AipsError("RONewMSAntennaColumns::matchAntenna(...) - "
                      "the row you suggest is too big"));
    }
    if (!flagRow()(tr) &&
	matchName(tr, antName) &&
	matchPosition(tr, antPosInM, tolInM)) {
      return tr;
    }
    if (tr == r-1) r--;
  }
  while (r > 0) {
    r--;
    if (!flagRow()(r) &&
	matchName(r, antName) &&
	matchPosition(r, antPosInM, tolInM)) {
      return r;
    }
  }
  return -1;
}

Bool RONewMSAntennaColumns::matchName(uInt row, const String& antName) const {
  DebugAssert(row < nrow(), AipsError);
  return antName.matches(name()(row));
}

Bool RONewMSAntennaColumns::
matchPosition(uInt row, const Vector<Double>& antPosInM,
	      const Double tolInM) const {
  DebugAssert(row < nrow(), AipsError);
  DebugAssert(antPosInM.nelements() == 3, AipsError);
  return allNearAbs(position()(row), antPosInM, tolInM);
}

NewMSAntennaColumns::NewMSAntennaColumns(NewMSAntenna& msAntenna):
  RONewMSAntennaColumns(msAntenna),
  dishDiameter_p(msAntenna, NewMSAntenna::
		 columnName(NewMSAntenna::DISH_DIAMETER)),
  flagRow_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::FLAG_ROW)),
  mount_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::MOUNT)),
  name_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::NAME)),
  offset_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  position_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  station_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::STATION)),
  type_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::TYPE)),
  meanOrbit_p(),
  orbitId_p(),
  phasedArrayId_p(),
  offsetMeas_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionMeas_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  dishDiameterQuant_p(msAntenna, NewMSAntenna::
		      columnName(NewMSAntenna::DISH_DIAMETER)),
  offsetQuant_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionQuant_p(msAntenna, NewMSAntenna::columnName(NewMSAntenna::POSITION))
{
  const ColumnDescSet& cds=msAntenna.tableDesc().columnDescSet();
  const String& meanOrbit=NewMSAntenna::columnName(NewMSAntenna::MEAN_ORBIT);
  if (cds.isDefined(meanOrbit)) meanOrbit_p.attach(msAntenna,meanOrbit);
  const String& orbitId=NewMSAntenna::columnName(NewMSAntenna::ORBIT_ID);
  if (cds.isDefined(orbitId)) orbitId_p.attach(msAntenna,orbitId);
  const String& phasedArrayId=NewMSAntenna::columnName(NewMSAntenna::PHASED_ARRAY_ID);
  if (cds.isDefined(phasedArrayId)) phasedArrayId_p.attach(msAntenna,phasedArrayId);
}

NewMSAntennaColumns::~NewMSAntennaColumns() {}

NewMSAntennaColumns::NewMSAntennaColumns():
  RONewMSAntennaColumns(),
  dishDiameter_p(),
  flagRow_p(),
  mount_p(),
  name_p(),
  offset_p(),
  position_p(),
  station_p(),
  type_p(),
  meanOrbit_p(),
  orbitId_p(),
  phasedArrayId_p(),
  offsetMeas_p(),
  positionMeas_p(),
  dishDiameterQuant_p(),
  offsetQuant_p(),
  positionQuant_p()
{
}

void NewMSAntennaColumns::attach(NewMSAntenna& msAntenna)
{
  RONewMSAntennaColumns::attach(msAntenna);
  dishDiameter_p.attach(msAntenna, NewMSAntenna::
			columnName(NewMSAntenna::DISH_DIAMETER));
  flagRow_p.attach(msAntenna, NewMSAntenna::
		   columnName(NewMSAntenna::FLAG_ROW));
  mount_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::MOUNT));
  name_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::NAME));
  offset_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::OFFSET));
  position_p.attach(msAntenna, NewMSAntenna::
		    columnName(NewMSAntenna::POSITION));
  station_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::STATION));
  type_p.attach(msAntenna, NewMSAntenna::columnName(NewMSAntenna::TYPE));
  offsetMeas_p.attach(msAntenna, NewMSAntenna::
		      columnName(NewMSAntenna::OFFSET));
  positionMeas_p.attach(msAntenna, NewMSAntenna::
			columnName(NewMSAntenna::POSITION));
  dishDiameterQuant_p.attach(msAntenna, NewMSAntenna::
			     columnName(NewMSAntenna::DISH_DIAMETER));
  offsetQuant_p.attach(msAntenna, NewMSAntenna::
		       columnName(NewMSAntenna::OFFSET));
  positionQuant_p.attach(msAntenna, NewMSAntenna::
			 columnName(NewMSAntenna::POSITION));
  attachOptionalCols(msAntenna);
}

void NewMSAntennaColumns::attachOptionalCols(NewMSAntenna& msAntenna)
{
  const ColumnDescSet& cds=msAntenna.tableDesc().columnDescSet();
  const String& meanOrbit=NewMSAntenna::columnName(NewMSAntenna::MEAN_ORBIT);
  if (cds.isDefined(meanOrbit)) meanOrbit_p.attach(msAntenna,meanOrbit);
  const String& orbitId=NewMSAntenna::columnName(NewMSAntenna::ORBIT_ID);
  if (cds.isDefined(orbitId)) orbitId_p.attach(msAntenna,orbitId);
  const String& phasedArrayId=NewMSAntenna::
    columnName(NewMSAntenna::PHASED_ARRAY_ID);
  if (cds.isDefined(phasedArrayId)) {
    phasedArrayId_p.attach(msAntenna, phasedArrayId);
  }
}


void NewMSAntennaColumns::setPositionRef(MPosition::Types ref)
{
  positionMeas_p.setDescRefCode(ref);
}

void NewMSAntennaColumns::setOffsetRef(MPosition::Types ref) 
{
  offsetMeas_p.setDescRefCode(ref);
}
// Local Variables: 
// compile-command: "gmake NewMSAntennaColumns"
// End: 
