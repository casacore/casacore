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

//# Includes
#include <aips/MeasurementSets/NewMSAntennaColumns.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Measures/MPosition.h>

NewMSAntennaColumns::NewMSAntennaColumns(NewMSAntenna& msAntenna):
  dishDiameter_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::DISH_DIAMETER)),
  flagRow_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::FLAG_ROW)),
  mount_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::MOUNT)),
  name_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::NAME)),
  offset_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  position_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  station_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::STATION)),
  type_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::TYPE)),
  offsetMeas_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionMeas_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  dishDiameterQuant_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::DISH_DIAMETER)),
  offsetQuant_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionQuant_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::POSITION))
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

void NewMSAntennaColumns::setPositionRef(Int ref) 
{
  position_p.rwKeywordSet().rwSubRecord("MEASINFO").
    define("Ref",MPosition::showType(ref));
}

void NewMSAntennaColumns::setOffsetRef(Int ref) 
{
  offset_p.rwKeywordSet().rwSubRecord("MEASINFO").
    define("Ref",MPosition::showType(ref));
}



RONewMSAntennaColumns::RONewMSAntennaColumns(const NewMSAntenna& msAntenna):
  dishDiameter_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::DISH_DIAMETER)),
  flagRow_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::FLAG_ROW)),
  mount_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::MOUNT)),
  name_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::NAME)),
  offset_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  position_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  station_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::STATION)),
  type_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::TYPE)),
  offsetMeas_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionMeas_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::POSITION)),
  dishDiameterQuant_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::DISH_DIAMETER)),
  offsetQuant_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::OFFSET)),
  positionQuant_p(msAntenna,NewMSAntenna::columnName(NewMSAntenna::POSITION))
{
  const ColumnDescSet& cds=msAntenna.tableDesc().columnDescSet();
  const String& meanOrbit=NewMSAntenna::columnName(NewMSAntenna::MEAN_ORBIT);
  if (cds.isDefined(meanOrbit)) meanOrbit_p.attach(msAntenna,meanOrbit);
  const String& orbitId=NewMSAntenna::columnName(NewMSAntenna::ORBIT_ID);
  if (cds.isDefined(orbitId)) orbitId_p.attach(msAntenna,orbitId);
  const String& phasedArrayId=NewMSAntenna::columnName(NewMSAntenna::PHASED_ARRAY_ID);
  if (cds.isDefined(phasedArrayId)) phasedArrayId_p.attach(msAntenna,
							   phasedArrayId);
}

RONewMSAntennaColumns::~RONewMSAntennaColumns() {}

