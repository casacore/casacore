//# NewMSFieldColumns.cc:  provides easy access to NewMeasurementSet columns
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
#include <aips/MeasurementSets/NewMSFieldColumns.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Measures/MDirection.h>
#include <aips/Arrays/ArrayMath.h>

NewMSFieldColumns::NewMSFieldColumns(NewMSField& msField):
code_p(msField,NewMSField::columnName(NewMSField::CODE)),
delayDir_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
flagRow_p(msField,NewMSField::columnName(NewMSField::FLAG_ROW)),
name_p(msField,NewMSField::columnName(NewMSField::NAME)),
numPoly_p(msField,NewMSField::columnName(NewMSField::NUM_POLY)),
phaseDir_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
referenceDir_p(msField,NewMSField::columnName(NewMSField::REFERENCE_DIR)),
sourceId_p(msField,NewMSField::columnName(NewMSField::SOURCE_ID)),
time_p(msField,NewMSField::columnName(NewMSField::TIME)),
delayDirMeas_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
phaseDirMeas_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
referenceDirMeas_p(msField,NewMSField::columnName(NewMSField::REFERENCE_DIR)),
timeMeas_p(msField,NewMSField::columnName(NewMSField::TIME)),
//delayDirQuant_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
//phaseDirQuant_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
//referenceDirQuant_p(msField,NewMSField::columnName(NewMSField::REFERENCE_DIR)),
timeQuant_p(msField,NewMSField::columnName(NewMSField::TIME))
{
  const ColumnDescSet& cds=msField.tableDesc().columnDescSet();
  const String& ephemerisId=NewMSField::columnName(NewMSField::EPHEMERIS_ID);
  if (cds.isDefined(ephemerisId)) ephemerisId_p.attach(msField,ephemerisId);
}

NewMSFieldColumns::~NewMSFieldColumns() {}

MDirection NewMSFieldColumns::delayDirMeas(Int row, Double interTime)
{
  return interpolateDirMeas(delayDirMeasCol()(row), numPoly()(row),
			    interTime, time()(row)); 
}

MDirection NewMSFieldColumns::phaseDirMeas(Int row, Double interTime)
{
  return interpolateDirMeas(phaseDirMeasCol()(row), numPoly()(row),
			    interTime, time()(row)); 
}

MDirection NewMSFieldColumns::referenceDirMeas(Int row, Double interTime)
{
  return interpolateDirMeas(referenceDirMeasCol()(row), numPoly()(row),
			    interTime, time()(row)); 
}

MDirection NewMSFieldColumns::interpolateDirMeas
(const Array<MDirection>& arrDir, Int numPoly, 
 Double interTime, Double timeOrigin)
{
  Vector<MDirection> vecDir(arrDir);
  if ((numPoly == 0) || interTime<1 || nearAbs(interTime, timeOrigin)) {
    return vecDir(0);
  } else {
    Vector<Double> dir(vecDir(0).getAngle().getValue()), tmp; 
    Double dt = interTime - timeOrigin;
    Double fac = 1;
    for (Int i=1; i<(numPoly+1); i++) {
      fac *= dt;
      tmp = vecDir(i).getAngle().getValue();
      tmp *= fac;
      dir += tmp;
    }
    return MDirection(MVDirection(dir),vecDir(0).getRef());
  }
}

RONewMSFieldColumns::RONewMSFieldColumns(const NewMSField& msField):
code_p(msField,NewMSField::columnName(NewMSField::CODE)),
delayDir_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
flagRow_p(msField,NewMSField::columnName(NewMSField::FLAG_ROW)),
name_p(msField,NewMSField::columnName(NewMSField::NAME)),
numPoly_p(msField,NewMSField::columnName(NewMSField::NUM_POLY)),
phaseDir_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
referenceDir_p(msField,NewMSField::columnName(NewMSField::REFERENCE_DIR)),
sourceId_p(msField,NewMSField::columnName(NewMSField::SOURCE_ID)),
time_p(msField,NewMSField::columnName(NewMSField::TIME)),
delayDirMeas_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
phaseDirMeas_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
referenceDirMeas_p(msField,NewMSField::columnName(NewMSField::REFERENCE_DIR)),
timeMeas_p(msField,NewMSField::columnName(NewMSField::TIME)),
//delayDirQuant_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
//phaseDirQuant_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
//referenceDirQuant_p(msField,NewMSField::columnName(NewMSField::REFERENCE_DIR)),
timeQuant_p(msField,NewMSField::columnName(NewMSField::TIME))
{
  const ColumnDescSet& cds=msField.tableDesc().columnDescSet();
  const String& ephemerisId=NewMSField::columnName(NewMSField::EPHEMERIS_ID);
  if (cds.isDefined(ephemerisId)) ephemerisId_p.attach(msField,ephemerisId);
}

RONewMSFieldColumns::~RONewMSFieldColumns() {}

MDirection RONewMSFieldColumns::delayDirMeas(Int row, Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(delayDirMeasCol()(row), numPoly()(row),
			    interTime, time()(row)); 
}

MDirection RONewMSFieldColumns::phaseDirMeas(Int row, Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(phaseDirMeasCol()(row), numPoly()(row),
			    interTime, time()(row)); 
}

MDirection RONewMSFieldColumns::referenceDirMeas(Int row, Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(referenceDirMeasCol()(row), numPoly()(row),
			    interTime, time()(row)); 
}

