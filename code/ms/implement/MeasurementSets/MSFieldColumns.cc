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

#include <aips/MeasurementSets/NewMSFieldColumns.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/MeasurementSets/NewMSField.h>
#include <aips/Measures/MeasRef.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Quanta/MVDirection.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Tables/ColDescSet.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Utilities/Assert.h>

RONewMSFieldColumns::RONewMSFieldColumns(const NewMSField& msField):
  name_p(msField, NewMSField::columnName(NewMSField::NAME)),
  code_p(msField, NewMSField::columnName(NewMSField::CODE)),
  time_p(msField, NewMSField::columnName(NewMSField::TIME)),
  numPoly_p(msField, NewMSField::columnName(NewMSField::NUM_POLY)),
  delayDir_p(msField, NewMSField::columnName(NewMSField::DELAY_DIR)),
  phaseDir_p(msField, NewMSField::columnName(NewMSField::PHASE_DIR)),
  referenceDir_p(msField, NewMSField::columnName(NewMSField::REFERENCE_DIR)),
  sourceId_p(msField, NewMSField::columnName(NewMSField::SOURCE_ID)),
  flagRow_p(msField, NewMSField::columnName(NewMSField::FLAG_ROW)),
  ephemerisId_p(),
  timeMeas_p(msField, NewMSField::columnName(NewMSField::TIME)),
  delayDirMeas_p(msField, NewMSField::columnName(NewMSField::DELAY_DIR)),
  phaseDirMeas_p(msField, NewMSField::columnName(NewMSField::PHASE_DIR)),
  referenceDirMeas_p(msField,
		     NewMSField::columnName(NewMSField::REFERENCE_DIR)),
  timeQuant_p(msField, NewMSField::columnName(NewMSField::TIME))
{
  attachOptionalCols(msField);
}

RONewMSFieldColumns::~RONewMSFieldColumns() {}

MDirection RONewMSFieldColumns::delayDirMeas(Int row, Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(delayDirMeasCol()(row), 
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection RONewMSFieldColumns::phaseDirMeas(Int row, Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(phaseDirMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection RONewMSFieldColumns::referenceDirMeas(Int row, 
						 Double interTime) const
{
  return NewMSFieldColumns::interpolateDirMeas(referenceDirMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

Int RONewMSFieldColumns::matchDirection(const MDirection& referenceDirection,
					const MDirection& delayDirection,
					const MDirection& phaseDirection,
					const MVAngle& maxSeparation) {
  uInt r = nrow();
  if (r == 0) return -1;
  // convert the supplied directions to the same reference frame as the ones in
  // the Table. It would be nice if this converter could be cached somewhere.
  MDirection::Convert c(referenceDirection, delayDirMeasCol().getMeasRef());
  const MVDirection refVal = c().getValue();
  // Create these here to avoid creating them lots of times as a temporaries
  const MVDirection delayVal = c(delayDirection).getValue();
  const MVDirection phaseVal = c(phaseDirection).getValue();
  const Double sepInRad = maxSeparation.radian();
  Matrix<Double> mdir(IPosition(2,1,2));
  Vector<Double> dir(mdir.nonDegenerate()); // A reference to the mdir matrix
  while (r > 0) {
    r--;
    if (flagRow()(r) == False && numPoly()(r) == 0) {
      delayDir().get(r, mdir);
      if (delayVal.separation(MVDirection(dir)) < sepInRad) {
 	phaseDir().get(r, mdir);
 	if (phaseVal.separation(MVDirection(dir)) < sepInRad) {
 	  referenceDir().get(r, mdir);
 	  if (refVal.separation(MVDirection(dir)) < sepInRad) {
 	    DebugAssert(dir.nrefs() == 2, AipsError); 
 	    DebugAssert(mdir.nrefs() == 2, AipsError);
 	    return r;
 	  }
 	}
      }
    }
  }
  DebugAssert(dir.nrefs() == 2, AipsError);
  DebugAssert(mdir.nrefs() == 2, AipsError);
  return -1;
}

RONewMSFieldColumns::RONewMSFieldColumns():
  name_p(),
  code_p(),
  time_p(),
  numPoly_p(),
  delayDir_p(),
  phaseDir_p(),
  referenceDir_p(),
  sourceId_p(),
  flagRow_p(),
  ephemerisId_p(),
  timeMeas_p(),
  delayDirMeas_p(),
  phaseDirMeas_p(),
  referenceDirMeas_p(),
  timeQuant_p()
{
}

void RONewMSFieldColumns::attach(const NewMSField& msField)
{
  name_p.attach(msField, NewMSField::columnName(NewMSField::NAME));
  code_p.attach(msField, NewMSField::columnName(NewMSField::CODE));
  time_p.attach(msField, NewMSField::columnName(NewMSField::TIME));
  numPoly_p.attach(msField, NewMSField::columnName(NewMSField::NUM_POLY));
  delayDir_p.attach(msField, NewMSField::columnName(NewMSField::DELAY_DIR));
  phaseDir_p.attach(msField, NewMSField::columnName(NewMSField::PHASE_DIR));
  referenceDir_p.attach(msField,
			NewMSField::columnName(NewMSField::REFERENCE_DIR));
  sourceId_p.attach(msField, NewMSField::columnName(NewMSField::SOURCE_ID));
  flagRow_p.attach(msField, NewMSField::columnName(NewMSField::FLAG_ROW));
  timeMeas_p.attach(msField, NewMSField::columnName(NewMSField::TIME));
  delayDirMeas_p.attach(msField,NewMSField::columnName(NewMSField::DELAY_DIR));
  phaseDirMeas_p.attach(msField,NewMSField::columnName(NewMSField::PHASE_DIR));
  referenceDirMeas_p.attach(msField,
			    NewMSField::columnName(NewMSField::REFERENCE_DIR));
  timeQuant_p.attach(msField, NewMSField::columnName(NewMSField::TIME));
  attachOptionalCols(msField);
}

void RONewMSFieldColumns::attachOptionalCols(const NewMSField& msField)
{
  const ColumnDescSet& cds = msField.tableDesc().columnDescSet();
  const String& ephemerisId = NewMSField::columnName(NewMSField::EPHEMERIS_ID);
  if (cds.isDefined(ephemerisId)) ephemerisId_p.attach(msField, ephemerisId);
}

NewMSFieldColumns::NewMSFieldColumns(NewMSField& msField):
  RONewMSFieldColumns(msField),
  name_p(msField,NewMSField::columnName(NewMSField::NAME)),
  code_p(msField,NewMSField::columnName(NewMSField::CODE)),
  time_p(msField,NewMSField::columnName(NewMSField::TIME)),
  numPoly_p(msField,NewMSField::columnName(NewMSField::NUM_POLY)),
  delayDir_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
  phaseDir_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
  referenceDir_p(msField,NewMSField::columnName(NewMSField::REFERENCE_DIR)),
  sourceId_p(msField,NewMSField::columnName(NewMSField::SOURCE_ID)),
  flagRow_p(msField,NewMSField::columnName(NewMSField::FLAG_ROW)),
  ephemerisId_p(),
  timeMeas_p(msField,NewMSField::columnName(NewMSField::TIME)),
  delayDirMeas_p(msField,NewMSField::columnName(NewMSField::DELAY_DIR)),
  phaseDirMeas_p(msField,NewMSField::columnName(NewMSField::PHASE_DIR)),
  referenceDirMeas_p(msField,
		     NewMSField::columnName(NewMSField::REFERENCE_DIR)),
  timeQuant_p(msField,NewMSField::columnName(NewMSField::TIME))
{
  attachOptionalCols(msField);
}

NewMSFieldColumns::~NewMSFieldColumns() {}

NewMSFieldColumns::NewMSFieldColumns():
  RONewMSFieldColumns(),
  name_p(),
  code_p(),
  time_p(),
  numPoly_p(),
  delayDir_p(),
  phaseDir_p(),
  referenceDir_p(),
  sourceId_p(),
  flagRow_p(),
  ephemerisId_p(),
  timeMeas_p(),
  delayDirMeas_p(),
  phaseDirMeas_p(),
  referenceDirMeas_p(),
  timeQuant_p()
{
}

void NewMSFieldColumns::attach(NewMSField& msField)
{
  RONewMSFieldColumns::attach(msField);
  name_p.attach(msField, NewMSField::columnName(NewMSField::NAME));
  code_p.attach(msField, NewMSField::columnName(NewMSField::CODE));
  time_p.attach(msField, NewMSField::columnName(NewMSField::TIME));
  numPoly_p.attach(msField, NewMSField::columnName(NewMSField::NUM_POLY));
  delayDir_p.attach(msField, NewMSField::columnName(NewMSField::DELAY_DIR));
  phaseDir_p.attach(msField, NewMSField::columnName(NewMSField::PHASE_DIR));
  referenceDir_p.attach(msField,
			NewMSField::columnName(NewMSField::REFERENCE_DIR));
  sourceId_p.attach(msField, NewMSField::columnName(NewMSField::SOURCE_ID));
  flagRow_p.attach(msField, NewMSField::columnName(NewMSField::FLAG_ROW));
  timeMeas_p.attach(msField, NewMSField::columnName(NewMSField::TIME));
  delayDirMeas_p.attach(msField,NewMSField::columnName(NewMSField::DELAY_DIR));
  phaseDirMeas_p.attach(msField,NewMSField::columnName(NewMSField::PHASE_DIR));
  referenceDirMeas_p.attach(msField,
			    NewMSField::columnName(NewMSField::REFERENCE_DIR));
  timeQuant_p.attach(msField, NewMSField::columnName(NewMSField::TIME));
  attachOptionalCols(msField);
}

void NewMSFieldColumns::attachOptionalCols(NewMSField& msField)
{
  const ColumnDescSet& cds = msField.tableDesc().columnDescSet();
  const String& ephemerisId = NewMSField::columnName(NewMSField::EPHEMERIS_ID);
  if (cds.isDefined(ephemerisId)) ephemerisId_p.attach(msField, ephemerisId);
}

MDirection NewMSFieldColumns::
interpolateDirMeas(const Array<MDirection>& arrDir, Int numPoly, 
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
// Local Variables: 
// compile-command: "gmake NewMSFieldColumns"
// End: 
