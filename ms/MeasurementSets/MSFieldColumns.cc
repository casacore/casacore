//# MSFieldColumns.cc:  provides easy access to MeasurementSet columns
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

#include <ms/MeasurementSets/MSFieldColumns.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicMath/Math.h>
#include <ms/MeasurementSets/MSField.h>
#include <measures/Measures/MeasRef.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/Quantum.h>
#include <tables/Tables/ColDescSet.h>
#include <tables/Tables/TableDesc.h>
#include <casa/Utilities/Assert.h>

namespace casa { //# NAMESPACE CASA - BEGIN

ROMSFieldColumns::ROMSFieldColumns(const MSField& msField):
  name_p(msField, MSField::columnName(MSField::NAME)),
  code_p(msField, MSField::columnName(MSField::CODE)),
  time_p(msField, MSField::columnName(MSField::TIME)),
  numPoly_p(msField, MSField::columnName(MSField::NUM_POLY)),
  delayDir_p(msField, MSField::columnName(MSField::DELAY_DIR)),
  phaseDir_p(msField, MSField::columnName(MSField::PHASE_DIR)),
  referenceDir_p(msField, MSField::columnName(MSField::REFERENCE_DIR)),
  sourceId_p(msField, MSField::columnName(MSField::SOURCE_ID)),
  flagRow_p(msField, MSField::columnName(MSField::FLAG_ROW)),
  ephemerisId_p(),
  timeMeas_p(msField, MSField::columnName(MSField::TIME)),
  delayDirMeas_p(msField, MSField::columnName(MSField::DELAY_DIR)),
  phaseDirMeas_p(msField, MSField::columnName(MSField::PHASE_DIR)),
  referenceDirMeas_p(msField,
		     MSField::columnName(MSField::REFERENCE_DIR)),
  timeQuant_p(msField, MSField::columnName(MSField::TIME))
{
  attachOptionalCols(msField);
}

ROMSFieldColumns::~ROMSFieldColumns() {}

MDirection ROMSFieldColumns::delayDirMeas(Int row, Double interTime) const
{
  return MSFieldColumns::interpolateDirMeas(delayDirMeasCol()(row), 
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection ROMSFieldColumns::phaseDirMeas(Int row, Double interTime) const
{
  return MSFieldColumns::interpolateDirMeas(phaseDirMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection ROMSFieldColumns::referenceDirMeas(Int row, 
						 Double interTime) const
{
  return MSFieldColumns::interpolateDirMeas(referenceDirMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

Bool ROMSFieldColumns::
matchReferenceDir(uInt row, const MVDirection& dirVal, const Double& sepInRad, 
		  Matrix<Double>& mdir, MVDirection& mvdir) const 
{
  referenceDir().get(row, mdir);
  mvdir.setAngle(mdir(0, 0), mdir(1, 0));
  if (dirVal.separation(mvdir) < sepInRad) {
    return True;
  } else {
    return False;
  }
}

Bool ROMSFieldColumns::
matchDelayDir(uInt row, const MVDirection& dirVal, const Double& sepInRad, 
	      Matrix<Double>& mdir, MVDirection& mvdir) const 
{
  delayDir().get(row, mdir);
  mvdir.setAngle(mdir(0, 0), mdir(1, 0));
  if (dirVal.separation(mvdir) < sepInRad) {
    return True;
  } else {
    return False;
  }
}

Bool ROMSFieldColumns::
matchPhaseDir(uInt row, const MVDirection& dirVal, const Double& sepInRad, 
	      Matrix<Double>& mdir, MVDirection& mvdir) const 
{
  phaseDir().get(row, mdir);
  mvdir.setAngle(mdir(0, 0), mdir(1, 0));
  if (dirVal.separation(mvdir) < sepInRad) {
    return True;
  } else {
    return False;
  }
}

Int ROMSFieldColumns::matchDirection(const MDirection& referenceDirection,
					const MDirection& delayDirection,
					const MDirection& phaseDirection,
					const Quantum<Double>& maxSeparation,
					Int tryRow) {
  uInt r = nrow();
  if (r == 0) return -1;
  const MVDirection& referenceDirVal = referenceDirection.getValue();
  const MVDirection& delayDirVal = delayDirection.getValue();
  const MVDirection& phaseDirVal = phaseDirection.getValue();
  // Convert the maximum separation to radians
  const Unit rad("rad");
  DebugAssert(maxSeparation.check(UnitVal::ANGLE), AipsError);
  const Double tolInRad = maxSeparation.getValue(rad);

  // Main matching loop
  MVDirection mvdir;
  Matrix<Double> mdir(2, 1);
  if (tryRow >= 0) {
    const uInt tr = tryRow;
    if (tr >= r) {
      throw(AipsError("ROMSFieldColumns::matchDirection(...) - "
		      "the row you suggest is too big"));
    }
    if (!flagRow()(tr) &&
	numPoly()(tr) == 0){
      // Get the reference frame
      const MDirection::Types refType = 
	MDirection::castType(referenceDirMeas(tr).getRef().getType());
      // for a solar system object only the frame has to match
      if((refType>=MDirection::MERCURY && refType<MDirection::N_Planets) ||
	 (matchReferenceDir(tr, referenceDirVal, tolInRad, mdir, mvdir) &&
	  matchDelayDir(tr, delayDirVal, tolInRad, mdir, mvdir) &&
	  matchPhaseDir(tr, phaseDirVal, tolInRad, mdir, mvdir))
	 ) {
	if ((MDirection::castType(referenceDirection.getRef().getType())==refType) &&
	    (MDirection::castType(delayDirection.getRef().getType()) == refType) &&
	    (MDirection::castType(phaseDirection.getRef().getType()) == refType)) {
	  return tr;
	}
      }
    }
    if (tr == r-1) r--;
  }
  while (r > 0) {
    r--;
    if (!flagRow()(r) &&
	numPoly()(r) == 0){
      // Get the reference frame
      const MDirection::Types refType = 
	MDirection::castType(referenceDirMeas(r).getRef().getType());
      // for a solar system object only the frame has to match
      if((refType>=MDirection::MERCURY && refType<MDirection::N_Planets) ||
	 (matchReferenceDir(r, referenceDirVal, tolInRad, mdir, mvdir) &&
	  matchDelayDir(r, delayDirVal, tolInRad, mdir, mvdir) &&
	  matchPhaseDir(r, phaseDirVal, tolInRad, mdir, mvdir))
	  ) {
	if ((MDirection::castType(referenceDirection.getRef().getType())==refType) &&
	    (MDirection::castType(delayDirection.getRef().getType()) == refType) &&
	    (MDirection::castType(phaseDirection.getRef().getType()) == refType)) {
	  return r;
	}
      }
    }
  }
  return -1;
}

ROMSFieldColumns::ROMSFieldColumns():
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

void ROMSFieldColumns::attach(const MSField& msField)
{
  name_p.attach(msField, MSField::columnName(MSField::NAME));
  code_p.attach(msField, MSField::columnName(MSField::CODE));
  time_p.attach(msField, MSField::columnName(MSField::TIME));
  numPoly_p.attach(msField, MSField::columnName(MSField::NUM_POLY));
  delayDir_p.attach(msField, MSField::columnName(MSField::DELAY_DIR));
  phaseDir_p.attach(msField, MSField::columnName(MSField::PHASE_DIR));
  referenceDir_p.attach(msField,
			MSField::columnName(MSField::REFERENCE_DIR));
  sourceId_p.attach(msField, MSField::columnName(MSField::SOURCE_ID));
  flagRow_p.attach(msField, MSField::columnName(MSField::FLAG_ROW));
  timeMeas_p.attach(msField, MSField::columnName(MSField::TIME));
  delayDirMeas_p.attach(msField,MSField::columnName(MSField::DELAY_DIR));
  phaseDirMeas_p.attach(msField,MSField::columnName(MSField::PHASE_DIR));
  referenceDirMeas_p.attach(msField,
			    MSField::columnName(MSField::REFERENCE_DIR));
  timeQuant_p.attach(msField, MSField::columnName(MSField::TIME));
  attachOptionalCols(msField);
}

void ROMSFieldColumns::attachOptionalCols(const MSField& msField)
{
  const ColumnDescSet& cds = msField.tableDesc().columnDescSet();
  const String& ephemerisId = MSField::columnName(MSField::EPHEMERIS_ID);
  if (cds.isDefined(ephemerisId)) ephemerisId_p.attach(msField, ephemerisId);
}

MSFieldColumns::MSFieldColumns(MSField& msField):
  ROMSFieldColumns(msField),
  name_p(msField,MSField::columnName(MSField::NAME)),
  code_p(msField,MSField::columnName(MSField::CODE)),
  time_p(msField,MSField::columnName(MSField::TIME)),
  numPoly_p(msField,MSField::columnName(MSField::NUM_POLY)),
  delayDir_p(msField,MSField::columnName(MSField::DELAY_DIR)),
  phaseDir_p(msField,MSField::columnName(MSField::PHASE_DIR)),
  referenceDir_p(msField,MSField::columnName(MSField::REFERENCE_DIR)),
  sourceId_p(msField,MSField::columnName(MSField::SOURCE_ID)),
  flagRow_p(msField,MSField::columnName(MSField::FLAG_ROW)),
  ephemerisId_p(),
  timeMeas_p(msField,MSField::columnName(MSField::TIME)),
  delayDirMeas_p(msField,MSField::columnName(MSField::DELAY_DIR)),
  phaseDirMeas_p(msField,MSField::columnName(MSField::PHASE_DIR)),
  referenceDirMeas_p(msField,
		     MSField::columnName(MSField::REFERENCE_DIR)),
  timeQuant_p(msField,MSField::columnName(MSField::TIME))
{
  attachOptionalCols(msField);
}

MSFieldColumns::~MSFieldColumns() {}

MSFieldColumns::MSFieldColumns():
  ROMSFieldColumns(),
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

void MSFieldColumns::attach(MSField& msField)
{
  ROMSFieldColumns::attach(msField);
  name_p.attach(msField, MSField::columnName(MSField::NAME));
  code_p.attach(msField, MSField::columnName(MSField::CODE));
  time_p.attach(msField, MSField::columnName(MSField::TIME));
  numPoly_p.attach(msField, MSField::columnName(MSField::NUM_POLY));
  delayDir_p.attach(msField, MSField::columnName(MSField::DELAY_DIR));
  phaseDir_p.attach(msField, MSField::columnName(MSField::PHASE_DIR));
  referenceDir_p.attach(msField,
			MSField::columnName(MSField::REFERENCE_DIR));
  sourceId_p.attach(msField, MSField::columnName(MSField::SOURCE_ID));
  flagRow_p.attach(msField, MSField::columnName(MSField::FLAG_ROW));
  timeMeas_p.attach(msField, MSField::columnName(MSField::TIME));
  delayDirMeas_p.attach(msField,MSField::columnName(MSField::DELAY_DIR));
  phaseDirMeas_p.attach(msField,MSField::columnName(MSField::PHASE_DIR));
  referenceDirMeas_p.attach(msField,
			    MSField::columnName(MSField::REFERENCE_DIR));
  timeQuant_p.attach(msField, MSField::columnName(MSField::TIME));
  attachOptionalCols(msField);
}

void MSFieldColumns::attachOptionalCols(MSField& msField)
{
  const ColumnDescSet& cds = msField.tableDesc().columnDescSet();
  const String& ephemerisId = MSField::columnName(MSField::EPHEMERIS_ID);
  if (cds.isDefined(ephemerisId)) ephemerisId_p.attach(msField, ephemerisId);
}

MDirection MSFieldColumns::
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

void MSFieldColumns::setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty) {
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

void MSFieldColumns::setDirectionRef(MDirection::Types ref) {
  delayDirMeas_p.setDescRefCode(ref);
  phaseDirMeas_p.setDescRefCode(ref); 
  referenceDirMeas_p.setDescRefCode(ref);
}
// Local Variables: 
// compile-command: "gmake MSFieldColumns"
// End: 

} //# NAMESPACE CASA - END

