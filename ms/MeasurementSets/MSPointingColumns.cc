//# MSPointingColumns.cc:  provides easy access to MeasurementSet columns
//# Copyright (C) 1999,2000,2001
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

#include <casacore/ms/MeasurementSets/MSPointingColumns.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSPointing.h>
#include <casacore/tables/Tables/ColDescSet.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

ROMSPointingColumns::
ROMSPointingColumns(const MSPointing& msPointing):
  antennaId_p(msPointing, MSPointing::
	      columnName(MSPointing::ANTENNA_ID)),
  direction_p(msPointing, MSPointing::columnName(MSPointing::DIRECTION)),
  interval_p(msPointing, MSPointing::columnName(MSPointing::INTERVAL)),
  name_p(msPointing, MSPointing::columnName(MSPointing::NAME)),
  numPoly_p(msPointing, MSPointing::columnName(MSPointing::NUM_POLY)),
  target_p(msPointing, MSPointing::columnName(MSPointing::TARGET)),
  time_p(msPointing, MSPointing::columnName(MSPointing::TIME)),
  timeOrigin_p(msPointing, MSPointing::
	       columnName(MSPointing::TIME_ORIGIN)),
  tracking_p(msPointing, MSPointing::columnName(MSPointing::TRACKING)),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(msPointing, MSPointing::
		  columnName(MSPointing::DIRECTION)),
  targetMeas_p(msPointing, MSPointing::columnName(MSPointing::TARGET)),
  timeMeas_p(msPointing, MSPointing::columnName(MSPointing::TIME)),
  timeOriginMeas_p(msPointing, MSPointing::
		   columnName(MSPointing::TIME_ORIGIN)),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(msPointing, MSPointing::
		  columnName(MSPointing::INTERVAL)),
  timeQuant_p(msPointing, MSPointing::columnName(MSPointing::TIME)),
  timeOriginQuant_p(msPointing, MSPointing::
		    columnName(MSPointing::TIME_ORIGIN))
{ 
  attachOptionalCols(msPointing);
}

ROMSPointingColumns::~ROMSPointingColumns() {}

MDirection ROMSPointingColumns::directionMeas(Int row, 
						 Double interTime) const
{
  return MSFieldColumns::interpolateDirMeas(directionMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection ROMSPointingColumns::targetMeas(Int row, Double interTime) const
{
  return MSFieldColumns::interpolateDirMeas(targetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection ROMSPointingColumns::pointingOffsetMeas(Int row, 
						      Double interTime) const
{
  if (pointingOffsetMeasCol().isNull()) return MDirection();
  return MSFieldColumns::interpolateDirMeas(pointingOffsetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection ROMSPointingColumns::sourceOffsetMeas(Int row,
						    Double interTime) const
{
  if (sourceOffsetMeasCol().isNull()) return MDirection();
  return MSFieldColumns::interpolateDirMeas(sourceOffsetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}


  Int ROMSPointingColumns::pointingIndex(Int antenna, Double ptime, Int guessRow) const
{
  if((this->nrow()) < 1)
    return -1;
  // return the first row matching the requirements
  const Int nrow = antennaId().nrow();
  //take up from where we left last time
  //hopefully time is monotonic
  //otherwise it will go through the table each time
  if(guessRow <0)
    guessRow=0;
  for (Int k=0; k< 2; ++k){
    Int start=guessRow;
    Int end=nrow;
    if(k==1){
      start=0;
      end=guessRow;
    }
    for (Int i=start; i<end; i++) {
      if (antennaId()(i)==antenna) {
	Double halfInt=0.0;  
	if(interval()(i)==0){
	  Int counter=0;
	  Int adder=1;
	  
	  while(time()(i+counter)==time()(i)){
	    counter=counter+adder;
	    if(nrow <= i+counter){
	      adder=-1; 
	      counter=0;
	    }        
	  }       
	  halfInt = abs(time()(i+counter)-time()(i))/2.0;
	}
	else{
	  halfInt = interval()(i)/2.0;
	}
	if (halfInt>0.0) {
	  if (time()(i) >= ptime - halfInt && time()(i) <= ptime + halfInt) {
	    return i;
	  }
	} else {
	  // valid for all times (we should also handle interval<0 -> timestamps)
	  return i;
	}
      }
    }
  }
  return -1;
}

ROMSPointingColumns::ROMSPointingColumns():
  antennaId_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numPoly_p(),
  target_p(),
  time_p(),
  timeOrigin_p(),
  tracking_p(),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(),
  targetMeas_p(),
  timeMeas_p(),
  timeOriginMeas_p(),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  timeOriginQuant_p()
{ 
}

void ROMSPointingColumns::attach(const MSPointing& msPointing)
{
  antennaId_p.attach(msPointing, MSPointing::
	      columnName(MSPointing::ANTENNA_ID));
  direction_p.attach(msPointing, MSPointing::
		     columnName(MSPointing::DIRECTION));
  interval_p.attach(msPointing, MSPointing::
		    columnName(MSPointing::INTERVAL));
  name_p.attach(msPointing, MSPointing::columnName(MSPointing::NAME));
  numPoly_p.attach(msPointing, MSPointing::
		   columnName(MSPointing::NUM_POLY));
  target_p.attach(msPointing, MSPointing::
		  columnName(MSPointing::TARGET));
  time_p.attach(msPointing, MSPointing::columnName(MSPointing::TIME));
  timeOrigin_p.attach(msPointing, MSPointing::
	       columnName(MSPointing::TIME_ORIGIN));
  tracking_p.attach(msPointing, MSPointing::
		    columnName(MSPointing::TRACKING));
  directionMeas_p.attach(msPointing, MSPointing::
		  columnName(MSPointing::DIRECTION));
  targetMeas_p.attach(msPointing, MSPointing::
		      columnName(MSPointing::TARGET));
  timeMeas_p.attach(msPointing, MSPointing::
		    columnName(MSPointing::TIME));
  timeOriginMeas_p.attach(msPointing, MSPointing::
		   columnName(MSPointing::TIME_ORIGIN));
  intervalQuant_p.attach(msPointing, MSPointing::
		  columnName(MSPointing::INTERVAL));
  timeQuant_p.attach(msPointing, MSPointing::
		     columnName(MSPointing::TIME));
  timeOriginQuant_p.attach(msPointing, MSPointing::
			   columnName(MSPointing::TIME_ORIGIN));
  attachOptionalCols(msPointing);
}

void ROMSPointingColumns::
attachOptionalCols(const MSPointing& msPointing)
{
  const ColumnDescSet& cds = msPointing.tableDesc().columnDescSet();
  const String& encoder = MSPointing::columnName(MSPointing::ENCODER);
  if (cds.isDefined(encoder)) {
    encoder_p.attach(msPointing, encoder);
    encoderMeas_p.attach(msPointing, encoder);
  }
  const String& onSource = MSPointing::columnName(MSPointing::ON_SOURCE);
  if (cds.isDefined(onSource)) onSource_p.attach(msPointing, onSource);
  const String& pointingModelId = 
    MSPointing::columnName(MSPointing::POINTING_MODEL_ID);
  if (cds.isDefined(pointingModelId)) {
    pointingModelId_p.attach(msPointing, pointingModelId);
  }
  const String& pointingOffset = MSPointing::
    columnName(MSPointing::POINTING_OFFSET);
  if (cds.isDefined(pointingOffset)) {
    pointingOffset_p.attach(msPointing, pointingOffset);
    pointingOffsetMeas_p.attach(msPointing, pointingOffset);
  }
  const String& sourceOffset = MSPointing::
    columnName(MSPointing::SOURCE_OFFSET);
  if (cds.isDefined(sourceOffset)) {
    sourceOffset_p.attach(msPointing, sourceOffset);
    sourceOffsetMeas_p.attach(msPointing, sourceOffset);
  }
  const String& overTheTop = 
    MSPointing::columnName(MSPointing::OVER_THE_TOP);
  if (cds.isDefined(overTheTop)) overTheTop_p.attach(msPointing, overTheTop);
}

MSPointingColumns::MSPointingColumns(MSPointing& msPointing):
  ROMSPointingColumns(msPointing),
  antennaId_p(msPointing, MSPointing::
	      columnName(MSPointing::ANTENNA_ID)),
  direction_p(msPointing, MSPointing::columnName(MSPointing::DIRECTION)),
  interval_p(msPointing, MSPointing::columnName(MSPointing::INTERVAL)),
  name_p(msPointing, MSPointing::columnName(MSPointing::NAME)),
  numPoly_p(msPointing, MSPointing::columnName(MSPointing::NUM_POLY)),
  target_p(msPointing, MSPointing::columnName(MSPointing::TARGET)),
  time_p(msPointing, MSPointing::columnName(MSPointing::TIME)),
  timeOrigin_p(msPointing, MSPointing::
	       columnName(MSPointing::TIME_ORIGIN)),
  tracking_p(msPointing, MSPointing::columnName(MSPointing::TRACKING)),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(msPointing, MSPointing::
		  columnName(MSPointing::DIRECTION)),
  targetMeas_p(msPointing, MSPointing::columnName(MSPointing::TARGET)),
  timeMeas_p(msPointing, MSPointing::columnName(MSPointing::TIME)),
  timeOriginMeas_p(msPointing, MSPointing::
		   columnName(MSPointing::TIME_ORIGIN)),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(msPointing, MSPointing::
		  columnName(MSPointing::INTERVAL)),
  timeQuant_p(msPointing, MSPointing::columnName(MSPointing::TIME)),
  timeOriginQuant_p(msPointing, MSPointing::
		    columnName(MSPointing::TIME_ORIGIN))
{ 
  attachOptionalCols(msPointing);
}

MSPointingColumns::~MSPointingColumns() {}

MSPointingColumns::MSPointingColumns():
  ROMSPointingColumns(),
  antennaId_p(),
  direction_p(),
  interval_p(),
  name_p(),
  numPoly_p(),
  target_p(),
  time_p(),
  timeOrigin_p(),
  tracking_p(),
  encoder_p(),
  onSource_p(),
  pointingModelId_p(),
  pointingOffset_p(),
  sourceOffset_p(),
  overTheTop_p(),
  directionMeas_p(),
  targetMeas_p(),
  timeMeas_p(),
  timeOriginMeas_p(),
  encoderMeas_p(),
  pointingOffsetMeas_p(),
  sourceOffsetMeas_p(),
  intervalQuant_p(),
  timeQuant_p(),
  timeOriginQuant_p()
{ 
}

void MSPointingColumns::attach(MSPointing& msPointing)
{
  ROMSPointingColumns::attach(msPointing);
  antennaId_p.attach(msPointing, MSPointing::
		     columnName(MSPointing::ANTENNA_ID));
  direction_p.attach(msPointing, MSPointing::
		     columnName(MSPointing::DIRECTION));
  interval_p.attach(msPointing, MSPointing::
		    columnName(MSPointing::INTERVAL));
  name_p.attach(msPointing, MSPointing::columnName(MSPointing::NAME));
  numPoly_p.attach(msPointing, MSPointing::
		   columnName(MSPointing::NUM_POLY));
  target_p.attach(msPointing, MSPointing::
		  columnName(MSPointing::TARGET));
  time_p.attach(msPointing, MSPointing::columnName(MSPointing::TIME));
  timeOrigin_p.attach(msPointing, MSPointing::
	       columnName(MSPointing::TIME_ORIGIN));
  tracking_p.attach(msPointing, MSPointing::
		    columnName(MSPointing::TRACKING));
  directionMeas_p.attach(msPointing, MSPointing::
		  columnName(MSPointing::DIRECTION));
  targetMeas_p.attach(msPointing, MSPointing::
		      columnName(MSPointing::TARGET));
  timeMeas_p.attach(msPointing, MSPointing::
		    columnName(MSPointing::TIME));
  timeOriginMeas_p.attach(msPointing, MSPointing::
		   columnName(MSPointing::TIME_ORIGIN));
  intervalQuant_p.attach(msPointing, MSPointing::
		  columnName(MSPointing::INTERVAL));
  timeQuant_p.attach(msPointing, MSPointing::
		     columnName(MSPointing::TIME));
  timeOriginQuant_p.attach(msPointing, MSPointing::
			   columnName(MSPointing::TIME_ORIGIN));
  attachOptionalCols(msPointing);
}

void MSPointingColumns::attachOptionalCols(MSPointing& msPointing)
{
  const ColumnDescSet& cds = msPointing.tableDesc().columnDescSet();
  const String& encoder = MSPointing::columnName(MSPointing::ENCODER);
  if (cds.isDefined(encoder)) {
    encoder_p.attach(msPointing, encoder);
    encoderMeas_p.attach(msPointing, encoder);
  }
  const String& onSource = MSPointing::columnName(MSPointing::ON_SOURCE);
  if (cds.isDefined(onSource)) onSource_p.attach(msPointing, onSource);
  const String& pointingModelId = 
    MSPointing::columnName(MSPointing::POINTING_MODEL_ID);
  if (cds.isDefined(pointingModelId)) {
    pointingModelId_p.attach(msPointing, pointingModelId);
  }
  const String& pointingOffset = MSPointing::
    columnName(MSPointing::POINTING_OFFSET);
  if (cds.isDefined(pointingOffset)) {
    pointingOffset_p.attach(msPointing, pointingOffset);
    pointingOffsetMeas_p.attach(msPointing, pointingOffset);
  }
  const String& sourceOffset = MSPointing::
    columnName(MSPointing::SOURCE_OFFSET);
  if (cds.isDefined(sourceOffset)) {
    sourceOffset_p.attach(msPointing, sourceOffset);
    sourceOffsetMeas_p.attach(msPointing, sourceOffset);
  }
  const String& overTheTop = 
    MSPointing::columnName(MSPointing::OVER_THE_TOP);
  if (cds.isDefined(overTheTop)) overTheTop_p.attach(msPointing, overTheTop);
}

void MSPointingColumns::
setEpochRef(MEpoch::Types ref, Bool tableMustBeEmpty) {
  timeMeas_p.setDescRefCode(ref, tableMustBeEmpty);
  timeOriginMeas_p.setDescRefCode(ref, tableMustBeEmpty);
}

void MSPointingColumns::setDirectionRef(MDirection::Types ref) {
  directionMeas_p.setDescRefCode(ref);
  targetMeas_p.setDescRefCode(ref);
  if (!pointingOffsetMeas_p.isNull()) {
    pointingOffsetMeas_p.setDescRefCode(ref);
  }
  if (!sourceOffsetMeas_p.isNull()) {
    sourceOffsetMeas_p.setDescRefCode(ref);
  }
}

void MSPointingColumns::setEncoderDirectionRef(MDirection::Types ref) 
{
  if (!encoderMeas_p.isNull()) {
    encoderMeas_p.setDescRefCode(ref);
  }
}
// Local Variables: 
// compile-command: "gmake MSPointingColumns"
// End: 

} //# NAMESPACE CASACORE - END

