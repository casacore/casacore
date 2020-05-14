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

MSPointingColumns::MSPointingColumns()
{ 
}

MSPointingColumns::MSPointingColumns(const MSPointing& msPointing)
{ 
  attach(msPointing);
}

MSPointingColumns::~MSPointingColumns() {}

void MSPointingColumns::attach(const MSPointing& msPointing)
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

void MSPointingColumns::attachOptionalCols(const MSPointing& msPointing)
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

MDirection MSPointingColumns::directionMeas(rownr_t row, 
						 Double interTime) const
{
  return MSFieldColumns::interpolateDirMeas(directionMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection MSPointingColumns::targetMeas(rownr_t row, Double interTime) const
{
  return MSFieldColumns::interpolateDirMeas(targetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection MSPointingColumns::pointingOffsetMeas(rownr_t row, 
						      Double interTime) const
{
  if (pointingOffsetMeasCol().isNull()) return MDirection();
  return MSFieldColumns::interpolateDirMeas(pointingOffsetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}

MDirection MSPointingColumns::sourceOffsetMeas(rownr_t row,
						    Double interTime) const
{
  if (sourceOffsetMeasCol().isNull()) return MDirection();
  return MSFieldColumns::interpolateDirMeas(sourceOffsetMeasCol()(row),
					       numPoly()(row),
					       interTime, time()(row)); 
}


Int64 MSPointingColumns::pointingIndex(Int antenna, Double ptime, Int64 guessRow) const
{
  if((this->nrow()) < 1)
    return -1;
  // return the first row matching the requirements
  const Int64 nrow = antennaId().nrow();
  //take up from where we left last time
  //hopefully time is monotonic
  //otherwise it will go through the table each time
  if(guessRow <0)
    guessRow=0;
  for (Int k=0; k< 2; ++k){
    Int64 start=guessRow;
    Int64 end=nrow;
    if(k==1){
      start=0;
      end=guessRow;
    }
    for (Int64 i=start; i<end; i++) {
      if (antennaId()(i)==antenna) {
	Double halfInt=0.0;  
	if(interval()(i)==0.0){
	  Int64 counter=0;
	  Int64 adder=1;
	  
	  while(!( (time()(i+counter)!=time()(i))&& (antennaId()(i+counter) == antenna))){
	    counter=counter+adder;
	    if(nrow <= i+counter){
	      adder=-1; 
	      counter=0;
	    }
	    ////Could not find another point (interval is infinite)  hence only 1 valid point
	    if( (i+counter) < 0)
	      return i;
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

} //# NAMESPACE CASACORE - END
