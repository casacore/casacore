//# MSSelection.cc: Implementation of MSSelection.h
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//# $Id: 
//----------------------------------------------------------------------------

#include <trial/MeasurementSets/MSSelection.h>
#include <trial/MeasurementSets/MSFieldIndex.h>
#include <trial/MeasurementSets/MSDataDescIndex.h>
#include <trial/MeasurementSets/MSSourceIndex.h>
#include <trial/MeasurementSets/MSAntennaIndex.h>
#include <trial/MeasurementSets/MSSpWindowIndex.h>
#include <aips/MeasurementSets/MSMainColumns.h>
#include <aips/Measures/MeasureHolder.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/QuantumHolder.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Record.h>
#include <aips/Utilities/DataType.h>
#include <aips/iostream.h>

//----------------------------------------------------------------------------

MSSelection::MSSelection() : 
  startTime_p(), endTime_p(), velocityStart_p(), velocityStep_p(), 
  startUV_p(0), endUV_p(0), fieldNames_p(), sourceNames_p(), 
  antennaNames_p(), corrTypes_p(), obsModes_p(), calGrps_p(), 
  interferometerIds_p(), fieldIds_p(0), scanNos_p(0), spwIds_p(0), 
  freqGrps_p(0), antennaIds_p(0), feedIds_p(0), arrayIds_p(0), 
  msSelect_p(), nchan_p(0), start_p(0), step_p(0), 
  selectStartTime_p(False), selectEndTime_p(False), 
  selectFieldIds_p(False), selectFieldNames_p(False), 
  selectSourceNames_p(False), selectScanNos_p(False), selectSpwIds_p(False), 
  selectFreqGrps_p(False), selectChanSel_p(False), selectVelocitySel_p(False), 
  selectAntennaIds_p(False), selectAntennaNames_p(False), 
  selectInterferometerIds_p(False), selectFeedIds_p(False), 
  selectCorrTypes_p(False), selectArrayIds_p(False), selectUVRange_p(False), 
  selectMSSelect_p(False), selectObsModes_p(False), selectCalGrps_p(False)
{
// Default null constructor 
// Output to private data:
//    startTime_p               MEpoch          Start time
//    endTime_p                 Mepoch          End time
//    velocityStart_p           MRadialVelocity Start velocity
//    velocityStep_p            MRadialVelocity Velocity step
//    startUV_p                 Double          Inner uv-distance (lambda)
//    endUV_p                   Double          Outer uv-distance (lambda)
//    fieldNames_p              Vector<String>  Field names
//    sourceNames_p             Vector<String>  Source names
//    antennaNames_p            Vector<String>  Antenna names
//    corrTypes_p               Vector<String>  Correlation polzn. types
//    obsModes_p                Vector<String>  Observing modes
//    calGrps_p                 Vector<String>  Calibration groups
//    interferometerIds         Vector<Int>     Interferometer id's
//    fieldIds_p                Vector<Int>     Field id's
//    scanNos_p                 Vector<Int>     Scan numbers
//    spwIds_p                  Vector<Int>     Spectral window id's
//    freqGrps_p                Vector<Int>     Frequency groups
//    antennaIds_p              Vector<Int>     Antenna id's
//    feedIds_p                 Vector<Int>     Feed id's
//    arrayIds_p                Vector<Int>     Array id's
//    msSelect_p                String          MS selection string
//    nchan_p                   Int             No of channels
//    start_p                   Int             Start channel
//    step_p                    Int             Channel step
//    selectStartTime_p         Bool            True if start time selected
//    selectEndTime_p           Bool            True if end time selected
//    selectFieldIds_p          Bool            True if field id's selected
//    selectFieldNames_p        Bool            True if field names selected
//    selectSourceNames_p       Bool            True if source names selected
//    selectScanNos_p           Bool            True if scan no's selected
//    selectSpwIds_p            Bool            True if spw id's selected
//    selectFreqGrps_p          Bool            True if freq groups selected
//    selectChanSel_p           Bool            True if channels selected
//    selectVelocitySel_p       Bool            True if velocities selected
//    selectAntennaIds_p        Bool            True if antenna id's selected
//    selectAntennaNames_p      Bool            True if antenna names selected
//    selectInterferometerIds_p Bool            True if interferometer id's 
//                                              selected
//    selectFeedIds_p           Bool            True if feed id's selected
//    selectCorrTypes_p         Bool            True if polzn corr selected
//    selectArrayIds_p          Bool            True if array id's selected
//    selectUVRange_p           Bool            True if UV range selected
//    selectMSSelect_p          Bool            True if MS select string is set
//    selectObsModes_p          Bool            True if obs modes selected
//    selectCalGrps_p           Bool            True if cal groups selected
//
};

//----------------------------------------------------------------------------

MSSelection::~MSSelection()
{
// Default desctructor
//
};

//----------------------------------------------------------------------------

MSSelection::MSSelection(const Record& selectionItem) : 
  startTime_p(), endTime_p(), velocityStart_p(), velocityStep_p(), 
  startUV_p(0), endUV_p(0), fieldNames_p(), sourceNames_p(), 
  antennaNames_p(), corrTypes_p(), obsModes_p(), calGrps_p(), 
  interferometerIds_p(), fieldIds_p(0), scanNos_p(0), spwIds_p(0), 
  freqGrps_p(0), antennaIds_p(0), feedIds_p(0), arrayIds_p(0), 
  msSelect_p(), nchan_p(0), start_p(0), step_p(0), 
  selectStartTime_p(False), selectEndTime_p(False),
  selectFieldIds_p(False), selectFieldNames_p(False), 
  selectSourceNames_p(False), selectScanNos_p(False), 
  selectSpwIds_p(False), selectFreqGrps_p(False), 
  selectChanSel_p(False), selectVelocitySel_p(False), 
  selectAntennaIds_p(False), selectAntennaNames_p(False), 
  selectInterferometerIds_p(False), selectFeedIds_p(False), 
  selectCorrTypes_p(False), selectArrayIds_p(False), selectUVRange_p(False), 
  selectMSSelect_p(False), selectObsModes_p(False), selectCalGrps_p(False)
{
// Construct from a record representing a selection item
// Output to private data:
//    startTime_p               MEpoch          Start time
//    endTime_p                 Mepoch          End time
//    velocityStart_p           MRadialVelocity Start velocity
//    velocityStep_p            MRadialVelocity Velocity step
//    startUV_p                 Double          Inner uv-distance (lambda)
//    endUV_p                   Double          Outer uv-distance (lambda)
//    fieldNames_p              Vector<String>  Field names
//    sourceNames_p             Vector<String>  Source names
//    antennaNames_p            Vector<String>  Antenna names
//    corrTypes_p               Vector<String>  Correlation polzn. types
//    obsModes_p                Vector<String>  Observing modes
//    calGrps_p                 Vector<String>  Calibration groups
//    interferometerIds         Vector<Int>     Interferometer id's
//    fieldIds_p                Vector<Int>     Field id's
//    scanNos_p                 Vector<Int>     Scan numbers
//    spwIds_p                  Vector<Int>     Spectral window id's
//    freqGrps_p                Vector<Int>     Frequency groups
//    antennaIds_p              Vector<Int>     Antenna id's
//    feedIds_p                 Vector<Int>     Feed id's
//    arrayIds_p                Vector<Int>     Array id's
//    msSelect_p                String          MS selection string
//    nchan_p                   Int             No of channels
//    start_p                   Int             Start channel
//    step_p                    Int             Channel step
//    selectStartTime_p         Bool            True if start time selected
//    selectEndTime_p           Bool            True if end time selected
//    selectFieldIds_p          Bool            True if field id's selected
//    selectFieldNames_p        Bool            True if field names selected
//    selectSourceNames_p       Bool            True if source names selected
//    selectScanNos_p           Bool            True if scan no's selected
//    selectSpwIds_p            Bool            True if spw id's selected
//    selectFreqGrps_p          Bool            True if freq groups selected
//    selectChanSel_p           Bool            True if channels selected
//    selectVelocitySel_p       Bool            True if velocities selected
//    selectAntennaIds_p        Bool            True if antenna id's selected
//    selectAntennaNames_p      Bool            True if antenna names selected
//    selectInterferometerIds_p Bool            True if interferometer id's 
//                                              selected
//    selectFeedIds_p           Bool            True if feed id's selected
//    selectCorrTypes_p         Bool            True if polzn corr selected
//    selectArrayIds_p          Bool            True if array id's selected
//    selectUVRange_p           Bool            True if UV range selected
//    selectMSSelect_p          Bool            True if MS select string is set
//    selectObsModes_p          Bool            True if obs modes selected
//    selectCalGrps_p           Bool            True if cal groups selected
//
  // Extract fields from the selection item record
  fromSelectionItem(selectionItem);
};

//----------------------------------------------------------------------------

MSSelection::MSSelection (const MSSelection& other)
{
// Copy constructor
// Input:
//    other            const MSSelection&    Existing MSSelection object
// Output to private data:
//
};

//----------------------------------------------------------------------------

MSSelection& MSSelection::operator= (const MSSelection& other)
{
// Assignment operator
// Input:
//    other            const MSSelection&    RHS MSSelection object
// Output to private data:
//
  Bool identity = (this == &other);
  return *this;
};

//----------------------------------------------------------------------------

void MSSelection::setStartTime(const MEpoch& startTime)
{
// Set the selected start time
// Input:
//    startTime            const MEpoch&            Start time
// Output to private data:
//    startTime_p          MEpoch                   Start time
//    selectStartTime_p    Bool                     True if start time selected
// 
  startTime_p = startTime;
  selectStartTime_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setEndTime(const MEpoch& endTime)
{
// Set the selected end time
// Input:
//    endTime          const MEpoch&            End time
// Output to private data:
//    endTime_p        MEpoch                   End time
//    selectEndTime_p  Bool                     True if end time selected
// 
  endTime_p = endTime;
  selectEndTime_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setFieldIds(const Vector<Int>& fieldIds)
{
// Set the selected field id's
// Input:
//    fieldIds         const Vector<Int>&       Selected field id's
// Output to private data:
//    fieldIds_p       Vector<Int>              Selected field id's
//    selectFieldIds_p Bool                     True if field id's selected
// 
  fieldIds_p = fieldIds;
  selectFieldIds_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setFieldNames(const Vector<String>& fieldNames)
{
// Set the selected field names
// Input:
//    fieldNames         const Vector<String>&    Selected field names
// Output to private data:
//    fieldNames_p       Vector<String>           Selected field names
//    selectFieldNames_p Bool                     True if field names selected
// 
  fieldNames_p = fieldNames;
  selectFieldNames_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setSourceNames(const Vector<String>& sourceNames)
{
// Set the selected source names
// Input:
//    sourceNames         const Vector<String>&   Selected source names
// Output to private data:
//    sourceNames_p       Vector<String>          Selected source names
//    selectSourceNames_p Bool                    True if source names selected
// 
  sourceNames_p = sourceNames;
  selectSourceNames_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setScanNos(const Vector<Int>& scanNos)
{
// Set the selected scan numbers
// Input:
//    scanNos         const Vector<Int>&       Selected scan numbers
// Output to private data:
//    scanNos_p       Vector<Int>              Selected scan numbers
//    selectScanNos_p Bool                     True if scan no's selected
// 
  scanNos_p = scanNos;
  selectScanNos_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setSpwIds(const Vector<Int>& spwIds)
{
// Set the selected spw id's
// Input:
//    spwIds         const Vector<Int>&       Selected spw id's
// Output to private data:
//    spwIds_p       Vector<Int>              Selected spw id's
//    selectSpwIds_p Bool                     True if spw id's selected
// 
  spwIds_p = spwIds;
  selectSpwIds_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setFreqGrps(const Vector<Int>& freqGrps)
{
// Set the selected frequency groups
// Input:
//    freqGrps         const Vector<Int>&       Selected freq groups
// Output to private data:
//    freqGrps_p       Vector<Int>              Selected freq groups
//    selectFreqGrps_p Bool                     True if freq groups selected
// 
  freqGrps_p = freqGrps;
  selectFreqGrps_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setChanSel(const Int& nchan, const Int& start,
			     const Int& step)
{
// Set the selected frequency channels
// Input:
//    nchan            const Int&               No of frequency channels
//    start            const Int&               Start channel no.
//    step             const Int&               Channel increment
// Output to private data:
//    nchan_p          Int                      No of frequency channels
//    start_p          Int                      Start channel no.
//    step_p           Int                      Channel increment
//    selectChanSel_p  Bool                     True if freq channels selected
// 
  nchan_p = nchan;
  start_p = start;
  step_p = step;
  selectChanSel_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setVelocitySel(const Int& nchan, 
				 const MRadialVelocity& velocityStart,
				 const MRadialVelocity& velocityStep)
{
// Set the selected velocity range
// Input:
//    nchan                const Int&               No of velocity channels
//    velocityStart        const MRadialVelocity&   Start velocity
//    velocityStep         const MRadialVelcoity&   Velocity increment
// Output to private data:
//    nchan_p              Int                      No of velocity channels
//    velocityStart_p      MRadialVelocity&         Start velocity
//    velocityStep_p       MRadialVelocity&         Velocity increment
//    selectVelocitySel_p  Bool                     True if velocities selected
// 
  nchan_p = nchan;
  velocityStart_p = velocityStart;
  velocityStep_p = velocityStep;
  selectVelocitySel_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setAntennaIds(const Vector<Int>& antennaIds)
{
// Set the selected antenna id's
// Input:
//    antennaIds         const Vector<Int>&       Selected antenna id's
// Output to private data:
//    antennaIds_p       Vector<Int>              Selected antenna id's
//    selectAntennaIds_p Bool                     True if antenna id's selected
// 
  antennaIds_p = antennaIds;
  selectAntennaIds_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setAntennaNames(const Vector<String>& antennaNames)
{
// Set the selected antenna names
// Input:
//    antennaNames            const Vector<String>&    Selected antenna names
// Output to private data:
//    antennaNames_p          Vector<String>           Selected antenna names
//    selectAntennaNames_p    Bool                     True if antenna names
//                                                     selected
// 
  antennaNames_p = antennaNames;
  selectAntennaNames_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setInterferometerIds(const Matrix<Int>& interferometerIds)
{
// Set the selected interferometer id's
// Input:
//    interferometerIds         const Matrix<Int>&    Selected ifr id's
// Output to private data:
//    interferometerIds_p       Matrix<Int>           Selected ifr id's
//    selectInterferometerIds_p Bool                  True if ifr id's selected
// 
  interferometerIds_p = interferometerIds;
  selectInterferometerIds_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setFeedIds(const Vector<Int>& feedIds)
{
// Set the selected feed id's
// Input:
//    feedIds         const Vector<Int>&       Selected feed id's
// Output to private data:
//    feedIds_p       Vector<Int>              Selected feed id's
//    selectFeedIds_p Bool                     True if feed id's selected
// 
  feedIds_p = feedIds;
  selectFeedIds_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setCorrTypes(const Vector<String>& corrTypes)
{
// Set the selected polarization correlation types
// Input:
//    corrTypes          const Vector<String>&    Selected polzn correlations
// Output to private data:
//    corrTypes_p        Vector<String>           Selected polzn correlations
//    selectCorrTypes_p  Bool                     True if polzn. corr. selected
// 
  corrTypes_p = corrTypes;
  selectCorrTypes_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setArrayIds(const Vector<Int>& arrayIds)
{
// Set the selected array id's
// Input:
//    arrayIds         const Vector<Int>&       Selected array id's
// Output to private data:
//    arrayIds_p       Vector<Int>              Selected array id's
//    selectArrayIds_p Bool                     True if array id's selected
// 
  arrayIds_p = arrayIds;
  selectArrayIds_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setUVRange(const Double& startUV, const Double& endUV)
{
// Set the selected UV range
// Input:
//    startUV         const Double&            Start uv range (lambda)
//    endUV           const Double&            End uv range (lambda)
// Output to private data:
//    startUV_p       Double                   Start uv range (lambda)
//    endUV_p         Double                   End uv range (lambda)
//    selectUVRange_p Bool                     True if uv range selected
// 
  startUV_p = startUV;
  endUV_p = endUV;
  selectUVRange_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setMSSelect(const String& msSelect)
{
// Set the selected observing modes
// Input:
//    msSelect          const String&          Supplementary MS selection
// Output to private data:
//    msSelect_p        String                 Supplementary MS selection
//    selectMSSelecy_p  Bool                   True if uv range selected
// 
  msSelect_p = msSelect;
  selectMSSelect_p = True;
};

//----------------------------------------------------------------------------

void MSSelection::setObsModes(const Vector<String>& obsModes)
{
// Set the supplementary MS select string
// Input:
//    obsModes          const Vector<String>&  Observing modes
// Output to private data:
//    obsModes_p        Vector<String>         Observing modes
//    selectObsModes_p  Bool                   True if obs modes selected
// 
  obsModes_p = obsModes;
  selectObsModes_p = True;

};

//----------------------------------------------------------------------------

void MSSelection::setCalGrps(const Vector<String>& calGrps)
{
// Set the supplementary MS select string
// Input:
//    obsModes          const Vector<String>&  Calibration groups
// Output to private data:
//    obsModes_p        Vector<String>         Calibration groups
//    selectObsModes_p  Bool                   True if cal groups selected
// 
  calGrps_p = calGrps;
  selectCalGrps_p = True;
};

//----------------------------------------------------------------------------

TableExprNode MSSelection::toTableExprNode(const MeasurementSet& ms)
{
// Convert the MS selection to a TableExprNode object, 
// representing a TaQL selection in C++.
// Input:
//    ms               const MeasurementSet&     MeasurementSet to bind TaQL
// Output:
//    toTableExprNode  TableExprNode             Table expression node
//
  TableExprNode condition;
  Bool setCond=False;
  String colName;

  // Check all fields by turn in the MS selection
  //
  // Start time
  if (selectStartTime_p) {
    colName = MS::columnName(MS::TIME);
    MVTime mvStart(startTime_p.getValue());
    if(!setCond){
      condition=(ms.col(colName) >= mvStart);
      setCond=True;
    }
    else{
      condition = condition && (ms.col(colName) >= mvStart);
    }
  };

  // End time
  if (selectEndTime_p) {
    colName = MS::columnName(MS::TIME);
    MVTime mvEnd(endTime_p.getValue());
    if(!setCond){
      condition=(ms.col(colName) <= mvEnd);
      setCond=True;
    }
    else{
      condition = condition && (ms.col(colName) <= mvEnd);
    }
  };

  // Field id's
  if (selectFieldIds_p) {
    colName = MS::columnName(MS::FIELD_ID);
    if(!setCond){
      condition=(ms.col(colName).in(fieldIds_p));
      setCond=True;
    }
    else{
      condition = condition && (ms.col(colName).in(fieldIds_p));
    }
  };



  // Field names
  if (selectFieldNames_p) {
    // Look-up in FIELD sub-table
    MSFieldIndex msFI(ms.field());
    colName = MS::columnName(MS::FIELD_ID);

    if(!setCond){
      condition=(ms.col(colName).in(msFI.matchFieldName(fieldNames_p)));
      setCond=True;
    } 
    else{
      condition = condition && 
	(ms.col(colName).in(msFI.matchFieldName(fieldNames_p)));
    }
  };

  // Source names
  if (selectSourceNames_p && !ms.source().isNull()) {
    // Look-up in SOURCE sub-table
    MSSourceIndex msSI(ms.source());
    MSFieldIndex msFI(ms.field());
    colName = MS::columnName(MS::FIELD_ID);
    if(!setCond){
      condition=(ms.col(colName).in
		 (msFI.matchSourceId(msSI.matchSourceName(sourceNames_p))));
      setCond=True;
    } 
    else{
      condition = condition && 
      (ms.col(colName).in
       (msFI.matchSourceId(msSI.matchSourceName(sourceNames_p))));
    }
  };

  // Scan numbers
  if (selectScanNos_p) {
    colName = MS::columnName(MS::SCAN_NUMBER);
    if(!setCond){
      condition=(ms.col(colName).in(scanNos_p));
      setCond=True;
    }
    else{
      condition = condition && (ms.col(colName).in(scanNos_p));
    }
  };

  // Spectral window id's
  if (selectSpwIds_p) {
    // Look-up in DATA_DESC sub-table
    MSDataDescIndex msDDI(ms.dataDescription());
    colName = MS::columnName(MS::DATA_DESC_ID);
    if(!setCond){
      condition= (ms.col(colName).in(msDDI.matchSpwId(spwIds_p)));
      setCond=True;
    }
    else{
      condition = condition &&
	(ms.col(colName).in(msDDI.matchSpwId(spwIds_p)));
    }
  };

  // Frequency groups
  if (selectFreqGrps_p) {
    // Look-up in SPECTRAL_WINDOW sub-table
    MSSpWindowIndex msSPWI(ms.spectralWindow());
    MSDataDescIndex msDDI(ms.dataDescription());
    colName = MS::columnName(MS::DATA_DESC_ID);
   if(!setCond){
     condition=(ms.col(colName).in
		(msDDI.matchSpwId(msSPWI.matchFreqGrp(freqGrps_p))));
     setCond=True;
   }
   else{
     condition = condition && 
       (ms.col(colName).in
	(msDDI.matchSpwId(msSPWI.matchFreqGrp(freqGrps_p))));
   }
  };

  // Antenna id's
  if (selectAntennaIds_p) {
    colName = MS::columnName(MS::ANTENNA1);
    TableExprNode antennaCondition = ms.col(colName).in(antennaIds_p);
    colName = MS::columnName(MS::ANTENNA2);
    antennaCondition = antennaCondition || (ms.col(colName).in(antennaIds_p));
    
    if(!setCond){
      condition=antennaCondition;
      setCond=True;
    }
    else{
      condition = condition && antennaCondition;
    }
  };

  // Antenna names
  if (selectAntennaNames_p) {
    MSAntennaIndex msAI(ms.antenna());
    colName = MS::columnName(MS::ANTENNA1);
    TableExprNode antennaCondition = ms.col(colName).in
      (msAI.matchAntennaName(antennaNames_p));
    colName = MS::columnName(MS::ANTENNA2);
    antennaCondition = antennaCondition || 
      (ms.col(colName).in(msAI.matchAntennaName(antennaNames_p)));

    if(!setCond){
      condition=antennaCondition;
      setCond=True;
    }
    else{
      condition = condition && antennaCondition;
    }

  };

  // Interferometer id's
  if (selectInterferometerIds_p) {
  };

  // Feed id's
  if (selectFeedIds_p) {
    colName = MS::columnName(MS::FEED1);
    TableExprNode feedCondition = ms.col(colName).in(feedIds_p);
    colName = MS::columnName(MS::FEED2);
    feedCondition = feedCondition || (ms.col(colName).in(feedIds_p));
 
    if(!setCond){
      condition=feedCondition;
      setCond=True;
    }
    else{
      condition = condition && feedCondition;
    }


  };

  // Polarization correlation types

  // Array id's
  if (selectArrayIds_p) {
    colName = MS::columnName(MS::ARRAY_ID);

    if(!setCond){
      condition=(ms.col(colName).in(arrayIds_p));
      setCond=True;
    }
    else{
      condition = condition && (ms.col(colName).in(arrayIds_p));
    }

  };

  // UV range
  if (selectUVRange_p) {
    // Column accessors
    ROMSMainColumns msMainCol(ms);
    ROMSSpWindowColumns msSpwCol(ms.spectralWindow());
    ROMSDataDescColumns msDataDescCol(ms.dataDescription());

    // Loop over all rows in the MS
    Vector<Int> rowsel;
    Int nRowSel = 0;
    for (uInt row=0; row<ms.nrow(); row++) {
      Int ddid = msMainCol.dataDescId()(row);
      Int spwid = msDataDescCol.spectralWindowId()(ddid);
      Double refFreq = msSpwCol.refFrequency()(spwid);
      Vector<Double> uvw = msMainCol.uvw()(row);
      Double uvDist = sqrt(uvw(0)*uvw(0) + uvw(1)*uvw(1)) * refFreq / C::c;
      if ((startUV_p <= uvDist) && (uvDist <= endUV_p)) {
	nRowSel++;
	rowsel.resize(nRowSel, True);
	rowsel(nRowSel) = row;
      };
    };
    
    if(!setCond){
      condition=(ms.nodeRownr().in(rowsel));
      setCond=True;
    }
    else{
      condition = condition && (ms.nodeRownr().in(rowsel));
    }
  };
  
  // MS selection string

  // Obs modes

  // Calibration groups

  return condition;
};

//----------------------------------------------------------------------------

void MSSelection::fromSelectionItem(const Record& selectionItem)
{
// Convert from an input selection data item
// Input:
//    selectionItem     const Record&     Selection item
// Output to private data:
// 
  // Convert to an AIPS++ Record
  Record selRec = selectionItem;

  // Debug print statements
  cout << "MSSel::fromSI, at start, selRec.nfields=" << selRec.nfields()
       << endl;
  for (uInt fld=0; fld<selRec.nfields(); fld++) {
    cout << "MSSel::fromGR, fld= " << fld << ", name= "
	 << selRec.name(fld) << ", type= "
	 << selRec.type(fld) << endl;
  };
  cout << "------------------------------------------------------" << endl;

  MeasureHolder mh;
  QuantumHolder qh;
  String error;

  // Extract and set all fields
  //
  // Start time
  if (definedAndSet(selRec,"starttime")) {
    if (selRec.dataType("starttime") == TpRecord) {
      mh.fromRecord(error, selRec.asRecord("starttime"));
      MEpoch mStartTime(mh.asMEpoch());
      setStartTime(mStartTime);
      mStartTime.print(cout); cout << ", starttime record" << endl;
      cout << "ref= " << mStartTime.getRefString() 
	   << ", in s= " << mStartTime.get("s") << endl;
    } else {
      qh.fromString(error, selRec.asString("starttime"));
      MEpoch mStartTime(qh.asQuantity());
      setStartTime(mStartTime);
      mStartTime.print(cout); cout << ", starttime string" << endl;
      cout << "ref= " << mStartTime.getRefString() 
	   << ", in s= " << mStartTime.get("s") << endl;
    };
  };

  // End time
  if (definedAndSet(selRec,"endtime")) {
    if (selRec.dataType("endtime") == TpRecord) {
      mh.fromRecord(error, selRec.asRecord("endtime"));
      MEpoch mEndTime(mh.asMEpoch());
      setEndTime(mEndTime);
      mEndTime.print(cout); cout << ", endtime record" << endl;
      cout << "ref= " << mEndTime.getRefString() 
	   << ", in s= " << mEndTime.get("s") << endl;
    } else {
      qh.fromString(error, selRec.asString("endtime"));
      MEpoch mEndTime(qh.asQuantity());
      setEndTime(mEndTime);
      mEndTime.print(cout); cout << ", endtime record" << endl;
      cout << "ref= " << mEndTime.getRefString() 
	   << ", in s= " << mEndTime.get("s") << endl;
    };
  };

  // Field id's
  if (definedAndSet(selRec,"fieldids")) {
    setFieldIds(selRec.asArrayInt("fieldids"));
    cout << fieldIds_p << ", fieldids" << endl;
  };

  // Field names
  if (definedAndSet(selRec,"fieldnames")) {
    setFieldNames(selRec.asArrayString("fieldnames"));
    cout << fieldNames_p << ", field names" << endl;
  };

  // Source names
  if (definedAndSet(selRec,"sourcenames")) {
    setSourceNames(selRec.asArrayString("sourcenames"));
    cout << sourceNames_p << ", source names" << endl;
  };

  // Scan numbers
  if (definedAndSet(selRec,"scannos")) {
    setScanNos(selRec.asArrayInt("scannos"));
    cout << scanNos_p << ", scan numbers" << endl;
  };

  // Spectral window id's
  if (definedAndSet(selRec,"spwids")) {
    setSpwIds(selRec.asArrayInt("spwids"));
    cout << spwIds_p << ", spw ids" << endl;
  };

  // Frequency groups
  if (definedAndSet(selRec,"freqgrps")) {
    setFreqGrps(selRec.asArrayInt("freqgrps"));
    cout << freqGrps_p << ", freq grps" << endl;
  };

  // Frequency selection
  if (definedAndSet(selRec,"freqsel")) {
    Record subRec(selRec.subRecord("freqsel"));
    Int isFreqsel;
    if (definedAndSet(subRec,"isFreqsel")) {
      isFreqsel = subRec.asInt("isFreqsel");
      switch (isFreqsel) {
      case 0: 
	Int nchan, start, step;
	if (definedAndSet(subRec,"nchan")) {
	  nchan = subRec.asInt("nchan");
	  if (definedAndSet(subRec,"start")) {
	    start = subRec.asInt("start");
	    if (definedAndSet(subRec,"step")) {
	      step = subRec.asInt("step");
	      setChanSel(nchan, start, step);
	      cout << "nchan= " << nchan << ", start= "
		   << start << ", step= " << step << endl;
	    }
	  }
	}
	break;
      case 1:
	// VELOCITY selection
	String mstart, mstep, frame;
	if (definedAndSet(subRec,"frame")) {
	  frame = subRec.asString("frame");

	  cout << "frame= " << frame << endl;

	  MRadialVelocity::Types freqFrame;

	  if (MRadialVelocity::getType(freqFrame, frame)) {

	    cout << "freqFrame= " << freqFrame << endl;

	    if (definedAndSet(subRec,"nchan")) {
	      nchan = subRec.asInt("nchan");
	      MRadialVelocity mRadVelStart, mRadVelStep;
	      if (definedAndSet(subRec,"mstart")) {
		if (subRec.dataType("mstart") == TpRecord) {
		  mh.fromRecord(error, subRec.asRecord("mstart"));
		  mRadVelStart = mh.asMRadialVelocity();
		  mRadVelStart.print(cout); cout << ", mstart" << endl;
		  cout << "ref= " << mRadVelStart.getRefString() 
		       << ", in km/s= " << mRadVelStart.get("km/s") << endl;
		} else {
		  qh.fromString(error, subRec.asString("mstart"));
		  MRadialVelocity mtemp(qh.asQuantity(), freqFrame);
		  mRadVelStart = mtemp;
		  mRadVelStart.print(cout); cout << ", mstart" << endl;
		  cout << "ref= " << mRadVelStart.getRefString() 
		       << ", in km/s= " << mRadVelStart.get("km/s") << endl;
		};
		if (definedAndSet(subRec,"mstep")) {
		  if (subRec.dataType("mstep") == TpRecord) {
		    mh.fromRecord(error, subRec.asRecord("mstep"));
		    mRadVelStep = mh.asMRadialVelocity();
		    mRadVelStep.print(cout); cout << ", mstep" << endl;

		    cout << "ref= " << mRadVelStep.getRefString() 
			 << ", in km/s= " << mRadVelStep.get("km/s") << endl;

		  } else {
		    qh.fromString(error, subRec.asString("mstep"));
		    MRadialVelocity mtemp(qh.asQuantity(), freqFrame);
		    mRadVelStep = mtemp;
		    mRadVelStep.print(cout); cout << ", mstep" << endl;

		    cout << "ref= " << mRadVelStep.getRefString() 
			 << ", in km/s= " << mRadVelStep.get("km/s") << endl;

		  };
		};
	      }
	    };
	  };
	};
	break;
      };
    };
  };

  // Antenna id's
  if (definedAndSet(selRec,"antennaids")) {
    setAntennaIds(selRec.asArrayInt("antennaids"));
    cout << antennaIds_p << ", antenna id's" << endl;
  };

  // Antenna names
  if (definedAndSet(selRec,"antennanames")) {
    setAntennaNames(selRec.asArrayString("antennanames"));
    cout << antennaNames_p << ", antenna names" << endl;
  };

  // Interferometer id's
  if (definedAndSet(selRec,"interferometerids")) {
    setInterferometerIds(selRec.asArrayInt("interferometerids"));
    cout << interferometerIds_p << ", interferometer ids" << endl;
  };

  // Feed id's
  if (definedAndSet(selRec,"feedids")) {
    setFeedIds(selRec.asArrayInt("feedids"));
    cout << feedIds_p << ", feed ids" << endl;
  };

  // Polarization correlation types
  if (definedAndSet(selRec,"corrtypes")) {
    setCorrTypes(selRec.asArrayString("corrtypes"));
    cout << corrTypes_p << ", corr types" << endl;
  };

  // Array id's
  if (definedAndSet(selRec,"arrayids")) {
    setArrayIds(selRec.asArrayInt("arrayids"));
    cout << arrayIds_p << ", array ids" << endl;
  };

  // UV range
  if (definedAndSet(selRec,"uvrange")) {
    Vector<Double> uvlimits(selRec.asArrayDouble("uvrange"));
    setUVRange(uvlimits(0), uvlimits(1));
    cout << startUV_p << " " << endUV_p << ", start and end UV" << endl;
  };

  // MS select
  if (definedAndSet(selRec,"msselect")) {
    setMSSelect(selRec.asString("msselect"));
    cout << msSelect_p << ", msselect" << endl;
  };

  // Observing modes
  if (definedAndSet(selRec,"obsmodes")) {
    setObsModes(selRec.asArrayString("obsmodes"));
    cout << obsModes_p << ", obs modes" << endl;
  };

  // Calibration groups
  if (definedAndSet(selRec,"calgrps")) {
    setCalGrps(selRec.asArrayString("calgrps"));
    cout << calGrps_p << ", cal grps" << endl;
  };
};

//----------------------------------------------------------------------------

Bool MSSelection::definedAndSet(const Record& inpRec, const String& fieldName)
{
// Check if a record field is defined and not AIPS++ unset
// Input:
//    inpRec          const Record&     Input Record
//    fieldName       const String&     Field name
// Ouput:
//    definedAndSet   Bool              True if field defined and
//                                      not AIPS++ unset
// 
  Bool retval = False;
  // Check if record field is defined
  if (inpRec.isDefined(fieldName)) {
    retval = True;
    // Now check if AIPS++ unset 
    if (inpRec.dataType(fieldName) == TpRecord) {
      Record gr = inpRec.subRecord(fieldName);
      retval = (gr.nfields() > 0);
    };
  };
  return retval;
};

//----------------------------------------------------------------------------













