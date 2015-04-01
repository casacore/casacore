//# MSCalEngine.cc: Engine to calculate derived MS values
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

#include <casacore/derivedmscal/DerivedMC/MSCalEngine.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCBaseline.h>
#include <casacore/measures/Measures/Muvw.h>
#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore {

MSCalEngine::MSCalEngine()
  : itsLastCalInx   (-1),
    itsReadFieldDir (True),
    itsDirColName   ("PHASE_DIR")
{}

MSCalEngine::~MSCalEngine()
{}

void MSCalEngine::setTable (const Table& table)
{
  // Set a new table.
  itsTable = table;
  // Clear everything, so it can be re-initialized.
  itsLastCalInx = -1;
  itsAntPos.clear();
  itsMount.clear();
  itsAntMB.clear();
  if (itsReadFieldDir) {
    itsFieldDir.clear();
  }
  itsCalIdMap.clear();
}

double MSCalEngine::getHA (Int antnr, uInt rownr)
{
  setData (antnr, rownr);
  return itsRADecToHADec().getValue().get()[0];
}

void MSCalEngine::getHaDec (Int antnr, uInt rownr, Array<double>& data)
{
  setData (antnr, rownr);
  data = itsRADecToHADec().getValue().get();
}

double MSCalEngine::getPA (Int antnr, uInt rownr)
{
  Int mount = setData (antnr, rownr);
  if (mount == 1) {
    // Do the conversions using the machines.
    return itsRADecToAzEl().getValue().positionAngle
      (itsPoleToAzEl().getValue());
  }
  return 0.;
}

double MSCalEngine::getLAST (Int antnr, uInt rownr)
{
  setData (antnr, rownr);
  return itsUTCToLAST().getValue().get();
}

void MSCalEngine::getAzEl (Int antnr, uInt rownr, Array<double>& data)
{
  setData (antnr, rownr);
  data = itsRADecToAzEl().getValue().get();
}

void MSCalEngine::getUVWJ2000 (uInt rownr, Array<double>& data)
{
  setData (1, rownr);
  Int ant1 = itsAntCol[0](rownr);
  Int ant2 = itsAntCol[1](rownr);
  if (ant1 == ant2) {
    data = 0.;
  } else {
    vector<MBaseline>& antMB        = itsAntMB[itsLastCalInx];
    vector<Vector<Double> >& antUvw = itsAntUvw[itsLastCalInx];
    Block<Bool>& uvwFilled          = itsUvwFilled[itsLastCalInx];
    // Calculate UVW per antenna and subtract to get baseline.
    // Only calculate for an antenna if not done yet.
    Int ant = ant1;
    for (int i=0; i<2; ++i) {
      if (!uvwFilled[ant]) {
        itsBLToJ2000.setModel (antMB[ant]);
        MVBaseline bas = itsBLToJ2000().getValue();
        MVuvw jvguvw(bas, itsLastDirJ2000.getValue());
        antUvw[ant] = Muvw(jvguvw, Muvw::J2000).getValue().getVector();
        uvwFilled[ant] = true;
      }
      ant = ant2;
    }
    // The UVW of the baseline is the difference of the antennae UVW.
    data = antUvw[ant2] - antUvw[ant1];
  }
}

void MSCalEngine::setDirection (const MDirection& dir)
{
  // Direction is explicitly given, so do not read from FIELD table.
  itsFieldDir.resize (1);
  itsFieldDir[0].resize (1);
  itsFieldDir[0][0] = dir;
  itsReadFieldDir = False;
}

void MSCalEngine::setDirColName (const String& colName)
{
  itsDirColName = colName;
  itsReadFieldDir = True;
}

Int MSCalEngine::setData (Int antnr, uInt rownr)
{
  // Initialize if not done yet.
  if (itsLastCalInx < 0) {
    init();
  }
  // Get the CAL_DESC_ID (if present).
  Int calInx = 0;
  Int calDescId = 0;
  if (! itsCalCol.isNull()) {
    calDescId = itsCalCol(rownr);
    // Update the CAL_DESC info if needed.
    if (calDescId >= Int(itsCalIdMap.size())) {
      fillCalDesc();
    }
    // Map CAL_DESC_ID to the cal index.
    // Initialize other last ids if a new cal index.
    calInx = itsCalIdMap[calDescId];
    if (calInx != itsLastCalInx) {
      itsLastFieldId = -1000;
      itsLastAntId   = -1000;
    }
  }
  itsLastCalInx = calInx;
  // Get the array or antenna position and put into the measure frame.
  // Also get mount type (alt-az or other).
  Int mount = 0;
  if (antnr < 0) {
    // Set the array position if needed.
    if (antnr != itsLastAntId) {
      itsFrame.resetPosition (itsArrayPos);
      itsLastAntId = antnr;
    }
  } else {
    // Get the antenna id from the table.
    // Update the antenna positions if a higher antenna id is found.
    // In practice this will not happen, but it is possible that the ANTENNA
    // table was not fully filled yet.
    Int antId = itsAntCol[antnr](rownr);
    if (antId != itsLastAntId) {
      if (antId >= Int(itsAntPos[calInx].size())) {
        fillAntPos (calDescId, calInx);
      }
      AlwaysAssert (antId < Int(itsAntPos[calInx].size()), AipsError);
      itsFrame.resetPosition (itsAntPos[calInx][antId]);
      itsLastAntId = antId;
    }
    mount = itsMount[calInx][antId];
  }
  // If needed, get the direction and put into the measure frame.
  // Get field id from the table; update the field positions if needed.
  Int fieldId = 0;
  if (itsReadFieldDir) {
    fieldId = itsFieldCol(rownr);
  }
  if (fieldId != itsLastFieldId) {
    if (fieldId >= Int(itsFieldDir[calInx].size())) {
      fillFieldDir (calDescId, calInx);
    }
    AlwaysAssert (fieldId < Int(itsFieldDir[calInx].size()), AipsError);
    const MDirection& dir = itsFieldDir[calInx][fieldId];
    itsDirToJ2000.setModel (dir);
    // We can already convert the direction to J2000 if it is not a model
    // (thus not time dependent).
    // Otherwise force the time to change, so the J2000 is calculated there.
    if (dir.isModel()) {
      itsLastTime = -1e30;
    } else {
      itsLastDirJ2000 = itsDirToJ2000();
      itsRADecToAzEl.setModel (itsLastDirJ2000);
      itsRADecToHADec.setModel(itsLastDirJ2000);
      itsFrame.resetDirection (itsLastDirJ2000);
    }
    /// or better set above models to dir??? Ask Wim. *****
    itsLastFieldId = fieldId;
  }
  // Set the epoch in the measure frame.
  Double time = itsTimeCol(rownr);
  if (time != itsLastTime) {
    MEpoch epoch = itsTimeMeasCol(rownr);
    itsFrame.resetEpoch (epoch);
    if (itsFieldDir[calInx][fieldId].isModel()) {
      itsLastDirJ2000 = itsDirToJ2000();
      itsRADecToAzEl.setModel (itsLastDirJ2000);
      itsRADecToHADec.setModel(itsLastDirJ2000);
      itsFrame.resetDirection (itsLastDirJ2000);
    }
    itsUTCToLAST.setModel (epoch);
    itsLastTime = time;
    itsUvwFilled[calInx] = False;
  }
  return mount;
}

void MSCalEngine::init()
{
  const TableDesc& td = itsTable.tableDesc();
  itsLastFieldId = -1000;
  itsLastAntId   = -1000;
  itsLastTime    = -1e30;
  itsAntCol[0].attach (itsTable, "ANTENNA1");
  if (td.isColumn("ANTENNA2")) {
    itsAntCol[1].attach (itsTable, "ANTENNA2");
  } else {
    itsAntCol[1].attach (itsTable, "ANTENNA1");
  }
  if (td.isColumn("FEED1")) {
    itsFeedCol[0].attach (itsTable, "FEED1");
    if (td.isColumn("FEED2")) {
      itsFeedCol[1].attach (itsTable, "FEED2");
    } else {
      itsFeedCol[1].attach (itsTable, "FEED1");
    }
  }
  itsFieldCol.attach (itsTable, "FIELD_ID");
  itsTimeCol.attach (itsTable, "TIME");
  itsTimeMeasCol.attach (itsTable, "TIME");
  Table obsTab;
  if (td.isColumn("CAL_DESC_ID")) {
    itsCalCol.attach (itsTable, "CAL_DESC_ID");
    // Fill CAl_DESC info from calibration table.
    fillCalDesc();
    obsTab = getSubTable (0, "OBSERVATION", False);
  } else {
    // Nothing special, so simply initialize.
    itsAntPos.resize   (1);
    itsMount.resize    (1);
    itsAntMB.resize    (1);
    itsAntUvw.resize   (1);
    itsUvwFilled.resize(1);
    if (itsReadFieldDir) {
      itsFieldDir.resize (1);
    }
    itsCalIdMap = vector<Int>(1,0);
    if (itsTable.keywordSet().isDefined("OBSERVATION")) {
      obsTab = itsTable.keywordSet().asTable("OBSERVATION");
    }
  }
  // Fill the antenna positions of the first CAL_DESC_ID.
  fillAntPos (0, 0);
  // Find observatory position.
  // Get it from the OBSERVATION subtable; otherwise try keyword TELESCOPE_NAME.
  // If not found, set it to the position of the middle antenna.
  Bool fndObs = False;
  if (! obsTab.isNull()) {
    if (obsTab.nrow() > 0) {
      String telescope = ScalarColumn<String>(obsTab, "TELESCOPE_NAME")(0);
      fndObs = MeasTable::Observatory (itsArrayPos, telescope);
    }
  }
  if (!fndObs  &&  itsTable.keywordSet().isDefined("TELESCOPE_NAME")) {
    String telescope = itsTable.keywordSet().asString("TELESCOPE_NAME");
    fndObs = MeasTable::Observatory (itsArrayPos, telescope);
  }
  if (!fndObs  &&  itsAntPos.size() > 0) {
    uInt nant = itsAntPos[0].size();
    if (nant > 0) {
      itsArrayPos = itsAntPos[0][nant/2];
    }
  }
  // Initialize the converters.
  // Set up the frame for epoch and antenna position.
  itsFrame.set (MEpoch(), MPosition(), MDirection());
  // Make the HADec pole as expressed in HADec. The pole is the default.
  MDirection::Ref rHADec(MDirection::HADEC, itsFrame);
  MDirection mHADecPole;
  mHADecPole.set (rHADec);
  itsPoleToAzEl.set (mHADecPole, MDirection::Ref(MDirection::AZEL,itsFrame));
  // Set up the machine to convert RaDec to AzEl.
  itsRADecToAzEl.set (MDirection(), MDirection::Ref(MDirection::AZEL,itsFrame));
  // Idem RaDec to HaDec.
  itsRADecToHADec.set (MDirection(), rHADec);
  // Idem direction to J2000.
  itsDirToJ2000.set (MDirection(), MDirection::Ref(MDirection::J2000,itsFrame));
  // Idem UTC to LAST.
  itsUTCToLAST.set (MEpoch(), MEpoch::Ref(MEpoch::LAST,itsFrame));
  // Idem MBaseline ITRF to J2000.
  itsBLToJ2000.set (MBaseline(), MBaseline::Ref(MBaseline::J2000,itsFrame));
}

void MSCalEngine::fillAntPos (Int calDescId, Int calInx)
{
  Table tab;
  if (itsCalCol.isNull()) {
    tab = itsTable.keywordSet().asTable("ANTENNA");
  } else {
    tab = getSubTable (calDescId, "ANTENNA");
  }
  ScalarMeasColumn<MPosition> posCol(tab, "POSITION");
  ScalarColumn<String>      mountCol(tab, "MOUNT");
  vector<MPosition>& antPos = itsAntPos[calInx];
  vector<Int>& mounts = itsMount[calInx];
  vector<MBaseline>& antMB = itsAntMB[calInx];
  vector<Vector<Double> >& antUvw = itsAntUvw[calInx];
  Block<Bool>& uvwFilled = itsUvwFilled[calInx];
  antPos.reserve (tab.nrow());
  mounts.reserve (tab.nrow());
  antMB.reserve  (tab.nrow());
  for (uInt i=antPos.size(); i<tab.nrow(); ++i) {
    String mount = mountCol(i);
    mount.downcase();
    Int mountType = 0;
    if (mount.size() >= 6  &&  mount(0,6) == "alt-az") {
      mountType = 1;
    }
    mounts.push_back (mountType);
    antPos.push_back (MPosition::Convert (posCol(i), MPosition::ITRF)());
    // Form an MBaseline per antenna (use first antenna as baseline origin).
    Vector<Double> pos  = antPos[i].getValue().getVector();
    Vector<Double> pos0 = antPos[0].getValue().getVector();
    MVPosition mvpos((pos[0] - pos0[0]),
                     (pos[1] - pos0[1]),
                     (pos[2] - pos0[2]));
    antMB.push_back (MBaseline (MVBaseline(mvpos), MBaseline::ITRF));
  }
  antUvw.resize    (antPos.size());
  uvwFilled.resize (antPos.size());
  uvwFilled = False;
}

void MSCalEngine::fillFieldDir (Int calDescId, Int calInx)
{
  // If direction is explicitly given, copy from the first one.
  if (!itsReadFieldDir) {
    if (calInx > 0) {
      itsFieldDir[calInx] = itsFieldDir[0];
    }
  } else {
    // Read the directions from the FIELD subtable.
    Table tab;
    if (itsCalCol.isNull()) {
      tab = itsTable.keywordSet().asTable("FIELD");
    } else {
      tab = getSubTable (calDescId, "FIELD");
    }
    ArrayMeasColumn<MDirection> dirCol(tab, itsDirColName);
    vector<MDirection>& fieldDir = itsFieldDir[calInx];
    fieldDir.reserve (tab.nrow());
    for (uInt i=fieldDir.size(); i<tab.nrow(); ++i) {
      // Get first value of MDirection array in this row.
      fieldDir.push_back (dirCol(i).data()[0]);
    }
  }
}

void MSCalEngine::fillCalDesc()
{
  // Fill the CAL_DESC info.
  // CAL_DESC contains rows with names of referenced MSs. The same MS can
  // occur multiple times (for different spwid).
  // Each MS has its own subtables (ANTENNA, FIELD, etc.).
  Table tab (itsTable.keywordSet().asTable("CAL_DESC"));
  ScalarColumn<String> nameCol(tab, "MS_NAME");
  // Handle CAL_DESC_IDs not seen so far.
  itsCalIdMap.reserve (tab.nrow());
  for (uInt i=itsCalIdMap.size(); i<tab.nrow(); ++i) {
    String msName = nameCol(i);
    Int inx = itsCalMap.size();
    map<string,int>::iterator iter = itsCalMap.find (msName);
    if (iter == itsCalMap.end()) {
      // New MS name, so add it.
      itsCalMap[msName] = inx;
    } else {
      inx = iter->second;
    }
    // Map this calDescId to inx.
    itsCalIdMap.push_back (inx);
  }
  // Resize others in case new entries added.
  itsAntPos.resize   (itsCalMap.size());
  itsMount.resize    (itsCalMap.size());
  itsAntMB.resize    (itsCalMap.size());
  itsAntUvw.resize   (itsCalMap.size());
  itsUvwFilled.resize(itsCalMap.size());
  itsFieldDir.resize (itsCalMap.size());
}

Table MSCalEngine::getSubTable (Int calDescId, const String& subTabName,
                                Bool mustExist)
{
  // If defined, open a subtable in the MS referred to by the name in the
  // MS_NAME column of the CAL_DESC subtable.
  Table calDescTab (itsTable.keywordSet().asTable("CAL_DESC"));
  ScalarColumn<String> nameCol(calDescTab, "MS_NAME");
  // If the path is relative, use the CalTable's directory.
  String msName = nameCol(calDescId);
  if (msName.empty()) {
    throw DataManError ("MSCalEngine: no MS name given in CAL_DESC table");
  }
  if (msName[0] != '/') {
    msName = Path(itsTable.tableName()).dirName() + '/' + msName;
  }
  Table ms(msName);
  if (ms.keywordSet().isDefined (subTabName)) {
    return ms.keywordSet().asTable(subTabName);
  }
  if (mustExist) {
    throw DataManError ("MSCalEngine: subtable " + subTabName +
                        " in CalTable's MS " + ms.tableName() + 
                        " does not exist");
  }
  return Table();
}

} //# end namespace
