//# DerivedMSCal.cc: Virtual column engine to return MS values
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

#include <derivedmscal/DerivedMC/DerivedMSCal.h>
#include <derivedmscal/DerivedMC/DerivedColumn.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/DataManError.h>
#include <measures/Measures/MeasTable.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MCPosition.h>
#include <measures/Measures/MCEpoch.h>
#include <measures/Measures/MCBaseline.h>
#include <measures/Measures/Muvw.h>
#include <measures/TableMeasures/ScalarMeasColumn.h>
#include <measures/TableMeasures/ArrayMeasColumn.h>
#include <casa/Containers/Record.h>
#include <casa/OS/Path.h>
#include <casa/Utilities/Assert.h>

using namespace casa;


namespace LOFAR {

DerivedMSCal::DerivedMSCal()
  : itsLastCalInx  (-1)
{}

DerivedMSCal::DerivedMSCal (const Record&)
  : itsLastCalInx  (-1)
{}

DerivedMSCal::DerivedMSCal (const DerivedMSCal&)
  : VirtualColumnEngine(),
    itsLastCalInx  (-1)
{}

DerivedMSCal::~DerivedMSCal()
{
  for (uInt i=0; i<ncolumn(); i++) {
    delete itsColumns[i];
  }
}

DataManager* DerivedMSCal::clone() const
{
  return new DerivedMSCal (*this);
}

String DerivedMSCal::dataManagerType() const
{
  return "DerivedMSCal";
}

Record DerivedMSCal::dataManagerSpec() const
{
  return Record();
}


DataManagerColumn* DerivedMSCal::makeScalarColumn (const String& name,
                                                   int,
                                                   const String&)
{
  DataManagerColumn* col;
  if (name == "HA") {
    col = new HourangleColumn(this, -1);   // array center position
  } else if (name == "HA1") {
    col = new HourangleColumn(this, 0);    // antenna1 position
  } else if (name == "HA2") {
    col = new HourangleColumn(this, 1);    // antenna2 position
  } else if (name == "LAST") {
    col = new LASTColumn(this, -1);
  } else if (name == "LAST1") {
    col = new LASTColumn(this, 0);
  } else if (name == "LAST2") {
    col = new LASTColumn(this, 1);
  } else if (name == "PA1") {
    col = new ParAngleColumn(this, 0);
  } else if (name == "PA2") {
    col = new ParAngleColumn(this, 1);
  } else {
    throw DataManError (name +
                        " is an unknown scalar column for DerivedMSCal");
  }
  itsColumns.push_back (col);
  return col;
}

DataManagerColumn* DerivedMSCal::makeIndArrColumn (const String& name,
                                                   int,
                                                   const String&)
{
  DataManagerColumn* col;
  if (name == "AZEL1") {
    col = new AzElColumn(this, 0);
  } else if (name == "AZEL2") {
    col = new AzElColumn(this, 1);
  } else if (name == "UVW_J2000") {
    col = new UVWJ2000Column(this);
  } else {
    throw DataManError (name +
                        " is an unknown array column for DerivedMSCal");
  }
  itsColumns.push_back (col);
  return col;
}

DataManager* DerivedMSCal::makeObject (const String&,
                                       const Record& spec)
{
  // This function is called when reading a table back.
  return new DerivedMSCal (spec);
}

void DerivedMSCal::registerClass()
{
  DataManager::registerCtor ("DerivedMSCal", makeObject);
}

Bool DerivedMSCal::canAddColumn() const
{
  return True;
}
Bool DerivedMSCal::canRemoveColumn() const
{
  return True;
}

void DerivedMSCal::addColumn (DataManagerColumn*)
{}
void DerivedMSCal::removeColumn (DataManagerColumn*)
{}

double DerivedMSCal::getHA (Int antnr, uInt rownr)
{
  setData (antnr, rownr);
  return itsRADecToHADec().getValue().get()[0];
}

double DerivedMSCal::getPA (Int antnr, uInt rownr)
{
  Int mount = setData (antnr, rownr);
  if (mount == 1) {
    // Do the conversions using the machines.
    return itsRADecToAzEl().getValue().positionAngle
      (itsPoleToAzEl().getValue());
  }
  return 0.;
}

double DerivedMSCal::getLAST (Int antnr, uInt rownr)
{
  setData (antnr, rownr);
  return itsUTCToLAST().getValue().get();
}

void DerivedMSCal::getAzEl (Int antnr, uInt rownr, Array<double>& data)
{
  setData (antnr, rownr);
  data = itsRADecToAzEl().getValue().get();
}

void DerivedMSCal::getUVWJ2000 (uInt rownr, Array<double>& data)
{
  setData (1, rownr);
  Int ant1 = itsAntCol[0](rownr);
  Int ant2 = itsAntCol[1](rownr);
  if (ant1 == ant2) {
    data = 0.;
  } else {
    vector<MDirection>& fieldDir    = itsFieldDir[itsLastCalInx];
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
        MVuvw jvguvw(bas, fieldDir[itsLastFieldId].getValue());
        antUvw[ant] = Muvw(jvguvw, Muvw::J2000).getValue().getVector();
        uvwFilled[ant] = true;
      }
      ant = ant2;
    }
    // The UVW of the baseline is the difference of the antennae UVW.
    data = antUvw[ant2] - antUvw[ant1];
  }
}

Int DerivedMSCal::setData (Int antnr, uInt rownr)
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
  // Also get mount type (alt-az or else).
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
  // Get the direction and put into the measure frame.
  // Get field id from the table; update the field positions if needed.
  Int fieldId = itsFieldCol(rownr);
  if (fieldId != itsLastFieldId) {
    if (fieldId >= Int(itsFieldDir[calInx].size())) {
      fillFieldDir (calDescId, calInx);
    }
    AlwaysAssert (fieldId < Int(itsFieldDir[calInx].size()), AipsError);
    const MDirection& dir = itsFieldDir[calInx][fieldId];
    itsRADecToAzEl.setModel (dir);
    itsRADecToHADec.setModel(dir);
    itsFrame.resetDirection (dir);
    itsLastFieldId = fieldId;
  }
  // Set the epoch in the measure frame.
  Double time = itsTimeCol(rownr);
  if (time != itsLastTime) {
    MEpoch epoch = itsTimeMeasCol(rownr);
    itsUTCToLAST.setModel (epoch);
    itsFrame.resetEpoch (epoch);
    itsLastTime = time;
    itsUvwFilled[calInx] = False;
  }
  return mount;
}

void DerivedMSCal::init()
{
  const TableDesc& td = table().tableDesc();
  itsLastFieldId = -1000;
  itsLastAntId   = -1000;
  itsLastTime    = -1e30;
  itsAntCol[0].attach (table(), "ANTENNA1");
  if (td.isColumn("ANTENNA2")) {
    itsAntCol[1].attach (table(), "ANTENNA2");
  } else {
    itsAntCol[1].attach (table(), "ANTENNA1");
  }
  itsFeedCol[0].attach (table(), "FEED1");
  if (td.isColumn("FEED2")) {
    itsFeedCol[1].attach (table(), "FEED2");
  } else {
    itsFeedCol[1].attach (table(), "FEED1");
  }
  itsFieldCol.attach (table(), "FIELD_ID");
  itsTimeCol.attach (table(), "TIME");
  itsTimeMeasCol.attach (table(), "TIME");
  Table obsTab;
  if (td.isColumn("CAL_DESC_ID")) {
    itsCalCol.attach (table(), "CAL_DESC_ID");
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
    itsFieldDir.resize (1);
    itsCalIdMap = vector<Int>(1,0);
    if (table().keywordSet().isDefined("OBSERVATION")) {
      obsTab = table().keywordSet().asTable("OBSERVATION");
    }
  }
  // Fill the antenna positions of the first CAL_DESC_ID.
  fillAntPos (0, 0);
  // Find observatory position.
  // If not found, set it to the position of the middle antenna.
  Bool fndObs = False;
  if (! obsTab.isNull()) {
    if (obsTab.nrow() > 0) {
      String telescope = ROScalarColumn<String>(obsTab, "TELESCOPE_NAME")(0);
      fndObs = MeasTable::Observatory (itsArrayPos, telescope);
    }
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
  // Idem UTC to LAST.
  itsUTCToLAST.set (MEpoch(), MEpoch::Ref(MEpoch::LAST,itsFrame));
  // Idem MBaseline ITRF to J2000.
  itsBLToJ2000.set (MBaseline(), MBaseline::Ref(MBaseline::J2000,itsFrame));
}

void DerivedMSCal::fillAntPos (Int calDescId, Int calInx)
{
  Table tab;
  if (itsCalCol.isNull()) {
    tab = table().keywordSet().asTable("ANTENNA");
  } else {
    tab = getSubTable (calDescId, "ANTENNA");
  }
  ROScalarMeasColumn<MPosition> posCol(tab, "POSITION");
  ROScalarColumn<String>      mountCol(tab, "MOUNT");
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

void DerivedMSCal::fillFieldDir (Int calDescId, Int calInx)
{
  Table tab;
  if (itsCalCol.isNull()) {
    tab = table().keywordSet().asTable("FIELD");
  } else {
    tab = getSubTable (calDescId, "FIELD");
  }
  ROArrayMeasColumn<MDirection> dirCol(tab, "DELAY_DIR");
  vector<MDirection>& fieldDir = itsFieldDir[calInx];
  fieldDir.reserve (tab.nrow());
  for (uInt i=fieldDir.size(); i<tab.nrow(); ++i) {
    // Get first value of MDirection array in this row.
    fieldDir.push_back (dirCol(i).data()[0]);
  }
}

void DerivedMSCal::fillCalDesc()
{
  // Fill the CAL_DESC info.
  // CAL_DESC contains rows with names of referenced MSs. The same MS can
  // occur multiple times (for different spwid).
  // Each MS has its own subtables (ANTENNA, FIELD, etc.).
  Table tab (table().keywordSet().asTable("CAL_DESC"));
  ROScalarColumn<String> nameCol(tab, "MS_NAME");
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

Table DerivedMSCal::getSubTable (Int calDescId, const String& subTabName,
                                 Bool mustExist)
{
  // If defined, open a subtable in the MS referred to by the name in the
  // MS_NAME column of the CAL_DESC subtable.
  Table calDescTab (table().keywordSet().asTable("CAL_DESC"));
  ROScalarColumn<String> nameCol(calDescTab, "MS_NAME");
  // The MS referred to must be in the same directory as the CalTable.
  Table ms (Path(table().tableName()).dirName() + '/' + nameCol(calDescId));
  if (ms.keywordSet().isDefined (subTabName)) {
    return ms.keywordSet().asTable(subTabName);
  }
  if (mustExist) {
    throw DataManError ("DerivedMSCal: subtable " + subTabName +
                        " in CalTable's MS " + ms.tableName() + 
                        " does not exist");
  }
  return Table();
}

} //# end namespace
