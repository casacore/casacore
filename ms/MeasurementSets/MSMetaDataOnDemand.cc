//# MSMetaData.cc
//# Copyright (C) 1998,1999,2000,2001
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

#include <ms/MeasurementSets/MSMetaDataOnDemand.h>

#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableParse.h>
#include <tables/Tables/TableProxy.h>

#include <iomanip>

// DEBUG ONLY
/*
#include <casa/Arrays/ArrayIO.h>
#include <casa/OS/PrecTimer.h>
*/

#define _ORIGIN "MSMetaDataOnDemand::" + String(__FUNCTION__) + ": "

namespace casa {

MSMetaDataOnDemand::MSMetaDataOnDemand(const MeasurementSet& ms)
	: _ms(ms) {}

MSMetaDataOnDemand::~MSMetaDataOnDemand() {}

uInt MSMetaDataOnDemand::nStates() const {
	return _getNStates(_ms);
}

std::set<String> MSMetaDataOnDemand::getIntents() const {
	vector<std::set<String> > statesToIntentsMap;
	std::set<String> uniqueIntents;
	_getStateToIntentsMap(
		statesToIntentsMap,
		uniqueIntents,
		_ms
	);
	return uniqueIntents;
}

std::set<uInt> MSMetaDataOnDemand::getScanNumbers() const {
	String taql = "select unique(SCAN_NUMBER) from " + _ms.tableName();
	Table result(tableCommand(taql));
	ROScalarColumn<Int> scanCol(result, "SCAN_NUMBER");
	vector<uInt> scans = _toUIntVector(scanCol.getColumn().tovector());
	return std::set<uInt>(scans.begin(), scans.end());
}

uInt MSMetaDataOnDemand::nScans() const {
	return getScanNumbers().size();
}

uInt MSMetaDataOnDemand::nVisibilities() const {
	return _ms.nrow();
}

std::set<uInt> MSMetaDataOnDemand::getScansForState(const uInt stateID) const {
	if (stateID >= nStates()) {
		throw AipsError(
			_ORIGIN + "Specified stateID exceeds the number of states for this dataset."
		);
	}
	std::map<uInt, std::set<uInt> > scanToStatesMap;
	std::set<uInt> uniqueScans;
	vector<uInt> scans = _getScans(_ms);
	scanToStatesMap = _getScanToStatesMap(scans, _getStates(_ms));
	uniqueScans.insert(scans.begin(), scans.end());
	std::set<uInt>::const_iterator lastScan = uniqueScans.end();
	std::set<uInt> scansForState;
	for (
		std::set<uInt>::const_iterator scanNum=uniqueScans.begin();
		scanNum!=lastScan; scanNum++
	) {
		std::set<uInt> statesSet = scanToStatesMap.find(*scanNum)->second;
		if (statesSet.find(stateID) != statesSet.end()) {
			scansForState.insert(*scanNum);
		}
	}
	return scansForState;
}

std::set<String> MSMetaDataOnDemand::getIntentsForScan(const uInt scan) const {
	_checkScan(scan, getScanNumbers());
	String taql = "select OBS_MODE from " + _ms.tableName() + "/STATE where "
		+ "ROWID() in [select unique(STATE_ID) from " + _ms.tableName()
		+ " where SCAN_NUMBER==" + String::toString(scan) + "]";
	Table result(tableCommand(taql));
	ROScalarColumn<String> intentsCol(result, "OBS_MODE");
	Vector<String> intents = intentsCol.getColumn();
	Vector<String>::const_iterator end = intents.end();
	std::set<String> intentsForScan;
	for (
		Vector<String>::const_iterator iter=intents.begin();
		iter!=end; iter++
	) {
		Vector<String> intentSet = casa::stringToVector(*iter, ',');
		intentsForScan.insert(intentSet.begin(), intentSet.end());
	}
	return intentsForScan;
}

std::set<uInt> MSMetaDataOnDemand::getSpwsForIntent(const String& intent) const {
	std::set<String> uniqueIntents;
	vector<std::set<String> > statesToIntentsMap;
	_getStateToIntentsMap(
		statesToIntentsMap,
		uniqueIntents, _ms
	);
	if (uniqueIntents.find(intent) == uniqueIntents.end()) {
		throw AipsError(
			_ORIGIN + "Unknown intent "
			+ intent + " for this dataset"
		);
	}
	vector<std::set<String> > spwToIntentsMap = _getSpwToIntentsMap();
	std::set<uInt> spws;
	for (uInt i=0; i<spwToIntentsMap.size(); i++) {
		if (
			spwToIntentsMap[i].find(intent) != spwToIntentsMap[i].end()
		) {
			spws.insert(i);
		}
	}
	return spws;
}

uInt MSMetaDataOnDemand::nSpw() const {
	return _ms.spectralWindow().nrow();
}

std::set<String> MSMetaDataOnDemand::getIntentsForSpw(const uInt spw) const {
	if (spw >= nSpw()) {
		throw AipsError(
			_ORIGIN + "spectral window out of range"
		);
	}
	return _getSpwToIntentsMap()[spw];
}

uInt MSMetaDataOnDemand::nFields() const {
	return _ms.field().nrow();
}


std::set<uInt> MSMetaDataOnDemand::getSpwsForField(const uInt fieldID) const {
	_checkFieldID(fieldID);
	String taql = "select unique(SPECTRAL_WINDOW_ID) from " + _ms.tableName()
		+ "/DATA_DESCRIPTION where ROWID() in "
		+ "[select unique(DATA_DESC_ID) from " + _ms.tableName()
		+ " where FIELD_ID == " + String::toString(fieldID) + "]";
	Table result(tableCommand(taql));
	ROScalarColumn<Int> spwCol(result, "SPECTRAL_WINDOW_ID");
	vector<uInt> spws = _toUIntVector(spwCol.getColumn().tovector());
	std::set<uInt> spwIds(spws.begin(), spws.end());
	return spwIds;
}

std::set<uInt> MSMetaDataOnDemand::getSpwsForField(const String& fieldName) const {
	uInt nFields = _ms.field().nrow();
	vector<String> fieldNames = _getFieldNames(_ms);

	for (uInt i=0; i<nFields; i++) {
		if (fieldNames[i] == fieldName) {
			return getSpwsForField(i);
		}
	}
	throw AipsError(
		_ORIGIN + "field (" + fieldName + " does not exist"
	);
}

std::set<uInt> MSMetaDataOnDemand::getFieldIDsForSpw(const uInt spw) const {
	if (spw >= _ms.spectralWindow().nrow()) {
		throw AipsError(_ORIGIN + "spectral window out of range");
	}
	String taql = "select unique(FIELD_ID) from " + _ms.tableName()
		+ " where DATA_DESC_ID in " +
		"[select ROWID() from ::DATA_DESCRIPTION where SPECTRAL_WINDOW_ID=="
		+ String::toString(spw) + "]";
	Table result(tableCommand(taql));
	ROScalarColumn<Int> fieldCol(result, "FIELD_ID");
	vector<uInt> fields = _toUIntVector(fieldCol.getColumn().tovector());
	std::set<uInt> fieldIds(fields.begin(), fields.end());
	return fieldIds;
}

std::set<String> MSMetaDataOnDemand::getFieldNamesForSpw(const uInt spw) const {
	std::set<uInt> fieldIDs = getFieldIDsForSpw(spw);
	std::set<String> fieldNames;
	vector<String> allFieldNames = _getFieldNames(_ms);
	for (
		std::set<uInt>::const_iterator fieldID = fieldIDs.begin();
		fieldID!=fieldIDs.end(); fieldID++
	) {
		fieldNames.insert(allFieldNames[*fieldID]);
	}
	return fieldNames;
}

std::set<uInt> MSMetaDataOnDemand::getSpwsForScan(const uInt scan) const {
	_checkScan(scan, getScanNumbers());
	String taql = "select unique(SPECTRAL_WINDOW_ID) from " + _ms.tableName()
		+ "/DATA_DESCRIPTION where ROWID() in "
		+ "[select unique(DATA_DESC_ID) from " + _ms.tableName()
		+ " where SCAN_NUMBER == " + String::toString(scan) + "]";
	Table result(tableCommand(taql));
	ROScalarColumn<Int> spwCol(result, "SPECTRAL_WINDOW_ID");
	vector<uInt> spws = _toUIntVector(spwCol.getColumn().tovector());
	std::set<uInt> spwIds(spws.begin(), spws.end());
	return spwIds;
}

std::set<uInt> MSMetaDataOnDemand::getScansForSpw(const uInt spw) const {
	if (spw >= nSpw()) {
		throw AipsError(
			_ORIGIN + "spectral window out of range"
		);
	}
	String taql = "select unique(SCAN_NUMBER) from " + _ms.tableName()
		+ " where DATA_DESC_ID in " +
		"[select ROWID() from ::DATA_DESCRIPTION where SPECTRAL_WINDOW_ID=="
		+ String::toString(spw) + "]";
	Table result(tableCommand(taql));
	ROScalarColumn<Int> scanCol(result, "SCAN_NUMBER");
	vector<uInt> scans = _toUIntVector(scanCol.getColumn().tovector());
	std::set<uInt> scanIds(scans.begin(), scans.end());
	return scanIds;
}

uInt MSMetaDataOnDemand::nAntennas() const {
	return _ms.antenna().nrow();
}

vector<String> MSMetaDataOnDemand::getAntennaNames(
	const vector<uInt>& antennaIDs
) const {
	std::map<String, uInt> mymap;
	vector<String> myAnts = _getAntennaNames(mymap, _ms);
	uInt nAnts = myAnts.size();
	vector<uInt>::const_iterator end = antennaIDs.end();
	vector<String> names;
	for (
		vector<uInt>::const_iterator id=antennaIDs.begin();
		id!=end; id++
	) {
		if (*id >= nAnts) {
			throw AipsError(
				_ORIGIN + "Antenna ID " + String::toString(*id)
				+ " out of range."
			);
		}
		names.push_back(myAnts[*id]);
	}
	return names;
}

vector<uInt> MSMetaDataOnDemand::getAntennaIDs(
	const vector<String>& antennaNames
) const {
	std::map<String, uInt> namesToIDsMap;
	vector<String> names = _getAntennaNames(namesToIDsMap, _ms);
	vector<String>::const_iterator end = antennaNames.end();
	std::map<String, uInt>::const_iterator mapEnd = namesToIDsMap.end();
	vector<uInt> ids;
	for (
		vector<String>::const_iterator name=antennaNames.begin();
		name!=end; name++
	) {
		std::map<String, uInt>::const_iterator pair = namesToIDsMap.find(*name);
		if (pair == mapEnd) {
			throw AipsError(
				_ORIGIN + "Unknown antenna " + *name
			);
		}
		ids.push_back(pair->second);
	}
	return ids;
}

std::set<uInt> MSMetaDataOnDemand::getTDMSpw() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, _ms);
	return tdmSpw;
}

std::set<uInt> MSMetaDataOnDemand::getFDMSpw() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, _ms);
	return fdmSpw;
}

std::set<uInt> MSMetaDataOnDemand::getChannelAvgSpw() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, _ms);
	return avgSpw;
}

std::set<uInt> MSMetaDataOnDemand::getWVRSpw() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, _ms);
	return wvrSpw;
}

std::set<uInt> MSMetaDataOnDemand::getScansForTimes(
	const Double center, const Double tol
) const {
	_checkTolerance(tol);
	vector<uInt> allScans = _getScans(_ms);
	std::set<uInt> uniqueScans;
	uniqueScans.insert(allScans.begin(), allScans.end());
	std::map<uInt, std::set<Double> > scanToTimesMap = _getScanToTimesMap(allScans, _getTimes(_ms));
	Double minTime = center - tol;
	Double maxTime = center + tol;
	std::set<uInt> scans;

	std::set<uInt>::const_iterator end = uniqueScans.end();
	for (
		std::set<uInt>::const_iterator scan=uniqueScans.begin();
		scan!=end; scan++
	) {
		std::set<Double> times = scanToTimesMap.find(*scan)->second;
		if (*(++times.rend()) >= minTime && *times.begin() <= maxTime) {
			scans.insert(*scan);
		}
	}
	return scans;
}

std::set<Double> MSMetaDataOnDemand::getTimesForScans(
	const std::set<uInt> scans
) const {
	std::set<Double> times;
	std::map<uInt, std::set<Double> > scanToTimesMap = _getScanToTimesMap(
		_getScans(_ms), _getTimes(_ms)
	);
	for (
		std::set<uInt>::const_iterator scan=scans.begin();
		scan!=scans.end(); scan++
	) {
		_checkScan(*scan, getScanNumbers());
		times.insert(
			scanToTimesMap.find(*scan)->second.begin(),
			scanToTimesMap.find(*scan)->second.end()
		);
	}
	return times;
}

std::set<uInt> MSMetaDataOnDemand::getStatesForScan(const uInt scan) const {
	const vector<uInt> scans = _getScans(_ms);
	_checkScan(scan, getScanNumbers());
	return _getScanToStatesMap(scans, _getStates(_ms)).find(scan)->second;
}

std::set<uInt> MSMetaDataOnDemand::getScansForIntent(const String& intent) const {
	String taql = "select unique(SCAN_NUMBER) from " + _ms.tableName()
		+ " where STATE_ID in "
		+ "[select ROWID() from ::STATE where OBS_MODE=pattern('*" + intent + "*')]";
	Table result(tableCommand(taql));
	ROScalarColumn<Int> scanCol(result, "SCAN_NUMBER");
	vector<uInt> scans = _toUIntVector(scanCol.getColumn().tovector());
	return std::set<uInt>(scans.begin(), scans.end());
}

std::set<uInt> MSMetaDataOnDemand::getScansForFieldID(const uInt fieldID) const {
	if (fieldID >= _ms.field().nrow()) {
		throw AipsError(
			_ORIGIN + "No such field ID " + String::toString(fieldID)
		);
	}
	std::set<uInt> scans;
	uInt count = 0;
	vector<uInt> fieldIds = _getFieldIDs(_ms);
	vector<uInt>::const_iterator end = fieldIds.end();
	vector<uInt> allScans = _getScans(_ms);
	for (
		vector<uInt>::const_iterator curFieldID=fieldIds.begin();
		curFieldID!=end; curFieldID++, count++
	) {
		if (fieldID == *curFieldID) {
			scans.insert(allScans[count]);
		}
	}
	return scans;
}

std::set<uInt> MSMetaDataOnDemand::getFieldIDsForField(
	const String& field
) const {
	std::set<uInt> fieldIDs;
	String name = field;
	vector<String> fieldNames = _getFieldNames(_ms);
	uInt nNames = fieldNames.size();
	name.upcase();
	for (uInt i=0; i<nNames; i++) {
		String testName = fieldNames[i];
		testName.upcase();
		if (name == testName) {
			fieldIDs.insert(i);
		}
	}
	if (fieldIDs.empty()) {
		throw AipsError(
			_ORIGIN + "Unknown field name " + field
		);
	}
	return fieldIDs;
}

std::set<uInt> MSMetaDataOnDemand::getFieldsForScan(const uInt scan) const {
	vector<uInt> allScans = _getScans(_ms);
	_checkScan(scan, getScanNumbers());
	String taql = "select unique(FIELD_ID) from " + _ms.tableName()
		+ " where SCAN_NUMBER == " + String::toString(scan);
	Table result(tableCommand(taql));
	ROScalarColumn<Int> fieldCol(result, "FIELD_ID");
	vector<uInt> fields = _toUIntVector(fieldCol.getColumn().tovector());
	return std::set<uInt>(fields.begin(), fields.end());
}

std::set<uInt> MSMetaDataOnDemand::getFieldsForScans(const std::set<uInt>& scans) const {
	std::set<uInt> uniqueScans = getScanNumbers();
	String scanString;
	std::set<uInt>::const_iterator end = scans.end();
	for (
		std::set<uInt>::const_iterator iter=scans.begin();
		iter!=end; iter++
	) {
		_checkScan(*iter, uniqueScans);
		if (iter!=scans.begin()) {
			scanString += ", ";
		}
		scanString += String::toString(*iter);
	}
	String taql = "select unique(FIELD_ID) from " + _ms.tableName()
		+ " where SCAN_NUMBER in [" + scanString + "]";
	Table result(tableCommand(taql));
	ROScalarColumn<Int> fieldCol(result, "FIELD_ID");
	vector<uInt> fields = _toUIntVector(fieldCol.getColumn());
	return std::set<uInt>(fields.begin(), fields.end());
}

std::set<uInt> MSMetaDataOnDemand::getFieldsForIntent(const String& intent) const {
	String taql = "select unique(FIELD_ID) from " + _ms.tableName()
		+ " where STATE_ID in "
		+ "[select ROWID() from ::STATE "
		+ "where OBS_MODE=pattern('*" + intent + "*')]";
	Table result(tableCommand(taql));
	ROScalarColumn<Int> fieldCol(result, "FIELD_ID");
	vector<uInt> fields = _toUIntVector(fieldCol.getColumn().tovector());
	return std::set<uInt>(fields.begin(), fields.end());
}

vector<String> MSMetaDataOnDemand::getFieldNamesForFieldIDs(
	const vector<uInt>& fieldIDs
) const {
	_checkFieldIDs(fieldIDs);
	if (fieldIDs.size() == 0) {
		return _getFieldNames(_ms);
	}
	vector<String> allNames = _getFieldNames(_ms);
	vector<String> names;
	vector<uInt>::const_iterator end = fieldIDs.end();
	for (
		vector<uInt>::const_iterator iter=fieldIDs.begin();
		iter!=end; iter++
	) {
		names.push_back(allNames[*iter]);
	}
	return names;
}

std::set<uInt> MSMetaDataOnDemand::getFieldsForTimes(
	const Double center, const Double tol
) const {
	_checkTolerance(tol);
	Double minTime = center - tol;
	Double maxTime = center + tol;
	ostringstream timeString;
	timeString << std::setprecision(12) << minTime << " and " << maxTime;
	String taql = "select FIELD_ID from " + _ms.tableName()
		+ " where TIME BETWEEN " + String(timeString.str());
	Table result(tableCommand(taql));
	ROScalarColumn<Int> fieldCol(result, "FIELD_ID");
	vector<uInt> fields = _toUIntVector(fieldCol.getColumn().tovector());
	return std::set<uInt>(fields.begin(), fields.end());
}

std::set<Double> MSMetaDataOnDemand::getTimesForField(const uInt fieldID) const {
	if (fieldID >= _ms.field().nrow()) {
		throw AipsError(
			_ORIGIN + "No such field ID " + String::toString(fieldID)
		);
	}
	String taql = "select unique(TIME) from " + _ms.tableName()
		+ " where FIELD_ID=" + String::toString(fieldID);
	Table result(tableCommand(taql));
	ROScalarColumn<Double> timeCol(result, "TIME");
	Vector<double> times = timeCol.getColumn();
	return std::set<Double>(times.begin(), times.end());
}

vector<String> MSMetaDataOnDemand::getObservatoryNames() const {
	String tnameColName = MSObservation::columnName(MSObservationEnums::TELESCOPE_NAME);
	ROScalarColumn<String> telescopeNameCol(_ms.observation(), tnameColName);
	return telescopeNameCol.getColumn().tovector();
}

MPosition MSMetaDataOnDemand::getObservatoryPosition(uInt which) const {
	if (which >= _ms.observation().nrow()) {
		throw AipsError(_ORIGIN + " out of range exception.");
	}
	vector<String> names;
	return _getObservatoryPositions(names, _ms)[which];
}

vector<MPosition> MSMetaDataOnDemand::getAntennaPositions(
	const vector<uInt>& which
) const {
	vector<String> antNames;
	if (which.size() == 0) {
		return _getAntennaPositions(antNames, _ms);
	}
	if (max(Vector<uInt>(which)) >= _ms.antenna().nrow()) {
		throw AipsError(_ORIGIN + "out of range exception.");
	}
	vector<MPosition> output;
	vector<uInt>::const_iterator end = which.end();
	vector<MPosition> pos = _getAntennaPositions(antNames, _ms);
	for (
		vector<uInt>::const_iterator iter=which.begin();
		iter!=end; iter++
	) {
		output.push_back(pos[*iter]);
	}
	return output;
}

vector<MPosition> MSMetaDataOnDemand::getAntennaPositions(
	const vector<String>& names
) const {
	if (names.size() == 0) {
		throw AipsError(_ORIGIN + "names cannot be empty");
	}
	return getAntennaPositions(getAntennaIDs(names));
}

Quantum<Vector<Double> > MSMetaDataOnDemand::getAntennaOffset(uInt which) const {
	if (which >= _ms.antenna().nrow()) {
		throw AipsError(_ORIGIN + "Out of range exception.");
	}
	vector<String> antNames, obsNames;
	return _getAntennaOffsets(
		_getAntennaPositions(antNames, _ms),
		_getObservatoryPositions(obsNames, _ms)[0]
	)[which];
}

vector<Quantum<Vector<Double> > > MSMetaDataOnDemand::getAntennaOffsets(
	const vector<MPosition>& positions
) const {
	uInt nPos = positions.size();
	vector<String> names;
	vector<String> obsNames;
	if (nPos > 0) {
		if (nPos != nAntennas()) {
			throw AipsError(_ORIGIN + "Incorrect number of positions provided.");
		}
		return _getAntennaOffsets(
			positions,
			_getObservatoryPositions(obsNames, _ms)[0]
		);
	}
	return _getAntennaOffsets(
		_getAntennaPositions(names, _ms),
		_getObservatoryPositions(obsNames, _ms)[0]
	);
}

Quantum<Vector<Double> > MSMetaDataOnDemand::getAntennaOffset(
	const String& name
) const {
	vector<String> names(1);
	names[0] = name;
	return getAntennaOffset(getAntennaIDs(names)[0]);
}

vector<std::set<uInt> > MSMetaDataOnDemand::_getSpwToFieldMap() {
	vector<std::set<uInt> > spwToFieldIDsMap(_ms.spectralWindow().nrow());
	uInt count = 0;
	vector<uInt> dataDescIDs = _getDataDescIDs(_ms);
	vector<uInt> dataDescToSpwMap = _getDataDescIDToSpwMap(_ms);
	vector<uInt> fieldIds = _getFieldIDs(_ms);
	for (
		vector<uInt>::const_iterator dataDescID=dataDescIDs.begin();
		dataDescID!=dataDescIDs.end(); dataDescID++, count++
	) {
		Int spw = dataDescToSpwMap[*dataDescID];
		spwToFieldIDsMap[spw].insert(fieldIds[count]);
	}
	return spwToFieldIDsMap;
}

uInt MSMetaDataOnDemand::_getNumberOfPolarizations() const {
	Int myscan = -1;
	std::set<uInt> uniqueScans = getScanNumbers();
	for (
		std::set<uInt>::const_iterator scanNum=uniqueScans.begin();
		scanNum!=uniqueScans.end(); scanNum++
	) {
		std::set<String> intents = getIntentsForScan(*scanNum);
		for (
			std::set<String>::const_iterator intent=intents.begin();
			intent!=intents.end(); intent++
		) {
			if (intent->contains("OBSERVE_TARGET")) {
				myscan = *scanNum;
				break;
			}
		}
		if (myscan >= 0) {
			break;
		}
	}
	if (myscan < 0) {
		// if there is no OBSERVE_TARGET, then just use the first scan
		myscan = 0;
	}

	String dataName, correctedName, modelName;
	_getDataColumnNames(
		dataName, correctedName,
		modelName
	);
	ROArrayColumn<Complex> dataCol(
		_ms, dataName
	);
	Array<Complex> arr;
	uInt nPolarizations;
	if (myscan == 0) {
		// assume the first row in the table is for the first scan, to save time
		dataCol.get(0, arr);
		nPolarizations = arr.shape()[0];
	}
	else {
		vector<uInt> _scans = _getScans(_ms);
		nPolarizations = 0;
		uInt i = 0;
		for (
			vector<uInt>::const_iterator scanNum=_scans.begin();
			scanNum!=_scans.end(); scanNum++, i++
		) {
			if ((Int)*scanNum == myscan) {
				dataCol.get(i, arr);
				nPolarizations = arr.shape()[0];
				break;
			}
		}
	}
	return nPolarizations;
}

void MSMetaDataOnDemand::_getDataColumnNames(
	String& dataName, String& correctedName,
	String& modelName
) const {
	dataName = "";
	correctedName = "";
	modelName = "";
	TableProxy t(_ms);
	Vector<String> colNames = t.columnNames();
	std::set<String> cNames(colNames.begin(), colNames.end());
	std::set<String>::const_iterator end = cNames.end();
	if (cNames.find("FLOAT_DATA") != end) {
		dataName = "FLOAT_DATA";
		correctedName = "FLOAT_DATA";
	}
	else if (cNames.find("DATA") != end) {
		dataName = "DATA";
	}
	if (cNames.find("CORRECTED_DATA") != end) {
		correctedName = "CORRECTED_DATA";
	}
	if (cNames.find("MODEL_DATA") != end) {
		modelName = "MODEL_DATA";
	}
}

vector<std::set<uInt> > MSMetaDataOnDemand::_getSpwToScansMap() {
	vector<std::set<uInt> > spwToScansMap(nSpw());
	uInt count = 0;
	vector<uInt> dataDescIDs = _getDataDescIDs(_ms);
	vector<uInt>::const_iterator end = dataDescIDs.end();
	vector<uInt> dataDescToSpwMap = _getDataDescIDToSpwMap(_ms);
	vector<uInt> scans = _getScans(_ms);
	for (
		vector<uInt>::const_iterator iter=dataDescIDs.begin();
		iter!=end; iter++, count++
	) {
		Int spw = dataDescToSpwMap[*iter];
		spwToScansMap[spw].insert(scans[count]);
	}
	return spwToScansMap;
}

vector<std::set<String> > MSMetaDataOnDemand::_getSpwToIntentsMap() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, _ms);
	std::set<String> emptySet;
	vector<SpwProperties>::const_iterator end = spwInfo.end();
	vector<std::set<String> > spwToIntentsMap;
	for (
		vector<SpwProperties>::const_iterator iter=spwInfo.begin();
		iter!=end; iter++
	) {
		spwToIntentsMap.push_back(emptySet);
	}
	vector<std::set<String> > stateToIntentsMap;
	std::set<String> uniqueIntents;
	_getStateToIntentsMap(stateToIntentsMap, uniqueIntents, _ms);
	if (uniqueIntents.size() == 0) {
		return spwToIntentsMap;
	}
	uInt count = 0;
	std::set<String> intents;
	uInt spw;
	std::map<uInt, std::set<uInt> > checkedMap;
	vector<uInt> dataDescIDs = _getDataDescIDs(_ms);
	vector<uInt>::const_iterator endDDID = dataDescIDs.end();
	vector<uInt> states = _getStates(_ms);
	vector<uInt> dataDescToSpwMap = _getDataDescIDToSpwMap(_ms);
	for (
		vector<uInt>::const_iterator curDDID=dataDescIDs.begin();
		curDDID!=endDDID; curDDID++, count++
	) {
		uInt curState = states[count];
		spw = dataDescToSpwMap[*curDDID];
		intents = stateToIntentsMap[curState];
		spwToIntentsMap[spw].insert(intents.begin(), intents.end());
		checkedMap[*curDDID].insert(curState);
	}
	return spwToIntentsMap;
}

void MSMetaDataOnDemand::_checkScan(const uInt scan, const std::set<uInt> uniqueScans) {
	if (uniqueScans.find(scan) == uniqueScans.end()) {
		throw AipsError(
			_ORIGIN + "Unknown scan number " + String::toString(scan)
		);
	}
}

void MSMetaDataOnDemand::_checkFieldID(const uInt fieldID) const {
	if (fieldID >= _ms.field().nrow()) {
		throw AipsError(
			_ORIGIN + "field ID ("
			+ String::toString(fieldID) + ") out of range"
		);
	}
}

void MSMetaDataOnDemand::_checkFieldIDs(const vector<uInt>& fieldIDs) const {
	if (fieldIDs.size() > 0) {
		if (uInt myMax = max(Vector<uInt>(fieldIDs)) >= _ms.field().nrow()) {
			throw AipsError(
				_ORIGIN + "At least one field ID (" + String::toString(myMax)
					+ ") is out of range"
			);
		}
	}
}


std::set<uInt> MSMetaDataOnDemand::_getUnique(const vector<uInt>& v) {
	std::set<uInt> ret;
	ret.insert(v.begin(), v.end());
	return ret;
}

}

