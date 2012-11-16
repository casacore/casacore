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

#include <ms/MeasurementSets/MSMetaData.h>

#include <casa/Utilities/GenSort.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableProxy.h>

// DEBUG ONLY
#include <casa/Arrays/ArrayIO.h>
#include <iomanip>
#include <casa/OS/PrecTimer.h>

namespace casa {

MSMetaData::MSMetaData(const MeasurementSet& ms)
	: _scans(), _uniqueScans(), _times(),
	  _scanToTimesMap() {
	_makeScanToTimeMap(ms);
	_makeFieldsAndSources(ms);
	_makeStateToIntentsMap(ms);
	_makeAntennaNames(ms);
	_makeScanToStateMap(ms);
	_makeFieldNameToTimesMap(ms);
	_makeBaselineToTimesMap(ms);
	_makeBaselineToStatesMap();
	_makeDataDescID(ms);
	_makeDataDescIDToSpwMap(ms);
	_setNumberOfPolarizations(ms);
	_setSpwInfo(ms);
	_makeSpwToScanMap();
	_makeSpwToFieldMap();
	_makeSpwToIntentsMap();
}

uInt MSMetaData::nStates() const {
	return _nStates;
}

std::set<String> MSMetaData::getIntents() const {
	return _uniqueIntents;
}

std::set<uInt> MSMetaData::getScanNumbers() const {
	return _uniqueScans;
}

uInt MSMetaData::nScans() const {
	return _uniqueScans.size();
}

uInt MSMetaData::nVisibilities() const {
	return _scans.size();
}

std::set<uInt> MSMetaData::getScansForState(const uInt stateID) const {
	if (stateID >= _nStates) {
		throw AipsError(
			"MSMetaData::getIntentsForScan: Specified stateID exceeds the number of states for this dataset."
		);
	}
	std::set<uInt> scansForState;
	for (
		std::set<uInt>::const_iterator scanNum=_uniqueScans.begin();
		scanNum!=_uniqueScans.end(); scanNum++
	) {
		std::set<uInt> statesSet = _scanToStatesMap.find(*scanNum)->second;
		if (statesSet.find(stateID) != statesSet.end()) {
			scansForState.insert(*scanNum);
		}
	}
	return scansForState;
}

std::set<String> MSMetaData::getIntentsForScan(const uInt scan) const {
	_checkScan(scan);
	std::set<String> intentsForScan;
	Int i=0;
	for (
		vector<std::set<String> >::const_iterator iter=_stateToIntentsMap.begin();
		iter!=_stateToIntentsMap.end(); iter++, i++
	) {
		std::set<String> subIntents = *iter;
		std::set<uInt> scans = getScansForState(i);
		if (scans.find(scan) != scans.end()) {
			for (
				std::set<String>::const_iterator intent=subIntents.begin();
				intent!=subIntents.end(); intent++
			) {
				intentsForScan.insert(*intent);
			}
		}
	}
	return intentsForScan;
}

std::set<uInt> MSMetaData::getSpwsForIntent(const String& intent) const {
	if (_uniqueIntents.find(intent) == _uniqueIntents.end()) {
		throw AipsError(
			"MSMetaData::getSpwsForIntent: Unknown intent "
			+ intent + " for this dataset"
		);
	}
	std::set<uInt> spws;
	for (uInt i=0; i<_spwToIntentsMap.size(); i++) {
		if (
			_spwToIntentsMap[i].find(intent) != _spwToIntentsMap[i].end()
		) {
			spws.insert(i);
		}
	}
	return spws;
}

uInt MSMetaData::nSpw() const {
	return _spwInfo.size();
}

std::set<String> MSMetaData::getIntentsForSpw(const uInt spw) const {
	if (spw >= nSpw()) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__) + " : spectral window out of range"
		);
	}
	return _spwToIntentsMap[spw];
}

uInt MSMetaData::nFields() const {
	return _nFields;
}

std::set<uInt> MSMetaData::getSpwsForField(const uInt fieldID) const {
	_checkFieldID(fieldID);
	std::set<uInt> spws;
	uInt count = 0;
	for (
		vector<std::set<uInt> >::const_iterator fieldIDs=_spwToFieldIDsMap.begin();
		fieldIDs!=_spwToFieldIDsMap.end(); fieldIDs++, count++
	) {
		if (fieldIDs->find(fieldID) != fieldIDs->end()) {
			spws.insert(count);
		}
	}
	return spws;
}

std::set<uInt> MSMetaData::getSpwsForField(const String& fieldName) const {
	for (uInt i=0; i<_fieldNames.size(); i++) {
		if (_fieldNames[i] == fieldName) {
			return getSpwsForField(i);
		}
	}
	throw AipsError(
		"MSMetaData::" + String(__FUNCTION__) + " : field ("
			+ fieldName + " does not exist"
	);
}

std::set<uInt> MSMetaData::getFieldIDsForSpw(const uInt spw) const {
	if (spw >= _spwInfo.size()) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__)
			+ " : spectral window out of range"
		);
	}
	return _spwToFieldIDsMap[spw];
}

std::set<String> MSMetaData::getFieldNamesForSpw(const uInt spw) const {
	std::set<uInt> fieldIDs = getFieldIDsForSpw(spw);
	std::set<String> fieldNames;
	for (
		std::set<uInt>::const_iterator fieldID = fieldIDs.begin();
		fieldID!=fieldIDs.end(); fieldID++
	) {
		fieldNames.insert(this->_fieldNames[*fieldID]);
	}
	return fieldNames;
}

std::set<uInt> MSMetaData::getSpwsForScan(const uInt scan) const {
	_checkScan(scan);
	std::set<uInt> spws;
	uInt count = 0;
	for (
		vector<std::set<uInt> >::const_iterator scanSet=_spwToScansMap.begin();
		scanSet!=_spwToScansMap.end(); scanSet++, count++
	) {
		if (scanSet->find(scan) != scanSet->end()) {
			spws.insert(count);
		}
	}
	return spws;
}

std::set<uInt> MSMetaData::getScansForSpw(const uInt spw) const {
	if (spw >= nSpw()) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__)
			+ " : spectral window out of range"
		);
	}
	return _spwToScansMap[spw];
}

uInt MSMetaData::nAntennas() const {
	return _antennaNames.size();
}

String MSMetaData::getAntennaName(const uInt antennaID) const {
	if (antennaID >= nAntennas()) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__)
			+ " : antennaID out of range"
		);
	}
	return _antennaNames[antennaID];
}

uInt MSMetaData::getAntennaID(const String& antennaName) const {
	for (Int i=0; i<(Int)nAntennas(); i++) {
		if (antennaName == _antennaNames[i]) {
			return i;
		}
	}
	throw AipsError(
		"MSMetaData::" + String(__FUNCTION__)
		+ " : Unknown antenna " + antennaName
	);
}

std::set<uInt> MSMetaData::getTDMSpw() const {
	return _tdmspw;
}

std::set<uInt> MSMetaData::getFDMSpw() const {
	return _fdmspw;
}

std::set<uInt> MSMetaData::getChannelAvgSpw() const {
	return _avgspw;
}

std::set<uInt> MSMetaData::getWVRSpw() const {
	return _wvrspw;
}

std::set<uInt> MSMetaData::getScansForTimes(
	const Double center, const Double tol
) const {
	_checkTolerance(tol);
	Double minTime = center - tol;
	Double maxTime = center + tol;
	std::set<uInt> scans;
	for (
		std::set<uInt>::const_iterator scan=_uniqueScans.begin();
		scan!=_uniqueScans.end(); scan++
	) {
		std::set<Double> times = _scanToTimesMap.find(*scan)->second;
		if (*(++times.rend()) >= minTime && *times.begin() <= maxTime) {
			scans.insert(*scan);
		}
	}
	return scans;
}

std::set<Double> MSMetaData::getTimesForScans(
	const std::set<uInt> scans
) const {
	std::set<Double> times;
	for (
		std::set<uInt>::const_iterator scan=scans.begin();
		scan!=scans.end(); scan++
	) {
		_checkScan(*scan);
		times.insert(
			_scanToTimesMap.find(*scan)->second.begin(),
			_scanToTimesMap.find(*scan)->second.end()
		);
	}
	return times;
}

std::set<Double> MSMetaData::getTimesForScan(const uInt scan) const {
	std::set<uInt> scans;
	scans.insert(scan);
	// scan validity check is done in getTimesForScans()
	return getTimesForScans(scans);
}

std::set<uInt> MSMetaData::getStatesForScan(const uInt scan) const {
	_checkScan(scan);
	return _scanToStatesMap.find(scan)->second;
}

std::set<uInt> MSMetaData::getScansForIntent(const String& intent) const {
	if (_uniqueIntents.find(intent) == _uniqueIntents.end()) {
		throw AipsError(
			"Unknown intent " + intent
			+ " or it has no associated states"
		);
	}
	std::set<uInt> scans;
	for (
		std::set<uInt>::const_iterator scanNum=_uniqueScans.begin();
		scanNum!=_uniqueScans.end(); scanNum++
	) {
		std::set<String> testIntents = getIntentsForScan(*scanNum);
		if (testIntents.find(intent) != testIntents.end()) {
			scans.insert(*scanNum);
		}
	}
	return scans;
}

std::set<uInt> MSMetaData::getScansForFieldID(const uInt fieldID) const {
	if (fieldID >= _nFields) {
		throw AipsError(
			"No such field ID " + String::toString(fieldID)
		);
	}
	std::set<uInt> scans;
	uInt count = 0;
	// FIXME full column scan, make a map instead at construction
	for (
		vector<uInt>::const_iterator curFieldID=_fieldIds.begin();
		curFieldID!=_fieldIds.end(); curFieldID++, count++
	) {
		if (fieldID == *curFieldID) {
			scans.insert(_scans[count]);
		}
	}
	return scans;
}

std::set<uInt> MSMetaData::getFieldIDsForField(
	const String& field
) const {
	std::set<uInt> fieldIDs;
	String name = field;
	name.upcase();
	for (uInt i=0; i<_fieldNames.size(); i++) {
		String testName = _fieldNames[i];
		testName.upcase();
		if (name == testName) {
			fieldIDs.insert(i);
		}
	}
	if (fieldIDs.empty()) {
		throw AipsError(
			"Unknown field name " + field
		);
	}
	return fieldIDs;
}

std::set<uInt> MSMetaData::getScansForField(const String& field) const {
	std::set<uInt> fieldIDs = getFieldIDsForField(field);
	std::set<uInt> scans;
	for (
		std::set<uInt>::const_iterator fieldID=fieldIDs.begin();
		fieldID!=fieldIDs.end(); fieldID++
	) {
		std::set<uInt> myscans = getScansForFieldID(*fieldID);
		scans.insert(myscans.begin(), myscans.end());
	}
	return scans;
}

std::set<uInt> MSMetaData::getFieldsForScan(const uInt scan) const {
	_checkScan(scan);
	std::set<uInt> fields;
	vector<uInt>::const_iterator curScan = _scans.begin();
	vector<uInt>::const_iterator curField = _fieldIds.begin();
	while (curScan != _scans.end()) {
		if (*curScan == scan) {
			fields.insert(*curField);
		}
		curScan++;
		curField++;
	}
	return fields;
}

std::set<uInt> MSMetaData::getFieldsForScans(const std::set<uInt>& scans) const {
	std::set<uInt> fields;
	for (
		std::set<uInt>::const_iterator curScan=scans.begin();
		curScan!=scans.end(); curScan++
	) {
		std::set<uInt> myFieldIDs = getFieldsForScan(*curScan);
		fields.insert(myFieldIDs.begin(), myFieldIDs.end());
	}
	return fields;
}

std::set<uInt> MSMetaData::getFieldsForIntent(const String& intent) const {
	std::set<uInt> scans = getScansForIntent(intent);
	return getFieldsForScans(scans);
}

String MSMetaData::getFieldNameForFieldID(const uInt fieldID) const {
	_checkFieldID(fieldID);
	return this->_fieldNames[fieldID];
}

std::set<uInt> MSMetaData::getFieldsForTimes(
	const Double center, const Double tol
) const {
	_checkTolerance(tol);
	Double minTime = center - tol;
	Double maxTime = center + tol;
	std::set<uInt> fieldIDs;
	vector<Double>::const_iterator time=_times.begin();
	vector<uInt>::const_iterator fieldID=_fieldIds.begin();
	while (time != _times.end()) {
		if (*time >= minTime && *time <= maxTime) {
			fieldIDs.insert(*fieldID);
		}
		time++;
		fieldID++;
	}
	return fieldIDs;
}

std::set<Double> MSMetaData::getTimesForField(const uInt fieldID) const {
	this->_checkFieldID(fieldID);
	return _fieldNameToTimesMap.find(getFieldNameForFieldID(fieldID))->second;
}


void MSMetaData::_makeFieldsAndSources(const MeasurementSet& ms) {
	String fieldNameColName = MSTable<
			MSFieldEnums::PredefinedColumns,
			MSFieldEnums::PredefinedKeywords
		>::columnName(MSFieldEnums::NAME);
	ROScalarColumn<String> nameCol(ms.field(), fieldNameColName);
	nameCol.getColumn().tovector(_fieldNames);
	_nFields = _fieldNames.size();
}

void MSMetaData::_makeAntennaNames(const MeasurementSet& ms) {
	String antNameColName = MSTable<
			MSAntennaEnums::PredefinedColumns,
			MSAntennaEnums::PredefinedKeywords
		>::columnName(MSAntennaEnums::NAME);
	ROScalarColumn<String> nameCol(ms.antenna(), antNameColName);
	nameCol.getColumn().tovector(_antennaNames);
}

void MSMetaData::_makeScanToStateMap(const MeasurementSet& ms) {
	// _scans should be set before this method is called
	String stateColName = MSTable<
			MSMainEnums::PredefinedColumns,
			MSMainEnums::PredefinedKeywords
		>::columnName(MSMainEnums::STATE_ID);
	ROScalarColumn<Int> stateCol(ms, stateColName);
	_states = _toUIntVector(stateCol.getColumn());
	vector<uInt>::const_iterator curScan = _scans.begin();
	vector<uInt>::const_iterator curStateID = _states.begin();
	while (curScan != _scans.end()) {
		_scanToStatesMap[*curScan].insert(*curStateID);
		curScan++;
		curStateID++;
	}
}

void MSMetaData::_makeSpwToFieldMap() {
	_spwToFieldIDsMap = vector<std::set<uInt> >(_spwInfo.size());
	uInt count = 0;
	for (
		vector<uInt>::const_iterator dataDescID=_dataDescIDs.begin();
		dataDescID!=_dataDescIDs.end(); dataDescID++, count++
	) {
		Int spw = _dataDescToSpwMap[*dataDescID];
		_spwToFieldIDsMap[spw].insert(_fieldIds[count]);
	}
}

void MSMetaData::_makeStateToIntentsMap(const MeasurementSet& ms) {
	String intentsColName = MSTable<
			MSStateEnums::PredefinedColumns,
			MSStateEnums::PredefinedKeywords
		>::columnName(MSStateEnums::OBS_MODE);
	ROScalarColumn<String> intentsCol(ms.state(), intentsColName);
	Vector<String> intentSets = intentsCol.getColumn();
	_nStates = intentSets.size();
	_stateToIntentsMap.resize(_nStates);
	vector<std::set<String> >::iterator sIter = _stateToIntentsMap.begin();
	for(
		Vector<String>::const_iterator curIntentSet=intentSets.begin();
		curIntentSet!=intentSets.end(); curIntentSet++, sIter++
	) {
		Vector<String> intents = casa::stringToVector(*curIntentSet, ',');
		*sIter = std::set <String>(intents.begin(), intents.end());
		_uniqueIntents.insert(intents.begin(), intents.end());
	}
}

void MSMetaData::_makeFieldNameToTimesMap(const MeasurementSet& ms) {
	String fieldIdColName = MSTable<
			MSMainEnums::PredefinedColumns,
			MSMainEnums::PredefinedKeywords
		>::columnName(MSMainEnums::FIELD_ID);
	ROScalarColumn<Int> fieldIdCol(ms, fieldIdColName);
	_fieldIds = _toUIntVector(fieldIdCol.getColumn());
	std::map<String, std::set<Double> > tmpMap;
	vector<Double>::const_iterator tIter = _times.begin();
	for (
		vector<uInt>::const_iterator iter=_fieldIds.begin();
		iter!=_fieldIds.end(); iter++, tIter++
	) {
		String fieldName = _fieldNames[*iter];
		if (
			_fieldNameToTimesMap.find(fieldName)==_fieldNameToTimesMap.end()
		) {
			tmpMap[fieldName] = std::set<Double>();
		}
		_fieldNameToTimesMap[fieldName].insert(*tIter);
	}
}

void MSMetaData::_makeBaselineToTimesMap(const MeasurementSet& ms) {
	String ant1ColName = MSTable<
			MSMainEnums::PredefinedColumns,
			MSMainEnums::PredefinedKeywords
		>::columnName(MSMainEnums::ANTENNA1);
	ROScalarColumn<Int> ant1Col(ms, ant1ColName);
	_antenna1 = _toUIntVector(ant1Col.getColumn());
	String ant2ColName = MSTable<
			MSMainEnums::PredefinedColumns,
			MSMainEnums::PredefinedKeywords
		>::columnName(MSMainEnums::ANTENNA2);
	ROScalarColumn<Int> ant2Col(ms, ant2ColName);
	_antenna2 = _toUIntVector(ant2Col.getColumn());
	Matrix<vector<Double> > tmpMap(
		_antennaNames.size(), _antennaNames.size(),
		vector<Double>(0)
	);
	vector<uInt>::const_iterator ant1Iter = _antenna1.begin();
	vector<uInt>::const_iterator ant2Iter = _antenna2.begin();
	for (
		vector<Double>::const_iterator iter=_times.begin();
		iter!=_times.end(); iter++, ant1Iter++, ant2Iter++
	) {
		tmpMap(*ant1Iter, *ant2Iter).push_back(*iter);
		tmpMap(*ant2Iter, *ant1Iter).push_back(*iter);
	}
	_baselineToTimesMap = Matrix<vector<Double> >(
			_antennaNames.size(), _antennaNames.size(),
			vector<Double>(0)
		);
	Matrix<vector<Double> >::iterator current = _baselineToTimesMap.begin();
	for (
		Matrix<vector<Double> >::const_iterator iter=tmpMap.begin();
		iter!=tmpMap.end(); iter++, current++
	) {
		*current = vector<Double>(*iter);
	}
}

void MSMetaData::_makeBaselineToStatesMap() {
	Matrix<vector<uInt> > tmpMap(
		_antennaNames.size(), _antennaNames.size(),
		vector<uInt>(0)
	);
	vector<uInt>::const_iterator ant1Iter = _antenna1.begin();
	vector<uInt>::const_iterator ant2Iter = _antenna2.begin();
	for (
		vector<uInt>::const_iterator iter=_states.begin();
		iter!=_states.end(); iter++, ant1Iter++, ant2Iter++
	) {
		tmpMap(*ant1Iter, *ant2Iter).push_back(*iter);
		tmpMap(*ant2Iter, *ant1Iter).push_back(*iter);
	}
	_baselineToStatesMap = Matrix<vector<uInt> >(
			_antennaNames.size(), _antennaNames.size(),
			vector<uInt>(0)
		);
	Matrix<vector<uInt> >::iterator current = _baselineToStatesMap.begin();
	for (
		Matrix<vector<uInt> >::const_iterator iter=tmpMap.begin();
		iter!=tmpMap.end(); iter++, current++
	) {
		*current = vector<uInt>(*iter);
	}
}

void MSMetaData::_makeDataDescID(const MeasurementSet& ms) {
	String ddColName = MSTable<
			MSMainEnums::PredefinedColumns,
			MSMainEnums::PredefinedKeywords
		>::columnName(MSMainEnums::DATA_DESC_ID);
	ROScalarColumn<Int> ddCol(ms, ddColName);
	_dataDescIDs = _toUIntVector(ddCol.getColumn());
}

void MSMetaData::_makeDataDescIDToSpwMap(const MeasurementSet& ms) {
	String spwColName = MSTable<
			MSDataDescriptionEnums::PredefinedColumns,
			MSDataDescriptionEnums::PredefinedKeywords
		>::columnName(MSDataDescriptionEnums::SPECTRAL_WINDOW_ID);
	ROScalarColumn<Int> spwCol(ms.dataDescription(), spwColName);
	_dataDescToSpwMap = _toUIntVector(spwCol.getColumn());
}

void MSMetaData::_setNumberOfPolarizations(const MeasurementSet& ms) {
	Int myscan = -1;
	for (
		std::set<uInt>::const_iterator scanNum=_uniqueScans.begin();
		scanNum!=_uniqueScans.end(); scanNum++
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
	_setDataColumnNames(ms);
	ROArrayColumn<Complex> dataCol(
		ms, _dataColumnName
	);
	Array<Complex> arr;
	if (myscan == 0) {
		// assume the first row in the table is for the first scan, to save time
		dataCol.get(0, arr);
		_nPolarizations = arr.shape()[0];
	}
	else {
		_scans = _getScans(ms);
		_nPolarizations = 0;
		uInt i = 0;
		for (
			vector<uInt>::const_iterator scanNum=_scans.begin();
			scanNum!=_scans.end(); scanNum++, i++
		) {
			if ((Int)*scanNum == myscan) {
				dataCol.get(i, arr);
				_nPolarizations = arr.shape()[0];
				break;
			}
		}
	}
}

void MSMetaData::_setDataColumnNames(const MeasurementSet& ms) {
	TableProxy t(ms);
	Vector<String> colNames = t.columnNames();
	std::set<String> cNames(colNames.begin(), colNames.end());
	std::set<String>::const_iterator end = cNames.end();
	if (cNames.find("FLOAT_DATA") != end) {
		_dataColumnName = "FLOAT_DATA";
		_correctedDataColumnName = "FLOAT_DATA";
	}
	else if (cNames.find("DATA") != end) {
		_dataColumnName = "DATA";
	}
	if (cNames.find("CORRECTED_DATA") != end) {
		_correctedDataColumnName = "CORRECTED_DATA";
	}
	if (cNames.find("MODEL_DATA") != end) {
		_modelDataColumnName = "MODEL_DATA";
	}
}

void MSMetaData::_makeScanToTimeMap(const MeasurementSet& ms) {
	_scans = _getScans(ms);
	String timeColName = MSTable<
			MSMainEnums::PredefinedColumns,
			MSMainEnums::PredefinedKeywords
		>::columnName(MSMainEnums::TIME);
	ROScalarColumn<Double> timeCol(ms, timeColName);
	timeCol.getColumn().tovector(_times);
	_uniqueScans.insert(_scans.begin(), _scans.end());
	vector<uInt>::const_iterator curScan = _scans.begin();
	vector<Double>::const_iterator curTime = _times.begin();
	std::map<uInt, vector<Double> > tmpMap;
	while (curScan != _scans.end()) {
		_scanToTimesMap[*curScan].insert(*curTime);
		curScan++;
		curTime++;
	}
}

void MSMetaData::_setSpwInfo(const MeasurementSet& ms) {
	const MSSpectralWindow spw = ms.spectralWindow();
	String bandwidthColName = MSTable<
			MSSpectralWindowEnums::PredefinedColumns,
			MSSpectralWindowEnums::PredefinedKeywords
		>::columnName(MSSpectralWindowEnums::TOTAL_BANDWIDTH);
	ROScalarColumn<Double> bwCol(spw, bandwidthColName);
	Vector<Double> bws = bwCol.getColumn();
	String cfColName = MSTable<
				MSSpectralWindowEnums::PredefinedColumns,
				MSSpectralWindowEnums::PredefinedKeywords
			>::columnName(MSSpectralWindowEnums::CHAN_FREQ);
	ROArrayColumn<Double> cfCol(spw, cfColName);
	String cwColName = MSTable<
			MSSpectralWindowEnums::PredefinedColumns,
			MSSpectralWindowEnums::PredefinedKeywords
		>::columnName(MSSpectralWindowEnums::CHAN_WIDTH);
	ROArrayColumn<Double> cwCol(spw, cwColName);
	String nsColName = MSTable<
			MSSpectralWindowEnums::PredefinedColumns,
			MSSpectralWindowEnums::PredefinedKeywords
		>::columnName(MSSpectralWindowEnums::NET_SIDEBAND);
	ROScalarColumn<Int> nsCol(spw, nsColName);
	Vector<Int> nss = nsCol.getColumn();
	SpwProperties props;
	vector<Double> freqLimits(2);
	Vector<Double> djunk;
	Vector<Double> tmp;
	for (uInt i=0; i<bws.size(); i++) {
		props.bandwidth = bws[i];
		props.chanfreqs.resize(0);
		tmp.resize(0);
		cfCol.get(i, tmp);
		tmp.tovector(props.chanfreqs);
		freqLimits[0] = min(tmp);
		freqLimits[1] = max(tmp);
		props.edgechans = freqLimits;
		djunk.resize(0);
		cwCol.get(i, djunk);
		props.chanwidth = djunk[0];
		// coded this way in ValueMapping
		props.netsideband = nss[i] == 2 ? 1 : -1;
		props.meanfreq = mean(tmp);
		tmp.tovector(props.chanfreqs);
		props.nchans = props.chanfreqs.size();
		_spwInfo.push_back(props);
		if (props.nchans==64 || props.nchans==128 || props.nchans==256) {
			_tdmspw.insert(i);
		}
		else if (props.nchans==1) {
			_avgspw.insert(i);
		}
		else if (props.nchans==4) {
			_wvrspw.insert(i);
		}
		else {
			_fdmspw.insert(i);
		}
	}
}

void MSMetaData::_makeSpwToScanMap() {
	_spwToScansMap.resize(_spwInfo.size());
	uInt count = 0;
	for (
		vector<uInt>::const_iterator iter=_dataDescIDs.begin();
		iter!=_dataDescIDs.end(); iter++, count++
	) {
		Int spw = _dataDescToSpwMap[*iter];
		_spwToScansMap[spw].insert(_scans[count]);
	}
}

void MSMetaData::_makeSpwToIntentsMap() {
	std::set<String> emptySet;
	for (
		vector<SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=_spwInfo.end(); iter++
	) {
		_spwToIntentsMap.push_back(emptySet);
	}
	if (_uniqueIntents.size() == 0) {
		return;
	}
	uInt count = 0;
	std::set<String> intents;
	uInt spw;
	std::map<uInt, std::set<uInt> > checkedMap;
	for (
		vector<uInt>::const_iterator curDDID=_dataDescIDs.begin();
		curDDID!=_dataDescIDs.end(); curDDID++, count++
	) {
		uInt curState = _states[count];
		spw = _dataDescToSpwMap[*curDDID];
		intents = _stateToIntentsMap[curState];
		_spwToIntentsMap[spw].insert(intents.begin(), intents.end());
		checkedMap[*curDDID].insert(curState);
	}
}

vector<uInt> MSMetaData::_getScans(const MeasurementSet& ms) {
	if (_scans.size() == 0) {
		String scanColName = MSTable<
				MSMainEnums::PredefinedColumns,
				MSMainEnums::PredefinedKeywords
			>::columnName(MSMainEnums::SCAN_NUMBER);
		ROScalarColumn<Int> scanCol(ms, scanColName);
		_scans = _toUIntVector(scanCol.getColumn());
	}
	return _scans;
}

vector<uInt> MSMetaData::_toUIntVector(const Vector<Int>& v) {
	if (anyLT(v, 0)) {
		throw AipsError("Column that should contain nonnegative ints has a negative int");
	}
	vector<uInt> u;
	for (Vector<Int>::const_iterator iter=v.begin(); iter!=v.end(); iter++) {
		u.push_back(*iter);
	}
	return u;
}

void MSMetaData::_checkScan(const uInt scan) const {
	if (_uniqueScans.find(scan) == _uniqueScans.end()) {
		throw AipsError(
			"Unknown scan number " + String::toString(scan)
		);
	}
}

void MSMetaData::_checkFieldID(const uInt fieldID) const {
	if (fieldID >= _nFields) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__) + " : field ID ("
			+ String::toString(fieldID) + ") out of range"
		);
	}
}

void MSMetaData::_checkTolerance(const Double tol) const {
	if (tol < 0) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__)
			+ " : Tolerance cannot be less than zero"
		);
	}
}

}




