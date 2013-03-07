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

#include <ms/MeasurementSets/MSMetaDataPreload.h>

#include <casa/Utilities/GenSort.h>
#include <measures/Measures/MCPosition.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/Measures/MeasTable.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableProxy.h>
#include <tables/Tables/TableRecord.h>

// DEBUG ONLY
/*
#include <casa/Arrays/ArrayIO.h>
#include <iomanip>
#include <casa/OS/PrecTimer.h>
*/

#define _ORIGIN "MSMetaDataPreload::" + String(__FUNCTION__) + ": "

namespace casa {

MSMetaDataPreload::MSMetaDataPreload(const MeasurementSet& ms)
	: _scans(), _uniqueScans(), _times(),
	  _scanToTimesMap(), _nStates(0) {
	_makeDataDescIDToSpwMap(ms);
	_makeDataDescID(ms);
	_makeScanToTimeMap(ms);
	// _makeTimeToExposureMap(ms);
	_makeFieldsAndSources(ms);
	_makeStateToIntentsMap(ms);
	_setObservation(ms);
	_makeAntennaInfo(ms);
	_makeScanToStateMap(ms);
	_makeFieldNameToTimesMap(ms);
	_makeUniqueBaselines(ms);
	_setNumberOfPolarizations(ms);
	_setSpwInfo(ms);
	_makeSpwToScanMap();
	_makeSpwToFieldMap();
	_makeSpwToIntentsMap();
	_makeTotalEffectiveExposureTime(ms);
	_makeRowStats(ms);
}

MSMetaDataPreload::~MSMetaDataPreload() {}

uInt MSMetaDataPreload::nStates() {
	return _nStates;
}

std::set<String> MSMetaDataPreload::getIntents() {
	return _uniqueIntents;
}

std::set<uInt> MSMetaDataPreload::getScanNumbers() {
	return _uniqueScans;
}

uInt MSMetaDataPreload::nScans() {
	return _uniqueScans.size();
}

uInt MSMetaDataPreload::nObservations() {
	return _nObservations;
}

uInt MSMetaDataPreload::nArrays() {
	return _nArrays;
}

uInt MSMetaDataPreload::nRows() const {
	return _scans.size();
}

uInt MSMetaDataPreload::nRows(CorrelationType cType) {
	if (cType == AUTO) {
		return _nACRows;
	}
	else if (cType == CROSS) {
		return _nXCRows;
	}
	else {
		return _nACRows + _nXCRows;
	}
}

uInt MSMetaDataPreload::nRows(
	CorrelationType cType, uInt arrayID, uInt observationID,
	uInt scanNumber, uInt fieldID
) {
	if (cType == AUTO) {
		return _scanToNACRowsMap[arrayID][observationID][scanNumber][fieldID];
	}
	else if (cType == CROSS) {
		return _scanToNXCRowsMap[arrayID][observationID][scanNumber][fieldID];
	}
	else {
		return _scanToNACRowsMap[arrayID][observationID][scanNumber][fieldID]
		    + _scanToNXCRowsMap[arrayID][observationID][scanNumber][fieldID];
	}
}

uInt MSMetaDataPreload::nRows(CorrelationType cType, uInt fieldID) {
	if (cType == AUTO) {
		return _fieldToNACRowsMap[fieldID];
	}
	else if (cType == CROSS) {
		return _fieldToNXCRowsMap[fieldID];
	}
	else {
		return _fieldToNACRowsMap[fieldID] + _fieldToNXCRowsMap[fieldID];
	}
}


Double MSMetaDataPreload::nUnflaggedRows() {
	return _nUnflaggedACRows + _nUnflaggedXCRows;
}

Double MSMetaDataPreload::nUnflaggedRows(CorrelationType cType) {
	if (cType == AUTO) {
		return _nUnflaggedACRows;
	}
	else if (cType == CROSS) {
		return _nUnflaggedXCRows;
	}
	else {
		return nUnflaggedRows();
	}
}

Double MSMetaDataPreload::nUnflaggedRows(
	CorrelationType cType, uInt arrayID, uInt observationID,
	uInt scanNumber, uInt fieldID
) {
	if (cType == AUTO) {
		return _scanToNUnflaggedACRowsMap[arrayID][observationID][scanNumber][fieldID];
	}
	else if (cType == CROSS) {
		return _scanToNUnflaggedXCRowsMap[arrayID][observationID][scanNumber][fieldID];
	}
	else {
		return _scanToNUnflaggedACRowsMap[arrayID][observationID][scanNumber][fieldID]
		    + _scanToNUnflaggedXCRowsMap[arrayID][observationID][scanNumber][fieldID];
	}
}

Double MSMetaDataPreload::nUnflaggedRows(CorrelationType cType, uInt fieldID) {
	if (cType == AUTO) {
		return _fieldToNUnflaggedACRows[fieldID];
	}
	else if (cType == CROSS) {
		return _fieldToNUnflaggedXCRows[fieldID];
	}
	else {
		return _fieldToNUnflaggedACRows[fieldID] + _fieldToNUnflaggedXCRows[fieldID];
	}
}

std::set<uInt> MSMetaDataPreload::getScansForState(const uInt stateID) {
	if (stateID >= _nStates) {
		throw AipsError(
			_ORIGIN + "Specified stateID exceeds the number of states for this dataset."
		);
	}
	std::set<uInt>::const_iterator lastScan = _uniqueScans.end();
	std::set<uInt> scansForState;
	for (
		std::set<uInt>::const_iterator scanNum=_uniqueScans.begin();
		scanNum!=lastScan; scanNum++
	) {
		std::set<uInt> statesSet = _scanToStatesMap.find(*scanNum)->second;
		if (statesSet.find(stateID) != statesSet.end()) {
			scansForState.insert(*scanNum);
		}
	}
	return scansForState;
}

std::set<String> MSMetaDataPreload::getIntentsForScan(const uInt scan) {
	_checkScan(scan);
	std::set<String> intentsForScan;
	Int i=0;
	vector<std::set<String> >::const_iterator lastPair = _stateToIntentsMap.end();
	for (
		vector<std::set<String> >::const_iterator iter=_stateToIntentsMap.begin();
		iter!=lastPair; iter++, i++
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

std::set<uInt> MSMetaDataPreload::getSpwsForIntent(const String& intent) {
	if (_uniqueIntents.find(intent) == _uniqueIntents.end()) {
		throw AipsError(
			_ORIGIN + "Unknown intent " + intent + " for this dataset"
		);
	}
	std::set<uInt> spws;
	for (uInt i=0; i<_spwToIntentsMap.size(); i++) {
		if (_spwToIntentsMap[i].find(intent) != _spwToIntentsMap[i].end()) {
			spws.insert(i);
		}
	}
	return spws;
}

uInt MSMetaDataPreload::nSpw(Bool includeWVR) {
	return includeWVR ? _spwInfo.size() : _spwInfo.size() - getWVRSpw().size();
}

std::set<String> MSMetaDataPreload::getIntentsForSpw(const uInt spw) {
	if (spw >= nSpw(True)) {
		throw AipsError(
			_ORIGIN + "spectral window out of range"
		);
	}
	return _spwToIntentsMap[spw];
}

uInt MSMetaDataPreload::nFields() {
	return _nFields;
}

std::set<uInt> MSMetaDataPreload::getSpwsForField(const uInt fieldID) {
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

std::set<uInt> MSMetaDataPreload::getSpwsForField(const String& fieldName) {
	for (uInt i=0; i<_fieldNames.size(); i++) {
		if (_fieldNames[i] == fieldName) {
			return getSpwsForField(i);
		}
	}
	throw AipsError(
		_ORIGIN + "field ("	+ fieldName + " does not exist"
	);
}

std::set<uInt> MSMetaDataPreload::getFieldIDsForSpw(const uInt spw) {
	if (spw >= _spwInfo.size()) {
		throw AipsError(
			_ORIGIN + "spectral window out of range"
		);
	}
	return _spwToFieldIDsMap[spw];
}

std::set<String> MSMetaDataPreload::getFieldNamesForSpw(const uInt spw) {
	std::set<uInt> fieldIDs = getFieldIDsForSpw(spw);
	std::set<String> fieldNames;
	for (
		std::set<uInt>::const_iterator fieldID = fieldIDs.begin();
		fieldID!=fieldIDs.end(); fieldID++
	) {
		fieldNames.insert(_fieldNames[*fieldID]);
	}
	return fieldNames;
}

std::set<uInt> MSMetaDataPreload::getSpwsForScan(const uInt scan) {
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

std::set<uInt> MSMetaDataPreload::getScansForSpw(const uInt spw) {
	if (spw >= nSpw(True)) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__)
			+ " : spectral window out of range"
		);
	}
	return _spwToScansMap[spw];
}

uInt MSMetaDataPreload::nAntennas() {
	return _antennaNames.size();
}

vector<String> MSMetaDataPreload::getAntennaNames(
	std::map<String, uInt>& namesToIDsMap,
	const vector<uInt>& antennaIDs
) {
	if (antennaIDs.empty()) {
		namesToIDsMap = _antennaNamesToIDs;
		return _antennaNames;
	}
	vector<String> antNames;
	uInt nAnts = nAntennas();
	vector<uInt>::const_iterator end = antennaIDs.end();
	for (
		vector<uInt>::const_iterator iter=antennaIDs.begin();
		iter!=end; iter++
	) {
		if (*iter >= nAnts) {
			throw AipsError(
				_ORIGIN + "antennaID " + String::toString(*iter)
				+ " out of range"
			);
		}
		antNames.push_back(_antennaNames[*iter]);
	}
	namesToIDsMap = _antennaNamesToIDs;
	return antNames;
}

vector<uInt> MSMetaDataPreload::getAntennaIDs(
	const vector<String>& antennaNames
) {
	vector<String>::const_iterator end = antennaNames.end();
	map<String, uInt>::const_iterator mapEnd = _antennaNamesToIDs.end();
	vector<uInt> ids;
	for (
		vector<String>::const_iterator name=antennaNames.begin();
		name!=end; name++
	) {
		map<String, uInt>::const_iterator pair = _antennaNamesToIDs.find(*name);
		if (pair == mapEnd) {
			throw AipsError(
				_ORIGIN + "Unknown antenna " + *name
			);
		}
		ids.push_back(pair->second);
	}
	return ids;
}

std::set<uInt> MSMetaDataPreload::getTDMSpw() {
	return _tdmspw;
}

std::set<uInt> MSMetaDataPreload::getFDMSpw() {
	return _fdmspw;
}

std::set<uInt> MSMetaDataPreload::getChannelAvgSpw() {
	return _avgspw;
}

std::set<uInt> MSMetaDataPreload::getWVRSpw() {
	return _wvrspw;
}

vector<Double> MSMetaDataPreload::getBandWidths() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<Double> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->bandwidth);
	}
	return out;
}

vector<Quantum<Vector<Double> > > MSMetaDataPreload::getChanFreqs() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<Quantum<Vector<Double> > > out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->chanfreqs);
	}
	return out;
}

vector<vector<Double> > MSMetaDataPreload::getChanWidths() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<vector<Double> > out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->chanwidths);
	}
	return out;
}

vector<Int> MSMetaDataPreload::getNetSidebands() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<Int> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->netsideband);
	}
	return out;
}

vector<Quantity> MSMetaDataPreload::getMeanFreqs() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<Quantity> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->meanfreq);
	}
	return out;
}

vector<uInt> MSMetaDataPreload::nChans() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<uInt> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->nchans);
	}
	return out;
}

vector<vector<Double> > MSMetaDataPreload::getEdgeChans() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<vector<Double> > out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->edgechans);
	}
	return out;
}

vector<uInt> MSMetaDataPreload::getBBCNos() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	if (! _hasBBCNo) {
		throw AipsError("This MS's SPECTRAL_WINDOW table does not have a BBC_NO column");
	}
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<uInt> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->bbcno);
	}
	return out;
}

vector<String> MSMetaDataPreload::getSpwNames() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
	vector<MSMetaData::SpwProperties>::const_iterator end = _spwInfo.end();
	vector<String> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=_spwInfo.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->name);
	}
	return out;
}


std::set<uInt> MSMetaDataPreload::getScansForTimes(
	const Double center, const Double tol
) {
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

std::set<Double> MSMetaDataPreload::getTimesForScans(
	const std::set<uInt>& scans
) {
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

std::vector<Double> MSMetaDataPreload::getTimeRangeForScan(uInt scan) {
	_checkScan(scan);
	return _scanToTimeRange[scan];
}

std::map<uInt, Double> MSMetaDataPreload::getAverageIntervalsForScan(uInt scan) {
	_checkScan(scan);
	return _scanSpwToIntervalMap[scan];
}


std::set<uInt> MSMetaDataPreload::getStatesForScan(const uInt scan) {
	_checkScan(scan);
	return _scanToStatesMap.find(scan)->second;
}

std::set<uInt> MSMetaDataPreload::getScansForIntent(const String& intent) {
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

std::set<uInt> MSMetaDataPreload::getScansForFieldID(const uInt fieldID) {
	if (fieldID >= _nFields) {
		throw AipsError(
			_ORIGIN + "No such field ID " + String::toString(fieldID)
		);
	}
	std::set<uInt> scans;
	uInt count = 0;
	// FIXME full column scan, make a map instead at construction
	Vector<Int>::const_iterator end = _fieldIds.end();
	for (
		Vector<Int>::const_iterator curFieldID=_fieldIds.begin();
		curFieldID!=end; curFieldID++, count++
	) {
		if ((Int)fieldID == *curFieldID) {
			scans.insert(_scans[count]);
		}
	}
	return scans;
}

std::set<uInt> MSMetaDataPreload::getFieldIDsForField(
	const String& field
) {
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
			_ORIGIN + "Unknown field name " + field
		);
	}
	return fieldIDs;
}

std::set<uInt> MSMetaDataPreload::getFieldsForScan(const uInt scan) {
	_checkScan(scan);
	std::set<uInt> fields;
	Vector<Int>::const_iterator curScan = _scans.begin();
	Vector<Int>::const_iterator end = _scans.end();
	Vector<Int>::const_iterator curField = _fieldIds.begin();
	Int iScan = (Int)scan;
	while (curScan != end) {
		if (*curScan == iScan) {
			fields.insert(*curField);
		}
		curScan++;
		curField++;
	}
	return fields;
}

std::set<uInt> MSMetaDataPreload::getFieldsForScans(const std::set<uInt>& scans) {
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

std::set<uInt> MSMetaDataPreload::getFieldsForIntent(const String& intent) {
	std::set<uInt> scans = getScansForIntent(intent);
	return getFieldsForScans(scans);
}

vector<String> MSMetaDataPreload::getFieldNamesForFieldIDs(
	const vector<uInt>& fieldIDs
) {
	_checkFieldIDs(fieldIDs);
	if (fieldIDs.size() == 0) {
		return _fieldNames;
	}
	vector<String> names;
	vector<uInt>::const_iterator end = fieldIDs.end();
	for (
		vector<uInt>::const_iterator iter=fieldIDs.begin();
		iter!=end; iter++
	) {
		names.push_back(_fieldNames[*iter]);
	}
	return names;
}

std::set<uInt> MSMetaDataPreload::getFieldsForTimes(
	const Double center, const Double tol
) const {
	_checkTolerance(tol);
	Double minTime = center - tol;
	Double maxTime = center + tol;
	std::set<uInt> fieldIDs;
	Vector<Double>::const_iterator time=_times.begin();
	Vector<Double>::const_iterator end=_times.end();
	Vector<Int>::const_iterator fieldID=_fieldIds.begin();
	while (time != end) {
		if (*time >= minTime && *time <= maxTime) {
			fieldIDs.insert(*fieldID);
		}
		time++;
		fieldID++;
	}
	return fieldIDs;
}

std::set<Double> MSMetaDataPreload::getTimesForField(const uInt fieldID) {
	_checkFieldID(fieldID);
	return _fieldNameToTimesMap.find(getFieldNamesForFieldIDs(vector<uInt>(1, fieldID))[0])->second;
}

vector<String> MSMetaDataPreload::getObservatoryNames() {
	return _observatoryNames;
}

MPosition MSMetaDataPreload::getObservatoryPosition(uInt which) {
	if (which >= _observatoryNames.size()) {
		throw AipsError("MSMetaData::getTelescopePosition out of range exception.");
	}
	return _observatoryPositions[which];
}

vector<MPosition> MSMetaDataPreload::getAntennaPositions(
	const vector<uInt>& which
) {
	if (which.size() == 0) {
		return _antennaPositions;
	}
	if (max(Vector<uInt>(which)) >= _antennaNames.size()) {
		throw AipsError(_ORIGIN + "Out of range exception.");
	}
	vector<MPosition> output;
	vector<uInt>::const_iterator end = which.end();
	for (
		vector<uInt>::const_iterator iter=which.begin();
		iter!=end; iter++
	) {
		output.push_back(_antennaPositions[*iter]);
	}
	return output;
}

vector<MPosition> MSMetaDataPreload::getAntennaPositions(
	const vector<String>& names
) {
	if (names.size() == 0) {
		throw AipsError(_ORIGIN + "names cannot be empty");
	}
	return getAntennaPositions(getAntennaIDs(names));
}

Quantum<Vector<Double> > MSMetaDataPreload::getAntennaOffset(uInt which) {
	if (which >= _antennaNames.size()) {
		throw AipsError(_ORIGIN + "Out of range exception.");
	}
	return _antennaOffsets[which];
}

vector<Quantum<Vector<Double> > > MSMetaDataPreload::getAntennaOffsets(
	const vector<MPosition>&
) {
	return _antennaOffsets;
}

Quantum<Vector<Double> > MSMetaDataPreload::getAntennaOffset(const String& name) {
	vector<String> names(1);
	names[0] = name;
	return _antennaOffsets[getAntennaIDs(names)[0]];
}

Matrix<Bool> MSMetaDataPreload::getUniqueBaselines() {
	return _baselines;
}

Quantity MSMetaDataPreload::getEffectiveTotalExposureTime() {
	return _totalEffectiveExposureTime;
}


void MSMetaDataPreload::_makeTotalEffectiveExposureTime(const MeasurementSet& ms) {
	std::map<Double, Double> timeToBWMap = _getTimeToTotalBWMap(
		_times, _dataDescIDs, _dataDescToSpwMap, _spwInfo
	);
	_totalEffectiveExposureTime =  _getTotalExposureTime(
		ms, timeToBWMap, _spwInfo, _dataDescToSpwMap
	);
}

void MSMetaDataPreload::_makeFieldsAndSources(const MeasurementSet& ms) {
	_fieldNames = _getFieldNames(ms);
	_nFields = _fieldNames.size();
}

void MSMetaDataPreload::_makeAntennaInfo(const MeasurementSet& ms) {
	_antennaNames = _getAntennaNames(_antennaNamesToIDs, ms);
	String antPosColName = MSAntenna::columnName(MSAntennaEnums::POSITION);
	ArrayColumn<Double> posCol(ms.antenna(), antPosColName);
	Array<Double> xyz = posCol.getColumn();
	Vector<String> posUnits = posCol.keywordSet().asArrayString("QuantumUnits");
	String sFrame = posCol.keywordSet().asRecord("MEASINFO").asString("Ref");
	MPosition::Types posType = MPosition::getType(sFrame);
	Array<Double>::const_iterator end = xyz.end();
	Quantity x(0, posUnits[0]);
	Quantity y(0, posUnits[1]);
	Quantity z(0, posUnits[2]);
	for (Array<Double>::const_iterator iter=xyz.begin(); iter!=end; iter++) {
		x.setValue(*iter);
		Double xm = x.getValue("m");
		iter++;
		y.setValue(*iter);
		Double ym = y.getValue("m");
		iter++;
		z.setValue(*iter);
		Double zm = z.getValue("m");
		MPosition antPos(MVPosition(xm, ym, zm), posType);
		_antennaPositions.push_back(antPos);
	}
	_antennaOffsets = _getAntennaOffsets(
		_antennaPositions,
		_observatoryPositions[0]
	);
}

void MSMetaDataPreload::_makeScanToStateMap(const MeasurementSet& ms) {
	if (_states.size() == 0) {
		_states = _getStates(ms);
	}
	if (_scans.size() == 0) {
		_scans = MSMetaData::_getScans(ms);
	}
	_scanToStatesMap = _getScanToStatesMap(_scans, _states);
}

void MSMetaDataPreload::_makeSpwToFieldMap() {
	_spwToFieldIDsMap = vector<std::set<uInt> >(_spwInfo.size());
	uInt count = 0;
	Vector<Int>::const_iterator end = _dataDescIDs.end();
	for (
		Vector<Int>::const_iterator dataDescID=_dataDescIDs.begin();
		dataDescID!=end; dataDescID++, count++
	) {
		Int spw = _dataDescToSpwMap[*dataDescID];
		_spwToFieldIDsMap[spw].insert(_fieldIds[count]);
	}
}

void MSMetaDataPreload::_makeStateToIntentsMap(const MeasurementSet& ms) {
	_nStates = _getNStates(ms);
	_getStateToIntentsMap(_stateToIntentsMap, _uniqueIntents, ms);
}

void MSMetaDataPreload::_makeFieldNameToTimesMap(const MeasurementSet& ms) {
	_fieldIds = _getFieldIDs(ms);
	std::map<String, std::set<Double> > tmpMap;
	Vector<Double>::const_iterator tIter = _times.begin();
	Vector<Int>::const_iterator end = _fieldIds.end();
	for (
		Vector<Int>::const_iterator iter=_fieldIds.begin();
		iter!=end; iter++, tIter++
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

void MSMetaDataPreload::_makeRowStats(const MeasurementSet& ms) {
	_getAntennas(_antenna1, _antenna2, ms);
	MSMetaData::_getRowStats(
		_nACRows, _nXCRows, _scanToNACRowsMap,
		_scanToNXCRowsMap, _fieldToNACRowsMap,
		_fieldToNXCRowsMap, _antenna1, _antenna2,
		_scans, _fieldIds, _obsIDs, _arrayIDs
	);
	/*
	_flags.reset(_getFlags(ms));
	_flagRow = _getFlagRows(ms);
	_getUnflaggedRowStats(
		_nUnflaggedACRows, _nUnflaggedXCRows, _antenna1, _antenna2,
		_flagRow, _dataDescIDs, _dataDescToSpwMap,
		_spwInfo, *_flags
	);
	*/
	MSMetaData::_getUnflaggedRowStats(
		_nUnflaggedACRows, _nUnflaggedXCRows,
		_fieldToNUnflaggedACRows, _fieldToNUnflaggedXCRows,
		_scanToNUnflaggedACRowsMap, _scanToNUnflaggedXCRowsMap,
		_dataDescToSpwMap, _spwInfo, ms
	);

}

/*
void MSMetaDataPreload::_makeBaselineToTimesMap(const MeasurementSet& ms) {
	String ant1ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA1);
	ROScalarColumn<Int> ant1Col(ms, ant1ColName);
	_antenna1 = _toUIntVector(ant1Col.getColumn());
	String ant2ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA2);
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

void MSMetaDataPreload::_makeBaselineToStatesMap() {
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
*/

void MSMetaDataPreload::_makeDataDescID(const MeasurementSet& ms) {
	_dataDescIDs = _getDataDescIDs(ms);
}

void MSMetaDataPreload::_makeDataDescIDToSpwMap(const MeasurementSet& ms) {
	_dataDescToSpwMap = _getDataDescIDToSpwMap(ms);
}

void MSMetaDataPreload::_setNumberOfPolarizations(const MeasurementSet& ms) {
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
	ArrayColumn<Complex> dataCol(
		ms, _dataColumnName
	);
	Array<Complex> arr;
	if (myscan == 0) {
		// assume the first row in the table is for the first scan, to save time
		dataCol.get(0, arr);
		_nPolarizations = arr.shape()[0];
	}
	else {
		if (_scans.size() == 0) {
			_scans = _getScans(ms);
		}
		_nPolarizations = 0;
		uInt i = 0;
		for (
			Vector<Int>::const_iterator scanNum=_scans.begin();
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

void MSMetaDataPreload::_setDataColumnNames(const MeasurementSet& ms) {
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

void MSMetaDataPreload::_makeScanToTimeMap(const MeasurementSet& ms) {
	_scans = _getScans(ms);
	_times = _getTimes(ms);
	_scanToTimesMap = _getScanToTimesMap(_scans, _times);
	_uniqueScans.insert(_scans.begin(), _scans.end());
	Vector<Int>::const_iterator curScan = _scans.begin();
	Vector<Int>::const_iterator end = _scans.end();
	Vector<Double>::const_iterator curTime = _times.begin();
	std::map<uInt, vector<Double> > tmpMap;
	while (curScan != end) {
		_scanToTimesMap[*curScan].insert(*curTime);
		curScan++;
		curTime++;
	}
	_scanToTimeRange = _getScanToTimeRangeMap(
		_scanSpwToIntervalMap,
		_scans, _getTimeCentroids(ms), _getIntervals(ms),
		_dataDescIDs, _dataDescToSpwMap, _uniqueScans
	);
}

/*
void MSMetaDataPreload::_makeTimeToExposureMap(const MeasurementSet& ms) {
	if (_times.size() == 0) {
		_times = _getTimes(ms);
	}
	_timeToExposureMap = _getTimeToAggregateExposureMap(_times, _getExposures(ms));
}
*/

void MSMetaDataPreload::_setSpwInfo(const MeasurementSet& ms) {
	_spwInfo = _getSpwInfo(_avgspw, _tdmspw, _fdmspw, _wvrspw, ms);
	_hasBBCNo = hasBBCNo(ms);
}

void MSMetaDataPreload::_makeSpwToScanMap() {
	_spwToScansMap.resize(_spwInfo.size());
	uInt count = 0;
	Vector<Int>::const_iterator end = _dataDescIDs.end();
	for (
		Vector<Int>::const_iterator iter=_dataDescIDs.begin();
		iter!=end; iter++, count++
	) {
		Int spw = _dataDescToSpwMap[*iter];
		_spwToScansMap[spw].insert(_scans[count]);
	}
}

void MSMetaDataPreload::_makeSpwToIntentsMap() {
	std::set<String> emptySet;
	for (
		uInt i=0; i<_spwInfo.size(); i++
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
	Vector<Int>::const_iterator end = _dataDescIDs.end();
	for (
		Vector<Int>::const_iterator curDDID=_dataDescIDs.begin();
		curDDID!=end; curDDID++, count++
	) {
		uInt curState = _states[count];
		spw = _dataDescToSpwMap[*curDDID];
		intents = _stateToIntentsMap[curState];
		_spwToIntentsMap[spw].insert(intents.begin(), intents.end());
		checkedMap[*curDDID].insert(curState);
	}
}

void MSMetaDataPreload::_makeUniqueBaselines(const MeasurementSet& ms) {
	if (_antenna1.empty() || _antenna2.empty()) {
		_getAntennas(_antenna1, _antenna2, ms);
	}
	_baselines = _getUniqueBaselines(_antenna1, _antenna2);
}

void MSMetaDataPreload::_setObservation(const MeasurementSet& ms) {
	if (_observatoryNames.empty()) {
		_observatoryPositions = _getObservatoryPositions(_observatoryNames, ms);
	}
	_nObservations = ms.observation().nrow();
	_obsIDs = _getObservationIDs(ms);

	_arrayIDs = _getArrayIDs(ms);
	_nArrays = max(_arrayIDs) + 1;
}

void MSMetaDataPreload::_checkScan(const uInt scan) const {
	if (_uniqueScans.find(scan) == _uniqueScans.end()) {
		throw AipsError(
			_ORIGIN + "Unknown scan number " + String::toString(scan)
		);
	}
}

void MSMetaDataPreload::_checkFieldID(const uInt fieldID) const {
	if (fieldID >= _nFields) {
		throw AipsError(
			_ORIGIN + "field ID ("
			+ String::toString(fieldID) + ") out of range"
		);
	}
}

void MSMetaDataPreload::_checkFieldIDs(const vector<uInt>& fieldIDs) const {
	if (fieldIDs.size() > 0) {
		if (uInt myMax = max(Vector<uInt>(fieldIDs)) >= _nFields) {
			throw AipsError(
				_ORIGIN + "At least one fieldID (" + String::toString(myMax)
					+ ") is out of range"
			);
		}
	}
}

}

