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
//# $Id$

#include <casacore/ms/MSOper/MSMetaData.h>

#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/OS/File.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/ms/MSOper/MSKeys.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/Tables/TableProxy.h>

#define _ORIGIN "MSMetaData::" + String(__FUNCTION__) + ": "

namespace casacore {

MSMetaData::MSMetaData(const MeasurementSet *const &ms, const Float maxCacheSizeMB)
	: _ms(ms), _cacheMB(0), _maxCacheMB(maxCacheSizeMB), _nStates(0),
	  _nACRows(0), _nXCRows(0), _nSpw(0), _nFields(0),
	  _nAntennas(0), _nObservations(0), _nScans(0), _nArrays(0),
	  _nrows(0), _nPol(0), _nDataDescIDs(0),
	  _scanToSpwsMap(),
	  _scanToDDIDsMap(),
	  _dataDescIDToSpwMap(),
	  _dataDescIDToPolIDMap(),
	  _fieldToSpwMap(),
	  _scanToStatesMap(), _scanToFieldsMap(),
	  _fieldToStatesMap(), _stateToFieldsMap(),
	  _sourceToFieldsMap(),
	  _antennaNameToIDMap(),
	  _intentToFieldIDMap(),
	  _intentToScansMap(),
	  _scanToTimeRangeMap(),
	  _scanSpwToIntervalMap(),
	  _uniqueIntents(),
	  _uniqueFieldIDs(), _uniqueStateIDs(),
	  _avgSpw(), _tdmSpw(),
	  _fdmSpw(), _wvrSpw(), _sqldSpw(), _antenna1(), _antenna2(),
	  _scans(), _fieldIDs(), _stateIDs(), _dataDescIDs(),
	  _observationIDs(),
	  _subScanToNACRowsMap(), _subScanToNXCRowsMap(),
	  _fieldToNACRowsMap(), _fieldToNXCRowsMap(),
	  _scanToIntentsMap(), _stateToIntentsMap(),
	  _spwToIntentsMap(),
	  _spwInfo(0),
	  _spwToFieldIDsMap(), _obsToArraysMap(), _spwToScansMap(),
	  _fieldToScansMap(),
	  _fieldNames(0),
	  _antennaNames(0), _observatoryNames(0),
	   _times(),
	  _scanToTimesMap(),
	  _fieldToTimesMap(), _observatoryPositions(0),
	  _antennaOffsets(0), _uniqueBaselines(0, 0),
	  _exposureTime(0), _nUnflaggedACRows(0),
	  _nUnflaggedXCRows(0), _unflaggedFieldNACRows(),
	  _unflaggedFieldNXCRows(), _unflaggedSubScanNACRows(),
	  _unflaggedSubScanNXCRows(),
	  _taqlTableName(
		File(ms->tableName()).exists() ? ms->tableName() : "$1"
	  ),
	  _taqlTempTable(
		File(ms->tableName()).exists() ? 0 : 1, ms
	  ), _flagsColumn(),
	   _spwInfoStored(False) {}

MSMetaData::~MSMetaData() {}

uInt MSMetaData::nStates() const {
	if (_nStates == 0) {
		_nStates = _ms->state().nrow();
	}
	return _nStates;
}

std::set<String> MSMetaData::getIntents() const {
	if (! _uniqueIntents.empty()) {
		return _uniqueIntents;
	}
	vector<std::set<String> > statesToIntentsMap;
	std::set<String> uniqueIntents;
	_getStateToIntentsMap(
		statesToIntentsMap,
		uniqueIntents
	);
	return uniqueIntents;
}

void MSMetaData::_getStateToIntentsMap(
	vector<std::set<String> >& stateToIntentsMap,
	std::set<String>& uniqueIntents
) const {
	if (
		! _uniqueIntents.empty() && ! _stateToIntentsMap.empty()
	) {
		uniqueIntents = _uniqueIntents;
		stateToIntentsMap = _stateToIntentsMap;
		return;
	}
	uniqueIntents.clear();
	String intentsColName = MSState::columnName(MSStateEnums::OBS_MODE);
	ROScalarColumn<String> intentsCol(_ms->state(), intentsColName);
	Vector<String> intentSets = intentsCol.getColumn();
	stateToIntentsMap.resize(nStates());

	Vector<String>::const_iterator end = intentSets.end();
	vector<std::set<String> >::iterator sIter = stateToIntentsMap.begin();
	for(
		Vector<String>::const_iterator curIntentSet=intentSets.begin();
		curIntentSet!=end; ++curIntentSet, ++sIter
	) {
		Vector<String> intents = stringToVector(*curIntentSet, ',');
		*sIter = std::set <String>(intents.begin(), intents.end());
		uniqueIntents.insert(intents.begin(), intents.end());
	}

	std::set<String>::const_iterator lastIntent = uniqueIntents.end();
	uInt mysize = 0;

	vector<std::set<String> >::const_iterator lastState = stateToIntentsMap.end();
	uInt count = 0;
	for (
		vector<std::set<String> >::const_iterator iter=stateToIntentsMap.begin();
		iter!=lastState; ++iter, ++count
	) {
		std::set<String>::const_iterator lastIntent=iter->end();
		for (
			std::set<String>::const_iterator intent=iter->begin();
			intent!=lastIntent; ++intent
		) {
			mysize += intent->size();
		}
	}
	for (
		std::set<String>::const_iterator intent=uniqueIntents.begin();
		intent!=lastIntent; ++intent
	) {
		mysize += intent->size();
	}
	if (_cacheUpdated(mysize)) {
		_uniqueIntents = uniqueIntents;
		_stateToIntentsMap = stateToIntentsMap;
	}
}

std::set<Int> MSMetaData::getScanNumbers(Int obsID, Int arrayID) const {
	ArrayKey arrayKey;
	arrayKey.obsID = obsID;
	arrayKey.arrayID = arrayID;
	return _getScanNumbers(arrayKey);
}

uInt MSMetaData::nScans() {
	if (_nScans == 0) {
		_nScans = _getScanKeys().size();
	}
	return _nScans;
}

uInt MSMetaData::nObservations() const {
	if (_nObservations == 0) {
		_nObservations = _ms->observation().nrow();
	}
	return _nObservations;
}

uInt MSMetaData::nArrays() {
	if (_nArrays == 0) {
		// because the ARRAY table apparently is optional
		_nArrays = max(*_getArrayIDs()) + 1;
	}
	return _nArrays;
}

uInt MSMetaData::nRows() const {
	return _ms->nrow();
}
uInt MSMetaData::nRows(CorrelationType cType) {
	if (cType == BOTH) {
		return nRows();
	}
	uInt nACRows, nXCRows;
	CountedPtr<std::map<SubScanKey, uInt> > subScanToNACRowsMap, subScanToNXCRowsMap;
	CountedPtr<vector<uInt> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getRowStats(
		nACRows, nXCRows, subScanToNACRowsMap,
		subScanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return nACRows;
	}
	else {
		return nXCRows;
	}
}

uInt MSMetaData::nRows(
	CorrelationType cType, Int arrayID, Int observationID,
	Int scanNumber, Int fieldID
) const {
	SubScanKey subScanKey;
	subScanKey.obsID = observationID;
	subScanKey.arrayID = arrayID;
	subScanKey.scan = scanNumber;
	subScanKey.fieldID = fieldID;
	_checkSubScan(subScanKey);
	uInt nACRows, nXCRows;
	CountedPtr<std::map<SubScanKey, uInt> > subScanToNACRowsMap, subScanToNXCRowsMap;
	CountedPtr<vector<uInt> > fieldToNACRowsMap, fieldToNXCRowsMap;
    _getRowStats(
		nACRows, nXCRows, subScanToNACRowsMap,
		subScanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return (*subScanToNACRowsMap)[subScanKey];
	}
	else if (cType == CROSS) {
		return (*subScanToNXCRowsMap)[subScanKey];
	}
	else {
		return (*subScanToNACRowsMap)[subScanKey]
		    + (*subScanToNXCRowsMap)[subScanKey];
	}
}

uInt MSMetaData::nRows(CorrelationType cType, uInt fieldID) const {
	_checkField(fieldID);
	uInt nACRows, nXCRows;
	CountedPtr<std::map<SubScanKey, uInt> > subScanToNACRowsMap, subScanToNXCRowsMap;
	CountedPtr<vector<uInt> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getRowStats(
		nACRows, nXCRows, subScanToNACRowsMap,
		subScanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return (*fieldToNACRowsMap)[fieldID];
	}
	else if (cType == CROSS) {
		return (*fieldToNXCRowsMap)[fieldID];
	}
	else {
		return (*fieldToNACRowsMap)[fieldID] + (*fieldToNXCRowsMap)[fieldID];
	}
}

Double MSMetaData::nUnflaggedRows() const {
	Double nACRows, nXCRows;
	CountedPtr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
	CountedPtr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, subScanToNACRowsMap,
		subScanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	return nACRows + nXCRows;
}
Double MSMetaData::nUnflaggedRows(CorrelationType cType) const {
	if (cType == BOTH) {
		return nUnflaggedRows();
	}
	Double nACRows, nXCRows;
	CountedPtr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
	CountedPtr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, subScanToNACRowsMap,
		subScanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return nACRows;
	}
	else {
		return nXCRows;
	}
}

Double MSMetaData::nUnflaggedRows(
	CorrelationType cType, Int arrayID, uInt observationID,
	Int scanNumber, uInt fieldID
) const {
	SubScanKey subScanKey;
	subScanKey.obsID = observationID;
	subScanKey.arrayID = arrayID;
	subScanKey.scan = scanNumber;
	subScanKey.fieldID = fieldID;
	_checkSubScan(subScanKey);
	Double nACRows, nXCRows;
	CountedPtr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
	CountedPtr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, subScanToNACRowsMap,
		subScanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return (*subScanToNACRowsMap)[subScanKey];
	}
	else if (cType == CROSS) {
		return (*subScanToNXCRowsMap)[subScanKey];
	}
	else {
		return (*subScanToNACRowsMap)[subScanKey]
		    + (*subScanToNXCRowsMap)[subScanKey];
	}
}

Double MSMetaData::nUnflaggedRows(CorrelationType cType, Int fieldID) const {
	Double nACRows, nXCRows;
	CountedPtr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
	CountedPtr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, subScanToNACRowsMap,
		subScanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return (*fieldToNACRowsMap)[fieldID];
	}
	else if (cType == CROSS) {
		return (*fieldToNXCRowsMap)[fieldID];
	}
	else {
		return (*fieldToNACRowsMap)[fieldID] + (*fieldToNXCRowsMap)[fieldID];
	}
}

void MSMetaData::_getRowStats(
	uInt& nACRows, uInt& nXCRows,
	std::map<SubScanKey, uInt>*& subScanToNACRowsMap,
	std::map<SubScanKey, uInt>*& subScanToNXCRowsMap,
	vector<uInt>*& fieldToNACRowsMap,
	vector<uInt>*& fieldToNXCRowsMap
) const {
	nACRows = 0;
	nXCRows = 0;
	subScanToNACRowsMap = new std::map<SubScanKey, uInt>();
	subScanToNXCRowsMap = new std::map<SubScanKey, uInt>();
	uInt myNFields = nFields();
	fieldToNACRowsMap = new vector<uInt>(myNFields, 0);
	fieldToNXCRowsMap = new vector<uInt>(myNFields, 0);
	std::set<SubScanKey> subScanKeys = _getSubScanKeys();
	std::set<SubScanKey>::const_iterator subIter = subScanKeys.begin();
	std::set<SubScanKey>::const_iterator subEnd = subScanKeys.end();
	while (subIter != subEnd) {
		(*subScanToNACRowsMap)[*subIter] = 0;
		(*subScanToNXCRowsMap)[*subIter] = 0;
		++subIter;
	}
	CountedPtr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);
	CountedPtr<Vector<Int> > scans = _getScans();
	CountedPtr<Vector<Int> > fieldIDs = _getFieldIDs();
	CountedPtr<Vector<Int> > obsIDs = _getObservationIDs();
	CountedPtr<Vector<Int> > arrIDs = _getArrayIDs();
	Vector<Int>::const_iterator aEnd = ant1->end();
	Vector<Int>::const_iterator a1Iter = ant1->begin();
	Vector<Int>::const_iterator a2Iter = ant2->begin();
	Vector<Int>::const_iterator sIter = scans->begin();
	Vector<Int>::const_iterator fIter = fieldIDs->begin();
	Vector<Int>::const_iterator oIter = obsIDs->begin();
	Vector<Int>::const_iterator arIter = arrIDs->begin();
	SubScanKey subScanKey;
	while (a1Iter!=aEnd) {
		subScanKey.obsID = *oIter;
		subScanKey.arrayID = *arIter;
		subScanKey.scan = *sIter;
		subScanKey.fieldID = *fIter;
		if (*a1Iter == *a2Iter) {
			++nACRows;
			++(*subScanToNACRowsMap)[subScanKey];
			++(*fieldToNACRowsMap)[*fIter];
		}
		else {
			++nXCRows;
			++(*subScanToNXCRowsMap)[subScanKey];
			++(*fieldToNXCRowsMap)[*fIter];
		}
		++a1Iter;
		++a2Iter;
		++sIter;
		++fIter;
		++arIter;
		++oIter;
	}
}

void MSMetaData::_getRowStats(
	uInt& nACRows, uInt& nXCRows,
	CountedPtr<std::map<SubScanKey, uInt> >& scanToNACRowsMap,
	CountedPtr<std::map<SubScanKey, uInt> >& scanToNXCRowsMap,
	CountedPtr<vector<uInt> >& fieldToNACRowsMap,
	CountedPtr<vector<uInt> >& fieldToNXCRowsMap
) const {
	// this method is responsible for setting _nACRows, _nXCRows, _subScanToNACRowsMap,
	// _subScanToNXCRowsMap, _fieldToNACRowsMap, _fieldToNXCRowsMap
	if (_nACRows > 0 || _nXCRows > 0) {
		nACRows = _nACRows;
		nXCRows = _nXCRows;
		scanToNACRowsMap = _subScanToNACRowsMap;
		scanToNXCRowsMap = _subScanToNXCRowsMap;
		fieldToNACRowsMap = _fieldToNACRowsMap;
		fieldToNXCRowsMap = _fieldToNXCRowsMap;
		return;
	}

	std::map<SubScanKey, uInt> *myScanToNACRowsMap, *myScanToNXCRowsMap;
	vector<uInt> *myFieldToNACRowsMap, *myFieldToNXCRowsMap;
	_getRowStats(
		nACRows, nXCRows, myScanToNACRowsMap,
		myScanToNXCRowsMap, myFieldToNACRowsMap,
		myFieldToNXCRowsMap
	);
	scanToNACRowsMap = myScanToNACRowsMap;
	scanToNXCRowsMap = myScanToNXCRowsMap;
	fieldToNACRowsMap = myFieldToNACRowsMap;
	fieldToNXCRowsMap = myFieldToNXCRowsMap;
	uInt size = 2*(
		sizeof(uInt) + _sizeof(*scanToNACRowsMap)
		+ _sizeof(*fieldToNACRowsMap)
	);
	if (_cacheUpdated(size)) {
		_nACRows = nACRows;
		_nXCRows = nXCRows;
		_subScanToNACRowsMap = scanToNACRowsMap;
		_subScanToNXCRowsMap = scanToNXCRowsMap;
		_fieldToNACRowsMap = fieldToNACRowsMap;
		_fieldToNXCRowsMap = fieldToNXCRowsMap;
	}
}

void MSMetaData::_getAntennas(
	CountedPtr<Vector<Int> >& ant1,
	CountedPtr<Vector<Int> >& ant2
) const {
	if (
		_antenna1 && _antenna1->size() > 0
		&& _antenna2 && _antenna2->size() > 0
	) {
		ant1 = _antenna1;
		ant2 = _antenna2;
	}
	String ant1ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA1);
	ROScalarColumn<Int> ant1Col(*_ms, ant1ColName);
	Vector<Int> a1 = ant1Col.getColumn();
	String ant2ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA2);
	ROScalarColumn<Int> ant2Col(*_ms, ant2ColName);
	Vector<Int> a2 = ant2Col.getColumn();

	ant1.reset(new Vector<Int>(a1));
	ant2.reset(new Vector<Int>(a2));

	if (_cacheUpdated(2*sizeof(Int)*ant1->size())) {
		_antenna1 = ant1;
		_antenna2 = ant2;
	}
}

CountedPtr<Vector<Int> > MSMetaData::_getScans() const {
	if (_scans && _scans->size() > 0) {
		return _scans;
	}
	String scanColName = MeasurementSet::columnName(MSMainEnums::SCAN_NUMBER);
	CountedPtr<Vector<Int> > scans(new Vector<Int>(ROScalarColumn<Int>(*_ms, scanColName).getColumn()));
	if (_cacheUpdated(sizeof(Int)*scans->size())) {
		_scans = scans;
	}
	return scans;
}

CountedPtr<Vector<Int> > MSMetaData::_getObservationIDs() const {
	if (_observationIDs && _observationIDs->size() > 0) {
		return _observationIDs;
	}
	static const String obsColName = MeasurementSet::columnName(MSMainEnums::OBSERVATION_ID);
	CountedPtr<Vector<Int> > obsIDs(
		new Vector<Int>(ROScalarColumn<Int>(*_ms, obsColName).getColumn())
    );
	if (_cacheUpdated(sizeof(Int)*obsIDs->size())) {
		_observationIDs = obsIDs;
    }
	return obsIDs;
}

CountedPtr<Vector<Int> > MSMetaData::_getArrayIDs() const {
	if (_arrayIDs && _arrayIDs->size() > 0) {
		return _arrayIDs;
	}
	static const String arrColName = MeasurementSet::columnName(MSMainEnums::ARRAY_ID);
	CountedPtr<Vector<Int> > arrIDs(
		new Vector<Int>(ROScalarColumn<Int>(*_ms, arrColName).getColumn())
	);
	if (_cacheUpdated(sizeof(Int)*arrIDs->size())) {
		_arrayIDs = arrIDs;
	}
	return arrIDs;
}

CountedPtr<Vector<Int> > MSMetaData::_getFieldIDs() const {
	if (_fieldIDs && ! _fieldIDs->empty()) {
		return _fieldIDs;
	}
	String fieldIdColName = MeasurementSet::columnName(MSMainEnums::FIELD_ID);
	CountedPtr<Vector<Int> > fields(
		new Vector<Int>(ROScalarColumn<Int>(*_ms, fieldIdColName).getColumn())
	);
	if (_cacheUpdated(sizeof(Int)*fields->size())) {
		_fieldIDs = fields;
	}
	return fields;
}

CountedPtr<Vector<Int> > MSMetaData::_getStateIDs() const {
	if (_stateIDs && _stateIDs->size() > 0) {
		return _stateIDs;
	}
	static const String stateColName = MeasurementSet::columnName(MSMainEnums::STATE_ID);
	CountedPtr<Vector<Int> > states(
		new Vector<Int>(ROScalarColumn<Int>(*_ms, stateColName).getColumn())
	);
    Int maxState = max(*states);
    Int nstates = (Int)nStates();
    ThrowIf(
    	maxState >= nstates,
        "MS only has " + String::toString(nstates)
        + " rows in its STATE table, but references STATE_ID "
        + String::toString(maxState) + " in its main table."
    );
    if (_cacheUpdated(sizeof(Int)*states->size())) {
		_stateIDs = states;
	}
	return states;
}

CountedPtr<Vector<Int> > MSMetaData::_getDataDescIDs() const {
	if (_dataDescIDs && ! _dataDescIDs->empty()) {
		return _dataDescIDs;
	}
	static const String ddColName = MeasurementSet::columnName(MSMainEnums::DATA_DESC_ID);
	ROScalarColumn<Int> ddCol(*_ms, ddColName);
	CountedPtr<Vector<Int> > dataDescIDs(
		new Vector<Int>(ddCol.getColumn())
	);
	if (_cacheUpdated(sizeof(Int)*dataDescIDs->size())) {
		_dataDescIDs = dataDescIDs;
	}
	return dataDescIDs;
}

std::set<Int> MSMetaData::getScansForState(
	Int stateID, Int obsID, Int arrayID
) {
	if (! _hasStateID(stateID)) {
		return std::set<Int>();
	}
	std::map<ScanKey, std::set<Int> > myScanToStatesMap = _getScanToStatesMap();
	ArrayKey arrayKey;
	arrayKey.obsID = obsID;
	arrayKey.arrayID = arrayID;
	std::set<ScanKey> scanKeys = _getScanKeys(arrayKey);
	//CountedPtr<Vector<Int> > scans = _getScans();
	std::set<ScanKey>::const_iterator iter = scanKeys.begin();
	std::set<ScanKey>::const_iterator end = scanKeys.end();
	std::set<Int> stateIDs, scansForState;
	while (iter != end) {
		stateIDs = myScanToStatesMap[*iter];
		if (stateIDs.find(stateID) != stateIDs.end()) {
			scansForState.insert(iter->scan);
		}
		++iter;
	}
	return scansForState;
}

std::map<ScanKey, std::set<Int> > MSMetaData::_getScanToStatesMap() const {
	if (! _scanToStatesMap.empty()) {
		return _scanToStatesMap;
	}
	std::map<ScanKey, std::set<Int> > myScanToStatesMap;
	if (nStates() == 0) {
		std::set<Int> empty;
		std::set<ScanKey> scanKeys = this->_getScanKeys();
		//std::set<SubScanKey> subScanKeys;
		//_getSubScanKeys(subScanKeys, scanKeys);
		std::set<ScanKey>::const_iterator end = scanKeys.end();
		for (
			std::set<ScanKey>::const_iterator scanKey=scanKeys.begin();
			scanKey!=end; ++scanKey
		) {
			myScanToStatesMap[*scanKey] = empty;
		}
	}
	else {
		map<SubScanKey, SubScanProperties> subScanProps = _getSubScanProperties();
		//map<ScanKey, ScanProperties> scanProps;
		//map<ArrayKey, ArrayProperties> arrayProps;

		//_getSubScanProperties(subScanProps, scanProps, arrayProps);
		map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps.begin();
		map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps.end();
		//ScanKey key;
		while (iter != end) {
			SubScanKey subKey = iter->first;
			//key.obsID = subKey.obsID;
			//key.arrayID = subKey.arrayID;
			//key.scan = subKey.scan;
			SubScanProperties subProps = iter->second;
			myScanToStatesMap[scanKey(subKey)].insert(subProps.stateIDs.begin(), subProps.stateIDs.end());
			++iter;
		}
	}
	std::map<ScanKey, std::set<Int> >::const_iterator end = myScanToStatesMap.end();
	uInt mySize = sizeof(ScanKey)*myScanToStatesMap.size();
	for (
		std::map<ScanKey, std::set<Int> >::const_iterator iter=myScanToStatesMap.begin();
		iter!=end; ++iter
	) {
		mySize += sizeof(Int)*iter->second.size();
	}
	if (_cacheUpdated(mySize)) {
		_scanToStatesMap = myScanToStatesMap;
	}
	return myScanToStatesMap;
}

void MSMetaData::_getScansAndIntentsMaps(
	std::map<ScanKey, std::set<String> >& scanToIntentsMap,
	std::map<String, std::set<ScanKey> >& intentToScansMap
) const {
	// This method is responsible for setting _scanToIntentsMap and _intentToScansMap
	if (! _scanToIntentsMap.empty() && ! _intentToScansMap.empty()) {
		scanToIntentsMap = _scanToIntentsMap;
		intentToScansMap = _intentToScansMap;
		return;
	}
	vector<std::set<String> > stateToIntentsMap;
	std::set<String> uniqueIntents;
	_getStateToIntentsMap(
		stateToIntentsMap, uniqueIntents
	);
	std::map<ScanKey, std::set<Int> > scanToStatesMap = _getScanToStatesMap();
	std::map<ScanKey, std::set<Int> >::const_iterator end = scanToStatesMap.end();
	std::set<Int> states;
	std::set<String> intents;
	for (
		std::map<ScanKey, std::set<Int> >::const_iterator iter=scanToStatesMap.begin();
		iter!=end; ++iter
	) {
		ScanKey scan = iter->first;
		states = iter->second;
		std::set<Int>::const_iterator endState = states.end();
		for (
			std::set<Int>::const_iterator myState=states.begin();
			myState!=endState; ++myState
		) {
            intents = stateToIntentsMap[*myState];
            scanToIntentsMap[scan].insert(intents.begin(), intents.end());
			std::set<String>::const_iterator endIntent = intents.end();
			for (
				std::set<String>::const_iterator myIntent=intents.begin();
				myIntent!=endIntent; ++myIntent
			) {
				intentToScansMap[*myIntent].insert(scan);
			}
		}
	}
	if (_cacheUpdated(_sizeof(scanToIntentsMap) + _sizeof(intentToScansMap))) {
		_scanToIntentsMap = scanToIntentsMap;
		_intentToScansMap = intentToScansMap;
	}
}

uInt MSMetaData::_sizeof(
	const std::map<Double, MSMetaData::TimeStampProperties> & m
) {
	uInt sizeInt = sizeof(Int);
	uInt size = m.size()*(sizeof(Double) + sizeInt);
	std::map<Double, MSMetaData::TimeStampProperties>::const_iterator iter = m.begin();
	std::map<Double, MSMetaData::TimeStampProperties>::const_iterator end = m.end();
	uInt n = 0;
	while (iter != end) {
		n += iter->second.ddIDs.size();
		++iter;
	}
	return size + n*sizeInt;
}

template <class T>
uInt MSMetaData::_sizeof(const std::map<T, std::set<String> >& m) {
	uInt size = sizeof(T) * m.size();
	typename std::map<T, std::set<String> >::const_iterator iter = m.begin();
	typename std::map<T, std::set<String> >::const_iterator end = m.end();
	while (iter != end) {
		std::set<String>::const_iterator end2 = iter->second.end();
		for (
			std::set<String>::const_iterator iter2=iter->second.begin();
			iter2!=end2; ++iter2
		) {
			size += iter2->size();
		}
		++iter;
	}
	return size;
}

template <class T, class U>
uInt MSMetaData::_sizeof(const std::map<T, std::set<U> >& m) {
	uInt size = sizeof(T)*m.size();
	typename std::map<T, std::set<U> >::const_iterator iter = m.begin();
	typename std::map<T, std::set<U> >::const_iterator end = m.end();
	uInt nElements = 0;
	while (iter != end) {
		nElements += iter->second.size();
		++iter;
	}
	size += sizeof(U)*nElements;
	return size;
}

template <class T, class U>
uInt MSMetaData::_sizeof(const std::map<T, U>& m) {
	return m.size()*(sizeof(T) + sizeof(U));
}

uInt MSMetaData::_sizeof(const vector<std::set<String> >& m) {
	uInt size = sizeof(Int) * m.size();
	vector<std::set<String> >::const_iterator end = m.end();
	for (
		vector<std::set<String> >::const_iterator iter=m.begin();
		iter!=end; ++iter
	) {
		std::set<String>::const_iterator end2 = iter->end();
		for (
			std::set<String>::const_iterator iter2=iter->begin();
			iter2!=end2; ++iter2
		) {
			size += iter2->size();
		}
	}
	return size;
}

uInt MSMetaData::_sizeof(const vector<String>& m) {
	vector<String>::const_iterator end = m.end();
	uInt size = 0;
	for (
		vector<String>::const_iterator iter=m.begin();
		iter!=end; ++iter
	) {
		size += iter->length();
	}
	return size;
}

template <class T>
uInt MSMetaData::_sizeof(const vector<T>& v) {
	return v.size()*sizeof(T);
}

uInt MSMetaData::_sizeof(const Quantum<Vector<Double> >& m) {
	return (sizeof(Double)+10)*m.getValue().size();
}

template <class T> uInt MSMetaData::_sizeof(const std::map<String, std::set<T> >& m) {
	uInt setssize = 0;
	uInt size = 0;
	typename std::map<String, std::set<T> >::const_iterator end = m.end();
	for (
		typename std::map<String, std::set<T> >::const_iterator iter=m.begin();
		iter!=end; ++iter
	) {
		size += iter->first.size();
		setssize += iter->second.size();
	}
	size += sizeof(T) * setssize;
	return size;
}

uInt MSMetaData::_sizeof(const vector<std::map<Int, Quantity> >& m) {
	uInt size = 0;
	vector<std::map<Int, Quantity> >::const_iterator end = m.end();
	uInt intsize = sizeof(Int);
	uInt qsize = 20;
	for (
		vector<std::map<Int, Quantity> >::const_iterator iter = m.begin();
		iter!=end; ++iter
	) {
		size += iter->size()*(2*intsize + qsize);
	}
	return size;
}

std::set<String> MSMetaData::getIntentsForScan(const ScanKey& scan) const {
	_checkScan(scan);
	std::map<ScanKey, std::set<String> > scanToIntentsMap;
	std::map<String, std::set<ScanKey> > intentToScansMap;
	_getScansAndIntentsMaps(
		scanToIntentsMap,
		intentToScansMap
	);
	return scanToIntentsMap[scan];
}

Bool MSMetaData::_cacheUpdated(const Float incrementInBytes) const {
	Float newSize = _cacheMB + incrementInBytes/1e6;
	if (newSize <= _maxCacheMB) {
		_cacheMB = newSize;
		return True;
	}
	return False;
}

std::set<uInt> MSMetaData::getSpwsForIntent(const String& intent) {
	if (! _hasIntent(intent)) {
		return std::set<uInt>();
	}
	vector<std::set<String> > spwToIntentsMap = _getSpwToIntentsMap();
	std::set<uInt> spws;
	for (uInt i=0; i<spwToIntentsMap.size(); ++i) {
		if (
			spwToIntentsMap[i].find(intent) != spwToIntentsMap[i].end()
		) {
			spws.insert(i);
		}
	}
	return spws;
}

std::vector<std::set<uInt> > MSMetaData::getSpwToDataDescriptionIDMap() const {
	// TODO perhaps cache the result, but atm doesn't seem worth doing
	std::map<std::pair<uInt, uInt>, uInt> spwPolToDDID = getSpwIDPolIDToDataDescIDMap();
	std::vector<std::set<uInt> > mymap(nSpw(True));
	std::map<std::pair<uInt, uInt>, uInt>::const_iterator iter = spwPolToDDID.begin();
	std::map<std::pair<uInt, uInt>, uInt>::const_iterator end = spwPolToDDID.end();
	while (iter != end) {
		mymap[iter->first.first].insert(iter->second);
		++iter;
	}
	return mymap;
}

uInt MSMetaData::nSpw(Bool includewvr) const {
	if (_nSpw > 0) {
		return includewvr ? _nSpw : _nSpw - getWVRSpw().size();
	}
	uInt nSpw = _ms->spectralWindow().nrow();
	_nSpw = nSpw;
	return includewvr ? nSpw : nSpw - getWVRSpw().size();
}

uInt MSMetaData::nPol() {
	if (_nPol == 0) {
		_nPol = _ms->polarization().nrow();
	}
	return _nPol;
}

std::set<String> MSMetaData::getIntentsForSpw(const uInt spw) {
	if (spw >= nSpw(True)) {
		throw AipsError(
			_ORIGIN + "spectral window out of range"
		);
	}
	return _getSpwToIntentsMap()[spw];
}

std::set<String> MSMetaData::getIntentsForField(Int fieldID) {
	if (! _hasFieldID(fieldID)) {
		return std::set<String>();
	}
	vector<std::set<String> > fieldToIntentsMap;
	std::map<String, std::set<Int> > intentToFieldsMap;
	_getFieldsAndIntentsMaps(
		fieldToIntentsMap, intentToFieldsMap
	);
	return fieldToIntentsMap[fieldID];
}


uInt MSMetaData::nFields() const {
	if (_nFields > 0) {
		return _nFields;
	}
	uInt nFields = _ms->field().nrow();
	_nFields = nFields;
	return nFields;
}

void MSMetaData::_getFieldsAndSpwMaps(
	std::map<Int, std::set<uInt> >& fieldToSpwMap,
	vector<std::set<Int> >& spwToFieldMap
) {
	// This method has the responsibility of setting _fieldToSpwMap and _spwToFieldIDMap
	if (! _fieldToSpwMap.empty() && ! _spwToFieldIDsMap.empty()) {
		fieldToSpwMap = _fieldToSpwMap;
		spwToFieldMap = _spwToFieldIDsMap;
		return;
	}
	CountedPtr<Vector<Int> >  allDDIDs = _getDataDescIDs();
	CountedPtr<Vector<Int> >  allFieldIDs = _getFieldIDs();
	Vector<Int>::const_iterator endDDID = allDDIDs->end();
	Vector<Int>::const_iterator curField = allFieldIDs->begin();
	fieldToSpwMap.clear();
	spwToFieldMap.resize(nSpw(True));
	vector<uInt> ddidToSpwMap = _getDataDescIDToSpwMap();
	for (
		Vector<Int>::const_iterator curDDID=allDDIDs->begin();
		curDDID!=endDDID; ++curDDID, ++curField
	) {
		uInt spw = ddidToSpwMap[*curDDID];
		fieldToSpwMap[*curField].insert(spw);
		spwToFieldMap[spw].insert(*curField);
	}
	std::map<Int, std::set<uInt> >::const_iterator mapEnd = fieldToSpwMap.end();
	uInt mySize = 0;
	for (
		std::map<Int, std::set<uInt> >::const_iterator curMap = fieldToSpwMap.begin();
		curMap != mapEnd; ++curMap
	) {
		mySize += curMap->second.size();
	}
	mySize *= sizeof(uInt);
	mySize += sizeof(Int) * fieldToSpwMap.size() + sizeof(uInt)*spwToFieldMap.size();
	vector<std::set<Int> >::const_iterator map2End = spwToFieldMap.end();
	uInt count = 0;
	for (
		vector<std::set<Int> >::const_iterator curMap = spwToFieldMap.begin();
		curMap != map2End; ++curMap
	) {
		count += curMap->size();
	}
	mySize += sizeof(Int)*count;
	if (_cacheUpdated(mySize)) {
		_fieldToSpwMap = fieldToSpwMap;
		_spwToFieldIDsMap = spwToFieldMap;
	}
}

std::set<uInt> MSMetaData::getSpwsForField(Int fieldID) {
	if (! _hasFieldID(fieldID)) {
		return std::set<uInt>();
	}
	std::map<Int, std::set<uInt> > myFieldToSpwMap;
	vector<std::set<Int> > mySpwToFieldMap;
	_getFieldsAndSpwMaps(myFieldToSpwMap, mySpwToFieldMap);
    return myFieldToSpwMap[fieldID];
}

std::set<uInt> MSMetaData::getSpwsForField(const String& fieldName) {
	uInt myNFields = nFields();
	vector<String> fieldNames = _getFieldNames();
	std::set<uInt> spws;
	for (uInt i=0; i<myNFields; ++i) {
		if (fieldNames[i] == fieldName) {
			std::set<uInt> myspws = getSpwsForField(i);
			spws.insert(myspws.begin(), myspws.end());
		}
	}
	ThrowIf(
		spws.empty(),
		_ORIGIN + "field (" + fieldName + " does not exist."
	);
	return spws;
}

vector<String> MSMetaData::_getFieldNames() const {
	if (! _fieldNames.empty()) {
		return _fieldNames;
	}

	String fieldNameColName = MSField::columnName(MSFieldEnums::NAME);
	ROScalarColumn<String> nameCol(_ms->field(), fieldNameColName);
	vector<String> fieldNames = nameCol.getColumn().tovector();
	uInt mysize = 0;
	vector<String>::const_iterator end = fieldNames.end();
	for (
		vector<String>::const_iterator name=fieldNames.begin();
		name!=end; ++name
	) {
		mysize += name->size();
	}
	if (_cacheUpdated(mysize)) {
		_fieldNames = fieldNames;
	}
	return fieldNames;
}

std::set<Int> MSMetaData::getFieldIDsForSpw(const uInt spw) {
	uInt myNSpw = nSpw(True);
	if (spw >= myNSpw) {
		throw AipsError(_ORIGIN + "spectral window out of range");
	}
	std::map<Int, std::set<uInt> > myFieldToSpwMap;
	vector<std::set<Int> > mySpwToFieldMap;
	_getFieldsAndSpwMaps(myFieldToSpwMap, mySpwToFieldMap);
	return mySpwToFieldMap[spw];
}

std::set<String> MSMetaData::getFieldNamesForSpw(const uInt spw) {
	std::set<Int> fieldIDs = getFieldIDsForSpw(spw);
	std::set<String> fieldNames;
	vector<String> allFieldNames = _getFieldNames();
	for (
		std::set<Int>::const_iterator fieldID = fieldIDs.begin();
		fieldID!=fieldIDs.end(); ++fieldID
	) {
		fieldNames.insert(allFieldNames[*fieldID]);
	}
	return fieldNames;
}

std::set<ScanKey> MSMetaData::_getScanKeys() const {
	if (! _scanKeys.empty()) {
		return _scanKeys;
	}
	std::set<ScanKey> scanKeys;
	std::set<SubScanKey> subScanKeys = _getSubScanKeys();
	std::set<SubScanKey>::const_iterator iter = subScanKeys.begin();
	std::set<SubScanKey>::const_iterator end = subScanKeys.end();
	while (iter != end) {
		scanKeys.insert(scanKey(*iter));
		++iter;
	}
	if (_cacheUpdated(sizeof(ScanKey)*scanKeys.size())) {
		_scanKeys = scanKeys;
	}
	return scanKeys;
}

std::set<ScanKey> MSMetaData::_getScanKeys(const ArrayKey& arrayKey) const {
	std::set<ScanKey> allScanKeys = _getScanKeys();
	Bool doAllObsIDs = arrayKey.obsID < 0;
	Bool doAllArrayIDs = arrayKey.arrayID < 0;
	if (doAllObsIDs && doAllArrayIDs) {
		return allScanKeys;
	}
	std::set<ScanKey> scanKeys;
	std::set<ScanKey>::const_iterator iter = allScanKeys.begin();
	std::set<ScanKey>::const_iterator end = allScanKeys.end();

	while (iter != end) {
		if (
			(doAllObsIDs || iter->obsID == arrayKey.obsID)
			&& (doAllArrayIDs || iter->arrayID == arrayKey.arrayID)
		) {
			scanKeys.insert(*iter);
		}
		++iter;
	}
	return scanKeys;
}

std::set<ScanKey> MSMetaData::_getScanKeys(
	const std::set<ScanKey>& scanKeys, const ArrayKey& arrayKey
) const {
	Int obsID = arrayKey.obsID;
	Int arrayID = arrayKey.arrayID;
	std::set<ScanKey>::const_iterator iter = scanKeys.begin();
	std::set<ScanKey>::const_iterator end = scanKeys.end();
	std::set<ScanKey> filteredKeys;
	while (iter != end) {
		if (iter->obsID == obsID && iter->arrayID == arrayID) {
			filteredKeys.insert(*iter);
		}
	}
	return filteredKeys;
}

std::set<Int> MSMetaData::_getScanNumbers(const ArrayKey& arrayKey) const {
	std::set<ScanKey> scans = _getScanKeys(arrayKey);
	std::set<Int> scanNumbers;
	std::set<ScanKey>::const_iterator iter = scans.begin();
	std::set<ScanKey>::const_iterator end = scans.end();
	Int obsID = arrayKey.obsID;
	Int arrayID = arrayKey.arrayID;
	while (iter != end) {
		if (iter->obsID == obsID && iter->arrayID == arrayID) {
			scanNumbers.insert(iter->scan);
		}
		++iter;
	}
	return scanNumbers;
}

std::set<Int> MSMetaData::_getScanNumbers(
	const std::set<ScanKey>& scanKeys
) const {
	std::set<Int> scanNumbers;
	std::set<ScanKey>::const_iterator iter = scanKeys.begin();
	std::set<ScanKey>::const_iterator end = scanKeys.end();
	while (iter != end) {
		scanNumbers.insert(iter->scan);
		++iter;
	}
	return scanNumbers;
}

void MSMetaData::_getScansAndDDIDMaps(
	std::map<ScanKey, std::set<uInt> >& scanToDDIDMap,
	vector<std::set<ScanKey> >& ddIDToScanMap
) const {
	// this method is responsible for setting _scanToDDIDsMap and _ddidToScansMap
	if (! _scanToDDIDsMap.empty()) {
		scanToDDIDMap = _scanToDDIDsMap;
		ddIDToScanMap = _ddidToScansMap;
		return;
	}
	scanToDDIDMap.clear();
	ddIDToScanMap.clear();
	ddIDToScanMap.resize(nDataDescriptions());
	std::map<SubScanKey, SubScanProperties> subScanProps = _getSubScanProperties();
	std::map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps.begin();
	std::map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps.end();
	ScanKey myScanKey;
	std::set<uInt> ddIDs;
	std::set<uInt>::const_iterator dIter;
	std::set<uInt>::const_iterator dEnd;
	while (iter != end) {
		myScanKey = scanKey(iter->first);
		ddIDs = iter->second.ddIDs;
		scanToDDIDMap[myScanKey].insert(ddIDs.begin(), ddIDs.end());
		dIter = ddIDs.begin();
		dEnd = ddIDs.end();
		while (dIter != dEnd) {
			ddIDToScanMap[*dIter].insert(myScanKey);
			++dIter;
		}
		++iter;
	}
	if (_cacheUpdated(_sizeof(scanToDDIDMap)) + _sizeof(ddIDToScanMap)) {
		_scanToDDIDsMap = scanToDDIDMap;
		_ddidToScansMap = ddIDToScanMap;
	}
}

void MSMetaData::_getScansAndSpwMaps(
	std::map<ScanKey, std::set<uInt> >& scanToSpwMap,
	vector<std::set<ScanKey> >& spwToScanMap
) const {
	// This method is responsible for setting _scanToSpwsMap and _spwToScansMap
	if (! _scanToSpwsMap.empty() && ! _spwToScansMap.empty()) {
		scanToSpwMap = _scanToSpwsMap;
		spwToScanMap = _spwToScansMap;
		return;
	}
	scanToSpwMap.clear();
	spwToScanMap.clear();
	spwToScanMap.resize(nSpw(True));
	std::map<ScanKey, std::set<uInt> > scanToDDIDMap;
	vector<std::set<ScanKey> > ddIDToScanMap;
	_getScansAndDDIDMaps(scanToDDIDMap, ddIDToScanMap);
	vector<uInt> ddToSpw = _getDataDescIDToSpwMap();
	std::map<ScanKey, std::set<uInt> >::const_iterator iter = scanToDDIDMap.begin();
	std::map<ScanKey, std::set<uInt> >::const_iterator end = scanToDDIDMap.end();
	std::set<uInt>::const_iterator dIter;
	std::set<uInt>::const_iterator dEnd;
	while (iter != end) {
		ScanKey scanKey = iter->first;
		std::set<uInt> ddids = scanToDDIDMap[scanKey];
		dIter = ddids.begin();
		dEnd = ddids.end();
		while (dIter != dEnd) {
			uInt spw = ddToSpw[*dIter];
			scanToSpwMap[scanKey].insert(spw);
			spwToScanMap[spw].insert(scanKey);
			++dIter;
		}
		++iter;
	}
	if (_cacheUpdated(_sizeof(scanToSpwMap)) + _sizeof(spwToScanMap)) {
		_scanToSpwsMap = scanToSpwMap;
		_spwToScansMap = spwToScanMap;
	}
}

template <class T>
uInt MSMetaData::_sizeof(const vector<std::set<T> >& v) {
	uInt size = 0;
	typename vector<std::set<T> >::const_iterator iter = v.begin();
	typename vector<std::set<T> >::const_iterator end = v.end();
	while (iter != end) {
		size = iter->size();
		++iter;
	}
	size *= sizeof(T);
	return size;
}

std::set<uInt> MSMetaData::getSpwsForScan(const ScanKey& scan) const {
    _checkScan(scan);
    std::map<ScanKey, std::set<uInt> > scanToSpwMap;
    vector<std::set<ScanKey> > spwToScanMap;
    _getScansAndSpwMaps(
    	scanToSpwMap, spwToScanMap
    );
    return scanToSpwMap[scan];
}

std::set<Int> MSMetaData::getScansForSpw(
	const uInt spw, Int obsID, Int arrayID
) const {
	uInt myNSpw = nSpw(True);
	ThrowIf(
		spw >= myNSpw, "spectral window out of range"
	);
	ArrayKey arrayKey;
	arrayKey.obsID = obsID;
	arrayKey.arrayID = arrayID;
	std::set<ScanKey> myScanKeys = _getScanKeys(arrayKey);
	std::map<ScanKey, std::set<uInt> > scanToSpwMap;
	vector<std::set<ScanKey> > spwToScanMap;
	_getScansAndSpwMaps(
		scanToSpwMap, spwToScanMap
	);
	std::set<ScanKey>::const_iterator iter = myScanKeys.begin();
	std::set<ScanKey>::const_iterator end = myScanKeys.end();
	std::set<Int> scanNumbers;
	std::set<uInt> spws;
	while (iter != end) {
		spws = scanToSpwMap[*iter];
		if (spws.find(spw) != spws.end()) {
			scanNumbers.insert(iter->scan);
		}

		++iter;
	}
	return scanNumbers;
}

uInt MSMetaData::nAntennas() const {
	if (_nAntennas > 0) {
		return _nAntennas;
	}
	uInt nAnts = _ms->antenna().nrow();
	_nAntennas = nAnts;
	return nAnts;
}

uInt MSMetaData::nDataDescriptions() const {
	if (_nDataDescIDs == 0) {
		_nDataDescIDs = _ms->dataDescription().nrow();
	}
	return _nDataDescIDs;
}

vector<String> MSMetaData::_getAntennaNames(
	std::map<String, uInt>& namesToIDsMap
) {
	if (! _antennaNames.empty()) {
		namesToIDsMap = _antennaNameToIDMap;
		return _antennaNames;
	}
	namesToIDsMap.clear();
	std::map<String, uInt> mymap;
	String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
	ROScalarColumn<String> nameCol(_ms->antenna(), antNameColName);
	Vector<String> names = nameCol.getColumn();
	Vector<String>::const_iterator end = names.end();
	uInt i = 0;
	for (
		Vector<String>::const_iterator name=names.begin();
		name!=end; ++name, ++i
	) {
		namesToIDsMap[*name] = i;
	}

	uInt mysize = names.size()*sizeof(uInt);
	for (
		Vector<String>::const_iterator name=names.begin();
		name!=end; ++name
	) {
		mysize += 2*name->size();
	}
	if (_cacheUpdated(mysize)) {
		_antennaNames = names.tovector();
		_antennaNameToIDMap = namesToIDsMap;
	}
	return names.tovector();
}

vector<String> MSMetaData::getAntennaNames(
	std::map<String, uInt>& namesToIDsMap,
	const vector<uInt>& antennaIDs
) {
	uInt nAnts = nAntennas();
	std::map<String, uInt> allMap;
	vector<String> allNames = _getAntennaNames(allMap);
	if (antennaIDs.empty()) {
		namesToIDsMap = allMap;
		return allNames;
	}
	uInt mymax = max(Vector<uInt>(antennaIDs));
	ThrowIf(
		mymax >= nAnts,
		"Antenna ID " + String::toString(mymax)
		+ " out of range."
	);
	vector<String> names;
	vector<uInt>::const_iterator end = antennaIDs.end();
	for (
		vector<uInt>::const_iterator id=antennaIDs.begin();
		id!=end; ++id
	) {
		String antName = allNames[*id];
		names.push_back(antName);
		namesToIDsMap[antName] = *id;
	}
	return names;
}

vector<uInt> MSMetaData::getAntennaIDs(
	const vector<String>& antennaNames
) {
	std::map<String, uInt> namesToIDsMap;
	vector<String> names = getAntennaNames(namesToIDsMap);
	vector<String>::const_iterator end = antennaNames.end();
	std::map<String, uInt>::const_iterator mapEnd = namesToIDsMap.end();
	vector<uInt> ids;
	for (
		vector<String>::const_iterator name=antennaNames.begin();
		name!=end; ++name
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

vector<std::map<Int, Quantity> > MSMetaData::getFirstExposureTimeMap() {
	if (! _firstExposureTimeMap.empty()) {
		return _firstExposureTimeMap;
	}
	uInt nDataDescIDs = nDataDescriptions();
	CountedPtr<Vector<Int> > scans = _getScans();
	CountedPtr<Vector<Int> > dataDescIDs = _getDataDescIDs();
	CountedPtr<Vector<Double> > times = _getTimes();
	CountedPtr<Quantum<Vector<Double> > > exposureTimes = _getExposureTimes();
	vector<std::map<Int, Quantity> > firstExposureTimeMap(nDataDescIDs);
	vector<std::map<Int, Double> > tmap(nDataDescIDs);
	Vector<Int>::const_iterator siter = scans->begin();
	Vector<Int>::const_iterator send = scans->end();
	Vector<Int>::const_iterator diter = dataDescIDs->begin();
	Vector<Double>::const_iterator titer = times->begin();
	Vector<Double> eTimes = exposureTimes->getValue();
	String unit = exposureTimes->getUnit();
	Vector<Double>::const_iterator eiter = eTimes.begin();
	while (siter != send) {
		std::map<Int, Quantity> mymap = firstExposureTimeMap[*diter];
		if (
			mymap.find(*siter) == mymap.end()
			|| *titer < tmap[*diter][*siter]
		) {
			firstExposureTimeMap[*diter][*siter] = Quantity(*eiter, unit);
			tmap[*diter][*siter] = *titer;
		}
		++siter;
		++diter;
		++titer;
		++eiter;
	}

	if (_cacheUpdated(_sizeof(firstExposureTimeMap))) {
		_firstExposureTimeMap = firstExposureTimeMap;
	}
	return firstExposureTimeMap;
}

vector<String> MSMetaData::getAntennaStations(const vector<uInt>& antennaIDs) {
	vector<String> allStations = _getStationNames();
	if (antennaIDs.empty()) {
		return allStations;
	}
	_hasAntennaID(max(Vector<uInt>(antennaIDs)));
	vector<String> myStationNames;
	vector<uInt>::const_iterator end = antennaIDs.end();
	for (
		vector<uInt>::const_iterator iter=antennaIDs.begin();
		iter!=end; ++iter
	) {
		myStationNames.push_back(allStations[*iter]);
	}
	return myStationNames;
}

vector<String> MSMetaData::getAntennaStations(const vector<String>& antennaNames) {
	return getAntennaStations(getAntennaIDs(antennaNames));
}

vector<String> MSMetaData::_getStationNames() {
	if (! _stationNames.empty()) {
		return _stationNames;
	}
	String antStationColName = MSAntenna::columnName(MSAntennaEnums::STATION);
	vector<String> stationNames = ROScalarColumn<String>(
		_ms->antenna(), antStationColName
	).getColumn().tovector();
	if (_cacheUpdated(_sizeof(stationNames))) {
		_stationNames = stationNames;
	}
	return stationNames;
}

std::set<SubScanKey> MSMetaData::_getSubScanKeys() const {
	// responsible for setting _subscans
	if (! _subscans.empty()) {
		return _subscans;
	}
	std::set<SubScanKey> mysubscans;
	CountedPtr<Vector<Int> > scans = _getScans();
	CountedPtr<Vector<Int> > fields = _getFieldIDs();
	CountedPtr<Vector<Int> > arrays = _getArrayIDs();
	CountedPtr<Vector<Int> > obs = _getObservationIDs();
	Vector<Int>::const_iterator scanIter = scans->begin();
	Vector<Int>::const_iterator scanEnd = scans->end();
	Vector<Int>::const_iterator fIter = fields->begin();
	Vector<Int>::const_iterator oIter = obs->begin();
	Vector<Int>::const_iterator aIter = arrays->begin();
	SubScanKey subScanKey;
	while (scanIter != scanEnd) {
		subScanKey.obsID = *oIter;
		subScanKey.arrayID = *aIter;
		subScanKey.scan = *scanIter;
		subScanKey.fieldID = *fIter;
		mysubscans.insert(subScanKey);
		++scanIter;
		++fIter;
		++oIter;
		++aIter;
	}
	if (_cacheUpdated(mysubscans.size()*sizeof(SubScanKey))) {
		_subscans = mysubscans;
	}
	return mysubscans;
}

std::set<SubScanKey> MSMetaData::_getSubScanKeys(const ScanKey& scanKey) const {
	_checkScan(scanKey);
	return _getScanToSubScansMap()[scanKey];
}

std::map<ScanKey, std::set<SubScanKey> > MSMetaData::_getScanToSubScansMap() const {
	// sets _scanToSubScans;
	if (! _scanToSubScans.empty()) {
		return _scanToSubScans;
	}
	std::set<SubScanKey> allSubScans = _getSubScanKeys();
	std::set<SubScanKey>::const_iterator iter = allSubScans.begin();
	std::set<SubScanKey>::const_iterator end = allSubScans.end();
	std::map<ScanKey, std::set<SubScanKey> > mymap;
	ScanKey scanKey;
	while (iter != end) {
		scanKey.obsID = iter->obsID;
		scanKey.arrayID = iter->arrayID;
		scanKey.scan = iter->scan;
		mymap[scanKey].insert(*iter);
		++iter;
	}
	if (_cacheUpdated(_sizeof(mymap))) {
		_scanToSubScans = mymap;
	}
	return mymap;
}

Quantum<Vector<Double> > MSMetaData::getAntennaDiameters() {
	if (! _antennaDiameters.getValue().empty()) {
		return _antennaDiameters;
	}
	String antDiamColName = MSAntenna::columnName(MSAntennaEnums::DISH_DIAMETER);
	ROScalarColumn<Double> diamCol(_ms->antenna(), antDiamColName);
	Vector<Double> diams = diamCol.getColumn();
	String unit = *diamCol.keywordSet().asArrayString("QuantumUnits").begin();
	Quantum<Vector<Double> > antennaDiameters = Quantum<Vector<Double> >(diams, unit);
	if (_cacheUpdated(_sizeof(antennaDiameters))) {
		_antennaDiameters = antennaDiameters;
	}
	return antennaDiameters;
}

std::set<uInt> MSMetaData::getTDMSpw() {
	if (! _tdmSpw.empty()) {
		return _tdmSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return tdmSpw;
}

vector<Double> MSMetaData::getBandWidths() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Double> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->bandwidth);
	}
	return out;
}

vector<QVD> MSMetaData::getChanFreqs() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<QVD> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->chanfreqs);
	}
	return out;
}

vector<QVD> MSMetaData::getChanWidths() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<QVD> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->chanwidths);
	}
	return out;
}

vector<Int> MSMetaData::getNetSidebands() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Int> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->netsideband);
	}
	return out;
}

vector<Quantity> MSMetaData::getMeanFreqs() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Quantity> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->meanfreq);
	}
	return out;
}

vector<Quantity> MSMetaData::getCenterFreqs() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Quantity> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->centerfreq);
	}
	return out;
}

vector<uInt> MSMetaData::nChans() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<uInt> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->nchans);
	}
	return out;
}

vector<vector<Double> > MSMetaData::getEdgeChans() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<vector<Double> > out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->edgechans);
	}
	return out;
}

vector<uInt> MSMetaData::getBBCNos() const {
	if (! hasBBCNo()) {
		throw AipsError("This MS's SPECTRAL_WINDOW table does not have a BBC_NO column");
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<uInt> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->bbcno);
	}
	return out;
}

std::map<uInt, std::set<uInt> > MSMetaData::getBBCNosToSpwMap(
	SQLDSwitch sqldSwitch
) {
	vector<uInt> mymap = getBBCNos();
	std::map<uInt, std::set<uInt> > out;
	vector<uInt>::const_iterator end = mymap.end();
	std::set<uInt> sqldSpw;
	if (sqldSwitch != SQLD_INCLUDE) {
		std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw;
		_getSpwInfo(
			avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
		);
	}
	uInt i = 0;
	Bool found = True;
	for (
		vector<uInt>::const_iterator iter=mymap.begin();
		iter!=end; ++iter, ++i
	) {
		if (out.find(*iter) == out.end()) {
			out[*iter] = std::set<uInt>();
		}
		if (sqldSwitch != SQLD_INCLUDE) {
			found = sqldSpw.find(i) != sqldSpw.end();
		}
		if (
			sqldSwitch == SQLD_INCLUDE
			|| (
				sqldSwitch == SQLD_EXCLUDE
				&& ! found
			)
			|| (
				sqldSwitch == SQLD_ONLY
				&& found
			)
		) {
			out[*iter].insert(i);
		}
	}
	return out;
}

vector<String> MSMetaData::getSpwNames() const {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<String> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; ++iter
	) {
		out.push_back(iter->name);
	}
	return out;
}

std::set<uInt> MSMetaData::getFDMSpw() {
	if (! _fdmSpw.empty()) {
		return _fdmSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return fdmSpw;
}

std::set<uInt> MSMetaData::getChannelAvgSpw() {
	if (! _avgSpw.empty()) {
		return _avgSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return avgSpw;
}

std::set<uInt> MSMetaData::getWVRSpw() const {
	if (_spwInfoStored) {
		return _wvrSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return wvrSpw;
}

std::set<uInt> MSMetaData::getSQLDSpw() {
	if (_spwInfoStored) {
		return _sqldSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return sqldSpw;
}

std::set<Int> MSMetaData::getScansForTimes(
	Double center, Double tol, Int obsID, Int arrayID
) const {
	_checkTolerance(tol);
	// std::set<Int> uniqueScans = getScanNumbers();
	ArrayKey arrayKey;
	arrayKey.obsID = obsID;
	arrayKey.arrayID = arrayID;
	std::set<ScanKey> uniqueScans = _getScanKeys(arrayKey);
	CountedPtr<std::map<ScanKey, std::set<Double> > > scanToTimesMap = _getScanToTimesMap();
	Double minTime = center - tol;
	Double maxTime = center + tol;
	std::set<Int> scans;
	std::set<ScanKey>::const_iterator scan = uniqueScans.begin();
	std::set<ScanKey>::const_iterator end = uniqueScans.end();
	while (scan != end) {
		std::set<Double> times = scanToTimesMap->find(*scan)->second;
		if (*(++times.rend()) >= minTime && *times.begin() <= maxTime) {
			scans.insert(scan->scan);
		}
		++scan;
	}
	return scans;
}

CountedPtr<std::map<ScanKey, std::set<Double> > > MSMetaData::_getScanToTimesMap() const {
	if (_scanToTimesMap && ! _scanToTimesMap->empty()) {
		return _scanToTimesMap;
	}
	CountedPtr<Vector<Int> > scans = _getScans();
	CountedPtr<Vector<Int> > obsIDs = _getObservationIDs();
	CountedPtr<Vector<Int> > arrayIDs = _getArrayIDs();
	Vector<Int>::const_iterator curScan = scans->begin();
	Vector<Int>::const_iterator lastScan = scans->end();
	CountedPtr<Vector<Double> > times = _getTimes();
	Vector<Double>::const_iterator curTime = times->begin();
	Vector<Int>::const_iterator curObs = obsIDs->begin();
	Vector<Int>::const_iterator curArray = arrayIDs->begin();
	CountedPtr<std::map<ScanKey, std::set<Double> > > scanToTimesMap(
		new std::map<ScanKey, std::set<Double> >()
	);
	ScanKey scanKey;
	while (curScan != lastScan) {
		scanKey.obsID = *curObs;
		scanKey.arrayID = *curArray;
		scanKey.scan = *curScan;
		(*scanToTimesMap)[scanKey].insert(*curTime);
		++curScan;
		++curTime;
		++curObs;
		++curArray;
	}
	if (_cacheUpdated(_sizeof(*scanToTimesMap))) {
		_scanToTimesMap = scanToTimesMap;
	}
	return scanToTimesMap;
}

CountedPtr<Vector<Double> > MSMetaData::_getTimes() const {
	if (_times && ! _times->empty()) {
		return _times;
	}
	String timeColName = MeasurementSet::columnName(MSMainEnums::TIME);
	CountedPtr<Vector<Double> > times(
		new Vector<Double>(	 ScalarColumn<Double>(*_ms, timeColName).getColumn())
	);
	if (_cacheUpdated(sizeof(Double)*times->size())) {
		_times = times;
	}
	return times;
}

/*
CountedPtr<std::map<Double, MSMetaData::TimeStampProperties> > MSMetaData::_getTimeStampProperties() const {
	if (_timeStampPropsMap) {
		return _timeStampPropsMap;
	}
	CountedPtr<Vector<Double> > times = _getTimes();
	CountedPtr<Vector<Int> > ddids = _getDataDescIDs();
	CountedPtr<std::map<Double, TimeStampProperties> > mymap(
		new std::map<Double, TimeStampProperties>()
	);
	Vector<Double>::const_iterator titer = times->begin();
	Vector<Double>::const_iterator tend = times->end();
	Vector<Int>::const_iterator diter = ddids->begin();
	while (titer != tend) {
		if (mymap->find(*titer) == mymap->end()) {
			(*mymap)[*titer].nrows = 1;
		}
		else {
			++((*mymap)[*titer].nrows);
		}
		(*mymap)[*titer].ddIDs.insert(*diter);
		++titer;
		++diter;
	}
	if (_cacheUpdated(_sizeof(*mymap))) {
		_timeStampPropsMap = mymap;
	}
	return mymap;
}
*/


CountedPtr<Quantum<Vector<Double> > > MSMetaData::_getExposureTimes() {
	if (_exposures && ! _exposures->getValue().empty()) {
		return _exposures;
	}
	String colName = MeasurementSet::columnName(MSMainEnums::EXPOSURE);
	ScalarColumn<Double> exposure (*_ms, colName);
	String unit = *exposure.keywordSet().asArrayString("QuantumUnits").begin();
	CountedPtr<Quantum<Vector<Double> > > ex(
		new Quantum<Vector<Double> >(exposure.getColumn(), unit)
	);
	if (_cacheUpdated((20 + sizeof(Double))*ex->getValue().size())) {
		_exposures = ex;
	}
	return ex;
}

CountedPtr<ArrayColumn<Bool> > MSMetaData::_getFlags() const {
	if (_flagsColumn && _flagsColumn->nrow() > 0) {
		return _flagsColumn;
	}
	String flagColName = MeasurementSet::columnName(MSMainEnums::FLAG);
	CountedPtr<ArrayColumn<Bool> > flagsColumn(
		new ArrayColumn<Bool>(*_ms, flagColName)
	);
	uInt mysize = 0;
	for (uInt i=0; i<flagsColumn->nrow(); ++i) {
		mysize += flagsColumn->get(i).size();
	}
	if (_cacheUpdated(sizeof(Bool)*mysize)) {
		_flagsColumn = flagsColumn;
	}
	return flagsColumn;
}

std::set<Double> MSMetaData::getTimesForScans(
	std::set<ScanKey> scans
) const {
	std::set<Double> times;
	if (scans.empty()) {
		CountedPtr<Vector<Double> > allTimes = _getTimes();
		times.insert(allTimes->begin(), allTimes->end());
		return times;
	}
	CountedPtr<std::map<ScanKey, std::set<Double> > > scanToTimesMap = _getScanToTimesMap();
	// std::set<Int> scanNumbers = getScanNumbers();
	std::set<ScanKey>::const_iterator scan = scans.begin();
	std::set<ScanKey>::const_iterator end = scans.end();
	std::set<ScanKey> scanKeys = _getScanKeys();
	while (scan != end) {
		_checkScan(*scan);
		times.insert(
			scanToTimesMap->find(*scan)->second.begin(),
			scanToTimesMap->find(*scan)->second.end()
		);
		++scan;
	}
	return times;
}

std::set<Double> MSMetaData::getTimesForScan(const ScanKey& scan) const {
	std::set<ScanKey> scans;
	scans.insert(scan);
	// scan validity check is done in getTimesForScans()
	return getTimesForScans(scans);
}

void MSMetaData::_getTimesAndInvervals(
	std::map<ScanKey, std::pair<Double, Double> >& scanToTimeRangeMap,
	std::map<ScanKey, std::map<uInt, Double> >& scanSpwToAverageIntervalMap
) const {
	if (
		! _scanToTimeRangeMap.empty()
		&& ! _scanSpwToIntervalMap.empty()
	) {
		scanToTimeRangeMap = _scanToTimeRangeMap;
		scanSpwToAverageIntervalMap = _scanSpwToIntervalMap;
		return;
	}
	scanToTimeRangeMap.clear();
	CountedPtr<Vector<Int> > obsIDs = _getObservationIDs();
	Vector<Int>::const_iterator oIter = obsIDs->begin();
	CountedPtr<Vector<Int> > arrayIDs = _getArrayIDs();
	Vector<Int>::const_iterator aIter = arrayIDs->begin();
	CountedPtr<Vector<Int> > scans = _getScans();
	Vector<Int>::const_iterator sIter = scans->begin();
	Vector<Int>::const_iterator sEnd = scans->end();

	CountedPtr<Vector<Int> > dataDescIDs = _getDataDescIDs();
	Vector<Int>::const_iterator dIter = dataDescIDs->begin();
	CountedPtr<Vector<Double> > times = _getTimes();

	Vector<Double>::const_iterator  tIter = times->begin();
	String intervalColName = MeasurementSet::columnName(MSMainEnums::INTERVAL);
	Vector<Double> intervals = ScalarColumn<Double>(*_ms, intervalColName).getColumn();
	Vector<Double>::const_iterator  iIter = intervals.begin();
	scanSpwToAverageIntervalMap.clear();
	std::map<ScanKey, std::map<uInt, uInt> > counts;
	vector<uInt> dataDesIDToSpwMap = _getDataDescIDToSpwMap();
	ScanKey scanKey;
	//std::pair<ScanKey, uInt> scanSpwToAverageIntervalKey;
	uInt mysize = 0;
	while (sIter != sEnd) {
		scanKey.obsID = *oIter;
		scanKey.arrayID = *aIter;
		scanKey.scan = *sIter;
		Double half = *iIter/2;
		if (scanToTimeRangeMap.find(scanKey) == scanToTimeRangeMap.end()) {
			scanToTimeRangeMap[scanKey] = std::pair<Double, Double>(*tIter-half, *tIter+half);
		}
		else {
			scanToTimeRangeMap[scanKey].first = min(scanToTimeRangeMap[scanKey].first, *tIter-half);
			scanToTimeRangeMap[scanKey].second = max(scanToTimeRangeMap[scanKey].second, *tIter+half);
		}
		uInt spw = dataDesIDToSpwMap[*dIter];
		if (
			(
				scanSpwToAverageIntervalMap.find(scanKey)
				== scanSpwToAverageIntervalMap.end()
			)
			|| (
				scanSpwToAverageIntervalMap.find(scanKey)->second.find(spw)
				== scanSpwToAverageIntervalMap.find(scanKey)->second.end()
			)
		) {
			// map does not yet have key scanKey or second map does not yet
			// have key spw
			scanSpwToAverageIntervalMap[scanKey][spw] = *iIter;
			counts[scanKey][spw] = 1;
		}
		else {
			scanSpwToAverageIntervalMap[scanKey][spw] += *iIter;
			++counts[scanKey][spw];
		}
		++sIter;
		++tIter;
		++iIter;
		++dIter;
		++oIter;
		++aIter;
	}
	std::map<ScanKey, std::map<uInt, Double> >::iterator mIter = scanSpwToAverageIntervalMap.begin();
	std::map<ScanKey, std::map<uInt, Double> >::iterator mEnd = scanSpwToAverageIntervalMap.end();
	std::map<uInt, Double>::iterator pIter, pEnd;
	uInt spw;
	while (mIter != mEnd) {
		scanKey = mIter->first;
		pIter = mIter->second.begin();
		pEnd = mIter->second.end();
		while (pIter != pEnd) {
			spw = pIter->first;
			pIter->second /= counts[scanKey][spw];
			++pIter;
		}
		mysize += mIter->second.size()*(sizeof(uInt) + sizeof(Double));
		++mIter;
	}
	mysize *= sizeof(ScanKey);
	mysize += scanToTimeRangeMap.size()*(sizeof(ScanKey)+2*sizeof(Double));
	//mysize += scanSpwToAverageIntervalMap.size()*sizeof(ScanKey) * nSpw(True) * sizeof(Double);
	if (_cacheUpdated(mysize)) {
		_scanToTimeRangeMap = scanToTimeRangeMap;
		_scanSpwToIntervalMap = scanSpwToAverageIntervalMap;
	}
}

std::pair<Double, Double> MSMetaData::getTimeRangeForScan(
	const ScanKey& scanKey
) const {
	_checkScan(scanKey);

	// use this (orignal) method, rather than the subscans method because
	// this method takes into account finite intervals (eg +/- half intervals
	// and the min and max timestamps.

	std::map<ScanKey, std::pair<Double,Double> > scanToTimeRangeMap;
	std::map<ScanKey, std::map<uInt, Double> > scanSpwToAverageIntervalMap;
	_getTimesAndInvervals(scanToTimeRangeMap, scanSpwToAverageIntervalMap);
	return scanToTimeRangeMap.find(scanKey)->second;
}

std::pair<Double, Double> MSMetaData::getTimeRange() const {
	std::map<ScanKey, std::pair<Double,Double> > scanToTimeRangeMap;
	std::map<ScanKey, std::map<uInt, Double> > scanSpwToAverageIntervalMap;
	_getTimesAndInvervals(scanToTimeRangeMap, scanSpwToAverageIntervalMap);
	std::map<ScanKey, std::pair<Double,Double> >::const_iterator iter = scanToTimeRangeMap.begin();
	std::map<ScanKey, std::pair<Double,Double> >::const_iterator end = scanToTimeRangeMap.end();
	std::pair<Double, Double> timerange(
		iter->second.first, iter->second.second
	);

	++iter;
	while (iter != end) {
		timerange.first = min(timerange.first, iter->second.first);
		timerange.second = max(timerange.second, iter->second.second);
		++iter;
	}
	return timerange;
}

std::map<uInt, Double> MSMetaData::getAverageIntervalsForScan(
	const ScanKey& scan
) const {
	_checkScan(scan);
	if (! _scanSpwToIntervalMap.empty()) {
		return _scanSpwToIntervalMap[scan];
	}
	std::map<ScanKey, std::pair<Double, Double> > scanToTimeRangeMap;
	std::map<ScanKey, std::map<uInt, Double> > scanSpwToIntervalMap;
	_getTimesAndInvervals(
		scanToTimeRangeMap,
		scanSpwToIntervalMap
	);
	return scanSpwToIntervalMap[scan];

}

std::map<String, std::set<Int> > MSMetaData::getIntentToFieldsMap() {
	vector<std::set<String> > fieldToIntentsMap;
	std::map<String, std::set<Int> > intentToFieldsMap;
	_getFieldsAndIntentsMaps(
		fieldToIntentsMap, intentToFieldsMap
	);
	return intentToFieldsMap;
}

std::map<String, std::set<ScanKey> > MSMetaData::getIntentToScansMap() {
	std::map<ScanKey, std::set<String> > scanToIntentsMap;
	std::map<String, std::set<ScanKey> > intentToScansMap;
	_getScansAndIntentsMaps(
		scanToIntentsMap,
		intentToScansMap
	);
	return intentToScansMap;
}

std::map<String, std::set<uInt> > MSMetaData::getIntentToSpwsMap() {
	vector<std::set<String> > spwToIntentsMap;
	std::map<String, std::set<uInt> > intentToSpwsMap;
	_getSpwsAndIntentsMaps(
		spwToIntentsMap,
		intentToSpwsMap
	);
	return intentToSpwsMap;
}

std::set<Int> MSMetaData::getScansForField(
	const String& field, Int obsID, Int arrayID
) const {
	std::set<Int> fieldIDs = getFieldIDsForField(field);
	std::set<Int> scans;
	for (
		std::set<Int>::const_iterator fieldID=fieldIDs.begin();
		fieldID!=fieldIDs.end(); ++fieldID
	) {
		std::set<Int> myscans = getScansForFieldID(*fieldID, obsID, arrayID);
		scans.insert(myscans.begin(), myscans.end());
	}
	return scans;
}

std::set<Int> MSMetaData::getScansForFieldID(
	const Int fieldID, Int obsID, Int arrayID
) const {
	if (! _hasFieldID(fieldID)) {
		return std::set<Int>();
	}
	vector<std::set<ScanKey> > fieldToScansMap;
	std::map<ScanKey, std::set<Int> > scanToFieldsMap;
	_getFieldsAndScansMaps(
		fieldToScansMap,  scanToFieldsMap
	);
	ArrayKey arrayKey;
	arrayKey.obsID = obsID;
	arrayKey.arrayID = arrayID;
	std::set<ScanKey> scanKeys = _getScanKeys(arrayKey);
	std::set<ScanKey>::const_iterator iter = scanKeys.begin();
	std::set<ScanKey>::const_iterator end = scanKeys.end();
	std::set<Int> scanNumbers;
	std::set<Int> fields;
	while (iter != end) {
		fields = scanToFieldsMap[*iter];
		if (fields.find(fieldID) != fields.end()) {
			scanNumbers.insert(iter->scan);
		}
		++iter;
	}
	return scanNumbers;
}

std::set<Int> MSMetaData::getFieldsForIntent(const String& intent) {
	if (! _hasIntent(intent)) {
		return std::set<Int>();
	}
	vector<std::set<String> > fieldToIntentsMap;
	std::map<String, std::set<Int> > intentToFieldsMap;
	_getFieldsAndIntentsMaps(
		fieldToIntentsMap, intentToFieldsMap
	);
	return intentToFieldsMap[intent];
}

std::set<Int> MSMetaData::getFieldsForScan(const ScanKey& scan) const {
	_checkScan(scan);
	vector<std::set<ScanKey> > fieldToScansMap;
	std::map<ScanKey, std::set<Int> > scanToFieldsMap;
	_getFieldsAndScansMaps(
		fieldToScansMap,  scanToFieldsMap
	);
	return scanToFieldsMap[scan];
}

std::set<Int> MSMetaData::getFieldsForScans(
	const std::set<Int>& scans, Int obsID, Int arrayID
) const {
	ArrayKey arrayKey;
	arrayKey.obsID = obsID;
	arrayKey.arrayID = arrayID;
	std::set<ScanKey> myScanKeys = scanKeys(scans, arrayKey);
	_checkScans(myScanKeys);
	std::set<ScanKey>::const_iterator iter = myScanKeys.begin();
	std::set<ScanKey>::const_iterator end = myScanKeys.end();
	std::set<Int> fields;
	while (iter != end) {
		std::set<Int> myfields = getFieldsForScan(*iter);
		fields.insert(myfields.begin(), myfields.end());
		++iter;

	}
	return fields;
}

std::set<Int> MSMetaData::getFieldIDsForField(
	const String& field
) const {
	std::set<Int> fieldIDs;
	String name = field;
	vector<String> fieldNames = _getFieldNames();
	uInt nNames = fieldNames.size();
	name.upcase();
	for (uInt i=0; i<nNames; ++i) {
		String testName = fieldNames[i];
		testName.upcase();
		if (name == testName) {
			fieldIDs.insert(i);
		}
	}
	ThrowIf(
		fieldIDs.empty(),
		"Unknown field name " + field
	);
	return fieldIDs;
}

std::set<Int> MSMetaData::getScansForIntent(
	const String& intent, Int obsID, Int arrayID
) const {
	std::set<String> uniqueIntents = getIntents();
	ThrowIf(
		uniqueIntents.find(intent) == uniqueIntents.end(),
		"Intent " + intent + " is not present in this dataset"
	);
	std::map<ScanKey, std::set<String> > scanToIntentsMap;
	std::map<String, std::set<ScanKey> > intentToScansMap;
	_getScansAndIntentsMaps(
		scanToIntentsMap,
		intentToScansMap
	);
	ArrayKey arrayKey;
	arrayKey.obsID = obsID;
	arrayKey.arrayID = arrayID;
	std::set<ScanKey> scanKeys = _getScanKeys(arrayKey);
	std::set<ScanKey>::const_iterator iter = scanKeys.begin();
	std::set<ScanKey>::const_iterator end = scanKeys.end();
	std::set<ScanKey> foundScans = intentToScansMap[intent];
	std::set<ScanKey>::const_iterator foundEnd = foundScans.end();
	std::set<ScanKey> filteredScans;
	while (iter != end) {
		if (foundScans.find(*iter) != foundEnd) {
			filteredScans.insert(*iter);
		}
		++iter;
	}
	return _getScanNumbers(filteredScans);
}

std::set<Int> MSMetaData::getStatesForScan(
	uInt obsID, uInt arrayID, Int scan
) const {
	ScanKey scanKey;
	scanKey.obsID = obsID;
	scanKey.arrayID = arrayID;
	scanKey.scan = scan;
	_checkScan(scanKey);
	return _getScanToStatesMap().find(scanKey)->second;
}

Record MSMetaData::getSummary() const {
	Record spectralTable;
	spectralTable.define("names", Vector<String>(getSpwNames()));
	Record polTable;
	polTable.define("n correlations", Vector<Int>(_getNumCorrs()));
	Record dataDescTable;
	vector<uInt> ddToSpw = _getDataDescIDToSpwMap();
	vector<uInt> ddToPolID = getDataDescIDToPolIDMap();
	vector<uInt>::const_iterator siter = ddToSpw.begin();
	vector<uInt>::const_iterator send = ddToSpw.end();
	vector<uInt>::const_iterator piter = ddToPolID.begin();
	vector<Int> spws(ddToSpw.size());
	vector<Int> polids(ddToPolID.size());
	uInt ddid = 0;
	while (siter != send) {
		spws[ddid] = *siter;
		polids[ddid] = *piter;
		++siter;
		++piter;
		++ddid;
	}
	dataDescTable.define("spectral windows", Vector<Int>(spws));
	dataDescTable.define("polarization ids", Vector<Int>(polids));
	Record summary;
	summary.defineRecord("spectral windows", spectralTable);
	summary.defineRecord("polarizations", polTable);
	summary.defineRecord("data descriptions", dataDescTable);
	summary.define("fields", Vector<String>(_getFieldNames()));
	vector<std::set<Int> > obsToArraysMap = _getObservationIDToArrayIDsMap();
	vector<std::set<Int> >::const_iterator oIter = obsToArraysMap.begin();
	vector<std::set<Int> >::const_iterator oEnd = obsToArraysMap.end();
	std::map<SubScanKey, SubScanProperties> subScanProps = _getSubScanProperties();
	uInt oCount = 0;
	while (oIter != oEnd) {
		std::set<Int>::const_iterator aIter = oIter->begin();
		std::set<Int>::const_iterator aEnd = oIter->end();
		Record obsRec;
		while (aIter != aEnd) {
			Record arrayRec;
			ArrayKey aKey;
			aKey.obsID = oCount;
			aKey.arrayID = *aIter;
			_createScanRecords(
				arrayRec, aKey, subScanProps
			);
			obsRec.defineRecord("arrayID=" + String::toString(*aIter), arrayRec);
			++aIter;
		}
		summary.defineRecord("observationID=" + String::toString(oCount), obsRec);
		++oIter;
		++oCount;
	}
	summary.define("nrows", nRows());
	CountedPtr<Vector<Double> > times = _getTimes();
	summary.define("begin time", min(*times));
	summary.define("end time", max(*times));
	return summary;
}

void MSMetaData::_createScanRecords(
	Record& parent, const ArrayKey& arrayKey,
	const std::map<SubScanKey, SubScanProperties>& subScanProps
) const {
	std::set<ScanKey> scanKeys = _getScanKeys(arrayKey);
	std::set<ScanKey>::const_iterator scanIter = scanKeys.begin();
	std::set<ScanKey>::const_iterator scanEnd = scanKeys.end();
	while(scanIter != scanEnd) {
		ScanKey scanKey = *scanIter;
		std::set<Int> antennasForScan;
		uInt scanNRows = 0;
		Record scanRec;
		_createSubScanRecords(
			scanRec, scanNRows, antennasForScan,
			scanKey, subScanProps
		);
		scanRec.define("nrows", scanNRows);
		scanRec.define("antennas", Vector<Int>(antennasForScan.begin(), antennasForScan.size(), 0));
		parent.defineRecord("scan=" + String::toString(scanKey.scan), scanRec);
		++scanIter;
	}
}

void MSMetaData::_createSubScanRecords(
	Record& parent, uInt& scanNRows, std::set<Int>& antennasForScan,
	const ScanKey& scanKey, const std::map<SubScanKey, SubScanProperties>& subScanProps
) const {
	std::set<SubScanKey> subScans = _getSubScanKeys(scanKey);
	std::set<SubScanKey>::const_iterator subScanIter = subScans.begin();
	std::set<SubScanKey>::const_iterator subScanEnd = subScans.end();
	uInt subScanCount = 0;
	while (subScanIter != subScanEnd) {
		Record subScanRec;
		SubScanProperties props = subScanProps.find(*subScanIter)->second;
		subScanRec.define("data description IDs", Vector<Int>(props.ddIDs.begin(), props.ddIDs.size(), 0));
		subScanRec.define("nrows", props.nrows);
		scanNRows += props.nrows;
		subScanRec.define("antennas", Vector<Int>(props.antennas.begin(), props.antennas.size(), 0));
		antennasForScan.insert(props.antennas.begin(), props.antennas.end());
		subScanRec.define("begin time", props.beginTime);
		subScanRec.define("end time", props.endTime);
		subScanRec.define("state IDs", Vector<Int>(props.stateIDs.begin(), props.stateIDs.size(), 0));
		//subScanRec.define("field ID", subScanIter->fieldID);
		_createTimeStampRecords(subScanRec, props);
		parent.defineRecord("fieldID=" + String::toString(subScanIter->fieldID), subScanRec);
		++subScanCount;
		++subScanIter;
	}
}

void MSMetaData::_createTimeStampRecords(
	Record& parent, const SubScanProperties& subScanProps
) {
	std::map<Double, TimeStampProperties>::const_iterator tpIter = subScanProps.timeProps.begin();
	std::map<Double, TimeStampProperties>::const_iterator tpEnd = subScanProps.timeProps.end();
	uInt timeCount = 0;
	while (tpIter != tpEnd) {
		Record timeRec;
		timeRec.define(
			"data description IDs",
			Vector<Int>(tpIter->second.ddIDs.begin(), tpIter->second.ddIDs.size(), 0)
		);
		timeRec.define("nrows", tpIter->second.nrows);
		timeRec.define("time", tpIter->first);
		parent.defineRecord(String::toString(timeCount), timeRec);
		++tpIter;
		++timeCount;
	}
}

std::set<Double> MSMetaData::getTimesForIntent(const String& intent) const {
	if (! _hasIntent(intent)) {
		return std::set<Double>();
	}
	std::map<String, std::set<Double> > mymap = _getIntentsToTimesMap();
	if (mymap.find(intent) == mymap.end()) {
		return std::set<Double>();
	}
	else {
		return mymap[intent];
	}
}

Bool MSMetaData::hasBBCNo() const {
	return _ms->spectralWindow().isColumn(MSSpectralWindowEnums::BBC_NO);
}

std::map<String, std::set<Double> > MSMetaData::_getIntentsToTimesMap() const {
	if (! _intentToTimesMap.empty()) {
		return _intentToTimesMap;
	}
	vector<std::set<String> > stateToIntentsMap;
	std::set<String> uniqueIntents;
	_getStateToIntentsMap(
		stateToIntentsMap, uniqueIntents
	);
	std::map<String, std::set<Double> > mymap;
	if (uniqueIntents.empty()) {
		return mymap;
	}
	CountedPtr<Vector<Int> > stateIDs = _getStateIDs();
	CountedPtr<Vector<Double> > times = _getTimes();
	Vector<Int>::const_iterator state = stateIDs->begin();
	Vector<Double>::const_iterator time = times->begin();
	Vector<Int>::const_iterator end = stateIDs->end();
	vector<std::set<Double> > stateToTimes(nStates());
	while(state != end) {
		stateToTimes[*state].insert(*time);
		++state;
		++time;
	}
	vector<std::set<String> >::const_iterator intents = stateToIntentsMap.begin();
	vector<std::set<String> >::const_iterator endState = stateToIntentsMap.end();
	uInt count = 0;
	while (intents != endState) {
		std::set<String>::const_iterator intent = intents->begin();
		std::set<String>::const_iterator eintent = intents->end();
		while (intent != eintent) {
			if (mymap.find(*intent) == mymap.end()) {
				mymap[*intent] = std::set<Double>();
			}
			std::set<Double> times = stateToTimes[count];
			mymap[*intent].insert(times.begin(), times.end());
			++intent;
		}
		++count;
		++intents;
	}
	if (_cacheUpdated(_sizeof(mymap))) {
		_intentToTimesMap = mymap;
	}
	return mymap;
}

void MSMetaData::_getFieldsAndScansMaps(
	vector<std::set<ScanKey> >& fieldToScansMap,
	std::map<ScanKey, std::set<Int> >& scanToFieldsMap
) const {
	// This method is responsible for setting _fieldToScansMap and _scanToFieldsMap
	if (! _fieldToScansMap.empty() && ! _scanToFieldsMap.empty()) {
		fieldToScansMap = _fieldToScansMap;
		scanToFieldsMap = _scanToFieldsMap;
		return;
	}
	scanToFieldsMap.clear();
	fieldToScansMap = vector<std::set<ScanKey> >(nFields());
	std::map<ScanKey, std::set<SubScanKey> > scanToSubScans = _getScanToSubScansMap();
	std::map<ScanKey, std::set<SubScanKey> >::const_iterator iter = scanToSubScans.begin();
	std::map<ScanKey, std::set<SubScanKey> >::const_iterator end = scanToSubScans.end();
	ScanKey scanKey;
	std::set<SubScanKey> subScanKeys;
	std::set<SubScanKey>::const_iterator subIter, subEnd;
	while (iter != end) {
		scanKey = iter->first;
		subScanKeys = iter->second;
		subIter = subScanKeys.begin();
		subEnd = subScanKeys.end();
		while (subIter != subEnd) {
			uInt fieldID = subIter->fieldID;
			scanToFieldsMap[scanKey].insert(fieldID);
			fieldToScansMap[fieldID].insert(scanKey);
			++subIter;
		}
		++iter;
	}
	if (_cacheUpdated(_sizeof(fieldToScansMap) + _sizeof(scanToFieldsMap))) {
		_fieldToScansMap = fieldToScansMap;
		_scanToFieldsMap = scanToFieldsMap;
	}
}

vector<Int> MSMetaData::_getNumCorrs() const {
	// responsible for setting _numCorrs
	if (! _numCorrs.empty()) {
		return _numCorrs;
	}
	String colName = MSPolarization::columnName(MSPolarization::NUM_CORR);
	ROScalarColumn<Int> col(_ms->polarization(), colName);
	vector<Int> myvec = col.getColumn().tovector();
	if (_cacheUpdated(sizeof(myvec))) {
		_numCorrs = myvec;
	}
	return myvec;
}

vector<std::set<Int> > MSMetaData::_getObservationIDToArrayIDsMap() const {
	// this method is responsible for setting _obsToArraysMap
	if (! _obsToArraysMap.empty()) {
		return _obsToArraysMap;
	}
	CountedPtr<Vector<Int> > obsIDs = _getObservationIDs();
	CountedPtr<Vector<Int> > arrayIDs = _getArrayIDs();
	Vector<Int>::const_iterator oIter = obsIDs->begin();
	Vector<Int>::const_iterator oEnd = obsIDs->end();
	Vector<Int>::const_iterator aIter = arrayIDs->begin();
	vector<std::set<Int> > mymap(nObservations());
	while (oIter != oEnd) {
		mymap[*oIter].insert(*aIter);
		++oIter;
		++aIter;
	}
	if (_cacheUpdated(_sizeof(mymap))) {
		_obsToArraysMap = mymap;
	}
	return mymap;
}

Bool MSMetaData::_hasIntent(const String& intent) const {
	std::set<String> uniqueIntents = getIntents();
	return uniqueIntents.find(intent) != uniqueIntents.end();
}

vector<String> MSMetaData::getFieldNamesForFieldIDs(
	const vector<uInt>& fieldIDs
) {
	if (fieldIDs.size() == 0) {
		return _getFieldNames();
	}
	// Do not use _checkFieldIDs since fieldIDs that may not be in the
	// main table can be valid. CAS-5168
	uInt max = *max_element(fieldIDs.begin(), fieldIDs.end());
	uInt nField = nFields();
	if (max >= nField) {
		ostringstream os;
		os << "MSMetaData::" << __FUNCTION__ << ": This MS only has "
			<< nField << " fields so requested field number " << max
			<< " does not exist";
		throw AipsError(os.str());
	}
	vector<String> allNames = _getFieldNames();
	vector<String> names;
	vector<uInt>::const_iterator end = fieldIDs.end();
	for (
		vector<uInt>::const_iterator iter=fieldIDs.begin();
		iter!=end; ++iter
	) {
		names.push_back(allNames[*iter]);
	}
	return names;
}

std::set<Int> MSMetaData::getFieldsForTimes(
	const Double center, const Double tol
) {
	_checkTolerance(tol);
	Double minTime = center - tol;
	Double maxTime = center + tol;
	CountedPtr<std::map<Int, std::set<Double> > > fieldToTimesMap;
	CountedPtr<std::map<Double, std::set<Int> > > timeToFieldsMap;
	_getFieldsAndTimesMaps(
		fieldToTimesMap, timeToFieldsMap
	);
	std::set<Int> fields;
	std::map<Double, std::set<Int> >::const_iterator end = timeToFieldsMap->end();
	// A std::set is always ordered.
	// FIXME could do a binary search to make this faster
	for (
		std::map<Double, std::set<Int> >::const_iterator iter=timeToFieldsMap->begin();
		iter!=end; ++iter
	) {
		Double curTime = iter->first;
		if (curTime >= minTime) {
			std::set<Int> curFields = iter->second;
			fields.insert(curFields.begin(), curFields.end());
		}
		if (curTime > maxTime) {
			break;
		}
	}
	return fields;
}

void MSMetaData::_checkTolerance(const Double tol) {
	ThrowIf(
		tol < 0,
		"Tolerance cannot be less than zero"
	);
}

void MSMetaData::_getFieldsAndTimesMaps(
		CountedPtr<std::map<Int, std::set<Double> > >& fieldToTimesMap,
		CountedPtr<std::map<Double, std::set<Int> > >& timeToFieldsMap
) {
	// This method is responsible for setting _fieldToTimesMap and _timeToFieldMap
	if (
		_fieldToTimesMap && ! _fieldToTimesMap->empty()
		&& _timeToFieldsMap && ! _timeToFieldsMap->empty()
	) {
		fieldToTimesMap = _fieldToTimesMap;
		timeToFieldsMap = _timeToFieldsMap;
		return;
	}
	fieldToTimesMap.reset(new std::map<Int, std::set<Double> >());
	timeToFieldsMap.reset(new std::map<Double, std::set<Int> >());
	CountedPtr<Vector<Int> > allFields = _getFieldIDs();
	CountedPtr<Vector<Double> > allTimes = this->_getTimes();
	Vector<Int>::const_iterator lastField = allFields->end();
	Vector<Double>::const_iterator curTime = allTimes->begin();
	for (
		Vector<Int>::const_iterator curField=allFields->begin();
		curField!=lastField; ++curField, ++curTime
	) {
		(*fieldToTimesMap)[*curField].insert(*curTime);
		(*timeToFieldsMap)[*curTime].insert(*curField);
	}
	if (
		_cacheUpdated(_sizeof(*fieldToTimesMap) + _sizeof(*timeToFieldsMap))
	) {
		_fieldToTimesMap = fieldToTimesMap;
		_timeToFieldsMap = timeToFieldsMap;
	}
}

std::set<Double> MSMetaData::getTimesForField(const Int fieldID) {
	if (! _hasFieldID(fieldID)) {
		return std::set<Double>();
	}
	CountedPtr<std::map<Int, std::set<Double> > > fieldToTimesMap;
	CountedPtr<std::map<Double, std::set<Int> > > timeToFieldsMap;
	_getFieldsAndTimesMaps(
		fieldToTimesMap, timeToFieldsMap
	);
	return (*fieldToTimesMap)[fieldID];
}

vector<String> MSMetaData::getObservatoryNames() {
	if (! _observatoryNames.empty()) {
		return _observatoryNames;
	}
	String tnameColName = MSObservation::columnName(MSObservationEnums::TELESCOPE_NAME);
	ROScalarColumn<String> telescopeNameCol(_ms->observation(), tnameColName);
	vector<String> names = telescopeNameCol.getColumn().tovector();
	uInt mysize = 0;
	vector<String>::const_iterator end = names.end();
	for (
		vector<String>::const_iterator iter=names.begin();
		iter!=end; ++iter
	) {
		mysize += iter->size();
	}
	if (_cacheUpdated(mysize)) {
		_observatoryNames = names;
	}
	return names;
}

MPosition MSMetaData::getObservatoryPosition(uInt which) const {
	if (which >= _ms->observation().nrow()) {
		throw AipsError(_ORIGIN + " out of range exception.");
	}
	if (! _observatoryPositions.empty()) {
		return _observatoryPositions[which];
	}
	String tnameColName = MSObservation::columnName(MSObservationEnums::TELESCOPE_NAME);
	ROScalarColumn<String> telescopeNameCol(_ms->observation(), tnameColName);
	vector<String> names = telescopeNameCol.getColumn().tovector();
	vector<MPosition> observatoryPositions(names.size());
	for (uInt i=0; i<observatoryPositions.size(); ++i) {
		ThrowIf(
			names[i].empty(),
			"The name of the telescope is not stored in the measurement set."
		);
		ThrowIf(
			! MeasTable::Observatory(observatoryPositions[i], names[i]),
			"Telescope " + names[i] + " is not recognized by CASA"
		);
	}
	if (_cacheUpdated(30*observatoryPositions.size())) {
		_observatoryPositions = observatoryPositions;
	}
	return observatoryPositions[which];
}

vector<MPosition> MSMetaData::_getAntennaPositions() const {
	// This method is responsible for setting _antennaPositions
	if (! _antennaPositions.empty()) {
		return _antennaPositions;
	}
	String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
	ROScalarColumn<String> nameCol(_ms->antenna(), antNameColName);
	String antPosColName = MSAntenna::columnName(MSAntennaEnums::POSITION);
	ArrayColumn<Double> posCol(_ms->antenna(), antPosColName);
	Array<Double> xyz = posCol.getColumn();
	Vector<String> posUnits = posCol.keywordSet().asArrayString("QuantumUnits");
	String sFrame = posCol.keywordSet().asRecord("MEASINFO").asString("Ref");
	MPosition::Types posType = MPosition::getType(sFrame);
	Array<Double>::const_iterator end = xyz.end();
	Quantity x(0, posUnits[0]);
	Quantity y(0, posUnits[1]);
	Quantity z(0, posUnits[2]);
    vector<MPosition> antennaPositions;
	for (Array<Double>::const_iterator iter=xyz.begin(); iter!=end; ++iter) {
		x.setValue(*iter);
		Double xm = x.getValue("m");
		++iter;
		y.setValue(*iter);
		Double ym = y.getValue("m");
		++iter;
		z.setValue(*iter);
		Double zm = z.getValue("m");
		MPosition antPos(MVPosition(xm, ym, zm), posType);
		antennaPositions.push_back(antPos);
	}
	if(_cacheUpdated(30*antennaPositions.size())) {
		_antennaPositions = antennaPositions;
	}
	return antennaPositions;
}

vector<MPosition> MSMetaData::getAntennaPositions (
	const vector<uInt>& which
) const {
	vector<MPosition> allPos = _getAntennaPositions();
	if (which.empty()) {
		return allPos;
	}
	ThrowIf(
		max(Vector<uInt>(which)) >= nAntennas(),
		"Antenna ID out of range"
	);
	vector<MPosition> output;
	vector<uInt>::const_iterator end = which.end();
	for (
		vector<uInt>::const_iterator iter=which.begin();
		iter!=end; ++iter
	) {
		output.push_back(allPos[*iter]);
	}
	return output;
}

vector<MPosition> MSMetaData::getAntennaPositions(
	const vector<String>& names
) {
	if (names.size() == 0) {
		throw AipsError(_ORIGIN + "names cannot be empty");
	}
	return getAntennaPositions(getAntennaIDs(names));
}

Quantum<Vector<Double> > MSMetaData::getAntennaOffset(uInt which) {
	if (which >= nAntennas()) {
		throw AipsError(_ORIGIN + "Out of range exception.");
	}
	return getAntennaOffsets()[which];
}

vector<Quantum<Vector<Double> > > MSMetaData::getAntennaOffsets() const {
	// This method is responsble for setting _antennaOffsets
	if (! _antennaOffsets.empty()) {
		return _antennaOffsets;
	}
	MPosition obsPos = getObservatoryPosition(0);
	if (obsPos.type() != MPosition::ITRF) {
		MeasConvert<MPosition> toItrf(obsPos, MPosition::ITRF);
		obsPos = toItrf(obsPos);
	}
	Vector<Double> obsXYZ = obsPos.get("m").getValue();
	Double xo = obsXYZ[0];
	Double yo = obsXYZ[1];
	Double zo = obsXYZ[2];
	Double rObs = sqrt(xo*xo + yo*yo + zo*zo);
	Vector<Double> obsLongLat = obsPos.getAngle("rad").getValue();
	Double longObs = obsLongLat[0];
	Double latObs = obsLongLat[1];
	vector<MPosition> antennaPositions = _getAntennaPositions();
	vector<MPosition>::const_iterator end = antennaPositions.end();
	vector<Quantum<Vector<Double> > > antennaOffsets;
	for (
		vector<MPosition>::const_iterator iter=antennaPositions.begin();
		iter!=end; ++iter
	) {
		Vector<Double> xyz = iter->get("m").getValue();
		Double x = xyz[0];
		Double y = xyz[1];
		Double z = xyz[2];
		Double rAnt = sqrt(x*x + y*y + z*z);
		Vector<Double> antLongLat = iter->getAngle("rad").getValue();
		Double longAnt = antLongLat[0];
		Double latAnt = antLongLat[1];
		Vector<Double> offset(3);
		offset[0] = (longAnt - longObs)*rObs*cos(latObs);
		offset[1] = (latAnt - latObs)*rObs;
		offset[2] = rAnt - rObs;
		Quantum<Vector<Double> > qoffset(offset, "m");
		antennaOffsets.push_back(qoffset);
	}
	if (_cacheUpdated(30*antennaOffsets.size())) {
		_antennaOffsets = antennaOffsets;
	}
	return antennaOffsets;
}

uInt MSMetaData::nBaselines() {
	Matrix<Bool> baselines = getUniqueBaselines();
	for (uInt i=0; i<baselines.nrow(); ++i) {
		// discard autocorrelation "baselines" for calculation
		baselines(i, i) = False;
	}
	return ntrue(baselines)/2;
}

Matrix<Bool> MSMetaData::getUniqueBaselines() {
	if (! _uniqueBaselines.empty()) {
		return _uniqueBaselines;
	}
	CountedPtr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);

	Vector<Int>::const_iterator a1Iter = ant1->begin();
	Vector<Int>::const_iterator a2Iter = ant2->begin();
	Vector<Int>::const_iterator end = ant1->end();
	uInt nAnts = nAntennas();
	Matrix<Bool> baselines(nAnts, nAnts, False);
	while (a1Iter != end) {
		baselines(*a1Iter, *a2Iter) = True;
		baselines(*a2Iter, *a1Iter) = True;
		++a1Iter;
		++a2Iter;
	}
	//Matrix<Bool> uBaselines = _getUniqueBaselines(*ant1, *ant2);
	if (_cacheUpdated(sizeof(Bool)*baselines.size())) {
		_uniqueBaselines = baselines;
	}
	return baselines;
}

Quantum<Vector<Double> > MSMetaData::getAntennaOffset(
	const String& name
) {
	vector<String> names(1);
	names[0] = name;
	return getAntennaOffset(getAntennaIDs(names)[0]);
}

Quantity MSMetaData::getEffectiveTotalExposureTime() {
	// This method has the responsibility of setting _exposureTime.
	if (_exposureTime.getValue() > 0) {
		return _exposureTime;
	}
	uInt nAnts = nAntennas();
	uInt maxNBaselines = nAnts*(nAnts-1)/2;
	Double totalExposure = 0;
	String taql = "select FLAG, DATA_DESC_ID, EXPOSURE, TIME from "
		+ _ms->tableName() + " where ANTENNA1 != ANTENNA2";
	Table result(tableCommand(taql));
	Vector<Int> ddIDs = ScalarColumn<Int>(result, "DATA_DESC_ID").getColumn();
	Vector<Double> exposures = ScalarColumn<Double>(result, "EXPOSURE").getColumn();
	Vector<Double> times = ScalarColumn<Double>(result, "TIME").getColumn();
	// each row represents a unique baseline, data description ID, and time combination
	uInt nrows = result.nrow();
	vector<uInt> dataDescToSpwIdMap = _getDataDescIDToSpwMap();
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	std::map<Double, Double> timeToBWMap = _getTimeToTotalBWMap(
		times, ddIDs
	);
	for (uInt i=0; i<nrows; ++i) {
		uInt ddID = ddIDs[i];
		uInt spw = dataDescToSpwIdMap[ddID];
		Quantum<Vector<Double> > channelWidths = spwInfo[spw].chanwidths;
		Matrix<Bool> flagsMatrix(ArrayColumn<Bool>(result, "FLAG").get(i));
		uInt nCorrelations = flagsMatrix.nrow();
		Double denom = (timeToBWMap.find(times[i])->second)*maxNBaselines*nCorrelations;
		for (uInt corr=0; corr<nCorrelations; ++corr) {
			Vector<Bool> goodData = ! flagsMatrix.row(corr);
			if (anyTrue(goodData)) {
				MaskedArray<Double> flaggedChannelWidths(
					channelWidths.getValue("Hz"), goodData, True
				);
				Double effectiveBW = sum(flaggedChannelWidths);
				totalExposure += exposures[i]*effectiveBW/denom;
			}
		}
	}
	String unit = ScalarColumn<Double>(*_ms, "EXPOSURE").keywordSet().asArrayString("QuantumUnits").tovector()[0];
	Quantity eTime(totalExposure, unit);
	if (_cacheUpdated(10)) {
		_exposureTime = eTime;
	}
	return eTime;
}

std::map<SubScanKey, MSMetaData::SubScanProperties> MSMetaData::_getSubScanProperties(
) const {
	// responsible for setting _subScanProperties, _scanProperties,
	// and _arrayProperties
	// a sub scan is defined by a unique combination of scan number and field ID
	if (! _subScanProperties.empty()) {
		return _subScanProperties;
	}
	CountedPtr<Vector<Int> > scans = _getScans();
	CountedPtr<Vector<Int> > fields = _getFieldIDs();
	CountedPtr<Vector<Int> > ddIDs = _getDataDescIDs();
	CountedPtr<Vector<Int> > states = _getStateIDs();
	CountedPtr<Vector<Double> > times = _getTimes();
	CountedPtr<Vector<Int> > arrays = _getArrayIDs();
	CountedPtr<Vector<Int> > observations = _getObservationIDs();
	CountedPtr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);
	Vector<Int>::const_iterator scanIter = scans->begin();
	Vector<Int>::const_iterator scanEnd = scans->end();
	Vector<Int>::const_iterator a1Iter = ant1->begin();
	Vector<Int>::const_iterator a2Iter = ant2->begin();
	Vector<Int>::const_iterator fIter = fields->begin();
	Vector<Int>::const_iterator dIter = ddIDs->begin();
	Vector<Int>::const_iterator stateIter = states->begin();
	Vector<Int>::const_iterator oIter = observations->begin();
	Vector<Int>::const_iterator arIter = arrays->begin();
	Vector<Double>::const_iterator tIter = times->begin();
    std::map<SubScanKey, SubScanProperties> mysubscans;
    SubScanKey subScanKey;
	while (scanIter != scanEnd) {
        subScanKey.obsID = *oIter;
        subScanKey.arrayID = *arIter;
        subScanKey.scan = *scanIter;
        subScanKey.fieldID = *fIter;
        if (
            mysubscans.find(subScanKey) == mysubscans.end()
        ) {
			SubScanProperties props;
			props.beginTime = *tIter;
			props.endTime = *tIter;
			props.nrows = 1;
            mysubscans[subScanKey] = props;
		}
		else {
            mysubscans[subScanKey].beginTime = min(*tIter, mysubscans[subScanKey].beginTime);
            mysubscans[subScanKey].endTime = max(*tIter, mysubscans[subScanKey].endTime);
            ++mysubscans[subScanKey].nrows;
		}
        mysubscans[subScanKey].antennas.insert(*a1Iter);
        mysubscans[subScanKey].antennas.insert(*a2Iter);
        mysubscans[subScanKey].ddIDs.insert(*dIter);
        mysubscans[subScanKey].stateIDs.insert(*stateIter);
        if (mysubscans[subScanKey].timeProps.find(*tIter) == mysubscans[subScanKey].timeProps.end()) {
        	mysubscans[subScanKey].timeProps[*tIter].nrows = 1;
        }
        else {
        	++(mysubscans[subScanKey].timeProps[*tIter].nrows);
        }
        mysubscans[subScanKey].timeProps[*tIter].ddIDs.insert(*dIter);
		++tIter;
		++scanIter;
		++fIter;
		++dIter;
		++stateIter;
		++oIter;
		++arIter;
		++a1Iter;
		++a2Iter;
	}
	uInt structSize = 2*sizeof(Double) + sizeof(Int);
    uInt keySize = 4*sizeof(Int);
    std::map<SubScanKey, SubScanProperties>::const_iterator mIter = mysubscans.begin();
    std::map<SubScanKey, SubScanProperties>::const_iterator mEnd = mysubscans.end();
    uInt mapSize = mysubscans.size() * (structSize + keySize);
    while (mIter != mEnd) {
        mapSize += sizeof(Int)*(mIter->second.ddIDs.size() + mIter->second.stateIDs.size());
        mapSize += (sizeof(Double) + sizeof(Int)) + mIter->second.timeProps.size();
        ++mIter;
    }
    if (_cacheUpdated(mapSize)) {
    	_subScanProperties = mysubscans;
	}
	return mysubscans;
}

std::map<Double, Double> MSMetaData::_getTimeToTotalBWMap(
	const Vector<Double>& times, const Vector<Int>& ddIDs
) {
	std::map<Double, Double> timeToBWMap;
	std::map<Double,std::set<uInt> > timeToDDIDMap;
	Vector<Double>::const_iterator end = times.end();
	Vector<Double>::const_iterator tIter = times.begin();
	Vector<Int>::const_iterator dIter = ddIDs.begin();
	while (tIter!=end) {
		timeToDDIDMap[*tIter].insert(*dIter);
		++tIter;
		++dIter;
	}
	std::map<Double, std::set<uInt> >::const_iterator end1 = timeToDDIDMap.end();
	vector<uInt> dataDescIDToSpwMap = _getDataDescIDToSpwMap();
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	for (
		std::map<Double,std::set<uInt> >::const_iterator iter=timeToDDIDMap.begin();
		iter!=end1; ++iter
	) {
		std::set<uInt> ddIDs = iter->second;
		timeToBWMap[iter->first] = 0;
		std::set<uInt>::const_iterator end2 = ddIDs.end();
		for (
			std::set<uInt>::const_iterator dIter=ddIDs.begin();
			dIter!=end2; ++dIter
		) {
			uInt spw = dataDescIDToSpwMap[*dIter];
			timeToBWMap[iter->first] += spwInfo[spw].bandwidth;
		}
	}
	return timeToBWMap;
}

void MSMetaData::_getUnflaggedRowStats(
	Double& nACRows, Double& nXCRows,
	CountedPtr<std::map<SubScanKey, Double> >& subScanNACRows,
	CountedPtr<std::map<SubScanKey, Double> >& subScanNXCRows,
	CountedPtr<vector<Double> >& fieldNACRows,
	CountedPtr<vector<Double> >& fieldNXCRows
) const {
	// This method is responsible for setting _nUnflaggedACRows, _nUnflaggedXCRows,
	// _unflaggedFieldNACRows, _unflaggedFieldNXCRows, _unflaggedScanNACRows,
	// _unflaggedScanNXCRows
	if (_unflaggedFieldNACRows && ! _unflaggedFieldNACRows->empty()) {
		nACRows = _nUnflaggedACRows;
		nXCRows = _nUnflaggedXCRows;
		fieldNACRows = _unflaggedFieldNACRows;
		fieldNXCRows = _unflaggedFieldNXCRows;
		subScanNACRows = _unflaggedSubScanNACRows;
		subScanNXCRows = _unflaggedSubScanNXCRows;
		return;
	}
	std::map<SubScanKey, Double> *mySubScanNACRows, *mySubScanNXCRows;
	vector<Double> *myFieldNACRows, *myFieldNXCRows;
	_getUnflaggedRowStats(
		nACRows, nXCRows, myFieldNACRows,
		myFieldNXCRows, mySubScanNACRows, mySubScanNXCRows
	);

	fieldNACRows.reset(myFieldNACRows);
	fieldNXCRows.reset(myFieldNXCRows);
	subScanNACRows.reset(mySubScanNACRows);
	subScanNXCRows.reset(mySubScanNXCRows);
	uInt mysize = 2*(
		sizeof(Double) + _sizeof(*fieldNACRows)
		+ _sizeof(*subScanNACRows)
	);
	if (_cacheUpdated(mysize)) {
		_nUnflaggedACRows = nACRows;
		_nUnflaggedXCRows = nXCRows;
		_unflaggedFieldNACRows = fieldNACRows;
		_unflaggedFieldNXCRows = fieldNXCRows;
		_unflaggedSubScanNACRows = subScanNACRows;
		_unflaggedSubScanNXCRows = subScanNXCRows;
	}
}

void MSMetaData::_getUnflaggedRowStats(
	Double& nACRows, Double& nXCRows,
	vector<Double>*& fieldNACRows, vector<Double>*& fieldNXCRows,
	std::map<SubScanKey, Double> *& subScanNACRows,
	std::map<SubScanKey, Double> *& subScanNXCRows
) const {
	nACRows = 0;
	nXCRows = 0;
	uInt myNFields = nFields();
	fieldNACRows = new vector<Double>(myNFields, 0);
	fieldNXCRows = new vector<Double>(myNFields, 0);
	subScanNACRows = new std::map<SubScanKey, Double>();
	subScanNXCRows = new std::map<SubScanKey, Double>();
	std::set<SubScanKey> subScanKeys = _getSubScanKeys();
	std::set<SubScanKey>::const_iterator iter = subScanKeys.begin();
	std::set<SubScanKey>::const_iterator end = subScanKeys.end();
	while (iter != end) {
		(*subScanNACRows)[*iter] = 0;
		(*subScanNXCRows)[*iter] = 0;
		++iter;
	}
	CountedPtr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);
	CountedPtr<Vector<Int> > dataDescIDs = _getDataDescIDs();
	CountedPtr<Vector<Int> > scans = _getScans();
	CountedPtr<Vector<Int> > fieldIDs = _getFieldIDs();
	CountedPtr<Vector<Int> > obsIDs = _getObservationIDs();
	CountedPtr<Vector<Int> > arrIDs = _getArrayIDs();
	Vector<Int>::const_iterator aEnd = ant1->end();
	Vector<Int>::const_iterator a1Iter = ant1->begin();
	Vector<Int>::const_iterator a2Iter = ant2->begin();
	Vector<Int>::const_iterator sIter = scans->begin();
	Vector<Int>::const_iterator fIter = fieldIDs->begin();
	Vector<Int>::const_iterator oIter = obsIDs->begin();
	Vector<Int>::const_iterator arIter = arrIDs->begin();
	Vector<Int>::const_iterator dIter = dataDescIDs->begin();
	uInt i = 0;
    //uInt64 count = 0;
    // a flag value of True means the datum is bad (flagged), so False => unflagged
    vector<uInt> dataDescIDToSpwMap = _getDataDescIDToSpwMap();
	std::set<uInt> a, b, c, d, e;
	vector<SpwProperties> spwInfo = _getSpwInfo(a, b, c, d, e);
	CountedPtr<ArrayColumn<Bool> > flags = _getFlags();
	while (a1Iter != aEnd) {
		uInt spw = dataDescIDToSpwMap[*dIter];
		SpwProperties spwProp = spwInfo[spw];
		Vector<Double> channelWidths(
			Vector<Double>(spwProp.chanwidths.getValue("Hz"))
		);
		const Matrix<Bool>& flagsMatrix(flags->get(i));
		//count += flagsMatrix.size();
		Double x = 0;
		if (! anyTrue(flagsMatrix)) {
			// all channels are unflagged
			x = 1;
		}
		else if (allTrue(flagsMatrix)) {
			// do nothing. All channels are flagged for this row
			// do not put a continue though, because counters still must
			// incremented below
		}
		else {
			// some channels are flagged, some aren't
			uInt nCorrelations = flagsMatrix.nrow();
			Double denom = spwProp.bandwidth*nCorrelations;
			Double bwSum = 0;

			for (uInt corr=0; corr<nCorrelations; ++corr) {
				// invert the meaning here, so that a True value
				// in corrRow means the datum is good (unflagged)
				// it will make the masked sum below more obvious
				Vector<Bool> corrRow = ! flagsMatrix.row(corr);
				if (allTrue(corrRow)) {
					// all channels for this correlation are unflagged
					bwSum += spwProp.bandwidth;
				}
				else if (! anyTrue(corrRow)) {
					// do nothing, all channels for this correlation
					// have been flagged
					// but allow fall through to iterator increments
				}
				else {
					// some channels are flagged for this correlation, some aren't
					MaskedArray<Double> unFlaggedChannelWidths(
						channelWidths, corrRow, True
					);
					bwSum += sum(unFlaggedChannelWidths);
				}
			}
			x = bwSum/denom;
		}
		SubScanKey subScanKey;
		subScanKey.obsID = *oIter;
		subScanKey.arrayID = *arIter;
		subScanKey.scan = *sIter;
		subScanKey.fieldID = *fIter;
		if (*a1Iter == *a2Iter) {
			(*fieldNACRows)[*fIter] += x;
			(*subScanNACRows)[subScanKey] += x;
		}
		else {
			(*fieldNXCRows)[*fIter]+= x;
			(*subScanNXCRows)[subScanKey] += x;
		}
		++a1Iter;
		++a2Iter;
		++sIter;
		++fIter;
		++arIter;
		++oIter;
		++dIter;
		++i;
	}
	vector<Double>::const_iterator faIter = fieldNACRows->begin();
	vector<Double>::const_iterator faEnd = fieldNACRows->end();
	vector<Double>::const_iterator fxIter = fieldNXCRows->begin();
	while (faIter != faEnd) {
		nACRows += *faIter;
		nXCRows += *fxIter;
		++faIter;
		++fxIter;
	}
}

void MSMetaData::_getSpwsAndIntentsMaps(
	vector<std::set<String> >& spwToIntentsMap,
	std::map<String, std::set<uInt> >& intentToSpwsMap
) {
	if (! _spwToIntentsMap.empty() && ! _intentToSpwsMap.empty()) {
		spwToIntentsMap = _spwToIntentsMap;
		intentToSpwsMap = _intentToSpwsMap;
	}
	spwToIntentsMap.clear();
	intentToSpwsMap.clear();
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<SpwProperties> spwInfo = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	std::set<String> emptySet;
	vector<SpwProperties>::const_iterator end = spwInfo.end();
	for (
		vector<SpwProperties>::const_iterator iter=spwInfo.begin();
		iter!=end; ++iter
	) {
		spwToIntentsMap.push_back(emptySet);
	}
	vector<std::set<String> > stateToIntentsMap;
	std::set<String> uniqueIntents;
	_getStateToIntentsMap(stateToIntentsMap, uniqueIntents);
	if (uniqueIntents.size() == 0) {
		_spwToIntentsMap = spwToIntentsMap;
		_intentToSpwsMap = intentToSpwsMap;
		return;
	}
	CountedPtr<Vector<Int> > dataDescIDs = _getDataDescIDs();
	Vector<Int>::const_iterator curDDID = dataDescIDs->begin();
	Vector<Int>::const_iterator endDDID = dataDescIDs->end();
	CountedPtr<Vector<Int> > states = _getStateIDs();
	Vector<Int>::const_iterator curState = states->begin();
	vector<uInt> dataDescToSpwMap = _getDataDescIDToSpwMap();
	while (curDDID!=endDDID) {
		uInt spw = dataDescToSpwMap[*curDDID];
		std::set<String> intents = stateToIntentsMap[*curState];
		std::set<String>::const_iterator beginIntent = intents.begin();
		std::set<String>::const_iterator endIntent = intents.end();
		spwToIntentsMap[spw].insert(beginIntent, endIntent);
		std::set<String>::const_iterator curIntent = beginIntent;
		while (curIntent != endIntent) {
			intentToSpwsMap[*curIntent].insert(spw);
			++curIntent;
		}
		++curDDID;
		++curState;
	}
	if (_cacheUpdated(_sizeof(spwToIntentsMap) + _sizeof(intentToSpwsMap))) {
		_spwToIntentsMap = spwToIntentsMap;
		_intentToSpwsMap = intentToSpwsMap;
	}
}

vector<std::set<String> > MSMetaData::_getSpwToIntentsMap() {
	vector<std::set<String> > spwToIntentsMap;
	std::map<String, std::set<uInt> > intentToSpwsMap;
	_getSpwsAndIntentsMaps(
		spwToIntentsMap, intentToSpwsMap
	);
	return spwToIntentsMap;
}

void MSMetaData::_getFieldsAndStatesMaps(
	std::map<Int, std::set<Int> >& fieldToStatesMap,
	std::map<Int, std::set<Int> >& stateToFieldsMap
) {
	// This method is responsible for setting _fieldToStatesMap and _stateToFieldMap.
	if (! _fieldToStatesMap.empty() && ! _stateToFieldsMap.empty()) {
		fieldToStatesMap = _fieldToStatesMap;
		stateToFieldsMap = _stateToFieldsMap;
		return;
	}
	CountedPtr<Vector<Int> > allStates = _getStateIDs();
	CountedPtr<Vector<Int> > allFields = _getFieldIDs();
	Vector<Int>::const_iterator endState = allStates->end();
	Vector<Int>::const_iterator curField = allFields->begin();
	fieldToStatesMap.clear();
	stateToFieldsMap.clear();
	for (
		Vector<Int>::const_iterator curState=allStates->begin();
		curState!=endState; ++curState, ++curField
	) {
		fieldToStatesMap[*curField].insert(*curState);
		stateToFieldsMap[*curState].insert(*curField);
	}
	if (
		_cacheUpdated(
			_sizeof(fieldToStatesMap)
			+ _sizeof(stateToFieldsMap)
		)
	) {
		_fieldToStatesMap = fieldToStatesMap;
		_stateToFieldsMap = stateToFieldsMap;
	}
}

map<Int, std::set<String> > MSMetaData::getFieldNamesForSourceMap() const {
	map<Int, std::set<Int> > idsToSource = getFieldsForSourceMap();
	map<Int, std::set<Int> >::const_iterator iter = idsToSource.begin();
	map<Int, std::set<Int> >::const_iterator end = idsToSource.end();
	map<Int, std::set<String> > namesMap;
	vector<String> names = _getFieldNames();
	while (iter != end) {
		Int sourceID = iter->first;
		namesMap[sourceID] = std::set<String>();
		std::set<Int> fieldIDs = idsToSource[sourceID];
		std::set<Int>::const_iterator siter = fieldIDs.begin();
		std::set<Int>::const_iterator send = fieldIDs.end();
		while (siter != send) {
			namesMap[sourceID].insert(names[*siter]);
			++siter;
		}
		++iter;
	}
	return namesMap;
}

map<Int, std::set<Int> > MSMetaData::getFieldsForSourceMap() const {
	// This method sets _sourceToFieldsMap
	if (! _sourceToFieldsMap.empty()) {
		return _sourceToFieldsMap;
	}
	String sourceIDName = MSField::columnName(MSFieldEnums::SOURCE_ID);
	Vector<Int> sourceIDs = ROScalarColumn<Int>(_ms->field(), sourceIDName).getColumn();
	map<Int, std::set<Int> > mymap;
	std::set<Int> uSourceIDs(sourceIDs.begin(), sourceIDs.end());
	std::set<Int>::const_iterator iter = uSourceIDs.begin();
	std::set<Int>::const_iterator  end = uSourceIDs.end();
	while (iter != end) {
		mymap[*iter] = std::set<Int>();
		++iter;
	}
	Vector<Int>::const_iterator miter = sourceIDs.begin();
	Vector<Int>::const_iterator mend = sourceIDs.end();
	Int rowNumber = 0;
	while (miter != mend) {
		mymap[*miter].insert(rowNumber);
		++miter;
		++rowNumber;
	}
	uInt mysize = _sizeof(mymap);
	if (_cacheUpdated(mysize)) {
		_sourceToFieldsMap = mymap;
	}
	return mymap;
}


void MSMetaData::_getFieldsAndIntentsMaps(
	vector<std::set<String> >& fieldToIntentsMap,
	std::map<String, std::set<Int> >& intentToFieldsMap
) {
	// This method is responsible for setting _intentToFieldIDMap and _fieldToIntentsMap
	if (! _intentToFieldIDMap.empty() && ! _fieldToIntentsMap.empty()) {
		fieldToIntentsMap = _fieldToIntentsMap;
		intentToFieldsMap = _intentToFieldIDMap;
		return;
	}
	fieldToIntentsMap.resize(nFields());
	vector<std::set<String> > stateToIntentsMap;
	std::set<String> uniqueIntents;
	_getStateToIntentsMap(
		stateToIntentsMap,
		uniqueIntents
	);
	std::map<Int, std::set<Int> > fieldToStatesMap;
	std::map<Int, std::set<Int> > stateToFieldsMap;
	_getFieldsAndStatesMaps(
		fieldToStatesMap, stateToFieldsMap
	);
	std::map<Int, std::set<Int> >::const_iterator end = stateToFieldsMap.end();
	for (
		std::map<Int, std::set<Int> >::const_iterator iter=stateToFieldsMap.begin();
		iter!=end; ++iter
	) {
		Int state = iter->first;
		std::set<Int> fields = iter->second;
		std::set<String> intents = stateToIntentsMap[state];
		std::set<Int>::const_iterator endField = fields.end();
		for (
			std::set<Int>::const_iterator curField=fields.begin();
			curField!=endField; ++curField
		) {
			fieldToIntentsMap[*curField].insert(intents.begin(), intents.end());
		}
		std::set<String>::const_iterator endIntent = intents.end();
		for (
			std::set<String>::const_iterator curIntent=intents.begin();
			curIntent!=endIntent; ++curIntent
		) {
			intentToFieldsMap[*curIntent].insert(fields.begin(), fields.end());
		}
	}
	if (
		_cacheUpdated(
			_sizeof(fieldToIntentsMap) + _sizeof(intentToFieldsMap)
		)
	) {
		_fieldToIntentsMap = fieldToIntentsMap;
		_intentToFieldIDMap = intentToFieldsMap;
	}
}

std::map<std::pair<uInt, uInt>, uInt> MSMetaData::getSpwIDPolIDToDataDescIDMap() const {
	if (! _spwPolIDToDataDescIDMap.empty()) {
		return _spwPolIDToDataDescIDMap;
	}
	vector<uInt> dataDescIDToSpwMap = _getDataDescIDToSpwMap();
	vector<uInt>::const_iterator i1 = dataDescIDToSpwMap.begin();
	vector<uInt>::const_iterator end = dataDescIDToSpwMap.end();
	std::map<std::pair<uInt, uInt>, uInt> spwPolIDToDataDescIDMap;
	vector<uInt> dataDescIDToPolIDMap = getDataDescIDToPolIDMap();
	uInt dataDesc = 0;
	while (i1 != end) {
		uInt spw = *i1;
		uInt polID = dataDescIDToPolIDMap[dataDesc];
		spwPolIDToDataDescIDMap[std::make_pair(spw, polID)] = dataDesc;
		++i1;
		++dataDesc;
	}
	uInt mysize = 2*sizeof(Int)*spwPolIDToDataDescIDMap.size();
	if (_cacheUpdated(mysize)) {
		_spwPolIDToDataDescIDMap = spwPolIDToDataDescIDMap;
	}
	return spwPolIDToDataDescIDMap;
}

std::pair<MDirection, MDirection> MSMetaData::getPointingDirection(
	Int& antenna1, Int& antenna2, Double& time, uInt row,
	Bool interpolate, Int initialguess

) const {
	ThrowIf(
		row >= this->nRows(),
		"Row number exceeds number of rows in the MS"
	);
	CountedPtr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);
	antenna1 = (*ant1)[row];
	antenna2 = (*ant2)[row];
	time = (*_getTimes())[row];
	ROMSPointingColumns pCols(_ms->pointing());
	Int pidx1, pidx2;
	pidx1 = pCols.pointingIndex(antenna1, time, initialguess);
	pidx2 = pCols.pointingIndex(antenna2, time, initialguess);
	String intervalColName = MeasurementSet::columnName(MSMainEnums::INTERVAL);
	Double interval = ScalarColumn<Double>(*_ms, intervalColName).getColumn()[row];
	MDirection dir1, dir2;
	if (!interpolate || interval >= pCols.interval()(pidx1)) {
		dir1 = pCols.directionMeas(pidx1);
	}
	else {
		dir1 = _getInterpolatedDirection(pCols, pidx1, time);
	}
	if (!interpolate || interval >= pCols.interval()(pidx2)) {
		dir2 = pCols.directionMeas(pidx2);
	}
	else {
		dir2 = _getInterpolatedDirection(pCols, pidx2, time);
	}
	return std::make_pair(dir1, dir2);
}

MDirection MSMetaData::_getInterpolatedDirection(
	const ROMSPointingColumns& pCols, const Int& index1,
	const Double& time
) const {
	Int antenna = pCols.antennaId()(index1);
	Double interval1 = pCols.interval()(index1);
	Double time1 = pCols.time()(index1);
	Int index2;
	if (time >= time1) {
		index2 = pCols.pointingIndex(antenna, time1+interval1,index1+1);
	}
	else {
		index2 = pCols.pointingIndex(antenna, time1-interval1);
	}
	if (index2 < 0 || index2==index1) {
		// look in opposite direction
		if (time >= time1) {
			index2 = pCols.pointingIndex(antenna, time1-interval1);
		}
		else {
			index2 = pCols.pointingIndex(antenna, time1+interval1,index1+1);
		}
	}
	ThrowIf(
		index2 < 0 || index2==index1,
		"Failed to find pointing index to interpolate direction."
	);

	Double time2 = pCols.time()(index2);
	ThrowIf(
		time2 == time1,
		"Failed to find pointing index with valid timestamp to interpolate direction."
	);

	// Interpolate (time1, time2),(dir1,dir2)
	Vector<Double> dirvec1 = pCols.directionMeas(index1).getAngle("rad").getValue();
	Vector<Double> dirvec2 = pCols.directionMeas(index2).getAngle("rad").getValue();
	MDirection::Ref rf = pCols.directionMeas(index1).getRef();
	Vector<Double> newdir = dirvec1 + (dirvec2-dirvec1)*(time-time1)/(time2-time1);
	Quantity qlon(newdir(0), "rad");
	Quantity qlat(newdir(1), "rad");
	return MDirection(qlon, qlat, rf);
}

vector<uInt> MSMetaData::_getDataDescIDToSpwMap() const {
	if (! _dataDescIDToSpwMap.empty()) {
		return _dataDescIDToSpwMap;
	}
	String spwColName = MSDataDescription::columnName(MSDataDescriptionEnums::SPECTRAL_WINDOW_ID);
	ROScalarColumn<Int> spwCol(_ms->dataDescription(), spwColName);
	Vector<Int> spws = spwCol.getColumn();
	vector<uInt> dataDescToSpwMap(spws.begin(), spws.end());
	uInt mysize = sizeof(Int) * dataDescToSpwMap.size();
	if (_cacheUpdated(mysize)) {
		_dataDescIDToSpwMap = dataDescToSpwMap;
	}
	return dataDescToSpwMap;
}

std::set<uInt> MSMetaData::getPolarizationIDs(
	uInt obsID, Int arrayID, Int scan, uInt spwid
) const {
	ScanKey scanKey;
	scanKey.obsID = obsID;
	scanKey.arrayID = arrayID;
	scanKey.scan = scan;
	_checkScan(scanKey);
	if (! _scanSpwToPolIDMap.empty()) {
		return _scanSpwToPolIDMap.find(std::pair<ScanKey, uInt>(scanKey, spwid))->second;
	}
	vector<uInt> ddToPolMap = getDataDescIDToPolIDMap();
	vector<uInt> ddToSpwMap = _getDataDescIDToSpwMap();
	std::map<ScanKey, std::set<uInt> > scanToDDIDMap;
	vector<std::set<ScanKey> > ddIDToScanMap;
	_getScansAndDDIDMaps(scanToDDIDMap, ddIDToScanMap);
	std::map<std::pair<ScanKey, uInt>, std::set<uInt> > mymap;
	std::map<ScanKey, std::set<uInt> >::const_iterator iter = scanToDDIDMap.begin();
	std::map<ScanKey, std::set<uInt> >::const_iterator end = scanToDDIDMap.end();
	while (iter != end) {
		std::set<uInt> ddids = iter->second;
		std::set<uInt>::const_iterator diter = ddids.begin();
		std::set<uInt>::const_iterator dend = ddids.end();
		while (diter != dend) {
			std::pair<ScanKey, uInt> key(iter->first, ddToSpwMap[*diter]);
			mymap[key].insert(ddToPolMap[*diter]);
			++diter;
		}
		++iter;
	}
	if (_cacheUpdated(_sizeof(mymap))) {
		_scanSpwToPolIDMap = mymap;
	}
	return mymap[std::pair<ScanKey, uInt>(scanKey, spwid)];
}

uInt MSMetaData::_sizeof(const std::map<std::pair<Int, uInt>, std::set<uInt> >& map) {
	uInt size = 0;
	uInt uSize = sizeof(uInt);
	uInt iSize = sizeof(Int);
	for (
		std::map<std::pair<Int, uInt>, std::set<uInt> >::const_iterator iter=map.begin();
		iter!=map.end(); ++iter
	) {
		size += iSize + uSize*(iter->second.size() + 1);
	}
	return size;
}

vector<uInt> MSMetaData::getDataDescIDToPolIDMap() const {
	if (! _dataDescIDToPolIDMap.empty()) {
		return _dataDescIDToPolIDMap;
	}
	String polColName = MSDataDescription::columnName(MSDataDescriptionEnums::POLARIZATION_ID);
	ROScalarColumn<Int> polCol(_ms->dataDescription(), polColName);
	Vector<Int> pols = polCol.getColumn();
	vector<uInt> dataDescToPolIDMap(pols.begin(), pols.end());
	uInt mysize = sizeof(Int) * dataDescToPolIDMap.size();
	if (_cacheUpdated(mysize)) {
		_dataDescIDToPolIDMap = dataDescToPolIDMap;
	}
	return dataDescToPolIDMap;
}

vector<MSMetaData::SpwProperties> MSMetaData::_getSpwInfo(
	std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw,
	std::set<uInt>& fdmSpw, std::set<uInt>& wvrSpw,
	std::set<uInt>& sqldSpw
) const {
	if (_spwInfoStored) {
		avgSpw = _avgSpw;
		tdmSpw = _tdmSpw;
		fdmSpw = _fdmSpw;
		wvrSpw = _wvrSpw;
		sqldSpw = _sqldSpw;
		return _spwInfo;
	}
	vector<SpwProperties> spwInfo = _getSpwInfo2(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	uInt mysize = sizeof(uInt)*(
			avgSpw.size() + tdmSpw.size() + fdmSpw.size()
			+ wvrSpw.size() + sqldSpw.size()
		) + 2*sizeof(Int)*spwInfo.size()
		+ 2*sizeof(Double)*spwInfo.size();
	vector<SpwProperties>::const_iterator end = spwInfo.end();
	for (
		vector<SpwProperties>::const_iterator iter=spwInfo.begin();
		iter!=end; ++iter
	) {
		mysize += 2*(sizeof(Double)*iter->nchans + 20);
		mysize += sizeof(Double)*iter->edgechans.size();
	}
	if (_cacheUpdated(mysize)) {
		_avgSpw = avgSpw;
		_tdmSpw = tdmSpw;
		_fdmSpw = fdmSpw;
		_wvrSpw = wvrSpw;
		_sqldSpw = sqldSpw;
		_spwInfo = spwInfo;
		_spwInfoStored = True;
	}
	return spwInfo;
}

void MSMetaData::_checkField(uInt fieldID) const {
	ThrowIf(
		fieldID >= nFields(),
		"Unknown fieldID " + String::toString(fieldID)
	);
}

void MSMetaData::_checkScan(const ScanKey& key) const {
	std::set<ScanKey> allKeys = _getScanKeys();
	ThrowIf(
		allKeys.find(key) == allKeys.end(),
		"Unknown scan " + toString(key)
	);
}

void MSMetaData::_checkScans(const std::set<ScanKey>& scanKeys) const {
	std::set<ScanKey> allKeys = _getScanKeys();
	std::set<ScanKey>::const_iterator iter = scanKeys.begin();
	std::set<ScanKey>::const_iterator end = scanKeys.end();
	while (iter != end) {
		ThrowIf(
			allKeys.find(*iter) == allKeys.end(),
			"Unknown scan " + toString(*iter)
		);
		++iter;
	}
}

void MSMetaData::_checkSubScan(const SubScanKey& key) const {
	std::set<SubScanKey> allKeys = _getSubScanKeys();
	ThrowIf(
		allKeys.find(key) == allKeys.end(),
		"Unknown subscan " + toString(key)
	);
}

Bool MSMetaData::_hasFieldID(const Int fieldID) const {
	ThrowIf (
		fieldID >= (Int)nFields(),
		"Requested field ID "
		+ String::toString(fieldID)
		+ " is greater than or equal to the number of records ("
		+ String::toString(nFields())
		+ ") in this MS's FIELD table"
	);
	std::set<Int> uniqueFields = getUniqueFiedIDs();
	return uniqueFields.find(fieldID) != uniqueFields.end();
}

std::set<Int> MSMetaData::getUniqueFiedIDs() const {
	if (_uniqueFieldIDs.empty()) {
		CountedPtr<Vector<Int> > allFieldIDs = _getFieldIDs();
		_uniqueFieldIDs.insert(allFieldIDs->begin(), allFieldIDs->end());
	}
	return _uniqueFieldIDs;
}

Bool MSMetaData::_hasStateID(const Int stateID) {
	// This method is responsible for setting _uniqueStateIDs
	ThrowIf(
		stateID >= (Int)nStates(),
		"Requested state ID "
		+ String::toString(stateID)
		+ " is greater than or equal to the number of records ("
		+ String::toString(nStates())
		+ ") in this MS's STATE table"
	);
	if (_uniqueStateIDs.empty()) {
		CountedPtr<Vector<Int> > allStateIDs = _getStateIDs();
		_uniqueStateIDs.insert(allStateIDs->begin(), allStateIDs->end());
	}
	return _uniqueStateIDs.find(stateID) != _uniqueStateIDs.end();
}

void MSMetaData::_hasAntennaID(Int antennaID) {
	ThrowIf(
		antennaID >= (Int)nAntennas(),
		_ORIGIN + "Requested antenna ID "
		+ String::toString(antennaID)
		+ " is greater than or equal to the number of records ("
		+ String::toString(nAntennas())
		+ ") in this MS's ANTENNA table"
	);
}

vector<MSMetaData::SpwProperties>  MSMetaData::_getSpwInfo2(
	std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
	std::set<uInt>& wvrSpw, std::set<uInt>& sqldSpw
) const {
	static const Regex rxSqld("BB_[0-9]#SQLD");
	ROMSSpWindowColumns spwCols(_ms->spectralWindow());
	Vector<Double> bws = spwCols.totalBandwidth().getColumn();
	ArrayColumn<Double> cfCol = spwCols.chanFreq();
	Array<String> cfUnits;
	cfCol.keywordSet().get("QuantumUnits", cfUnits);
	ArrayColumn<Double> cwCol = spwCols.chanWidth();
	Array<String> cwUnits;
	cwCol.keywordSet().get("QuantumUnits", cwUnits);

	Vector<Int> nss  = spwCols.netSideband().getColumn();
	Vector<String> name = spwCols.name().getColumn();
	Bool myHasBBCNo = hasBBCNo();
	Vector<Int> bbcno = myHasBBCNo ? spwCols.bbcNo().getColumn() : Vector<Int>();
	vector<Double> freqLimits(2);
	Vector<Double> tmp;
	vector<SpwProperties> spwInfo(bws.size());
	for (uInt i=0; i<bws.size(); ++i) {
		spwInfo[i].bandwidth = bws[i];
		tmp.resize(0);
		cfCol.get(i, tmp);
		spwInfo[i].chanfreqs = QVD(tmp, *cfUnits.begin());
		spwInfo[i].meanfreq = Quantity(mean(tmp), *cfUnits.begin());
		freqLimits[0] = min(tmp);
		freqLimits[1] = max(tmp);
		spwInfo[i].edgechans = freqLimits;
		tmp.resize(0);
		cwCol.get(i, tmp);
		spwInfo[i].chanwidths = QVD(tmp, *cwUnits.begin());
		// coded this way in ValueMapping
		spwInfo[i].netsideband = nss[i] == 2 ? 1 : -1;
		spwInfo[i].nchans = tmp.size();
		uInt nchan = spwInfo[i].nchans;
		QVD halfWidths = (spwInfo[i].chanwidths)/2.0;
		Quantity lowFreq = (spwInfo[i].chanfreqs - halfWidths).min();
		Quantity highFreq = (spwInfo[i].chanfreqs + halfWidths).max();
		spwInfo[i].centerfreq = (lowFreq + highFreq)/2;
		spwInfo[i].name = name[i];
		if (myHasBBCNo) {
			spwInfo[i].bbcno = bbcno[i];
		    if (name[i].contains(rxSqld)) {
		    	sqldSpw.insert(i);
		    }
		}
		// algorithm from thunter, CAS-5794
		if (
			nchan >= 15
			&& ! (
				nchan == 256 || nchan == 128 || nchan == 64 || nchan == 32
				|| nchan == 16 || nchan == 248 || nchan == 124
				|| nchan == 62 || nchan == 31
			)
		) {
			fdmSpw.insert(i);
		}
		else if (
			spwInfo[i].nchans == 1 && ! name[i].contains("FULL_RES")
		) {
			avgSpw.insert(i);
		}
		else if (spwInfo[i].nchans == 4) {
			wvrSpw.insert(i);
		}
		else {
			tdmSpw.insert(i);
		}
	}
	return spwInfo;
}

std::map<Int, uInt> MSMetaData::_toUIntMap(const Vector<Int>& v) {
	ThrowIf(
		anyLT(v, 0), "Column that should contain nonnegative ints has a negative int"
	);
	std::map<Int, uInt> m;
	Int count = 0;
	for (Vector<Int>::const_iterator iter=v.begin(); iter!=v.end(); ++iter, ++count) {
		m[count] = *iter;
	}
	return m;
}

}

