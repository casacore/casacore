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

#include <casa/OS/File.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableParse.h>
#include <tables/Tables/TableProxy.h>
// DEBUG ONLY

/*
#include <iomanip>
#include <casa/Arrays/ArrayIO.h>

#include <casa/OS/PrecTimer.h>
*/

#define _ORIGIN "MSMetaDataOnDemand::" + String(__FUNCTION__) + ": "

namespace casa {

MSMetaDataOnDemand::MSMetaDataOnDemand(const MeasurementSet *const &ms, const Float maxCacheSizeMB)
	: _ms(ms), _cacheMB(0), _maxCacheMB(maxCacheSizeMB), _nStates(0),
	  _nACRows(0), _nXCRows(0), _nSpw(0), _nFields(0),
	  _nAntennas(0), _nObservations(0), _nScans(0), _nArrays(0),
	  _nrows(0), _nPol(0), _uniqueIntents(), _scanToSpwsMap(),
	  _uniqueScanNumbers(),_uniqueFieldIDs(), _uniqueStateIDs(),
	  _avgSpw(), _tdmSpw(),
	  _fdmSpw(), _wvrSpw(), _sqldSpw(), _antenna1(), _antenna2(),
	  _scans(), _fieldIDs(), _stateIDs(), _dataDescIDs(),
	  _observationIDs(),
	  _scanToNACRowsMap(), _scanToNXCRowsMap(),
	  _fieldToNACRowsMap(), _fieldToNXCRowsMap(),
	  _dataDescIDToSpwMap(),
	  _scanToIntentsMap(), _stateToIntentsMap(),
	  _spwToIntentsMap(),
	  _spwInfo(0), _fieldToSpwMap(),
	  _spwToFieldIDsMap(0), _spwToScansMap(0),
	  _scanToStatesMap(), _scanToFieldsMap(),
	  _fieldToScansMap(),
	  _fieldNames(0),
	  _antennaNames(0), _observatoryNames(0),
	  _antennaNameToIDMap(), _times(),
	  _scanToTimesMap(), _intentToFieldIDMap(),
	  _fieldToTimesMap(), _observatoryPositions(0),
	  _antennaOffsets(0), _uniqueBaselines(0, 0),
	  _exposureTime(0), _nUnflaggedACRows(0),
	  _nUnflaggedXCRows(0), _unflaggedFieldNACRows(),
	  _unflaggedFieldNXCRows(), _unflaggedScanNACRows(),
	  _unflaggedScanNXCRows(),
	  _taqlTableName(
		File(ms->tableName()).exists() ? ms->tableName() : "$1"
	  ),
	  _taqlTempTable(
		File(ms->tableName()).exists() ? 0 : 1, ms
	  ), _flagsColumn(), _scanToTimeRangeMap(),
	  _scanSpwToIntervalMap(), _spwInfoStored(False) {}

MSMetaDataOnDemand::~MSMetaDataOnDemand() {}

uInt MSMetaDataOnDemand::nStates() {
	if (_nStates == 0) {
		_nStates = _getNStates(*_ms);
	}
	return _nStates;
}

std::set<String> MSMetaDataOnDemand::getIntents() {
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

void MSMetaDataOnDemand::_getStateToIntentsMap(
	vector<std::set<String> >& stateToIntentsMap,
	std::set<String>& uniqueIntents
) {
	if (! _uniqueIntents.empty() && ! _stateToIntentsMap.empty()) {
		uniqueIntents = _uniqueIntents;
		stateToIntentsMap = _stateToIntentsMap;
		return;
	}
	MSMetaData::_getStateToIntentsMap(
		stateToIntentsMap,
		uniqueIntents, *_ms
	);

	std::set<String>::const_iterator lastIntent = uniqueIntents.end();
	uInt mysize = 0;
	for (
		std::set<String>::const_iterator intent=uniqueIntents.begin();
		intent!=lastIntent; intent++
	) {
		mysize += intent->size();
	}
	vector<std::set<String> >::const_iterator lastState = stateToIntentsMap.end();
	for (
		vector<std::set<String> >::const_iterator iter=stateToIntentsMap.begin();
		iter!=lastState; iter++
	) {
		std::set<String>::const_iterator lastIntent=iter->end();
		for (
			std::set<String>::const_iterator intent=iter->begin();
			intent!=lastIntent; intent++
		) {
			mysize += intent->size();
		}
	}
	if (_cacheUpdated(mysize)) {
		_uniqueIntents = uniqueIntents;
		_stateToIntentsMap = stateToIntentsMap;
	}
}

std::set<Int> MSMetaDataOnDemand::getScanNumbers() {
	// This method is responsible for setting _uniqueScanNumbers
	if (_uniqueScanNumbers.size() > 0) {
		return _uniqueScanNumbers;
	}
	std::tr1::shared_ptr<Vector<Int> > allScans = _getScans();
	std::set<Int> myUniqueScans(allScans->begin(), allScans->end());
	if (_cacheUpdated(sizeof(Int)*myUniqueScans.size())) {
		_uniqueScanNumbers = myUniqueScans;
	}
	return myUniqueScans;
}

uInt MSMetaDataOnDemand::nScans() {
	if (_nScans == 0) {
		_nScans = getScanNumbers().size();
	}
	return _nScans;
}

uInt MSMetaDataOnDemand::nObservations() {
	if (_nObservations == 0) {
		_nObservations = _ms->observation().nrow();
	}
	return _nObservations;
}

uInt MSMetaDataOnDemand::nArrays() {
	if (_nArrays == 0) {
		// because the ARRAY table apparently is optional
		_nArrays = max(*_getArrayIDs()) + 1;
	}
	return _nArrays;
}

uInt MSMetaDataOnDemand::nRows() {
	return _ms->nrow();
	/*
    if (_nrows == 0) {
        _nrows = _ms->nrow();
    }
	return _nrows;
	*/
}
uInt MSMetaDataOnDemand::nRows(CorrelationType cType) {

	if (cType == BOTH) {
		return nRows();
	}
	uInt nACRows, nXCRows;
	std::tr1::shared_ptr<AOSFMapI> scanToNACRowsMap, scanToNXCRowsMap;
	std::tr1::shared_ptr<std::map<Int, uInt> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getRowStats(
		nACRows, nXCRows, scanToNACRowsMap,
		scanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return nACRows;
	}
	else {
		return nXCRows;
	}
}

uInt MSMetaDataOnDemand::nRows(
	CorrelationType cType, Int arrayID, Int observationID,
	Int scanNumber, Int fieldID
) {
	uInt nACRows, nXCRows;
	std::tr1::shared_ptr<AOSFMapI> scanToNACRowsMap, scanToNXCRowsMap;
	std::tr1::shared_ptr<std::map<Int, uInt> > fieldToNACRowsMap, fieldToNXCRowsMap;
    _getRowStats(
		nACRows, nXCRows, scanToNACRowsMap,
		scanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return (*scanToNACRowsMap)[arrayID][observationID][scanNumber][fieldID];
	}
	else if (cType == CROSS) {
		return (*scanToNXCRowsMap)[arrayID][observationID][scanNumber][fieldID];
	}
	else {
		return (*scanToNACRowsMap)[arrayID][observationID][scanNumber][fieldID]
		    + (*scanToNXCRowsMap)[arrayID][observationID][scanNumber][fieldID];
	}

}

uInt MSMetaDataOnDemand::nRows(CorrelationType cType, Int fieldID) {
	uInt nACRows, nXCRows;
	std::tr1::shared_ptr<AOSFMapI> scanToNACRowsMap, scanToNXCRowsMap;
	std::tr1::shared_ptr<std::map<Int, uInt> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getRowStats(
		nACRows, nXCRows, scanToNACRowsMap,
		scanToNXCRowsMap, fieldToNACRowsMap,
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

Double MSMetaDataOnDemand::nUnflaggedRows() {
	Double nACRows, nXCRows;
	std::tr1::shared_ptr<AOSFMapD> scanToNACRowsMap, scanToNXCRowsMap;
	std::tr1::shared_ptr<std::map<Int, Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, scanToNACRowsMap,
		scanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	return nACRows + nXCRows;
}
Double MSMetaDataOnDemand::nUnflaggedRows(CorrelationType cType) {
	if (cType == BOTH) {
		return nUnflaggedRows();
	}
	Double nACRows, nXCRows;
	std::tr1::shared_ptr<AOSFMapD> scanToNACRowsMap, scanToNXCRowsMap;
	std::tr1::shared_ptr<std::map<Int, Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, scanToNACRowsMap,
		scanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return nACRows;
	}
	else {
		return nXCRows;
	}
}

Double MSMetaDataOnDemand::nUnflaggedRows(
	CorrelationType cType, Int arrayID, Int observationID,
	Int scanNumber, Int fieldID
) {
	Double nACRows, nXCRows;
	std::tr1::shared_ptr<AOSFMapD> scanToNACRowsMap, scanToNXCRowsMap;
	std::tr1::shared_ptr<std::map<Int, Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, scanToNACRowsMap,
		scanToNXCRowsMap, fieldToNACRowsMap,
		fieldToNXCRowsMap
	);
	if (cType == AUTO) {
		return (*scanToNACRowsMap)[arrayID][observationID][scanNumber][fieldID];
	}
	else if (cType == CROSS) {
		return (*scanToNXCRowsMap)[arrayID][observationID][scanNumber][fieldID];
	}
	else {
		return (*scanToNACRowsMap)[arrayID][observationID][scanNumber][fieldID]
		    + (*scanToNXCRowsMap)[arrayID][observationID][scanNumber][fieldID];
	}
}

Double MSMetaDataOnDemand::nUnflaggedRows(CorrelationType cType, Int fieldID) {
	Double nACRows, nXCRows;
	std::tr1::shared_ptr<AOSFMapD> scanToNACRowsMap, scanToNXCRowsMap;
	std::tr1::shared_ptr<std::map<Int, Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
	_getUnflaggedRowStats(
		nACRows, nXCRows, scanToNACRowsMap,
		scanToNXCRowsMap, fieldToNACRowsMap,
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

void MSMetaDataOnDemand::_getRowStats(
	uInt& nACRows, uInt& nXCRows,
	std::tr1::shared_ptr<AOSFMapI>& scanToNACRowsMap,
	std::tr1::shared_ptr<AOSFMapI>& scanToNXCRowsMap,
	std::tr1::shared_ptr<std::map<Int, uInt> >& fieldToNACRowsMap,
	std::tr1::shared_ptr<std::map<Int, uInt> >& fieldToNXCRowsMap
) {
	// this method is responsible for setting _nACRows, _nXCRows, _scanToNACRowsMap,
	// _scanToNXCRowsMap, _fieldToNACRowsMap, _fieldToNXCRowsMap
	if (
		_nACRows > 0 || _nXCRows > 0
	) {
		nACRows = _nACRows;
		nXCRows = _nXCRows;
		scanToNACRowsMap = _scanToNACRowsMap;
		scanToNXCRowsMap = _scanToNXCRowsMap;
		fieldToNACRowsMap = _fieldToNACRowsMap;
		fieldToNXCRowsMap = _fieldToNXCRowsMap;
		return;
	}
	std::tr1::shared_ptr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);
	AOSFMapI *myScanToNACRowsMap, *myScanToNXCRowsMap;
	std::map<Int, uInt> *myFieldToNACRowsMap, *myFieldToNXCRowsMap;
	MSMetaData::_getRowStats(
		nACRows, nXCRows, myScanToNACRowsMap,
		myScanToNXCRowsMap, myFieldToNACRowsMap,
		myFieldToNXCRowsMap, *ant1, *ant2,
		*(_getScans()), *(_getFieldIDs()),
		*(_getObservationIDs()), *(_getArrayIDs())
	);
	scanToNACRowsMap.reset(myScanToNACRowsMap);
	scanToNXCRowsMap.reset(myScanToNXCRowsMap);
	fieldToNACRowsMap.reset(myFieldToNACRowsMap);
	fieldToNXCRowsMap.reset(myFieldToNXCRowsMap);

	Float newSize = _cacheMB + sizeof(Int)*(
		2 + 2*scanToNACRowsMap->size()
		+ 2*scanToNXCRowsMap->size()
		+ 2*fieldToNACRowsMap->size()
		+ fieldToNACRowsMap->size()
		+ fieldToNXCRowsMap->size()
	);

	if (_cacheUpdated(newSize)) {
		_nACRows = nACRows;
		_nXCRows = nXCRows;
		_scanToNACRowsMap = scanToNACRowsMap;
		_scanToNXCRowsMap = scanToNXCRowsMap;
		_fieldToNACRowsMap = fieldToNACRowsMap;
		_fieldToNXCRowsMap = fieldToNXCRowsMap;
	}
}

void MSMetaDataOnDemand::_getAntennas(
	std::tr1::shared_ptr<Vector<Int> >& ant1,
	std::tr1::shared_ptr<Vector<Int> >& ant2
) {
	if (
		_antenna1 && _antenna1->size() > 0
		&& _antenna2 && _antenna2->size() > 0
	) {
		ant1 = _antenna1;
		ant2 = _antenna2;
	}
	Vector<Int> a1, a2;
	MSMetaData::_getAntennas(a1, a2, *_ms);
	ant1.reset(new Vector<Int>(a1));
	ant2.reset(new Vector<Int>(a2));

	if (_cacheUpdated(2*sizeof(Int)*ant1->size())) {
		_antenna1 = ant1;
		_antenna2 = ant2;
	}
}

std::tr1::shared_ptr<Vector<Int> > MSMetaDataOnDemand::_getScans() {
	if (_scans && _scans->size() > 0) {
		return _scans;
	}
	std::tr1::shared_ptr<Vector<Int> > scans(new Vector<Int>(MSMetaData::_getScans(*_ms)));
	if (_cacheUpdated(sizeof(Int)*scans->size())) {
		_scans = scans;
	}
	return scans;
}

std::tr1::shared_ptr<Vector<Int> > MSMetaDataOnDemand::_getObservationIDs() {
	if (_observationIDs && _observationIDs->size() > 0) {
		return _observationIDs;
	}
	std::tr1::shared_ptr<Vector<Int> > obsIDs(
		new Vector<Int>(MSMetaData::_getObservationIDs(*_ms))
	);
	if (_cacheUpdated(sizeof(Int)*obsIDs->size())) {
		_observationIDs = obsIDs;
	}
	return obsIDs;
}

std::tr1::shared_ptr<Vector<Int> > MSMetaDataOnDemand::_getArrayIDs() {
	if (_arrayIDs && _arrayIDs->size() > 0) {
		return _arrayIDs;
	}
	std::tr1::shared_ptr<Vector<Int> > arrIDs(
		new Vector<Int>(MSMetaData::_getArrayIDs(*_ms))
	);
	if (_cacheUpdated(sizeof(Int)*arrIDs->size())) {
		_arrayIDs = arrIDs;
	}
	return arrIDs;
}

std::tr1::shared_ptr<Vector<Int> > MSMetaDataOnDemand::_getFieldIDs() {
	if (_fieldIDs && _fieldIDs->size() > 0) {
		return _fieldIDs;
	}
	std::tr1::shared_ptr<Vector<Int> > fields(
		new Vector<Int>(MSMetaData::_getFieldIDs(*_ms))
	);
	if (_cacheUpdated(sizeof(Int)*fields->size())) {
		_fieldIDs = fields;
	}
	return fields;
}

std::tr1::shared_ptr<Vector<Int> > MSMetaDataOnDemand::_getStateIDs() {
	if (_stateIDs && _stateIDs->size() > 0) {
		return _stateIDs;
	}
	std::tr1::shared_ptr<Vector<Int> > states(
		new Vector<Int>(MSMetaData::_getStates(*_ms))
	);
    Int maxState = max(*states);
    Int nstates = (Int)nStates();
    if (maxState >= nstates) {
        ostringstream oss;
        oss << "MSMetaDataOnDemand::_getStateIDs(): Error: MS only has " << nstates
             << " rows in its STATE table, but references STATE_ID "
             << maxState << " in its main table.";
        throw AipsError(oss.str());
    }
    if (_cacheUpdated(sizeof(Int)*states->size())) {
		_stateIDs = states;
	}
	return states;
}

std::tr1::shared_ptr<Vector<Int> > MSMetaDataOnDemand::_getDataDescIDs() {
	if (_dataDescIDs && ! _dataDescIDs->empty()) {
		return _dataDescIDs;
	}
	std::tr1::shared_ptr<Vector<Int> > dataDescIDs(
		new Vector<Int>(MSMetaData::_getDataDescIDs(*_ms))
	);
	if (_cacheUpdated(sizeof(Int)*dataDescIDs->size())) {
		_dataDescIDs = dataDescIDs;
	}
	return dataDescIDs;
}

std::set<Int> MSMetaDataOnDemand::getScansForState(const Int stateID) {
	if (! _hasStateID(stateID)) {
		return std::set<Int>();
	}
	std::set<Int> uniqueScans;
	std::map<Int, std::set<Int> > myScanToStatesMap = _getScanToStatesMap();
	std::tr1::shared_ptr<Vector<Int> > scans = _getScans();
	uniqueScans.insert(scans->begin(), scans->end());
	std::set<Int>::const_iterator lastScan = uniqueScans.end();
	std::set<Int> scansForState;
	for (
		std::set<Int>::const_iterator scanNum=uniqueScans.begin();
		scanNum!=lastScan; scanNum++
	) {
		std::set<Int> statesSet = myScanToStatesMap.find(*scanNum)->second;
		if (statesSet.find(stateID) != statesSet.end()) {
			scansForState.insert(*scanNum);
		}
	}
	return scansForState;
}

std::map<Int, std::set<Int> > MSMetaDataOnDemand::_getScanToStatesMap() {
	if (! _scanToStatesMap.empty()) {
		return _scanToStatesMap;
	}
	std::map<Int, std::set<Int> > myScanToStatesMap;
	uInt mySize = 0;

	if (_ms->state().nrow() == 0) {
		std::set<Int> empty;
		std::set<Int> uniqueScans = getScanNumbers();
		std::set<Int>::const_iterator end = uniqueScans.end();
		for (
			std::set<Int>::const_iterator scanNum=uniqueScans.begin();
			scanNum!=end; scanNum++
		) {
			myScanToStatesMap[*scanNum] = empty;
		}
	}
	else {
		myScanToStatesMap = MSMetaData::_getScanToStatesMap(
			*(_getScans()), *(_getStateIDs())
		);
	}
	std::map<Int, std::set<Int> >::const_iterator end = myScanToStatesMap.end();
	for (
		std::map<Int, std::set<Int> >::const_iterator iter=myScanToStatesMap.begin();
		iter!=end; iter++
	) {
		mySize += iter->second.size() + 1;
	}
	if (_cacheUpdated(mySize*sizeof(Int)/1e6)) {
		_scanToStatesMap = myScanToStatesMap;
	}
	return myScanToStatesMap;
}

void MSMetaDataOnDemand::_getScansAndIntentsMaps(
	std::map<Int, std::set<String> >& scanToIntentsMap,
	std::map<String, std::set<Int> >& intentToScansMap
) {
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
	std::map<Int, std::set<Int> > scanToStatesMap = _getScanToStatesMap();
	std::map<Int, std::set<Int> >::const_iterator end = scanToStatesMap.end();
	std::set<Int> states;
	std::set<String> intents;
	for (
		std::map<Int, std::set<Int> >::const_iterator iter=scanToStatesMap.begin();
		iter!=end; iter++
	) {
		uInt scan = iter->first;
		states = iter->second;
		std::set<Int>::const_iterator endState = states.end();
		for (
			std::set<Int>::const_iterator myState=states.begin();
			myState!=endState; myState++
		) {
            intents = stateToIntentsMap[*myState];
            scanToIntentsMap[scan].insert(intents.begin(), intents.end());
			std::set<String>::const_iterator endIntent = intents.end();
			for (
				std::set<String>::const_iterator myIntent=intents.begin();
				myIntent!=endIntent; myIntent++
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

uInt MSMetaDataOnDemand::_sizeof(const std::map<Int, std::set<String> >& m) {
	uInt size = sizeof(Int) * m.size();
	std::map<Int, std::set<String> >::const_iterator end = m.end();
	for (
		std::map<Int, std::set<String> >::const_iterator iter=m.begin();
		iter!=end; iter++
	) {
		std::set<String>::const_iterator end2 = iter->second.end();
		for (
			std::set<String>::const_iterator iter2=iter->second.begin();
			iter2!=end2; iter2++
		) {
			size += iter2->size();
		}
	}
	return size;
}

uInt MSMetaDataOnDemand::_sizeof(const vector<std::set<String> >& m) {
	uInt size = sizeof(Int) * m.size();
	vector<std::set<String> >::const_iterator end = m.end();
	for (
		vector<std::set<String> >::const_iterator iter=m.begin();
		iter!=end; iter++
	) {
		std::set<String>::const_iterator end2 = iter->end();
		for (
			std::set<String>::const_iterator iter2=iter->begin();
			iter2!=end2; iter2++
		) {
			size += iter2->size();
		}
	}
	return size;
}


uInt MSMetaDataOnDemand::_sizeof(const vector<String>& m) {
	vector<String>::const_iterator end = m.end();
	uInt size = 0;
	for (
		vector<String>::const_iterator iter=m.begin();
		iter!=end; iter++
	) {
		size += iter->length();
	}
	return size;
}

uInt MSMetaDataOnDemand::_sizeof(const std::map<String, std::set<Int> >& m) {
	uInt setssize = 0;
	uInt size = 0;
	std::map<String, std::set<Int> >::const_iterator end = m.end();
	for (
		std::map<String, std::set<Int> >::const_iterator iter=m.begin();
		iter!=end; iter++
	) {
		size += iter->first.size();
		setssize += iter->second.size();
	}
	size += sizeof(Int) * setssize;
	return size;
}


uInt MSMetaDataOnDemand::_sizeof(const std::map<String, std::set<uInt> >& m) {
	uInt setssize = 0;
	uInt size = 0;
	std::map<String, std::set<uInt> >::const_iterator end = m.end();
	for (
		std::map<String, std::set<uInt> >::const_iterator iter=m.begin();
		iter!=end; iter++
	) {
		size += iter->first.size();
		setssize += iter->second.size();
	}
	size += sizeof(uInt) * setssize;
	return size;
}

uInt MSMetaDataOnDemand::_sizeof(const std::map<Double, std::set<Int> >& m) {
	uInt setssize = 0;
	uInt size = sizeof(Double) * m.size();
	std::map<Double, std::set<Int> >::const_iterator end = m.end();
	for (
		std::map<Double, std::set<Int> >::const_iterator iter=m.begin();
		iter!=end; iter++
	) {
		setssize += iter->second.size();
	}
	size += sizeof(Int) * setssize;
	return size;
}

uInt MSMetaDataOnDemand::_sizeof(const std::map<Int, std::set<Double> >& m) {
	uInt setssize = 0;
	uInt size = sizeof(Int) * m.size();
	std::map<Int, std::set<Double> >::const_iterator end = m.end();
	for (
		std::map<Int, std::set<Double> >::const_iterator iter=m.begin();
		iter!=end; iter++
	) {
		setssize += iter->second.size();
	}
	size += sizeof(Double) * setssize;
	return size;
}

std::set<String> MSMetaDataOnDemand::getIntentsForScan(const Int scan) {
	_checkScan(scan, getScanNumbers());
	std::map<Int, std::set<String> > scanToIntentsMap;
	std::map<String, std::set<Int> > intentToScansMap;
	_getScansAndIntentsMaps(
		scanToIntentsMap,
		intentToScansMap
	);
	return scanToIntentsMap[scan];
}

Bool MSMetaDataOnDemand::_cacheUpdated(const Float incrementInBytes) {
	Float newSize = _cacheMB + incrementInBytes/1e6;
	if (newSize <= _maxCacheMB) {
		_cacheMB = newSize;
		return True;
	}
	return False;
}

std::set<uInt> MSMetaDataOnDemand::getSpwsForIntent(const String& intent) {
	if (! _hasIntent(intent)) {
		return std::set<uInt>();
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

uInt MSMetaDataOnDemand::nSpw(Bool includewvr) {
	if (_nSpw > 0) {
		return includewvr ? _nSpw : _nSpw - getWVRSpw().size();
	}
	uInt nSpw = _ms->spectralWindow().nrow();
	_nSpw = nSpw;
	return includewvr ? nSpw : nSpw - getWVRSpw().size();
}

uInt MSMetaDataOnDemand::nPol() {
	if (_nPol == 0) {
		_nPol = _ms->polarization().nrow();
	}
	return _nPol;
}

std::set<String> MSMetaDataOnDemand::getIntentsForSpw(const uInt spw) {
	if (spw >= nSpw(True)) {
		throw AipsError(
			_ORIGIN + "spectral window out of range"
		);
	}
	return _getSpwToIntentsMap()[spw];
}

std::set<String> MSMetaDataOnDemand::getIntentsForField(Int fieldID) {
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


uInt MSMetaDataOnDemand::nFields() {
	if (_nFields > 0) {
		return _nFields;
	}
	uInt nFields = _ms->field().nrow();
	_nFields = nFields;
	return nFields;
}

void MSMetaDataOnDemand::_getFieldsAndSpwMaps(
	std::map<Int, std::set<uInt> >& fieldToSpwMap,
	vector<std::set<Int> >& spwToFieldMap
) {
	// This method has the responsibility of setting _fieldToSpwMap and _spwToFieldIDMap
	if (! _fieldToSpwMap.empty() && ! _spwToFieldIDsMap.empty()) {
		fieldToSpwMap = _fieldToSpwMap;
		spwToFieldMap = _spwToFieldIDsMap;
		return;
	}
	std::tr1::shared_ptr<Vector<Int> >  allDDIDs = _getDataDescIDs();
	std::tr1::shared_ptr<Vector<Int> >  allFieldIDs = _getFieldIDs();
	Vector<Int>::const_iterator endDDID = allDDIDs->end();
	Vector<Int>::const_iterator curField = allFieldIDs->begin();
	fieldToSpwMap.clear();
	spwToFieldMap.resize(nSpw(True));
	std::map<Int, uInt> ddidToSpwMap = _getDataDescIDToSpwMap();
	for (
		Vector<Int>::const_iterator curDDID=allDDIDs->begin();
		curDDID!=endDDID; curDDID++, curField++
	) {
		uInt spw = ddidToSpwMap[*curDDID];
		fieldToSpwMap[*curField].insert(spw);
		spwToFieldMap[spw].insert(*curField);
	}
	std::map<Int, std::set<uInt> >::const_iterator mapEnd = fieldToSpwMap.end();
	uInt mySize = 0;
	for (
		std::map<Int, std::set<uInt> >::const_iterator curMap = fieldToSpwMap.begin();
		curMap != mapEnd; curMap++
	) {
		mySize += curMap->second.size();
	}
	mySize *= sizeof(uInt);
	mySize += sizeof(Int) * fieldToSpwMap.size() + sizeof(uInt)*spwToFieldMap.size();
	vector<std::set<Int> >::const_iterator map2End = spwToFieldMap.end();
	uInt count = 0;
	for (
		vector<std::set<Int> >::const_iterator curMap = spwToFieldMap.begin();
		curMap != map2End; curMap++
	) {
		count += curMap->size();
	}
	mySize += sizeof(Int)*count;
	if (_cacheUpdated(mySize)) {
		_fieldToSpwMap = fieldToSpwMap;
		_spwToFieldIDsMap = spwToFieldMap;
	}
}

std::set<uInt> MSMetaDataOnDemand::getSpwsForField(Int fieldID) {
	if (! _hasFieldID(fieldID)) {
		return std::set<uInt>();
	}
	std::map<Int, std::set<uInt> > myFieldToSpwMap;
	vector<std::set<Int> > mySpwToFieldMap;
	_getFieldsAndSpwMaps(myFieldToSpwMap, mySpwToFieldMap);
    return myFieldToSpwMap[fieldID];
}

std::set<uInt> MSMetaDataOnDemand::getSpwsForField(const String& fieldName) {
	uInt myNFields = nFields();
	vector<String> fieldNames = _getFieldNames();
	std::set<uInt> spws;
	for (uInt i=0; i<myNFields; i++) {
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

vector<String> MSMetaDataOnDemand::_getFieldNames() {
	if (! _fieldNames.empty()) {
		return _fieldNames;
	}
	vector<String> fieldNames = MSMetaData::_getFieldNames(*_ms);
	uInt mysize = 0;
	vector<String>::const_iterator end = fieldNames.end();
	for (
		vector<String>::const_iterator name=fieldNames.begin();
		name!=end; name++
	) {
		mysize += name->size();
	}
	if (_cacheUpdated(mysize)) {
		_fieldNames = fieldNames;
	}
	return fieldNames;
}

std::set<Int> MSMetaDataOnDemand::getFieldIDsForSpw(const uInt spw) {
	uInt myNSpw = nSpw(True);
	if (spw >= myNSpw) {
		throw AipsError(_ORIGIN + "spectral window out of range");
	}
	std::map<Int, std::set<uInt> > myFieldToSpwMap;
	vector<std::set<Int> > mySpwToFieldMap;
	_getFieldsAndSpwMaps(myFieldToSpwMap, mySpwToFieldMap);
	return mySpwToFieldMap[spw];
}

std::set<String> MSMetaDataOnDemand::getFieldNamesForSpw(const uInt spw) {
	std::set<Int> fieldIDs = getFieldIDsForSpw(spw);
	std::set<String> fieldNames;
	vector<String> allFieldNames = _getFieldNames();
	for (
		std::set<Int>::const_iterator fieldID = fieldIDs.begin();
		fieldID!=fieldIDs.end(); fieldID++
	) {
		fieldNames.insert(allFieldNames[*fieldID]);
	}
	return fieldNames;
}

void MSMetaDataOnDemand::_getScansAndSpwMaps(
	std::map<Int, std::set<uInt> >& scanToSpwMap,
	vector<std::set<Int> >& spwToScanMap
) {
	// This method is responsible for setting _scanToSpwsMap and _spwToScansMap
	if (! _scanToSpwsMap.empty() && ! _spwToScansMap.empty()) {
		scanToSpwMap = _scanToSpwsMap;
		spwToScanMap = _spwToScansMap;
		return;
	}
	std::tr1::shared_ptr<Vector<Int> > allDDIDs = _getDataDescIDs();
	std::tr1::shared_ptr<Vector<Int> > allScans = _getScans();
	std::map<Int, uInt> ddToSpw = _getDataDescIDToSpwMap();
	Vector<Int>::const_iterator end = allDDIDs->end();
	Vector<Int>::const_iterator myscan = allScans->begin();
	spwToScanMap.resize(this->nSpw(True));
	for (
		Vector<Int>::const_iterator ddID=allDDIDs->begin();
		ddID!=end; ddID++, myscan++
	) {
		uInt spw = ddToSpw[*ddID];
		scanToSpwMap[*myscan].insert(spw);
		spwToScanMap[spw].insert(*myscan);
	}
	if (_cacheUpdated(_sizeof(scanToSpwMap)) + _sizeof(spwToScanMap)) {
		_scanToSpwsMap = scanToSpwMap;
		_spwToScansMap = spwToScanMap;
	}
}

uInt MSMetaDataOnDemand::_sizeof(const std::map<Int, std::set<uInt> >& map) {
	uInt size = 0;
	std::map<Int, std::set<uInt> >::const_iterator end = map.end();
	for (
		std::map<Int, std::set<uInt> >::const_iterator iter=map.begin();
		iter!=end; iter++
	) {
		size += iter->second.size();
	}
	size *= sizeof(uInt);
	size += map.size()*sizeof(Int);
	return size;
}

uInt MSMetaDataOnDemand::_sizeof(const std::map<Int, std::set<Int> >& map) {
	uInt size = 0;
	std::map<Int, std::set<Int> >::const_iterator end = map.end();
	for (
		std::map<Int, std::set<Int> >::const_iterator iter=map.begin();
		iter!=end; iter++
	) {
		size += iter->second.size();
	}
	size *= sizeof(Int);
	size += map.size()*sizeof(Int);
	return size;
}

uInt MSMetaDataOnDemand::_sizeof(const vector<std::set<Int> >& v) {
	uInt size = 0;
	vector<std::set<Int> >::const_iterator end = v.end();
	for (
		vector<std::set<Int> >::const_iterator iter=v.begin();
		iter!=end; iter++
	) {
		size = iter->size();
	}
	size *= sizeof(Int);
	return size;
}

std::set<uInt> MSMetaDataOnDemand::getSpwsForScan(const Int scan) {
    _checkScan(scan, getScanNumbers());
    std::map<Int, std::set<uInt> > scanToSpwMap;
    vector<std::set<Int> > spwToScanMap;
    _getScansAndSpwMaps(
    	scanToSpwMap, spwToScanMap
    );
    return scanToSpwMap[scan];
}

std::set<Int> MSMetaDataOnDemand::getScansForSpw(const uInt spw) {
	uInt myNSpw = nSpw(True);
	if (spw >= myNSpw) {
		throw AipsError(
			_ORIGIN + "spectral window out of range"
		);
	}
	std::map<Int, std::set<uInt> > scanToSpwMap;
	vector<std::set<Int> > spwToScanMap;
	_getScansAndSpwMaps(
		scanToSpwMap, spwToScanMap
	);
	return spwToScanMap[spw];
}

uInt MSMetaDataOnDemand::nAntennas() {
	if (_nAntennas > 0) {
		return _nAntennas;
	}
	uInt nAnts = _ms->antenna().nrow();
	_nAntennas = nAnts;
	return nAnts;
}

vector<String> MSMetaDataOnDemand::getAntennaNames(
	std::map<String, uInt>& namesToIDsMap,
	const vector<uInt>& antennaIDs
) {
	uInt nAnts = nAntennas();
	vector<String> names, myAnts;
	if (antennaIDs.empty()) {
		if (_antennaNames.empty()) {
			std::map<String, uInt> mymap;
			names = _getAntennaNames(namesToIDsMap, *_ms);
			myAnts = names;
		}
		else {
			namesToIDsMap = _antennaNameToIDMap;
			return _antennaNames;
		}
	}
	else {
		uInt mymax = max(Vector<uInt>(antennaIDs));
		if (mymax >= nAnts) {
			throw AipsError(
				_ORIGIN + "Antenna ID " + String::toString(mymax)
			+ " out of range."
			);
		}
		if (! _antennaNames.empty()) {
			vector<uInt>::const_iterator end = antennaIDs.end();
			for (
				vector<uInt>::const_iterator id=antennaIDs.begin();
				id!=end; id++
			) {
				names.push_back(_antennaNames[*id]);
			}
			namesToIDsMap = _antennaNameToIDMap;
			return names;
		}
		else {
			std::map<String, uInt> mymap;
			myAnts = _getAntennaNames(namesToIDsMap, *_ms);
			vector<uInt>::const_iterator end = antennaIDs.end();
			for (
				vector<uInt>::const_iterator id=antennaIDs.begin();
				id!=end; id++
			) {
				names.push_back(myAnts[*id]);
			}
		}
	}
	vector<String>::const_iterator end1 = myAnts.end();
	uInt mysize = nAnts*sizeof(uInt);
	for (
		vector<String>::const_iterator name=myAnts.begin();
		name!=end1; name++
	) {
		mysize += 2*name->size();
	}
	if (_cacheUpdated(mysize)) {
		_antennaNames = myAnts;
		_antennaNameToIDMap = namesToIDsMap;
	}
	return names;
}

vector<uInt> MSMetaDataOnDemand::getAntennaIDs(
	const vector<String>& antennaNames
) {
	std::map<String, uInt> namesToIDsMap;
	vector<String> names = getAntennaNames(namesToIDsMap);
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

vector<String> MSMetaDataOnDemand::getAntennaStations(const vector<uInt>& antennaIDs) {
	vector<String> allStations = _getStationNames();
	if (antennaIDs.empty()) {
		return allStations;
	}
	_hasAntennaID(max(Vector<uInt>(antennaIDs)));
	vector<String> myStationNames;
	vector<uInt>::const_iterator end = antennaIDs.end();
	for (
		vector<uInt>::const_iterator iter=antennaIDs.begin();
		iter!=end; iter++
	) {
		myStationNames.push_back(allStations[*iter]);
	}
	return myStationNames;
}

vector<String> MSMetaDataOnDemand::getAntennaStations(const vector<String>& antennaNames) {
	return getAntennaStations(getAntennaIDs(antennaNames));
}

vector<String> MSMetaDataOnDemand::_getStationNames() {
	if (! _stationNames.empty()) {
		return _stationNames;
	}
	vector<String> stationNames = MSMetaData::_getAntennaStationNames(*_ms);
	if (_cacheUpdated(_sizeof(stationNames))) {
		_stationNames = stationNames;
	}
	return stationNames;
}

std::set<uInt> MSMetaDataOnDemand::getTDMSpw() {
	if (! _tdmSpw.empty()) {
		return _tdmSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return tdmSpw;
}

vector<Double> MSMetaDataOnDemand::getBandWidths() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Double> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->bandwidth);
	}
	return out;
}

vector<Quantum<Vector<Double> > > MSMetaDataOnDemand::getChanFreqs() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Quantum<Vector<Double> > > out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->chanfreqs);
	}
	return out;
}

vector<Quantum<Vector<Double> > > MSMetaDataOnDemand::getChanWidths() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Quantum<Vector<Double> > > out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->chanwidths);
	}
	return out;
}

vector<Int> MSMetaDataOnDemand::getNetSidebands() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Int> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->netsideband);
	}
	return out;
}

vector<Quantity> MSMetaDataOnDemand::getMeanFreqs() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<Quantity> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->meanfreq);
	}
	return out;
}

vector<uInt> MSMetaDataOnDemand::nChans() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<uInt> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->nchans);
	}
	return out;
}

vector<vector<Double> > MSMetaDataOnDemand::getEdgeChans() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<vector<Double> > out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->edgechans);
	}
	return out;
}

vector<uInt> MSMetaDataOnDemand::getBBCNos() {
	if (! hasBBCNo(*_ms)) {
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
		iter!=end; iter++
	) {
		out.push_back(iter->bbcno);
	}
	return out;
}

std::map<uInt, std::set<uInt> > MSMetaDataOnDemand::getBBCNosToSpwMap(
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
		iter!=end; iter++, i++
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

vector<String> MSMetaDataOnDemand::getSpwNames() {
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<MSMetaData::SpwProperties> props = _getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
	);
	vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
	vector<String> out;
	for (
		vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
		iter!=end; iter++
	) {
		out.push_back(iter->name);
	}
	return out;
}

std::set<uInt> MSMetaDataOnDemand::getFDMSpw() {
	if (! _fdmSpw.empty()) {
		return _fdmSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return fdmSpw;
}

std::set<uInt> MSMetaDataOnDemand::getChannelAvgSpw() {
	if (! _avgSpw.empty()) {
		return _avgSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return avgSpw;
}

std::set<uInt> MSMetaDataOnDemand::getWVRSpw() {
	if (_spwInfoStored) {
		return _wvrSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return wvrSpw;
}

std::set<uInt> MSMetaDataOnDemand::getSQLDSpw() {
	if (_spwInfoStored) {
		return _sqldSpw;
	}
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	_getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	return sqldSpw;
}

std::set<Int> MSMetaDataOnDemand::getScansForTimes(
	const Double center, const Double tol
) {
	_checkTolerance(tol);
	std::set<Int> uniqueScans = getScanNumbers();
	std::tr1::shared_ptr<std::map<Int, std::set<Double> > > scanToTimesMap = _getScanToTimesMap();
	Double minTime = center - tol;
	Double maxTime = center + tol;
	std::set<Int> scans;
	std::set<Int>::const_iterator end = uniqueScans.end();
	for (
		std::set<Int>::const_iterator scan=uniqueScans.begin();
		scan!=end; scan++
	) {
		std::set<Double> times = scanToTimesMap->find(*scan)->second;
		if (*(++times.rend()) >= minTime && *times.begin() <= maxTime) {
			scans.insert(*scan);
		}
	}
	return scans;
}

std::tr1::shared_ptr<std::map<Int, std::set<Double> > > MSMetaDataOnDemand::_getScanToTimesMap() {
	if (_scanToTimesMap && ! _scanToTimesMap->empty()) {
		return _scanToTimesMap;
	}
	std::tr1::shared_ptr<std::map<Int, std::set<Double> > > scanToTimesMap(
		new std::map<Int, std::set<Double> >(
			MSMetaData::_getScanToTimesMap(
				*_getScans(), *_getTimes()
			)
		)
	);
	uInt mysize = 0;
	std::map<Int, std::set<Double> >::const_iterator end = scanToTimesMap->end();
	for (
		std::map<Int, std::set<Double> >::const_iterator iter=scanToTimesMap->begin();
		iter!=end; iter++
	) {
		mysize += iter->second.size();
	}
	mysize *= sizeof(Double);
	mysize += sizeof(Int)*scanToTimesMap->size();
	if (_cacheUpdated(mysize)) {
		_scanToTimesMap = scanToTimesMap;
	}
	return scanToTimesMap;
}

std::tr1::shared_ptr<Vector<Double> > MSMetaDataOnDemand::_getTimes() {
	if (_times && ! _times->empty()) {
		return _times;
	}
	std::tr1::shared_ptr<Vector<Double> > times(
		new Vector<Double>(MSMetaData::_getTimes(*_ms))
	);
	if (_cacheUpdated(sizeof(Double)*times->size())) {
		_times = times;
	}
	return times;
}

std::tr1::shared_ptr<ArrayColumn<Bool> > MSMetaDataOnDemand::_getFlags() {
	if (_flagsColumn && ! _flagsColumn->nrow() > 0) {
			return _flagsColumn;
		}
		std::tr1::shared_ptr<ArrayColumn<Bool> > flagsColumn(
			MSMetaData::_getFlags(*_ms)
		);
		uInt mysize = 0;
		for (uInt i=0; i<flagsColumn->nrow(); i++) {
			mysize += flagsColumn->get(i).size();
		}
		if (_cacheUpdated(sizeof(Bool)*mysize)) {
			_flagsColumn = flagsColumn;
		}
		return flagsColumn;
}


std::set<Double> MSMetaDataOnDemand::getTimesForScans(
	const std::set<Int>& scans
) {
	std::set<Double> times;
	std::tr1::shared_ptr<std::map<Int, std::set<Double> > > scanToTimesMap = _getScanToTimesMap();
	std::set<Int> scanNumbers = getScanNumbers();
	for (
		std::set<Int>::const_iterator scan=scans.begin();
		scan!=scans.end(); scan++
	) {
		_checkScan(*scan, scanNumbers);
		times.insert(
			scanToTimesMap->find(*scan)->second.begin(),
			scanToTimesMap->find(*scan)->second.end()
		);
	}
	return times;
}

void MSMetaDataOnDemand::_getTimesAndInvervals(
	std::map<Int, vector<Double> >& scanToTimeRangeMap,
	std::map<Int, std::map<uInt, Double> >& scanSpwToIntervalMap
) {

	scanToTimeRangeMap = MSMetaData::_getScanToTimeRangeMap(
		scanSpwToIntervalMap,
		*_getScans(), MSMetaData::_getTimeCentroids(*_ms), _getIntervals(*_ms),
		*_getDataDescIDs(), _getDataDescIDToSpwMap(), getScanNumbers()
	);
	uInt mysize = scanToTimeRangeMap.size()*(sizeof(Int)+2*sizeof(Double));
	if (_cacheUpdated(mysize)) {
		_scanToTimeRangeMap = scanToTimeRangeMap;
	}
	mysize = scanSpwToIntervalMap.size() * nSpw(True) * sizeof(Double);
	if (_cacheUpdated(mysize)) {
		_scanSpwToIntervalMap = scanSpwToIntervalMap;
	}
}

vector<Double> MSMetaDataOnDemand::getTimeRangeForScan(Int scan) {
	_checkScan(scan, getScanNumbers());
	if (! _scanToTimeRangeMap.empty()) {
		return _scanToTimeRangeMap[scan];
	}
	std::map<Int, vector<Double> > scanToTimeRangeMap;
	std::map<Int, std::map<uInt, Double> > scanSpwToIntervalMap;
	_getTimesAndInvervals(
		scanToTimeRangeMap,
		scanSpwToIntervalMap
	);
	return scanToTimeRangeMap[scan];
}

std::map<uInt, Double> MSMetaDataOnDemand::getAverageIntervalsForScan(Int scan) {
	_checkScan(scan, getScanNumbers());
	if (! _scanSpwToIntervalMap.empty()) {
		return _scanSpwToIntervalMap[scan];
	}
	std::map<Int, vector<Double> > scanToTimeRangeMap;
	std::map<Int, std::map<uInt, Double> > scanSpwToIntervalMap;
	_getTimesAndInvervals(
		scanToTimeRangeMap,
		scanSpwToIntervalMap
	);
	return scanSpwToIntervalMap[scan];

}

std::set<Int> MSMetaDataOnDemand::getStatesForScan(const Int scan) {
	_checkScan(scan, getScanNumbers());
	return _getScanToStatesMap().find(scan)->second;
}

std::set<Int> MSMetaDataOnDemand::getScansForIntent(const String& intent) {
	std::set<String> uniqueIntents = getIntents();
	if (uniqueIntents.find(intent) == uniqueIntents.end()) {
		ostringstream oss;
		oss << "MSMetaDataOnDemand::" << __FUNCTION__ << ": Intent " << intent
			<< " is not present in this dataset";
		throw AipsError(oss.str());
	}
	std::map<Int, std::set<String> > scanToIntentsMap;
	std::map<String, std::set<Int> > intentToScansMap;
	_getScansAndIntentsMaps(
		scanToIntentsMap,
		intentToScansMap
	);
	return intentToScansMap[intent];
}

void MSMetaDataOnDemand::_getFieldsAndScansMaps(
	std::map<Int, std::set<Int> >& fieldToScansMap,
	std::map<Int, std::set<Int> >& scanToFieldsMap
) {
	// This method is responsible for setting _fieldToScansMap and _scanToFieldsMap
	if (! _fieldToScansMap.empty() && ! _scanToFieldsMap.empty()) {
		fieldToScansMap = _fieldToScansMap;
		scanToFieldsMap = _scanToFieldsMap;
		return;
	}
	fieldToScansMap.clear();
	scanToFieldsMap.clear();
	std::set<Int> scans;
	std::tr1::shared_ptr<Vector<Int> > fieldIds = _getFieldIDs();
	Vector<Int>::const_iterator curFieldID = fieldIds->begin();
	Vector<Int>::const_iterator end = fieldIds->end();
	std::tr1::shared_ptr<Vector<Int> > allScans = _getScans();
	Vector<Int>::const_iterator curScan = allScans->begin();
	while (curFieldID != end) {
		fieldToScansMap[*curFieldID].insert(*curScan);
		scanToFieldsMap[*curScan].insert(*curFieldID);
		curFieldID++;
		curScan++;
	}
	if (_cacheUpdated(_sizeof(fieldToScansMap) + _sizeof(scanToFieldsMap))) {
		_fieldToScansMap = fieldToScansMap;
		_scanToFieldsMap = scanToFieldsMap;
	}
}

std::set<Int> MSMetaDataOnDemand::getScansForFieldID(const Int fieldID) {
	if (! _hasFieldID(fieldID)) {
		return std::set<Int>();
	}
	std::map<Int, std::set<Int> > fieldToScansMap;
	std::map<Int, std::set<Int> > scanToFieldsMap;
	_getFieldsAndScansMaps(
		fieldToScansMap,  scanToFieldsMap
	);
	return fieldToScansMap[fieldID];
}

std::set<Int> MSMetaDataOnDemand::getFieldIDsForField(
	const String& field
) {
	std::set<Int> fieldIDs;
	String name = field;
	vector<String> fieldNames = _getFieldNames();
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

std::set<Int> MSMetaDataOnDemand::getFieldsForScan(const Int scan) {
	_checkScan(scan, getScanNumbers());
	std::map<Int, std::set<Int> > fieldToScansMap;
	std::map<Int, std::set<Int> > scanToFieldsMap;
	_getFieldsAndScansMaps(
		fieldToScansMap,  scanToFieldsMap
	);
	return scanToFieldsMap[scan];
}

std::set<Int> MSMetaDataOnDemand::getFieldsForScans(const std::set<Int>& scans) {
	_checkScan(*(++scans.rend()), getScanNumbers());
	std::set<Int>::const_iterator end = scans.end();
	std::set<Int> fields;
	for (
		std::set<Int>::const_iterator iter=scans.begin();
		iter!=end; iter++
	) {
		std::set<Int> myfields = getFieldsForScan(*iter);
		fields.insert(myfields.begin(), myfields.end());
	}
	return fields;
}

std::set<Int> MSMetaDataOnDemand::getFieldsForIntent(const String& intent) {
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

std::map<String, std::set<Int> > MSMetaDataOnDemand::getIntentToFieldsMap() {
	vector<std::set<String> > fieldToIntentsMap;
	std::map<String, std::set<Int> > intentToFieldsMap;
	_getFieldsAndIntentsMaps(
		fieldToIntentsMap, intentToFieldsMap
	);
	return intentToFieldsMap;
}

std::map<String, std::set<Int> > MSMetaDataOnDemand::getIntentToScansMap() {
	std::map<Int, std::set<String> > scanToIntentsMap;
	std::map<String, std::set<Int> > intentToScansMap;
	_getScansAndIntentsMaps(
		scanToIntentsMap,
		intentToScansMap
	);
	return intentToScansMap;
}

std::map<String, std::set<uInt> > MSMetaDataOnDemand::getIntentToSpwsMap() {
	vector<std::set<String> > spwToIntentsMap;
	std::map<String, std::set<uInt> > intentToSpwsMap;
	_getSpwsAndIntentsMaps(
		spwToIntentsMap,
		intentToSpwsMap
	);
	return intentToSpwsMap;
}


Bool MSMetaDataOnDemand::_hasIntent(const String& intent) {
	std::set<String> uniqueIntents = getIntents();
	return uniqueIntents.find(intent) != uniqueIntents.end();
}

vector<String> MSMetaDataOnDemand::getFieldNamesForFieldIDs(
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
		os << "MSMetaDataOnDemand::" << __FUNCTION__ << ": This MS only has "
			<< nField << " fields so requested field number " << max
			<< " does not exist";
		throw AipsError(os.str());
	}
	vector<String> allNames = _getFieldNames();
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

std::set<Int> MSMetaDataOnDemand::getFieldsForTimes(
	const Double center, const Double tol
) {
	_checkTolerance(tol);
	Double minTime = center - tol;
	Double maxTime = center + tol;
	std::tr1::shared_ptr<std::map<Int, std::set<Double> > > fieldToTimesMap;
	std::tr1::shared_ptr<std::map<Double, std::set<Int> > > timeToFieldsMap;
	_getFieldsAndTimesMaps(
		fieldToTimesMap, timeToFieldsMap
	);
	std::set<Int> fields;
	std::map<Double, std::set<Int> >::const_iterator end = timeToFieldsMap->end();
	// A std::set is always ordered.
	// FIXME could do a binary search to make this faster
	for (
		std::map<Double, std::set<Int> >::const_iterator iter=timeToFieldsMap->begin();
		iter!=end; iter++
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

void MSMetaDataOnDemand::_getFieldsAndTimesMaps(
		std::tr1::shared_ptr<std::map<Int, std::set<Double> > >& fieldToTimesMap,
		std::tr1::shared_ptr<std::map<Double, std::set<Int> > >& timeToFieldsMap
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
	std::tr1::shared_ptr<Vector<Int> > allFields = _getFieldIDs();
	std::tr1::shared_ptr<Vector<Double> > allTimes = this->_getTimes();
	Vector<Int>::const_iterator lastField = allFields->end();
	Vector<Double>::const_iterator curTime = allTimes->begin();
	for (
		Vector<Int>::const_iterator curField=allFields->begin();
		curField!=lastField; curField++, curTime++
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

std::set<Double> MSMetaDataOnDemand::getTimesForField(const Int fieldID) {
	if (! _hasFieldID(fieldID)) {
		return std::set<Double>();
	}
	std::tr1::shared_ptr<std::map<Int, std::set<Double> > > fieldToTimesMap;
	std::tr1::shared_ptr<std::map<Double, std::set<Int> > > timeToFieldsMap;
	_getFieldsAndTimesMaps(
		fieldToTimesMap, timeToFieldsMap
	);
	return (*fieldToTimesMap)[fieldID];
}

vector<String> MSMetaDataOnDemand::getObservatoryNames() {
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
		iter!=end; iter++
	) {
		mysize += iter->size();
	}
	if (_cacheUpdated(mysize)) {
		_observatoryNames = names;
	}
	return names;
}

MPosition MSMetaDataOnDemand::getObservatoryPosition(uInt which) {
	if (which >= _ms->observation().nrow()) {
		throw AipsError(_ORIGIN + " out of range exception.");
	}
	if (! _observatoryPositions.empty()) {
		return _observatoryPositions[which];
	}

	vector<String> names;
	vector<MPosition> positions = _getObservatoryPositions(names, *_ms);
	if (_cacheUpdated(30*positions.size())) {
		_observatoryPositions = positions;
	}
	return positions[which];
}

vector<MPosition> MSMetaDataOnDemand::getAntennaPositions(
	const vector<uInt>& which
) {
	vector<String> antNames;
	vector<MPosition> output, allAnts;
	if (which.size() == 0) {
		if (_antennaPositions.empty()) {
			output = _getAntennaPositions(antNames, *_ms);
			allAnts = output;
		}
		else {
			return _antennaPositions;
		}
	}
	else if (max(Vector<uInt>(which)) >= nAntennas()) {
		throw AipsError(_ORIGIN + "out of range exception.");
	}
	else {
		allAnts = _antennaPositions.empty()
			? _getAntennaPositions(antNames, *_ms)
			: _antennaPositions;
		vector<uInt>::const_iterator end = which.end();
		for (
			vector<uInt>::const_iterator iter=which.begin();
			iter!=end; iter++
		) {
			output.push_back(allAnts[*iter]);
		}
	}
	if (_antennaPositions.empty() && _cacheUpdated(30*allAnts.size())) {
		_antennaPositions = allAnts;
	}
	return output;
}

vector<MPosition> MSMetaDataOnDemand::getAntennaPositions(
	const vector<String>& names
) {
	if (names.size() == 0) {
		throw AipsError(_ORIGIN + "names cannot be empty");
	}
	return getAntennaPositions(getAntennaIDs(names));
}

Quantum<Vector<Double> > MSMetaDataOnDemand::getAntennaOffset(uInt which) {
	if (which >= nAntennas()) {
		throw AipsError(_ORIGIN + "Out of range exception.");
	}
	return getAntennaOffsets()[which];
}

vector<Quantum<Vector<Double> > > MSMetaDataOnDemand::getAntennaOffsets(
	const vector<MPosition>& positions
) {
	if (! _antennaOffsets.empty()) {
		return _antennaOffsets;
	}
	uInt nPos = positions.size();
	vector<String> names;
	vector<String> obsNames;
	vector<Quantum<Vector<Double> > > offsets;
	if (nPos > 0 && nPos != nAntennas()) {
		throw AipsError(_ORIGIN + "Incorrect number of positions provided.");
	}
	offsets = (nPos > 0)
		? _getAntennaOffsets(
			positions, _getObservatoryPositions(obsNames, *_ms)[0]
		)
		: _getAntennaOffsets(
			_getAntennaPositions(names, *_ms),
			_getObservatoryPositions(obsNames, *_ms)[0]
		);
	if (_cacheUpdated(30*offsets.size())) {
		_antennaOffsets = offsets;
	}
	return offsets;
}

Matrix<Bool> MSMetaDataOnDemand::getUniqueBaselines() {
	if (! _uniqueBaselines.empty()) {
		return _uniqueBaselines;
	}
	std::tr1::shared_ptr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);
	Matrix<Bool> uBaselines = _getUniqueBaselines(*ant1, *ant2);
	if (_cacheUpdated(sizeof(Bool)*uBaselines.size())) {
		_uniqueBaselines = uBaselines;
	}
	return uBaselines;
}

Quantum<Vector<Double> > MSMetaDataOnDemand::getAntennaOffset(
	const String& name
) {
	vector<String> names(1);
	names[0] = name;
	return getAntennaOffset(getAntennaIDs(names)[0]);
}

Quantity MSMetaDataOnDemand::getEffectiveTotalExposureTime() {
	// This method has the responsibility of setting _exposureTime.
	if (_exposureTime.getValue() > 0) {
		return _exposureTime;
	}
	std::tr1::shared_ptr<Vector<Double> > times = _getTimes();
	std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
	vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
	std::map<Int, uInt> dataDescIDToSpwMap = _getDataDescIDToSpwMap();
	std::map<Double, Double> timeToBWMap = _getTimeToTotalBWMap(
		*times, *_getDataDescIDs(), dataDescIDToSpwMap, spwInfo
	);
	Quantity eTime = MSMetaData::_getTotalExposureTime(
		*_ms, timeToBWMap, spwInfo, dataDescIDToSpwMap
	);
	if (_cacheUpdated(10)) {
		_exposureTime = eTime;
	}
	return eTime;
}

void MSMetaDataOnDemand::_getUnflaggedRowStats(
	Double& nACRows, Double& nXCRows,
	std::tr1::shared_ptr<AOSFMapD>& scanNACRows,
	std::tr1::shared_ptr<AOSFMapD>& scanNXCRows,
	std::tr1::shared_ptr<std::map<Int, Double> >& fieldNACRows,
	std::tr1::shared_ptr<std::map<Int, Double> >& fieldNXCRows
) {
	// This method is responsible for setting _nUnflaggedACRows, _nUnflaggedXCRows,
	// _unflaggedFieldNACRows, _unflaggedFieldNXCRows, _unflaggedScanNACRows,
	// _unflaggedScanNXCRows
	if (_unflaggedFieldNACRows && ! _unflaggedFieldNACRows->empty()) {
		nACRows = _nUnflaggedACRows;
		nXCRows = _nUnflaggedXCRows;
		fieldNACRows = _unflaggedFieldNACRows;
		fieldNXCRows = _unflaggedFieldNXCRows;
		scanNACRows = _unflaggedScanNACRows;
		scanNXCRows = _unflaggedScanNXCRows;
		return;
	}
	AOSFMapD *myScanNACRows, *myScanNXCRows;
	std::map<Int, Double> *myFieldNACRows, *myFieldNXCRows;

	std::tr1::shared_ptr<Vector<Int> > ant1, ant2;
	_getAntennas(ant1, ant2);
	std::set<uInt> a, b, c, d, e;
	MSMetaData::_getUnflaggedRowStats(
		nACRows, nXCRows, myFieldNACRows,
		myFieldNXCRows, myScanNACRows, myScanNXCRows, *ant1,
		*ant2, /*_getFlagRows(*_ms),*/ *_getDataDescIDs(),
		_getDataDescIDToSpwMap(),
		_getSpwInfo(a, b, c, d, e), *MSMetaData::_getFlags(*_ms), *_getFieldIDs(),
		*_getScans(), *_getObservationIDs(), *_getArrayIDs()
	);
	fieldNACRows.reset(myFieldNACRows);
	fieldNXCRows.reset(myFieldNXCRows);
	scanNACRows.reset(myScanNACRows);
	scanNXCRows.reset(myScanNXCRows);

	uInt mysize = fieldNACRows->size() + fieldNXCRows->size()
		+ scanNACRows->size() + scanNXCRows->size();
	mysize *= sizeof(Double);
	if (_cacheUpdated(mysize)) {
		_nUnflaggedACRows = nACRows;
		_nUnflaggedXCRows = nXCRows;
		_unflaggedFieldNACRows = fieldNACRows;
		_unflaggedFieldNXCRows = fieldNXCRows;
		_unflaggedScanNACRows = scanNACRows;
		_unflaggedScanNXCRows = scanNXCRows;
	}
}

void MSMetaDataOnDemand::_getSpwsAndIntentsMaps(
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
		iter!=end; iter++
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
	std::tr1::shared_ptr<Vector<Int> > dataDescIDs = _getDataDescIDs();
	Vector<Int>::const_iterator curDDID = dataDescIDs->begin();
	Vector<Int>::const_iterator endDDID = dataDescIDs->end();
	std::tr1::shared_ptr<Vector<Int> > states = _getStateIDs();
	Vector<Int>::const_iterator curState = states->begin();
	std::map<Int, uInt> dataDescToSpwMap = _getDataDescIDToSpwMap();
	while (curDDID!=endDDID) {
		uInt spw = dataDescToSpwMap[*curDDID];
		std::set<String> intents = stateToIntentsMap[*curState];
		std::set<String>::const_iterator beginIntent = intents.begin();
		std::set<String>::const_iterator endIntent = intents.end();
		spwToIntentsMap[spw].insert(beginIntent, endIntent);
		std::set<String>::const_iterator curIntent = beginIntent;
		while (curIntent != endIntent) {
			intentToSpwsMap[*curIntent].insert(spw);
			curIntent++;
		}
		curDDID++;
		curState++;
	}
	if (_cacheUpdated(_sizeof(spwToIntentsMap) + _sizeof(intentToSpwsMap))) {
		_spwToIntentsMap = spwToIntentsMap;
		_intentToSpwsMap = intentToSpwsMap;
	}
}

vector<std::set<String> > MSMetaDataOnDemand::_getSpwToIntentsMap() {
	vector<std::set<String> > spwToIntentsMap;
	std::map<String, std::set<uInt> > intentToSpwsMap;
	_getSpwsAndIntentsMaps(
		spwToIntentsMap, intentToSpwsMap
	);
	return spwToIntentsMap;
}

void MSMetaDataOnDemand::_getFieldsAndStatesMaps(
	std::map<Int, std::set<Int> >& fieldToStatesMap,
	std::map<Int, std::set<Int> >& stateToFieldsMap
) {
	// This method is responsible for setting _fieldToStatesMap and _stateToFieldMap.
	if (! _fieldToStatesMap.empty() && ! _stateToFieldsMap.empty()) {
		fieldToStatesMap = _fieldToStatesMap;
		stateToFieldsMap = _stateToFieldsMap;
		return;
	}
	std::tr1::shared_ptr<Vector<Int> > allStates = _getStateIDs();
	std::tr1::shared_ptr<Vector<Int> > allFields = _getFieldIDs();
	Vector<Int>::const_iterator endState = allStates->end();
	Vector<Int>::const_iterator curField = allFields->begin();
	fieldToStatesMap.clear();
	stateToFieldsMap.clear();
	for (
		Vector<Int>::const_iterator curState=allStates->begin();
		curState!=endState; curState++, curField++
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

void MSMetaDataOnDemand::_getFieldsAndIntentsMaps(
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
		iter!=end; iter++
	) {
		Int state = iter->first;
		std::set<Int> fields = iter->second;
		std::set<String> intents = stateToIntentsMap[state];
		std::set<Int>::const_iterator endField = fields.end();
		for (
			std::set<Int>::const_iterator curField=fields.begin();
			curField!=endField; curField++
		) {
			fieldToIntentsMap[*curField].insert(intents.begin(), intents.end());
		}
		std::set<String>::const_iterator endIntent = intents.end();
		for (
			std::set<String>::const_iterator curIntent=intents.begin();
			curIntent!=endIntent; curIntent++
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

std::map<std::pair<uInt, uInt>, Int> MSMetaDataOnDemand::getSpwIDPolIDToDataDescIDMap() {
	if (! _spwPolIDToDataDescIDMap.empty()) {
		return _spwPolIDToDataDescIDMap;
	}
	std::map<std::pair<uInt, uInt>, Int> spwPolIDToDataDescIDMap = MSMetaData::_getSpwIDPolIDToDataDescIDMap(
		_getDataDescIDToSpwMap(),
		_getDataDescIDToPolIDMap()
	);
	uInt mysize = 2*sizeof(Int)*spwPolIDToDataDescIDMap.size();
	if (_cacheUpdated(mysize)) {
		_spwPolIDToDataDescIDMap = spwPolIDToDataDescIDMap;
	}
	return spwPolIDToDataDescIDMap;
}

std::map<Int, uInt> MSMetaDataOnDemand::_getDataDescIDToSpwMap() {
	if (! _dataDescIDToSpwMap.empty()) {
		return _dataDescIDToSpwMap;
	}
	std::map<Int, uInt> dataDescToSpwMap = MSMetaData::_getDataDescIDToSpwMap(*_ms);
	uInt mysize = sizeof(Int) * dataDescToSpwMap.size();
	if (_cacheUpdated(mysize)) {
		_dataDescIDToSpwMap = dataDescToSpwMap;
	}
	return dataDescToSpwMap;
}

std::map<Int, uInt> MSMetaDataOnDemand::_getDataDescIDToPolIDMap() {
	if (! _dataDescIDToPolIDMap.empty()) {
		return _dataDescIDToPolIDMap;
	}
	std::map<Int, uInt> dataDescToPolIDMap = MSMetaData::_getDataDescIDToPolIDMap(*_ms);
	uInt mysize = sizeof(Int) * dataDescToPolIDMap.size();
	if (_cacheUpdated(mysize)) {
		_dataDescIDToPolIDMap = dataDescToPolIDMap;
	}
	return dataDescToPolIDMap;
}

vector<MSMetaData::SpwProperties> MSMetaDataOnDemand::_getSpwInfo(
	std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw,
	std::set<uInt>& fdmSpw, std::set<uInt>& wvrSpw,
	std::set<uInt>& sqldSpw
) {
	if (_spwInfoStored) {
		avgSpw = _avgSpw;
		tdmSpw = _tdmSpw;
		fdmSpw = _fdmSpw;
		wvrSpw = _wvrSpw;
		sqldSpw = _sqldSpw;
		return _spwInfo;
	}
	vector<SpwProperties> spwInfo = MSMetaData::_getSpwInfo(
		avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw, *_ms
	);
	uInt mysize = sizeof(uInt)*(
			avgSpw.size() + tdmSpw.size() + fdmSpw.size()
			+ wvrSpw.size() + sqldSpw.size()
		) + 2*sizeof(Int)*spwInfo.size()
		+ 2*sizeof(Double)*spwInfo.size();
	vector<SpwProperties>::const_iterator end = spwInfo.end();
	for (
		vector<SpwProperties>::const_iterator iter=spwInfo.begin();
		iter!=end; iter++
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

void MSMetaDataOnDemand::_checkScan(const Int scan, const std::set<Int> uniqueScans) {
	if (uniqueScans.find(scan) == uniqueScans.end()) {
		throw AipsError(
			_ORIGIN + "Unknown scan number " + String::toString(scan)
		);
	}
}

Bool MSMetaDataOnDemand::_hasFieldID(const Int fieldID) {
	if (fieldID >= (Int)nFields()) {
		throw AipsError(
			_ORIGIN + "Requested field ID "
			+ String::toString(fieldID)
			+ " is greater than or equal to the number of records ("
			+ String::toString(nFields())
		    + ") in this MS's FIELD table"
		);
	}
	std::set<Int> uniqueFields = _getUniqueFiedIDs();
	return uniqueFields.find(fieldID) != uniqueFields.end();
}

std::set<Int> MSMetaDataOnDemand::_getUniqueFiedIDs() {
	if (_uniqueFieldIDs.empty()) {
		std::tr1::shared_ptr<Vector<Int> > allFieldIDs = _getFieldIDs();
		_uniqueFieldIDs.insert(allFieldIDs->begin(), allFieldIDs->end());
	}
	return _uniqueFieldIDs;
}

Bool MSMetaDataOnDemand::_hasStateID(const Int stateID) {
	// This method is responsible for setting _uniqueStateIDs
	if (stateID >= (Int)nStates()) {
		throw AipsError(
			_ORIGIN + "Requested state ID "
			+ String::toString(stateID)
			+ " is greater than or equal to the number of records ("
			+ String::toString(nStates())
			+ ") in this MS's STATE table"
		);
	}
	if (_uniqueStateIDs.empty()) {
		std::tr1::shared_ptr<Vector<Int> > allStateIDs = _getStateIDs();
		_uniqueStateIDs.insert(allStateIDs->begin(), allStateIDs->end());
	}
	return _uniqueStateIDs.find(stateID) != _uniqueStateIDs.end();

}

void MSMetaDataOnDemand::_hasAntennaID(Int antennaID) {
	ThrowIf(
		antennaID >= (Int)nAntennas(),
		_ORIGIN + "Requested antenna ID "
		+ String::toString(antennaID)
		+ " is greater than or equal to the number of records ("
		+ String::toString(nAntennas())
		+ ") in this MS's ANTENNA table"
	);
}


}

