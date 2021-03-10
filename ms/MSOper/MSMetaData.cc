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
//# $Id: MSMetaData.cc 21590 2015-03-26 19:30:16Z gervandiepen $

#include <casacore/ms/MSOper/MSMetaData.h>

#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/ms/MSOper/MSKeys.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/scimath/StatsFramework/ClassicalStatistics.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/casa/Containers/ValueHolder.h>

#include <utility>

#define _ORIGIN "MSMetaData::" + String(__func__) + ": "

namespace casacore {

MSMetaData::MSMetaData(const MeasurementSet *const &ms, const Float maxCacheSizeMB)
    : _ms(ms), _showProgress(False), _cacheMB(0), _maxCacheMB(maxCacheSizeMB),
      _nACRows(0), _nXCRows(0), _nStates(0), _nSpw(0), _nFields(0),
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
      _uniqueIntents(),
      _uniqueFieldIDs(), _uniqueStateIDs(),
      _avgSpw(), _tdmSpw(),
      _fdmSpw(), _wvrSpw(), _sqldSpw(),
      _subScanToNACRowsMap(), _subScanToNXCRowsMap(),
      _fieldToNACRowsMap(), _fieldToNXCRowsMap(),
      _scanToIntentsMap(), _stateToIntentsMap(),
      _spwToIntentsMap(),
      _spwInfo(0),
      _spwToFieldIDsMap(), _obsToArraysMap(), _spwToScansMap(),
      _fieldToScansMap(),
      _fieldNames(0),
      _antennaNames(0), _observatoryNames(0),
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
      ),
       _spwInfoStored(False), _forceSubScanPropsToCache(False),
       _sourceTimes() {}

MSMetaData::~MSMetaData() {}

uInt MSMetaData::nStates() const {
    if (_nStates == 0) {
        _nStates = _ms->state().nrow();
                // Allow an empty STATE table.
                if (_nStates == 0) {
                       _nStates = 1;
                }
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
    ScalarColumn<String> intentsCol(_ms->state(), intentsColName);
    Vector<String> intentSets = intentsCol.getColumn();
        // Allow an empty STATE table.
        if (intentSets.empty()) {
                intentSets.reference (Vector<String>(1, String()));
        }
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

vector<std::pair<Quantity, Quantity> > MSMetaData::getProperMotions() const {
    // this method is responsible for setting _properMotions
    if (! _properMotions.empty()) {
        return _properMotions;
    }
    String colName = MSSource::columnName(MSSource::PROPER_MOTION);
    ArrayQuantColumn<Double> col(_ms->source(), colName);
    rownr_t nrow = _ms->source().nrow();
    vector<std::pair<Quantity, Quantity> > myvec(nrow);
    Vector<Quantity> av(2);
    for (rownr_t i=0; i<nrow; ++i) {
        col.get(i, av, False);
        myvec[i].first = av[0];
        myvec[i].second = av[1];
    }
    if (_cacheUpdated(sizeof(myvec))) {
        _properMotions = myvec;
    }
    return myvec;
}

std::set<Int> MSMetaData::getScanNumbers(Int obsID, Int arrayID) const {
    ArrayKey arrayKey;
    arrayKey.obsID = obsID;
    arrayKey.arrayID = arrayID;
    return _getScanNumbers(arrayKey);
}

uInt MSMetaData::nScans() {
    if (_nScans == 0) {
        _nScans = getScanKeys().size();
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

rownr_t MSMetaData::nRows() const {
    return _ms->nrow();
}

rownr_t MSMetaData::nRows(CorrelationType cType) {
    if (cType == BOTH) {
        return nRows();
    }
    rownr_t nACRows, nXCRows;
    std::shared_ptr<std::map<SubScanKey, rownr_t> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<map<Int, rownr_t> > fieldToNACRowsMap, fieldToNXCRowsMap;
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

std::shared_ptr<const map<SubScanKey, rownr_t> > MSMetaData::getNRowMap(CorrelationType cType) const {
    rownr_t nACRows, nXCRows;
    std::shared_ptr<map<SubScanKey, rownr_t> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<map<Int, rownr_t> > fieldToNACRowsMap, fieldToNXCRowsMap;
    _getRowStats(
        nACRows, nXCRows, subScanToNACRowsMap,
        subScanToNXCRowsMap, fieldToNACRowsMap,
        fieldToNXCRowsMap
    );
    if (cType == AUTO) {
        return subScanToNACRowsMap;
    }
    else if (cType == CROSS) {
        return subScanToNXCRowsMap;
    }
    std::shared_ptr<map<SubScanKey, rownr_t> > mymap(
        new map<SubScanKey, rownr_t>()
    );
    map<SubScanKey, rownr_t>::const_iterator iter = subScanToNACRowsMap->begin();
    map<SubScanKey, rownr_t>::const_iterator end = subScanToNACRowsMap->end();
    for (; iter!=end; ++iter) {
        SubScanKey key = iter->first;
        (*mymap)[key] = iter->second + (*subScanToNXCRowsMap)[key];
    }
    return mymap;
}

rownr_t MSMetaData::nRows(
    CorrelationType cType, Int arrayID, Int observationID,
    Int scanNumber, Int fieldID
) const {
    SubScanKey subScanKey;
    subScanKey.obsID = observationID;
    subScanKey.arrayID = arrayID;
    subScanKey.scan = scanNumber;
    subScanKey.fieldID = fieldID;
    _checkSubScan(subScanKey);
    rownr_t nACRows, nXCRows;
    std::shared_ptr<map<SubScanKey, rownr_t> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<map<Int, rownr_t> > fieldToNACRowsMap, fieldToNXCRowsMap;
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

rownr_t MSMetaData::nRows(CorrelationType cType, uInt fieldID) const {
    _checkField(fieldID);
    rownr_t nACRows, nXCRows;
    std::shared_ptr<map<SubScanKey, rownr_t> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<map<Int, rownr_t> > fieldToNACRowsMap, fieldToNXCRowsMap;
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
    std::shared_ptr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
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
    std::shared_ptr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
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
    std::shared_ptr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
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
    std::shared_ptr<std::map<SubScanKey, Double> > subScanToNACRowsMap, subScanToNXCRowsMap;
    std::shared_ptr<vector<Double> > fieldToNACRowsMap, fieldToNXCRowsMap;
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
    rownr_t& nACRows, rownr_t& nXCRows,
    std::map<SubScanKey, rownr_t>*& subScanToNACRowsMap,
    std::map<SubScanKey, rownr_t>*& subScanToNXCRowsMap,
    map<Int, rownr_t>*& fieldToNACRowsMap,
    map<Int, rownr_t>*& fieldToNXCRowsMap
) const {
    nACRows = 0;
    nXCRows = 0;
    subScanToNACRowsMap = new std::map<SubScanKey, rownr_t>();
    subScanToNXCRowsMap = new std::map<SubScanKey, rownr_t>();
    fieldToNACRowsMap = new map<Int, rownr_t>();
    fieldToNXCRowsMap = new map<Int, rownr_t>();
    std::set<SubScanKey> subScanKeys = _getSubScanKeys();
    std::set<SubScanKey>::const_iterator subIter = subScanKeys.begin();
    std::set<SubScanKey>::const_iterator subEnd = subScanKeys.end();
    for (; subIter != subEnd; ++subIter) {
        // initialize subscan map
        (*subScanToNACRowsMap)[*subIter] = 0;
        (*subScanToNXCRowsMap)[*subIter] = 0;
    }
    std::set<Int> fieldIDs = getUniqueFieldIDs();
    std::set<Int>::const_iterator fIter = fieldIDs.begin();
    std::set<Int>::const_iterator fEnd = fieldIDs.end();
    for (; fIter!=fEnd; ++fIter) {
        // initialize field maps
        (*fieldToNACRowsMap)[*fIter] = 0;
        (*fieldToNXCRowsMap)[*fIter] = 0;
    }
    if (_subScanProperties) {
        map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
        map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
        for (; iter!=end; ++iter) {
            const SubScanKey& subscan = iter->first;
            const SubScanProperties& props = iter->second;
            const Int& fieldID = subscan.fieldID;
            nACRows += props.acRows;
            (*subScanToNACRowsMap)[subscan] += props.acRows;
            (*subScanToNXCRowsMap)[subscan] += props.xcRows;
            (*fieldToNACRowsMap)[fieldID] += props.acRows;
            (*fieldToNXCRowsMap)[fieldID] += props.xcRows;
        }
        // doing this single computation is more effecient than summing
        // the xcRows in the loop
        nXCRows = nACRows - nRows();
    }
    else {
        std::shared_ptr<Vector<Int> > ant1, ant2;
        _getAntennas(ant1, ant2);
        std::shared_ptr<Vector<Int> > scans = _getScans();
        std::shared_ptr<Vector<Int> > fieldIDs = _getFieldIDs();
        std::shared_ptr<Vector<Int> > obsIDs = _getObservationIDs();
        std::shared_ptr<Vector<Int> > arrIDs = _getArrayIDs();
        Vector<Int>::const_iterator aEnd = ant1->end();
        Vector<Int>::const_iterator a1Iter = ant1->begin();
        Vector<Int>::const_iterator a2Iter = ant2->begin();
        Vector<Int>::const_iterator sIter = scans->begin();
        Vector<Int>::const_iterator fIter = fieldIDs->begin();
        Vector<Int>::const_iterator oIter = obsIDs->begin();
        Vector<Int>::const_iterator arIter = arrIDs->begin();
        SubScanKey subScanKey;
        for (
            ; a1Iter!=aEnd;
            ++a1Iter, ++a2Iter, ++sIter, ++fIter, ++arIter, ++oIter
        ) {
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
        }
    }
}

void MSMetaData::_getRowStats(
    rownr_t& nACRows, rownr_t& nXCRows,
    std::shared_ptr<std::map<SubScanKey, rownr_t> >& scanToNACRowsMap,
    std::shared_ptr<std::map<SubScanKey, rownr_t> >& scanToNXCRowsMap,
    std::shared_ptr<map<Int, rownr_t> >& fieldToNACRowsMap,
    std::shared_ptr<map<Int, rownr_t> >& fieldToNXCRowsMap
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

    std::map<SubScanKey, rownr_t> *myScanToNACRowsMap, *myScanToNXCRowsMap;
    map<Int, rownr_t> *myFieldToNACRowsMap, *myFieldToNXCRowsMap;
    _getRowStats(
        nACRows, nXCRows, myScanToNACRowsMap,
        myScanToNXCRowsMap, myFieldToNACRowsMap,
        myFieldToNXCRowsMap
    );
    scanToNACRowsMap.reset(myScanToNACRowsMap);
    scanToNXCRowsMap.reset(myScanToNXCRowsMap);
    fieldToNACRowsMap.reset(myFieldToNACRowsMap);
    fieldToNXCRowsMap.reset(myFieldToNXCRowsMap);
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
    std::shared_ptr<Vector<Int> >& ant1,
    std::shared_ptr<Vector<Int> >& ant2
) const {
    ant1 = _getMainScalarColumn<Int>(MSMainEnums::ANTENNA1);
    ant2 = _getMainScalarColumn<Int>(MSMainEnums::ANTENNA2);
}

std::shared_ptr<Vector<Int> > MSMetaData::_getScans() const {
    return _getMainScalarColumn<Int>(
        MSMainEnums::SCAN_NUMBER
    );
}

std::shared_ptr<Vector<Int> > MSMetaData::_getObservationIDs() const {
    return _getMainScalarColumn<Int>(
        MSMainEnums::OBSERVATION_ID
    );
}

std::shared_ptr<Vector<Int> > MSMetaData::_getArrayIDs() const {
    return _getMainScalarColumn<Int>(
        MSMainEnums::ARRAY_ID
    );
}

std::shared_ptr<Vector<Int> > MSMetaData::_getFieldIDs() const {
    return _getMainScalarColumn<Int>(
        MSMainEnums::FIELD_ID
    );
}

std::shared_ptr<Vector<Int> > MSMetaData::_getStateIDs() const {
    std::shared_ptr<Vector<Int> > states = _getMainScalarColumn<Int>(
        MSMainEnums::STATE_ID
    );
    Int maxState = max(*states);
    Int nstates = (Int)nStates();
    ThrowIf(
        maxState >= nstates,
        "MS only has " + String::toString(nstates)
        + " rows in its STATE table, but references STATE_ID "
        + String::toString(maxState) + " in its main table."
    );
    return states;
}

std::shared_ptr<Vector<Int> > MSMetaData::_getDataDescIDs() const {
    return _getMainScalarColumn<Int>(
        MSMainEnums::DATA_DESC_ID
    );
}

std::set<Int> MSMetaData::getScansForState(
    Int stateID, Int obsID, Int arrayID
) const {
    if (! _hasStateID(stateID)) {
        return std::set<Int>();
    }
    std::map<ScanKey, std::set<Int> > myScanToStatesMap = getScanToStatesMap();
    ArrayKey arrayKey;
    arrayKey.obsID = obsID;
    arrayKey.arrayID = arrayID;
    std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
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

std::map<ScanKey, std::set<Int> > MSMetaData::_getScanToAntennasMap() const {
    // this method is responsible for setting _scanToAntennasMap
    if (! _scanToAntennasMap.empty()) {
        return _scanToAntennasMap;
    }
    std::map<ScanKey, std::set<Int> > myScanToAntsMap;
    map<SubScanKey, SubScanProperties> subScanProps = *getSubScanProperties();
    map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps.begin();
    map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps.end();
    while (iter != end) {
        SubScanKey subKey = iter->first;
        SubScanProperties subProps = iter->second;
        myScanToAntsMap[scanKey(subKey)].insert(subProps.antennas.begin(), subProps.antennas.end());
        ++iter;
    }
    std::map<ScanKey, std::set<Int> >::const_iterator end1 = myScanToAntsMap.end();
    uInt mySize = sizeof(ScanKey)*myScanToAntsMap.size();
    for (
        std::map<ScanKey, std::set<Int> >::const_iterator iter=myScanToAntsMap.begin();
        iter!=end1; ++iter
    ) {
        mySize += sizeof(Int)*iter->second.size();
    }
    if (_cacheUpdated(mySize)) {
        _scanToAntennasMap = myScanToAntsMap;
    }
    return myScanToAntsMap;
}

std::map<ScanKey, std::set<Int> > MSMetaData::getScanToStatesMap() const {
    if (! _scanToStatesMap.empty()) {
        return _scanToStatesMap;
    }
    std::map<ScanKey, std::set<Int> > myScanToStatesMap;
    if (nStates() == 0) {
        std::set<Int> empty;
        std::set<ScanKey> scanKeys = getScanKeys();
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
        map<SubScanKey, SubScanProperties> subScanProps = *getSubScanProperties();
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

void MSMetaData::_getSubScansAndIntentsMaps(
    std::shared_ptr<const map<SubScanKey, std::set<String> > >& subScanToIntentsMap,
    map<String, std::set<SubScanKey> >& intentToSubScansMap
) const {
    // This method is responsible for setting _subScanToIntentsMap and _intentToSubScansMap
    if (_subScanToIntentsMap && ! _intentToSubScansMap.empty()) {
        subScanToIntentsMap = _subScanToIntentsMap;
        intentToSubScansMap = _intentToSubScansMap;
        return;
    }
    std::shared_ptr<map<SubScanKey, std::set<String> > > ssToIntents(
        new map<SubScanKey, std::set<String> >()
    );
    intentToSubScansMap.clear();
    if (_ms->state().nrow() == 0) {
        // because the decision was made to support MSes with non-existent STATE tables
        std::set<SubScanKey> sskeys = _getSubScanKeys();
        std::set<SubScanKey>::const_iterator ssiter = sskeys.begin();
        std::set<SubScanKey>::const_iterator ssend = sskeys.end();
        for (; ssiter!=ssend; ++ssiter) {
            (*ssToIntents)[*ssiter] = std::set<String>();
        }
    }
    else {
        vector<std::set<String> > stateToIntentsMap;
        std::set<String> uniqueIntents;
        _getStateToIntentsMap(stateToIntentsMap, uniqueIntents);
        map<SubScanKey, MSMetaData::SubScanProperties> props = *getSubScanProperties();
        map<SubScanKey, MSMetaData::SubScanProperties>::const_iterator iter = props.begin();
        map<SubScanKey, MSMetaData::SubScanProperties>::const_iterator end = props.end();
        for (; iter!=end; ++iter) {
            SubScanKey sskey = iter->first;
            std::set<Int> stateIDs = iter->second.stateIDs;
            std::set<Int>::const_iterator siter = stateIDs.begin();
            std::set<Int>::const_iterator send = stateIDs.end();
            for (; siter!=send; ++siter) {
                std::set<String> intents = stateToIntentsMap[*siter];
                (*ssToIntents)[sskey].insert(intents.begin(), intents.end());
                std::set<String>::const_iterator initer = intents.begin();
                std::set<String>::const_iterator inend = intents.end();
                for (; initer!=inend; ++initer) {
                    intentToSubScansMap[*initer].insert(sskey);
                }
            }
        }
    }
    subScanToIntentsMap = ssToIntents;
    if (_cacheUpdated(_sizeof(*subScanToIntentsMap) + _sizeof(intentToSubScansMap))) {
        _subScanToIntentsMap = subScanToIntentsMap;
        _intentToSubScansMap = intentToSubScansMap;
    }
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
    std::map<ScanKey, std::set<Int> > scanToStatesMap = getScanToStatesMap();
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
        if (*myState < 0) {
            continue;
        }
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

uInt MSMetaData::_sizeof(const vector<vector<String> >& v) {
    vector<vector<String> >::const_iterator iter = v.begin();
    vector<vector<String> >::const_iterator end = v.end();
    uInt size = 0;
    while (iter != end) {
        size += _sizeof(*iter);
        ++iter;
    }
    return size;
}

uInt MSMetaData::_sizeof(const Quantum<Vector<Double> >& m) {
    return (sizeof(Double))*m.getValue().size() + m.getUnit().size();
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

std::set<String> MSMetaData::getIntentsForSubScan(
    const SubScanKey& subScan
) const {
    _checkSubScan(subScan);
    std::shared_ptr<const std::map<SubScanKey, std::set<String> > > subScanToIntentsMap;
    std::map<String, std::set<SubScanKey> > intentToSubScansMap;
    _getSubScansAndIntentsMaps(
        subScanToIntentsMap,
        intentToSubScansMap
    );
    return subScanToIntentsMap->find(subScan)->second;
}

std::shared_ptr<const std::map<SubScanKey, std::set<String> > > MSMetaData::getSubScanToIntentsMap() const {
    std::shared_ptr<const std::map<SubScanKey, std::set<String> > > subScanToIntentsMap;
    std::map<String, std::set<SubScanKey> > intentToSubScansMap;
    _getSubScansAndIntentsMaps(
        subScanToIntentsMap,
        intentToSubScansMap
    );
    return subScanToIntentsMap;
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
    vector<std::set<String>> fieldToIntentsMap;
    std::map<String, std::set<Int>> intentToFieldsMap;
    _getFieldsAndIntentsMaps(fieldToIntentsMap, intentToFieldsMap);
    return fieldToIntentsMap[fieldID];
}

std::set<String> MSMetaData::getIntentsForField(String field) {
    vector<std::set<String>> fieldToIntentsMap;
    std::map<String, std::set<Int>> intentToFieldsMap;
    _getFieldsAndIntentsMaps(fieldToIntentsMap, intentToFieldsMap);
    const auto fieldNames = getFieldNames();
    std::set<String> intents;
    uInt i = 0;
    for (const auto& name: fieldNames) {
        if (name == field) {
            const auto myIntents = fieldToIntentsMap[i];
            intents.insert(myIntents.begin(), myIntents.end());
        }
        ++i;
    }
    return intents;
}

uInt MSMetaData::nFields() const {
    if (_nFields > 0) {
        return _nFields;
    }
    uInt nFields = _ms->field().nrow();
    _nFields = nFields;
    return nFields;
}

std::shared_ptr<std::set<Int> > MSMetaData::_getEphemFieldIDs() const {
    // responsible for setting _ephemFields
    if (_ephemFields) {
        return _ephemFields;
    }
    MSFieldColumns msfc(_ms->field());
    ScalarColumn<Int> ephemCol = msfc.ephemerisId();
    _ephemFields.reset(new std::set<Int>());
    if (ephemCol.isNull()) {
        return _ephemFields;
    }
    Vector<Int> colData = ephemCol.getColumn();
    Vector<Int>::const_iterator iter = colData.begin();
    Vector<Int>::const_iterator end = colData.end();
    uInt i = 0;
    for (; iter!=end; ++iter, ++i) {
        if (*iter >= 0) {
            _ephemFields->insert(i);
        }
    }
    return _ephemFields;
}

MDirection MSMetaData::phaseDirFromFieldIDAndTime(const uInt fieldID,  const MEpoch& ep) const {
    _hasFieldID(fieldID);
    MSFieldColumns msfc(_ms->field());
    if(! msfc.needInterTime(fieldID)) {
        return msfc.phaseDirMeas(fieldID, 0.0);
    }
    MEpoch::Types msType = MEpoch::castType(msfc.timeMeas()(fieldID).getRef().getType());
    Unit sec("s");
    Double inSeconds= MEpoch::Convert(ep, msType)().get(sec).getValue();
    return msfc.phaseDirMeas(fieldID, inSeconds);
} 

MDirection MSMetaData::getReferenceDirection(
    const uInt fieldID,  const MEpoch& ep
) const {
    _hasFieldID(fieldID);
    MSFieldColumns msfc(_ms->field());
    if(! msfc.needInterTime(fieldID)) {
        return msfc.referenceDirMeas(fieldID, 0.0);
    }
    MEpoch::Types msType = MEpoch::castType(msfc.timeMeas()(fieldID).getRef().getType());
    Unit sec("s");
    Double inSeconds = MEpoch::Convert(ep, msType)().get(sec).getValue();
    return msfc.referenceDirMeas(fieldID, inSeconds);
}

void MSMetaData::_getFieldsAndSpwMaps(
    std::map<Int, std::set<uInt> >& fieldToSpwMap,
    vector<std::set<Int> >& spwToFieldMap
) const {
    // This method has the responsibility of setting _fieldToSpwMap and _spwToFieldIDMap
    if (! _fieldToSpwMap.empty() && ! _spwToFieldIDsMap.empty()) {
        fieldToSpwMap = _fieldToSpwMap;
        spwToFieldMap = _spwToFieldIDsMap;
        return;
    }
    fieldToSpwMap.clear();
    spwToFieldMap.resize(nSpw(True));
    std::shared_ptr<const std::map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const std::map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(scanProps, subScanProps, _showProgress);
    std::map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps->begin();
    std::map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps->end();
    for (; iter!=end; ++iter) {
        Int fieldID = iter->first.fieldID;
        const std::set<uInt>& spws = iter->second.spws;
        fieldToSpwMap[fieldID].insert(spws.begin(), spws.end());
        std::set<uInt>::const_iterator spwIter = spws.begin();
        std::set<uInt>::const_iterator spwEnd = spws.end();
        for (; spwIter!=spwEnd; ++spwIter) {
            spwToFieldMap[*spwIter].insert(fieldID);
        }
    }
    if (_cacheUpdated(_sizeof(fieldToSpwMap) + _sizeof(spwToFieldMap))) {
        _fieldToSpwMap = fieldToSpwMap;
        _spwToFieldIDsMap = spwToFieldMap;
    }
}

std::map<Int, std::set<uInt> > MSMetaData::getFieldsToSpwsMap() const {
    std::map<Int, std::set<uInt> > myFieldToSpwMap;
    vector<std::set<Int> > mySpwToFieldMap;
    _getFieldsAndSpwMaps(myFieldToSpwMap, mySpwToFieldMap);
    return myFieldToSpwMap;
}

std::set<uInt> MSMetaData::getSpwsForField(Int fieldID) const {
    if (! _hasFieldID(fieldID)) {
        return std::set<uInt>();
    }
    return getFieldsToSpwsMap()[fieldID];
}

std::set<uInt> MSMetaData::getSpwsForField(const String& fieldName) {
    uInt myNFields = nFields();
    vector<String> fieldNames = getFieldNames();
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

vector<String> MSMetaData::getFieldNames() const {
    if (! _fieldNames.empty()) {
        return _fieldNames;
    }

    String fieldNameColName = MSField::columnName(MSFieldEnums::NAME);
    ScalarColumn<String> nameCol(_ms->field(), fieldNameColName);
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

vector<String> MSMetaData::getFieldCodes() const {
    // this method is responsible for setting _fieldCodes
    if (! _fieldCodes.empty()) {
        return _fieldCodes;
    }
    String fieldCodeColName = MSField::columnName(MSFieldEnums::CODE);
    ScalarColumn<String> codeCol(_ms->field(), fieldCodeColName);
    vector<String> fieldCodes = codeCol.getColumn().tovector();
    if (_cacheUpdated(_sizeof(fieldCodes))) {
        _fieldCodes = fieldCodes;
    }
    return fieldCodes;
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
    vector<String> allFieldNames = getFieldNames();
    for (
        std::set<Int>::const_iterator fieldID = fieldIDs.begin();
        fieldID!=fieldIDs.end(); ++fieldID
    ) {
        fieldNames.insert(allFieldNames[*fieldID]);
    }
    return fieldNames;
}

std::shared_ptr<const map<ScanKey, MSMetaData::ScanProperties> >
MSMetaData::_generateScanPropsIfWanted() const {
    if (_scanProperties) {
        // we already have it, just return it
        return _scanProperties;
    }
    if (_forceSubScanPropsToCache) {
        // it hasn't been generated yet, but we will likely
        // need it later, so just generate it now
        std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps;
        std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps;
        _getScanAndSubScanProperties(scanProps, subScanProps, False);
        return scanProps;
    }
    // we don't have it, and we aren't going to want it later
    return std::shared_ptr<const map<ScanKey, ScanProperties> >();
}


std::shared_ptr<const map<SubScanKey, MSMetaData::SubScanProperties> >
MSMetaData::_generateSubScanPropsIfWanted() const {
    if (_subScanProperties) {
        // we already have it, just return it
        return _subScanProperties;
    }
    if (_forceSubScanPropsToCache) {
        // it hasn't been generated yet, but we will likely
        // need it later, so just generate it now
        std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps;
        std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps;
        _getScanAndSubScanProperties(scanProps, subScanProps, False);
        return subScanProps;
    }
    // we don't have it, and we aren't going to want it later
    return std::shared_ptr<const map<SubScanKey, SubScanProperties> >();
}
 
std::set<ScanKey> MSMetaData::getScanKeys() const {
    if (! _scanKeys.empty()) {
        return _scanKeys;
    }
    std::set<ScanKey> scanKeys;
    std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps = _generateScanPropsIfWanted();
    if (scanProps) {
        map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
        map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
        for (; iter!=end; ++iter) {
            scanKeys.insert(iter->first);
        }
    }
    else {
        // fastest way if we don't have _scanProperties and aren't going to need it later
        std::set<SubScanKey> subScanKeys = _getSubScanKeys();
        std::set<SubScanKey>::const_iterator iter = subScanKeys.begin();
        std::set<SubScanKey>::const_iterator end = subScanKeys.end();
        for (; iter!=end; ++iter) {
            scanKeys.insert(scanKey(*iter));
        }
    }
    if (_cacheUpdated(sizeof(ScanKey)*scanKeys.size())) {
        _scanKeys = scanKeys;
    }
    return scanKeys;
}

std::set<ScanKey> MSMetaData::getScanKeys(const ArrayKey& arrayKey) const {
    std::set<ScanKey> allScanKeys = getScanKeys();
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
    return scanNumbers(getScanKeys(arrayKey));
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
    std::map<SubScanKey, SubScanProperties> subScanProps = *getSubScanProperties();
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

std::map<ScanKey, std::set<uInt> > MSMetaData::getScanToSpwsMap() const {
    std::map<ScanKey, std::set<uInt> > scanToSpwMap;
    vector<std::set<ScanKey> > spwToScanMap;
    _getScansAndSpwMaps(scanToSpwMap, spwToScanMap);
    return scanToSpwMap;
}

vector<std::set<ScanKey> > MSMetaData::getSpwToScansMap() const {
    std::map<ScanKey, std::set<uInt> > scanToSpwMap;
    vector<std::set<ScanKey> > spwToScanMap;
    _getScansAndSpwMaps(scanToSpwMap, spwToScanMap);
    return spwToScanMap;
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
    std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps
        = _generateSubScanPropsIfWanted();
    if (subScanProps) {
        map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps->begin();
        map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps->end();
        for (; iter!=end; ++iter) {
            ScanKey sk = scanKey(iter->first);
            std::set<uInt> spws = iter->second.spws;
            scanToSpwMap[sk].insert(spws.begin(), spws.end());
            std::set<uInt>::const_iterator spwIter = spws.begin();
            std::set<uInt>::const_iterator spwEnd = spws.end();
            for (; spwIter!=spwEnd; ++spwIter) {
                spwToScanMap[*spwIter].insert(sk);
            }
        }
    }
    else {
        // fastest way to generate what we want if we don't have _subScanProperties
        std::map<ScanKey, std::set<uInt> > scanToDDIDMap;
        vector<std::set<ScanKey> > ddIDToScanMap;
        _getScansAndDDIDMaps(scanToDDIDMap, ddIDToScanMap);
        vector<uInt> ddToSpw = getDataDescIDToSpwMap();
        std::map<ScanKey, std::set<uInt> >::const_iterator iter = scanToDDIDMap.begin();
        std::map<ScanKey, std::set<uInt> >::const_iterator end = scanToDDIDMap.end();
        std::set<uInt>::const_iterator dIter;
        std::set<uInt>::const_iterator dEnd;
        for (; iter!=end; ++iter) {
            ScanKey scanKey = iter->first;
            std::set<uInt> ddids = scanToDDIDMap[scanKey];
            dIter = ddids.begin();
            dEnd = ddids.end();
            for (; dIter!=dEnd; ++dIter) {
                uInt spw = ddToSpw[*dIter];
                scanToSpwMap[scanKey].insert(spw);
                spwToScanMap[spw].insert(scanKey);
            }
        }
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

std::set<Int> MSMetaData::getAntennasForScan(const ScanKey& scan) const {
    _checkScan(scan);
    return _getScanToAntennasMap()[scan];
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

std::set<uInt> MSMetaData::getSpwsForSubScan(const SubScanKey& subScan) const {
    return getSubScanProperties(subScan).spws;
}

std::set<Int> MSMetaData::getScansForSpw(
    const uInt spw, Int obsID, Int arrayID
) const {
    uInt myNSpw = nSpw(True);
    ThrowIf(spw >= myNSpw, "spectral window out of range");
    ArrayKey arrayKey;
    arrayKey.obsID = obsID;
    arrayKey.arrayID = arrayID;
    std::set<ScanKey> myScanKeys = getScanKeys(arrayKey);
    std::map<ScanKey, std::set<uInt> > scanToSpwMap;
    vector<std::set<ScanKey> > spwToScanMap;
    _getScansAndSpwMaps(scanToSpwMap, spwToScanMap);
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
    std::map<String, std::set<uInt> >& namesToIDsMap
) const {
    if (! _antennaNames.empty()) {
        namesToIDsMap = _antennaNameToIDMap;
        return _antennaNames;
    }
    namesToIDsMap.clear();
    std::map<String, uInt> mymap;
    String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
    ScalarColumn<String> nameCol(_ms->antenna(), antNameColName);
    Vector<String> names = nameCol.getColumn();
    Vector<String>::const_iterator end = names.end();
    uInt i = 0;
    for (
        Vector<String>::const_iterator name=names.begin();
        name!=end; ++name, ++i
    ) {
        namesToIDsMap[*name].insert(i);
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
) const {
    namesToIDsMap.clear();
    std::map<String, std::set<uInt> > allMap;
    vector<String> names = getAntennaNames(allMap, antennaIDs);
    std::map<String, std::set<uInt> >::const_iterator iter = allMap.begin();
    std::map<String, std::set<uInt> >::const_iterator end = allMap.end();
    for (; iter!=end; ++iter) {
        namesToIDsMap[iter->first] = *iter->second.rbegin();
    }
    return names;
}

vector<String> MSMetaData::getAntennaNames(
    std::map<String, std::set<uInt> >& namesToIDsMap,
    const vector<uInt>& antennaIDs
) const {
    uInt nAnts = nAntennas();
    std::map<String, std::set<uInt> > allMap;
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
        namesToIDsMap[antName].insert(*id);
    }
    return names;
}

uInt MSMetaData::getAntennaID(const String& antennaName) const {
    return *getAntennaIDs(antennaName).rbegin();
}

std::set<uInt> MSMetaData::getAntennaIDs(
    const String& antennaName
) const {
    return getAntennaIDs(vector<String>(1, antennaName))[0];
}

vector<std::set<uInt> > MSMetaData::getAntennaIDs(
    const vector<String>& antennaNames
) const {
    std::map<String, std::set<uInt> > namesToIDsMap;
    vector<String> names = getAntennaNames(namesToIDsMap);
    vector<String>::const_iterator end = antennaNames.end();
    std::map<String, std::set<uInt> >::const_iterator mapEnd = namesToIDsMap.end();
    vector<std::set<uInt> > ids;
    for (
        vector<String>::const_iterator name=antennaNames.begin();
        name!=end; ++name
    ) {
        std::map<String, std::set<uInt> >::const_iterator pair = namesToIDsMap.find(*name);
        ThrowIf(
            pair == mapEnd, _ORIGIN + "Unknown antenna " + *name
        );
        ids.push_back(pair->second);
    }
    return ids;
}

map<ScanKey, MSMetaData::FirstExposureTimeMap> MSMetaData::getScanToFirstExposureTimeMap(
    Bool showProgress
) const {
    std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties> > scanProps
        = _getScanProperties(showProgress);
    std::map<ScanKey, MSMetaData::ScanProperties>::const_iterator iter = scanProps->begin();
    std::map<ScanKey, MSMetaData::ScanProperties>::const_iterator end = scanProps->end();
    map<ScanKey, FirstExposureTimeMap> ret;
    for (; iter!=end; ++iter) {
        ret[iter->first] = iter->second.firstExposureTime;
    }
    return ret;
}

// deprecated, no longer supported
vector<std::map<Int, Quantity> > MSMetaData::getFirstExposureTimeMap() {
    if (! _firstExposureTimeMap.empty()) {
        return _firstExposureTimeMap;
    }
    uInt nDataDescIDs = nDataDescriptions();
    std::shared_ptr<Vector<Int> > scans = _getScans();
    std::shared_ptr<Vector<Int> > dataDescIDs = _getDataDescIDs();
    std::shared_ptr<Vector<Double> > times = _getTimes();
    std::shared_ptr<Quantum<Vector<Double> > > exposureTimes = _getExposureTimes();
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

vector<vector<String> > MSMetaData::getAntennaStations(const vector<String>& antennaNames) {
    vector<std::set<uInt> > ids = getAntennaIDs(antennaNames);
    vector<std::set<uInt> >::const_iterator iter = ids.begin();
    vector<std::set<uInt> >::const_iterator end = ids.end();
    vector<vector<String> > stations;
    for (; iter!=end; ++iter) {
        std::set<uInt>::const_iterator siter = iter->begin();
        std::set<uInt>::const_iterator send = iter->end();
        vector<String> myStations;
        for (; siter!=send; ++siter) {
            myStations.push_back(getAntennaStations(vector<uInt>(1, *siter))[0]);
        }
        stations.push_back(myStations);
    }
    return stations;
}

vector<String> MSMetaData::_getStationNames() {
    if (! _stationNames.empty()) {
        return _stationNames;
    }
    String antStationColName = MSAntenna::columnName(MSAntennaEnums::STATION);
    vector<String> stationNames = ScalarColumn<String>(
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
    if (_subScanProperties) {
        map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
        map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
        for (; iter!=end; ++iter) {
            mysubscans.insert(iter->first);
        }
    }
    else {
        std::shared_ptr<Vector<Int> > scans = _getScans();
        std::shared_ptr<Vector<Int> > fields = _getFieldIDs();
        std::shared_ptr<Vector<Int> > arrays = _getArrayIDs();
        std::shared_ptr<Vector<Int> > obs = _getObservationIDs();
        Vector<Int>::const_iterator scanIter = scans->begin();
        Vector<Int>::const_iterator scanEnd = scans->end();
        Vector<Int>::const_iterator fIter = fields->begin();
        Vector<Int>::const_iterator oIter = obs->begin();
        Vector<Int>::const_iterator aIter = arrays->begin();
        SubScanKey subScanKey;
        for (; scanIter != scanEnd; ++scanIter, ++fIter, ++oIter, ++aIter) {
            subScanKey.obsID = *oIter;
            subScanKey.arrayID = *aIter;
            subScanKey.scan = *scanIter;
            subScanKey.fieldID = *fIter;
            mysubscans.insert(subScanKey);
        }
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

QVD MSMetaData::getAntennaDiameters() const {
    if (! _antennaDiameters.getValue().empty()) {
        return _antennaDiameters;
    }
    String antDiamColName = MSAntenna::columnName(MSAntennaEnums::DISH_DIAMETER);
    ScalarColumn<Double> diamCol(_ms->antenna(), antDiamColName);
    Vector<Double> diams = diamCol.getColumn();
    String unit = *diamCol.keywordSet().asArrayString("QuantumUnits").begin();
    QVD antennaDiameters = QVD(diams, unit);
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

QVD MSMetaData::_freqWidthToVelWidth(
    const QVD& v, const Quantity& refFreq
) {
    QVD dv = v;
    dv.convert("Hz");
    dv = dv/refFreq.getValue("Hz");
    dv.scale(C::c/1000);
    dv.setUnit("km/s");
    return dv;
}

vector<QVD> MSMetaData::getChanEffectiveBWs(Bool asVelWidths) const {
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
        if (
            asVelWidths
            && iter->effbw.isConform("Hz")
            && iter->meanfreq.getValue() > 0
        ) {

            out.push_back(
                _freqWidthToVelWidth(iter->effbw, iter->meanfreq)
            );
        }
        else {
            out.push_back(iter->effbw);
        }
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

vector<QVD>MSMetaData::getChanResolutions(Bool asVelWidths) const {
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
        if (
            asVelWidths
            && iter->resolution.isConform("Hz")
            && iter->meanfreq.getValue() > 0
        ) {
            out.push_back(
                _freqWidthToVelWidth(iter->resolution, iter->meanfreq)
            );
        }
        else {
            out.push_back(iter->resolution);
        }
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

vector<Int> MSMetaData::getNetSidebands() const {
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

vector<Quantity> MSMetaData::getMeanFreqs() const {
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

vector<MFrequency> MSMetaData::getRefFreqs() const {
    std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
    vector<MSMetaData::SpwProperties> props = _getSpwInfo(
        avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw
    );
    vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
    vector<MFrequency> out;
    for (
        vector<MSMetaData::SpwProperties>::const_iterator iter=props.begin();
        iter!=end; ++iter
    ) {
        out.push_back(iter->reffreq);
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

std::set<uInt> MSMetaData::getSpwIDs() const {
    const Vector<Int> ddIDs = *_getDataDescIDs();
    const vector<uInt>& ddIDToSpw = getDataDescIDToSpwMap();
    Vector<Int>::const_iterator iter = ddIDs.begin();
    Vector<Int>::const_iterator end = ddIDs.end();
    std::set<uInt> spws;
    for ( ; iter!=end; ++iter) {
        if (*iter >= 0) {
            spws.insert(ddIDToSpw[*iter]);
        }
    }
    return spws;
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
    ArrayKey arrayKey;
    arrayKey.obsID = obsID;
    arrayKey.arrayID = arrayID;
    std::set<ScanKey> uniqueScans = getScanKeys(arrayKey);
    std::shared_ptr<std::map<ScanKey, std::set<Double> > > scanToTimesMap = _getScanToTimesMap();
    Double minTime = center - tol;
    Double maxTime = center + tol;
    std::set<Int> scans;
    std::set<ScanKey>::const_iterator scan = uniqueScans.begin();
    std::set<ScanKey>::const_iterator end = uniqueScans.end();
    while (scan != end) {
        std::set<Double> times = scanToTimesMap->find(*scan)->second;
        // rbegin() points to the last element in a container. For a std::set,
        // the last element is the largest, and the first is the smallest.
        if (*times.rbegin() >= minTime && *times.begin() <= maxTime) {
            scans.insert(scan->scan);
        }
        ++scan;
    }
    return scans;
}

std::shared_ptr<std::map<ScanKey, std::set<Double> > > MSMetaData::_getScanToTimesMap() const {
    if (_scanToTimesMap && ! _scanToTimesMap->empty()) {
        return _scanToTimesMap;
    }
    std::shared_ptr<Vector<Int> > scans = _getScans();
    std::shared_ptr<Vector<Int> > obsIDs = _getObservationIDs();
    std::shared_ptr<Vector<Int> > arrayIDs = _getArrayIDs();
    Vector<Int>::const_iterator curScan = scans->begin();
    Vector<Int>::const_iterator lastScan = scans->end();
    std::shared_ptr<Vector<Double> > times = _getTimes();
    Vector<Double>::const_iterator curTime = times->begin();
    Vector<Int>::const_iterator curObs = obsIDs->begin();
    Vector<Int>::const_iterator curArray = arrayIDs->begin();
    std::shared_ptr<std::map<ScanKey, std::set<Double> > > scanToTimesMap(
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

template <class T> std::shared_ptr<Vector<T> > MSMetaData::_getMainScalarColumn(
    MSMainEnums::PredefinedColumns col
) const {
    String name = MeasurementSet::columnName(col);
    ScalarColumn<T> mycol(*_ms, name);
    std::shared_ptr<Vector<T> > v(new Vector<T>());
    mycol.getColumn(*v);
    return v;
}

std::shared_ptr<Vector<Double> > MSMetaData::_getTimes() const {
    return _getMainScalarColumn<Double>(MSMainEnums::TIME);
}

std::shared_ptr<Quantum<Vector<Double> > > MSMetaData::_getExposureTimes() const {
    ScalarQuantColumn<Double> col(
        *_ms, MeasurementSet::columnName(MSMainEnums::EXPOSURE)
    );
    return col.getColumn();
}

std::shared_ptr<ArrayColumn<Bool> > MSMetaData::_getFlags() const {
    String flagColName = MeasurementSet::columnName(MSMainEnums::FLAG);
    return std::shared_ptr<ArrayColumn<Bool> >(
        new ArrayColumn<Bool>(*_ms, flagColName)
    );
}

std::set<Double> MSMetaData::getTimesForScans(
    std::set<ScanKey> scans
) const {
    std::set<Double> times;
    if (scans.empty()) {
        std::shared_ptr<Vector<Double> > allTimes = _getTimes();
        times.insert(allTimes->begin(), allTimes->end());
        return times;
    }
    std::shared_ptr<std::map<ScanKey, std::set<Double> > > scanToTimesMap = _getScanToTimesMap();
    // std::set<Int> scanNumbers = getScanNumbers();
    std::set<ScanKey>::const_iterator scan = scans.begin();
    std::set<ScanKey>::const_iterator end = scans.end();
    std::set<ScanKey> scanKeys = getScanKeys();
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

std::map<uInt, std::set<Double> > MSMetaData::getSpwToTimesForScan(
    const ScanKey& scan
) const {
    _checkScan(scan);
    std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(scanProps, subScanProps, False);
    return scanProps->find(scan)->second.times;
}

std::pair<Double, Double> MSMetaData::getTimeRangeForScan(
    const ScanKey& scanKey
) const {
    _checkScan(scanKey);
    std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(scanProps, subScanProps, False);
    return scanProps->find(scanKey)->second.timeRange;
}

std::shared_ptr<const map<ScanKey, pair<Double,Double> > > MSMetaData::getScanToTimeRangeMap() const {
    std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(
        scanProps, subScanProps, False
    );
    std::shared_ptr<map<ScanKey, pair<Double,Double> > > ret(
        new map<ScanKey, pair<Double,Double> >()
    );
    map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
    map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
    for (;iter!=end; ++iter) {
        (*ret)[iter->first] = iter->second.timeRange;
    }
    return ret;
}

pair<Double, Double> MSMetaData::getTimeRange(Bool showProgress) const {
    // can't just use TIME column because that does not take into account
    // the interval
    std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(scanProps, subScanProps, showProgress);
    map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
    map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
    pair<Double, Double> timerange = iter->second.timeRange;
    ++iter;
    for (;iter!=end; ++iter) {
        const pair<Double, Double>& range = iter->second.timeRange;
        timerange.first = min(timerange.first, range.first);
        timerange.second = max(timerange.second, range.second);
    }
    return timerange;
}

map<uInt, Double> MSMetaData::getAverageIntervalsForScan(
    const ScanKey& scan
) const {
    _checkScan(scan);
    std::shared_ptr<const map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(scanProps, subScanProps, False);
    map<uInt, Quantity> meanIntervals = scanProps->find(scan)->second.meanInterval;
    map<uInt, Double> ret;
    map<uInt, Quantity>::const_iterator iter = meanIntervals.begin();
    map<uInt, Quantity>::const_iterator end = meanIntervals.end();
    for (; iter!=end; ++iter) {
        ret[iter->first] = iter->second.getValue();
    }
    return ret;
}

std::map<uInt, Quantity> MSMetaData::getAverageIntervalsForSubScan(
    const SubScanKey& subScan
) const {
    return getSubScanProperties(subScan).meanInterval;
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
    std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
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
    return getFieldsForScans(myScanKeys);
}

std::set<Int> MSMetaData::getFieldsForScans(
    const std::set<ScanKey>& scanKeys
) const {
    _checkScans(scanKeys);
    std::set<ScanKey>::const_iterator iter = scanKeys.begin();
    std::set<ScanKey>::const_iterator end = scanKeys.end();
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
    vector<String> fieldNames = getFieldNames();
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
    std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
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
    return scanNumbers(filteredScans);
}

std::set<Int> MSMetaData::getStatesForScan(
    Int obsID, Int arrayID, Int scan
) const {
    ArrayKey arrayKey;
    arrayKey.obsID = obsID;
    arrayKey.arrayID = arrayID;
    std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
    std::map<ScanKey, std::set<Int> > scanToStates = getScanToStatesMap();
    std::set<Int> states;
    std::set<ScanKey>::const_iterator iter = scanKeys.begin();
    std::set<ScanKey>::const_iterator end = scanKeys.end();
    while (iter != end) {
        if (iter->scan == scan) {
            std::set<Int> myStates = scanToStates[*iter];
            states.insert(myStates.begin(), myStates.end());
        }
        ++iter;
    }
    return states;
}

std::vector<std::set<Double> > MSMetaData::getTimesForSpws(Bool showProgress) const {
    std::shared_ptr<const std::map<ScanKey, ScanProperties> > scanProps
        = this->_getScanProperties(showProgress);
    std::vector<std::set<Double> > myvec(nSpw(True));
    std::map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
    std::map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
    for (; iter!=end; ++iter) {
        const std::map<uInt, std::set<Double> >& times = iter->second.times;
        std::map<uInt, std::set<Double> >::const_iterator titer = times.begin();
        std::map<uInt, std::set<Double> >::const_iterator tend = times.end();
        for (; titer!=tend; ++titer) {
            const std::set<Double>& spwTimes = titer->second;
            myvec[titer->first].insert(spwTimes.begin(), spwTimes.end());
        }
    }
    return myvec;
}

Record MSMetaData::getSummary() const {
    Record spectralTable;
    spectralTable.define("names", Vector<String>(getSpwNames()));
    Record polTable;
    polTable.define("n correlations", Vector<Int>(getNumCorrs()));
    Record dataDescTable;
    vector<uInt> ddToSpw = getDataDescIDToSpwMap();
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
    summary.define("fields", Vector<String>(getFieldNames()));
    vector<std::set<Int> > obsToArraysMap = _getObservationIDToArrayIDsMap();
    vector<std::set<Int> >::const_iterator oIter = obsToArraysMap.begin();
    vector<std::set<Int> >::const_iterator oEnd = obsToArraysMap.end();
    std::map<SubScanKey, SubScanProperties> subScanProps = *getSubScanProperties();
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
    summary.define("nrows", (Int64)nRows());
    std::shared_ptr<Vector<Double> > times = _getTimes();
    summary.define("begin time", min(*times));
    summary.define("end time", max(*times));
    return summary;
}

void MSMetaData::_createScanRecords(
    Record& parent, const ArrayKey& arrayKey,
    const std::map<SubScanKey, SubScanProperties>& subScanProps
) const {
    std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
    std::set<ScanKey>::const_iterator scanIter = scanKeys.begin();
    std::set<ScanKey>::const_iterator scanEnd = scanKeys.end();
    while(scanIter != scanEnd) {
        ScanKey scanKey = *scanIter;
        std::set<Int> antennasForScan;
        rownr_t scanNRows = 0;
        Record scanRec;
        _createSubScanRecords(
            scanRec, scanNRows, antennasForScan,
            scanKey, subScanProps
        );
        scanRec.define("nrows", (Int64)scanNRows);
        scanRec.define("antennas", Vector<Int>(antennasForScan.begin(), antennasForScan.size(), 0));
        parent.defineRecord("scan=" + String::toString(scanKey.scan), scanRec);
        ++scanIter;
    }
}

void MSMetaData::_createSubScanRecords(
    Record& parent, rownr_t& scanNRows, std::set<Int>& antennasForScan,
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
        rownr_t nrows = props.acRows + props.xcRows;
        subScanRec.define("nrows", (Int64)nrows);
        scanNRows += nrows;
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
        timeRec.define("nrows", (Int64)(tpIter->second.nrows));
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
    std::shared_ptr<Vector<Int> > stateIDs = _getStateIDs();
    std::shared_ptr<Vector<Double> > times = _getTimes();
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

vector<std::set<ScanKey> > MSMetaData::getFieldToScansMap() const {
    vector<std::set<ScanKey> > fieldToScansMap;
    std::map<ScanKey, std::set<Int> > scanToFieldsMap;
    _getFieldsAndScansMaps(fieldToScansMap, scanToFieldsMap);
    return fieldToScansMap;
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

vector<Array<Int> > MSMetaData::getCorrProducts() const {
    // responsible for setting _corrProds
    if (! _corrProds.empty()) {
        return _corrProds;
    }
    String colName = MSPolarization::columnName(MSPolarizationEnums::CORR_PRODUCT);
    ArrayColumn<Int> col(_ms->polarization(), colName);
    uInt colSize = col.nrow();
    vector<Array<Int> > contents(colSize);
    for (uInt i=0; i<colSize; ++i) {
        contents[i] = col.get(i);
    }
    if (_cacheUpdated(_sizeof(contents))) {
        _corrProds = contents;
    }
    return contents;
}

vector<vector<Int> > MSMetaData::getCorrTypes() const {
    // responsible for setting _corrTypes
    if (! _corrTypes.empty()) {
        return _corrTypes;
    }
    String colName = MSPolarization::columnName(MSPolarizationEnums::CORR_TYPE);
    ArrayColumn<Int> col(_ms->polarization(), colName);
    uInt colSize = col.nrow();
    vector<vector<Int> > contents(colSize);
    for (uInt i=0; i<colSize; ++i) {
        contents[i] = col.get(i).tovector();
    }
    if (_cacheUpdated(_sizeof(contents))) {
        _corrTypes = contents;
    }
    return contents;
}

vector<Int> MSMetaData::getNumCorrs() const {
    // responsible for setting _numCorrs
    if (! _numCorrs.empty()) {
        return _numCorrs;
    }
    String colName = MSPolarization::columnName(MSPolarization::NUM_CORR);
    ScalarColumn<Int> col(_ms->polarization(), colName);
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
    std::shared_ptr<Vector<Int> > obsIDs = _getObservationIDs();
    std::shared_ptr<Vector<Int> > arrayIDs = _getArrayIDs();
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

vector<int> MSMetaData::getFieldTableSourceIDs() const {
    // this method is responsible for setting _field_sourceIDs
    if (! _field_sourceIDs.empty()) {
        return _field_sourceIDs;
    }
    String colName = MSField::columnName(MSField::SOURCE_ID);
    ScalarColumn<Int> col(_ms->field(), colName);
    vector<Int> myvec = col.getColumn().tovector();
    if (_cacheUpdated(sizeof(myvec))) {
        _field_sourceIDs = myvec;
    }
    return myvec;
}

vector<MDirection> MSMetaData::getSourceDirections() const {
    // this method is responsible for setting _sourceDirs
    if (! _sourceDirs.empty()) {
        return _sourceDirs;
    }
    String colName = MSSource::columnName(MSSource::DIRECTION);
    ScalarMeasColumn<MDirection> col(_ms->source(), colName);
    uInt nrows = _ms->source().nrow();
    vector<MDirection> myvec(nrows);
    MDirection direction;
    for (uInt i=0; i<nrows; ++i) {
        col.get(i, direction);
        myvec[i] = direction;
    }
    if (_cacheUpdated(sizeof(myvec))) {
        _sourceDirs = myvec;
    }
    return myvec;
}

std::shared_ptr<const Quantum<Vector<Double > > > MSMetaData::getSourceTimes() const {
    if (_sourceTimes) {
        return _sourceTimes;
    }
    String colName = MSSource::columnName(MSSource::TIME);
    ScalarQuantColumn<Double> time(_ms->source(), colName);
    std::shared_ptr<const Quantum<Vector<Double> > > col = time.getColumn();
    if (_cacheUpdated(_sizeof(*col))) {
        _sourceTimes = col;
    }
    return col;
}

map<SourceKey, MSMetaData::SourceProperties>
MSMetaData::_getSourceInfo() const {
    // this method is responsible for setting _sourceInfo
    if (! _sourceInfo.empty()) {
        return _sourceInfo;
    }
    auto colName = MSSource::columnName(MSSource::SOURCE_ID);
    ScalarColumn<Int> id(_ms->source(), colName);
    colName = MSSource::columnName(MSSource::SPECTRAL_WINDOW_ID);
    ScalarColumn<Int> spw(_ms->source(), colName);
    colName = MSSource::columnName(MSSource::NAME);
    ScalarColumn<String> name(_ms->source(), colName);
    colName = MSSource::columnName(MSSource::REST_FREQUENCY);
    ArrayMeasColumn<MFrequency> restfreq(_ms->source(), colName);
    colName = MSSource::columnName(MSSource::TRANSITION);
    ArrayColumn<String> transition(_ms->source(), colName);
    map<SourceKey, SourceProperties> mymap;
    uInt nrows = _ms->source().nrow();
    Array<MFrequency> rf;
    SourceKey key;
    SourceProperties props;
    static const Unit emptyUnit;
    static const Unit hz("Hz");
    for (uInt i=0; i<nrows; ++i) {
        key.id = id(i);
        key.spw = spw(i);
        props.name = name(i);
        if (restfreq.isDefined(i)) {
            // resize=True because the array lengths may differ
            // from cell to cell, CAS-10409
            restfreq.get(i, rf, True);
            props.restfreq.reset(
                new std::vector<MFrequency>(rf.tovector())
            );
        }
        else {
            props.restfreq.reset();
        }
        if (transition.isDefined(i)) {
            props.transition.reset(
                new std::vector<String>(transition(i).tovector())
            );
        }
        else {
            props.transition.reset();
        }
        mymap[key] = props;
    }
    ThrowIf(
        mymap.size() < nrows,
        "Too few source keys found, there are duplicate SOURCE table keys"
    );
    // this is a reasonable approximation for now
    auto mysize = nrows*(2*sizeof(uInt) + sizeof(Double) + 30);
    if (_cacheUpdated(mysize)) {
        _sourceInfo = mymap;
    }
    return mymap;
}


map<SourceKey, std::shared_ptr<vector<MFrequency>>>
MSMetaData::getRestFrequencies() const {
    auto mymap = _getSourceInfo();
    map<SourceKey, std::shared_ptr<vector<MFrequency>>> ret;
    for_each(
        mymap.cbegin(), mymap.cend(),
        [&ret] (const std::pair<SourceKey, SourceProperties>& val) {
        ret[val.first] = val.second.restfreq;
    }
    );
    return ret;
}

map<SourceKey, std::shared_ptr<vector<String> > > MSMetaData::getTransitions() const {
    map<SourceKey, SourceProperties> mymap = _getSourceInfo();
    map<SourceKey, std::shared_ptr<vector<String> > > ret;
    map<SourceKey, SourceProperties>::const_iterator iter = mymap.begin();
    map<SourceKey, SourceProperties>::const_iterator end = mymap.end();
    while (iter != end) {
        ret[iter->first] = iter->second.transition;
        ++iter;
    }
    return ret;
}

vector<String> MSMetaData::getSourceNames() const {
    // this method is responsible for setting _sourceNames
    if (! _sourceNames.empty()) {
        return _sourceNames;
    }
    String colName = MSSource::columnName(MSSource::NAME);
    ScalarColumn<String> col(_ms->source(), colName);
    vector<String> myvec = col.getColumn().tovector();
    if (_cacheUpdated(sizeof(myvec))) {
        _sourceNames = myvec;
    }
    return myvec;
}

vector<int> MSMetaData::getSourceTableSourceIDs() const {
    // this method is responsible for setting _source_sourceIDs
    if (! _source_sourceIDs.empty()) {
        return _source_sourceIDs;
    }
    String colName = MSSource::columnName(MSSource::SOURCE_ID);
    ScalarColumn<Int> col(_ms->source(), colName);
    vector<Int> myvec = col.getColumn().tovector();
    if (_cacheUpdated(sizeof(myvec))) {
        _source_sourceIDs = myvec;
    }
    return myvec;
}

uInt MSMetaData::nUniqueSourceIDsFromSourceTable() const {
    String colName = MSSource::columnName(MSSource::SOURCE_ID);
    ScalarColumn<Int> col(_ms->source(), colName);
    Vector<Int> myvec = col.getColumn();
    std::set<Int> myset(myvec.begin(), myvec.end());
    return myset.size();
}

Bool MSMetaData::_hasIntent(const String& intent) const {
    std::set<String> uniqueIntents = getIntents();
    return uniqueIntents.find(intent) != uniqueIntents.end();
}

vector<String> MSMetaData::getFieldNamesForFieldIDs(
    const vector<uInt>& fieldIDs
) {
    if (fieldIDs.size() == 0) {
        return getFieldNames();
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
    vector<String> allNames = getFieldNames();
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
    std::shared_ptr<std::map<Int, std::set<Double> > > fieldToTimesMap;
    std::shared_ptr<std::map<Double, std::set<Int> > > timeToFieldsMap;
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
        std::shared_ptr<std::map<Int, std::set<Double> > >& fieldToTimesMap,
        std::shared_ptr<std::map<Double, std::set<Int> > >& timeToFieldsMap
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
    std::shared_ptr<Vector<Int> > allFields = _getFieldIDs();
    std::shared_ptr<Vector<Double> > allTimes = this->_getTimes();
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
    std::shared_ptr<std::map<Int, std::set<Double> > > fieldToTimesMap;
    std::shared_ptr<std::map<Double, std::set<Int> > > timeToFieldsMap;
    _getFieldsAndTimesMaps(
        fieldToTimesMap, timeToFieldsMap
    );
    return (*fieldToTimesMap)[fieldID];
}

vector<String> MSMetaData::getObservers() const {
    if (! _observers.empty()) {
        return _observers;
    }
    String colName = MSObservation::columnName(MSObservationEnums::OBSERVER);
    ScalarColumn<String> col(_ms->observation(), colName);
    vector<String> contents = col.getColumn().tovector();
    if (_cacheUpdated(_sizeof(contents))) {
        _observers = contents;
    }
    return contents;
}

vector<String> MSMetaData::getObservatoryNames() {
    if (! _observatoryNames.empty()) {
        return _observatoryNames;
    }
    String tnameColName = MSObservation::columnName(MSObservationEnums::TELESCOPE_NAME);
    ScalarColumn<String> telescopeNameCol(_ms->observation(), tnameColName);
    vector<String> names = telescopeNameCol.getColumn().tovector();
    if (_cacheUpdated(_sizeof(names))) {
        _observatoryNames = names;
    }
    return names;
}

vector<String> MSMetaData::getProjects() const {
    if (! _projects.empty()) {
        return _projects;
    }
    String colName = MSObservation::columnName(MSObservationEnums::PROJECT);
    ScalarColumn<String> col(_ms->observation(), colName);
    vector<String> projects = col.getColumn().tovector();
    if (_cacheUpdated(_sizeof(projects))) {
        _projects = projects;
    }
    return projects;
}

vector<vector<String> > MSMetaData::getSchedules() const {
    // responsible for setting _schedules
    if (! _schedules.empty()) {
        return _schedules;
    }
    String colName = MSObservation::columnName(MSObservationEnums::SCHEDULE);
    ArrayColumn<String> col(_ms->observation(), colName);
    uInt colSize = col.nrow();
    vector<vector<String> > contents(colSize);
    for (uInt i=0; i<colSize; ++i) {
        contents[i] = col.get(i).tovector();
    }
    if (_cacheUpdated(_sizeof(contents))) {
        _schedules = contents;
    }
    return contents;
}

vector<std::pair<MEpoch, MEpoch> > MSMetaData::getTimeRangesOfObservations() const {
    if (! _timeRangesForObs.empty()) {
        return _timeRangesForObs;
    }
    String colName = MSObservation::columnName(MSObservationEnums::TIME_RANGE);
    ArrayColumn<Double> col(_ms->observation(), colName);
    TableRecord kv = col.keywordSet();
    String unit = kv.asArrayString("QuantumUnits").tovector()[0];
    MEpoch::Types myRF;
    MEpoch::getType(myRF, kv.asRecord("MEASINFO").asString("Ref"));
    uInt n = col.nrow();
    vector<std::pair<MEpoch, MEpoch> > contents(n);
    for (uInt i=0; i<n; ++i) {
        Vector<Double> row = col.get(i);
        Quantity begin(row[0], unit);
        Quantity end(row[1], unit);
        contents[i] = std::pair<MEpoch, MEpoch>(MEpoch(begin, myRF), MEpoch(end, myRF));
    }
    if (_cacheUpdated(_sizeof(contents))) {
        _timeRangesForObs = contents;
    }
    return contents;
}

MPosition MSMetaData::getObservatoryPosition(uInt which) const {
    if (which >= _ms->observation().nrow()) {
        throw AipsError(_ORIGIN + " out of range exception.");
    }
    if (! _observatoryPositions.empty()) {
        return _observatoryPositions[which];
    }
    String tnameColName = MSObservation::columnName(MSObservationEnums::TELESCOPE_NAME);
    ScalarColumn<String> telescopeNameCol(_ms->observation(), tnameColName);
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

vector<MDirection> MSMetaData::getPhaseDirs(const MEpoch& ep) const {
    // this method is responsible for setting _phaseDirs
    vector<MDirection> myDirs;
    if (_phaseDirs.empty()) {
        String name = MSField::columnName(MSFieldEnums::PHASE_DIR);
        ScalarMeasColumn<MDirection> phaseDirCol(_ms->field(), name);
        uInt nrows = nFields();
        for (uInt i=0; i<nrows; ++i) {
            myDirs.push_back(phaseDirCol(i));
        }
        if (_cacheUpdated(_sizeof(myDirs))) {
            _phaseDirs = myDirs;
        }
    }
    else {
        myDirs = _phaseDirs;
    }
    // get the correct directions for ephemeris objects and put them
    // in the vector
    std::shared_ptr<std::set<Int> > ephems = _getEphemFieldIDs();
    std::set<Int>::const_iterator iter = ephems->begin();
    std::set<Int>::const_iterator end = ephems->end();
    for (; iter!=end; ++iter) {
        myDirs[*iter] = phaseDirFromFieldIDAndTime(*iter, ep);
    }
    return myDirs;
}

vector<MPosition> MSMetaData::_getAntennaPositions() const {
    // This method is responsible for setting _antennaPositions
    if (! _antennaPositions.empty()) {
        return _antennaPositions;
    }
    String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
    ScalarColumn<String> nameCol(_ms->antenna(), antNameColName);
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

vector<MPosition> MSMetaData::getAntennaPositions(
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

vector<vector<MPosition> > MSMetaData::getAntennaPositions(
    const vector<String>& names
) {
    ThrowIf(
        names.empty(), _ORIGIN + "names cannot be empty"
    );
    vector<std::set<uInt> > ids = getAntennaIDs(names);
    vector<std::set<uInt> >::const_iterator iter = ids.begin();
    vector<std::set<uInt> >::const_iterator end = ids.end();
    vector<vector<MPosition> > pos;
    for (; iter!=end; ++iter) {
        std::vector<MPosition> mypos;
        std::set<uInt>::const_iterator siter = iter->begin();
        std::set<uInt>::const_iterator send = iter->end();
        for (; siter!=send; ++siter) {
            mypos.push_back(getAntennaPositions(vector<uInt>(1, *siter))[0]);
        }
        pos.push_back(mypos);
    }
    return pos;
}

QVD MSMetaData::getAntennaOffset(uInt which) const {
    ThrowIf(
        which >= nAntennas(), "Out of range exception."
    );
    return getAntennaOffsets()[which];
}

vector<QVD> MSMetaData::getAntennaOffsets() const {
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
    vector<QVD> antennaOffsets;
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
        QVD qoffset(offset, "m");
        antennaOffsets.push_back(qoffset);
    }
    if (_cacheUpdated(30*antennaOffsets.size())) {
        _antennaOffsets = antennaOffsets;
    }
    return antennaOffsets;
}

uInt MSMetaData::nBaselines(Bool includeAutoCorrelation) {
    Matrix<Bool> baselines = getUniqueBaselines().copy();
    uInt ac = 0;
    uInt nrows = baselines.nrow();
    for (uInt i=0; i<nrows; ++i) {
        if (includeAutoCorrelation && baselines(i, i)) {
            // count autocorrelations separately from cross correlations
            ++ac;
        }
        baselines(i, i) = False;
    }
    return ntrue(baselines)/2 + ac;
}

Matrix<Bool> MSMetaData::getUniqueBaselines() {
    if (! _uniqueBaselines.empty()) {
        return _uniqueBaselines;
    }
    std::shared_ptr<Vector<Int> > ant1, ant2;
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
    if (_cacheUpdated(sizeof(Bool)*baselines.size())) {
        _uniqueBaselines = baselines;
    }
    return baselines;
}

QVD MSMetaData::getAntennaOffset(
    const String& name
) const {
    return getAntennaOffsets(name)[0];
}
    
std::vector<QVD> MSMetaData::getAntennaOffsets(
    const String& name
) const {
    std::set<uInt> ids = getAntennaIDs(name);
    std::vector<QVD> offsets;
    std::set<uInt>::const_iterator iter = ids.begin();
    std::set<uInt>::const_iterator end = ids.end();
    for(; iter!=end; ++iter) {
        offsets.push_back(getAntennaOffset(*iter));
    }
    return offsets;
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
    vector<uInt> dataDescToSpwIdMap = getDataDescIDToSpwMap();
    std::set<uInt> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
    vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
    std::map<Double, Double> timeToBWMap = _getTimeToTotalBWMap(
        times, ddIDs
    );
    for (uInt i=0; i<nrows; ++i) {
        uInt ddID = ddIDs[i];
        uInt spw = dataDescToSpwIdMap[ddID];
        QVD channelWidths = spwInfo[spw].chanwidths;
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

MSMetaData::SubScanProperties MSMetaData::getSubScanProperties(
    const SubScanKey& subScan, Bool showProgress
) const {
    _checkSubScan(subScan);
    return getSubScanProperties(showProgress)->find(subScan)->second;
}

void MSMetaData::_getScalarIntColumn(
    Vector<Int>& v, TableProxy& tp, const String& colname,
    rownr_t beginRow, rownr_t nrows
) {
    v = tp.getColumn(colname, beginRow, nrows, 1).asArrayInt();
}

void MSMetaData::_getScalarDoubleColumn(
    Vector<Double>& v, TableProxy& tp, const String& colname,
    rownr_t beginRow, rownr_t nrows
) {
    v = tp.getColumn(colname, beginRow, nrows, 1).asArrayDouble();
}

void MSMetaData::_getScalarQuantDoubleColumn(
        Quantum<Vector<Double> >& v, TableProxy& tp, const String& colname,
        rownr_t beginRow, rownr_t nrows
) {
    ScalarQuantColumn<Double> mycol(tp.table(), colname);
    v = Quantum<Vector<Double> >(
        tp.getColumn(colname, beginRow, nrows, 1).asArrayDouble(),
        mycol.getUnits()
    );
}

void MSMetaData::_computeScanAndSubScanProperties(
    std::shared_ptr<std::map<ScanKey, MSMetaData::ScanProperties> >& scanProps,
    std::shared_ptr<std::map<SubScanKey, MSMetaData::SubScanProperties> >& subScanProps,
    Bool showProgress
) const {
    std::shared_ptr<ProgressMeter> pm;
    if (showProgress || _showProgress) {
        LogIO log;
        const static String title = "Computing scan and subscan properties...";
        log << LogOrigin("MSMetaData", __func__, WHERE)
            << LogIO::NORMAL << title << LogIO::POST;
        pm.reset(new ProgressMeter(0, _ms->nrow(), title));
    }
    const static String scanName = MeasurementSet::columnName(MSMainEnums::SCAN_NUMBER);
    const static String fieldName = MeasurementSet::columnName(MSMainEnums::FIELD_ID);
    const static String ddidName = MeasurementSet::columnName(MSMainEnums::DATA_DESC_ID);
    const static String stateName = MeasurementSet::columnName(MSMainEnums::STATE_ID);
    const static String timeName = MeasurementSet::columnName(MSMainEnums::TIME);
    const static String arrayName = MeasurementSet::columnName(MSMainEnums::ARRAY_ID);
    const static String obsName = MeasurementSet::columnName(MSMainEnums::OBSERVATION_ID);
    const static String ant1Name = MeasurementSet::columnName(MSMainEnums::ANTENNA1);
    const static String ant2Name = MeasurementSet::columnName(MSMainEnums::ANTENNA2);
    const static String exposureName = MeasurementSet::columnName(MSMainEnums::EXPOSURE);
    const static String intervalName = MeasurementSet::columnName(MSMainEnums::INTERVAL);
    TableProxy tp(*_ms);
    std::vector<
        pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties> >
    >  props;
    std::vector<uInt> ddIDToSpw = getDataDescIDToSpwMap();
    scanProps.reset(
        new std::map<ScanKey, ScanProperties>()
    );
    subScanProps.reset(
        new std::map<SubScanKey, SubScanProperties>()
    );
    rownr_t doneRows = 0;
    rownr_t msRows = _ms->nrow();
    static const rownr_t rowsInChunk = 10000000;
    for (rownr_t row=0; row<msRows; row += rowsInChunk) {
        rownr_t nrows = min(rowsInChunk, msRows - row);
        Vector<Int> scans, fields, ddIDs, states,
            arrays, observations, ant1, ant2;
        _getScalarIntColumn(scans, tp, scanName, row, nrows);
        _getScalarIntColumn(fields, tp, fieldName, row, nrows);
        _getScalarIntColumn(ddIDs, tp, ddidName, row, nrows);
        _getScalarIntColumn(states, tp, stateName, row, nrows);
        _getScalarIntColumn(arrays, tp, arrayName, row, nrows);
        _getScalarIntColumn(observations, tp, obsName, row, nrows);
        _getScalarIntColumn(ant1, tp, ant1Name, row, nrows);
        _getScalarIntColumn(ant2, tp, ant2Name, row, nrows);
        Vector<Double> times;
        _getScalarDoubleColumn(times, tp, timeName, row, nrows);
        Quantum<Vector<Double> > exposureTimes, intervalTimes;
        _getScalarQuantDoubleColumn(
            exposureTimes, tp, exposureName, row, nrows
        );
        _getScalarQuantDoubleColumn(
            intervalTimes, tp, intervalName, row, nrows
        );
        rownr_t nchunks = min((rownr_t)1000, nrows);
        rownr_t chunkSize = nrows/nchunks;
        if (nrows % nchunks > 0) {
            // integer division
            nchunks = nrows/chunkSize + 1;
        }
        pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties> > *fut =
            new pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties> >[nchunks];
#ifdef _OPENMP
#pragma omp parallel for
#endif
        for (uInt i=0; i<nchunks; ++i) {
            fut[i] = _getChunkSubScanProperties(
                scans, fields, ddIDs, states, times, arrays,
                observations, ant1, ant2, exposureTimes,
                intervalTimes,  ddIDToSpw,  i * chunkSize,
                std::min((i + 1) * chunkSize, nrows)
            );
        }
        props.insert(props.begin(), fut, fut+nchunks);
        delete [] fut;
        if (pm) {
            doneRows += nrows;
            pm->update(doneRows);
        }
    }
    _mergeScanProps(scanProps, subScanProps, props);
}

void MSMetaData::_mergeScanProps(
    std::shared_ptr<std::map<ScanKey, MSMetaData::ScanProperties> >& scanProps,
    std::shared_ptr<std::map<SubScanKey, MSMetaData::SubScanProperties> >& subScanProps,
    const std::vector<
        pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties> >
    >&  props
) const {
    scanProps.reset(
        new std::map<ScanKey, ScanProperties>()
    );
    subScanProps.reset(
        new std::map<SubScanKey, SubScanProperties>()
    );
    map<SubScanKey, map<uInt, Quantity> > ssSumInterval;
    uInt nTotChunks = props.size();
    for (uInt i=0; i<nTotChunks; ++i) {
        const map<ScanKey, ScanProperties>& vScanProps = props[i].first;
        map<ScanKey, ScanProperties>::const_iterator viter = vScanProps.begin();
        map<ScanKey, ScanProperties>::const_iterator vend = vScanProps.end();
        for (; viter!=vend; ++viter) {
            // iterate over scans in this chunk
            const ScanKey& scanKey = viter->first;
            const std::pair<Double, Double>& range = viter->second.timeRange;
            if (scanProps->find(scanKey) == scanProps->end()) {
                (*scanProps)[scanKey].timeRange = range;
            }
            else {
                std::pair<Double, Double>& tr = (*scanProps)[scanKey].timeRange;
                tr.first = min(tr.first, range.first);
                tr.second = max(tr.second, range.second);
            }
            std::map<uInt, rownr_t>::const_iterator spnIter = viter->second.spwNRows.begin();
            std::map<uInt, rownr_t>::const_iterator spnEnd = viter->second.spwNRows.end();
            for (; spnIter!=spnEnd; ++spnIter) {
                const uInt& spw = spnIter->first;
                const std::set<Double> scanTimes = viter->second.times.find(spw)->second;
                if ((*scanProps)[scanKey].spwNRows.find(spw) == (*scanProps)[scanKey].spwNRows.end()) {
                    (*scanProps)[scanKey].spwNRows[spw] = spnIter->second;
                    (*scanProps)[scanKey].times[spw] = scanTimes;
                }
                else {
                    (*scanProps)[scanKey].spwNRows[spw] += spnIter->second;
                    (*scanProps)[scanKey].times[spw].insert(scanTimes.begin(), scanTimes.end());
                }
                const std::set<Double> mytimes = vScanProps.find(scanKey)->second.times.find(spw)->second;
                (*scanProps)[scanKey].times[spw].insert(mytimes.begin(), mytimes.end());
            }
        }
        map<SubScanKey, SubScanProperties>::const_iterator ssIter = props[i].second.begin();
        map<SubScanKey, SubScanProperties>::const_iterator ssEnd = props[i].second.end();
        for (; ssIter!=ssEnd; ++ssIter) {
            // iterate over subscans in this chunk
            const SubScanKey& ssKey = ssIter->first;
            const SubScanProperties& val = ssIter->second;
            if (subScanProps->find(ssKey) == subScanProps->end()) {
                // first time this subscan has been seen in any chunks
                (*subScanProps)[ssKey] = val;
                map<uInt, Quantity>::const_iterator mi = val.meanInterval.begin();
                map<uInt, Quantity>::const_iterator me = val.meanInterval.end();
                for (; mi!=me; ++mi) {
                    const uInt& spw = mi->first;
                    ssSumInterval[ssKey][spw] = mi->second*Quantity(val.spwNRows.find(spw)->second);
                }
            }
            else {
                SubScanProperties& fp = (*subScanProps)[ssKey];
                // the rows increment must come before the mean exposure time
                // computation
                fp.acRows += val.acRows;
                fp.xcRows += val.xcRows;
                fp.antennas.insert(val.antennas.begin(), val.antennas.end());
                fp.beginTime = min(fp.beginTime, val.beginTime);
                fp.ddIDs.insert(val.ddIDs.begin(), val.ddIDs.end());
                fp.endTime = max(fp.endTime, val.endTime);
                rownr_t nrows = fp.acRows + fp.xcRows;
                fp.meanExposureTime = (fp.meanExposureTime*Quantity(nrows - 1) + val.meanExposureTime)/nrows;
                fp.stateIDs.insert(val.stateIDs.begin(), val.stateIDs.end());
                fp.spws.insert(val.spws.begin(), val.spws.end());

                std::set<uInt>::const_iterator spwIter = val.spws.begin();
                std::set<uInt>::const_iterator spwEnd = val.spws.end();
                for (; spwIter!=spwEnd; ++spwIter) {
                    const uInt& spw = *spwIter;
                    rownr_t vSpwNRows = val.spwNRows.find(spw)->second;
                    fp.spwNRows[spw] += vSpwNRows;
                    const Quantity& vMeanInt = val.meanInterval.find(spw)->second;
                    if (ssSumInterval[ssKey].find(spw) == ssSumInterval[ssKey].end()) {
                        ssSumInterval[ssKey][spw] = vMeanInt * Quantity(vSpwNRows, "");
                    }
                    else {
                        ssSumInterval[ssKey][spw] += vMeanInt * Quantity(vSpwNRows, "");
                    }
                }
                map<Double, TimeStampProperties>::const_iterator tpIter = val.timeProps.begin();
                map<Double, TimeStampProperties>::const_iterator tpEnd = val.timeProps.end();
                for (; tpIter!=tpEnd; ++tpIter) {
                    Double time = tpIter->first;
                    const TimeStampProperties& tprops = tpIter->second;
                    if (fp.timeProps.find(time) == fp.timeProps.end()) {
                        fp.timeProps[time] = tprops;
                    }
                    else {
                        fp.timeProps[time].ddIDs.insert(tprops.ddIDs.begin(), tprops.ddIDs.end());
                        fp.timeProps[time].nrows += tprops.nrows;
                    }
                }
                _modifyFirstExposureTimeIfNecessary(fp.firstExposureTime, val.firstExposureTime);
            }
        }
    }
    map<ScanKey, map<uInt, Quantity> > scanSumInterval;
    map<SubScanKey, SubScanProperties>::iterator ssIter = subScanProps->begin();
    map<SubScanKey, SubScanProperties>::iterator ssEnd = subScanProps->end();
    for (; ssIter!=ssEnd; ++ssIter) {
        const SubScanKey& ssKey = ssIter->first;
        SubScanProperties& props = ssIter->second;
        map<uInt, rownr_t>::const_iterator ssSpwNRowsIter = props.spwNRows.begin();
        map<uInt, rownr_t>::const_iterator ssSpwNRowsEnd = props.spwNRows.end();
        for (; ssSpwNRowsIter!=ssSpwNRowsEnd; ++ssSpwNRowsIter) {
            const uInt& spw = ssSpwNRowsIter->first;
            props.meanInterval[spw] = ssSumInterval[ssKey][spw]/ssSpwNRowsIter->second;
        }
        const ScanKey scanKey = casacore::scanKey(ssKey);
        if (scanSumInterval.find(scanKey) == scanSumInterval.end()) {
            // first time associated scan key has been seen
            scanSumInterval[scanKey] = ssSumInterval[ssKey];
            (*scanProps)[scanKey].firstExposureTime = props.firstExposureTime;
        }
        else {
            map<uInt, Quantity>::const_iterator spwSumIter = ssSumInterval[ssKey].begin();
            map<uInt, Quantity>::const_iterator spwSumEnd = ssSumInterval[ssKey].end();
            for (; spwSumIter!=spwSumEnd; ++spwSumIter) {
                const uInt& spw = spwSumIter->first;
                if (scanSumInterval[scanKey].find(spw) == scanSumInterval[scanKey].end()) {
                    scanSumInterval[scanKey][spw] = spwSumIter->second;
                }
                else {
                    scanSumInterval[scanKey][spw] += spwSumIter->second;
                }
            }
            _modifyFirstExposureTimeIfNecessary(
                (*scanProps)[scanKey].firstExposureTime, ssIter->second.firstExposureTime
            );
        }
    }
    map<ScanKey, ScanProperties>::iterator scanPropsIter = scanProps->begin();
    map<ScanKey, ScanProperties>::iterator scanPropsEnd = scanProps->end();
    for (; scanPropsIter!=scanPropsEnd; ++scanPropsIter) {
        const ScanKey& scanKey = scanPropsIter->first;
        map<uInt, rownr_t>::const_iterator scanSpwNRowsIter = scanPropsIter->second.spwNRows.begin();
        map<uInt, rownr_t>::const_iterator scanSpwNRowsEnd = scanPropsIter->second.spwNRows.end();
        for (; scanSpwNRowsIter!=scanSpwNRowsEnd; ++scanSpwNRowsIter) {
            const uInt& spw = scanSpwNRowsIter->first;
            const rownr_t& nrows = scanSpwNRowsIter->second;
            scanPropsIter->second.meanInterval[spw] = scanSumInterval[scanKey][spw]/nrows;
        }
    }
}

void MSMetaData::_modifyFirstExposureTimeIfNecessary(
    FirstExposureTimeMap& current, const FirstExposureTimeMap& test
) {
    // deal with first exposure time maps
    FirstExposureTimeMap::const_iterator feiter = test.begin();
    FirstExposureTimeMap::const_iterator feend = test.end();
    for (; feiter!=feend; ++feiter) {
        Int ddID = feiter->first;
        Double timestamp = feiter->second.first;
        // could be implemented in terms of overloaded _modifyFirstExposureTimeIfNecessary,
        // but would require decomposing the exposure quantity into value and unit. Since
        // this is called for every row in the main table, that may impact performance so
        // I've opted to leave it as is.
        if (
            current.find(ddID) == current.end()
            || timestamp < current[ddID].first
        ) {
            // dd ID not yet encountered yet
            // or the first time stamp for this ddID is
            // before what has yet been encountered
            current[ddID] = feiter->second;
        }
    }
}

void MSMetaData::_modifyFirstExposureTimeIfNecessary(
    FirstExposureTimeMap& current, Int dataDescID,
    Double time, Double exposure, const Unit& eunit
) {
    if (current.find(dataDescID) == current.end()) {
        // the data description ID for this sub scan has not yet been
        // encountered in this chunk
        current[dataDescID].first = time;
        current[dataDescID].second = Quantity(exposure, eunit);
    }
    else if (time < current[dataDescID].first) {
        current[dataDescID].first = time;
        // unit is already set from first time, so only need to reset value
        current[dataDescID].second.setValue(exposure);
    }
}

pair<map<ScanKey, MSMetaData::ScanProperties>, map<SubScanKey, MSMetaData::SubScanProperties> >
MSMetaData::_getChunkSubScanProperties(
    const Vector<Int>& scans, const Vector<Int>& fields,
    const Vector<Int>& ddIDs, const Vector<Int>& states,
    const Vector<Double>& times, const Vector<Int>& arrays,
    const Vector<Int>& observations, const Vector<Int>& ant1,
    const Vector<Int>& ant2, const Quantum<Vector<Double> >& exposureTimes,
    const Quantum<Vector<Double> >& intervalTimes, const vector<uInt>& ddIDToSpw, rownr_t beginRow,
    rownr_t endRow
) const {
    VectorSTLIterator<Int> scanIter(scans);
    scanIter += beginRow;
    VectorSTLIterator<Int> a1Iter(ant1);
    a1Iter += beginRow;
    VectorSTLIterator<Int> a2Iter(ant2);
    a2Iter += beginRow;
    VectorSTLIterator<Int> fIter(fields);
    fIter += beginRow;
    VectorSTLIterator<Int> dIter(ddIDs);
    dIter += beginRow;
    VectorSTLIterator<Int> stateIter(states);
    stateIter += beginRow;
    VectorSTLIterator<Int> oIter(observations);
    oIter += beginRow;
    VectorSTLIterator<Int> arIter(arrays);
    arIter += beginRow;
    VectorSTLIterator<Double> tIter(times);
    tIter += beginRow;
    VectorSTLIterator<Double> eiter(exposureTimes.getValue());
    eiter += beginRow;
    VectorSTLIterator<Double> iIter(intervalTimes.getValue());
    iIter += beginRow;
    map<ScanKey, ScanProperties> scanProps;
    map<SubScanKey, SubScanProperties> mysubscans;
    map<SubScanKey, Double> exposureSum;
    map<pair<SubScanKey, uInt>, Double> intervalSum;
    ScanKey scanKey;
    SubScanKey subScanKey;
    pair<SubScanKey, uInt> subScanSpw;
    rownr_t row = beginRow;
    const Unit& eunit = exposureTimes.getFullUnit();
    while (row < endRow) {
        scanKey.obsID = *oIter;
        scanKey.arrayID = *arIter;
        scanKey.scan = *scanIter;
        Double half = *iIter/2;
        uInt spw = ddIDToSpw[*dIter];
        if (scanProps.find(scanKey) == scanProps.end()) {
            // first time this scan has been encountered in this chunk
            scanProps[scanKey].timeRange = std::make_pair(*tIter-half, *tIter+half);
            scanProps[scanKey].times[spw] = std::set<Double>();
            scanProps[scanKey].spwNRows[spw] = 1;
        }
        else {
            pair<Double, Double>& timeRange = scanProps[scanKey].timeRange;
            timeRange.first = min(timeRange.first, *tIter-half);
            timeRange.second = max(timeRange.second, *tIter+half);
            map<uInt, std::set<Double> >& times = scanProps[scanKey].times;
            if (times.find(spw) == times.end()) {
                times[spw] = std::set<Double>();
                scanProps[scanKey].spwNRows[spw] = 1;
            }
            else {
                ++scanProps[scanKey].spwNRows[spw];
            }
        }
        scanProps[scanKey].times[spw].insert(*tIter);
        subScanKey.obsID = *oIter;
        subScanKey.arrayID = *arIter;
        subScanKey.scan = *scanIter;
        subScanKey.fieldID = *fIter;
        Bool autocorr = *a1Iter == *a2Iter;
        if (
            mysubscans.find(subScanKey) == mysubscans.end()
        ) {
            // first time this subscan has been encountered in this chunk
            SubScanProperties props;
            props.acRows = autocorr ? 1 : 0;
            props.xcRows = autocorr ? 0 : 1;
            props.beginTime = *tIter;
            props.endTime = *tIter;
            props.firstExposureTime[*dIter].first = *tIter;
            props.firstExposureTime[*dIter].second = Quantity(*eiter, eunit);
            mysubscans[subScanKey] = props;
            exposureSum[subScanKey] = *eiter;
        }
        else {
            if (autocorr) {
                ++mysubscans[subScanKey].acRows;
            }
            else {
                ++mysubscans[subScanKey].xcRows;
            }
            mysubscans[subScanKey].beginTime = min(*tIter, mysubscans[subScanKey].beginTime);
            mysubscans[subScanKey].endTime = max(*tIter, mysubscans[subScanKey].endTime);
            _modifyFirstExposureTimeIfNecessary(
                mysubscans[subScanKey].firstExposureTime, *dIter,
                *tIter, *eiter, eunit
            );
            exposureSum[subScanKey] += *eiter;
        }
        subScanSpw.first = subScanKey;
        subScanSpw.second = spw;
        if (intervalSum.find(subScanSpw) == intervalSum.end()) {
            intervalSum[subScanSpw] = *iIter;
            mysubscans[subScanKey].spwNRows[spw] = 1;
        }
        else {
            intervalSum[subScanSpw] += *iIter;
            ++mysubscans[subScanKey].spwNRows[spw];
        }
        mysubscans[subScanKey].antennas.insert(*a1Iter);
        mysubscans[subScanKey].antennas.insert(*a2Iter);
        mysubscans[subScanKey].ddIDs.insert(*dIter);
        mysubscans[subScanKey].spws.insert(spw);
        mysubscans[subScanKey].stateIDs.insert(*stateIter);
        std::map<Double, TimeStampProperties>& timeProps = mysubscans[subScanKey].timeProps;
        if (timeProps.find(*tIter) == timeProps.end()) {
            timeProps[*tIter].nrows = 1;
        }
        else {
            ++timeProps[*tIter].nrows;
        }
        timeProps[*tIter].ddIDs.insert(*dIter);
        ++tIter;
        ++scanIter;
        ++fIter;
        ++dIter;
        ++stateIter;
        ++oIter;
        ++arIter;
        ++a1Iter;
        ++a2Iter;
        ++eiter;
        ++iIter;
        ++row;
    }
    map<SubScanKey, SubScanProperties>::iterator ssIter = mysubscans.begin();
    map<SubScanKey, SubScanProperties>::iterator ssEnd = mysubscans.end();
    for (; ssIter!=ssEnd; ++ssIter) {
        SubScanProperties& props = ssIter->second;
        props.meanExposureTime = Quantity(
            exposureSum[ssIter->first]/(props.acRows + props.xcRows), eunit
        );
    }
    const Unit& unit = intervalTimes.getFullUnit();
    map<pair<SubScanKey, uInt>, Double>::const_iterator intSumIter = intervalSum.begin();
    map<pair<SubScanKey, uInt>, Double>::const_iterator intSumEnd = intervalSum.end();
    for (; intSumIter!=intSumEnd; ++intSumIter) {
        const SubScanKey& ssKey = intSumIter->first.first;
        const uInt& spw = intSumIter->first.second;
        const Double& sum = intSumIter->second;
        SubScanProperties& props = mysubscans[ssKey];
        props.meanInterval[spw] = Quantity(sum/props.spwNRows[spw], unit);
    }
    return make_pair(scanProps, mysubscans);
}

std::shared_ptr<const std::map<SubScanKey, MSMetaData::SubScanProperties> > MSMetaData::getSubScanProperties(
    Bool showProgress
) const {
    std::shared_ptr<const std::map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const std::map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(
        scanProps, subScanProps, showProgress
    );
    return subScanProps;
}

std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties> > MSMetaData::_getScanProperties(
    Bool showProgress
) const {
    std::shared_ptr<const std::map<ScanKey, ScanProperties> > scanProps;
    std::shared_ptr<const std::map<SubScanKey, SubScanProperties> > subScanProps;
    _getScanAndSubScanProperties(
        scanProps, subScanProps, showProgress
    );
    return scanProps;
}

void MSMetaData::_getScanAndSubScanProperties(
    std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties> >& scanProps,
    std::shared_ptr<const std::map<SubScanKey, MSMetaData::SubScanProperties> >& subScanProps,
    Bool showProgress
) const {
    // responsible for setting _scanProperties and _subScanProperties
    // a sub scan is defined by a unique combination of scan number and field ID
    if (_scanProperties && _subScanProperties) {
        scanProps = _scanProperties;
        subScanProps = _subScanProperties;
        return;
    }
    std::shared_ptr<std::map<SubScanKey, SubScanProperties> > myssprops;
    std::shared_ptr<std::map<ScanKey, ScanProperties> > myscanprops;
    _computeScanAndSubScanProperties(
        myscanprops, myssprops, showProgress
    );
    scanProps = myscanprops;
    subScanProps = myssprops;

    static const uInt iSize = sizeof(Int);
    static const uInt dSize = sizeof(Double);

    static const uInt scanStructSize = 2*dSize;
    static const uInt scanKeySize = 3*iSize;
    // fudge of sizeof(Unit)
    static const uInt unitSize = 16;
    static const uInt feSize = iSize + 2*dSize + unitSize;
    uInt scanSize = scanProps->size() * (scanKeySize + scanStructSize);
    std::map<ScanKey, ScanProperties>::const_iterator scanIter = scanProps->begin();
    std::map<ScanKey, ScanProperties>::const_iterator scanEnd = scanProps->end();
    for (; scanIter!=scanEnd; ++scanIter) {
        const ScanProperties& props = scanIter->second;
        scanSize += props.meanInterval.size() * (dSize + 4*iSize);
        const map<uInt, std::set<Double> >& spwTimes = props.times;
        map<uInt, std::set<Double> >::const_iterator tIter = spwTimes.begin();
        map<uInt, std::set<Double> >::const_iterator tEnd = spwTimes.end();
        for (; tIter!=tEnd; ++tIter) {
            scanSize += dSize * tIter->second.size();
        }
        scanSize += feSize * props.firstExposureTime.size();
    }

    static const uInt ssStructSize = 3*dSize + 2*iSize;
    static const uInt sskeySize = 4*iSize;
    uInt subScanSize = subScanProps->size() * (ssStructSize + sskeySize);
    std::map<SubScanKey, SubScanProperties>::const_iterator subIter = subScanProps->begin();
    std::map<SubScanKey, SubScanProperties>::const_iterator subEnd = subScanProps->end();
    for ( ; subIter != subEnd; ++subIter) {
        const SubScanProperties& props = subIter->second;
        subScanSize += iSize*(
            props.antennas.size() + props.ddIDs.size() + props.stateIDs.size()
            + props.spws.size()
        );
        subScanSize += (dSize + iSize) + props.timeProps.size();
        // meanInterval + spwNRows
        subScanSize += (3*iSize + dSize) * props.meanInterval.size();
        subScanSize += feSize * props.firstExposureTime.size();
    }
    uInt mysize = scanSize + subScanSize;
    if (_cacheUpdated(mysize)) {
        _scanProperties = scanProps;
        _subScanProperties = subScanProps;
    }
    else if (_forceSubScanPropsToCache) {
       _cacheMB += mysize/1e6;
       _scanProperties = scanProps;
       _subScanProperties = subScanProps;
    }
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
    vector<uInt> dataDescIDToSpwMap = getDataDescIDToSpwMap();
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
    std::shared_ptr<std::map<SubScanKey, Double> >& subScanNACRows,
    std::shared_ptr<std::map<SubScanKey, Double> >& subScanNXCRows,
    std::shared_ptr<vector<Double> >& fieldNACRows,
    std::shared_ptr<vector<Double> >& fieldNXCRows
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
    std::shared_ptr<Vector<Int> > ant1, ant2;
    _getAntennas(ant1, ant2);
    std::shared_ptr<Vector<Int> > dataDescIDs = _getDataDescIDs();
    std::shared_ptr<Vector<Int> > scans = _getScans();
    std::shared_ptr<Vector<Int> > fieldIDs = _getFieldIDs();
    std::shared_ptr<Vector<Int> > obsIDs = _getObservationIDs();
    std::shared_ptr<Vector<Int> > arrIDs = _getArrayIDs();
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
    vector<uInt> dataDescIDToSpwMap = getDataDescIDToSpwMap();
    std::set<uInt> a, b, c, d, e;
    vector<SpwProperties> spwInfo = _getSpwInfo(a, b, c, d, e);
    std::shared_ptr<ArrayColumn<Bool> > flags = _getFlags();
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
    std::shared_ptr<Vector<Int> > dataDescIDs = _getDataDescIDs();
    Vector<Int>::const_iterator curDDID = dataDescIDs->begin();
    Vector<Int>::const_iterator endDDID = dataDescIDs->end();
    std::shared_ptr<Vector<Int> > states = _getStateIDs();
    Vector<Int>::const_iterator curState = states->begin();
    vector<uInt> dataDescToSpwMap = getDataDescIDToSpwMap();
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
    std::shared_ptr<Vector<Int> > allStates = _getStateIDs();
    std::shared_ptr<Vector<Int> > allFields = _getFieldIDs();
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
    vector<String> names = getFieldNames();
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
    Vector<Int> sourceIDs = ScalarColumn<Int>(_ms->field(), sourceIDName).getColumn();
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
    if (getIntents().empty()) {
        fieldToIntentsMap = vector<std::set<String> >(nFields());
        intentToFieldsMap.clear();
        return;
    }
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
    vector<uInt> dataDescIDToSpwMap = getDataDescIDToSpwMap();
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
    Int& antenna1, Int& antenna2, Double& time, rownr_t row,
    Bool interpolate, Int initialguess

) const {
    ThrowIf(
        row >= this->nRows(),
        "Row number exceeds number of rows in the MS"
    );
    const String& ant1ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA1);
    const String& ant2ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA2);
    antenna1 = ScalarColumn<Int>(*_ms, ant1ColName).get(row);
    antenna2 = ScalarColumn<Int>(*_ms, ant2ColName).get(row);
    bool autocorr = (antenna1==antenna2);
    const String& timeColName = MeasurementSet::columnName(MSMainEnums::TIME);
    time = ScalarColumn<Double>(*_ms, timeColName).get(row);
    MSPointingColumns pCols(_ms->pointing());
    Int pidx1, pidx2;
    pidx1 = pCols.pointingIndex(antenna1, time, initialguess);
    if (autocorr) {
        pidx2 = pidx1;
    }
    else pidx2 = pCols.pointingIndex(antenna2, time, initialguess);
    const String& intervalColName = MeasurementSet::columnName(MSMainEnums::INTERVAL);
    Double interval = ScalarColumn<Double>(*_ms, intervalColName).get(row);
    MDirection dir1, dir2;
    if (!interpolate || interval >= pCols.interval()(pidx1)) {
        dir1 = pCols.directionMeas(pidx1);
    }
    else {
        dir1 = _getInterpolatedDirection(pCols, pidx1, time);
    }
    if (autocorr) {
        dir2 = dir1;
    }
    else if (!interpolate || interval >= pCols.interval()(pidx2)) {
        dir2 = pCols.directionMeas(pidx2);
    }
    else {
        dir2 = _getInterpolatedDirection(pCols, pidx2, time);
    }
    return std::make_pair(dir1, dir2);
}

MDirection MSMetaData::_getInterpolatedDirection(
    const MSPointingColumns& pCols, const Int& index1,
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

vector<uInt> MSMetaData::getDataDescIDToSpwMap() const {
    if (! _dataDescIDToSpwMap.empty()) {
        return _dataDescIDToSpwMap;
    }
    String spwColName = MSDataDescription::columnName(MSDataDescriptionEnums::SPECTRAL_WINDOW_ID);
    ScalarColumn<Int> spwCol(_ms->dataDescription(), spwColName);
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
    vector<uInt> ddToSpwMap = getDataDescIDToSpwMap();
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
    ScalarColumn<Int> polCol(_ms->dataDescription(), polColName);
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
        mysize += 4*(sizeof(Double)*iter->nchans + 20);
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
    std::set<ScanKey> allKeys = getScanKeys();
    ThrowIf(
        allKeys.find(key) == allKeys.end(),
        "Unknown scan " + toString(key)
    );
}

void MSMetaData::_checkScans(const std::set<ScanKey>& scanKeys) const {
    std::set<ScanKey> allKeys = getScanKeys();
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
    std::set<Int> uniqueFields = getUniqueFieldIDs();
    return uniqueFields.find(fieldID) != uniqueFields.end();
}

const std::set<Int>& MSMetaData::getUniqueAntennaIDs() const {
    // this method is responsible for setting _uniqueAntennas
    if (_uniqueAntennaIDs.empty()) {
        if (_subScanProperties) {
            map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
            map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
            for (; iter!=end; ++iter) {
                const std::set<Int>& ants = iter->second.antennas;
                _uniqueAntennaIDs.insert(ants.begin(), ants.end());
            }
        }
        else {
            std::shared_ptr<Vector<Int> > ant1, ant2;
            _getAntennas(ant1, ant2);
            _uniqueAntennaIDs.insert(ant1->begin(), ant1->end());
            _uniqueAntennaIDs.insert(ant2->begin(), ant2->end());
        }
    }    
    return _uniqueAntennaIDs;
}

std::set<uInt> MSMetaData::getUniqueDataDescIDs() const {
    // this method is responsible for setting _uniqueDataDescIDs
    if (_uniqueDataDescIDs.empty()) {
        if (_subScanProperties) {
            map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
            map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
            for (; iter!=end; ++iter) {
                const std::set<uInt>& ddIDs = iter->second.ddIDs; 
                _uniqueDataDescIDs.insert(ddIDs.begin(), ddIDs.end());
            }
        }
        else {
            std::shared_ptr<Vector<Int> > allDDIDs = _getDataDescIDs();
            _uniqueDataDescIDs.insert(allDDIDs->begin(), allDDIDs->end());
        }
    }
    return _uniqueDataDescIDs;
}

std::set<Int> MSMetaData::getUniqueFieldIDs() const {
    if (_uniqueFieldIDs.empty()) {
        if (_subScanProperties) {
            map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
            map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
            for (; iter!=end; ++iter) {
                _uniqueFieldIDs.insert(iter->first.fieldID);
            }
        }
        else {
            std::shared_ptr<Vector<Int> > allFieldIDs = _getFieldIDs();
            _uniqueFieldIDs.insert(allFieldIDs->begin(), allFieldIDs->end());
        }
    }
    return _uniqueFieldIDs;
}

std::set<uInt> MSMetaData::getUniqueSpwIDs() const {
    vector<uInt> ddToSpw = getDataDescIDToSpwMap();
    std::set<uInt> uDDs = getUniqueDataDescIDs();
    std::set<uInt> uSpws;
    std::set<uInt>::const_iterator iter = uDDs.begin();
    std::set<uInt>::const_iterator end = uDDs.end();
    for (; iter!=end; ++iter) {
        uSpws.insert(ddToSpw[*iter]);
    }
    return uSpws;
}

Bool MSMetaData::_hasStateID(const Int stateID) const {
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
        std::shared_ptr<Vector<Int> > allStateIDs = _getStateIDs();
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

std::shared_ptr<Quantum<Vector<Double> > > MSMetaData::_getIntervals() const {
    ScalarQuantColumn<Double> col(
        *_ms, MeasurementSet::columnName(MSMainEnums::INTERVAL)
    );
    std::shared_ptr<Quantum<Vector<Double> > > intervals = col.getColumn();
    return intervals;
}

MSMetaData::ColumnStats MSMetaData::getIntervalStatistics() const {
    std::shared_ptr<Quantum<Vector<Double> > > intervals = _getIntervals();
    Vector<Double> intInSec = intervals->getValue("s");
    ColumnStats stats;
    ClassicalStatistics<Double, Vector<Double>::const_iterator> cs;
    cs.setData(intInSec.begin(), intInSec.size());
    cs.getMinMax(stats.min, stats.max);
    stats.median = cs.getMedian();
    return stats;
}

vector<MSMetaData::SpwProperties>  MSMetaData::_getSpwInfo2(
    std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
    std::set<uInt>& wvrSpw, std::set<uInt>& sqldSpw
) const {
    static const Regex rxSqld("BB_[0-9]#SQLD");
    MSSpWindowColumns spwCols(_ms->spectralWindow());
    Vector<Double> bws = spwCols.totalBandwidth().getColumn();
    ArrayQuantColumn<Double> cfCol(
        _ms->spectralWindow(),
        MSSpectralWindow::columnName(MSSpectralWindowEnums::CHAN_FREQ)
    );
    ArrayQuantColumn<Double> cwCol(
        _ms->spectralWindow(),
        MSSpectralWindow::columnName(MSSpectralWindowEnums::CHAN_WIDTH)
    );
    ScalarMeasColumn<MFrequency> reffreqs(
        _ms->spectralWindow(),
        MSSpectralWindow::columnName(MSSpectralWindowEnums::REF_FREQUENCY)
    );
    ArrayQuantColumn<Double> ebwCol(
        _ms->spectralWindow(),
        MSSpectralWindow::columnName(MSSpectralWindowEnums::EFFECTIVE_BW)
    );
    ArrayQuantColumn<Double> resCol(
        _ms->spectralWindow(),
        MSSpectralWindow::columnName(MSSpectralWindowEnums::RESOLUTION)
    );
    auto nss  = spwCols.netSideband().getColumn();
    auto name = spwCols.name().getColumn();
    auto myHasBBCNo = hasBBCNo();
    auto bbcno = myHasBBCNo ? spwCols.bbcNo().getColumn() : Vector<Int>();
    vector<Double> freqLimits(2);
    Vector<Quantity> tmp;
    vector<SpwProperties> spwInfo(bws.size());
    std::set<uInt> wvrFirst, wvrSecond;
    const static Unit emptyUnit;
    const static Unit hz("Hz");
    const static String wvr = "WVR";
    const static String wvrNominal = "WVR#NOMINAL";
    uInt nrows = bws.size();
    for (uInt i=0; i<nrows; ++i) {
        spwInfo[i].bandwidth = bws[i];
        tmp.resize(0);
        cfCol.get(i, tmp);
        spwInfo[i].chanfreqs = QVD(tmp);
        auto u = spwInfo[i].chanfreqs.getFullUnit();
        spwInfo[i].meanfreq = Quantity(
            mean(spwInfo[i].chanfreqs.getValue()), u
        );
        freqLimits[0] = spwInfo[i].chanfreqs.min().getValue(u);
        freqLimits[1] = spwInfo[i].chanfreqs.max().getValue(u);
        spwInfo[i].edgechans = freqLimits;
        tmp.resize(0);
        cwCol.get(i, tmp);
        spwInfo[i].chanwidths = QVD(tmp);
        spwInfo[i].netsideband = nss[i];
        spwInfo[i].nchans = tmp.size();
        uInt nchan = spwInfo[i].nchans;
        tmp.resize(0);
        ebwCol.get(i, tmp);
        spwInfo[i].effbw = QVD(tmp);
        tmp.resize(0);
        resCol.get(i, tmp);
        spwInfo[i].resolution = QVD(tmp);
        auto halfWidths = (spwInfo[i].chanwidths)/2.0;
        auto lowFreq = (spwInfo[i].chanfreqs - halfWidths).min();
        auto highFreq = (spwInfo[i].chanfreqs + halfWidths).max();
        spwInfo[i].centerfreq = (lowFreq + highFreq)/2;
        spwInfo[i].name = name[i];
        if (myHasBBCNo) {
            spwInfo[i].bbcno = bbcno[i];
            if (name[i].contains(rxSqld)) {
                sqldSpw.insert(i);
            }
        }
        spwInfo[i].reffreq = reffreqs(i);
        if (spwInfo[i].reffreq.getUnit() == emptyUnit) {
            spwInfo[i].reffreq.set(hz);
        }
        // types of ALMA spws
        // algorithm from thunter, CAS-5794, CAS-12592, CAS-13362
        if (name[i].contains(wvr)) {
            wvrFirst.insert(i);
            if (name[i] == wvrNominal) {
                wvrSecond.insert(i);
            }
        }
        else if (
            spwInfo[i].nchans == 1 && ! name[i].contains("FULL_RES")
        ) {
            avgSpw.insert(i);
        }
        else if (
            spwInfo[i].bandwidth < 2e9 || (
                nchan >= 15 && ! (
                    nchan == 256 || nchan == 128 || nchan == 64
                    || nchan == 32 || nchan == 16 || nchan == 248
                    || nchan == 124 || nchan == 62 || nchan == 31
                )
            )
        ) {
            fdmSpw.insert(i);
        }
        else {
            tdmSpw.insert(i);
        }
    }
    wvrSpw = wvrSecond.empty() ? wvrFirst : wvrSecond;
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

std::set<SubScanKey> MSMetaData::getSubScanKeys(
    const ArrayKey& arrayKey
) const {
    std::map<ArrayKey, std::set<SubScanKey> > mymap = _getArrayKeysToSubScanKeys();
    std::map<ArrayKey, std::set<SubScanKey> >::const_iterator iter = mymap.find(arrayKey);
    ThrowIf(
        iter == mymap.end(),
        "MS does not contain requested ArrayKey"
    );
    return iter->second;
}

std::map<ArrayKey, std::set<SubScanKey> > MSMetaData::_getArrayKeysToSubScanKeys() const {
    // this method is responsible for setting _arrayToSubScans
    if (! _arrayToSubScans.empty()) {
        return _arrayToSubScans;
    }
    std::set<SubScanKey> subScans = _getSubScanKeys();
    std::set<SubScanKey>::const_iterator iter = subScans.begin();
    std::set<SubScanKey>::const_iterator end = subScans.end();
    std::map<ArrayKey, std::set<SubScanKey> > mymap;
    ArrayKey akey;
    for ( ; iter != end; ++iter) {
        akey.arrayID = iter->arrayID;
        akey.obsID = iter->obsID;
        if (mymap.find(akey) == mymap.end()) {
            mymap[akey] = std::set<SubScanKey>();
        }
        mymap[akey].insert(*iter);
    }
    uInt mysize = 0;
    std::map<ArrayKey, std::set<SubScanKey> >::const_iterator miter = mymap.begin();
    std::map<ArrayKey, std::set<SubScanKey> >::const_iterator mend = mymap.end();
    for ( ; miter != mend; ++miter) {
        mysize += sizeof(SubScanKey)*miter->second.size();
    }
    mysize += mymap.size() * sizeof(ArrayKey);
    if (_cacheUpdated(mysize)) {
        _arrayToSubScans = mymap;
    }
    return mymap;
}
/*
map<SubScanKey, Quantity> MSMetaData::_getMeanExposureTimes() const {
    // this method is responsible for setting _meanExposureTimeMap
    if (! _meanExposureTimeMap.empty()) {
        return _meanExposureTimeMap;
    }

}
*/

}

