// # MSMetaData.cc
// # Copyright (C) 1998,1999,2000,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/ms/MSOper/MSMetaData.h>

#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/File.h>
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

#include <regex>
#include <utility>

#define _ORIGIN "MSMetaData::" + std::string(__func__) + ": "

namespace casacore {

MSMetaData::MSMetaData(const MeasurementSet* const& ms, const float maxCacheSizeMB)
    : _ms(ms),
      _showProgress(false),
      _cacheMB(0),
      _maxCacheMB(maxCacheSizeMB),
      _nACRows(0),
      _nXCRows(0),
      _nStates(0),
      _nSpw(0),
      _nFields(0),
      _nAntennas(0),
      _nObservations(0),
      _nScans(0),
      _nArrays(0),
      _nrows(0),
      _nPol(0),
      _nDataDescIDs(0),
      _scanToSpwsMap(),
      _scanToDDIDsMap(),
      _dataDescIDToSpwMap(),
      _dataDescIDToPolIDMap(),
      _fieldToSpwMap(),
      _scanToStatesMap(),
      _scanToFieldsMap(),
      _fieldToStatesMap(),
      _stateToFieldsMap(),
      _sourceToFieldsMap(),
      _antennaNameToIDMap(),
      _intentToFieldIDMap(),
      _intentToScansMap(),
      _uniqueIntents(),
      _uniqueFieldIDs(),
      _uniqueStateIDs(),
      _avgSpw(),
      _tdmSpw(),
      _fdmSpw(),
      _wvrSpw(),
      _sqldSpw(),
      _subScanToNACRowsMap(),
      _subScanToNXCRowsMap(),
      _fieldToNACRowsMap(),
      _fieldToNXCRowsMap(),
      _scanToIntentsMap(),
      _stateToIntentsMap(),
      _spwToIntentsMap(),
      _spwInfo(0),
      _spwToFieldIDsMap(),
      _obsToArraysMap(),
      _spwToScansMap(),
      _fieldToScansMap(),
      _fieldNames(0),
      _antennaNames(0),
      _observatoryNames(0),
      _scanToTimesMap(),
      _fieldToTimesMap(),
      _observatoryPositions(0),
      _antennaOffsets(0),
      _uniqueBaselines(0, 0),
      _exposureTime(0),
      _nUnflaggedACRows(0),
      _nUnflaggedXCRows(0),
      _unflaggedFieldNACRows(),
      _unflaggedFieldNXCRows(),
      _unflaggedSubScanNACRows(),
      _unflaggedSubScanNXCRows(),
      _taqlTableName(File(ms->tableName()).exists() ? ms->tableName() : "$1"),
      _taqlTempTable(File(ms->tableName()).exists() ? 0 : 1, ms),
      _spwInfoStored(false),
      _forceSubScanPropsToCache(false),
      _sourceTimes() {}

MSMetaData::~MSMetaData() {}

unsigned int MSMetaData::nStates() const {
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
  if (!_uniqueIntents.empty()) {
    return _uniqueIntents;
  }
  vector<std::set<String>> statesToIntentsMap;
  std::set<String> uniqueIntents;
  _getStateToIntentsMap(statesToIntentsMap, uniqueIntents);
  return uniqueIntents;
}

void MSMetaData::_getStateToIntentsMap(vector<std::set<String>>& stateToIntentsMap,
                                       std::set<String>& uniqueIntents) const {
  if (!_uniqueIntents.empty() && !_stateToIntentsMap.empty()) {
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
    intentSets.reference(Vector<String>(1, String()));
  }
  stateToIntentsMap.resize(nStates());

  Vector<String>::const_iterator end = intentSets.end();
  vector<std::set<String>>::iterator sIter = stateToIntentsMap.begin();
  for (Vector<String>::const_iterator curIntentSet = intentSets.begin(); curIntentSet != end;
       ++curIntentSet, ++sIter) {
    Vector<String> intents = stringToVector(*curIntentSet, ',');
    *sIter = std::set<String>(intents.begin(), intents.end());
    uniqueIntents.insert(intents.begin(), intents.end());
  }

  std::set<String>::const_iterator lastIntent = uniqueIntents.end();
  unsigned int mysize = 0;

  vector<std::set<String>>::const_iterator lastState = stateToIntentsMap.end();
  for (vector<std::set<String>>::const_iterator iter = stateToIntentsMap.begin(); iter != lastState;
       ++iter) {
    std::set<String>::const_iterator lastIntent = iter->end();
    for (std::set<String>::const_iterator intent = iter->begin(); intent != lastIntent; ++intent) {
      mysize += intent->size();
    }
  }
  for (std::set<String>::const_iterator intent = uniqueIntents.begin(); intent != lastIntent;
       ++intent) {
    mysize += intent->size();
  }
  if (_cacheUpdated(mysize)) {
    _uniqueIntents = uniqueIntents;
    _stateToIntentsMap = stateToIntentsMap;
  }
}

vector<std::pair<Quantity, Quantity>> MSMetaData::getProperMotions() const {
  // this method is responsible for setting _properMotions
  if (!_properMotions.empty()) {
    return _properMotions;
  }
  String colName = MSSource::columnName(MSSource::PROPER_MOTION);
  ArrayQuantColumn<double> col(_ms->source(), colName);
  rownr_t nrow = _ms->source().nrow();
  vector<std::pair<Quantity, Quantity>> myvec(nrow);
  Vector<Quantity> av(2);
  for (rownr_t i = 0; i < nrow; ++i) {
    col.get(i, av, false);
    myvec[i].first = av[0];
    myvec[i].second = av[1];
  }
  if (_cacheUpdated(sizeof(myvec))) {
    _properMotions = myvec;
  }
  return myvec;
}

std::set<int> MSMetaData::getScanNumbers(int obsID, int arrayID) const {
  ArrayKey arrayKey;
  arrayKey.obsID = obsID;
  arrayKey.arrayID = arrayID;
  return _getScanNumbers(arrayKey);
}

unsigned int MSMetaData::nScans() {
  if (_nScans == 0) {
    _nScans = getScanKeys().size();
  }
  return _nScans;
}

unsigned int MSMetaData::nObservations() const {
  if (_nObservations == 0) {
    _nObservations = _ms->observation().nrow();
  }
  return _nObservations;
}

unsigned int MSMetaData::nArrays() {
  if (_nArrays == 0) {
    // because the ARRAY table apparently is optional
    _nArrays = max(*_getArrayIDs()) + 1;
  }
  return _nArrays;
}

rownr_t MSMetaData::nRows() const { return _ms->nrow(); }

rownr_t MSMetaData::nRows(CorrelationType cType) {
  if (cType == BOTH) {
    return nRows();
  }
  rownr_t nACRows, nXCRows;
  std::shared_ptr<std::map<SubScanKey, rownr_t>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<map<int, rownr_t>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap, fieldToNACRowsMap,
               fieldToNXCRowsMap);
  if (cType == AUTO) {
    return nACRows;
  } else {
    return nXCRows;
  }
}

std::shared_ptr<const map<SubScanKey, rownr_t>> MSMetaData::getNRowMap(
    CorrelationType cType) const {
  rownr_t nACRows, nXCRows;
  std::shared_ptr<map<SubScanKey, rownr_t>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<map<int, rownr_t>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap, fieldToNACRowsMap,
               fieldToNXCRowsMap);
  if (cType == AUTO) {
    return subScanToNACRowsMap;
  } else if (cType == CROSS) {
    return subScanToNXCRowsMap;
  }
  std::shared_ptr<map<SubScanKey, rownr_t>> mymap(new map<SubScanKey, rownr_t>());
  map<SubScanKey, rownr_t>::const_iterator iter = subScanToNACRowsMap->begin();
  map<SubScanKey, rownr_t>::const_iterator end = subScanToNACRowsMap->end();
  for (; iter != end; ++iter) {
    SubScanKey key = iter->first;
    (*mymap)[key] = iter->second + (*subScanToNXCRowsMap)[key];
  }
  return mymap;
}

rownr_t MSMetaData::nRows(CorrelationType cType, int arrayID, int observationID, int scanNumber,
                          int fieldID) const {
  SubScanKey subScanKey;
  subScanKey.obsID = observationID;
  subScanKey.arrayID = arrayID;
  subScanKey.scan = scanNumber;
  subScanKey.fieldID = fieldID;
  _checkSubScan(subScanKey);
  rownr_t nACRows, nXCRows;
  std::shared_ptr<map<SubScanKey, rownr_t>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<map<int, rownr_t>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap, fieldToNACRowsMap,
               fieldToNXCRowsMap);
  if (cType == AUTO) {
    return (*subScanToNACRowsMap)[subScanKey];
  } else if (cType == CROSS) {
    return (*subScanToNXCRowsMap)[subScanKey];
  } else {
    return (*subScanToNACRowsMap)[subScanKey] + (*subScanToNXCRowsMap)[subScanKey];
  }
}

rownr_t MSMetaData::nRows(CorrelationType cType, unsigned int fieldID) const {
  _checkField(fieldID);
  rownr_t nACRows, nXCRows;
  std::shared_ptr<map<SubScanKey, rownr_t>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<map<int, rownr_t>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap, fieldToNACRowsMap,
               fieldToNXCRowsMap);
  if (cType == AUTO) {
    return (*fieldToNACRowsMap)[fieldID];
  } else if (cType == CROSS) {
    return (*fieldToNXCRowsMap)[fieldID];
  } else {
    return (*fieldToNACRowsMap)[fieldID] + (*fieldToNXCRowsMap)[fieldID];
  }
}

double MSMetaData::nUnflaggedRows() const {
  double nACRows, nXCRows;
  std::shared_ptr<std::map<SubScanKey, double>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<vector<double>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getUnflaggedRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap,
                        fieldToNACRowsMap, fieldToNXCRowsMap);
  return nACRows + nXCRows;
}
double MSMetaData::nUnflaggedRows(CorrelationType cType) const {
  if (cType == BOTH) {
    return nUnflaggedRows();
  }
  double nACRows, nXCRows;
  std::shared_ptr<std::map<SubScanKey, double>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<vector<double>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getUnflaggedRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap,
                        fieldToNACRowsMap, fieldToNXCRowsMap);
  if (cType == AUTO) {
    return nACRows;
  } else {
    return nXCRows;
  }
}

double MSMetaData::nUnflaggedRows(CorrelationType cType, int arrayID, unsigned int observationID,
                                  int scanNumber, unsigned int fieldID) const {
  SubScanKey subScanKey;
  subScanKey.obsID = observationID;
  subScanKey.arrayID = arrayID;
  subScanKey.scan = scanNumber;
  subScanKey.fieldID = fieldID;
  _checkSubScan(subScanKey);
  double nACRows, nXCRows;
  std::shared_ptr<std::map<SubScanKey, double>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<vector<double>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getUnflaggedRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap,
                        fieldToNACRowsMap, fieldToNXCRowsMap);
  if (cType == AUTO) {
    return (*subScanToNACRowsMap)[subScanKey];
  } else if (cType == CROSS) {
    return (*subScanToNXCRowsMap)[subScanKey];
  } else {
    return (*subScanToNACRowsMap)[subScanKey] + (*subScanToNXCRowsMap)[subScanKey];
  }
}

double MSMetaData::nUnflaggedRows(CorrelationType cType, int fieldID) const {
  double nACRows, nXCRows;
  std::shared_ptr<std::map<SubScanKey, double>> subScanToNACRowsMap, subScanToNXCRowsMap;
  std::shared_ptr<vector<double>> fieldToNACRowsMap, fieldToNXCRowsMap;
  _getUnflaggedRowStats(nACRows, nXCRows, subScanToNACRowsMap, subScanToNXCRowsMap,
                        fieldToNACRowsMap, fieldToNXCRowsMap);
  if (cType == AUTO) {
    return (*fieldToNACRowsMap)[fieldID];
  } else if (cType == CROSS) {
    return (*fieldToNXCRowsMap)[fieldID];
  } else {
    return (*fieldToNACRowsMap)[fieldID] + (*fieldToNXCRowsMap)[fieldID];
  }
}

void MSMetaData::_getRowStats(rownr_t& nACRows, rownr_t& nXCRows,
                              std::map<SubScanKey, rownr_t>*& subScanToNACRowsMap,
                              std::map<SubScanKey, rownr_t>*& subScanToNXCRowsMap,
                              map<int, rownr_t>*& fieldToNACRowsMap,
                              map<int, rownr_t>*& fieldToNXCRowsMap) const {
  nACRows = 0;
  nXCRows = 0;
  subScanToNACRowsMap = new std::map<SubScanKey, rownr_t>();
  subScanToNXCRowsMap = new std::map<SubScanKey, rownr_t>();
  fieldToNACRowsMap = new map<int, rownr_t>();
  fieldToNXCRowsMap = new map<int, rownr_t>();
  std::set<SubScanKey> subScanKeys = _getSubScanKeys();
  std::set<SubScanKey>::const_iterator subIter = subScanKeys.begin();
  std::set<SubScanKey>::const_iterator subEnd = subScanKeys.end();
  for (; subIter != subEnd; ++subIter) {
    // initialize subscan map
    (*subScanToNACRowsMap)[*subIter] = 0;
    (*subScanToNXCRowsMap)[*subIter] = 0;
  }
  std::set<int> fieldIDs = getUniqueFieldIDs();
  std::set<int>::const_iterator fIter = fieldIDs.begin();
  std::set<int>::const_iterator fEnd = fieldIDs.end();
  for (; fIter != fEnd; ++fIter) {
    // initialize field maps
    (*fieldToNACRowsMap)[*fIter] = 0;
    (*fieldToNXCRowsMap)[*fIter] = 0;
  }
  if (_subScanProperties) {
    map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
    map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
    for (; iter != end; ++iter) {
      const SubScanKey& subscan = iter->first;
      const SubScanProperties& props = iter->second;
      const int& fieldID = subscan.fieldID;
      nACRows += props.acRows;
      (*subScanToNACRowsMap)[subscan] += props.acRows;
      (*subScanToNXCRowsMap)[subscan] += props.xcRows;
      (*fieldToNACRowsMap)[fieldID] += props.acRows;
      (*fieldToNXCRowsMap)[fieldID] += props.xcRows;
    }
    // doing this single computation is more effecient than summing
    // the xcRows in the loop
    nXCRows = nACRows - nRows();
  } else {
    std::shared_ptr<Vector<int>> ant1, ant2;
    _getAntennas(ant1, ant2);
    std::shared_ptr<Vector<int>> scans = _getScans();
    std::shared_ptr<Vector<int>> fieldIDs = _getFieldIDs();
    std::shared_ptr<Vector<int>> obsIDs = _getObservationIDs();
    std::shared_ptr<Vector<int>> arrIDs = _getArrayIDs();
    Vector<int>::const_iterator aEnd = ant1->end();
    Vector<int>::const_iterator a1Iter = ant1->begin();
    Vector<int>::const_iterator a2Iter = ant2->begin();
    Vector<int>::const_iterator sIter = scans->begin();
    Vector<int>::const_iterator fIter = fieldIDs->begin();
    Vector<int>::const_iterator oIter = obsIDs->begin();
    Vector<int>::const_iterator arIter = arrIDs->begin();
    SubScanKey subScanKey;
    for (; a1Iter != aEnd; ++a1Iter, ++a2Iter, ++sIter, ++fIter, ++arIter, ++oIter) {
      subScanKey.obsID = *oIter;
      subScanKey.arrayID = *arIter;
      subScanKey.scan = *sIter;
      subScanKey.fieldID = *fIter;
      if (*a1Iter == *a2Iter) {
        ++nACRows;
        ++(*subScanToNACRowsMap)[subScanKey];
        ++(*fieldToNACRowsMap)[*fIter];
      } else {
        ++nXCRows;
        ++(*subScanToNXCRowsMap)[subScanKey];
        ++(*fieldToNXCRowsMap)[*fIter];
      }
    }
  }
}

void MSMetaData::_getRowStats(rownr_t& nACRows, rownr_t& nXCRows,
                              std::shared_ptr<std::map<SubScanKey, rownr_t>>& scanToNACRowsMap,
                              std::shared_ptr<std::map<SubScanKey, rownr_t>>& scanToNXCRowsMap,
                              std::shared_ptr<map<int, rownr_t>>& fieldToNACRowsMap,
                              std::shared_ptr<map<int, rownr_t>>& fieldToNXCRowsMap) const {
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

  std::map<SubScanKey, rownr_t>*myScanToNACRowsMap, *myScanToNXCRowsMap;
  map<int, rownr_t>*myFieldToNACRowsMap, *myFieldToNXCRowsMap;
  _getRowStats(nACRows, nXCRows, myScanToNACRowsMap, myScanToNXCRowsMap, myFieldToNACRowsMap,
               myFieldToNXCRowsMap);
  scanToNACRowsMap.reset(myScanToNACRowsMap);
  scanToNXCRowsMap.reset(myScanToNXCRowsMap);
  fieldToNACRowsMap.reset(myFieldToNACRowsMap);
  fieldToNXCRowsMap.reset(myFieldToNXCRowsMap);
  unsigned int size = 2 * (sizeof(unsigned int) + _sizeof(*scanToNACRowsMap) + _sizeof(*fieldToNACRowsMap));
  if (_cacheUpdated(size)) {
    _nACRows = nACRows;
    _nXCRows = nXCRows;
    _subScanToNACRowsMap = scanToNACRowsMap;
    _subScanToNXCRowsMap = scanToNXCRowsMap;
    _fieldToNACRowsMap = fieldToNACRowsMap;
    _fieldToNXCRowsMap = fieldToNXCRowsMap;
  }
}

void MSMetaData::_getAntennas(std::shared_ptr<Vector<int>>& ant1,
                              std::shared_ptr<Vector<int>>& ant2) const {
  ant1 = _getMainScalarColumn<int>(MSMainEnums::ANTENNA1);
  ant2 = _getMainScalarColumn<int>(MSMainEnums::ANTENNA2);
}

std::shared_ptr<Vector<int>> MSMetaData::_getScans() const {
  return _getMainScalarColumn<int>(MSMainEnums::SCAN_NUMBER);
}

std::shared_ptr<Vector<int>> MSMetaData::_getObservationIDs() const {
  return _getMainScalarColumn<int>(MSMainEnums::OBSERVATION_ID);
}

std::shared_ptr<Vector<int>> MSMetaData::_getArrayIDs() const {
  return _getMainScalarColumn<int>(MSMainEnums::ARRAY_ID);
}

std::shared_ptr<Vector<int>> MSMetaData::_getFieldIDs() const {
  return _getMainScalarColumn<int>(MSMainEnums::FIELD_ID);
}

std::shared_ptr<Vector<int>> MSMetaData::_getStateIDs() const {
  std::shared_ptr<Vector<int>> states = _getMainScalarColumn<int>(MSMainEnums::STATE_ID);
  int maxState = max(*states);
  int nstates = (int)nStates();
  ThrowIf(maxState >= nstates, "MS only has " + std::to_string(nstates) +
                                   " rows in its STATE table, but references STATE_ID " +
                                   std::to_string(maxState) + " in its main table.");
  return states;
}

std::shared_ptr<Vector<int>> MSMetaData::_getDataDescIDs() const {
  return _getMainScalarColumn<int>(MSMainEnums::DATA_DESC_ID);
}

std::set<int> MSMetaData::getScansForState(int stateID, int obsID, int arrayID) const {
  if (!_hasStateID(stateID)) {
    return std::set<int>();
  }
  std::map<ScanKey, std::set<int>> myScanToStatesMap = getScanToStatesMap();
  ArrayKey arrayKey;
  arrayKey.obsID = obsID;
  arrayKey.arrayID = arrayID;
  std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
  std::set<ScanKey>::const_iterator iter = scanKeys.begin();
  std::set<ScanKey>::const_iterator end = scanKeys.end();
  std::set<int> stateIDs, scansForState;
  while (iter != end) {
    stateIDs = myScanToStatesMap[*iter];
    if (stateIDs.find(stateID) != stateIDs.end()) {
      scansForState.insert(iter->scan);
    }
    ++iter;
  }
  return scansForState;
}

std::map<ScanKey, std::set<int>> MSMetaData::_getScanToAntennasMap() const {
  // this method is responsible for setting _scanToAntennasMap
  if (!_scanToAntennasMap.empty()) {
    return _scanToAntennasMap;
  }
  std::map<ScanKey, std::set<int>> myScanToAntsMap;
  map<SubScanKey, SubScanProperties> subScanProps = *getSubScanProperties();
  map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps.begin();
  map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps.end();
  while (iter != end) {
    SubScanKey subKey = iter->first;
    SubScanProperties subProps = iter->second;
    myScanToAntsMap[scanKey(subKey)].insert(subProps.antennas.begin(), subProps.antennas.end());
    ++iter;
  }
  std::map<ScanKey, std::set<int>>::const_iterator end1 = myScanToAntsMap.end();
  unsigned int mySize = sizeof(ScanKey) * myScanToAntsMap.size();
  for (std::map<ScanKey, std::set<int>>::const_iterator iter = myScanToAntsMap.begin();
       iter != end1; ++iter) {
    mySize += sizeof(int) * iter->second.size();
  }
  if (_cacheUpdated(mySize)) {
    _scanToAntennasMap = myScanToAntsMap;
  }
  return myScanToAntsMap;
}

std::map<ScanKey, std::set<int>> MSMetaData::getScanToStatesMap() const {
  if (!_scanToStatesMap.empty()) {
    return _scanToStatesMap;
  }
  std::map<ScanKey, std::set<int>> myScanToStatesMap;
  if (nStates() == 0) {
    std::set<int> empty;
    std::set<ScanKey> scanKeys = getScanKeys();
    // std::set<SubScanKey> subScanKeys;
    //_getSubScanKeys(subScanKeys, scanKeys);
    std::set<ScanKey>::const_iterator end = scanKeys.end();
    for (std::set<ScanKey>::const_iterator scanKey = scanKeys.begin(); scanKey != end; ++scanKey) {
      myScanToStatesMap[*scanKey] = empty;
    }
  } else {
    map<SubScanKey, SubScanProperties> subScanProps = *getSubScanProperties();
    // map<ScanKey, ScanProperties> scanProps;
    // map<ArrayKey, ArrayProperties> arrayProps;

    //_getSubScanProperties(subScanProps, scanProps, arrayProps);
    map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps.begin();
    map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps.end();
    // ScanKey key;
    while (iter != end) {
      SubScanKey subKey = iter->first;
      // key.obsID = subKey.obsID;
      // key.arrayID = subKey.arrayID;
      // key.scan = subKey.scan;
      SubScanProperties subProps = iter->second;
      myScanToStatesMap[scanKey(subKey)].insert(subProps.stateIDs.begin(), subProps.stateIDs.end());
      ++iter;
    }
  }
  std::map<ScanKey, std::set<int>>::const_iterator end = myScanToStatesMap.end();
  unsigned int mySize = sizeof(ScanKey) * myScanToStatesMap.size();
  for (std::map<ScanKey, std::set<int>>::const_iterator iter = myScanToStatesMap.begin();
       iter != end; ++iter) {
    mySize += sizeof(int) * iter->second.size();
  }
  if (_cacheUpdated(mySize)) {
    _scanToStatesMap = myScanToStatesMap;
  }
  return myScanToStatesMap;
}

void MSMetaData::_getSubScansAndIntentsMaps(
    std::shared_ptr<const map<SubScanKey, std::set<String>>>& subScanToIntentsMap,
    map<String, std::set<SubScanKey>>& intentToSubScansMap) const {
  // This method is responsible for setting _subScanToIntentsMap and _intentToSubScansMap
  if (_subScanToIntentsMap && !_intentToSubScansMap.empty()) {
    subScanToIntentsMap = _subScanToIntentsMap;
    intentToSubScansMap = _intentToSubScansMap;
    return;
  }
  std::shared_ptr<map<SubScanKey, std::set<String>>> ssToIntents(
      new map<SubScanKey, std::set<String>>());
  intentToSubScansMap.clear();
  if (_ms->state().nrow() == 0) {
    // because the decision was made to support MSes with non-existent STATE tables
    std::set<SubScanKey> sskeys = _getSubScanKeys();
    std::set<SubScanKey>::const_iterator ssiter = sskeys.begin();
    std::set<SubScanKey>::const_iterator ssend = sskeys.end();
    for (; ssiter != ssend; ++ssiter) {
      (*ssToIntents)[*ssiter] = std::set<String>();
    }
  } else {
    vector<std::set<String>> stateToIntentsMap;
    std::set<String> uniqueIntents;
    _getStateToIntentsMap(stateToIntentsMap, uniqueIntents);
    map<SubScanKey, MSMetaData::SubScanProperties> props = *getSubScanProperties();
    map<SubScanKey, MSMetaData::SubScanProperties>::const_iterator iter = props.begin();
    map<SubScanKey, MSMetaData::SubScanProperties>::const_iterator end = props.end();
    for (; iter != end; ++iter) {
      SubScanKey sskey = iter->first;
      std::set<int> stateIDs = iter->second.stateIDs;
      std::set<int>::const_iterator siter = stateIDs.begin();
      std::set<int>::const_iterator send = stateIDs.end();
      for (; siter != send; ++siter) {
        std::set<String> intents = stateToIntentsMap[*siter];
        (*ssToIntents)[sskey].insert(intents.begin(), intents.end());
        std::set<String>::const_iterator initer = intents.begin();
        std::set<String>::const_iterator inend = intents.end();
        for (; initer != inend; ++initer) {
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
    std::map<ScanKey, std::set<String>>& scanToIntentsMap,
    std::map<String, std::set<ScanKey>>& intentToScansMap) const {
  // This method is responsible for setting _scanToIntentsMap and _intentToScansMap
  if (!_scanToIntentsMap.empty() && !_intentToScansMap.empty()) {
    scanToIntentsMap = _scanToIntentsMap;
    intentToScansMap = _intentToScansMap;
    return;
  }
  vector<std::set<String>> stateToIntentsMap;
  std::set<String> uniqueIntents;
  _getStateToIntentsMap(stateToIntentsMap, uniqueIntents);
  std::map<ScanKey, std::set<int>> scanToStatesMap = getScanToStatesMap();
  std::map<ScanKey, std::set<int>>::const_iterator end = scanToStatesMap.end();
  std::set<int> states;
  std::set<String> intents;
  for (std::map<ScanKey, std::set<int>>::const_iterator iter = scanToStatesMap.begin(); iter != end;
       ++iter) {
    ScanKey scan = iter->first;
    states = iter->second;
    std::set<int>::const_iterator endState = states.end();
    for (std::set<int>::const_iterator myState = states.begin(); myState != endState; ++myState) {
      if (*myState < 0) {
        continue;
      }
      intents = stateToIntentsMap[*myState];
      scanToIntentsMap[scan].insert(intents.begin(), intents.end());
      std::set<String>::const_iterator endIntent = intents.end();
      for (std::set<String>::const_iterator myIntent = intents.begin(); myIntent != endIntent;
           ++myIntent) {
        intentToScansMap[*myIntent].insert(scan);
      }
    }
  }
  if (_cacheUpdated(_sizeof(scanToIntentsMap) + _sizeof(intentToScansMap))) {
    _scanToIntentsMap = scanToIntentsMap;
    _intentToScansMap = intentToScansMap;
  }
}

unsigned int MSMetaData::_sizeof(const std::map<double, MSMetaData::TimeStampProperties>& m) {
  unsigned int sizeInt = sizeof(int);
  unsigned int size = m.size() * (sizeof(double) + sizeInt);
  std::map<double, MSMetaData::TimeStampProperties>::const_iterator iter = m.begin();
  std::map<double, MSMetaData::TimeStampProperties>::const_iterator end = m.end();
  unsigned int n = 0;
  while (iter != end) {
    n += iter->second.ddIDs.size();
    ++iter;
  }
  return size + n * sizeInt;
}

template <class T>
unsigned int MSMetaData::_sizeof(const std::map<T, std::set<String>>& m) {
  unsigned int size = sizeof(T) * m.size();
  typename std::map<T, std::set<String>>::const_iterator iter = m.begin();
  typename std::map<T, std::set<String>>::const_iterator end = m.end();
  while (iter != end) {
    std::set<String>::const_iterator end2 = iter->second.end();
    for (std::set<String>::const_iterator iter2 = iter->second.begin(); iter2 != end2; ++iter2) {
      size += iter2->size();
    }
    ++iter;
  }
  return size;
}

template <class T, class U>
unsigned int MSMetaData::_sizeof(const std::map<T, std::set<U>>& m) {
  unsigned int size = sizeof(T) * m.size();
  typename std::map<T, std::set<U>>::const_iterator iter = m.begin();
  typename std::map<T, std::set<U>>::const_iterator end = m.end();
  unsigned int nElements = 0;
  while (iter != end) {
    nElements += iter->second.size();
    ++iter;
  }
  size += sizeof(U) * nElements;
  return size;
}

template <class T, class U>
unsigned int MSMetaData::_sizeof(const std::map<T, U>& m) {
  return m.size() * (sizeof(T) + sizeof(U));
}

unsigned int MSMetaData::_sizeof(const vector<std::set<String>>& m) {
  unsigned int size = sizeof(int) * m.size();
  vector<std::set<String>>::const_iterator end = m.end();
  for (vector<std::set<String>>::const_iterator iter = m.begin(); iter != end; ++iter) {
    std::set<String>::const_iterator end2 = iter->end();
    for (std::set<String>::const_iterator iter2 = iter->begin(); iter2 != end2; ++iter2) {
      size += iter2->size();
    }
  }
  return size;
}

unsigned int MSMetaData::_sizeof(const vector<String>& m) {
  vector<String>::const_iterator end = m.end();
  unsigned int size = 0;
  for (vector<String>::const_iterator iter = m.begin(); iter != end; ++iter) {
    size += iter->length();
  }
  return size;
}

template <class T>
unsigned int MSMetaData::_sizeof(const vector<T>& v) {
  return v.size() * sizeof(T);
}

unsigned int MSMetaData::_sizeof(const vector<vector<String>>& v) {
  vector<vector<String>>::const_iterator iter = v.begin();
  vector<vector<String>>::const_iterator end = v.end();
  unsigned int size = 0;
  while (iter != end) {
    size += _sizeof(*iter);
    ++iter;
  }
  return size;
}

unsigned int MSMetaData::_sizeof(const Quantum<Vector<double>>& m) {
  return (sizeof(double)) * m.getValue().size() + m.getUnit().size();
}

template <class T>
unsigned int MSMetaData::_sizeof(const std::map<String, std::set<T>>& m) {
  unsigned int setssize = 0;
  unsigned int size = 0;
  typename std::map<String, std::set<T>>::const_iterator end = m.end();
  for (typename std::map<String, std::set<T>>::const_iterator iter = m.begin(); iter != end;
       ++iter) {
    size += iter->first.size();
    setssize += iter->second.size();
  }
  size += sizeof(T) * setssize;
  return size;
}

unsigned int MSMetaData::_sizeof(const vector<std::map<int, Quantity>>& m) {
  unsigned int size = 0;
  vector<std::map<int, Quantity>>::const_iterator end = m.end();
  unsigned int intsize = sizeof(int);
  unsigned int qsize = 20;
  for (vector<std::map<int, Quantity>>::const_iterator iter = m.begin(); iter != end; ++iter) {
    size += iter->size() * (2 * intsize + qsize);
  }
  return size;
}

std::set<String> MSMetaData::getIntentsForScan(const ScanKey& scan) const {
  _checkScan(scan);
  std::map<ScanKey, std::set<String>> scanToIntentsMap;
  std::map<String, std::set<ScanKey>> intentToScansMap;
  _getScansAndIntentsMaps(scanToIntentsMap, intentToScansMap);
  return scanToIntentsMap[scan];
}

std::set<String> MSMetaData::getIntentsForSubScan(const SubScanKey& subScan) const {
  _checkSubScan(subScan);
  std::shared_ptr<const std::map<SubScanKey, std::set<String>>> subScanToIntentsMap;
  std::map<String, std::set<SubScanKey>> intentToSubScansMap;
  _getSubScansAndIntentsMaps(subScanToIntentsMap, intentToSubScansMap);
  return subScanToIntentsMap->find(subScan)->second;
}

std::shared_ptr<const std::map<SubScanKey, std::set<String>>> MSMetaData::getSubScanToIntentsMap()
    const {
  std::shared_ptr<const std::map<SubScanKey, std::set<String>>> subScanToIntentsMap;
  std::map<String, std::set<SubScanKey>> intentToSubScansMap;
  _getSubScansAndIntentsMaps(subScanToIntentsMap, intentToSubScansMap);
  return subScanToIntentsMap;
}

bool MSMetaData::_cacheUpdated(const float incrementInBytes) const {
  float newSize = _cacheMB + incrementInBytes / 1e6;
  if (newSize <= _maxCacheMB) {
    _cacheMB = newSize;
    return true;
  }
  return false;
}

std::set<unsigned int> MSMetaData::getSpwsForIntent(const String& intent) {
  if (!_hasIntent(intent)) {
    return std::set<unsigned int>();
  }
  vector<std::set<String>> spwToIntentsMap = _getSpwToIntentsMap();
  std::set<unsigned int> spws;
  for (unsigned int i = 0; i < spwToIntentsMap.size(); ++i) {
    if (spwToIntentsMap[i].find(intent) != spwToIntentsMap[i].end()) {
      spws.insert(i);
    }
  }
  return spws;
}

std::vector<std::set<unsigned int>> MSMetaData::getSpwToDataDescriptionIDMap() const {
  // TODO perhaps cache the result, but atm doesn't seem worth doing
  std::map<std::pair<unsigned int, unsigned int>, unsigned int> spwPolToDDID = getSpwIDPolIDToDataDescIDMap();
  std::vector<std::set<unsigned int>> mymap(nSpw(true));
  std::map<std::pair<unsigned int, unsigned int>, unsigned int>::const_iterator iter = spwPolToDDID.begin();
  std::map<std::pair<unsigned int, unsigned int>, unsigned int>::const_iterator end = spwPolToDDID.end();
  while (iter != end) {
    mymap[iter->first.first].insert(iter->second);
    ++iter;
  }
  return mymap;
}

unsigned int MSMetaData::nSpw(bool includewvr) const {
  if (_nSpw > 0) {
    return includewvr ? _nSpw : _nSpw - getWVRSpw().size();
  }
  unsigned int nSpw = _ms->spectralWindow().nrow();
  _nSpw = nSpw;
  return includewvr ? nSpw : nSpw - getWVRSpw().size();
}

unsigned int MSMetaData::nPol() {
  if (_nPol == 0) {
    _nPol = _ms->polarization().nrow();
  }
  return _nPol;
}

std::set<String> MSMetaData::getIntentsForSpw(const unsigned int spw) {
  if (spw >= nSpw(true)) {
    throw AipsError(_ORIGIN + "spectral window out of range");
  }
  return _getSpwToIntentsMap()[spw];
}

std::set<String> MSMetaData::getIntentsForField(int fieldID) {
  if (!_hasFieldID(fieldID)) {
    return std::set<String>();
  }
  vector<std::set<String>> fieldToIntentsMap;
  std::map<String, std::set<int>> intentToFieldsMap;
  _getFieldsAndIntentsMaps(fieldToIntentsMap, intentToFieldsMap);
  return fieldToIntentsMap[fieldID];
}

std::set<String> MSMetaData::getIntentsForField(String field) {
  vector<std::set<String>> fieldToIntentsMap;
  std::map<String, std::set<int>> intentToFieldsMap;
  _getFieldsAndIntentsMaps(fieldToIntentsMap, intentToFieldsMap);
  const auto fieldNames = getFieldNames();
  std::set<String> intents;
  unsigned int i = 0;
  for (const auto& name : fieldNames) {
    if (name == field) {
      const auto myIntents = fieldToIntentsMap[i];
      intents.insert(myIntents.begin(), myIntents.end());
    }
    ++i;
  }
  return intents;
}

unsigned int MSMetaData::nFields() const {
  if (_nFields > 0) {
    return _nFields;
  }
  unsigned int nFields = _ms->field().nrow();
  _nFields = nFields;
  return nFields;
}

std::shared_ptr<std::set<int>> MSMetaData::_getEphemFieldIDs() const {
  // responsible for setting _ephemFields
  if (_ephemFields) {
    return _ephemFields;
  }
  MSFieldColumns msfc(_ms->field());
  ScalarColumn<int> ephemCol = msfc.ephemerisId();
  _ephemFields = std::make_shared<std::set<int>>();
  if (ephemCol.isNull()) {
    return _ephemFields;
  }
  Vector<int> colData = ephemCol.getColumn();
  Vector<int>::const_iterator iter = colData.begin();
  Vector<int>::const_iterator end = colData.end();
  unsigned int i = 0;
  for (; iter != end; ++iter, ++i) {
    if (*iter >= 0) {
      _ephemFields->insert(i);
    }
  }
  return _ephemFields;
}

MDirection MSMetaData::phaseDirFromFieldIDAndTime(const unsigned int fieldID, const MEpoch& ep) const {
  _hasFieldID(fieldID);
  MSFieldColumns msfc(_ms->field());
  if (!msfc.needInterTime(fieldID)) {
    return msfc.phaseDirMeas(fieldID, 0.0);
  }
  MEpoch::Types msType = MEpoch::castType(msfc.timeMeas()(fieldID).getRef().getType());
  Unit sec("s");
  double inSeconds = MEpoch::Convert(ep, msType)().get(sec).getValue();
  return msfc.phaseDirMeas(fieldID, inSeconds);
}

MDirection MSMetaData::getReferenceDirection(const unsigned int fieldID, const MEpoch& ep) const {
  _hasFieldID(fieldID);
  MSFieldColumns msfc(_ms->field());
  if (!msfc.needInterTime(fieldID)) {
    return msfc.referenceDirMeas(fieldID, 0.0);
  }
  MEpoch::Types msType = MEpoch::castType(msfc.timeMeas()(fieldID).getRef().getType());
  Unit sec("s");
  double inSeconds = MEpoch::Convert(ep, msType)().get(sec).getValue();
  return msfc.referenceDirMeas(fieldID, inSeconds);
}

void MSMetaData::_getFieldsAndSpwMaps(std::map<int, std::set<unsigned int>>& fieldToSpwMap,
                                      vector<std::set<int>>& spwToFieldMap) const {
  // This method has the responsibility of setting _fieldToSpwMap and _spwToFieldIDMap
  if (!_fieldToSpwMap.empty() && !_spwToFieldIDsMap.empty()) {
    fieldToSpwMap = _fieldToSpwMap;
    spwToFieldMap = _spwToFieldIDsMap;
    return;
  }
  fieldToSpwMap.clear();
  spwToFieldMap.resize(nSpw(true));
  std::shared_ptr<const std::map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const std::map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, _showProgress);
  std::map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps->begin();
  std::map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps->end();
  for (; iter != end; ++iter) {
    int fieldID = iter->first.fieldID;
    const std::set<unsigned int>& spws = iter->second.spws;
    fieldToSpwMap[fieldID].insert(spws.begin(), spws.end());
    std::set<unsigned int>::const_iterator spwIter = spws.begin();
    std::set<unsigned int>::const_iterator spwEnd = spws.end();
    for (; spwIter != spwEnd; ++spwIter) {
      spwToFieldMap[*spwIter].insert(fieldID);
    }
  }
  if (_cacheUpdated(_sizeof(fieldToSpwMap) + _sizeof(spwToFieldMap))) {
    _fieldToSpwMap = fieldToSpwMap;
    _spwToFieldIDsMap = spwToFieldMap;
  }
}

std::map<int, std::set<unsigned int>> MSMetaData::getFieldsToSpwsMap() const {
  std::map<int, std::set<unsigned int>> myFieldToSpwMap;
  vector<std::set<int>> mySpwToFieldMap;
  _getFieldsAndSpwMaps(myFieldToSpwMap, mySpwToFieldMap);
  return myFieldToSpwMap;
}

std::set<unsigned int> MSMetaData::getSpwsForField(int fieldID) const {
  if (!_hasFieldID(fieldID)) {
    return std::set<unsigned int>();
  }
  return getFieldsToSpwsMap()[fieldID];
}

std::set<unsigned int> MSMetaData::getSpwsForField(const String& fieldName) {
  unsigned int myNFields = nFields();
  vector<String> fieldNames = getFieldNames();
  std::set<unsigned int> spws;
  for (unsigned int i = 0; i < myNFields; ++i) {
    if (fieldNames[i] == fieldName) {
      std::set<unsigned int> myspws = getSpwsForField(i);
      spws.insert(myspws.begin(), myspws.end());
    }
  }
  ThrowIf(spws.empty(), _ORIGIN + "field (" + std::string(fieldName) + " does not exist.");
  return spws;
}

vector<String> MSMetaData::getFieldNames() const {
  if (!_fieldNames.empty()) {
    return _fieldNames;
  }

  String fieldNameColName = MSField::columnName(MSFieldEnums::NAME);
  ScalarColumn<String> nameCol(_ms->field(), fieldNameColName);
  vector<String> fieldNames = nameCol.getColumn().tovector();
  unsigned int mysize = 0;
  vector<String>::const_iterator end = fieldNames.end();
  for (vector<String>::const_iterator name = fieldNames.begin(); name != end; ++name) {
    mysize += name->size();
  }
  if (_cacheUpdated(mysize)) {
    _fieldNames = fieldNames;
  }
  return fieldNames;
}

vector<String> MSMetaData::getFieldCodes() const {
  // this method is responsible for setting _fieldCodes
  if (!_fieldCodes.empty()) {
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

std::set<int> MSMetaData::getFieldIDsForSpw(const unsigned int spw) {
  unsigned int myNSpw = nSpw(true);
  if (spw >= myNSpw) {
    throw AipsError(_ORIGIN + "spectral window out of range");
  }
  std::map<int, std::set<unsigned int>> myFieldToSpwMap;
  vector<std::set<int>> mySpwToFieldMap;
  _getFieldsAndSpwMaps(myFieldToSpwMap, mySpwToFieldMap);
  return mySpwToFieldMap[spw];
}

std::set<String> MSMetaData::getFieldNamesForSpw(const unsigned int spw) {
  std::set<int> fieldIDs = getFieldIDsForSpw(spw);
  std::set<String> fieldNames;
  vector<String> allFieldNames = getFieldNames();
  for (std::set<int>::const_iterator fieldID = fieldIDs.begin(); fieldID != fieldIDs.end();
       ++fieldID) {
    fieldNames.insert(allFieldNames[*fieldID]);
  }
  return fieldNames;
}

std::shared_ptr<const map<ScanKey, MSMetaData::ScanProperties>>
MSMetaData::_generateScanPropsIfWanted() const {
  if (_scanProperties) {
    // we already have it, just return it
    return _scanProperties;
  }
  if (_forceSubScanPropsToCache) {
    // it hasn't been generated yet, but we will likely
    // need it later, so just generate it now
    std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps;
    std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps;
    _getScanAndSubScanProperties(scanProps, subScanProps, false);
    return scanProps;
  }
  // we don't have it, and we aren't going to want it later
  return std::shared_ptr<const map<ScanKey, ScanProperties>>();
}

std::shared_ptr<const map<SubScanKey, MSMetaData::SubScanProperties>>
MSMetaData::_generateSubScanPropsIfWanted() const {
  if (_subScanProperties) {
    // we already have it, just return it
    return _subScanProperties;
  }
  if (_forceSubScanPropsToCache) {
    // it hasn't been generated yet, but we will likely
    // need it later, so just generate it now
    std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps;
    std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps;
    _getScanAndSubScanProperties(scanProps, subScanProps, false);
    return subScanProps;
  }
  // we don't have it, and we aren't going to want it later
  return std::shared_ptr<const map<SubScanKey, SubScanProperties>>();
}

std::set<ScanKey> MSMetaData::getScanKeys() const {
  if (!_scanKeys.empty()) {
    return _scanKeys;
  }
  std::set<ScanKey> scanKeys;
  std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps = _generateScanPropsIfWanted();
  if (scanProps) {
    map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
    map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
    for (; iter != end; ++iter) {
      scanKeys.insert(iter->first);
    }
  } else {
    // fastest way if we don't have _scanProperties and aren't going to need it later
    std::set<SubScanKey> subScanKeys = _getSubScanKeys();
    std::set<SubScanKey>::const_iterator iter = subScanKeys.begin();
    std::set<SubScanKey>::const_iterator end = subScanKeys.end();
    for (; iter != end; ++iter) {
      scanKeys.insert(scanKey(*iter));
    }
  }
  if (_cacheUpdated(sizeof(ScanKey) * scanKeys.size())) {
    _scanKeys = scanKeys;
  }
  return scanKeys;
}

std::set<ScanKey> MSMetaData::getScanKeys(const ArrayKey& arrayKey) const {
  std::set<ScanKey> allScanKeys = getScanKeys();
  bool doAllObsIDs = arrayKey.obsID < 0;
  bool doAllArrayIDs = arrayKey.arrayID < 0;
  if (doAllObsIDs && doAllArrayIDs) {
    return allScanKeys;
  }
  std::set<ScanKey> scanKeys;
  std::set<ScanKey>::const_iterator iter = allScanKeys.begin();
  std::set<ScanKey>::const_iterator end = allScanKeys.end();

  while (iter != end) {
    if ((doAllObsIDs || iter->obsID == arrayKey.obsID) &&
        (doAllArrayIDs || iter->arrayID == arrayKey.arrayID)) {
      scanKeys.insert(*iter);
    }
    ++iter;
  }
  return scanKeys;
}

std::set<ScanKey> MSMetaData::_getScanKeys(const std::set<ScanKey>& scanKeys,
                                           const ArrayKey& arrayKey) const {
  int obsID = arrayKey.obsID;
  int arrayID = arrayKey.arrayID;
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

std::set<int> MSMetaData::_getScanNumbers(const ArrayKey& arrayKey) const {
  return scanNumbers(getScanKeys(arrayKey));
}

void MSMetaData::_getScansAndDDIDMaps(std::map<ScanKey, std::set<unsigned int>>& scanToDDIDMap,
                                      vector<std::set<ScanKey>>& ddIDToScanMap) const {
  // this method is responsible for setting _scanToDDIDsMap and _ddidToScansMap
  if (!_scanToDDIDsMap.empty()) {
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
  std::set<unsigned int> ddIDs;
  std::set<unsigned int>::const_iterator dIter;
  std::set<unsigned int>::const_iterator dEnd;
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

std::map<ScanKey, std::set<unsigned int>> MSMetaData::getScanToSpwsMap() const {
  std::map<ScanKey, std::set<unsigned int>> scanToSpwMap;
  vector<std::set<ScanKey>> spwToScanMap;
  _getScansAndSpwMaps(scanToSpwMap, spwToScanMap);
  return scanToSpwMap;
}

vector<std::set<ScanKey>> MSMetaData::getSpwToScansMap() const {
  std::map<ScanKey, std::set<unsigned int>> scanToSpwMap;
  vector<std::set<ScanKey>> spwToScanMap;
  _getScansAndSpwMaps(scanToSpwMap, spwToScanMap);
  return spwToScanMap;
}

void MSMetaData::_getScansAndSpwMaps(std::map<ScanKey, std::set<unsigned int>>& scanToSpwMap,
                                     vector<std::set<ScanKey>>& spwToScanMap) const {
  // This method is responsible for setting _scanToSpwsMap and _spwToScansMap
  if (!_scanToSpwsMap.empty() && !_spwToScansMap.empty()) {
    scanToSpwMap = _scanToSpwsMap;
    spwToScanMap = _spwToScansMap;
    return;
  }
  scanToSpwMap.clear();
  spwToScanMap.clear();
  spwToScanMap.resize(nSpw(true));
  std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps =
      _generateSubScanPropsIfWanted();
  if (subScanProps) {
    map<SubScanKey, SubScanProperties>::const_iterator iter = subScanProps->begin();
    map<SubScanKey, SubScanProperties>::const_iterator end = subScanProps->end();
    for (; iter != end; ++iter) {
      ScanKey sk = scanKey(iter->first);
      std::set<unsigned int> spws = iter->second.spws;
      scanToSpwMap[sk].insert(spws.begin(), spws.end());
      std::set<unsigned int>::const_iterator spwIter = spws.begin();
      std::set<unsigned int>::const_iterator spwEnd = spws.end();
      for (; spwIter != spwEnd; ++spwIter) {
        spwToScanMap[*spwIter].insert(sk);
      }
    }
  } else {
    // fastest way to generate what we want if we don't have _subScanProperties
    std::map<ScanKey, std::set<unsigned int>> scanToDDIDMap;
    vector<std::set<ScanKey>> ddIDToScanMap;
    _getScansAndDDIDMaps(scanToDDIDMap, ddIDToScanMap);
    vector<unsigned int> ddToSpw = getDataDescIDToSpwMap();
    std::map<ScanKey, std::set<unsigned int>>::const_iterator iter = scanToDDIDMap.begin();
    std::map<ScanKey, std::set<unsigned int>>::const_iterator end = scanToDDIDMap.end();
    std::set<unsigned int>::const_iterator dIter;
    std::set<unsigned int>::const_iterator dEnd;
    for (; iter != end; ++iter) {
      ScanKey scanKey = iter->first;
      std::set<unsigned int> ddids = scanToDDIDMap[scanKey];
      dIter = ddids.begin();
      dEnd = ddids.end();
      for (; dIter != dEnd; ++dIter) {
        unsigned int spw = ddToSpw[*dIter];
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
unsigned int MSMetaData::_sizeof(const vector<std::set<T>>& v) {
  unsigned int size = 0;
  typename vector<std::set<T>>::const_iterator iter = v.begin();
  typename vector<std::set<T>>::const_iterator end = v.end();
  while (iter != end) {
    size = iter->size();
    ++iter;
  }
  size *= sizeof(T);
  return size;
}

std::set<int> MSMetaData::getAntennasForScan(const ScanKey& scan) const {
  _checkScan(scan);
  return _getScanToAntennasMap()[scan];
}

std::set<unsigned int> MSMetaData::getSpwsForScan(const ScanKey& scan) const {
  _checkScan(scan);
  std::map<ScanKey, std::set<unsigned int>> scanToSpwMap;
  vector<std::set<ScanKey>> spwToScanMap;
  _getScansAndSpwMaps(scanToSpwMap, spwToScanMap);
  return scanToSpwMap[scan];
}

std::set<unsigned int> MSMetaData::getSpwsForSubScan(const SubScanKey& subScan) const {
  return getSubScanProperties(subScan).spws;
}

std::set<int> MSMetaData::getScansForSpw(const unsigned int spw, int obsID, int arrayID) const {
  unsigned int myNSpw = nSpw(true);
  ThrowIf(spw >= myNSpw, "spectral window out of range");
  ArrayKey arrayKey;
  arrayKey.obsID = obsID;
  arrayKey.arrayID = arrayID;
  std::set<ScanKey> myScanKeys = getScanKeys(arrayKey);
  std::map<ScanKey, std::set<unsigned int>> scanToSpwMap;
  vector<std::set<ScanKey>> spwToScanMap;
  _getScansAndSpwMaps(scanToSpwMap, spwToScanMap);
  std::set<ScanKey>::const_iterator iter = myScanKeys.begin();
  std::set<ScanKey>::const_iterator end = myScanKeys.end();
  std::set<int> scanNumbers;
  std::set<unsigned int> spws;
  while (iter != end) {
    spws = scanToSpwMap[*iter];
    if (spws.find(spw) != spws.end()) {
      scanNumbers.insert(iter->scan);
    }
    ++iter;
  }
  return scanNumbers;
}

unsigned int MSMetaData::nAntennas() const {
  if (_nAntennas > 0) {
    return _nAntennas;
  }
  unsigned int nAnts = _ms->antenna().nrow();
  _nAntennas = nAnts;
  return nAnts;
}

unsigned int MSMetaData::nDataDescriptions() const {
  if (_nDataDescIDs == 0) {
    _nDataDescIDs = _ms->dataDescription().nrow();
  }
  return _nDataDescIDs;
}

vector<String> MSMetaData::_getAntennaNames(std::map<String, std::set<unsigned int>>& namesToIDsMap) const {
  if (!_antennaNames.empty()) {
    namesToIDsMap = _antennaNameToIDMap;
    return _antennaNames;
  }
  namesToIDsMap.clear();
  std::map<String, unsigned int> mymap;
  String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
  ScalarColumn<String> nameCol(_ms->antenna(), antNameColName);
  Vector<String> names = nameCol.getColumn();
  Vector<String>::const_iterator end = names.end();
  unsigned int i = 0;
  for (Vector<String>::const_iterator name = names.begin(); name != end; ++name, ++i) {
    namesToIDsMap[*name].insert(i);
  }
  unsigned int mysize = names.size() * sizeof(unsigned int);
  for (Vector<String>::const_iterator name = names.begin(); name != end; ++name) {
    mysize += 2 * name->size();
  }
  if (_cacheUpdated(mysize)) {
    _antennaNames = names.tovector();
    _antennaNameToIDMap = namesToIDsMap;
  }
  return names.tovector();
}

vector<String> MSMetaData::getAntennaNames(std::map<String, unsigned int>& namesToIDsMap,
                                           const vector<unsigned int>& antennaIDs) const {
  namesToIDsMap.clear();
  std::map<String, std::set<unsigned int>> allMap;
  vector<String> names = getAntennaNames(allMap, antennaIDs);
  std::map<String, std::set<unsigned int>>::const_iterator iter = allMap.begin();
  std::map<String, std::set<unsigned int>>::const_iterator end = allMap.end();
  for (; iter != end; ++iter) {
    namesToIDsMap[iter->first] = *iter->second.rbegin();
  }
  return names;
}

vector<String> MSMetaData::getAntennaNames(std::map<String, std::set<unsigned int>>& namesToIDsMap,
                                           const vector<unsigned int>& antennaIDs) const {
  unsigned int nAnts = nAntennas();
  std::map<String, std::set<unsigned int>> allMap;
  vector<String> allNames = _getAntennaNames(allMap);
  if (antennaIDs.empty()) {
    namesToIDsMap = allMap;
    return allNames;
  }
  unsigned int mymax = max(Vector<unsigned int>(antennaIDs));
  ThrowIf(mymax >= nAnts, "Antenna ID " + std::to_string(mymax) + " out of range.");
  vector<String> names;
  vector<unsigned int>::const_iterator end = antennaIDs.end();
  for (vector<unsigned int>::const_iterator id = antennaIDs.begin(); id != end; ++id) {
    String antName = allNames[*id];
    names.push_back(antName);
    namesToIDsMap[antName].insert(*id);
  }
  return names;
}

unsigned int MSMetaData::getAntennaID(const String& antennaName) const {
  return *getAntennaIDs(antennaName).rbegin();
}

std::set<unsigned int> MSMetaData::getAntennaIDs(const String& antennaName) const {
  return getAntennaIDs(vector<String>(1, antennaName))[0];
}

vector<std::set<unsigned int>> MSMetaData::getAntennaIDs(const vector<String>& antennaNames) const {
  std::map<String, std::set<unsigned int>> namesToIDsMap;
  vector<String> names = getAntennaNames(namesToIDsMap);
  vector<String>::const_iterator end = antennaNames.end();
  std::map<String, std::set<unsigned int>>::const_iterator mapEnd = namesToIDsMap.end();
  vector<std::set<unsigned int>> ids;
  for (vector<String>::const_iterator name = antennaNames.begin(); name != end; ++name) {
    std::map<String, std::set<unsigned int>>::const_iterator pair = namesToIDsMap.find(*name);
    ThrowIf(pair == mapEnd, _ORIGIN + "Unknown antenna " + std::string(*name));
    ids.push_back(pair->second);
  }
  return ids;
}

map<ScanKey, MSMetaData::FirstExposureTimeMap> MSMetaData::getScanToFirstExposureTimeMap(
    bool showProgress) const {
  std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties>> scanProps =
      _getScanProperties(showProgress);
  std::map<ScanKey, MSMetaData::ScanProperties>::const_iterator iter = scanProps->begin();
  std::map<ScanKey, MSMetaData::ScanProperties>::const_iterator end = scanProps->end();
  map<ScanKey, FirstExposureTimeMap> ret;
  for (; iter != end; ++iter) {
    ret[iter->first] = iter->second.firstExposureTime;
  }
  return ret;
}

// deprecated, no longer supported
vector<std::map<int, Quantity>> MSMetaData::getFirstExposureTimeMap() {
  if (!_firstExposureTimeMap.empty()) {
    return _firstExposureTimeMap;
  }
  unsigned int nDataDescIDs = nDataDescriptions();
  std::shared_ptr<Vector<int>> scans = _getScans();
  std::shared_ptr<Vector<int>> dataDescIDs = _getDataDescIDs();
  std::shared_ptr<Vector<double>> times = _getTimes();
  std::shared_ptr<Quantum<Vector<double>>> exposureTimes = _getExposureTimes();
  vector<std::map<int, Quantity>> firstExposureTimeMap(nDataDescIDs);
  vector<std::map<int, double>> tmap(nDataDescIDs);
  Vector<int>::const_iterator siter = scans->begin();
  Vector<int>::const_iterator send = scans->end();
  Vector<int>::const_iterator diter = dataDescIDs->begin();
  Vector<double>::const_iterator titer = times->begin();
  Vector<double> eTimes = exposureTimes->getValue();
  String unit = exposureTimes->getUnit();
  Vector<double>::const_iterator eiter = eTimes.begin();
  while (siter != send) {
    std::map<int, Quantity> mymap = firstExposureTimeMap[*diter];
    if (mymap.find(*siter) == mymap.end() || *titer < tmap[*diter][*siter]) {
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

vector<String> MSMetaData::getAntennaStations(const vector<unsigned int>& antennaIDs) {
  vector<String> allStations = _getStationNames();
  if (antennaIDs.empty()) {
    return allStations;
  }
  _hasAntennaID(max(Vector<unsigned int>(antennaIDs)));
  vector<String> myStationNames;
  vector<unsigned int>::const_iterator end = antennaIDs.end();
  for (vector<unsigned int>::const_iterator iter = antennaIDs.begin(); iter != end; ++iter) {
    myStationNames.push_back(allStations[*iter]);
  }
  return myStationNames;
}

vector<vector<String>> MSMetaData::getAntennaStations(const vector<String>& antennaNames) {
  vector<std::set<unsigned int>> ids = getAntennaIDs(antennaNames);
  vector<std::set<unsigned int>>::const_iterator iter = ids.begin();
  vector<std::set<unsigned int>>::const_iterator end = ids.end();
  vector<vector<String>> stations;
  for (; iter != end; ++iter) {
    std::set<unsigned int>::const_iterator siter = iter->begin();
    std::set<unsigned int>::const_iterator send = iter->end();
    vector<String> myStations;
    for (; siter != send; ++siter) {
      myStations.push_back(getAntennaStations(vector<unsigned int>(1, *siter))[0]);
    }
    stations.push_back(myStations);
  }
  return stations;
}

vector<String> MSMetaData::_getStationNames() {
  if (!_stationNames.empty()) {
    return _stationNames;
  }
  String antStationColName = MSAntenna::columnName(MSAntennaEnums::STATION);
  vector<String> stationNames =
      ScalarColumn<String>(_ms->antenna(), antStationColName).getColumn().tovector();
  if (_cacheUpdated(_sizeof(stationNames))) {
    _stationNames = stationNames;
  }
  return stationNames;
}

std::set<SubScanKey> MSMetaData::_getSubScanKeys() const {
  // responsible for setting _subscans
  if (!_subscans.empty()) {
    return _subscans;
  }
  std::set<SubScanKey> mysubscans;
  if (_subScanProperties) {
    map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
    map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
    for (; iter != end; ++iter) {
      mysubscans.insert(iter->first);
    }
  } else {
    std::shared_ptr<Vector<int>> scans = _getScans();
    std::shared_ptr<Vector<int>> fields = _getFieldIDs();
    std::shared_ptr<Vector<int>> arrays = _getArrayIDs();
    std::shared_ptr<Vector<int>> obs = _getObservationIDs();
    Vector<int>::const_iterator scanIter = scans->begin();
    Vector<int>::const_iterator scanEnd = scans->end();
    Vector<int>::const_iterator fIter = fields->begin();
    Vector<int>::const_iterator oIter = obs->begin();
    Vector<int>::const_iterator aIter = arrays->begin();
    SubScanKey subScanKey;
    for (; scanIter != scanEnd; ++scanIter, ++fIter, ++oIter, ++aIter) {
      subScanKey.obsID = *oIter;
      subScanKey.arrayID = *aIter;
      subScanKey.scan = *scanIter;
      subScanKey.fieldID = *fIter;
      mysubscans.insert(subScanKey);
    }
  }
  if (_cacheUpdated(mysubscans.size() * sizeof(SubScanKey))) {
    _subscans = mysubscans;
  }
  return mysubscans;
}

std::set<SubScanKey> MSMetaData::_getSubScanKeys(const ScanKey& scanKey) const {
  _checkScan(scanKey);
  return _getScanToSubScansMap()[scanKey];
}

std::map<ScanKey, std::set<SubScanKey>> MSMetaData::_getScanToSubScansMap() const {
  // sets _scanToSubScans;
  if (!_scanToSubScans.empty()) {
    return _scanToSubScans;
  }
  std::set<SubScanKey> allSubScans = _getSubScanKeys();
  std::set<SubScanKey>::const_iterator iter = allSubScans.begin();
  std::set<SubScanKey>::const_iterator end = allSubScans.end();
  std::map<ScanKey, std::set<SubScanKey>> mymap;
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
  if (!_antennaDiameters.getValue().empty()) {
    return _antennaDiameters;
  }
  String antDiamColName = MSAntenna::columnName(MSAntennaEnums::DISH_DIAMETER);
  ScalarColumn<double> diamCol(_ms->antenna(), antDiamColName);
  Vector<double> diams = diamCol.getColumn();
  String unit = *diamCol.keywordSet().asArrayString("QuantumUnits").begin();
  QVD antennaDiameters = QVD(diams, unit);
  if (_cacheUpdated(_sizeof(antennaDiameters))) {
    _antennaDiameters = antennaDiameters;
  }
  return antennaDiameters;
}

std::set<unsigned int> MSMetaData::getTDMSpw() {
  if (!_tdmSpw.empty()) {
    return _tdmSpw;
  }
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  return tdmSpw;
}

vector<double> MSMetaData::getBandWidths() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<double> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->bandwidth);
  }
  return out;
}

QVD MSMetaData::_freqWidthToVelWidth(const QVD& v, const Quantity& refFreq) {
  QVD dv = v;
  dv.convert("Hz");
  dv = dv / refFreq.getValue("Hz");
  dv.scale(C::c / 1000);
  dv.setUnit("km/s");
  return dv;
}

vector<QVD> MSMetaData::getChanEffectiveBWs(bool asVelWidths) const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<QVD> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    if (asVelWidths && iter->effbw.isConform("Hz") && iter->meanfreq.getValue() > 0) {
      out.push_back(_freqWidthToVelWidth(iter->effbw, iter->meanfreq));
    } else {
      out.push_back(iter->effbw);
    }
  }
  return out;
}

vector<QVD> MSMetaData::getChanFreqs() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<QVD> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->chanfreqs);
  }
  return out;
}

vector<QVD> MSMetaData::getChanResolutions(bool asVelWidths) const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<QVD> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    if (asVelWidths && iter->resolution.isConform("Hz") && iter->meanfreq.getValue() > 0) {
      out.push_back(_freqWidthToVelWidth(iter->resolution, iter->meanfreq));
    } else {
      out.push_back(iter->resolution);
    }
  }
  return out;
}

vector<QVD> MSMetaData::getChanWidths() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<QVD> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->chanwidths);
  }
  return out;
}

vector<int> MSMetaData::getNetSidebands() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<int> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->netsideband);
  }
  return out;
}

vector<Quantity> MSMetaData::getMeanFreqs() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<Quantity> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->meanfreq);
  }
  return out;
}

vector<Quantity> MSMetaData::getCenterFreqs() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<Quantity> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->centerfreq);
  }
  return out;
}

vector<MFrequency> MSMetaData::getRefFreqs() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<MFrequency> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->reffreq);
  }
  return out;
}

vector<unsigned int> MSMetaData::nChans() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<unsigned int> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->nchans);
  }
  return out;
}

vector<vector<double>> MSMetaData::getEdgeChans() {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<vector<double>> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->edgechans);
  }
  return out;
}

vector<unsigned int> MSMetaData::getBBCNos() const {
  if (!hasBBCNo()) {
    throw AipsError("This MS's SPECTRAL_WINDOW table does not have a BBC_NO column");
  }
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<unsigned int> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->bbcno);
  }
  return out;
}

vector<String> MSMetaData::getCorrBits() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<String> out;
  for (const auto& element : props) {
    out.push_back(element.corrbit);
  }
  return out;
}

std::map<unsigned int, std::set<unsigned int>> MSMetaData::getBBCNosToSpwMap(SQLDSwitch sqldSwitch) {
  vector<unsigned int> mymap = getBBCNos();
  std::map<unsigned int, std::set<unsigned int>> out;
  vector<unsigned int>::const_iterator end = mymap.end();
  std::set<unsigned int> sqldSpw;
  if (sqldSwitch != SQLD_INCLUDE) {
    std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw;
    _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  }
  unsigned int i = 0;
  bool found = true;
  for (vector<unsigned int>::const_iterator iter = mymap.begin(); iter != end; ++iter, ++i) {
    if (out.find(*iter) == out.end()) {
      out[*iter] = std::set<unsigned int>();
    }
    if (sqldSwitch != SQLD_INCLUDE) {
      found = sqldSpw.find(i) != sqldSpw.end();
    }
    if (sqldSwitch == SQLD_INCLUDE || (sqldSwitch == SQLD_EXCLUDE && !found) ||
        (sqldSwitch == SQLD_ONLY && found)) {
      out[*iter].insert(i);
    }
  }
  return out;
}

vector<String> MSMetaData::getSpwNames() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<MSMetaData::SpwProperties> props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<MSMetaData::SpwProperties>::const_iterator end = props.end();
  vector<String> out;
  for (vector<MSMetaData::SpwProperties>::const_iterator iter = props.begin(); iter != end;
       ++iter) {
    out.push_back(iter->name);
  }
  return out;
}

vector<int> MSMetaData::getSpwReceiverBands() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  const auto props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<int> out;
  for (const auto& spw : props) {
    out.push_back(spw.rb);
  }
  return out;
}

vector<int> MSMetaData::getSpwSubwindows() const {
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  const auto props = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  vector<int> out;
  for (const auto& spw : props) {
    out.push_back(spw.sw);
  }
  return out;
}

std::set<unsigned int> MSMetaData::getSpwIDs() const {
  const Vector<int> ddIDs = *_getDataDescIDs();
  const vector<unsigned int>& ddIDToSpw = getDataDescIDToSpwMap();
  Vector<int>::const_iterator iter = ddIDs.begin();
  Vector<int>::const_iterator end = ddIDs.end();
  std::set<unsigned int> spws;
  for (; iter != end; ++iter) {
    if (*iter >= 0) {
      spws.insert(ddIDToSpw[*iter]);
    }
  }
  return spws;
}

std::set<unsigned int> MSMetaData::getFDMSpw() {
  if (!_fdmSpw.empty()) {
    return _fdmSpw;
  }
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  return fdmSpw;
}

std::set<unsigned int> MSMetaData::getChannelAvgSpw() {
  if (!_avgSpw.empty()) {
    return _avgSpw;
  }
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  return avgSpw;
}

std::set<unsigned int> MSMetaData::getWVRSpw() const {
  if (_spwInfoStored) {
    return _wvrSpw;
  }
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  return wvrSpw;
}

std::set<unsigned int> MSMetaData::getSQLDSpw() {
  if (_spwInfoStored) {
    return _sqldSpw;
  }
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  return sqldSpw;
}

std::set<int> MSMetaData::getScansForTimes(double center, double tol, int obsID,
                                           int arrayID) const {
  _checkTolerance(tol);
  ArrayKey arrayKey;
  arrayKey.obsID = obsID;
  arrayKey.arrayID = arrayID;
  std::set<ScanKey> uniqueScans = getScanKeys(arrayKey);
  std::shared_ptr<std::map<ScanKey, std::set<double>>> scanToTimesMap = _getScanToTimesMap();
  double minTime = center - tol;
  double maxTime = center + tol;
  std::set<int> scans;
  std::set<ScanKey>::const_iterator scan = uniqueScans.begin();
  std::set<ScanKey>::const_iterator end = uniqueScans.end();
  while (scan != end) {
    std::set<double> times = scanToTimesMap->find(*scan)->second;
    // rbegin() points to the last element in a container. For a std::set,
    // the last element is the largest, and the first is the smallest.
    if (*times.rbegin() >= minTime && *times.begin() <= maxTime) {
      scans.insert(scan->scan);
    }
    ++scan;
  }
  return scans;
}

std::shared_ptr<std::map<ScanKey, std::set<double>>> MSMetaData::_getScanToTimesMap() const {
  if (_scanToTimesMap && !_scanToTimesMap->empty()) {
    return _scanToTimesMap;
  }
  std::shared_ptr<Vector<int>> scans = _getScans();
  std::shared_ptr<Vector<int>> obsIDs = _getObservationIDs();
  std::shared_ptr<Vector<int>> arrayIDs = _getArrayIDs();
  Vector<int>::const_iterator curScan = scans->begin();
  Vector<int>::const_iterator lastScan = scans->end();
  std::shared_ptr<Vector<double>> times = _getTimes();
  Vector<double>::const_iterator curTime = times->begin();
  Vector<int>::const_iterator curObs = obsIDs->begin();
  Vector<int>::const_iterator curArray = arrayIDs->begin();
  std::shared_ptr<std::map<ScanKey, std::set<double>>> scanToTimesMap(
      new std::map<ScanKey, std::set<double>>());
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

template <class T>
std::shared_ptr<Vector<T>> MSMetaData::_getMainScalarColumn(
    MSMainEnums::PredefinedColumns col) const {
  String name = MeasurementSet::columnName(col);
  ScalarColumn<T> mycol(*_ms, name);
  std::shared_ptr<Vector<T>> v(new Vector<T>());
  mycol.getColumn(*v);
  return v;
}

std::shared_ptr<Vector<double>> MSMetaData::_getTimes() const {
  return _getMainScalarColumn<double>(MSMainEnums::TIME);
}

std::shared_ptr<Quantum<Vector<double>>> MSMetaData::_getExposureTimes() const {
  ScalarQuantColumn<double> col(*_ms, MeasurementSet::columnName(MSMainEnums::EXPOSURE));
  return col.getColumn();
}

std::shared_ptr<ArrayColumn<bool>> MSMetaData::_getFlags() const {
  String flagColName = MeasurementSet::columnName(MSMainEnums::FLAG);
  return std::shared_ptr<ArrayColumn<bool>>(new ArrayColumn<bool>(*_ms, flagColName));
}

std::set<double> MSMetaData::getTimesForScans(std::set<ScanKey> scans) const {
  std::set<double> times;
  if (scans.empty()) {
    std::shared_ptr<Vector<double>> allTimes = _getTimes();
    times.insert(allTimes->begin(), allTimes->end());
    return times;
  }
  std::shared_ptr<std::map<ScanKey, std::set<double>>> scanToTimesMap = _getScanToTimesMap();
  // std::set<Int> scanNumbers = getScanNumbers();
  std::set<ScanKey>::const_iterator scan = scans.begin();
  std::set<ScanKey>::const_iterator end = scans.end();
  std::set<ScanKey> scanKeys = getScanKeys();
  while (scan != end) {
    _checkScan(*scan);
    times.insert(scanToTimesMap->find(*scan)->second.begin(),
                 scanToTimesMap->find(*scan)->second.end());
    ++scan;
  }
  return times;
}

std::set<double> MSMetaData::getTimesForScan(const ScanKey& scan) const {
  std::set<ScanKey> scans;
  scans.insert(scan);
  // scan validity check is done in getTimesForScans()
  return getTimesForScans(scans);
}

std::map<unsigned int, std::set<double>> MSMetaData::getSpwToTimesForScan(const ScanKey& scan) const {
  _checkScan(scan);
  std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, false);
  return scanProps->find(scan)->second.times;
}

std::pair<double, double> MSMetaData::getTimeRangeForScan(const ScanKey& scanKey) const {
  _checkScan(scanKey);
  std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, false);
  return scanProps->find(scanKey)->second.timeRange;
}

std::shared_ptr<const map<ScanKey, pair<double, double>>> MSMetaData::getScanToTimeRangeMap()
    const {
  std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, false);
  std::shared_ptr<map<ScanKey, pair<double, double>>> ret(new map<ScanKey, pair<double, double>>());
  map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
  map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
  for (; iter != end; ++iter) {
    (*ret)[iter->first] = iter->second.timeRange;
  }
  return ret;
}

pair<double, double> MSMetaData::getTimeRange(bool showProgress) const {
  // can't just use TIME column because that does not take into account
  // the interval
  std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, showProgress);
  map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
  map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
  pair<double, double> timerange = iter->second.timeRange;
  ++iter;
  for (; iter != end; ++iter) {
    const pair<double, double>& range = iter->second.timeRange;
    timerange.first = min(timerange.first, range.first);
    timerange.second = max(timerange.second, range.second);
  }
  return timerange;
}

map<unsigned int, double> MSMetaData::getAverageIntervalsForScan(const ScanKey& scan) const {
  _checkScan(scan);
  std::shared_ptr<const map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, false);
  map<unsigned int, Quantity> meanIntervals = scanProps->find(scan)->second.meanInterval;
  map<unsigned int, double> ret;
  map<unsigned int, Quantity>::const_iterator iter = meanIntervals.begin();
  map<unsigned int, Quantity>::const_iterator end = meanIntervals.end();
  for (; iter != end; ++iter) {
    ret[iter->first] = iter->second.getValue();
  }
  return ret;
}

std::map<unsigned int, Quantity> MSMetaData::getAverageIntervalsForSubScan(
    const SubScanKey& subScan) const {
  return getSubScanProperties(subScan).meanInterval;
}

std::map<String, std::set<int>> MSMetaData::getIntentToFieldsMap() {
  vector<std::set<String>> fieldToIntentsMap;
  std::map<String, std::set<int>> intentToFieldsMap;
  _getFieldsAndIntentsMaps(fieldToIntentsMap, intentToFieldsMap);
  return intentToFieldsMap;
}

std::map<String, std::set<ScanKey>> MSMetaData::getIntentToScansMap() {
  std::map<ScanKey, std::set<String>> scanToIntentsMap;
  std::map<String, std::set<ScanKey>> intentToScansMap;
  _getScansAndIntentsMaps(scanToIntentsMap, intentToScansMap);
  return intentToScansMap;
}

std::map<String, std::set<unsigned int>> MSMetaData::getIntentToSpwsMap() {
  vector<std::set<String>> spwToIntentsMap;
  std::map<String, std::set<unsigned int>> intentToSpwsMap;
  _getSpwsAndIntentsMaps(spwToIntentsMap, intentToSpwsMap);
  return intentToSpwsMap;
}

std::set<int> MSMetaData::getScansForField(const String& field, int obsID, int arrayID) const {
  std::set<int> fieldIDs = getFieldIDsForField(field);
  std::set<int> scans;
  for (std::set<int>::const_iterator fieldID = fieldIDs.begin(); fieldID != fieldIDs.end();
       ++fieldID) {
    std::set<int> myscans = getScansForFieldID(*fieldID, obsID, arrayID);
    scans.insert(myscans.begin(), myscans.end());
  }
  return scans;
}

std::set<int> MSMetaData::getScansForFieldID(const int fieldID, int obsID, int arrayID) const {
  if (!_hasFieldID(fieldID)) {
    return std::set<int>();
  }
  vector<std::set<ScanKey>> fieldToScansMap;
  std::map<ScanKey, std::set<int>> scanToFieldsMap;
  _getFieldsAndScansMaps(fieldToScansMap, scanToFieldsMap);
  ArrayKey arrayKey;
  arrayKey.obsID = obsID;
  arrayKey.arrayID = arrayID;
  std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
  std::set<ScanKey>::const_iterator iter = scanKeys.begin();
  std::set<ScanKey>::const_iterator end = scanKeys.end();
  std::set<int> scanNumbers;
  std::set<int> fields;
  while (iter != end) {
    fields = scanToFieldsMap[*iter];
    if (fields.find(fieldID) != fields.end()) {
      scanNumbers.insert(iter->scan);
    }
    ++iter;
  }
  return scanNumbers;
}

std::set<int> MSMetaData::getFieldsForIntent(const String& intent) {
  if (!_hasIntent(intent)) {
    return std::set<int>();
  }
  vector<std::set<String>> fieldToIntentsMap;
  std::map<String, std::set<int>> intentToFieldsMap;
  _getFieldsAndIntentsMaps(fieldToIntentsMap, intentToFieldsMap);
  return intentToFieldsMap[intent];
}

std::set<int> MSMetaData::getFieldsForScan(const ScanKey& scan) const {
  _checkScan(scan);
  vector<std::set<ScanKey>> fieldToScansMap;
  std::map<ScanKey, std::set<int>> scanToFieldsMap;
  _getFieldsAndScansMaps(fieldToScansMap, scanToFieldsMap);
  return scanToFieldsMap[scan];
}

std::set<int> MSMetaData::getFieldsForScans(const std::set<int>& scans, int obsID,
                                            int arrayID) const {
  ArrayKey arrayKey;
  arrayKey.obsID = obsID;
  arrayKey.arrayID = arrayID;
  std::set<ScanKey> myScanKeys = scanKeys(scans, arrayKey);
  return getFieldsForScans(myScanKeys);
}

std::set<int> MSMetaData::getFieldsForScans(const std::set<ScanKey>& scanKeys) const {
  _checkScans(scanKeys);
  std::set<ScanKey>::const_iterator iter = scanKeys.begin();
  std::set<ScanKey>::const_iterator end = scanKeys.end();
  std::set<int> fields;
  while (iter != end) {
    std::set<int> myfields = getFieldsForScan(*iter);
    fields.insert(myfields.begin(), myfields.end());
    ++iter;
  }
  return fields;
}

std::set<int> MSMetaData::getFieldIDsForField(const String& field) const {
  std::set<int> fieldIDs;
  String name = field;
  vector<String> fieldNames = getFieldNames();
  unsigned int nNames = fieldNames.size();
  ToUpperCaseInPlace(name);
  for (unsigned int i = 0; i < nNames; ++i) {
    String testName = fieldNames[i];
    ToUpperCaseInPlace(testName);
    if (name == testName) {
      fieldIDs.insert(i);
    }
  }
  ThrowIf(fieldIDs.empty(), "Unknown field name " + field);
  return fieldIDs;
}

std::set<int> MSMetaData::getScansForIntent(const String& intent, int obsID, int arrayID) const {
  std::set<String> uniqueIntents = getIntents();
  ThrowIf(uniqueIntents.find(intent) == uniqueIntents.end(),
          "Intent " + intent + " is not present in this dataset");
  std::map<ScanKey, std::set<String>> scanToIntentsMap;
  std::map<String, std::set<ScanKey>> intentToScansMap;
  _getScansAndIntentsMaps(scanToIntentsMap, intentToScansMap);
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

std::set<int> MSMetaData::getStatesForScan(int obsID, int arrayID, int scan) const {
  ArrayKey arrayKey;
  arrayKey.obsID = obsID;
  arrayKey.arrayID = arrayID;
  std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
  std::map<ScanKey, std::set<int>> scanToStates = getScanToStatesMap();
  std::set<int> states;
  std::set<ScanKey>::const_iterator iter = scanKeys.begin();
  std::set<ScanKey>::const_iterator end = scanKeys.end();
  while (iter != end) {
    if (iter->scan == scan) {
      std::set<int> myStates = scanToStates[*iter];
      states.insert(myStates.begin(), myStates.end());
    }
    ++iter;
  }
  return states;
}

std::vector<std::set<double>> MSMetaData::getTimesForSpws(bool showProgress) const {
  std::shared_ptr<const std::map<ScanKey, ScanProperties>> scanProps =
      this->_getScanProperties(showProgress);
  std::vector<std::set<double>> myvec(nSpw(true));
  std::map<ScanKey, ScanProperties>::const_iterator iter = scanProps->begin();
  std::map<ScanKey, ScanProperties>::const_iterator end = scanProps->end();
  for (; iter != end; ++iter) {
    const std::map<unsigned int, std::set<double>>& times = iter->second.times;
    std::map<unsigned int, std::set<double>>::const_iterator titer = times.begin();
    std::map<unsigned int, std::set<double>>::const_iterator tend = times.end();
    for (; titer != tend; ++titer) {
      const std::set<double>& spwTimes = titer->second;
      myvec[titer->first].insert(spwTimes.begin(), spwTimes.end());
    }
  }
  return myvec;
}

Record MSMetaData::getSummary() const {
  Record spectralTable;
  spectralTable.define("names", Vector<String>(getSpwNames()));
  Record polTable;
  polTable.define("n correlations", Vector<int>(getNumCorrs()));
  Record dataDescTable;
  vector<unsigned int> ddToSpw = getDataDescIDToSpwMap();
  vector<unsigned int> ddToPolID = getDataDescIDToPolIDMap();
  vector<unsigned int>::const_iterator siter = ddToSpw.begin();
  vector<unsigned int>::const_iterator send = ddToSpw.end();
  vector<unsigned int>::const_iterator piter = ddToPolID.begin();
  vector<int> spws(ddToSpw.size());
  vector<int> polids(ddToPolID.size());
  unsigned int ddid = 0;
  while (siter != send) {
    spws[ddid] = *siter;
    polids[ddid] = *piter;
    ++siter;
    ++piter;
    ++ddid;
  }
  dataDescTable.define("spectral windows", Vector<int>(spws));
  dataDescTable.define("polarization ids", Vector<int>(polids));
  Record summary;
  summary.defineRecord("spectral windows", spectralTable);
  summary.defineRecord("polarizations", polTable);
  summary.defineRecord("data descriptions", dataDescTable);
  summary.define("fields", Vector<String>(getFieldNames()));
  vector<std::set<int>> obsToArraysMap = _getObservationIDToArrayIDsMap();
  vector<std::set<int>>::const_iterator oIter = obsToArraysMap.begin();
  vector<std::set<int>>::const_iterator oEnd = obsToArraysMap.end();
  std::map<SubScanKey, SubScanProperties> subScanProps = *getSubScanProperties();
  unsigned int oCount = 0;
  while (oIter != oEnd) {
    std::set<int>::const_iterator aIter = oIter->begin();
    std::set<int>::const_iterator aEnd = oIter->end();
    Record obsRec;
    while (aIter != aEnd) {
      Record arrayRec;
      ArrayKey aKey;
      aKey.obsID = oCount;
      aKey.arrayID = *aIter;
      _createScanRecords(arrayRec, aKey, subScanProps);
      obsRec.defineRecord("arrayID=" + std::to_string(*aIter), arrayRec);
      ++aIter;
    }
    summary.defineRecord("observationID=" + std::to_string(oCount), obsRec);
    ++oIter;
    ++oCount;
  }
  summary.define("nrows", (Int64)nRows());
  std::shared_ptr<Vector<double>> times = _getTimes();
  summary.define("begin time", min(*times));
  summary.define("end time", max(*times));
  return summary;
}

void MSMetaData::_createScanRecords(
    Record& parent, const ArrayKey& arrayKey,
    const std::map<SubScanKey, SubScanProperties>& subScanProps) const {
  std::set<ScanKey> scanKeys = getScanKeys(arrayKey);
  std::set<ScanKey>::const_iterator scanIter = scanKeys.begin();
  std::set<ScanKey>::const_iterator scanEnd = scanKeys.end();
  while (scanIter != scanEnd) {
    ScanKey scanKey = *scanIter;
    std::set<int> antennasForScan;
    rownr_t scanNRows = 0;
    Record scanRec;
    _createSubScanRecords(scanRec, scanNRows, antennasForScan, scanKey, subScanProps);
    scanRec.define("nrows", (Int64)scanNRows);
    scanRec.define("antennas", Vector<int>(antennasForScan.begin(), antennasForScan.size(), 0));
    parent.defineRecord("scan=" + std::to_string(scanKey.scan), scanRec);
    ++scanIter;
  }
}

void MSMetaData::_createSubScanRecords(
    Record& parent, rownr_t& scanNRows, std::set<int>& antennasForScan, const ScanKey& scanKey,
    const std::map<SubScanKey, SubScanProperties>& subScanProps) const {
  std::set<SubScanKey> subScans = _getSubScanKeys(scanKey);
  std::set<SubScanKey>::const_iterator subScanIter = subScans.begin();
  std::set<SubScanKey>::const_iterator subScanEnd = subScans.end();
  while (subScanIter != subScanEnd) {
    Record subScanRec;
    SubScanProperties props = subScanProps.find(*subScanIter)->second;
    subScanRec.define("data description IDs",
                      Vector<int>(props.ddIDs.begin(), props.ddIDs.size(), 0));
    rownr_t nrows = props.acRows + props.xcRows;
    subScanRec.define("nrows", (Int64)nrows);
    scanNRows += nrows;
    subScanRec.define("antennas", Vector<int>(props.antennas.begin(), props.antennas.size(), 0));
    antennasForScan.insert(props.antennas.begin(), props.antennas.end());
    subScanRec.define("begin time", props.beginTime);
    subScanRec.define("end time", props.endTime);
    subScanRec.define("state IDs", Vector<int>(props.stateIDs.begin(), props.stateIDs.size(), 0));
    // subScanRec.define("field ID", subScanIter->fieldID);
    _createTimeStampRecords(subScanRec, props);
    parent.defineRecord("fieldID=" + std::to_string(subScanIter->fieldID), subScanRec);
    ++subScanIter;
  }
}

void MSMetaData::_createTimeStampRecords(Record& parent, const SubScanProperties& subScanProps) {
  std::map<double, TimeStampProperties>::const_iterator tpIter = subScanProps.timeProps.begin();
  std::map<double, TimeStampProperties>::const_iterator tpEnd = subScanProps.timeProps.end();
  unsigned int timeCount = 0;
  while (tpIter != tpEnd) {
    Record timeRec;
    timeRec.define("data description IDs",
                   Vector<int>(tpIter->second.ddIDs.begin(), tpIter->second.ddIDs.size(), 0));
    timeRec.define("nrows", (Int64)(tpIter->second.nrows));
    timeRec.define("time", tpIter->first);
    parent.defineRecord(std::to_string(timeCount), timeRec);
    ++tpIter;
    ++timeCount;
  }
}

std::set<double> MSMetaData::getTimesForIntent(const String& intent) const {
  if (!_hasIntent(intent)) {
    return std::set<double>();
  }
  std::map<String, std::set<double>> mymap = _getIntentsToTimesMap();
  if (mymap.find(intent) == mymap.end()) {
    return std::set<double>();
  } else {
    return mymap[intent];
  }
}

bool MSMetaData::hasBBCNo() const {
  return _ms->spectralWindow().isColumn(MSSpectralWindowEnums::BBC_NO);
}

std::map<String, std::set<double>> MSMetaData::_getIntentsToTimesMap() const {
  if (!_intentToTimesMap.empty()) {
    return _intentToTimesMap;
  }
  vector<std::set<String>> stateToIntentsMap;
  std::set<String> uniqueIntents;
  _getStateToIntentsMap(stateToIntentsMap, uniqueIntents);
  std::map<String, std::set<double>> mymap;
  if (uniqueIntents.empty()) {
    return mymap;
  }
  std::shared_ptr<Vector<int>> stateIDs = _getStateIDs();
  std::shared_ptr<Vector<double>> times = _getTimes();
  Vector<int>::const_iterator state = stateIDs->begin();
  Vector<double>::const_iterator time = times->begin();
  Vector<int>::const_iterator end = stateIDs->end();
  vector<std::set<double>> stateToTimes(nStates());
  while (state != end) {
    stateToTimes[*state].insert(*time);
    ++state;
    ++time;
  }
  vector<std::set<String>>::const_iterator intents = stateToIntentsMap.begin();
  vector<std::set<String>>::const_iterator endState = stateToIntentsMap.end();
  unsigned int count = 0;
  while (intents != endState) {
    std::set<String>::const_iterator intent = intents->begin();
    std::set<String>::const_iterator eintent = intents->end();
    while (intent != eintent) {
      if (mymap.find(*intent) == mymap.end()) {
        mymap[*intent] = std::set<double>();
      }
      std::set<double> times = stateToTimes[count];
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

vector<std::set<ScanKey>> MSMetaData::getFieldToScansMap() const {
  vector<std::set<ScanKey>> fieldToScansMap;
  std::map<ScanKey, std::set<int>> scanToFieldsMap;
  _getFieldsAndScansMaps(fieldToScansMap, scanToFieldsMap);
  return fieldToScansMap;
}

void MSMetaData::_getFieldsAndScansMaps(vector<std::set<ScanKey>>& fieldToScansMap,
                                        std::map<ScanKey, std::set<int>>& scanToFieldsMap) const {
  // This method is responsible for setting _fieldToScansMap and _scanToFieldsMap
  if (!_fieldToScansMap.empty() && !_scanToFieldsMap.empty()) {
    fieldToScansMap = _fieldToScansMap;
    scanToFieldsMap = _scanToFieldsMap;
    return;
  }
  scanToFieldsMap.clear();
  fieldToScansMap = vector<std::set<ScanKey>>(nFields());
  std::map<ScanKey, std::set<SubScanKey>> scanToSubScans = _getScanToSubScansMap();
  std::map<ScanKey, std::set<SubScanKey>>::const_iterator iter = scanToSubScans.begin();
  std::map<ScanKey, std::set<SubScanKey>>::const_iterator end = scanToSubScans.end();
  ScanKey scanKey;
  std::set<SubScanKey> subScanKeys;
  std::set<SubScanKey>::const_iterator subIter, subEnd;
  while (iter != end) {
    scanKey = iter->first;
    subScanKeys = iter->second;
    subIter = subScanKeys.begin();
    subEnd = subScanKeys.end();
    while (subIter != subEnd) {
      unsigned int fieldID = subIter->fieldID;
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

vector<Array<int>> MSMetaData::getCorrProducts() const {
  // responsible for setting _corrProds
  if (!_corrProds.empty()) {
    return _corrProds;
  }
  String colName = MSPolarization::columnName(MSPolarizationEnums::CORR_PRODUCT);
  ArrayColumn<int> col(_ms->polarization(), colName);
  unsigned int colSize = col.nrow();
  vector<Array<int>> contents(colSize);
  for (unsigned int i = 0; i < colSize; ++i) {
    contents[i] = col.get(i);
  }
  if (_cacheUpdated(_sizeof(contents))) {
    _corrProds = contents;
  }
  return contents;
}

vector<vector<int>> MSMetaData::getCorrTypes() const {
  // responsible for setting _corrTypes
  if (!_corrTypes.empty()) {
    return _corrTypes;
  }
  String colName = MSPolarization::columnName(MSPolarizationEnums::CORR_TYPE);
  ArrayColumn<int> col(_ms->polarization(), colName);
  unsigned int colSize = col.nrow();
  vector<vector<int>> contents(colSize);
  for (unsigned int i = 0; i < colSize; ++i) {
    contents[i] = col.get(i).tovector();
  }
  if (_cacheUpdated(_sizeof(contents))) {
    _corrTypes = contents;
  }
  return contents;
}

vector<int> MSMetaData::getNumCorrs() const {
  // responsible for setting _numCorrs
  if (!_numCorrs.empty()) {
    return _numCorrs;
  }
  String colName = MSPolarization::columnName(MSPolarization::NUM_CORR);
  ScalarColumn<int> col(_ms->polarization(), colName);
  vector<int> myvec = col.getColumn().tovector();
  if (_cacheUpdated(sizeof(myvec))) {
    _numCorrs = myvec;
  }
  return myvec;
}

vector<std::set<int>> MSMetaData::_getObservationIDToArrayIDsMap() const {
  // this method is responsible for setting _obsToArraysMap
  if (!_obsToArraysMap.empty()) {
    return _obsToArraysMap;
  }
  std::shared_ptr<Vector<int>> obsIDs = _getObservationIDs();
  std::shared_ptr<Vector<int>> arrayIDs = _getArrayIDs();
  Vector<int>::const_iterator oIter = obsIDs->begin();
  Vector<int>::const_iterator oEnd = obsIDs->end();
  Vector<int>::const_iterator aIter = arrayIDs->begin();
  vector<std::set<int>> mymap(nObservations());
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
  if (!_field_sourceIDs.empty()) {
    return _field_sourceIDs;
  }
  String colName = MSField::columnName(MSField::SOURCE_ID);
  ScalarColumn<int> col(_ms->field(), colName);
  vector<int> myvec = col.getColumn().tovector();
  if (_cacheUpdated(sizeof(myvec))) {
    _field_sourceIDs = myvec;
  }
  return myvec;
}

vector<MDirection> MSMetaData::getSourceDirections() const {
  // this method is responsible for setting _sourceDirs
  if (!_sourceDirs.empty()) {
    return _sourceDirs;
  }
  String colName = MSSource::columnName(MSSource::DIRECTION);
  ScalarMeasColumn<MDirection> col(_ms->source(), colName);
  unsigned int nrows = _ms->source().nrow();
  vector<MDirection> myvec(nrows);
  MDirection direction;
  for (unsigned int i = 0; i < nrows; ++i) {
    col.get(i, direction);
    myvec[i] = direction;
  }
  if (_cacheUpdated(sizeof(myvec))) {
    _sourceDirs = myvec;
  }
  return myvec;
}

std::shared_ptr<const Quantum<Vector<double>>> MSMetaData::getSourceTimes() const {
  if (_sourceTimes) {
    return _sourceTimes;
  }
  String colName = MSSource::columnName(MSSource::TIME);
  ScalarQuantColumn<double> time(_ms->source(), colName);
  std::shared_ptr<const Quantum<Vector<double>>> col = time.getColumn();
  if (_cacheUpdated(_sizeof(*col))) {
    _sourceTimes = col;
  }
  return col;
}

map<SourceKey, MSMetaData::SourceProperties> MSMetaData::_getSourceInfo() const {
  // this method is responsible for setting _sourceInfo
  if (!_sourceInfo.empty()) {
    return _sourceInfo;
  }
  auto colName = MSSource::columnName(MSSource::SOURCE_ID);
  ScalarColumn<int> id(_ms->source(), colName);
  colName = MSSource::columnName(MSSource::SPECTRAL_WINDOW_ID);
  ScalarColumn<int> spw(_ms->source(), colName);
  colName = MSSource::columnName(MSSource::NAME);
  ScalarColumn<String> name(_ms->source(), colName);
  colName = MSSource::columnName(MSSource::REST_FREQUENCY);
  ArrayMeasColumn<MFrequency> restfreq(_ms->source(), colName);
  colName = MSSource::columnName(MSSource::TRANSITION);
  ArrayColumn<String> transition(_ms->source(), colName);
  map<SourceKey, SourceProperties> mymap;
  unsigned int nrows = _ms->source().nrow();
  Array<MFrequency> rf;
  SourceKey key;
  SourceProperties props;
  static const Unit emptyUnit;
  static const Unit hz("Hz");
  for (unsigned int i = 0; i < nrows; ++i) {
    key.id = id(i);
    key.spw = spw(i);
    props.name = name(i);
    if (restfreq.isDefined(i)) {
      // resize=True because the array lengths may differ
      // from cell to cell, CAS-10409
      restfreq.get(i, rf, true);
      props.restfreq = std::make_shared<std::vector<MFrequency>>(rf.tovector());
    } else {
      props.restfreq.reset();
    }
    if (transition.isDefined(i)) {
      props.transition = std::make_shared<std::vector<String>>(transition(i).tovector());
    } else {
      props.transition.reset();
    }
    mymap[key] = props;
  }
  ThrowIf(mymap.size() < nrows, "Too few source keys found, there are duplicate SOURCE table keys");
  // this is a reasonable approximation for now
  auto mysize = nrows * (2 * sizeof(unsigned int) + sizeof(double) + 30);
  if (_cacheUpdated(mysize)) {
    _sourceInfo = mymap;
  }
  return mymap;
}

map<SourceKey, std::shared_ptr<vector<MFrequency>>> MSMetaData::getRestFrequencies() const {
  auto mymap = _getSourceInfo();
  map<SourceKey, std::shared_ptr<vector<MFrequency>>> ret;
  for_each(mymap.cbegin(), mymap.cend(), [&ret](const std::pair<SourceKey, SourceProperties>& val) {
    ret[val.first] = val.second.restfreq;
  });
  return ret;
}

map<SourceKey, std::shared_ptr<vector<String>>> MSMetaData::getTransitions() const {
  map<SourceKey, SourceProperties> mymap = _getSourceInfo();
  map<SourceKey, std::shared_ptr<vector<String>>> ret;
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
  if (!_sourceNames.empty()) {
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
  if (!_source_sourceIDs.empty()) {
    return _source_sourceIDs;
  }
  String colName = MSSource::columnName(MSSource::SOURCE_ID);
  ScalarColumn<int> col(_ms->source(), colName);
  vector<int> myvec = col.getColumn().tovector();
  if (_cacheUpdated(sizeof(myvec))) {
    _source_sourceIDs = myvec;
  }
  return myvec;
}

unsigned int MSMetaData::nUniqueSourceIDsFromSourceTable() const {
  String colName = MSSource::columnName(MSSource::SOURCE_ID);
  ScalarColumn<int> col(_ms->source(), colName);
  Vector<int> myvec = col.getColumn();
  std::set<int> myset(myvec.begin(), myvec.end());
  return myset.size();
}

bool MSMetaData::_hasIntent(const String& intent) const {
  std::set<String> uniqueIntents = getIntents();
  return uniqueIntents.find(intent) != uniqueIntents.end();
}

vector<String> MSMetaData::getFieldNamesForFieldIDs(const vector<unsigned int>& fieldIDs) {
  if (fieldIDs.size() == 0) {
    return getFieldNames();
  }
  // Do not use _checkFieldIDs since fieldIDs that may not be in the
  // main table can be valid. CAS-5168
  unsigned int max = *max_element(fieldIDs.begin(), fieldIDs.end());
  unsigned int nField = nFields();
  if (max >= nField) {
    std::ostringstream os;
    os << "MSMetaData::" << __FUNCTION__ << ": This MS only has " << nField
       << " fields so requested field number " << max << " does not exist";
    throw AipsError(os.str());
  }
  vector<String> allNames = getFieldNames();
  vector<String> names;
  vector<unsigned int>::const_iterator end = fieldIDs.end();
  for (vector<unsigned int>::const_iterator iter = fieldIDs.begin(); iter != end; ++iter) {
    names.push_back(allNames[*iter]);
  }
  return names;
}

std::set<int> MSMetaData::getFieldsForTimes(const double center, const double tol) {
  _checkTolerance(tol);
  double minTime = center - tol;
  double maxTime = center + tol;
  std::shared_ptr<std::map<int, std::set<double>>> fieldToTimesMap;
  std::shared_ptr<std::map<double, std::set<int>>> timeToFieldsMap;
  _getFieldsAndTimesMaps(fieldToTimesMap, timeToFieldsMap);
  std::set<int> fields;
  std::map<double, std::set<int>>::const_iterator end = timeToFieldsMap->end();
  // A std::set is always ordered.
  // FIXME could do a binary search to make this faster
  for (std::map<double, std::set<int>>::const_iterator iter = timeToFieldsMap->begin(); iter != end;
       ++iter) {
    double curTime = iter->first;
    if (curTime >= minTime) {
      std::set<int> curFields = iter->second;
      fields.insert(curFields.begin(), curFields.end());
    }
    if (curTime > maxTime) {
      break;
    }
  }
  return fields;
}

void MSMetaData::_checkTolerance(const double tol) {
  ThrowIf(tol < 0, "Tolerance cannot be less than zero");
}

void MSMetaData::_getFieldsAndTimesMaps(
    std::shared_ptr<std::map<int, std::set<double>>>& fieldToTimesMap,
    std::shared_ptr<std::map<double, std::set<int>>>& timeToFieldsMap) {
  // This method is responsible for setting _fieldToTimesMap and _timeToFieldMap
  if (_fieldToTimesMap && !_fieldToTimesMap->empty() && _timeToFieldsMap &&
      !_timeToFieldsMap->empty()) {
    fieldToTimesMap = _fieldToTimesMap;
    timeToFieldsMap = _timeToFieldsMap;
    return;
  }
  fieldToTimesMap = std::make_shared<std::map<int, std::set<double>>>();
  timeToFieldsMap = std::make_shared<std::map<double, std::set<int>>>();
  std::shared_ptr<Vector<int>> allFields = _getFieldIDs();
  std::shared_ptr<Vector<double>> allTimes = this->_getTimes();
  Vector<int>::const_iterator lastField = allFields->end();
  Vector<double>::const_iterator curTime = allTimes->begin();
  for (Vector<int>::const_iterator curField = allFields->begin(); curField != lastField;
       ++curField, ++curTime) {
    (*fieldToTimesMap)[*curField].insert(*curTime);
    (*timeToFieldsMap)[*curTime].insert(*curField);
  }
  if (_cacheUpdated(_sizeof(*fieldToTimesMap) + _sizeof(*timeToFieldsMap))) {
    _fieldToTimesMap = fieldToTimesMap;
    _timeToFieldsMap = timeToFieldsMap;
  }
}

std::set<double> MSMetaData::getTimesForField(const int fieldID) {
  if (!_hasFieldID(fieldID)) {
    return std::set<double>();
  }
  std::shared_ptr<std::map<int, std::set<double>>> fieldToTimesMap;
  std::shared_ptr<std::map<double, std::set<int>>> timeToFieldsMap;
  _getFieldsAndTimesMaps(fieldToTimesMap, timeToFieldsMap);
  return (*fieldToTimesMap)[fieldID];
}

vector<String> MSMetaData::getObservers() const {
  if (!_observers.empty()) {
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
  if (!_observatoryNames.empty()) {
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
  if (!_projects.empty()) {
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

vector<vector<String>> MSMetaData::getSchedules() const {
  // responsible for setting _schedules
  if (!_schedules.empty()) {
    return _schedules;
  }
  String colName = MSObservation::columnName(MSObservationEnums::SCHEDULE);
  ArrayColumn<String> col(_ms->observation(), colName);
  unsigned int colSize = col.nrow();
  vector<vector<String>> contents(colSize);
  for (unsigned int i = 0; i < colSize; ++i) {
    contents[i] = col.get(i).tovector();
  }
  if (_cacheUpdated(_sizeof(contents))) {
    _schedules = contents;
  }
  return contents;
}

vector<std::pair<MEpoch, MEpoch>> MSMetaData::getTimeRangesOfObservations() const {
  if (!_timeRangesForObs.empty()) {
    return _timeRangesForObs;
  }
  String colName = MSObservation::columnName(MSObservationEnums::TIME_RANGE);
  ArrayColumn<double> col(_ms->observation(), colName);
  TableRecord kv = col.keywordSet();
  String unit = kv.asArrayString("QuantumUnits").tovector()[0];
  MEpoch::Types myRF;
  MEpoch::getType(myRF, kv.asRecord("MEASINFO").asString("Ref"));
  unsigned int n = col.nrow();
  vector<std::pair<MEpoch, MEpoch>> contents(n);
  for (unsigned int i = 0; i < n; ++i) {
    Vector<double> row = col.get(i);
    Quantity begin(row[0], unit);
    Quantity end(row[1], unit);
    contents[i] = std::pair<MEpoch, MEpoch>(MEpoch(begin, myRF), MEpoch(end, myRF));
  }
  if (_cacheUpdated(_sizeof(contents))) {
    _timeRangesForObs = contents;
  }
  return contents;
}

MPosition MSMetaData::getObservatoryPosition(unsigned int which) const {
  if (which >= _ms->observation().nrow()) {
    throw AipsError(_ORIGIN + " out of range exception.");
  }
  if (!_observatoryPositions.empty()) {
    return _observatoryPositions[which];
  }
  String tnameColName = MSObservation::columnName(MSObservationEnums::TELESCOPE_NAME);
  ScalarColumn<String> telescopeNameCol(_ms->observation(), tnameColName);
  vector<String> names = telescopeNameCol.getColumn().tovector();
  vector<MPosition> observatoryPositions(names.size());
  for (unsigned int i = 0; i < observatoryPositions.size(); ++i) {
    ThrowIf(names[i].empty(), "The name of the telescope is not stored in the measurement set.");
    ThrowIf(!MeasTable::Observatory(observatoryPositions[i], names[i]),
            "Telescope " + names[i] + " is not recognized by CASA");
  }
  if (_cacheUpdated(30 * observatoryPositions.size())) {
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
    unsigned int nrows = nFields();
    for (unsigned int i = 0; i < nrows; ++i) {
      myDirs.push_back(phaseDirCol(i));
    }
    if (_cacheUpdated(_sizeof(myDirs))) {
      _phaseDirs = myDirs;
    }
  } else {
    myDirs = _phaseDirs;
  }
  // get the correct directions for ephemeris objects and put them
  // in the vector
  std::shared_ptr<std::set<int>> ephems = _getEphemFieldIDs();
  std::set<int>::const_iterator iter = ephems->begin();
  std::set<int>::const_iterator end = ephems->end();
  for (; iter != end; ++iter) {
    myDirs[*iter] = phaseDirFromFieldIDAndTime(*iter, ep);
  }
  return myDirs;
}

vector<MPosition> MSMetaData::_getAntennaPositions() const {
  // This method is responsible for setting _antennaPositions
  if (!_antennaPositions.empty()) {
    return _antennaPositions;
  }
  String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
  ScalarColumn<String> nameCol(_ms->antenna(), antNameColName);
  String antPosColName = MSAntenna::columnName(MSAntennaEnums::POSITION);
  ArrayColumn<double> posCol(_ms->antenna(), antPosColName);
  Array<double> xyz = posCol.getColumn();
  Vector<String> posUnits = posCol.keywordSet().asArrayString("QuantumUnits");
  String sFrame = posCol.keywordSet().asRecord("MEASINFO").asString("Ref");
  MPosition::Types posType = MPosition::getType(sFrame);
  Array<double>::const_iterator end = xyz.end();
  Quantity x(0, posUnits[0]);
  Quantity y(0, posUnits[1]);
  Quantity z(0, posUnits[2]);
  vector<MPosition> antennaPositions;
  for (Array<double>::const_iterator iter = xyz.begin(); iter != end; ++iter) {
    x.setValue(*iter);
    double xm = x.getValue("m");
    ++iter;
    y.setValue(*iter);
    double ym = y.getValue("m");
    ++iter;
    z.setValue(*iter);
    double zm = z.getValue("m");
    MPosition antPos(MVPosition(xm, ym, zm), posType);
    antennaPositions.push_back(antPos);
  }
  if (_cacheUpdated(30 * antennaPositions.size())) {
    _antennaPositions = antennaPositions;
  }
  return antennaPositions;
}

vector<MPosition> MSMetaData::getAntennaPositions(const vector<unsigned int>& which) const {
  vector<MPosition> allPos = _getAntennaPositions();
  if (which.empty()) {
    return allPos;
  }
  ThrowIf(max(Vector<unsigned int>(which)) >= nAntennas(), "Antenna ID out of range");
  vector<MPosition> output;
  vector<unsigned int>::const_iterator end = which.end();
  for (vector<unsigned int>::const_iterator iter = which.begin(); iter != end; ++iter) {
    output.push_back(allPos[*iter]);
  }
  return output;
}

vector<vector<MPosition>> MSMetaData::getAntennaPositions(const vector<String>& names) {
  ThrowIf(names.empty(), _ORIGIN + "names cannot be empty");
  vector<std::set<unsigned int>> ids = getAntennaIDs(names);
  vector<std::set<unsigned int>>::const_iterator iter = ids.begin();
  vector<std::set<unsigned int>>::const_iterator end = ids.end();
  vector<vector<MPosition>> pos;
  for (; iter != end; ++iter) {
    std::vector<MPosition> mypos;
    std::set<unsigned int>::const_iterator siter = iter->begin();
    std::set<unsigned int>::const_iterator send = iter->end();
    for (; siter != send; ++siter) {
      mypos.push_back(getAntennaPositions(vector<unsigned int>(1, *siter))[0]);
    }
    pos.push_back(mypos);
  }
  return pos;
}

QVD MSMetaData::getAntennaOffset(unsigned int which) const {
  ThrowIf(which >= nAntennas(), "Out of range exception.");
  return getAntennaOffsets()[which];
}

vector<QVD> MSMetaData::getAntennaOffsets() const {
  // This method is responsble for setting _antennaOffsets
  if (!_antennaOffsets.empty()) {
    return _antennaOffsets;
  }
  MPosition obsPos = getObservatoryPosition(0);
  if (obsPos.getRef().getType() != MPosition::ITRF) {
    MeasConvert<MPosition> toItrf(obsPos, MPosition::ITRF);
    obsPos = toItrf(obsPos);
  }
  Vector<double> obsXYZ = obsPos.get("m").getValue();
  double xo = obsXYZ[0];
  double yo = obsXYZ[1];
  double zo = obsXYZ[2];
  double rObs = sqrt(xo * xo + yo * yo + zo * zo);
  Vector<double> obsLongLat = obsPos.getAngle("rad").getValue();
  double longObs = obsLongLat[0];
  double latObs = obsLongLat[1];
  vector<MPosition> antennaPositions = _getAntennaPositions();
  vector<MPosition>::const_iterator end = antennaPositions.end();
  vector<QVD> antennaOffsets;
  for (vector<MPosition>::const_iterator iter = antennaPositions.begin(); iter != end; ++iter) {
    Vector<double> xyz = iter->get("m").getValue();
    double x = xyz[0];
    double y = xyz[1];
    double z = xyz[2];
    double rAnt = sqrt(x * x + y * y + z * z);
    Vector<double> antLongLat = iter->getAngle("rad").getValue();
    double longAnt = antLongLat[0];
    double latAnt = antLongLat[1];
    Vector<double> offset(3);
    offset[0] = (longAnt - longObs) * rObs * cos(latObs);
    offset[1] = (latAnt - latObs) * rObs;
    offset[2] = rAnt - rObs;
    QVD qoffset(offset, "m");
    antennaOffsets.push_back(qoffset);
  }
  if (_cacheUpdated(30 * antennaOffsets.size())) {
    _antennaOffsets = antennaOffsets;
  }
  return antennaOffsets;
}

unsigned int MSMetaData::nBaselines(bool includeAutoCorrelation) {
  Matrix<bool> baselines = getUniqueBaselines().copy();
  unsigned int ac = 0;
  unsigned int nrows = baselines.nrow();
  for (unsigned int i = 0; i < nrows; ++i) {
    if (includeAutoCorrelation && baselines(i, i)) {
      // count autocorrelations separately from cross correlations
      ++ac;
    }
    baselines(i, i) = false;
  }
  return ntrue(baselines) / 2 + ac;
}

Matrix<bool> MSMetaData::getUniqueBaselines() {
  if (!_uniqueBaselines.empty()) {
    return _uniqueBaselines;
  }
  std::shared_ptr<Vector<int>> ant1, ant2;
  _getAntennas(ant1, ant2);

  Vector<int>::const_iterator a1Iter = ant1->begin();
  Vector<int>::const_iterator a2Iter = ant2->begin();
  Vector<int>::const_iterator end = ant1->end();
  unsigned int nAnts = nAntennas();
  Matrix<bool> baselines(nAnts, nAnts, false);
  while (a1Iter != end) {
    baselines(*a1Iter, *a2Iter) = true;
    baselines(*a2Iter, *a1Iter) = true;
    ++a1Iter;
    ++a2Iter;
  }
  if (_cacheUpdated(sizeof(bool) * baselines.size())) {
    _uniqueBaselines = baselines;
  }
  return baselines;
}

QVD MSMetaData::getAntennaOffset(const String& name) const { return getAntennaOffsets(name)[0]; }

std::vector<QVD> MSMetaData::getAntennaOffsets(const String& name) const {
  std::set<unsigned int> ids = getAntennaIDs(name);
  std::vector<QVD> offsets;
  std::set<unsigned int>::const_iterator iter = ids.begin();
  std::set<unsigned int>::const_iterator end = ids.end();
  for (; iter != end; ++iter) {
    offsets.push_back(getAntennaOffset(*iter));
  }
  return offsets;
}

Quantity MSMetaData::getEffectiveTotalExposureTime() {
  // This method has the responsibility of setting _exposureTime.
  if (_exposureTime.getValue() > 0) {
    return _exposureTime;
  }
  unsigned int nAnts = nAntennas();
  unsigned int maxNBaselines = nAnts * (nAnts - 1) / 2;
  double totalExposure = 0;
  String taql = "select FLAG, DATA_DESC_ID, EXPOSURE, TIME from " + _ms->tableName() +
                " where ANTENNA1 != ANTENNA2";
  Table result(tableCommand(taql).table());
  Vector<int> ddIDs = ScalarColumn<int>(result, "DATA_DESC_ID").getColumn();
  Vector<double> exposures = ScalarColumn<double>(result, "EXPOSURE").getColumn();
  Vector<double> times = ScalarColumn<double>(result, "TIME").getColumn();
  // each row represents a unique baseline, data description ID, and time combination
  unsigned int nrows = result.nrow();
  vector<unsigned int> dataDescToSpwIdMap = getDataDescIDToSpwMap();
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  std::map<double, double> timeToBWMap = _getTimeToTotalBWMap(times, ddIDs);
  for (unsigned int i = 0; i < nrows; ++i) {
    unsigned int ddID = ddIDs[i];
    unsigned int spw = dataDescToSpwIdMap[ddID];
    QVD channelWidths = spwInfo[spw].chanwidths;
    Matrix<bool> flagsMatrix(ArrayColumn<bool>(result, "FLAG").get(i));
    unsigned int nCorrelations = flagsMatrix.nrow();
    double denom = (timeToBWMap.find(times[i])->second) * maxNBaselines * nCorrelations;
    for (unsigned int corr = 0; corr < nCorrelations; ++corr) {
      Vector<bool> goodData = !flagsMatrix.row(corr);
      if (anyTrue(goodData)) {
        MaskedArray<double> flaggedChannelWidths(channelWidths.getValue("Hz"), goodData, true);
        double effectiveBW = sum(flaggedChannelWidths);
        totalExposure += exposures[i] * effectiveBW / denom;
      }
    }
  }
  String unit = ScalarColumn<double>(*_ms, "EXPOSURE")
                    .keywordSet()
                    .asArrayString("QuantumUnits")
                    .tovector()[0];
  Quantity eTime(totalExposure, unit);
  if (_cacheUpdated(10)) {
    _exposureTime = eTime;
  }
  return eTime;
}

MSMetaData::SubScanProperties MSMetaData::getSubScanProperties(const SubScanKey& subScan,
                                                               bool showProgress) const {
  _checkSubScan(subScan);
  return getSubScanProperties(showProgress)->find(subScan)->second;
}

void MSMetaData::_getScalarIntColumn(Vector<int>& v, TableProxy& tp, const String& colname,
                                     rownr_t beginRow, rownr_t nrows) {
  v = tp.getColumn(colname, beginRow, nrows, 1).asArrayInt();
}

void MSMetaData::_getScalarDoubleColumn(Vector<double>& v, TableProxy& tp, const String& colname,
                                        rownr_t beginRow, rownr_t nrows) {
  v = tp.getColumn(colname, beginRow, nrows, 1).asArrayDouble();
}

void MSMetaData::_getScalarQuantDoubleColumn(Quantum<Vector<double>>& v, TableProxy& tp,
                                             const String& colname, rownr_t beginRow,
                                             rownr_t nrows) {
  ScalarQuantColumn<double> mycol(tp.table(), colname);
  v = Quantum<Vector<double>>(tp.getColumn(colname, beginRow, nrows, 1).asArrayDouble(),
                              mycol.getUnits());
}

void MSMetaData::_computeScanAndSubScanProperties(
    std::shared_ptr<std::map<ScanKey, MSMetaData::ScanProperties>>& scanProps,
    std::shared_ptr<std::map<SubScanKey, MSMetaData::SubScanProperties>>& subScanProps,
    bool showProgress) const {
  std::shared_ptr<ProgressMeter> pm;
  if (showProgress || _showProgress) {
    LogIO log;
    const static String title = "Computing scan and subscan properties...";
    log << LogOrigin("MSMetaData", __func__, WHERE) << LogIO::NORMAL << title << LogIO::POST;
    pm = std::make_shared<ProgressMeter>(0, _ms->nrow(), title);
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
  std::vector<pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties>>> props;
  std::vector<unsigned int> ddIDToSpw = getDataDescIDToSpwMap();
  scanProps = std::make_shared<std::map<ScanKey, ScanProperties>>();
  subScanProps = std::make_shared<std::map<SubScanKey, SubScanProperties>>();
  rownr_t doneRows = 0;
  rownr_t msRows = _ms->nrow();
  static const rownr_t rowsInChunk = 10000000;
  for (rownr_t row = 0; row < msRows; row += rowsInChunk) {
    rownr_t nrows = min(rowsInChunk, msRows - row);
    Vector<int> scans, fields, ddIDs, states, arrays, observations, ant1, ant2;
    _getScalarIntColumn(scans, tp, scanName, row, nrows);
    _getScalarIntColumn(fields, tp, fieldName, row, nrows);
    _getScalarIntColumn(ddIDs, tp, ddidName, row, nrows);
    _getScalarIntColumn(states, tp, stateName, row, nrows);
    _getScalarIntColumn(arrays, tp, arrayName, row, nrows);
    _getScalarIntColumn(observations, tp, obsName, row, nrows);
    _getScalarIntColumn(ant1, tp, ant1Name, row, nrows);
    _getScalarIntColumn(ant2, tp, ant2Name, row, nrows);
    Vector<double> times;
    _getScalarDoubleColumn(times, tp, timeName, row, nrows);
    Quantum<Vector<double>> exposureTimes, intervalTimes;
    _getScalarQuantDoubleColumn(exposureTimes, tp, exposureName, row, nrows);
    _getScalarQuantDoubleColumn(intervalTimes, tp, intervalName, row, nrows);
    rownr_t nchunks = min((rownr_t)1000, nrows);
    rownr_t chunkSize = nrows / nchunks;
    if (nrows % nchunks > 0) {
      // integer division
      nchunks = nrows / chunkSize + 1;
    }
    pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties>>* fut =
        new pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties>>[nchunks];
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (unsigned int i = 0; i < nchunks; ++i) {
      fut[i] = _getChunkSubScanProperties(scans, fields, ddIDs, states, times, arrays, observations,
                                          ant1, ant2, exposureTimes, intervalTimes, ddIDToSpw,
                                          i * chunkSize, std::min((i + 1) * chunkSize, nrows));
    }
    props.insert(props.begin(), fut, fut + nchunks);
    delete[] fut;
    if (pm) {
      doneRows += nrows;
      pm->update(doneRows);
    }
  }
  _mergeScanProps(scanProps, subScanProps, props);
}

void MSMetaData::_mergeScanProps(
    std::shared_ptr<std::map<ScanKey, MSMetaData::ScanProperties>>& scanProps,
    std::shared_ptr<std::map<SubScanKey, MSMetaData::SubScanProperties>>& subScanProps,
    const std::vector<pair<map<ScanKey, ScanProperties>, map<SubScanKey, SubScanProperties>>>&
        props) const {
  scanProps = std::make_shared<std::map<ScanKey, ScanProperties>>();
  subScanProps = std::make_shared<std::map<SubScanKey, SubScanProperties>>();
  map<SubScanKey, map<unsigned int, Quantity>> ssSumInterval;
  unsigned int nTotChunks = props.size();
  for (unsigned int i = 0; i < nTotChunks; ++i) {
    const map<ScanKey, ScanProperties>& vScanProps = props[i].first;
    map<ScanKey, ScanProperties>::const_iterator viter = vScanProps.begin();
    map<ScanKey, ScanProperties>::const_iterator vend = vScanProps.end();
    for (; viter != vend; ++viter) {
      // iterate over scans in this chunk
      const ScanKey& scanKey = viter->first;
      const std::pair<double, double>& range = viter->second.timeRange;
      if (scanProps->find(scanKey) == scanProps->end()) {
        (*scanProps)[scanKey].timeRange = range;
      } else {
        std::pair<double, double>& tr = (*scanProps)[scanKey].timeRange;
        tr.first = min(tr.first, range.first);
        tr.second = max(tr.second, range.second);
      }
      std::map<unsigned int, rownr_t>::const_iterator spnIter = viter->second.spwNRows.begin();
      std::map<unsigned int, rownr_t>::const_iterator spnEnd = viter->second.spwNRows.end();
      for (; spnIter != spnEnd; ++spnIter) {
        const unsigned int& spw = spnIter->first;
        const std::set<double> scanTimes = viter->second.times.find(spw)->second;
        if ((*scanProps)[scanKey].spwNRows.find(spw) == (*scanProps)[scanKey].spwNRows.end()) {
          (*scanProps)[scanKey].spwNRows[spw] = spnIter->second;
          (*scanProps)[scanKey].times[spw] = scanTimes;
        } else {
          (*scanProps)[scanKey].spwNRows[spw] += spnIter->second;
          (*scanProps)[scanKey].times[spw].insert(scanTimes.begin(), scanTimes.end());
        }
        const std::set<double> mytimes = vScanProps.find(scanKey)->second.times.find(spw)->second;
        (*scanProps)[scanKey].times[spw].insert(mytimes.begin(), mytimes.end());
      }
    }
    map<SubScanKey, SubScanProperties>::const_iterator ssIter = props[i].second.begin();
    map<SubScanKey, SubScanProperties>::const_iterator ssEnd = props[i].second.end();
    for (; ssIter != ssEnd; ++ssIter) {
      // iterate over subscans in this chunk
      const SubScanKey& ssKey = ssIter->first;
      const SubScanProperties& val = ssIter->second;
      if (subScanProps->find(ssKey) == subScanProps->end()) {
        // first time this subscan has been seen in any chunks
        (*subScanProps)[ssKey] = val;
        map<unsigned int, Quantity>::const_iterator mi = val.meanInterval.begin();
        map<unsigned int, Quantity>::const_iterator me = val.meanInterval.end();
        for (; mi != me; ++mi) {
          const unsigned int& spw = mi->first;
          ssSumInterval[ssKey][spw] = mi->second * Quantity(val.spwNRows.find(spw)->second);
        }
      } else {
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
        fp.meanExposureTime =
            (fp.meanExposureTime * Quantity(nrows - 1) + val.meanExposureTime) / nrows;
        fp.stateIDs.insert(val.stateIDs.begin(), val.stateIDs.end());
        fp.spws.insert(val.spws.begin(), val.spws.end());

        std::set<unsigned int>::const_iterator spwIter = val.spws.begin();
        std::set<unsigned int>::const_iterator spwEnd = val.spws.end();
        for (; spwIter != spwEnd; ++spwIter) {
          const unsigned int& spw = *spwIter;
          rownr_t vSpwNRows = val.spwNRows.find(spw)->second;
          fp.spwNRows[spw] += vSpwNRows;
          const Quantity& vMeanInt = val.meanInterval.find(spw)->second;
          if (ssSumInterval[ssKey].find(spw) == ssSumInterval[ssKey].end()) {
            ssSumInterval[ssKey][spw] = vMeanInt * Quantity(vSpwNRows, "");
          } else {
            ssSumInterval[ssKey][spw] += vMeanInt * Quantity(vSpwNRows, "");
          }
        }
        map<double, TimeStampProperties>::const_iterator tpIter = val.timeProps.begin();
        map<double, TimeStampProperties>::const_iterator tpEnd = val.timeProps.end();
        for (; tpIter != tpEnd; ++tpIter) {
          double time = tpIter->first;
          const TimeStampProperties& tprops = tpIter->second;
          if (fp.timeProps.find(time) == fp.timeProps.end()) {
            fp.timeProps[time] = tprops;
          } else {
            fp.timeProps[time].ddIDs.insert(tprops.ddIDs.begin(), tprops.ddIDs.end());
            fp.timeProps[time].nrows += tprops.nrows;
          }
        }
        _modifyFirstExposureTimeIfNecessary(fp.firstExposureTime, val.firstExposureTime);
      }
    }
  }
  map<ScanKey, map<unsigned int, Quantity>> scanSumInterval;
  map<SubScanKey, SubScanProperties>::iterator ssIter = subScanProps->begin();
  map<SubScanKey, SubScanProperties>::iterator ssEnd = subScanProps->end();
  for (; ssIter != ssEnd; ++ssIter) {
    const SubScanKey& ssKey = ssIter->first;
    SubScanProperties& props = ssIter->second;
    map<unsigned int, rownr_t>::const_iterator ssSpwNRowsIter = props.spwNRows.begin();
    map<unsigned int, rownr_t>::const_iterator ssSpwNRowsEnd = props.spwNRows.end();
    for (; ssSpwNRowsIter != ssSpwNRowsEnd; ++ssSpwNRowsIter) {
      const unsigned int& spw = ssSpwNRowsIter->first;
      props.meanInterval[spw] = ssSumInterval[ssKey][spw] / ssSpwNRowsIter->second;
    }
    const ScanKey scanKey = casacore::scanKey(ssKey);
    if (scanSumInterval.find(scanKey) == scanSumInterval.end()) {
      // first time associated scan key has been seen
      scanSumInterval[scanKey] = ssSumInterval[ssKey];
      (*scanProps)[scanKey].firstExposureTime = props.firstExposureTime;
    } else {
      map<unsigned int, Quantity>::const_iterator spwSumIter = ssSumInterval[ssKey].begin();
      map<unsigned int, Quantity>::const_iterator spwSumEnd = ssSumInterval[ssKey].end();
      for (; spwSumIter != spwSumEnd; ++spwSumIter) {
        const unsigned int& spw = spwSumIter->first;
        if (scanSumInterval[scanKey].find(spw) == scanSumInterval[scanKey].end()) {
          scanSumInterval[scanKey][spw] = spwSumIter->second;
        } else {
          scanSumInterval[scanKey][spw] += spwSumIter->second;
        }
      }
      _modifyFirstExposureTimeIfNecessary((*scanProps)[scanKey].firstExposureTime,
                                          ssIter->second.firstExposureTime);
    }
  }
  map<ScanKey, ScanProperties>::iterator scanPropsIter = scanProps->begin();
  map<ScanKey, ScanProperties>::iterator scanPropsEnd = scanProps->end();
  for (; scanPropsIter != scanPropsEnd; ++scanPropsIter) {
    const ScanKey& scanKey = scanPropsIter->first;
    map<unsigned int, rownr_t>::const_iterator scanSpwNRowsIter = scanPropsIter->second.spwNRows.begin();
    map<unsigned int, rownr_t>::const_iterator scanSpwNRowsEnd = scanPropsIter->second.spwNRows.end();
    for (; scanSpwNRowsIter != scanSpwNRowsEnd; ++scanSpwNRowsIter) {
      const unsigned int& spw = scanSpwNRowsIter->first;
      const rownr_t& nrows = scanSpwNRowsIter->second;
      scanPropsIter->second.meanInterval[spw] = scanSumInterval[scanKey][spw] / nrows;
    }
  }
}

void MSMetaData::_modifyFirstExposureTimeIfNecessary(FirstExposureTimeMap& current,
                                                     const FirstExposureTimeMap& test) {
  // deal with first exposure time maps
  FirstExposureTimeMap::const_iterator feiter = test.begin();
  FirstExposureTimeMap::const_iterator feend = test.end();
  for (; feiter != feend; ++feiter) {
    int ddID = feiter->first;
    double timestamp = feiter->second.first;
    // could be implemented in terms of overloaded _modifyFirstExposureTimeIfNecessary,
    // but would require decomposing the exposure quantity into value and unit. Since
    // this is called for every row in the main table, that may impact performance so
    // I've opted to leave it as is.
    if (current.find(ddID) == current.end() || timestamp < current[ddID].first) {
      // dd ID not yet encountered yet
      // or the first time stamp for this ddID is
      // before what has yet been encountered
      current[ddID] = feiter->second;
    }
  }
}

void MSMetaData::_modifyFirstExposureTimeIfNecessary(FirstExposureTimeMap& current, int dataDescID,
                                                     double time, double exposure,
                                                     const Unit& eunit) {
  if (current.find(dataDescID) == current.end()) {
    // the data description ID for this sub scan has not yet been
    // encountered in this chunk
    current[dataDescID].first = time;
    current[dataDescID].second = Quantity(exposure, eunit);
  } else if (time < current[dataDescID].first) {
    current[dataDescID].first = time;
    // unit is already set from first time, so only need to reset value
    current[dataDescID].second.setValue(exposure);
  }
}

pair<map<ScanKey, MSMetaData::ScanProperties>, map<SubScanKey, MSMetaData::SubScanProperties>>
MSMetaData::_getChunkSubScanProperties(
    const Vector<int>& scans, const Vector<int>& fields, const Vector<int>& ddIDs,
    const Vector<int>& states, const Vector<double>& times, const Vector<int>& arrays,
    const Vector<int>& observations, const Vector<int>& ant1, const Vector<int>& ant2,
    const Quantum<Vector<double>>& exposureTimes, const Quantum<Vector<double>>& intervalTimes,
    const vector<unsigned int>& ddIDToSpw, rownr_t beginRow, rownr_t endRow) const {
  VectorSTLIterator<int> scanIter(scans);
  scanIter += beginRow;
  VectorSTLIterator<int> a1Iter(ant1);
  a1Iter += beginRow;
  VectorSTLIterator<int> a2Iter(ant2);
  a2Iter += beginRow;
  VectorSTLIterator<int> fIter(fields);
  fIter += beginRow;
  VectorSTLIterator<int> dIter(ddIDs);
  dIter += beginRow;
  VectorSTLIterator<int> stateIter(states);
  stateIter += beginRow;
  VectorSTLIterator<int> oIter(observations);
  oIter += beginRow;
  VectorSTLIterator<int> arIter(arrays);
  arIter += beginRow;
  VectorSTLIterator<double> tIter(times);
  tIter += beginRow;
  VectorSTLIterator<double> eiter(exposureTimes.getValue());
  eiter += beginRow;
  VectorSTLIterator<double> iIter(intervalTimes.getValue());
  iIter += beginRow;
  map<ScanKey, ScanProperties> scanProps;
  map<SubScanKey, SubScanProperties> mysubscans;
  map<SubScanKey, double> exposureSum;
  map<pair<SubScanKey, unsigned int>, double> intervalSum;
  ScanKey scanKey;
  SubScanKey subScanKey;
  pair<SubScanKey, unsigned int> subScanSpw;
  rownr_t row = beginRow;
  const Unit& eunit = exposureTimes.getFullUnit();
  while (row < endRow) {
    scanKey.obsID = *oIter;
    scanKey.arrayID = *arIter;
    scanKey.scan = *scanIter;
    double half = *iIter / 2;
    unsigned int spw = ddIDToSpw[*dIter];
    if (scanProps.find(scanKey) == scanProps.end()) {
      // first time this scan has been encountered in this chunk
      scanProps[scanKey].timeRange = std::make_pair(*tIter - half, *tIter + half);
      scanProps[scanKey].times[spw] = std::set<double>();
      scanProps[scanKey].spwNRows[spw] = 1;
    } else {
      pair<double, double>& timeRange = scanProps[scanKey].timeRange;
      timeRange.first = min(timeRange.first, *tIter - half);
      timeRange.second = max(timeRange.second, *tIter + half);
      map<unsigned int, std::set<double>>& times = scanProps[scanKey].times;
      if (times.find(spw) == times.end()) {
        times[spw] = std::set<double>();
        scanProps[scanKey].spwNRows[spw] = 1;
      } else {
        ++scanProps[scanKey].spwNRows[spw];
      }
    }
    scanProps[scanKey].times[spw].insert(*tIter);
    subScanKey.obsID = *oIter;
    subScanKey.arrayID = *arIter;
    subScanKey.scan = *scanIter;
    subScanKey.fieldID = *fIter;
    bool autocorr = *a1Iter == *a2Iter;
    if (mysubscans.find(subScanKey) == mysubscans.end()) {
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
    } else {
      if (autocorr) {
        ++mysubscans[subScanKey].acRows;
      } else {
        ++mysubscans[subScanKey].xcRows;
      }
      mysubscans[subScanKey].beginTime = min(*tIter, mysubscans[subScanKey].beginTime);
      mysubscans[subScanKey].endTime = max(*tIter, mysubscans[subScanKey].endTime);
      _modifyFirstExposureTimeIfNecessary(mysubscans[subScanKey].firstExposureTime, *dIter, *tIter,
                                          *eiter, eunit);
      exposureSum[subScanKey] += *eiter;
    }
    subScanSpw.first = subScanKey;
    subScanSpw.second = spw;
    if (intervalSum.find(subScanSpw) == intervalSum.end()) {
      intervalSum[subScanSpw] = *iIter;
      mysubscans[subScanKey].spwNRows[spw] = 1;
    } else {
      intervalSum[subScanSpw] += *iIter;
      ++mysubscans[subScanKey].spwNRows[spw];
    }
    mysubscans[subScanKey].antennas.insert(*a1Iter);
    mysubscans[subScanKey].antennas.insert(*a2Iter);
    mysubscans[subScanKey].ddIDs.insert(*dIter);
    mysubscans[subScanKey].spws.insert(spw);
    mysubscans[subScanKey].stateIDs.insert(*stateIter);
    std::map<double, TimeStampProperties>& timeProps = mysubscans[subScanKey].timeProps;
    if (timeProps.find(*tIter) == timeProps.end()) {
      timeProps[*tIter].nrows = 1;
    } else {
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
  for (; ssIter != ssEnd; ++ssIter) {
    SubScanProperties& props = ssIter->second;
    props.meanExposureTime =
        Quantity(exposureSum[ssIter->first] / (props.acRows + props.xcRows), eunit);
  }
  const Unit& unit = intervalTimes.getFullUnit();
  map<pair<SubScanKey, unsigned int>, double>::const_iterator intSumIter = intervalSum.begin();
  map<pair<SubScanKey, unsigned int>, double>::const_iterator intSumEnd = intervalSum.end();
  for (; intSumIter != intSumEnd; ++intSumIter) {
    const SubScanKey& ssKey = intSumIter->first.first;
    const unsigned int& spw = intSumIter->first.second;
    const double& sum = intSumIter->second;
    SubScanProperties& props = mysubscans[ssKey];
    props.meanInterval[spw] = Quantity(sum / props.spwNRows[spw], unit);
  }
  return make_pair(scanProps, mysubscans);
}

std::shared_ptr<const std::map<SubScanKey, MSMetaData::SubScanProperties>>
MSMetaData::getSubScanProperties(bool showProgress) const {
  std::shared_ptr<const std::map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const std::map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, showProgress);
  return subScanProps;
}

std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties>> MSMetaData::_getScanProperties(
    bool showProgress) const {
  std::shared_ptr<const std::map<ScanKey, ScanProperties>> scanProps;
  std::shared_ptr<const std::map<SubScanKey, SubScanProperties>> subScanProps;
  _getScanAndSubScanProperties(scanProps, subScanProps, showProgress);
  return scanProps;
}

void MSMetaData::_getScanAndSubScanProperties(
    std::shared_ptr<const std::map<ScanKey, MSMetaData::ScanProperties>>& scanProps,
    std::shared_ptr<const std::map<SubScanKey, MSMetaData::SubScanProperties>>& subScanProps,
    bool showProgress) const {
  // responsible for setting _scanProperties and _subScanProperties
  // a sub scan is defined by a unique combination of scan number and field ID
  if (_scanProperties && _subScanProperties) {
    scanProps = _scanProperties;
    subScanProps = _subScanProperties;
    return;
  }
  std::shared_ptr<std::map<SubScanKey, SubScanProperties>> myssprops;
  std::shared_ptr<std::map<ScanKey, ScanProperties>> myscanprops;
  _computeScanAndSubScanProperties(myscanprops, myssprops, showProgress);
  scanProps = myscanprops;
  subScanProps = myssprops;

  static const unsigned int iSize = sizeof(int);
  static const unsigned int dSize = sizeof(double);

  static const unsigned int scanStructSize = 2 * dSize;
  static const unsigned int scanKeySize = 3 * iSize;
  // fudge of sizeof(Unit)
  static const unsigned int unitSize = 16;
  static const unsigned int feSize = iSize + 2 * dSize + unitSize;
  unsigned int scanSize = scanProps->size() * (scanKeySize + scanStructSize);
  std::map<ScanKey, ScanProperties>::const_iterator scanIter = scanProps->begin();
  std::map<ScanKey, ScanProperties>::const_iterator scanEnd = scanProps->end();
  for (; scanIter != scanEnd; ++scanIter) {
    const ScanProperties& props = scanIter->second;
    scanSize += props.meanInterval.size() * (dSize + 4 * iSize);
    const map<unsigned int, std::set<double>>& spwTimes = props.times;
    map<unsigned int, std::set<double>>::const_iterator tIter = spwTimes.begin();
    map<unsigned int, std::set<double>>::const_iterator tEnd = spwTimes.end();
    for (; tIter != tEnd; ++tIter) {
      scanSize += dSize * tIter->second.size();
    }
    scanSize += feSize * props.firstExposureTime.size();
  }

  static const unsigned int ssStructSize = 3 * dSize + 2 * iSize;
  static const unsigned int sskeySize = 4 * iSize;
  unsigned int subScanSize = subScanProps->size() * (ssStructSize + sskeySize);
  std::map<SubScanKey, SubScanProperties>::const_iterator subIter = subScanProps->begin();
  std::map<SubScanKey, SubScanProperties>::const_iterator subEnd = subScanProps->end();
  for (; subIter != subEnd; ++subIter) {
    const SubScanProperties& props = subIter->second;
    subScanSize += iSize * (props.antennas.size() + props.ddIDs.size() + props.stateIDs.size() +
                            props.spws.size());
    subScanSize += (dSize + iSize) + props.timeProps.size();
    // meanInterval + spwNRows
    subScanSize += (3 * iSize + dSize) * props.meanInterval.size();
    subScanSize += feSize * props.firstExposureTime.size();
  }
  unsigned int mysize = scanSize + subScanSize;
  if (_cacheUpdated(mysize)) {
    _scanProperties = scanProps;
    _subScanProperties = subScanProps;
  } else if (_forceSubScanPropsToCache) {
    _cacheMB += mysize / 1e6;
    _scanProperties = scanProps;
    _subScanProperties = subScanProps;
  }
}

std::map<double, double> MSMetaData::_getTimeToTotalBWMap(const Vector<double>& times,
                                                          const Vector<int>& ddIDs) {
  std::map<double, double> timeToBWMap;
  std::map<double, std::set<unsigned int>> timeToDDIDMap;
  Vector<double>::const_iterator end = times.end();
  Vector<double>::const_iterator tIter = times.begin();
  Vector<int>::const_iterator dIter = ddIDs.begin();
  while (tIter != end) {
    timeToDDIDMap[*tIter].insert(*dIter);
    ++tIter;
    ++dIter;
  }
  std::map<double, std::set<unsigned int>>::const_iterator end1 = timeToDDIDMap.end();
  vector<unsigned int> dataDescIDToSpwMap = getDataDescIDToSpwMap();
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  for (std::map<double, std::set<unsigned int>>::const_iterator iter = timeToDDIDMap.begin(); iter != end1;
       ++iter) {
    std::set<unsigned int> ddIDs = iter->second;
    timeToBWMap[iter->first] = 0;
    std::set<unsigned int>::const_iterator end2 = ddIDs.end();
    for (std::set<unsigned int>::const_iterator dIter = ddIDs.begin(); dIter != end2; ++dIter) {
      unsigned int spw = dataDescIDToSpwMap[*dIter];
      timeToBWMap[iter->first] += spwInfo[spw].bandwidth;
    }
  }
  return timeToBWMap;
}

void MSMetaData::_getUnflaggedRowStats(
    double& nACRows, double& nXCRows, std::shared_ptr<std::map<SubScanKey, double>>& subScanNACRows,
    std::shared_ptr<std::map<SubScanKey, double>>& subScanNXCRows,
    std::shared_ptr<vector<double>>& fieldNACRows,
    std::shared_ptr<vector<double>>& fieldNXCRows) const {
  // This method is responsible for setting _nUnflaggedACRows, _nUnflaggedXCRows,
  // _unflaggedFieldNACRows, _unflaggedFieldNXCRows, _unflaggedScanNACRows,
  // _unflaggedScanNXCRows
  if (_unflaggedFieldNACRows && !_unflaggedFieldNACRows->empty()) {
    nACRows = _nUnflaggedACRows;
    nXCRows = _nUnflaggedXCRows;
    fieldNACRows = _unflaggedFieldNACRows;
    fieldNXCRows = _unflaggedFieldNXCRows;
    subScanNACRows = _unflaggedSubScanNACRows;
    subScanNXCRows = _unflaggedSubScanNXCRows;
    return;
  }
  std::map<SubScanKey, double>*mySubScanNACRows, *mySubScanNXCRows;
  vector<double>*myFieldNACRows, *myFieldNXCRows;
  _getUnflaggedRowStats(nACRows, nXCRows, myFieldNACRows, myFieldNXCRows, mySubScanNACRows,
                        mySubScanNXCRows);

  fieldNACRows.reset(myFieldNACRows);
  fieldNXCRows.reset(myFieldNXCRows);
  subScanNACRows.reset(mySubScanNACRows);
  subScanNXCRows.reset(mySubScanNXCRows);
  unsigned int mysize = 2 * (sizeof(double) + _sizeof(*fieldNACRows) + _sizeof(*subScanNACRows));
  if (_cacheUpdated(mysize)) {
    _nUnflaggedACRows = nACRows;
    _nUnflaggedXCRows = nXCRows;
    _unflaggedFieldNACRows = fieldNACRows;
    _unflaggedFieldNXCRows = fieldNXCRows;
    _unflaggedSubScanNACRows = subScanNACRows;
    _unflaggedSubScanNXCRows = subScanNXCRows;
  }
}

void MSMetaData::_getUnflaggedRowStats(double& nACRows, double& nXCRows,
                                       vector<double>*& fieldNACRows, vector<double>*& fieldNXCRows,
                                       std::map<SubScanKey, double>*& subScanNACRows,
                                       std::map<SubScanKey, double>*& subScanNXCRows) const {
  nACRows = 0;
  nXCRows = 0;
  unsigned int myNFields = nFields();
  fieldNACRows = new vector<double>(myNFields, 0);
  fieldNXCRows = new vector<double>(myNFields, 0);
  subScanNACRows = new std::map<SubScanKey, double>();
  subScanNXCRows = new std::map<SubScanKey, double>();
  std::set<SubScanKey> subScanKeys = _getSubScanKeys();
  std::set<SubScanKey>::const_iterator iter = subScanKeys.begin();
  std::set<SubScanKey>::const_iterator end = subScanKeys.end();
  while (iter != end) {
    (*subScanNACRows)[*iter] = 0;
    (*subScanNXCRows)[*iter] = 0;
    ++iter;
  }
  std::shared_ptr<Vector<int>> ant1, ant2;
  _getAntennas(ant1, ant2);
  std::shared_ptr<Vector<int>> dataDescIDs = _getDataDescIDs();
  std::shared_ptr<Vector<int>> scans = _getScans();
  std::shared_ptr<Vector<int>> fieldIDs = _getFieldIDs();
  std::shared_ptr<Vector<int>> obsIDs = _getObservationIDs();
  std::shared_ptr<Vector<int>> arrIDs = _getArrayIDs();
  Vector<int>::const_iterator aEnd = ant1->end();
  Vector<int>::const_iterator a1Iter = ant1->begin();
  Vector<int>::const_iterator a2Iter = ant2->begin();
  Vector<int>::const_iterator sIter = scans->begin();
  Vector<int>::const_iterator fIter = fieldIDs->begin();
  Vector<int>::const_iterator oIter = obsIDs->begin();
  Vector<int>::const_iterator arIter = arrIDs->begin();
  Vector<int>::const_iterator dIter = dataDescIDs->begin();
  unsigned int i = 0;
  // uInt64 count = 0;
  //  a flag value of true means the datum is bad (flagged), so false => unflagged
  vector<unsigned int> dataDescIDToSpwMap = getDataDescIDToSpwMap();
  std::set<unsigned int> a, b, c, d, e;
  vector<SpwProperties> spwInfo = _getSpwInfo(a, b, c, d, e);
  std::shared_ptr<ArrayColumn<bool>> flags = _getFlags();
  while (a1Iter != aEnd) {
    unsigned int spw = dataDescIDToSpwMap[*dIter];
    SpwProperties spwProp = spwInfo[spw];
    Vector<double> channelWidths(Vector<double>(spwProp.chanwidths.getValue("Hz")));
    const Matrix<bool>& flagsMatrix(flags->get(i));
    // count += flagsMatrix.size();
    double x = 0;
    if (!anyTrue(flagsMatrix)) {
      // all channels are unflagged
      x = 1;
    } else if (allTrue(flagsMatrix)) {
      // do nothing. All channels are flagged for this row
      // do not put a continue though, because counters still must
      // incremented below
    } else {
      // some channels are flagged, some aren't
      unsigned int nCorrelations = flagsMatrix.nrow();
      double denom = spwProp.bandwidth * nCorrelations;
      double bwSum = 0;

      for (unsigned int corr = 0; corr < nCorrelations; ++corr) {
        // invert the meaning here, so that a true value
        // in corrRow means the datum is good (unflagged)
        // it will make the masked sum below more obvious
        Vector<bool> corrRow = !flagsMatrix.row(corr);
        if (allTrue(corrRow)) {
          // all channels for this correlation are unflagged
          bwSum += spwProp.bandwidth;
        } else if (!anyTrue(corrRow)) {
          // do nothing, all channels for this correlation
          // have been flagged
          // but allow fall through to iterator increments
        } else {
          // some channels are flagged for this correlation, some aren't
          MaskedArray<double> unFlaggedChannelWidths(channelWidths, corrRow, true);
          bwSum += sum(unFlaggedChannelWidths);
        }
      }
      x = bwSum / denom;
    }
    SubScanKey subScanKey;
    subScanKey.obsID = *oIter;
    subScanKey.arrayID = *arIter;
    subScanKey.scan = *sIter;
    subScanKey.fieldID = *fIter;
    if (*a1Iter == *a2Iter) {
      (*fieldNACRows)[*fIter] += x;
      (*subScanNACRows)[subScanKey] += x;
    } else {
      (*fieldNXCRows)[*fIter] += x;
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
  vector<double>::const_iterator faIter = fieldNACRows->begin();
  vector<double>::const_iterator faEnd = fieldNACRows->end();
  vector<double>::const_iterator fxIter = fieldNXCRows->begin();
  while (faIter != faEnd) {
    nACRows += *faIter;
    nXCRows += *fxIter;
    ++faIter;
    ++fxIter;
  }
}

void MSMetaData::_getSpwsAndIntentsMaps(vector<std::set<String>>& spwToIntentsMap,
                                        std::map<String, std::set<unsigned int>>& intentToSpwsMap) {
  if (!_spwToIntentsMap.empty() && !_intentToSpwsMap.empty()) {
    spwToIntentsMap = _spwToIntentsMap;
    intentToSpwsMap = _intentToSpwsMap;
  }
  spwToIntentsMap.clear();
  intentToSpwsMap.clear();
  std::set<unsigned int> avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw;
  vector<SpwProperties> spwInfo = _getSpwInfo(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  std::set<String> emptySet;
  vector<SpwProperties>::const_iterator end = spwInfo.end();
  for (vector<SpwProperties>::const_iterator iter = spwInfo.begin(); iter != end; ++iter) {
    spwToIntentsMap.push_back(emptySet);
  }
  vector<std::set<String>> stateToIntentsMap;
  std::set<String> uniqueIntents;
  _getStateToIntentsMap(stateToIntentsMap, uniqueIntents);
  if (uniqueIntents.size() == 0) {
    _spwToIntentsMap = spwToIntentsMap;
    _intentToSpwsMap = intentToSpwsMap;
    return;
  }
  std::shared_ptr<Vector<int>> dataDescIDs = _getDataDescIDs();
  Vector<int>::const_iterator curDDID = dataDescIDs->begin();
  Vector<int>::const_iterator endDDID = dataDescIDs->end();
  std::shared_ptr<Vector<int>> states = _getStateIDs();
  Vector<int>::const_iterator curState = states->begin();
  vector<unsigned int> dataDescToSpwMap = getDataDescIDToSpwMap();
  while (curDDID != endDDID) {
    unsigned int spw = dataDescToSpwMap[*curDDID];
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

vector<std::set<String>> MSMetaData::_getSpwToIntentsMap() {
  vector<std::set<String>> spwToIntentsMap;
  std::map<String, std::set<unsigned int>> intentToSpwsMap;
  _getSpwsAndIntentsMaps(spwToIntentsMap, intentToSpwsMap);
  return spwToIntentsMap;
}

void MSMetaData::_getFieldsAndStatesMaps(std::map<int, std::set<int>>& fieldToStatesMap,
                                         std::map<int, std::set<int>>& stateToFieldsMap) {
  // This method is responsible for setting _fieldToStatesMap and _stateToFieldMap.
  if (!_fieldToStatesMap.empty() && !_stateToFieldsMap.empty()) {
    fieldToStatesMap = _fieldToStatesMap;
    stateToFieldsMap = _stateToFieldsMap;
    return;
  }
  std::shared_ptr<Vector<int>> allStates = _getStateIDs();
  std::shared_ptr<Vector<int>> allFields = _getFieldIDs();
  Vector<int>::const_iterator endState = allStates->end();
  Vector<int>::const_iterator curField = allFields->begin();
  fieldToStatesMap.clear();
  stateToFieldsMap.clear();
  for (Vector<int>::const_iterator curState = allStates->begin(); curState != endState;
       ++curState, ++curField) {
    fieldToStatesMap[*curField].insert(*curState);
    stateToFieldsMap[*curState].insert(*curField);
  }
  if (_cacheUpdated(_sizeof(fieldToStatesMap) + _sizeof(stateToFieldsMap))) {
    _fieldToStatesMap = fieldToStatesMap;
    _stateToFieldsMap = stateToFieldsMap;
  }
}

map<int, std::set<String>> MSMetaData::getFieldNamesForSourceMap() const {
  map<int, std::set<int>> idsToSource = getFieldsForSourceMap();
  map<int, std::set<int>>::const_iterator iter = idsToSource.begin();
  map<int, std::set<int>>::const_iterator end = idsToSource.end();
  map<int, std::set<String>> namesMap;
  vector<String> names = getFieldNames();
  while (iter != end) {
    int sourceID = iter->first;
    namesMap[sourceID] = std::set<String>();
    std::set<int> fieldIDs = idsToSource[sourceID];
    std::set<int>::const_iterator siter = fieldIDs.begin();
    std::set<int>::const_iterator send = fieldIDs.end();
    while (siter != send) {
      namesMap[sourceID].insert(names[*siter]);
      ++siter;
    }
    ++iter;
  }
  return namesMap;
}

map<int, std::set<int>> MSMetaData::getFieldsForSourceMap() const {
  // This method sets _sourceToFieldsMap
  if (!_sourceToFieldsMap.empty()) {
    return _sourceToFieldsMap;
  }
  String sourceIDName = MSField::columnName(MSFieldEnums::SOURCE_ID);
  Vector<int> sourceIDs = ScalarColumn<int>(_ms->field(), sourceIDName).getColumn();
  map<int, std::set<int>> mymap;
  std::set<int> uSourceIDs(sourceIDs.begin(), sourceIDs.end());
  std::set<int>::const_iterator iter = uSourceIDs.begin();
  std::set<int>::const_iterator end = uSourceIDs.end();
  while (iter != end) {
    mymap[*iter] = std::set<int>();
    ++iter;
  }
  Vector<int>::const_iterator miter = sourceIDs.begin();
  Vector<int>::const_iterator mend = sourceIDs.end();
  int rowNumber = 0;
  while (miter != mend) {
    mymap[*miter].insert(rowNumber);
    ++miter;
    ++rowNumber;
  }
  unsigned int mysize = _sizeof(mymap);
  if (_cacheUpdated(mysize)) {
    _sourceToFieldsMap = mymap;
  }
  return mymap;
}

void MSMetaData::_getFieldsAndIntentsMaps(vector<std::set<String>>& fieldToIntentsMap,
                                          std::map<String, std::set<int>>& intentToFieldsMap) {
  // This method is responsible for setting _intentToFieldIDMap and _fieldToIntentsMap
  if (getIntents().empty()) {
    fieldToIntentsMap = vector<std::set<String>>(nFields());
    intentToFieldsMap.clear();
    return;
  }
  if (!_intentToFieldIDMap.empty() && !_fieldToIntentsMap.empty()) {
    fieldToIntentsMap = _fieldToIntentsMap;
    intentToFieldsMap = _intentToFieldIDMap;
    return;
  }
  fieldToIntentsMap.resize(nFields());
  vector<std::set<String>> stateToIntentsMap;
  std::set<String> uniqueIntents;
  _getStateToIntentsMap(stateToIntentsMap, uniqueIntents);
  std::map<int, std::set<int>> fieldToStatesMap;
  std::map<int, std::set<int>> stateToFieldsMap;
  _getFieldsAndStatesMaps(fieldToStatesMap, stateToFieldsMap);
  std::map<int, std::set<int>>::const_iterator end = stateToFieldsMap.end();
  for (std::map<int, std::set<int>>::const_iterator iter = stateToFieldsMap.begin(); iter != end;
       ++iter) {
    int state = iter->first;
    std::set<int> fields = iter->second;
    std::set<String> intents = stateToIntentsMap[state];
    std::set<int>::const_iterator endField = fields.end();
    for (std::set<int>::const_iterator curField = fields.begin(); curField != endField;
         ++curField) {
      fieldToIntentsMap[*curField].insert(intents.begin(), intents.end());
    }
    std::set<String>::const_iterator endIntent = intents.end();
    for (std::set<String>::const_iterator curIntent = intents.begin(); curIntent != endIntent;
         ++curIntent) {
      intentToFieldsMap[*curIntent].insert(fields.begin(), fields.end());
    }
  }
  if (_cacheUpdated(_sizeof(fieldToIntentsMap) + _sizeof(intentToFieldsMap))) {
    _fieldToIntentsMap = fieldToIntentsMap;
    _intentToFieldIDMap = intentToFieldsMap;
  }
}

std::map<std::pair<unsigned int, unsigned int>, unsigned int> MSMetaData::getSpwIDPolIDToDataDescIDMap() const {
  if (!_spwPolIDToDataDescIDMap.empty()) {
    return _spwPolIDToDataDescIDMap;
  }
  vector<unsigned int> dataDescIDToSpwMap = getDataDescIDToSpwMap();
  vector<unsigned int>::const_iterator i1 = dataDescIDToSpwMap.begin();
  vector<unsigned int>::const_iterator end = dataDescIDToSpwMap.end();
  std::map<std::pair<unsigned int, unsigned int>, unsigned int> spwPolIDToDataDescIDMap;
  vector<unsigned int> dataDescIDToPolIDMap = getDataDescIDToPolIDMap();
  unsigned int dataDesc = 0;
  while (i1 != end) {
    unsigned int spw = *i1;
    unsigned int polID = dataDescIDToPolIDMap[dataDesc];
    spwPolIDToDataDescIDMap[std::make_pair(spw, polID)] = dataDesc;
    ++i1;
    ++dataDesc;
  }
  unsigned int mysize = 2 * sizeof(int) * spwPolIDToDataDescIDMap.size();
  if (_cacheUpdated(mysize)) {
    _spwPolIDToDataDescIDMap = spwPolIDToDataDescIDMap;
  }
  return spwPolIDToDataDescIDMap;
}

std::pair<MDirection, MDirection> MSMetaData::getPointingDirection(int& antenna1, int& antenna2,
                                                                   double& time, rownr_t row,
                                                                   bool interpolate,
                                                                   int initialguess

) const {
  ThrowIf(row >= this->nRows(), "Row number exceeds number of rows in the MS");
  const String& ant1ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA1);
  const String& ant2ColName = MeasurementSet::columnName(MSMainEnums::ANTENNA2);
  antenna1 = ScalarColumn<int>(*_ms, ant1ColName).get(row);
  antenna2 = ScalarColumn<int>(*_ms, ant2ColName).get(row);
  bool autocorr = (antenna1 == antenna2);
  const String& timeColName = MeasurementSet::columnName(MSMainEnums::TIME);
  time = ScalarColumn<double>(*_ms, timeColName).get(row);
  MSPointingColumns pCols(_ms->pointing());
  int pidx1, pidx2;
  pidx1 = pCols.pointingIndex(antenna1, time, initialguess);
  if (autocorr) {
    pidx2 = pidx1;
  } else
    pidx2 = pCols.pointingIndex(antenna2, time, initialguess);
  const String& intervalColName = MeasurementSet::columnName(MSMainEnums::INTERVAL);
  double interval = ScalarColumn<double>(*_ms, intervalColName).get(row);
  MDirection dir1, dir2;
  if (!interpolate || interval >= pCols.interval()(pidx1)) {
    dir1 = pCols.directionMeas(pidx1);
  } else {
    dir1 = _getInterpolatedDirection(pCols, pidx1, time);
  }
  if (autocorr) {
    dir2 = dir1;
  } else if (!interpolate || interval >= pCols.interval()(pidx2)) {
    dir2 = pCols.directionMeas(pidx2);
  } else {
    dir2 = _getInterpolatedDirection(pCols, pidx2, time);
  }
  return std::make_pair(dir1, dir2);
}

MDirection MSMetaData::_getInterpolatedDirection(const MSPointingColumns& pCols, const int& index1,
                                                 const double& time) const {
  int antenna = pCols.antennaId()(index1);
  double interval1 = pCols.interval()(index1);
  double time1 = pCols.time()(index1);
  int index2;
  if (time >= time1) {
    index2 = pCols.pointingIndex(antenna, time1 + interval1, index1 + 1);
  } else {
    index2 = pCols.pointingIndex(antenna, time1 - interval1);
  }
  if (index2 < 0 || index2 == index1) {
    // look in opposite direction
    if (time >= time1) {
      index2 = pCols.pointingIndex(antenna, time1 - interval1);
    } else {
      index2 = pCols.pointingIndex(antenna, time1 + interval1, index1 + 1);
    }
  }
  ThrowIf(index2 < 0 || index2 == index1,
          "Failed to find pointing index to interpolate direction.");

  double time2 = pCols.time()(index2);
  ThrowIf(time2 == time1,
          "Failed to find pointing index with valid timestamp to interpolate direction.");

  // Interpolate (time1, time2),(dir1,dir2)
  Vector<double> dirvec1 = pCols.directionMeas(index1).getAngle("rad").getValue();
  Vector<double> dirvec2 = pCols.directionMeas(index2).getAngle("rad").getValue();
  MDirection::Ref rf = pCols.directionMeas(index1).getRef();
  Vector<double> newdir = dirvec1 + (dirvec2 - dirvec1) * (time - time1) / (time2 - time1);
  Quantity qlon(newdir(0), "rad");
  Quantity qlat(newdir(1), "rad");
  return MDirection(qlon, qlat, rf);
}

vector<unsigned int> MSMetaData::getDataDescIDToSpwMap() const {
  if (!_dataDescIDToSpwMap.empty()) {
    return _dataDescIDToSpwMap;
  }
  String spwColName = MSDataDescription::columnName(MSDataDescriptionEnums::SPECTRAL_WINDOW_ID);
  ScalarColumn<int> spwCol(_ms->dataDescription(), spwColName);
  Vector<int> spws = spwCol.getColumn();
  vector<unsigned int> dataDescToSpwMap(spws.begin(), spws.end());
  unsigned int mysize = sizeof(int) * dataDescToSpwMap.size();
  if (_cacheUpdated(mysize)) {
    _dataDescIDToSpwMap = dataDescToSpwMap;
  }
  return dataDescToSpwMap;
}

std::set<unsigned int> MSMetaData::getPolarizationIDs(unsigned int obsID, int arrayID, int scan, unsigned int spwid) const {
  ScanKey scanKey;
  scanKey.obsID = obsID;
  scanKey.arrayID = arrayID;
  scanKey.scan = scan;
  _checkScan(scanKey);
  if (!_scanSpwToPolIDMap.empty()) {
    return _scanSpwToPolIDMap.find(std::pair<ScanKey, unsigned int>(scanKey, spwid))->second;
  }
  vector<unsigned int> ddToPolMap = getDataDescIDToPolIDMap();
  vector<unsigned int> ddToSpwMap = getDataDescIDToSpwMap();
  std::map<ScanKey, std::set<unsigned int>> scanToDDIDMap;
  vector<std::set<ScanKey>> ddIDToScanMap;
  _getScansAndDDIDMaps(scanToDDIDMap, ddIDToScanMap);
  std::map<std::pair<ScanKey, unsigned int>, std::set<unsigned int>> mymap;
  std::map<ScanKey, std::set<unsigned int>>::const_iterator iter = scanToDDIDMap.begin();
  std::map<ScanKey, std::set<unsigned int>>::const_iterator end = scanToDDIDMap.end();
  while (iter != end) {
    std::set<unsigned int> ddids = iter->second;
    std::set<unsigned int>::const_iterator diter = ddids.begin();
    std::set<unsigned int>::const_iterator dend = ddids.end();
    while (diter != dend) {
      std::pair<ScanKey, unsigned int> key(iter->first, ddToSpwMap[*diter]);
      mymap[key].insert(ddToPolMap[*diter]);
      ++diter;
    }
    ++iter;
  }
  if (_cacheUpdated(_sizeof(mymap))) {
    _scanSpwToPolIDMap = mymap;
  }
  return mymap[std::pair<ScanKey, unsigned int>(scanKey, spwid)];
}

unsigned int MSMetaData::_sizeof(const std::map<std::pair<int, unsigned int>, std::set<unsigned int>>& map) {
  unsigned int size = 0;
  unsigned int uSize = sizeof(unsigned int);
  unsigned int iSize = sizeof(int);
  for (std::map<std::pair<int, unsigned int>, std::set<unsigned int>>::const_iterator iter = map.begin();
       iter != map.end(); ++iter) {
    size += iSize + uSize * (iter->second.size() + 1);
  }
  return size;
}

vector<unsigned int> MSMetaData::getDataDescIDToPolIDMap() const {
  if (!_dataDescIDToPolIDMap.empty()) {
    return _dataDescIDToPolIDMap;
  }
  String polColName = MSDataDescription::columnName(MSDataDescriptionEnums::POLARIZATION_ID);
  ScalarColumn<int> polCol(_ms->dataDescription(), polColName);
  Vector<int> pols = polCol.getColumn();
  vector<unsigned int> dataDescToPolIDMap(pols.begin(), pols.end());
  unsigned int mysize = sizeof(int) * dataDescToPolIDMap.size();
  if (_cacheUpdated(mysize)) {
    _dataDescIDToPolIDMap = dataDescToPolIDMap;
  }
  return dataDescToPolIDMap;
}

vector<MSMetaData::SpwProperties> MSMetaData::_getSpwInfo(std::set<unsigned int>& avgSpw,
                                                          std::set<unsigned int>& tdmSpw,
                                                          std::set<unsigned int>& fdmSpw,
                                                          std::set<unsigned int>& wvrSpw,
                                                          std::set<unsigned int>& sqldSpw) const {
  if (_spwInfoStored) {
    avgSpw = _avgSpw;
    tdmSpw = _tdmSpw;
    fdmSpw = _fdmSpw;
    wvrSpw = _wvrSpw;
    sqldSpw = _sqldSpw;
    return _spwInfo;
  }
  vector<SpwProperties> spwInfo = _getSpwInfo2(avgSpw, tdmSpw, fdmSpw, wvrSpw, sqldSpw);
  unsigned int mysize = sizeof(unsigned int) * (avgSpw.size() + tdmSpw.size() + fdmSpw.size() + wvrSpw.size() +
                                sqldSpw.size()) +
                2 * sizeof(int) * spwInfo.size() + 2 * sizeof(double) * spwInfo.size();
  vector<SpwProperties>::const_iterator end = spwInfo.end();
  for (vector<SpwProperties>::const_iterator iter = spwInfo.begin(); iter != end; ++iter) {
    mysize += 4 * (sizeof(double) * iter->nchans + 20);
    mysize += sizeof(double) * iter->edgechans.size();
  }
  if (_cacheUpdated(mysize)) {
    _avgSpw = avgSpw;
    _tdmSpw = tdmSpw;
    _fdmSpw = fdmSpw;
    _wvrSpw = wvrSpw;
    _sqldSpw = sqldSpw;
    _spwInfo = spwInfo;
    _spwInfoStored = true;
  }
  return spwInfo;
}

void MSMetaData::_checkField(unsigned int fieldID) const {
  ThrowIf(fieldID >= nFields(), "Unknown fieldID " + std::to_string(fieldID));
}

void MSMetaData::_checkScan(const ScanKey& key) const {
  std::set<ScanKey> allKeys = getScanKeys();
  ThrowIf(allKeys.find(key) == allKeys.end(), "Unknown scan " + toString(key));
}

void MSMetaData::_checkScans(const std::set<ScanKey>& scanKeys) const {
  std::set<ScanKey> allKeys = getScanKeys();
  std::set<ScanKey>::const_iterator iter = scanKeys.begin();
  std::set<ScanKey>::const_iterator end = scanKeys.end();
  while (iter != end) {
    ThrowIf(allKeys.find(*iter) == allKeys.end(), "Unknown scan " + toString(*iter));
    ++iter;
  }
}

void MSMetaData::_checkSubScan(const SubScanKey& key) const {
  std::set<SubScanKey> allKeys = _getSubScanKeys();
  ThrowIf(allKeys.find(key) == allKeys.end(), "Unknown subscan " + toString(key));
}

bool MSMetaData::_hasFieldID(const int fieldID) const {
  ThrowIf(fieldID >= (int)nFields(), "Requested field ID " + std::to_string(fieldID) +
                                         " is greater than or equal to the number of records (" +
                                         std::to_string(nFields()) + ") in this MS's FIELD table");
  std::set<int> uniqueFields = getUniqueFieldIDs();
  return uniqueFields.find(fieldID) != uniqueFields.end();
}

const std::set<int>& MSMetaData::getUniqueAntennaIDs() const {
  // this method is responsible for setting _uniqueAntennas
  if (_uniqueAntennaIDs.empty()) {
    if (_subScanProperties) {
      map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
      map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
      for (; iter != end; ++iter) {
        const std::set<int>& ants = iter->second.antennas;
        _uniqueAntennaIDs.insert(ants.begin(), ants.end());
      }
    } else {
      std::shared_ptr<Vector<int>> ant1, ant2;
      _getAntennas(ant1, ant2);
      _uniqueAntennaIDs.insert(ant1->begin(), ant1->end());
      _uniqueAntennaIDs.insert(ant2->begin(), ant2->end());
    }
  }
  return _uniqueAntennaIDs;
}

std::set<unsigned int> MSMetaData::getUniqueDataDescIDs() const {
  // this method is responsible for setting _uniqueDataDescIDs
  if (_uniqueDataDescIDs.empty()) {
    if (_subScanProperties) {
      map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
      map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
      for (; iter != end; ++iter) {
        const std::set<unsigned int>& ddIDs = iter->second.ddIDs;
        _uniqueDataDescIDs.insert(ddIDs.begin(), ddIDs.end());
      }
    } else {
      std::shared_ptr<Vector<int>> allDDIDs = _getDataDescIDs();
      _uniqueDataDescIDs.insert(allDDIDs->begin(), allDDIDs->end());
    }
  }
  return _uniqueDataDescIDs;
}

std::set<int> MSMetaData::getUniqueFieldIDs() const {
  if (_uniqueFieldIDs.empty()) {
    if (_subScanProperties) {
      map<SubScanKey, SubScanProperties>::const_iterator iter = _subScanProperties->begin();
      map<SubScanKey, SubScanProperties>::const_iterator end = _subScanProperties->end();
      for (; iter != end; ++iter) {
        _uniqueFieldIDs.insert(iter->first.fieldID);
      }
    } else {
      std::shared_ptr<Vector<int>> allFieldIDs = _getFieldIDs();
      _uniqueFieldIDs.insert(allFieldIDs->begin(), allFieldIDs->end());
    }
  }
  return _uniqueFieldIDs;
}

std::set<unsigned int> MSMetaData::getUniqueSpwIDs() const {
  vector<unsigned int> ddToSpw = getDataDescIDToSpwMap();
  std::set<unsigned int> uDDs = getUniqueDataDescIDs();
  std::set<unsigned int> uSpws;
  std::set<unsigned int>::const_iterator iter = uDDs.begin();
  std::set<unsigned int>::const_iterator end = uDDs.end();
  for (; iter != end; ++iter) {
    uSpws.insert(ddToSpw[*iter]);
  }
  return uSpws;
}

bool MSMetaData::_hasStateID(const int stateID) const {
  // This method is responsible for setting _uniqueStateIDs
  ThrowIf(stateID >= (int)nStates(), "Requested state ID " + std::to_string(stateID) +
                                         " is greater than or equal to the number of records (" +
                                         std::to_string(nStates()) + ") in this MS's STATE table");
  if (_uniqueStateIDs.empty()) {
    std::shared_ptr<Vector<int>> allStateIDs = _getStateIDs();
    _uniqueStateIDs.insert(allStateIDs->begin(), allStateIDs->end());
  }
  return _uniqueStateIDs.find(stateID) != _uniqueStateIDs.end();
}

void MSMetaData::_hasAntennaID(int antennaID) {
  ThrowIf(antennaID >= (int)nAntennas(),
          _ORIGIN + "Requested antenna ID " + std::to_string(antennaID) +
              " is greater than or equal to the number of records (" + std::to_string(nAntennas()) +
              ") in this MS's ANTENNA table");
}

std::shared_ptr<Quantum<Vector<double>>> MSMetaData::_getIntervals() const {
  ScalarQuantColumn<double> col(*_ms, MeasurementSet::columnName(MSMainEnums::INTERVAL));
  std::shared_ptr<Quantum<Vector<double>>> intervals = col.getColumn();
  return intervals;
}

MSMetaData::ColumnStats MSMetaData::getIntervalStatistics() const {
  std::shared_ptr<Quantum<Vector<double>>> intervals = _getIntervals();
  Vector<double> intInSec = intervals->getValue("s");
  ColumnStats stats;
  ClassicalStatistics<double, Vector<double>::const_iterator> cs;
  cs.setData(intInSec.begin(), intInSec.size());
  cs.getMinMax(stats.min, stats.max);
  stats.median = cs.getMedian();
  return stats;
}

std::shared_ptr<vector<int>> MSMetaData::_almaReceiverBands(uint nspw) const {
  // for ALMA specific receiver band (RB) info
  static const std::regex rb("RB_(\\d\\d)");
  static const std::regex id("SpectralWindow_(\\d+)");
  std::shared_ptr<vector<int>> freqBands(new vector<int>(nspw, -1));
  const File asdm_rx(_ms->tableName() + "/ASDM_RECEIVER");
  Vector<String> freqBand;
  if (asdm_rx.exists() && asdm_rx.isDirectory()) {
    auto spwid = ScalarColumn<String>(Table(asdm_rx.path().originalName()), "spectralWindowId")
                     .getColumn()
                     .tovector();
    if (spwid.size() <= nspw) {
      auto fb = ScalarColumn<String>(Table(asdm_rx.path().originalName()), "frequencyBand")
                    .getColumn()
                    .tovector();
      // reverse order so that if the spw is defined multiple times,
      // the first value for the rx band is used
      reverse(spwid.begin(), spwid.end());
      reverse(fb.begin(), fb.end());
      const auto n = fb.size();
      // to get type of i
      auto i = n;
      std::smatch match;
      for (i = 0; i < n; ++i) {
        if (regex_search(fb[i], match, rb)) {
          auto myrb = stoi(match.str(1));
          // the receiver band is encoded, so we must get the spwid
          // it is associated with
          if (regex_search(spwid[i], match, id)) {
            auto myid = stoi(match.str(1));
            (*freqBands)[myid] = myrb;
          } else {
            std::ostringstream os;
            os << "Unable to find spw id for row " << i << " in table "
               << asdm_rx.path().absoluteName();
            throw AipsError(os.str());
          }
        }
      }
    }
  }
  return freqBands;
}

std::vector<std::vector<unsigned int>> MSMetaData::_getSpwToPolMap() const {
  if (!_spwIDToPolIDMap.empty()) {
    return _spwIDToPolIDMap;
  }
  const auto spwPolToDDID = getSpwIDPolIDToDataDescIDMap();
  _spwIDToPolIDMap.resize(nSpw(true));
  for (const auto& p : spwPolToDDID) {
    unsigned int spwID = p.first.first;
    unsigned int polID = p.first.second;
    _spwIDToPolIDMap[spwID].push_back(polID);
  }
  for (auto& v : _spwIDToPolIDMap) {
    std::sort(v.begin(), v.end());
  }
  return _spwIDToPolIDMap;
}

vector<MSMetaData::SpwProperties> MSMetaData::_getSpwInfo2(std::set<unsigned int>& avgSpw,
                                                           std::set<unsigned int>& tdmSpw,
                                                           std::set<unsigned int>& fdmSpw,
                                                           std::set<unsigned int>& wvrSpw,
                                                           std::set<unsigned int>& sqldSpw) const {
  static const std::regex rxSqld("BB_[0-9]#SQLD");
  static const std::regex rb("RB_(\\d\\d)");
  static const std::regex sw("SW-(\\d\\d)");
  std::smatch match;
  MSSpWindowColumns spwCols(_ms->spectralWindow());
  Vector<double> bws = spwCols.totalBandwidth().getColumn();
  ArrayQuantColumn<double> cfCol(_ms->spectralWindow(),
                                 MSSpectralWindow::columnName(MSSpectralWindowEnums::CHAN_FREQ));
  ArrayQuantColumn<double> cwCol(_ms->spectralWindow(),
                                 MSSpectralWindow::columnName(MSSpectralWindowEnums::CHAN_WIDTH));
  ScalarMeasColumn<MFrequency> reffreqs(
      _ms->spectralWindow(), MSSpectralWindow::columnName(MSSpectralWindowEnums::REF_FREQUENCY));
  ArrayQuantColumn<double> ebwCol(
      _ms->spectralWindow(), MSSpectralWindow::columnName(MSSpectralWindowEnums::EFFECTIVE_BW));
  ArrayQuantColumn<double> resCol(_ms->spectralWindow(),
                                  MSSpectralWindow::columnName(MSSpectralWindowEnums::RESOLUTION));
  auto nss = spwCols.netSideband().getColumn();
  auto name = spwCols.name().getColumn();
  auto myHasBBCNo = hasBBCNo();
  auto bbcno = myHasBBCNo ? spwCols.bbcNo().getColumn() : Vector<int>();
  const auto spwTableName = _ms->spectralWindowTableName();
  const Table spwTable(spwTableName);
  Vector<String> scb;
  if (spwTable.tableDesc().isColumn("SDM_CORR_BIT")) {
    // CAS-13749 SPECTRAL_WINDOW::SDM_CORR_BIT
    // is an adhoc, ALMA-specific column
    ScalarColumn<String> scbCol(_ms->spectralWindow(), "SDM_CORR_BIT");
    scb = scbCol.getColumn();
  }
  vector<double> freqLimits(2);
  Vector<Quantity> tmp;
  vector<SpwProperties> spwInfo(bws.size());
  std::set<unsigned int> wvrFirst, wvrSecond;
  const static Unit emptyUnit;
  const static Unit hz("Hz");
  const static String wvr = "WVR";
  const static String wvrNominal = "WVR#NOMINAL";
  unsigned int nrows = bws.size();
  std::shared_ptr<vector<int>> freqBands;
  for (unsigned int i = 0; i < nrows; ++i) {
    spwInfo[i].bandwidth = bws[i];
    tmp.resize(0);
    cfCol.get(i, tmp);
    spwInfo[i].chanfreqs = QVD(tmp);
    auto u = spwInfo[i].chanfreqs.getFullUnit();
    spwInfo[i].meanfreq = Quantity(mean(spwInfo[i].chanfreqs.getValue()), u);
    freqLimits[0] = spwInfo[i].chanfreqs.min().getValue(u);
    freqLimits[1] = spwInfo[i].chanfreqs.max().getValue(u);
    spwInfo[i].edgechans = freqLimits;
    tmp.resize(0);
    cwCol.get(i, tmp);
    spwInfo[i].chanwidths = QVD(tmp);
    spwInfo[i].netsideband = nss[i];
    spwInfo[i].nchans = tmp.size();
    unsigned int nchan = spwInfo[i].nchans;
    tmp.resize(0);
    ebwCol.get(i, tmp);
    spwInfo[i].effbw = QVD(tmp);
    tmp.resize(0);
    resCol.get(i, tmp);
    spwInfo[i].resolution = QVD(tmp);
    auto halfWidths = (spwInfo[i].chanwidths) / 2.0;
    auto lowFreq = (spwInfo[i].chanfreqs - halfWidths).min();
    auto highFreq = (spwInfo[i].chanfreqs + halfWidths).max();
    spwInfo[i].centerfreq = (lowFreq + highFreq) / 2;
    spwInfo[i].name = name[i];
    if (myHasBBCNo) {
      spwInfo[i].bbcno = bbcno[i];
      if (std::regex_search(name[i], rxSqld)) {
        sqldSpw.insert(i);
      }
    }
    spwInfo[i].corrbit = scb.size() == 0 ? "UNKNOWN" : scb[i];
    spwInfo[i].reffreq = reffreqs(i);
    if (spwInfo[i].reffreq.getUnit() == emptyUnit) {
      spwInfo[i].reffreq.set(hz);
    }
    // types of ALMA spws
    // algorithm from thunter, CAS-5794, CAS-12592, CAS-13362
    if (name[i].find(wvr) != std::string::npos) {
      wvrFirst.insert(i);
      if (name[i] == wvrNominal) {
        wvrSecond.insert(i);
      }
    } else if (spwInfo[i].nchans == 1 && name[i].find("FULL_RES") == std::string::npos) {
      avgSpw.insert(i);
    } else if (spwInfo[i].bandwidth < 2e9 ||
               (nchan >= 15 &&
                !(nchan == 256 || nchan == 128 || nchan == 64 || nchan == 32 || nchan == 16 ||
                  nchan == 248 || nchan == 124 || nchan == 62 || nchan == 31))) {
      fdmSpw.insert(i);
    } else {
      const auto spwToPol = _getSpwToPolMap();
      if (spwToPol[i].size() == 1 &&
          spwInfo[i].nchans * getNumCorrs()[*(spwToPol[i].begin())] > 256) {
        fdmSpw.insert(i);
      } else {
        tdmSpw.insert(i);
      }
    }
    // CAS-13973 for ALMA get subwindow and receiver band
    spwInfo[i].rb = -1;
    if (regex_search(name[i], match, rb)) {
      spwInfo[i].rb = stoi(match.str(1));
    } else {
      if (!freqBands) {
        freqBands = _almaReceiverBands(nrows);
      }
      spwInfo[i].rb = (*freqBands)[i];
    }
    spwInfo[i].sw = -1;
    if (regex_search(name[i], match, sw)) {
      spwInfo[i].sw = stoi(match.str(1));
    }
    // CAS-13973 for ALMA get subwindow and receiver band
    spwInfo[i].rb = -1;
    if (regex_search(name[i], match, rb)) {
      spwInfo[i].rb = stoi(match.str(1));
    } else {
      if (!freqBands) {
        freqBands = _almaReceiverBands(nrows);
      }
      spwInfo[i].rb = (*freqBands)[i];
    }
    spwInfo[i].sw = -1;
    if (regex_search(name[i], match, sw)) {
      spwInfo[i].sw = stoi(match.str(1));
    }
  }
  wvrSpw = wvrSecond.empty() ? wvrFirst : wvrSecond;
  return spwInfo;
}

std::map<int, unsigned int> MSMetaData::_toUIntMap(const Vector<int>& v) {
  ThrowIf(anyLT(v, 0), "Column that should contain nonnegative ints has a negative int");
  std::map<int, unsigned int> m;
  int count = 0;
  for (Vector<int>::const_iterator iter = v.begin(); iter != v.end(); ++iter, ++count) {
    m[count] = *iter;
  }
  return m;
}

std::set<SubScanKey> MSMetaData::getSubScanKeys(const ArrayKey& arrayKey) const {
  std::map<ArrayKey, std::set<SubScanKey>> mymap = _getArrayKeysToSubScanKeys();
  std::map<ArrayKey, std::set<SubScanKey>>::const_iterator iter = mymap.find(arrayKey);
  ThrowIf(iter == mymap.end(), "MS does not contain requested ArrayKey");
  return iter->second;
}

std::map<ArrayKey, std::set<SubScanKey>> MSMetaData::_getArrayKeysToSubScanKeys() const {
  // this method is responsible for setting _arrayToSubScans
  if (!_arrayToSubScans.empty()) {
    return _arrayToSubScans;
  }
  std::set<SubScanKey> subScans = _getSubScanKeys();
  std::set<SubScanKey>::const_iterator iter = subScans.begin();
  std::set<SubScanKey>::const_iterator end = subScans.end();
  std::map<ArrayKey, std::set<SubScanKey>> mymap;
  ArrayKey akey;
  for (; iter != end; ++iter) {
    akey.arrayID = iter->arrayID;
    akey.obsID = iter->obsID;
    if (mymap.find(akey) == mymap.end()) {
      mymap[akey] = std::set<SubScanKey>();
    }
    mymap[akey].insert(*iter);
  }
  unsigned int mysize = 0;
  std::map<ArrayKey, std::set<SubScanKey>>::const_iterator miter = mymap.begin();
  std::map<ArrayKey, std::set<SubScanKey>>::const_iterator mend = mymap.end();
  for (; miter != mend; ++miter) {
    mysize += sizeof(SubScanKey) * miter->second.size();
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

}  // namespace casacore
