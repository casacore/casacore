//# MSMetaData.h
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

#ifndef MS_MSMETADATA_H
#define MS_MSMETADATA_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/QVector.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSPointingColumns.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <map>

namespace casacore {

template <class T> class ArrayColumn;
struct ArrayKey;
struct ScanKey;
struct SubScanKey;

// <summary>
// Class to interrogate  an MS for metadata. Interrogation happens on demand
// and resulting metadata are stored for use by subsequent queries if the
// cache has not exceeded the specified limit.
// </summary>

class MSMetaData {

public:

	// for retrieving stats
	enum CorrelationType {
		AUTO,
		CROSS,
		BOTH
	};

	enum SQLDSwitch {
		SQLD_INCLUDE,
		SQLD_EXCLUDE,
		SQLD_ONLY
	};

	// construct an object which stores a pointer to the MS and queries the MS
	// only as necessary. The MeasurementSet pointer passed in should not go out
	// of scope in the calling code until the caller has finished with this object,
	// or else subsequent method calls on this object will result in a segmentation
	// fault; the pointer is not copied.
	// <src>maxCacheSizeMB</src> is the maximum cache size in megabytes. <=0 means
	// do not use a cache, in which case, each method call will have to (re)query
	// the MS. It is highly recommended to use a cache of reasonable size for the
	// specified MS if multiple methods are going to be called.
	MSMetaData(const MeasurementSet *const &ms, const Float maxCacheSizeMB);

	virtual ~MSMetaData();

	// get the antenna diameters
	Quantum<Vector<Double> > getAntennaDiameters();

	// get the antenna ID for the antenna with the specified name.
	vector<uInt> getAntennaIDs(	const vector<String>& antennaNames);

	// get the name of the antenna for the specified antenna ID
	vector<String> getAntennaNames(
		std::map<String, uInt>& namesToIDsMap,
		const vector<uInt>& antennaIDs=vector<uInt>(0)
	);

	// get the antenna stations for the specified antenna IDs
	vector<String> getAntennaStations(const vector<uInt>& antennaIDs);

	// get the antenna stations for the specified antenna names
	vector<String> getAntennaStations(const vector<String>& antennaNames);

	vector<uInt> getDataDescIDToPolIDMap() const;

	std::map<String, std::set<Int> > getIntentToFieldsMap();

	std::map<String, std::set<ScanKey> > getIntentToScansMap();

	std::map<String, std::set<uInt> > getIntentToSpwsMap();

	std::set<String> getIntentsForScan(const ScanKey& scan) const;

	// get all intents, in no particular (nor guaranteed) order.
	std::set<String> getIntents() const;

	// get a set of intents corresponding to a specified field
	std::set<String> getIntentsForField(Int fieldID);

	// get a set of intents corresponding to the specified spectral window
	std::set<String> getIntentsForSpw(const uInt spw);
	// get unique scan numbers

	std::set<Int> getScanNumbers(Int obsID, Int arrayID) const;

	std::set<Int> getScansForState(
		Int stateID, Int obsID, Int arrayID
	);

	// get a set of spectral windows for which the specified <src>intent</src>
	// applies.
	virtual std::set<uInt> getSpwsForIntent(const String& intent);

	// get the number of visibilities
	uInt nRows() const;

	uInt nRows(CorrelationType cType);

	uInt nRows(
		CorrelationType cType, Int arrayID, Int observationID,
		Int scanNumber, Int fieldID
	) const;

	uInt nRows(CorrelationType cType, uInt fieldID) const;

	// get number of spectral windows
	uInt nSpw(Bool includewvr) const;

	// number of unique states (number of rows from the STATE table)
	uInt nStates() const;

	// get the number of fields.
	uInt nFields() const;

	// get a mapping of spectral window ID to data descrption IDs
	std::vector<std::set<uInt> > getSpwToDataDescriptionIDMap() const;

	// get a set of spectral windows corresponding to the specified fieldID
	std::set<uInt> getSpwsForField(const Int fieldID);

	// get a set of spectral windows corresponding to the specified field name
	std::set<uInt> getSpwsForField(const String& fieldName);

	// get the set of field IDs corresponding to the specified spectral window.
	std::set<Int> getFieldIDsForSpw(const uInt spw);

	// get the set of field names corresponding to the specified spectral window.
	std::set<String> getFieldNamesForSpw(const uInt spw);


	// get the set of spectral windows for the specified scan number.
	std::set<uInt> getSpwsForScan(const ScanKey& scan) const;

	// get the set of scan numbers for the specified spectral window.
	std::set<Int> getScansForSpw(uInt spw, Int obsID, Int arrayID) const;

	// get the number of antennas in the ANTENNA table
	uInt nAntennas() const;

	// ALMA-specific. get set of spectral windows used for TDM. These are windows that have
	// 64, 128, or 256 channels
	std::set<uInt> getTDMSpw();

	// ALMA-specific. get set of spectral windows used for FDM. These are windows that do not
	// have 1, 4, 64, 128, or 256 channels.
	std::set<uInt> getFDMSpw();

	// ALMA-specific. get spectral windows that have been averaged. These are windows with 1 channel.
	std::set<uInt> getChannelAvgSpw();

	// ALMA-specific. Get the spectral window set used for WVR measurements. These have 4 channels each.
	std::set<uInt> getWVRSpw() const;

	// ALMA-specific. Get the square law detector (total power) spectral windows.
	std::set<uInt> getSQLDSpw();

	// Get the scans which fail into the specified time range (center-tol to center+tol)
	std::set<Int> getScansForTimes(
		Double center, Double tol,
		Int obsID, Int arrayID
	) const;

	// Get the times for the specified scans
	std::set<Double> getTimesForScans(std::set<ScanKey> scans) const;

	// get the times for the specified scan.
	// The return values come from the TIME column.
	std::set<Double> getTimesForScan(const ScanKey& scan) const;

	// get the time range for the specified scan. The pair will contain
	// the start and stop time of the scan, determined from min(TIME(x)-0.5*INTERVAL(x)) and
	// max(TIME(x)-0.5*INTERVAL(x))
	std::pair<Double, Double> getTimeRangeForScan(const ScanKey& scanKey) const;

	// get the times for the specified scan
	// std::set<Double> getTimesForScan(const uInt scan) const;

	// get the stateIDs associated with the specified scan.
	std::set<Int> getStatesForScan(uInt obsID, uInt arrayID, Int scan) const;

	// get the position of the specified antenna relative to the observatory position.
	// the three vector returned represents the longitudinal, latitudinal, and elevation
	// offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
	// measured along the surface of a sphere centered at the earth's center and whose surface
	// intersects the position of the observatory.
	Quantum<Vector<Double> > getAntennaOffset(uInt which);

	Quantum<Vector<Double> > getAntennaOffset(const String& name);

	vector<Quantum<Vector<Double> > > getAntennaOffsets() const;
	// get the positions of the specified antennas. If <src>which</src> is empty, return
	// all antenna positions.

	vector<MPosition> getAntennaPositions(
		const vector<uInt>& which=std::vector<uInt>(0)
	) const;

	// <src>names</src> cannot be empty.
	vector<MPosition> getAntennaPositions(const vector<String>& names);

	std::map<uInt, Double> getAverageIntervalsForScan(const ScanKey& scan) const;

	vector<uInt> getBBCNos() const;

	std::map<uInt, std::set<uInt> > getBBCNosToSpwMap(SQLDSwitch sqldSwitch);

	vector<vector<Double> > getEdgeChans();

	// get the field IDs for the specified field name. Case insensitive.
	std::set<Int> getFieldIDsForField(const String& field) const;

	// get field IDs associated with the specified scan number.
	std::set<Int> getFieldsForScan(const ScanKey& scan) const;

	// get the field IDs associated with the specified scans
	std::set<Int> getFieldsForScans(
		const std::set<Int>& scans, Int obsID, Int arrayID
	) const;

	// get the field IDs associated with the specified intent.
	std::set<Int> getFieldsForIntent(const String& intent);

	// get the field IDs associated with the specified source.
	std::set<Int> getFieldsForIntent(uInt sourceID) const;

	std::map<Int, std::set<Int> > getFieldsForSourceMap() const;

	std::map<Int, std::set<String> > getFieldNamesForSourceMap() const;

	// get the field names associated with the specified field IDs. If <src>fieldIDs</src>
	// is empty, a vector of all the field names is returned.
	vector<String> getFieldNamesForFieldIDs(const vector<uInt>& fieldIDs);

	// Get the fields which fail into the specified time range (center-tol to center+tol)
	std::set<Int> getFieldsForTimes(Double center, Double tol);

	// get telescope names in the order they are listed in the OBSERVATION table. These are
	// the telescopes (observatories), not the antenna names.
	vector<String> getObservatoryNames();

	// get the position of the specified telescope (observatory).
	MPosition getObservatoryPosition(uInt which) const;

	// get the scans associated with the specified intent
	std::set<Int> getScansForIntent(
		const String& intent, Int obsID, Int arrayID
	) const;

	// get the scan numbers associated with the specified field ID.
	std::set<Int> getScansForFieldID(Int fieldID, Int obsID, Int arrayID) const;

	// get the scan numbers associated with the specified field. Subclasses should not implement or override.
	std::set<Int> getScansForField(const String& field, Int obsID, Int arrayID) const;

	// The first value of the pair is spw, the second is polarization ID.
	std::map<std::pair<uInt, uInt>, uInt> getSpwIDPolIDToDataDescIDMap() const;

	vector<String> getSpwNames() const;

	// get a data structure, consumable by users, representing a summary of the dataset
	Record getSummary() const;

	// get the times for which the specified field was observed
	std::set<Double> getTimesForField(Int fieldID);

	// get the time stamps associated with the specified intent
	std::set<Double> getTimesForIntent(const String& intent) const;
	Bool hasBBCNo() const;

	//std::map<Double, Double> getExposuresForTimes() const;

	// get the unique baselines in the MS. These are not necessarily every combination of the
	// n(n-1)/2 possible antenna pairs, but rather the number of unique baselines represented in
	// the main MS table, which in theory can be less than n(n-1)/2 (for example if samples for
	// certain antenna pairs are not recorded. The returned Matrix is nAnts x nAnts in size. Pairs
	// that are true represent baselines represented in the main MS table.
	Matrix<Bool> getUniqueBaselines();

	// get the number of unique baselines represented in the main MS table which in theory can be
	// less than n*(n-1)/2
	virtual uInt nBaselines();

	// get the effective total exposure time. This is the effective time spent collecting unflagged data.
	Quantity getEffectiveTotalExposureTime();

	// get the number of scans in the dataset
	uInt nScans();

	// get the number of observations (from the OBSERVATIONS table) in the dataset
	uInt nObservations() const;

	// get the number of arrays (from the ARRAY table) in the dataset
	uInt nArrays();

	// get the number of data description IDs (from the DATA_DESCRIPTION table)
	uInt nDataDescriptions() const;

	// get the number of unflagged rows
	Double nUnflaggedRows() const;

	Double nUnflaggedRows(CorrelationType cType) const;

	Double nUnflaggedRows(
		CorrelationType cType, Int arrayID, uInt observationID,
		Int scanNumber, uInt fieldID
	) const;

	Double nUnflaggedRows(CorrelationType cType, Int fieldID) const;

	inline Float getCache() const { return _cacheMB;}

	vector<Double> getBandWidths() const;

	vector<QVD> getChanFreqs() const;

	vector<QVD> getChanWidths() const;

	vector<Int> getNetSidebands();

	vector<Quantity> getMeanFreqs();

	vector<Quantity> getCenterFreqs() const;

	vector<uInt> nChans() const;

	uInt nPol();

	// get a map of data desc ID, scan number pair to exposure time for the first time
	// for that data desc ID, scan number pair
	vector<std::map<Int, Quantity> > getFirstExposureTimeMap();

	// get polarization IDs for the specified scan and spwid
	std::set<uInt> getPolarizationIDs(uInt obsID, Int arrayID, Int scan, uInt spwid) const;

	// get unique field IDs that exist in the main table.
	std::set<Int> getUniqueFiedIDs() const;

	// get the pointing directions associated with antenna1 and antenna2 for
	// the specified row of the main MS table
	std::pair<MDirection, MDirection> getPointingDirection(
		Int& ant1, Int& ant2, Double& time, uInt row,
		Bool interpolate=false, Int initialguess=0
	) const;

	// get the time range for the entire dataset. min(TIME(x) - 0.5*INTERVAL(x)) to
	// max(TIME(x) + 0.5*INTERVAL(x))
	std::pair<Double, Double> getTimeRange() const;

private:

	struct SpwProperties {
		Double bandwidth;
		QVD chanfreqs;
		QVD chanwidths;
		Int netsideband;
		// The sum of all channel frequencies divided by the number of channels
		Quantity meanfreq;
		// The mean of the low frequency extent of the lowest frequency channel and
		// the high frequency extend of the highest frequency channel. Often, but not
		// necessarily, the same as meanfreq
		Quantity centerfreq;
		uInt nchans;
		// The center frequencies of the two channels at the edges of the window
		vector<Double> edgechans;
		uInt bbcno;
		String name;
	};

	struct TimeStampProperties {
		std::set<Int> ddIDs;
		uInt nrows;
	};

	struct SubScanProperties {
		std::set<Int> antennas;
		Double beginTime;
		std::set<uInt> ddIDs;
		Double endTime;
		uInt nrows;
		std::set<Int> stateIDs;
		std::map<Double, TimeStampProperties> timeProps;
	};

	// The general pattern is that a mutable gets set only once, on demand, when its
	// setter is called for the first time. If this pattern is broken, defective behavior
	// will occur.

	const MeasurementSet* _ms;
	mutable Float _cacheMB;
	const Float _maxCacheMB;
	mutable uInt _nStates, _nACRows, _nXCRows, _nSpw, _nFields, _nAntennas,
		_nObservations, _nScans, _nArrays, _nrows, _nPol, _nDataDescIDs;
	mutable std::map<ScanKey, std::set<uInt> > _scanToSpwsMap, _scanToDDIDsMap;
	mutable vector<uInt> _dataDescIDToSpwMap, _dataDescIDToPolIDMap /*, _scanToNRowsMap */;
	std::map<Int, std::set<uInt> > _fieldToSpwMap;
	mutable std::map<ScanKey, std::set<Int> > _scanToStatesMap, _scanToFieldsMap;
	mutable std::map<Int, std::set<Int> >	_fieldToStatesMap, _stateToFieldsMap, _sourceToFieldsMap /*,
		_arrayToScansMap, _scanToAntennasMap */;
	mutable std::map<std::pair<uInt, uInt>, uInt> _spwPolIDToDataDescIDMap;
	std::map<String, uInt> _antennaNameToIDMap;
	//mutable std::map<ArrayKey, ArrayProperties> _arrayProperties;
	//mutable std::map<ScanKey, ScanProperties> _scanProperties;
	mutable std::map<SubScanKey, SubScanProperties> _subScanProperties;

	mutable std::map<String, std::set<Int> > _intentToFieldIDMap;
	mutable std::map<String, std::set<ScanKey> > _intentToScansMap;
	mutable std::map<ScanKey, std::pair<Double, Double> > _scanToTimeRangeMap;
	mutable std::map<ScanKey, std::map<uInt, Double> > _scanSpwToIntervalMap;
	mutable std::map<std::pair<ScanKey, uInt>, std::set<uInt> > _scanSpwToPolIDMap;
	mutable std::set<String> _uniqueIntents;
	mutable std::set<Int> /*_uniqueScanNumbers, */ _uniqueFieldIDs, _uniqueStateIDs;
	mutable std::set<uInt> _avgSpw, _tdmSpw, _fdmSpw, _wvrSpw, _sqldSpw;
	mutable CountedPtr<Vector<Int> > _antenna1, _antenna2, _scans, _fieldIDs,
		_stateIDs, _dataDescIDs, _observationIDs, _arrayIDs;
	//mutable CountedPtr<AOSFMapI> _scanToNACRowsMap, _scanToNXCRowsMap;
	mutable CountedPtr<std::map<SubScanKey, uInt> > _subScanToNACRowsMap, _subScanToNXCRowsMap;
	//mutable CountedPtr<std::map<Int, uInt> > _fieldToNACRowsMap, _fieldToNXCRowsMap;

	mutable CountedPtr<vector<uInt> > _fieldToNACRowsMap, _fieldToNXCRowsMap;
 	mutable std::map<ScanKey, std::set<String> > _scanToIntentsMap;
	mutable vector<std::set<String> > _stateToIntentsMap, _spwToIntentsMap, _fieldToIntentsMap;
	mutable vector<SpwProperties> _spwInfo;
	mutable vector<std::set<Int> > _spwToFieldIDsMap, _obsToArraysMap;
	mutable vector<std::set<ScanKey> > _spwToScansMap, _ddidToScansMap, _fieldToScansMap;

	mutable vector<String> _fieldNames, _antennaNames, _observatoryNames, _stationNames;
	mutable CountedPtr<Vector<Double> > _times;
	CountedPtr<Quantum<Vector<Double> > > _exposures;
	mutable CountedPtr<std::map<ScanKey, std::set<Double> > > _scanToTimesMap;
	std::map<String, std::set<uInt> > _intentToSpwsMap;
	mutable std::map<String, std::set<Double> > _intentToTimesMap;

	CountedPtr<std::map<Int, std::set<Double> > > _fieldToTimesMap;
	CountedPtr<std::map<Double, std::set<Int> > > _timeToFieldsMap;

	mutable vector<MPosition> _observatoryPositions, _antennaPositions;
	mutable vector<Quantum<Vector<Double> > > _antennaOffsets;
	Quantum<Vector<Double> > _antennaDiameters;
	Matrix<Bool> _uniqueBaselines;
	Quantity _exposureTime;
	mutable Double _nUnflaggedACRows, _nUnflaggedXCRows;
	mutable CountedPtr<vector<Double> > _unflaggedFieldNACRows, _unflaggedFieldNXCRows;
	mutable CountedPtr<std::map<SubScanKey, Double> > _unflaggedSubScanNACRows, _unflaggedSubScanNXCRows;
	// mutable CountedPtr<AOSFMapD> _unflaggedScanNACRows, _unflaggedScanNXCRows;
	const String _taqlTableName;
	const vector<const Table*> _taqlTempTable;
	mutable CountedPtr<ArrayColumn<Bool> > _flagsColumn;

	mutable Bool _spwInfoStored;
	vector<std::map<Int, Quantity> > _firstExposureTimeMap;
	mutable vector<Int> _numCorrs;

	mutable std::set<ArrayKey> _arrayKeys;
	mutable std::set<ScanKey> _scanKeys;
	mutable std::set<SubScanKey> _subscans;
	mutable std::map<ScanKey, std::set<SubScanKey> > _scanToSubScans;

	//mutable CountedPtr<std::map<Double, TimeStampProperties> > _timeStampPropsMap;

	// disallow copy constructor and = operator
	MSMetaData(const MSMetaData&);
	MSMetaData operator =(const MSMetaData&);

	// This comment from thunter in the original ValueMapping python class
	// # Determine the number of polarizations for the first OBSERVE_TARGET intent.
    // # Used by plotbandpass for BPOLY plots since the number of pols cannot be inferred
    // # correctly from the caltable alone.  You cannot not simply use the first row, because
    // # it may be a pointing scan which may have different number of polarizations than what
    // # the TARGET and BANDPASS calibrator will have.
    // # -- T. Hunter
	// uInt _getNumberOfPolarizations();

	void _setSpwInfo(const MeasurementSet& ms);

	// set metadata from OBSERVATION table
	void _setObservation(const MeasurementSet& ms);

	Bool _cacheUpdated(const Float incrementInBytes) const;

	void _checkField(uInt fieldID) const;

	//static void _checkScan(const Int scan, const std::set<Int> allScans);

	void _checkScan(const ScanKey& key) const;

	void _checkScans(const std::set<ScanKey>& scanKeys) const;

	void _checkSubScan(const SubScanKey& key) const;

	static void _checkTolerance(const Double tol);

	void _createScanRecords(
		Record& parent, const ArrayKey& arrayKey,
		const std::map<SubScanKey, SubScanProperties>& subScanProps
	) const;

	void _createSubScanRecords(
		Record& parent, uInt& scanNRows, std::set<Int>& antennasForScan,
		const ScanKey& scanKey, const std::map<SubScanKey, SubScanProperties>& subScanProps
	) const;

	static void _createTimeStampRecords(
		Record& parent,
		const SubScanProperties& subScanProps
	);

	vector<String> _getAntennaNames(
		std::map<String, uInt>& namesToIDsMap
	);

	vector<MPosition> _getAntennaPositions() const;

	void _getAntennas(
		CountedPtr<Vector<Int> >& ant1,
		CountedPtr<Vector<Int> >& ant2
	) const;
	CountedPtr<Vector<Int> > _getArrayIDs() const;

	CountedPtr<Vector<Int> > _getDataDescIDs() const;

	vector<uInt> _getDataDescIDToSpwMap() const;

	CountedPtr<Quantum<Vector<Double> > > _getExposureTimes();

	void _getFieldsAndIntentsMaps(
		vector<std::set<String> >& fieldToIntentsMap,
		std::map<String, std::set<Int> >& intentToFieldsMap
	);

	void _getFieldsAndScansMaps(
		vector<std::set<ScanKey> >& fieldToScansMap,
		std::map<ScanKey, std::set<Int> >& scanToFieldsMap
	) const;

	void _getFieldsAndSpwMaps(
		std::map<Int, std::set<uInt> >& fieldToSpwMap,
		vector<std::set<Int> >& spwToFieldMap
	);

	void _getFieldsAndStatesMaps(
		std::map<Int, std::set<Int> >& fieldToStatesMap,
		std::map<Int, std::set<Int> >& stateToFieldsMap
	);

	void _getFieldsAndTimesMaps(
		CountedPtr<std::map<Int, std::set<Double> > >& fieldToTimesMap,
		CountedPtr<std::map<Double, std::set<Int> > >& timesToFieldMap
	);

	CountedPtr<Vector<Int> > _getFieldIDs() const;

	vector<String> _getFieldNames() const;

	CountedPtr<ArrayColumn<Bool> > _getFlags() const;

	std::map<String, std::set<Double> > _getIntentsToTimesMap() const;

	CountedPtr<Vector<Int> > _getObservationIDs() const;

	CountedPtr<Vector<Int> > _getScans() const;

	vector<std::set<String> > _getSpwToIntentsMap();

	CountedPtr<Vector<Int> > _getStateIDs() const;

	CountedPtr<Vector<Double> > _getTimes() const;

	//CountedPtr<std::map<Double, TimeStampProperties> > _getTimeStampProperties() const;

	Bool _hasIntent(const String& intent) const;

	Bool _hasFieldID(Int fieldID) const;

	Bool _hasStateID(Int stateID);

	void _hasAntennaID(Int antennaID);

	std::map<Double, Double> _getTimeToTotalBWMap(
		const Vector<Double>& times, const Vector<Int>& ddIDs
	);

	MDirection _getInterpolatedDirection(
		const ROMSPointingColumns& pCols, const Int& index,
		const Double& time
	) const;

	// number of correlations from the polarzation table.
	vector<Int> _getNumCorrs() const;

	vector<std::set<Int> > _getObservationIDToArrayIDsMap() const;

	vector<MPosition> _getObservatoryPositions();

	void _getRowStats(
		uInt& nACRows, uInt& nXCRows,
		std::map<SubScanKey, uInt>*& subScanToNACRowsMap,
		std::map<SubScanKey, uInt>*& subScanToNXCRowsMap,
		vector<uInt>*& fieldToNACRowsMap,
		vector<uInt>*& fieldToNXCRowsMap
	) const;

	void _getRowStats(
		uInt& nACRows, uInt& nXCRows,
		CountedPtr<std::map<SubScanKey, uInt> >& scanToNACRowsMap,
		CountedPtr<std::map<SubScanKey, uInt> >& scanToNXCRowsMap,
		CountedPtr<vector<uInt> >& fieldToNACRowsMap,
		CountedPtr<vector<uInt> >& fieldToNXCRowsMap
	) const;

	// get all ScanKeys in the dataset
	std::set<ScanKey> _getScanKeys() const;

	// get all ScanKeys in the dataset that have the specified arrayKey
	std::set<ScanKey> _getScanKeys(const ArrayKey& arrayKey) const;

	// get the scan keys in the specified set that have the associated arrayKey
	std::set<ScanKey> _getScanKeys(
		const std::set<ScanKey>& scanKeys, const ArrayKey& arrayKey
	) const;

	// get all valid scan numbers associated with the specified arrayKey
	std::set<Int> _getScanNumbers(const ArrayKey& arrayKey) const;

	// get the scan numbers associated with the scanKeys. The members of
	// scanKeys do not have to have the same ArrayKey, and no warning is
	// given if they are not.
	std::set<Int> _getScanNumbers(const std::set<ScanKey>& scanKeys) const;

	void _getScansAndDDIDMaps(
		std::map<ScanKey, std::set<uInt> >& scanToDDIDMap,
		vector<std::set<ScanKey> >& ddIDToScanMap
	) const;

	void _getScansAndIntentsMaps(
		std::map<ScanKey, std::set<String> >& scanToIntentsMap,
		std::map<String, std::set<ScanKey> >& intentToScansMap
	) const;

	void _getScansAndSpwMaps(
		std::map<ScanKey, std::set<uInt> >& scanToSpwMap,
		vector<std::set<ScanKey> >& spwToScanMap
	) const;

	std::map<ScanKey, std::set<Int> > _getScanToStatesMap() const;

	std::map<ScanKey, std::set<SubScanKey> > _getScanToSubScansMap() const;

	CountedPtr<std::map<ScanKey, std::set<Double> > > _getScanToTimesMap() const;

	vector<SpwProperties> _getSpwInfo(
		std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw,
		std::set<uInt>& fdmSpw, std::set<uInt>& wvrSpw,
		std::set<uInt>& sqldSpw
	) const;

	void _getSpwsAndIntentsMaps(
		vector<std::set<String> >& spwToIntentsMap,
		std::map<String, std::set<uInt> >& intentToSpwsMap
	);

	vector<SpwProperties>  _getSpwInfo2(
		std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
		std::set<uInt>& wvrSpw, std::set<uInt>& sqldSpw
	) const;

	void _getStateToIntentsMap(
		vector<std::set<String> >& statesToIntentsMap,
		std::set<String>& uniqueIntents
	) const;

	vector<String> _getStationNames();

	std::map<SubScanKey, SubScanProperties> _getSubScanProperties() const;

	std::set<SubScanKey> _getSubScanKeys() const;

	// get subscans related to the given scan
	std::set<SubScanKey> _getSubScanKeys(const ScanKey& scanKey) const;

	//In scanSpwToIntervalMap, the key is a scan, spw pair.
	void _getTimesAndInvervals(
		std::map<ScanKey, std::pair<Double, Double> >& scanToTimeRangeMap,
		std::map<ScanKey, std::map<uInt, Double> >& scanSpwToIntervalMap
	) const;

	void _getUnflaggedRowStats(
		Double& nACRows, Double& nXCRows,
		CountedPtr<std::map<SubScanKey, Double> >& subScanToNACRowsMap,
		CountedPtr<std::map<SubScanKey, Double> >& subScanToNXCRowsMap,
		CountedPtr<vector<Double> >& fieldToNACRowsMap,
		CountedPtr<vector<Double> >& fieldToNXCRowsMap
	) const;

	void _getUnflaggedRowStats(
		Double& nACRows, Double& nXCRows,
		vector<Double>*& fieldNACRows, vector<Double>*& fieldNXCRows,
		std::map<SubScanKey, Double>*& scanNACRows,
		std::map<SubScanKey, Double>*& scanNXCRows
	) const;

	static uInt _sizeof(const std::map<Double, MSMetaData::TimeStampProperties> & m);

	template <class T>
	static uInt _sizeof(const std::map<T, std::set<String> >& m);

	template <class T, class U>
	static uInt _sizeof(const std::map<T, std::set<U> >& m);

	template <class T, class U>
	static uInt _sizeof(const std::map<T, U>& m);

	static uInt _sizeof(const vector<std::set<String> >& m);

	static uInt _sizeof(const vector<String>& m);

	template <class T>
	static uInt _sizeof(const vector<T>& v);

	static uInt _sizeof(const Quantum<Vector<Double> >& m);

	template <class T>
	static uInt _sizeof(const vector<std::set<T> >& v);

	template <class T> static uInt _sizeof(const std::map<String, std::set<T> >& map);

	static uInt _sizeof(const vector<std::map<Int, Quantity> >& map);

	static uInt _sizeof(const std::map<std::pair<Int, uInt>, std::set<uInt> >& map);

	static std::map<Int, uInt> _toUIntMap(const Vector<Int>& v);

};



}

#endif
