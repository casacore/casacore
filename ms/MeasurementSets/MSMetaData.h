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

#ifndef MS_MSMETADATA_H
#define MS_MSMETADATA_H

#include <casa/Quanta/QVector.h>
#include <measures/Measures/MPosition.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <casa/Utilities/CountedPtr.h>
#include <map>

namespace casa {

template <class T> class ArrayColumn;


// <summary>
// Class to interrogate  an MS for metadata. Interrogation happens on demand
// and resulting metadata are stored if the cache has not exceeded the specified
// limit.
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
	// do not use a cache.
	MSMetaData(const MeasurementSet *const &ms, const Float maxCacheSizeMB);

	virtual ~MSMetaData();

	// number of unique states (number of rows from the STATE table)
	uInt nStates();

	// get unique scan numbers
	std::set<Int> getScanNumbers();

	std::set<Int> getScansForState(const Int stateID);

	std::set<String> getIntentsForScan(const Int scan);

	// get all intents, in no particular (nor guaranteed) order.
	std::set<String> getIntents();

	// get a set of spectral windows for which the specified <src>intent</src>
	// applies.
	virtual std::set<uInt> getSpwsForIntent(const String& intent);

	// get number of spectral windows
	uInt nSpw(Bool includewvr);

	// get the number of visibilities
	uInt nRows();

	uInt nRows(CorrelationType cType);

	uInt nRows(
		CorrelationType cType, Int arrayID, Int observationID,
		Int scanNumber, Int fieldID
	);

	uInt nRows(CorrelationType cType, Int fieldID);

	// get a set of intents corresponding to the specified spectral window
	std::set<String> getIntentsForSpw(const uInt spw);

	// get a set of intents corresponding to a specified field
	std::set<String> getIntentsForField(Int fieldID);

	std::map<String, std::set<Int> > getIntentToFieldsMap();

	std::map<String, std::set<Int> > getIntentToScansMap();

	std::map<String, std::set<uInt> > getIntentToSpwsMap();

	// get the number of fields.
	uInt nFields();

	// get a set of spectral windows corresponding to the specified fieldID
	std::set<uInt> getSpwsForField(const Int fieldID);

	// get a set of spectral windows corresponding to the specified field name
	std::set<uInt> getSpwsForField(const String& fieldName);

	// get the set of field IDs corresponding to the specified spectral window.
	std::set<Int> getFieldIDsForSpw(const uInt spw);

	// get the set of field names corresponding to the specified spectral window.
	std::set<String> getFieldNamesForSpw(const uInt spw);

	// get the number of scans in the dataset
	uInt nScans();

	// get the number of observations (from the OBSERVATIONS table) in the dataset
	uInt nObservations();

	// get the number of arrays (from the ARRAY table) in the dataset
	uInt nArrays();

	// get the number of data description IDs (from the DATA_DESCRIPTION table)
	uInt nDataDescriptions();

	// get the set of spectral windows for the specified scan number.
	std::set<uInt> getSpwsForScan(const Int scan);

	// get the set of scan numbers for the specified spectral window.
	std::set<Int> getScansForSpw(const uInt spw);

	// get the number of antennas in the ANTENNA table
	uInt nAntennas();

	// get the name of the antenna for the specified antenna ID
	vector<String> getAntennaNames(
		std::map<String, uInt>& namesToIDsMap,
		const vector<uInt>& antennaIDs=vector<uInt>(0)
	);

	// get the antenna ID for the antenna with the specified name.
	vector<uInt> getAntennaIDs(	const vector<String>& antennaNames);

	// get the antenna stations for the specified antenna IDs
	vector<String> getAntennaStations(const vector<uInt>& antennaIDs);

	// get the antenna stations for the specified antenna names
	vector<String> getAntennaStations(const vector<String>& antennaNames);

	// get the antenna diameters
	Quantum<Vector<Double> > getAntennaDiameters();

	// ALMA-specific. get set of spectral windows used for TDM. These are windows that have
	// 64, 128, or 256 channels
	std::set<uInt> getTDMSpw();

	// ALMA-specific. get set of spectral windows used for FDM. These are windows that do not
	// have 1, 4, 64, 128, or 256 channels.
	std::set<uInt> getFDMSpw();

	// ALMA-specific. get spectral windows that have been averaged. These are windows with 1 channel.
	std::set<uInt> getChannelAvgSpw();

	// ALMA-specific. Get the spectral window set used for WVR measurements. These have 4 channels each.
	std::set<uInt> getWVRSpw();

	// ALMA-specific. Get the square law detector (total power) spectral windows.
	std::set<uInt> getSQLDSpw();

	// Get the scans which fail into the specified time range (center-tol to center+tol)
	std::set<Int> getScansForTimes(const Double center, const Double tol);

	// Get the times for the specified scans
	std::set<Double> getTimesForScans(const std::set<Int>& scans);

	// get the times for the specified scan.
	// The return values come from the TIME column.
	std::set<Double> getTimesForScan(const Int scan);

	// get the time range for the specified scan. The vector returned will contain two elements,
	// the start and stop time of the scan, determined from min(TIME_CENTROID(x)-0.5*INTERVAL(x)) and
	// max(TIME_CENTROID(x)-0.5*INTERVAL(x))
	std::vector<Double> getTimeRangeForScan(Int scan);

	// get the times for the specified scan
	// std::set<Double> getTimesForScan(const uInt scan) const;

	// get the stateIDs associated with the specified scan number.
	std::set<Int> getStatesForScan(const Int scan);

	// get the scans associated with the specified intent
	std::set<Int> getScansForIntent(const String& intent);

	// get the scan numbers associated with the specified field ID.
	std::set<Int> getScansForFieldID(const Int fieldID);

	// get the scan numbers associated with the specified field. Subclasses should not implement or override.
	std::set<Int> getScansForField(const String& field);

	// get the field IDs for the specified field name. Case insensitive.
	std::set<Int> getFieldIDsForField(const String& field);

	// get field IDs associated with the specified scan number.
	std::set<Int> getFieldsForScan(const Int scan);

	// get the field IDs associated with the specified scans
	std::set<Int> getFieldsForScans(const std::set<Int>& scans);

	// get the field IDs associated with the specified intent.
	std::set<Int> getFieldsForIntent(const String& intent);

	// get the field names associated with the specified field IDs. If <src>fieldIDs</src>
	// is empty, a vector of all the field names is returned.
	vector<String> getFieldNamesForFieldIDs(const vector<uInt>& fieldIDs);

	// Get the fields which fail into the specified time range (center-tol to center+tol)
	std::set<Int> getFieldsForTimes(Double center, Double tol);

	// get the times for which the specified field was observed
	std::set<Double> getTimesForField(Int fieldID);

	// get telescope names in the order they are listed in the OBSERVATION table. These are
	// the telescopes (observatories), not the antenna names.
	vector<String> getObservatoryNames();

	// get the position of the specified telescope (observatory).
	MPosition getObservatoryPosition(uInt which);

	// get the positions of the specified antennas. If <src>which</src> is empty, return
	// all antenna positions.
	vector<MPosition> getAntennaPositions(const vector<uInt>& which=std::vector<uInt>(0));

	// <src>names</src> cannot be empty.
	vector<MPosition> getAntennaPositions(const vector<String>& names);

	// get the position of the specified antenna relative to the observatory position.
	// the three vector returned represents the longitudinal, latitudinal, and elevation
	// offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
	// measured along the surface of a sphere centered at the earth's center and whose surface
	// intersects the position of the observatory.
	Quantum<Vector<Double> > getAntennaOffset(uInt which);

	Quantum<Vector<Double> > getAntennaOffset(const String& name);

	vector<Quantum<Vector<Double> > > getAntennaOffsets();

	Bool hasBBCNo();

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

	// get the number of unflagged rows
	Double nUnflaggedRows();

	Double nUnflaggedRows(CorrelationType cType);

	Double nUnflaggedRows(
		CorrelationType cType, Int arrayID, Int observationID,
		Int scanNumber, Int fieldID
	);

	Double nUnflaggedRows(CorrelationType cType, Int fieldID);

	inline Float getCache() const { return _cacheMB;}

	vector<Double> getBandWidths();

	vector<QVD> getChanFreqs();

	vector<QVD> getChanWidths();

	vector<Int> getNetSidebands();

	vector<Quantity> getMeanFreqs();

	vector<Quantity> getCenterFreqs();

	vector<uInt> nChans();

	vector<vector<Double> > getEdgeChans();

	vector<uInt> getBBCNos();

	std::map<uInt, std::set<uInt> > getBBCNosToSpwMap(SQLDSwitch sqldSwitch);

	vector<String> getSpwNames();

	std::map<uInt, Double> getAverageIntervalsForScan(Int scan);

	// The first value of the pair is spw, the second is polarization ID.
	std::map<std::pair<uInt, uInt>, Int> getSpwIDPolIDToDataDescIDMap();

	uInt nPol();

	// get a map of data desc ID, scan number pair to exposure time for the first time
	// for that data desc ID, scan number pair
	vector<std::map<Int, Quantity> > getFirstExposureTimeMap();

	// get polarization IDs for the specified scan and spwid
	std::set<uInt> getPolarizationIDs(Int scan, uInt spwid);

	// get unique field IDs that exist in the main table.
	std::set<Int> getUniqueFiedIDs();

private:

	// (array_id, observation_id, scan_number, field_id) -> stuff mappings
	typedef std::map<Int, std::map<Int, std::map<Int, std::map<Int, uInt> > > > AOSFMapI;
	typedef std::map<Int, std::map<Int, std::map<Int, std::map<Int, Double> > > > AOSFMapD;

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

	const MeasurementSet* _ms;
	Float _cacheMB;
	const Float _maxCacheMB;
	uInt _nStates, _nACRows, _nXCRows, _nSpw, _nFields, _nAntennas,
		_nObservations, _nScans, _nArrays, _nrows, _nPol, _nDataDescIDs;
	std::set<String> _uniqueIntents;
	std::map<Int, std::set<uInt> > _scanToSpwsMap, _scanToDDIDsMap;
	std::set<Int> _uniqueScanNumbers, _uniqueFieldIDs, _uniqueStateIDs;
	std::set<uInt> _avgSpw, _tdmSpw, _fdmSpw, _wvrSpw, _sqldSpw;
	CountedPtr<Vector<Int> > _antenna1, _antenna2, _scans, _fieldIDs,
		_stateIDs, _dataDescIDs, _observationIDs, _arrayIDs;
	CountedPtr<AOSFMapI> _scanToNACRowsMap, _scanToNXCRowsMap;
	CountedPtr<std::map<Int, uInt> > _fieldToNACRowsMap, _fieldToNXCRowsMap;
	std::map<Int, uInt> _dataDescIDToSpwMap, _dataDescIDToPolIDMap;
	std::map<std::pair<uInt, uInt>, Int> _spwPolIDToDataDescIDMap;
 	std::map<Int, std::set<String> > _scanToIntentsMap;
	vector<std::set<String> > _stateToIntentsMap, _spwToIntentsMap, _fieldToIntentsMap;
	vector<SpwProperties> _spwInfo;
	std::map<Int, std::set<uInt> > _fieldToSpwMap;
	vector<std::set<Int> > _spwToFieldIDsMap, _spwToScansMap, _ddidToScansMap;
	std::map<Int, std::set<Int> > _scanToStatesMap, _scanToFieldsMap, _fieldToScansMap,
		_fieldToStatesMap, _stateToFieldsMap;
	vector<String> _fieldNames, _antennaNames, _observatoryNames, _stationNames;
	std::map<String, uInt> _antennaNameToIDMap;
	CountedPtr<Vector<Double> > _times;
	CountedPtr<Quantum<Vector<Double> > > _exposures;
	CountedPtr<std::map<Int, std::set<Double> > > _scanToTimesMap;
	std::map<String, std::set<Int> > _intentToFieldIDMap, _intentToScansMap;
	std::map<String, std::set<uInt> > _intentToSpwsMap;

	CountedPtr<std::map<Int, std::set<Double> > > _fieldToTimesMap;
	CountedPtr<std::map<Double, std::set<Int> > > _timeToFieldsMap;

	vector<MPosition> _observatoryPositions, _antennaPositions;
	vector<Quantum<Vector<Double> > > _antennaOffsets;
	Quantum<Vector<Double> > _antennaDiameters;
	Matrix<Bool> _uniqueBaselines;
	Quantity _exposureTime;
	Double _nUnflaggedACRows, _nUnflaggedXCRows;
	CountedPtr<std::map<Int, Double> > _unflaggedFieldNACRows, _unflaggedFieldNXCRows;
	CountedPtr<AOSFMapD> _unflaggedScanNACRows, _unflaggedScanNXCRows;
	const String _taqlTableName;
	const vector<const Table*> _taqlTempTable;
	CountedPtr<ArrayColumn<Bool> > _flagsColumn;
	std::map<Int, vector<Double> > _scanToTimeRangeMap;
	std::map<Int, std::map<uInt, Double> > _scanSpwToIntervalMap;
	Bool _spwInfoStored;
	vector<std::map<Int, Quantity> > _firstExposureTimeMap;
	std::map<std::pair<Int, uInt>, std::set<uInt> > _scanSpwToPolIDMap;

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

	// static vector<uInt> _toUIntVector(const Vector<Int>& v);

	static void _checkScan(const Int scan, const std::set<Int> allScans);

	Bool _hasIntent(const String& intent);

	Bool _hasFieldID(Int fieldID);

	Bool _hasStateID(Int stateID);

	void _hasAntennaID(Int antennaID);

	vector<std::set<String> > _getSpwToIntentsMap();

	void _getAntennas(
		CountedPtr<Vector<Int> >& ant1,
		CountedPtr<Vector<Int> >& ant2
	);

	CountedPtr<Vector<Int> > _getScans();

	CountedPtr<Vector<Int> > _getObservationIDs();

	CountedPtr<Vector<Int> > _getArrayIDs();

	CountedPtr<Vector<Int> > _getFieldIDs();

	CountedPtr<Vector<Int> > _getStateIDs();

	CountedPtr<Vector<Int> > _getDataDescIDs();

	CountedPtr<Vector<Double> > _getTimes();

	CountedPtr<Quantum<Vector<Double> > > _getExposureTimes();

	CountedPtr<ArrayColumn<Bool> > _getFlags();

	std::map<Int, std::set<Int> > _getScanToStatesMap();

	Bool _cacheUpdated(const Float incrementInBytes);

	void _getStateToIntentsMap(
		vector<std::set<String> >& statesToIntentsMap,
		std::set<String>& uniqueIntents
	);

	vector<SpwProperties> _getSpwInfo(
		std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw,
		std::set<uInt>& fdmSpw, std::set<uInt>& wvrSpw,
		std::set<uInt>& sqldSpw
	);

	static uInt _sizeof(const std::map<Int, std::set<uInt> >& map);

	static uInt _sizeof(const std::map<Int, std::set<Int> >& map);

	static uInt _sizeof(const vector<std::set<Int> >& v);

	static uInt _sizeof(const std::map<String, std::set<uInt> >& map);

	static uInt _sizeof(const vector<std::map<Int, Quantity> >& map);

	static uInt _sizeof(const std::map<std::pair<Int, uInt>, std::set<uInt> >& map);

	void _getFieldsAndSpwMaps(
		std::map<Int, std::set<uInt> >& fieldToSpwMap,
		vector<std::set<Int> >& spwToFieldMap
	);

	void _getScansAndDDIDMaps(
		std::map<Int, std::set<uInt> >& scanToDDIDMap,
		vector<std::set<Int> >& ddIDToScanMap
	);

	void _getScansAndSpwMaps(
		std::map<Int, std::set<uInt> >& scanToSpwMap,
		vector<std::set<Int> >& spwToScanMap
	);

	void _getFieldsAndIntentsMaps(
		vector<std::set<String> >& fieldToIntentsMap,
		std::map<String, std::set<Int> >& intentToFieldsMap
	);

	static uInt _sizeof(const std::map<Int, std::set<String> >& m);

	static uInt _sizeof(const std::map<String, std::set<Int> >& m);

	static uInt _sizeof(const vector<std::set<String> >& m);

	static uInt _sizeof(const vector<String>& m);

	static uInt _sizeof(const Quantum<Vector<Double> >& m);


	static uInt _sizeof(const std::map<Int, std::set<Double> >& m);

	static uInt _sizeof(const std::map<Double, std::set<Int> >& m);

	void _getScansAndIntentsMaps(
		std::map<Int, std::set<String> >& scanToIntentsMap,
		std::map<String, std::set<Int> >& intentToScansMap
	);

	void _getSpwsAndIntentsMaps(
		vector<std::set<String> >& spwToIntentsMap,
		std::map<String, std::set<uInt> >& intentToSpwsMap
	);

	void _getFieldsAndScansMaps(
		std::map<Int, std::set<Int> >& fieldToScansMap,
		std::map<Int, std::set<Int> >& scanToFieldsMap
	);

	void _getFieldsAndStatesMaps(
		std::map<Int, std::set<Int> >& fieldToStatesMap,
		std::map<Int, std::set<Int> >& stateToFieldsMap
	);

	void _getFieldsAndTimesMaps(
		CountedPtr<std::map<Int, std::set<Double> > >& fieldToTimesMap,
		CountedPtr<std::map<Double, std::set<Int> > >& timesToFieldMap
	);

	std::map<Int, uInt> _getDataDescIDToSpwMap();

	std::map<Int, uInt> _getDataDescIDToPolIDMap();

	vector<String> _getFieldNames();

	vector<String> _getStationNames();

	CountedPtr<std::map<Int, std::set<Double> > > _getScanToTimesMap();

	void _getRowStats(
		uInt& nACRows, uInt& nXCRows,
		CountedPtr<AOSFMapI>& scanToNACRowsMap,
		CountedPtr<AOSFMapI>& scanToNXCRowsMap,
		CountedPtr<std::map<Int, uInt> >& fieldToNACRowsMap,
		CountedPtr<std::map<Int, uInt> >& fieldToNXCRowsMap
	);

	void _getUnflaggedRowStats(
		Double& nACRows, Double& nXCRows,
		CountedPtr<AOSFMapD>& scanToNACRowsMap,
		CountedPtr<AOSFMapD>& scanToNXCRowsMap,
		CountedPtr<std::map<Int, Double> >& fieldToNACRowsMap,
		CountedPtr<std::map<Int, Double> >& fieldToNXCRowsMap
	);

	void _getTimesAndInvervals(
		std::map<Int, vector<Double> >& scanToTimeRangeMap,
		std::map<Int, std::map<uInt, Double> >& scanSpwToIntervalMap
	);

	vector<SpwProperties>  _getSpwInfo2(
		std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
		std::set<uInt>& wvrSpw, std::set<uInt>& sqldSpw
	);

	static void _checkTolerance(const Double tol);

	vector<MPosition> _getObservatoryPositions();

	vector<String> _getAntennaNames(
		std::map<String, uInt>& namesToIDsMap
	);

	vector<MPosition> _getAntennaPositions();

	static std::map<Int, uInt> _toUIntMap(const Vector<Int>& v);

	std::map<Double, Double> _getTimeToTotalBWMap(
		const Vector<Double>& times, const Vector<Int>& ddIDs
	);

	void _getRowStats(
		uInt& nACRows, uInt& nXCRows,
		AOSFMapI*& scanToNACRowsMap,
		AOSFMapI*& scanToNXCRowsMap,
		std::map<Int, uInt>*& fieldToNACRowsMap,
		std::map<Int, uInt>*& fieldToNXCRowsMap
	);

	void _getUnflaggedRowStats(
		Double& nACRows, Double& nXCRows,
		std::map<Int, Double>*& fieldNACRows, std::map<Int, Double>*& fieldNXCRows,
		AOSFMapD*& scanNACRows,
		AOSFMapD*& scanNXCRows
	);

};
}

#endif
