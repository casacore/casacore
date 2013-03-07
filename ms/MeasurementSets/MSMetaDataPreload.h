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

#ifndef MS_MSMETADATAPRELOAD_H
#define MS_MSMETADATAPRELOAD_H

#include <ms/MeasurementSets/MSMetaData.h>

#include <measures/Measures/MPosition.h>

namespace casa {

// <summary>
// Class to hold metadata pertaining to a measurement set. MS is interrogated and all
// metadata are stored at construction time.
// </summary>

class MSMetaDataPreload : public MSMetaData {
public:

	// construct an object which loads metadata from the specified MS.
	// MS is interogated and all metadata are loaded at construction time.
	MSMetaDataPreload(const MeasurementSet& ms);

	virtual ~MSMetaDataPreload();
	// number of unique states (number of rows from the STATE table)
	uInt nStates();

	// get unique scan numbers
	std::set<uInt> getScanNumbers();

	std::set<uInt> getScansForState(const uInt stateID);

	std::set<String> getIntentsForScan(const uInt scan);

	// get all intents, in no particular (nor guaranteed) order.
	std::set<String> getIntents();

	// get a set of spectral windows for which the specified <src>intent</src>
	// applies.
	std::set<uInt> getSpwsForIntent(const String& intent);

	// get number of spectral windows
	uInt nSpw(Bool includeWVR);

	// get the number of rows
	uInt nRows() const;

	uInt nRows(CorrelationType cType);

	uInt nRows(
		CorrelationType cType, uInt arrayID, uInt observationID,
		uInt scanNumber, uInt fieldID
	);

	uInt nRows(CorrelationType cType, uInt fieldID);

	// get a set of intents corresponding to the specified spectral window
	std::set<String> getIntentsForSpw(const uInt spw);

	// get the number of fields.
	uInt nFields();

	// get a set of spectral windows corresponding to the specified fieldID
	std::set<uInt> getSpwsForField(const uInt fieldID);

	// get a set of spectral windows corresponding to the specified field name
	std::set<uInt> getSpwsForField(const String& fieldName);

	// get the set of field IDs corresponding to the specified spectral window.
	std::set<uInt> getFieldIDsForSpw(const uInt spw);

	// get the set of field names corresponding to the specified spectral window.
	std::set<String> getFieldNamesForSpw(const uInt spw);

	// get the number of scans in the dataset
	uInt nScans();

	// get the number of observations (from the OBSERVATIONS table) in the dataset
	uInt nObservations();

	// get the number of arrays (from the ARRAY table) in the dataset
	uInt nArrays();

	// get the set of spectral windows for the specified scan number.
	std::set<uInt> getSpwsForScan(const uInt scan);

	// get the set of scan numbers for the specified spectral window.
	std::set<uInt> getScansForSpw(const uInt spw);

	// get the number of antennas in the ANTENNA table
	uInt nAntennas();

	// get the name of the antenna for the specified antenna ID
	vector<String> getAntennaNames(
		std::map<String, uInt>& namesToIDsMap,
		const vector<uInt>& antennaIDs=vector<uInt>(0)
	);

	// get the antenna ID for the antenna with the specified name.
	vector<uInt> getAntennaIDs(const vector<String>& antennaNames);

	// get set of spectral windows used for TDM. These are windows that have
	// 64, 128, or 256 channels
	std::set<uInt> getTDMSpw();

	// get set of spectral windows used for FDM. These are windows that do not
	// have 1, 4, 64, 128, or 256 channels.
	std::set<uInt> getFDMSpw();

	// get spectral windows that have been averaged. These are windows with 1 channel.
	std::set<uInt> getChannelAvgSpw();

	// Get the spectral window set used for WVR measurements. These have 4 channels each.
	std::set<uInt> getWVRSpw();

	// Get the scans which fail into the specified time range (center-tol to center+tol)
	std::set<uInt> getScansForTimes(const Double center, const Double tol);

	// Get the times for the specified scans
	std::set<Double> getTimesForScans(const std::set<uInt>& scans);

	// get the time range for the specified scan. The vector returned will contain two elements,
	// the start and stop time of the scan, determined from min(TIME_CENTROID(x)-0.5*INTERVAL(x)) and
	// max(TIME_CENTROID(x)-0.5*INTERVAL(x))
	std::vector<Double> getTimeRangeForScan(uInt scan);

	// get the times for the specified scan
	// std::set<Double> getTimesForScan(const uInt scan) const;

	// get the stateIDs associated with the specified scan number.
	std::set<uInt> getStatesForScan(const uInt scan);

	// get the scans associated with the specified intent
	std::set<uInt> getScansForIntent(const String& intent);

	// get the scan numbers associated with the specified field ID.
	std::set<uInt> getScansForFieldID(const uInt fieldID);

	// get the field IDs for the specified field name. Case insensitive.
	std::set<uInt> getFieldIDsForField(const String& field);

	// get the scan numbers associated with the specified field.
	//std::set<uInt> getScansForField(const String& field) const;

	// get field IDs associated with the specified scan number.
	std::set<uInt> getFieldsForScan(const uInt scan);

	// get the field IDs associated with the specified scans
	std::set<uInt> getFieldsForScans(const std::set<uInt>& scans);

	// get the field IDs associated with the specified intent.
	std::set<uInt> getFieldsForIntent(const String& intent);

	// get the field names associated with the specified field IDs. If <src>fieldIDs</src>
	// is empty, a vector of all the field names is returned.
	vector<String> getFieldNamesForFieldIDs(const vector<uInt>& fieldIDs);

	// Get the fields which fail into the specified time range (center-tol to center+tol)
	std::set<uInt> getFieldsForTimes(Double center, Double tol) const;

	// get the times for which the specified field was observed
	std::set<Double> getTimesForField(uInt fieldID);

	// get telescope names in the order they are listed in the OBSERVATION table. These are
	// the telescopes (observatories), not the antenna names.
	vector<String> getObservatoryNames();

	// get the position of the specified telescope (observatory).
	MPosition getObservatoryPosition(uInt which);

	// get the positions of the specified antennas. If <src>which</src> is empty, all positions
	// of all antennas are returned.
	vector<MPosition> getAntennaPositions(const vector<uInt>& which=vector<uInt>(0));

	// <src>names</src> cannot be empty.
	vector<MPosition> getAntennaPositions(const vector<String>& names);

	// get the position of the specified antenna relative to the observatory position.
	// the three vector returned represents the longitudinal, latitudinal, and elevation
	// offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
	// measured along the surface of a sphere centered at the earth's center and whose surface
	// intersects the position of the observatory.
	Quantum<Vector<Double> > getAntennaOffset(uInt which);

	Quantum<Vector<Double> > getAntennaOffset(const String& name);

	// The parameter is not used by this method.
	vector<Quantum<Vector<Double> > > getAntennaOffsets(
		const vector<MPosition>& positions=vector<MPosition>(0)
	);

	// get a map relating time stamps to exposure times. An exception is thrown if there
	// are different exposure lengths for records with the same time stamp
	// virtual std::map<Double, Double> getExposuresForTimes() const;

	// get the unique baselines in the MS. These are not necessarily every combination of the
	// n(n-1)/2 possible antenna pairs, but rather the number of unique baselines represented in
	// the main MS table, which in theory can be less than n(n-1)/2 (for example if samples for
	// certain antenna pairs are not recorded. The returned Matrix is nAnts x nAnts in size. Pairs
	// that are true represent baselines represented in the main MS table.
	Matrix<Bool> getUniqueBaselines();

	// get the effective total exposure time. This is the effective time spent
	// collecting unflagged data.
	Quantity getEffectiveTotalExposureTime();

	// get the number of unflagged rows
	Double nUnflaggedRows();

	Double nUnflaggedRows(CorrelationType cType);

	Double nUnflaggedRows(
		CorrelationType cType, uInt arrayID, uInt observationID,
		uInt scanNumber, uInt fieldID
	);

	Double nUnflaggedRows(CorrelationType cType, uInt fieldID);

	vector<Double> getBandWidths();

	vector<Quantum<Vector<Double> > > getChanFreqs();

	vector<vector<Double> > getChanWidths();

	vector<Int> getNetSidebands();

	vector<Quantity> getMeanFreqs();

	vector<uInt> nChans();

	vector<vector<Double> > getEdgeChans();

	vector<uInt> getBBCNos();

	vector<String> getSpwNames();

	std::map<uInt, Double> getAverageIntervalsForScan(uInt scan);


private:
	Vector<Int> _antenna1,	_antenna2, _dataDescIDs,
		_scans, _fieldIds, _states, _obsIDs, _arrayIDs;
	vector<uInt> _dataDescToSpwMap,
		_fieldToNACRowsMap, _fieldToNXCRowsMap;
	std::set<uInt> _uniqueScans;
	std::set<String> _uniqueIntents;
	Vector<Double> _times;
	std::map<uInt, std::set<Double> > _scanToTimesMap;
	vector<String> _fieldNames, _antennaNames,
		_dataColumnNames, _observatoryNames;
	std::map<String, uInt> _antennaNamesToIDs;
	std::map<uInt, std::set<uInt> > _scanToStatesMap;
	vector<std::set<String> > _stateToIntentsMap;
	std::map<String, std::set<Double> > _fieldNameToTimesMap;
	Matrix<Bool> _baselines;
	uInt _nPolarizations, _nStates, _nIntents, _nFields, _nACRows, _nXCRows,
		_nObservations, _nArrays;
	Double _nUnflaggedACRows, _nUnflaggedXCRows;
	String _correctedDataColumnName, _modelDataColumnName,
		_dataColumnName;
	vector<MSMetaData::SpwProperties> _spwInfo;
	vector<std::set<uInt> > _spwToScansMap, _spwToFieldIDsMap;
	vector<std::set<String> > _spwToIntentsMap;
	std::set<uInt> _tdmspw, _fdmspw, _wvrspw, _avgspw;
	vector<MPosition> _observatoryPositions, _antennaPositions;
	vector<Quantum<Vector<Double> > > _antennaOffsets;
	Quantity _totalEffectiveExposureTime;
	Vector<Bool> _flagRow;
	std::auto_ptr<ArrayColumn<Bool> > _flags;
	AOSFMapI _scanToNACRowsMap, _scanToNXCRowsMap;
	AOSFMapD _scanToNUnflaggedACRowsMap, _scanToNUnflaggedXCRowsMap;
	vector<Double> _fieldToNUnflaggedACRows, _fieldToNUnflaggedXCRows;
	std::map<Int, vector<Double> > _scanToTimeRange;
	std::map<Int, std::map<uInt, Double> > _scanSpwToIntervalMap;
	Bool _hasBBCNo;

	// disallow copy constructor and = operator
	MSMetaDataPreload(const MSMetaData&);
	MSMetaDataPreload operator =(const MSMetaDataPreload&);

	void _makeScanToTimeMap(const MeasurementSet& ms);

	void _makeFieldsAndSources(const MeasurementSet& ms);

	void _makeAntennaInfo(const MeasurementSet& ms);

	void _makeScanToStateMap(const MeasurementSet& ms);

	void _makeStateToIntentsMap(const MeasurementSet& ms);

	void _makeFieldNameToTimesMap(const MeasurementSet& ms);

	void _makeTotalEffectiveExposureTime(const MeasurementSet& ms);

	void _makeDataDescID(const MeasurementSet& ms);

	void _makeDataDescIDToSpwMap(const MeasurementSet& ms);

	//void _makeTimeToExposureMap(const MeasurementSet& ms);

	// This comment from thunter in the original ValueMapping python class
	// # Determine the number of polarizations for the first OBSERVE_TARGET intent.
    // # Used by plotbandpass for BPOLY plots since the number of pols cannot be inferred
    // # correctly from the caltable alone.  You cannot not simply use the first row, because
    // # it may be a pointing scan which may have different number of polarizations than what
    // # the TARGET and BANDPASS calibrator will have.
    // # -- T. Hunter
	void _setNumberOfPolarizations(const MeasurementSet& ms);

	void _setSpwInfo(const MeasurementSet& ms);

	void _makeSpwToScanMap();

	void _makeSpwToFieldMap();

	void _makeSpwToIntentsMap();

	void _makeUniqueBaselines(const MeasurementSet& ms);

	void _makeRowStats(const MeasurementSet& ms);

	void _setDataColumnNames(const MeasurementSet& ms);

	// static vector<uInt> _getScans(const MeasurementSet& ms);

	// set metadata from OBSERVATION table
	void _setObservation(const MeasurementSet& ms);

	//static vector<uInt> _toUIntVector(const Vector<Int>& v);

	void _checkScan(const uInt scan) const;

	void _checkFieldID(const uInt fieldID) const;

	void _checkFieldIDs(const vector<uInt>& fieldIDs) const;


};
}

#endif /* MSMETADATA_H_ */
