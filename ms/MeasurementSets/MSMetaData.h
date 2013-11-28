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

#include <casa/Quanta/Quantum.h>
#include <tables/Tables/ArrayColumn.h>

#include <map>
#include <set>

namespace casa {

class MeasurementSet;
class MPosition;

template <class T> class Matrix;
template <class T> class Vector;

// <summary>
// Abstract base class to hold metadata pertaining to a measurement set.
// </summary>
// <todo>
// This initial implementation mostly parallels the implementation
// in ValueMapping to minimize differences between the two. A second
// implementation iteration should be done to improve performance and/or
// minimize memory usage.
// </todo>

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

	virtual ~MSMetaData();

	// construct an object which loads metadata from the specified MS. If onDemand=False,
	// the MS is
	// interrogated at construction time, so this operation can take some time
	// for a large MS, the benefit being that all metadata are immediately accessible.
	// If onDemand=True, a pointer to the MS is maintained internally, and portions of
	// the MS are queried only when necessary. The benefit being that construction time
	// is short. However, queries may take longer than if onDemand=False and because the
	// MS is interrogated only when the query is run and the query results are not stored
	// (which allows query methods to be const). The MS object cannot go out of scope in the calling code while
	// the MSMetaData object exists or seg faults will likely occur.
	// Use onDemand=True if MS is large and/or only a small number of queries are needed.
	// MSMetaData(const MeasurementSet& ms, Bool onDemand);

	// number of unique states (number of rows from the STATE table)
	virtual uInt nStates() = 0;

	// get unique scan numbers
	virtual std::set<Int> getScanNumbers() = 0;

	virtual std::set<Int> getScansForState(const Int stateID) = 0;

	virtual std::set<String> getIntentsForScan(const Int scan) = 0;

	// get all intents, in no particular (nor guaranteed) order.
	virtual std::set<String> getIntents() = 0;

	// get a set of spectral windows for which the specified <src>intent</src>
	// applies.
	virtual std::set<uInt> getSpwsForIntent(const String& intent) = 0;

	// get number of spectral windows
	virtual uInt nSpw(Bool includewvr) = 0;

	// get number of polarization configurations
	virtual uInt nPol() = 0;

	// get the number of rows
	virtual uInt nRows()  = 0;

	virtual uInt nRows(CorrelationType cType) = 0;

	virtual uInt nRows(
		CorrelationType cType, Int arrayID, Int observationID,
		Int scanNumber, Int fieldID
	) = 0;

	virtual uInt nRows(CorrelationType cType, Int fieldID) = 0;

	// get a set of intents corresponding to the specified spectral window
	virtual std::set<String> getIntentsForSpw(const uInt spw) = 0;

	// get a set of intents corresponding to a specified field
	virtual std::set<String> getIntentsForField(Int fieldID) = 0;

	// get the number of fields.
	virtual uInt nFields() = 0;

	// get a set of spectral windows corresponding to the specified fieldID
	virtual std::set<uInt> getSpwsForField(const Int fieldID) = 0;

	// get a set of spectral windows corresponding to the specified field name
	virtual std::set<uInt> getSpwsForField(const String& fieldName) = 0;

	// get the set of field IDs corresponding to the specified spectral window.
	virtual std::set<Int> getFieldIDsForSpw(const uInt spw) = 0;

	// get the set of field names corresponding to the specified spectral window.
	virtual std::set<String> getFieldNamesForSpw(const uInt spw) = 0;

	// get the number of scans in the dataset
	virtual uInt nScans() = 0;

	// get the number of observations (from the OBSERVATIONS table) in the dataset
	virtual uInt nObservations() = 0;

	// get the number of arrays (from the ARRAY table) in the dataset
	virtual uInt nArrays() = 0;

	// get the set of spectral windows for the specified scan number.
	virtual std::set<uInt> getSpwsForScan(const Int scan) = 0;

	// get the set of scan numbers for the specified spectral window.
	virtual std::set<Int> getScansForSpw(const uInt spw) = 0;

	// get the number of antennas in the ANTENNA table
	virtual uInt nAntennas() = 0;

	// get the name of the antenna for the specified antenna ID
	virtual vector<String> getAntennaNames(
		std::map<String, uInt>& namesToIDsMap,
		const vector<uInt>& antennaIDs=vector<uInt>(0)
	) = 0;

	// get the antenna ID for the antenna with the specified name.
	virtual vector<uInt> getAntennaIDs(const vector<String>& antennaName) = 0;

	// get the antenna stations for the specified antenna IDs
	virtual vector<String> getAntennaStations(const vector<uInt>& antennaIDs) = 0;

	// get the antenna stations for the specified antenna names
	virtual vector<String> getAntennaStations(const vector<String>& antennaNames) = 0;

	// ALMA-specific. Get set of spectral windows used for TDM. These are windows that have
	// 64, 128, or 256 channels
	virtual std::set<uInt> getTDMSpw() = 0;

	// ALMA-specific. Get set of spectral windows used for FDM. These are windows that do not
	// have 1, 4, 64, 128, or 256 channels.
	virtual std::set<uInt> getFDMSpw() = 0;

	// ALMA-specific. Get spectral windows that have been averaged. These are windows with 1 channel.
	virtual std::set<uInt> getChannelAvgSpw() = 0;

	// ALMA-specific. Get the spectral window set used for WVR measurements. These have 4 channels each.
	virtual std::set<uInt> getWVRSpw() = 0;

	// ALMA-specific. Get the square law detector (total power) spectral windows.
	virtual std::set<uInt> getSQLDSpw() = 0;

	// Get the scans which fail into the specified time range (center-tol to center+tol)
	virtual std::set<Int> getScansForTimes(const Double center, const Double tol) = 0;

	// Get the times for the specified scans
	virtual std::set<Double> getTimesForScans(const std::set<Int>& scans) = 0;

	// get the times for the specified scan. No need to be implemented or overridden in subclasses.
	// The return values come from the TIME column.
	std::set<Double> getTimesForScan(const Int scan);

	// get the time range for the specified scan. The vector returned will contain two elements,
	// the start and stop time of the scan, determined from min(TIME_CENTROID(x)-0.5*INTERVAL(x)) and
	// max(TIME_CENTROID(x)-0.5*INTERVAL(x))
	virtual std::vector<Double> getTimeRangeForScan(Int scan) = 0;

	// get the stateIDs associated with the specified scan number.
	virtual std::set<Int> getStatesForScan(const Int scan) = 0;

	// get the scans associated with the specified intent
	virtual std::set<Int> getScansForIntent(const String& intent) = 0;

	// get the scan numbers associated with the specified field ID.
	virtual std::set<Int> getScansForFieldID(const Int fieldID) = 0;

	// get the field IDs for the specified field name. Case insensitive.
	virtual std::set<Int> getFieldIDsForField(const String& field) = 0;

	// get the scan numbers associated with the specified field. Subclasses should not implement or override.
	std::set<Int> getScansForField(const String& field);

	// get field IDs associated with the specified scan number.
	virtual std::set<Int> getFieldsForScan(const Int scan) = 0;

	// get the field IDs associated with the specified scans
	virtual std::set<Int> getFieldsForScans(const std::set<Int>& scans) = 0;

	// get the field IDs associated with the specified intent.
	virtual std::set<Int> getFieldsForIntent(const String& intent) = 0;

	// get the field names associated with the specified field IDs. If <src>fieldIDs</src>
	// is empty, a list of all field names will be returned.
	virtual vector<String> getFieldNamesForFieldIDs(const vector<uInt>& fieldIDs) = 0;

	virtual std::map<String, std::set<Int> > getIntentToFieldsMap() = 0;

	virtual std::map<String, std::set<Int> > getIntentToScansMap() = 0;

	virtual std::map<String, std::set<uInt> > getIntentToSpwsMap() = 0;

	// Get the fields which fail into the specified time range (center-tol to center+tol)
	virtual std::set<Int> getFieldsForTimes(Double center, Double tol) = 0;

	// get the times for which the specified field was observed
	virtual std::set<Double> getTimesForField(Int fieldID) = 0;

	// get telescope names in the order they are listed in the OBSERVATION table. These are
	// the telescopes (observatories), not the antenna names.
	virtual vector<String> getObservatoryNames() = 0;

	// get the position of the specified telescope (observatory).
	virtual MPosition getObservatoryPosition(uInt which) = 0;

	// get the position of the specified antennas. If <src>which</src> is empty,
	// all antenna positions will be returned.
	virtual vector<MPosition> getAntennaPositions(const vector<uInt>& which=vector<uInt>(0)) = 0;

	// <src>names</src> cannot be empty.
	virtual vector<MPosition> getAntennaPositions(const vector<String>& names) = 0;

	// get the position of the specified antenna relative to the observatory position.
	// the three vector returned represents the longitudinal, latitudinal, and elevation
	// offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
	// measured along the surface of a sphere centered at the earth's center and whose surface
	// intersects the position of the observatory.
	virtual Quantum<Vector<Double> > getAntennaOffset(uInt which) = 0;

	virtual Quantum<Vector<Double> > getAntennaOffset(const String& name) = 0;

	virtual vector<Quantum<Vector<Double> > > getAntennaOffsets(
		const vector<MPosition>& positions
	) = 0;

	// get a map relating time stamps to exposure times. An exception is thrown if there
	// are different exposure lengths for records with the same time stamp
	//virtual std::map<Double, Double> getExposuresForTimes() const = 0;

	// get the unique baselines in the MS. These are not necessarily every combination of the
	// n(n-1)/2 possible antenna pairs, but rather the number of unique baselines represented in
	// the main MS table, which in theory can be less than n(n-1)/2 (for example if samples for
	// certain antenna pairs are not recorded. The returned Matrix is nAnts x nAnts in size. Pairs
	// that are true represent baselines represented in the main MS table.
	virtual Matrix<Bool> getUniqueBaselines() = 0;

	// get the number of unique baselines represented in the main MS table which in theory can be
	// less than n*(n-1)/2
	virtual uInt nBaselines();

	// get the effective total exposure time. This is the effective time spent collecting unflagged data.
	virtual Quantity getEffectiveTotalExposureTime() = 0;

	// get the number of unflagged rows
	virtual Double nUnflaggedRows() = 0;

	virtual Double nUnflaggedRows(CorrelationType cType) = 0;

	virtual Double nUnflaggedRows(
		CorrelationType cType, Int arrayID, Int observationID,
		Int scanNumber, Int fieldID
	) = 0;

	virtual Double nUnflaggedRows(CorrelationType cType, Int fieldID) = 0;

	inline virtual Float getCache() const { return 0;}

	static Bool hasBBCNo(const MeasurementSet& ms);

	virtual vector<Double> getBandWidths() = 0;

	virtual vector<Quantum<Vector<Double> > > getChanFreqs() = 0;

	virtual vector<Quantum<Vector<Double> > > getChanWidths() = 0;

	virtual vector<Int> getNetSidebands() = 0;

	virtual vector<Quantity> getMeanFreqs() = 0;

	virtual vector<uInt> nChans() = 0;

	virtual vector<vector<Double> > getEdgeChans() = 0;

	virtual vector<uInt> getBBCNos() = 0;

	virtual std::map<uInt, std::set<uInt> > getBBCNosToSpwMap(SQLDSwitch sqldSwitch) = 0;

	virtual vector<String> getSpwNames() = 0;

	// the returned map are the average intervals for each spectral window for the
	// specified scan
	virtual std::map<uInt, Double> getAverageIntervalsForScan(Int scan) = 0;

	virtual std::map<std::pair<uInt, uInt>, Int> getSpwIDPolIDToDataDescIDMap() = 0;

protected:

	// (array_id, observation_id, scan_number, field_id) -> stuff mappings
	typedef std::map<Int, std::map<Int, std::map<Int, std::map<Int, uInt> > > > AOSFMapI;
	typedef std::map<Int, std::map<Int, std::map<Int, std::map<Int, Double> > > > AOSFMapD;

	struct SpwProperties {
		Double bandwidth;
		Quantum<Vector<Double> > chanfreqs;
		Quantum<Vector<Double> > chanwidths;
		Int netsideband;
		Quantity meanfreq;
		uInt nchans;
		vector<Double> edgechans;
		uInt bbcno;
		String name;
	};

	static uInt _getNStates(const MeasurementSet& ms);

	static void _getStateToIntentsMap(
		vector<std::set<String> >& statesToIntentsMap,
		std::set<String>& uniqueIntents,
		const MeasurementSet& ms
	);

	static Vector<Int> _getScans(const MeasurementSet& ms);

	// state IDs can be < 0
	static std::map<Int, std::set<Int> > _getScanToStatesMap(
		const Vector<Int>& scans, const Vector<Int>& states
	);

	static vector<SpwProperties>  _getSpwInfo(
		std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
		std::set<uInt>& wvrSpw, std::set<uInt>& sqldSpw, const MeasurementSet& ms
	);

	static Vector<Int> _getDataDescIDs(const MeasurementSet& ms);

	static Vector<Int> _getStates(const MeasurementSet& ms);

	static Vector<Int> _getObservationIDs(const MeasurementSet& ms);

	static Vector<Int> _getArrayIDs(const MeasurementSet& ms);

	static std::map<Int, uInt> _getDataDescIDToSpwMap(const MeasurementSet& ms);

	static std::map<Int, uInt> _getDataDescIDToPolIDMap(const MeasurementSet& ms);

	// The first member of the pair is spwID, the second is polID.
	static std::map<std::pair<uInt, uInt>, Int> _getSpwIDPolIDToDataDescIDMap(
		const std::map<Int, uInt>& dataDescIDToSpwMap,
		const std::map<Int, uInt>& dataDescIDToPolIDMap
	);

	static Vector<Int> _getFieldIDs(const MeasurementSet& ms);

	static vector<String> _getFieldNames(const MeasurementSet& ms);

	static void _checkTolerance(const Double tol);

	static Vector<Double> _getTimes(const MeasurementSet& ms);

	static Vector<Double> _getTimeCentroids(const MeasurementSet& ms);

	static Vector<Double> _getIntervals(const MeasurementSet& ms);

	//static Vector<Bool> _getFlagRows(const MeasurementSet& ms);

	static ArrayColumn<Bool>* _getFlags(const MeasurementSet& ms);

	static std::map<Int, std::set<Double> > _getScanToTimesMap(
		const Vector<Int>& scans, const Vector<Double>& times
	);

	static vector<MPosition> _getObservatoryPositions(
		vector<String>& names, const MeasurementSet& ms
	);

	static vector<String> _getAntennaNames(
		std::map<String, uInt>& namesToIDs, const MeasurementSet& ms
	);

	// get all antenna positions. A vector of antenna names is also returned.
	static vector<MPosition> _getAntennaPositions(
		vector<String>& antennaNames, const MeasurementSet& ms
	);

	static vector<MPosition> _getAntennaPositions(
		const MeasurementSet& ms
	);

	static vector<Quantum<Vector<Double> > > _getAntennaOffsets(
		const vector<MPosition>& antennaPositions,
		const MPosition& observatoryPosition
	);

	static vector<String> _getAntennaStationNames(
		const MeasurementSet& ms
	);

	static std::map<Int, uInt> _toUIntMap(const Vector<Int>& v);


	static std::map<Double, Double> _getTimeToAggregateExposureMap(
		const vector<Double>& times, const vector<Double>& exposures
	);

	Matrix<Bool> _getUniqueBaselines(
		const Vector<Int>& antanna1, const Vector<Int>& antenna2
	);

	Quantity _getTotalExposureTime(
		const MeasurementSet& ms, const std::map<Double, Double>& timeToBWMap,
		const vector<SpwProperties>& spwProperties,
		const std::map<Int, uInt>& dataDescToSpwIdMap
	);

	static std::map<Double, Double> _getTimeToTotalBWMap(
		const Vector<Double>& times, const Vector<Int>& ddIDs,
		const std::map<Int, uInt>& dataDescIDToSpwMap,
		const vector<MSMetaData::SpwProperties>& spwInfo
	);

	static void _getAntennas(
		Vector<Int>& ant1, Vector<Int>& ant2, const MeasurementSet& ms
	);

	void _getRowStats(
		uInt& nACRows, uInt& nXCRows,
		AOSFMapI*& scanToNACRowsMap,
		AOSFMapI*& scanToNXCRowsMap,
		std::map<Int, uInt>*& fieldToNACRowsMap,
		std::map<Int, uInt>*& fieldToNXCRowsMap,
		const Vector<Int>& ant1, const Vector<Int>& ant2,
		const Vector<Int>& scans, const Vector<Int>& fieldIDs,
		const Vector<Int>& obsIDs, const Vector<Int>& arIDs
	);

	void _getUnflaggedRowStats(
		Double& nACRows, Double& nXCRows,
		std::map<Int, Double>*& fieldNACRows, std::map<Int, Double>*& fieldNXCRows,
		AOSFMapD*& scanNACRows,
		AOSFMapD*& scanNXCRows,
		const Vector<Int>& ant1, const Vector<Int>& ant2,
		/*const Vector<Bool>& flagRow, */const Vector<Int>& dataDescIDs,
		const std::map<Int, uInt>& dataDescIDToSpwMap,
		const vector<SpwProperties>& spwInfo,
		const ArrayColumn<Bool>& flags,
		const Vector<Int>& fieldIDs, const Vector<Int>& scan,
		const Vector<Int>& obsIDs, const Vector<Int>& arIDs
	);

	static std::map<Int, vector<Double> > _getScanToTimeRangeMap(
		std::map<Int, std::map<uInt, Double> >& scanSpwToAverageIntervalMap,
		const Vector<Int>& scans, const Vector<Double>& timeCentroids,
		const Vector<Double>& intervals, const Vector<Int>& dataDescIDs,
		const std::map<Int, uInt>& dataDesIDToSpwMap,
		const std::set<Int>& uniqueScans
	);


private:

	// This comment from thunter in the original ValueMapping python class
	// # Determine the number of polarizations for the first OBSERVE_TARGET intent.
    // # Used by plotbandpass for BPOLY plots since the number of pols cannot be inferred
    // # correctly from the caltable alone.  You cannot not simply use the first row, because
    // # it may be a pointing scan which may have different number of polarizations than what
    // # the TARGET and BANDPASS calibrator will have.
    // # -- T. Hunter
	void _setNumberOfPolarizations(const MeasurementSet& ms);

	// set metadata from OBSERVATION table
	void _setObservation(const MeasurementSet& ms);

};
}

#endif /* MSMETADATA_H_ */
