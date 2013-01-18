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

#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Vector.h>
#include <measures/Measures/MPosition.h>
#include <ms/MeasurementSets/MeasurementSet.h>

#include <map>
#include <set>

namespace casa {

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
	virtual uInt nStates() const = 0;

	// get unique scan numbers
	virtual std::set<uInt> getScanNumbers() const = 0;

	virtual std::set<uInt> getScansForState(const uInt stateID) const = 0;

	virtual std::set<String> getIntentsForScan(const uInt scan) const = 0;

	// get all intents, in no particular (nor guaranteed) order.
	virtual std::set<String> getIntents() const = 0;

	// get a set of spectral windows for which the specified <src>intent</src>
	// applies.
	virtual std::set<uInt> getSpwsForIntent(const String& intent) const = 0;

	// get number of spectral windows
	virtual uInt nSpw() const = 0;

	// get the number of visibilities
	virtual uInt nVisibilities() const = 0;

	// get a set of intents corresponding to the specified spectral window
	virtual std::set<String> getIntentsForSpw(const uInt spw) const = 0;

	// get the number of fields.
	virtual uInt nFields() const = 0;

	// get a set of spectral windows corresponding to the specified fieldID
	virtual std::set<uInt> getSpwsForField(const uInt fieldID) const = 0;

	// get a set of spectral windows corresponding to the specified field name
	virtual std::set<uInt> getSpwsForField(const String& fieldName) const = 0;

	// get the set of field IDs corresponding to the specified spectral window.
	virtual std::set<uInt> getFieldIDsForSpw(const uInt spw) const = 0;

	// get the set of field names corresponding to the specified spectral window.
	virtual std::set<String> getFieldNamesForSpw(const uInt spw) const = 0;

	// get the number of scans in the dataset
	virtual uInt nScans() const = 0;

	// get the set of spectral windows for the specified scan number.
	virtual std::set<uInt> getSpwsForScan(const uInt scan) const = 0;

	// get the set of scan numbers for the specified spectral window.
	virtual std::set<uInt> getScansForSpw(const uInt spw) const = 0;

	// get the number of antennas in the ANTENNA table
	virtual uInt nAntennas() const = 0;

	// get the name of the antenna for the specified antenna ID
	virtual vector<String> getAntennaNames(const vector<uInt>& antennaID) const = 0;

	// get the antenna ID for the antenna with the specified name.
	virtual vector<uInt> getAntennaIDs(const vector<String>& antennaName) const = 0;

	// get set of spectral windows used for TDM. These are windows that have
	// 64, 128, or 256 channels
	virtual std::set<uInt> getTDMSpw() const = 0;

	// get set of spectral windows used for FDM. These are windows that do not
	// have 1, 4, 64, 128, or 256 channels.
	virtual std::set<uInt> getFDMSpw() const = 0;

	// get spectral windows that have been averaged. These are windows with 1 channel.
	virtual std::set<uInt> getChannelAvgSpw() const = 0;

	// Get the spectral window set used for WVR measurements. These have 4 channels each.
	virtual std::set<uInt> getWVRSpw() const = 0;

	// Get the scans which fail into the specified time range (center-tol to center+tol)
	virtual std::set<uInt> getScansForTimes(const Double center, const Double tol) const = 0;

	// Get the times for the specified scans
	virtual std::set<Double> getTimesForScans(const std::set<uInt> scans) const = 0;

	// get the times for the specified scan. No need to be implemented or overridden in subclasses.
	std::set<Double> getTimesForScan(const uInt scan) const;

	// get the stateIDs associated with the specified scan number.
	virtual std::set<uInt> getStatesForScan(const uInt scan) const = 0;

	// get the scans associated with the specified intent
	virtual std::set<uInt> getScansForIntent(const String& intent) const = 0;

	// get the scan numbers associated with the specified field ID.
	virtual std::set<uInt> getScansForFieldID(const uInt fieldID) const = 0;

	// get the field IDs for the specified field name. Case insensitive.
	virtual std::set<uInt> getFieldIDsForField(const String& field) const = 0;

	// get the scan numbers associated with the specified field. Subclasses should not implement or override.
	std::set<uInt> getScansForField(const String& field) const;

	// get field IDs associated with the specified scan number.
	virtual std::set<uInt> getFieldsForScan(const uInt scan) const = 0;

	// get the field IDs associated with the specified scans
	virtual std::set<uInt> getFieldsForScans(const std::set<uInt>& scans) const = 0;

	// get the field IDs associated with the specified intent.
	virtual std::set<uInt> getFieldsForIntent(const String& intent) const = 0;

	// get the field names associated with the specified field IDs. If <src>fieldIDs</src>
	// is empty, a list of all field names will be returned.
	virtual vector<String> getFieldNamesForFieldIDs(const vector<uInt>& fieldIDs) const = 0;

	// Get the fields which fail into the specified time range (center-tol to center+tol)
	virtual std::set<uInt> getFieldsForTimes(Double center, Double tol) const = 0;

	// get the times for which the specified field was observed
	virtual std::set<Double> getTimesForField(uInt fieldID) const = 0;

	// get telescope names in the order they are listed in the OBSERVATION table. These are
	// the telescopes (observatories), not the antenna names.
	virtual vector<String> getObservatoryNames() const = 0;

	// get the position of the specified telescope (observatory).
	virtual MPosition getObservatoryPosition(uInt which) const = 0;

	// get the position of the specified antennas. If <src>which</src> is empty,
	// all antenna positions will be returned.
	virtual vector<MPosition> getAntennaPositions(const vector<uInt>& which=vector<uInt>(0)) const = 0;

	// <src>names</src> cannot be empty.
	virtual vector<MPosition> getAntennaPositions(const vector<String>& names) const = 0;

	// get the position of the specified antenna relative to the observatory position.
	// the three vector returned represents the longitudinal, latitudinal, and elevation
	// offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
	// measured along the surface of a sphere centered at the earth's center and whose surface
	// intersects the position of the observatory.
	virtual Quantum<Vector<Double> > getAntennaOffset(uInt which) const = 0;

	virtual Quantum<Vector<Double> > getAntennaOffset(const String& name) const = 0;

	virtual vector<Quantum<Vector<Double> > > getAntennaOffsets(
		const vector<MPosition>& positions
	) const = 0;

protected:

	struct SpwProperties {
		Double bandwidth;
		vector<Double> chanfreqs;
		Double chanwidth;
		Int netsideband;
		Double meanfreq;
		uInt nchans;
		vector<Double> edgechans;
	};

	static uInt _getNStates(const MeasurementSet& ms);

	static void _getStateToIntentsMap(
		vector<std::set<String> >& statesToIntentsMap,
		std::set<String>& uniqueIntents,
		const MeasurementSet& ms
	);

	static vector<uInt> _getScans(const MeasurementSet& ms);

	static std::map<uInt, std::set<uInt> > _getScanToStatesMap(
		const vector<uInt>& scans, const vector<uInt>& states
	);

	static vector<SpwProperties>  _getSpwInfo(
		std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
		std::set<uInt>& wvrSpw, const MeasurementSet& ms
	);

	static vector<uInt> _getDataDescIDs(const MeasurementSet& ms);

	static vector<uInt> _getStates(const MeasurementSet& ms);

	static vector<uInt> _getDataDescIDToSpwMap(const MeasurementSet& ms);

	static vector<uInt> _getFieldIDs(const MeasurementSet& ms);

	static vector<String> _getFieldNames(const MeasurementSet& ms);

	static void _checkTolerance(const Double tol);

	static vector<Double> _getTimes(const MeasurementSet& ms);

	static std::map<uInt, std::set<Double> > _getScanToTimesMap(
		const vector<uInt>& scans, const vector<Double>& times
	);

	static vector<MPosition> _getObservatoryPositions(
		vector<String>& names, const MeasurementSet& ms
	);

	static vector<String> _getAntennaNames(
		std::map<String, uInt>& namesToIDs, const MeasurementSet ms
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

	static vector<uInt> _toUIntVector(const Vector<Int>& v);


private:
	// This comment from thunter in the original ValueMapping python class
	// # Determine the number of polarizations for the first OBSERVE_TARGET intent.
    // # Used by plotbandpass for BPOLY plots since the number of pols cannot be inferred
    // # correctly from the caltable alone.  You cannot not simply use the first row, because
    // # it may be a pointing scan which may have different number of polarizations than what
    // # the TARGET and BANDPASS calibrator will have.
    // # -- T. Hunter
	void _setNumberOfPolarizations(const MeasurementSet& ms);

	void _setDataColumnNames(const MeasurementSet& ms);

	// set metadata from OBSERVATION table
	void _setObservation(const MeasurementSet& ms);

};
}

#endif /* MSMETADATA_H_ */
