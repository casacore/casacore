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

#ifndef MS_MSMETADATAONDEMAND_H
#define MS_MSMETADATAONDEMAND_H

#include <ms/MeasurementSets/MSMetaData.h>

namespace casa {

// <summary>
// Class to interrogate  an MS for metadata. Interrogation happens on demand
// and metadata are not stored.
// </summary>

class MSMetaDataOnDemand : public MSMetaData {
public:

	// construct an object which stores a pointer to the MS and queries the MS
	// only as necessary. The MeasurementSet pointer passed in should not go out
	// of scope in the calling code until the caller has finished with this object,
	// or else subsequent method calls on this object will result in a segmentation
	// fault; the pointer is not copied.
	MSMetaDataOnDemand(const MeasurementSet& ms);

	virtual ~MSMetaDataOnDemand();

	// number of unique states (number of rows from the STATE table)
	uInt nStates() const;

	// get unique scan numbers
	std::set<uInt> getScanNumbers() const;

	std::set<uInt> getScansForState(const uInt stateID) const;

	std::set<String> getIntentsForScan(const uInt scan) const;

	// get all intents, in no particular (nor guaranteed) order.
	std::set<String> getIntents() const;

	// get a set of spectral windows for which the specified <src>intent</src>
	// applies.
	virtual std::set<uInt> getSpwsForIntent(const String& intent) const;

	// get number of spectral windows
	uInt nSpw() const;

	// get the number of visibilities
	uInt nVisibilities() const;

	// get a set of intents corresponding to the specified spectral window
	std::set<String> getIntentsForSpw(const uInt spw) const;

	// get the number of fields.
	uInt nFields() const;

	// get a set of spectral windows corresponding to the specified fieldID
	std::set<uInt> getSpwsForField(const uInt fieldID) const;

	// get a set of spectral windows corresponding to the specified field name
	std::set<uInt> getSpwsForField(const String& fieldName) const;

	// get the set of field IDs corresponding to the specified spectral window.
	std::set<uInt> getFieldIDsForSpw(const uInt spw) const;

	// get the set of field names corresponding to the specified spectral window.
	std::set<String> getFieldNamesForSpw(const uInt spw) const;

	// get the number of scans in the dataset
	uInt nScans() const;

	// get the set of spectral windows for the specified scan number.
	std::set<uInt> getSpwsForScan(const uInt scan) const;

	// get the set of scan numbers for the specified spectral window.
	std::set<uInt> getScansForSpw(const uInt spw) const;

	// get the number of antennas in the ANTENNA table
	uInt nAntennas() const;

	// get the name of the antenna for the specified antenna ID
	vector<String> getAntennaNames(const vector<uInt>& antennaIDs) const;

	// get the antenna ID for the antenna with the specified name.
	vector<uInt> getAntennaIDs(const vector<String>& antennaNames) const;

	// get set of spectral windows used for TDM. These are windows that have
	// 64, 128, or 256 channels
	std::set<uInt> getTDMSpw() const;

	// get set of spectral windows used for FDM. These are windows that do not
	// have 1, 4, 64, 128, or 256 channels.
	std::set<uInt> getFDMSpw() const;

	// get spectral windows that have been averaged. These are windows with 1 channel.
	std::set<uInt> getChannelAvgSpw() const;

	// Get the spectral window set used for WVR measurements. These have 4 channels each.
	std::set<uInt> getWVRSpw() const;

	// Get the scans which fail into the specified time range (center-tol to center+tol)
	std::set<uInt> getScansForTimes(const Double center, const Double tol) const;

	// Get the times for the specified scans
	std::set<Double> getTimesForScans(const std::set<uInt> scans) const;

	// get the times for the specified scan
	// std::set<Double> getTimesForScan(const uInt scan) const;

	// get the stateIDs associated with the specified scan number.
	std::set<uInt> getStatesForScan(const uInt scan) const;

	// get the scans associated with the specified intent
	std::set<uInt> getScansForIntent(const String& intent) const;

	// get the scan numbers associated with the specified field ID.
	std::set<uInt> getScansForFieldID(const uInt fieldID) const;

	// get the field IDs for the specified field name. Case insensitive.
	std::set<uInt> getFieldIDsForField(const String& field) const;

	// get the scan numbers associated with the specified field.
	//std::set<uInt> getScansForField(const String& field) const;

	// get field IDs associated with the specified scan number.
	std::set<uInt> getFieldsForScan(const uInt scan) const;

	// get the field IDs associated with the specified scans
	std::set<uInt> getFieldsForScans(const std::set<uInt>& scans) const;

	// get the field IDs associated with the specified intent.
	std::set<uInt> getFieldsForIntent(const String& intent) const;

	// get the field names associated with the specified field IDs. If <src>fieldIDs</src>
	// is empty, a vector of all the field names is returned.
	vector<String> getFieldNamesForFieldIDs(const vector<uInt>& fieldIDs) const;

	// Get the fields which fail into the specified time range (center-tol to center+tol)
	std::set<uInt> getFieldsForTimes(Double center, Double tol) const;

	// get the times for which the specified field was observed
	std::set<Double> getTimesForField(uInt fieldID) const;

	// get telescope names in the order they are listed in the OBSERVATION table. These are
	// the telescopes (observatories), not the antenna names.
	vector<String> getObservatoryNames() const;

	// get the position of the specified telescope (observatory).
	MPosition getObservatoryPosition(uInt which) const;

	// get the positions of the specified antennas. If <src>which</src> is empty, return
	// all antenna positions.
	vector<MPosition> getAntennaPositions(const vector<uInt>& which=std::vector<uInt>(0)) const;

	// <src>names</src> cannot be empty.
	vector<MPosition> getAntennaPositions(const vector<String>& names) const;

	// get the position of the specified antenna relative to the observatory position.
	// the three vector returned represents the longitudinal, latitudinal, and elevation
	// offsets (elements 0, 1, and 2 respectively). The longitude and latitude offsets are
	// measured along the surface of a sphere centered at the earth's center and whose surface
	// intersects the position of the observatory.
	Quantum<Vector<Double> > getAntennaOffset(uInt which) const;

	Quantum<Vector<Double> > getAntennaOffset(const String& name) const;

	// if not empty, <src>positions</src> must contain the same number of elements as the
	// number of antennas in the MS. These will be used instead of also retrieving the antenna
	// positions from the MS.
	vector<Quantum<Vector<Double> > > getAntennaOffsets(
		const vector<MPosition>& positions=vector<MPosition>(0)
	) const;

private:
	const MeasurementSet _ms;

	// disallow copy constructor and = operator
	MSMetaDataOnDemand(const MSMetaDataOnDemand&);
	MSMetaDataOnDemand operator =(const MSMetaDataOnDemand&);

	// This comment from thunter in the original ValueMapping python class
	// # Determine the number of polarizations for the first OBSERVE_TARGET intent.
    // # Used by plotbandpass for BPOLY plots since the number of pols cannot be inferred
    // # correctly from the caltable alone.  You cannot not simply use the first row, because
    // # it may be a pointing scan which may have different number of polarizations than what
    // # the TARGET and BANDPASS calibrator will have.
    // # -- T. Hunter
	uInt _getNumberOfPolarizations() const;

	void _setSpwInfo(const MeasurementSet& ms);

	void _getDataColumnNames(
		String& dataName, String& correctedName,
		String& modelName
	) const;

	//static vector<uInt> _getScans(const MeasurementSet& ms);

	// set metadata from OBSERVATION table
	void _setObservation(const MeasurementSet& ms);

	// static vector<uInt> _toUIntVector(const Vector<Int>& v);

	static void _checkScan(const uInt scan, const std::set<uInt> allScans);

	void _checkFieldID(const uInt fieldID) const;

	void _checkFieldIDs(const vector<uInt>& fieldIDs) const;

	vector<std::set<String> > _getSpwToIntentsMap() const;

	vector<std::set<uInt> > _getSpwToFieldMap();

	vector<std::set<uInt> > _getSpwToScansMap();

	static std::set<uInt> _getUnique(const vector<uInt>& v);

};
}

#endif /* MSMETADATA_H_ */
