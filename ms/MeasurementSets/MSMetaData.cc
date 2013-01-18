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

#include <ms/MeasurementSets/MSMetaData.h>

#include <casa/Utilities/GenSort.h>
#include <measures/Measures/MCPosition.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/Measures/MeasTable.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/TableProxy.h>
#include <tables/Tables/TableRecord.h>

// DEBUG ONLY
/*
#include <casa/Arrays/ArrayIO.h>
#include <iomanip>
#include <casa/OS/PrecTimer.h>
*/

#define _ORIGIN "MSMetaData::" + String(__FUNCTION__) + ": "

namespace casa {

MSMetaData::~MSMetaData() {}

// leave intact in this class
std::set<Double> MSMetaData::getTimesForScan(const uInt scan) const {
	std::set<uInt> scans;
	scans.insert(scan);
	// scan validity check is done in getTimesForScans()
	return getTimesForScans(scans);
}

std::set<uInt> MSMetaData::getScansForField(const String& field) const {
	std::set<uInt> fieldIDs = getFieldIDsForField(field);
	std::set<uInt> scans;
	for (
		std::set<uInt>::const_iterator fieldID=fieldIDs.begin();
		fieldID!=fieldIDs.end(); fieldID++
	) {
		std::set<uInt> myscans = getScansForFieldID(*fieldID);
		scans.insert(myscans.begin(), myscans.end());
	}
	return scans;
}

vector<String> MSMetaData::_getFieldNames(const MeasurementSet& ms) {
	String fieldNameColName = MSField::columnName(MSFieldEnums::NAME);
	ROScalarColumn<String> nameCol(ms.field(), fieldNameColName);
	return nameCol.getColumn().tovector();
}

vector<MPosition> MSMetaData::_getAntennaPositions(
	vector<String>& antennaNames, const MeasurementSet& ms
) {
	String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
	ROScalarColumn<String> nameCol(ms.antenna(), antNameColName);
	antennaNames = nameCol.getColumn().tovector();
	String antPosColName = MSAntenna::columnName(MSAntennaEnums::POSITION);
	ROArrayColumn<Double> posCol(ms.antenna(), antPosColName);
	Array<Double> xyz = posCol.getColumn();
	Vector<String> posUnits = posCol.keywordSet().asArrayString("QuantumUnits");
	String sFrame = posCol.keywordSet().asRecord("MEASINFO").asString("Ref");
	MPosition::Types posType = MPosition::getType(sFrame);
	Array<Double>::const_iterator end = xyz.end();
	Quantity x(0, posUnits[0]);
	Quantity y(0, posUnits[1]);
	Quantity z(0, posUnits[2]);
    vector<MPosition> antennaPositions;
	for (Array<Double>::const_iterator iter=xyz.begin(); iter!=end; iter++) {
		x.setValue(*iter);
		Double xm = x.getValue("m");
		iter++;
		y.setValue(*iter);
		Double ym = y.getValue("m");
		iter++;
		z.setValue(*iter);
		Double zm = z.getValue("m");
		MPosition antPos(MVPosition(xm, ym, zm), posType);
		antennaPositions.push_back(antPos);
	}
	return antennaPositions;
}

vector<Quantum<Vector<Double> > > MSMetaData::_getAntennaOffsets(
	const vector<MPosition>& antennaPositions,
	const MPosition& observatoryPosition
) {
	MPosition obsPos = observatoryPosition;
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
	vector<MPosition>::const_iterator end = antennaPositions.end();
	vector<Quantum<Vector<Double> > > antennaOffsets;
	for (
		vector<MPosition>::const_iterator iter=antennaPositions.begin();
		iter!=end; iter++
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
		// cout << "offset " << offset << endl;
		Quantum<Vector<Double> > qoffset(offset, "m");
		antennaOffsets.push_back(qoffset);
	}
	return antennaOffsets;
}

vector<String> MSMetaData::_getAntennaNames(
	map<String, uInt>& namesToIDs, const MeasurementSet ms
) {
	String antNameColName = MSAntenna::columnName(MSAntennaEnums::NAME);
	ROScalarColumn<String> nameCol(ms.antenna(), antNameColName);
	Vector<String> names = nameCol.getColumn();
	Vector<String>::const_iterator end = names.end();
	uInt i = 0;
	for (
		Vector<String>::const_iterator name=names.begin();
		name!=end; name++, i++
	) {
		namesToIDs[*name] = i;
	}
	return names.tovector();
}

std::map<uInt, std::set<uInt> > MSMetaData::_getScanToStatesMap(
	const vector<uInt>& scans, const vector<uInt>& states
) {
	vector<uInt>::const_iterator curScan = scans.begin();
	vector<uInt>::const_iterator lastScan = scans.end();
	vector<uInt>::const_iterator curStateID = states.begin();
	std::map<uInt, std::set<uInt> > scanToStatesMap;
	while (curScan != lastScan) {
		scanToStatesMap[*curScan].insert(*curStateID);
		curScan++;
		curStateID++;
	}
	return scanToStatesMap;
}

vector<uInt> MSMetaData::_getStates(const MeasurementSet& ms) {
	String stateColName = MeasurementSet::columnName(MSMainEnums::STATE_ID);
	ROScalarColumn<Int> stateCol(ms, stateColName);
	return _toUIntVector(stateCol.getColumn());
}

void MSMetaData::_getStateToIntentsMap(
	vector<std::set<String> >& stateToIntentsMap,
	std::set<String>& uniqueIntents, const MeasurementSet& ms
) {
	String intentsColName = MSState::columnName(MSStateEnums::OBS_MODE);
	ROScalarColumn<String> intentsCol(ms.state(), intentsColName);
	Vector<String> intentSets = intentsCol.getColumn();
	stateToIntentsMap.resize(_getNStates(ms));
	Vector<String>::const_iterator end = intentSets.end();
	vector<std::set<String> >::iterator sIter = stateToIntentsMap.begin();
	for(
		Vector<String>::const_iterator curIntentSet=intentSets.begin();
		curIntentSet!=end; curIntentSet++, sIter++
	) {
		Vector<String> intents = casa::stringToVector(*curIntentSet, ',');
		*sIter = std::set <String>(intents.begin(), intents.end());
		uniqueIntents.insert(intents.begin(), intents.end());
	}
}

uInt MSMetaData::_getNStates(const MeasurementSet& ms) {
	return ms.state().nrow();
}

vector<uInt> MSMetaData::_getDataDescIDs(const MeasurementSet& ms) {
	String ddColName = MeasurementSet::columnName(MSMainEnums::DATA_DESC_ID);
	ROScalarColumn<Int> ddCol(ms, ddColName);
	return _toUIntVector(ddCol.getColumn());
}

vector<uInt> MSMetaData::_getDataDescIDToSpwMap(const MeasurementSet& ms) {
	String spwColName = MSDataDescription::columnName(MSDataDescriptionEnums::SPECTRAL_WINDOW_ID);
	ROScalarColumn<Int> spwCol(ms.dataDescription(), spwColName);
	return _toUIntVector(spwCol.getColumn());
}

vector<uInt> MSMetaData::_getFieldIDs(const MeasurementSet& ms) {
	String fieldIdColName = MeasurementSet::columnName(MSMainEnums::FIELD_ID);
	ROScalarColumn<Int> fieldIdCol(ms, fieldIdColName);
	return _toUIntVector(fieldIdCol.getColumn());
}

std::map<uInt, std::set<Double> > MSMetaData::_getScanToTimesMap(
	const vector<uInt>& scans, const vector<Double>& times
) {
	vector<uInt>::const_iterator curScan = scans.begin();
	vector<uInt>::const_iterator lastScan = scans.end();
	vector<Double>::const_iterator curTime = times.begin();
	std::map<uInt, std::set<Double> > scanToTimesMap;
	while (curScan != lastScan) {
		scanToTimesMap[*curScan].insert(*curTime);
		curScan++;
		curTime++;
	}
	return scanToTimesMap;
}

vector<Double> MSMetaData::_getTimes(const MeasurementSet& ms) {
	String timeColName = MeasurementSet::columnName(MSMainEnums::TIME);
	ROScalarColumn<Double> timeCol(ms, timeColName);
	return timeCol.getColumn().tovector();
}

vector<MSMetaData::SpwProperties>  MSMetaData::_getSpwInfo(
	std::set<uInt>& avgSpw, std::set<uInt>& tdmSpw, std::set<uInt>& fdmSpw,
	std::set<uInt>& wvrSpw, const MeasurementSet& ms
) {
	const MSSpectralWindow spw = ms.spectralWindow();
	String bandwidthColName = MSSpectralWindow::columnName(MSSpectralWindowEnums::TOTAL_BANDWIDTH);
	ROScalarColumn<Double> bwCol(spw, bandwidthColName);
	Vector<Double> bws = bwCol.getColumn();
	String cfColName = MSSpectralWindow::columnName(MSSpectralWindowEnums::CHAN_FREQ);
	ROArrayColumn<Double> cfCol(spw, cfColName);
	String cwColName = MSSpectralWindow::columnName(MSSpectralWindowEnums::CHAN_WIDTH);
	ROArrayColumn<Double> cwCol(spw, cwColName);
	String nsColName = MSSpectralWindow::columnName(MSSpectralWindowEnums::NET_SIDEBAND);
	ROScalarColumn<Int> nsCol(spw, nsColName);
	Vector<Int> nss = nsCol.getColumn();
	SpwProperties props;
	vector<Double> freqLimits(2);
	Vector<Double> djunk;
	Vector<Double> tmp;
	vector<SpwProperties> spwInfo;
	for (uInt i=0; i<bws.size(); i++) {
		props.bandwidth = bws[i];
		props.chanfreqs.resize(0);
		tmp.resize(0);
		cfCol.get(i, tmp);
		tmp.tovector(props.chanfreqs);
		freqLimits[0] = min(tmp);
		freqLimits[1] = max(tmp);
		props.edgechans = freqLimits;
		djunk.resize(0);
		cwCol.get(i, djunk);
		props.chanwidth = djunk[0];
		// coded this way in ValueMapping
		props.netsideband = nss[i] == 2 ? 1 : -1;
		props.meanfreq = mean(tmp);
		tmp.tovector(props.chanfreqs);
		props.nchans = props.chanfreqs.size();
		spwInfo.push_back(props);
		if (props.nchans==64 || props.nchans==128 || props.nchans==256) {
			tdmSpw.insert(i);
		}
		else if (props.nchans==1) {
			avgSpw.insert(i);
		}
		else if (props.nchans==4) {
			wvrSpw.insert(i);
		}
		else {
			fdmSpw.insert(i);
		}
	}
	return spwInfo;
}

vector<uInt> MSMetaData::_getScans(const MeasurementSet& ms) {
	String scanColName = MeasurementSet::columnName(MSMainEnums::SCAN_NUMBER);
	ROScalarColumn<Int> scanCol(ms, scanColName);
	vector<uInt> scans = _toUIntVector(scanCol.getColumn());
	return scans;
}

vector<MPosition> MSMetaData::_getObservatoryPositions(
	vector<String>& names, const MeasurementSet& ms
) {
	String tnameColName = MSObservation::columnName(MSObservationEnums::TELESCOPE_NAME);
	ROScalarColumn<String> telescopeNameCol(ms.observation(), tnameColName);
	names = telescopeNameCol.getColumn().tovector();
	vector<MPosition> observatoryPositions(names.size());
	for (uInt i=0; i<observatoryPositions.size(); i++) {
		if (names[i].length() == 0) {
			throw AipsError(
				_ORIGIN
				+ "The name of the telescope is not stored in the measurement set."
			);
		}
		if (! MeasTable::Observatory(observatoryPositions[i], names[i])) {
			throw AipsError(
				_ORIGIN
				+ "The name of the telescope is not stored in the measurement set."
			);
		}
	}
	return observatoryPositions;
}

vector<uInt> MSMetaData::_toUIntVector(const Vector<Int>& v) {
	if (anyLT(v, 0)) {
		throw AipsError("Column that should contain nonnegative ints has a negative int");
	}
	vector<uInt> u;
	for (Vector<Int>::const_iterator iter=v.begin(); iter!=v.end(); iter++) {
		u.push_back(*iter);
	}
	return u;
}

void MSMetaData::_checkTolerance(const Double tol) {
	if (tol < 0) {
		throw AipsError(
			"MSMetaData::" + String(__FUNCTION__)
			+ " : Tolerance cannot be less than zero"
		);
	}
}

}

