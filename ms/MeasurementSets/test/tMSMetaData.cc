//# tMSReader.cc: This program tests the MSReader class
//# Copyright (C) 2000,2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

//# Includes
#include <casa/aips.h>

#include <ms/MeasurementSets/MSMetaDataOnDemand.h>
#include <ms/MeasurementSets/MSMetaDataPreload.h>

#include <casa/BasicMath/StdLogical.h>
#include <casa/OS/Directory.h>
#include <casa/OS/EnvVar.h>
#include <ms/MeasurementSets/MeasurementSet.h>

#include <casa/Arrays/ArrayIO.h>

#include <casa/namespace.h>

void _printSet(const std::set<uInt>& set) {
	const std::set<uInt>::const_iterator end = set.end();
	for (
		std::set<uInt>::const_iterator iter = set.begin();
		iter!=end; iter++
	) {
		if (iter!=set.begin()) {
			cout << ", ";
		}
		cout << *iter;
	}
	cout << endl;
}

void testIt(MSMetaData& md) {
	cout << "*** test nStates()" << endl;
	AlwaysAssert(md.nStates() == 43, AipsError);
	cout << "*** test getScansForState()" << endl;
	for (uInt stateID=0; stateID<md.nStates(); stateID++) {
		std::set<uInt> scans = md.getScansForState(stateID);
		std::set<uInt> exp;
		if (stateID < 5) {
			uInt myints[]= {1, 5, 8};
			exp.insert(myints, myints + 3);
		}
		else if (stateID < 7) {
			exp.insert(2);
		}
		else if (stateID < 10) {
			uInt myints[]= {3, 6, 9, 11, 13, 15, 17, 19, 22, 24, 26, 29, 31};
			exp.insert(myints, myints + 13);
		}
		else if (stateID < 26) {
			exp.insert(4);
		}
		else if (stateID < 32) {
			exp.insert(7);
		}
		else if (stateID < 33) {
			uInt myints[] = {10, 14, 18, 21, 25, 28, 32};
			exp.insert(myints, myints + 7);
		}
		else if (stateID < 37) {
			uInt myints[] = {12, 16, 20, 23, 27, 30};
			exp.insert(myints, myints + 6);
		}
		else {
			uInt myints[] = {12, 16, 20, 23};
			exp.insert(myints, myints + 4);
		}
		AlwaysAssert(scans == exp, AipsError);

	}
	AlwaysAssert(md.getIntents().size() == 11, AipsError);

	cout << "*** test getScanNumbers()" << endl;
	std::set<uInt> scans = md.getScanNumbers();
	cout << __FILE__ << " " << __LINE__ << endl;
	uInt myints[] = {
			1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
			18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32
	};
	{
		std::set<uInt> exp;
		exp.insert(myints, myints+32);
		AlwaysAssert(scans == exp, AipsError);
	}
	std::set<String> uniqueIntents;
	cout << "*** test getIntentsForScan()" << endl;

	for (
			std::set<uInt>::const_iterator scanNum = scans.begin();
			scanNum!=scans.end(); scanNum++
	) {
		std::set<String> intents = md.getIntentsForScan(*scanNum);
		std::set<String> exp;

		if (*scanNum == 1 || *scanNum == 5 || *scanNum == 8) {
			String mystr[] = {
					"CALIBRATE_POINTING#ON_SOURCE", "CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+2);
		}
		else if (*scanNum == 2) {
			String mystr[] = {
					"CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE",
					"CALIBRATE_SIDEBAND_RATIO#ON_SOURCE",
					"CALIBRATE_WVR#OFF_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+4);
		}
		else if (
				*scanNum == 3 || *scanNum == 6
				|| *scanNum == 9 || *scanNum == 11
				|| *scanNum == 13 || *scanNum == 15
				|| *scanNum == 17 || *scanNum == 19
				|| *scanNum == 22 || *scanNum == 24
				|| *scanNum == 26 || *scanNum == 29
				|| *scanNum == 31
		) {
			String mystr[] = {
					"CALIBRATE_ATMOSPHERE#OFF_SOURCE",
					"CALIBRATE_ATMOSPHERE#ON_SOURCE",
					"CALIBRATE_WVR#OFF_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+4);
		}
		else if (*scanNum == 4) {
			String mystr[] = {
					"CALIBRATE_BANDPASS#ON_SOURCE",
					"CALIBRATE_PHASE#ON_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+3);
		}
		else if (*scanNum == 7) {
			String mystr[] = {
					"CALIBRATE_AMPLI#ON_SOURCE",
					"CALIBRATE_PHASE#ON_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+3);
		}
		else if (
				*scanNum == 10 || *scanNum == 14
				|| *scanNum == 18 || *scanNum == 21
				|| *scanNum == 25 || *scanNum == 28
				|| *scanNum == 32
		) {
			String mystr[] = {
					"CALIBRATE_PHASE#ON_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+2);
		}
		else if (
				*scanNum == 12 || *scanNum == 16
				|| *scanNum == 20 || *scanNum == 23
				|| *scanNum == 27 || *scanNum == 30
		) {
			exp.insert("OBSERVE_TARGET#ON_SOURCE");
		}
		uniqueIntents.insert(exp.begin(), exp.end());
		AlwaysAssert(intents == exp, AipsError);
	}
	cout << __FILE__ << " " << __LINE__ << endl;

	AlwaysAssert(md.getIntents() == uniqueIntents, AipsError);
	cout << "*** test getSpwsForIntent()" << endl;
	for (
			std::set<String>::const_iterator intent=uniqueIntents.begin();
			intent!=uniqueIntents.end(); intent++
	) {
		std::set<uInt> exp;
		if (
				*intent == "CALIBRATE_AMPLI#ON_SOURCE"
						|| *intent == "CALIBRATE_BANDPASS#ON_SOURCE"
								|| *intent == "CALIBRATE_PHASE#ON_SOURCE"
										|| *intent == "OBSERVE_TARGET#ON_SOURCE"
		) {
			uInt myints[] = {0, 17, 18, 19, 20, 21, 22, 23, 24};
			exp.insert(myints, myints+9);
		}
		else if (
				*intent == "CALIBRATE_ATMOSPHERE#OFF_SOURCE"
						|| *intent == "CALIBRATE_ATMOSPHERE#ON_SOURCE"
								|| *intent == "CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE"
										|| *intent == "CALIBRATE_SIDEBAND_RATIO#ON_SOURCE"
												|| *intent == "CALIBRATE_WVR#OFF_SOURCE"
		) {
			uInt myints[] = {0, 9, 10, 11, 12, 13, 14, 15, 16};
			exp.insert(myints, myints+9);
		}
		else if (
				*intent == "CALIBRATE_POINTING#ON_SOURCE"
		) {
			uInt myints[] = {0, 1, 2, 3, 4, 5, 6, 7, 8};
			exp.insert(myints, myints+9);
		}
		else if (
				*intent == "CALIBRATE_WVR#ON_SOURCE"
		) {
			uInt myints[] = {
					0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
					10, 11, 12, 13, 14, 15, 16,
					17, 18, 19, 20, 21, 22, 23, 24
			};
			exp.insert(myints, myints+25);
		}
		AlwaysAssert(md.getSpwsForIntent(*intent) == exp, AipsError);
	}
	cout << "*** test nSpw()" << endl;
	uInt nSpw = md.nSpw();
	AlwaysAssert(nSpw == 40, AipsError);
	cout << "*** test getIntentsForSpw()" << endl;
	for (uInt spw=0; spw<nSpw; spw++) {
		std::set<String> exp;
		if (spw == 0) {
			String mystr[] = {
					"CALIBRATE_AMPLI#ON_SOURCE",
					"CALIBRATE_ATMOSPHERE#OFF_SOURCE",
					"CALIBRATE_ATMOSPHERE#ON_SOURCE",
					"CALIBRATE_BANDPASS#ON_SOURCE",
					"CALIBRATE_PHASE#ON_SOURCE",
					"CALIBRATE_POINTING#ON_SOURCE",
					"CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE",
					"CALIBRATE_SIDEBAND_RATIO#ON_SOURCE",
					"CALIBRATE_WVR#OFF_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE",
					"OBSERVE_TARGET#ON_SOURCE"
			};
			exp.insert(mystr, mystr+11);
		}
		else if (spw < 9) {
			String mystr[] = {
					"CALIBRATE_POINTING#ON_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+2);
		}
		else if (spw < 17) {
			String mystr[] = {
					"CALIBRATE_ATMOSPHERE#OFF_SOURCE",
					"CALIBRATE_ATMOSPHERE#ON_SOURCE",
					"CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE",
					"CALIBRATE_SIDEBAND_RATIO#ON_SOURCE",
					"CALIBRATE_WVR#OFF_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE"
			};
			exp.insert(mystr, mystr+6);
		}
		else if (spw < 25) {
			String mystr[] = {
					"CALIBRATE_AMPLI#ON_SOURCE",
					"CALIBRATE_BANDPASS#ON_SOURCE",
					"CALIBRATE_PHASE#ON_SOURCE",
					"CALIBRATE_WVR#ON_SOURCE",
					"OBSERVE_TARGET#ON_SOURCE"
			};
			exp.insert(mystr, mystr+5);
		}
		AlwaysAssert(md.getIntentsForSpw(spw) == exp, AipsError);
	}
	{
		cout << "*** test nFields()" << endl;
		uInt nFields = md.nFields();
		AlwaysAssert(nFields == 6, AipsError);
		cout << "*** test getSpwsForField()" << endl;
		String names[] = {
				"3C279", "J1337-129", "Titan",
				"J1625-254", "V866 Sco", "RNO 90"
		};
		for (uInt i=0; i<nFields; i++) {
			std::set<uInt> exp;
			if (i==0 || i==3) {
				uInt myints[] = {
						0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
						10, 11, 12, 13, 14, 15, 16, 17,
						18, 19, 20, 21, 22, 23, 24
				};
				exp.insert(myints, myints+25);
			}
			if (i == 1) {
				uInt myints[] = {
						0, 1, 2, 3, 4, 5, 6, 7, 8
				};
				exp.insert(myints, myints+9);
			}
			if (i==2 || i==4 || i==5) {
				uInt myints[] = {
						0, 9, 10, 11, 12, 13, 14, 15, 16,
						17, 18, 19, 20, 21, 22, 23, 24
				};
				exp.insert(myints, myints+17);
			}
			AlwaysAssert(md.getSpwsForField(i) == exp, AipsError);
			AlwaysAssert(md.getSpwsForField(names[i]) == exp, AipsError);
		}
		cout << "*** test getFieldIDsForSpw()" << endl;
		for (uInt i=0; i<md.nSpw(); i++) {
			std::set<uInt> exp;
			std::set<String> expNames;
			if (i==0) {
				uInt myints[] = {0, 1, 2, 3, 4, 5};
				exp.insert(myints, myints+6);
				String mystr[] = {
						"3C279", "J1337-129", "Titan", "J1625-254",
						"V866 Sco", "RNO 90"
				};
				expNames.insert(mystr, mystr+6);

			}
			else if (i<9) {
				uInt myints[] = {0, 1, 3};
				exp.insert(myints, myints+3);
				String mystr[] = {
						"3C279", "J1337-129", "J1625-254"
				};
				expNames.insert(mystr, mystr+3);
			}
			else if (i<25) {
				uInt myints[] = {0, 2, 3, 4, 5};
				exp.insert(myints, myints+5);
				String mystr[] = {
						"3C279", "Titan", "J1625-254",
						"V866 Sco", "RNO 90"

				};
				expNames.insert(mystr, mystr+5);
			}
			else {
				// nothing, exp is an empty set
			}
			AlwaysAssert(md.getFieldIDsForSpw(i) == exp, AipsError);
			AlwaysAssert(md.getFieldNamesForSpw(i) == expNames, AipsError);
		}
	}
	{
		cout << "*** test nScans()" << endl;
		AlwaysAssert(md.nScans() == 32, AipsError);
		std::set<uInt> scanNumbers = md.getScanNumbers();
		cout << "*** test getSpwsForScan()" << endl;
		for (
				std::set<uInt>::const_iterator scan=scanNumbers.begin();
				scan!=scanNumbers.end(); scan++
		) {
			std::set<uInt> exp;
			if (*scan == 1 || *scan==5 || *scan==8) {
				uInt myints[] = {
						0, 1, 2, 3, 4, 5, 6, 7, 8
				};
				exp.insert(myints, myints+9);
			}
			else if (
					*scan == 2 || *scan==3 || *scan==6 || *scan==9
					|| *scan==11 || *scan==13 || *scan==15 || *scan==17
					|| *scan==19 || *scan==22 || *scan==24 || *scan==26
					|| *scan==29 || *scan==31
			) {
				Int myints[] = {
						0, 9, 10, 11, 12, 13, 14, 15, 16
				};
				exp.insert(myints, myints+9);
			}
			else if (
					*scan==4 || *scan==7 || *scan==10 || *scan==12
					|| *scan==14 || *scan==16 || *scan==18 || *scan==20
					|| *scan==21 || *scan==23 || *scan==25 || *scan==27
					|| *scan==28 || *scan==30 || *scan==32
			) {
				uInt myints[] = {
						0, 17, 18, 19, 20, 21, 22, 23, 24
				};
				exp.insert(myints, myints+9);
			}
			AlwaysAssert(md.getSpwsForScan(*scan) == exp, AipsError);
		}
		cout << "*** test getScansForSpw()" << endl;
		for (uInt i=0; i<md.nSpw(); i++) {
			std::set<uInt> exp;
			if (i==0) {
				uInt myints[] = {
						1,  2,  3,  4,  5,  6,  7,  8,  9,
						10, 11, 12, 13, 14, 15, 16, 17, 18,
						19, 20, 21, 22, 23, 24, 25, 26, 27,
						28, 29, 30, 31, 32
				};
				exp.insert(myints, myints+32);
			}
			else if (i<9) {
				uInt myints[] = {1, 5, 8};
				exp.insert(myints, myints+3);
			}
			else if (i<17) {
				uInt myints[] = {
						2,  3,  6,  9, 11, 13, 15,
						17, 19, 22, 24, 26, 29, 31
				};
				exp.insert(myints, myints+14);
			}
			else if (i<25) {
				uInt myints[] = {
						4,  7, 10, 12, 14, 16, 18,
						20, 21, 23, 25, 27, 28, 30, 32
				};
				exp.insert(myints, myints+15);
			}
			else {
				// empty set
			}

			AlwaysAssert(md.getScansForSpw(i) == exp, AipsError);
		}
		{
			cout << "*** test nAntennas()" << endl;
			AlwaysAssert(md.nAntennas()==15, AipsError);
			cout << "*** test getAntennaName()" << endl;
			String name;
			String expnames[] = {
					"DA43", "DA44", "DV02", "DV03", "DV05",
					"DV07", "DV08", "DV10", "DV12", "DV13",
					"DV14", "DV15", "DV16", "DV17", "DV18"
			};
			for (uInt i=0; i<md.nAntennas(); i++) {
				vector<uInt> ids(1);
				ids[0] = i;
				AlwaysAssert(
						md.getAntennaNames(ids)[0] == expnames[i],
						AipsError
				);
			}
			cout << "*** test getAntennaID()" << endl;
			for (uInt i=0; i<md.nAntennas(); i++) {
				vector<uInt> ids(1);
				ids[0] = i;
				AlwaysAssert(
						md.getAntennaIDs(md.getAntennaNames(ids))[0]==i,
						AipsError
				);
			}
		}
		{
			cout << "*** test getTDMSpw()" << endl;
			std::set<uInt> exp;
			uInt myints[] = {1, 3, 5, 7, 9, 11, 13, 15};
			exp.insert(myints, myints+8);
			AlwaysAssert(md.getTDMSpw() == exp, AipsError);
		}
		{
			cout << "*** test getFDMSpw()" << endl;
			std::set<uInt> exp;
			uInt myints[] = {17, 19, 21, 23};
			exp.insert(myints, myints+4);
			AlwaysAssert(md.getFDMSpw() == exp, AipsError);
		}
		{
			cout << "*** test getChannelAvgSpw()" << endl;
			std::set<uInt> exp;
			uInt myints[] = {
					2, 4, 6, 8, 10, 12, 14,
					16, 18, 20, 22, 24
			};
			exp.insert(myints, myints+12);
			AlwaysAssert(md.getChannelAvgSpw() == exp, AipsError);
		}
		{
			cout << "*** test getWVRSpw()" << endl;
			std::set<uInt> exp;
			uInt myints[] = {
					0, 25, 26, 27, 28, 29, 30, 31,
					32, 33, 34, 35, 36, 37, 38, 39
			};
			exp.insert(myints, myints+16);
			AlwaysAssert(md.getWVRSpw() == exp, AipsError);
		}
		{
			cout << "*** test getScansForTimes()" << endl;
			std::set<uInt> exp;
			exp.insert(27);
			AlwaysAssert(
					md.getScansForTimes(4.84282937e+09,20) == exp,
					AipsError
			);
			exp.insert(24);
			exp.insert(25);
			exp.insert(26);
			exp.insert(28);
			AlwaysAssert(
					md.getScansForTimes(4.84282937e+09,200) == exp,
					AipsError
			);

		}
		{
			cout << "*** test getTimesForScans()" << endl;
			std::set<Double> expec;
			Double myd[] = {
					4842825928.7, 4842825929.5,
					4842825930.0,
					4842825930.6, 4842825941.4,
					4842825942.2, 4842825942.5,
					4842825942.7, 4842825943.2,
					4842825954.0, 4842825954.9,
					4842825955.2, 4842825955.4,
					4842825955.9, 4842825003.6,
					4842825004.0, 4842825004.5,
					4842825004.8, 4842825005.0,
					4842825016.3, 4842825016.6,
					4842825017.1, 4842825017.5,
					4842825017.6, 4842825029.0,
					4842825029.3, 4842825029.8,
					4842825030.1, 4842825030.3
			};
			expec.insert(myd, myd+29);
			std::set<uInt> myscans;
			myscans.insert(3);
			myscans.insert(6);
			AlwaysAssert(
					allNearAbs(md.getTimesForScans(myscans), expec, 0.1),
					AipsError
			);
		}
		{
			cout << "*** test getTimesForScan()" << endl;
			std::set<Double> expec;
			Double myd[] = {
					4842825003.6,
					4842825004.0, 4842825004.5,
					4842825004.8, 4842825005.0,
					4842825016.3, 4842825016.6,
					4842825017.1, 4842825017.5,
					4842825017.6, 4842825029.0,
					4842825029.3, 4842825029.8,
					4842825030.1, 4842825030.3
			};
			expec.insert(myd, myd+15);
			std::set<uInt> myscans;
			myscans.insert(3);
			AlwaysAssert(
					allNearAbs(md.getTimesForScans(myscans), expec, 0.1),
					AipsError
			);
		}
		{
			cout << "*** test getStatesForScan()" << endl;
			std::set<uInt> expec;
			std::set<uInt> scanNumbers = md.getScanNumbers();
			for (
					std::set<uInt>::const_iterator curScan=scanNumbers.begin();
					curScan!=scanNumbers.end(); curScan++
			) {
				expec.clear();
				if (*curScan == 1 || *curScan == 5 || *curScan == 8) {
					Int mine[] = {0, 1, 2, 3, 4};
					expec.insert(mine, mine+5);
				}
				else if (*curScan == 2) {
					Int mine[] = {5, 6};
					expec.insert(mine, mine+2);
				}
				else if (
						*curScan == 3 || *curScan==6 || *curScan==9
						|| *curScan==11 || *curScan==13 || *curScan==15
						|| *curScan==17 || *curScan==19 || *curScan==22
						|| *curScan==24 || *curScan==26 || *curScan==29
						|| *curScan==31
				) {
					Int mine[] = {7, 8, 9};
					expec.insert(mine, mine+3);
				}
				else if (*curScan==4) {
					Int mine[] = {
							10, 11, 12, 13, 14, 15, 16, 17, 18,
							19, 20, 21, 22, 23, 24, 25
					};
					expec.insert(mine, mine+16);
				}
				else if (*curScan==7) {
					Int mine[] = {26, 27, 28, 29, 30, 31};
					expec.insert(mine, mine+6);
				}
				else if (
						*curScan==10 || *curScan==14 || *curScan==18
						|| *curScan==21 || *curScan==25 || *curScan==28
						|| *curScan==32
				) {
					expec.insert(32);
				}
				else if (
						*curScan==12 || *curScan==16
						|| *curScan==20 || *curScan==23
				) {
					Int mine[] = {
							33, 34, 35, 36, 37, 38, 39, 40, 41, 42
					};
					expec.insert(mine, mine+10);
				}
				else {
					Int mine[] = {33, 34, 35, 36};
					expec.insert(mine, mine+4);
				}
				std::set<uInt> got = md.getStatesForScan(*curScan);
				AlwaysAssert(got == expec, AipsError);
			}
		}
		{
			cout << "*** test getScansForIntent()" << endl;
			std::set<String> intents = md.getIntents();
			for (
					std::set<String>::const_iterator intent=intents.begin();
					intent!=intents.end(); intent++
			) {
				std::set<uInt> expec;
				if (
						*intent=="CALIBRATE_AMPLI#ON_SOURCE"
				) {
					expec.insert(7);
				}
				else if (
						*intent=="CALIBRATE_ATMOSPHERE#OFF_SOURCE"
								|| *intent=="CALIBRATE_ATMOSPHERE#ON_SOURCE"
				) {
					uInt mine[] = {
							3, 6, 9, 11, 13, 15, 17,
							19, 22, 24, 26, 29, 31
					};
					expec.insert(mine, mine+13);
				}
				else if (*intent=="CALIBRATE_BANDPASS#ON_SOURCE") {
					expec.insert(4);
				}
				else if (*intent=="CALIBRATE_PHASE#ON_SOURCE") {
					uInt mine[] = {
							4, 7, 10, 14, 18, 21, 25, 28, 32
					};
					expec.insert(mine, mine+9);
				}
				else if (*intent=="CALIBRATE_POINTING#ON_SOURCE") {
					uInt mine[] = {1, 5, 8};
					expec.insert(mine, mine+3);
				}
				else if (
						*intent=="CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE"
								|| *intent=="CALIBRATE_SIDEBAND_RATIO#ON_SOURCE"
				) {
					expec.insert(2);
				}
				else if (*intent=="CALIBRATE_WVR#OFF_SOURCE") {
					uInt mine[] = {
							2, 3, 6, 9, 11, 13, 15, 17,
							19, 22, 24, 26, 29, 31
					};
					expec.insert(mine, mine+14);
				}
				else if (*intent=="CALIBRATE_WVR#ON_SOURCE") {
					uInt mine[] = {
							1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
							11, 13, 14, 15, 17, 18, 19, 21,
							22, 24, 25, 26, 28, 29, 31, 32
					};
					expec.insert(mine, mine+26);
				}
				else {
					uInt mine[] = {12, 16, 20, 23, 27, 30};
					expec.insert(mine, mine+6);
				}
				std::set<uInt> got = md.getScansForIntent(*intent);
				AlwaysAssert(md.getScansForIntent(*intent) == expec, AipsError);
			}
		}
		{
			cout << "*** test getScansForFieldID()" << endl;
			std::set<uInt> expec;
			for (uInt i=0; i<6; i++) {
				expec.clear();
				switch(i) {
				case 0:
				{
					uInt mine[] = {1, 2, 3, 4};
					expec.insert(mine, mine+4);
					break;
				}
				case 1:
					expec.insert(5);
					break;
				case 2:
					expec.insert(6);
					expec.insert(7);
					break;
				case 3: {
					uInt mine[] = {
							8, 9, 10, 13, 14, 17, 18,
							21, 24, 25, 28, 31, 32
					};
					expec.insert(mine, mine+13);
					break;
				}
				case 4: {
					uInt mine[] = {
							11, 12, 19, 20, 26, 27
					};
					expec.insert(mine, mine+6);
					break;
				}
				case 5: {
					uInt mine[] = {
							15, 16, 22, 23, 29, 30
					};
					expec.insert(mine, mine+6);
					break;
				}
				default:
					throw AipsError("bad fieldID");
				}
				AlwaysAssert(md.getScansForFieldID(i) == expec, AipsError);
			}
		}
		{
			cout << "*** test getFieldIDsForField()" << endl;
			for (uInt i=0; i<6; i++) {
				std::set<uInt> expec;
				expec.insert(i);
				String name = i == 0 ? "3C279"
						: i == 1 ? "J1337-129"
								: i == 2 ? "Titan"
										: i == 3 ? "J1625-254"
												: i == 4 ? "V866 Sco"
														: "RNO 90";
				AlwaysAssert(
						md.getFieldIDsForField(name) == expec,
						AipsError
				);
			}
		}
		{
			cout << "*** test getScansForField()" << endl;
			for (uInt i=0; i<6; i++) {
				std::set<uInt> expec;
				String name;
				switch(i) {
				case 0:
				{
					name = "3C279";
					uInt mine[] = {1, 2, 3, 4};
					expec.insert(mine, mine+4);
					break;
				}
				case 1:
					name = "J1337-129";
					expec.insert(5);
					break;
				case 2:
					name = "Titan";
					expec.insert(6);
					expec.insert(7);
					break;
				case 3: {
					name = "J1625-254";
					uInt mine[] = {
							8, 9, 10, 13, 14, 17, 18,
							21, 24, 25, 28, 31, 32
					};
					expec.insert(mine, mine+13);
					break;
				}
				case 4: {
					name = "V866 Sco";
					uInt mine[] = {
							11, 12, 19, 20, 26, 27
					};
					expec.insert(mine, mine+6);
					break;
				}
				case 5: {
					name = "RNO 90";
					uInt mine[] = {
							15, 16, 22, 23, 29, 30
					};
					expec.insert(mine, mine+6);
					break;
				}
				default:
					throw AipsError("bad fieldID");
				}
				AlwaysAssert(md.getScansForField(name) == expec, AipsError);
			}
		}
		{
			cout << "*** test getFieldsForScan() and getFieldsForScans()" << endl;
			std::set<uInt> scans = md.getScanNumbers();
			std::set<uInt> expec2;
			std::set<uInt> curScanSet;
			for (
					std::set<uInt>::const_iterator curScan=scans.begin();
					curScan!=scans.end(); curScan++
			) {
				std::set<uInt> expec;
				curScanSet.insert(*curScan);
				if (*curScan<=4) {
					expec.insert(0);
					expec2.insert(0);
				}
				else if (*curScan==5) {
					expec.insert(1);
					expec2.insert(1);
				}
				else if (*curScan<=7) {
					expec.insert(2);
					expec2.insert(2);
				}
				else if (
						*curScan<=10 || *curScan==13
						|| *curScan==14 || *curScan==17
						|| *curScan==18 || *curScan==21
						|| *curScan==24 || *curScan==25
						|| *curScan==28 || *curScan==31
						|| *curScan==32
				) {
					expec.insert(3);
					expec2.insert(3);
				}
				else if (
						*curScan==11 || *curScan==12
						|| *curScan==19 || *curScan==20
						|| *curScan==26 || *curScan==27
				) {
					expec.insert(4);
					expec2.insert(4);
				}
				else {
					expec.insert(5);
					expec2.insert(5);
				}
				AlwaysAssert(
						md.getFieldsForScan(*curScan) == expec,
						AipsError
				);
				AlwaysAssert(
						md.getFieldsForScans(curScanSet) == expec2,
						AipsError
				);
			}
		}
		{
			cout << "*** test getFieldsForIntent()" << endl;
			std::set<String> intents = md.getIntents();
			for (
					std::set<String>::const_iterator intent=intents.begin();
					intent!=intents.end(); intent++
			) {
				std::set<uInt> expec;
				if (
						*intent=="CALIBRATE_AMPLI#ON_SOURCE"
				) {
					expec.insert(2);
				}
				else if (
						*intent=="CALIBRATE_BANDPASS#ON_SOURCE"
								|| *intent=="CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE"
										|| *intent=="CALIBRATE_SIDEBAND_RATIO#ON_SOURCE"
				) {
					expec.insert(0);
				}
				else if (
						*intent=="CALIBRATE_ATMOSPHERE#OFF_SOURCE"
								|| *intent=="CALIBRATE_ATMOSPHERE#ON_SOURCE"
										|| *intent=="CALIBRATE_WVR#OFF_SOURCE"
				) {
					uInt mine[] = {0, 2, 3, 4, 5};
					expec.insert(mine, mine+5);
				}
				else if (
						*intent=="CALIBRATE_PHASE#ON_SOURCE"
				) {
					uInt mine[] = {0, 2, 3};
					expec.insert(mine, mine+3);
				}
				else if (
						*intent=="CALIBRATE_POINTING#ON_SOURCE"
				) {
					uInt mine[] = {0, 1, 3};
					expec.insert(mine, mine+3);
				}
				else if (*intent=="CALIBRATE_WVR#ON_SOURCE") {
					uInt mine[] = {0, 1, 2, 3, 4, 5};
					expec.insert(mine, mine+6);
				}
				else {
					uInt mine[] = {4, 5};
					expec.insert(mine, mine+2);
				}
				AlwaysAssert(
						md.getFieldsForIntent(*intent) == expec,
						AipsError
				);
			}
		}
		{
			cout << "*** test getFieldNamesForFieldIDs()" << endl;
			for (uInt i=0; i<md.nFields(); i++) {
				String name;
				switch(i) {
				case 0:
					name = "3C279";
					break;
				case 1:
					name = "J1337-129";
					break;
				case 2:
					name = "Titan";
					break;
				case 3:
					name = "J1625-254";
					break;
				case 4:
					name = "V866 Sco";
					break;
				case 5:
					name = "RNO 90";
					break ;
				default:
					throw AipsError("Unknown field ID");
				}
				String got = md.getFieldNamesForFieldIDs(vector<uInt>(1, i))[0];
				cout << "*** expec " << name << " got " << got << endl;
				AlwaysAssert(
					got == name,
					AipsError
				);
			}
		}
		{
			cout << "*** test getFieldsForTime()" << endl;
			std::set<uInt> expec;
			expec.insert(0);
			AlwaysAssert(md.getFieldsForTimes(4842824746.0, 10) == expec, AipsError);
			uInt mine[] = {1, 2, 3, 4, 5};
			expec.insert(mine, mine+5);
			AlwaysAssert(
					md.getFieldsForTimes(4842824746.0, 10000) == expec,
					AipsError
			);
		}
		{
			cout << "*** test getTimesForField()" << endl;
			uInt nfields = md.nFields();
			for (uInt i=0; i< nfields; i++) {
				std::set<Double> times = md.getTimesForField(i);
				uInt expec = i == 0 ? 818
						: i == 1 ? 81
								: i == 2 ? 248
										: i == 3 ? 402
												: i == 4 ? 963
														: i == 5 ? 965
																: 0;
				AlwaysAssert(md.getTimesForField(i).size() == expec, AipsError);
			}
		}
		{
			cout << "*** test getObservatoryNames()" << endl;
			vector<String> names = md.getObservatoryNames();
			AlwaysAssert(names.size() == 1, AipsError);
			AlwaysAssert(names[0] == "ALMA", AipsError);
		}
		{
			cout << "*** test getObservatoryPosition()" << endl;
			MPosition tPos = md.getObservatoryPosition(0);
			Vector<Double> angles = tPos.getAngle("deg").getValue();
			AlwaysAssert(near(angles[0], -67.7549, 1e-6), AipsError);
			AlwaysAssert(near(angles[1], -23.0229, 1e-6), AipsError);
		}
		{
			cout << "*** getAntennaPosition()" << endl;
			cout
				<< Vector<MPosition>(
					md.getAntennaPositions(vector<uInt>(1, 2))
				)
				<< endl;
		}
		{
			cout << "*** getAntennaOffset()" << endl;
			cout << md.getAntennaOffset(2) << endl;
		}
	}
}

int main() {
    try {
    	String *parts = new String[2];
    	split(EnvironmentVariable::get("CASAPATH"), parts, 2, String(" "));
    	String datadir = parts[0] + "/data/";
    	casa::MeasurementSet ms(datadir + "regression/unittest/MSMetaData/MSMetaData.ms");
    	cout << "*** test non-on-demand constructor" << endl;
    	MSMetaDataPreload md(ms);
    	testIt(md);

    	cout << "*** test on-demand constructor" << endl;
    	MSMetaDataOnDemand md1(ms);
    	testIt(md1);
    	cout << "OK" << endl;
    } 
    catch (const AipsError& x) {
    	cerr << "Exception : " << x.getMesg() << endl;
    	return 1;
    }
    return 0;
}
