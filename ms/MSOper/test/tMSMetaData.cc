//# tMSMetaData.cc: This program tests the MSMetaData class
//# Copyright (C) 2013
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

#include <casacore/casa/aips.h>

#include <casacore/ms/MSOper/MSMetaData.h>

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/StdLogical.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/OS/Directory.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/ms/MSOper/MSKeys.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/measures/Measures/MDirection.h>

#include <casacore/casa/BasicSL/STLIO.h>
#include <iomanip>

#include <casacore/casa/namespace.h>

void _printSet(const std::set<uint32_t>& set) {
    const std::set<uint32_t>::const_iterator end = set.end();
    for (
        std::set<uint32_t>::const_iterator iter = set.begin();
        iter!=end; ++iter
    ) {
        if (iter!=set.begin()) {
            cout << ", ";
        }
        cout << *iter;
    }
    cout << endl;
}

void _printSet(const std::set<String>& set) {
    const std::set<String>::const_iterator end = set.end();
    for (
        std::set<String>::const_iterator iter = set.begin();
        iter!=end; ++iter
    ) {
        if (iter!=set.begin()) {
            cout << ", ";
        }
        cout << *iter;
    }
    cout << endl;
}

void testIt(MSMetaData& md) {
    ArrayKey arrayKey;
    arrayKey.obsID = 0;
    arrayKey.arrayID = 0;
    cout << "*** test nStates()" << endl;
    AlwaysAssert(md.nStates() == 43, AipsError);
    cout << "*** cache size " << md.getCache() << endl;

    cout << "*** test getScansForState()" << endl;
    for (uint32_t stateID=0; stateID<md.nStates(); ++stateID) {
        std::set<int32_t> scans = md.getScansForState(stateID, 0, 0);
        std::set<int32_t> expec;
        if (stateID < 5) {
            uint32_t myints[]= {1, 5, 8};
            expec.insert(myints, myints + 3);
        }
        else if (stateID < 7) {
            expec.insert(2);
        }
        else if (stateID < 10) {
            uint32_t myints[]= {3, 6, 9, 11, 13, 15, 17, 19, 22, 24, 26, 29, 31};
            expec.insert(myints, myints + 13);
        }
        else if (stateID < 26) {
            expec.insert(4);
        }
        else if (stateID < 32) {
            expec.insert(7);
        }
        else if (stateID < 33) {
            uint32_t myints[] = {10, 14, 18, 21, 25, 28, 32};
            expec.insert(myints, myints + 7);
        }
        else if (stateID < 37) {
            uint32_t myints[] = {12, 16, 20, 23, 27, 30};
            expec.insert(myints, myints + 6);
        }
        else {
            uint32_t myints[] = {12, 16, 20, 23};
            expec.insert(myints, myints + 4);
        }
        AlwaysAssert(scans == expec, AipsError);
    }
    cout << "*** cache size " << md.getCache() << endl;

    cout << "*** test getIntents()" << endl;
    cout << "*** size " << md.getIntents().size() << endl;
    cout << "*** size " << md.getIntents().size() << endl;

    AlwaysAssert(md.getIntents().size() == 11, AipsError);
    cout << "*** cache size " << md.getCache() << endl;

    cout << "*** test getScanNumbers()" << endl;
    std::set<int32_t> scans = md.getScanNumbers(0, 0);
    uint32_t myints[] = {
            1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
            18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32
    };
    {
        std::set<int32_t> exp;
        exp.insert(myints, myints+32);
        AlwaysAssert(scans == exp, AipsError);
        cout << "*** cache size " << md.getCache() << endl;
    }
    std::set<String> uniqueIntents;
    cout << "*** test getIntentsForScan()" << endl;
    ScanKey scanKey;
    scanKey.obsID = 0;
    scanKey.arrayID = 0;
    for (
        std::set<int32_t>::const_iterator scanNum = scans.begin();
        scanNum!=scans.end(); ++scanNum
    ) {
        scanKey.scan = *scanNum;
        std::set<String> intents = md.getIntentsForScan(scanKey);
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
    AlwaysAssert(md.getIntents() == uniqueIntents, AipsError);
    cout << "*** test getSpwsForIntent()" << endl;
    for (
        auto intent=uniqueIntents.cbegin();
        intent!=uniqueIntents.cend(); ++intent
    ) {
        std::set<uint32_t> exp;
        if (
            *intent == "CALIBRATE_AMPLI#ON_SOURCE"
            || *intent == "CALIBRATE_BANDPASS#ON_SOURCE"
            || *intent == "CALIBRATE_PHASE#ON_SOURCE"
            || *intent == "OBSERVE_TARGET#ON_SOURCE"
        ) {
            uint32_t myints[] = {0, 17, 18, 19, 20, 21, 22, 23, 24};
            exp.insert(myints, myints+9);
        }
        else if (
            *intent == "CALIBRATE_ATMOSPHERE#OFF_SOURCE"
            || *intent == "CALIBRATE_ATMOSPHERE#ON_SOURCE"
            || *intent == "CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE"
            || *intent == "CALIBRATE_SIDEBAND_RATIO#ON_SOURCE"
            || *intent == "CALIBRATE_WVR#OFF_SOURCE"
        ) {
            uint32_t myints[] = {0, 9, 10, 11, 12, 13, 14, 15, 16};
            exp.insert(myints, myints+9);
        }
        else if (
                *intent == "CALIBRATE_POINTING#ON_SOURCE"
        ) {
            uint32_t myints[] = {0, 1, 2, 3, 4, 5, 6, 7, 8};
            exp.insert(myints, myints+9);
        }
        else if (
                *intent == "CALIBRATE_WVR#ON_SOURCE"
        ) {
            uint32_t myints[] = {
                0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                10, 11, 12, 13, 14, 15, 16,
                17, 18, 19, 20, 21, 22, 23, 24
            };
            exp.insert(myints, myints+25);
        }
        AlwaysAssert(md.getSpwsForIntent(*intent) == exp, AipsError);
    }
    cout << "*** test nSpw()" << endl;
    uint32_t nSpw = md.nSpw(true);
    AlwaysAssert(nSpw == 40, AipsError);
    AlwaysAssert(md.nSpw(false) == 40, AipsError);
    cout << "*** test getIntentsForSpw()" << endl;
    for (uint32_t spw=0; spw<nSpw; ++spw) {
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
        uint32_t nFields = md.nFields();
        AlwaysAssert(nFields == 6, AipsError);
        cout << "*** test getSpwsForField() and getFieldsToSpwsMap()" << endl;
        std::map<int32_t, std::set<uint32_t> > mymap = md.getFieldsToSpwsMap();
        String names[] = {
                "3C279", "J1337-129", "Titan",
                "J1625-254", "V866 Sco", "RNO 90"
        };
        for (uint32_t i=0; i<nFields; ++i) {
            std::set<uint32_t> exp;
            if (i==0 || i==3) {
                uint32_t myints[] = {
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                    10, 11, 12, 13, 14, 15, 16, 17,
                    18, 19, 20, 21, 22, 23, 24
                };
                exp.insert(myints, myints+25);
            }
            if (i == 1) {
                uint32_t myints[] = {
                    0, 1, 2, 3, 4, 5, 6, 7, 8
                };
                exp.insert(myints, myints+9);
            }
            if (i==2 || i==4 || i==5) {
                uint32_t myints[] = {
                    0, 9, 10, 11, 12, 13, 14, 15, 16,
                    17, 18, 19, 20, 21, 22, 23, 24
                };
                exp.insert(myints, myints+17);
            }
            cout << "*** i " << i << " " << md.getSpwsForField(i) << endl;
            AlwaysAssert(md.getSpwsForField(i) == exp, AipsError);
            AlwaysAssert(md.getSpwsForField(names[i]) == exp, AipsError);
            AlwaysAssert(mymap[i] == exp, AipsError);
            cout << "*** cache size " << md.getCache() << endl;

        }
        cout << "*** test phaseDirFromFieldIDAndTime()" << endl;
        {

          MDirection phasCen=md.phaseDirFromFieldIDAndTime(2);
          AlwaysAssert(
                near(phasCen.getAngle().getValue()[0], -2.72554329 , 5e-7),
                AipsError
            );
        }
        cout << "*** test getFieldIDsForSpw()" << endl;
        for (uint32_t i=0; i<md.nSpw(true); ++i) {
            std::set<int32_t> exp;
            std::set<String> expNames;
            if (i==0) {
                uint32_t myints[] = {0, 1, 2, 3, 4, 5};
                exp.insert(myints, myints+6);
                String mystr[] = {
                    "3C279", "J1337-129", "Titan", "J1625-254",
                    "V866 Sco", "RNO 90"
                };
                expNames.insert(mystr, mystr+6);

            }
            else if (i<9) {
                uint32_t myints[] = {0, 1, 3};
                exp.insert(myints, myints+3);
                String mystr[] = {
                    "3C279", "J1337-129", "J1625-254"
                };
                expNames.insert(mystr, mystr+3);
            }
            else if (i<25) {
                uint32_t myints[] = {0, 2, 3, 4, 5};
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
        cout << "nscans " << md.nScans() << endl;
        AlwaysAssert(md.nScans() == 32, AipsError);
        std::set<int32_t> scanNumbers = md.getScanNumbers(0, 0);
        cout << "*** test getSpwsForScan(), getScanToSpwsMap(), getPolarizationIDs()" << endl;
        std::map<ScanKey, std::set<uint32_t> > mymap = md.getScanToSpwsMap();
        ScanKey scanKey;
        scanKey.obsID = 0;
        scanKey.arrayID = 0;
        for (
            std::set<int32_t>::const_iterator scan=scanNumbers.begin();
            scan!=scanNumbers.end(); ++scan
        ) {
            std::set<uint32_t> exp;
            if (*scan == 1 || *scan==5 || *scan==8) {
                uint32_t myints[] = {
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
                uint32_t myints[] = {
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
                uint32_t myints[] = {
                    0, 17, 18, 19, 20, 21, 22, 23, 24
                };
                exp.insert(myints, myints+9);
            }
            scanKey.scan = *scan;
            AlwaysAssert(md.getSpwsForScan(scanKey) == exp, AipsError);
            AlwaysAssert(mymap[scanKey] == exp, AipsError);
            for (
                std::set<uint32_t>::const_iterator spw=exp.begin();
                spw!=exp.end(); ++spw
            ) {
                std::set<uint32_t> exppols;
                std::set<uint32_t> pols = md.getPolarizationIDs(0, 0, *scan, *spw);
                if (*spw == 0) {
                    exppols.insert(1);
                }
                else {
                    exppols.insert(0);
                }
                AlwaysAssert(pols == exppols, AipsError);
            }
        }
        {
            cout << "*** test getScansForSpw() and getSpwToScansMap()" << endl;
            vector<std::set<ScanKey> > spwToScans = md.getSpwToScansMap();
            ScanKey scanKey;
            scanKey.obsID = 0;
            scanKey.arrayID = 0;
            for (uint32_t i=0; i<md.nSpw(true); ++i) {
                std::set<int32_t> exp;
                if (i==0) {
                    int32_t myints[] = {
                        1,  2,  3,  4,  5,  6,  7,  8,  9,
                        10, 11, 12, 13, 14, 15, 16, 17, 18,
                        19, 20, 21, 22, 23, 24, 25, 26, 27,
                        28, 29, 30, 31, 32
                    };
                    exp.insert(myints, myints+32);
                }
                else if (i<9) {
                    int32_t myints[] = {1, 5, 8};
                    exp.insert(myints, myints+3);
                }
                else if (i<17) {
                    int32_t myints[] = {
                        2,  3,  6,  9, 11, 13, 15,
                        17, 19, 22, 24, 26, 29, 31
                    };
                    exp.insert(myints, myints+14);
                }
                else if (i<25) {
                    int32_t myints[] = {
                        4,  7, 10, 12, 14, 16, 18,
                        20, 21, 23, 25, 27, 28, 30, 32
                    };
                    exp.insert(myints, myints+15);
                }
                else {
                    // empty set
                }
                AlwaysAssert(md.getScansForSpw(i, 0, 0) == exp, AipsError);
                std::set<int32_t>::const_iterator iter = exp.begin();
                std::set<int32_t>::const_iterator end = exp.end();
                std::set<ScanKey> expSet;
                for (; iter!=end; ++iter) {
                    scanKey.scan = *iter;
                    expSet.insert(scanKey);
                }
                AlwaysAssert(spwToScans[i] == expSet, AipsError);
            }
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
            for (uint32_t i=0; i<md.nAntennas(); ++i) {
                vector<uint32_t> ids(1);
                ids[0] = i;
                std::map<String, std::set<uint32_t> > mymap;
                AlwaysAssert(
                    md.getAntennaNames(mymap, ids)[0] == expnames[i],
                    AipsError
                );
            }
            cout << "*** test getAntennaID()" << endl;
            std::map<String, std::set<uint32_t> > mymap;
            for (uint32_t i=0; i<md.nAntennas(); ++i) {
                vector<uint32_t> ids(1);
                ids[0] = i;
                AlwaysAssert(
                        *md.getAntennaIDs(md.getAntennaNames(mymap, ids))[0].begin()==i,
                        AipsError
                );
            }
        }
        {
            cout << "*** test getTDMSpw()" << endl;
            std::set<uint32_t> exp;
            uint32_t myints[] = {
                0, 1, 3, 5, 7, 9, 11, 13, 15, 25, 26, 27, 28, 29,
                30, 31, 32, 33, 34, 35, 36, 37, 38, 39
            };
            exp.insert(myints, myints+24);
            AlwaysAssert(md.getTDMSpw() == exp, AipsError);
        }
        {
            cout << "*** test getFDMSpw()" << endl;
            std::set<uint32_t> exp;
            uint32_t myints[] = {17, 19, 21, 23};
            exp.insert(myints, myints+4);
            AlwaysAssert(md.getFDMSpw() == exp, AipsError);
        }
        {
            cout << "*** test getChannelAvgSpw()" << endl;
            std::set<uint32_t> exp;
            uint32_t myints[] = {
                2, 4, 6, 8, 10, 12, 14,
                16, 18, 20, 22, 24
            };
            exp.insert(myints, myints+12);
            AlwaysAssert(md.getChannelAvgSpw() == exp, AipsError);
        }
        {
            cout << "*** test getWVRSpw()" << endl;
            std::set<uint32_t> exp;
            /*
            uint32_t myints[] = {
                0, 25, 26, 27, 28, 29, 30, 31,
                32, 33, 34, 35, 36, 37, 38, 39
            };
            exp.insert(myints, myints+16);
            cout << "wvr " << md.getWVRSpw() << endl;
            */
            AlwaysAssert(md.getWVRSpw() == exp, AipsError);
        }
        {
            cout << "*** test getScansForTimes()" << endl;
            std::set<int32_t> exp;
            exp.insert(27);
            AlwaysAssert(
                md.getScansForTimes(4.84282937e+09, 20, 0, 0) == exp,
                AipsError
            );
            exp.insert(24);
            exp.insert(25);
            exp.insert(26);
            exp.insert(28);
            AlwaysAssert(
                md.getScansForTimes(4.84282937e+09, 200, 0, 0) == exp,
                AipsError
            );
        }
        {
            cout << "*** test getTimesForScans()" << endl;
            std::set<double> expec;
            double myd[] = {
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
            std::set<int32_t> myscans;
            myscans.insert(3);
            myscans.insert(6);
            AlwaysAssert(
                allNearAbs(md.getTimesForScans(scanKeys(myscans, arrayKey)), expec, 0.1),
                AipsError
            );
        }
        {
            cout << "*** test getTimesForScan()" << endl;
            std::set<double> expec;
            double myd[] = {
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
            std::set<int32_t> myscans;
            myscans.insert(3);
            AlwaysAssert(
                allNearAbs(
                    md.getTimesForScans(scanKeys(myscans, arrayKey)),
                    expec, 0.1
                ),
                AipsError
            );
        }
        {
            cout << "*** test getStatesForScan() getScanToStatesMap()" << endl;
            std::set<int32_t> expec;
            std::set<int32_t> scanNumbers = md.getScanNumbers(0, 0);
            std::map<ScanKey, std::set<int32_t> > mymap = md.getScanToStatesMap();
            AlwaysAssert(scanNumbers.size() == mymap.size(), AipsError);
            ScanKey scanKey;
            scanKey.scan = 0;
            scanKey.arrayID = 0;
            scanKey.obsID = 0;
            for (
                std::set<int32_t>::const_iterator curScan=scanNumbers.begin();
                curScan!=scanNumbers.end(); ++curScan
            ) {
                expec.clear();
                if (*curScan == 1 || *curScan == 5 || *curScan == 8) {
                    int32_t mine[] = {0, 1, 2, 3, 4};
                    expec.insert(mine, mine+5);
                }
                else if (*curScan == 2) {
                    int32_t mine[] = {5, 6};
                    expec.insert(mine, mine+2);
                }
                else if (
                    *curScan == 3 || *curScan==6 || *curScan==9
                    || *curScan==11 || *curScan==13 || *curScan==15
                    || *curScan==17 || *curScan==19 || *curScan==22
                    || *curScan==24 || *curScan==26 || *curScan==29
                    || *curScan==31
                ) {
                    int32_t mine[] = {7, 8, 9};
                    expec.insert(mine, mine+3);
                }
                else if (*curScan==4) {
                    int32_t mine[] = {
                        10, 11, 12, 13, 14, 15, 16, 17, 18,
                        19, 20, 21, 22, 23, 24, 25
                    };
                    expec.insert(mine, mine+16);
                }
                else if (*curScan==7) {
                    int32_t mine[] = {26, 27, 28, 29, 30, 31};
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
                    int32_t mine[] = {
                        33, 34, 35, 36, 37, 38, 39, 40, 41, 42
                    };
                    expec.insert(mine, mine+10);
                }
                else {
                    int32_t mine[] = {33, 34, 35, 36};
                    expec.insert(mine, mine+4);
                }
                std::set<int32_t> got = md.getStatesForScan(0, 0, *curScan);
                AlwaysAssert(got == expec, AipsError);
                scanKey.scan = *curScan;
                AlwaysAssert(mymap[scanKey] == expec, AipsError);
            }
            cout << "*** cache size " << md.getCache() << endl;
        }
        {
            cout << "*** test getScansForIntent()" << endl;
            std::set<String> intents = md.getIntents();
            for (
                std::set<String>::const_iterator intent=intents.begin();
                intent!=intents.end(); ++intent
            ) {
                std::set<int32_t> expec;
                if (*intent=="CALIBRATE_AMPLI#ON_SOURCE") {
                    expec.insert(7);
                }
                else if (
                    *intent=="CALIBRATE_ATMOSPHERE#OFF_SOURCE"
                    || *intent=="CALIBRATE_ATMOSPHERE#ON_SOURCE"
                ) {
                    int32_t mine[] = {
                        3, 6, 9, 11, 13, 15, 17,
                        19, 22, 24, 26, 29, 31
                    };
                    expec.insert(mine, mine+13);
                }
                else if (*intent=="CALIBRATE_BANDPASS#ON_SOURCE") {
                    expec.insert(4);
                }
                else if (*intent=="CALIBRATE_PHASE#ON_SOURCE") {
                    int32_t mine[] = {
                        4, 7, 10, 14, 18, 21, 25, 28, 32
                    };
                    expec.insert(mine, mine+9);
                }
                else if (*intent=="CALIBRATE_POINTING#ON_SOURCE") {
                    int32_t mine[] = {1, 5, 8};
                    expec.insert(mine, mine+3);
                }
                else if (
                    *intent=="CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE"
                    || *intent=="CALIBRATE_SIDEBAND_RATIO#ON_SOURCE"
                ) {
                    expec.insert(2);
                }
                else if (*intent=="CALIBRATE_WVR#OFF_SOURCE") {
                    int32_t mine[] = {
                        2, 3, 6, 9, 11, 13, 15, 17,
                        19, 22, 24, 26, 29, 31
                    };
                    expec.insert(mine, mine+14);
                }
                else if (*intent=="CALIBRATE_WVR#ON_SOURCE") {
                    int32_t mine[] = {
                        1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                        11, 13, 14, 15, 17, 18, 19, 21,
                        22, 24, 25, 26, 28, 29, 31, 32
                    };
                    expec.insert(mine, mine+26);
                }
                else {
                    int32_t mine[] = {12, 16, 20, 23, 27, 30};
                    expec.insert(mine, mine+6);
                }
                AlwaysAssert(md.getScansForIntent(*intent, 0, 0) == expec, AipsError);
                AlwaysAssert(
                    casacore::scanNumbers(md.getIntentToScansMap()[*intent]) == expec,
                    AipsError
                );
            }
        }
        {
            cout << "*** test getScansForFieldID() and getFieldToScansMap" << endl;
            vector<std::set<ScanKey> > mymap = md.getFieldToScansMap();
            std::set<int32_t> expec;
            ArrayKey aKey;
            aKey.arrayID = 0;
            aKey.obsID = 0;
            for (uint32_t i=0; i<6; ++i) {
                expec.clear();
                switch(i) {
                case 0:
                {
                    int32_t mine[] = {1, 2, 3, 4};
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
                    int32_t mine[] = {
                        8, 9, 10, 13, 14, 17, 18,
                        21, 24, 25, 28, 31, 32
                    };
                    expec.insert(mine, mine+13);
                    break;
                }
                case 4: {
                    int32_t mine[] = {11, 12, 19, 20, 26, 27};
                    expec.insert(mine, mine+6);
                    break;
                }
                case 5: {
                    int32_t mine[] = {15, 16, 22, 23, 29, 30};
                    expec.insert(mine, mine+6);
                    break;
                }
                default:
                    throw AipsError("bad fieldID");
                }
                AlwaysAssert(md.getScansForFieldID(i, 0, 0) == expec, AipsError);
                AlwaysAssert(mymap[i] == scanKeys(expec, aKey), AipsError);
            }
        }
        {
            cout << "*** test getFieldIDsForField()" << endl;
            for (uint32_t i=0; i<6; ++i) {
                std::set<int32_t> expec;
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
            for (uint32_t i=0; i<6; ++i) {
                std::set<int32_t> expec;
                String name;
                switch(i) {
                case 0:
                {
                    name = "3C279";
                    uint32_t mine[] = {1, 2, 3, 4};
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
                    int32_t mine[] = {
                        8, 9, 10, 13, 14, 17, 18,
                        21, 24, 25, 28, 31, 32
                    };
                    expec.insert(mine, mine+13);
                    break;
                }
                case 4: {
                    name = "V866 Sco";
                    int32_t mine[] = {
                        11, 12, 19, 20, 26, 27
                    };
                    expec.insert(mine, mine+6);
                    break;
                }
                case 5: {
                    name = "RNO 90";
                    int32_t mine[] = {
                        15, 16, 22, 23, 29, 30
                    };
                    expec.insert(mine, mine+6);
                    break;
                }
                default:
                    throw AipsError("bad fieldID");
                }
                AlwaysAssert(md.getScansForField(name, 0, 0) == expec, AipsError);
            }
            cout << "*** cache size " << md.getCache() << endl;
        }
        {
            cout << "*** test getFieldsForScan() and getFieldsForScans()" << endl;
            std::set<int32_t> scans = md.getScanNumbers(0, 0);
            std::set<int32_t> expec2;
            std::set<int32_t> curScanSet;
            for (
                std::set<int32_t>::const_iterator curScan=scans.begin();
                curScan!=scans.end(); ++curScan
            ) {
                std::set<int32_t> expec;
                curScanSet.insert(*curScan);
                if (*curScan <= 4) {
                    expec.insert(0);
                    expec2.insert(0);
                }
                else if (*curScan == 5) {
                    expec.insert(1);
                    expec2.insert(1);
                }
                else if (*curScan <= 7) {
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
                ScanKey scanKey;
                scanKey.obsID = 0;
                scanKey.arrayID = 0;
                scanKey.scan = *curScan;
                AlwaysAssert(
                    md.getFieldsForScan(scanKey) == expec,
                    AipsError
                );
                AlwaysAssert(
                    md.getFieldsForScans(curScanSet, 0, 0) == expec2,
                    AipsError
                );
            }
            std::set<int32_t> expec3;
            expec3.insert(3);
            expec3.insert(4);
            std::set<ScanKey> scanKeys;
            ScanKey x;
            x.obsID = 0;
            x.arrayID = 0;
            x.scan = 19;
            scanKeys.insert(x);
            x.scan = 31;
            scanKeys.insert(x);
            AlwaysAssert(
                md.getFieldsForScans(scanKeys) == expec3,
                AipsError
            );
        }
        {
            cout << "*** test getFieldsForIntent() and getIntentToFieldsMap()" << endl;
            std::map<String, std::set<int32_t> > mymap = md.getIntentToFieldsMap();
            std::set<String> intents = md.getIntents();
            for (
                std::set<String>::const_iterator intent=intents.begin();
                intent!=intents.end(); ++intent
            ) {
                std::set<int32_t> expec;
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
                    int32_t mine[] = {0, 2, 3, 4, 5};
                    expec.insert(mine, mine+5);
                }
                else if (
                    *intent=="CALIBRATE_PHASE#ON_SOURCE"
                ) {
                    int32_t mine[] = {0, 2, 3};
                    expec.insert(mine, mine+3);
                }
                else if (
                    *intent=="CALIBRATE_POINTING#ON_SOURCE"
                ) {
                    int32_t mine[] = {0, 1, 3};
                    expec.insert(mine, mine+3);
                }
                else if (*intent=="CALIBRATE_WVR#ON_SOURCE") {
                    int32_t mine[] = {0, 1, 2, 3, 4, 5};
                    expec.insert(mine, mine+6);
                }
                else {
                    int32_t mine[] = {4, 5};
                    expec.insert(mine, mine+2);
                }
                AlwaysAssert(
                    md.getFieldsForIntent(*intent) == expec,
                    AipsError
                );
                AlwaysAssert(mymap[*intent] == expec, AipsError);
            }
        }
        {
            cout << "*** test getFieldNamesForFieldIDs()" << endl;
            for (uint32_t i=0; i<md.nFields(); ++i) {
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
                String got = md.getFieldNamesForFieldIDs(vector<uint32_t>(1, i))[0];
                cout << "*** expec " << name << " got " << got << endl;
                AlwaysAssert(
                    got == name,
                    AipsError
                );
            }
            cout << "*** cache size " << md.getCache() << endl;
        }
        {
            cout << "*** test getFieldsForTime()" << endl;
            std::set<int32_t> expec;
            expec.insert(0);
            AlwaysAssert(md.getFieldsForTimes(4842824746.0, 10) == expec, AipsError);
            uint32_t mine[] = {1, 2, 3, 4, 5};
            expec.insert(mine, mine+5);
            AlwaysAssert(
                md.getFieldsForTimes(4842824746.0, 10000) == expec,
                AipsError
            );
        }
        {
            cout << "*** test getTimesForField()" << endl;
            uint32_t nfields = md.nFields();
            for (uint32_t i=0; i< nfields; ++i) {
                std::set<double> times = md.getTimesForField(i);
                uint32_t expec = i == 0 ? 818
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
            Vector<double> angles = tPos.getAngle("deg").getValue();
            cout << angles << endl;
            AlwaysAssert(near(angles[0], -67.7549, 1e-6), AipsError);
            AlwaysAssert(near(angles[1], -23.0229, 1e-6), AipsError);
            cout << "*** cache size " << md.getCache() << endl;

        }
        {
            cout << "*** test getAntennaPosition()" << endl;
            cout
                << Vector<MPosition>(
                    md.getAntennaPositions(vector<uint32_t>(1, 2))
                )
                << endl;
        }
        {
            cout << "*** test getAntennaOffset()" << endl;
            cout << md.getAntennaOffset(2) << endl;
        }
        {
            cout << "*** test getAntennaStations()" << endl;
            vector<uint32_t> ids(3);
            ids[0] = 2;
            ids[1] = 4;
            ids[2] = 3;
            vector<String> stations = md.getAntennaStations(ids);
            AlwaysAssert(
                stations[0] == "A077" && stations[1] == "A082"
                && stations[2] == "A137", AipsError
            );
            vector<String> names(3);
            names[0] = "DV02";
            names[1] = "DV05";
            names[2] = "DV03";
            vector<vector<String> > stationsByName = md.getAntennaStations(names);
            AlwaysAssert(
                stationsByName[0][0] == "A077" && stationsByName[1][0] == "A082"
                && stationsByName[2][0] == "A137", AipsError
            );
        }
        {
            cout << "*** test getAntennaDiameters" << endl;
            Quantum<Vector<double> > antennaDiameters = md.getAntennaDiameters();
            AlwaysAssert(
                allEQ(antennaDiameters.getValue(), 12.0), AipsError
            );
        }
        {
            cout << "*** Test getIntentsForField()" << endl;
            uint32_t nFields = md.nFields();
            const auto fieldNames = md.getFieldNames();
            for (uint32_t i=0; i<nFields; ++i) {
                std::set<String> expec;
                switch (i) {
                case 0: {
                    String mine[] = {
                        "CALIBRATE_ATMOSPHERE#OFF_SOURCE", "CALIBRATE_ATMOSPHERE#ON_SOURCE",
                        "CALIBRATE_BANDPASS#ON_SOURCE", "CALIBRATE_PHASE#ON_SOURCE",
                        "CALIBRATE_POINTING#ON_SOURCE", "CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE",
                        "CALIBRATE_SIDEBAND_RATIO#ON_SOURCE", "CALIBRATE_WVR#OFF_SOURCE",
                        "CALIBRATE_WVR#ON_SOURCE"
                    };
                    expec.insert(mine, mine+9);
                    break;
                }
                case 1: {
                    String mine[] = {
                        "CALIBRATE_POINTING#ON_SOURCE", "CALIBRATE_WVR#ON_SOURCE"
                    };
                    expec.insert(mine, mine+2);
                    break;
                }
                case 2: {
                    String mine[] = {
                        "CALIBRATE_AMPLI#ON_SOURCE", "CALIBRATE_ATMOSPHERE#OFF_SOURCE",
                        "CALIBRATE_ATMOSPHERE#ON_SOURCE", "CALIBRATE_PHASE#ON_SOURCE",
                        "CALIBRATE_WVR#OFF_SOURCE", "CALIBRATE_WVR#ON_SOURCE"
                    };
                    expec.insert(mine, mine+6);
                    break;
                }
                case 3: {
                    String mine[] = {
                        "CALIBRATE_ATMOSPHERE#OFF_SOURCE", "CALIBRATE_ATMOSPHERE#ON_SOURCE",
                        "CALIBRATE_PHASE#ON_SOURCE", "CALIBRATE_POINTING#ON_SOURCE",
                        "CALIBRATE_WVR#OFF_SOURCE", "CALIBRATE_WVR#ON_SOURCE"
                    };
                    expec.insert(mine, mine+6);
                    break;
                }
                case 4: {
                    String mine[] = {
                        "CALIBRATE_ATMOSPHERE#OFF_SOURCE", "CALIBRATE_ATMOSPHERE#ON_SOURCE",
                        "CALIBRATE_WVR#OFF_SOURCE", "CALIBRATE_WVR#ON_SOURCE",
                        "OBSERVE_TARGET#ON_SOURCE"
                    };
                    expec.insert(mine, mine+5);
                    break;
                }
                case 5: {
                    String mine[] = {
                        "CALIBRATE_ATMOSPHERE#OFF_SOURCE", "CALIBRATE_ATMOSPHERE#ON_SOURCE",
                        "CALIBRATE_WVR#OFF_SOURCE", "CALIBRATE_WVR#ON_SOURCE",
                        "OBSERVE_TARGET#ON_SOURCE"
                    };
                    expec.insert(mine, mine+5);
                    break;
                }
                default:
                    break;
                }
                cout << "*** i " << i << endl;
                _printSet(md.getIntentsForField(i));
                AlwaysAssert(md.getIntentsForField(i) == expec, AipsError);
                AlwaysAssert(
                    md.getIntentsForField(fieldNames[i]) == expec, AipsError
                );
            }
        }
        {
            cout << "*** test getUniqueBaselines() and nBaselines()" << endl;
            AlwaysAssert(md.nBaselines(false) == 21, AipsError);
            AlwaysAssert(md.nBaselines(true) == 25, AipsError);
        }
        {
            cout << "*** test getEffectiveTotalExposureTime()" << endl;
            cout << "effective exposure time is " << md.getEffectiveTotalExposureTime() << endl;
        }
        {
            cout << "*** test BBCNosToSpwMap()" << endl;
            for (uint32_t i=0; i<3; ++i) {
                MSMetaData::SQLDSwitch sqldSwitch = i == 0 ? MSMetaData::SQLD_INCLUDE
                    : i == 1 ? MSMetaData::SQLD_EXCLUDE : MSMetaData::SQLD_ONLY;
                std::map<uint32_t, std::set<uint32_t> > got = md.getBBCNosToSpwMap(sqldSwitch);
                std::map<uint32_t, std::set<uint32_t> >::const_iterator end = got.end();
                for (
                    std::map<uint32_t, std::set<uint32_t> >::const_iterator iter=got.begin();
                        iter!=end; ++iter
                ) {
                    std::set<uint32_t> expec;
                    switch(iter->first) {
                    case 0: {
                        if (sqldSwitch != MSMetaData::SQLD_ONLY) {
                            uint32_t mine[] = {
                                0, 25, 26, 27, 28, 29,
                                30, 31, 32, 33, 34, 35,
                                36, 37, 38, 39
                            };
                            expec.insert(mine, mine+16);
                        }
                        break;
                    }
                    case 1: {
                        if (sqldSwitch != MSMetaData::SQLD_ONLY) {
                            uint32_t mine[] = {
                                1, 2, 9, 10, 17, 18
                            };
                            expec.insert(mine, mine+6);
                        }
                        break;
                    }
                    case 2: {
                        if (sqldSwitch == MSMetaData::SQLD_INCLUDE) {
                            uint32_t mine[] = {
                                3, 4, 11, 12, 19, 20
                            };
                            expec.insert(mine, mine+6);
                        }
                        else if (sqldSwitch == MSMetaData::SQLD_EXCLUDE) {
                            uint32_t mine[] = {
                                4, 11, 12, 19, 20
                            };
                            expec.insert(mine, mine+5);
                        }
                        else {
                            // SQLD_ONLY
                            uint32_t mine[] = {3};
                            expec.insert(mine, mine+1);
                        }
                        break;
                    }
                    case 3: {
                        if (sqldSwitch != MSMetaData::SQLD_ONLY) {
                            uint32_t mine[] = {
                                5, 6, 13, 14, 21, 22
                            };
                            expec.insert(mine, mine+6);
                        }
                        break;
                    }
                    case 4: {
                        if (sqldSwitch != MSMetaData::SQLD_ONLY) {
                            uint32_t mine[] = {
                                7, 8, 15, 16, 23, 24
                            };
                            expec.insert(mine, mine+6);
                        }
                        break;
                    }
                    default:
                        throw AipsError();
                    }
                    AlwaysAssert(iter->second == expec, AipsError);
                }
            }
            {
                cout << "*** test getSpwIDPolIDToDataDescIDMap()" << endl;
                std::map<std::pair<uint32_t, uint32_t>, uint32_t> dataDescToPolID = md.getSpwIDPolIDToDataDescIDMap();
                std::map<std::pair<uint32_t, uint32_t>, uint32_t>::const_iterator iter;
                std::map<std::pair<uint32_t, uint32_t>, uint32_t>::const_iterator begin = dataDescToPolID.begin();
                std::map<std::pair<uint32_t, uint32_t>, uint32_t>::const_iterator end = dataDescToPolID.end();
                for(
                    iter=begin; iter!=end; ++iter
                ) {
                    std::pair<uint32_t, uint32_t> mypair = iter->first;
                    uint32_t spw = mypair.first;
                    uint32_t pol = mypair.second;
                    int32_t dataDesc = iter->second;
                    AlwaysAssert((int32_t)spw == dataDesc, AipsError);
                    AlwaysAssert(pol == (spw == 0 ? 1 : 0), AipsError);
                }
            }
        }
        {
            cout << "*** test nPol()" << endl;
            AlwaysAssert(md.nPol() == 2, AipsError);
        }
        {
            cout << "*** test getSQLDSpw()" << endl;
            std::set<uint32_t> res = md.getSQLDSpw();
            AlwaysAssert(res.size() == 1 && *res.begin() == 3, AipsError);
        }
        {
            cout << "*** test getFirstExposureTimeMap() (deprecated)" << endl;
            vector<std::map<int32_t, Quantity> > mymap = md.getFirstExposureTimeMap();
            cout << "val " << mymap[0][30].getValue("s") << endl;
            cout << "val " << mymap[0][30] << endl;
            AlwaysAssert(near(mymap[0][30].getValue("s"), 1.152), AipsError);
            AlwaysAssert(near(mymap[10][17].getValue("s"), 1.008), AipsError)
            cout << "mymap " << mymap[10][17] << endl;
        }
        {
            cout << "*** test getScanToFirstExposureTimeMap()" << endl;
            std::map<ScanKey, MSMetaData::FirstExposureTimeMap> mymap
                = md.getScanToFirstExposureTimeMap(false);
            ScanKey scan;
            scan.arrayID = 0;
            scan.obsID = 0;
            scan.scan = 30;
            AlwaysAssert(near(mymap[scan][0].second.getValue("s"), 1.152), AipsError);
            scan.scan = 17;
            AlwaysAssert(near(mymap[scan][10].second.getValue("s"), 1.008), AipsError)
        }
        {
            cout << "*** test getUniqueFiedIDs()" << endl;
            std::set<int32_t> expec;
            for (int32_t i=0; i<6; ++i) {
                expec.insert(i);
            }
            AlwaysAssert(md.getUniqueFieldIDs() == expec, AipsError);
        }
        {
            cout << "*** test getCenterFreqs()" << endl;
            vector<Quantity> centers = md.getCenterFreqs();
            double mine[] = {
                187550000000.0,    214250000000.0,
                214234375000.0,    216250000000.0,
                216234375000.0,    230250000000.0,
                230234375000.0,    232250000000.0,
                232234375000.0,    231471730000.0,
                231456105000.0,    233352270000.0,
                233336645000.0,    219465062500.0,
                219449437500.0,    218610562500.0,
                218594937500.0,    230534230000.0,
                230534214741.0,    232414770000.0,
                232414754741.0,    220402562500.0,
                220402547241.0,    219548062500.0,
                219548047241.0,    187550000000.0,
                187550000000.0,    187550000000.0,
                187550000000.0,    187550000000.0,
                187550000000.0,    187550000000.0,
                187550000000.0,    187550000000.0,
                187550000000.0,    187550000000.0,
                187550000000.0,    187550000000.0,
                187550000000.0,    187550000000.0
            };
            vector<double> expec(mine, mine + 39);
            for (uint32_t i=0; i<40; ++i) {
                AlwaysAssert(abs(centers[i].getValue("Hz")/mine[i] - 1) < 1e-8, AipsError);
            }
        }
        {
            cout << "*** Test getCorrBits" << endl;
            vector<String> cb = md.getCorrBits();
            for ( const auto &el : cb) {
                AlwaysAssert(el == "UNKNOWN", AipsError);
            }
        }
        {
            cout << "*** Test getFieldsForSourceMap" << endl;
            std::map<int32_t, std::set<int32_t> > res = md.getFieldsForSourceMap();
            std::map<int32_t, std::set<String> > res2 = md.getFieldNamesForSourceMap();
            String names[] = {
                "3C279", "J1337-129", "Titan", "J1625-254", "V866 Sco", "RNO 90"
            };
            AlwaysAssert(res.size() == 6, AipsError);
            AlwaysAssert(res2.size() == 6, AipsError);
            for (int32_t i=0; i<6; ++i) {
                AlwaysAssert(res[i].size() == 1 && *(res[i].begin()) == i, AipsError);
                AlwaysAssert(
                    res2[i].size() == 1 && *(res2[i].begin()) == names[i], AipsError
                );
            }
        }
        {
            cout << "*** Test getPointingDirection" << endl;
            int32_t ant1, ant2;
            double time;
            std::pair<MDirection, MDirection> pDirs = md.getPointingDirection(
                ant1, ant2, time, 500
            );
            AlwaysAssert(ant1 == 7, AipsError);
            AlwaysAssert(ant2 == 11, AipsError);
            AlwaysAssert(time == 4842824902.632, AipsError);
            AlwaysAssert(
                near(pDirs.first.getAngle().getValue()[0], -1.231522504, 2e-10),
                AipsError
            );
            AlwaysAssert(
                near(pDirs.first.getAngle().getValue()[1], 0.8713643132, 1e-9),
                AipsError
            );
            AlwaysAssert(
                near(pDirs.second.getAngle().getValue()[0], -1.231504278, 4e-10),
                AipsError
            );
            AlwaysAssert(
                near(pDirs.second.getAngle().getValue()[1], 0.8713175514, 1e-9),
                AipsError
            );
        }
        {
            cout << "*** Test getTimeRange()" << endl;
            std::pair<double, double> timerange = md.getTimeRange();
            AlwaysAssert(near(timerange.first, 4842824745.020, 1e-12), AipsError);
            AlwaysAssert(near(timerange.second, 4842830012.448, 1e-12), AipsError);
        }
        {
            cout << "*** test getTimesForIntent" << endl;
            std::set<String> intents = md.getIntents();
            std::set<String>::const_iterator intent = intents.begin();
            std::set<String>::const_iterator end = intents.end();
            while (intent != end) {
                std::set<double> times = md.getTimesForIntent(*intent);
                uint32_t nTimes = times.size();
                uint32_t exp = 0;
                if (*intent == "CALIBRATE_AMPLI#ON_SOURCE") {
                    exp = 234;
                }
                else if (*intent == "CALIBRATE_ATMOSPHERE#OFF_SOURCE") {
                    exp = 46;
                }
                else if (*intent == "CALIBRATE_ATMOSPHERE#ON_SOURCE") {
                    exp = 93;
                }
                else if (*intent == "CALIBRATE_BANDPASS#ON_SOURCE") {
                    exp = 623;
                }
                else if (*intent == "CALIBRATE_PHASE#ON_SOURCE") {
                    exp = 1128;
                }
                else if (*intent == "CALIBRATE_POINTING#ON_SOURCE") {
                    exp = 244;
                }
                else if(
                    *intent == "CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE"
                    || *intent == "CALIBRATE_SIDEBAND_RATIO#ON_SOURCE"
                ) {
                    exp = 49;
                }
                else if (*intent == "CALIBRATE_WVR#OFF_SOURCE") {
                    exp = 95;
                }
                else if (*intent == "CALIBRATE_WVR#ON_SOURCE") {
                    exp = 1514;
                }
                else if (*intent == "OBSERVE_TARGET#ON_SOURCE") {
                    exp = 1868;
                }
                AlwaysAssert(nTimes == exp, AipsError);
                ++intent;
            }
            {
                cout << "*** test getSummary()" << endl;
                //cout << "summary " << md.getSummary() << endl;
            }
            {
                cout << "*** test getProjects()" << endl;
                vector<String> projects = md.getProjects();
                AlwaysAssert(projects.size() == 1, AipsError);
                AlwaysAssert(projects[0] == "T.B.D.", AipsError);
            }
            {
                cout << "*** test getObservers()" << endl;
                vector<String> observers = md.getObservers();
                AlwaysAssert(observers.size() == 1, AipsError);
                AlwaysAssert(observers[0] == "csalyk", AipsError);
            }
            {
                cout << "*** test getSchedules()" << endl;
                vector<vector<String> > schedules = md.getSchedules();
                AlwaysAssert(schedules.size() == 1, AipsError);
                AlwaysAssert(schedules[0].size() == 2, AipsError);
                AlwaysAssert(schedules[0][0] == "SchedulingBlock uid://A002/X391d0b/X5e", AipsError);
                AlwaysAssert(schedules[0][1] == "ExecBlock uid://A002/X3f6a86/X5da", AipsError);
            }
            {
                // 4842824633.4720001
                // 4842830031.632
                cout << "*** test getTimeRangesOfObservations()" << endl;
                vector<std::pair<MEpoch, MEpoch> > timers = md.getTimeRangesOfObservations();
                AlwaysAssert(timers.size() == 1, AipsError);
                AlwaysAssert(timers[0].first.getRefString() == "UTC", AipsError);
                AlwaysAssert(timers[0].second.getRefString() == "UTC", AipsError);
                AlwaysAssert(timers[0].first.getUnit() == Unit("s"), AipsError);
                AlwaysAssert(timers[0].second.getUnit() == Unit("s"), AipsError);
                AlwaysAssert(
                    near(timers[0].first.get("s").getValue(), 4842824633.472), AipsError
                );
                AlwaysAssert(
                    near(timers[0].second.get("s").getValue(), 4842830031.632), AipsError
                );
            }
            {
                cout << "*** test getRefFreqs()" << endl;
                double expec[] = {
                    1.83300000e+11, 2.15250000e+11, 2.15250000e+11,
                    2.17250000e+11, 2.17250000e+11, 2.29250000e+11,
                    2.29250000e+11, 2.31250000e+11, 2.31250000e+11,
                    2.30471730e+11, 2.30471730e+11, 2.32352270e+11,
                    2.32352270e+11, 2.20465062e+11, 2.20465062e+11,
                    2.19610562e+11, 2.19610562e+11, 2.30471730e+11,
                    2.30471730e+11, 2.32352270e+11, 2.32352270e+11,
                    2.20465062e+11, 2.20465062e+11, 2.19610562e+11,
                    2.19610562e+11, 1.83310000e+11, 1.83320000e+11,
                    1.83330000e+11, 1.83340000e+11, 1.83350000e+11,
                    1.83360000e+11, 1.83370000e+11, 1.83380000e+11,
                    1.83390000e+11, 1.83400000e+11, 1.83410000e+11,
                    1.83420000e+11, 1.83430000e+11, 1.83440000e+11,
                    1.83450000e+11
                };
                uint32_t n = md.nSpw(true);
                vector<MFrequency> rf = md.getRefFreqs();
                for (uint32_t i=0; i<n; ++i) {
                    AlwaysAssert(rf[i].getRefString() == "TOPO", AipsError);
                    AlwaysAssert(rf[i].getUnit() == Unit("Hz"), AipsError);
                    AlwaysAssert(near(rf[i].get("Hz").getValue(), expec[i], 1e-8), AipsError);
                }
            }
        }
        {
            cout << "*** test getCorrTypes" << endl;
            vector<vector<int32_t> > corrTypes = md.getCorrTypes();
            AlwaysAssert(corrTypes[0].size() == 2, AipsError);
            AlwaysAssert(corrTypes[0][0] == 9, AipsError);
            AlwaysAssert(corrTypes[0][1] == 12, AipsError);
            AlwaysAssert(corrTypes[1].size() == 1, AipsError);
            AlwaysAssert(corrTypes[1][0] == 1, AipsError);
        }
        {
            cout << "*** test getCorrProducts" << endl;
            vector<Array<int32_t> > corrProds = md.getCorrProducts();
            AlwaysAssert(corrProds[0].size() == 4, AipsError);
            AlwaysAssert(corrProds[0](IPosition(2, 0, 0)) == 0, AipsError);
            AlwaysAssert(corrProds[0](IPosition(2, 0, 1)) == 1, AipsError);
            AlwaysAssert(corrProds[0](IPosition(2, 1, 0)) == 0, AipsError);
            AlwaysAssert(corrProds[0](IPosition(2, 1, 1)) == 1, AipsError);
            AlwaysAssert(corrProds[1].size() == 2, AipsError);
            AlwaysAssert(allTrue(corrProds[1] == 0), AipsError);
        }
        {
            cout << "*** test getSourceTableSourceIDs" << endl;
            vector<int32_t> sourceIDs = md.getSourceTableSourceIDs();
            AlwaysAssert(sourceIDs.size() == 200, AipsError);
            for (uint32_t i=0; i<200; ++i) {
                int32_t expec = 0;
                if (
                    (i >= 40 && i <= 63)
                    || (i >= 80 && i <= 95)
                ) {
                    expec = 1;
                }
                else if (
                    (i >= 64 && i <= 79)
                    || (i >= 112 && i <= 135)
                ) {
                    expec = 2;
                }
                else if (
                    (i >= 96 && i <= 111)
                    || (i >= 152 && i <= 167)
                ) {
                    expec = 3;
                }
                else if (
                    (i >= 136 && i <= 151)
                    || (i >= 184 && i <= 199)
                ) {
                    expec = 4;
                }
                else if (i >= 168 && i <= 183) {
                    expec = 5;
                }
                AlwaysAssert(sourceIDs[i] == expec, AipsError);
            }
        }
        {
            cout << "*** test getPhaseDirs()" << endl;
            vector<MDirection> phaseDirs = md.getPhaseDirs();
            AlwaysAssert(phaseDirs.size() == md.nFields(), AipsError);
            double elong[] = {
                -2.8964345 , -2.71545722, -2.72554329,
                -1.98190197, -2.04411602, -1.94537525
            };
            double elat[] = {
                -0.10104256, -0.22613985, -0.1219181,
                -0.44437211, -0.32533384, -0.27584353
            };
            for (uint32_t i=0; i<phaseDirs.size(); ++i) {
                AlwaysAssert(phaseDirs[i].getRefString() == "J2000", AipsError);
                Vector<double> angle = phaseDirs[i].getAngle("rad").getBaseValue();
                AlwaysAssert(near(angle[0], elong[i], 1e-7), AipsError);
                AlwaysAssert(near(angle[1], elat[i], 1e-7), AipsError);
            }
        }
        {
            cout << "*** test getFieldTableSourceIDs" << endl;
            vector<int32_t> sourceIDs = md.getFieldTableSourceIDs();
            AlwaysAssert(sourceIDs.size() == md.nFields(), AipsError);
            for (uint32_t i=0; i<sourceIDs.size(); ++i) {
                AlwaysAssert(sourceIDs[i] == (int32_t)i, AipsError);
            }
        }
        {
            cout << "*** test getAntennasForScan()" << endl;
            std::set<int32_t> scans = md.getScanNumbers(0, 0);
            std::set<int32_t>::const_iterator iter = scans.begin();
            std::set<int32_t>::const_iterator end = scans.end();
            ScanKey key;
            key.obsID = 0;
            key.arrayID = 0;
            while (iter != end) {
                key.scan = *iter;
                std::set<int32_t> ants = md.getAntennasForScan(key);
                uint32_t n = *iter == 9 ? 12 : 13;
                AlwaysAssert(ants.size() == n, AipsError);
                std::set<int32_t>::const_iterator aIter = ants.begin();
                for (int32_t i=0; i<14; ++i, ++aIter) {
                    if (i == 12 || (*iter == 9 && i == 7)) {
                        ++i;
                        continue;
                    }
                    else {
                        AlwaysAssert(*aIter == i, AipsError);
                    }
                }
                ++iter;
            }
        }
        {
            cout << "*** test getSourceNames()" << endl;
            vector<String> sourceNames = md.getSourceNames();
            String expec[] = {
                "3C279", "3C279", "3C279", "3C279", "3C279", "3C279", "3C279",
                "3C279", "3C279", "3C279", "3C279", "3C279", "3C279", "3C279",
                "3C279", "3C279", "3C279", "3C279", "3C279", "3C279", "3C279",
                "3C279", "3C279", "3C279", "3C279", "3C279", "3C279", "3C279",
                "3C279", "3C279", "3C279", "3C279", "3C279", "3C279", "3C279",
                "3C279", "3C279", "3C279", "3C279", "3C279", "J1337-129",
                "J1337-129", "J1337-129", "J1337-129", "J1337-129", "J1337-129",
                "J1337-129", "J1337-129", "J1337-129", "J1337-129", "J1337-129",
                "J1337-129", "J1337-129", "J1337-129", "J1337-129", "J1337-129",
                "J1337-129", "J1337-129", "J1337-129", "J1337-129", "J1337-129",
                "J1337-129", "J1337-129", "J1337-129", "Titan", "Titan", "Titan",
                "Titan", "Titan", "Titan", "Titan", "Titan", "Titan", "Titan",
                "Titan", "Titan", "Titan", "Titan", "Titan", "Titan", "Titan",
                "Titan", "Titan", "Titan", "Titan", "Titan", "Titan", "Titan",
                "Titan", "Titan", "Titan", "Titan", "Titan", "Titan", "Titan",
                "Titan", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "J1625-254", "J1625-254", "J1625-254", "J1625-254",
                "J1625-254", "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco",
                "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco",
                "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco",
                "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco",
                "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco",
                "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco", "V866 Sco",
                "V866 Sco", "V866 Sco", "V866 Sco", "RNO 90", "RNO 90", "RNO 90",
                "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90",
                "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90",
                "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90",
                "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90",
                "RNO 90", "RNO 90", "RNO 90", "RNO 90", "RNO 90"
            };
            for (uint32_t i=0; i<sourceNames.size(); ++i) {
                AlwaysAssert(sourceNames[i] == expec[i], AipsError);
            }
        }
        {
            cout << "*** test getSourceDirections()" << endl;
            vector<MDirection> dirs = md.getSourceDirections();
            AlwaysAssert(dirs.size() == 200, AipsError);
            double elong[] = {
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 , -2.8964345 ,
                -2.71545722, -2.71545722, -2.71545722, -2.71545722, -2.71545722,
                -2.71545722, -2.71545722, -2.71545722, -2.71545722, -2.71545722,
                -2.71545722, -2.71545722, -2.71545722, -2.71545722, -2.71545722,
                -2.71545722, -2.71545722, -2.71545722, -2.71545722, -2.71545722,
                -2.71545722, -2.71545722, -2.71545722, -2.71545722, -2.72554329,
                -2.72554329, -2.72554329, -2.72554329, -2.72554329, -2.72554329,
                -2.72554329, -2.72554329, -2.72554329, -2.72554329, -2.72554329,
                -2.72554329, -2.72554329, -2.72554329, -2.72554329, -2.72554329,
                -2.72554329, -2.72554329, -2.72554329, -2.72554329, -2.72554329,
                -2.72554329, -2.72554329, -2.72554329, -2.72554329, -2.72554329,
                -2.72554329, -2.72554329, -2.72554329, -2.72554329, -2.72554329,
                -2.72554329, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -1.98190197, -1.98190197, -1.98190197, -1.98190197,
                -1.98190197, -2.04411602, -2.04411602, -2.04411602, -2.04411602,
                -2.04411602, -2.04411602, -2.04411602, -2.04411602, -2.04411602,
                -2.04411602, -2.04411602, -2.04411602, -2.04411602, -2.04411602,
                -2.04411602, -2.04411602, -2.04411602, -2.04411602, -2.04411602,
                -2.04411602, -2.04411602, -2.04411602, -2.04411602, -2.04411602,
                -2.04411602, -2.04411602, -2.04411602, -2.04411602, -2.04411602,
                -2.04411602, -2.04411602, -2.04411602, -1.94537525, -1.94537525,
                -1.94537525, -1.94537525, -1.94537525, -1.94537525, -1.94537525,
                -1.94537525, -1.94537525, -1.94537525, -1.94537525, -1.94537525,
                -1.94537525, -1.94537525, -1.94537525, -1.94537525, -1.94537525,
                -1.94537525, -1.94537525, -1.94537525, -1.94537525, -1.94537525,
                -1.94537525, -1.94537525, -1.94537525, -1.94537525, -1.94537525,
                -1.94537525, -1.94537525, -1.94537525, -1.94537525, -1.94537525
            };
            double elat[] = {
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.10104256, -0.10104256, -0.10104256, -0.10104256, -0.10104256,
                -0.22613985, -0.22613985, -0.22613985, -0.22613985, -0.22613985,
                -0.22613985, -0.22613985, -0.22613985, -0.22613985, -0.22613985,
                -0.22613985, -0.22613985, -0.22613985, -0.22613985, -0.22613985,
                -0.22613985, -0.22613985, -0.22613985, -0.22613985, -0.22613985,
                -0.22613985, -0.22613985, -0.22613985, -0.22613985, -0.1219181 ,
                -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 ,
                -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 ,
                -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 ,
                -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 ,
                -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 ,
                -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 , -0.1219181 ,
                -0.1219181 , -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.44437211, -0.44437211, -0.44437211, -0.44437211,
                -0.44437211, -0.32533384, -0.32533384, -0.32533384, -0.32533384,
                -0.32533384, -0.32533384, -0.32533384, -0.32533384, -0.32533384,
                -0.32533384, -0.32533384, -0.32533384, -0.32533384, -0.32533384,
                -0.32533384, -0.32533384, -0.32533384, -0.32533384, -0.32533384,
                -0.32533384, -0.32533384, -0.32533384, -0.32533384, -0.32533384,
                -0.32533384, -0.32533384, -0.32533384, -0.32533384, -0.32533384,
                -0.32533384, -0.32533384, -0.32533384, -0.27584353, -0.27584353,
                -0.27584353, -0.27584353, -0.27584353, -0.27584353, -0.27584353,
                -0.27584353, -0.27584353, -0.27584353, -0.27584353, -0.27584353,
                -0.27584353, -0.27584353, -0.27584353, -0.27584353, -0.27584353,
                -0.27584353, -0.27584353, -0.27584353, -0.27584353, -0.27584353,
                -0.27584353, -0.27584353, -0.27584353, -0.27584353, -0.27584353,
                -0.27584353, -0.27584353, -0.27584353, -0.27584353, -0.27584353
            };
            for (uint32_t i=0; i<dirs.size(); ++i) {
                AlwaysAssert(dirs[i].getRefString() == "J2000", AipsError);
                Vector<double> angle = dirs[i].getAngle("rad").getBaseValue();
                AlwaysAssert(near(angle[0], elong[i], 1e-7), AipsError);
                AlwaysAssert(near(angle[1], elat[i], 1e-7), AipsError);
            }
        }
        {
            cout << "*** test getProperMotions()" << endl;
            vector<std::pair<Quantity, Quantity> > pm = md.getProperMotions();
            AlwaysAssert(pm.size() == 200, AipsError);
            for (uint32_t i=0; i<200; ++i) {
                AlwaysAssert(pm[0].first.getValue() == 0, AipsError);
                AlwaysAssert(pm[0].second.getValue() == 0, AipsError);
                AlwaysAssert(pm[0].first.getUnit() == "rad/s", AipsError);
                AlwaysAssert(pm[0].second.getUnit() == "rad/s", AipsError);
            }
        }
        {
            cout << "*** test getSpwToTimesForScan()" << endl;
            ScanKey scan;
            scan.obsID = 0;
            scan.arrayID = 0;
            scan.scan = 5;
            std::map<uint32_t, std::set<double> > times = md.getSpwToTimesForScan(scan);
            AlwaysAssert(times.size() == 9, AipsError);
            std::set<double> expec;
            for (uint32_t i=0; i<9; ++i) {
                if(i == 0) {
                    double z[] = {
                        4842825782.3999996185, 4842825800.8319997787, 4842825807.7440004349,
                        4842825826.1760005951, 4842825844.6079998016, 4842825861.8879995346,
                        4842825869.9519996643
                    };
                    expec = std::set<double>(z, z+7);
                }
                else if (i == 1) {
                    double z[] = {
                        4842825778.6560001373, 4842825780.6719999313, 4842825782.6879997253,
                        4842825784.704000473,  4842825786.720000267,  4842825799.3920001984,
                        4842825801.4079999924, 4842825803.4239997864, 4842825805.4400005341,
                        4842825807.4560003281, 4842825820.1280002594, 4842825822.1440000534,
                        4842825824.1599998474, 4842825826.1760005951, 4842825828.1920003891,
                        4842825840.8640003204, 4842825842.8800001144, 4842825844.8959999084,
                        4842825846.9119997025, 4842825848.9279994965, 4842825861.6000003815,
                        4842825863.6159992218, 4842825865.6319999695, 4842825867.6479997635,
                        4842825869.6639995575
                    };
                    expec = std::set<double>(z, z+25);
                }
                else if (i == 2) {
                    double z[] = {
                        4842825778.1519994736, 4842825779.1600008011, 4842825780.1679992676,
                        4842825781.1760005951, 4842825782.1839990616, 4842825783.1920003891,
                        4842825784.1999998093, 4842825785.2080001831, 4842825786.2159996033,
                        4842825787.2240009308, 4842825798.8879995346, 4842825799.8960008621,
                        4842825800.9039993286, 4842825801.9120006561, 4842825802.9199991226,
                        4842825803.9280004501, 4842825804.9359998703, 4842825805.9440002441,
                        4842825806.9519996643, 4842825807.9600009918, 4842825819.6239995956,
                        4842825820.6320009232, 4842825821.6399993896, 4842825822.6480007172,
                        4842825823.6560001373, 4842825824.6640005112, 4842825825.6719999313,
                        4842825826.6800003052, 4842825827.6879997253, 4842825828.6960010529,
                        4842825840.3599996567, 4842825841.3680009842, 4842825842.3759994507,
                        4842825843.3840007782, 4842825844.3920001984, 4842825845.4000005722,
                        4842825846.4079990387, 4842825847.4160003662, 4842825848.4239988327,
                        4842825849.4320001602, 4842825861.0959997177, 4842825862.1040010452,
                        4842825863.1119995117, 4842825864.1200008392, 4842825865.1279993057,
                        4842825866.1360006332, 4842825867.1439990997, 4842825868.1520004272,
                        4842825869.1599998474, 4842825870.1680002213
                    };
                    expec = std::set<double>(z, z+50);
                }
                else if (i == 3) {
                    double z[] = {
                        4842825778.6560001373, 4842825780.6719999313, 4842825782.6879997253,
                        4842825784.704000473,  4842825786.720000267,  4842825799.3920001984,
                        4842825801.4079999924, 4842825803.4239997864, 4842825805.4400005341,
                        4842825807.4560003281, 4842825820.1280002594, 4842825822.1440000534,
                        4842825824.1599998474, 4842825826.1760005951, 4842825828.1920003891,
                        4842825840.8640003204, 4842825842.8800001144, 4842825844.8959999084,
                        4842825846.9119997025, 4842825848.9279994965, 4842825861.6000003815,
                        4842825863.6159992218, 4842825865.6319999695, 4842825867.6479997635,
                        4842825869.6639995575
                    };
                    expec = std::set<double>(z, z+25);
                }
                else if (i == 4) {
                    double z[] = {
                        4842825778.1519994736, 4842825779.1600008011, 4842825780.1679992676,
                        4842825781.1760005951, 4842825782.1839990616, 4842825783.1920003891,
                        4842825784.1999998093, 4842825785.2080001831, 4842825786.2159996033,
                        4842825787.2240009308, 4842825798.8879995346, 4842825799.8960008621,
                        4842825800.9039993286, 4842825801.9120006561, 4842825802.9199991226,
                        4842825803.9280004501, 4842825804.9359998703, 4842825805.9440002441,
                        4842825806.9519996643, 4842825807.9600009918, 4842825819.6239995956,
                        4842825820.6320009232, 4842825821.6399993896, 4842825822.6480007172,
                        4842825823.6560001373, 4842825824.6640005112, 4842825825.6719999313,
                        4842825826.6800003052, 4842825827.6879997253, 4842825828.6960010529,
                        4842825840.3599996567, 4842825841.3680009842, 4842825842.3759994507,
                        4842825843.3840007782, 4842825844.3920001984, 4842825845.4000005722,
                        4842825846.4079990387, 4842825847.4160003662, 4842825848.4239988327,
                        4842825849.4320001602, 4842825861.0959997177, 4842825862.1040010452,
                        4842825863.1119995117, 4842825864.1200008392, 4842825865.1279993057,
                        4842825866.1360006332, 4842825867.1439990997, 4842825868.1520004272,
                        4842825869.1599998474, 4842825870.1680002213
                    };
                    expec = std::set<double>(z, z+50);
                }
                else if (i == 5) {
                    double z[] = {
                        4842825778.6560001373, 4842825780.6719999313, 4842825782.6879997253,
                        4842825784.704000473,  4842825786.720000267,  4842825799.3920001984,
                        4842825801.4079999924, 4842825803.4239997864, 4842825805.4400005341,
                        4842825807.4560003281, 4842825820.1280002594, 4842825822.1440000534,
                        4842825824.1599998474, 4842825826.1760005951, 4842825828.1920003891,
                        4842825840.8640003204, 4842825842.8800001144, 4842825844.8959999084,
                        4842825846.9119997025, 4842825848.9279994965, 4842825861.6000003815,
                        4842825863.6159992218, 4842825865.6319999695, 4842825867.6479997635,
                        4842825869.6639995575
                    };
                    expec = std::set<double>(z, z+25);
                }
                else if (i == 6) {
                    double z[] = {
                        4842825778.1519994736, 4842825779.1600008011, 4842825780.1679992676,
                        4842825781.1760005951, 4842825782.1839990616, 4842825783.1920003891,
                        4842825784.1999998093, 4842825785.2080001831, 4842825786.2159996033,
                        4842825787.2240009308, 4842825798.8879995346, 4842825799.8960008621,
                        4842825800.9039993286, 4842825801.9120006561, 4842825802.9199991226,
                        4842825803.9280004501, 4842825804.9359998703, 4842825805.9440002441,
                        4842825806.9519996643, 4842825807.9600009918, 4842825819.6239995956,
                        4842825820.6320009232, 4842825821.6399993896, 4842825822.6480007172,
                        4842825823.6560001373, 4842825824.6640005112, 4842825825.6719999313,
                        4842825826.6800003052, 4842825827.6879997253, 4842825828.6960010529,
                        4842825840.3599996567, 4842825841.3680009842, 4842825842.3759994507,
                        4842825843.3840007782, 4842825844.3920001984, 4842825845.4000005722,
                        4842825846.4079990387, 4842825847.4160003662, 4842825848.4239988327,
                        4842825849.4320001602, 4842825861.0959997177, 4842825862.1040010452,
                        4842825863.1119995117, 4842825864.1200008392, 4842825865.1279993057,
                        4842825866.1360006332, 4842825867.1439990997, 4842825868.1520004272,
                        4842825869.1599998474, 4842825870.1680002213
                    };
                    expec = std::set<double>(z, z+50);
                }
                else if (i == 7) {
                    double z[] = {
                        4842825778.6560001373, 4842825780.6719999313, 4842825782.6879997253,
                        4842825784.704000473,  4842825786.720000267,  4842825799.3920001984,
                        4842825801.4079999924, 4842825803.4239997864, 4842825805.4400005341,
                        4842825807.4560003281, 4842825820.1280002594, 4842825822.1440000534,
                        4842825824.1599998474, 4842825826.1760005951, 4842825828.1920003891,
                        4842825840.8640003204, 4842825842.8800001144, 4842825844.8959999084,
                        4842825846.9119997025, 4842825848.9279994965, 4842825861.6000003815,
                        4842825863.6159992218, 4842825865.6319999695, 4842825867.6479997635,
                        4842825869.6639995575
                    };
                    expec = std::set<double>(z, z+25);
                }
                else if (i == 8) {
                    double z[] = {
                        4842825778.1519994736, 4842825779.1600008011, 4842825780.1679992676,
                        4842825781.1760005951, 4842825782.1839990616, 4842825783.1920003891,
                        4842825784.1999998093, 4842825785.2080001831, 4842825786.2159996033,
                        4842825787.2240009308, 4842825798.8879995346, 4842825799.8960008621,
                        4842825800.9039993286, 4842825801.9120006561, 4842825802.9199991226,
                        4842825803.9280004501, 4842825804.9359998703, 4842825805.9440002441,
                        4842825806.9519996643, 4842825807.9600009918, 4842825819.6239995956,
                        4842825820.6320009232, 4842825821.6399993896, 4842825822.6480007172,
                        4842825823.6560001373, 4842825824.6640005112, 4842825825.6719999313,
                        4842825826.6800003052, 4842825827.6879997253, 4842825828.6960010529,
                        4842825840.3599996567, 4842825841.3680009842, 4842825842.3759994507,
                        4842825843.3840007782, 4842825844.3920001984, 4842825845.4000005722,
                        4842825846.4079990387, 4842825847.4160003662, 4842825848.4239988327,
                        4842825849.4320001602, 4842825861.0959997177, 4842825862.1040010452,
                        4842825863.1119995117, 4842825864.1200008392, 4842825865.1279993057,
                        4842825866.1360006332, 4842825867.1439990997, 4842825868.1520004272,
                        4842825869.1599998474, 4842825870.1680002213
                    };
                    expec = std::set<double>(z, z+50);
                }
                else {
                    cout << "found channel " << i << " which shouldn't be in this set" << endl;
                    AlwaysAssert(false, AipsError);
                }
                AlwaysAssert(times[i].size() == expec.size(), AipsError);
                std::set<double>::const_iterator iter = times[i].begin();
                std::set<double>::const_iterator end = times[i].end();
                std::set<double>::const_iterator eIter = expec.begin();
                while (iter != end) {
                    AlwaysAssert(near(*iter, *eIter), AipsError);
                    ++iter;
                    ++eIter;
                }
            }
        }
        {
            cout << "*** test nUniqueSourceIDsFromSourceTable()" << endl;
            uint32_t n = md.nUniqueSourceIDsFromSourceTable();
            AlwaysAssert(n == 6, AipsError);
        }
        {
            cout << "*** test getFieldNames()" << endl;
            vector<String> fnames = md.getFieldNames();
            String z[] = {"3C279", "J1337-129", "Titan", "J1625-254", "V866 Sco", "RNO 90"};
            vector<String> expec(z, z+6);
            vector<String>::const_iterator iter = fnames.begin();
            vector<String>::const_iterator end = fnames.end();
            vector<String>::const_iterator eIter = expec.begin();
            while (iter != end) {
                AlwaysAssert(*iter == *eIter, AipsError);
                ++iter;
                ++eIter;
            }
        }
        {
            cout << "*** test getNetSidebands()" << endl;
            vector<int32_t> netsb = md.getNetSidebands();
            int32_t expec[] = {
                3, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2,
                2, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
            };
            uint32_t n = netsb.size();
            for(uint32_t i=0; i<n; ++i) {
                AlwaysAssert(netsb[i] == expec[i], AipsError);
            }
        }
        {
            cout << "*** test getReferenceDirection()" << endl;
            uint32_t nfields = md.nFields();
            for (uint32_t i=0; i<nfields; ++i) {
                MDirection dir = md.getReferenceDirection(i);
                Vector<double> angle = dir.getAngle().getValue();
                switch(i) {
                case 0:
                    AlwaysAssert(near(angle[0], -2.8964345, 1e-6), AipsError);
                    AlwaysAssert(near(angle[1], -0.10104256, 1e-6), AipsError);
                    break;
                case 1:
                    AlwaysAssert(near(angle[0], -2.71545722, 1e-6), AipsError);
                    AlwaysAssert(near(angle[1], -0.22613985, 1e-6), AipsError);
                    break;
                case 2:
                    AlwaysAssert(near(angle[0], -2.72554329, 1e-6), AipsError);
                    AlwaysAssert(near(angle[1], -0.1219181, 1e-6), AipsError);
                    break;
                case 3:
                    AlwaysAssert(near(angle[0], -1.98190197, 1e-6), AipsError);
                    AlwaysAssert(near(angle[1], -0.44437211, 1e-6), AipsError);
                    break;
                case 4:
                    AlwaysAssert(near(angle[0], -2.04411602, 1e-6), AipsError);
                    AlwaysAssert(near(angle[1], -0.32533384, 1e-6), AipsError);
                    break;
                case 5:
                    AlwaysAssert(near(angle[0], -1.94537525, 1e-6), AipsError);
                    AlwaysAssert(near(angle[1], -0.27584353, 1e-6), AipsError);
                    break;
                default:
                    break;
                }
            }
        }
        {
            cout << "*** test getChanEffectiveBWs()" << endl;
            vector<QVector<double> > ebw = md.getChanEffectiveBWs(false);
            vector<QVector<double> >::const_iterator iter = ebw.begin();
            vector<QVector<double> >::const_iterator end = ebw.end();
            double expec = 0;
            while (iter != end) {
                size_t nchans = iter->size();
                if (nchans == 1) {
                    ++iter;
                    continue;
                }
                else if (nchans == 4) {
                    expec = 7.5e9;
                }
                else if (nchans == 128) {
                    expec = 1.5625e7;
                }
                else if (nchans == 3840) {
                    expec = 30517.578125;
                }
                Vector<double> vals = iter->getValue();
                Vector<double>::const_iterator jiter = vals.begin();
                Vector<double>::const_iterator jend = vals.end();
                while (jiter != jend) {
                    AlwaysAssert(*jiter == expec, AipsError);
                    ++jiter;
                }
                ++iter;
            }
            vector<QVector<double> > ebwv = md.getChanEffectiveBWs(true);
            AlwaysAssert(near(ebwv[9].getValue()[0], 20.23684342, 1e-8), AipsError);
            AlwaysAssert(ebwv[9].getUnit() == "km/s", AipsError);
        }
        {
            cout << "*** test getChanResolutions()" << endl;
            vector<QVector<double> > ebw = md.getChanResolutions(false);
            vector<QVector<double> >::const_iterator iter = ebw.begin();
            vector<QVector<double> >::const_iterator end = ebw.end();
            double expec = 0;
            while (iter != end) {
                size_t nchans = iter->size();
                if (nchans == 1) {
                    ++iter;
                    continue;
                }
                else if (nchans == 4) {
                    expec = 7.5e9;
                }
                else if (nchans == 128) {
                    expec = 1.5625e7;
                }
                else if (nchans == 3840) {
                    expec = 30517.578125;
                }
                Vector<double> vals = iter->getValue();
                Vector<double>::const_iterator jiter = vals.begin();
                Vector<double>::const_iterator jend = vals.end();
                while (jiter != jend) {
                    AlwaysAssert(*jiter == expec, AipsError);
                    ++jiter;
                }
                ++iter;
            }
            vector<QVector<double> > ebwv = md.getChanResolutions(true);
            AlwaysAssert(near(ebwv[9].getValue()[0], 20.23684342, 1e-8), AipsError);
            AlwaysAssert(ebwv[9].getUnit() == "km/s", AipsError);
        }
        {
            cout << "test getRestFrequencies()" << endl;
            std::map<SourceKey, std::shared_ptr<vector<MFrequency> > > rfs = md.getRestFrequencies();
            std::map<SourceKey, std::shared_ptr<vector<MFrequency> > >::const_iterator iter = rfs.begin();
            std::map<SourceKey, std::shared_ptr<vector<MFrequency> > >::const_iterator end = rfs.end();
            while (iter != end) {
                if (iter->second ) {
                    AlwaysAssert(
                        iter->first.id == 0 && iter->first.spw == 34, AipsError
                    );
                    AlwaysAssert(iter->second->size() == 2, AipsError);
                    AlwaysAssert((*iter->second)[0].get("Hz").getValue() == 1e10, AipsError);
                    AlwaysAssert((*iter->second)[1].get("Hz").getValue() == 2e10, AipsError);
                }
                ++iter;
            }
        }
        {
            cout << "test getTransitions()" << endl;
            std::map<SourceKey, std::shared_ptr<vector<String> > > rfs = md.getTransitions();
            std::map<SourceKey, std::shared_ptr<vector<String> > >::const_iterator iter = rfs.begin();
            std::map<SourceKey, std::shared_ptr<vector<String> > >::const_iterator end = rfs.end();
            while (iter != end) {
                if (iter->second ) {
                    AlwaysAssert(
                        iter->first.id == 0 && iter->first.spw == 34, AipsError
                    );
                    AlwaysAssert(iter->second->size() == 2, AipsError);
                    AlwaysAssert((*iter->second)[0] == "myline", AipsError);
                    AlwaysAssert((*iter->second)[1] == "yourline", AipsError);
                }
                ++iter;
            }
        }
        {
            cout << "test getSubScanProperties" << endl;
            SubScanKey sskey;
            sskey.arrayID = 0;
            sskey.fieldID = 0;
            sskey.obsID = 0;
            sskey.scan = 0;
            bool thrown = false;
            try {
                md.getSubScanProperties(sskey);
            }
            catch (const std::exception& x) {
                thrown = true;
            }
            AlwaysAssert(thrown, AipsError);
            sskey.scan = 1;
            MSMetaData::SubScanProperties props = md.getSubScanProperties(sskey);
            AlwaysAssert(props.acRows + props.xcRows == 367, AipsError);
            std::shared_ptr<const std::map<SubScanKey, MSMetaData::SubScanProperties> > allProps
                = md.getSubScanProperties();
            AlwaysAssert(
                allProps->find(sskey)->second.acRows + allProps->find(sskey)->second.xcRows == 367,
                AipsError
            );
            for (uint32_t i=0; i<9; ++i) {
                double expec = 0;
                if (i == 0) {
                    expec = 1.152;
                }
                else if (i == 1 || i == 3 || i == 5 || i == 7) {
                    expec = 2.016;
                }
                else {
                    expec = 1.008;
                }
                AlwaysAssert(near(props.meanInterval[i].getValue(), expec), AipsError);
            }
        }
        {
            cout << "*** test getScanKeys()" << endl;
            std::set<ScanKey> keys = md.getScanKeys();
            ScanKey expec;
            expec.arrayID = 0;
            expec.obsID = 0;
            for (int32_t i=1; i<34; ++i) {
                expec.scan = i;
                if (i == 33) {
                    AlwaysAssert(keys.find(expec) == keys.end(), AipsError);
                }
                else {
                    AlwaysAssert(keys.find(expec) != keys.end(), AipsError);
                }
            }
        }
        {
            cout << "*** test getIntentsForSubScan() and getSubScanToIntentsMap()" << endl;
            ArrayKey arrayKey;
            arrayKey.obsID = 0;
            arrayKey.arrayID = 0;
            std::set<SubScanKey> sskeys = md.getSubScanKeys(arrayKey);
            std::set<SubScanKey>::const_iterator ssiter = sskeys.begin();
            std::set<SubScanKey>::const_iterator ssend = sskeys.end();
            std::shared_ptr<const std::map<SubScanKey, std::set<String> > > mymap = md.getSubScanToIntentsMap();
            for (; ssiter!=ssend; ++ssiter) {
                std::set<String> intents = md.getIntentsForSubScan(*ssiter);
                std::set<String> exp;
                int32_t scan = ssiter->scan;
                if (scan == 1 || scan == 5 || scan == 8) {
                    String mystr[] = {
                        "CALIBRATE_POINTING#ON_SOURCE", "CALIBRATE_WVR#ON_SOURCE"
                    };
                    exp.insert(mystr, mystr+2);
                }
                else if (scan == 2) {
                    String mystr[] = {
                        "CALIBRATE_SIDEBAND_RATIO#OFF_SOURCE",
                        "CALIBRATE_SIDEBAND_RATIO#ON_SOURCE",
                        "CALIBRATE_WVR#OFF_SOURCE",
                        "CALIBRATE_WVR#ON_SOURCE"
                    };
                    exp.insert(mystr, mystr+4);
                }
                else if (
                    scan == 3 || scan == 6
                    || scan == 9 || scan == 11
                    || scan == 13 || scan == 15
                    || scan == 17 || scan == 19
                    || scan == 22 || scan == 24
                    || scan == 26 || scan == 29
                    || scan == 31
                ) {
                    String mystr[] = {
                        "CALIBRATE_ATMOSPHERE#OFF_SOURCE",
                        "CALIBRATE_ATMOSPHERE#ON_SOURCE",
                        "CALIBRATE_WVR#OFF_SOURCE",
                        "CALIBRATE_WVR#ON_SOURCE"
                    };
                    exp.insert(mystr, mystr+4);
                }
                else if (scan == 4) {
                    String mystr[] = {
                        "CALIBRATE_BANDPASS#ON_SOURCE",
                        "CALIBRATE_PHASE#ON_SOURCE",
                        "CALIBRATE_WVR#ON_SOURCE"
                    };
                    exp.insert(mystr, mystr+3);
                }
                else if (scan == 7) {
                    String mystr[] = {
                        "CALIBRATE_AMPLI#ON_SOURCE",
                        "CALIBRATE_PHASE#ON_SOURCE",
                        "CALIBRATE_WVR#ON_SOURCE"
                    };
                    exp.insert(mystr, mystr+3);
                }
                else if (
                    scan == 10 || scan == 14
                    || scan == 18 || scan == 21
                    || scan == 25 || scan == 28
                    || scan == 32
                ) {
                    String mystr[] = {
                        "CALIBRATE_PHASE#ON_SOURCE",
                        "CALIBRATE_WVR#ON_SOURCE"
                    };
                    exp.insert(mystr, mystr+2);
                }
                else if (
                    scan == 12 || scan == 16
                    || scan == 20 || scan == 23
                    || scan == 27 || scan == 30
                ) {
                    exp.insert("OBSERVE_TARGET#ON_SOURCE");
                }
                uniqueIntents.insert(exp.begin(), exp.end());
                AlwaysAssert(intents == exp, AipsError);
                AlwaysAssert(mymap->find(*ssiter)->second == exp, AipsError);
            }
        }
        {
            cout << "*** test getSpwsForSubScan()" << endl;
            ArrayKey arrayKey;
            arrayKey.obsID = 0;
            arrayKey.arrayID = 0;
            std::set<SubScanKey> sskeys = md.getSubScanKeys(arrayKey);
            std::set<SubScanKey>::const_iterator ssiter = sskeys.begin();
            std::set<SubScanKey>::const_iterator ssend = sskeys.end();
            for (; ssiter!=ssend; ++ssiter) {
                std::set<uint32_t> exp;
                int32_t scan = ssiter->scan;
                if (scan == 1 || scan==5 || scan==8) {
                    uint32_t myints[] = {
                        0, 1, 2, 3, 4, 5, 6, 7, 8
                    };
                    exp.insert(myints, myints+9);
                }
                else if (
                    scan == 2 || scan==3 || scan==6 || scan==9
                    || scan==11 || scan==13 || scan==15 || scan==17
                    || scan==19 || scan==22 || scan==24 || scan==26
                    || scan==29 || scan==31
                ) {
                    uint32_t myints[] = {
                        0, 9, 10, 11, 12, 13, 14, 15, 16
                    };
                    exp.insert(myints, myints+9);
                }
                else if (
                    scan==4 || scan==7 || scan==10 || scan==12
                    || scan==14 || scan==16 || scan==18 || scan==20
                    || scan==21 || scan==23 || scan==25 || scan==27
                    || scan==28 || scan==30 || scan==32
                ) {
                    uint32_t myints[] = {
                        0, 17, 18, 19, 20, 21, 22, 23, 24
                    };
                    exp.insert(myints, myints+9);
                }
                AlwaysAssert(md.getSpwsForSubScan(*ssiter) == exp, AipsError);
            }
        }
        {
            cout << "test getAverageIntervalsForSubScan()" << endl;
            ArrayKey arrayKey;
            arrayKey.obsID = 0;
            arrayKey.arrayID = 0;
            std::set<SubScanKey> sskeys = md.getSubScanKeys(arrayKey);
            std::set<SubScanKey>::const_iterator ssiter = sskeys.begin();
            std::set<SubScanKey>::const_iterator ssend = sskeys.end();
            for (; ssiter!=ssend; ++ssiter) {
                std::map<uint32_t, Quantity> mIntervals = md.getAverageIntervalsForSubScan(*ssiter);
                std::set<uint32_t> spws = md.getSpwsForSubScan(*ssiter);
                AlwaysAssert(mIntervals.size() == spws.size(), AipsError);
                std::map<uint32_t, Quantity>::const_iterator miter = mIntervals.begin();
                std::map<uint32_t, Quantity>::const_iterator mend = mIntervals.end();
                for (; miter!=mend; ++miter) {
                    uint32_t spw = miter->first;
                    AlwaysAssert(spws.find(spw) != spws.end(), AipsError);
                    double v = 0;
                    if (spw == 0) {
                        v = 1.152;
                    }
                    else if (
                        spw == 1 || spw == 3 || spw == 5 || spw == 7
                        || spw == 9 || spw == 11 || spw == 13 || spw == 15
                    ) {
                        v = 2.016;
                    }
                    else if (
                        spw == 2 || spw == 4 || spw == 6 || spw == 8
                        || spw == 10 || spw == 12 || spw == 14 || spw == 16
                        || spw == 18 || spw == 20 || spw == 22 || spw == 24
                    ) {
                        v = 1.008;
                    }
                    else if (
                        spw == 17 || spw == 19 || spw == 21 || spw == 23
                    ) {
                        v = 6.048;

                    }
                    Quantity expec(v, "s");
                    AlwaysAssert(near(miter->second, expec, 1e-1), AipsError);
                }
            }
        }
        {
            cout << "*** test getSpwIDs()" << endl;
            std::set<uint32_t> spws = md.getSpwIDs();
            AlwaysAssert(spws.size() == 25, AipsError);
            std::set<uint32_t>::const_iterator iter = spws.begin();
            std::set<uint32_t>::const_iterator end = spws.end();
            uint32_t i = 0;
            for (; iter!=end; ++iter, ++i) {
                AlwaysAssert(*iter == i, AipsError);
            }
        }
        {
            cout << "*** test getScanToTimeRangeMap()" << endl;
            std::shared_ptr<const std::map<ScanKey, std::pair<double,double> > > mymap
                = md.getScanToTimeRangeMap();
            ScanKey key;
            key.arrayID = 0;
            key.obsID = 0;
            key.scan = 1;
            AlwaysAssert(near(mymap->find(key)->second.first,  4842824745.0, 1.0), AipsError);
            AlwaysAssert(near(mymap->find(key)->second.second, 4842824839.0, 1.0), AipsError);
        }
        {
            cout << "*** test getNRowsMap()" << endl;
            SubScanKey key;
            key.arrayID = 0;
            key.obsID = 0;
            key.scan = 1;
            key.fieldID = 0;
            std::shared_ptr<const std::map<SubScanKey, rownr_t> > both = md.getNRowMap(MSMetaData::BOTH);
            AlwaysAssert(both->find(key)->second == 367, AipsError);
            std::shared_ptr<const std::map<SubScanKey, rownr_t> > ac = md.getNRowMap(MSMetaData::AUTO);
            AlwaysAssert(ac->find(key)->second == 51, AipsError);
            std::shared_ptr<const std::map<SubScanKey, rownr_t> > xc = md.getNRowMap(MSMetaData::CROSS);
            AlwaysAssert(xc->find(key)->second == 316, AipsError);
        }
        {
            cout << "*** test getFieldCodes()" << endl;
            Vector<String> codes = Vector<String>(md.getFieldCodes());
            AlwaysAssert(allEQ(codes, String("none")), AipsError);
        }
        {
            cout << "*** test getUniqueDataDescIDs()" << endl;
            std::set<uint32_t> ddids = md.getUniqueDataDescIDs();
            Vector<uint32_t> expec = indgen(25, (uint32_t)0, (uint32_t)1);
            AlwaysAssert(
                allEQ(
                    Vector<uint32_t>(vector<uint32_t>(ddids.begin(), ddids.end())), expec
                ), AipsError
            );
        }
        {
            cout << "*** test getUniqueAntennaIDs()" << endl;
            std::set<int32_t> ants = md.getUniqueAntennaIDs();
            int32_t evals[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13};
            Vector<int32_t> expec(vector<int32_t>(evals, evals+13));
            AlwaysAssert(
                allEQ(
                    Vector<int32_t>(vector<int32_t>(ants.begin(), ants.end())), expec
                ), AipsError
            );
        }
        {
            cout << "*** test getFirstExposureTimeMap()" << endl;
            vector<std::map<int32_t, Quantity> > mymap = md.getFirstExposureTimeMap();
            AlwaysAssert(mymap.size() == 25, AipsError);
            for (int32_t i=0; i<25; ++i) {
                uint32_t expSize = 0;
                Quantity expExposure(0, "s");
                if (i == 0) {
                    expSize = 32;
                    expExposure.setValue(1.152);
                }
                else if (i == 1 || i == 3 || i == 5 || i == 7) {
                    expSize = 3;
                    expExposure.setValue(2.016);
                }
                else if (i == 2 || i == 4 || i == 6 || i == 8) {
                    expSize = 3;
                    expExposure.setValue(1.008);
                }
                else if (i == 9 || i == 11 || i == 13 || i == 15) {
                    expSize = 14;
                    expExposure.setValue(2.016);
                }
                else if (i == 10 || i == 12 || i == 14 || i == 16) {
                    expSize = 14;
                    expExposure.setValue(1.008);
                }
                else if (i == 17 || i == 19 || i == 21 || i == 23) {
                    expSize = 15;
                    expExposure.setValue(6.048);
                }
                else if (i == 18 || i == 20 || i == 22 || i == 24) {
                    expSize = 15;
                    expExposure.setValue(1.008);
                }
                AlwaysAssert(mymap[i].size() == expSize, AipsError);
                AlwaysAssert(mymap[i].begin()->second == expExposure, AipsError);
            }
        }
        {
            cout << "*** test getUniqueSpwIDs()" << endl;
            std::set<uint32_t> spws = md.getUniqueSpwIDs();
            Vector<int32_t> expV = casacore::indgen(25, 0, 1);
            std::set<uint32_t> expec(expV.begin(), expV.end());
            AlwaysAssert(spws == expec, AipsError);
        }
        {
            cout << "*** test getSourceTimes()" << endl;
            std::shared_ptr<const Quantum<Vector<double> > > times = md.getSourceTimes();
            Vector<double> v = times->getValue();
            AlwaysAssert(v.size() == 200, AipsError);
            AlwaysAssert(times->getUnit() == "s", AipsError);
            Vector<double> expec(200, 7033098335);
            AlwaysAssert(allNear(v, expec, 1e-10), AipsError);
        }
        {
            cout << "*** test getIntervalStatistics()" << endl;
            MSMetaData::ColumnStats stats = md.getIntervalStatistics();
            AlwaysAssert(near(stats.min, 1.008), AipsError);
            AlwaysAssert(near(stats.max, 6.048), AipsError);
            AlwaysAssert(near(stats.median, 1.008), AipsError);
        }
        {
            cout << "test getTimesForSpws()" << endl;
            std::vector<std::set<double> > vec = md.getTimesForSpws();
            uint32_t n = vec.size();
            AlwaysAssert(n == 40, AipsError);
            uint32_t evals[] = {
                351, 75, 150, 75, 150, 75, 150, 75, 150, 69, 138, 69, 138,
                69, 138, 69, 138, 385, 2310, 385, 2310, 385, 2310, 385, 2310
            };
            std::vector<uint32_t> expec(evals, evals+25);
            for (uint32_t i=0; i<n; ++i) {
                uint32_t esize = i < 25 ? expec[i] : 0;
                AlwaysAssert(vec[i].size() == esize, AipsError);
            }
            double etimes[] = {
                4.842824746560e+09, 4.842824748576e+09, 4.842824750592e+09,
                4.842824752608e+09, 4.842824754624e+09, 4.842824767296e+09,
                4.842824769312e+09, 4.842824771328e+09, 4.842824773344e+09,
                4.842824775360e+09, 4.842824788032e+09, 4.842824790048e+09,
                4.842824792064e+09, 4.842824794080e+09, 4.842824796096e+09,
                4.842824808768e+09, 4.842824810784e+09, 4.842824812800e+09,
                4.842824814816e+09, 4.842824816832e+09, 4.842824829504e+09,
                4.842824831520e+09, 4.842824833536e+09, 4.842824835552e+09,
                4.842824837568e+09, 4.842825778656e+09, 4.842825780672e+09,
                4.842825782688e+09, 4.842825784704e+09, 4.842825786720e+09,
                4.842825799392e+09, 4.842825801408e+09, 4.842825803424e+09,
                4.842825805440e+09, 4.842825807456e+09, 4.842825820128e+09,
                4.842825822144e+09, 4.842825824160e+09, 4.842825826176e+09,
                4.842825828192e+09, 4.842825840864e+09, 4.842825842880e+09,
                4.842825844896e+09, 4.842825846912e+09, 4.842825848928e+09,
                4.842825861600e+09, 4.842825863616e+09, 4.842825865632e+09,
                4.842825867648e+09, 4.842825869664e+09, 4.842826317312e+09,
                4.842826319328e+09, 4.842826321344e+09, 4.842826323360e+09,
                4.842826325376e+09, 4.842826338048e+09, 4.842826340064e+09,
                4.842826342080e+09, 4.842826344096e+09, 4.842826346112e+09,
                4.842826358784e+09, 4.842826360800e+09, 4.842826362816e+09,
                4.842826364832e+09, 4.842826366848e+09, 4.842826379520e+09,
                4.842826381536e+09, 4.842826383552e+09, 4.842826385568e+09,
                4.842826387584e+09, 4.842826400256e+09, 4.842826402272e+09,
                4.842826404288e+09, 4.842826406304e+09, 4.842826408320e+09
            };
            std::vector<double> expectimes(etimes, etimes+75);
            AlwaysAssert(allNearAbs(vec[1], expectimes, 1e-6), AipsError);

        }
        {
            cout << "*** cache size " << md.getCache() << endl;
        }
    }
}

int main() {
    try {
        String *parts = new String[2];
        split(EnvironmentVariable::get("CASAPATH"), parts, 2, String(" "));
        String datadir = parts[0] + "/data/";
        delete [] parts;
        casacore::MeasurementSet ms(datadir + "regression/unittest/MSMetaData/MSMetaData.ms");
        cout << "*** test on-demand constructor" << endl;
        MSMetaData md1(&ms, 100);
        cout << "*** cache size " << md1.getCache() << endl;

        testIt(md1);
        // test after everything is cached
        testIt(md1);
        // test using no cache
        MSMetaData md2(&ms, 0);
        testIt(md2);
        AlwaysAssert(md2.getCache() == 0, AipsError);
        cout << "OK" << endl;
    } 
    catch (const std::exception& x) {
        cerr << "Exception : " << x.what() << endl;
        return 1;
    }
    return 0;
}
