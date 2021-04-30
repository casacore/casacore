//# tMSIter.cc : this program tests the MSIter class
//# Copyright (C) 2010
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

#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSIter.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MeasurementSets/MSFieldColumns.h>
#include <casacore/ms/MeasurementSets/MSDataDescColumns.h>
#include <casacore/ms/MeasurementSets/MSSpWindowColumns.h>
#include <casacore/ms/MeasurementSets/MSPolColumns.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/ms/MeasurementSets/MSFeedColumns.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <iostream>
#include <sstream>

using namespace casacore;
using namespace std;

void createMS (int nant, int ntime, double msinterval)
{
  // Create the MS (with DATA column) and its subtables.
  TableDesc td (MS::requiredTableDesc());
  MS::addColumnToDesc(td, MS::DATA, 2);
  SetupNewTable newtab("tMSIter_tmp.ms", td, Table::New);
  MeasurementSet ms(newtab);
  ms.createDefaultSubtables(Table::New);
  // Write main table with columns needed for MSIter.
  Array<Complex> data(IPosition(2,4,8));
  indgen (data);
  MSColumns mscols(ms);
  uInt rownr = 0;
  for (int it=0; it<ntime; ++it) {
    for (int i1=0; i1<nant; ++i1) {
      for (int i2=i1; i2<nant; ++i2) {
        ms.addRow();
        mscols.time().put (rownr, 1e9 + msinterval*(it+0.5));
        mscols.interval().put (rownr, msinterval);
        mscols.antenna1().put (rownr, i1);
        mscols.antenna2().put (rownr, i2);
        mscols.arrayId().put (rownr, 0);
        mscols.dataDescId().put (rownr, 0);
        mscols.fieldId().put (rownr, 0);
        mscols.data().put (rownr, data);
        data += Complex(float(data.size()), 0.);
        ++rownr;
      }
    }
  }
  // Write subtables needed for MSIter.
  ms.field().addRow(1);
  MSFieldColumns fieldcols(ms.field());
  Array<double> dir(IPosition(2,2,1));
  dir.data()[0] = 1.1; dir.data()[1] = 1.5;
  fieldcols.delayDir().put (0, dir);
  fieldcols.phaseDir().put (0, dir);
  fieldcols.name().put (0, "TESTFIELD");
  fieldcols.sourceId().put (0, 0);

  ms.dataDescription().addRow(1);
  MSDataDescColumns ddcols(ms.dataDescription());
  ddcols.spectralWindowId().put (0, 0);
  ddcols.polarizationId().put (0, 0);

  ms.spectralWindow().addRow(1);
  MSSpWindowColumns spwcols(ms.spectralWindow());
  Vector<Double>freqs(8);
  indgen (freqs, 1e9, 1e6);
  spwcols.chanFreq().put (0, freqs);

  ms.polarization().addRow(1);
  MSPolarizationColumns polcols(ms.polarization());
  Vector<Int> corrTypes(2, 0);
  corrTypes[1] = 1;
  polcols.numCorr().put (0, 4);
  polcols.corrType().put (0, corrTypes);

  ms.antenna().addRow(nant);
  MSAntennaColumns antcols(ms.antenna());
  Vector<double> pos(3);
  indgen (pos, 6.4e6, 1e3);
  for (int i=0; i<nant; ++i) {
    antcols.mount().put (i, "equatorial");
    antcols.position().put (i, pos);
    pos += 10.;
  }

  ms.feed().addRow(nant);
  MSFeedColumns feedcols(ms.feed());
  for (int i=0; i<nant; ++i) {
    feedcols.antennaId().put (i, i);
    feedcols.feedId().put (i, i);
    feedcols.spectralWindowId().put (i, 0);
    feedcols.time().put (i, 60.);
    feedcols.interval().put (i, 0.);    // no time dependence
    feedcols.numReceptors().put (i, 2);
    feedcols.beamOffset().put (i, Array<Double>(IPosition(2,2,2),0.));
    feedcols.receptorAngle().put (i, Vector<Double>(2,0.));
    feedcols.polResponse().put (i, Array<Complex>(IPosition(2,2,2)));
  }
}

void iterMS (double binwidth)
{
  MeasurementSet ms("tMSIter_tmp.ms");
  Block<int> sort(2);
  sort[0] = MS::ANTENNA1;
  sort[1] = MS::ANTENNA2;
  MSIter msIter(ms, sort, binwidth, False);
  for (msIter.origin(); msIter.more(); msIter++) {
    cout << "nrow=" << msIter.table().nrow()
         << " a1=" << ScalarColumn<Int>(msIter.table(), "ANTENNA1")(0)
         << " a2=" << ScalarColumn<Int>(msIter.table(), "ANTENNA2")(0)
         << " time="
         << ScalarColumn<double>(msIter.table(), "TIME").getColumn() - 1e9
         << " keyCh=" << msIter.keyChange()
         << endl;
  }
}

void iterMSMemory (double binwidth)
{
  MeasurementSet ms("tMSIter_tmp.ms");
  Block<int> sort(2);
  sort[0] = MS::ANTENNA1;
  sort[1] = MS::ANTENNA2;
  MSIter msIter(ms, sort, binwidth, False, False); // Use stored table in memory
  for (msIter.origin(); msIter.more(); msIter++) {
    cout << "nrow=" << msIter.table().nrow()
         << " a1=" << ScalarColumn<Int>(msIter.table(), "ANTENNA1")(0)
         << " a2=" << ScalarColumn<Int>(msIter.table(), "ANTENNA2")(0)
         << " time="
         << ScalarColumn<double>(msIter.table(), "TIME").getColumn() - 1e9
         << endl;
  }
}


void iter2MS (double binwidth)
{
  MeasurementSet ms("tMSIter_tmp.ms");
  Block<int> sort(2);
  sort[0] = MS::ANTENNA1;
  sort[1] = MS::ANTENNA2;
  MSIter msIter(ms, sort, binwidth, False);
  unsigned i = 0;
  for (msIter.origin(); msIter.more(); ++msIter) {
    cout << "nrow=" << msIter.table().nrow()
         << " a1=" << ScalarColumn<Int>(msIter.table(), "ANTENNA1")(0)
         << " a2=" << ScalarColumn<Int>(msIter.table(), "ANTENNA2")(0)
         << " time="
         << ScalarColumn<double>(msIter.table(), "TIME").getColumn() - 1e9
         << " keyCh=" << msIter.keyChange()
         << endl;
    if (++i == 4) {
      cout << "=====" << endl;
      MSIter msIter1 = msIter;
      for (msIter1.origin(); msIter1.more(); ++msIter1) {
        cout << "nrow=" << msIter1.table().nrow()
             << " a1=" << ScalarColumn<Int>(msIter1.table(), "ANTENNA1")(0)
             << " a2=" << ScalarColumn<Int>(msIter1.table(), "ANTENNA2")(0)
             << " time="
             << ScalarColumn<double>(msIter1.table(), "TIME").getColumn() - 1e9
             << " keyCh=" << msIter1.keyChange()
             << endl;
      }
      cout << "=====" << endl;
    }
  }
  cout << "=====" << endl;
}

void iter2MSMemory (double binwidth)
{
  MeasurementSet ms("tMSIter_tmp.ms");
  Block<int> sort(2);
  sort[0] = MS::ANTENNA1;
  sort[1] = MS::ANTENNA2;
  MSIter msIter(ms, sort, binwidth, False, False); // Use stored table in memory
  MSIter msIter1(ms, sort, binwidth, False, False);
  MSIter *it = &msIter;
  unsigned i = 0;
  for (it->origin(); it->more(); ++(*it)) {
    cout << "nrow=" << it->table().nrow()
         << " a1=" << ScalarColumn<Int>(it->table(), "ANTENNA1")(0)
         << " a2=" << ScalarColumn<Int>(it->table(), "ANTENNA2")(0)
         << " time="
         << ScalarColumn<double>(it->table(), "TIME").getColumn() - 1e9
         << endl;
    if (++i % 2 == 1) {
      msIter1 = msIter;
      it = &msIter1;
    } else {
      msIter = msIter1;
      it = &msIter;
    }
  }
}

// This test exercises the generic sorting function constructor
// with a trivial comparison which always compares two values equal.
// The result is that all the rows are grouped together.
void iterMSGenericSortFuncAlwaysTrue ()
{
  MeasurementSet ms("tMSIter_tmp.ms");
  CountedPtr<BaseCompare> alwaysTrue(new CompareAlwaysTrue());
  std::vector<std::pair<String, CountedPtr<BaseCompare>>> sortCols;
  sortCols.push_back(std::make_pair("ANTENNA1", alwaysTrue));
  MSIter msIter(ms, sortCols);
  size_t niter = 0;
  for (msIter.origin(); msIter.more(); msIter++) {
    cout << "nrow=" << msIter.table().nrow()<<endl;
    niter++;
  }
  AlwaysAssertExit(niter == 1);
}

class CompareAntennaGrouping : public BaseCompare
{
public:
  virtual ~CompareAntennaGrouping()
  {
  }

  // Comparison function that groups together antenna 0 and 1
  // and antenna 2 on a different group
  virtual int comp(const void * obj1, const void * obj2) const
  {
    const Int& v1 = *static_cast<const Int*>(obj1);
    const Int& v2 = *static_cast<const Int*>(obj2);
    double v1_c, v2_c;
    if( v1 == 0 || v1 == 1)
      v1_c = 0.5;
    else
      v1_c = v1;
    if( v2 == 0 || v2 == 1)
      v2_c = 0.5;
    else
      v2_c = v2;
    return (v1_c == v2_c ? 0 : (v1_c < v2_c ? -1 : 1));
  }
};

// This test exercises the generic sorting function constructor
// with a comparison function that groups together antennas 0 and 1
// and leaves antenna 2 in a different group
void iterMSGenericSortFuncAntennaGrouping ()
{
  MeasurementSet ms("tMSIter_tmp.ms");
  CountedPtr<BaseCompare> antennaCluster(new CompareAntennaGrouping());
  std::vector<std::pair<String, CountedPtr<BaseCompare>>> sortCols;
  sortCols.push_back(std::make_pair("ANTENNA1", antennaCluster));
  MSIter msIter(ms, sortCols);
  size_t nAnt01 = 0;
  size_t nAnt2 = 0;
  for (msIter.origin(); msIter.more(); msIter++) {
    cout << "nrow=" << msIter.table().nrow()<<endl;
    Int antenna =  ScalarColumn<Int>(msIter.table(), "ANTENNA1")(0);
    if(antenna == 0 || antenna == 1)
      nAnt01 += msIter.table().nrow();
    else
      nAnt2 += msIter.table().nrow();
  }
  AlwaysAssertExit(nAnt01 == 25);
  AlwaysAssertExit(nAnt2 == 5);
}

void createMSSeveralDD (int nant, int ntime, int ndd, double msinterval)
{
  // Create the MS (with DATA column) and its subtables.
  TableDesc td (MS::requiredTableDesc());
  MS::addColumnToDesc(td, MS::DATA, 2);
  SetupNewTable newtab("tMSIter_severaldd_tmp.ms", td, Table::New);
  MeasurementSet ms(newtab);
  ms.createDefaultSubtables(Table::New);
  // Write main table with columns needed for MSIter.
  Array<Complex> data(IPosition(2,4,8));
  indgen (data);
  MSColumns mscols(ms);
  uInt rownr = 0;
  for (int it=0; it<ntime; ++it) {
    for (int ddid=0; ddid<ndd; ++ddid) {
      for (int i1=0; i1<nant; ++i1) {
        for (int i2=i1; i2<nant; ++i2) {
          ms.addRow();
          mscols.time().put (rownr, 1e9 + msinterval*(it+0.5));
          mscols.interval().put (rownr, msinterval);
          mscols.antenna1().put (rownr, i1);
          mscols.antenna2().put (rownr, i2);
          mscols.feed1().put (rownr, i1);
          mscols.feed2().put (rownr, i2);
          mscols.arrayId().put (rownr, 0);
          mscols.dataDescId().put (rownr, ddid);
          mscols.fieldId().put (rownr, 0);
          mscols.data().put (rownr, data);
          data += Complex(float(data.size()), 0.);
          ++rownr;
        }
      }
    }
  }
  // Write subtables needed for MSIter.
  ms.field().addRow(1);
  MSFieldColumns fieldcols(ms.field());
  Array<double> dir(IPosition(2,2,1));
  dir.data()[0] = 1.1; dir.data()[1] = 1.5;
  fieldcols.delayDir().put (0, dir);
  fieldcols.phaseDir().put (0, dir);
  fieldcols.name().put (0, "TESTFIELD");
  fieldcols.sourceId().put (0, 0);

  ms.dataDescription().addRow(ndd);
  MSDataDescColumns ddcols(ms.dataDescription());
  for (int ddid=0; ddid<ndd; ++ddid) {
    ddcols.spectralWindowId().put (ddid, ddid);
    ddcols.polarizationId().put (ddid, ddid%2);
  }

  ms.spectralWindow().addRow(ndd);
  MSSpWindowColumns spwcols(ms.spectralWindow());
  Vector<Double>freqs(8);
  indgen (freqs, 1e9, 1e6);
  for (int ddid=0; ddid<ndd; ++ddid) {
    spwcols.chanFreq().put (ddid, freqs + ddid * 1e7);
  }

  ms.polarization().addRow(2);
  MSPolarizationColumns polcols(ms.polarization());
  Vector<Int> corrTypes1(2, 0);
  corrTypes1[1] = 1;
  polcols.numCorr().put (0, 4);
  polcols.corrType().put (0, corrTypes1);
  Vector<Int> corrTypes2(2, 0);
  corrTypes2[0] = Stokes::XX;
  corrTypes2[1] = Stokes::YY;
  polcols.numCorr().put (1, 4);
  polcols.corrType().put (1, corrTypes2);

  ms.antenna().addRow(nant);
  MSAntennaColumns antcols(ms.antenna());
  Vector<double> pos(3);
  indgen (pos, 6.4e6, 1e3);
  for (int i=0; i<nant; ++i) {
    antcols.mount().put (i, "equatorial");
    antcols.position().put (i, pos);
    pos += 10.;
  }

  ms.feed().addRow(nant*ndd);
  MSFeedColumns feedcols(ms.feed());
  for (int ddid=0; ddid<ndd; ++ddid) {
    for (int i=0; i<nant; ++i) {
      feedcols.antennaId().put (i + ddid * nant, i);
      feedcols.feedId().put (i + ddid * nant, 0); // no feed-arrays, one single feed
      feedcols.spectralWindowId().put (i + ddid * nant, ddid);
      feedcols.time().put (i + ddid * nant, 60.);
      feedcols.interval().put (i + ddid * nant, 0.);    // no time dependence
      feedcols.numReceptors().put (i + ddid * nant, 2);
      feedcols.beamOffset().put (i + ddid * nant, Array<Double>(IPosition(2,2,2), i*0.01+ddid*0.1));
      feedcols.receptorAngle().put (i + ddid * nant, Vector<Double>(2, i*0.01+ddid*0.1));
      feedcols.polResponse().put (i + ddid * nant, Array<Complex>(IPosition(2,2,2), Complex(i*0.01+ddid*0.1, i*0.01+ddid*0.1)));
    }
  }
}

// This test exercises the lazy caching mechanism for retrieving 
// metadata related to DD and FEED.
void iterMSCachedDDFeedInfo ()
{
  // Create synthetic MS with several DDIds
  int nAnt = 5;
  int nTime = 5;
  int nDD = 5;
  double msinterval = 60.;
  createMSSeveralDD(nAnt, nTime, nDD, msinterval);
  MeasurementSet ms("tMSIter_severaldd_tmp.ms");

  // Create a MSIter with DDID in the sorting columns
  // There will be as many iterations as DDIDs. 
  // This is done both for the traditional constructor as for the 
  // constructor with the generic sorting definition.
  cout << "Iteration with DDID sorting" << endl;
  cout << "===========================" << endl;
  for(int useGenericSortCons = 0 ; useGenericSortCons < 2 ; useGenericSortCons++)
  {
    std::unique_ptr<MSIter> msIter;
    if(useGenericSortCons)
    {
      // Use constructor with generic sorting definitions
      std::vector<std::pair<String, CountedPtr<BaseCompare>>> sortCols;
      sortCols.push_back(std::make_pair("DATA_DESC_ID", nullptr));
      msIter.reset(new MSIter(ms, sortCols));
    }
    else 
    {
      // Use traditional constructor
      Block<int> sort(1);
      sort[0] = MS::DATA_DESC_ID;
      msIter.reset(new MSIter(ms, sort, 0, False, False)); // Use stored table in memory
    }

    // Set the expected DD metadata in the first iteration
    int expectedDDId = 0;
    int expectedSPWId = 0;
    int expectedPolId = 0;
    int expectedPolFrame = 0;
    Vector<Double>expectedFreqs(8);
    indgen (expectedFreqs, 1e9, 1e6);

    // Set the expected Feed metadata in the first iteration
    int nReceptors = 2;
    int nFeed = 1;
    Cube<Double> expectedReceptorAngles;
    Cube<RigidVector<Double,2>> expectedBeamOffsets;
    Matrix<Complex> expectedPartialCJones(IPosition(2,2));
    expectedPartialCJones = 0;
    expectedReceptorAngles.resize(nReceptors, nAnt, nFeed);
    expectedBeamOffsets.resize(nReceptors, nAnt, nFeed);
    for(int iAnt = 0; iAnt < nAnt; iAnt++)
    {
      expectedReceptorAngles(0, iAnt, 0) = iAnt*0.01;
      expectedReceptorAngles(1, iAnt, 0) = iAnt*0.01;
      expectedBeamOffsets(0, iAnt, 0) = RigidVector<Double, 2>(iAnt*0.01);
      expectedBeamOffsets(1, iAnt, 0) = RigidVector<Double, 2>(iAnt*0.01);
    }

    // Iterate the MS
    // It is expected that in each iteration a new DDId is retrieved
    for (msIter->origin(); msIter->more(); (*msIter)++) 
    {
      cout << "nrow=" << msIter->table().nrow()<<endl;
      cout << "ddid = " << msIter->dataDescriptionId() << 
              " spwid = " << msIter->spectralWindowId() <<
              " polid = " << msIter->polarizationId() << endl;
      cout << "freqs = " << msIter->frequency() << " freq0 " << msIter->frequency0() << endl;

      // Check that the DD related metadata is what we expect
      AlwaysAssertExit(msIter->dataDescriptionId() == expectedDDId);
      AlwaysAssertExit(msIter->spectralWindowId() == expectedSPWId);
      AlwaysAssertExit(msIter->polarizationId() == expectedPolId);
      AlwaysAssertExit(msIter->polFrame() == expectedPolFrame);
      AlwaysAssertExit(allEQ(msIter->frequency(), expectedFreqs));
      for (size_t ddrow = 0 ; ddrow < msIter->table().nrow() ; ddrow++) {
        AlwaysAssertExit(msIter->colDataDescriptionIds()(ddrow) == expectedDDId);
      }
      Unit Hz(String("Hz"));
      AlwaysAssertExit(msIter->frequency0().get(Hz).getValue() == expectedFreqs[0]);

      // Check that the Feed related metadata is what we expect
      AlwaysAssertExit(allNear(msIter->receptorAngles(), expectedReceptorAngles, 1e-6));
      AlwaysAssertExit(allNear(msIter->receptorAngle(), expectedReceptorAngles[0], 1e-6));
      // Need to unwrap the loop because RigidVector doesn't have a comparison operator
      for(int iRec = 0 ; iRec < nReceptors; ++iRec)
      {
        for(int iAnt = 0; iAnt < nAnt; iAnt++)
        {
          for(int iFeed = 0; iFeed < nFeed; iFeed++)
          {
            AlwaysAssertExit(std::abs(msIter->getBeamOffsets()(iRec, iAnt, iFeed)(0) - expectedBeamOffsets(iRec, iAnt, iFeed)(0)) < 1e-6);
            AlwaysAssertExit(std::abs(msIter->getBeamOffsets()(iRec, iAnt, iFeed)(1) - expectedBeamOffsets(iRec, iAnt, iFeed)(1)) < 1e-6);
          }
        }
      }
      // Need to unwrap the loop because SquareMatrix doesn't have a comparison operator
      for(int iAnt = 0; iAnt < nAnt; iAnt++)
      {
        for(int iFeed = 0; iFeed < nFeed; iFeed++)
        {
          AlwaysAssertExit(allNear(msIter->CJonesAll()(iAnt, iFeed).matrix(), expectedPartialCJones + Complex(iAnt * 0.01, iAnt * 0.01), 1e-6));
        }
      }

      // Update the expected values for next iteration
      expectedDDId++;
      expectedSPWId++;
      expectedPolId = expectedDDId % 2;
      expectedPolFrame = expectedDDId % 2;
      expectedFreqs += 1e7;
      expectedReceptorAngles += 0.1;
      expectedBeamOffsets += RigidVector<Double,2>(0.1);
      expectedPartialCJones += Complex(0.1, 0.1);
    }
  }

  // Create a MSIter with DDID in the sorting columns
  // but not the fastest one. There will be as many iterations
  // as DDIds times the groups in the other sortign column.
  // Antenna1 has been choosen as the other one.
  // This is done both for the traditional constructor as for the
  // constructor with the generic sorting definition.
  cout << "Iteration with ANTENNA1 and DDID sorting" << endl;
  cout << "========================================" << endl;
  for(int useGenericSortCons = 0 ; useGenericSortCons < 2 ; useGenericSortCons++)
  {
    std::unique_ptr<MSIter> msIter;
    if(useGenericSortCons)
    {
      // Use constructor with generic sorting definitions
      std::vector<std::pair<String, CountedPtr<BaseCompare>>> sortCols;
      sortCols.push_back(std::make_pair("DATA_DESC_ID", nullptr));
      sortCols.push_back(std::make_pair("ANTENNA1", nullptr));
      msIter.reset(new MSIter(ms, sortCols));
    }
    else
    {
      // Use traditional constructor
      Block<int> sort(2);
      sort[0] = MS::DATA_DESC_ID;
      sort[1] = MS::ANTENNA1;
      msIter.reset(new MSIter(ms, sort, 0, False, False)); // Use stored table in memory
    }

    // Set the expected DD metadata in the first iteration
    int expectedDDId = 0;
    int expectedSPWId = 0;
    int expectedPolId = 0;
    int expectedPolFrame = 0;
    bool newSpw = true;
    bool newPol = true;
    bool newDD = true;
    Vector<Double>expectedFreqs(8);
    indgen (expectedFreqs, 1e9, 1e6);

    // Set the expected Feed metadata in the first iteration
    int nReceptors = 2;
    int nFeed = 1;
    Cube<Double> expectedReceptorAngles;
    Cube<RigidVector<Double,2>> expectedBeamOffsets;
    Matrix<Complex> expectedPartialCJones(IPosition(2,2));
    expectedPartialCJones = 0;
    expectedReceptorAngles.resize(nReceptors, nAnt, nFeed);
    expectedBeamOffsets.resize(nReceptors, nAnt, nFeed);
    for(int iAnt = 0; iAnt < nAnt; iAnt++)
    {
      expectedReceptorAngles(0, iAnt, 0) = iAnt*0.01;
      expectedReceptorAngles(1, iAnt, 0) = iAnt*0.01;
      expectedBeamOffsets(0, iAnt, 0) = RigidVector<Double, 2>(iAnt*0.01);
      expectedBeamOffsets(1, iAnt, 0) = RigidVector<Double, 2>(iAnt*0.01);
    }

    // Iterate the MS
    // It is expected that in each iteration antenna1 index runs faster
    // than DDId
    int antena1Idx = 0;
    for (msIter->origin(); msIter->more(); (*msIter)++)
    {
      cout << "nrow=" << msIter->table().nrow()<<endl;
      cout << "ddid = " << msIter->dataDescriptionId() <<
              " spwid = " << msIter->spectralWindowId() <<
              " polid = " << msIter->polarizationId() << endl;
      cout << "freqs = " << msIter->frequency() << " freq0 " << msIter->frequency0() << endl;

      // Check that the DD related metadata is what we expect
      AlwaysAssertExit(msIter->dataDescriptionId() == expectedDDId);
      AlwaysAssertExit(msIter->newDataDescriptionId() == newDD);
      AlwaysAssertExit(msIter->spectralWindowId() == expectedSPWId);
      AlwaysAssertExit(msIter->newSpectralWindow() == newSpw);
      AlwaysAssertExit(msIter->polarizationId() == expectedPolId);
      AlwaysAssertExit(msIter->newPolarizationId() == newPol);
      AlwaysAssertExit(msIter->polFrame() == expectedPolFrame);
      AlwaysAssertExit(allEQ(msIter->frequency(), expectedFreqs));
      for (size_t ddrow = 0 ; ddrow < msIter->table().nrow() ; ddrow++) {
        AlwaysAssertExit(msIter->colDataDescriptionIds()(ddrow) == expectedDDId);
      }
      Unit Hz(String("Hz"));
      AlwaysAssertExit(msIter->frequency0().get(Hz).getValue() == expectedFreqs[0]);

      // Check that the Feed related metadata is what we expect
      AlwaysAssertExit(allNear(msIter->receptorAngles(), expectedReceptorAngles, 1e-6));
      AlwaysAssertExit(allNear(msIter->receptorAngle(), expectedReceptorAngles[0], 1e-6));
      for(int iRec = 0 ; iRec < nReceptors; ++iRec)
      {
        for(int iAnt = 0; iAnt < nAnt; iAnt++)
        {
          for(int iFeed = 0; iFeed < nFeed; iFeed++)
          {
            AlwaysAssertExit(std::abs(msIter->getBeamOffsets()(iRec, iAnt, iFeed)(0) - expectedBeamOffsets(iRec, iAnt, iFeed)(0)) < 1e-6);
            AlwaysAssertExit(std::abs(msIter->getBeamOffsets()(iRec, iAnt, iFeed)(1) - expectedBeamOffsets(iRec, iAnt, iFeed)(1)) < 1e-6);
          }
        }
      }
      // Need to unwrap the loop because SquareMatrix doesn't have a comparison operator
      for(int iAnt = 0; iAnt < nAnt; iAnt++)
      {
        for(int iFeed = 0; iFeed < nFeed; iFeed++)
        {
          AlwaysAssertExit(allNear(msIter->CJonesAll()(iAnt, iFeed).matrix(), expectedPartialCJones + Complex(iAnt * 0.01, iAnt * 0.01), 1e-6));
        }
      }

      // Update the expected values for next iteration
      antena1Idx++;
      if(antena1Idx > nAnt - 1)
      { // All ANTENNA1 iterations done, now iterate on a new DD
        expectedDDId++;
        expectedSPWId++;
        expectedPolId = expectedDDId % 2;
        expectedPolFrame = expectedDDId % 2;
        expectedFreqs += 1e7;
        expectedReceptorAngles += 0.1;
        expectedBeamOffsets += RigidVector<Double,2>(0.1);
        expectedPartialCJones += Complex(0.1, 0.1);
        antena1Idx = 0;
        newSpw = true;
        newPol = true;
        newDD = true;
      }
      else
      { // Still iterating on ANTENNA1
        newSpw = false;
        newPol = false;
        newDD = false;
      }
    }
  }

  // Create a MSIter without DDID in the sorting columns
  // Sorting is done per timestamp. For each timestamp
  // all baselines and all DDIds are present
  // This is done both for the traditional constructor as for the 
  // constructor with the generic sorting definition.
  cout << "Iteration with TIME sorting" << endl;
  cout << "===========================" << endl;
  for(int useGenericSortCons = 0 ; useGenericSortCons < 2 ; useGenericSortCons++)
  {
    std::unique_ptr<MSIter> msIter;
    if(useGenericSortCons)
    {
      // Use constructor with generic sorting definitions
      std::vector<std::pair<String, CountedPtr<BaseCompare>>> sortCols;
      sortCols.push_back(std::make_pair("TIME", nullptr));
      msIter.reset(new MSIter(ms, sortCols));
    }
    else
    {
      // Use traditional constructor
      Block<int> sort(1);
      sort[0] = MS::TIME;
      msIter.reset(new MSIter(ms, sort, 1., False, False)); // Use stored table in memory
    }

    // Set the expected DD metadata in the first iteration
    Vector<Double>expectedFreqs(8);
    indgen (expectedFreqs, 1e9, 1e6);

    // Set the expected Feed metadata in the first iteration
    int nReceptors = 2;
    int nFeed = 1;
    Cube<Double> expectedReceptorAngles;
    Cube<RigidVector<Double,2>> expectedBeamOffsets;
    Matrix<Complex> expectedPartialCJones(IPosition(2,2));
    expectedPartialCJones = 0;
    expectedReceptorAngles.resize(nReceptors, nAnt, nFeed);
    expectedBeamOffsets.resize(nReceptors, nAnt, nFeed);
    for(int iAnt = 0; iAnt < nAnt; iAnt++)
    {
      expectedReceptorAngles(0, iAnt, 0) = iAnt*0.01;
      expectedReceptorAngles(1, iAnt, 0) = iAnt*0.01;
      expectedBeamOffsets(0, iAnt, 0) = RigidVector<Double, 2>(iAnt*0.01);
      expectedBeamOffsets(1, iAnt, 0) = RigidVector<Double, 2>(iAnt*0.01);
    }

    // Do not read the DD information for each iteration, but skip iterSkip
    // iterations before reading it. That should exercise the logic for the
    // lazy computation of the DD
    for (int iterSkip = 5; iterSkip >=0; iterSkip--)
    {
      // Iterate the MS
      // It is expected that in each iteration a single timestamp is retrieved,
      // which contains all DDIds
      int rowsToSkip = iterSkip;
      for (msIter->origin(); msIter->more(); (*msIter)++)
      {
        if(rowsToSkip == 0)
        {
          cout << "nrow=" << msIter->table().nrow()<<endl;
          cout << "ddid = " << msIter->dataDescriptionId() <<
                  " spwid = " << msIter->spectralWindowId() <<
                  " polid = " << msIter->polarizationId() << endl;
          cout << "freqs = " << msIter->frequency() << " freq0 " << msIter->frequency0() << endl;
          // Check that the DD related metadata is what we expect
          // For DDId, SPWId, polID: since there is no unique ID in this iteration
          // the first one (0) is returned
          AlwaysAssertExit(msIter->dataDescriptionId() == 0);
          AlwaysAssertExit(msIter->spectralWindowId() == 0);
          AlwaysAssertExit(msIter->polarizationId() == 0);
          AlwaysAssertExit(msIter->polFrame() == 0);
          AlwaysAssertExit(allEQ(msIter->frequency(), expectedFreqs));
          for (size_t ddrow = 0 ; ddrow < msIter->table().nrow() ; ddrow++) {
            // Every 15 rows (number of baselines) a new DDId appears
            AlwaysAssertExit(msIter->colDataDescriptionIds()(ddrow) == (int)(ddrow / 15));
          }
          Unit Hz(String("Hz"));
          //Frequency also refers to the first SPW
          AlwaysAssertExit(msIter->frequency0().get(Hz).getValue() == expectedFreqs[0]);

          // Check that the Feed related metadata is what we expect
          // Note that these are computed assuming there is a single SPW
          // in the iteration, so they will return values corresponding
          // to the first SPW, DDId present.
          AlwaysAssertExit(allNear(msIter->receptorAngles(), expectedReceptorAngles, 1e-6));
          AlwaysAssertExit(allNear(msIter->receptorAngle(), expectedReceptorAngles[0], 1e-6));
          for(int iRec = 0 ; iRec < nReceptors; ++iRec)
          {
            for(int iAnt = 0; iAnt < nAnt; iAnt++)
            {
              for(int iFeed = 0; iFeed < nFeed; iFeed++)
              {
                AlwaysAssertExit(std::abs(msIter->getBeamOffsets()(iRec, iAnt, iFeed)(0) - expectedBeamOffsets(iRec, iAnt, iFeed)(0)) < 1e-6);
                AlwaysAssertExit(std::abs(msIter->getBeamOffsets()(iRec, iAnt, iFeed)(1) - expectedBeamOffsets(iRec, iAnt, iFeed)(1)) < 1e-6);
              }
            }
          }
          // Need to unwrap the loop because SquareMatrix doesn't have a comparison operator
          for(int iAnt = 0; iAnt < nAnt; iAnt++)
          {
            for(int iFeed = 0; iFeed < nFeed; iFeed++)
            {
              AlwaysAssertExit(allNear(msIter->CJonesAll()(iAnt, iFeed).matrix(), expectedPartialCJones + Complex(iAnt * 0.01, iAnt * 0.01), 1e-6));
            }
          }

          rowsToSkip = iterSkip;
        }
        else
        {
          cout << " Skipping row" << endl;
          rowsToSkip--;
        }
      }
    }
  }

}

int main (int argc, char* argv[])
{
  try {
    int nant = 3;
    int ntime = 5;
    double msinterval = 60.;
    double binwidth = 120.;
    if (argc > 1) {
      istringstream iss(argv[1]);
      iss >> nant;
    }
    if (argc > 2) {
      istringstream iss(argv[2]);
      iss >> ntime;
    }
    if (argc > 3) {
      istringstream iss(argv[3]);
      iss >> msinterval;
    }
    if (argc > 4) {
      istringstream iss(argv[4]);
      iss >> binwidth;
    }
    createMS(nant, ntime, msinterval);
    iterMS(binwidth);
    cout << "########" << endl;
    iterMSMemory(binwidth);
    cout << "########" << endl;
    iter2MS(binwidth);
    cout << "########" << endl;
    iter2MSMemory(binwidth);
    cout << "########" << endl;
    iterMSGenericSortFuncAlwaysTrue();
    cout << "########" << endl;
    iterMSGenericSortFuncAntennaGrouping();
    cout << "########" << endl;
    iterMSCachedDDFeedInfo();
  } catch (std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
