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
#include <casacore/casa/Arrays/ArrayIO.h>
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
         << " a1=" << ROScalarColumn<Int>(msIter.table(), "ANTENNA1")(0)
         << " a2=" << ROScalarColumn<Int>(msIter.table(), "ANTENNA2")(0)
         << " time="
         << ROScalarColumn<double>(msIter.table(), "TIME").getColumn() - 1e9
         << endl;
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
    iterMS (binwidth);
  } catch (std::exception& x) {
    cerr << "Unexpected exception: " << x.what() << endl;
    return 1;
  } 
  return 0;
}
