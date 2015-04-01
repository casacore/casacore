//# tDerivedMSCal.h: Test program for class DerivedMSCal
//# Copyright (C) 2010
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
//# $Id$

#include <casacore/derivedmscal/DerivedMC/DerivedMSCal.h>
#include <casacore/ms/MSOper/MSDerivedValues.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSMainColumns.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/OS/Timer.h>
#include <iostream>

using namespace casacore;
using namespace std;

void check (MSDerivedValues& mdv,
            uInt rownr,
            ScalarColumn<double>& ha,
            ScalarColumn<double>& last)
{
  double mha = mdv.hourAngle();
  double tha = ha(rownr);
  AlwaysAssertExit (near(mha, tha, 1e-10));
  double mlast = mdv.last().getValue().get();
  double tlast = last(rownr);
  AlwaysAssertExit (near(mlast, tlast, 1e-10));
}

void check (MSDerivedValues& mdv,
            uInt rownr,
            ScalarColumn<double>& ha,
            ScalarColumn<double>& last,
            ScalarColumn<double>& pa,
            ArrayColumn<double>& azel)
{
  check (mdv, rownr, ha, last);
  double mpa = mdv.parAngle();
  double tpa = pa(rownr);
  AlwaysAssertExit (near(mpa, tpa, 1e-10));
  Vector<double> mazel = mdv.azel().getValue().get();
  Vector<double> tazel = azel(rownr);
  AlwaysAssertExit (allNear(mazel, tazel, 1e-10));
}

void check (uInt rownr,
            ArrayColumn<Double>& uvw,
            ArrayColumn<Double>& uvwJ2000)
{
  if (uvw.isNull()) {
    AlwaysAssertExit (allEQ (uvwJ2000(rownr), 0.));
  } else {
    if (! allNear (uvwJ2000(rownr), uvw(rownr), 1e-7)) {
      cout <<"UVW diff "<< uvwJ2000(rownr)<< uvw(rownr) << endl;
    }
    AlwaysAssertExit (allNear (uvwJ2000(rownr), uvw(rownr), 1e-5));
  }
}

int main(int argc, char* argv[])
{
  try {
    DerivedMSCal::registerClass();
    if (argc <= 1) {
      cout << "Run as:   tDerivedMSCal msname/caltablename [checkuvw]" << endl;
      return 3;
    }
    Bool checkUVW = (argc > 2);
    // Copy the input table.
    // Also determine the name of the MS containing ANTENNA, etc.
    String msName ("tDerivedMSCal_tmp.tab");
    {
      Table tab(argv[1]);
      tab.deepCopy ("tDerivedMSCal_tmp.tab", Table::New);
      if (tab.keywordSet().isDefined("CAL_DESC")) {
        msName = ScalarColumn<String> (tab.keywordSet().asTable("CAL_DESC"),
                                         "MS_NAME")(0);
      }
    }
    // Add the columns.
    {
      Table tab ("tDerivedMSCal_tmp.tab", Table::Update);
      TableDesc td;
      td.addColumn (ScalarColumnDesc<double>("HA"));
      td.addColumn (ScalarColumnDesc<double>("HA1"));
      td.addColumn (ScalarColumnDesc<double>("HA2"));
      td.addColumn (ScalarColumnDesc<double>("PA1"));
      td.addColumn (ScalarColumnDesc<double>("PA2"));
      td.addColumn (ScalarColumnDesc<double>("LAST"));
      td.addColumn (ScalarColumnDesc<double>("LAST1"));
      td.addColumn (ScalarColumnDesc<double>("LAST2"));
      td.addColumn (ArrayColumnDesc<double> ("AZEL1"));
      td.addColumn (ArrayColumnDesc<double> ("AZEL2"));
      td.addColumn (ArrayColumnDesc<double> ("UVW_J2000"));
      DerivedMSCal dataMan;
      tab.addColumn (td, dataMan);
    }
    // Loop through all rows and check values.
    MeasurementSet ms (msName);
    Table tab("tDerivedMSCal_tmp.tab");
    ScalarColumn<double> ha(tab, "HA");
    ScalarColumn<double> ha1(tab, "HA1");
    ScalarColumn<double> ha2(tab, "HA2");
    ScalarColumn<double> pa1(tab, "PA1");
    ScalarColumn<double> pa2(tab, "PA2");
    ScalarColumn<double> last(tab, "LAST");
    ScalarColumn<double> last1(tab, "LAST1");
    ScalarColumn<double> last2(tab, "LAST2");
    ArrayColumn<double> azel1(tab, "AZEL1");
    ArrayColumn<double> azel2(tab, "AZEL2");
    ArrayColumn<double> uvwJ2000(tab, "UVW_J2000");
    ScalarMeasColumn<MEpoch> time(tab, "TIME");
    ScalarColumn<Int> fld(tab, "FIELD_ID");
    ScalarColumn<Int> ant1(tab, "ANTENNA1");
    ScalarColumn<Int> ant2;
    if (tab.tableDesc().isColumn("ANTENNA2")) {
      ant2.attach (tab, "ANTENNA2");
    } else {
      ant2.attach (tab, "ANTENNA1");
    }
    ArrayColumn<Double> uvw;
    if (tab.tableDesc().isColumn("UVW")) {
      uvw.attach (tab, "UVW");
    }
    MSDerivedValues mdv;
    mdv.setMeasurementSet (ms);
    // Take care that the same array center is used.
    // Find observatory position.
    // If not found, set it to the position of the middle antenna.
    Bool fndObs = False;
    MPosition arrayPos;
    Table obstab (ms.keywordSet().asTable("OBSERVATION"));
    if (obstab.nrow() > 0) {
      String telescope = ScalarColumn<String>(obstab, "TELESCOPE_NAME")(0);
      fndObs = MeasTable::Observatory (arrayPos, telescope);
    }
    if (!fndObs) {
      ROMSAntennaColumns antcol(ms.antenna());
      arrayPos = antcol.positionMeas()(ms.antenna().nrow()/2);
    }
    mdv.setObservatoryPosition (arrayPos);
    // Now loop through quite some rows and compare result of DerivedMSCal
    // with MSDerivedValues.
    uInt nr = std::max(tab.nrow(), 1000u);
    Int lastFldId = -1;
    for (uInt i=0; i<nr; ++i) {
      Int fldId = fld(i);
      if (fldId != lastFldId) {
        mdv.setFieldCenter (fldId);
        lastFldId = fldId;
      }
      mdv.setEpoch (time(i));
      mdv.setAntenna (-1);
      check (mdv, i, ha, last);
      mdv.setAntenna (ant1(i));
      check (mdv, i, ha1, last1, pa1, azel1);
      mdv.setAntenna (ant2(i));
      check (mdv, i, ha2, last2, pa2, azel2);
      if (checkUVW) {
        check (i, uvw, uvwJ2000);
      }
    }
    // Now time getting the hourangle using DataMan and MSDerivedValues.
    double totha = 0;
    Timer timer;
    for (uInt i=0; i<tab.nrow(); ++i) {
      totha += ha(i);
    }
    timer.show ("DataMan  ha");
    totha = 0;
    timer.mark();
    for (uInt i=0; i<tab.nrow(); ++i) {
      // Note: setFieldCenter is very expensive; takes 95% of the time.
      // Therefore it is omitted in this loop.
      if (i == 0) {
        mdv.setFieldCenter (fld(i));
      }
      mdv.setEpoch (time(i));
      mdv.setAntenna (-1);
      totha += mdv.hourAngle();
    }
    timer.show ("Values   ha");
    timer.mark();
    for (uInt i=0; i<tab.nrow(); ++i) {
      uvwJ2000(i);
    }
    timer.show ("DataMan uvw");
    if (! uvw.isNull()) {
      timer.mark();
      for (uInt i=0; i<tab.nrow(); ++i) {
        uvw(i);
      }
      timer.show ("Table   uvw");
    }
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
