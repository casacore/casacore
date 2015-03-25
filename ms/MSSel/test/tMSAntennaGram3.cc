//# tMSAntennaGram3.cc: Test program for TMSAntennaGram without need for an MS
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/iostream.h>

#include <casacore/ms/MSSel/MSAntennaGram.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSAntennaColumns.h>
#include <casacore/ms/MSSel/MSSelection.h>
#include <casacore/tables/Tables/SetupNewTab.h>

using namespace casacore;

void makeMS()
{
  // Create an empty MS.
  TableDesc simpleDesc = MS::requiredTableDesc();
  SetupNewTable newTab("tMSAntennaGram3_tmp.ms", simpleDesc, Table::New);
  MeasurementSet ms(newTab);
  ms.createDefaultSubtables(Table::New);
  ms.flush();
  // Add entries to the ANTENNA subtable. Only names and positions are needed.
  MSAntenna& msant(ms.antenna());
  MSAntennaColumns antcol(msant);
  msant.addRow(10);
  antcol.name().put (0, "RT0");
  antcol.name().put (1, "RT1");
  antcol.name().put (2, "A1.R2");
  antcol.name().put (3, "20");
  antcol.name().put (4, "1:4");
  antcol.name().put (5, "1:5");
  antcol.name().put (6, "A2.R1");
  antcol.name().put (7, "A2.R2");
  antcol.name().put (8, "A2:R1");
  antcol.name().put (9, "A2:R2");
  Vector<Quantity> xyz(3);
  double val = 1;
  for (int i=0; i<10; ++i) {
    xyz[0] = Quantity(val, "m");
    xyz[1] = Quantity(val, "m");
    xyz[2] = Quantity(val, "m");
    MVPosition pos(xyz);
    antcol.positionMeas().put (i, MPosition(pos, MPosition::ITRF));
    val *= 2;
  }
}

void doSel (const MeasurementSet& ms, const String& command, bool showBL=False)
{
  cout << command << endl;
  Vector<Int> selectedAnts1;
  Vector<Int> selectedAnts2;
  Matrix<Int> selectedBaselines;
  msAntennaGramParseCommand (&ms, command,
                             selectedAnts1, selectedAnts2, selectedBaselines);
  cout << "  " << selectedAnts1 << ' ' << selectedAnts2 << endl;
  if (showBL) {
    cout << "  " << selectedBaselines << endl;
  }
}

void selMS()
{
  MeasurementSet ms("tMSAntennaGram3_tmp.ms");
  doSel (ms, "20&&");
  doSel (ms, "20");
  doSel (ms, "1&20");
  doSel (ms, "RT1&A1.R2");
  doSel (ms, "RT1&A2:R2");
  doSel (ms, "RT1&A2.R[123]");
  doSel (ms, "RT1&A2:R[123]");
  doSel (ms, "RT1&'A*[.:]R{1,2,3}'");
  doSel (ms, "!RT1&'A*[.:]R{1,2,3}'");
  doSel (ms, "RT1&A1.*");
  doSel (ms, "1:4& 1:5");
  doSel (ms, "'1:4'&'1:5'");
  doSel (ms, "1,2 & 3,4; 5,6 & 7", True);
  doSel (ms, "\\RT1 & RT0");
  doSel (ms, "/A.*/&", True);
  doSel (ms, "/A.*/&&", True);
  doSel (ms, "^A*&&", True);
  doSel (ms, "^/A.*/&&", True);
  doSel (ms, "!/A.*/&&", True);
  doSel (ms, "<3", True);
  doSel (ms, "1~5");
  doSel (ms, "1m~5m", True);
  doSel (ms, "1.~5.", True);
}

int main()
{
  try {
    makeMS();
    selMS();
  } catch (AipsError& x) {
    cout << "ERROR: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
