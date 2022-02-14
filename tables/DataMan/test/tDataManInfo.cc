//# tDataManInfo.cc: Test program for class DataManInfo
//# Copyright (C) 2021
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

#include <casacore/tables/DataMan/DataManInfo.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

using namespace casacore;
using namespace std;

// <summary>
// Test program for class DataManInfo
// </summary>

// Test modifying the dminfo record.
void testDM()
{
    TableDesc td;
    td.addColumn(ScalarColumnDesc<Int>("col1"));
    td.addColumn(ScalarColumnDesc<Int>("col2"));
    td.addColumn(ScalarColumnDesc<Int>("col3"));
    td.addColumn(ArrayColumnDesc<Int>("col4"));
    // Now create a new table from the description.
    SetupNewTable aNewTab("tTableCopy_tmp.dm", td, Table::New);
    Table tabl(aNewTab);
    Record dminfo = tabl.dataManagerInfo();
    cout << dminfo;
    Vector<String> remCols1 =
      DataManInfo::removeDminfoColumns (dminfo, Vector<String>(1, "col1"), "Standard");
    cout << dminfo << remCols1 << endl;
    Vector<String> remCols2 =
      DataManInfo::removeDminfoColumns (dminfo, Vector<String>(1, "col1"));
    cout << dminfo << remCols2 << endl;
    DataManInfo::setTiledStMan (dminfo, Vector<String>(1, "col3"),
                                "TiledShapeStMan", "TSMData", IPosition(3,3,4,5));
    cout << dminfo << endl;
}

void mergeTestEmpty()
{
  Record rec;
  DataManInfo::mergeInfo (rec, Record());
  AlwaysAssertExit (rec.nfields() == 0);
}

void mergeTestNonEmpty()
{
  Record dminfo;
  Record sub;
  sub.define("TYPE", "abc");
  sub.define ("COLUMNS", Vector<String>(1, "col1"));
  dminfo.defineRecord (0, sub);
  Record rec;
  DataManInfo::mergeInfo (rec, dminfo);
  AlwaysAssertExit (rec.nfields() == 1);
  const Record& dm1 = rec.subRecord(0);
  AlwaysAssertExit (dm1.asString("TYPE") =="abc");
  AlwaysAssertExit (! dm1.isDefined("NAME"));
  AlwaysAssertExit (dm1.asArrayString("COLUMNS").size() == 1);
  AlwaysAssertExit (dm1.asArrayString("COLUMNS").data()[0] == "col1");
}

void finalizeTestEmpty()
{
  Record rec = DataManInfo::finalizeMerge (TableDesc(), Record());
  AlwaysAssertExit (rec.nfields() == 0);
}

void finalizeTestNonEmpty()
{
  Record dminfo;
  Record sub;
  sub.define("TYPE", "abc");
  sub.define("NAME", String());
  sub.define ("COLUMNS", Vector<String>(1, "col1"));
  dminfo.defineRecord (0, sub);
  {
    Record rec = DataManInfo::finalizeMerge (TableDesc(), dminfo);
    AlwaysAssertExit (rec.nfields() == 0);
  }
  TableDesc desc;
  desc.addColumn (ScalarColumnDesc<Int>("col1"));
  Record rec;
  DataManInfo::mergeInfo (rec, dminfo);
  rec = DataManInfo::finalizeMerge (desc, dminfo);
  AlwaysAssertExit (rec.nfields() == 1);
  const Record& dm1 = rec.subRecord(0);
  AlwaysAssertExit (dm1.nfields() == 3);
  AlwaysAssertExit (dm1.asString("TYPE") =="abc");
  AlwaysAssertExit (dm1.asString("NAME") == "col1");
  AlwaysAssertExit (dm1.asArrayString("COLUMNS").size() == 1);
  AlwaysAssertExit (dm1.asArrayString("COLUMNS").data()[0] == "col1");
}

void largeTest()
{
  // Create a table description with various data manager types and names.
  TableDesc desc;
  desc.addColumn (ScalarColumnDesc<Int>("col1"));
  desc.addColumn (ScalarColumnDesc<Int>("col2", "", "dmtype1", "dmname1"));
  desc.addColumn (ScalarColumnDesc<Int>("col3", "", "dmtype2", ""));
  desc.addColumn (ScalarColumnDesc<Int>("col4", "", "dmtype1", "dmname2"));
  desc.addColumn (ScalarColumnDesc<Int>("col5", "", "", "dmname3"));
  desc.addColumn (ScalarColumnDesc<Int>("col6"));
  desc.addColumn (ScalarColumnDesc<Int>("col7"));
  desc.addColumn (ScalarColumnDesc<Int>("col8"));
  Record dminfoRes;
  {
    // Create a dminfo object for a few columns.
    Record dminfo;
    Record dm1;
    dm1.define ("TYPE", "dmtype1");
    dm1.define ("NAME", "dmname1a");
    dm1.define ("COLUMNS", Vector<String>(1, "col2"));
    dminfo.defineRecord (0, dm1);
    DataManInfo::mergeInfo (dminfoRes, dminfo);
    AlwaysAssertExit (dminfoRes.nfields() == 1);
    AlwaysAssertExit (dminfoRes.subRecord(0).nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(0).asString("TYPE") == "dmtype1");
    AlwaysAssertExit (dminfoRes.subRecord(0).asString("NAME") == "dmname1a");
    AlwaysAssertExit (dminfoRes.subRecord(0).asArrayString("COLUMNS").size() == 1);
    AlwaysAssertExit (dminfoRes.subRecord(0).asArrayString("COLUMNS").data()[0] == "col2");
  }
  // Merge another dminfo.
  {
    Record dminfo;
    Record dm1;
    dm1.define ("TYPE", "dmtype1");
    dm1.define ("NAME", "dmname2");
    Record spec1;
    spec1.define ("BSZ", 10);
    dm1.defineRecord ("SPEC", spec1);
    dminfo.defineRecord (0, dm1);
    Record dm2;
    dm2.define ("TYPE", "dmtype2");
    dm2.define ("NAME", "dmname2a");
    dm2.define ("COLUMNS", Vector<String>({"col6", "col7"}));
    dminfo.defineRecord (1, dm2);
    DataManInfo::mergeInfo (dminfoRes, dminfo);
    AlwaysAssertExit (dminfoRes.nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(0).nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(1).nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(2).nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(0).asString("TYPE") == "dmtype1");
    AlwaysAssertExit (dminfoRes.subRecord(1).asString("TYPE") == "dmtype1");
    AlwaysAssertExit (dminfoRes.subRecord(2).asString("TYPE") == "dmtype2");
    AlwaysAssertExit (dminfoRes.subRecord(0).asString("NAME") == "dmname1a");
    AlwaysAssertExit (dminfoRes.subRecord(1).asString("NAME") == "dmname2");
    AlwaysAssertExit (dminfoRes.subRecord(2).asString("NAME") == "dmname2a");
    AlwaysAssertExit (dminfoRes.subRecord(0).asArrayString("COLUMNS").size() == 1);
    AlwaysAssertExit (dminfoRes.subRecord(0).asArrayString("COLUMNS").data()[0] == "col2");
    AlwaysAssertExit (dminfoRes.subRecord(2).asArrayString("COLUMNS").size() == 2);
    AlwaysAssertExit (dminfoRes.subRecord(2).asArrayString("COLUMNS").data()[0] == "col6");
    AlwaysAssertExit (dminfoRes.subRecord(2).asArrayString("COLUMNS").data()[1] == "col7");
    AlwaysAssertExit (dminfoRes.subRecord(1).subRecord("SPEC").nfields() == 1);
    AlwaysAssertExit (dminfoRes.subRecord(1).subRecord("SPEC").asInt("BSZ") == 10);
  }
  // Merge another dminfo.
  {
    Record dminfo;
    Record dm1;
    dm1.define ("TYPE", "dmtype1");
    dm1.define ("NAME", "dmname1a");
    dm1.define ("COLUMNS", Vector<String>({"col6"}));
    dminfo.defineRecord (0, dm1);
    DataManInfo::mergeInfo (dminfoRes, dminfo);
    AlwaysAssertExit (dminfoRes.nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(0).nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(1).nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(2).nfields() == 3);
    AlwaysAssertExit (dminfoRes.subRecord(0).asString("TYPE") == "dmtype1");
    AlwaysAssertExit (dminfoRes.subRecord(1).asString("TYPE") == "dmtype1");
    AlwaysAssertExit (dminfoRes.subRecord(2).asString("TYPE") == "dmtype2");
    AlwaysAssertExit (dminfoRes.subRecord(0).asString("NAME") == "dmname1a");
    AlwaysAssertExit (dminfoRes.subRecord(1).asString("NAME") == "dmname2");
    AlwaysAssertExit (dminfoRes.subRecord(2).asString("NAME") == "dmname2a");
    AlwaysAssertExit (dminfoRes.subRecord(0).asArrayString("COLUMNS").size() == 2);
    AlwaysAssertExit (dminfoRes.subRecord(0).asArrayString("COLUMNS").data()[0] == "col2");
    AlwaysAssertExit (dminfoRes.subRecord(0).asArrayString("COLUMNS").data()[1] == "col6");
    AlwaysAssertExit (dminfoRes.subRecord(2).asArrayString("COLUMNS").size() == 1);
    AlwaysAssertExit (dminfoRes.subRecord(2).asArrayString("COLUMNS").data()[0] == "col7");
    AlwaysAssertExit (dminfoRes.subRecord(1).subRecord("SPEC").nfields() == 1);
    AlwaysAssertExit (dminfoRes.subRecord(1).subRecord("SPEC").asInt("BSZ") == 10);
  }
  Record dminfoFinal = DataManInfo::finalizeMerge (desc, dminfoRes);
  cout << dminfoFinal;
}

void testUniqueName()
{
  Record dminfo;
  Record dm;
  dminfo.defineRecord (0, dm);    // no NAME, becomes DM_1
  dm.define ("NAME", "nma");
  dminfo.defineRecord (1, dm);
  dminfo.defineRecord (2, dm);    // nma becomes nma_2    
  dm.define ("NAME", "nma_1");
  dminfo.defineRecord (3, dm);
  dm.define ("NAME", "");
  dminfo.defineRecord (4, dm);    // becomes DM
  dm.define ("COLUMNS", Vector<String>(1, "col1"));
  dminfo.defineRecord (5, dm);    // becomes col1
  AlwaysAssertExit (DataManInfo::uniqueName(dminfo, "nma") == "nma_2");
  AlwaysAssertExit (DataManInfo::uniqueName(dminfo, "nmb") == "nmb");
  DataManInfo::makeUniqueNames (dminfo);
  AlwaysAssertExit (dminfo.subRecord(0).asString("NAME") == "DM_1");
  AlwaysAssertExit (dminfo.subRecord(1).asString("NAME") == "nma");
  AlwaysAssertExit (dminfo.subRecord(2).asString("NAME") == "nma_2");
  AlwaysAssertExit (dminfo.subRecord(3).asString("NAME") == "nma_1");
  AlwaysAssertExit (dminfo.subRecord(4).asString("NAME") == "DM");
  AlwaysAssertExit (dminfo.subRecord(5).asString("NAME") == "col1");
}

int main()
{
  try {
    testDM();
    mergeTestEmpty();
    mergeTestNonEmpty();
    finalizeTestEmpty();
    finalizeTestNonEmpty();
    largeTest();
    testUniqueName();
  } catch (const std::exception& x) {
    cout << "Caught exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
