//# tConcatTable2.cc: Test program for the ConcatTable class
//# Copyright (C) 2008
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

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the ConcatTable class
// </summary>


// First build a description.
void fill(const String& name, const String& name2, Int stval)
{
  const Int nrrow = 10;
  {
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<Bool>("abool"));
    td.addColumn (ScalarColumnDesc<uChar>("auchar"));
    td.addColumn (ScalarColumnDesc<Short>("ashort"));
    td.addColumn (ScalarColumnDesc<Int>("aint"));
    td.addColumn (ScalarColumnDesc<uInt>("auint"));
    td.addColumn (ScalarColumnDesc<Float>("afloat"));
    td.addColumn (ScalarColumnDesc<Double>("adouble"));
    td.addColumn (ScalarColumnDesc<Complex>("acomplex"));
    td.addColumn (ScalarColumnDesc<DComplex>("adcomplex"));
    td.addColumn (ScalarColumnDesc<String>("astring"));
    // Now create a new table from the description.
    SetupNewTable newtab(name, td, Table::New);
    Table tab(newtab, nrrow);
    tab.rwKeywordSet().define ("key1", 1);
    tab.rwKeywordSet().define ("key2", "abc");
  }
  // Fill the table by means of a concatenated table.
  // Note the parts are not consecutive in the original table.
  Block<Table> tabs(3);
  {
    Table tab(name, Table::Update);
    tabs[0] = tab(tab.nodeRownr() < 3);
    tabs[1] = tab(tab.nodeRownr() >= 8);
    tabs[2] = tab(tab.nodeRownr() >=3  &&  tab.nodeRownr() < 8);
  }
  // Now create the ConcatTable and fill it.
  Table tab(tabs);
  AlwaysAssertExit (tab.nrow() == 10);
  AlwaysAssertExit (tab.keywordSet().nfields() == 2);
  ScalarColumn<Bool> abool(tab, "abool");
  ScalarColumn<uChar> auchar(tab, "auchar");
  ScalarColumn<Short> ashort(tab, "ashort");
  ScalarColumn<Int> aint(tab, "aint");
  ScalarColumn<uInt> auint(tab, "auint");
  ScalarColumn<Float> afloat(tab,  "afloat");
  ScalarColumn<Double> adouble(tab,"adouble");
  ScalarColumn<Complex> acomplex(tab, "acomplex");
  ScalarColumn<DComplex> adcomplex(tab, "adcomplex");
  ScalarColumn<String> astring(tab, "astring");
  char str[8];
  for (Int i=0; i<nrrow; i++) {
    abool.put (i, (stval%2 == 0));
    auchar.put (i, stval);
    ashort.put (i, stval);
    aint.put (i, stval);
    auint.put (i, stval);
    afloat.put (i, stval);
    adouble.put (i, stval);
    acomplex.put (i, Complex(stval,0));
    adcomplex.put (i, DComplex(0,stval));
    sprintf (str, "V%i", stval);
    astring.put (i, str);
    ++stval;
  }
  {
    // Create another ConcatTable and make it persistent.
    if (! name2.empty()) {
      Table tab(Block<String>(1,name));
      AlwaysAssertExit (tab.keywordSet().nfields() == 2);
      tab.rwKeywordSet().define ("key3", "def");
      tab.rwKeywordSet().define ("key4", 3.14);
      tab.rename (name2, Table::New);
      AlwaysAssertExit (tab.keywordSet().nfields() == 4);
      AlwaysAssertExit (tab.keywordSet().asInt("key1") == 1);
      AlwaysAssertExit (tab.keywordSet().asString("key2") == "abc");
      AlwaysAssertExit (tab.keywordSet().asString("key3") == "def");
      AlwaysAssertExit (tab.keywordSet().asDouble("key4") == 3.14);
    }
  }
}

void checkTable (const Table& tab, uInt nkey, uInt nsubrow, Int stval,
		 Bool reorder=True, uInt nrow=10)
{
  AlwaysAssertExit (tab.nrow() == nrow);
  AlwaysAssertExit (tab.keywordSet().nfields() == nkey);
  AlwaysAssertExit (tab.keywordSet().asInt("key1") == 1);
  AlwaysAssertExit (tab.keywordSet().asString("key2") == "abc");
  if (nkey == 3) {
    AlwaysAssertExit (tab.keywordSet().asTable("keysub").nrow() == nsubrow);
  }
  ScalarColumn<Bool> abool(tab, "abool");
  ScalarColumn<uChar> auchar(tab, "auchar");
  ScalarColumn<Short> ashort(tab, "ashort");
  ScalarColumn<Int> aint(tab, "aint");
  ScalarColumn<uInt> auint(tab, "auint");
  ScalarColumn<Float> afloat(tab,  "afloat");
  ScalarColumn<Double> adouble(tab,"adouble");
  ScalarColumn<Complex> acomplex(tab, "acomplex");
  ScalarColumn<DComplex> adcomplex(tab, "adcomplex");
  ScalarColumn<String> astring(tab, "astring");
  char str[8];
  // Values are stored as: 0 1 2 5 6 7 8 9 3 4
  for (uInt i=0; i<tab.nrow(); i++) {
    Int row = i%10;
    Int rowd = i - row;
    if (reorder) {
      if (row>=5) {
	row-=2;
      } else if (row>=3) {
	row+=5;
      }
    }
    row += rowd;
    AlwaysAssertExit (abool(row) == (stval%2==0));
    AlwaysAssertExit (auchar(row) == stval);
    AlwaysAssertExit (ashort(row) == stval);
    AlwaysAssertExit (aint(row) == stval);
    AlwaysAssertExit (auint(row) == uInt(stval));
    AlwaysAssertExit (afloat(row) == stval);
    AlwaysAssertExit (adouble(row) == stval);
    AlwaysAssertExit (acomplex(row) == Complex(stval,0));
    AlwaysAssertExit (adcomplex(row) == DComplex(0,stval));
    sprintf (str, "V%i", stval);
    AlwaysAssertExit (astring(row) == str);
    ++stval;
  }
}

void check (const String& name, uInt nkey, Int stval)
{
  checkTable (Table(name), nkey, 10, stval);
}

void checkComb (const String& name1, const String& name2, uInt nkey, Int stval)
{
  Block<Table> tabs(2);
  tabs[0] = Table(name1);
  tabs[1] = Table(name2);
  Table tab(tabs);
  checkTable (tab, nkey, 10, stval, True, 20);
}

void checkSplit (const String& name, uInt nkey, Int stval)
{
  Table tab(name);
  // Split and concatenate the table such that we get the original order.
  // Values are stored as: 0 1 2 5 6 7 8 9 3 4
  Block<Table> tabs(3);
  tabs[0] = tab(tab.nodeRownr() < 3);
  tabs[1] = tab(tab.nodeRownr() >= 8);
  tabs[2] = tab(tab.nodeRownr() >=3  &&  tab.nodeRownr() < 8);
  checkTable (Table(tabs), nkey, 10, stval, False);
}

void checkFull (const String& name, Int stval)
{
  Table tab(name);
  // Make a ConcatTable of a table for each row.
  Block<Table> tabs(10);
  for (uInt i=0; i<10; ++i) {
    tabs[i] = tab(tab.nodeRownr() == i);
  }
  // Concatenate the subtable, so we get a subtable of 10*10 rows.
  Table ctab(tabs, Block<String>(1,"keysub"));
  checkTable (ctab, 3, 100, stval);
  // Check each subtable.
  Table subtab = ctab.keywordSet().asTable("keysub");
  // Check if each part of 10 rows is correct.
  for (Int i=0; i<10; ++i) {
    checkTable(subtab(subtab.nodeRownr()>=10*i && subtab.nodeRownr()<10*(i+1)),
	       2, 10, 21);
  }
}

void fillSub(const String& name, const String& subname)
{
  Table tab(name, Table::Update);
  fill (subname, String(), 21);
  Table subtab(subname);
  tab.rwKeywordSet().defineTable("keysub", subtab);
}

int main()
{
  try {
    fill("tConcatTable2_tmp.data", "tConcatTable2_tmp.data2", 0);
    cout<< "done fill 0" << endl;
    fill("tConcatTable2_tmp.datb", "tConcatTable2_tmp.datb2", 10);
    cout<< "done fill 10" << endl;
    check("tConcatTable2_tmp.data", 2, 0);
    cout<< "done check 0" << endl;
    check("tConcatTable2_tmp.datb", 2, 10);
    cout<< "done check 10" << endl;
    checkComb("tConcatTable2_tmp.data", "tConcatTable2_tmp.datb", 2, 0);
    cout << "done checkComb" << endl;
    checkComb("tConcatTable2_tmp.data2", "tConcatTable2_tmp.datb2", 2, 0);
    cout << "done checkComb2" << endl;
    check("tConcatTable2_tmp.data2", 2, 0);
    cout<< "done check2" << endl;
    checkSplit("tConcatTable2_tmp.data", 2, 0);
    cout<< "done checkSplit" << endl;
    checkSplit("tConcatTable2_tmp.data2", 2, 0);
    cout<< "done checkSplit2" << endl;
    fillSub("tConcatTable2_tmp.data", "tConcatTable2_tmp.datasub");
    cout<< "done fillSub" << endl;
    check("tConcatTable2_tmp.data", 3, 0);
    cout<< "done check" << endl;
    checkFull("tConcatTable2_tmp.data", 0);
    cout<< "done checkFull" << endl;
  } catch (AipsError x) {
    cout << "Exception caught: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
