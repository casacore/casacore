//# tStMan.cc: Test program for the various storage managers
//# Copyright (C) 2000
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

#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableLock.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/StandardStMan.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <strstream.h>

// <summary>
// Test program for the various storage managers.
// </summary>


// Create and fill a new table.
void newtab (uInt nrrow, const DataManager& stman)
{
  // Build the table description.
  TableDesc td("", "1", TableDesc::Scratch);
  td.addColumn (ScalarColumnDesc<String>("str1"));
  ScalarColumnDesc<String> dstr2("str2");
  dstr2.setMaxLength (20);
  td.addColumn (dstr2);
  td.addColumn (ArrayColumnDesc<String>("stra1",IPosition(2,2,3),
					ColumnDesc::Direct));
  ArrayColumnDesc<String> dstra2("stra2",IPosition(2,2,3),
				 ColumnDesc::Direct);
  dstra2.setMaxLength (20);
  td.addColumn (dstra2);
  td.addColumn (ArrayColumnDesc<String>("stra3", -1, ColumnDesc::FixedShape));
  ArrayColumnDesc<String> dstra4("stra4");
  dstra4.setMaxLength (20);
  td.addColumn (dstra4);

  // Now create a new table from the description.
  // Use copy constructor to test if it works fine.
  // (newtab and newtabcp have the same underlying object).
  SetupNewTable newtab("tStMan_tmp.data", td, Table::New);
  // Create a storage manager for it.
  newtab.bindAll (stman);
  newtab.setShapeColumn("stra3",IPosition(2,2,3));
  Table tab(newtab, nrrow);

  Array<String> emptyArray(IPosition(2,2,3));

  ScalarColumn<String> str1 (tab, "str1");
  ScalarColumn<String> str2 (tab, "str2");
  ArrayColumn<String> stra1 (tab, "stra1");
  ArrayColumn<String> stra2 (tab, "stra2");
  ArrayColumn<String> stra3 (tab, "stra3");
  ArrayColumn<String> stra4 (tab, "stra4");
  for (uInt i=0; i<nrrow; i++) {
    AlwaysAssertExit (str1(i) == "");
    AlwaysAssertExit (str2(i) == "");
    AlwaysAssertExit (allEQ (stra1(i), emptyArray));
    AlwaysAssertExit (allEQ (stra2(i), emptyArray));
    AlwaysAssertExit (allEQ (stra3(i), emptyArray));
    AlwaysAssertExit (! stra4.isDefined(i));
  }
}

void checktab1()
{
  Table tab("tStMan_tmp.data");

  Array<String> emptyArray(IPosition(2,2,3));

  ROScalarColumn<String> str1 (tab, "str1");
  ROScalarColumn<String> str2 (tab, "str2");
  ROArrayColumn<String> stra1 (tab, "stra1");
  ROArrayColumn<String> stra2 (tab, "stra2");
  ROArrayColumn<String> stra3 (tab, "stra3");
  ROArrayColumn<String> stra4 (tab, "stra4");
  uInt nrrow = tab.nrow();
  for (uInt i=0; i<nrrow; i++) {
    AlwaysAssertExit (str1(i) == "");
    AlwaysAssertExit (str2(i) == "");
    AlwaysAssertExit (allEQ (stra1(i), emptyArray));
    AlwaysAssertExit (allEQ (stra2(i), emptyArray));
    AlwaysAssertExit (allEQ (stra3(i), emptyArray));
    AlwaysAssertExit (! stra4.isDefined(i));
  }
}

void checktab (const String& prefix)
{
  Table tab("tStMan_tmp.data");

  Array<String> emptyArray(IPosition(2,2,3));
  Array<String> filledArray(IPosition(2,2,3));
  filledArray(IPosition(2,0,0)) = prefix + "str_00_";
  filledArray(IPosition(2,0,1)) = "str_01_";
  filledArray(IPosition(2,0,2)) = "str_02_";
  filledArray(IPosition(2,1,0)) = "str_10_";
  filledArray(IPosition(2,1,1)) = "str_11_";
  filledArray(IPosition(2,1,2)) = "str_12_";

  ROScalarColumn<String> str1 (tab, "str1");
  ROScalarColumn<String> str2 (tab, "str2");
  ROArrayColumn<String> stra1 (tab, "stra1");
  ROArrayColumn<String> stra2 (tab, "stra2");
  ROArrayColumn<String> stra3 (tab, "stra3");
  ROArrayColumn<String> stra4 (tab, "stra4");

  uInt nrrow = tab.nrow();
  char buf[8];
  {
    String s1(prefix + "str1_");
    String s2(prefix + "str2_");
    for (uInt i=0; i<nrrow; i++) {
      sprintf (buf, "%d", i);
      s1 += buf;
      s2 += buf;
      if (s2.length() > 20) {
	s2 = prefix + "str2_";
      }
      AlwaysAssertExit (str1(i) == s1);
      AlwaysAssertExit (str2(i) == s2);
      AlwaysAssertExit (allEQ (stra1(i),
			       String("a1_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (stra1.getSlice (i, Slicer(IPosition(2,1,0),
							 IPosition(2,1,2))),
			       (String("a1_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (stra2(i),
			       String("a2_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (stra2.getSlice (i, Slicer(IPosition(2,1,0),
							 IPosition(2,1,2))),
			       (String("a2_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (stra3(i),
			       String("a3_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (stra3.getSlice (i, Slicer(IPosition(2,1,0),
							 IPosition(2,1,2))),
			       (String("a3_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      if (i%2 == 0) {
	AlwaysAssertExit (allEQ (stra4(i),
				 String("a4_")+filledArray+String(buf)));
	AlwaysAssertExit (allEQ (stra4.getSlice (i, Slicer(IPosition(2,1,0),
							   IPosition(2,1,2))),
				 (String("a4_")+filledArray+String(buf))
				 (IPosition(2,1,0), IPosition(2,1,1))));
      } else {
	AlwaysAssertExit (allEQ (stra4(i), emptyArray));
	AlwaysAssertExit (allEQ (stra4.getSlice (i, Slicer(IPosition(2,1,0),
							   IPosition(2,1,2))),
				 emptyArray
				 (IPosition(2,1,0), IPosition(2,1,1))));
      }
    }
  }
  {
    String s1(prefix + "str1_");
    String s2(prefix + "str2_");
    Vector<String> vec1 = str1.getColumn();
    Vector<String> vec2 = str2.getColumn();
    Array<String> arr1 = stra1.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<String> arr2 = stra2.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<String> arr3 = stra3.getColumn().reform(IPosition(2,2,3*nrrow));
    Array<String> arr4 = stra4.getColumn().reform(IPosition(2,2,3*nrrow));
    for (uInt i=0; i<nrrow; i++) {
      sprintf (buf, "%d", i);
      s1 += buf;
      s2 += buf;
      if (s2.length() > 20) {
	s2 = prefix + "str2_";
      }
      AlwaysAssertExit (vec1(i) == s1);
      AlwaysAssertExit (vec2(i) == s2);
      AlwaysAssertExit (allEQ (arr1(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       String("a1_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (arr2(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       String("a2_")+filledArray+String(buf)));
      AlwaysAssertExit (allEQ (arr3(IPosition(2,0,3*i), IPosition(2,1,3*i+2)),
			       String("a3_")+filledArray+String(buf)));
      if (i%2 == 0) {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,3*i),IPosition(2,1,3*i+2)),
				 String("a4_")+filledArray+String(buf)));
      } else {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,3*i),IPosition(2,1,3*i+2)),
				 emptyArray));
      }
    }
  }
  {
    String s1(prefix + "str1_");
    String s2(prefix + "str2_");
    Array<String> arr1 = stra1.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<String> arr2 = stra2.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<String> arr3 = stra3.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    Array<String> arr4 = stra4.getColumn(Slicer(IPosition(2,1,0),
						IPosition(2,1,2))).
                         reform(IPosition(2,1,2*nrrow));
    for (uInt i=0; i<nrrow; i++) {
      sprintf (buf, "%d", i);
      s1 += buf;
      s2 += buf;
      if (s2.length() > 20) {
	s2 = prefix + "str2_";
      }
      AlwaysAssertExit (allEQ (arr1(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (String("a1_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (arr2(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (String("a2_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      AlwaysAssertExit (allEQ (arr3(IPosition(2,0,2*i), IPosition(2,0,2*i+1)),
			       (String("a3_")+filledArray+String(buf))
			       (IPosition(2,1,0), IPosition(2,1,1))));
      if (i%2 == 0) {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,2*i),IPosition(2,0,2*i+1)),
				 (String("a4_")+filledArray+String(buf))
			         (IPosition(2,1,0), IPosition(2,1,1))));
      } else {
	AlwaysAssertExit (allEQ (arr4(IPosition(2,0,2*i),IPosition(2,0,2*i+1)),
				 emptyArray
	                         (IPosition(2,1,0), IPosition(2,1,1))));
      }
    }
  }
}

void extab (const String& prefix)
{
  Table tab("tStMan_tmp.data", Table::Update);

  Array<String> filledArray(IPosition(2,2,3));
  filledArray(IPosition(2,0,0)) = prefix + "str_00_";
  filledArray(IPosition(2,0,1)) = "str_01_";
  filledArray(IPosition(2,0,2)) = "str_02_";
  filledArray(IPosition(2,1,0)) = "str_10_";
  filledArray(IPosition(2,1,1)) = "str_11_";
  filledArray(IPosition(2,1,2)) = "str_12_";

  ScalarColumn<String> str1 (tab, "str1");
  ScalarColumn<String> str2 (tab, "str2");
  ArrayColumn<String> stra1 (tab, "stra1");
  ArrayColumn<String> stra2 (tab, "stra2");
  ArrayColumn<String> stra3 (tab, "stra3");
  ArrayColumn<String> stra4 (tab, "stra4");

  char buf[8];
  String s1(prefix + "str1_");
  String s2(prefix + "str2_");
  uInt nrrow = tab.nrow();
  for (uInt i=0; i<nrrow; i++) {
    sprintf (buf, "%d", i);
    s1 += buf;
    s2 += buf;
    if (s2.length() > 20) {
      s2 = prefix + "str2_";
    }
    str1.put (i, s1);
    str2.put (i, s2);
    stra1.put (i, String("a1_")+filledArray+String(buf));
    stra2.put (i, String("a2_")+filledArray+String(buf));
    stra3.put (i, String("a3_")+filledArray+String(buf));
    if (i%2 == 0) {
      stra4.put (i, String("a4_")+filledArray+String(buf));
    } else {
      stra4.setShape (i, filledArray.shape());
    }
  }

  checktab(prefix);
}

void doTest (uInt nrrow, const DataManager& stman)
{
  newtab (nrrow, stman);
  checktab1();
  for (uInt i=0; i<4; i++) {
    extab ("");
    checktab ("");
  }
  extab ("p");
  checktab ("p");
}

main (int argc, const char* argv[])
{
  uInt nrrow = 10;
  uInt bucketSize = 100;
  if (argc > 1) {
    istrstream istr(argv[1]);
    istr >> nrrow;
  }
  if (argc > 2) {
    istrstream istr(argv[2]);
    istr >> bucketSize;
  }

  try {
    doTest (nrrow, StManAipsIO());
    doTest (nrrow, StandardStMan (max(bucketSize,100u)));
    doTest (nrrow, IncrementalStMan (max(bucketSize,1000u), False));
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } end_try;
  return 0;                           // exit with success status
}
