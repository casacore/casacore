//# tTableDesc.cc: Test program for the TableDesc class
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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

#include <tTableDesc.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScaRecordColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/SubTabDesc.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary> Test program for the TableDesc class </summary>

// This program tests the class TableDesc and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

// First build a description.
void a (Bool doExcp)
{
    // Add Scalar/ArrayColumnDesc<ExampleDesc> to column type map.
    ScalarColumnDesc<ExampleDesc>("x").registerClass();
    ArrayColumnDesc<ExampleDesc>("x").registerClass();

    // First build the description of a subtable.
    // Do it in separate scope to destruct it (thus to write it).
    {
	TableDesc subtd("tTableDesc_tmp_sub", "1", TableDesc::New);
	subtd.rwKeywordSet().define ("subint", Int(10));
	subtd.addColumn (ScalarColumnDesc<double> ("ra"));
	subtd.addColumn (ScalarColumnDesc<double> ("dec"));
    }
    TableDesc subtd("tTableDesc_tmp_sub", "1", TableDesc::Update);
    // Now build the main table description.
    uInt i;
    ColumnDesc cd,cd2;
    Vector<double> arr(4);
    for (i=0; i<4; i++) {
	arr(i) = i;
    }
    TableDesc td("tTableDesc_tmp", "1", TableDesc::New);
    td.comment() = "A test of class TableDesc";
    td.rwKeywordSet().define ("ra", float(3.14));
    td.rwKeywordSet().define ("equinox", double(1950));
    td.rwKeywordSet().define ("aa", Int(1));

    td.addColumn (ScalarColumnDesc<Int> ("ab","Comment for column ab"));
    if (doExcp) {
	try {
	    td.addColumn (ScalarColumnDesc<Int> ("ab"));   // already exists
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    }
    td.addColumn (ScalarColumnDesc<Int> ("ac"));
    td.rwColumnDesc("ac").rwKeywordSet().define ("scale", Complex(0,0));
    td.rwColumnDesc("ac").rwKeywordSet().define ("unit", "");
    td.addColumn (ScalarColumnDesc<uInt> ("ad","comment for ad"));
    td.rwColumnDesc("ac").rwKeywordSet().define ("unit", "DEG");
    td.addColumn (ScalarColumnDesc<ExampleDesc> ("ae"));
    td.addColumn (ArrayColumnDesc<ExampleDesc> ("arr0"));
    td.addColumn (ScalarRecordColumnDesc ("rec0"));
    if (doExcp) {
	try {
	    td.addColumn (ScalarColumnDesc<ExampleDesc>
			                     ("af", ColumnDesc::Undefined));
    	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // undefined given
	} 
    }

    td.addColumn (ArrayColumnDesc<Complex> ("Arr1","comment for Arr1",0));
    td.addColumn (ArrayColumnDesc<Int> ("A2r1","comment for Arr1",3));
    ArrayColumnDesc<uInt> coldes("Arr3","comment for Arr1",
				 IPosition(2,3,4), ColumnDesc::Direct);
    td.addColumn (coldes);

    // Set the shape of some columns.
    if (doExcp) {
	try {
	    td.rwColumnDesc("ab").setNdim (2);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // column is a scalar
	} 
	try {
	    td.rwColumnDesc("ab").setShape (IPosition());
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // column is a scalar
	} 
	try {
	    td.rwColumnDesc("Arr3").setNdim (2);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // ndim already defined
	} 
	try {
	    td.rwColumnDesc("Arr3").setShape (IPosition());
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // shape already defined
	} 
    }
    td.addColumn (SubTableDesc("sub1", "subtable by name",
                  "tTableDesc_tmp_sub"));
    td.addColumn (SubTableDesc("sub2", "subtable copy", subtd));
    td.addColumn (SubTableDesc("sub3", "subtable pointer", &subtd));
    td.addColumn (SubTableDesc("sub4", "subtable by name",
                  "tTableDesc_tmp_sub1"), "sub4Renamed");
    td.show();
    cout << td.keywordSet().isDefined("aa") << td.isColumn("ab")
	 << td.keywordSet().isDefined("ab") << td.isColumn("aa") << endl;

    // Not all subtables are known.
    cout << "checkSubTableDesc: ";
    if (doExcp) {
	try {
	    td.checkSubTableDesc();
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;     // subtable sub1 does not exist
	} 
    }

    // Add another column and keyword to the subtable to test
    // if they get reflected in the subtable. They should for sub3,
    // but not in sub1 and sub2.
    subtd.addColumn (ScalarColumnDesc<float> ("Equinox"));
    subtd.rwKeywordSet().define ("CoordSys", "equatorial");
    td["sub1"].tableDesc()->show();     // should have 2 columns
    td["sub3"].tableDesc()->show();     // should have 3 columns
    td["sub2"].tableDesc()->show();     // should have 2 columns

    // Test isDisjoint, etc..
    TableDesc tdx(td);
    const ColumnDescSet& set1 = td["sub1"].tableDesc()->columnDescSet();
    const ColumnDescSet& set2 = td["sub2"].tableDesc()->columnDescSet();
    const ColumnDescSet& set3 = td["sub3"].tableDesc()->columnDescSet();
    Bool equalDataTypes;
    AlwaysAssertExit (set2.isDisjoint (tdx.columnDescSet()));
    tdx.addColumn (ScalarColumnDesc<float> ("ra"));
    AlwaysAssertExit (! set2.isDisjoint (tdx.columnDescSet()));
    AlwaysAssertExit (! set2.isSubset (tdx.columnDescSet(), equalDataTypes));
    tdx.addColumn (ScalarColumnDesc<double> ("dec"));
    AlwaysAssertExit (set2.isSubset (tdx.columnDescSet(), equalDataTypes));
    AlwaysAssertExit (! equalDataTypes);
    
    AlwaysAssertExit (! set1.isDisjoint (set2));
    AlwaysAssertExit (set1.isEqual (set2, equalDataTypes));
    AlwaysAssertExit (equalDataTypes);
    AlwaysAssertExit (set1.isSubset (set2, equalDataTypes));
    AlwaysAssertExit (set1.isSuperset (set2, equalDataTypes));
    AlwaysAssertExit (! set1.isStrictSubset (set2, equalDataTypes));
    AlwaysAssertExit (! set1.isStrictSuperset (set2, equalDataTypes));
    AlwaysAssertExit (! set3.isEqual (set2, equalDataTypes));
    AlwaysAssertExit (! set3.isSubset (set2, equalDataTypes));
    AlwaysAssertExit (set3.isSuperset (set2, equalDataTypes));
    AlwaysAssertExit (! set3.isStrictSubset (set2, equalDataTypes));
    AlwaysAssertExit (set3.isStrictSuperset (set2, equalDataTypes));

    // Try some ColumnDesc functions.
    // First add the column; remove it at the end.
    td.addColumn (ArrayColumnDesc<Int> ("ArrExtra"));
    ColumnDesc& cdesc = td.rwColumnDesc("ArrExtra");
    AlwaysAssertExit (cdesc.ndim() == -1);
    AlwaysAssertExit (cdesc.shape() == IPosition());
    AlwaysAssertExit (cdesc.options() == 0);
    cdesc.setShape (IPosition(1,4));
    AlwaysAssertExit (cdesc.ndim() == 1);
    AlwaysAssertExit (cdesc.shape() == IPosition(1,4));
    AlwaysAssertExit (cdesc.options() == ColumnDesc::FixedShape);
    cdesc.setNdim (0);
    AlwaysAssertExit (cdesc.ndim() == -1);
    AlwaysAssertExit (cdesc.shape() == IPosition());
    AlwaysAssertExit (cdesc.options() == ColumnDesc::FixedShape);
    cdesc.setShape (IPosition(2,4,5), True);
    AlwaysAssertExit (cdesc.ndim() == 2);
    AlwaysAssertExit (cdesc.shape() == IPosition(2,4,5));
    AlwaysAssertExit (cdesc.options() ==
		      (ColumnDesc::FixedShape|ColumnDesc::Direct));
    cdesc.setNdim (0);
    AlwaysAssertExit (cdesc.ndim() == -1);
    AlwaysAssertExit (cdesc.shape() == IPosition());
    AlwaysAssertExit (cdesc.options() ==
		      (ColumnDesc::FixedShape|ColumnDesc::Direct));
    cdesc.setShape (IPosition(1,4));
    AlwaysAssertExit (cdesc.ndim() == 1);
    AlwaysAssertExit (cdesc.shape() == IPosition(1,4));
    AlwaysAssertExit (cdesc.options() ==
		      (ColumnDesc::FixedShape|ColumnDesc::Direct));
    cdesc.setNdim (0);
    AlwaysAssertExit (cdesc.ndim() == -1);
    AlwaysAssertExit (cdesc.shape() == IPosition());
    AlwaysAssertExit (cdesc.options() ==
		      (ColumnDesc::FixedShape|ColumnDesc::Direct));
    cdesc.setShape (IPosition(2,4,5), False);
    AlwaysAssertExit (cdesc.ndim() == 2);
    AlwaysAssertExit (cdesc.shape() == IPosition(2,4,5));
    AlwaysAssertExit (cdesc.options() == ColumnDesc::FixedShape);
    cdesc.setOptions (0);
    AlwaysAssertExit (cdesc.ndim() == -1);
    AlwaysAssertExit (cdesc.shape() == IPosition());
    AlwaysAssertExit (cdesc.options() == 0);
    cdesc.setOptions (ColumnDesc::Direct);
    AlwaysAssertExit (cdesc.ndim() == -1);
    AlwaysAssertExit (cdesc.shape() == IPosition());
    AlwaysAssertExit (cdesc.options() ==
		      (ColumnDesc::FixedShape|ColumnDesc::Direct));
    td.removeColumn ("ArrExtra");
}

// Remove some keywords/columns.
// Do some tests of the options for the constructor.
void b (Bool doExcp) {
    TableDesc td("tTableDesc_tmp", TableDesc::Update);
    cout << td.columnNames() << endl;
    cout << (td.columnDesc("ab") == td.columnDesc("ac"));
    cout << (td.columnDesc("ad") == td.columnDesc("ae"));
    cout << (td.columnDesc("ab") == td.columnDesc("Arr1"));
    cout << (td.columnDesc("Arr1") == td.columnDesc("Arr3"));
    cout << (td.columnDesc("Arr3") == td.columnDesc("ad"));
    cout << (td.columnDesc("A2r1") == td.columnDesc("ab"));
    cout << (td.columnDesc("sub1") != td.columnDesc("sub2"));
    cout << (td.columnDesc("sub1") != td.columnDesc("ab"));
    cout << endl;
    td.show();
    td["sub1"].tableDesc()->show();     // should have 3 columns
    td["sub2"].tableDesc()->show();     // should have 2 columns
    td["sub3"].tableDesc()->show();     // should have 3 columns
    cout << "endshow" << endl;

    // Set the shape of Arr1.
    td.rwColumnDesc("Arr1").setNdim (2);
    if (doExcp) {
	try {
	    td.rwColumnDesc("Arr1").setShape (IPosition(3,4,5,6));
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;        // shape mimatches ndim
	} 
    }
    td.rwColumnDesc("Arr1").setShape (IPosition(2,4,5));

    if (doExcp) {
	try {
	    td["Arr1"].tableDesc();              // no subtable
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    }

    // Define another descr. and add it to the first descr.
    TableDesc tdscr("TabSub",TableDesc::Scratch);
    tdscr.rwKeywordSet().define ("key1", Int(0));
    ScalarColumnDesc<String> colaDesc ("cola");
    colaDesc.setMaxLength (32);
    tdscr.addColumn (colaDesc);
    td.addColumn (SubTableDesc("colsub","colsub comment",tdscr));
    tdscr.rwKeywordSet().define ("key2", Int(0));
    tdscr.show();
    cout<<endl;
    TableDesc tda(td,"OtherName","O2",TableDesc::Scratch);   // copy the descr.
    tda.show();
    tda.add (tdscr, False);
    tda.rwKeywordSet().removeField ("ra");
    tda.removeColumn ("sub2");
    ColumnDesc& cd = tda.rwColumnDesc("ac");
    cd.rwKeywordSet().removeField ("scale");
    td.show();
    tda.show();

    if (doExcp) {
	try {
	    cout << "Tryerr update" << endl;
	    TableDesc td1("",TableDesc::Update);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    }
}

// Do some more erronous constructions.
void c (Bool doExcp) {
    // The next 2 statements are outcommented, because they result
    // in a bus error with the g++ compiler for reasons not understood.
    // The error occurs at the very end of the program.
//#//    TableDesc td("tTableDesc_tmp");
//#//    td.show();
    TableDesc* td1 = 0;
    TableDesc* td2 = 0;
    TableDesc* td3 = 0;

    if (doExcp) {
	try {
	    cout << "Tryerr update not exist" << endl;
	    td1 = new TableDesc("tTableDescXX_tmp", TableDesc::Update);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	    td1 = 0;
	} 

	try {
	    cout << "Tryerr old not exist" << endl;
	    td2 = new TableDesc("tTableDescXX_tmp", TableDesc::Old);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	    td2 = 0;
	} 

	try {
	    cout << "Tryerr newnoreplace" << endl;
	    td3 = new TableDesc("tTableDesc_tmp", TableDesc::NewNoReplace);
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	    td3 = 0;
	} 
    }

    delete td1;
    delete td2;
    delete td3;
    cout << endl;
}

void d (Bool doExcp)
{
    // Create a new description.
    TableDesc td("tTableDesc_tmp1", TableDesc::New);

    // Add an Int column.
    ColumnDesc c1 = td.addColumn(ScalarColumnDesc<Int>("colint","comment"));

    // Define a keyword colint_key1 (=10) for that column.
    c1.rwKeywordSet().define ("colint_key1", Int(10));

    // It can also be done the other way around.
    ScalarColumnDesc<Int> colint("colint2","comment2");
    colint.rwKeywordSet().define ("colint_key1", Int(20));
    td.addColumn(colint);

    // Add a third column.
    c1 = td.addColumn (ScalarColumnDesc<Int> ("colint3","comment"));

    // Extend the comment.
    td.rwColumnDesc("colint").comment() += " addition";

    // Rename the column and back.
    td.renameColumn ("colintnew", "colint");
    AlwaysAssertExit (td.isColumn ("colintnew"));
    AlwaysAssertExit (! td.isColumn ("colint"));
    td.renameColumn ("colint", "colintnew");
    AlwaysAssertExit (! td.isColumn ("colintnew"));
    AlwaysAssertExit (td.isColumn ("colint"));

    // Rename to already existing column must fail.
    // Also a non-existing column cannot be renamed.
    if (doExcp) {
	try {
	    td.renameColumn ("colint2", "colint");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
	try {
	    td.renameColumn ("colintnew", "colintxxx");
	} catch (AipsError x) {
	    cout << x.getMesg() << endl;
	} 
    }

    // Define a keyword for the table.
    td.rwKeywordSet().define ("tab_key1", "this is a string");

    // Register engines.
    DataManager::registerCtor ("c1_engine",0);
    DataManager::registerCtor ("c2_engine",0);

    // Define a virtual column.
    td.addColumn(ScalarColumnDesc<Int>("c1", "c1-comment",
				       "c1_engine", ""));

    // Show name and comment of all column descriptions.
    for (uInt jj=0; jj<td.ncolumn(); jj++) {
	c1 = td[jj];
	cout << c1.name() << " " << c1.comment() << endl;
    }
    TableDesc tda(td, "namex", "v2.0.1", TableDesc::Scratch);
    tda.show();
}

void e (Bool)
{
    TableDesc td("tTableDesc_tmp1");
    td.show();
}


int main (int argc, const char*[])
{
    try {
	d ( (argc<2));
	e ( (argc<2));
	a ( (argc<2));
	b ( (argc<2));
	c ( (argc<2));
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    return 0;                           // exit with success status
}
