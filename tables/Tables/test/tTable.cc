//# tTable.cc: Test program for the Table classes
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002
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
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/TaQL/ExprNodeSet.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the Table classes
// </summary>

// This program tests the class SetupNewTable and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


// Define the callback for handling (scratch) tables.
void cbFunc (const String& name, Bool isScratch, const String& oldname)
{
    String nm1 = name.empty() ? name : Path(name).baseName();
    String nm2 = oldname.empty() ? oldname : Path(oldname).baseName();
    cout << "ScratchCallBack:  name=" << nm1
	 << "  isScratch=" << isScratch
	 << "  oldName=" << nm2 << endl;
}

// Remove the dirname from the table name in an error message.
String removeDir (const String& msg)
{
  String s = msg;
  s.gsub (Regex("/.*/t"), "t");
  return s;
}

// Define the endian format.
#ifdef AIPS_LITTLE_ENDIAN
Table::EndianFormat theEndianFormat = Table::LittleEndian;
#else
Table::EndianFormat theEndianFormat = Table::BigEndian;
#endif


// First build a description.
void a (Bool doExcp)
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class Table";
    td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
    td.addColumn (ScalarColumnDesc<DComplex>("ag"));
    td.addColumn (ArrayColumnDesc<float>("arr1",IPosition(3,2,3,4),
					 ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<float>("arr2",0));
    td.addColumn (ArrayColumnDesc<float>("arr3",0,ColumnDesc::Direct));

    // Now create a new table from the description.
    // Use copy constructor to test if it works fine.
    // (newtab and newtabcp have the same underlying object).
    SetupNewTable newtab("tTable_tmp.data", td, Table::New);
    SetupNewTable newtabcp(newtab);
    // Create a storage manager for it.
    StManAipsIO sm1;
    StManAipsIO sm2;
    newtab.bindAll (sm1);
    newtab.bindColumn ("ab",sm2);
    if (doExcp) {
	try {
	    newtab.setShapeColumn("arr2",IPosition(3,2,3,4));
	} catch (AipsError& x) {
            // not FixedShape
	    cout << "Expected exception: "<< x.getMesg() << endl;
	} 
    }
    newtab.setShapeColumn("arr3",IPosition(3,2,3,4));
    Table tab(newtabcp, 10, False, Table::LocalEndian);
    AlwaysAssertExit (tab.endianFormat() == theEndianFormat);
    tab.tableInfo().setType ("testtype");
    tab.tableInfo().setSubType ("testsubtype");
    tab.tableInfo().readmeAddLine ("first readme line");
    tab.tableInfo().readmeAddLine ("second test readme line");


    // Determine if columns are stored.
    uInt i;
    cout << "stored columns: ";
    for (i=0; i<tab.tableDesc().ncolumn(); i++) {
	cout << tab.isColumnStored(i);
    }
    cout << endl;

    ScalarColumn<Int> ab1(tab,"ab");
    ScalarColumn<Int> ab2(tab,"ab");
    ScalarColumn<uInt> ad(tab,"ad");
    TableColumn ag1(tab,"ag");
    ScalarColumn<DComplex> ag(tab,"ag");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<float> arr3(tab,"arr3");
    Cube<float> arrf(IPosition(3,2,3,4));
    char str[8];
    indgen (arrf);
    for (i=0; i<10; i++) {
	ab1.put (i, i);
	ad.put (i, i+2);
	arr1.put(i,arrf);
	arr2.put(i,arrf);
	arr3.put(i,arrf);
	arrf += (float)(arrf.nelements());
    }
    ag1.putColumn (ad);
    Int abval;
    uInt adval;
    DComplex agval;
    Cube<float> arrval(IPosition(3,2,3,4));
    arrf -= (float)(arrf.nelements()*tab.nrow());
    for (i=0; i<10; i++) {
	ab2.get (i, abval);
	ad.get (i, adval);
	ag.get (i, agval);
	if (abval != Int(i)  ||  adval != i+2  ||  agval != DComplex(i+2)) {
	    cout << "error in row " << i << ": " << abval
		 << ", " << adval << ", " << agval << endl;
	}
	arr1.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr1 in row " << i << endl;
	}
	arr2.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 in row " << i << endl;
	}
	arr3.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr3 in row " << i << endl;
	}
	arrf += (float)(arrf.nelements());
    }

    // Add a column.
    tab.addColumn (ScalarColumnDesc<Int> ("ac"));
    // Add a few columns with a new storage manager.
    TableDesc tempTD ("", "", TableDesc::Scratch);
    tempTD.addColumn (ScalarColumnDesc<float> ("ae"));
    ScalarColumnDesc<String> afcoldesc("af");
    afcoldesc.setMaxLength (10);
    tempTD.addColumn (afcoldesc);
    StManAipsIO stmanAdd;
    tab.addColumn (tempTD, stmanAdd);
    tab.tableDesc().show();
    ScalarColumn<Int> ac (tab,"ac");
    ScalarColumn<float> ae(tab,"ae");
    ScalarColumn<String> af(tab,"af");
    for (i=0; i<tab.nrow(); i++) {
	ac.put (i, i+1);
	ae.put (i, i+3);
	sprintf (str, "V%i", i);
	af.put (i, str);
    }
    Vector<float> vec2;
    if (doExcp) {
	try {
	    af.put (0, "12345678901");
	} catch (AipsError& x) {
            // value too long
	    cout << "Expected exception: " << x.getMesg() << endl;
	} 
	try {
	    arr1.put (0, vec2);
	} catch (AipsError& x) {
            // shape cannot change
	    cout << "Expected exception: " << x.getMesg() << endl;
	} 
    }
}


void b (Bool doExcp)
{
    // Get the description and #rows of the Table.
    cout << "get layout in static way" << endl;
    TableDesc layout;
    cout << "Table::getlayout #rows = "
	  << Table::getLayout (layout, "tTable_tmp.data");
    layout.show (cout);
    cout << endl;
    TableInfo info(Table::tableInfo ("tTable_tmp.data"));
    cout << "type = " << info.type() << endl;
    cout << "subtype = " << info.subType() << endl;
    cout << info.readme() << endl;
    // Check if some table files can be accessed.
    cout << Table::isReadable("tTable_tmp.data");
    cout << Table::isWritable("tTable_tmp.data");
    cout << Table::isReadable("tTablex.data");
    cout << Table::isWritable("tTablex.data");
    if (Table::nonWritableFiles("tTable_tmp.data").nelements() > 0) {
	cout << "There should be no non-writable table files" << endl;
    }
    cout << endl;

    // Read back the table.
    cout << "start reading Table" << endl;
    Table tab("tTable_tmp.data", TableLock(TableLock::PermanentLockingWait));
    AlwaysAssertExit (tab.endianFormat() == theEndianFormat);
    cout << "end reading Table" << endl;
    cout << "type = " << tab.tableInfo().type() << endl;
    cout << "subtype = " << tab.tableInfo().subType() << endl;
    cout << tab.tableInfo().readme() << endl;
    if (doExcp) {
	try {
	    tab.addColumn (ScalarColumnDesc<Int>("ab"));
	} catch (AipsError& x) {
            // table not writable
            cout << "Expected exception: " << removeDir(x.getMesg()) << endl;
	} 
    }
    ScalarColumn<Int> ab2(tab,"ab");
    ScalarColumn<Int> ac (tab,"ac");
    ScalarColumn<uInt> ad(tab,"ad");
    ScalarColumn<float> ae(tab,"ae");
    ScalarColumn<String> af(tab,"af");
    ScalarColumn<DComplex> ag(tab,"ag");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<float> arr3(tab,"arr3");
    cout << arr1.columnDesc().isFixedShape() << " arr1.shapeColumn() = "
	 << arr1.shapeColumn() << endl;
    cout << arr2.columnDesc().isFixedShape() << " arr2.shapeColumn() = "
	 << arr2.shapeColumn() << endl;
    cout << arr3.columnDesc().isFixedShape() << " arr3.shapeColumn() = "
	 << arr3.shapeColumn() << endl;
    cout << "datatypes ab = " << ab2.columnDesc().dataType() << " "
	 << ab2.columnDesc().trueDataType() << endl;
    cout << "datatypes ac = " << ac.columnDesc().dataType() << " "
	 << ac.columnDesc().trueDataType() << endl;
    cout << "datatypes ad = " << ad.columnDesc().dataType() << " "
	 << ad.columnDesc().trueDataType() << endl;
    cout << "datatypes ae = " << ae.columnDesc().dataType() << " "
	 << ae.columnDesc().trueDataType() << endl;
    cout << "datatypes af = " << af.columnDesc().dataType() << " "
	 << af.columnDesc().trueDataType() << endl;
    cout << "datatypes ag = " << ag.columnDesc().dataType() << " "
	 << ag.columnDesc().trueDataType() << endl;
    cout << "datatypes ar1 = " << arr1.columnDesc().dataType() << " "
	 << arr1.columnDesc().trueDataType() << endl;
    cout << "datatypes arr2 = " << arr2.columnDesc().dataType() << " "
	 << arr2.columnDesc().trueDataType() << endl;
    cout << "datatypes arr3 = " << arr3.columnDesc().dataType() << " "
	 << arr3.columnDesc().trueDataType() << endl;
    uInt i;
    Int abval, acval;
    uInt adval;
    float aeval;
    String afval;
    DComplex agval;
    char str[8];
    Cube<float> arrf(IPosition(3,2,3,4));
    Cube<float> arrval(IPosition(3,2,3,4));
    Cube<float> arrvalslice(arrval(Slice(0,1),Slice(0,1,2),Slice(0,2,2)));
    Slice tmp;
    Slicer nslice (tmp, tmp, tmp,  Slicer::endIsLength);
    Slicer nslice2(Slice(0,1), Slice(0,1,2), Slice(0,2,2),
		   Slicer::endIsLength);
    indgen (arrf);
    for (i=0; i<10; i++) {
	cout << "get scalar row " << i << endl;
	ab2.get (i, abval);
	ac.get (i, acval);
	ad.get (i, adval);
	ae.get (i, aeval);
	af.get (i, afval);
	ag.get (i, agval);
	sprintf (str, "V%i", i);
	if (abval != Int(i)  ||  acval != Int(i+1)
        ||  adval != i+2  ||  aeval != i+3
	||  afval != str  ||  agval != DComplex(i+2)) {
	    cout << "error in row " << i << ": " << abval
		 << ", " << acval << ", " << adval
		 << ", " << aeval << ", " << afval
		 << ", " << agval << endl;
	}
	arr1.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr1 in row " << i << endl;
	}
	arr2.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 in row " << i << endl;
	}
	cout << "get array row " << i << endl;
	arr3.get (i, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr3 in row " << i << endl;
	}
	arr2.getSlice (i, nslice, arrval);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 (entire slice) in row " << i << endl;
	}
	arr2.getSlice (i, nslice2, arrvalslice);
	if (!allEQ (arrval, arrf)) {
	    cout << "error in arr2 (partial slice) in row " << i << endl;
	}
	arrf += (float)(arrf.nelements());
    }
    Vector<Int> abvec = ab2.getColumn();
    cout << tab.nrow() << " " << abvec.nelements() << endl;
    for (i=0; i<10; i++) {
	if (abvec(i) != Int(i)) {
	    cout << "error in getColumn " << i << ": " << abvec(i) << endl;
	}
    }
    Array<float> arr1a = arr1.getColumn();
    if (arr1a.ndim() != 4) {
	cout << "arr1a not 4-dim" << endl;
    }
    i=0;
    uInt j0,j1,j2,j3;
    for (j3=0; j3<10; j3++)
	for (j2=0; j2<4; j2++)
	    for (j1=0; j1<3; j1++)
		for (j0=0; j0<2; j0++) {
		    if (arr1a(IPosition(4,j0,j1,j2,j3)) != i++) {
			cout <<"arr1a error at " <<j0<<" "<<j1<<" "<<j2<<" "
			    <<j3<<" should be: "<<i<<endl;
		    }
		}
    Array<float> arr1b = arr1.getColumn(nslice);
    if (arr1b.ndim() != 4) {
	cout << "arr1b not 4-dim" << endl;
    }
    i=0;
    for (j3=0; j3<10; j3++)
	for (j2=0; j2<4; j2++)
	    for (j1=0; j1<3; j1++)
		for (j0=0; j0<2; j0++) {
		    if (arr1b(IPosition(4,j0,j1,j2,j3)) != i++) {
			cout <<"arr1b error at " <<j0<<" "<<j1<<" "<<j2<<" "
			    <<j3<<" should be: "<<i<<endl;
		    }
		}

    // Sort the table.
    Table sortab = tab.sort ("ae", Sort::Descending);
    sortab.tableInfo().readmeAddLine ("a sortab line");
    if (sortab.nrow() != 10) {
	cout << "sortab does not contain 10 rows" << endl;
    }
    ScalarColumn<float> sorae(sortab, "ae");
    cout << sorae.getColumn() << endl;
    cout << "#columns in sortab: " << sortab.tableDesc().ncolumn() << endl;

    Table sortab2 = sortab.sort ("ad");
    if (sortab2.nrow() != 10) {
	cout << "sortab2 does not contain 10 rows" << endl;
    }
    ScalarColumn<uInt> sorad(sortab2, "ad");
    cout << sorad.getColumn() << endl;
    cout << "#columns in sortab2: " << sortab2.tableDesc().ncolumn() << endl;
    cout << "sortab2 type = " << sortab2.tableInfo().type() << endl;
    cout << "sortab2 subtype = " << sortab2.tableInfo().subType() << endl;
    cout << sortab2.tableInfo().readme() << endl;

    // Test using an empty selection.
    sortab = sortab(TableExprNode());
    AlwaysAssertExit (sortab.nrow() == sortab2.nrow());
    sortab2 = sortab2(TableExprNode(), 5);
    AlwaysAssertExit (sortab2.nrow() == 5);

    // Test using a const Bool expression.
    Table csortab = sortab(TableExprNode(2) + 3 == 5);
    AlwaysAssertExit (csortab.nrow() == sortab.nrow());
    csortab = sortab(TableExprNode(False));
    AlwaysAssertExit (csortab.nrow() == 0);
    csortab = sortab(TableExprNode(True), 5);
    AlwaysAssertExit (csortab.nrow() == 5);

    // Select using an empty set.
    // Select using the IN function.
    TableExprNodeSet set;
    Table seltabset = sortab (sortab.col("af").in (set));
    AlwaysAssertExit (seltabset.nrow() == 0);
    set.add (TableExprNodeSetElem ("V3"));
    set.add (TableExprNodeSetElem ("V1"));
    set.add (TableExprNodeSetElem ("V9"));
    set.add (TableExprNodeSetElem ("V6"));
    seltabset = sortab (sortab.col("af").in (set));
    if (seltabset.nrow() != 4) {
	cout << "seltabset does not contain 4 rows" << endl;
    }
    cout << seltabset.rowNumbers() << endl;;
    cout << seltabset.rowNumbers(tab) << endl;;
    cout << seltabset.rowNumbers(sortab) << endl;
    seltabset = sortab (sortab.col("arr1")(IPosition(3,0,0,0)) < 100);
    if (seltabset.nrow() != 5) {
	cout << "seltabset does not contain 5 rows" << endl;
    }
    seltabset = sortab (sortab.col("arr1")(IPosition(3,1,0,0)) == 97);
    if (seltabset.nrow() != 1) {
	cout << "seltabset does not contain 1 row" << endl;
    }

    // Select an empty table and use that as input for a select and sort.
    {
      Table selempty1 = sortab (sortab.col("ab") < -10);
      if (selempty1.nrow() != 0) {
        cout << "selempty1 is not empty" << endl;
      }
      Table selempty2 = selempty1 (selempty1.col("ab") < -10
				   ||  TableExprNode());
      if (selempty2.nrow() != 0) {
        cout << "selempty2 is not empty" << endl;
      }
      Table sorempty1 = selempty1.sort ("ab");
      if (sorempty1.nrow() != 0) {
        cout << "sorempty1 is not empty" << endl;
      }
    }

    // Get a subset of the table via row numbers.
    Vector<uInt> rownrs(4);
    rownrs(0)=3;
    rownrs(1)=1;
    rownrs(2)=9;
    rownrs(3)=6;
    Table seltab1 = sortab(rownrs);
    if (seltab1.nrow() != 4) {
	cout << "seltab1 does not contain 4 rows" << endl;
    }
    ScalarColumn<Int> sel1ab(seltab1, "ab");
    cout << sel1ab.getColumn() << endl;
    cout << "#columns in seltab1: " << seltab1.tableDesc().ncolumn() << endl;

    // Project the table.
    Block<String> projname(3);
    projname[0] = "ae";
    projname[1] = "ab";
    projname[2] = "arr1";
    Table seltab2 = seltab1.project (projname);
    if (seltab2.nrow() != 4) {
	cout << "seltab2 does not contain 4 rows" << endl;
    }
    ScalarColumn<Int> sel2ab(seltab2, "ab");
    cout << sel2ab.getColumn() << endl;
    cout << "#columns in seltab2: " << seltab2.tableDesc().ncolumn() << endl;

    // Get a subset via a mask.
    Block<Bool> mask(4,True);
    mask[0] = False;
    mask[3] = False;
    Table seltab3 = seltab2(mask);
    if (seltab3.nrow() != 2) {
	cout << "seltab3 does not contain 2 rows" << endl;
    }
    ScalarColumn<Int> sel3ab(seltab3, "ab");
    cout << sel3ab.getColumn() << endl;
    cout << "#columns in seltab3: " << seltab3.tableDesc().ncolumn() << endl;
    seltab3.tableDesc().show();

    Table xortab = sortab ^ seltab1;
    if (xortab.nrow() != 6) {
	cout << "xortab does not contain 6 rows" << endl;
    }
    ScalarColumn<Int> xorab(xortab, "ab");
    cout << xorab.getColumn() << endl;
    cout << "#columns in xortab: " << xortab.tableDesc().ncolumn() << endl;

    Table or1tab = xortab | seltab3;
    if (or1tab.nrow() != 8) {
	cout << "or1tab does not contain 8 rows" << endl;
    }
    ScalarColumn<Int> or1ab(or1tab, "ab");
    cout << or1ab.getColumn() << endl;
    cout << "#columns in or1tab: " << or1tab.tableDesc().ncolumn() << endl;

    Table or2tab = seltab3 | xortab;
    if (or2tab.nrow() != 8) {
	cout << "or2tab does not contain 8 rows" << endl;
    }
    ScalarColumn<Int> or2ab(or2tab, "ab");
    cout << or2ab.getColumn() << endl;
    cout << "#columns in or2tab: " << or2tab.tableDesc().ncolumn() << endl;

    Table exprtab = sortab(TableExprNode()  &&  sortab.col("ab") >= 5);
    if (exprtab.nrow() != 5) {
	cout << "exprtab does not contain 5 rows" << endl;
    }
    ScalarColumn<Int> exprab(exprtab, "ab");
    cout << exprab.getColumn() << endl;

    Table expr2tab = tab(tab.col("af") == "V3"  ||
			 (tab.col("ab") >= 5  &&  tab.col("ab") < 8));
    if (expr2tab.nrow() != 4) {
	cout << "expr2tab does not contain 4 rows" << endl;
    }
    ScalarColumn<Int> expr2ab(expr2tab, "ab");
    cout << expr2ab.getColumn() << endl;

    // Test persistency of reference tables.
    {
        Table ex1tab = tab(tab.col("ab") > 5);
	ex1tab.renameColumn ("abnew", "ab");
	AlwaysAssertExit (ex1tab.tableDesc().isColumn ("abnew"));
	AlwaysAssertExit (! ex1tab.tableDesc().isColumn ("ab"));
	AlwaysAssertExit (tab.tableDesc().isColumn ("ab"));
	AlwaysAssertExit (! tab.tableDesc().isColumn ("abnew"));
	ScalarColumn<Int> abcol(ex1tab, "abnew");
	cout << abcol.getColumn() << endl;
	cout << ">>>" << endl;
	ex1tab.rename ("tTable_tmp.ex1", Table::New);
	cout << "<<<" << endl;
    }
    {
        Table ex1tab ("tTable_tmp.ex1");
	AlwaysAssertExit (! ex1tab.tableDesc().isColumn ("ab"));
	ScalarColumn<Int> abcol(ex1tab, "abnew");
	Table ex2tab = ex1tab (ex1tab.col("abnew") > 6);
	AlwaysAssertExit (! ex2tab.tableDesc().isColumn ("ab"));
	ScalarColumn<Int> abcol2(ex2tab, "abnew");
	ex1tab.renameColumn ("abnew1", "abnew");
	ex2tab.renameColumn ("abnew2", "abnew");
	AlwaysAssertExit (ex1tab.tableDesc().isColumn ("abnew1"));
	AlwaysAssertExit (! ex1tab.tableDesc().isColumn ("abnew"));
	AlwaysAssertExit (! ex1tab.tableDesc().isColumn ("abnew2"));
	AlwaysAssertExit (ex2tab.tableDesc().isColumn ("abnew2"));
	AlwaysAssertExit (! ex2tab.tableDesc().isColumn ("abnew"));
	AlwaysAssertExit (! ex2tab.tableDesc().isColumn ("abnew1"));
	ScalarColumn<Int> abcola(ex1tab, "abnew1");
	ScalarColumn<Int> abcol2a(ex2tab, "abnew2");
    }
}

//# Test deletion of rows, array of Strings, and some more.
void c (Bool doExcp)
{
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<Int>("ac"));
    td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
    td.addColumn (ScalarColumnDesc<float>("ae"));
    td.addColumn (ScalarColumnDesc<String>("af"));
    td.addColumn (ScalarColumnDesc<DComplex>("ag"));
    td.addColumn (ArrayColumnDesc<float>("arr1",3,ColumnDesc::Direct));
    td.addColumn (ArrayColumnDesc<float>("arr2",0));
    td.addColumn (ArrayColumnDesc<String>("arr3",0,ColumnDesc::Direct));

    // Now create a new table from the description.
    SetupNewTable newtab("tTable_tmp.data1", td, Table::New);
    // Create a storage manager for it.
    StManAipsIO sm1;
    StManAipsIO sm2;
    newtab.bindAll (sm1);
    newtab.bindColumn ("ab",sm2);
    newtab.bindColumn ("ac",sm2);
    newtab.setShapeColumn("arr1",IPosition(3,2,3,4));
    newtab.setShapeColumn("arr3",IPosition(1,2));
    Table tab(newtab);
    tab.rename ("tTable_tmp.data2", Table::New);
    tab.rename ("tTable_tmp.data2", Table::New);
    tab.rename ("tTable_tmp.data2", Table::Scratch);
    tab.rename ("tTable_tmp.data2a", Table::Scratch);
    tab.rename ("tTable_tmp.data2a", Table::Scratch);
    tab.rename ("tTable_tmp.data2a", Table::New);
    tab.rename ("tTable_tmp.data2", Table::Scratch);
    tab.rename ("tTable_tmp.data2a", Table::New);
    if (doExcp) {
	try {
	    // Create a normal file, so rename will fail.
	    RegularFile file("tTable_tmp.file");
	    file.create();
	    tab.rename ("tTable_tmp.file", Table::NewNoReplace);
	} catch (AipsError& x) {
            // exists as file
	    cout << "Expected exception: " << removeDir(x.getMesg()) << endl;
	} 
	try {
	    tab.rename ("tTable_tmp.data", Table::NewNoReplace);
	} catch (AipsError& x) {
            // already exists
	    cout << "Expected exception: " << removeDir(x.getMesg()) << endl;
	} 
	try {
	    tab.rename ("tTable.datx", Table::Update);
	} catch (AipsError& x) {
            // does not exist
	    cout << "Expected exception: " << removeDir(x.getMesg()) << endl;
	} 
	try {
	    tab.addColumn (ScalarColumnDesc<Int>("ab"));
	} catch (AipsError& x) {
            // column already exists
	    cout << "Expected exception: " << x.getMesg() << endl;
	} 
    }

    // Rename a column. It'll be renamed back later.
    tab.renameColumn ("acnew", "ac");

    ScalarColumn<Int> ab1(tab,"ab");
    ScalarColumn<Int> ab2(tab,"ab");
    ScalarColumn<Int> ac (tab,"acnew");
    ScalarColumn<uInt> ad(tab,"ad");
    ScalarColumn<float> ae(tab,"ae");
    ScalarColumn<String> af(tab,"af");
    TableColumn ag1(tab,"ag");
    ScalarColumn<DComplex> ag(tab,"ag");
    ArrayColumn<float> arr1(tab,"arr1");
    ArrayColumn<float> arr2(tab,"arr2");
    ArrayColumn<String> arr3(tab,"arr3");
    Cube<float> arrf(IPosition(3,2,3,4));
    Vector<String> vecstr (stringToVector ("0,1,23,4,5,6,7,8,9,100,"
					   "1,2,34,5,6,7,8,9,0,101"));
    uInt i;
    char str[8];
    indgen (arrf);
    for (i=0; i<10; i++) {
	tab.addRow();
	ab1.put (i, i);
	ac.put (i, i+1);
	ad.put (i, i+2);
	ae.put (i, i+3);
	sprintf (str, "V%i", i);
	af.put (i, str);
	arr1.put(i,arrf);
	arr2.put(i,arrf);
	arr3.put(i,vecstr(Slice(i*2,2)));
	arrf += (float)(arrf.nelements());
    }
    ag1.putColumn (ad);
    arrf -= (float)(arrf.nelements()*tab.nrow());

    // Rename the column back.
    tab.renameColumn ("ac", "acnew");

    //# Select some rows from the table.
    Table expr2tab = tab(tab.col("af") == "V3"  ||
			 (tab.col("ab") >= 5  &&  tab.col("ab") < 8));
    if (expr2tab.nrow() != 4) {
	cout << "expr2tab does not contain 4 rows" << endl;
    }
    ScalarColumn<Int> expr2ab(expr2tab, "ab");
    cout << expr2ab.getColumn() << endl;
    if (!allEQ (expr2tab.rowNumbers(), expr2tab.rowNumbers(tab))) {
        cout << "error in expr2tab.rowNumbers()" << endl;
    }
    
    //# Remove 2 rows from the selected rows.
    expr2tab.removeRow (1);
    expr2tab.removeRow (2);
    if (expr2tab.nrow() != 2) {
	cout << "expr2tab does not contain 2 rows" << endl;
    }
    cout << expr2ab.getColumn() << endl;

    //# Remove those rows.
    tab.removeRow (expr2tab.rowNumbers());
    if (tab.nrow() != 8) {
	cout << "tab does not contain 8 rows" << endl;
    }
    cout << ab2.getColumn() << endl;
    tab.removeRow (7);
    if (tab.nrow() != 7) {
	cout << "tab does not contain 7 rows" << endl;
    }
    if (doExcp) {
	try {
	    tab.removeRow (7);
	} catch (AipsError& x) {
            // row does not exist
	    cout << "Expected exception: " << removeDir(x.getMesg()) << endl;
	} 
    }
    cout << ab2.getColumn() << endl;

    //# Check if the values are still okay.
    Int abval;
    uInt adval;
    DComplex agval;
    Cube<float> arrval(IPosition(3,2,3,4));
    for (i=0; i<tab.nrow(); i++) {
	ab2.get (i, abval);
	ad.get  (i, adval);
	ag.get  (i, agval);
	if (Int(adval) != abval+2  ||  agval != DComplex(abval+2)) {
	    cout << "after remove error in row " << i << ": " << abval
		 << ", " << adval << ", " << agval << endl;
	}
	arr1.get (i, arrval);
	if (!allEQ (arrval, arrf + float(arrf.nelements()*abval))) {
	    cout << "after remove error in arr1 in row " << i << endl;
	}
	arr2.get (i, arrval);
	if (!allEQ (arrval, arrf + float(arrf.nelements()*abval))) {
	    cout << "after remove error in arr2 in row " << i << endl;
	}
	if (!allEQ (arr3(i), vecstr(Slice(2*abval,2)))) {
	    cout << "after remove error in arr3 in row " << i << endl;
	}
	cout << arr3(i) << endl;
    }
    Matrix<String> matstr;
    arr3.getColumn (matstr);
    cout << matstr << endl;
}


void d()
{
    Vector<Complex> arrf2(100);
    indgen (arrf2);
    {
	// Build the table description.
	TableDesc td("", "1", TableDesc::Scratch);
	td.addColumn (ScalarColumnDesc<Int>("ab","Comment for column ab"));
	td.addColumn (ScalarColumnDesc<uInt>("ad","comment for ad"));
	td.addColumn (ScalarColumnDesc<Complex>("ag"));
	td.addColumn (ArrayColumnDesc<String> ("arr1",1,ColumnDesc::Direct));
	td.addColumn (ArrayColumnDesc<Complex>("arr2",0));
	td.addColumn (ArrayColumnDesc<Int>    ("arr3",0,ColumnDesc::Direct));
	
	// Now create a new table from the description.
	SetupNewTable newtab("tTable_tmp.data3", td, Table::New);
	// Create a storage manager for it.
	StManAipsIO sm1;
	newtab.bindAll (sm1);
	newtab.setShapeColumn ("arr1",IPosition(1,2));
	newtab.setShapeColumn ("arr3",IPosition(2,2,2));
	Table tab(newtab);

	uInt i;
	ScalarColumn<Int>  ab(tab,"ab");
	ScalarColumn<uInt> ad(tab,"ad");
	TableColumn ag1(tab,"ag");
	ArrayColumn<String>   arr1(tab,"arr1");
	ArrayColumn<Complex>  arr2(tab,"arr2");
	ArrayColumn<Int>      arr3(tab,"arr3");
	Vector<Complex> arrf(IPosition(1,3));
	Matrix<Int>     arri(IPosition(2,2,2));
	Vector<String>  arrs (stringToVector ("aa,bbb"));
	indgen (arrf);
	indgen (arri);
	for (i=0; i<10000; i++) {
	    tab.addRow();
	    ab.put (i, i);
	    ad.put (i, i+2);
	    arr1.put(i,arrs);
	    arr2.put(i,arrf);
	    arr3.put(i,arri);
	    arrf += (Complex)(arrf.nelements());
	    arri += (Int)(arri.nelements());
	}
	ag1.putColumn (ad);
    }
    {
	Table tab ("tTable_tmp.data3");
	ScalarColumn<Int>     ab(tab,"ab");
	ScalarColumn<uInt>    ad(tab,"ad");
	ScalarColumn<Complex> ag(tab,"ag");
	ArrayColumn<String>  arr1(tab,"arr1");
	ArrayColumn<Complex> arr2(tab,"arr2");
	ArrayColumn<Int>     arr3(tab,"arr3");
	Int abval;
	uInt adval;
	Complex agval;
	Vector<Complex> arrf(IPosition(1,3));
	Matrix<Int>     arri(IPosition(2,2,2));
	Vector<String>  arrs (stringToVector ("aa,bbb"));
	indgen (arrf);
	indgen (arri);
	uInt i;
	for (i=0; i<10; i++) {
	    ab.get (i, abval);
	    ad.get (i, adval);
	    ag.get (i, agval);
	    if (abval != Int(i)  ||  adval != i+2  ||  agval != Complex(i+2)) {
		cout << "error in row " << i << ": " << abval
		    << ", " << adval << ", " << agval << endl;
	    }
	    if (!allEQ (arr1(i), arrs)) {
		cout << "error in arr1 in row " << i << endl;
	    }
	    if (!allEQ (arr2(i), arrf)) {
		cout << "error in arr2 in row " << i << endl;
	    }
	    if (!allEQ (arr3(i), arri)) {
		cout << "error in arr3 in row " << i << endl;
	    }
	    arrf += (Complex)(arrf.nelements());
	    arri += (Int)(arri.nelements());
	}

	// Open the same table read/write.
	Table rwtab ("tTable_tmp.data3", Table::Update);
	AlwaysAssertExit (rwtab.isWritable());
	for (i=0; i<rwtab.tableDesc().ncolumn(); i++) {
	    AlwaysAssertExit (rwtab.isColumnWritable (i));
	}
	ScalarColumn<Int>    rwab(rwtab,"ab");
	ArrayColumn<Complex> rwarr2(rwtab,"arr2");
	rwab.put (0,1);
	rwarr2.put (0,arrf);
	if (!allEQ( arr2(0), arrf)) {
	    cout << "first error in arr2" << endl;
	}
	rwarr2.put (0,arrf2);
	if (!allEQ( arr2(0), arrf2)) {
	    cout << "second error in arr2" << endl;
	}
    }
    {
	Table tab ("tTable_tmp.data3");
	ScalarColumn<Int>     ab(tab,"ab");
	ScalarColumn<uInt>    ad(tab,"ad");
	ScalarColumn<Complex> ag(tab,"ag");
	ArrayColumn<String>  arr1(tab,"arr1");
	ArrayColumn<Complex> arr2(tab,"arr2");
	ArrayColumn<Int>     arr3(tab,"arr3");
	Int abval;
	uInt adval;
	Complex agval;
	Vector<Complex> arrf(IPosition(1,3));
	Matrix<Int>     arri(IPosition(2,2,2));
	Vector<String>  arrs (stringToVector ("aa,bbb"));
	indgen (arrf);
	indgen (arri);
	uInt i;
	uInt nrow = tab.nrow();
	for (i=0; i<nrow; i++) {
	    ab.get (i, abval);
	    if (i == 0) {
		abval--;
	    }
	    ad.get (i, adval);
	    ag.get (i, agval);
	    if (abval != Int(i)  ||  adval != i+2  ||  agval != Complex(i+2)) {
		cout << "error in row " << i << ": " << abval
		    << ", " << adval << ", " << agval << endl;
	    }
	    if (!allEQ (arr1(i), arrs)) {
		cout << "error in arr1 in row " << i << endl;
	    }
	    if (i > 0) {
		if (!allEQ (arr2(i), arrf)) {
		    cout << "error in arr2 in row " << i << endl;
		}
	    }
	    if (!allEQ (arr3(i), arri)) {
		cout << "error in arr3 in row " << i << endl;
	    }
	    arrf += (Complex)(arrf.nelements());
	    arri += (Int)(arri.nelements());
	}
	if (!allEQ( arr2(0), arrf2)) {
	    cout << "error in rereading arr2" << endl;
	}
        // Check the locked tables.
        Vector<String> vec (Table::getLockedTables());
        AlwaysAssertExit (vec.size() == 1);
        AlwaysAssertExit (removeDir(vec[0]) == "tTable_tmp.data3");
    }
    // No locked tables should be left.
    AlwaysAssertExit (Table::getLockedTables().size() == 0);
}

int main (int argc,const char*[])
{
    try {
	Table::setScratchCallback (cbFunc);
	a ( (argc<2));
	b ( (argc<2));
	c ( (argc<2));
        d ();
    } catch (AipsError& x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
