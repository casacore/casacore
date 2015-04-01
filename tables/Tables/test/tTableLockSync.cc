//# tTableLockSync.cc: Interactive test program for concurrent access to tables
//# Copyright (C) 1997,1998,1999,2000,2001
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
#include <casacore/tables/Tables/TableLocker.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the Table classes
// </summary>

// This program tests concurrent access to tables.


// Create the table.
void a()
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<Int>("col1"));
    td.addColumn (ScalarColumnDesc<Int>("col2"));
    td.addColumn (ScalarColumnDesc<Int>("col3"));
    td.addColumn (ScalarColumnDesc<String>("cols"));
    td.addColumn (ArrayColumnDesc<float> ("Pol", IPosition(1,16),
					  ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float> ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float> ("Data", 2, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float> ("Data2", 2, ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  2,
			  stringToVector ("Data"),
			  stringToVector ("Pol,Freq"));
    td.defineHypercolumn ("TSMExample2",
			  3,
			  stringToVector ("Data2"));
    td.rwKeywordSet().define ("k0", Int(0));

    // Now create a new table from the description.
    SetupNewTable newtab("tTableLockSync_tmp.tab", td, Table::New);
    StManAipsIO sm1;
    StandardStMan sm2(128);
    IncrementalStMan sm3;
    TiledCellStMan sm4 ("TSMExample", IPosition(2,5,6));
    TiledShapeStMan sm5 ("TSMExample2", IPosition(2,5,6));
    newtab.setShapeColumn ("Freq", IPosition(1,25));
    newtab.setShapeColumn ("Data", IPosition(2,16,25));
    newtab.setShapeColumn ("Data2", IPosition(2,16,25));
    newtab.bindAll (sm4);
    newtab.bindColumn ("col1", sm1);
    newtab.bindColumn ("col2", sm2);
    newtab.bindColumn ("cols", sm2);
    newtab.bindColumn ("col3", sm3);
    newtab.bindColumn ("Data2", sm5);
    Table tab(newtab, 1);
}

void b (Bool noReadLocking, Bool permLocking)
{
    // Open the table for update with UserLocking.
    TableLock lt(TableLock::UserLocking);
    if (permLocking) {
        lt = TableLock::PermanentLocking;
    } else if (noReadLocking) {
        lt = TableLock::UserNoReadLocking;
    }
    Table tab("tTableLockSync_tmp.tab", lt, Table::Update);
    try {
	TableLocker lock1 (tab, FileLocker::Write, 1);
    } catch (AipsError x) {
	cout << "table is write-locked" << endl;
    } 
    ScalarColumn<Int> col1 (tab, "col1");
    ScalarColumn<Int> col2 (tab, "col2");
    ScalarColumn<Int> col3 (tab, "col3");
    ScalarColumn<String> cols (tab, "cols");
    ArrayColumn<float> freq (tab, "Freq");
    ArrayColumn<float> pol (tab, "Pol");
    ArrayColumn<float> data (tab, "Data");
    ArrayColumn<float> data2 (tab, "Data2");

    Vector<float> freqValues(25);
    Vector<float> polValues(16);
    Matrix<float> dataValues(IPosition(2,16,25));
    Matrix<float> data2Values(IPosition(2,16,25));
    Int opt, rownr, val;
    while (True) {
	cout << "0=quit, 1=quit/delete, 2=rdlock, 3=rdlockw, 4=wrlock, 5=wrlockw, 6=unlock" << endl;
	cout << "7=status, 8=get, 9=put, 10=rdkey, 11=wrkey, 12=flush, 13=resync" << endl;
	cout << "14=hasChanged: ";
	cin >> opt;
	if (opt <= 1) {
	    break;
	} else if (opt == 2) {
	    if (! tab.lock (False, 1)) {
		cout << "Could not acquire a read lock" << endl;
	    }
	} else if (opt == 3) {
	    if (! tab.lock (False, 0)) {
		cout << "Could not acquire a read lock" << endl;
	    }
	} else if (opt == 4) {
	    if (! tab.lock (True, 1)) {
		cout << "Could not acquire a write lock" << endl;
	    }
	} else if (opt == 5) {
	    if (! tab.lock (True, 0)) {
		cout << "Could not acquire a write lock" << endl;
	    }
	} else if (opt == 6) {
	    tab.unlock();
	} else if (opt == 7) {
	  cout << " hasReadLock=" << tab.hasLock (FileLocker::Read) << endl;
	  cout << " hasWriteLock=" << tab.hasLock (FileLocker::Write) << endl;
	  cout << " isMultiUsed = " << tab.isMultiUsed() << endl;
	} else if (opt == 12) {
	    tab.flush();
	} else if (opt == 13) {
	    tab.resync();
	    cout << "nrows=" << tab.nrow() << endl;
	} else if (opt == 14) {
	    cout << "hasDataChanged = " << tab.hasDataChanged() << endl;
	} else {
	  if (opt == 8  ||  opt == 9) {
	    // First test if get or put is possible (using row 0).
	    Bool err = False;
	    try {
	        col1.get (0, val);
		if (opt == 9) {
		    col1.put (0, val);
		}
	    } catch (AipsError x) {
	        cout << x.getMesg() << endl;
		err = True;
	    } 
	    if (!err) {
	        cout << "rownr: ";
		cin >> rownr;
		if (opt == 9) {
		    cout << "value: ";
		    cin >> val;
		    if (rownr >= Int(tab.nrow())) {
		        Int n = 1 + rownr - tab.nrow();
			tab.addRow (n);
			cout << "added " << n << " rows" << endl;
		    }
		    col1.put (rownr, val);
		    col2.put (rownr, val+1);
		    col3.put (rownr, val+2);
		    cols.put (rownr, "ARatherLongTestString" +
			             String::toString(val));
		    indgen (freqValues, float(val+2));
		    indgen (polValues, float(val+3));
		    indgen (dataValues, float(val+4));
		    indgen (data2Values, float(val+5));
		    data.put (rownr, dataValues);
		    freq.put (rownr, freqValues);
		    pol.put (rownr, polValues);
		    data2.put (rownr, data2Values);
		}else{
		    if (rownr >= Int(tab.nrow())) {
		        cout << "Only " << tab.nrow()
			     << " rows in table" << endl;
		    }else{
		        cout << "Row " << rownr << " has value "
			     << col1(rownr) << ' ' << col2(rownr) << ' '
			     << col3(rownr) << ' ' << cols(rownr) << ' '
			     << freq(rownr)(IPosition(1,0)) << '-'
			     << freq(rownr)(IPosition(1,24)) - 24 << ' '
			     << pol(rownr)(IPosition(1,0)) << '-'
			     << pol(rownr)(IPosition(1,15)) - 15 << ' '
			     << data(rownr)(IPosition(2,0,0)) << '-'
			     << data(rownr)(IPosition(2,15,24)) - 399 << ' '
			     << data2(rownr)(IPosition(2,0,0)) << '-'
			     << data2(rownr)(IPosition(2,15,24)) - 399 << endl;
		    }
		}
	    }
	  } else {
	    // First test if get or put is possible (using key k0).
	    Bool err = False;
	    try {
	        val = tab.keywordSet().asInt ("k0");
		if (opt == 11) {
		    tab.rwKeywordSet().define ("k0", val);
		}
	    } catch (AipsError x) {
	        cout << x.getMesg() << endl;
		err = True;
	    } 
	    if (!err) {
		cout << "keyword name: ";
		String name;
		cin >> name;
		if (opt == 10) {
		  if (! tab.keywordSet().isDefined (name)) {
		    cout << "Keyword " << name << " does not exist" << endl;
		  } else {
		    cout << tab.keywordSet().asInt (name) << endl;
		  }
		} else {
		    cout << "value: ";
		    cin >> val;
		    tab.rwKeywordSet().define(name, val);
		}
	    }
	  }
	}
    }
    if (opt == 1) {
	Table tabd ("tTableLockSync_tmp.tab", Table::Delete);
    }
}


int main (int argc, const char* argv[])
{
    if (argc < 2) {
	cout << "Execute as: tTableLockSync 1 x  to create new table"
	     << endl;
	cout << "            tTableLockSync 0 x  to update existing table"
	     << endl;
	cout << "where x=1 means NoReadLocking and x=2 means PermanentLocking"
	     << endl;
    }else{
	try {
	    Bool noReadLocking = False;
	    Bool permLocking = False;
	    if (argc >= 3) {
	        if (*(argv[2]) == '1') {
		    noReadLocking = True;
	        } else if (*(argv[2]) == '2') {
		    permLocking = True;
		}
	    }
	    if (*(argv[1]) == '1') {
		a();
	    }
	    b (noReadLocking, permLocking);
	} catch (AipsError x) {
	    cout << "Caught an exception: " << x.getMesg() << endl;
	    return 1;
	} 
    }
    return 0;                           // exit with success status
}
