//# tTableLockSync.cc: Interactive test program for concurrent access to tables
//# Copyright (C) 1997,1998
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
#include <aips/Tables/TableLocker.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Tables/TiledCellStMan.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>

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
    td.addColumn (ArrayColumnDesc<float> ("Pol", IPosition(1,16),
					  ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float> ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float> ("Data", 2, ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  2,
			  stringToVector ("Data"),
			  stringToVector ("Pol,Freq"));

    // Now create a new table from the description.
    SetupNewTable newtab("tTableLockSync_tmp.tab", td, Table::New);
    StManAipsIO sm1;
    IncrementalStMan sm2;
    TiledCellStMan sm3 ("TSMExample", IPosition(2,5,6));
    newtab.setShapeColumn ("Freq", IPosition(1,25));
    newtab.setShapeColumn ("Data", IPosition(2,16,25));
    newtab.bindAll (sm3);
    newtab.bindColumn ("col1", sm1);
    newtab.bindColumn ("col2", sm2);
    Table tab(newtab);
}

void b()
{
    // Open the table for update with UserLocking.
    Table tab ("tTableLockSync_tmp.tab", TableLock(TableLock::UserLocking),
	       Table::Update);
    try {
	TableLocker lock1 (tab, FileLocker::Write, 1);
	tab.addRow();
    } catch (AipsError x) {
	cout << "table is write-locked" << endl;
    } end_try;
    ScalarColumn<Int> col1 (tab, "col1");
    ScalarColumn<Int> col2 (tab, "col2");
    ArrayColumn<float> freq (tab, "Freq");
    ArrayColumn<float> pol (tab, "Pol");
    ArrayColumn<float> data (tab, "Data");

    Vector<float> freqValues(25);
    Vector<float> polValues(16);
    Matrix<float> dataValues(IPosition(2,16,25));
    Int opt, rownr, val;
    while (True) {
	cout << "0=quit, 1=quit/delete, 2=rdlock, 3=rdlockwait, 4=wrlock, 5=wrlockwait" << endl;
	cout << "6=get, 7=put, 8=unlock, 9=hasChanged, 10=multiUsed: ";
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
	} else if (opt == 8) {
	    tab.unlock();
	} else if (opt == 9) {
	    cout << "hasDataChanged = " << tab.hasDataChanged() << endl;
	} else if (opt == 10) {
	    cout << "isMultiUsed = " << tab.isMultiUsed() << endl;
	} else {
	    if (! tab.hasLock (ToBool(opt==7))) {
		cout << "Cannot get/put; table is not locked" << endl;
	    }else{
		cout << "rownr: ";
		cin >> rownr;
		if (opt == 7) {
		    cout << "value: ";
		    cin >> val;
		    if (rownr >= Int(tab.nrow())) {
			Int n = 1 + rownr - tab.nrow();
			tab.addRow (n);
			cout << "added " << n << " rows" << endl;
		    }
		    col1.put (rownr, val);
		    col2.put (rownr, val+1);
		    indgen (freqValues.ac(), float(val+2));
		    indgen (polValues.ac(), float(val+3));
		    indgen (dataValues.ac(), float(val+4));
		    data.put (rownr, dataValues);
		    freq.put (rownr, freqValues);
		    pol.put (rownr, polValues);
		}else{
		    if (rownr >= Int(tab.nrow())) {
			cout << "Only " << tab.nrow()
			     << " rows in table" << endl;
		    }else{
			cout << "Row " << rownr << " has value "
			    << col1(rownr) << ' ' << col2(rownr) << ' '
			    << freq(rownr)(IPosition(1,0)) << '-'
			    << freq(rownr)(IPosition(1,24)) - 24 << ' '
			    << pol(rownr)(IPosition(1,0)) << '-'
			    << pol(rownr)(IPosition(1,15)) - 15 << ' '
			    << data(rownr)(IPosition(2,0,0)) << '-'
			    << data(rownr)(IPosition(2,15,24)) - 399 << endl;
		    }
		}
	    }
	}
    }
    if (opt == 1) {
	Table tabd ("tTableLockSync_tmp.tab", Table::Delete);
    }
}


main (int argc, char** argv)
{
    if (argc < 2) {
	cout << "Execute as: tTableLockSync 1  to create new table"
	     << endl;
	cout << "            tTableLockSync 0  to update existing table"
	     << endl;
    }else{
	try {
	    if (*(argv[1]) == '1') {
		a();
	    }
	    b();
	} catch (AipsError x) {
	    cout << "Caught an exception: " << x.getMesg() << endl;
	    return 1;
	} end_try;
    }
    return 0;                           // exit with success status
}
