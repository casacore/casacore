//# tTableLockSync.cc: Test program for concurrent access to tables
//# Copyright (C) 1997,1999,2000,2001,2003
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
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>
#include <unistd.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for the Table classes
// </summary>

// This program tests concurrent access to tables.
// It is doing this by running the program simultaneously in different
// processes and with different options.
// It can be run as follows:
//   tTableLockSync_2 option inspection_interval wait_period get_put nrrow/wait
//        option               1=Permanent 2=PermanentWait 3=Auto 4=User
//        inspection_interval  only meaningful for Auto (in seconds)
//        wait_period          period to sleep before next get or put
//        get_put              0=get 1=put
//        nrrow                #rows to put

void tlock (Table& tab, Bool write, Bool show)
{
    if (show) {
	Time time;
	double sec = time.modifiedJulianDay() * 86400;
	cout << time << ' ' << sec - floor(sec) << ": ";
	cout << "Starting to lock" << endl;
    }
    tab.lock (write);
    if (show) {
	Time time1;
	double sec1 = time1.modifiedJulianDay() * 86400;
	cout << time1 << ' ' << sec1 - floor(sec1) << ": ";
	cout << "Lock acquired" << endl;
    }
}
void tunlock (Table& tab, Bool show)
{
    if (show) {
	Time time;
	double sec = time.modifiedJulianDay() * 86400;
	cout << time << ' ' << sec - floor(sec) << ": ";
	cout << "Starting to unlock" << endl;
    }
    tab.unlock();
    if (show) {
	Time time1;
	double sec1 = time1.modifiedJulianDay() * 86400;
	cout << time1 << ' ' << sec1 - floor(sec1) << ": ";
	cout << "Unlock ended" << endl;
    }
}

// Create the table.
void a()
{
    // Check if the table exists.
    // If not, create it.
    if (Table::isReadable ("tTableLockSync_2_tmp.tab")) {
	return;
    }
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.addColumn (ScalarColumnDesc<uInt>("seq"));
    td.addColumn (ScalarColumnDesc<Int>("col1"));
    td.addColumn (ScalarColumnDesc<Int>("col2"));
    td.addColumn (ArrayColumnDesc<float> ("Pol", IPosition(1,16),
					  ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float> ("Freq", 1, ColumnDesc::FixedShape));
    td.addColumn (ArrayColumnDesc<float> ("Data", 2, ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
//			  2,
			  3,
			  stringToVector ("Data"),
//			  stringToVector ("Pol,Freq"));
			  stringToVector ("Pol,Freq,seq"));
    td.rwKeywordSet().define ("seqnr", uInt(0));

    // Now create a new table from the description.
    SetupNewTable newtab("tTableLockSync_2_tmp.tab", td, Table::New);
    StManAipsIO sm1;
    IncrementalStMan sm2;
//    TiledCellStMan sm3 ("TSMExample", IPosition(2,5,6));
    TiledColumnStMan sm3 ("TSMExample", IPosition(3,5,6,1));
    newtab.setShapeColumn ("Freq", IPosition(1,25));
    newtab.setShapeColumn ("Data", IPosition(2,16,25));
    newtab.bindAll (sm3);
//    newtab.bindColumn ("seq", sm1);
    newtab.bindColumn ("col1", sm1);
//    newtab.bindColumn ("seq", sm2);
//    newtab.bindColumn ("col1", sm2);
    newtab.bindColumn ("col2", sm2);
    Table tab(newtab);
}

void b (const TableLock& lockMode, uInt wait, uInt nrrow, Bool show)
{
    // Check if user locking.
    Bool userLocking =  (lockMode.option() == TableLock::UserLocking);
    // Open the table for update.
    Table tab ("tTableLockSync_2_tmp.tab", lockMode, Table::Update);
    ScalarColumn<uInt> seq (tab, "seq");
    ScalarColumn<Int> col1 (tab, "col1");
    ScalarColumn<Int> col2 (tab, "col2");
    ArrayColumn<float> freq (tab, "Freq");
    ArrayColumn<float> pol (tab, "Pol");
    ArrayColumn<float> data (tab, "Data");
    // Get and update the sequencenumber.
    if (userLocking) tlock(tab, True, show);
    TableRecord& keyset = tab.rwKeywordSet();
    uInt seqnr = keyset.asuInt ("seqnr");
    seqnr++;
    keyset.define ("seqnr", seqnr);
    if (userLocking) tunlock (tab, show);

    Vector<float> freqValues(25);
    Vector<float> polValues(16);
    Matrix<float> dataValues(IPosition(2,16,25));
    Int rownr, val;
    for (uInt i=0; i<nrrow; i++) {
	if (userLocking) tlock (tab, True, show);
	if (show) {
	    Time time;
	    double sec = time.modifiedJulianDay() * 86400;
	    cout << time << ' ' << sec - floor(sec) << ": ";
	    cout << "Starting new row" << endl;
	}
	tab.addRow();
	rownr = tab.nrow() - 1;
	val = rownr + 10;
	if (show) {
	    cout << "Writing row " << rownr << " by process " << seqnr << endl;
	}
	seq.put (rownr, seqnr);
	col1.put (rownr, val);
	col2.put (rownr, val+1);
	indgen (freqValues, float(val+2));
	indgen (polValues, float(val+3));
	indgen (dataValues, float(val+4));
	data.put (rownr, dataValues);
	freq.put (rownr, freqValues);
	pol.put (rownr, polValues);
	if (userLocking) tunlock (tab, show);
	if (wait > 0) {
	    sleep (wait);
	}
    }
}

void c (const TableLock& lockMode, uInt wait, uInt lastWait, Bool show)
{
    // Check if user locking.
    Bool userLocking =  (lockMode.option() == TableLock::UserLocking);
    // Open the table for read.
    Table tab ("tTableLockSync_2_tmp.tab", lockMode);
    ScalarColumn<uInt> seq (tab, "seq");
    ScalarColumn<Int> col1 (tab, "col1");
    ScalarColumn<Int> col2 (tab, "col2");
    ArrayColumn<float> freq (tab, "Freq");
    ArrayColumn<float> pol (tab, "Pol");
    ArrayColumn<float> data (tab, "Data");
    Block<uInt> count;
    Time* lastTime = 0;

    Vector<float> freqValues;
    Vector<float> polValues;
    Matrix<float> dataValues;
    Int val;
    uInt oldNrrow = 0;
    uInt nrrow = 0;
    while (True) {
	if (userLocking) tlock (tab, False, show);
	nrrow = tab.nrow();
	for (uInt rownr=oldNrrow; rownr<nrrow; rownr++) {
	    if (show) {
		Time time;
		double sec = time.modifiedJulianDay() * 86400;
		cout << time << ' ' << sec - floor(sec) << ": ";
		cout << "Reading row " << rownr << endl;
	    }
	    val = rownr + 10;
	    Int result;
	    result = col1(rownr);
	    if (result != val) {
		cout << "col1 in row " << rownr << " has value " << result
		     << endl;
	    }
	    result = col2(rownr);
	    if (result != val+1) {
		cout << "col2 in row " << rownr << " has value " << result
		     << endl;
	    }
	    freq.get (rownr, freqValues);
	    if (freqValues (IPosition(1,0)) != val+2
		||  freqValues (IPosition(1,24)) != val+2+24) {
		cout << "freq in row " << rownr << " has value "
		     << freqValues (IPosition(1,0)) << '-'
		     << freqValues (IPosition(1,24)) << endl;
	    }
	    pol.get (rownr, polValues);
	    if (polValues (IPosition(1,0)) != val+3
		||  polValues (IPosition(1,15)) != val+3+15) {
		cout << "pol in row " << rownr << " has value "
		     << polValues (IPosition(1,0)) << '-'
		     << polValues (IPosition(1,15)) << endl;
	    }
	    data.get (rownr, dataValues);
	    if (dataValues (IPosition(2,0,0)) != val+4
		||  dataValues (IPosition(2,15,24)) != val+4+399) {
		cout << "data in row " << rownr << " has value "
		     << dataValues (IPosition(2,0,0)) << '-'
		     << dataValues (IPosition(2,15,24)) << endl;
	    }
	    result = seq(rownr);
	    uInt nr = count.nelements();
	    if (result >= Int(nr)) {
		count.resize (result+1);
		for (Int i=nr; i<=result; i++) {
		    count[i] = 0;
		}
	    }
	    count[result]++;
	}
	if (userLocking) tunlock (tab, show);
	if (wait > 0) {
	    sleep (wait);
	}
	// When no more rows in last wait, stop the program.
	if (nrrow == oldNrrow) {
	    if (lastTime != 0) {
		if (lastTime->age() > lastWait) {
		    break;
		}
	    }else{
		lastTime = new Time();
	    }
	}else{
	    delete lastTime;
	    lastTime = 0;
	    oldNrrow = nrrow;
	}
    }
    delete lastTime;
    cout << "seqnr\t#rows" << endl;
    uInt nrread = 0;
    for (uInt i=0; i<count.nelements(); i++) {
	if (count[i] > 0) {
	    cout << i << '\t' << count[i] << endl;
	    nrread += count[i];
	}
    }
    cout << "total\t" << nrread << " rows read" << endl;
    if (nrread != nrrow) {
	cout << " *** but nrrow = " << nrrow << " in table" << endl;
    }
}


int main (int argc, const char* argv[])
{
    if (argc < 6) {
	cout << "Execute as:" << endl;
	cout << "  tTableLockSync_2 option inspection_interval "
	        "wait_period get_put nrrow/lastWait show"
	     << endl;
	cout << "    option   1=Permanent 2=PermanentWait 3=Auto 4=User"
	     << endl;
	cout << "    get_put  0=get else=put" << endl;
	cout << "    When putting, last parameter is #rows to put" << endl;
	cout << "    When getting, last parameter is 'no change' period"
	        " to stop" << endl;
	return 0;
    }
    Bool show =  (argc > 6);
	
    uInt var[5];
    for (uInt i=0; i<5; i++) {
	istringstream str(argv[i+1]);
	str >> var[i];
    }
    
    // Determine the correct locking mode.
    TableLock lockMode (TableLock::UserLocking);
    if (var[0] == 1) {
	lockMode = TableLock(TableLock::PermanentLocking);
    } else if (var[0] == 2) {
	lockMode = TableLock(TableLock::PermanentLockingWait);
    } else if (var[0] == 3) {
	lockMode = TableLock(TableLock::AutoLocking, var[1]);
    }
    Bool getsw = (var[3] == 0);
    try {
	if (!getsw) {
	    a();
	    b (lockMode, var[2], var[4], show);
	}else{
	    c (lockMode, var[2], var[4], show);
	}
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
