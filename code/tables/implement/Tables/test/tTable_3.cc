//# tTable_3.cc: Program to test some performance aspects of the table system
//# Copyright (C) 1998
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
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Tables/StManAipsIO.h>
#include <aips/Tables/IncrementalStMan.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Lattices/Slicer.h>
#include <aips/OS/Timer.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <iostream.h>
#include <stdio.h>


// <summary>
// Program to test some performance aspects of the table system.
// </summary>


// First build a description.
void a (uInt nrrow)
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class Table";
    td.addColumn (ScalarColumnDesc<uInt>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<Int>("ad","comment for ad"));

    // Now create a new table from the description.
    // Use copy constructor to test if it works fine.
    // (newtab and newtabcp have the same underlying object).
    SetupNewTable newtab("tTable_tmp.data", td, Table::New);
    IncrementalStMan stman2;
    newtab.bindColumn ("ad", stman2);
    Table tab(newtab, TableLock(TableLock::PermanentLocking), nrrow);
    tab.tableInfo().setType ("testtype");
    tab.tableInfo().setSubType ("testsubtype");
    tab.tableInfo().readmeAddLine ("first readme line");
    tab.tableInfo().readmeAddLine ("second test readme line");

    ScalarColumn<uInt> ab1(tab,"ab");
    ScalarColumn<Int> ad(tab,"ad");
    for (uInt i=0; i<nrrow; i++) {
	ab1.put (i, i);
	ad.put (i, i/10);
    }
    Timer timer;
    Vector<uInt> abv = ab1.getColumn();
    timer.show();
    timer.mark();
    Vector<Int> adv = ad.getColumn();
    timer.show();
    timer.mark();
    for (i=0; i<nrrow; i++) {
	abv(i) = ab1(i);
    }
    timer.show();
    timer.mark();
    for (i=0; i<nrrow; i++) {
	adv(i) = ad(i);
    }
    timer.show();
    {
	// Get entire column (minus last cell) to test range performance.
	timer.mark();
	Vector<uInt> abv1 = ab1.getColumnRange (Slicer(IPosition(1,0),
						       IPosition(1,nrrow-1)));
	timer.show("range AIO");
	timer.mark();
	Vector<Int> adv1 = ad.getColumnRange (Slicer(IPosition(1,0),
						     IPosition(1,nrrow-1)));
	timer.show("range ISM");
	for (i=0; i<nrrow-1; i++) {
	    if (abv1(i) != i  ||  adv1(i) != Int(i/10)) {
		cout << i << ',';
	    }
	}
	cout << endl;
    }
    {
	Table rtab (tab(abv));
	ScalarColumn<uInt> ab1(rtab,"ab");
	ScalarColumn<Int> ad(rtab,"ad");
	Timer timer;
	{
	    Vector<uInt> abv = ab1.getColumn();
	    timer.show("cells AIO");
	    timer.mark();
	    Vector<Int> adv = ad.getColumn();
	    timer.show("cells ISM");
	    Bool del;
	    uInt st = 0;
	    const uInt* abvv = abv.getStorage(del);
	    timer.mark();
	    for (i=0; i<nrrow; i++) {
		uInt row = adv(i);
		if (row>st && row<nrrow) {
		    uInt off = row-st;
		    adv(i) = abvv[off];
		}
	    }	
	    timer.show("b");
	    for (i=0; i<nrrow; i++) {
		if (abv(i) != i  ||  adv(i) != Int(i/10)) {
		    cout << i << ',';
		}
	    }
	    cout << endl;
	}
	{
	    // Get entire column (minus last cell) to test range performance.
	    timer.mark();
	    Vector<uInt> abv1 = ab1.getColumnRange (Slicer(IPosition(1,0),
							 IPosition(1,nrrow-1)));
	    timer.show("cells/range AIO");
	    timer.mark();
	    Vector<Int> adv1 = ad.getColumnRange (Slicer(IPosition(1,0),
							 IPosition(1,nrrow-1)));
	    timer.show("cells/range ISM");
	    for (i=0; i<nrrow-1; i++) {
		if (abv1(i) != i  ||  adv1(i) != Int(i/10)) {
		    cout << i << ',';
		}
	    }
	    cout << endl;
	}
    }
}

main (int argc, char** argv)
{
    try {
	uInt nrrow = 100000;
	if (argc > 1) {
	    nrrow = atoi (argv[1]);
	}
	cout << nrrow << " rows" << endl;
	a (nrrow);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } end_try;
    return 0;                           // exit with success status
}
