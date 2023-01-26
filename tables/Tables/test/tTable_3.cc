//# tTable_3.cc: Program to test some performance aspects of the table system
//# Copyright (C) 1998,1999,2000,2001
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


#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdio.h>


#include <casacore/casa/namespace.h>
// <summary>
// Program to test some performance aspects of the table system.
// </summary>


// First build a description.
void a (uint32_t nrrow)
{
    // Build the table description.
    TableDesc td("", "1", TableDesc::Scratch);
    td.comment() = "A test of class Table";
    td.addColumn (ScalarColumnDesc<uint32_t>("ab","Comment for column ab"));
    td.addColumn (ScalarColumnDesc<int32_t>("ad","comment for ad"));

    // Now create a new table from the description.
    // Use copy constructor to test if it works fine.
    // (newtab and newtabcp have the same underlying object).
    SetupNewTable newtab("tTable_3_tmp.data", td, Table::New);
    IncrementalStMan stman2;
    newtab.bindColumn ("ad", stman2);
    Table tab(newtab, TableLock(TableLock::PermanentLocking), nrrow);
    tab.tableInfo().setType ("testtype");
    tab.tableInfo().setSubType ("testsubtype");
    tab.tableInfo().readmeAddLine ("first readme line");
    tab.tableInfo().readmeAddLine ("second test readme line");

    ScalarColumn<uint32_t> ab1(tab,"ab");
    ScalarColumn<int32_t> ad(tab,"ad");
    uint32_t i;
    for (i=0; i<nrrow; i++) {
	ab1.put (i, i);
	ad.put (i, i/10);
    }
    Timer timer;
    Vector<uint32_t> abv = ab1.getColumn();
    timer.show();
    timer.mark();
    Vector<int32_t> adv = ad.getColumn();
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
	Vector<uint32_t> abv1 = ab1.getColumnRange (Slicer(IPosition(1,0),
						       IPosition(1,nrrow-1)));
	timer.show("range AIO");
	timer.mark();
	Vector<int32_t> adv1 = ad.getColumnRange (Slicer(IPosition(1,0),
						     IPosition(1,nrrow-1)));
	timer.show("range ISM");
	for (i=0; i<nrrow-1; i++) {
	    AlwaysAssertExit (abv1(i) == i);
	    AlwaysAssertExit (adv1(i) == int32_t(i/10));
	}
    }
    {
        Vector<rownr_t> abvcp(abv.size());
        convertArray (abvcp, abv);
	Table rtab (tab(abvcp));
	ScalarColumn<uint32_t> ab1(rtab,"ab");
	ScalarColumn<int32_t> ad(rtab,"ad");
	Timer timer;
	{
	    Vector<uint32_t> abv = ab1.getColumn();
	    timer.show("cells AIO");
	    timer.mark();
	    Vector<int32_t> adv = ad.getColumn();
	    timer.show("cells ISM");
	    bool del;
	    uint32_t st = 0;
	    const uint32_t* abvv = abv.getStorage(del);
	    timer.mark();
	    for (i=0; i<nrrow; i++) {
		uint32_t row = adv(i);
		if (row>st && row<nrrow) {
		    uint32_t off = row-st;
		    adv(i) = abvv[off];
		}
	    }	
	    timer.show("b");
	    for (i=0; i<nrrow; i++) {
                AlwaysAssertExit (abv(i) == i);
                AlwaysAssertExit (adv(i) == int32_t(i/10));
	    }
	}
	{
	    // Get entire column (minus last cell) to test range performance.
	    timer.mark();
	    Vector<uint32_t> abv1 = ab1.getColumnRange (Slicer(IPosition(1,0),
							 IPosition(1,nrrow-1)));
	    timer.show("cells/range AIO");
	    timer.mark();
	    Vector<int32_t> adv1 = ad.getColumnRange (Slicer(IPosition(1,0),
							 IPosition(1,nrrow-1)));
	    timer.show("cells/range ISM");
	    for (i=0; i<nrrow-1; i++) {
                AlwaysAssertExit (abv1(i) == i);
                AlwaysAssertExit (adv1(i) == int32_t(i/10));
	    }
	}
    }
}

int main (int argc, const char* argv[])
{
    try {
	uint32_t nrrow = 100000;
	if (argc > 1) {
	    nrrow = atoi (argv[1]);
	}
	cout << nrrow << " rows" << endl;
	a (nrrow);
    } catch (std::exception& x) {
	cout << "Caught an exception: " << x.what() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
