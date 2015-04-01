//# tTableVector.cc: This program tests the table vectors
//# Copyright (C) 1994,1995,1996,1999,2000,2001,2003
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

#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableVector.h>
#include <casacore/tables/Tables/TabVecMath.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/OS/Timer.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
// This program tests the table vectors..
// It creates a description and a table.
// It creates various vectors, operates on them and checks the results.
// It writes some timing data to stdout.

void credes();
void cretab(uInt);
void dovec (Int);

int main (int argc, const char* argv[])
{
    uInt nr = 5000;
    if (argc > 1) {
	istringstream istr(argv[1]);
	istr >> nr;
    }
    credes();          // make description
    cretab(nr);        // create table (and write it)
    dovec (nr);        // do table vector operations
    return 0;          // successfully executed
}

// Create the description of the table and its subtable.
// Define some defaults (as scalar and array) to test if they
// are filled in correctly when a row is added to a table.
void credes () {
    TableDesc txp ("tTableVector_tmp", TableDesc::New);
    txp.addColumn (ScalarColumnDesc<Int> ("col1"));
    txp.addColumn (ScalarColumnDesc<double> ("col2"));
    txp.addColumn (ScalarColumnDesc<float> ("col3"));
    txp.addColumn (ScalarColumnDesc<Complex> ("col4"));
}

// Write data into the table.
void cretab (uInt nr)
{
    uInt i;
    SetupNewTable newtab ("tTableVector_tmp.data", "tTableVector_tmp",
                          Table::New);
    Table tab (newtab);
    ScalarColumn<Int>     col1 (tab, "col1");
    ScalarColumn<double>  col2 (tab, "col2");
    ScalarColumn<float>   col3 (tab, "col3");
    ScalarColumn<Complex> col4 (tab, "col4");
    Timer tim;
    for (i=0; i<nr; i++) {
	tab.addRow();
	col1.put (i, 100000-i);
	col2.put (i, i+1);
	col3.put (i, i+2);
	col4.put (i, i+3);
    }
    cout << ">>>" << endl;
    tim.show();
    cout << "<<<" << endl;
    cout << "Filling done" << endl;
}


void dovec (Int nr) {
    Int i;
    Vector<Int> vec(nr);
    Timer tim;
    for (i=0; i<nr; i++) {
	vec(i) = i;
    }
    cout << ">>>" << endl;
    tim.show ("Initializing Vector     ");

    TableVector<Int> tvec(nr);
    tim.mark();
    for (i=0; i<nr; i++) {
	tvec.set (i, i);
    }
    tim.show ("Initializing TableVector");

    tim.mark();
    TableVector<Int> tvec2 = tvec + 1;
    tim.show ("Adding constant to TVec ");
    cout << "<<<" << endl;
    cout << tvec2.nelements() << " elements" << endl;
    for (i=0; i<nr; i++) {
	Int j = tvec2(i);
	if (j != i+1) {
	    cout << "error in addition of element " << i << endl;
	    cout << tvec(i) << " " << tvec2(i) << endl;
	}
    }

    tim.mark();
    TableVector<Int> tvec3 = tvec + tvec2;
    cout << ">>>" << endl;
    tim.show ("Adding TVec to TVec     ");
    tim.mark();
    tvec3 = tvec3 + tvec2;
    tvec3 += tvec;
    tim.show ("Adding TVec+TVec+TVec   ");
    cout << "<<<" << endl;
    for (i=0; i<nr; i++) {
	if (tvec3(i) != 4*i+2) {
	    cout << "error in addition of element " << i << endl;
	}
    }

    Table tab ("tTableVector_tmp.data",Table::Update);
    TableVector<Int> tvec4(tab,"col1");
    TableVector<float> tvecFloat(tab,"col3");
//#    tvecFloat = 3;                            // Should fail when compiling
    cout << "got tvec4" << endl;
    cout << tvec4(1) << " (must be 99999)" << endl;

    tim.mark();
    tvec3 = tvec3 + tvec4;
    cout << ">>>" << endl;
    tim.show ("Adding ColVec to TVec   ");
    cout << "<<<" << endl;
    for (i=0; i<nr; i++) {
	if (tvec3(i) != 4*i+2+100000-i) {
	    cout << "error in addition of element " << i << endl;
	}
    }

    tim.mark();
    tvec4 = tvec + tvec4;
    cout << ">>>" << endl;
    tim.show ("Adding TVec to ColVec   ");
    tvec4 = tvec4 + tvec2;
    Vector<Int> vec2;
    ScalarColumn<Int> col1 (tab, "col1");
    tim.mark();
    col1.getColumn (vec2);
    tim.show ("Getting a table column  ");
    cout << "<<<" << endl;

    tim.mark();
    for (i=0; i<nr; i++) {
	if (tvec4(i) != 100000-i+i+i+1) {
	    cout << "error in addition of element " << i << endl;
	}
    }
    cout << ">>>" << endl;
    tim.show ("Loop through TableVector");
    cout << "<<<" << endl;

    tim.mark();
    for (i=0; i<nr; i++) {
	if (vec2(i) != 100000-i+i+i+1) {
	    cout << "error in vector element " << i << endl;
	}
    }
    cout << ">>>" << endl;
    tim.show ("Loop through Vector     ");
    cout << "<<<" << endl;

    // Get a vector from the table column vector.
    Vector<Int> vec3;
    vec3 = tvec4.makeVector();
    for (i=0; i<nr; i++) {
	if (vec3(i) != 100000-i+i+i+1) {
	    cout << "error in tvec3.makeVector element " << i << endl;
	}
    }

    // Do the same for tvec.
    // Assign value to be sure that copy semantics are used.
    vec3 = tvec.makeVector();
    for (i=0; i<nr; i++) {
	if (vec3(i) != i) {
	    cout << "error in tvec.makeVector element " << i << endl;
	}
    }
    tvec *= 2;
    for (i=0; i<nr; i++) {
	if (tvec(i) != 2*i) {
	    cout << "error in tvec*=2 element " << i << endl;
	}
    }
    for (i=0; i<nr; i++) {
	if (vec3(i) != i) {
	    cout << "assign to tvec affected vec3 " << i << endl;
	}
    }
    vec3 += 1;
    for (i=0; i<nr; i++) {
	if (tvec(i) != 2*i) {
	    cout << "assign to vec3 affected tvec " << i << endl;
	}
    }
}
