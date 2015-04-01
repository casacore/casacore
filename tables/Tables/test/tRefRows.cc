//# tRefRows.cc: This program tests class RefRows
//# Copyright (C) 1998,1999,2000,2002
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

#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test program for class RefRows.
// </summary>


void doIt()
{
    Vector<uInt> vec(18);
    vec(0) = 1;
    vec(1) = 1;
    vec(2) = 2;
    vec(3) = 3;
    vec(4) = 4;
    vec(5) = 6;
    vec(6) = 7;
    vec(7) = 9;
    vec(8) = 11;
    vec(9) = 5;
    vec(10)= 10;
    vec(11)= 15;
    vec(12)= 20;
    vec(13)= 25;
    vec(14)= 30;
    vec(15)= 35;
    vec(16)= 40;
    vec(17)= 4;
    {
	RefRows ref(vec);
	AlwaysAssertExit (ref.nrows() == 18);
	AlwaysAssertExit (!ref.isSliced());
	RefRowsSliceIter iter1(ref);
	cout << "unSliced,unCollapse" << endl;
	while (!iter1.pastEnd()) {
	    cout << iter1.sliceStart() << ' ' << iter1.sliceEnd()
		 << ' ' << iter1.sliceIncr() << endl;
	    iter1++;
	}
    }
    {
	RefRows ref(vec, True);
	AlwaysAssertExit (ref.nrows() == 7);
	AlwaysAssertExit (ref.isSliced());
	RefRowsSliceIter iter1(ref);
	cout << "sliced" << endl;
	while (!iter1.pastEnd()) {
	    cout << iter1.sliceStart() << ' ' << iter1.sliceEnd()
		 << ' ' << iter1.sliceIncr() << endl;
	    iter1++;
	}
    }
    {
	RefRows ref(vec, False, True);
	AlwaysAssertExit (ref.nrows() == 18);
	AlwaysAssertExit (ref.isSliced());
	RefRowsSliceIter iter1(ref);
	cout << "unSliced,collapse" << endl;
	while (!iter1.pastEnd()) {
	    cout << iter1.sliceStart() << ' ' << iter1.sliceEnd()
		 << ' ' << iter1.sliceIncr() << endl;
	    iter1++;
	}
    }
    {
	RefRows ref(3,9,2);
	AlwaysAssertExit (ref.nrows() == 4);
	AlwaysAssertExit (ref.isSliced());
	RefRowsSliceIter iter1(ref);
	cout << "one slice" << endl;
	while (!iter1.pastEnd()) {
	    cout << iter1.sliceStart() << ' ' << iter1.sliceEnd()
		 << ' ' << iter1.sliceIncr() << endl;
	    iter1++;
	}
    }
    {
	Vector<uInt> rows(18);
	indgen (rows, uInt(1));
	rows(17) = 0;
	RefRows ref(rows);
	AlwaysAssertExit (ref.nrows() == 18);
	AlwaysAssertExit (!ref.isSliced());
	cout << ref.convert(vec) << endl;
    }
    {
	Vector<uInt> rows(18);
	indgen (rows, uInt(1));
	rows(17) = 0;
	RefRows ref(rows, False, True);
	AlwaysAssertExit (ref.nrows() == 18);
	AlwaysAssertExit (ref.isSliced());
	RefRowsSliceIter iter1(ref);
	while (!iter1.pastEnd()) {
	    cout << iter1.sliceStart() << ' ' << iter1.sliceEnd()
		 << ' ' << iter1.sliceIncr() << endl;
	    iter1++;
	}
	cout << ref.convert(vec) << endl;;
    }
}

int main()
{
    try {
	doIt();
    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } 
    return 0;               // successfully executed
}
