//# tRefRows.cc: This program tests class RefRows
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

#include <aips/Tables/RefRows.h>

// <summary>
// Test program for class RefRows.
// </summary>


void doIt()
{
    Vector<uInt> vec(10);
    vec(0) = 1;
    vec(1) = 1;
    vec(2) = 2;
    vec(3) = 3;
    vec(4) = 4;
    vec(5) = 6;
    vec(6) = 7;
    vec(7) = 9;
    vec(8) = 11;
    vec(9) = 4;

    RefRows ref(vec);
    RefRowsSliceIter iter1(ref);
    while (!iter1.pastEnd()) {
	cout << iter1.sliceStart() << ' ' << iter1.sliceEnd()
	     << ' ' << iter1.sliceIncr() << endl;
	iter1++;
    }
    RefRowsRowIter iter2(ref);
    while (!iter2.pastEnd()) {
	cout << iter2.row() << ' ';
	iter2++;
    }
    cout << endl;
}

main() {
    try {
	doIt();
    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
        return 1;
    } end_try;
    return 0;               // successfully executed
}
