//# tTiledShape.cc: Test program of TiledShape class
//# Copyright (C) 1997,2000,2001,2003
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

#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for TiledShape class.
// </summary>

// This program tests the class TiledShape.
// The user has the option to enter an interactive session


void testClass()
{
    {
	TiledShape t (IPosition(3,32,16,5));
	AlwaysAssertExit (t.shape() == IPosition(3,32,16,5));
	AlwaysAssertExit (t.tileShape() == IPosition(3,32,16,5));
	AlwaysAssertExit (!t.isTileShapeDefined());
    }
    {
	TiledShape t (IPosition(3,32,16,5), IPosition(3,32,6,5));
	AlwaysAssertExit (t.shape() == IPosition(3,32,16,5));
	AlwaysAssertExit (t.tileShape() == IPosition(3,32,6,5));
	AlwaysAssertExit (t.isTileShapeDefined());
    }
    {
	TiledShape t (IPosition(3,32,16,5), IPosition(3,32,6,5));
	TiledShape t1(t);
	AlwaysAssertExit (t1.shape() == IPosition(3,32,16,5));
	AlwaysAssertExit (t1.tileShape() == IPosition(3,32,6,5));
	AlwaysAssertExit (t1.isTileShapeDefined());
	TiledShape t2 (IPosition(2,15,2));
	AlwaysAssertExit (t2.shape() == IPosition(2,15,2));
	AlwaysAssertExit (t2.tileShape() == IPosition(2,15,2));
	AlwaysAssertExit (!t2.isTileShapeDefined());
	t1 = t2;
	AlwaysAssertExit (t1.shape() == IPosition(2,15,2));
	AlwaysAssertExit (t1.tileShape() == IPosition(2,15,2));
	AlwaysAssertExit (!t1.isTileShapeDefined());
    }
}

IPosition getVec (uInt nrdim, const String& prompt)
{
    while (True) {
	cout << prompt;
	String str;
	cin >> str;
	if (str == "end") {
	    return IPosition();
	}
	Vector<String> vec = stringToVector (str);
	if (vec.nelements() > nrdim) {
	    cout << "value can contain max. " << nrdim << " values" << endl;
	}else{
	    Bool error = False;
	    IPosition pos(vec.nelements());
	    for (uInt i=0; i<vec.nelements(); i++) {
		istringstream istr(vec(i).chars());
		istr >> pos(i);
		if (pos(i) < 0) {
		    cout << "Value " << pos(i) << " must be >= 0" << endl;
		    error = True;
		    break;
		}
	    }
	    if (!error) {
		return pos;
	    }
	}
    }
}

void testTiling (uInt tileSize)
{
    // Convert the command line argument to shape.
    while (True) {
	IPosition shape = getVec (10, "array shape (end means stop): ");
	if (shape.nelements() == 0) {
	    break;
	}
	
	cout << "tileshape = "
	     << TiledShape(shape).defaultTileShape (tileSize, 0.5) << endl;
    }
}
	
int main (int argc, const char* argv[])
{
    try {
	testClass();
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    cout << "OK" << endl;
    if (argc < 2) {
	cout << ">>>" << endl;
	cout << "The function defaultTileShape can be tested by" << endl;
	cout << "invoking as tTiledShape tileSize (in pixels)" << endl;
	cout << "  Eg. tTiledShape 32768" << endl;
	cout << "<<<" << endl;
	return 0;
    }
    try {
	// Get the command line argument as tile size.
	uInt tileSize;
	istringstream istr1(argv[1]);
	istr1 >> tileSize;
	testTiling (tileSize);
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}
