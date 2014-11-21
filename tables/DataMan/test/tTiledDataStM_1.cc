//# tTiledDataStM_1.cc: Test program for performance of TiledDataStMan class
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

#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/DataMan/TiledDataStManAccessor.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/ArrayIter.h>
#include <casacore/casa/Arrays/ArrayPosIter.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


#include <casacore/casa/namespace.h>
// <summary>
// Test program for performance of TiledDataStMan class.
// </summary>

// This program tests the class TiledDataStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

void a (const char* argv[]);
void b();

int main (int argc, const char* argv[])
{
    // Get the command line arguments as cell shape, cube shape, tile shape.
    if (argc != 5) {
	cout << ">>>" << endl;
	cout << "tTiledDataStM_1 uses TiledDataStMan to store float arrays."
	     << endl;
	cout << "It writes the data and reads them back per cell and "
	        "per column-slice" << endl;
	cout << "and shows timing and cache statistics." << endl;
	cout << "Invoke as tTiledDataStM_1 arrayNdim cubeShape tileShape MaxCacheSize"
	     << endl;
	cout << "Eg. tTiledDataStM_1 2 4,128,96,1000 4,16,4,5 0" << endl;
	cout << "<<<" << endl;
	return 0;
    }
    try {
	a (argv);
	b ();
    } catch (AipsError& x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

// First build a description.
void a (const char* argum[])
{
    // Convert the command line arguments to shapes.
    uInt i, nrdim, maxCacheSize;
    istringstream istr0(argum[1]);
    istr0 >> nrdim;
    Vector<String> cubeV (stringToVector (argum[2]));
    Vector<String> tileV (stringToVector (argum[3]));
    istringstream istr1(argum[4]);
    istr1 >> maxCacheSize;
    if (cubeV.nelements() != tileV.nelements()) {
	throw AipsError("Cube and tile must have same dimensionality");
    }
    if (nrdim > cubeV.nelements()) {
	throw AipsError("Cell-dimensionality must be <= cube-dimensionality");
    }
    IPosition cellShape (nrdim);
    IPosition cubeShape (cubeV.nelements());
    IPosition tileShape (tileV.nelements());
    for (i=0; i<cubeV.nelements(); i++) {
	istringstream istr(cubeV(i).chars());
	istr >> cubeShape(i);
	if (cubeShape(i) <= 0) {
	    throw AipsError("Cubeshape " + String::toString(cubeShape(i))
			    + " must be > 0");
	}
    }
    for (i=0; i<tileV.nelements(); i++) {
	istringstream istr(tileV(i).chars());
	istr >> tileShape(i);
	if (tileShape(i) <= 0) {
	    throw AipsError("Tileshape " + String::toString(tileShape(i))
			    + " must be > 0");
	}
    }
    for (i=0; i<nrdim; i++) {
	cellShape(i) = cubeShape(i);
    }
    //# Determine # rows needed.
    uInt nrrow = 1;
    for (i=nrdim; i<cubeShape.nelements(); i++) {
	nrrow *= cubeShape(i);
    }

    // Test how long it takes to iterate through an array.
    // Do this only when the array is not too big.
    // First test with slices in natural direction.
    if (cubeShape.product() < 10*1024*1024) {
	Array<float> array (cubeShape);
	array = 0;
	ArrayIterator<float> iter (array, nrdim);
	Timer timer;
	while (! iter.pastEnd()) {
	    Array<float> result;
	    result = iter.array();
	    iter.next();
	}
	timer.show ("ArrayIter");
    }
    // Now test with slices in orthogonal direction.
    if (cubeShape.product() < 10*1024*1024) {
	uInt nr = cubeShape.product() / 1024;
	if (nr == 0) {
	    nr = 1;
	}
	Array<float> array (IPosition(2,1024,nr));
	array = 0;
	Timer timer;
	for (i=0; i<1024; i++) {
	    Array<float> result;
	    result = array(IPosition(2,i,0), IPosition(2,i,nr-1));
	}
	cout << "ArrayIter [1024, " << nr << "]" << endl;
	timer.show ("         ");
    }
	
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<float> ("Data", cellShape,
					  ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  cubeShape.nelements(),
			  stringToVector ("Data"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledDataStM_1_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledDataStMan sm1 ("TSMExample", maxCacheSize);
    newtab.bindAll (sm1);
    Table table(newtab, nrrow);
    TiledDataStManAccessor accessor(table, "TSMExample");
    Record values;
    accessor.addHypercube (cubeShape, tileShape, values);
    ArrayColumn<float> data (table, "Data");
    Array<float> array(cellShape);
    indgen (array);
    Timer timer;
    for (i=0; i<nrrow; i++) {
	data.put (i, array);
    }
    timer.show ("put      ");
    table.flush();
    timer.show ("put+flush");
    timer.mark();
    for (i=0; i<nrrow; i++) {
	data.get (i, array);
    }
    timer.show ("get      ");
    accessor.showCacheStatistics (cout);
}

void b()
{
    IPosition cellShape;
    uInt i, nrrow;
    Timer timer;
    {
	Table table("tTiledDataStM_1_tmp.data");
	timer.show ("reopen   ");
	ROTiledStManAccessor accessor(table, "TSMExample");
	nrrow = table.nrow();
	ArrayColumn<float> data (table, "Data");
	cellShape = data.shape (0);
	Array<float> result;
	timer.mark();
	for (i=0; i<nrrow; i++) {
	    data.get (i, result);
	}
	timer.show ("get cell ");
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    uInt nrdim = cellShape.nelements();
    IPosition length (nrdim, 1);
    length(0) = cellShape(0);
    IPosition origin (nrdim, 0);
    {
	Table table("tTiledDataStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	ArrayColumn<float> data (table, "Data");
	Array<float> result;
	ArrayPositionIterator iter (cellShape, origin, 1u);
	timer.mark();
	uInt nr = 0;
	while (! iter.pastEnd()) {
	    nr++;
	    data.getColumn (Slicer(iter.pos(), length), result);
	    iter.next();
	}
	cout << "columnSlice " << length << " (" << nr << " passes)" << endl;
	timer.show ("get      ");
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    {
	Table table("tTiledDataStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	ArrayColumn<float> data (table, "Data");
	Array<float> result;
	ArrayPositionIterator iter (cellShape, origin, 0u);
	length(0) = 1;
	uInt nr = 0;
	timer.mark();
	while (! iter.pastEnd()) {
	    nr++;
	    data.getColumn (Slicer(iter.pos(), length), result);
	    iter.next();
	}
	cout << "columnSlice " << length << " (" << nr << " passes)" << endl;
	timer.show ("get      ");
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
}
