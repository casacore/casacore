//# tTiledCellStM_1.cc: Test program for performance of TiledCellStMan class
//# Copyright (C) 1996,1997,1999,2000,2001,2002,2003
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
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/tables/DataMan/TiledStManAccessor.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

#ifdef PABLO_IO
#include "IOTrace.h"
#include "PabloTrace.h"
extern "C" Int setTraceFileName(char *);
extern "C" Int endTracing(void);
#endif // PABLO_IO

#include <casacore/casa/namespace.h>

// <summary>
// Test program for performance of TiledCellStMan class.
// </summary>

// This program tests the class TiledCellStMan and related classes.
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

#ifdef PABLO_IO
void openPablo (const char* argv[]);
void closePablo ();
#endif // PABLO_IO
void makeCube (const char* argv[]);
void getCube (Bool trav, Bool ask);
void traverse (const IPosition& cubeShape, const IPosition& tileShape);
IPosition getVec (uInt nrdim, const String& prompt);

int main (int argc, const char* argv[])
{
    // Get the command line arguments as cube shape, tile shape.
    if (argc < 4) {
	cout << ">>>" << endl;
	cout << "tTiledCellStM_1 uses TiledCellStMan to store nD Float "
	        "arrays in one cell." << endl;
	cout << "It writes the data, reads the cell back, and iterates "
	        "along tiles." << endl;
	cout << "For 3D arrays it also iterates along lines and planes"
	     << endl;
	cout << "when the 4th argument is given and is not equal 0." << endl;
	cout << "It shows timing and cache statistics." << endl;
	cout << "Invoke as tTiledCellStM_1 arrayShape tileShape MaxCacheSize"
	     << endl;
	cout << "  Eg. tTiledCellStM_1 256,256,100 20,20,20 0" << endl;
	cout << "TiledStMan::makeTileShape is used when tileShape is given "
	        "as 0" << endl;
	cout << "If a 5th argument is given, the user will be asked for"
	     << endl;
	cout << "slice shapes, axis path, and window start and length "
	        "until 'end' is given" << endl;
	cout << "This tests the function setCacheSize" << endl;
	cout << "<<<" << endl;
	return 0;
    }
    try {
#ifdef PABLO_IO
        openPablo(argv);
#endif
	makeCube(argv);
        getCube ((argc>4 && String(argv[4])!="0"), (argc>5));
#ifdef PABLO_IO
        closePablo();
#endif
    } catch (AipsError x) {
	cout << "Caught an exception: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;                           // exit with success status
}

#ifdef PABLO_IO
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/namespace.h>
void openPablo (char** argv)
{
  // We set the name of the trace file here.  Typically, you want the
  // file to be written to a local disk, and if possible to a file to.
  // Also, make sure the location you select has a lot of free space.
  // For multiprocessor applications, set 'tracenode' to the
  // appropriate node for each processor (for example, using the MPI
  // rank) - here we just set it to 0.
  //
  // OK there are two environment variables that drive us.  First
  // PABLOSTATS is a colon(:) seperated list (need those leading and
  // trailing :'s) which tell us what processes to trace.  i.e.
  // PABLOSTATS = ':imager:quanta:calibrater:' or PABLOSTATS = 'all'
  // to get them all Second PABLOSTATSDIR is the directory where the
  // stats files should go the default is /var/tmp be careful...
  //
  ostringstream oss;
  Path myname(argv[0]);
  int tracenode(0);
  if (EnvironmentVariable::isDefined("PABLOSTATS")) {
     String pablostats(EnvironmentVariable::get("PABLOSTATS"));
     Regex lookfor(String("\:")+myname.baseName()+String("\:"));
     if(pablostats.contains(lookfor) || pablostats == String("all")){
        cout << "Writing output Pablo file" << endl;
        String pablostatsdir("/var/tmp");
        if(gevs.isSet(String("PABLOSTATSDIR"))){
           pablostatsdir = gevs.value("PABLOSTATSDIR");
        }
        oss << pablostatsdir << "/" << myname.baseName() << "_node"
	    << tracenode << ".PabloIO";
        setTraceFileName( oss.str() );

        initIOTrace();
        traceEvent(1,"Starting instrumentation",24);
     }
  }
}

void closePablo ()
{
    traceEvent(1,"Ending instrumentation",24);
    endIOTrace();
    endTracing();
}
#endif

// First build a description.
void makeCube (const char* argv[])
{
    // Convert the command line arguments to shapes.
    uInt i, maxCacheSize;
    Vector<String> cubeV (stringToVector (argv[1]));
    Vector<String> tileV (stringToVector (argv[2]));
    istringstream istr1(argv[3]);
    istr1 >> maxCacheSize;
    uInt nrdim = cubeV.nelements();
    IPosition cubeShape (nrdim);
    IPosition tileShape (nrdim);
    for (i=0; i<nrdim; i++) {
	istringstream istr(cubeV(i).chars());
	istr >> cubeShape(i);
	if (cubeShape(i) <= 0) {
	    throw AipsError("Arrayshape "  + String::toString(cubeShape(i))
			    + " must be > 0");
	}
    }
    if (tileV.nelements() != nrdim) {
	if (tileV.nelements() != 1  ||  tileV(0) != "0") {
	    throw AipsError("Array and tile must have same dimensionality");
	}
	tileShape = TiledStMan::makeTileShape (cubeShape);
    }else{
	for (i=0; i<nrdim; i++) {
	    istringstream istr(tileV(i).chars());
	    istr >> tileShape(i);
	    if (tileShape(i) <= 0) {
		throw AipsError("Tileshape " + String::toString(tileShape(i))
				+ " must be > 0");
	    }
	}
    }
    Vector<double> weight(nrdim);
    Vector<double> tolerance(nrdim);
    for (i=0; i<nrdim; i++) {
	weight(i) = i;
	tolerance(i) = 0.5;
    }
    cout << TiledStMan::makeTileShape (cubeShape) << endl;
    cout << TiledStMan::makeTileShape (cubeShape, weight, tolerance) << endl;
	
    // Build the table description.
    TableDesc td ("", "1", TableDesc::Scratch);
    td.addColumn (ArrayColumnDesc<Float> ("Data", cubeShape,
					  ColumnDesc::FixedShape));
    td.defineHypercolumn ("TSMExample",
			  nrdim,
			  stringToVector ("Data"));
    
    // Now create a new table from the description.
    SetupNewTable newtab("tTiledCellStM_1_tmp.data", td, Table::New);
    // Create a storage manager for it.
    TiledCellStMan sm1 ("TSMExample", tileShape, maxCacheSize);
    newtab.bindAll (sm1);
    Table table(newtab, 1);
    ArrayColumn<Float> data (table, "Data");
    Array<Float> array(cubeShape);
    Timer timer;
    indgen (array);
    timer.show ("indgen   ");
    timer.mark();
    data.put (0, array);
    timer.show ("put      ");
    table.flush();
    timer.show ("put+flush");
    timer.mark();
    table.copy ("tTiledCellStM_1_tmp.data2", Table::New);
    timer.show ("copy     ");
    timer.mark();
    data.get (0, array);
    timer.show ("get      ");
    ROTiledStManAccessor accessor(table, "TSMExample");
    accessor.showCacheStatistics (cout);
}

void getCube (Bool trav, Bool ask)
{
    IPosition cubeShape;
    IPosition tileShape;
    double sizeMb = sizeof(Float);
    double realtime;
    uInt i, nrdim;
    Timer timer;
    {
	Table table("tTiledCellStM_1_tmp.data2");
	timer.show ("reopen   ");
	ROTiledStManAccessor accessor(table, "TSMExample");
	ArrayColumn<Float> data (table, "Data");
	cubeShape = data.shape (0);
	sizeMb *= cubeShape.product();
	sizeMb /= 1024*1024;
	tileShape = accessor.tileShape (0);
	nrdim = cubeShape.nelements();
	Array<Float> result;
	timer.mark();
	data.get (0, result);
	realtime = timer.real();
	timer.show ("get cell ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (nrdim == 3) {
	Table table("tTiledCellStM_1_tmp.data2");
	ROTiledStManAccessor accessor(table, "TSMExample");
	ArrayColumn<Float> data (table, "Data");
	cubeShape = data.shape (0);
	tileShape = accessor.tileShape (0);
	nrdim = cubeShape.nelements();
	Array<Float> result;
	timer.mark();
	IPosition blc(nrdim, 0);
	IPosition len = cubeShape;
	len[1] = 1;
	sizeMb *= len.product();
	sizeMb /= 1024*1024;
	data.getSlice (0, Slicer(blc,len), result);
	realtime = timer.real();
	timer.show ("get slice");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    {
	Table table("tTiledCellStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, tileShape, IPosition());
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	IPosition last(nrdim);
	IPosition nrt(nrdim);
	for (i=0; i<nrdim; i++) {
	    last(i) = cubeShape(i) % tileShape(i);
	    if (last(i) == 0) {
		last(i) = tileShape(i);
	    }
	    nrt(i) = (cubeShape(i)-1) / tileShape(i);
	}
	IPosition start(nrdim);
	start = 0;
	IPosition nrsteps(start);
        IPosition stepnr(start);
	IPosition length(tileShape);
	while (True) {
	    Array<Float> arr = data.getSlice (0, Slicer (start, length));
	    nr++;
	    for (i=0; i<nrdim; i++) {
		start(i) += tileShape(i);
		nrsteps(i) += 1;
		stepnr(i) += 1;
		if (stepnr(i) <= nrt(i)) {
		    if (stepnr(i) == nrt(i)) {
			length(i) = last(i);
		    }
		    break;
		}
		length(i) = tileShape(i);
		start(i) = 0;
		stepnr(i) = 0;
	    }
	    if (i == nrdim) {
		break;
	    }
	}
	cout << "array x,y,z along tiles" << " (" << nr << " passes  "
	     << nrsteps << ")" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }

    // Traverse through 3D cubes.
    if (trav  &&  nrdim == 3) {
	traverse (cubeShape, tileShape);
    }

    // Ask for iteration shapes when needed.
    if (!ask) {
	return;
    }
    cout << "Give slice shapes, etc.. End by giving end" << endl;
    Table table("tTiledCellStM_1_tmp.data");
    ROTiledStManAccessor accessor(table, "TSMExample");
    while (True) {
	IPosition slice = getVec (nrdim, "slice shape (end means stop): ");
	if (slice.nelements() == 0) {
	    break;
	}
	accessor.setCacheSize (0, slice,
			       getVec (nrdim, "window start: "),
			       getVec (nrdim, "window shape: "),
			       getVec (nrdim, "axis path:    "));
	cout << "  result is:" << endl;
	accessor.showCacheStatistics (cout);
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
    return IPosition();
}


void traverse (const IPosition& cubeShape, const IPosition& tileShape)
{
    double sizeMb = sizeof(Float) * cubeShape.product();
    sizeMb /= 1024*1024;
    double realtime;
    Timer timer;
    if (cubeShape(2) > 1) {
	IPosition length (3, 1, 1, cubeShape(2));
	Table table("tTiledCellStM_1_tmp.data2");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition(2,2,1));
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int i=0; i<cubeShape(0); i++) {
	    for (Int j=0; j<cubeShape(1); j++) {
		Array<Float> arr = data.getSlice
		                (0, Slicer (IPosition(3,i,j,0), length));
		nr++;
	    }
	}
	cout << "arraySlice z along y,x" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(2) > 1) {
	IPosition length (3, 1, 1, cubeShape(2));
	Table table("tTiledCellStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition(1,2));
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int j=0; j<cubeShape(1); j++) {
	    for (Int i=0; i<cubeShape(0); i++) {
		Array<Float> arr = data.getSlice
		                (0, Slicer (IPosition(3,i,j,0), length));
		nr++;
	    }
	}
	cout << "arraySlice z along x,y" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(1) > 1) {
	IPosition length (3, 1, cubeShape(1), 1);
	Table table("tTiledCellStM_1_tmp.data2");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition(3,1,2,0));
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int i=0; i<cubeShape(0); i++) {
	    for (Int j=0; j<cubeShape(2); j++) {
		Array<Float> arr = data.getSlice
		                (0, Slicer (IPosition(3,i,0,j), length));
		nr++;
	    }
	}
	cout << "arraySlice y along z,x" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(1) > 1) {
	IPosition length (3, 1, cubeShape(1), 1);
	Table table("tTiledCellStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition(1,1));
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int j=0; j<cubeShape(2); j++) {
	    for (Int i=0; i<cubeShape(0); i++) {
		Array<Float> arr = data.getSlice
		                (0, Slicer (IPosition(3,i,0,j), length));
		nr++;
	    }
	}
	cout << "arraySlice y along x,z" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(0) > 1) {
	IPosition length (3, cubeShape(0), 1, 1);
	Table table("tTiledCellStM_1_tmp.data2");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition(2,0,2));
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int i=0; i<cubeShape(1); i++) {
	    for (Int j=0; j<cubeShape(2); j++) {
		Array<Float> arr = data.getSlice
		                (0, Slicer (IPosition(3,0,i,j), length));
		nr++;
	    }
	}
	cout << "arraySlice x along z,y" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(0) > 1) {
	IPosition length (3, cubeShape(0), 1, 1);
	Table table("tTiledCellStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition());
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int j=0; j<cubeShape(2); j++) {
	    for (Int i=0; i<cubeShape(1); i++) {
		Array<Float> arr = data.getSlice
		                (0, Slicer (IPosition(3,0,i,j), length));
		nr++;
	    }
	}
	cout << "arraySlice x along y,z" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(0) > 1  &&  cubeShape(1) > 1  &&  cubeShape(2) > 1) {
	IPosition length (3, cubeShape(0), cubeShape(1), 1);
	Table table("tTiledCellStM_1_tmp.data2");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition());
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int j=0; j<cubeShape(2); j++) {
	    Array<Float> arr = data.getSlice
		                 (0, Slicer (IPosition(3,0,0,j), length));
	    nr++;
	}
	cout << "arrayPlane x,y along z" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(0) > 1  &&  cubeShape(1) > 1  &&  cubeShape(2) > 1) {
	IPosition length (3, cubeShape(0), 1, cubeShape(2));
	Table table("tTiledCellStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition(3,0,2,1));
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int j=0; j<cubeShape(1); j++) {
	    Array<Float> arr = data.getSlice
		                 (0, Slicer (IPosition(3,0,j,0), length));
	    nr++;
	}
	cout << "arrayPlane x,z along y" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(0) > 1  &&  cubeShape(1) > 1  &&  cubeShape(2) > 1) {
	IPosition length (3, 1, cubeShape(1), cubeShape(2));
	Table table("tTiledCellStM_1_tmp.data2");
	ROTiledStManAccessor accessor(table, "TSMExample");
	accessor.setCacheSize (0, length, IPosition(3,1,2,0));
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	for (Int j=0; j<cubeShape(0); j++) {
	    Array<Float> arr = data.getSlice
		                 (0, Slicer (IPosition(3,j,0,0), length));
	    nr++;
	}
	cout << "arrayPlane y,z along x" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
    if (cubeShape(2) > 1) {
	Table table("tTiledCellStM_1_tmp.data");
	ROTiledStManAccessor accessor(table, "TSMExample");
	IPosition length (3, cubeShape(0), cubeShape(1), tileShape(2));
	accessor.setCacheSize (0, length, IPosition());
	ArrayColumn<Float> data (table, "Data");
	Array<Float> result;
	uInt nr = 0;
	timer.mark();
	Int last = cubeShape(2) % tileShape(2);
	if (last == 0) last = tileShape(2);
	Int nrk = (cubeShape(2)-1)/tileShape(2);
	for (Int k=0; k<=nrk; k++) {
	    if (k==nrk) {
		length(2) = last;
	    }
	    Array<Float> arr = data.getSlice
		(0, Slicer (IPosition(3, 0, 0, k*tileShape(2)), length));
	    nr++;
	}
	cout << "array x,y,z along z-tiles" << " (" << nr << " passes)" << endl;
	realtime = timer.real();
	timer.show ("get      ");
	cout << "Throughput " << sizeMb/realtime << " Mb/sec" << endl;
	accessor.showCacheStatistics (cout);
	accessor.clearCaches();
    }
}
