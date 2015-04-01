//# TiledDataStMan.cc: Storage manager for tables using tiled hypercubes
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/tables/DataMan/DataManError.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

TiledDataStMan::TiledDataStMan ()
: TiledStMan     (),
  nrUsedRowMap_p (0),
  nrrowLast_p    (0)
{}

TiledDataStMan::TiledDataStMan (const String& hypercolumnName,
				uInt maximumCacheSize)
: TiledStMan     (hypercolumnName, maximumCacheSize),
  nrUsedRowMap_p (0),
  nrrowLast_p    (0)
{}

TiledDataStMan::TiledDataStMan (const String& hypercolumnName,
				const Record& spec)
: TiledStMan     (hypercolumnName, 0),
  nrUsedRowMap_p (0),
  nrrowLast_p    (0)
{
    if (spec.isDefined ("MAXIMUMCACHESIZE")) {
        setPersMaxCacheSize (spec.asInt ("MAXIMUMCACHESIZE"));
    }
}

TiledDataStMan::~TiledDataStMan()
{}

DataManager* TiledDataStMan::clone() const
{
    TiledDataStMan* smp = new TiledDataStMan (hypercolumnName_p,
					      maximumCacheSize());
    return smp;
}

DataManager* TiledDataStMan::makeObject (const String& group,
					 const Record& spec)
{
    TiledDataStMan* smp = new TiledDataStMan (group, spec);
    return smp;
}

String TiledDataStMan::dataManagerType() const
    { return "TiledDataStMan"; }


void TiledDataStMan::create (uInt nrrow)
{
    // Set up the various things.
    setup(-1);
    // Add the rows for the given number of rows.
    addRow (nrrow);
}
	    

Bool TiledDataStMan::flush (AipsIO&, Bool fsync)
{
    // Flush the caches.
    // Exit if nothing has changed.
    if (! flushCaches (fsync)) {
	return False;
    }
    // Create the header file and write data in it.
    AipsIO* headerFile = headerFileCreate();
    headerFile->putstart ("TiledDataStMan", 1);
    // Let the base class write its data.
    headerFilePut (*headerFile, cubeSet_p.nelements());
    // Write the data from this object.
    *headerFile << nrrowLast_p;
    *headerFile << nrUsedRowMap_p;
    putBlock (*headerFile, rowMap_p,  Int(nrUsedRowMap_p));
    putBlock (*headerFile, cubeMap_p, Int(nrUsedRowMap_p));
    putBlock (*headerFile, posMap_p,  Int(nrUsedRowMap_p));
    headerFile->putend();
    headerFileClose (headerFile);
    return True;
}

void TiledDataStMan::readHeader (uInt tabNrrow, Bool firstTime)
{
    // Open the header file and read data from it.
    AipsIO* headerFile = headerFileOpen();
    headerFile->getstart ("TiledDataStMan");
    // Let the base class read and initialize its data.
    headerFileGet (*headerFile, tabNrrow, firstTime, -1);
    // Read the data for this object.
    *headerFile >> nrrowLast_p;
    *headerFile >> nrUsedRowMap_p;
    getBlock (*headerFile, rowMap_p);
    getBlock (*headerFile, cubeMap_p);
    getBlock (*headerFile, posMap_p);
    headerFile->getend();
    headerFileClose (headerFile);
}


void TiledDataStMan::addRow (uInt nrow)
{
    nrrow_p += nrow;
    setDataChanged();
}


void TiledDataStMan::checkNrrow (const IPosition& cubeShape,
				 uInt incrInLastDim) const
{
    uInt nrrow = addedNrrow (cubeShape, incrInLastDim);
    if (nrrowLast_p + nrrow > nrrow_p) {
	throw (TSMError
	             ("Insufficient #rows in table for add/extendHypercube"));
    }
}


void TiledDataStMan::addHypercube (const IPosition& cubeShape,
				   const IPosition& tileShape,
				   const Record& values)
{
    // Check if the number of rows involved fits in the table.
    checkNrrow (cubeShape, cubeShape(nrdim_p - 1));
    // Check the hypercube definition and create the hypercube.
    checkAddHypercube (cubeShape, values);
    TSMCube* hypercube = makeHypercube (cubeShape, tileShape, values);
    uInt ncube = cubeSet_p.nelements();
    cubeSet_p.resize (ncube + 1);
    cubeSet_p[ncube] = hypercube;
    // Update the row map with the number of pixels in last dimension.
    updateRowMap (ncube, cubeShape(nrdim_p-1));
}

void TiledDataStMan::extendHypercube (uInt incrInLastDim,
				      const Record& values)
{
    // Check if id values are correctly given.
    // Get the hypercube using the id values.
    checkValues (idColSet_p, values);
    Int cubeNr = getCubeIndex (values);
    if (cubeNr < 0) {
	throw (TSMError ("extendHypercube with unknown id values"));
    }
    // Check if the number of rows involved fits in the table.
    checkNrrow (cubeSet_p[cubeNr]->cubeShape(), incrInLastDim);
    // Check if values for the last coordinate are given correctly.
    PtrBlock<TSMColumn*> lastCoord (1, coordColSet_p[nrdim_p-1]);
    IPosition lastDim (1, incrInLastDim);
    checkCoordinates (lastCoord, lastDim, values);
    cubeSet_p[cubeNr]->extend (incrInLastDim, values, lastCoord[0]);
    updateRowMap (cubeNr, incrInLastDim);
    setDataChanged();
}


void TiledDataStMan::updateRowMap (uInt cubeNr, uInt incrInLastDim)
{
    if (incrInLastDim == 0) {
	return;
    }
    // Extend the maps when needed.
    if (nrUsedRowMap_p == rowMap_p.nelements()) {
	rowMap_p.resize (nrUsedRowMap_p + 64);
	cubeMap_p.resize (nrUsedRowMap_p + 64);
	posMap_p.resize (nrUsedRowMap_p + 64);
    }
    // Determine the numbers of rows added via the extension.
    const IPosition& shape = cubeSet_p[cubeNr]->cubeShape();
    rowMap_p[nrUsedRowMap_p] = nrrowLast_p;
    cubeMap_p[nrUsedRowMap_p] = cubeNr;
    posMap_p[nrUsedRowMap_p] = shape(nrdim_p-1) - incrInLastDim;
    nrUsedRowMap_p++;
    // Now update the last row number used with the
    // number of rows this extension represents.
    uInt nr = addedNrrow (cubeSet_p[cubeNr]->cubeShape(), incrInLastDim);
    nrrowLast_p += nr;
}

TSMCube* TiledDataStMan::getHypercube (uInt rownr)
{
    IPosition pos;
    return TiledDataStMan::getHypercube (rownr, pos);
}
TSMCube* TiledDataStMan::getHypercube (uInt rownr, IPosition& position)
{
    // Check if the row number is correct.
    if (rownr >= nrrowLast_p ) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    // Find the closest row number in the map (equal or less).
    Bool found;
    uInt index = binarySearchBrackets (found, rowMap_p, rownr, nrUsedRowMap_p);
    if (!found) {
	index--;
    }
    // Get the hypercube and the rownr relative to the start
    // of the hypercube chunk the requested row is in.
    TSMCube* hypercube = cubeSet_p[cubeMap_p[index]];
    uInt rowDiff = rownr - rowMap_p[index];
    // Transform the relative rownr into a hypercube position.
    // When the hypercube has axes with vector coordinates, those
    // axes are part of the data in the cell (thus only scalar
    // coordinates have to be taken into account).
    const IPosition& shape = hypercube->cubeShape();
    position.resize (0);
    position = shape;
    for (uInt i=nrCoordVector_p; i<nrdim_p-1; i++) {
	position(i) = rowDiff % shape(i);
	rowDiff /= shape(i);
    }
    // Add the starting position of the hypercube chunk the row is in.
    position(nrdim_p - 1) = rowDiff + posMap_p[index];
    return hypercube;
}

} //# NAMESPACE CASACORE - END

