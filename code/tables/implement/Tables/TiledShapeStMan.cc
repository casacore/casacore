//# TiledShapeStMan.cc: Tiled Data Storage Manager using the shape as id
//# Copyright (C) 1998
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

#include <aips/Tables/TiledShapeStMan.h>
#include <aips/Tables/TSMColumn.h>
#include <aips/Tables/TSMCube.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Utilities/String.h>
#include <aips/Containers/BlockIO.h>
#include <aips/IO/AipsIO.h>
#include <aips/Tables/DataManError.h>



//# Allocate an empty record to avoid reconstructing it over and over
//# again when addRow is called many times.
static Record emptyRecord;



TiledShapeStMan::TiledShapeStMan ()
: TiledStMan()
{}

TiledShapeStMan::TiledShapeStMan (const String& hypercolumnName,
				  const IPosition& defaultTileShape,
				  uInt maximumCacheSize)
: TiledStMan         (hypercolumnName, maximumCacheSize),
  defaultTileShape_p (defaultTileShape)
{}

TiledShapeStMan::~TiledShapeStMan()
{}

DataManager* TiledShapeStMan::clone() const
{
    TiledShapeStMan* smp = new TiledShapeStMan (hypercolumnName_p,
						defaultTileShape_p,
						maximumCacheSize());
    if (smp == 0) {
	throw (AllocError ("TiledShapeStMan::clone", 1));
    }
    return smp;
}

DataManager* TiledShapeStMan::makeObject (const String&)
{
    TiledShapeStMan* smp = new TiledShapeStMan();
    if (smp == 0) {
	throw (AllocError ("TiledShapeStMan::makeObject", 1));
    }
    return smp;
}

String TiledShapeStMan::dataManagerType() const
    { return "TiledShapeStMan"; }


IPosition TiledShapeStMan::defaultTileShape() const
{
    return defaultTileShape_p;
}


void TiledShapeStMan::setShape (uInt rownr, TSMCube*,
				const IPosition& shape,
				const IPosition& tileShape)
{
    IPosition cubeShape = shape;
    uInt n = shape.nelements();
    cubeShape.resize (n+1);
    cubeShape(n) = 0;                   // hypercube is extensible
    // Find a hypercube with given shape.
    Int index = findHypercube (cubeShape);
    // Extend hypercube when found.
    // Otherwise create a new one.
    if (index >= 0) {
	extendHypercube (rownr, index);
    }else{
	addHypercube (rownr, cubeShape, tileShape);
    }
    // Clear the value record in the first (dummy) cube, since it may
    // contain coordinates defined before the shape was defined.
    cubeSet_p[0]->rwValueRecord() = emptyRecord;
}

Int TiledShapeStMan::findHypercube (const IPosition& shape)
{
    // A hypercube matches when its shape matches.
    // Its last axis is excluded, because it represents the rows.
    uInt n = cubeSet_p.nelements();
    for (uInt i=1; i<n; i++) {
	if (shape.isEqual (cubeSet_p[i]->cubeShape(), nrdim_p-1)) {
	    return i;
	}
    }
    return -1;
}

void TiledShapeStMan::setupCheck (const TableDesc& tableDesc,
				  const Vector<String>& dataNames) const
{
    // The data columns may only contain arrays with the correct
    // dimensionality, which should be one less than the hypercube
    // dimensionality.
    Int ndim = nrdim_p - 1;
    for (uInt i=0; i<dataNames.nelements(); i++) {
	const ColumnDesc& columnDesc = tableDesc.columnDesc (dataNames(i));
	if (! columnDesc.isArray()  ||  ndim != columnDesc.ndim()) {
	    throw (TSMError ("Dimensionality of column " + dataNames(i) +
			     " is incorrect"));
	}
    }
}


void TiledShapeStMan::create (uInt nrrow)
{
    // Set up the various things.
    setup();
    // Create a cubeset (with no file attached) for undefined cells.
    cubeSet_p.resize (1);
    cubeSet_p[0] = new TSMCube (this, 0);
    // Add the rows for the given number of rows.
    addRow (nrrow);
}
	    

Bool TiledShapeStMan::flush (AipsIO&, Bool fsync)
{
    // Flush the caches.
    // Exit if nothing has changed.
    if (! flushCaches (fsync)) {
	return False;
    }
    // Create the header file and write data in it.
    AipsIO* headerFile = headerFileCreate();
    headerFile->putstart ("TiledShapeStMan", 1);
    // Let the base class write its data.
    headerFilePut (*headerFile, cubeSet_p.nelements());
    // Write the data from this object.
    *headerFile << defaultTileShape_p;
    putBlock (*headerFile, cubeMap_p, Int(nrrow_p));
    putBlock (*headerFile, posMap_p,  Int(nrrow_p));
    headerFile->putend();
    headerFileClose (headerFile);
    return True;
}

void TiledShapeStMan::readHeader (uInt tabNrrow, Bool firstTime)
{
    // Open the header file and read data from it.
    AipsIO* headerFile = headerFileOpen();
    headerFile->getstart ("TiledShapeStMan");
    // Let the base class read and initialize its data.
    headerFileGet (*headerFile, tabNrrow, firstTime);
    // Read the data for this object.
    *headerFile >> defaultTileShape_p;
    getBlock (*headerFile, cubeMap_p);
    getBlock (*headerFile, posMap_p);
    headerFile->getend();
    headerFileClose (headerFile);
}


void TiledShapeStMan::addRow (uInt nrow)
{
    if (fixedCellShape_p.nelements() > 0) {
	for (uInt i=nrrow_p; i<nrrow_p+nrow; i++) {
	    setShape (i, 0, fixedCellShape_p, defaultTileShape_p);
	}
    }else{
	updateRowMap (0, 0, nrrow_p, nrow);
    }
    nrrow_p += nrow;
    setDataChanged();
}


void TiledShapeStMan::addHypercube (uInt rownr,
				    const IPosition& cubeShape,
				    const IPosition& tileShape)
{
    // Set last axis of tile shape to 1 in case it is not defined.
    IPosition tshape = tileShape;
    if (tshape.nelements() == nrdim_p-1) {
	tshape.resize (nrdim_p);
	tshape(nrdim_p-1) = 1;
    } else if (tshape.nelements() != nrdim_p) {
	throw (TSMError ("setShape: nelements in tileShape is incorrect"));
    }
    // Check the given cube shape.
    // Note that a coordinate may have been defined already,
    // so also check against the values of the first (dummy) cube.
    TSMCube* zeroCube = cubeSet_p[0];
    checkCubeShape (zeroCube, cubeShape);
    TSMCube* hypercube = makeHypercube (cubeShape, tshape,
					zeroCube->valueRecord());
    uInt ncube = cubeSet_p.nelements();
    cubeSet_p.resize (ncube + 1);
    cubeSet_p[ncube] = hypercube;
    // Extend the hypercube.
    extendHypercube (rownr, ncube);
}

void TiledShapeStMan::extendHypercube (uInt rownr, uInt cubeNr)
{
    TSMCube* hypercube = cubeSet_p[cubeNr];
    uInt pos = hypercube->cubeShape()(nrdim_p-1);
    hypercube->extend (1, emptyRecord, coordColSet_p[nrdim_p - 1]);
    updateRowMap (cubeNr, pos, rownr, 1);
    setDataChanged();
}


void TiledShapeStMan::updateRowMap (uInt cubeNr, uInt pos,
				    uInt rownr, uInt nrow)
{
    // Extend the maps when needed.
    uInt nrend = rownr+nrow;
    if (nrend > cubeMap_p.nelements()) {
	uInt nrnew = max (nrend, nrrow_p+4096);
	cubeMap_p.resize (nrnew);
	posMap_p.resize (nrnew);
    }
    for (uInt i=rownr; i<nrend; i++) {
	cubeMap_p[i] = cubeNr;
	posMap_p[i] = pos;
    }
}

TSMCube* TiledShapeStMan::getHypercube (uInt rownr)
{
    IPosition pos;
    return TiledShapeStMan::getHypercube (rownr, pos);
}
TSMCube* TiledShapeStMan::getHypercube (uInt rownr, IPosition& position)
{
    // Check if the row number is correct.
    if (rownr >= nrrow_p ) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    // Get the hypercube and the position in it.
    TSMCube* hypercube = cubeSet_p[cubeMap_p[rownr]];
    position.resize (0);
    position = hypercube->cubeShape();
    // Add the starting position of the hypercube chunk the row is in.
    if (position.nelements() > 0) {
	position(nrdim_p - 1) = posMap_p[rownr];
    }
    return hypercube;
}
