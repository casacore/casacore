//# TiledShapeStMan.cc: Tiled Data Storage Manager using the shape as id
//# Copyright (C) 1998,1999,2000,2001,2002,2003
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

#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/tables/DataMan/DataManError.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Allocate an empty record to avoid reconstructing it over and over
//# again when addRow is called many times.
static Record emptyRecord;



TiledShapeStMan::TiledShapeStMan()
: TiledStMan     (),
  nrUsedRowMap_p (0),
  lastHC_p       (-1)
{}

TiledShapeStMan::TiledShapeStMan (const String& hypercolumnName,
				  const IPosition& defaultTileShape,
				  uInt maximumCacheSize)
: TiledStMan         (hypercolumnName, maximumCacheSize),
  defaultTileShape_p (defaultTileShape),
  nrUsedRowMap_p     (0),
  lastHC_p           (-1)
{}

TiledShapeStMan::TiledShapeStMan (const String& hypercolumnName,
				  const Record& spec)
: TiledStMan     (hypercolumnName, 0),
  nrUsedRowMap_p (0),
  lastHC_p       (-1)
{
    if (spec.isDefined ("DEFAULTTILESHAPE")) {
        defaultTileShape_p = IPosition (spec.asArrayInt ("DEFAULTTILESHAPE"));
    }
    if (spec.isDefined ("MAXIMUMCACHESIZE")) {
        setPersMaxCacheSize (spec.asInt ("MAXIMUMCACHESIZE"));
    }
}

TiledShapeStMan::~TiledShapeStMan()
{}

DataManager* TiledShapeStMan::clone() const
{
    TiledShapeStMan* smp = new TiledShapeStMan (hypercolumnName_p,
						defaultTileShape_p,
						maximumCacheSize());
    return smp;
}

DataManager* TiledShapeStMan::makeObject (const String& group,
					  const Record& spec)
{
    TiledShapeStMan* smp = new TiledShapeStMan (group, spec);
    return smp;
}

String TiledShapeStMan::dataManagerType() const
    { return "TiledShapeStMan"; }


Record TiledShapeStMan::dataManagerSpec() const
{
    Record rec = TiledStMan::dataManagerSpec();
    rec.define ("IndexSize", nrUsedRowMap_p);
    return rec;
}

IPosition TiledShapeStMan::defaultTileShape() const
{
    return defaultTileShape_p;
}

Bool TiledShapeStMan::canAccessColumn (Bool& reask) const
{
    // The entire column can be accessed if all rows are in the same hypercube,
    // thus if there is 1 row map entry and the last value is #rows.
    reask = True;
    return (nrUsedRowMap_p == 1  &&  rowMap_p[0] == nrrow_p-1);
}


TSMCube* TiledShapeStMan::singleHypercube()
{
    if (nrUsedRowMap_p != 1  ||  rowMap_p[0] != nrrow_p-1) {
	throw (TSMError ("TiledShapeStMan: function on hypercolumn " +
			 hypercolumnName_p + " cannot be done "
			 "when it is using multiple hypercubes"));
    }
    return cubeSet_p[1];
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
	if (! columnDesc.isArray()) {
	    throw (TSMError ("TiledShapeStMan cannot handle scalar column " +
			     dataNames(i)));
	}
	if (ndim != columnDesc.ndim()) {
	    throw (TSMError ("Dimensionality of column " + dataNames(i) +
			     " should be one less than hypercolumn"
			     " definition when used in TiledShapeStMan"));
	}
    }
    // There shouldn't be ID columns.
    if (idColSet_p.nelements() > 0) {
        throw TSMError("ID columns cannot be used with TiledShapeStMan");
    }
}


void TiledShapeStMan::create (uInt nrrow)
{
    // Set up the various things.
    setup(1);
    // Create a cubeset (with no file attached) for undefined cells.
    cubeSet_p.resize (1);
    cubeSet_p[0] = new TSMCube (this, 0, IPosition(), IPosition(),
                                Record(), -1);
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
    *headerFile << nrUsedRowMap_p;
    putBlock (*headerFile, rowMap_p, Int(nrUsedRowMap_p));
    putBlock (*headerFile, cubeMap_p, Int(nrUsedRowMap_p));
    putBlock (*headerFile, posMap_p,  Int(nrUsedRowMap_p));
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
    headerFileGet (*headerFile, tabNrrow, firstTime, 1);
    // Read the data for this object.
    *headerFile >> defaultTileShape_p;
    *headerFile >> nrUsedRowMap_p;
    getBlock (*headerFile, rowMap_p);
    getBlock (*headerFile, cubeMap_p);
    getBlock (*headerFile, posMap_p);
    headerFile->getend();
    headerFileClose (headerFile);
}


void TiledShapeStMan::addRow (uInt nrow)
{
    uInt oldnrrow = nrrow_p;
    nrrow_p += nrow;
    if (fixedCellShape_p.nelements() > 0) {
	for (uInt i=oldnrrow; i<oldnrrow+nrow; i++) {
	    setShape (i, 0, fixedCellShape_p, defaultTileShape_p);
	}
    }
    setDataChanged();
}


void TiledShapeStMan::addHypercube (uInt rownr,
				    const IPosition& cubeShape,
				    const IPosition& tileShape)
{
    // Check the given cube shape.
    // Note that a coordinate may have been defined already,
    // so also check against the values of the first (dummy) cube.
    TSMCube* zeroCube = cubeSet_p[0];
    checkCubeShape (zeroCube, cubeShape);
    TSMCube* hypercube = makeHypercube (cubeShape, tileShape,
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
    updateRowMap (cubeNr, pos, rownr);
    setDataChanged();
}


void TiledShapeStMan::updateRowMap (uInt cubeNr, uInt pos, uInt rownr)
{
    // Check if the row number is correct.
    if (rownr >= nrrow_p) {
	throw (TSMError ("TiledShapeStMan::updateRowMap: rownr is too high"));
    }
    // Determine the next row used and check (in debug mode) if it is right.
    uInt nextRow = 0;
    if (nrUsedRowMap_p > 0) {
        nextRow = 1 + rowMap_p[nrUsedRowMap_p-1];
    }
    DebugAssert (nextRow <= nrrow_p, AipsError);
    // The row can be past the end of the rowMap.
    // In that case it is a new row which will be added.
    // If needed, intermediate zero references will also be added for
    // the new rows which do not have a shape yet.
    if (rownr >= nextRow) {
        if (cubeNr == 0) {
	    return;                // not really a new reference
	}
	// If the maps need to be extended, an extra entry is needed
	// if intermediate rows are needed.
	uInt nrext = 2;
	if (rownr == nextRow) {
	    nrext = 1;
	    // If this row is consecutive to the previous one,
	    // only the maps need to be updated.
	    if (nrUsedRowMap_p > 0) {
	        uInt i = nrUsedRowMap_p-1;
	        if (cubeNr == cubeMap_p[i]  &&  pos == 1+posMap_p[i]) {
		    rowMap_p[i]++;
		    posMap_p[i]++;
		    return;
		}
	    }
	}
	// A new entry has to be inserted.
        // Extend the maps when needed.
        if (nrUsedRowMap_p + nrext > rowMap_p.nelements()) {
	    uInt nrnew = rowMap_p.nelements() + 64;
	    rowMap_p.resize (nrnew);
	    cubeMap_p.resize (nrnew);
	    posMap_p.resize (nrnew);
	}
	if (rownr > nextRow) {
	    rowMap_p[nrUsedRowMap_p] = rownr-1;
	    cubeMap_p[nrUsedRowMap_p] = 0;
	    posMap_p[nrUsedRowMap_p] = 0;
	    nrUsedRowMap_p++;
	}
	rowMap_p[nrUsedRowMap_p] = rownr;
	cubeMap_p[nrUsedRowMap_p] = cubeNr;
	posMap_p[nrUsedRowMap_p] = pos;
        nrUsedRowMap_p++;
	return;
    }
    // Some explanation about the maps.
    // rowMap gives the last row number for which the cubeMap applies
    // and for which the positions in the cube are consecutive.
    // Thus rowMap gives row intervals for which cubeMap and posMap apply.
    // cubeMap gives the index of the cube (cubenr 0 means no value).
    // posMap gives the position of the row in rowMap in the cube.
    // Previous rows are in the previous positions.
    // E.g.   rowMap    5  10  15
    //        cubeMap   1   2   1
    //        posMap    5   4  10
    // means: row  0-5  are in pos 0-5  of cube 1
    //        row  6-10 are in pos 0-4  of cube 2
    //        row 11-15 are in pos 6-10 of cube 1

    // The row is not past the end.
    // Find the closest row number in the map
    // (returns index of entry equal or less to given one).
    Bool found;
    uInt index = binarySearchBrackets (found, rowMap_p, rownr, nrUsedRowMap_p);
    // Exit immediately if the cube and pos did not change.
    uInt diffRow = rowMap_p[index] - rownr;
    if (cubeNr == cubeMap_p[index]  &&  pos == posMap_p[index] - diffRow) {
        return;
    }
    // Determine if the new entry is at the beginning or end of a row interval.
    // If so, determine if it matches previous or next entry.
    // To match, the cube has to be the same and the position has to
    // be consecutive.
    Bool atB = (rownr == 0  ||  (index > 0  &&  rownr-1 == rowMap_p[index-1]));
    Bool atE = found;
    Bool eqP = False;
    Bool eqN = False;
    if (atE  &&  index+1 < nrUsedRowMap_p) {
        uInt fpos = posMap_p[index+1] - (rowMap_p[index+1] - rowMap_p[index]);
	eqN = (cubeNr == cubeMap_p[index+1]  &&  pos == fpos);
    }
    if (atB  &&  index > 0) {
	eqP = (cubeNr == cubeMap_p[index-1]  &&  pos == 1+posMap_p[index-1]);
    }
    if (atB && atE) {
        // We have a single entry, so update the maps directly.
        cubeMap_p[index] = cubeNr;
	posMap_p[index] = pos;
        // If it equals previous and/or next, combine maps by moving
        // the entries to the left.
        uInt nm = 0;
	if (eqN) {
	    nm += 1;
	}
	if (eqP) {
	    nm += 1;
	    index -= 1;
	}
	if (nm > 0) {
	    uInt nr = nrUsedRowMap_p - (index+nm);
	    if (nr > 0) {
	        objmove (&(rowMap_p[index]),  &(rowMap_p[index+nm]),  nr);
		objmove (&(cubeMap_p[index]), &(cubeMap_p[index+nm]), nr);
		objmove (&(posMap_p[index]),  &(posMap_p[index+nm]),  nr);
	    }
	    nrUsedRowMap_p -= nm;
	}
	return;
    }
    // Not a single entry, so we may need to do more work.
    // If equal previous or next, only the maps need to be updated.
    if (eqP) {
        rowMap_p[index-1]++;
	posMap_p[index-1]++;
	return;
    }
    if (eqN) {
        rowMap_p[index]--;
        posMap_p[index]--;
	return;
    }
    // It is getting more and more complicated.
    // A new entry has to be inserted (or 2 if in the middle).
    // So shift to the right (after extending the maps when needed).
    uInt nm = (atB || atE  ?  1 : 2);
    if (nrUsedRowMap_p + nm > rowMap_p.nelements()) {
        uInt nrnew = rowMap_p.nelements() + 64;
	rowMap_p.resize (nrnew);
	cubeMap_p.resize (nrnew);
	posMap_p.resize (nrnew);
    }
    uInt nr = nrUsedRowMap_p - index;
    if (nr > 0) {
        objmove (&(rowMap_p[index+nm]),  &(rowMap_p[index]),  nr);
	objmove (&(cubeMap_p[index+nm]), &(cubeMap_p[index]), nr);
	objmove (&(posMap_p[index+nm]),  &(posMap_p[index]),  nr);
    }
    nrUsedRowMap_p += nm;
    if (!atB) {
        if (atE) {
	    rowMap_p[index]--;
	    posMap_p[index]--;
	} else {
	    posMap_p[index] -= diffRow+1;
	    rowMap_p[index] = rownr-1;
	}
        index++;
    }
    rowMap_p[index]  = rownr;
    cubeMap_p[index] = cubeNr;
    posMap_p[index]  = pos;
}

TSMCube* TiledShapeStMan::getHypercube (uInt rownr)
{
    if (rownr >= nrrow_p) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    // Get the hypercube.
    if (nrUsedRowMap_p == 0  ||  rownr > rowMap_p[nrUsedRowMap_p-1]) {
        return cubeSet_p[0];
    }
    // Test if the row number is in the most recently used interval.
    // See description in function updateRowMap (about line 340)
    // how intervals are defined.
    if (lastHC_p < 0  ||  rownr > rowMap_p[lastHC_p]
    ||  (lastHC_p > 0  &&  rownr <= rowMap_p[lastHC_p-1])) {
        Bool found;
	lastHC_p = binarySearchBrackets (found, rowMap_p, rownr,
					 nrUsedRowMap_p);
    }
    return cubeSet_p[cubeMap_p[lastHC_p]];
}

TSMCube* TiledShapeStMan::getHypercube (uInt rownr, IPosition& position)
{
    if (rownr >= nrrow_p) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    // Get the hypercube.
    if (nrUsedRowMap_p == 0  ||  rownr > rowMap_p[nrUsedRowMap_p-1]) {
        TSMCube* hypercube = cubeSet_p[0];
        const IPosition& shp = hypercube->cubeShape();
        if (position.nelements() != shp.nelements()) {
	    position.resize (shp.nelements());
	}
	position = shp;
        return hypercube;
    }
    // Test if the row number is in the most recently used interval.
    // See description in function updateRowMap (about line 340)
    // how intervals are defined.
    if (lastHC_p < 0  ||  rownr > rowMap_p[lastHC_p]
    ||  (lastHC_p > 0  &&  rownr <= rowMap_p[lastHC_p-1])) {
        Bool found;
	lastHC_p = binarySearchBrackets (found, rowMap_p, rownr,
					 nrUsedRowMap_p);
    }
    TSMCube* hypercube = cubeSet_p[cubeMap_p[lastHC_p]];
    const IPosition& shp = hypercube->cubeShape();
    if (position.nelements() != shp.nelements()) {
        position.resize (shp.nelements());
    }
    position = shp;
    // Add the starting position of the hypercube chunk the row is in.
    if (position.nelements() > 0) {
        position(nrdim_p - 1) = posMap_p[lastHC_p] -
	                        (rowMap_p[lastHC_p] - rownr);
    }
    return hypercube;
}

} //# NAMESPACE CASACORE - END
