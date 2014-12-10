//# TiledCellStMan.cc: Storage manager for tables using tiled hypercubes
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

#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/DataMan/TSMFile.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/tables/DataMan/DataManError.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

TiledCellStMan::TiledCellStMan ()
: TiledStMan ()
{}

TiledCellStMan::TiledCellStMan (const String& hypercolumnName,
				const IPosition& defaultTileShape,
				uInt maximumCacheSize)
: TiledStMan         (hypercolumnName, maximumCacheSize),
  defaultTileShape_p (defaultTileShape)
{}

TiledCellStMan::TiledCellStMan (const String& hypercolumnName,
				const Record& spec)
: TiledStMan  (hypercolumnName, 0)
{
    if (spec.isDefined ("DEFAULTTILESHAPE")) {
        defaultTileShape_p = IPosition (spec.asArrayInt ("DEFAULTTILESHAPE"));
    }
    if (spec.isDefined ("MAXIMUMCACHESIZE")) {
        setPersMaxCacheSize (spec.asInt ("MAXIMUMCACHESIZE"));
    }
}

TiledCellStMan::~TiledCellStMan()
{}

DataManager* TiledCellStMan::clone() const
{
    TiledCellStMan* smp = new TiledCellStMan (hypercolumnName_p,
					      defaultTileShape_p,
					      maximumCacheSize());
    return smp;
}

DataManager* TiledCellStMan::makeObject (const String& group,
					 const Record& spec)
{
    TiledCellStMan* smp = new TiledCellStMan (group, spec);
    return smp;
}

String TiledCellStMan::dataManagerType() const
    { return "TiledCellStMan"; }


IPosition TiledCellStMan::defaultTileShape() const
{
    return defaultTileShape_p;
}

Bool TiledCellStMan::canChangeShape() const
{
    return True;
}

void TiledCellStMan::setShape (uInt, TSMCube* hypercube,
			       const IPosition& shape,
			       const IPosition& tileShape)
{
    hypercube->setShape (shape, tileShape);
}


void TiledCellStMan::setupCheck (const TableDesc& tableDesc,
				 const Vector<String>& dataNames) const
{
    // The data columns should only contain arrays matching the
    // dimensionality of the hypercolumn.
    for (uInt i=0; i<dataNames.nelements(); i++) {
	const ColumnDesc& columnDesc = tableDesc.columnDesc (dataNames(i));
	if (! columnDesc.isArray()) {
	    throw (TSMError ("TiledCellStMan cannot handle scalar column " +
			     dataNames(i)));
	}
	if (Int(nrdim_p) != columnDesc.ndim()) {
	    throw (TSMError ("Dimensionality of column " + dataNames(i) +
			     " should be equal to hypercolumn"
			     " definition when used in TiledCellStMan"));
	}
    }
    // There shouldn't be ID columns.
    if (idColSet_p.nelements() > 0) {
        throw TSMError("ID columns cannot be used with TiledCellStMan");
    }
}


void TiledCellStMan::create (uInt nrrow)
{
    // Set up the various things.
    setup(0);
    // Create the one and single TSMFile object.
    createFile (0);
    // Add the rows for the given number of rows.
    addRow (nrrow);
}
	    

Bool TiledCellStMan::flush (AipsIO&, Bool fsync)
{
    // Flush the caches.
    // Exit if nothing has changed.
    if (! flushCaches (fsync)) {
	return False;
    }
    // Create the header file and write data in it.
    // A zero pointer is returned when nothing has changed, thus nothing
    // has to be written.
    AipsIO* headerFile = headerFileCreate();
    if (headerFile == 0) {
	return False;
    }
    headerFile->putstart ("TiledCellStMan", 1);
    *headerFile << defaultTileShape_p;
    // Let the base class write its data.
    headerFilePut (*headerFile, nrrow_p);
    headerFile->putend();
    headerFileClose (headerFile);
    return True;
}

void TiledCellStMan::readHeader (uInt tabNrrow, Bool firstTime)
{
    // Open the header file and read data from it.
    AipsIO* headerFile = headerFileOpen();
    headerFile->getstart ("TiledCellStMan");
    *headerFile >> defaultTileShape_p;
    // Let the base class read and initialize its data.
    headerFileGet (*headerFile, tabNrrow, firstTime, 0);
    headerFile->getend();
    headerFileClose (headerFile);
}


void TiledCellStMan::addRow (uInt nrow)
{
    // Resize block when needed.
    uInt size = cubeSet_p.nelements();
    if (size < nrrow_p + nrow) {
	size += 32;
	if (size < nrrow_p + nrow) {
	    size = nrrow_p + nrow;
	}
	cubeSet_p.resize (size);
	for (uInt i=nrrow_p; i<cubeSet_p.nelements(); i++) {
	    cubeSet_p[i] = 0;
	}
    }
    for (uInt i=nrrow_p; i<nrrow_p+nrow; i++) {
        TSMCube* hypercube = makeTSMCube (fileSet_p[0], IPosition(),
                                          IPosition(), Record());
	cubeSet_p[i] = hypercube;
	if (fixedCellShape_p.nelements() > 0) {
	    hypercube->setShape (fixedCellShape_p, defaultTileShape_p);
	}
    }
    nrrow_p += nrow;
    setDataChanged();
}


TSMCube* TiledCellStMan::getHypercube (uInt rownr)
{
    // Check if the row number is correct.
    if (rownr >= nrrow_p) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    return cubeSet_p[rownr];
}
TSMCube* TiledCellStMan::getHypercube (uInt rownr, IPosition& position)
{
    // Check if the row number is correct.
    if (rownr >= nrrow_p) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    TSMCube* hypercube = cubeSet_p[rownr];
    position.resize (0);
    position = hypercube->cubeShape();
    return hypercube;
}

} //# NAMESPACE CASACORE - END

