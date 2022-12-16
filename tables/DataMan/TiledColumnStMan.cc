//# TiledColumnStMan.cc: Storage manager for tables using tiled hypercubes
//# Copyright (C) 1995,1996,1997,1999,2000,2001,2003
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

#include <casacore/tables/DataMan/TiledColumnStMan.h>
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
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/tables/DataMan/DataManError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Allocate an empty record to avoid reconstructing it over and over
//# again when addRow is called many times.
static Record emptyRecord;



TiledColumnStMan::TiledColumnStMan ()
: TiledStMan ()
{}

TiledColumnStMan::TiledColumnStMan (const String& hypercolumnName,
				    const IPosition& tileShape,
				    const IPosition& rowShape,
				    uInt64 maximumCacheSize)
: TiledStMan  (hypercolumnName, maximumCacheSize),
  tileShape_p (tileShape),
  rowShape_p  (rowShape)
{}

TiledColumnStMan::TiledColumnStMan (const String& hypercolumnName,
				    const IPosition& tileShape,
				    uInt64 maximumCacheSize)
: TiledStMan  (hypercolumnName, maximumCacheSize),
  tileShape_p (tileShape)
{}

TiledColumnStMan::TiledColumnStMan (const String& hypercolumnName,
				    const Record& spec)
: TiledStMan  (hypercolumnName, 0)
{
    if (spec.isDefined ("DEFAULTTILESHAPE")) {
        tileShape_p = IPosition (spec.toArrayInt ("DEFAULTTILESHAPE"));
    }
    if (spec.isDefined ("ROWSHAPE")) {
        rowShape_p = IPosition (spec.toArrayInt ("ROWSHAPE"));
    }
    if (spec.isDefined ("MAXIMUMCACHESIZE")) {
        setPersMaxCacheSize (spec.asInt64 ("MAXIMUMCACHESIZE"));
    }
}

TiledColumnStMan::~TiledColumnStMan()
{}

DataManager* TiledColumnStMan::clone() const
{
    TiledColumnStMan* smp = new TiledColumnStMan (hypercolumnName_p,
						  tileShape_p,
                                                  rowShape_p,
						  maximumCacheSize());
    return smp;
}

DataManager* TiledColumnStMan::makeObject (const String& group,
					   const Record& spec)
{
    TiledColumnStMan* smp = new TiledColumnStMan (group, spec);
    return smp;
}

String TiledColumnStMan::dataManagerType() const
{
    return "TiledColumnStMan";
}

Bool TiledColumnStMan::canAccessColumn() const
{
    // This is only possible if an integral nr of rows fit the hypercube.
    // Note that rowMap_p always contains at least 1 element.
    DebugAssert (rowMap_p.size() > 0, AipsError);
    return nrrow_p % rowMap_p[rowMap_p.size() - 1] == 0;
}


void TiledColumnStMan::create64 (rownr_t nrrow)
{
    // Make the hypercube extendible.
    if (rowShape_p.empty()) {
      rowShape_p = IPosition(1,0);
    } else {
      rowShape_p.last() = 0;
    }
    // Set up the various things.
    setupRowMap();
    setup (rowShape_p.size());
    // Create the one and single TSMFile object.
    createFile (0);
    // Create the hypercube object.
    // Its shape is the cell shape plus an extensible last dimension.
    // Check if the hypercube dimensionality is one extra.
    if (nrdim_p != fixedCellShape_p.nelements() + rowShape_p.size()) {
	throw (TSMError ("TiledColumnStMan: hypercube dimensionality "
			 "has to be row shape + cell dimensionality"));
    }
    IPosition cubeShape (fixedCellShape_p);
    cubeShape.resize (nrdim_p);
    for (uInt i=0; i<rowShape_p.size(); ++i) {
      cubeShape(nrdim_p - rowShape_p.size() +  i) = rowShape_p(i);
    }
    cubeSet_p.resize (1);
    cubeSet_p[0] = makeTSMCube (fileSet_p[0],
				cubeShape, tileShape_p, emptyRecord);
    // Add the rows for the given number of rows.
    addRow64 (nrrow);
}
	    

Bool TiledColumnStMan::flush (AipsIO&, Bool fsync)
{
    // Flush the caches.
    // Exit if nothing has changed.
    if (! flushCaches (fsync)) {
	return False;
    }
    // Create the header file and write data into it.
    // Use version 1 if rowshape has 1 dim, so it can be used by older casacore.
    AipsIO* headerFile = headerFileCreate();
    if (rowShape_p.size() == 1) {
        headerFile->putstart ("TiledColumnStMan", 1);
        *headerFile << tileShape_p;
    } else {
        headerFile->putstart ("TiledColumnStMan", 2);
        *headerFile << tileShape_p;
        *headerFile << rowShape_p;
    }
    // Let the base class write its data; there is only one TSMCube to write.
    headerFilePut (*headerFile, 1);
    headerFile->putend();
    headerFileClose (headerFile);
    return True;
}

void TiledColumnStMan::readHeader (rownr_t tabNrrow, Bool firstTime)
{
    // Open the header file and read data from it.
    AipsIO* headerFile = headerFileOpen();
    uInt version = headerFile->getstart ("TiledColumnStMan");
    *headerFile >> tileShape_p;
    if (version == 2) {
        *headerFile >> rowShape_p;
    } else {
        AlwaysAssert (version==1, AipsError);
        rowShape_p = IPosition(1,0);
    }
    setupRowMap();
    // Let the base class read and initialize its data.
    headerFileGet (*headerFile, tabNrrow, firstTime, rowShape_p.size());
    headerFile->getend();
    headerFileClose (headerFile);
}

void TiledColumnStMan::setupRowMap()
{
  rowMap_p.resize (rowShape_p.size());
  ssize_t sz = 1;
  for (uInt i=0; i<rowShape_p.size(); ++i) {
    rowMap_p[i] = sz;
    sz *= rowShape_p[i];
  }
}

void TiledColumnStMan::rowToPosition (IPosition& pos, rownr_t row) const
{
  DebugAssert (pos.size() >= rowMap_p.size(), AipsError);
  uInt st = pos.size() - rowMap_p.size();
  ssize_t inx = row;
  for (uInt i=rowMap_p.size()-1; i>0; --i) {
    pos[st + i] = inx / rowMap_p[i];
    inx -= pos[st + i] * rowMap_p[i];
  }
  pos[st] = inx;
}

void TiledColumnStMan::setupCheck (const TableDesc& tableDesc,
				   const Vector<String>& dataNames) const
{
    // The data columns may only contain arrays with the correct
    // dimensionality, which should be less than the hypercube
    // dimensionality.
    Int ndim = nrdim_p - rowShape_p.size();
    for (uInt i=0; i<dataNames.nelements(); i++) {
	const ColumnDesc& columnDesc = tableDesc.columnDesc (dataNames(i));
	if (columnDesc.isScalar()) {
	    if (ndim != 0) {
	        throw (TSMError ("Using scalar column " + dataNames(i) +
				 " in TiledColumnStMan needs the hypercolumn"
				 " to be 1-dim"));
	    }
	} else {
	    if (! columnDesc.isArray()  ||  ndim != columnDesc.ndim()) {
	        throw (TSMError ("Dimensionality of column " + dataNames(i) +
				 " should be " +
                                 String::toString(rowShape_p.size()) +
                                 " less than hypercolumn"
				 " definition when used in TiledColumnStMan"));
	    }
	    // The data columns in a column hypercube must be fixed shape.
	    if ((columnDesc.options() & ColumnDesc::FixedShape)
	                                        != ColumnDesc::FixedShape) {
	      throw (TSMError ("TiledColumnStMan needs array column " +
			       dataNames(i) + " to be FixedShape"));
	    }
	}
    }
    // There shouldn't be ID columns.
    if (idColSet_p.nelements() > 0) {
        throw TSMError("ID columns cannot be used with TiledColumnStMan");
    }
}


IPosition TiledColumnStMan::defaultTileShape() const
{
    return tileShape_p;
}

void TiledColumnStMan::addRow64 (rownr_t nrow)
{
    // Extend the last dimension as much as needed.
    ssize_t nnew  = (nrrow_p+nrow) / rowMap_p.last() - nrrow_p / rowMap_p.last();
    // Make sure at least 1 is added if still empty.
    if (nrrow_p == 0  &&  nrow > 0  &&  nnew == 0) {
        nnew = 1;
    }
    if (nnew > 0) {
        cubeSet_p[0]->extend (nnew, emptyRecord, coordColSet_p[nrdim_p - 1]);
    }
    nrrow_p += nrow;
    setDataChanged();
}


TSMCube* TiledColumnStMan::getHypercube (rownr_t rownr)
{
    // Check if the row number is correct.
    if (rownr >= nrrow_p) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    return cubeSet_p[0];
}
TSMCube* TiledColumnStMan::getHypercube (rownr_t rownr, IPosition& position)
{
    // Check if the row number is correct.
    if (rownr >= nrrow_p) {
	throw (TSMError ("getHypercube: rownr is too high"));
    }
    // The rownr determines the position in the hypercube.
    position.resize (0);
    position = cubeSet_p[0]->cubeShape();
    rowToPosition (position, rownr);
    return cubeSet_p[0];
}

} //# NAMESPACE CASACORE - END

