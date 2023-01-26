//# TiledStMan.cc: Storage manager for tables using tiled hypercubes
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/TSMDataColumn.h>
#include <casacore/tables/DataMan/TSMCoordColumn.h>
#include <casacore/tables/DataMan/TSMIdColumn.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/DataMan/TSMCubeMMap.h>
#include <casacore/tables/DataMan/TSMCubeBuff.h>
#include <casacore/tables/DataMan/TSMFile.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/OS/DOos.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/tables/DataMan/DataManError.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

TiledStMan::TiledStMan ()
: DataManager       (),
  nrrow_p           (0),
  fileSet_p         (1, static_cast<TSMFile*>(0)),
  persMaxCacheSize_p(0),
  maxCacheSize_p    (0),
  nrdim_p           (0),
  nrCoordVector_p   (0),
  dataChanged_p     (false)
{}

TiledStMan::TiledStMan (const String& hypercolumnName, uint32_t maximumCacheSize)
: DataManager       (),
  hypercolumnName_p (hypercolumnName),
  nrrow_p           (0),
  fileSet_p         (1, static_cast<TSMFile*>(0)),
  persMaxCacheSize_p(maximumCacheSize),
  maxCacheSize_p    (maximumCacheSize),
  nrdim_p           (0),
  nrCoordVector_p   (0),
  dataChanged_p     (false)
{}

TiledStMan::~TiledStMan()
{
    uint32_t i;
    for (i=0; i<ncolumn(); i++) {
	delete colSet_p[i];
    }
    for (i=0; i<cubeSet_p.nelements(); i++) {
	delete cubeSet_p[i];
    }
    for (i=0; i<fileSet_p.nelements(); i++) {
	delete fileSet_p[i];
    }
}

IPosition TiledStMan::makeTileShape (const IPosition& hypercubeShape,
				     double tolerance,
				     uint64_t nrPixelsPerTile)
{
    Vector<double> weight(hypercubeShape.nelements());
    weight = double(1);
    Vector<double> tol(hypercubeShape.nelements());
    tol = tolerance;
    return makeTileShape (hypercubeShape, weight, tol, nrPixelsPerTile);
}
IPosition TiledStMan::makeTileShape (const IPosition& hypercubeShape,
				     const Vector<double>& weight,
				     const Vector<double>& tolerance,
				     uint64_t nrPixelsPerTile)
{
    uint32_t nrdim = hypercubeShape.nelements();
    if (weight.nelements() != nrdim  ||  tolerance.nelements() != nrdim) {
	throw (TSMError ("makeTileShape: nelements mismatch"));
    }
    double nrLeft = nrPixelsPerTile;
    Vector<double> tmpShape(nrdim);
    IPosition tileShape(nrdim, 0);
    uint32_t i;
    // Iterate until the tile shape is set nicely.
    // This is needed to prevent tile shape dimensions from underflow
    // or overflow.
    while (true) {
	double prod = 1;
	uint32_t n = 0;
	for (i=0; i<nrdim; i++) {
	    if (tileShape(i) == 0) {
		prod *= hypercubeShape(i) * weight(i);
		n++;
	    }
	}
	// Exit if nothing left.
	if (n == 0) {
	    break;
	}
	double factor = pow (nrLeft / prod, double(1) / n);
	double maxDiff = 0;
	double diff;
	int32_t maxIndex = -1;
	// Calculate the tile shape for the remaining dimensions.
	// Determine the greatest difference in case of underflow/overflow.
	// (note that the reciproke is used, thus in fact the minimum matters).
	// That tile dimension will be set and the iteration starts again.
	for (i=0; i<nrdim; i++) {
	    if (tileShape(i) == 0) {
		diff = hypercubeShape(i) * weight(i) * factor;
		tmpShape(i) = diff;
		if (diff > 1) {
		    diff = hypercubeShape(i) / diff;
		}
		if (maxIndex < 0  ||  diff < maxDiff) {
		    maxDiff  = diff;
		    maxIndex = i;
		}
	    }
	}
	// If there is no underflow/overflow we can copy the dimensions
	// and exit.
	if (maxDiff >= 1) {
	    for (i=0; i<nrdim; i++) {
		if (tileShape(i) == 0) {
		    tileShape(i) = int64_t(tmpShape(i) + 0.5);   // round-off
		}
	    }
	    break;
	}
	// Set the dimension with the greatest difference.
	if (tmpShape(maxIndex) < 1) {
	    tileShape(maxIndex) = 1;
	}else{
	    tileShape(maxIndex) = hypercubeShape(maxIndex);
	    nrLeft /= tileShape(maxIndex);
	}
    }
    // Return the found tile shape when fitting exactly.
    bool isFit = true;
    double size = 1;
    for (i=0; i<nrdim; i++) {
	if (hypercubeShape(i) % tileShape(i) != 0) {
	    isFit = false;
	}
	size *= hypercubeShape(i);
    }
    if (isFit) {
	return tileShape;
    }
    // When the cube shape <= 4* the maximum tile size, return that.
    if (size <= 4*nrPixelsPerTile) {
	return hypercubeShape;
    }

    // We have to do a bit more to find a nice tile shape.
    // Use the tolerance to find the tile shape boundaries to search.
    IPosition bestShape (tileShape);
    IPosition minShape (nrdim);
    IPosition maxShape (nrdim);
    double cubeSpace = 1;
    for (i=0; i<nrdim; i++) {
	minShape(i) = int64_t (tileShape(i) * tolerance(i));
	maxShape(i) = int64_t (tileShape(i) / tolerance(i) + 0.5);
	if (minShape(i) > maxShape(i)) {
	    int64_t sav = minShape(i);
	    minShape(i) = maxShape(i);
	    maxShape(i) = sav;
	}
	if (minShape(i) < 1) {
	    minShape(i) = 1;
	}
	if (maxShape(i) > hypercubeShape(i)) {
	    maxShape(i) = hypercubeShape(i);
	}
	cubeSpace *= hypercubeShape(i);
    }
    // Find the shapes on each axis that will be tried.
    Block<uint64_t> nval(nrdim, uint64_t(0));
    PtrBlock<Block<int64_t>*> values(nrdim);
    for (i=0; i<nrdim; i++) {
	values[i] = new Block<int64_t> (maxShape(i) - minShape(i) + 1);
	// First find exactly fitting shapes.
	for (int64_t j=minShape(i); j<=maxShape(i); j++) {
	    if (hypercubeShape(i) % j == 0) {
		(*values[i])[nval[i]] = j;
		nval[i]++;
	    }
	}
	// If none available, use all possible shapes within half the range.
	if (nval[i] == 0) {
	    for (int64_t j=(tileShape(i)+minShape(i))/2;
                 j<=(tileShape(i)+maxShape(i))/2; j++) {
		(*values[i])[nval[i]] = j;
		nval[i]++;
	    }
	}
    }
    // Now calculate the cost for all the possibilities.
    // Take the one with the lowest cost.
    Block<uint64_t> ndone (nrdim, uint64_t(0));
    IPosition tshape (nrdim);
    for (i=0; i<nrdim; i++) {
	tshape(i) = (*values[i])[0];
    }
    double minCost = 1000000;
    while (true) {
	int64_t totalSize = 1;
	double totalSpace = 1;
	double costAxes = 0;
	for (i=0; i<nrdim; i++) {
	    totalSize *= tshape(i);
	    int64_t ntile = (hypercubeShape(i) + tshape(i) - 1) / tshape(i);
	    totalSpace *= ntile * tshape(i);
	    costAxes += abs(tileShape(i) - tshape(i)) / double(tileShape(i));
	}
	double waste = (totalSpace - cubeSpace) / cubeSpace;
	double diff  = abs(double(totalSize) -
					   nrPixelsPerTile) / nrPixelsPerTile;
	double cost = (costAxes + 10*waste + diff);
	if (cost < minCost) {
	    bestShape = tshape;
	    minCost = cost;
	}
///	cout << cost << " " << costAxes << " " << waste << " "
///	     << diff << " " << tshape << endl;
	for (i=0; i<nrdim; i++) {
	    if (++ndone[i] < nval[i]) {
		tshape(i) = (*values[i])[ndone[i]];
		break;
	    }
	    ndone[i] = 0;
	    tshape(i) = (*values[i])[0];
	}
	if (i == nrdim) {
	    break;
	}
    }
    // Optimize the tile shape by recalculating tile length for the same
    // number of tiles.
    for (i=0; i<nrdim; i++) {
	delete values[i];
	uint64_t nrtile = (hypercubeShape(i) + bestShape(i) - 1) / bestShape(i);
	bestShape(i) = (hypercubeShape(i) + nrtile - 1) / nrtile;
    }
    return bestShape;
}


String TiledStMan::dataManagerName() const
    { return hypercolumnName_p; }

void TiledStMan::setDataManagerName(const String& newHypercolumnName)
    { hypercolumnName_p = newHypercolumnName; }

Record TiledStMan::dataManagerSpec() const
{
    Record rec = getProperties();
    rec.define ("DEFAULTTILESHAPE", defaultTileShape().asVector());
    rec.define ("MAXIMUMCACHESIZE", int64_t(persMaxCacheSize_p));
    Record subrec;
    int32_t nrrec=0;
    for (uint64_t i=0; i<cubeSet_p.nelements(); i++) {
	if (cubeSet_p[i] != 0  &&  cubeSet_p[i]->cubeShape().nelements() > 0) {
	    Record srec;
	    srec.define ("CubeShape", cubeSet_p[i]->cubeShape().asVector());
	    srec.define ("TileShape", cubeSet_p[i]->tileShape().asVector());
	    srec.define ("CellShape", cubeSet_p[i]->cellShape().asVector());
	    srec.define ("BucketSize", int32_t(cubeSet_p[i]->bucketSize()));
	    srec.defineRecord ("ID", cubeSet_p[i]->valueRecord());
	    subrec.defineRecord (nrrec++, srec);
	}
    }
    rec.defineRecord ("HYPERCUBES", subrec);
    rec.define ("SEQNR", sequenceNr());
    return rec;
}

Record TiledStMan::getProperties() const
{
    Record rec;
    rec.define ("MaxCacheSize", int32_t(maxCacheSize_p));
    return rec;
}

void TiledStMan::setProperties (const Record& rec)
{
    if (rec.isDefined("MaxCacheSize")) {
        setMaximumCacheSize (rec.asInt("MaxCacheSize"));
    }
}


void TiledStMan::setShape (rownr_t, TSMCube*, const IPosition&, const IPosition&)
{
    throw (TSMError ("setShape is not possible for TSM " + hypercolumnName_p));
}

void TiledStMan::reopenRW()
{
    for (uint32_t i=0; i<fileSet_p.nelements(); i++) {
	if (fileSet_p[i] != 0) {
	    fileSet_p[i]->bucketFile()->setRW();
	}
    }
}

void TiledStMan::deleteManager()
{
    for (uint32_t i=0; i<cubeSet_p.nelements(); i++) {
	if (cubeSet_p[i] != 0) {
	  cubeSet_p[i]->clearCache (false);
	}
    }
    for (uint32_t i=0; i<fileSet_p.nelements(); i++) {
	if (fileSet_p[i] != 0) {
	    fileSet_p[i]->bucketFile()->remove();
	}
    }
    // Remove the header file.
    ///    removeFile();
    DOos::remove (fileName(), false, false);
}

void TiledStMan::setMaximumCacheSize (uint32_t nMiB)
    { maxCacheSize_p = nMiB; }


bool TiledStMan::canChangeShape() const
{
    return false;
}

bool TiledStMan::canAccessColumn() const
{
    return (nhypercubes() == 1);
}

bool TiledStMan::hasMultiFileSupport() const
{
    return true;
}

//# Does the storage manager allow to add rows? (yes)
bool TiledStMan::canAddRow() const
{
    return true;
}

TSMCube* TiledStMan::makeTSMCube (TSMFile* file, const IPosition& cubeShape,
                                  const IPosition& tileShape,
                                  const Record& values,
                                  int64_t fileOffset)
{
    TSMCube* hypercube;
    if (tsmOption().option() == TSMOption::MMap) {
        //cout << "mmapping TSM1" << endl;
        AlwaysAssert (file->bucketFile()->isMapped(), AipsError);
        hypercube = new TSMCubeMMap (this, file, cubeShape, tileShape,
                                     values, fileOffset);
    } else if (tsmOption().option() == TSMOption::Buffer) {
        //cout << "buffered TSM1" << endl;
        AlwaysAssert (file->bucketFile()->isBuffered(), AipsError);
        hypercube = new TSMCubeBuff (this, file, cubeShape, tileShape,
                                     values, fileOffset,
                                     tsmOption().bufferSize());
    } else {
        //cout << "caching TSM1" << endl;
        AlwaysAssert (file->bucketFile()->isCached(), AipsError);
        hypercube = new TSMCube (this, file, cubeShape, tileShape,
                                 values, fileOffset);
    }
    return hypercube;
}

TSMCube* TiledStMan::getTSMCube (uint32_t hypercube)
{
    if (hypercube >= nhypercubes()  ||  cubeSet_p[hypercube] == 0) {
      throw (AipsError ("TiledStMan::getTSMCube - hypercube nr "
			+ String::toString(hypercube) + " does not exist in " +
                        hypercolumnName_p));
    }
    return cubeSet_p[hypercube];
}


const IPosition& TiledStMan::hypercubeShape (rownr_t rownr) const
{
    return getHypercube(rownr)->cubeShape();
}

const IPosition& TiledStMan::tileShape (rownr_t rownr) const
{
    return getHypercube(rownr)->tileShape();
}

uint64_t TiledStMan::bucketSize (rownr_t rownr) const
{
    return getHypercube(rownr)->bucketSize();
}

uint32_t TiledStMan::cacheSize (rownr_t rownr) const
{
    return getHypercube(rownr)->cacheSize();
}

uint32_t TiledStMan::calcCacheSize (rownr_t rownr,
                                const IPosition& sliceShape,
                                const IPosition& windowStart,
                                const IPosition& windowLength,
                                const IPosition& axisPath) const
{
    // Calculate the cache size for the given hypercube.
    return getHypercube(rownr)->calcCacheSize (sliceShape, windowStart,
					       windowLength, axisPath);
}

void TiledStMan::setCacheSize (rownr_t rownr,
			       const IPosition& sliceShape,
			       const IPosition& windowStart,
			       const IPosition& windowLength,
			       const IPosition& axisPath,
			       bool forceSmaller)
{
    // Set the cache size for the given hypercube.
    getHypercube(rownr)->setCacheSize (sliceShape, windowStart,
				       windowLength, axisPath,
				       forceSmaller, true);
}

void TiledStMan::setCacheSize (rownr_t rownr, uint32_t nbuckets, bool forceSmaller)
{
    // Set the cache size (in buckets) for the given hypercube.
    TSMCube* hypercube = getHypercube(rownr);
    hypercube->setCacheSize (nbuckets, forceSmaller, true);
}

void TiledStMan::setHypercubeCacheSize (uint32_t hypercube, uint32_t nbuckets, bool forceSmaller)
{
    // Set the cache size (in buckets) for the given hypercube.
    TSMCube* tsmCube = getTSMCube (hypercube);
    tsmCube->setCacheSize (nbuckets, forceSmaller, true);
}


bool TiledStMan::userSetCache (rownr_t rownr) const
{
    return getHypercube(rownr)->userSetCache();
}

void TiledStMan::emptyCaches()
{
    for (uint32_t i=0; i<cubeSet_p.nelements(); i++) {
	if (cubeSet_p[i] != 0) {
	    cubeSet_p[i]->emptyCache();
	}
    }
}

void TiledStMan::showCacheStatistics (ostream& os) const
{
    for (uint32_t i=0; i<cubeSet_p.nelements(); i++) {
	if (cubeSet_p[i] != 0) {
	    cubeSet_p[i]->showCacheStatistics (os);
	}
    }
}

TSMCube* TiledStMan::singleHypercube()
{
    if (cubeSet_p.nelements() != 1  ||  cubeSet_p[0] == 0) {
	throw (TSMError ("TiledStMan: function on hypercolumn " +
			 hypercolumnName_p + " cannot be done "
			 "when it is using multiple hypercubes"));
    }
    return cubeSet_p[0];
}


uint64_t TiledStMan::getLengthOffset (uint64_t nrPixels, Block<uint32_t>& dataOffset,
                                    Block<uint32_t>& localOffset,
                                    uint32_t& localTileLength) const
{
    localTileLength = 0;
    uint64_t length = 0;
    uint32_t nrcol = dataCols_p.nelements();
    dataOffset.resize (nrcol);
    localOffset.resize (nrcol);
    for (uint32_t i=0; i<nrcol; i++) {
	dataOffset[i] = length;
	localOffset[i] = localTileLength;
	length += dataCols_p[i]->dataLength (nrPixels);
	localTileLength += nrPixels * dataCols_p[i]->localPixelSize();
    }
    return length;
}

void TiledStMan::readTile (char* local,
			   const Block<uint32_t>& localOffset,
			   const char* external,
			   const Block<uint32_t>& externalOffset,
			   uint32_t nrPixels)
{
    uint32_t nr = dataCols_p.nelements();
    for (uint32_t i=0; i<nr; i++) {
	dataCols_p[i]->readTile (local + localOffset[i],
				 external + externalOffset[i],
				 nrPixels);
    }
}

void TiledStMan::writeTile (char* external,
			    const Block<uint32_t>& externalOffset,
			    const char* local,
			    const Block<uint32_t>& localOffset,
			    uint32_t nrPixels)
{
    uint32_t nr = dataCols_p.nelements();
    for (uint32_t i=0; i<nr; i++) {
	dataCols_p[i]->writeTile (external + externalOffset[i],
				  local + localOffset[i],
				  nrPixels);
    }
}


DataManagerColumn* TiledStMan::makeScalarColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId)
{
    return makeIndArrColumn (columnName, dataType, dataTypeId);
}
DataManagerColumn* TiledStMan::makeDirArrColumn (const String& columnName,
						 int dataType,
						 const String& dataTypeId)
{
    return makeIndArrColumn (columnName, dataType, dataTypeId);
}
DataManagerColumn* TiledStMan::makeIndArrColumn (const String& columnName,
						 int dataType,
						 const String&)
{
    //# Check if data type is not TpOther.
    throwDataTypeOther (columnName, dataType);
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    TSMColumn* colp = new TSMColumn (this, dataType, columnName);
    colSet_p[ncolumn()] = colp;
    return colp;
}

int TiledStMan::coordinateDataType (const String& columnName) const
{
    for (uint32_t i=0; i<coordColSet_p.nelements(); i++) {
	if (coordColSet_p[i] != 0) {
	    if (columnName == coordColSet_p[i]->columnName()) {
		return coordColSet_p[i]->dataType();
	    }
	}
    }
    throw (TSMError ("coordinateDataType: column " + columnName +
		     " is unknown"));
    return 0;
}

// Get the proper array data type.
int TiledStMan::arrayDataType (int dataType) const
{
    switch (dataType) {
    case TpBool:
	return TpArrayBool;
    case TpChar:
	return TpArrayChar;
    case TpUChar:
	return TpArrayUChar;
    case TpShort:
	return TpArrayShort;
    case TpUShort:
	return TpArrayUShort;
    case TpInt:
	return TpArrayInt;
    case TpUInt:
	return TpArrayUInt;
    case TpInt64:
	return TpArrayInt64;
    case TpFloat:
	return TpArrayFloat;
    case TpDouble:
	return TpArrayDouble;
    case TpComplex:
	return TpArrayComplex;
    case TpDComplex:
	return TpArrayDComplex;
    case TpString:
	return TpArrayString;
    }
    return dataType;
}


IPosition TiledStMan::defaultTileShape() const
{
    return IPosition();
}


bool TiledStMan::canReallocateColumns() const
    { return true; }

DataManagerColumn* TiledStMan::reallocateColumn (DataManagerColumn* column)
{
    for (uint32_t i=0; i<ncolumn(); i++) {
	if (column == colSet_p[i]) {
	    TSMColumn* ptr = colSet_p[i];
	    colSet_p[i] = ptr->unlink();
	    delete ptr;
	    return colSet_p[i];
	}
    }
    // The column is not part of this storage manager, so return column itself.
    return column;
}
    

void TiledStMan::setup (int32_t extraNdim)
{
    uint32_t i;
    // Get the description of the hypercolumn.
    Vector<String> dataNames;
    Vector<String> coordNames;
    Vector<String> idNames;
    const TableDesc& tableDesc = getDesc();
    if (extraNdim < 0  ||  tableDesc.isHypercolumn (hypercolumnName_p)) {
        // If defined as a hypercolumn get the columns in it.
        nrdim_p = tableDesc.hypercolumnDesc (hypercolumnName_p, dataNames,
					     coordNames, idNames);
	// Determine the number of vector coordinates.
	// This is the dimensionality of the cells.
	nrCoordVector_p = tableDesc.columnDesc(dataNames(0)).ndim();
    } else {
        // No hypercolumn definition; assume all columns are data columns.
        int32_t ndim = 0;
	dataNames.resize (ncolumn());
	for (uint32_t i=0; i<ncolumn(); i++) {
	  dataNames(i) = colSet_p[i]->columnName();
	  int32_t nd = tableDesc.columnDesc(dataNames(i)).ndim();
	  if (nd > 0) {
	    if (ndim == 0) {
	        ndim = nd;
	    } else if (nd != ndim) {
	        throw TSMError ("TiledStMan: dimensionality of column " +
				dataNames(i) + " mismatches other columns");
	    }
	  }
	}
	if (ndim == 0) {
	    throw TSMError ("TiledStMan: unknown dimensionality for column " +
			    dataNames(0));
	}
	nrCoordVector_p = ndim;
	nrdim_p = ndim + extraNdim;
    }
    // Check if the required columns are bound
    // and get the pointers to those columns.
    dataCols_p.resize (dataNames.nelements());
    dataColSet_p.resize (dataNames.nelements());
    coordColSet_p.resize (nrdim_p);
    idColSet_p.resize (idNames.nelements());
    uint32_t nrDataBound = getBindings (dataNames, dataColSet_p, true);
    uint32_t nrCoordBound = getBindings (coordNames, coordColSet_p, false);
    uint32_t nrIdBound = getBindings (idNames, idColSet_p, true);
    // Check if no non-TiledStMan columns are bound.
    if (nrDataBound + nrCoordBound + nrIdBound  !=  ncolumn()) {
	throw (TSMError ("non-TiledStMan columns bound in " +
                         hypercolumnName_p));
    }
    // Let the derived class do some more checks.
    setupCheck (tableDesc, dataNames);
    // Find the first fixed shape data column.
    // Check if FixedShape column shapes of data and coordinate columns match.
    for (i=0; i<dataColSet_p.nelements(); i++) {
	fixedCellShape_p = dataColSet_p[i]->shapeColumn();
	if (fixedCellShape_p.nelements() > 0) {
	    break;
	}
    }
    checkShapeColumn (fixedCellShape_p);
    // Construct the various TSMColumn objects.
    for (i=0; i<coordColSet_p.nelements(); i++) {
	if (coordColSet_p[i] != 0) {
	    coordColSet_p[i] = coordColSet_p[i]->makeCoordColumn (i);
	}
    }
    for (i=0; i<idColSet_p.nelements(); i++) {
	idColSet_p[i] = idColSet_p[i]->makeIdColumn();
    }
    uint32_t nrd = dataColSet_p.nelements();
    PtrBlock<TSMDataColumn*> dataColSet(nrd);
    for (i=0; i<nrd; i++) {
	dataColSet[i] = dataColSet_p[i]->makeDataColumn();
    }
    // Organize the pixel offset in the data columns in descending
    // order of external pixel length.
    // The sort is stable, so equal lengths will always occur in
    // the same order.
    // In that way we are sure that their data are aligned in a tile
    // (which may be needed for TSMCube::accessLine).
    Block<uint32_t> lengths(nrd);
    for (i=0; i<nrd; i++) {
	lengths[i] = dataColSet[i]->tilePixelSize();
    }
    Vector<uint32_t> inx;
    GenSortIndirect<uint32_t,uint32_t>::sort (inx, lengths, nrd, Sort::Descending);
    // Rearrange the objects and set their column number.
    // In this way function setLengths will behave correctly.
    for (i=0; i<nrd; i++) {
	dataCols_p[i] = dataColSet[inx(i)];
	dataCols_p[i]->setColumnNumber (i);
	dataColSet_p[i] = dataCols_p[i];
    }
}


const TableDesc& TiledStMan::getDesc() const
{
    return table().tableDesc();
}


void TiledStMan::setupCheck (const TableDesc&,
			     const Vector<String>&) const
{}

void TiledStMan::checkCubeShape (const TSMCube* hypercube,
				 const IPosition& cubeShape) const
{
    // Check if the dimensionalities are correct.
    if (cubeShape.nelements() != nrdim_p) {
	throw (TSMError ("addHypercube dimensionality mismatch in " +
                         hypercolumnName_p));
    }
    // Check if all dimensions are > 0.
    // Only the last one in shape can be 0 (meaning extensible).
    for (uint32_t i=0; i<nrdim_p-1; i++) {
	if (cubeShape(i) == 0) {
	    throw (TSMError ("addHypercube dimensions are zero in " +
                             hypercolumnName_p));
	}
    }
    // Check if cube shape matches fixed shaped columns.
    checkShapeColumn (cubeShape);
    // Check if cube shape matches possibly already defined coordinates.
    if (hypercube != 0) {
	checkCoordinatesShapes (hypercube, cubeShape);
    }
}

void TiledStMan::checkShapeColumn (const IPosition& shape) const
{
    // There is nothing to check if no shape is given.
    if (shape.nelements() == 0) {
	return;
    }
    uint32_t i;
    // First check if fixed data columns match.
    for (i=0; i<dataColSet_p.nelements(); i++) {
	const IPosition& shapeColumn = dataColSet_p[i]->shapeColumn();
	for (uint32_t j=0; j<shapeColumn.nelements(); j++) {
	    if (shape(j) != shapeColumn(j)) {
		throw (TSMError ("Mismatch in fixed shape of data column "
				 + dataColSet_p[i]->columnName()));
	    }
	}
    }
    for (i=0; i<nrCoordVector_p; i++) {
	if (coordColSet_p[i] != 0) {
	    const IPosition& shapeColumn = coordColSet_p[i]->shapeColumn();
	    if (shapeColumn.nelements() > 0) {
		if (shape(i) != shapeColumn(0)) {
		    throw (TSMError
			     ("Mismatch in fixed shape of coordinate column "
			      + coordColSet_p[i]->columnName()));
		}
	    }
	}
    }
}

void TiledStMan::checkCoordinatesShapes (const TSMCube* hypercube,
					 const IPosition& cubeShape) const
{
    //# Check for all coordinates if their length (if defined)
    //# matches the hypercube shape.
    for (uint32_t i=0; i<nrCoordVector_p; i++) {
	if (coordColSet_p[i] != 0) {
	    int32_t size = hypercube->coordinateSize
		                            (coordColSet_p[i]->columnName());
	    if (size != 0  &&  size != cubeShape(i)) {
		throw (TSMError ("Mismatch in shape of coordinate column "
				 + coordColSet_p[i]->columnName()));
	    }
	}
    }
}


void TiledStMan::initCoordinates (TSMCube* hypercube)
{
    for (uint32_t i=0; i<coordColSet_p.nelements(); i++) {
	if (coordColSet_p[i] != 0) {
	    hypercube->extendCoordinates (Record(),
					  coordColSet_p[i]->columnName(),
					  hypercube->cubeShape()(i));
	    dataChanged_p = true;
	}
    }
}


uint32_t TiledStMan::getBindings (const Vector<String>& columnNames,
			      PtrBlock<TSMColumn*>& colSet,
			      bool mustExist) const
{
    colSet = static_cast<TSMColumn*>(0);
    uint32_t nrfound = 0;
    uint32_t j;
    bool found = false;
    for (uint32_t i=0; i<columnNames.nelements(); i++) {
	for (j=0; j<ncolumn(); j++) {
	    if (columnNames(i) == colSet_p[j]->columnName()) {
		colSet[i] = colSet_p[j];
		found = true;
		nrfound++;
		break;
	    }
	}
	if (!found  &&  mustExist) {
	    throw (TSMError ("TiledStMan column " + columnNames(i) +
			     " is not bound"));
	}
    }
    return nrfound;
}


void TiledStMan::checkAddHypercube (const IPosition& cubeShape,
				    const Record& values) const
{
    //# Check if the cube shape is correct.
    checkCubeShape (0, cubeShape);
    // Check whether all id and coordinate values are given correctly.
    checkValues (idColSet_p, values);
    checkCoordinates (coordColSet_p, cubeShape, values);
    // Check whether no double id values are given.
    if (getCubeIndex (values) >= 0) {
	throw (TSMError ("addHypercube with already existing id values in " +
                         hypercolumnName_p));
    }
}

TSMCube* TiledStMan::makeHypercube (const IPosition& cubeShape,
				    const IPosition& tileShape,
				    const Record& values)
{
    dataChanged_p = true;
    // Pick a TSMFile object for the hypercube.
    // Non-extensible cubes share the first file; others get their own file.
    uint32_t filenr = 0;
    if (cubeShape(nrdim_p - 1) == 0) {
	filenr = fileSet_p.nelements();
	fileSet_p.resize (filenr + 1);
	fileSet_p[filenr] = 0;
    }
    // Create the file when needed.
    if (fileSet_p[filenr] == 0) {
	createFile (filenr);
    }
    // Create a TSMCube object.
    // Its data will be written at the end of the file.
    return makeTSMCube (fileSet_p[filenr], cubeShape, tileShape, values);
}

void TiledStMan::createFile (uint32_t index)
{
    TSMFile* file = new TSMFile (this, index, tsmOption(), multiFile());
    fileSet_p[index] = file;
}


int32_t TiledStMan::getCubeIndex (const Record& idValues) const
{
    // When there are no id columns, return the one and single hypercube
    // (or -1 if no one created yet).
    if (idColSet_p.nelements() == 0) {
	if (cubeSet_p.nelements() == 0) {
	    return -1;
	}
	return 0;
    }
    // Look if a hypercube matches the id values.
    for (uint32_t i=0; i<cubeSet_p.nelements(); i++) {
	if (cubeSet_p[i] != 0) {
	    if (cubeSet_p[i]->matches (idColSet_p, idValues)) {
		return i;
	    }
	}
    }
    return -1;
}


void TiledStMan::checkValues (const PtrBlock<TSMColumn*>& colSet,
			      const Record& values) const
{
    // Check if all values are given and if their data types match.
    for (uint32_t i=0; i<colSet.nelements(); i++) {
	if (colSet[i] != 0) {
	    const String& name = colSet[i]->columnName();
	    if (! values.isDefined (name)) {
		throw (TSMError ("No value given for column " + name));
	    }
	    if (values.dataType(name) != colSet[i]->dataType()) {
		throw (TSMError ("Data type mismatch for column " + name));
	    }
	}
    }
}

void TiledStMan::checkCoordinates (const PtrBlock<TSMColumn*>& coordColSet,
				   const IPosition& cubeShape,
				   const Record& values) const
{
    // Check if the coordinates data types and shapes are correct,
    // i.e. if the coordinates shapes match the hypercube shape.
    for (uint32_t i=0; i<coordColSet.nelements(); i++) {
	if (coordColSet[i] != 0) {
	    const String& name = coordColSet[i]->columnName();
	    if (values.isDefined (name)) {
		int dataType = arrayDataType (coordColSet[i]->dataType());
		if (values.dataType(name) != dataType) {
		    throw (TSMError ("Data type mismatch for coordinate " +
				     name));
		}
		IPosition shape = values.shape (name);
		if (shape.nelements() != 1) {
		    throw (TSMError ("Values of coordinate " + name +
				     " do not form a vector"));
		}
		if (shape(0) != cubeShape(i)) {
		    throw (TSMError ("Shape mismatch for coordinate " + name));
		}
	    }
	}
    }
}


rownr_t TiledStMan::addedNrrow (const IPosition& shape, uint32_t incrInLastDim) const
{
    rownr_t nrrowAdded = 1;
    for (uint32_t i=nrCoordVector_p; i<nrdim_p-1; i++) {
	nrrowAdded *= shape(i);
    }
    return nrrowAdded * incrInLastDim;
}


rownr_t TiledStMan::open64 (rownr_t nrrow, AipsIO&)
{
    // Read the header info (for the first time).
    readHeader (nrrow, true);
    return nrrow;
}
rownr_t TiledStMan::resync64 (rownr_t nrrow)
{
    // Reread the header info.
    readHeader (nrrow, false);
    return nrrow;
}

bool TiledStMan::flushCaches (bool fsync)
{
    if (!dataChanged_p) {
	return false;
    }
    dataChanged_p = false;
    uint32_t i;
    for (i=0; i<cubeSet_p.nelements(); i++) {
	if (cubeSet_p[i] != 0) {
	    cubeSet_p[i]->flushCache();
	}
    }
    if (fsync) {
	for (i=0; i<fileSet_p.nelements(); i++) {
	    if (fileSet_p[i] != 0) {
		fileSet_p[i]->bucketFile()->fsync();
	    }
	}
    }
    return true;
}


AipsIO* TiledStMan::headerFileCreate()
{
    return new AipsIO (fileName(), ByteIO::New, 16384, multiFile());
}

AipsIO* TiledStMan::headerFileOpen()
{
    return new AipsIO (fileName(), ByteIO::Old, 16384, multiFile());
}


void TiledStMan::headerFilePut (AipsIO& headerFile, uint64_t nrCube)
{
    // The endian switch is a new feature. So only put it if little endian
    // is used. In that way older software can read newer tables.
    // Similarly, use older version if number of rows less than maxUint.
    bool useNewVersion = false;
    if (nrrow_p > MAXROWNR32  ||
        persMaxCacheSize_p != uint32_t(persMaxCacheSize_p)) {
      headerFile.putstart ("TiledStMan", 3);
      headerFile << asBigEndian();
      useNewVersion = true;
    } else if (asBigEndian()) {
      headerFile.putstart ("TiledStMan", 1);
    } else {
      headerFile.putstart ("TiledStMan", 2);
      headerFile << asBigEndian();
    }
    //# Write StMan sequence number, the number of rows and columns,
    //# and the column data types.
    //# This is only done to check it when reading back.
    headerFile << sequenceNr();
    if (useNewVersion) {
      headerFile << nrrow_p;
    } else {
      headerFile << uint32_t(nrrow_p);
    }
    headerFile << ncolumn();
    for (uint32_t i=0; i<ncolumn(); i++) {
	headerFile << colSet_p[i]->dataType();
    }
    headerFile << hypercolumnName_p;
    if (useNewVersion) {
      headerFile << persMaxCacheSize_p;
    } else {
      headerFile << uint32_t(persMaxCacheSize_p);
    }
    headerFile << nrdim_p;
    // nrfile and nrcube can never exceed nrrow,
    // so it's safe to use uint32_t for old version.
    if (useNewVersion) {
      headerFile << uint64_t(fileSet_p.nelements());
    } else {
      headerFile << uint32_t(fileSet_p.nelements());
    }
    for (uint64_t i=0; i<fileSet_p.nelements(); i++) {
	if (fileSet_p[i] == 0) {
	    headerFile << false;
	}else{
	    headerFile << true;
	    fileSet_p[i]->putObject (headerFile);
	}
    }
    if (useNewVersion) {
      headerFile << nrCube;
    } else {
      headerFile << uint32_t(nrCube);
    }
    for (uint64_t i=0; i<nrCube; i++) {
	cubeSet_p[i]->putObject (headerFile);
    }
    headerFile.putend();
}

uint32_t TiledStMan::headerFileGet (AipsIO& headerFile, rownr_t tabNrrow,
				bool firstTime, int32_t extraNdim)
{
    nrrow_p = tabNrrow;
    uint32_t version = headerFile.getstart ("TiledStMan");
    bool bigEndian = true;
    if (version >= 2) {
        headerFile >> bigEndian;
    }
    if (bigEndian != asBigEndian()) {
        throw DataManError("Endian flag in TSM mismatches the table flag");
    }
    //# Get and check the number of rows and columns and the column types.
    rownr_t nrrow;
    uint32_t nrcol, seqnr;
    int  dtype;
    headerFile >> seqnr;
    if (version >= 3) {
        headerFile >> nrrow;
    } else {
        uint32_t nrrowOld;
        headerFile >> nrrowOld;
        nrrow = nrrowOld;
    }
    headerFile >> nrcol;
    if (seqnr != sequenceNr()  ||  nrcol != ncolumn()) {
      //# Temporary hack to fix a corrupted table.
      //#if (sequenceNr() != 7) {
	throw (DataManInternalError
	          ("TiledStMan::headerFileGet: mismatch in seqnr,#col"));
      //#}
    }
    if (nrrow != nrrow_p) {
#if defined(TABLEREPAIR)
        cerr << "TiledStMan::headerFileGet: mismatch in #row (expected "
	     << nrrow_p << ", found " << nrrow << ")" << endl;
	dataChanged_p = true;
#else
	throw (DataManInternalError
	          ("TiledStMan::headerFileGet: mismatch in #row; expected " +
		   String::toString(nrrow_p) + ", found " +
		   String::toString(nrrow)));
#endif
    }
    for (uint32_t i=0; i<ncolumn(); i++) {
	headerFile >> dtype;
	if (dtype != colSet_p[i]->dataType()) {
	    throw (DataManInternalError
		      ("TiledStMan::headerFileGet: mismatch in data type"));
	}
    }
    headerFile >> hypercolumnName_p;
    if (version >= 3) {
      headerFile >> persMaxCacheSize_p;
    } else {
      uint32_t tmp;
      headerFile >> tmp;
      persMaxCacheSize_p = tmp;
    }
    maxCacheSize_p = persMaxCacheSize_p;
    if (firstTime) {
	// Setup the various things (i.e. initialize other variables).
	setup (extraNdim);
    }
    uint32_t nrdim;
    headerFile >> nrdim;
    if (nrdim != nrdim_p) {
	throw (DataManInternalError
	              ("TiledStMan::headerFileGet: mismatch in nrdim"));
    }
    uint64_t nrFile;
    bool flag;
    if (version >= 3) {
      headerFile >> nrFile;
    } else {
      uint32_t nrFile32;
      headerFile >> nrFile32;
      nrFile = nrFile32;
    }
    uint64_t nrFileOld = fileSet_p.nelements();
    fileSet_p.resize (nrFile);
    for (uint64_t i=nrFileOld; i<nrFile; i++) {
	fileSet_p[i] = 0;
    }
    for (uint64_t i=0; i<nrFile; i++) {
	headerFile >> flag;
	if (flag) {
	    if (fileSet_p[i] == 0) {
              fileSet_p[i] = new TSMFile (this, headerFile, i, tsmOption(),
                                          multiFile());
	    }else{
		fileSet_p[i]->getObject (headerFile);
	    }
	}else{
	    delete fileSet_p[i];
	    fileSet_p[i] = 0;
	}
    }
    uint64_t nrCube;
    if (version >= 3) {
      headerFile >> nrCube;
    } else {
      uint32_t nrCube32;
      headerFile >> nrCube32;
      nrCube = nrCube32;
    }
    uint64_t nrCubeOld = cubeSet_p.nelements();
    cubeSet_p.resize (nrCube);
    for (uint64_t i=nrCubeOld; i<nrCube; i++) {
	cubeSet_p[i] = 0;
    }
    for (uint64_t i=0; i<nrCube; i++) {
	if (cubeSet_p[i] == 0) {
            if (tsmOption().option() == TSMOption::MMap) {
                //cout << "mmapping TSM" << endl;
                cubeSet_p[i] = new TSMCubeMMap (this, headerFile);
            } else if (tsmOption().option() == TSMOption::Buffer) {
                //cout << "buffered TSM" << endl;
                cubeSet_p[i] = new TSMCubeBuff (this, headerFile,
                                                tsmOption().bufferSize());
            }else{
                //cout << "caching TSM" << endl;
	        cubeSet_p[i] = new TSMCube (this, headerFile);
            }
	}else{
	    cubeSet_p[i]->resync (headerFile);
	}
    }
    headerFile.getend();
    //# The following can only be executed in case of TABLEREPAIR.
    if (nrrow < nrrow_p) {
      cubeSet_p[0]->extend (nrrow_p-nrrow, Record(),
			    coordColSet_p[nrdim_p - 1]);
    }
    return version;
}

void TiledStMan::headerFileClose (AipsIO* headerFile)
{
    delete headerFile;
}


TSMFile* TiledStMan::getFile (uint32_t sequenceNumber)
{
    //# Do internal check to see if TSMFile really exists.
    if (sequenceNumber >= fileSet_p.nelements()
    ||  fileSet_p[sequenceNumber] == 0) {
      throw (DataManInternalError ("TiledStMan::getFile in " +
                                   hypercolumnName_p));
    }
    return fileSet_p[sequenceNumber];
}

} //# NAMESPACE CASACORE - END

