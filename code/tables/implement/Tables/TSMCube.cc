//# TSMCube.cc: Tiled Hypercube Storage Manager for tables
//# Copyright (C) 1995,1996,1997,1998
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


//# Includes
#include <aips/aips.h>
#include <aips/Tables/TSMCube.h>
#include <aips/Tables/TiledStMan.h>
#include <aips/Tables/TSMFile.h>
#include <aips/Tables/TSMColumn.h>
#include <aips/Tables/BucketCache.h>
#include <aips/Tables/DataManError.h>
#include <aips/Arrays/ArrayUtil.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordField.h>
#include <aips/Containers/Block.h>
#include <aips/IO/AipsIO.h>
#include <aips/OS/Conversion.h>
#include <string.h>                           // for memcpy
#include <iostream.h>


TSMCube::TSMCube (TiledStMan* stman, TSMFile* file)
: stmanPtr_p     (stman),
  extensible_p   (False),
  nrdim_p        (0),
  tileSize_p     (0),
  filePtr_p      (file),
  fileOffset_p   (0),
  cache_p        (0),
  userSetCache_p (False)
{}


TSMCube::TSMCube (TiledStMan* stman, TSMFile* file,
                  const IPosition& cubeShape,
                  const IPosition& tileShape,
                  const Record& values)
: stmanPtr_p     (stman),
  values_p       (values),
  extensible_p   (ToBool (cubeShape(cubeShape.nelements()-1) == 0)),
  nrdim_p        (0),
  tileSize_p     (0),
  filePtr_p      (file),
  fileOffset_p   (0),
  cache_p        (0),
  userSetCache_p (False)
{
    setShape (cubeShape, tileShape);
}

TSMCube::TSMCube (TiledStMan* stman, AipsIO& ios)
: stmanPtr_p     (stman),
  filePtr_p      (0),
  cache_p        (0),
  userSetCache_p (False)
{
    Int fileSeqnr = getObject (ios);
    if (fileSeqnr >= 0) {
	filePtr_p = stmanPtr_p->getFile (fileSeqnr);
    }
    // Calculate the various variables.
    setup();
}

TSMCube::~TSMCube()
{
    delete cache_p;
}


void TSMCube::clearCache()
{
    if (cache_p != 0) {
        cache_p->clear();
    }
}
void TSMCube::emptyCache()
{
    if (cache_p != 0) {
        cache_p->resize (0);
    }
    userSetCache_p = False;
}

void TSMCube::showCacheStatistics (ostream& os) const
{
    if (cache_p != 0) {
        os << ">>> TSMCube cache statistics:" << endl;
        os << "cubeShape: " << cubeShape_p << endl;
        os << "tileShape: " << tileShape_p << endl;
        os << "maxCacheSz:" << stmanPtr_p->maximumCacheSize() << endl;
        cache_p->showStatistics (os);
        os << "<<<" << endl;
    }
}

uInt TSMCube::coordinateSize (const String& coordinateName) const
{
    if (! values_p.isDefined (coordinateName)) {
        return 0;
    }
    IPosition shape (values_p.shape (coordinateName));
    if (shape.nelements() == 0) {
	return 0;
    }
    return shape(0);
}


IPosition TSMCube::adjustTileShape (const IPosition& cubeShape,
                                    const IPosition& tileShape) const
{
    // Make this function independent of the length of tileShape,
    // so it can be shorter or longer than the cube shape.
    // The returned tile shape always has the length of the cube shape.
    IPosition tileShp (cubeShape.nelements(), 1);          // initialize to 1
    for (uInt i=0; i<cubeShape.nelements(); i++) {
        if (i < tileShape.nelements()) {
            if (tileShape(i) != 0) {
                tileShp(i) = tileShape(i);
            }
            if (cubeShape(i) != 0  &&  tileShp(i) > cubeShape(i)) {
                tileShp(i) = cubeShape(i);
            }
        }
    }
    return tileShp;
}


// setShape sets the shape of a hypercube.
// It is only used by TiledCellStMan columns, because the shape
// is always known for others.
void TSMCube::setShape (const IPosition& cubeShape, const IPosition& tileShape)
{
    // Check if the shape matches the shape of already known columns
    // (data columns and coordinate columns can be fixed shape or
    // coordinate columns can already be defined).
    stmanPtr_p->checkCubeShape (this, cubeShape);
    // When the shape is redefined, the cache may already exist.
    // So delete it first.
    delete cache_p;
    cache_p = 0;
    fileOffset_p = filePtr_p->length();
    nrdim_p      = cubeShape.nelements();
    cubeShape_p  = cubeShape;
    tileShape_p  = adjustTileShape (cubeShape, tileShape);
    // Calculate the various variables.
    setup();
    // Create the cache and extend the file.
    makeCache();
    filePtr_p->extend (nrTiles_p * bucketSize_p);
    // Initialize the coordinate columns (as far as needed).
    stmanPtr_p->initCoordinates (this);
    // Set flag if writing.
    stmanPtr_p->setDataChanged();
}


void TSMCube::putObject (AipsIO& ios)
{
    flushCache();
    ios << 1;                          // version 1
    ios << values_p;
    ios << extensible_p;
    ios << nrdim_p;
    ios << cubeShape_p;
    ios << tileShape_p;
    Int seqnr = -1;
    if (filePtr_p != 0) {
	seqnr = filePtr_p->sequenceNumber();
    }
    ios << seqnr;
    ios << fileOffset_p;
}
Int TSMCube::getObject (AipsIO& ios)
{
    uInt version;
    Int fileSeqnr;
    ios >> version;
    ios >> values_p;
    ios >> extensible_p;
    ios >> nrdim_p;
    ios >> cubeShape_p;
    ios >> tileShape_p;
    ios >> fileSeqnr;
    ios >> fileOffset_p;
    return fileSeqnr;
}


void TSMCube::setup()
{
    // Determine the nr of tiles in all but the last dimension.
    // This is needed when extending the last dimension.
    // Also determine the nr of tiles needed (total and per dimension).
    expandedTileShape_p = TSMShape (tileShape_p);
    tilesPerDim_p.resize (nrdim_p);
    nrTiles_p = 1;
    for (uInt i=0; i<nrdim_p; i++) {
        nrTilesSubCube_p = nrTiles_p;
        tilesPerDim_p(i) = (cubeShape_p(i) + tileShape_p(i) - 1)
                           / tileShape_p(i);
        nrTiles_p *= tilesPerDim_p(i);
    }
    expandedTilesPerDim_p = TSMShape (tilesPerDim_p);
    // Determine the bucket size for the cache.
    // Also determine the start offset for each data column
    // and the length of the tile when converted to local format..
    tileSize_p   = tileShape_p.product();
    bucketSize_p = stmanPtr_p->getLengthOffset (tileSize_p, externalOffset_p,
						localOffset_p,
						localTileLength_p);
}

void TSMCube::makeCache()
{
    // If there is no cache, make one with initially 1 slot.
    if (cache_p == 0) {
        cache_p = new BucketCache (filePtr_p->bucketFile(), fileOffset_p,
                                   bucketSize_p, nrTiles_p, 1, this,
                                   readCallBack, writeCallBack,
                                   initCallBack, deleteCallBack);
        if (cache_p == 0) {
            throw (AllocError ("TSMCube::TSMCube", 1));
        }
    }
}

void TSMCube::flushCache()
{
    if (cache_p != 0) {
	cache_p->flush();
    }
}

Bool TSMCube::isExtensible() const
{
    return extensible_p;
}


void TSMCube::extend (uInt nr, const Record& coordValues,
                      const TSMColumn* lastCoordColumn)
{
    if (!extensible_p) {
        throw (TSMError ("Hypercube is not extensible"));
    }
    uInt lastDim = nrdim_p - 1;
    uInt nrold = nrTiles_p;
    cubeShape_p(lastDim) += nr;
    tilesPerDim_p(lastDim) = (cubeShape_p(lastDim) + tileShape_p(lastDim) - 1)
                             / tileShape_p(lastDim);
    nrTiles_p = nrTilesSubCube_p * tilesPerDim_p(lastDim);
    getCache()->extend (nrTiles_p - nrold);
    filePtr_p->extend ((nrTiles_p - nrold) * bucketSize_p);
    // Update the last coordinate (if there).
    if (lastCoordColumn != 0) {
        extendCoordinates (coordValues, lastCoordColumn->columnName(),
                           cubeShape_p(lastDim));
    }
}

void TSMCube::extendCoordinates (const Record& coordValues,
                                 const String& name, uInt length)
{
    //# Determine if the coordinate field is already defined.
    Bool defined = values_p.isDefined (name);
    //# Determine the extension length of the coordinate vector.
    //# This is the given length (which is the entire cube axis)
    //# minus already defined coordinate length.
    uInt vectorLength = length;
    if (defined) {
        vectorLength -= coordinateSize (name);
    }
    //# Exit if no extend.
    if (vectorLength == 0) {
	return;
    }
    //# Determine the start and end of the new coordinate values.
    //# If they are not defined, start will be > end.
    IPosition start(1, length);
    IPosition end(1, length-1);
    if (coordValues.isDefined (name)) {
	IPosition shape = coordValues.shape (name);
	if (shape.nelements() > 0) {
	    start(0) -= shape(0);
	}
    }
    //# Now insert the new coordinate values.
    //# Note that the nr of new coordinate values can be less than
    //# the coordinate vector extension length.
    //# The algorithm is as follows:
    //# - Define coordinate values if not defined yet.
    //# - Extend the coordinate vector with default values.
    //# - Insert the new coordinate values.
    switch (stmanPtr_p->coordinateDataType (name)) {
    case TpBool:
    case TpArrayBool:
        {
            if (!defined) {
                values_p.define (name, Array<Bool>());
            }
            RecordFieldPtr<Array<Bool> > field (values_p, name);
            Array<Bool>& array = *field;
            if (vectorLength > 0) {
                Vector<Bool> vector(vectorLength);
                vector = False;
                Array<Bool> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArrayBool (name);
            }
        }
        break;
    case TpInt:
    case TpArrayInt:
        {
            if (!defined) {
                values_p.define (name, Array<Int>());
            }
            RecordFieldPtr<Array<Int> > field (values_p, name);
            Array<Int>& array = *field;
            if (vectorLength > 0) {
                Vector<Int> vector(vectorLength);
                vector = 0;
                Array<Int> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArrayInt (name);
            }
        }
        break;
    case TpUInt:
    case TpArrayUInt:
        {
            if (!defined) {
                values_p.define (name, Array<uInt>());
            }
            RecordFieldPtr<Array<uInt> > field (values_p, name);
            Array<uInt>& array = *field;
            if (vectorLength > 0) {
                Vector<uInt> vector(vectorLength);
                vector = 0;
                Array<uInt> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArrayuInt (name);
            }
        }
        break;
    case TpFloat:
    case TpArrayFloat:
        {
            if (!defined) {
                values_p.define (name, Array<float>());
            }
            RecordFieldPtr<Array<float> > field (values_p, name);
            Array<float>& array = *field;
            if (vectorLength > 0) {
                Vector<float> vector(vectorLength);
                vector = 0;
                Array<float> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArrayfloat (name);
            }
        }
        break;
    case TpDouble:
    case TpArrayDouble:
        {
            if (!defined) {
                values_p.define (name, Array<double>());
            }
            RecordFieldPtr<Array<double> > field (values_p, name);
            Array<double>& array = *field;
            if (vectorLength > 0) {
                Vector<double> vector(vectorLength);
                vector = 0;
                Array<double> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArraydouble (name);
            }
        }
        break;
    case TpComplex:
    case TpArrayComplex:
        {
            if (!defined) {
                values_p.define (name, Array<Complex>());
            }
            RecordFieldPtr<Array<Complex> > field (values_p, name);
            Array<Complex>& array = *field;
            if (vectorLength > 0) {
                Vector<Complex> vector(vectorLength);
                vector = Complex(0);
                Array<Complex> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArrayComplex (name);
            }
        }
        break;
    case TpDComplex:
    case TpArrayDComplex:
        {
            if (!defined) {
                values_p.define (name, Array<DComplex>());
            }
            RecordFieldPtr<Array<DComplex> > field (values_p, name);
            Array<DComplex>& array = *field;
            if (vectorLength > 0) {
                Vector<DComplex> vector(vectorLength);
                vector = DComplex(0);
                Array<DComplex> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArrayDComplex (name);
            }
        }
        break;
    case TpString:
    case TpArrayString:
        {
            if (!defined) {
                values_p.define (name, Array<String>());
            }
            RecordFieldPtr<Array<String> > field (values_p, name);
            Array<String>& array = *field;
            if (vectorLength > 0) {
                Vector<String> vector(vectorLength);
                vector = "";
                Array<String> newArray (concatenateArray (array, vector.ac()));
                array.reference (newArray);
            }
            if (start(0) < Int(length)) {
                array(start, end) = coordValues.asArrayString (name);
            }
        }
        break;
    default:
        throw (DataManInvDT ("extendCoordinates"));
    }
}

Bool TSMCube::matches (const PtrBlock<TSMColumn*> idColSet,
                       const Record& idValues)
{
    for (uInt i=0; i<idColSet.nelements(); i++) {
        const String& name = idColSet[i]->columnName();
        switch (values_p.dataType (name)) {
        case TpBool:
            if (idValues.asBool (name) != values_p.asBool (name)) {
                return False;
            }
            break;
        case TpString:
            if (idValues.asString (name) != values_p.asString (name)) {
                return False;
            }
            break;
        case TpComplex:
        case TpDComplex:
	    {
	        const DComplex& idVal = idValues.asDComplex (name);
	        const DComplex& val = values_p.asDComplex (name);
                if (idVal != val) {
	            return False;
                }
	    }
	    break;
        default:
            if (idValues.asdouble (name) != values_p.asdouble (name)) {
                return False;
            }
            break;
        }
    }
    return True;
}


char* TSMCube::readCallBack (void* owner, const char* external)
{
    return ((TSMCube*)owner)->readTile (external);
}
char* TSMCube::readTile (const char* external)
{
    char* local = new char[localTileLength_p];
    if (local == 0) {
        throw (AllocError ("TSMCube::readCallBack", localTileLength_p));
    }
    stmanPtr_p->readTile (local, localOffset_p, external, externalOffset_p,
			  tileSize_p);
    return local;
}
void TSMCube::writeCallBack (void* owner, char* external, const char* local)
{
    ((TSMCube*)owner)->writeTile (external, local);
}
void TSMCube::writeTile (char* external, const char* local)
{
    stmanPtr_p->writeTile (external, externalOffset_p, local, localOffset_p,
			   tileSize_p);
}
void TSMCube::deleteCallBack (void*, char* buffer)
{
    delete [] buffer;
}
char* TSMCube::initCallBack (void* owner)
{
    uInt size = ((TSMCube*)owner)->localTileLength();
    char* buffer = new char[size];
    if (buffer == 0) {
        throw (AllocError ("TSMCube::initCallBack", size));
    }
    for (uInt i=0; i<size; i++) {
        buffer[i] = 0;
    }
    return buffer;
}

uInt TSMCube::cacheSize() const
{
    if (cache_p == 0) {
	return 0;
    }
    return cache_p->cacheSize();
}

uInt TSMCube::validateCacheSize (uInt cacheSize) const
{
    // An overdraft of 10% is allowed.
    uInt maxSize = stmanPtr_p->maximumCacheSize();
    if (maxSize > 0  &&  cacheSize * bucketSize_p > maxSize) {
        uInt size = maxSize / bucketSize_p;
        if (10 * cacheSize  >  11 * size) {
            return size;
        }
    }
    return cacheSize;
}

void TSMCube::setCacheSize (uInt cacheSize, Bool forceSmaller, Bool userSet)
{
    // Resize the cache in the expectation that this access is
    // the first of a bunch of accesses at the same tiles.
    // However, don't let the cache exceed the maximum,
    // unless it is only 10% more.
    BucketCache* cachePtr = getCache();
    cacheSize = validateCacheSize (cacheSize);
    if (forceSmaller  ||  cacheSize > cachePtr->cacheSize()) {
        cachePtr->resize (cacheSize);
    }
////    cout << "cachesize=" << cacheSize << endl;
    userSetCache_p = userSet;
}

// Set the cache size for the given slice and access path.
void TSMCube::setCacheSize (const IPosition& sliceShape,
                            const IPosition& windowStart,
                            const IPosition& windowLength,
                            const IPosition& axisPath,
			    Bool forceSmaller, Bool userSet)
{
    setCacheSize (calcCacheSize (sliceShape, windowStart,
				 windowLength, axisPath),
		  forceSmaller, userSet);
}

// Calculate the cache size for the given slice and access path.
uInt TSMCube::calcCacheSize (const IPosition& sliceShape,
			     const IPosition& windowStart,
			     const IPosition& windowLength,
			     const IPosition& axisPath) const
{
    if (sliceShape.nelements() > nrdim_p
    ||  windowStart.nelements() > nrdim_p
    ||  windowLength.nelements() > nrdim_p
    ||  axisPath.nelements() > nrdim_p) {
        throw (TSMError ("calcCacheSize: invalid arguments"));
    }
    uInt i;
    // The unspecified sliceShape dimensions are 1.
    IPosition slice(nrdim_p, 1);
    for (i=0; i<sliceShape.nelements(); i++) {
	if (sliceShape(i) > 0) {
	    slice(i) = sliceShape(i);
	}
    }
    // The unspecified window start dimensions are 0.
    IPosition start(nrdim_p, 0);
    for (i=0; i<windowStart.nelements(); i++) {
        start(i) = min (windowStart(i), cubeShape_p(i)-1);
    }
    // The unspecified window length elements are set to the hypercube shape.
    IPosition end(cubeShape_p);
    for (i=0; i<windowLength.nelements(); i++) {
        end(i) = min (windowLength(i), cubeShape_p(i) - start(i));
    }
    end += start;
    // When extensible, set end to the end of the start tile.
    // Otherwise all reused values may get 0 in extensible hypercubes.
    if (extensible_p) {
	i = nrdim_p - 1;
	end(i) += tileShape_p(i) * (1 + start(i) / tileShape_p(i));
    }
    end -= 1;
    // Make the full axes path.
    IPosition path = IPosition::makeAxisPath (nrdim_p, axisPath);
    // Determine per dimension the number of tiles needed for the window.
    // Determine per dimension how many tiles are needed for a slice.
    // Determine per dimension how often a tile will be reused.
    IPosition ntiles(nrdim_p);
    IPosition reused(nrdim_p);
    IPosition sliceTiles(nrdim_p);
    for (i=0; i<nrdim_p; i++) {
        uInt axis = path(i);
        uInt startTile = start(axis) / tileShape_p(axis);
        uInt endTile   = end(axis) / tileShape_p(axis);
        ntiles(i) = 1 + endTile - startTile;
	// Get start pixel in first tile.
	Int st = start(axis) % tileShape_p(axis);
	// Nr of tiles needed for a slice (note that start plays a role).
	sliceTiles(i) = 1 + (st + slice(axis) - 1) / tileShape_p(axis);
        reused(i) = 0;
	// Determine if a tile is reused in an iteration.
	// It can only be reused when the slice is smaller than the window.
        if (slice(axis) < 1 + end(axis) - start(axis)) {
	    // It is reused when the slice is smaller than the tile.
	    // or when slice or start do not fit integrally in tile.
	    if (slice(axis) < tileShape_p(axis)
	    ||  st != 0
	    ||  slice(axis) % tileShape_p(axis) != 0) {
		reused(i) = 1;
            }
	}
    }
    // Try to cache as much as possible. When the maximum cache size
    // is exceeded, try it for one dimension less.
    // Determine the optimum cache size taking the maximum cache
    // size into account. This is done by starting at the highest
    // dimension and working our way down until the cache size fits.
    uInt nrd = nrdim_p;
    while (nrd > 0) {
        nrd--;
        // Caching is needed when lower dimensions are reused because
        // a higher dimension loops through a tile.
        // So skip dimensions until a reused dimension is found.
	uInt nr = nrd;
        while (nr > 0  &&  reused(nr) == 0) {
            nr--;
        }
	// The cache needs to contain the tiles needed for the entire window
	// of the remaining axes.
	// When a tile is reused, we also need to take into account
	// the number of tiles needed for the slice.
        uInt cacheSize = 1;
        for (i=0; i<nr; i++) {
            cacheSize *= ntiles(i);
        }
	if (reused(nr) > 0) {
	    for (i=nr+1; i<=nrd; i++) {
		cacheSize *= sliceTiles(i);
	    }
	}
        if (cacheSize == validateCacheSize (cacheSize)) {
	    return cacheSize;
        }
	nrd = nr;
    }
    return 1;
}

void TSMCube::accessSection (const IPosition& start, const IPosition& end,
                             char* section, uInt colnr,
                             uInt localPixelSize, Bool writeFlag)
{
    // Set flag if writing.
    if (writeFlag) {
	stmanPtr_p->setDataChanged();
    }
    // Prepare for the iteration through the necessary tiles.
    IPosition nrTiles (nrdim_p);               // #tiles needed for the section
    IPosition startTile (nrdim_p);             // first tile needed
    IPosition endTile (nrdim_p);               // last tile needed
    IPosition startPixelInFirstTile (nrdim_p); // first pixel in first tile
    IPosition endPixelInFirstTile (nrdim_p);   // last pixel in first tile
    IPosition endPixelInLastTile (nrdim_p);    // last pixel in last tile
    uInt i, j;

    // Initialize the various variables and determine the number of
    // tiles needed (which will determine the cache size).
    // Also determine if the slice happens to be an entire slice
    // or if it is a line (this cases occur quite often and can be
    // handled in a more optimal way).
    Bool oneEntireTile = True;
    uInt lineIndex = 0;
    uInt nOneLong = 0;
    for (i=0; i<nrdim_p; i++) {
        startTile(i) = start(i) / tileShape_p(i);
        endTile(i)   = end(i) / tileShape_p(i);
        nrTiles(i)   = 1 + endTile(i) - startTile(i);
        startPixelInFirstTile(i) = start(i) - startTile(i) * tileShape_p(i);
        endPixelInLastTile(i)    = end(i) - endTile(i) * tileShape_p(i);
        endPixelInFirstTile(i)   = tileShape_p(i) - 1;
        if (nrTiles(i) == 1) {
            endPixelInFirstTile(i) = endPixelInLastTile(i);
            if (startPixelInFirstTile(i) != 0
            ||  endPixelInFirstTile(i) != tileShape_p(i) - 1) {
                oneEntireTile = False;
            }
        }else{
            oneEntireTile = False;
        }
        if (start(i) == end(i)) {
            nOneLong++;
        }else{
            lineIndex = i;
        }
    }
    // Get the cache.
    BucketCache* cachePtr = getCache();
    
//    cout << "nrTiles=" << nrTiles << endl;
//    cout << "startTile=" << startTile << endl;
//    cout << "endTile=" << endTile << endl;
//    cout << "startPixelInFirstTile" << startPixelInFirstTile << endl;
//    cout << "endPixelInFirstTile" << endPixelInFirstTile << endl;
//    cout << "endPixelInLastTile" << endPixelInLastTile << endl;

    // A tile can contain more than one data array.
    // Each array is contiguous, so the first pixel of an array
    // starts after the other arrays.
    uInt pixelOffset = localOffset_p[colnr];

    // When the section matches the tile shape, we can simply
    // copy all values and do not have to do difficult iterations.
    if (oneEntireTile) {
        // Get the tile from the cache.
        uInt tileNr = expandedTilesPerDim_p.offset (startTile);
        char* dataArray = cachePtr->getBucket (tileNr);
        // When writing, set cache slot to dirty.
        if (writeFlag) {
            memcpy (dataArray+pixelOffset, section,
		    tileSize_p * localPixelSize);
            cachePtr->setDirty();
        }else{
            memcpy (section, dataArray+pixelOffset,
		    tileSize_p * localPixelSize);
        }
        return;
    }

    // When the section is a line, call a specialized function.
    // Note that a single pixel is also handled as a line.
    if (nOneLong >= nrdim_p - 1) {
        accessLine (section, pixelOffset, localPixelSize,
                    writeFlag, cachePtr,
                    startTile, endTile(lineIndex),
                    startPixelInFirstTile, endPixelInLastTile(lineIndex),
                    lineIndex);
        return;
    }

    // At this point we start looping through all tiles.
    // startPixel and endPixel will contain the first and last pixels
    // needed in the current tile.
    // tilePos contains the position of the current tile.
    IPosition startSection (start);            // start of section in cube
    IPosition sectionShape (end - start + 1);  // section shape
    TSMShape expandedSectionShape (sectionShape);
    IPosition startPixel (startPixelInFirstTile);
    IPosition endPixel   (endPixelInFirstTile);
    IPosition tilePos    (startTile);
    IPosition tileIncr = expandedTilesPerDim_p.offsetIncrement (nrTiles);
    IPosition dataLength(nrdim_p);
    IPosition dataPos   (nrdim_p);
    IPosition sectionPos(nrdim_p);
    uInt dataOffset;
    uInt sectionOffset;
    uInt tileNr = expandedTilesPerDim_p.offset (tilePos);

    while (True) {
//      cout << "tilePos=" << tilePos << endl;
//      cout << "tileNr=" << tileNr << endl;
//      cout << "start=" << startPixel << endl;
//      cout << "end=" << endPixel << endl;
        // Get the tile from the cache.
        // Set it to dirty when we are writing.
        char* dataArray = cachePtr->getBucket (tileNr);
        if (writeFlag) {
            cachePtr->setDirty();
        }

        // At this point we start looping through all pixels in the tile.
        // We do a vector at a time.
        // Calculate the start and end pixel in the tile.
        // Initialize the pixel position in the data and section.
        for (i=0; i<nrdim_p; i++) {
            dataLength(i) = 1 + endPixel(i) - startPixel(i);
            dataPos(i)    = startPixel(i);
            sectionPos(i) = tilePos(i) * tileShape_p(i)
                            + startPixel(i) - startSection(i);
        }
        dataOffset = pixelOffset + localPixelSize *
                            expandedTileShape_p.offset (startPixel);
        sectionOffset = localPixelSize *
                            expandedSectionShape.offset (sectionPos);
        IPosition dataIncr    = localPixelSize *
                            expandedTileShape_p.offsetIncrement (dataLength);
        IPosition sectionIncr = localPixelSize *
                            expandedSectionShape.offsetIncrement (dataLength);
        uInt localSize    = dataLength(0) * localPixelSize;
        while (True) {
            if (writeFlag) {
                memcpy (dataArray+dataOffset, section+sectionOffset,
			localSize);
            }else{
                memcpy (section+sectionOffset, dataArray+dataOffset,
			localSize);
            }
            dataOffset    += localSize;
            sectionOffset += localSize;
            for (j=1; j<nrdim_p; j++) {
                dataOffset    += dataIncr(j);
                sectionOffset += sectionIncr(j);
                if (++dataPos(j) <= endPixel(j)) {
                    break;
                }
                dataPos(j) = startPixel(j);
            }
            if (j == nrdim_p) {
                break;
            }
        }

        // Determine the next tile to access and the starting and
        // ending pixels in it.
        // We increase the tile position in a dimension.
        for (i=0; i<nrdim_p; i++) {
            tileNr += tileIncr(i);
            startPixel(i) = 0;
            if (++tilePos(i) < endTile(i)) {
                break;                                 // not at last tile
            }
            if (tilePos(i) == endTile(i)) {
                endPixel(i) = endPixelInLastTile(i);   // last tile
                break;
            }
            // Past last tile in this dimension.
            // Reset start and end.
            tilePos(i) = startTile(i);
            startPixel(i) = startPixelInFirstTile(i);
            endPixel(i)   = endPixelInFirstTile(i);
        }
        if (i == nrdim_p) {
            break;                                     // ready
        }
    }
}

void TSMCube::accessLine (char* section, uInt pixelOffset,
                          uInt localPixelSize,
                          Bool writeFlag, BucketCache* cachePtr,
                          const IPosition& startTile, uInt endTile,
                          const IPosition& startPixelInFirstTile,
                          uInt endPixelInLastTile,
                          uInt lineIndex)
{
    // Get the stride to get to the next tile.
    uInt tileIncr = expandedTilesPerDim_p(lineIndex);
    uInt tileNr = expandedTilesPerDim_p.offset (startTile);
    uInt stTile = startTile(lineIndex);
    // Get the stride to get to the next pixel in a tile.
    uInt stride = expandedTileShape_p(lineIndex) * localPixelSize;
    Bool contiguous = ToBool(stride == localPixelSize);
    // Calculate the absolute pixel offset in the first tile
    // and in the other tiles.
    uInt offset = pixelOffset + localPixelSize *
                           expandedTileShape_p.offset (startPixelInFirstTile);
    uInt offsetInOtherTile = offset - startPixelInFirstTile(lineIndex) *stride;
    uInt nrPixel = tileShape_p(lineIndex) - startPixelInFirstTile(lineIndex);
    // Loop through all tiles.
    while (stTile <= endTile) {
        if (stTile == endTile) {
            nrPixel -= tileShape_p(lineIndex) - endPixelInLastTile - 1;
        }
//      cout << "tilePos=" << startTile << endl;
//      cout << "tileNr=" << tileNr << endl;
//      cout << "start=" << startPixel << endl;
//      cout << "nrpixel=" << nrPixel << endl;
        // Get the tile from the cache.
        // Set it to dirty when we are writing.
        char* dataArray = cachePtr->getBucket (tileNr) + offset;
        if (writeFlag) {
            cachePtr->setDirty();
        }
        // Copy the data. When contiguous we can copy directly.
        // Otherwise loop through all pixels.
        if (contiguous) {
            if (writeFlag) {
                memcpy (dataArray, section, nrPixel * localPixelSize);
            }else{
		memcpy (section, dataArray, nrPixel * localPixelSize);
            }
            section += nrPixel * localPixelSize;
        }else{
            // Try to make the data copy as fast as possible.
            // Do this by specializing the cases (which occur very often)
            // where no conversion is needed.
            Bool convert = False;
            if (writeFlag) {
                if (!convert) {
                    switch (localPixelSize) {
                    case (sizeof(char)):
                        while (nrPixel > 0) {
                            *dataArray = *section++;
                            dataArray += stride;
                            nrPixel--;
                        }
                        break;
                    case (sizeof(short)):
                        {
                            short* sect = (short*)section;
                            while (nrPixel > 0) {
                                *(short*)dataArray = *sect++;
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    case (sizeof(Int)):
                        {
                            Int* sect = (Int*)section;
                            while (nrPixel > 0) {
                                *(Int*)dataArray = *sect++;
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    case (sizeof(double)):
                        {
                            double* sect = (double*)section;
                            while (nrPixel > 0) {
                                *(double*)dataArray = *sect++;
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    case (2*sizeof(double)):
                        {
                            double* sect = (double*)section;
                            while (nrPixel > 0) {
                                *(double*)dataArray = *sect++;
                                ((double*)dataArray)[1] = *sect++;
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    default:
                        convert = True;
                    }
                }
                if (convert) {
                    while (nrPixel > 0) {
                        memcpy (dataArray, section, localPixelSize);
                        dataArray += stride;
                        section   += localPixelSize;
                        nrPixel--;
                    }
                }
            }else{
                if (!convert) {
                    switch (localPixelSize) {
                    case (sizeof(char)):
                        while (nrPixel > 0) {
                            *section++ = *dataArray;
                            dataArray += stride;
                            nrPixel--;
                        }
                        break;
                    case (sizeof(short)):
                        {
                            short* sect = (short*)section;
                            while (nrPixel > 0) {
                                *sect++ = *(short*)dataArray;
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    case (sizeof(Int)):
                        {
                            Int* sect = (Int*)section;
                            while (nrPixel > 0) {
                                *sect++ = *(Int*)dataArray;
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    case (sizeof(double)):
                        {
                            double* sect = (double*)section;
                            while (nrPixel > 0) {
                                *sect++ = *(double*)dataArray;
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    case (2*sizeof(double)):
                        {
                            double* sect = (double*)section;
                            while (nrPixel > 0) {
                                *sect++ = *(double*)dataArray;
                                *sect++ = ((double*)dataArray)[1];
                                dataArray += stride;
                                nrPixel--;
                            }
                            section = (char*)sect;
                        }
                        break;
                    default:
                        convert = True;
                    }
                }
                if (convert) {
                    while (nrPixel > 0) {
                        memcpy (dataArray, section, localPixelSize);
                        dataArray += stride;
                        section   += localPixelSize;
                        nrPixel--;
                    }
                }
            }
        }
        offset  = offsetInOtherTile;
        nrPixel = tileShape_p(lineIndex);
        tileNr += tileIncr;
        stTile++;
    }
}


void TSMCube::accessStrided (const IPosition& start, const IPosition& end,
                             const IPosition& stride,
                             char* section, uInt colnr,
                             uInt localPixelSize, Bool writeFlag)
{
    // Set flag if writing.
    if (writeFlag) {
	stmanPtr_p->setDataChanged();
    }
    // If all strides are 1, use accessSection.
    uInt i, j;
    Bool contiguous = True;
    i = 0;
    while (contiguous  &&  i < nrdim_p) {
        if (stride(i++) != 1) {
            contiguous = False;
        }
    }
    if (contiguous) {
        accessSection (start, end, section, colnr, localPixelSize, writeFlag);
        return;
    }
    // Get the cache (when needed).
    BucketCache* cachePtr = getCache();

    // A tile can contain more than one data array.
    // Each array is contiguous, so the first pixel of an array
    // starts after the other arrays.
    uInt pixelOffset = localOffset_p[colnr];
    // At this point we start looping through all tiles.
    // startPixel initially contains the first pixel in the first tile.
    // tilePos contains the position of the current tile.
    IPosition pixelPos (end + 1);              // pixel position
    IPosition sectionPos (nrdim_p, 0);         // #pixels processed in section
    IPosition nrPixel (nrdim_p, 0);            // #pixels processed last time
    IPosition tilePos (nrdim_p);               // tile position
    IPosition startPixel (nrdim_p);            // start pixel in tile
    IPosition endPixel (nrdim_p);              // end pixel in tile
    IPosition startSection (start);            // start of section in cube
    IPosition sectionShape (end - start + stride);  // section shape
    sectionShape /= stride;
    TSMShape expandedSectionShape (sectionShape);
    IPosition dataLength(nrdim_p);
    IPosition dataPos   (nrdim_p);
    uInt dataOffset;
    uInt sectionOffset;

    // Determine if the first dimension is strided.
    Bool strided = ToBool (stride(0) != 1);
    // The first time all dimensions are evaluated to set pixelStart/End
    // correctly.
    Bool firstTime = True;
    while (True) {
        // Determine the tile position from the pixel position.
        for (i=0; i<nrdim_p; i++) {
            sectionPos(i) += nrPixel(i);
            Bool nextDim = False;
            if (pixelPos(i) > end(i)) {
                pixelPos(i)   = start(i);
                sectionPos(i) = 0;
                nextDim = True;              // also evaluate next dimension
            }
            tilePos(i) = pixelPos(i) / tileShape_p(i);
            startPixel(i) = pixelPos(i) - tilePos(i) * tileShape_p(i);
            uInt leng = (tileShape_p(i) - startPixel(i) + stride(i) - 1)
                        / stride(i);
            if (Int(leng + sectionPos(i)) > sectionShape(i)) {
                leng = sectionShape(i) - sectionPos(i);
            }
            nrPixel(i) = leng;
            leng *= stride(i);
            pixelPos(i) = pixelPos(i) + leng;
            endPixel(i) = startPixel(i) + leng - stride(i);
            if (!nextDim) {
                break;
            }
        }
        // Stop if not first time and if all dimensions are done.
        if (i == nrdim_p) {
            if (!firstTime) {
                break;
            }
            firstTime = False;
        }
        uInt tileNr = expandedTilesPerDim_p.offset (tilePos);
//      cout << "tilePos=" << tilePos << endl;
//      cout << "tileNr=" << tileNr << endl;
//      cout << "start=" << startPixel << endl;
        // Get the tile from the cache.
        // Set it to dirty when we are writing.
        char* dataArray = cachePtr->getBucket (tileNr);
        if (writeFlag) {
            cachePtr->setDirty();
        }

        // At this point we start looping through all pixels in the tile.
        // We do a vector at a time.
        // Calculate the start and end pixel in the tile.
        // Initialize the pixel position in the data and section.
        dataPos = startPixel;
        IPosition dataIncr    = localPixelSize *
                         expandedTileShape_p.offsetIncrement (nrPixel, stride);
        IPosition sectionIncr = localPixelSize *
                         expandedSectionShape.offsetIncrement (nrPixel);
        dataOffset = pixelOffset + localPixelSize *
                         expandedTileShape_p.offset (startPixel);
        sectionOffset = localPixelSize *
                         expandedSectionShape.offset (sectionPos);
        uInt strideSize = 0;
        uInt localSize  = nrPixel(0) * localPixelSize;
        if (strided) {
            strideSize = stride(0) * localPixelSize;
        }
        while (True) {
            if (strided) {
		uInt nrp = nrPixel(0);
                for (j=0; j<nrp; j++) {
                    if (writeFlag) {
                        memcpy (dataArray+dataOffset, section+sectionOffset,
				localPixelSize);
                    }else{
                        memcpy (section+sectionOffset, dataArray+dataOffset,
				localPixelSize);
                    }
                    dataOffset    += strideSize;
                    sectionOffset += localPixelSize;
                }
            }else{
                if (writeFlag) {
                    memcpy (dataArray+dataOffset, section+sectionOffset,
			    localSize);
                }else{
                    memcpy (section+sectionOffset, dataArray+dataOffset,
			    localSize);
                }
                dataOffset    += localSize;
                sectionOffset += localSize;
            }
            for (j=1; j<nrdim_p; j++) {
                dataOffset    += dataIncr(j);
                sectionOffset += sectionIncr(j);
                dataPos(j) += stride(j);
                if (dataPos(j) <= endPixel(j)) {
                    break;
                }
                dataPos(j) = startPixel(j);
            }
            if (j == nrdim_p) {
                break;
            }
        }
    }
}
