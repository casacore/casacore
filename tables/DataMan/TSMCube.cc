//# TSMCube.cc: Tiled Hypercube Storage Manager for tables
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TSMCube.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMFile.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/IO/BucketCache.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/string.h>                           // for memcpy
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// memcpy with constant argument is inlined
#define TSM_COPY(a, b, n) case n: memcpy(a, b, n); break
static void TSMCube_MoveData(char * a, char * b, int n)
{
    switch (n) {
        case 0: break;
        TSM_COPY(a, b, 1);
        TSM_COPY(a, b, 2);
        TSM_COPY(a, b, 3);
        TSM_COPY(a, b, 4);
        TSM_COPY(a, b, 5);
        TSM_COPY(a, b, 6);
        TSM_COPY(a, b, 7);
        TSM_COPY(a, b, 8);
        TSM_COPY(a, b, 12);
        TSM_COPY(a, b, 16);
        TSM_COPY(a, b, 20);
        TSM_COPY(a, b, 24);
        TSM_COPY(a, b, 28);
        TSM_COPY(a, b, 32);
        default: memcpy(a, b, n);
    }
}
#undef TSM_COPY



TSMCube::TSMCube (TiledStMan* stman, TSMFile* file,
                  const IPosition& cubeShape,
                  const IPosition& tileShape,
                  const Record& values,
                  int64_t fileOffset,
                  bool useDerived)
: cachedTile_p (0),
  stmanPtr_p     (stman),
  useDerived_p   (useDerived),
  values_p       (values),
  extensible_p   (false),
  nrdim_p        (0),
  nrTiles_p      (0),
  tileSize_p     (0),
  filePtr_p      (file),
  fileOffset_p   (0),
  cache_p        (0),
  userSetCache_p (false),
  lastColAccess_p(NoAccess)
{
    if (fileOffset < 0) {
        // TiledCellStMan uses an empty shape; setShape is called later. 
        if (! cubeShape.empty()) {
            // A shape is given, so set it.
            extensible_p = cubeShape(cubeShape.nelements()-1) == 0;
            setShape (cubeShape, tileShape);
        }
    } else {
        // Meant for TiledFileAccess.
        nrdim_p      = cubeShape.nelements();
        cubeShape_p  = cubeShape;
        tileShape_p  = tileShape;
        fileOffset_p = fileOffset;
        setup();
    }
}

TSMCube::TSMCube (TiledStMan* stman, AipsIO& ios,
                  bool useDerived)
: cachedTile_p (0),
  stmanPtr_p     (stman),
  useDerived_p   (useDerived),
  filePtr_p      (0),
  cache_p        (0),
  userSetCache_p (false),
  lastColAccess_p(NoAccess)
{
    int32_t fileSeqnr = getObject (ios);
    if (fileSeqnr >= 0) {
	filePtr_p = stmanPtr_p->getFile (fileSeqnr);
    }
    // Calculate the various variables.
    setup();
}

TSMCube::~TSMCube()
{
    delete cache_p;
    delete [] cachedTile_p;
}


void TSMCube::clearCache (bool doFlush)
{
    if (doFlush) {
        flushCache();
    }
    if (cache_p != 0) {
        cache_p->clear (0, false);
    }
}
void TSMCube::emptyCache()
{
    if (cache_p != 0) {
        cache_p->resize (0);
    }
    userSetCache_p = false;
    lastColAccess_p = NoAccess;
}

void TSMCube::showCacheStatistics (ostream& os) const
{
    if (cache_p != 0) {
        os << ">>> TSMCube cache statistics:" << endl;
        os << "cubeShape: " << cubeShape_p << endl;
        os << "tileShape: " << tileShape_p << endl;
        os << "maxCacheSz:" << stmanPtr_p->maximumCacheSize() << " MiB" << endl;
        cache_p->showStatistics (os);
        os << "<<<" << endl;
    }
}

uint32_t TSMCube::coordinateSize (const String& coordinateName) const
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

IPosition TSMCube::cellShape() const
{
    uint32_t nr = stmanPtr_p->nrCoordVector();
    if (nr < cubeShape_p.nelements()) {
        return cubeShape_p.getFirst (nr);
    }
    return cubeShape_p;
}


IPosition TSMCube::adjustTileShape (const IPosition& cubeShape,
                                    const IPosition& tileShape) const
{
    // Make this function independent of the length of tileShape,
    // so it can be shorter or longer than the cube shape.
    // The returned tile shape always has the length of the cube shape.
    uint32_t nrdim = cubeShape.nelements();
    // Make length of tile shape equal to length of cube shape.
    // Fill with 0 (meaning undefined tile axes).
    IPosition tileShp (nrdim, 0);
    IPosition cubeUnk (nrdim);
    uint32_t nrunk = 0;
    uint32_t length = 1;
    for (uint32_t i=0; i<nrdim; i++) {
        if (i < tileShape.nelements()) {
	    tileShp(i) = tileShape(i);
	}
      // Get the cube axes of all unknown tile axes.
      // Ignore an extendible cube axis (for the time being).
      if (tileShp(i) == 0) {
	  if (cubeShape(i) != 0) {
	      cubeUnk(nrunk++) = cubeShape(i);
	  }
      } else {
	  // Tile axis length cannot exceed cube axis length.
	  // Get total length of known tile axes.
	  if (tileShp(i) > cubeShape(i)  &&  cubeShape(i) != 0) {
	      tileShp(i) = cubeShape(i);
	  }
	  length *= tileShp(i);
      }
    }
    cubeUnk.resize (nrunk);
    // Calculate a default for the unknown tile axes.
    // Use the remainder of the 32768 for it.
    if (nrunk > 0) {
        float rem = 32768. / length;
	int32_t leng = max(1, int32_t(rem + 0.5));
	IPosition tileUnk = TiledStMan::makeTileShape (cubeUnk, 0.5, leng);
	length *= tileUnk.product();
	uint32_t j = 0;
	for (uint32_t i=0; i<nrdim; i++) {
	    if (tileShp(i) == 0  &&  j < nrunk) {
	        tileShp(i) = tileUnk(j++);
	    }
	}
    }
    // If the cube is extendible, calculate the last tile axis if needed.
    if (cubeShape(nrdim-1) == 0  &&  tileShp(nrdim-1) == 0) {
        float rem = 32768. / length;
        tileShp(nrdim-1) = max(1, int32_t(rem + 0.5));
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
    // If the shape is redefined, the cache may already exist.
    // So delete it first.
    deleteCache();
    fileOffset_p = filePtr_p->length();
    nrdim_p      = cubeShape.nelements();
    // Resize the tile section member variables used in accessSection()
    resizeTileSections();
    cubeShape_p  = cubeShape;
    tileShape_p  = adjustTileShape (cubeShape, tileShape);
    // Calculate the various variables.
    setup();
    // If used directly, create the cache.
    // It has to be done here, otherwise the file does not get extended if
    // no explicit put is done.
    if (!useDerived_p) {
      makeCache();
    }
    // Tell TSMFile that the file gets extended.
    filePtr_p->extend (nrTiles_p * bucketSize_p);
    // Initialize the coordinate columns (as far as needed).
    stmanPtr_p->initCoordinates (this);
    // Set flag if writing.
    stmanPtr_p->setDataChanged();
}


void TSMCube::putObject (AipsIO& ios)
{
    flushCache();
    // If the offset is small enough, write it as an old style file,
    // so older software can still read it.
    bool vers1 = (fileOffset_p < 2u*1024u*1024u*1024u);
    if (vers1) {
        ios << 1;                          // version 1
    } else {
        ios << 2;                          // version 2
    }
    ios << values_p;
    ios << extensible_p;
    ios << nrdim_p;
    ios << cubeShape_p;
    ios << tileShape_p;
    int32_t seqnr = -1;
    if (filePtr_p != 0) {
	seqnr = filePtr_p->sequenceNumber();
    }
    ios << seqnr;
    if (vers1) {
        ios << uint32_t(fileOffset_p);
    } else {
	ios << fileOffset_p;
    }
}
int32_t TSMCube::getObject (AipsIO& ios)
{
    uint32_t version;
    int32_t fileSeqnr;
    ios >> version;
    ios >> values_p;
    ios >> extensible_p;
    ios >> nrdim_p;
    ios >> cubeShape_p;
    ios >> tileShape_p;
    ios >> fileSeqnr;
    if (version == 1) {
        uint32_t offs;
	ios >> offs;
	fileOffset_p = offs;
    } else {
        ios >> fileOffset_p;
    }
    return fileSeqnr;
}

void TSMCube::resync (AipsIO& ios)
{
    getObject (ios);
    setupNrTiles();
    resyncCache();
}

void TSMCube::setup()
{
    // Determine the nr of tiles in all but the last dimension.
    // This is needed when extending the last dimension.
    // Also determine the nr of tiles needed (total and per dimension).
    setupNrTiles();
    expandedTileShape_p = TSMShape (tileShape_p);
    expandedTilesPerDim_p = TSMShape (tilesPerDim_p);
    // Determine the bucket size for the cache.
    // Also determine the start offset for each data column
    // and the length of the tile if converted to local format.
    tileSize_p   = tileShape_p.product();
    bucketSize_p = stmanPtr_p->getLengthOffset (tileSize_p, externalOffset_p,
						localOffset_p,
						localTileLength_p);

    // Resize IPosition member variables used in accessSection()
    resizeTileSections();
}

void TSMCube::setupNrTiles()
{
    // Determine the nr of tiles in all but the last dimension.
    // This is needed when extending the last dimension.
    // Also determine the nr of tiles needed (total and per dimension).
    tilesPerDim_p.resize (nrdim_p);
    nrTiles_p = 1;
    for (uint32_t i=0; i<nrdim_p; i++) {
        nrTilesSubCube_p = nrTiles_p;
        tilesPerDim_p(i) = (cubeShape_p(i) + tileShape_p(i) - 1)
                           / tileShape_p(i);
        nrTiles_p *= tilesPerDim_p(i);
    }
}

void TSMCube::makeCache()
{
    // If there is no cache, make one with initially 1 slot.
    if (cache_p == 0) {
        cache_p = new BucketCache (filePtr_p->bucketFile(), fileOffset_p,
                                   bucketSize_p, nrTiles_p, 1, this,
                                   readCallBack, writeCallBack,
                                   initCallBack, deleteCallBack);
    }
}

void TSMCube::flushCache()
{
    if (cache_p != 0) {
	cache_p->flush();
    }
}

void TSMCube::resyncCache()
{
    if (cache_p != 0) {
      cache_p->resync (nrTiles_p, 0, -1);
    }
}

void TSMCube::deleteCache()
{
    delete cache_p;
    cache_p = 0;
}


bool TSMCube::isExtensible() const
{
    return extensible_p;
}


void TSMCube::extend (uint64_t nr, const Record& coordValues,
                      const TSMColumn* lastCoordColumn)
{
    if (!extensible_p) {
      throw TSMError ("Hypercube in TSM " + stmanPtr_p->dataManagerName() +
                      " is not extensible");
    }
    // Make the cache here, otherwise nrTiles_p is too high.
    makeCache();
    uint32_t lastDim = nrdim_p - 1;
    uint32_t nrold = nrTiles_p;
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
                                 const String& name, uint32_t length)
{
    //# Determine if the coordinate field is already defined.
    bool defined = values_p.isDefined (name);
    //# Determine the extension length of the coordinate vector.
    //# This is the given length (which is the entire cube axis)
    //# minus already defined coordinate length.
    uint32_t vectorLength = length;
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
                values_p.define (name, Array<bool>());
            }
            RecordFieldPtr<Array<bool> > field (values_p, name);
            Array<bool>& array = *field;
            if (vectorLength > 0) {
                Vector<bool> vector(vectorLength);
                vector = false;
                Array<bool> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayBool (name);
            }
        }
        break;
    case TpInt:
    case TpArrayInt:
        {
            if (!defined) {
                values_p.define (name, Array<int32_t>());
            }
            RecordFieldPtr<Array<int32_t> > field (values_p, name);
            Array<int32_t>& array = *field;
            if (vectorLength > 0) {
                Vector<int32_t> vector(vectorLength);
                vector = 0;
                Array<int32_t> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayInt (name);
            }
        }
        break;
    case TpUInt:
    case TpArrayUInt:
        {
            if (!defined) {
                values_p.define (name, Array<uint32_t>());
            }
            RecordFieldPtr<Array<uint32_t> > field (values_p, name);
            Array<uint32_t>& array = *field;
            if (vectorLength > 0) {
                Vector<uint32_t> vector(vectorLength);
                vector = 0;
                Array<uint32_t> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayuInt (name);
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
                Array<float> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayFloat (name);
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
                Array<double> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayDouble (name);
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
                Array<Complex> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayComplex (name);
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
                Array<DComplex> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayDComplex (name);
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
                Array<String> newArray (concatenateArray (array, vector));
                array.reference (newArray);
            }
            if (start(0) < int32_t(length)) {
                array(start, end) = coordValues.toArrayString (name);
            }
        }
        break;
    default:
        throw DataManInvDT ("extendCoordinates in TSM " +
                            stmanPtr_p->dataManagerName());
    }
}

bool TSMCube::matches (const PtrBlock<TSMColumn*>& idColSet,
                       const Record& idValues)
{
    for (uint32_t i=0; i<idColSet.nelements(); i++) {
        const String& name = idColSet[i]->columnName();
        switch (values_p.dataType (name)) {
        case TpBool:
            if (idValues.asBool (name) != values_p.asBool (name)) {
                return false;
            }
            break;
        case TpString:
            if (idValues.asString (name) != values_p.asString (name)) {
                return false;
            }
            break;
        case TpComplex:
        case TpDComplex:
	    {
	        const DComplex& idVal = idValues.asDComplex (name);
	        const DComplex& val = values_p.asDComplex (name);
                if (idVal != val) {
	            return false;
                }
	    }
	    break;
        default:
            if (idValues.asdouble (name) != values_p.asdouble (name)) {
                return false;
            }
            break;
        }
    }
    return true;
}


char* TSMCube::readCallBack (void* owner, const char* external)
{
    return ((TSMCube*)owner)->readTile (external);
}
char* TSMCube::readTile (const char* external)
{
    char* local = 0;

    if (cachedTile_p != 0){
        local = cachedTile_p;
        cachedTile_p = 0;
    } else {
        local = new char[localTileLength_p];
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
void TSMCube::deleteCallBack (void* owner, char* buffer)
{
    TSMCube * tsmCube = ((TSMCube*)owner);
    if (tsmCube->cachedTile_p == 0){
        tsmCube->cachedTile_p = buffer;
    } else {
        delete [] buffer;
    }
}
char* TSMCube::initCallBack (void* owner)
{
    uint64_t size = ((TSMCube*)owner)->localTileLength();
    char* buffer = new char[size];
    memset(buffer, 0, size);
    return buffer;
}

uint32_t TSMCube::cacheSize() const
{
    if (cache_p == 0) {
	return 0;
    }
    return cache_p->cacheSize();
}

uint32_t TSMCube::validateCacheSize (uint32_t cacheSize) const
{
  return validateCacheSize (cacheSize, stmanPtr_p->maximumCacheSize(),
                            bucketSize_p);
}

uint32_t TSMCube::validateCacheSize (uint32_t cacheSize, uint32_t maxSizeMiB,
                                 uint32_t bucketSize)
{
    // An overdraft of 10% is allowed.
    uint32_t maxnb = std::max(1u, uint32_t(1024. * 1024. * maxSizeMiB / bucketSize));
    if (maxSizeMiB > 0  &&  cacheSize > maxnb) {
        if (10 * cacheSize  >  11 * maxnb) {
            return maxnb;
        }
    }
    return cacheSize;
}

void TSMCube::setCacheSize (uint32_t cacheSize, bool forceSmaller, bool userSet)
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
			    bool forceSmaller, bool userSet)
{
    uint32_t cacheSize = calcCacheSize (sliceShape, windowStart,
				    windowLength, axisPath);
    // If not userset, do not cache if more than 25% of the memory is needed.
    if (!userSet) {
      uint32_t maxSize = uint32_t(HostInfo::memoryTotal(true) * 1024.*0.25 /
                          bucketSize_p);
      if (cacheSize > maxSize) {
	cacheSize = 1;
      }
    }
    setCacheSize (cacheSize, forceSmaller, userSet);
}

// Calculate the cache size for the given slice and access path.
uint32_t TSMCube::calcCacheSize (const IPosition& sliceShape,
                             const IPosition& windowStart,
                             const IPosition& windowLength,
                             const IPosition& axisPath) const
{
    return calcCacheSize (cubeShape_p, tileShape_p, extensible_p,
                          sliceShape, windowStart, windowLength, axisPath,
                          stmanPtr_p->maximumCacheSize(), bucketSize_p);
}

uint32_t TSMCube::calcCacheSize (const IPosition& cubeShape,
                             const IPosition& tileShape,
                             bool extensible,
                             const IPosition& sliceShape,
                             const IPosition& windowStart,
                             const IPosition& windowLength,
                             const IPosition& axisPath,
                             uint32_t maxCacheSize, uint32_t bucketSize)
{
    uint32_t nrdim = cubeShape.nelements();
    if (sliceShape.nelements() > nrdim
    ||  windowStart.nelements() > nrdim
    ||  windowLength.nelements() > nrdim
    ||  axisPath.nelements() > nrdim) {
      throw TSMError ("calcCacheSize: invalid arguments");
    }
    uint32_t i;
    // The unspecified sliceShape dimensions are 1.
    IPosition slice(nrdim, 1);
    for (i=0; i<sliceShape.nelements(); i++) {
	if (sliceShape(i) > 0) {
	    slice(i) = sliceShape(i);
	}
    }
    // The unspecified window start dimensions are 0.
    IPosition start(nrdim, 0);
    for (i=0; i<windowStart.nelements(); i++) {
        start(i) = std::min (windowStart(i), cubeShape(i)-1);
    }
    // The unspecified window length elements are set to the hypercube shape.
    IPosition end(cubeShape);
    for (i=0; i<windowLength.nelements(); i++) {
        end(i) = std::min (windowLength(i), cubeShape(i) - start(i));
    }
    end += start;
    // If extensible, set end to the end of the tile.
    // Otherwise all reused values may get 0 for extensible hypercubes.
    if (extensible) {
	i = nrdim - 1;
	///old	end(i) += tileShape_p(i) * (1 + start(i) / tileShape_p(i));
	end(i) = tileShape(i) * (1 + (end(i) - 1) / tileShape(i));
    }
    end -= 1;
    // Make the full axes path.
    IPosition path = IPosition::makeAxisPath (nrdim, axisPath);
    // Determine per dimension the number of tiles needed for the window.
    // Determine per dimension how many tiles are needed for a slice.
    // Determine per dimension how often a tile will be reused.
    IPosition ntiles(nrdim);
    IPosition reused(nrdim);
    IPosition sliceTiles(nrdim);
    for (i=0; i<nrdim; i++) {
        uint32_t axis = path(i);
        uint32_t startTile = start(axis) / tileShape(axis);
        uint32_t endTile   = end(axis) / tileShape(axis);
        ntiles(i) = 1 + endTile - startTile;
	// Get start pixel in first tile.
	int32_t st = start(axis) % tileShape(axis);
	// Nr of tiles needed for a slice (note that start plays a role).
	sliceTiles(i) = 1 + (st + slice(axis) - 1) / tileShape(axis);
        reused(i) = 0;
	// Determine if a tile is reused in an iteration.
	// It can only be reused if the slice is smaller than the window.
        if (slice(axis) < 1 + end(axis) - start(axis)) {
	    // It is reused if the slice is smaller than the tile.
	    // or if slice or start do not fit integrally in tile.
	    if (slice(axis) < tileShape(axis)
	    ||  st != 0
	    ||  slice(axis) % tileShape(axis) != 0) {
		reused(i) = 1;
            }
	}
    }
    // Try to cache as much as possible. If the maximum cache size
    // is exceeded, try it for one dimension less.
    // Determine the optimum cache size taking the maximum cache
    // size into account. This is done by starting at the highest
    // dimension and working our way down until the cache size fits.
    uint32_t nrd = nrdim;
    while (nrd > 0) {
        nrd--;
        // Caching is needed if lower dimensions are reused because
        // a higher dimension loops through a tile.
        // So skip dimensions until a reused dimension is found.
	uint32_t nr = nrd;
        while (nr > 0  &&  reused(nr) == 0) {
            nr--;
        }
	// The cache needs to contain the tiles needed for the entire window
	// of the remaining axes.
	// If a tile is reused, we also need to take into account
	// the number of tiles needed for the slice.
        uint64_t cacheSize = 1;
        for (i=0; i<nr; i++) {
            cacheSize *= ntiles(i);
        }
	if (reused(nr) > 0) {
	    for (i=nr+1; i<=nrd; i++) {
		cacheSize *= sliceTiles(i);
	    }
	}
        if (cacheSize == validateCacheSize (cacheSize, maxCacheSize,
                                            bucketSize)) {
	    return cacheSize;
        }
	nrd = nr;
    }
    return 1;
}

void TSMCube::resizeTileSections()
{
  // Resize to dimension nrdim_p
  if (nrTileSection_p.nelements() != nrdim_p) {
    nrTileSection_p.resize(nrdim_p);
    startTile_p.resize(nrdim_p);
    endTile_p.resize(nrdim_p);
    startPixelInFirstTile_p.resize(nrdim_p);
    endPixelInFirstTile_p.resize(nrdim_p);
    endPixelInLastTile_p.resize(nrdim_p);
  }
  return;
}

void TSMCube::accessSection (const IPosition& start, const IPosition& end,
                             char* section, uint32_t colnr,
                             uint32_t localPixelSize, uint32_t, bool writeFlag)
{
    // Set flag if writing.
    if (writeFlag) {
	stmanPtr_p->setDataChanged();
    }
    // Prepare for the iteration through the necessary tiles.
    uint32_t i, j;

    // Initialize the various variables and determine the number of
    // tiles needed (which will determine the cache size).
    // Also determine if the slice happens to be an entire slice
    // or if it is a line (this cases occur quite often and can be
    // handled in a more optimal way).
    bool oneEntireTile = true;
    uint32_t lineIndex = 0;
    uint32_t nOneLong = 0;
    for (i=0; i<nrdim_p; i++) {
        startTile_p(i) = start(i) / tileShape_p(i);
        endTile_p(i)   = end(i) / tileShape_p(i);
        nrTileSection_p(i)   = 1 + endTile_p(i) - startTile_p(i);
        startPixelInFirstTile_p(i) = start(i) - startTile_p(i)*tileShape_p(i);
        endPixelInLastTile_p(i)    = end(i) - endTile_p(i) * tileShape_p(i);
        endPixelInFirstTile_p(i)   = tileShape_p(i) - 1;
        if (nrTileSection_p(i) == 1) {
            endPixelInFirstTile_p(i) = endPixelInLastTile_p(i);
            if (startPixelInFirstTile_p(i) != 0
            ||  endPixelInFirstTile_p(i) != tileShape_p(i) - 1) {
                oneEntireTile = false;
            }
        }else{
            oneEntireTile = false;
        }
        if (start(i) == end(i)) {
            nOneLong++;
        }else{
            lineIndex = i;
        }
    }
    // Get the cache.
    BucketCache* cachePtr = getCache();
    
//    cout << "nrTileSection_p=" << nrTileSection_p << endl;
//    cout << "startTile_p=" << startTile_p << endl;
//    cout << "endTile_p=" << endTile_p << endl;
//    cout << "startPixelInFirstTile_p" << startPixelInFirstTile_p << endl;
//    cout << "endPixelInFirstTile_p" << endPixelInFirstTile_p << endl;
//    cout << "endPixelInLastTile_p" << endPixelInLastTile_p << endl;

    // A tile can contain more than one data array.
    // Each array is contiguous, so the first pixel of an array
    // starts after the other arrays.
    uint32_t pixelOffset = localOffset_p[colnr];

    // If the section matches the tile shape, we can simply
    // copy all values and do not have to do difficult iterations.
    if (oneEntireTile) {
        // Get the tile from the cache.
        uint32_t tileNr = expandedTilesPerDim_p.offset (startTile_p);
        char* dataArray = cachePtr->getBucket (tileNr);
        // If writing, set cache slot to dirty.
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

    // If the section is a line, call a specialized function.
    // Note that a single pixel is also handled as a line.
    if (nOneLong >= nrdim_p - 1) {
        accessLine (section, pixelOffset, localPixelSize,
                    writeFlag, cachePtr,
                    startTile_p, endTile_p(lineIndex),
                    startPixelInFirstTile_p, endPixelInLastTile_p(lineIndex),
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
    IPosition startPixel (startPixelInFirstTile_p);
    IPosition endPixel   (endPixelInFirstTile_p);
    IPosition tilePos    (startTile_p);
    IPosition tileIncr = 
      expandedTilesPerDim_p.offsetIncrement (nrTileSection_p);
    IPosition dataLength(nrdim_p);
    IPosition dataPos   (nrdim_p);
    IPosition sectionPos(nrdim_p);
    uint32_t dataOffset;
    size_t sectionOffset;
    uint32_t tileNr = expandedTilesPerDim_p.offset (tilePos);

    while (true) {
//      cout << "tilePos=" << tilePos << endl;
//      cout << "tileNr=" << tileNr << endl;
//      cout << "start=" << startPixel << endl;
//      cout << "end=" << endPixel << endl;
        // Get the tile from the cache.
        // Set it to dirty if we are writing.
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

        while (true) {
            uint32_t localSize = dataLength(0) * localPixelSize;
            /* merge zero increments into one copy */
            for (j = 1; j < nrdim_p; j++) {
                if (dataIncr(j) == 0 && sectionIncr(j) == 0) {
                    localSize *= dataLength(j);
                    dataPos(j) = endPixel(j);
                }
                else {
                    break;
                }
            }

            if (writeFlag) {
                TSMCube_MoveData(dataArray + dataOffset,
                                 section + sectionOffset, localSize);
            } else {
                TSMCube_MoveData(section + sectionOffset,
                                 dataArray + dataOffset, localSize);
            }
            dataOffset += localSize;
            sectionOffset += localSize;
            for (j = 1; j < nrdim_p; j++) {
                dataOffset += dataIncr(j);
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
            if (++tilePos(i) < endTile_p(i)) {
                break;                                 // not at last tile
            }
            if (tilePos(i) == endTile_p(i)) {
                endPixel(i) = endPixelInLastTile_p(i);   // last tile
                break;
            }
            // Past last tile in this dimension.
            // Reset start and end.
            tilePos(i) = startTile_p(i);
            startPixel(i) = startPixelInFirstTile_p(i);
            endPixel(i)   = endPixelInFirstTile_p(i);
        }
        if (i == nrdim_p) {
            break;                                     // ready
        }
    }
}

void TSMCube::accessLine (char* section, uint32_t pixelOffset,
                          uint32_t localPixelSize,
                          bool writeFlag, BucketCache* cachePtr,
                          const IPosition& startTile, uint32_t endTile,
                          const IPosition& startPixelInFirstTile,
                          uint32_t endPixelInLastTile,
                          uint32_t lineIndex)
{
    // Get the stride to get to the next tile.
    uint32_t tileIncr = expandedTilesPerDim_p(lineIndex);
    uint32_t tileNr = expandedTilesPerDim_p.offset (startTile);
    uint32_t stTile = startTile(lineIndex);
    // Get the stride to get to the next pixel in a tile.
    uint32_t stride = expandedTileShape_p(lineIndex) * localPixelSize;
    bool contiguous = (stride == localPixelSize);
    // Calculate the absolute pixel offset in the first tile
    // and in the other tiles.
    uint32_t offset = pixelOffset + localPixelSize *
                           expandedTileShape_p.offset (startPixelInFirstTile);
    uint32_t offsetInOtherTile = offset - startPixelInFirstTile(lineIndex) *stride;
    uint32_t nrPixel = tileShape_p(lineIndex) - startPixelInFirstTile(lineIndex);

    // Loop through all tiles.
    while (stTile <= endTile) {
        if (stTile == endTile) {
            nrPixel -= tileShape_p(lineIndex) - endPixelInLastTile - 1;
        }
	uint32_t localSize = nrPixel * localPixelSize;

//      cout << "tilePos=" << startTile << endl;
//      cout << "tileNr=" << tileNr << endl;
//      cout << "start=" << startPixel << endl;
//      cout << "nrpixel=" << nrPixel << endl;
        // Get the tile from the cache.
        // Set it to dirty if we are writing.
        char* dataArray = cachePtr->getBucket (tileNr) + offset;
        if (writeFlag) {
            cachePtr->setDirty();
        }
        // Copy the data. If contiguous we can copy directly.
        // Otherwise loop through all pixels.
        if (contiguous) {
            if (writeFlag) {
                TSMCube_MoveData(dataArray,section,localSize);
            }
            else {
                TSMCube_MoveData(section,dataArray,localSize);
            }
            section += localSize;
        }else{
            // Try to make the data copy as fast as possible.
            // Do this by specializing the cases (which occur very often)
            // where no conversion is needed.
            bool convert = false;
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
                    case (sizeof(int32_t)):
                        {
                            int32_t* sect = (int32_t*)section;
                            while (nrPixel > 0) {
                                *(int32_t*)dataArray = *sect++;
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
                        convert = true;
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
                    case (sizeof(int32_t)):
                        {
                            int32_t* sect = (int32_t*)section;
                            while (nrPixel > 0) {
                                *sect++ = *(int32_t*)dataArray;
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
                        convert = true;
                    }
                }
                if (convert) {
                    while (nrPixel > 0) {
                      memcpy (section, dataArray, localPixelSize);
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
                             char* section, uint32_t colnr,
                             uint32_t localPixelSize, uint32_t externalPixelSize,
                             bool writeFlag)
{
    // If all strides are 1, use accessSection.
    if (stride.allOne()) {
        accessSection (start, end, section, colnr,
                       localPixelSize, externalPixelSize, writeFlag);
        return;
    }
    // Set flag if writing.
    if (writeFlag) {
	stmanPtr_p->setDataChanged();
    }
    uint32_t i, j;
    // Get the cache (if needed).
    BucketCache* cachePtr = getCache();

    // A tile can contain more than one data array.
    // Each array is contiguous, so the first pixel of an array
    // starts after the other arrays.
    uint32_t pixelOffset = localOffset_p[colnr];
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
    uint32_t dataOffset;
    size_t sectionOffset;

    // Determine if the first dimension is strided.
    bool strided = (stride(0) != 1);
    // The first time all dimensions are evaluated to set pixelStart/End
    // correctly.
    bool firstTime = true;
    while (true) {
        // Determine the tile position from the pixel position.
        for (i=0; i<nrdim_p; i++) {
            sectionPos(i) += nrPixel(i);
            bool nextDim = false;
            if (pixelPos(i) > end(i)) {
                pixelPos(i)   = start(i);
                sectionPos(i) = 0;
                nextDim = true;              // also evaluate next dimension
            }
            tilePos(i) = pixelPos(i) / tileShape_p(i);
            startPixel(i) = pixelPos(i) - tilePos(i) * tileShape_p(i);
            uint32_t leng = (tileShape_p(i) - startPixel(i) + stride(i) - 1)
                        / stride(i);
            if (int32_t(leng + sectionPos(i)) > sectionShape(i)) {
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
            firstTime = false;
        }
        uint32_t tileNr = expandedTilesPerDim_p.offset (tilePos);
//      cout << "tilePos=" << tilePos << endl;
//      cout << "tileNr=" << tileNr << endl;
//      cout << "start=" << startPixel << endl;
        // Get the tile from the cache.
        // Set it to dirty if we are writing.
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
        uint32_t strideSize = 0;
        uint32_t localSize  = nrPixel(0) * localPixelSize;
        if (strided) {
            strideSize = stride(0) * localPixelSize;
        }

        while (true) {
            if (strided) {
                uint32_t nrp = nrPixel(0);
                for (j=0; j<nrp; j++) {
                    if (writeFlag) {
                        TSMCube_MoveData((char*)(dataArray+dataOffset),
                                         (char*)(section+sectionOffset),
                                         localPixelSize);
                    }
                    else {
                        TSMCube_MoveData((char*)(section+sectionOffset),
                                         (char*)(dataArray+dataOffset),
                                         localPixelSize);
                    }
                    dataOffset    += strideSize;
                    sectionOffset += localPixelSize;
                }
            }
            else {
                if (writeFlag) {
                    TSMCube_MoveData(dataArray+dataOffset,
                                     section+sectionOffset,localSize);
                }
                else {
                    TSMCube_MoveData(section+sectionOffset,
                                     dataArray+dataOffset,localSize);
                }
                dataOffset    += localSize;
                sectionOffset += localSize;
            }
            for (j=1; j<nrdim_p; j++) {
                // Catch attempt to increment dataOffset below 0
                DebugAssert(dataIncr(j) >= 0 ||
                            dataOffset >= static_cast<uint32_t>(-dataIncr(j)),
                            DataManError);
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


} //# NAMESPACE CASACORE - END
