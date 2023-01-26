//# TSMCubeMMap.cc: Tiled Hypercube Storage Manager for tables using mmap
//# Copyright (C) 2009
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
#include <casacore/tables/DataMan/TSMCubeMMap.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/DataMan/TSMFile.h>
#include <casacore/tables/DataMan/TSMDataColumn.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/IO/BucketMapped.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/OS/HostInfo.h>
#include <casacore/casa/string.h>                           // for memcpy
#include <casacore/casa/iostream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


TSMCubeMMap::TSMCubeMMap (TiledStMan* stman, TSMFile* file,
                          const IPosition& cubeShape,
                          const IPosition& tileShape,
                          const Record& values,
                          int64_t fileOffset)
  : TSMCube (stman, file, cubeShape, tileShape, values, fileOffset, true),
    cache_p (0)
{
  // Note that the TSMCube constructor can call setShape.
  // However, because it is in the constructor TSMCube's setShape is called.
  // Hence we have to make the cache here.
  if (fileOffset < 0  &&  nrTiles_p > 0) {
    makeCache();
  }
}

TSMCubeMMap::TSMCubeMMap (TiledStMan* stman, AipsIO& ios)
  : TSMCube (stman, ios, true),
    cache_p (0)
{}

TSMCubeMMap::~TSMCubeMMap()
{
    delete cache_p;
}

void TSMCubeMMap::showCacheStatistics (ostream& os) const
{
  if (cache_p != 0) {
    os << ">>> No TSMCube cache statistics (uses mmap)" << endl;
    os << "<<<" << endl;
  }
}


void TSMCubeMMap::makeCache()
{
    // If there is no cache, make one.
    if (cache_p == 0) {
        cache_p = new BucketMapped (filePtr_p->bucketFile(), fileOffset_p,
                                    bucketSize_p, nrTiles_p);
    }
}

void TSMCubeMMap::flushCache()
{
    if (cache_p != 0) {
	cache_p->flush();
    }
}

void TSMCubeMMap::resyncCache()
{
    if (cache_p != 0) {
	cache_p->resync (nrTiles_p);
    }
}

void TSMCubeMMap::deleteCache()
{
    delete cache_p;
    cache_p = 0;
}

void TSMCubeMMap::setShape (const IPosition& cubeShape,
                            const IPosition& tileShape)
{
  TSMCube::setShape (cubeShape, tileShape);
  makeCache();
}

void TSMCubeMMap::extend (uint64_t nr, const Record& coordValues,
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
    // Extend the cache which extends the file and remaps.
    // Note that extending TSMFile only means updating its length.
    getCache()->extend (nrTiles_p - nrold);
    filePtr_p->extend ((nrTiles_p - nrold) * bucketSize_p);
    // Update the last coordinate (if there).
    if (lastCoordColumn != 0) {
        extendCoordinates (coordValues, lastCoordColumn->columnName(),
                           cubeShape_p(lastDim));
    }
}

void TSMCubeMMap::setCacheSize (uint32_t, bool, bool)
{}

void TSMCubeMMap::setCacheSize (const IPosition&,
                                const IPosition&,
                                const IPosition&,
                                const IPosition&,
                                bool, bool)
{}

void TSMCubeMMap::accessSection (const IPosition& start, const IPosition& end,
                                 char* section, uint32_t colnr,
                                 uint32_t localPixelSize, uint32_t externalPixelSize,
                                 bool writeFlag)
{
  // A tile can contain more than one data column.
  // Get the offset of the column's data array in the tile.
  uint32_t tileOffset = externalOffset_p[colnr];
  // Get convert function and nr of elements per value to convert.
  const TSMDataColumn* dataColumn = stmanPtr_p->getDataColumn(colnr);
  Conversion::ValueFunction* convertFunc =
    dataColumn->getConvertFunction (writeFlag);
  uint32_t nrConvElem = dataColumn->getNrConvert();
  // Conversion (or memcpy) is necessary if different byte order or if
  // not aligned properly.
  // If not needed, it is possible to use assignment because for smaller arrays
  // it is faster than memcpy.
  // Not done yet; might be in future.
  ///  bool mustConvert = dataColumn->isConversionNeeded();
  ///  if (!mustConvert) {
  ///    if (tileOffset % sizeof(int) != 0
  ///    ||  tileSize_p % sizeof(int) != 0
  ///    ||  localPixelSize % sizeof(int) != 0) {
  ///      mustConvert = true;
  ///    }
  ///  }
  // A bool column is stored as bits and has to be treated differently.
  uint32_t dataPixelSize = externalPixelSize;
  bool useBool = false;
  if (dataPixelSize == 0) {
    useBool = true;
    dataPixelSize = 1;
  }
  // Set flag if writing.
  if (writeFlag) {
    stmanPtr_p->setDataChanged();
  }

  // Initialize the various variables and determine the number of
  // tiles needed (which will determine the cache size).
  // Also determine if the slice happens to be an entire tile
  // or if it is a line (these cases occur quite often and can be
  // handled in a faster way).
  bool oneEntireTile = true;
  for (uint32_t i=0; i<nrdim_p; i++) {
    startTile_p(i) = start(i) / tileShape_p(i);
    endTile_p(i)   = end(i) / tileShape_p(i);
    nrTileSection_p(i)         = 1 + endTile_p(i) - startTile_p(i);
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
  }
  // Get the cache.
  BucketMapped* cachePtr = getCache();
    
//    cout << "nrTileSection_p=" << nrTileSection_p << endl;
//    cout << "startTile_p=" << startTile_p << endl;
//    cout << "endTile_p=" << endTile_p << endl;
//    cout << "startPixelInFirstTile_p" << startPixelInFirstTile_p << endl;
//    cout << "endPixelInFirstTile_p" << endPixelInFirstTile_p << endl;
//    cout << "endPixelInLastTile_p" << endPixelInLastTile_p << endl;


  // If the section matches the tile shape, we can simply
  // copy all values and do not have to do difficult iterations.
  if (oneEntireTile) {
    // Get the tile from the cache.
    uint32_t tileNr = expandedTilesPerDim_p.offset (startTile_p);
    // If writing, set cache slot to dirty.
    if (writeFlag) {
      char* dataArray = cachePtr->getrwBucket (tileNr);
      convertFunc (dataArray+tileOffset, section,
                   tileSize_p*nrConvElem);
    }else{
      const char* dataArray = cachePtr->getBucket (tileNr);
      convertFunc (const_cast<char*>(section), dataArray+tileOffset,
                   tileSize_p*nrConvElem);
    }
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

  // Loop over all tiles.
  while (true) {
//      cout << "tilePos=" << tilePos << endl;
//      cout << "tileNr=" << tileNr << endl;
//      cout << "start=" << startPixel << endl;
//      cout << "end=" << endPixel << endl;
    // At this point we start looping through all pixels in the tile.
    // We do a vector at a time.
    // Calculate the start and end pixel in the tile.
    // Initialize the pixel position in the data and section.
    // Note that for Bools it counts external in bits.
    for (uint32_t i=0; i<nrdim_p; i++) {
      dataLength(i) = 1 + endPixel(i) - startPixel(i);
      dataPos(i)    = startPixel(i);
      sectionPos(i) = tilePos(i) * tileShape_p(i)
        + startPixel(i) - startSection(i);
    }
    dataOffset    = expandedTileShape_p.offset (startPixel);
    sectionOffset = localPixelSize * expandedSectionShape.offset (sectionPos);
    IPosition dataIncr    = dataPixelSize *
      expandedTileShape_p.offsetIncrement (dataLength);
    IPosition sectionIncr = localPixelSize *
      expandedSectionShape.offsetIncrement (dataLength);

    // Calculate the largest number of pixels, nSec
    // that are consequtive in data and in section
    uint32_t nSec = dataLength(0);
    uint32_t secDim = 1;
    while (secDim < nrdim_p &&
           dataLength(secDim-1) == tileShape_p(secDim-1) &&
           dataLength(secDim-1) == sectionShape(secDim-1)) {

        nSec *= dataLength(secDim);
        secDim++;
    }

    uint32_t nrval     = nSec * nrConvElem;
    uint32_t localSize = nSec * localPixelSize;
    uint32_t dataSize  = nSec * dataPixelSize;

    // Loop through the data in the tile.
    // Handle bool specifically because they are stored as bits.
    // On read, it converts the data from the external to the local format.
    // On write it does the opposite.
    if (useBool) {
      // Get the required part of the tile from the cache.
      char* dataArray;
      if (writeFlag) {
        dataArray = cachePtr->getrwBucket (tileNr);
      } else {
        dataArray = const_cast<char*>(cachePtr->getBucket (tileNr));
      }
      // Determine the byte to start with.
      dataArray += tileOffset + dataOffset/8;
      // Determine the bit to start with.
      dataOffset %= 8;
      while (true) {
        if (writeFlag) {
          Conversion::boolToBit (dataArray, section+sectionOffset, dataOffset,
                                 nrval);
        }else{
          Conversion::bitToBool (section+sectionOffset, dataArray, dataOffset,
                                 nrval);
        }
        dataOffset    += dataSize;
        sectionOffset += localSize;
        uint32_t j;
        for (j=secDim; j<nrdim_p; j++) {
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
    } else {
      // Get the required part of the tile from the cache.
      char* dataArray;
      if (writeFlag) {
        dataArray = cachePtr->getrwBucket (tileNr);
      } else {
        dataArray = const_cast<char*>(cachePtr->getBucket (tileNr));
      }
      // Determine the byte to start with.
      dataArray += tileOffset;
      dataOffset *= dataPixelSize;
      while (true) {
        if (writeFlag) {
          convertFunc (dataArray+dataOffset, section+sectionOffset, nrval);
        }else{
          convertFunc (section+sectionOffset, dataArray+dataOffset, nrval);
        }
        dataOffset    += dataSize;
        sectionOffset += localSize;
        uint32_t j;
        for (j=secDim; j<nrdim_p; j++) {
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
    }

    // Determine the next tile to access and the starting and
    // ending pixels in it.
    // We increase the tile position in a dimension.
    uint32_t i;
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

void TSMCubeMMap::accessStrided (const IPosition& start, const IPosition& end,
                                 const IPosition& stride,
                                 char* section, uint32_t colnr,
                                 uint32_t localPixelSize, uint32_t externalPixelSize,
                                 bool writeFlag)
{
  // If no strides, use accessSection.
  if (stride.allOne()) {
    accessSection (start, end, section, colnr,
                   localPixelSize, externalPixelSize, writeFlag);
    return;
  }
  // Get the data by getting the array part and stride it thereafter.
  // When writing it is the opposite.
  // Handle the arrays as chars to be type-agnostic, so add an axis for it.
  // It is less efficient than doing a proper accessStrided, but much easier.
  // Since strided access is used seldomly, the price is not too high.
  IPosition sectShape ((end - start + stride) / stride);
  IPosition fullShape (end - start + 1);
  IPosition incr(stride);
  // Add the first axis to handle as chars.
  if (localPixelSize != 1) {
    sectShape.prepend (IPosition(1, localPixelSize));
    fullShape.prepend (IPosition(1, localPixelSize));
    incr.prepend (IPosition(1,1));
  }
  IPosition fst(incr.size(), 0);
  IPosition fend(fullShape - 1);
  Array<char> fullArr(fullShape);
  Array<char> partArr = fullArr(fst, fend, incr);
  Array<char> sectArr(sectShape, section, SHARE);
  // Read the data of the full array.
  // Thereafter copy the part needed.
  accessSection (start, end, fullArr.data(), colnr,
                 localPixelSize, externalPixelSize, false);
  if (writeFlag) {
    partArr = sectArr;
    accessSection (start, end, fullArr.data(), colnr,
                   localPixelSize, externalPixelSize, true);
  } else {
    sectArr = partArr;
  }
}


} //# NAMESPACE CASACORE - END
