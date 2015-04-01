//# LatticeCache.cc: Cache for accessing a Lattice in Tiles
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

#ifndef LATTICES_LATTICECACHE_TCC
#define LATTICES_LATTICECACHE_TCC

#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/Lattice.h>

#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>

#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

#include <casacore/lattices/Lattices/LatticeCache.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LatticeCache<T>::LatticeCache(const LatticeCache<T> & other)
{
  operator=(other);
}

template <class T>
LatticeCache<T> &LatticeCache<T>::operator=(const LatticeCache<T> & other)
{
  tileLocs=other.tileLocs;
  tileSequence=other.tileSequence;
  tileContents=other.tileContents;
  numberTiles=other.numberTiles;
  tileShape=other.tileShape;
  tileShapeVec=other.tileShapeVec;
  tileOffsetVec=other.tileOffsetVec;
  tileOverlap=other.tileOverlap;
  cacheSize=other.cacheSize;
  cacheAccesses=other.cacheAccesses;
  cacheHits=other.cacheHits;
  cacheMisses=other.cacheMisses;
  cacheReads=other.cacheReads;
  cacheWrites=other.cacheWrites;
  image_p=other.image_p;
  additive=other.additive;
  return *this;
}

// Construct a cache for a given tile shape and a given size of the
// cache.
template <class T>
LatticeCache<T>::LatticeCache(Lattice<T> &image,
				    Int iCacheSize, IPosition iTileShape,
				    Vector<Float>& iTileOverlap,
				    Bool iadditive)
  :  numberTiles(0), additive(iadditive), cacheAccesses(0), cacheHits(0),
     cacheMisses(0), cacheReads(0), cacheWrites(0), image_p(&image)
{

  AlwaysAssert(iTileShape.conform(image.shape()), AipsError);
  AlwaysAssert(iTileShape.product(), AipsError);

  tileShape=iTileShape;
  tileShapeVec=tileShape.asVector();
  tileOverlap=iTileOverlap;
  uInt i;
  for (i=0;i<tileShapeVec.nelements();i++) {
    AlwaysAssert(tileOverlap(i)>=0.0, AipsError);
    AlwaysAssert(tileOverlap(i)<1.0, AipsError);
  }
  tileOffsetVec.resize(tileShapeVec.nelements());
  for (i=0;i<tileShapeVec.nelements();i++) {
    tileOffsetVec(i)=Int(tileOverlap(i)*Float(tileShapeVec(i)));
  }
  cacheSize=iCacheSize;
  numberTiles=cacheSize/tileShape.product();

  AlwaysAssert(numberTiles, AipsError);

  // Set up the locations, sequence and contents caches
  tileLocs.resize(numberTiles);
  tileSequence.resize(numberTiles);
  tileContents.resize(numberTiles);

  // Initialize. We use the sequence number to determine if
  // a tile has any contents, and for least-recently-used 
  // caching.
  for (Int tile=0;tile<numberTiles;tile++) {
    tileSequence[tile]=-1;    
  }
}

// Destructor
template <class T>
LatticeCache<T>::~LatticeCache() {}

// Flush: write all extant tiles out
template <class T>
void LatticeCache<T>::flush() {
  for(Int tile=0;tile<numberTiles;tile++) {
    if(tileSequence[tile]>-1) {
      writeTile(tile);
    }
  }
}

// Return a specified tile. If we cannot locate it in the
// cache, then fill it into the cache. This is the prime
// interface. If readonly is true then we discard the current
// contents, otherwise we write them out, possibly adding to current
// tile.
template <class T>
Array<T>& LatticeCache<T>::tile(IPosition& cacheLoc, const IPosition& tileLoc,
				Bool readonly) {
  cacheLoc=cacheLocation(cacheLoc, tileLoc);
  cacheAccesses++;
  Int foundTile=-1;
  for(Int tile=0;tile<numberTiles;tile++) {
    // Much of the time is spent in this next isEqual.
    if((tileSequence[tile]>-1)&&(tileLocs[tile].isEqual(cacheLoc))) {
      foundTile=tile;
      break;
    }
  }
  // Keep track of hits and misses
  if(foundTile>-1) {
    cacheHits++;
  }
  else {
    cacheMisses++;
    foundTile=getFreeTile(readonly);
    AlwaysAssert(foundTile>-1, AipsError);
    tileLocs[foundTile]=cacheLoc;
    readTile(foundTile,readonly);
  }
  AlwaysAssert(foundTile>-1, AipsError);

  // Return the contents of this tile
  tileSequence[foundTile]=cacheAccesses;
  return tileContents[foundTile];
}
 

// Return a specified tile. If we cannot locate it in the
// cache, then fill it into the cache. This is the prime
// interface. If readonly is true then we discard the current
// contents, otherwise we write them out, possibly adding to current
// tile.
template <class T>
Array<T>& LatticeCache<T>::tile(const IPosition& tileLoc, Bool readonly) {
  IPosition cacheLoc;
  return tile(cacheLoc, tileLoc, readonly);
}

// Const version
template <class T>
const Array<T>& LatticeCache<T>::tile(const IPosition& tileLoc) {
  return tile(tileLoc, True);
}

// Print the Cache Statistics
template <class T>
void LatticeCache<T>::showCacheStatistics(ostream &os) {

  os<<"Cache Statistics"<<endl;
  os<<"   Lattice shape   "<<image_p->shape()<<endl;
  os<<"   Cache size      "<<cacheSize<<endl;
  os<<"   Tile shape      "<<tileShape<<endl;
  os<<"   Tile overlap    "<<tileOverlap<<endl;
  os<<"   Tile offset     "<<tileOffsetVec<<endl;
  os<<"   Number of tiles "<<numberTiles<<endl;
  os<<"   Accesses        "<<cacheAccesses<<endl;
  os<<"   Hits            "<<cacheHits<<endl;
  os<<"   Misses          "<<cacheMisses<<endl;
  os<<"   Hit rate        "<<100.0*Float(cacheHits)/Float(cacheAccesses)<<"%"<<endl;
  os<<"   Reads           "<<cacheReads<<endl;
  os<<"   Writes          "<<cacheWrites<<endl;
}

// Clear the Cache Statistics
template <class T>
void LatticeCache<T>::clearCacheStatistics() {

  cacheAccesses=0;
  cacheHits=0;
  cacheMisses=0;
  cacheReads=0;
  cacheWrites=0;
}

// Find the cache location (i.e. only on a grid).
template <class T>
IPosition& LatticeCache<T>::cacheLocation(IPosition& cacheLoc, const IPosition& tileLoc) {
  for (uInt i=0;i<tileLoc.nelements();i++) {
    if(tileOffsetVec(i)>0) {
      Int loco=tileLoc(i);
      Int loc=loco;
      loc-=loc%tileOffsetVec(i);
      if((loco-loc)>=3*tileShapeVec(i)/4) loc+=tileOffsetVec(i);
      if((loco-loc)<   tileShapeVec(i)/4) loc-=tileOffsetVec(i);
      if((loco-loc)<0) loc-=tileOffsetVec(i);
      if(loc<0) loc+=tileOffsetVec(i);
      cacheLoc(i)=loc;
    }
    else {
      cacheLoc(i)=0;
    }
  }
  return cacheLoc;
}

// ******************************************************************
// Start of private functions
// ******************************************************************

// Write a specified tile
template <class T>
void LatticeCache<T>::writeTile(Int tile) {
  tileSequence[tile]=cacheAccesses;
  if(additive) {
    Array<T> tileOnDisk(tileContents[tile].shape());
    tileOnDisk=0.0;
    image_p->getSlice(tileOnDisk, tileLocs[tile], tileContents[tile].shape(), 
		      IPosition(tileShape.nelements(), 1));
    tileContents[tile]+=tileOnDisk;
    cacheReads++;
  }
  image_p->putSlice(tileContents[tile], tileLocs[tile], 
    IPosition(tileShape.nelements(), 1));
  cacheWrites++;
}

// Read a specified tile and validate it
template <class T>
void LatticeCache<T>::readTile(Int tile, Bool readonly) {
  tileSequence[tile]=cacheAccesses;
  AlwaysAssert(tileLocs[tile].conform(tileShape), AipsError);
  Vector<Int> endLocVec=(tileLocs[tile]+tileShape).asVector();
  Vector<Int> imageShapeVec=image_p->shape().asVector();
  for (uInt i=0;i<imageShapeVec.nelements();i++) {
    endLocVec(i)=min(endLocVec(i), imageShapeVec(i));
  }
  IPosition actualShape=IPosition(endLocVec)-tileLocs[tile];
  if(additive&&!readonly) {
    tileContents[tile].resize(actualShape);
    tileContents[tile]=0.0;
  }
  else {
    image_p->getSlice(tileContents[tile], tileLocs[tile], actualShape,
		      IPosition(tileShape.nelements(), 1));
    cacheReads++;
  }
}

// Get a free tile. The contents are undefined since
// we will overwrite them immediately anyway. If readonly is
// True then we discard the current contents iso possibly
// writing them out. This is needed for a const version of tile.
template <class T>
Int LatticeCache<T>::getFreeTile(Bool readonly) {
  Int foundTile=-1;

  // First search for unallocated tiles
  for(Int tile=0;tile<numberTiles;tile++) {
    if(tileSequence[tile]<0) {
      foundTile=tile;
      break;
    }
  }

  if(foundTile<0) {
    // We didn't find an unallocated tile so we look for the
    // least-recently-used tile and use it, if readonly is
    // False, we have to first write it to disk
    Int oldest=cacheAccesses;
    for(Int tile=0;tile<numberTiles;tile++) {
      if((tileSequence[tile]>0)&&(tileSequence[tile]<oldest)) {
        oldest=tileSequence[tile];
        foundTile=tile;
      }
    }
    AlwaysAssert(foundTile>-1, AipsError);
    if(!readonly) {
      writeTile(foundTile);
    }
    tileSequence[foundTile]=-1;
  }
  AlwaysAssert(foundTile>-1, AipsError);
  return foundTile;
}



} //# NAMESPACE CASACORE - END


#endif
