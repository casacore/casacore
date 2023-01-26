//# LatticeCache: Cache for accessing a Lattice in Tiles
//# Copyright (C) 1995,1996,1997,1999,2000,2001
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

#ifndef LATTICES_LATTICECACHE_H
#define LATTICES_LATTICECACHE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Block.h>

//# Forward Declarations
#include <casacore/casa/iosfwd.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> class Block;
template <class T> class Lattice;

// <summary> a class for caching image access via tiles</summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Lattice>Lattice</linkto>
// </prerequisite>
//
// <etymology>
// This class divides an image into Tiles that are stored in
// a Cache.
// </etymology>
//
// <synopsis> 
// An image is divided into tiles of a specified shape. Access to the
// image pixels is via these tiles. A cache of active tiles is kept
// in memory up to a specified limit in memory allocation. Tiles
// are flushed to disk using a Least-Recently-Used Criterion.
//
// The tile size specified is the maximum dimension. Near the edge,
// smaller tiles are returned if necessary.
//
// The offset needed to get back to true pixels is also available.
//
// The cache hit rate for a sufficient number of random accesses
// goes as the ratio of cache size to image size.
//
// Tiles may be overlapped. If there is any overlap then the
// caller is responsible for dealing with the overlap. Normally
// one will only want overlapping windows for additive operations
// in which case the additive flag to the constructor should be 
// used.
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock>
//
// </example>
//
// <motivation> 
// To aid in gridding
// </motivation>
//
// <todo asof="1997/02/27">
// </todo>

template <class T> class LatticeCache 
{
public:

  // Constructor: cachesize in units of T. tileOverlap is the fractional
  // overlap between neighbouring tile. 
  LatticeCache(Lattice<T> &image, int32_t cacheSize, IPosition tileShape,
	       Vector<float>& tileOverlap, bool additive);

  LatticeCache(const LatticeCache<T> & other);

  LatticeCache<T> &operator=(const LatticeCache<T> & other);

  virtual ~LatticeCache();

  // Return the tile for a given location
  // <group>
  Array<T>& tile(IPosition& cacheLoc, const IPosition& tileLoc, bool discard=true);
  Array<T>& tile(const IPosition& tileLoc, bool discard=true);
  // </group>

  // const version is needed
  const Array<T>& tile(const IPosition& tileLoc);

  // Return the IPosition for the start of this tile
  IPosition& cacheLocation(IPosition& cacheLoc, const IPosition& tileLoc);

  // Show the statistics of cache access
  virtual void showCacheStatistics(ostream& os);

  // Clear the statistics of cache access
  virtual void clearCacheStatistics();

  // Flush contents
  virtual void flush();

protected:

  LatticeCache() {};

  int32_t numberTiles;
  IPosition tileShape;
  Vector<int32_t> tileShapeVec, tileOffsetVec;
  Vector<float> tileOverlap;
  bool additive;

  int32_t cacheSize;
  int32_t cacheAccesses;
  int32_t cacheHits;
  int32_t cacheMisses;
  int32_t cacheReads;
  int32_t cacheWrites;

  int32_t getFreeTile(bool readonly);

  Block<IPosition> tileLocs;
  Block<int32_t> tileSequence;
  Block<Array<T> > tileContents;

  void writeTile(int32_t tile);
  void readTile(int32_t tile, bool readonly);

  Lattice<T>* image_p;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/LatticeCache.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
