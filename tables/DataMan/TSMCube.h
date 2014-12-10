//# TSMCube.h: Tiled hypercube in a table
//# Copyright (C) 1995,1996,1997,1999,2000,2001,2002
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

#ifndef TABLES_TSMCUBE_H
#define TABLES_TSMCUBE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TSMShape.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class TiledStMan;
class TSMFile;
class TSMColumn;
class BucketCache;
template<class T> class Block;

// <summary>
// Tiled hypercube in a table
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TiledStMan>TiledStMan</linkto>
//   <li> <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
//        for a discussion of the maximum cache size
//   <li> <linkto class=TSMFile>TSMFile</linkto>
//   <li> <linkto class=BucketCache>BucketCache</linkto>
// </prerequisite>

// <etymology>
// TSMCube represents a hypercube in the Tiled Storage Manager.
// </etymology>

// <synopsis>
// TSMCube defines a tiled hypercube. The data is stored in a TSMFile
// object and accessed using a BucketCache object. The hypercube can
// be extensible in its last dimension to support tables with a size
// which is not known in advance.
// <br>
// Normally hypercubes share the same TSMFile object, but extensible
// hypercubes have their own TSMFile object (to be extensible).
// If the hypercolumn has multiple data columns, their cells share the same
// tiles. Per tile data column A appears first, thereafter B, etc..
// <br>
// The data in the cache is held in external format and is converted
// when accessed. The alternative would be to hold it in the cache in
// local format and convert it when read/written from the file. It was
// felt that the latter approach would generate more needless conversions.
// <p>
// The possible id and coordinate values are stored in a Record
// object. They are written in the main hypercube AipsIO file.
// <p>
// TSMCube uses the maximum cache size set for a Tiled Storage manager.
// The description of class
// <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
// contains a discussion about the effect of setting the maximum cache size.
// </synopsis> 

// <motivation>
// TSMCube encapsulates all operations on a hypercube.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TSMCube
{
public:
    // Define the possible access types for TSMDataColumn.
    enum AccessType {
	NoAccess,
	CellAccess,
	SliceAccess,
	ColumnAccess,
	ColumnSliceAccess
    };

    // Construct the hypercube using the given file with the given shape.
    // The record contains the id and possible coordinate values.
    // <br>If the cubeshape is empty, the hypercube is still undefined and
    // can be added later with setShape. That is only used by TiledCellStMan.
    // <br> The fileOffset argument is meant for class TiledFileAccess.
    TSMCube (TiledStMan* stman, TSMFile* file,
	     const IPosition& cubeShape,
	     const IPosition& tileShape,
	     const Record& values,
             Int64 fileOffset,
             Bool useDerived = False);

    // Reconstruct the hypercube by reading its data from the AipsIO stream.
    // It will link itself to the correct TSMFile. The TSMFile objects
    // must have been reconstructed in advance.
    TSMCube (TiledStMan* stman, AipsIO& ios,
             Bool useDerived = False);

    virtual ~TSMCube();

    // Flush the data in the cache.
    virtual void flushCache();

    // Clear the cache, so data will be reread.
    // If wanted, the data is flushed before the cache is cleared.
    void clearCache (Bool doFlush = True);

    // Empty the cache.
    // It will flush the cache as needed and remove all buckets from it
    // resulting in a possibly large drop in memory used.
    // It'll also clear the <src>userSetCache_p</src> flag.
    void emptyCache();

    // Show the cache statistics.
    virtual void showCacheStatistics (ostream& os) const;

    // Put the data of the object into the AipsIO stream.
    void putObject (AipsIO& ios);

    // Get the data of the object from the AipsIO stream.
    // It returns the data manager sequence number, which is -1 if
    // no file is attached to the cube (for cells without a value).
    Int getObject (AipsIO& ios);

    // Resync the object with the data file.
    // It reads the object, and adjusts the cache.
    virtual void resync (AipsIO& ios);

    // Is the hypercube extensible?
    Bool isExtensible() const;

    // Get the bucket size (which is the length of a tile in external format).
    uInt bucketSize() const;

    // Get the length of a tile in local format.
    uInt localTileLength() const;

    // Set the hypercube shape.
    // This is only possible if the shape was not defined yet.
    virtual void setShape (const IPosition& cubeShape,
                           const IPosition& tileShape);

    // Get the shape of the hypercube.
    const IPosition& cubeShape() const;

    // Get the shape of the tiles.
    const IPosition& tileShape() const;

    // Get the shape of the data cells in the cube.
    IPosition cellShape() const;

    // Get the size of a coordinate (i.e. the number of values in it).
    // If not defined, it returns zero.
    uInt coordinateSize (const String& coordinateName) const;

    // Get the record containing the id and coordinate values.
    // It is used by TSMIdColumn and TSMCoordColumn.
    // <group>
    const Record& valueRecord() const;
    Record& rwValueRecord();
    // </group>

    // Test if the id values match.
    Bool matches (const PtrBlock<TSMColumn*>& idColSet,
                 const Record& idValues);

    // Extend the last dimension of the cube with the given number.
    // The record can contain the coordinates of the elements added.
    virtual void extend (uInt nr, const Record& coordValues,
                         const TSMColumn* lastCoordColumn);

    // Extend the coordinates vector for the given coordinate
    // to the given length with the given coordValues.
    // It will be initialized to zero if no coordValues are given.
    // If the coordinate vector does not exist yet, it will be created.
    void extendCoordinates (const Record& coordValues,
			    const String& coordName, uInt length);

    // Read or write a section in the cube.
    // It is assumed that the section buffer is long enough.
    virtual void accessSection (const IPosition& start, const IPosition& end,
                                char* section, uInt colnr,
                                uInt localPixelSize, uInt externalPixelSize,
                                Bool writeFlag);

    // Read or write a section in a strided way.
    // It is assumed that the section buffer is long enough.
    virtual void accessStrided (const IPosition& start, const IPosition& end,
                                const IPosition& stride,
                                char* section, uInt colnr,
                                uInt localPixelSize, uInt externalPixelSize,
                                Bool writeFlag);

    // Get the current cache size (in buckets).
    uInt cacheSize() const;

    // Calculate the cache size (in buckets) for the given slice
    // and access path.
    // <group>
    uInt calcCacheSize (const IPosition& sliceShape,
			const IPosition& windowStart,
			const IPosition& windowLength,
			const IPosition& axisPath) const;
    static uInt calcCacheSize (const IPosition& cubeShape,
                               const IPosition& tileShape,
                               Bool extensible,
                               const IPosition& sliceShape,
                               const IPosition& windowStart,
                               const IPosition& windowLength,
                               const IPosition& axisPath,
                               uInt maxCacheSize, uInt bucketSize);
    // </group>

    // Set the cache size for the given slice and access path.
    virtual void setCacheSize (const IPosition& sliceShape,
                               const IPosition& windowStart,
                               const IPosition& windowLength,
                               const IPosition& axisPath,
                               Bool forceSmaller, Bool userSet);

    // Resize the cache object.
    // If forceSmaller is False, the cache will only be resized when it grows.
    // If the given size exceeds the maximum size with more
    // than 10%, the maximum size will be used.
    // The cacheSize has to be given in buckets.
    // <br>The flag <src>userSet</src> inidicates if the cache size is set by
    // the user (by an Accessor object) or automatically (by TSMDataColumn).
    virtual void setCacheSize (uInt cacheSize, Bool forceSmaller, Bool userSet);

    // Validate the cache size (in buckets).
    // This means it will return the given cache size if smaller
    // than the maximum cache size. Otherwise the maximum is returned.
    // <group>
    uInt validateCacheSize (uInt cacheSize) const;
    static uInt validateCacheSize (uInt cacheSize, uInt maxSize,
                                   uInt bucketSize);
    // </group>

    // Determine if the user set the cache size (using setCacheSize).
    Bool userSetCache() const;

    // Functions for TSMDataColumn to keep track of the last type of
    // access to a hypercube. It uses it to determine if the cache
    // has to be reset.
    // <group>
    AccessType getLastColAccess() const;
    const IPosition& getLastColSlice() const;
    void setLastColAccess (AccessType type);
    void setLastColSlice (const IPosition& slice);
    // </group>

protected:
    // Initialize the various variables.
    // <group>
    void setup();
    void setupNrTiles();
    // </group>

    // Adjust the tile shape to the hypercube shape.
    // A size of 0 gets set to 1.
    // A tile size > cube size gets set to the cube size.
    IPosition adjustTileShape (const IPosition& cubeShape,
			       const IPosition& tileShape) const;

    // Resize the IPosition member variables used in accessSection()
    // if nrdim_p changes value.
    void resizeTileSections();

private:
    // Forbid copy constructor.
    TSMCube (const TSMCube&);

    // Forbid assignment.
    TSMCube& operator= (const TSMCube&);

    // Get the cache object.
    // This will construct the cache object if not present yet.
    BucketCache* getCache();

    // Construct the cache object (if not constructed yet).
    virtual void makeCache();

    // Resync the cache object.
    virtual void resyncCache();

    // Delete the cache object.
    virtual void deleteCache();

    // Access a line in a more optimized way.
    void accessLine (char* section, uInt pixelOffset,
		     uInt localPixelSize,
		     Bool writeFlag, BucketCache* cachePtr,
		     const IPosition& startTile, uInt endTile,
		     const IPosition& startPixelInFirstTile,
		     uInt endPixelInLastTile,
		     uInt lineIndex);

    // Define the callback functions for the BucketCache.
    // <group>
    static char* readCallBack (void* owner, const char* external);
    static void writeCallBack (void* owner, char* external,
			       const char* local);
    static char* initCallBack (void* owner);
    static void deleteCallBack (void* owner, char* buffer);
    // </group>

    // Define the functions doing the actual read and write of the 
    // data in the tile and converting it to/from local format.
    // <group>
    char* readTile (const char* external);
    void writeTile (char* external, const char* local);
    // </group>

protected:
    //# Declare member variables.
    // Pointer to the parent storage manager.
    TiledStMan*     stmanPtr_p;
    // Is the class used directly or only by a derived class only?
    Bool            useDerived_p;
    // The values of the possible id and coordinate columns.
    Record          values_p;
    // Is the hypercube extensible?
    Bool            extensible_p;
    // Dimensionality of the hypercube.
    uInt            nrdim_p;
    // Number of tiles in the hypercube.
    uInt            nrTiles_p;
    // The shape of the hypercube.
    IPosition       cubeShape_p;
    // The shape of the tiles in the hypercube.
    IPosition       tileShape_p;
    // The number of tiles in each hypercube dimension.
    IPosition       tilesPerDim_p;
    // Precomputed tileShape information.
    TSMShape        expandedTileShape_p;
    // Precomputed tilesPerDim information.
    TSMShape        expandedTilesPerDim_p;
    // Number of tiles in all but last dimension (used when extending).
    uInt            nrTilesSubCube_p;
    // The tilesize in pixels.
    uInt            tileSize_p;
    // Pointer to the TSMFile object holding the data.
    TSMFile*        filePtr_p;
    // Offset in the TSMFile object where the data of this hypercube starts.
    Int64           fileOffset_p;
    // Offset for each data column in a tile (in external format).
    Block<uInt>     externalOffset_p;
    // Offset for each data column in a tile (in local format).
    Block<uInt>     localOffset_p;
    // The bucket size in bytes (is equal to tile size in bytes).
    uInt            bucketSize_p;
    // The tile size in bytes in local format.
    uInt            localTileLength_p;
    // The bucket cache.
    BucketCache*    cache_p;
    // Did the user set the cache size?
    Bool            userSetCache_p;
    // Was the last column access to a cell, slice, or column?
    AccessType      lastColAccess_p;
    // The slice shape of the last column access to a slice.
    IPosition       lastColSlice_p;

    // IPosition variables used in accessSection(); declared here
    // as member variables to avoid significant construction and
    // desctruction overhead if they are local to accessSection()
    // #tiles needed for the section
    IPosition nrTileSection_p;
    // First tile needed
    IPosition startTile_p;
    // Last tile needed
    IPosition endTile_p;
    // First pixel in first tile
    IPosition startPixelInFirstTile_p;
    // Last pixel in first tile
    IPosition endPixelInFirstTile_p;
    // Last pixel in last tile
    IPosition endPixelInLastTile_p;
};



inline BucketCache* TSMCube::getCache()
{
    if (cache_p == 0) {
	makeCache();
    }
    return cache_p;
}
inline uInt TSMCube::bucketSize() const
{ 
    return bucketSize_p;
}
inline uInt TSMCube::localTileLength() const
{ 
    return localTileLength_p;
}
inline const IPosition& TSMCube::cubeShape() const
{ 
    return cubeShape_p;
}
inline const IPosition& TSMCube::tileShape() const
{ 
    return tileShape_p;
}
inline const Record& TSMCube::valueRecord() const
{
    return values_p;
}
inline Record& TSMCube::rwValueRecord()
{
    return values_p;
}
inline Bool TSMCube::userSetCache() const
{
    return userSetCache_p;
}
inline TSMCube::AccessType TSMCube::getLastColAccess() const
{
    return lastColAccess_p;
}
inline const IPosition& TSMCube::getLastColSlice() const
{
    return lastColSlice_p;
}
inline void TSMCube::setLastColAccess (TSMCube::AccessType type)
{
    lastColAccess_p = type;
}
inline void TSMCube::setLastColSlice (const IPosition& slice)
{
    lastColSlice_p.resize (slice.nelements());
    lastColSlice_p = slice;
}




} //# NAMESPACE CASACORE - END

#endif
