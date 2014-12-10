//# TSMCubeMMap.h: Tiled hypercube in a table
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
//#
//# $Id$

#ifndef TABLES_TSMCUBEMMAP_H
#define TABLES_TSMCUBEMMAP_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TSMCube.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
class BucketMapped;

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
//   <li> <linkto class=BucketMapped>BucketMapped</linkto>
// </prerequisite>

// <etymology>
// TSMCubeMMap represents a hypercube in the Tiled Storage Manager.
// </etymology>

// <synopsis>
// TSMCubeMMap defines a tiled hypercube. The data is stored in a TSMFile
// object and accessed using a BucketMapped object. The hypercube can
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
// TSMCubeMMap uses the maximum cache size set for a Tiled Storage manager.
// The description of class
// <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
// contains a discussion about the effect of setting the maximum cache size.
// </synopsis> 

// <motivation>
// TSMCubeMMap encapsulates all operations on a hypercube.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TSMCubeMMap: public TSMCube
{
public:
    // Construct the hypercube using the given file with the given shape.
    // The record contains the id and possible coordinate values.
    // <br>If the cubeshape is empty, the hypercube is still undefined and
    // can be added later with setShape. That is only used by TiledCellStMan.
    // <br> The fileOffset argument is meant for class TiledFileAccess.
    TSMCubeMMap (TiledStMan* stman, TSMFile* file,
                 const IPosition& cubeShape,
                 const IPosition& tileShape,
                 const Record& values,
                 Int64 fileOffset);

    // Reconstruct the hypercube by reading its data from the AipsIO stream.
    // It will link itself to the correct TSMFile. The TSMFile objects
    // must have been reconstructed in advance.
    TSMCubeMMap (TiledStMan* stman, AipsIO& ios);

    virtual ~TSMCubeMMap();

    // Flush the data in the cache.
    virtual void flushCache();

    // Show the cache statistics.
    virtual void showCacheStatistics (ostream& os) const;

    // Set the hypercube shape.
    // This is only possible if the shape was not defined yet.
    virtual void setShape (const IPosition& cubeShape,
                           const IPosition& tileShape);

    // Extend the last dimension of the cube with the given number.
    // The record can contain the coordinates of the elements added.
    virtual void extend (uInt nr, const Record& coordValues,
                         const TSMColumn* lastCoordColumn);

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

private:
    // Forbid copy constructor.
    TSMCubeMMap (const TSMCubeMMap&);

    // Forbid assignment.
    TSMCubeMMap& operator= (const TSMCubeMMap&);

    // Get the cache object.
    // This will construct the cache object if not present yet.
    BucketMapped* getCache();

    // Construct the cache object (if not constructed yet).
    virtual void makeCache();

    // Resync the cache object.
    virtual void resyncCache();

    // Delete the cache object.
    virtual void deleteCache();

    //# Declare member variables.
    // The bucket cache.
    BucketMapped* cache_p;
};



inline BucketMapped* TSMCubeMMap::getCache()
{
    if (cache_p == 0) {
	makeCache();
    }
    return cache_p;
}



} //# NAMESPACE CASACORE - END

#endif
