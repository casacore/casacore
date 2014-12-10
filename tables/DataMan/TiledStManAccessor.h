//# TiledStManAccessor.h: Gives access to some TiledStMan functions
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001
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

#ifndef TABLES_TILEDSTMANACCESSOR_H
#define TABLES_TILEDSTMANACCESSOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManAccessor.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TiledStMan;
class DataManager;
class Table;
class IPosition;
class String;
class Record;

// <summary>
// Give access to some TiledStMan functions
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
// <li> <linkto class=TiledStMan>TiledStMan</linkto>
// </prerequisite>

// <synopsis>
// The Table system has one or more storage managers underneath.
// These storage managers are invisible and there is no way to
// get access to them.
// However, the <linkto class=TiledStMan>TiledStMan</linkto>-type
// storage managers are quite specific.
// This class ROTiledStManAccessor gives the user the means to
// access a TiledStMan-type object and to control it in some way.
// <p>
// The actions that can be performed deal with the caches used in
// a tiled storage manager. Per hypercube a cache is used to keep as many
// tiles in memory as needed for efficient access to the data.
// The cache size needed is calculated automatically. However,
// it may be possible that a cache uses too much memory. Therefore
// a maximum cache size can be specified, which can be done in 2 ways:
// <ol>
//  <li> To the constructor of a tiled storage manager. This is
//       persistent and acts as the default maximum cache size.
//  <li> Using the function setMaximumCacheSize in this accessor class.
//       This is not persistent and acts as a temporary overwrite
//       of the default maximum cache size.
// </ol>
// It is recommended to set the maximum cache size only when the
// tiled storage manager may use too much memory. Setting a
// maximum could have the effect that the optimal number of tiles
// does not fit in memory leading to excessive read/write activity.
// <br>For example:<br>
// A hypercube has shape [12,20,30,42] and tile shape [4,5,6,7].
// The hypercube contains doubles, so the tilesize is 6720 bytes.
// The number of tiles per dimension is [3,4,5,6] resulting in 360 tiles.
// Iterating through that hypercube requires that some tiles are kept in
// memory to avoid too many read operations. When iterating like
// <srcblock>
// for (uInt i3=0; i3<42; i3++)
//   for (uInt i2=0; i2<30; i2++)
//     for (uInt i1=0; i1<20; i1++)
//       for (uInt i0=0; i0<12; i0++)
//         do something with data[i0,i1,i2,i3]
// </srcblock>
// it is clear that it is best to have a cache which can contain at least
// 3*4*5 tiles. In that way each tile is read only once resulting in
// 360 reads.
// <br>When the cache can hold 3*4 tiles, the first tiles of the 3rd
// dimension have been flushed out when the second step in the 4th dimension
// gets executed. So the tiles have to be reread for each step in the 4th
// dimension, resulting in 3*4*5*42 = 2520 reads.
// <br>When the cache can hold only one tile, the situation is dramatic.
// A tile has to be read for every 4 pixels, resulting in 75600 reads.
// <p>
// Apart from setting the maximum cache size, one can also clear the
// caches. This can be useful to free memory when an iteration through the
// data in the tiled storage manager has been done completely. Clearing
// the caches also clears their statistics (see below).
// <p>
// Showing the statistics of the caches used by a tiled storage
// manager is possible. Per cache it shows the number of tiles accessed and
// the number of tiles actually read, written, or initialized. The hit ratio
// gives a good idea of the cache behaviour.
// <p>
// Note that the maximum cache size is not an absolute maximum.
// When the optimal number of tiles do not fit, it is tried if they fit
// when using an overdrawn of maximum 10%. If so, it uses that overdrawn.
// If not, it uses the maximum cache size.
// <p>
// A few functions exist to get information about a hypercube.
// The 'get' functions get the information for the given hypercube,
// while similar functions without the 'get' prefix do the same for the
// given row.
// </synopsis> 

// <motivation>
// In principle a pointer to TiledStMan could be used.
// However, that would give access to all public functions.
// Furthermore it could not distinguish between read/write and readonly
// tables. 
// </motivation>

// <example>
// This example shows how to set the maximum cache size for
// the tiled storage manager with the name "TSMExample". The cache
// size is not persistent, i.e. when the same table is reopened
// at a later time, this cache size is not remembered.
// <srcblock>
//  // Open a table.
//  Table table("someName.data");
//  // Set the maximum cache size of its tiled hypercube storage
//  // manager TSMExample to 0.5 Mb.
//  ROTiledStManAccessor accessor(table, "TSMExample");
//  accessor.setMaximumCacheSize (512*1024);
// </srcblock>
// </example>

//# <todo asof="$DATE:$">
//# </todo>


class ROTiledStManAccessor : public RODataManAccessor
{
public:
    // Default constructor should be used with care.
    // The resulting object cannot be used for any other operation
    // until a 'true' ROTiledStManAccessor object is assigned to it.
    ROTiledStManAccessor ();

    // Construct the object for a data manager in the table given the name
    // of the data manager or the column.
    // An exception is thrown if the data manager type is not any tiled
    // storage manager.
    ROTiledStManAccessor (const Table& table, const String& name,
                          Bool byColumn=False);

    virtual ~ROTiledStManAccessor();

    // Copy constructor (reference semantics).
    ROTiledStManAccessor (const ROTiledStManAccessor& that);

    // Assignment (reference semantics).
    ROTiledStManAccessor& operator= (const ROTiledStManAccessor& that);

    // Set the maximum cache size (in bytes) to be used by a hypercube
    // in the storage manager. Note that each hypercube has its own cache.
    // 0 means unlimited.
    // The initial maximum cache size is unlimited.
    // The maximum cache size given in this way is not persistent.
    // Only the maximum cache size given to the constructors of the tiled
    // storage managers, is persistent.
    void setMaximumCacheSize (uInt nbytes);

    // Get the maximum cache size (in bytes).
    uInt maximumCacheSize() const;

    // Get the current cache size (in buckets) for the hypercube in
    // the given row.
    uInt cacheSize (uInt rownr) const;

    // Get the hypercube shape of the data in the given row.
    const IPosition& hypercubeShape (uInt rownr) const;

    // Get the tile shape of the data in the given row.
    const IPosition& tileShape (uInt rownr) const;

    // Get the bucket size (in bytes) of the hypercube in the given row.
    uInt bucketSize (uInt rownr) const;

    // Get coordinate and id values of the hypercube in the given row.
    const Record& valueRecord (uInt rownr) const;

    // Return the number of hypercubes.
    uInt nhypercubes() const;

    // Get the current cache size (in buckets) for the given hypercube.
    uInt getCacheSize (uInt hypercube) const;

    // Get the shape of the given hypercube.
    const IPosition& getHypercubeShape (uInt hypercube) const;

    // Get the tile shape of the given hypercube.
    const IPosition& getTileShape (uInt hypercube) const;

     // Get the bucket size (in bytes) of the given hypercube.
    uInt getBucketSize (uInt hypercube) const;

    // Get coordinate and id values of the given hypercube.
    const Record& getValueRecord (uInt hypercube) const;

    // Calculate the cache size (in buckets) for accessing the hypercube
    // containing the given row. It takes the maximum cache size into
    // account (allowing an overdraft of 10%).
    // It uses the given axisPath (i.e. traversal order) to determine
    // the optimum size. A window can be specified to indicate that only
    // the given subset of the hypercube will be accessed. The window
    // defaults to the entire hypercube.
    // <br>
    // The length of the slice and window arguments and <src>axisPath</src>
    // must be less or equal to the dimensionality of the hypercube.
    // The non-specified <src>windowStart</src> parts default to 0.
    // The non-specified <src>windowLength</src> parts default to
    // the hypercube shape.
    // The non-specified <src>sliceShape</src> parts default to 1.
    // <br>
    // Axispath = [2,0,1] indicates that the z-axis changes most rapidly,
    // thereafter x and y. An axis can occur only once in the axisPath.
    // The non-specified <src>axisPath</src> parts get the natural order.
    // E.g. in the previous example axisPath=[2] defines the same path.
    // <group>
    uInt calcCacheSize (uInt rownr, const IPosition& sliceShape,
			const IPosition& axisPath) const;
    uInt calcCacheSize (uInt rownr, const IPosition& sliceShape,
			const IPosition& windowStart,
			const IPosition& windowLength,
			const IPosition& axisPath) const;
    // </group>

    // Set the cache size using the corresponding <src>calcCacheSize</src>
    // function mentioned above.
    // <br>When forceSmaller is False, the cache is not resized when the
    // new size is smaller.
    // <group>
    void setCacheSize (uInt rownr, const IPosition& sliceShape,
		       const IPosition& axisPath,
		       Bool forceSmaller = True);
    void setCacheSize (uInt rownr, const IPosition& sliceShape,
		       const IPosition& windowStart,
		       const IPosition& windowLength,
		       const IPosition& axisPath,
		       Bool forceSmaller = True);
    // </group>

    // Set the cache size for accessing the hypercube containing the given row.
    // When the give cache size exceeds the maximum cache size with more
    // than 10%, the maximum cache size is used instead.
    // <br>When forceSmaller is False, the cache is not resized when the
    // new size is smaller.
    void setCacheSize (uInt rownr, uInt nbuckets, Bool forceSmaller = True);

    // Clear the caches used by the hypercubes in this storage manager.
    // It will flush the caches as needed and remove all buckets from them
    // resulting in a possibly large drop in memory used.
    void clearCaches();


protected:
    // Get the data manager.
    DataManager* getDataManager() const;


private:
    //# Declare the data members.
    TiledStMan* dataManPtr_p;
};




} //# NAMESPACE CASACORE - END

#endif
