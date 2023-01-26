//# TiledStMan.h: Base class for Tiled Storage Managers
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

#ifndef TABLES_TILEDSTMAN_H
#define TABLES_TILEDSTMAN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/OS/Conversion.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TSMColumn;
class TSMDataColumn;
class TSMCube;
class TSMFile;
class TableDesc;
class Record;

// <summary>
// Base class for Tiled Storage Manager classes
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Description of Tiled Storage Manager in module file
//        <linkto module=Tables:TiledStMan>Tables.h</linkto>
//   <li> <linkto class=DataManager>DataManager</linkto>
//   <li> <linkto class=TSMColumn>TSMColumn</linkto>
//   <li> <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
//        for a discussion of the maximum cache size
// </prerequisite>

// <synopsis> 
// TiledStMan is the base class for Tiled Storage Managers.
// A tiled storage manager is capable of storing a hypercolumn
// (as defined by <linkto file="TableDesc.h#defineHypercolumn">
// TableDesc::defineHypercolumn</linkto>)
// in one or more hypercubes.
// <br>It is not necessary to define a hypercolumn. If not defined,
// it is assumed that all columns bound to this storage manager are
// data columns. At least one of the columns must have a fixed
// dimensionality and is used to determine the hypercube dimnensionality.
// <br>The general concept of these storage managers is explained in the
// <linkto module="Tables:TiledStMan">Tables module description</linkto>.
// <p>
// TiledStMan contains all common functions for the different tiled
// storage managers. In particular, it contains functions
// to check if the definitions of the shapes of hypercubes, coordinates, and
// data cells are consistent.
// It also contains various data members and functions to make them
// persistent by writing them into an AipsIO stream.
// </synopsis> 

// <motivation>
// This base class contains the common functionality of all
// tiled storage managers. The base class is still abstract.
// Only concrete tiled storage managers derived from it can
// be instantiated.
// <p>
// Tiled storage managers make access to array data possible with
// more or less the same efficiency for access along different axes.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TiledStMan : public DataManager
{
public:
    // Create a TiledStMan.
    TiledStMan();

    // Create a TiledStMan storage manager.
    // The given maximum cache size (in MibiByte) is persistent,
    // thus will be reused when the table is read back. Note that the class
    // <linkto class=ROTiledStManAccessor>ROTiledStManAccessor</linkto>
    // allows one to overwrite the maximum cache size temporarily.
    // Its description contains a discussion about the effects of
    // setting a maximum cache.
    TiledStMan (const String& hypercolumnName, uint32_t maximumCacheSizeMiB);

    virtual ~TiledStMan();

    // Get the name given to the storage manager.
    // This is the name of the hypercolumn.
    virtual String dataManagerName() const;

    void setDataManagerName (const String& newHypercolumnName);

    // Return a record containing data manager specifications.
    virtual Record dataManagerSpec() const;

    // Get data manager properties that can be modified.
    // It is only MaxCacheSize (the maximum cache size in MibiByte).
    // It is a subset of the data manager specification.
    virtual Record getProperties() const;

    // Modify data manager properties.
    // Only MaxCacheSize can be used. It is similar to function setCacheSize
    // with <src>canExceedNrBuckets=false</src>.
    virtual void setProperties (const Record& spec);

    // Set the flag to "data has changed since last flush".
    void setDataChanged();

    // Derive the tile shape from the hypercube shape for the given
    // number of pixels per tile. It is tried to get the same number
    // of tiles for each dimension.
    // When a weight vector is given, the number of tiles for a dimension
    // is proportional to the weight.
    // <br>After the initial guess it tries to optimize it by trying
    // to waste as little space as possible, while trying to keep as close
    // to the initial guess. The given tolerance (possibly per axis)
    // gives the minimum and maximum possible length of a tile axis
    // (minimum = initial_guess*tolerance; maximum = initial_guess/tolerance).
    // The heuristic is such that a tile axis length dividing the cube length
    // exactly is always favoured.
    // The test program <src>tTiledStMan</src> can be used to see how
    // the algorithm works out for a given tile size and cube shape.
    // <group>
    static IPosition makeTileShape (const IPosition& hypercubeShape,
				    double tolerance = 0.5,
				    uint64_t maxNrPixelsPerTile = 4*1024*1024);
    static IPosition makeTileShape (const IPosition& hypercubeShape,
				    const Vector<double>& weight,
				    const Vector<double>& tolerance,
				    uint64_t maxNrPixelsPerTile = 4*1024*1024);
    // </group>

    // Set the maximum cache size (in MiB) in a non-persistent way.
    virtual void setMaximumCacheSize (uint32_t nMiB);

    // Get the current maximum cache size (in MiB (MibiByte)).
    uint32_t maximumCacheSize() const;

    // Get the current cache size (in buckets) for the hypercube in
    // the given row.
    uint32_t cacheSize (rownr_t rownr) const;

    // Get the hypercube shape of the data in the given row.
    const IPosition& hypercubeShape (rownr_t rownr) const;

    // Get the tile shape of the data in the given row.
    const IPosition& tileShape (rownr_t rownr) const;

    // Get the bucket size (in bytes) of the hypercube in the given row.
    uint64_t bucketSize (rownr_t rownr) const;

    // Can the tiled storage manager handle changing array shapes?
    // The default is no (but TiledCellStMan can).
    virtual bool canChangeShape() const;

    // Can the tiled storage manager access an entire column.
    // TiledColumnStMan can always do that.
    // The others might be able to do it (for this time).
    // The default implementation returns true if there is only 1 hypercube.
    virtual bool canAccessColumn() const;

    // The data manager supports use of MultiFile.
    virtual bool hasMultiFileSupport() const;

    // Calculate the cache size (in buckets) for accessing the hypercube
    // containing the given row. It takes the maximum cache size into
    // account (allowing an overdraft of 10%).
    // It uses the given axisPath (i.e. traversal order) to determine
    // the optimum size. A window can be specified to indicate that only
    // the given subset of the hypercube will be accessed.
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
    // <br>When forceSmaller is false, the cache is not resized when the
    // new size is smaller.
    // <br>A flag is set indicating that the TSMDataColumn
    // access functions do not need to size the cache.
    uint32_t calcCacheSize (rownr_t rownr, const IPosition& sliceShape,
			const IPosition& windowStart,
			const IPosition& windowLength,
			const IPosition& axisPath) const;

    // Set the cache size using the <src>calcCacheSize</src>
    // function mentioned above.
    void setCacheSize (rownr_t rownr, const IPosition& sliceShape,
		       const IPosition& windowStart,
		       const IPosition& windowLength,
		       const IPosition& axisPath,
		       bool forceSmaller);

    // Set the cache size for accessing the hypercube containing the given row.
    // When the give cache size exceeds the maximum cache size with more
    // than 10%, the maximum cache size is used instead.
    // <br>When forceSmaller is false, the cache is not resized when the
    // new size is smaller.
    // <br>A flag is set indicating that the TSMDataColumn
    // access functions do not need to size the cache.
    void setCacheSize (rownr_t rownr, uint32_t nbuckets, bool forceSmaller);

    // Sets the cache size using the hypercube instead of the row number.
    // Useful for iterating over all hypercubes.
    void setHypercubeCacheSize (uint32_t hypercube, uint32_t nbuckets, bool forceSmaller);

    // Determine if the user set the cache size (using setCacheSize).
    bool userSetCache (rownr_t rownr) const;

    // Empty the caches used by the hypercubes in this storage manager.
    // It will flush the caches as needed and remove all buckets from them
    // resulting in a possibly large drop in memory used.
    // It also clears the userSetCache flag.
    void emptyCaches();

    // Show the statistics of all caches used.
    void showCacheStatistics (ostream& os) const;

    // Get the length of the data for the given number of pixels.
    // This can be used to calculate the length of a tile.
    uint64_t getLengthOffset (uint64_t nrPixels, Block<uint32_t>& dataOffset,
                            Block<uint32_t>& localOffset,
                            uint32_t& localTileLength) const;

    // Get the number of coordinate vectors.
    uint32_t nrCoordVector() const;

    // Get the nr of rows in this storage manager.
    rownr_t nrow() const;

    // Does the storage manager allow to add rows? (yes)
    bool canAddRow() const;

    // Get the default tile shape.
    // By default it returns a zero-length IPosition.
    virtual IPosition defaultTileShape() const;

    // Return the number of hypercubes.
    uint32_t nhypercubes() const;

    // Test if only one hypercube is used by this storage manager.
    // If not, throw an exception. Otherwise return the hypercube.
    virtual TSMCube* singleHypercube();

    // Get the given hypercube.
    // <group>
    const TSMCube* getTSMCube (uint32_t hypercube) const;
    TSMCube* getTSMCube (uint32_t hypercube);
    // </group>
    
    // Get the hypercube in which the given row is stored.
    // <group>
    const TSMCube* getHypercube (rownr_t rownr) const;
    virtual TSMCube* getHypercube (rownr_t rownr) = 0;
    // </group>

    // Get the hypercube in which the given row is stored.
    // It also returns the position of the row in that hypercube.
    virtual TSMCube* getHypercube (rownr_t rownr, IPosition& position) = 0;

    // Make the correct TSMCube type (depending on tsmOption()).
    TSMCube* makeTSMCube (TSMFile* file, const IPosition& cubeShape,
                          const IPosition& tileShape,
                          const Record& values, int64_t fileOffset=-1);

    // Read a tile and convert the data to local format.
    void readTile (char* local, const Block<uint32_t>& localOffset,
		   const char* external, const Block<uint32_t>& externalOffset,
		   uint32_t nrpixels);

    // Write a tile after converting the data to external format.
    void writeTile (char* external, const Block<uint32_t>& externalOffset,
		    const char* local, const Block<uint32_t>& localOffset,
		    uint32_t nrpixels);

    // Get the TSMFile object with the given sequence number.
    TSMFile* getFile (uint32_t sequenceNumber);

    // Open the storage manager for an existing table.
    virtual rownr_t open64 (rownr_t nrrow, AipsIO&);

    // Resync the storage manager with the new file contents.
    virtual rownr_t resync64 (rownr_t nrrow);

    // Reopen all files used in this storage manager for read/write access.
    virtual void reopenRW();

    // The data manager will be deleted (because all its columns are
    // requested to be deleted).
    // So clean up the things needed (e.g. delete files).
    virtual void deleteManager();

    // Create a column in the storage manager on behalf of a table column.
    // <group>
    // Create a scalar column.
    DataManagerColumn* makeScalarColumn (const String& name, int dataType,
					 const String& dataTypeID);
    // Create a direct array column.
    DataManagerColumn* makeDirArrColumn (const String& name, int dataType,
					 const String& dataTypeID);
    // Create an indirect array column.
    DataManagerColumn* makeIndArrColumn (const String& name, int dataType,
					 const String& dataTypeID);
    // </group>

    // The TiledStMan wants to do reallocateColumn.
    bool canReallocateColumns() const;

    // Reallocate the column object if it is part of this data manager.
    // It returns a pointer to the new column object.
    // It is used to remove the indirection of the TSMColumn objects
    // resulting in only one iso. two virtual column calls to get the data.
    DataManagerColumn* reallocateColumn (DataManagerColumn* column);

    // Set the shape and tile shape of a hypercube.
    // By default it throws an "impossible" exception.
    virtual void setShape (rownr_t rownr, TSMCube* hypercube,
			   const IPosition& shape,
			   const IPosition& tileShape);

    // Check the shape to be set for a hypercube.
    // It checks if it matches predefined (fixed shape) columns
    // and the shape of already defined coordinate columns.
    void checkCubeShape (const TSMCube* hypercube,
			 const IPosition& cubeShape) const;

    // Get the data type of the coordinate column with the given name.
    // An exception is thrown when the column is unknown.
    int coordinateDataType (const String& columnName) const;

    // Initialize the new coordinates for the given cube.
    void initCoordinates (TSMCube* hypercube);

    // Get pointer to data column object.
    const TSMDataColumn* getDataColumn (uint32_t colnr) const
      { return dataCols_p[colnr]; }

protected:
    // Set the persistent maximum cache size (in MiB).
    void setPersMaxCacheSize (uint32_t nMiB);

    // Get the bindings of the columns with the given names.
    // If bound, the pointer to the TSMColumn object is stored in the block.
    // If mustExist is true, an exception is thrown if the column
    // is not bound.
    // It returns the number of bound columns.
    uint32_t getBindings (const Vector<String>& columnNames,
		      PtrBlock<TSMColumn*>& colSet,
		      bool mustExist) const;

    // Function setup calls this function to allow the derived class
    // to check specific information. In case of errors, an exception
    // should be thrown.
    // By default it does nothing.
    virtual void setupCheck (const TableDesc& tableDesc,
			     const Vector<String>& dataNames) const;

    // Get the table description needed for the hypercolumn description.
    virtual const TableDesc& getDesc() const;

    // Check if values are given in the record for all columns in
    // the block. Also check if the data types are correct.
    // An exception is thrown if something is incorrect.
    void checkValues (const PtrBlock<TSMColumn*>& colSet,
		      const Record& values) const;

    // Check if the coordinate values are correct.
    // This calls checkValues and checks if their shapes match the
    // hypercube shape.
    // An exception is thrown if invalid.
    void checkCoordinates (const PtrBlock<TSMColumn*>& coordColSet,
			   const IPosition& cubeShape,
			   const Record& values) const;

    // Check if the shapes of FixedShape data and coordinate columns match.
    // An exception is thrown if not.
    void checkShapeColumn (const IPosition& shape) const;

    // Check if the cube shape matches that of defined coordinates.
    void checkCoordinatesShapes (const TSMCube* hypercube,
				 const IPosition& cubeShape) const;

    // Check if the hypercube to be added is correctly defined.
    void checkAddHypercube (const IPosition& cubeShape,
			    const Record& values) const;

    // Make a new TSMCube object.
    TSMCube* makeHypercube (const IPosition& cubeShape,
			    const IPosition& tileShape,
			    const Record& values);

    // Get the index of the hypercube with the given id-values.
    // If not found, -1 is returned.
    int32_t getCubeIndex (const Record& idValues) const;
    
    // Determine how many rows need to be added for an extension
    // (in the last dimension) of a hypercube with the given shape.
    rownr_t addedNrrow (const IPosition& shape, uint32_t incrInLastDim) const;

    // Flush the caches of all hypercubes.
    // If data have put and fsync is set, fsync all files.
    bool flushCaches (bool fsync);

    // Let a derived class read the header info.
    // This is used by the open and resync function.
    virtual void readHeader (rownr_t nrrow, bool firstTime) = 0;

    // Create the TSM header file.
    // It creates an AipsIO object for it.
    AipsIO* headerFileCreate();

    // Open the TSM header file.
    // It creates an AipsIO object for it.
    AipsIO* headerFileOpen();

    // Write the data into the header file.
    // The given number of TSMCube objects have to be written.
    void headerFilePut (AipsIO& headerFile, uint64_t nrCube);

    // Read the data from the header file.
    // When done for the first time, setup() is called to initialize
    // the various variables (using the extraNdim variable).
    // It returns the version of the AipsIO object in the header.
    uint32_t headerFileGet (AipsIO& headerFile, rownr_t tabNrrow, bool firstTime,
			int32_t extraNdim);

    // Close the header file.
    // It deletes the AipsIO object.
    void headerFileClose (AipsIO* headerFile);

    // Set up the TiledStMan variables from the table description.
    // The argument specifies the number of extra dimensions for the
    // hypercube compared to the data array (usually 0 or 1).
    // It is only used if no hypercolumn definition exists.
    // -1 means that the hypercolumn definition has to be present.
    void setup (int32_t extraNdim=-1);

    // Create a TSMFile object and store its pointer at the given index
    // in the block.
    void createFile (uint32_t index);

    // Convert the scalar data type to an array data type.
    // This function is temporary and can disappear when the ColumnDesc
    // classes use type TpArray*.
    int arrayDataType (int dataType) const;


    //# Declare all data members.
    // The name of the hypercolumn.
    String  hypercolumnName_p;
    // The number of rows in the columns.
    rownr_t nrrow_p;
    // The assembly of all columns.
    PtrBlock<TSMColumn*>  colSet_p;
    // The assembly of all data columns.
    PtrBlock<TSMDataColumn*> dataCols_p;
    PtrBlock<TSMColumn*>  dataColSet_p;
    // The assembly of all id columns.
    PtrBlock<TSMColumn*>  idColSet_p;
    // The assembly of all coordinate columns.
    PtrBlock<TSMColumn*>  coordColSet_p;
    // The assembly of all TSMFile objects.
    // The first file is for all non-extensible cubes, while the others
    // are for one file per extensible cube.
    PtrBlock<TSMFile*> fileSet_p;
    // The assembly of all TSMCube objects.
    PtrBlock<TSMCube*> cubeSet_p;
    // The persistent maximum cache size (in MiB) for a hypercube.
    uint32_t      persMaxCacheSize_p;
    // The actual maximum cache size for a hypercube (in MiB).
    uint32_t      maxCacheSize_p;
    // The dimensionality of the hypercolumn.
    uint32_t      nrdim_p;
    // The number of vector coordinates.
    uint32_t      nrCoordVector_p;
    // The fixed cell shape.
    IPosition fixedCellShape_p;
    // Has any data changed since the last flush?
    bool      dataChanged_p;

private:
    // Forbid copy constructor.
    TiledStMan (const TiledStMan&);

    // Forbid assignment.
    TiledStMan& operator= (const TiledStMan&);
};


inline uint32_t TiledStMan::maximumCacheSize() const
    { return maxCacheSize_p; }

inline uint32_t TiledStMan::nrCoordVector() const
    { return nrCoordVector_p; }

inline rownr_t TiledStMan::nrow() const
    { return nrrow_p; }

inline uint32_t TiledStMan::nhypercubes() const
    { return cubeSet_p.nelements(); }

inline void TiledStMan::setDataChanged()
    { dataChanged_p = true; }

inline const TSMCube* TiledStMan::getTSMCube (uint32_t hypercube) const
    { return const_cast<TiledStMan*>(this)->getTSMCube (hypercube); }

inline const TSMCube* TiledStMan::getHypercube (rownr_t rownr) const
    { return const_cast<TiledStMan*>(this)->getHypercube (rownr); }

inline void TiledStMan::setPersMaxCacheSize (uint32_t nMiB)
{
    persMaxCacheSize_p = nMiB;
    maxCacheSize_p = nMiB;
}




} //# NAMESPACE CASACORE - END

#endif
