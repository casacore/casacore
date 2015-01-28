//# LCPagedMask.h: Class to define a rectangular mask as a region
//# Copyright (C) 1998,1999,2000,2003
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

#ifndef LATTICES_LCPAGEDMASK_H
#define LATTICES_LCPAGEDMASK_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/Lattices/PagedArray.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class to define a rectangular mask as a region
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
// </prerequisite>

// <synopsis> 
// The LCPagedMask class is a specialization of class
// <linkto class=LCRegion>LCRegion</linkto>.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <todo asof="1997/11/11">
// </todo>

class LCPagedMask: public LCRegionSingle
{
public:
    LCPagedMask();

    // Construct a PagedMask object for (part of) a lattice.
    // The box defines the position of the mask.
    // The default mask shape is the lattice shape.
    // <group>
    LCPagedMask (const TiledShape& latticeShape, const String& tableName);
    LCPagedMask (const TiledShape& maskShape, const LCBox& box,
		 const String& tableName);
    LCPagedMask (PagedArray<Bool>& mask, const LCBox& box);
    // </group>

    // Copy constructor (copy semantics).
    LCPagedMask (const LCPagedMask& other);

    // Destructor
    virtual ~LCPagedMask();

    // Assignment (reference semantics).
    LCPagedMask& operator= (const LCPagedMask& other);

    // Comparison
    virtual Bool operator==(const LCRegion& other) const;

    // Make a copy of the derived object.
    virtual LCRegion* cloneRegion() const;

    // This function is used by the LatticeIterator class to generate an
    // iterator of the correct type for this Lattice. Not recommended
    // for general use. 
    virtual LatticeIterInterface<Bool>* makeIter
				(const LatticeNavigator& navigator,
				 Bool useRef) const;

    // Returns the maximum recommended number of pixels for a cursor.
    // This is the number of pixels in a tile. 
    virtual uInt advisedMaxPixels() const;

    // Help the user pick a cursor for most efficient access.
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

    // Maximum size - not necessarily all used. In pixels.
    virtual uInt maximumCacheSize() const;

    // Set the maximum (allowed) cache size as indicated.
    virtual void setMaximumCacheSize (uInt howManyPixels);

    // Set the cache size as to "fit" the indicated path.
    virtual void setCacheSizeFromPath (const IPosition& sliceShape,
				       const IPosition& windowStart,
				       const IPosition& windowLength,
				       const IPosition& axisPath);
    
    // Set the actual cache size for this Array to be be big enough for the
    // indicated number of tiles. This cache is not shared with PagedArrays
    // in other rows and is always clipped to be less than the maximum value
    // set using the setMaximumCacheSize member function.
    // tiles. Tiles are cached using a first in first out algorithm. 
    virtual void setCacheSizeInTiles (uInt howManyTiles);

    // Clears and frees up the caches, but the maximum allowed cache size is 
    // unchanged from when setCacheSize was called
    virtual void clearCache();

    // Report on cache success.
    virtual void showCacheStatistics (ostream& os) const;

    // Handle deletion of the region by deleting the associated table.
    virtual void handleDelete();

    // Handle renaming the region by renaming the associated table.
    // If overwrite=False, an exception will be thrown if a table with the
    // new name already exists.
    virtual void handleRename (const String& newName, Bool overwrite);

    // Handle the (un)locking.
    // <group>
    virtual Bool lock (FileLocker::LockType, uInt nattempts);
    virtual void unlock();
    virtual Bool hasLock (FileLocker::LockType) const;
    // </group>

    // Resynchronize the PagedArray object with the lattice file.
    // This function is only useful if no read-locking is used, ie.
    // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
    // In that cases the table system does not acquire a read-lock, thus
    // does not synchronize itself automatically.
    virtual void resync();

    // Flush the data (but do not unlock).
    virtual void flush();

    // Temporarily close the lattice.
    // It will be reopened automatically on the next access.
    virtual void tempClose();

    // Explicitly reopen the temporarily closed lattice.
    virtual void reopen();

    // Get the class name (to store in the record).
    static String className();

    // Region type. Returns class name.
    virtual String type() const;

    // Convert the (derived) object to a record.
    virtual TableRecord toRecord (const String& tableName) const;

    // Convert correct object from a record.
    static LCPagedMask* fromRecord (const TableRecord&,
				    const String& tablename);

    // An LCPagedMask is writable if the underlying PagedArray is.
    virtual Bool isWritable() const;

protected:
    // Construct another LCPagedMask (for e.g. another lattice) by moving
    // this one. It recalculates the bounding mask.
    // A positive translation value indicates "to right".
    virtual LCRegion* doTranslate (const Vector<Float>& translateVector,
				   const IPosition& newLatticeShape) const;

private:
    // Create the object from a record (for an existing mask).
    LCPagedMask (PagedArray<Bool>& mask,
		 const IPosition& blc,
		 const IPosition& latticeShape);


    LCBox            itsBox;
    PagedArray<Bool> itsMask;
};



} //# NAMESPACE CASACORE - END

#endif
