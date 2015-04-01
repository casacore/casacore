//# LatticeRegion.h: An optionally strided region in a lattice
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
//#
//# $Id$

#ifndef LATTICES_LATTICEREGION_H
#define LATTICES_LATTICEREGION_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/casa/Arrays/Slicer.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LCRegion;


// <summary>
// An optionally strided  region in a Lattice
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="LCRegion">LCRegion</linkto>
// </prerequisite>

// <synopsis>
// A LatticeRegion is a lattice referencing a subset of another lattice
// by means of a <linkto class="Slicer">Slicer</linkto> object.
// <br>It is useful when only a subset of a lattice needs to be accessed.
// <p>
// When the LatticeRegion is created from a const <src>Lattice</src> object,
// it is not writable, thus it can only be used as an rvalue.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

//# <todo asof="yyyy/mm/dd">
//# </todo>

class LatticeRegion: public Lattice<Bool>
{
public:
    // The default constructor creates a LatticeRegion that is useless for just
    // about everything, except that it can be assigned to with the assignment
    // operator.
    LatticeRegion();

    // Create from the given region.
    // The pointer to the parent can be 0.
    LatticeRegion (const LCRegion& region);

    // Create from the given region and take over the pointer.
    // This means the user should not delete the region object pointed to,
    // because it will be deleted by the LatticeRegion destructor.
    // The pointer to the parent is set to 0.
    LatticeRegion (LCRegion* region);

    // Construct from the given slicer. The lattice shape has to be
    // the lattice shape of the lattice where the region is taken from.
    LatticeRegion (const Slicer& slicer, const IPosition& latticeShape);

    // Copy constructor (reference semantics).
    LatticeRegion (const LatticeRegion& other);

    virtual ~LatticeRegion();

    // Assignment (reference semantics).
    LatticeRegion& operator= (const LatticeRegion& other);

    // Make a copy of the object (reference semantics).
    virtual Lattice<Bool>* clone() const;

    // Is the LatticeRegion writable?
    virtual Bool isWritable() const;

    // Has the region a mask?
    Bool hasMask() const;

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

    // Get the LCRegion object describing the region.
    // Note that it does not contain strides, even if this LatticeRegion
    // object was constructed from a Slicer with strides. In that case
    // the region only defines the resulting shape.
    const LCRegion& region() const;

    // Get the Slicer object describing the region.
    // Note that it may contain strides.
    const Slicer& slicer() const;

    // Returns the shape of the LatticeRegion including all degenerate axes
    // (i.e. axes with a length of one).
    virtual IPosition shape() const;

    // Returns the number of axes in this LatticeRegion. This includes all
    // degenerate axes.
    virtual uInt ndim() const;

    // Returns the total number of elements in this LatticeRegion.
    virtual size_t nelements() const;

    // Check class internals - used for debugging. Should always return True
    virtual Bool ok() const;

    // This function is used by the LatticeIterator class to generate an
    // iterator of the correct type for this Lattice. Not recommended
    // for general use. 
    virtual LatticeIterInterface<Bool>*  makeIter
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

    // The following "put" functions are described in detail in class
    // <linkto class=Lattice>Lattice</linkto>.
    // They'll throw an exception is no mask is available or if
    // the mask is not writable.
    // <group>
    virtual void set (const Bool& value);
    virtual void apply (Bool (*function)(Bool));
    virtual void apply (Bool (*function)(const Bool&));
    virtual void apply (const Functional<Bool,Bool>& function);
    virtual void putAt (const Bool& value, const IPosition& where);
    virtual void copyData (const Lattice<Bool>& from);
    // </group>

    // Convert positions to positions in the parent object.
    // <group>
    Slicer convert (const Slicer& slicer) const;
    IPosition convert (const IPosition& position) const;
    // </group>

    // Do the actual getting of the mask.
    virtual Bool doGetSlice (Array<Bool>& buffer, const Slicer& section);

    // Do the actual putting of the mask. Only possible if region is writable.
    virtual void doPutSlice (const Array<Bool>& sourceBuffer,
			     const IPosition& where,
			     const IPosition& stride);


private:
    LCRegion*  itsRegion;
    Slicer     itsSlicer;
    Bool       itsHasRegionMask;
};


inline Bool LatticeRegion::hasMask() const
{
    return itsHasRegionMask;
}
inline const LCRegion& LatticeRegion::region() const
{
    return *itsRegion;
}
inline const Slicer& LatticeRegion::slicer() const
{
    return itsSlicer;
}



} //# NAMESPACE CASACORE - END

#endif
