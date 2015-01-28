//# LatticeBase.h: A non-templated, abstract base class for array-like classes
//# Copyright (C) 1999,2000,2003
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

#ifndef LATTICES_LATTICEBASE_H
#define LATTICES_LATTICEBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LEL/LELCoordinates.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/IO/FileLocker.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LogIO;


// <summary>
// A non-templated, abstract base class for array-like objects.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Bob Garwood" date="2000/01/18" tests="tArrayLattice.cc" demos="dLattice.cc">
// </reviewed>

// <synopsis>
// This pure abstract base class defines the operations which may be
// performed on a lattice of any type.
// <br>See class <linkto class=Lattice>Lattice</linkto> for a detailed
// description of a lattice.
// </synopsis> 

// <motivation>
// It is very useful to be able to keep a pointer to a
// non-templated base class. Furthermore it gives the opportunity to
// factor out some non-templated code.
// </motivation>

// <note>
// The cache functions (maximumCacheSize, setMaximumCacheSize,
// setCacheSizeInTiles, setCacheSizeFromPath, clearCache, and
// showCacheStatistics) should all be over-ridden together as
// in PagedArray.
// </note>
//
//# <todo asof="1999/02/04">
//#   <li>
//# </todo>


class LatticeBase
{
public: 
  // A virtual destructor is needed so that it will use the actual destructor
  // in the derived class.
  virtual ~LatticeBase();

  // Make a copy of the derived object (reference semantics).
  virtual LatticeBase* clone() const = 0;

  // Get the image type (returns name of derived class).
  // The default implementation returns "Lattice".
  // Note it is made pure virtual in ImageInterface.
  virtual String imageType() const;

  // Get the data type of the lattice.
  virtual DataType dataType() const = 0;

  // Is the lattice persistent and can it be loaded by other processes as well?
  // That is the case for a PagedArray or PagedImage and for an ImageExpr
  // which does not use transient lattices or regions.
  // <br>The default implementation returns False.
  virtual Bool isPersistent() const;

  // Is the lattice paged to disk?
  // <br>The default implementation returns False.
  virtual Bool isPaged() const;

  // Can the lattice data be referenced as an array section?
  // That is the case for an ArrayLattice or a Temp/SubLattice using it.
  // It is used by LatticeIterInterface.
  // <br>The default implementation returns False.
  virtual Bool canReferenceArray() const;

  // Is the lattice writable?
  // <br>The default implementation returns True.
  virtual Bool isWritable() const;

  // Save the image in an AipsIO file with the given name.
  // Its purpose is to make ImageConcat and ImageExpr objects
  // persistent.
  // <br>The default implementation throws an exception.
  virtual void save (const String& fileName) const;

  // It is strongly recommended to use class
  // <linkto class=LatticeLocker>LatticeLocker</linkto> to
  // handle lattice locking. It also contains a more detailed
  // explanation of the locking process.
  // <br>By default the functions do not do anything at all.
  // lock() and hasLock return True, which is suitable for all
  // non-paged lattices.
  // <group>
  virtual Bool lock (FileLocker::LockType, uInt nattempts);
  virtual void unlock();
  virtual Bool hasLock (FileLocker::LockType) const;
  // </group>

  // Resynchronize the Lattice object with the lattice file.
  // This function is only useful if no read-locking is used, ie.
  // if the table lock option is UserNoReadLocking or AutoNoReadLocking.
  // In that cases the table system does not acquire a read-lock, thus
  // does not synchronize itself automatically.
  // <br>By default the function does not do anything at all.
  virtual void resync();

  // Flush the data (but do not unlock).
  // <br>By default the function does not do anything at all.
  virtual void flush();

  // Temporarily close the lattice.
  // It will be reopened automatically on the next access.
  // <br>By default the function does not do anything at all.
  virtual void tempClose();

  // Explicitly reopen the temporarily closed lattice.
  // <br>By default the function does not do anything at all.
  virtual void reopen();

  // Return the name of the current Lattice object. This will generally 
  // be a file name for lattices that have a persistent form.  Any path
  // before the actual file name can be optionally stripped off.
  // <br>The default implementation returns an empty string.
  virtual String name (Bool stripPath=False) const;

  // Return the shape of the Lattice including all degenerate axes
  // (ie. axes with a length of one)
  virtual IPosition shape() const = 0;
  
  // Return the number of axes in this Lattice. This includes all
  // degenerate axes.
  // <br>The default implementation returns shape().nelements().
  virtual uInt ndim() const;
  
  // Return the total number of elements in this Lattice.
  // <br>The default implementation returns shape().product().
  // <group>
  virtual size_t nelements() const;
  size_t size() const
    { return nelements(); }
  // </group>

  // Return a value of "True" if this instance of Lattice and 'other' have 
  // the same shape, otherwise returns a value of "False".
  Bool conform (const LatticeBase& other) const
    { return shape().isEqual (other.shape()); }

  // Return the coordinates of the lattice.
  // <br>The default implementation returns an 'empty' LELLattCoord object.
  virtual LELCoordinates lelCoordinates() const;

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator. The Lattice class has a default
  // implementation which returns a number that is a power of two and
  // includes enough pixels to consume between 4 and 8 MBytes of memory.
  virtual uInt advisedMaxPixels() const = 0;

  // Returns a recommended cursor shape for iterating through all the pixels
  // in the Lattice. The default implementation sets up a shape that
  // completely fills as many axes as possible, but always at least the
  // first axis. For example, given a 10x20x30 Lattice 
  // <srcblock>
  // maxPixels = 1     --> niceCursorShape = [10,1,1]
  //             100   --> niceCursorShape = [10,1,1]
  //             300   --> niceCursorShape = [10,20,1] 
  //             10000 --> niceCursorShape = [10,20,30] 
  // </srcblock>
  // The default argument is the result of <src>advisedMaxPixels()</src>.
  // <group>
  IPosition niceCursorShape (uInt maxPixels) const
    { return doNiceCursorShape (maxPixels); }
  IPosition niceCursorShape() const
    { return doNiceCursorShape (advisedMaxPixels()); }
  // </group>

  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;

  // The function (in the derived classes) doing the actual work.
  // This function is public, so it can be used internally in the
  // various Lattice classes.
  // <br>The default implementation tries to fit as many axes
  // as possible given <src>maxPixels</src>.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

  // Maximum cache size - not necessarily all used. In pixels.
  // Default returns 0, which means that there is no maximum.
  virtual uInt maximumCacheSize() const;

  // Set the maximum (allowed) cache size as indicated.
  // <br>The default implementation does nothing.
  virtual void setMaximumCacheSize (uInt howManyPixels);

  // Set the actual cache size for this Array to be big enough for the
  // indicated number of tiles. This cache is not shared with PagedArrays
  // in other rows and is always clipped to be less than the maximum value
  // set using the setMaximumCacheSize member function.
  // Tiles are cached using a first in first out algorithm.
  // <br>The default implementation does nothing.
  virtual void setCacheSizeInTiles (uInt howManyTiles);

  // Set the cache size as to "fit" the indicated path.
  // <br>The default implementation does nothing.
  virtual void setCacheSizeFromPath (const IPosition& sliceShape,
				     const IPosition& windowStart,
				     const IPosition& windowLength,
				     const IPosition& axisPath);

  // Clears and frees up the caches, but the maximum allowed cache size is
  // unchanged from when setCacheSize was called.
  // <br>The default implementation does nothing.
  virtual void clearCache();

  // Report on cache success.
  // <br>The default implementation does nothing.
  virtual void showCacheStatistics (ostream& os) const;


protected:
  // Define default constructor to be used by derived classes.
  LatticeBase() {};

  // Copy constructor and assignment can only be used by derived classes.
  // <group>
  LatticeBase (const LatticeBase&) {};
  LatticeBase& operator= (const LatticeBase&)
    { return *this; }
  // </group>

  // Throw an exception for arithmetic on a Bool Lattice.
  void throwBoolMath() const;
};



} //# NAMESPACE CASACORE - END

#endif
