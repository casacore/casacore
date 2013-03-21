//# TempLatticeImpl.h: A Lattice that can be used for temporary storage
//# Copyright (C) 1997,1998,1999,2000,2003
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
//# $Id: TempLatticeImpl.h 20739 2009-09-29 01:15:15Z Malte.Marquarding $

#ifndef LATTICES_TEMPLATTICEIMPL_H
#define LATTICES_TEMPLATTICEIMPL_H


//# Includes
#include <lattices/Lattices/Lattice.h>
#include <lattices/Lattices/TiledShape.h>
#include <tables/Tables/Table.h>
#include <casa/Utilities/CountedPtr.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class Table;


// <summary>
// The class implementing TempLattice
// </summary>

// <use visibility=local>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tTempLattice.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="TempLattice">Lattice</linkto>
// </prerequisite>

// <synopsis>
// The class is used as <src>CountedPtr<TempLatticeImpl></src> in class
// TempLattice. In that way the making a copy of a TempLattice uses the
// same object underneath.
// This was needed to have a correct implementation of tempClose. Otherwise
// when deleting a copy of a TempLattice, that destructor would delete the
// underlying table and the original TempLattice could not reopen it.
// </synopsis>


template<class T> class TempLatticeImpl
{
public:
  // The default constructor creates a TempLatticeImpl containing a
  // default ArrayLattice object.
  TempLatticeImpl();

  // Create a TempLatticeImpl of the specified shape. You can specify how much
  // memory the Lattice can consume before it becomes disk based by giving a
  // non-negative value to the maxMemoryInMB argument. Otherwise it will assume
  // it can use up to 25% of the memory on your machine as defined in aipsrc
  // (this algorithm may change). Setting maxMemoryInMB to zero will force
  // the lattice to disk.
  // <group>
  TempLatticeImpl (const TiledShape& shape, Int maxMemoryInMB);
  TempLatticeImpl (const TiledShape& shape, Double maxMemoryInMB);
  // </group>
  
  // The destructor removes the Lattice from memory and if necessary disk.
  ~TempLatticeImpl();

  // Is the TempLattice paged to disk?
  Bool isPaged() const
    { return  (! itsTableName.empty()); }

  // Can the lattice data be referenced as an array section?
  Bool canReferenceArray() const
    { return  (itsTableName.empty()); }

  // Is the TempLattice writable? It should be.
  Bool isWritable() const
    { return True; }

  // Flush the data.
  void flush()
    { if (itsTablePtr != 0) itsTablePtr->flush(); }

  // Close the Lattice temporarily (if it is paged to disk).
  // It'll be reopened automatically when needed or when
  // <src>reopen</src> is called explicitly.
  void tempClose();

  // If needed, reopen a temporarily closed TempLatticeImpl.
  void reopen();

  // Return the shape of the Lattice including all degenerate axes.
  // (ie. axes with a length of one)
  IPosition shape() const
    { doReopen(); return itsLatticePtr->shape(); } 

  // Set all of the elements in the Lattice to the given value.
  void set (const T& value)
    { doReopen(); itsLatticePtr->set (value); }

  // Replace every element, x, of the Lattice with the result of f(x).  You
  // must pass in the address of the function -- so the function must be
  // declared and defined in the scope of your program.  All versions of
  // apply require a function that accepts a single argument of type T (the
  // Lattice template type) and return a result of the same type.  The first
  // apply expects a function with an argument passed by value; the second
  // expects the argument to be passed by const reference; the third
  // requires an instance of the class <src>Functional<T,T></src>.  The
  // first form ought to run faster for the built-in types, which may be an
  // issue for large Lattices stored in memory, where disk access is not an
  // issue.
  // <group>
  void apply (T (*function)(T))
    { doReopen(); itsLatticePtr->apply (function); }
  void apply (T (*function)(const T&))
    { doReopen(); itsLatticePtr->apply (function); }
  void apply (const Functional<T,T>& function)
    { doReopen(); itsLatticePtr->apply (function); }
  // </group>

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  uInt advisedMaxPixels() const
    { doReopen(); return itsLatticePtr->advisedMaxPixels(); }

  // Get the best cursor shape.
  IPosition doNiceCursorShape (uInt maxPixels) 
    { doReopen(); return itsLatticePtr->niceCursorShape (maxPixels); }

  // Maximum size - not necessarily all used. In pixels.
  uInt maximumCacheSize() const
    { return itsLatticePtr->maximumCacheSize(); }

  // Set the maximum (allowed) cache size as indicated.
  void setMaximumCacheSize (uInt howManyPixels)
    { itsLatticePtr->setMaximumCacheSize (howManyPixels); }

  // Set the cache size as to "fit" the indicated path.
  void setCacheSizeFromPath (const IPosition& sliceShape,
  			             const IPosition& windowStart,
			             const IPosition& windowLength,
			             const IPosition& axisPath)
    { itsLatticePtr->setCacheSizeFromPath (sliceShape, windowStart, windowLength,
                                           axisPath); }
    
  // Set the actual cache size for this Array to be be big enough for the
  // indicated number of tiles. This cache is not shared with PagedArrays
  // in other rows and is always clipped to be less than the maximum value
  // set using the setMaximumCacheSize member function.
  // tiles. Tiles are cached using a first in first out algorithm. 
  void setCacheSizeInTiles (uInt howManyTiles)
    { itsLatticePtr->setCacheSizeInTiles (howManyTiles); }

  // Clears and frees up the caches, but the maximum allowed cache size is 
  // unchanged from when setCacheSize was called
  void clearCache()
    { itsLatticePtr->clearCache(); }

  // Report on cache success.
  void showCacheStatistics (ostream& os) const
    { itsLatticePtr->showCacheStatistics (os); }

  // Get or put a single element in the lattice.
  // Note that Lattice::operator() can also be used to get a single element.
  // <group>
  T getAt (const IPosition& where) const
    { doReopen(); return itsLatticePtr->getAt (where); }
  void putAt (const T& value, const IPosition& where)
    { doReopen(); itsLatticePtr->putAt (value, where); }
  // </group>
  
  // Check class internals - used for debugging. Should always return True
  Bool ok() const
    { doReopen(); return itsLatticePtr->ok(); }

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for this Lattice. Not recommended
  // for general use. 
  LatticeIterInterface<T>* makeIter (const LatticeNavigator& navigator,
                                     Bool useRef) const
    { doReopen(); return itsLatticePtr->makeIter (navigator, useRef); }

  // Do the actual getting of an array of values.
  Bool doGetSlice (Array<T>& buffer, const Slicer& section)
    { doReopen(); return itsLatticePtr->doGetSlice (buffer, section); }

  // Do the actual getting of an array of values.
  void doPutSlice (const Array<T>& sourceBuffer,
                   const IPosition& where,
                   const IPosition& stride)
    { doReopen(); itsLatticePtr->putSlice (sourceBuffer, where, stride); }
  
  // Do the reopen of the table (if not open already).
  void doReopen() const
    { if (itsIsClosed) tempReopen(); }

private:
  // The copy constructor cannot be used.
  TempLatticeImpl (const TempLatticeImpl<T>& other) ;
    
  // The assignment operator cannot be used.
  TempLatticeImpl<T>& operator= (const TempLatticeImpl<T>& other);

  // Initialize the object.
  void init (const TiledShape& shape, Double maxMemoryInMB=-1);

  // Do the actual reopen of the temporarily closed table (if not open already).
  void tempReopen() const;

  // Make sure that the temporary table gets deleted.
  void deleteTable();


  mutable Table*                  itsTablePtr;
  mutable CountedPtr<Lattice<T> > itsLatticePtr;
          String                  itsTableName;
  mutable Bool                    itsIsClosed;
};



} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <lattices/Lattices/TempLatticeImpl.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
