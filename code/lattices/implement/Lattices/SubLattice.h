//# SubLattice.h: A subset of a Lattice or MaskedLattice
//# Copyright (C) 1997,1998,1999,2000
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

#if !defined(AIPS_SUBLATTICE_H)
#define AIPS_SUBLATTICE_H


//# Includes
#include <aips/aips.h>
#include <trial/Lattices/MaskedLattice.h>
#include <trial/Lattices/LatticeRegion.h>

//# Forward Declarations


// <summary>
// A subset of a Lattice or MaskedLattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LatticeRegion">LatticeRegion</linkto>
// </prerequisite>

// <synopsis>
// A SubLattice is a lattice referencing a subset of another lattice
// by means of a <linkto class="Slicer">Slicer</linkto> object.
// <br>It is useful when only a subset of a lattice needs to be accessed.
// <p>
// When the SubLattice is created from a const <src>Lattice</src> object,
// it is not writable, thus it can only be used as an rvalue.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <templating arg=T>
// Any type that can be used by the Tables System can also be used by
// this class.
// </templating>

//# <todo asof="yyyy/mm/dd">
//# </todo>

template<class T> class SubLattice: public MaskedLattice<T>
{
public:
  // The default constructor creates a SubLattice that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  SubLattice();

  // Create a SubLattice from a Lattice.
  // This results in a SubLattice without a real mask.
  // <br>The "const Lattice" version yields a non-writable SubLattice,
  // while for the non-const version one has to specify if the SubLattice
  // should be writable (if the original lattice is non-writable, the
  // SubLattice is always set to non-writable).
  // <group>
  SubLattice (const Lattice<T>& lattice);
  SubLattice (Lattice<T>& lattice, Bool writableIfPossible);
  // </group>

  // Create a SubLattice from a MaskedLattice.
  // <br>The "const MaskedLattice" version yields a non-writable SubLattice,
  // while for the non-const version one has to specify if the SubLattice
  // should be writable (if the original lattice is non-writable, the
  // SubLattice is always set to non-writable).
  // <group>
  SubLattice (const MaskedLattice<T>& lattice);
  SubLattice (MaskedLattice<T>& lattice, Bool writableIfPossible);
  // </group>

  // Create a SubLattice from the given MaskedLattice and region.
  // Note that the region can be constructed from an
  // <linkto class=LCRegion>LCRegion</linkto> object or 
  // <linkto class=Slicer>Slicer</linkto> object (with an optional stride).
  // <br>An exception is thrown if the lattice shape used in the region
  // differs from the shape of the lattice.
  // <group>
  SubLattice (const Lattice<T>& lattice, const LatticeRegion& region);
  SubLattice (Lattice<T>& lattice, const LatticeRegion& region,
	      Bool writableIfPossible);
  SubLattice (const MaskedLattice<T>& lattice, const LatticeRegion& region);
  SubLattice (MaskedLattice<T>& lattice, const LatticeRegion& region,
	      Bool writableIfPossible);
  // </group>
  
  // Create a SubLattice from the given (Masked)Lattice and slicer.
  // The slicer can be strided.
  // <br>An exception is thrown if the slicer exceeds the lattice shape.
  // <group>
  SubLattice (const Lattice<T>& lattice, const Slicer& slicer);
  SubLattice (Lattice<T>& lattice, const Slicer& slicer,
	      Bool writableIfPossible);
  SubLattice (const MaskedLattice<T>& lattice, const Slicer& slicer);
  SubLattice (MaskedLattice<T>& lattice, const Slicer& slicer,
  	      Bool writableIfPossible);
  // </group>
  
  // Copy constructor (reference semantics).
  SubLattice (const SubLattice<T>& other);
    
  virtual ~SubLattice();

  // Assignment (reference semantics).
  SubLattice<T>& operator= (const SubLattice<T>& other);

  // Make a copy of the object (reference semantics).
  virtual MaskedLattice<T>* cloneML() const;

  // Is the lattice masked?
  // It is if its parent lattice or its region is masked.
  virtual Bool isMasked() const;

  // A SubLattice is persistent if no region is applied to the parent lattice.
  // That is true if the region has the same shape as the parent lattice
  // and the region has no mask.
  virtual Bool isPersistent() const;

  // Is the SubLattice paged to disk?
  virtual Bool isPaged() const;

  // Is the SubLattice writable?
  virtual Bool isWritable() const;

  // Handle locking of the SubLattice which is delegated to its parent.
  // <br>It is strongly recommended to use class
  // <linkto class=LatticeLocker>LatticeLocker</linkto> to
  // handle lattice locking. It also contains a more detailed
  // explanation of the locking process.
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

  // Does the SubLattice have a pixelmask?
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask.
  // An exception is thrown if the SubLattice does not have a pixelmask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Get a pointer the region/mask object describing this sublattice.
  virtual const LatticeRegion* getRegionPtr() const;

  // Returns the shape of the SubLattice including all degenerate axes
  // (i.e. axes with a length of one).
  virtual IPosition shape() const;
  
  // Return the name of the parent lattice.
  virtual String name (const Bool stripPath=False) const;

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt advisedMaxPixels() const;

  // Get or put a single element in the lattice.
  // <group>
  virtual T getAt (const IPosition& where) const;
  virtual void putAt (const T& value, const IPosition& where);
  // </group>
  
  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;

  // This function is used by the LatticeIterator class to generate an
  // iterator of the correct type for this Lattice. Not recommended
  // for general use. 
  virtual LatticeIterInterface<T>*
                      makeIter (const LatticeNavigator& navigator) const;

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

protected:
  // Set the various pointer needed to construct the object.
  // One of the pointers should be zero.
  // It takes over the pointer and deletes the object in the destructor.
  void setPtr (Lattice<T>* latticePtr,
	       MaskedLattice<T>* maskLatPtr,
	       Bool writableIfPossible);

  // Set the region object.
  // It also fills in the parent pointer when the SubLattice is taken
  // from a MaskedLattice.
  // The default region is the entire lattice.
  // <group>
  void setRegion (const LatticeRegion& region);
  void setRegion (const Slicer& slicer);
  void setRegion();
  // </group>


private:
  Lattice<T>*       itsLatticePtr;
  MaskedLattice<T>* itsMaskLatPtr;
  LatticeRegion     itsRegion;
  Bool              itsWritable;
  Bool              itsHasPixelMask;
  SubLattice<Bool>* itsPixelMask;
};


#endif
