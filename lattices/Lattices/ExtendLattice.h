//# ExtendLattice.h: A subset of a Lattice or MaskedLattice
//# Copyright (C) 2001,2003
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

#ifndef LATTICES_EXTENDLATTICE_H
#define LATTICES_EXTENDLATTICE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/casa/Arrays/ExtendSpecifier.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// An extension of a Lattice or MaskedLattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="LatticeRegion">LatticeRegion</linkto>
// </prerequisite>

// <synopsis>
// An ExtendLattice is a lattice virtually extending another lattice
// by stretching axes with length 1 and/or by adding new axes.
// It is useful for e.g. LEL to have the same shapes for lattices.
// An ExtendLattice is not writable (since many pixels map to the same
// underlying pixel).
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>

// <templating arg=T>
//  <li> Any type that can be used by the Tables System can also be used by
//       this class.
// </templating>

//# <todo asof="yyyy/mm/dd">
//# </todo>

template<class T> class ExtendLattice: public MaskedLattice<T>
{
public:
  // The default constructor creates a ExtendLattice that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  ExtendLattice();

  // Create a ExtendLattice from a Lattice.
  // <br><src>newShape</src> gives the new shape.
  // <br><src>newAxes</src> gives the new axes in newShape.
  // <br><src>stretchAxes</src> gives the stretched axes in newShape.
  // <br>E.g. lattice has shape [32,1,5,1], newShape=[32,1,4,5,10],
  // newAxes=[2], and stretchAxes=[4]. It means that axes 2 in the newShape
  // is a new axes and that axes 4 in the new shape is stretched. The other
  // axes in the new shape have to match the other axes in the old shape.
  // Note that stretched axes have to have length 1 in the old shape.
  // <group>
  ExtendLattice (const Lattice<T>& lattice, const IPosition& newShape,
		 const IPosition& extendAxes, const IPosition& stretchAxes);
  ExtendLattice (const MaskedLattice<T>& lattice, const IPosition& newShape,
		 const IPosition& newAxes, const IPosition& stretchAxes);
  // </group>

  // Copy constructor (reference semantics).
  ExtendLattice (const ExtendLattice<T>& other);
    
  virtual ~ExtendLattice();

  // Assignment (reference semantics).
  ExtendLattice<T>& operator= (const ExtendLattice<T>& other);

  // Make a copy of the object (reference semantics).
  virtual MaskedLattice<T>* cloneML() const;

  // Is the lattice masked?
  // It is if its parent lattice is masked.
  virtual Bool isMasked() const;

  // An ExtendLattice is not persistent.
  virtual Bool isPersistent() const;

  // Is the ExtendLattice paged to disk?
  virtual Bool isPaged() const;

  // An ExtendLattice is not writable.
  virtual Bool isWritable() const;

  // Handle locking of the ExtendLattice which is delegated to its parent.
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
  virtual void resync();

  // Flush the data.
  virtual void flush();

  // Close the Lattice temporarily (if it is paged to disk).
  // It'll be reopened automatically when needed or when
  // <src>reopen</src> is called explicitly.
  virtual void tempClose();

  // If needed, reopen a temporarily closed Lattice.
  virtual void reopen();

  // Does the ExtendLattice have a pixelmask?
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask.
  // An exception is thrown if the ExtendLattice does not have a pixelmask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Get the region used (always returns 0).
  virtual const LatticeRegion* getRegionPtr() const;
 
  // Returns the shape of the ExtendLattice.
  virtual IPosition shape() const;
  
  // Return the name of the parent lattice.
  virtual String name (Bool stripPath=False) const;

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt advisedMaxPixels() const;

  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Putting data is not possible.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
  // Get a section of the mask.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Get the best cursor shape.
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;

private:
  // Set the various pointer needed to construct the object.
  // One of the pointers should be zero.
  // It takes over the pointer and deletes the object in the destructor.
  void setPtr (Lattice<T>* latticePtr, MaskedLattice<T>* maskLatPtr);

  // Get mask data from mask.
  Bool getMaskDataSlice (Array<Bool>& buffer, const Slicer& section);

  Lattice<T>*          itsLatticePtr;
  MaskedLattice<T>*    itsMaskLatPtr;
  Bool                 itsHasPixelMask;
  ExtendLattice<Bool>* itsPixelMask;
  ExtendSpecifier      itsExtendSpec;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/ExtendLattice.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
