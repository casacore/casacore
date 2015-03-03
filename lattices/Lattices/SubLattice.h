//# SubLattice.h: A subset of a Lattice or MaskedLattice
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
//# $Id$

#ifndef LATTICES_SUBLATTICE_H
#define LATTICES_SUBLATTICE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/LRegions/LatticeRegion.h>
#include <casacore/casa/Arrays/AxesSpecifier.h>
#include <casacore/casa/Arrays/AxesMapping.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// <p>
// Using an <linkto class=AxesSpecifier>AxesSpecifier</linkto> object
// it is possible to remove some or all degenerate axes (i.e. axes
// with length 1) to get a lattice with a lower dimensionality.
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
  // <note>In the 2nd case the lattice could have been declared const,
  // but is not to indicate it can be changed. </note>
  // <group>
  SubLattice (const Lattice<T>& lattice, AxesSpecifier=AxesSpecifier());
  SubLattice (Lattice<T>& lattice, Bool writableIfPossible,
	      AxesSpecifier=AxesSpecifier());
  // </group>

  // Create a SubLattice from a MaskedLattice.
  // <br>The "const MaskedLattice" version yields a non-writable SubLattice,
  // while for the non-const version one has to specify if the SubLattice
  // should be writable (if the original lattice is non-writable, the
  // SubLattice is always set to non-writable).
  // <note>In the 2nd case the lattice could have been declared const,
  // but is not to indicate it can be changed. </note>
  // <group>
  SubLattice (const MaskedLattice<T>& lattice, AxesSpecifier=AxesSpecifier());
  SubLattice (MaskedLattice<T>& lattice, Bool writableIfPossible,
	      AxesSpecifier=AxesSpecifier());
  // </group>

  // Create a SubLattice from the given MaskedLattice and region.
  // Note that the region can be constructed from an
  // <linkto class=LCRegion>LCRegion</linkto> object or 
  // <linkto class=Slicer>Slicer</linkto> object (with an optional stride).
  // <br>An exception is thrown if the lattice shape used in the region
  // differs from the shape of the lattice.
  // <note>In the 2nd and 4th case the lattice could have been declared const,
  // but is not to indicate it can be changed. </note>
  // <group>
  SubLattice (const Lattice<T>& lattice, const LatticeRegion& region,
	      AxesSpecifier=AxesSpecifier());
  SubLattice (Lattice<T>& lattice, const LatticeRegion& region,
	      Bool writableIfPossible, AxesSpecifier=AxesSpecifier());
  SubLattice (const MaskedLattice<T>& lattice, const LatticeRegion& region,
	      AxesSpecifier=AxesSpecifier());
  SubLattice (MaskedLattice<T>& lattice, const LatticeRegion& region,
	      Bool writableIfPossible, AxesSpecifier=AxesSpecifier());
  // </group>
  
  // Create a SubLattice from the given (Masked)Lattice and slicer.
  // The slicer can be strided.
  // <br>An exception is thrown if the slicer exceeds the lattice shape.
  // <note>In the 2nd and 4th case the lattice could have been declared const,
  // but is not to indicate it can be changed. </note>
  // <group>
  SubLattice (const Lattice<T>& lattice, const Slicer& slicer,
	      AxesSpecifier=AxesSpecifier());
  SubLattice (Lattice<T>& lattice, const Slicer& slicer,
	      Bool writableIfPossible, AxesSpecifier=AxesSpecifier());
  SubLattice (const MaskedLattice<T>& lattice, const Slicer& slicer,
	      AxesSpecifier=AxesSpecifier());
  SubLattice (MaskedLattice<T>& lattice, const Slicer& slicer,
  	      Bool writableIfPossible, AxesSpecifier=AxesSpecifier());
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

  // Can the lattice data be referenced as an array section?
  virtual Bool canReferenceArray() const;

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
  virtual void resync();

  // Flush the data.
  virtual void flush();

  // Close the Lattice temporarily (if it is paged to disk).
  // It'll be reopened automatically when needed or when
  // <src>reopen</src> is called explicitly.
  virtual void tempClose();

  // If needed, reopen a temporarily closed Lattice.
  virtual void reopen();

  // Does the SubLattice have a pixelmask?
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask.
  // An exception is thrown if the SubLattice does not have a pixelmask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Use the given mask as the pixelmask.
  // If another mask was already used, the new one will be used instead.
  // It checks if its shape matches the shape of the sublattice.
  // <br>If <code>mayExist=False</code>, setting the pixelmask is only
  // possible if the underlying lattice does not have a pixelmask.
  // <br>If <code>mayExist=True</code>, the resulting pixelmask is the
  // AND of the given pixelmask and the pixelmask of the underlying lattice.
  void setPixelMask (const Lattice<Bool>& pixelMask, Bool mayExist);

  // Get a pointer the region/mask object describing this sublattice.
  virtual const LatticeRegion* getRegionPtr() const;

  // Returns the shape of the SubLattice including all degenerate axes
  // (i.e. axes with a length of one).
  virtual IPosition shape() const;
  
  // Return the name of the parent lattice.
  virtual String name (Bool stripPath=False) const;

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
  virtual LatticeIterInterface<T>* makeIter (const LatticeNavigator& navigator,
					     Bool useRef) const;

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

  // Set the axes mapping from the specification.
  const AxesMapping& getAxesMap() const
    { return itsAxesMap; }

  // Convert the specified position in the sublattice to the corresponding
  // position in the parent lattice.
  IPosition positionInParent(const IPosition& subLatticePosition) const
  {
    if (itsAxesMap.isRemoved()) {
      return itsRegion.convert (itsAxesMap.posToOld(subLatticePosition));
    } else {
      return itsRegion.convert (subLatticePosition);
    }
  }
    
  // Set the region object using a slicer.
  // Allows the region to be changed while keeping
  // the same lattice, so that new SubLattice objects do not have to be
  // created when one only wants to change the region of interest. Should
  // only be called when performance is an issue; otherwise, just create
  // a new SubLattice<T> object.
  void setRegion (const Slicer& slicer);

protected:
  // Set the region object.
  // It also fills in the parent pointer when the SubLattice is taken
  // from a MaskedLattice.
  // The default region is the entire lattice.
  // <group>
  void setRegion (const LatticeRegion& region);
  void setRegion();
  // </group>

  // Set the various pointers needed to construct the object.
  // One of the pointers should be zero.
  // It takes over the pointer and deletes the object in the destructor.
  void setPtr (Lattice<T>* latticePtr,
	       MaskedLattice<T>* maskLatPtr,
	       Bool writableIfPossible);

  // Set the axes mapping from the specification.
  void setAxesMap (const AxesSpecifier&);


private:
  // Get mask data from region and mask.
  // <group>
  Bool getRegionDataSlice (Array<Bool>& buffer, const Slicer& section);
  Bool getMaskDataSlice (Array<Bool>& buffer, const Slicer& section);
  // </group>

  // And tmpbuf into buffer. If buffer is a reference, first a copy is made.
  void andMask (Array<Bool>& buffer, Bool ref,
		const Array<Bool>& tmpbuf) const;

  Lattice<T>*       itsLatticePtr;
  MaskedLattice<T>* itsMaskLatPtr;
  LatticeRegion     itsRegion;
  Bool              itsWritable;
  Bool              itsHasLattPMask;   //# has underlying lattice a pixelmask?
  Lattice<Bool>*    itsPixelMask;      //# AND of lattice and own pixelmask
  Lattice<Bool>*    itsOwnPixelMask;   //# own pixelmask
  AxesSpecifier     itsAxesSpec;
  AxesMapping       itsAxesMap;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/SubLattice.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
