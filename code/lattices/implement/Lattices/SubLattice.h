//# SubLattice.h: A subset of a Lattice or MaskedLattice
//# Copyright (C) 1997,1998
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
  // <group>
  virtual Lattice<T>* clone() const;
  virtual MaskedLattice<T>* cloneML() const;
  // </group>

  // Is the SubLattice paged to disk?
  virtual Bool isPaged() const;

  // Is the SubLattice writable?
  virtual Bool isWritable() const;

  // Is the SubLattice really masked?
  // False means that it is only a rectangular box and that it is
  // not needed to look at the mask.
  virtual Bool isMasked() const;

  // Get the region/mask object describing this sublattice.
  virtual const LatticeRegion& region() const;

  // Returns the shape of the SubLattice including all degenerate axes
  // (i.e. axes with a length of one).
  virtual IPosition shape() const;
  
  // Returns the number of axes in this SubLattice. This includes all
  // degenerate axes.
  virtual uInt ndim() const;
  
  // Returns the total number of elements in this SubLattice.
  virtual uInt nelements() const;
  
  // returns a value of "True" if this instance of Lattice and 'other' have 
  // the same shape, otherwise returns a value of "False".
  virtual Bool conform (const Lattice<T>& other) const;
  
  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt maxPixels() const;

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

  // Do the actual get of the mask data.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

  // Do the actual getting of an array of values.
  virtual void doPutSlice (const Array<T>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);
  
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
};


#endif
