//# SubLattice.h: A subset of a Lattice
//# Copyright (C) 1997
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
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/PixelRegion.h>

//# Forward Declarations


// <summary>
// A subset of a Lattice
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice">Lattice</linkto>
//   <li> <linkto class="PixelRegion">PixelRegion</linkto>
// </prerequisite>

// <synopsis>
// A SubLattice is a lattice referencing a subset of another lattice
// by means of a <linkto class="PixelRegion">PixelRegion</linkto> object.
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

template<class T> class SubLattice: public Lattice<T>
{
public:
  // The default constructor creates a SubLattice that is useless for just
  // about everything, except that it can be assigned to with the assignment
  // operator.
  SubLattice();

  // Create a SubLattice from the given lattice and region.
  // The "const Lattice" version yields a non-writable SubLattice,
  // while for the non-const version one has to specify if the SubLattice
  // should be writable (if the original lattice is non-writable, the
  // SubLattice is always set to non-writable).
  // <br>An exception is thrown if the lattice shape used in the region
  // differs from the shape of the lattice.
  // <group>
  SubLattice (const Lattice<T>& lattice, const PixelRegion& region);
  SubLattice (Lattice<T>& lattice, const PixelRegion& region,
	      Bool writableIfPossible);
  // </group>
  
  // Copy constructor (reference semantics).
  SubLattice (const SubLattice<T>& other);
    
  virtual ~SubLattice();

  // Assignment (reference semantics).
  SubLattice<T>& operator= (const SubLattice<T>& other);

  // Make a copy of the object (reference semantics).
  virtual Lattice<T>* clone() const;

  // Is the SubLattice writable?
  virtual Bool isWritable() const;

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
  
  // Functions which extract an Array of values from a Lattice. All the
  // IPosition arguments must have the same number of axes as the underlying
  // Lattice, otherwise, an exception is thrown. <br>
  // The parameters are:
  // <ul>
  // <li> buffer: a <src>COWPtr<Array<T>></src> or an <src>Array<T></src>. See
  //      the <linkto class="Lattice">Lattice</linkto> class for an example of
  //      how to use a COWPtr.
  // <li> start: The starting position (or Bottom Left Corner), within 
  //      the Lattice, of the data to be extracted.
  // <li> shape: The shape of the data to be extracted.  This is not a
  //      position within the Lattice but the actual shape the buffer will 
  //      have after this function is called.  This argument added
  //      to the "start" argument minus one should be the "Top Right Corner".
  // <li> stride: The increment for each axis.  A stride of
  //      one will return every data element, a stride of two will return
  //      every other element.  The IPosition elements may be different for
  //      each respective axis.  Thus, a stride of IPosition(3,1,2,3) says:
  //      fill the buffer with every element whose position has a first 
  //      index between start(0) and start(0)+shape(0), a second index
  //      which is every other element between start(1) and 
  //      (start(1)+shape(1))*2, and a third index of every third element 
  //      between start(2) and (start(2)+shape(2))*3.
  // <li> section: An alternate way of specifying the start, shape and stride
  // <li> removeDegenerateAxes: a Bool which dictates whether to remove 
  //      "empty" axis created in buffer. (e.g. extracting an n-dimensional 
  //      from an (n+1)-dimensional will fill 'buffer' with an array that 
  //      has a degenerate axis (i.e. one axis will have a length = 1.) 
  //      Setting removeDegenerateAxes = True will return a buffer with 
  //      a shape that doesn't reflect these superfluous axes.)
  // </ul>
  // These functions return 'True' if "buffer" is a reference to Lattice data
  // and 'False' if it is a copy.
  // <group>   
  virtual Bool getSlice (COWPtr<Array<T> >& buffer, const IPosition& start,
			 const IPosition& shape, const IPosition& stride,
			 Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice (COWPtr<Array<T> >& buffer, const Slicer& section,
			 Bool removeDegenerateAxes=False) const;
  virtual Bool getSlice (Array<T>& buffer, const IPosition& start, 
			 const IPosition& shape, const IPosition& stride, 
			 Bool removeDegenerateAxes=False);
  virtual Bool getSlice (Array<T>& buffer, const Slicer& section, 
			 Bool removeDegenerateAxes=False);
  // </group>   
  
  // A function which places an Array of values within this instance of the
  // Lattice at the location specified by the IPosition "where", incrementing 
  // by "stride".  All of the IPosition arguments must be of the same
  // dimensionality as the Lattice.  The sourceBuffer array may (and probably
  // will) have less axes than the Lattice. The stride defaults to one if
  // not specified. 
  // <group>   
  virtual void putSlice (const Array<T>& sourceBuffer, const IPosition& where,
			 const IPosition& stride);
  virtual void putSlice (const Array<T>& sourceBuffer, const IPosition& where);
  // </group>   
  
  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator.
  virtual uInt maxPixels() const;

  // Help the user pick a cursor for most efficient access if they only want
  // pixel values and don't care about the order or dimension of the
  // cursor. Usually the tile shape is the best cursor shape, and this can
  // be obtained using:<br>
  // <src>IPosition shape = pa.niceCursorShape()</src> where
  // <src>pa</src> is a PagedArray object.
  // <br>The default argument is the result of <src>maxPixels()</src>.
  // <group>
  virtual IPosition niceCursorShape (uInt maxPixels) const;
  IPosition niceCursorShape() const
    { return niceCursorShape (maxPixels()); }
  // </group>

  // Get a put a single element in the lattice.
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

private:
  Lattice<T>*  itsLatticePtr;
  PixelRegion* itsRegionPtr;
  Bool         itsWritable;
};


#endif
