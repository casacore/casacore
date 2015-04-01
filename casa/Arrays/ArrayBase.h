//# ArrayBase.h: Non-templated base class for templated Array class
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
//# Associated Universities, Inc. Washington DC, USA,
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

#ifndef CASA_ARRAYBASE_H
#define CASA_ARRAYBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations.
class ArrayPositionIterator;
class Slicer;


// <summary>
// A global enum used by some Array constructors.
// </summary>
// <synopsis>
// StorageInitPolicy is used in functions where an array is formed from
// a shape and an ordinary pointer. This enum should be in Array but that
// causes gcc to be unhappy.
// </synopsis>
enum StorageInitPolicy {
  // COPY is used when an internal copy of the storage is to be made.
  // The array is NOT responsible for deleting the external storage.
  COPY,
  // TAKE_OVER is used to indicate that the Array should just use the
  // external storage (i.e., no copy is made). The Array class is now
  // responsible for deleting the storage (hence it must have come from
  // a call to new[]).
  TAKE_OVER,
  // Share means that the Array will just use the pointer (no copy), however
  // the Array will NOT delete it upon destruction.
  SHARE};


// <summary>
// Non-templated base class for templated Array class.
// </summary>

// ArrayBase is only used to factor out common code from the templated
// Array class.

class ArrayBase
{
public:
  ArrayBase();

  // Create an array of the given shape, i.e. after construction
  // array.ndim() == shape.nelements() and array.shape() == shape.
  // The origin of the Array is zero.
  explicit ArrayBase (const IPosition& shape);

  // Copy constructor.
  ArrayBase (const ArrayBase& other);

  // Assignment.
  ArrayBase& operator= (const ArrayBase&);

  // Destructor.
  virtual ~ArrayBase();

  // The dimensionality of this array.
  uInt ndim() const
    { return ndimen_p; }

  // How many elements does this array have? Product of all axis lengths.
  // <group>
  size_t nelements() const
    { return nels_p; }
  size_t size() const
    { return nels_p; }
  // </group>

  // Is the array empty (i.e. no elements)?
  Bool empty() const
    { return nels_p == 0; }

  // Are the array data contiguous?
  // If they are not contiguous, <src>getStorage</src> (see below)
  // needs to make a copy.
  Bool contiguousStorage() const
    { return contiguous_p; }

  // Check to see if the Array is consistent. This is about the same thing
  // as checking for invariants. If AIPS_DEBUG is defined, this is invoked
  // after construction and on entry to most member functions.
  virtual Bool ok() const;

  // The length of each axis.
  const IPosition& shape() const
    { return length_p; }

  // A convenience function: endPosition(i) = shape(i) - 1; i.e. this
  // is the IPosition of the last element of the Array.
  IPosition endPosition() const;

  // Return steps to be made if stepping one element in a dimension.
  // This is the 'physical' step, thus it also works correctly for
  // non-contiguous arrays. E.g. <src>data() + steps(0)</src> gives
  // the second element of the first axis.
  const IPosition& steps() const
    { return steps_p; }

  // Array version for major change (used by ArrayIO).
  // enum did not work properly with cfront 3.0.1), so replaced
  // by a static inline function. Users won't normally use this.
  static uInt arrayVersion()
    {return 3;}

  // Make an empty array of the same type.
  // <br>The default implementation in ArrayBase throws an exception.
  virtual CountedPtr<ArrayBase> makeArray() const;

  // Resize the array and optionally copy the values.
  // <br>The default implementation in ArrayBase throws an exception.
  virtual void resize(const IPosition &newShape, Bool copyValues=False);

  // Create an ArrayIterator object of the correct type.
  // This is implemented in the derived Array classes.
  // <br>The default implementation in ArrayBase throws an exception.
  virtual CountedPtr<ArrayPositionIterator> makeIterator (uInt byDim) const;

  // Get a reference to a section of an array.
  // This is the same as Array<T>::operator(), but without having to know
  // the exact template type.
  // <br>The default implementation in ArrayBase throws an exception.
  virtual CountedPtr<ArrayBase> getSection (const Slicer&) const;

  // Assign the source array to this array.
  // If <src>checkType==True</src>, it is checked if the underlying template
  // types match. Otherwise, it is only checked in debug mode (for performance).
  // <br>The default implementation in ArrayBase throws an exception.
  virtual void assignBase (const ArrayBase& source, Bool checkType=True);

  // The following functions behave the same as the corresponding getStorage
  // functions in the derived templated Array class.
  // They handle a pointer to a contiguous block of array data.
  // If the array is not contiguous, a copy is used to make it contiguous.
  // <group>
  virtual void* getVStorage (Bool& deleteIt);
  virtual const void* getVStorage (Bool& deleteIt) const;
  virtual void putVStorage(void*& storage, Bool deleteAndCopy);
  virtual void freeVStorage(const void*& storage, Bool deleteIt) const;
  // <group>

protected:
  void baseCopy (const ArrayBase& that)
    { operator= (that); }

  // Determine if the storage of a subset is contiguous.
  Bool isStorageContiguous() const;

  // Check if the shape of a vector is correct. If possible, adjust if not.
  // It is possible if at most one axis has length > 1.
  void checkVectorShape();

  // Check if the shape of a matrix is correct. Adjust it if smaller.
  void checkMatrixShape();

  // Check if the shape of a cube is correct. Adjust it if smaller.
  void checkCubeShape();

  // Reform the array to a shape with the same nr of elements.
  void baseReform (ArrayBase& tmp, const IPosition& shape) const;

  // Remove the degenerate axes from the Array object.
  // This is the implementation of the nonDegenerate functions.
  // It has a different name to be able to make it virtual without having
  // the "hide virtual function" message when compiling derived classes.
  void baseNonDegenerate (const ArrayBase& other, const IPosition& ignoreAxes);
    
  // These member functions return an Array reference with the specified
  // number of extra axes, all of length one, appended to the end of the
  // Array. Note that the <src>reform</src> function can also be
  // used to add extra axes.
  void baseAddDegenerate (ArrayBase&, uInt numAxes);

  // Make a subset of an array.
  // It checks if start,end,incr are within the array limits.
  // It returns the offset of the subset in the array.
  size_t makeSubset (ArrayBase& out,
                     const IPosition& b,
                     const IPosition& e,
                     const IPosition& i);

  // Are the shapes identical?
  Bool conform2 (const ArrayBase& other) const
    { return length_p.isEqual (other.length_p); }

  // Make the indexing step sizes.
  void baseMakeSteps();

  // Throw expection if vector dimensionality is incorrect.
  void throwNdimVector();

  // Helper function for templated Vector class.
  // It returns if this and other are conformant.
  Bool copyVectorHelper (const ArrayBase& other);

public:
  // Various helper functions.
  // <group>
  void validateConformance (const ArrayBase&) const;
  void validateIndex (const IPosition&) const;
  void validateIndex (uInt index) const;
  void validateIndex (uInt index1, uInt index2) const;
  void validateIndex (uInt index1, uInt index2, uInt index3) const;
  // </group>

protected:
  // Number of elements in the array. Cached rather than computed.
  size_t nels_p;
  // Dimensionality of the array.
  uInt ndimen_p;
  // Are the data contiguous?
  Bool contiguous_p;
  // Used to hold the shape, increment into the underlying storage
  // and originalLength of the array.
  IPosition length_p, inc_p, originalLength_p;
  // Used to hold the step to next element in each dimension.
  IPosition steps_p;
};


// <summary> General global functions for Arrays. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArray">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
// </prerequisite>
//
// <synopsis>
// These are generally useful global functions which operate on all
// Arrays.
// </synopsis>
//
// <linkfrom anchor="Array general global functions" classes="Array Vector Matrix Cube">
//   <here>Array general global functions</here> -- General global functions
//   for Arrays.
// </linkfrom>
//
// <group name="Array general global functions">

// 
// What is the volume of an N-dimensional array. 
// Shape[0]*Shape[1]*...*Shape[N-1]. An Array helper function.
//# Implemented in Array2.cc.
size_t ArrayVolume (uInt Ndim, const Int* Shape);

// 
// What is the linear index into an "Ndim" dimensional array of the given
// "Shape", "Origin", and "Increment" for a given IPosition Index.
//  An Array helper function.
// <group>
//# Implemented in Array2.cc.
size_t ArrayIndexOffset (uInt Ndim, const ssize_t* Shape, 
                         const ssize_t* Origin, const ssize_t* Inc, 
                         const IPosition& Index);
size_t ArrayIndexOffset (uInt Ndim, const ssize_t* Shape, 
                         const ssize_t* Inc, const IPosition& Index);
// </group>

// </group>

} //# NAMESPACE CASACORE - END

#endif
