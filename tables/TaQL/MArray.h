//# MArray.h: Class to handle an Array with an optional mask
//# Copyright (C) 2012
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
//# $Id: MArray.h 21399 2013-11-12 07:55:35Z gervandiepen $

#ifndef CASA_MARRAY_H
#define CASA_MARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/MArrayBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // Class to handle an Array with an optional mask
  // </summary>

  // <use visibility=local>

  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> <linkto class=Array>Array</linkto>
  //   <li> <linkto class=MArrayBase>MArrayBase</linkto>
  // </prerequisite>

  // <synopsis> 
  // This class makes it easier to handle arrays with ot without mask.
  // The array is always present, but the mask is optional. The mask is
  // contained in the non-templated base class MArrayBase and functions
  // to operate on the mask are defined there.
  // <br> The class is primarily developed for TaQL masked arrays, but
  // could be used elsewhere as well.
  //
  // A mask value True means that the corresponding value is masked off, thus
  // not taken into account in reduction functions like <src>sum</src>. This
  // is the same as the numpy masked array.
  //
  // MArrayMath.h contains many functions to operate on MArray objects
  // (addition, sin, etc.).
  // </synopsis> 

  template <typename T>
  class MArray: public MArrayBase
  {
  public:
    // Default constructor creates a null array.
    MArray()
      : MArrayBase (True)
    {}

    // Construct from an array without a mask.
    // It references the given array.
    explicit MArray (const Array<T>& array)
      : MArrayBase (False),
        itsArray  (array)
    {
      resizeBase (array, False);
    }

    // Construct from an array and a mask.
    // It references the given arrays.
    // <src>isNull=True</src> requires the arrays to be empty.
    MArray (const Array<T>& array, const Array<Bool>& mask, Bool isNull=False)
      : MArrayBase (array, mask, isNull),
        itsArray   (array)
    {}

    // Construct from an array with the mask and null from another MArray.
    // It references the given arrays.
    // The shapes of both arrays must match.
    MArray (const Array<T>& array, const MArrayBase& marray)
      : MArrayBase (array, marray),
        itsArray   (array)
    {}

    // Construct from two MArrays, one the array, the other the mask.
    // If one of them is null, the constructed MArray is null.
    MArray (const MArray<T>& array, const MArray<Bool>& mask)
      : MArrayBase (array.isNull()  ||  mask.isNull())
    {
      if (! isNull()) {
        itsArray.reference (array.array());
        setBase (itsArray, mask.array());
      }
    }

    // Reference another array.
    void reference (const MArray<T>& other)
    {
      itsArray.reference (other.itsArray);
      referenceBase (other);
    }

    // Resize the array and optionally the mask.
    // It always sets the MArray to non-null.
    void resize (const IPosition& shape, Bool useMask)
    {
      itsArray.resize (shape);
      resizeBase (itsArray, useMask);
    }

    // Copy the array data and possible mask from another one.
    // The shapes do not need to match.
    // The array data is copied, but the new mask references the possible
    // mask in <src>from</src>.
    template <typename U>
    void fill (const MArray<U>& from)
    {
      itsArray.resize (from.shape());
      convertArray (itsArray, from.array());
      setBase (itsArray, from.mask());
    }

    // Copy the array from a normal Array. The possible mask is removed.
    // The shapes do not need to match.
    // The array data is always copied.
    template <typename U>
    void fill (const Array<U>& from)
    {
      itsArray.resize (from.shape());
      convertArray (itsArray, from);
      resizeBase (itsArray, False);
    }

    // Get access to the array.
    // <group>
    const Array<T>& array() const
      { return itsArray; }
    Array<T>& array()
      { return itsArray; }
    // </group>

    // Flatten the unmasked elements of the array to a vector.
    Vector<T> flatten() const;
    // Copy the unmasked elements to the out. The argument <src>size</src>
    // gives the size of the output buffer which should be at least the
    // size of the array. It returns the nr of unmasked elements.
    size_t flatten (T* out, size_t size) const;

    // Get a subset of the array.
    MArray<T> operator() (const IPosition& start, const IPosition& end,
                          const IPosition& stride)
    {
      if (hasMask()) {
        return MArray<T> (itsArray(start, end, stride),
                          mask()(start, end, stride));
      }
      return MArray<T> (itsArray(start, end, stride));
    }

  private:
    Array<T> itsArray;
  };


  //# Implement functions.
  template<typename T>
  Vector<T> MArray<T>::flatten() const
  {
    Vector<T> vec(nvalid());
    // We lie about the size, because we know the buffer has the right size.
    flatten (vec.data(), itsArray.size());
    return vec;
  }

  template<typename T>
  size_t MArray<T>::flatten (T* out, size_t size) const
  {
    if (size < itsArray.size()) {
      throw ArrayError ("MArray::flatten - size " + std::to_string(size) +
                        " of output buffer is too small");
    }
    size_t nr = 0;
    if (!hasMask()) {
      // No mask, so copy all elements.
      Array<T> arr(itsArray.shape(), out, SHARE);
      arr = itsArray;
      nr  = arr.size();
    } else {
      // Copy only the valid elements.
      if (itsArray.contiguousStorage() && mask().contiguousStorage()) {
        typename Array<Bool>::const_contiter miter = mask().cbegin();
        typename Array<T>::const_contiter iterEnd = itsArray.cend();
        for (typename Array<T>::const_contiter iter=itsArray.cbegin();
             iter!=iterEnd; ++iter, ++miter) {
          if (!*miter) out[nr++] = *iter;
        }
      } else {
        typename Array<Bool>::const_iterator miter = mask().begin();
        typename Array<T>::const_iterator iterEnd = itsArray.end();
        for (typename Array<T>::const_iterator iter=itsArray.begin();
             iter!=iterEnd; ++iter, ++miter) {
          if (!*miter) out[nr++] = *iter;
        }
      }
    }
    return nr;
  }


} //# NAMESPACE CASACORE - END

#endif
