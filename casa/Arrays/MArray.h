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
//# $Id$

#ifndef CASA_MARRAY_H
#define CASA_MARRAY_H

//# Includes
#include <casa/aips.h>
#include <casa/Arrays/MArrayBase.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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
    // Default constructor creates empty array.
    MArray()
    {}

    // Construct from an array.
    explicit MArray (const Array<T>& array)
      : MArrayBase (array.size()),
        itsArray   (array)
    {}

    // Construct from an array with mask.
    MArray (const Array<T>& array, const Array<Bool>& mask)
      : MArrayBase (mask, array.size()),
        itsArray   (array)
    {}

    // Reference another array.
    void reference (const MArray<T>& other) {
      itsArray.reference (other.itsArray);
      referenceBase (other);
    }

    // Fill the array data and mask from another one.
    template <typename U>
    void fill (const MArray<U>& from)
    {
      itsArray.resize (from.shape());
      convertArray (itsArray, from.array());
      setMask (from.mask());
      setSize (itsArray.size());
    }

    // Fill the array from a normal Array. The possible mask is removed.
    template <typename U>
    void fill (const Array<U>& from)
    {
      itsArray.resize (from.shape());
      convertArray (itsArray, from);
      removeMask();
      setSize (itsArray.size());
    }

    // Get the number of elements.
    // <group>
    size_t size() const
      { return itsArray.size(); }
    size_t nelements() const
      { return itsArray.size(); }
    // </group>

    // Get the dimensionality.
    uInt ndim() const
      { return itsArray.ndim(); }

    // Get the shape.
    const IPosition& shape() const
      { return itsArray.shape(); }

    // Get access to the array.
    // <group>
    const Array<T>& array() const
      { return itsArray; }
    Array<T>& array()
      { return itsArray; }
    // </group>

    // Flatten the unmasked elements of the array to a vector.
    Vector<T> flatten() const;

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

  template<typename T>
  Vector<T> MArray<T>::flatten() const
  {
    Vector<T> vec;
    if (!hasMask()) {
      vec.resize (itsArray.size());
      Array<T> arr(itsArray.shape(), vec.data(), SHARE);
      arr = itsArray;
    } else {
      vec.resize (nvalid());
      Int64 inx = 0;
      if (itsArray.contiguousStorage() && mask().contiguousStorage()) {
        typename Array<Bool>::const_contiter miter = mask().cbegin();
        typename Array<T>::const_contiter iterEnd = itsArray.cend();
        for (typename Array<T>::const_contiter iter=itsArray.cbegin();
             iter!=iterEnd; ++iter, ++miter) {
          if (!*miter) vec[inx++] = *iter;
        }
      } else {
        typename Array<Bool>::const_iterator miter = mask().begin();
        typename Array<T>::const_iterator iterEnd = itsArray.end();
        for (typename Array<T>::const_iterator iter=itsArray.begin();
             iter!=iterEnd; ++iter, ++miter) {
          if (!*miter) vec[inx++] = *iter;
        }
      }
    }
    return vec;
  }


} //# NAMESPACE CASA - END

#endif
