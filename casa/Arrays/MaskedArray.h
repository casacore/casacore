//# MaskedArray.h: A templated N-D masked array class with zero origin.
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2005
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

#ifndef CASA_MASKEDARRAY2_H
#define CASA_MASKEDARRAY2_H


//# Includes
#include "ArrayLogical.h"
#include "ArrayFwd.h"
#include "IPosition.h"
#include "MaskLogiArrFwd.h"

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
//# <note role=warning>
//#   Array.h cannot be included in this header file.  Anything needed
//#   from it must be forwarded.  This is why LogicalArrayFwd.h is
//#   included instead of LogicalArray.h .
//# </note>
class Slicer;


// <summary> Class for masking an Array for operations on that Array. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMaskedArray tMaskArrExcp">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto group="LogiArray.h#LogicalArray">LogicalArray</linkto>
// </prerequisite>
//
// <etymology>
// MaskedArray is a class for masking elements of an Array while performing
// operations on that Array.
// </etymology>
//
// <synopsis>
// A MaskedArray is an association between an Array and a mask.  The mask
// selects elements of the Array.  Only elements of the Array where the
// corresponding element of the mask is true are defined.  Thus, operations
// on a MaskedArray only operate on those elements of the Array where the 
// corresponding element of the mask is true.
//
// A MaskedArray should be thought of as a manipulator for an Array, analogous
// to an iterator.  It allows one to perform whole Array operations on selected
// elements of the Array.
//
// The mask used in the constructor for the MaskedArray must conform to
// the Array, thus have the same shape.
// The internal mask is (will be) copy constructed with reference semantics
// from the input mask. Therefore, it is (will be) possible to change the
// internal mask by changing values in the input mask *after* the MaskedArray
// has been constructed.  To ensure that the internal mask is independent of
// the input mask after construction, use mask.copy() as the input argument.
//
// One can explicitly construct a MaskedArray from an Array and a mask or
// a MaskedArray and a mask.  One can also use operator() on an Array or
// a MaskedArray to implicitly create a MaskedArray.
//
// One can create a MaskedArray from a MaskedArray and a mask.  The resulting
// MaskedArray has as its Array the Array from the original MaskedArray.
// The mask for the resulting MaskedArray is the AND of the mask from the
// original MaskedArray and the input mask.
//
// Any operation involving a MaskedArray or a set of MaskedArrays is only
// performed for those elements where the AND of the masks is true.
//
// Any operation involving a MaskedArray or a set of MaskedArrays results
// in a MaskedArray whose mask is the AND of the masks of the original
// MaskedArrays.  The only exception to this is assignment, where the
// mask determines which elements of the underlying Array are assigned.
//
// Masks, which are LogicalArrays, can be constructed by logical operations
// involving Arrays.  They can also, of course, be constructed by individually
// setting individual elements of an LogicalArray.
//
// MaskedArrays constructed directly from Arrays are by default writeable.
// MaskedArrays constructed indirectly from Arrays by <src>operator()</src>
// are writeable if the Array is non-const and are readonly if the
// Array is const.
// MaskedArrays constructed from other MaskedArrays, either directly by
// constructors or indirectly by <src>operator()</src>, are by default
// writeable if the input MaskedArray is writeable, and readonly if the
// input MaskedArray is readonly.
//
// A given MaskedArray can be set to be readonly.  One specifies
// this in the constructor with the bool argument <src>isreadonly</src>,
// or calls the <src>setReadOnly()</src> member function.
// A MaskedArray which would default to be readonly cannot be forced to
// be writeable.  It will remain readonly even if the bool argument
// <src>isreadonly</src> is set to be <src>false</src>.
//
// The <src>isReadOnly(),</src> member function is used to test whether
// the MaskedArray is readonly.
//
// Member functions which change the MaskedArray test to see whether
// the MaskedArray is readonly, and throw an ArrayError exception if
// it is.  These member functions are:
// <ul>
//    <li> <src>operator=()</src>
//    <li> <src>getRWArray()</src>
//    <li> <src>getRWArrayStorage()</src>
//    <li> <src>putArrayStorage()</src>
// </ul>
//
// The copy() member function makes a deep copy of a MaskedArray.
// By default it returns a writeable MaskedArray, but the MaskedArray
// returned can be made readonly by using the bool argument "isreadonly"
// to copy() (or by calling setReadOnly() on the new MaskedArray).
//
// The valid elements of the MaskedArray can be manipulated as a
// "compressed" Array which contains only the valid elements.
// The number of elements in this "compressed" Array is the number of valid
// elements in the MaskedArray, <src>nelementsValid()</src>.
// The "compressed" Array can have any shape which meets this requirement.
// The MaskedArray can have any shape.
//
// The <src>getCompressedArray()</src> member functions get a compressed
// Array from the valid members of the MaskedArray, while the
// <src>setCompressedArray()</src> member function sets the valid members
// of the MaskedArray from the input compressed Array.
//
// Many mathematical and logical global operators and functions which operate
// on MaskedArrays are defined.  Typically, they are defined for all sensible
// combinations of MaskedArrays, Arrays, and scalars.
//
// Mathematical global operators and functions are defined in
// Arrays/MaskArrMath.h .
// The list is:
// <ul>
//   <li> operator+= ()
//   <li> operator-= ()
//   <li> operator*= ()
//   <li> operator/= ()
//   <li> operator+ ()
//   <li> operator- ()
//   <li> operator* ()
//   <li> operator/ ()
//   <li> sin ()
//   <li> cos ()
//   <li> tan ()
//   <li> asin ()
//   <li> acos ()
//   <li> atan ()
//   <li> sinh ()
//   <li> cosh ()
//   <li> tanh ()
//   <li> exp ()
//   <li> log ()
//   <li> log10 ()
//   <li> sqrt ()
//   <li> abs ()
//   <li> fabs ()
//   <li> ceil ()
//   <li> floor ()
//   <li> atan2 ()
//   <li> fmod ()
//   <li> pow ()
//   <li> minMax ()
//   <li> min ()
//   <li> max ()
//   <li> indgen ()
//   <li> sum ()
//   <li> sumsquares ()
//   <li> product ()
//   <li> mean ()
//   <li> variance ()
//   <li> stddev ()
//   <li> avdev ()
//   <li> median ()
//   <li> square ()
//   <li> cube ()
// </ul>
//
// Logical global operators and functions are defined in
// Arrays/MaskArrLogi.h .
// The list is:
// <ul>
//   <li> allLE ()
//   <li> allLT ()
//   <li> allGE ()
//   <li> allGT ()
//   <li> allEQ ()
//   <li> allNE ()
//   <li> allAND ()
//   <li> allOR ()
//   <li> anyLE ()
//   <li> anyLT ()
//   <li> anyGE ()
//   <li> anyGT ()
//   <li> anyEQ ()
//   <li> anyNE ()
//   <li> anyAND ()
//   <li> anyOR ()
//   <li> operator<= ()
//   <li> operator<  ()
//   <li> operator>= ()
//   <li> operator<  ()
//   <li> operator== ()
//   <li> operator!= ()
//   <li> operator&& ()
//   <li> operator|| ()
// </ul>
// </synopsis>
//
// <example>
// Use an explicit MaskedArray to limit the maximum value of an Array.
//
// <srcblock>
//   Vector<int> arr (20);
//      . . .
//   MaskedArray<int> marr (arr, (arr > 5));
//   marr = 5;
// </srcblock>
//
// This sets all elements of arr which are greater than 5 to 5.
// </example>
//
// <example>
// Use an implicit MaskedArray to limit the minimum value of an Array.
//
// <srcblock>
//   Vector<int> arr (20);
//      . . .
//   arr (arr < 0) = 0;
// </srcblock>
//
// This sets all elements of arr which are less than 0 to 0.
// </example>
//
// <example>
// It does not matter where in an expression the MaskedArrays are located.
// The operation is only performed on those elements where the AND of the
// masks is true.
//
// The following expressions are all equivalent.
// The first (and second) expressions are the most efficient, since the sum
// is only performed for those elements where ((a > 0) && (b > 0)).
// The third example is less efficient, since the sum is performed for
// all elements of a and b, and then the assignment is only performed for
// those elements where ((a > 0) && (b > 0)).
//
// <srcblock>
//   Vector<int> arr (20);
//   Vector<int> a (20);
//   Vector<int> b (20);
//      . . .
//   arr = a(a > 0) + b(b > 0);
//
//   arr = a(b > 0) + b(a > 0);
//
//   arr ((a > 0) && (b > 0)) = a + b;
//
//   arr = (a + b) ((a > 0) && (b > 0));
//
//   arr (a > 0) = a + b(b > 0);
//
// </srcblock>
//
// All of these expressions set those elements of arr where
// ((a > 0) && (b > 0)) to a + b.  Those elements of arr where the condition
// is false are unchanged.
// </example>
//
// <example>
// This example extracts the valid elements of the MaskedArray as a
// "compressed" Vector, manipulates this Vector, and then puts the result
// back into the MaskedArray.
//
// <srcblock>
//   Matrix<int> arr (20,5);
//      . . .
//   MaskedArray<int> marr (arr, (arr>0) && (arr<10));
//   Vector<int> vec (marr.getCompressedArray());
//      . . .
//   marr.setCompressedArray (vec);
// </srcblock>
//
// </example>
//
// <motivation>
// A MaskedArray is an association between an Array and a LogicalArray which
// masks the Array.  It allows one to perform whole Array manipulations
// with a single expression, selecting those elements of the Array to modify
// based either on a logical expression, typically involving some of the
// Arrays involved in the expression, or based on a specifically set mask.
// </motivation>
//
// <todo asof="$DATE:$>
//   <li> Consider whether there should be constructors for masks
//          specified as Array<bool>.
//   <li> Change constructors to always do copy construction with
//          reference semantics when creating the internal mask.
// </todo>


template<typename T, typename ArrayAlloc, typename MaskAlloc>
class MaskedArray
{

public:
  typedef Array<T, ArrayAlloc> array_type;
  typedef Array<LogicalArrayElem, MaskAlloc> mask_type;
  typedef MaskedArray<T, ArrayAlloc, MaskAlloc> masked_array_type;
  
  // Default constructor for a MaskedArray does not allocate any memory
  // for the Data array or Mask. Hence the masked array 
  // should not be used until some data is allocated to the object using one
  // of the set functions.
  MaskedArray();
  // Reset the data and mask of the the MaskedArray. There should perhaps be
  // a whole family of setData functions with different arguements,
  // analogous to the constructors. However these are sufficient for the
  // moment. 
  void setData(const array_type & data, const mask_type& mask, 
	  bool isReadOnly=false);
  void setData(const masked_array_type& array, bool isReadOnly=false);
    // Create a MaskedArray from an Array and a LogicalArray.
    //
    // The internal mask is a total copy of the input mask, and is
    // completely independent of the input mask.
    //
    // The Array is copy constructed, which means that it is a really smart
    // pointer to the underlying Block, and shares this Block with the input
    // Array.
    //
    // By default, the MaskedArray constructed is writeable.  If
    // <src>isreadonly</src> is <src>true</src>, then the MaskedArray
    // returned is readonly.
    //
    // <thrown>
    //    <li> ArrayConformanceError
    // </thrown>
    //
    // <group>
    MaskedArray(const array_type &inarray, const LogicalArray &inmask,
                bool isreadonly);
    MaskedArray(const array_type &inarray, const LogicalArray &inmask);
    // </group>

    // Create a MaskedArray from a MaskedArray and a LogicalArray.
    //
    // The internal mask is the AND of the input mask and the mask of
    // the input MaskedArray.
    //
    // The Array from the input MaskedArray is copy constructed, which
    // means that it is a really smart pointer to the underlying Block, and
    // shares this Block with the Array from the input MaskedArray.
    //
    // By default, the MaskedArray constructed is writeable if the input
    // MaskedArray is writeable, and readonly if the input MaskedArray
    // is readonly.  If <src>isreadonly</src> is <src>true</src>, then
    // the MaskedArray returned is readonly.  If <src>isreadonly</src> is
    // <src>false</src> and the input MaskedArray is readonly, then the
    // constructed MaskedArray is readonly.
    //
    // <thrown>
    //    <li> ArrayConformanceError
    // </thrown>
    //
    // <group>
    MaskedArray(const masked_array_type &inarray, const LogicalArray &inmask,
                bool isreadonly);
    MaskedArray(const masked_array_type &inarray, const LogicalArray &inmask);
    // </group>

    // Create a MaskedArray from an Array and a MaskedLogicalArray.
    //
    // The internal mask is the AND of the internal LogicalArray and the
    // internal mask of the MaskedLogicalArray.
    //
    // The Array is copy constructed, which means that it is a really smart
    // pointer to the underlying Block, and shares this Block with the input
    // Array.
    //
    // By default, the MaskedArray constructed is writeable.  If
    // <src>isreadonly</src> is <src>true</src>, then the MaskedArray
    // returned is readonly.
    //
    // <thrown>
    //    <li> ArrayConformanceError
    // </thrown>
    //
    // <group>
    MaskedArray(const array_type &inarray, const MaskedLogicalArray &inmask,
                bool isreadonly);
    MaskedArray(const array_type &inarray, const MaskedLogicalArray &inmask);
    // </group>

    // Create a MaskedArray from a MaskedArray and a MaskedLogicalArray.
    //
    // The internal mask is the AND of the internal LogicalArray and the
    // internal mask of the MaskedLogicalArray, ANDed with the mask of
    // the input MaskedArray.
    //
    // The Array from the input MaskedArray is copy constructed, which
    // means that it is a really smart pointer to the underlying Block, and
    // shares this Block with the Array from the input MaskedArray.
    //
    // By default, the MaskedArray constructed is writeable if the input
    // MaskedArray is writeable, and readonly if the input MaskedArray
    // is readonly.  If <src>isreadonly</src> is <src>true</src>, then
    // the MaskedArray returned is readonly.  If <src>isreadonly</src> is
    // <src>false</src> and the input MaskedArray is readonly, then the
    // constructed MaskedArray is readonly.
    //
    // <thrown>
    //    <li> ArrayConformanceError
    // </thrown>
    //
    // <group>
    MaskedArray(const masked_array_type &inarray,
                const MaskedLogicalArray &inmask,
                bool isreadonly);
    MaskedArray(const masked_array_type &inarray,
                const MaskedLogicalArray &inmask);
    // </group>

    // Copy constructor.
    //
    // The internal mask is a total copy of the mask from the input
    // MaskedArray, and is completely independent of this input mask.
    //
    // The Array from the input MaskedArray is copy constructed, which
    // means that it is a really smart pointer to the underlying Block, and
    // shares this Block with the Array from the input MaskedArray.
    //
    // By default, the MaskedArray constructed is writeable if the input
    // MaskedArray is writeable, and readonly if the input MaskedArray
    // is readonly.  If <src>isreadonly</src> is <src>true</src>, then
    // the MaskedArray returned is readonly.  If <src>isreadonly</src> is
    // <src>false</src> and the input MaskedArray is readonly, then the
    // constructed MaskedArray is readonly.
    //
    // <group>
    MaskedArray(const masked_array_type &other, bool isreadonly);
    MaskedArray(const masked_array_type &other);
    
    // The source is left empty after moving
    MaskedArray(masked_array_type&& source);
    // </group>

    // Return a MaskedArray.  The new MaskedArray is masked by the input
    // LogicalArray "anded" with the mask of the original MaskedArray.
    // This mask must conform to the array.
    //
    // The MaskedArray constructed is writeable if the input
    // MaskedArray is writeable, and readonly if the input MaskedArray
    // is readonly.
    //
    masked_array_type operator() (const LogicalArray &mask) const;

    // Return a MaskedArray.  The new MaskedArray is masked by the input
    // MaskedLogicalArray "anded" with the mask of the original MaskedArray.
    // This mask must conform to the array.
    //
    // The MaskedArray constructed is writeable if the input
    // MaskedArray is writeable, and readonly if the input MaskedArray
    // is readonly.
    //
    masked_array_type operator() (const MaskedLogicalArray &mask) const;

    // Get a reference to an array part which extends from "start" to end."
    // <group>
    masked_array_type operator()(const IPosition &start, const IPosition &end);
    // Along the ith axis, every inc[i]'th element is chosen.
    masked_array_type operator()(const IPosition &start, const IPosition &end,
			      const IPosition &inc);
    // </group>

    // Get a reference to an array using a Slicer.
    masked_array_type operator()(const Slicer&);
  
    // Make a copy of the masked array.
    //
    // This is a deep copy.  The Array and mask components of the returned
    // MaskedArray are deep copies of the Array and mask in the input
    // MaskedArray pointed to by this.  In other words, the Array and mask
    // in the output MaskedArray are completely independent of those in
    // the input MaskedArray.
    //
    // By default, the MaskedArray returned is writeable.  If
    // <src>isreadonly</src> is <src>true</src>, then the MaskedArray
    // returned is readonly.
    //
    // <group>
    masked_array_type copy(bool isreadonly) const;
    masked_array_type copy() const;
    // </group>

    // Return the internal Array.
    const array_type & getArray() const;

    // Return the internal Array, writeable.
    //
    // <thrown>
    //    <li> ArrayError
    // </thrown>
    //
    array_type & getRWArray() const;

    // Return the (const) internal Mask.
    const mask_type & getMask() const;

    // The dimensionality of this masked array.
    size_t ndim() const;

    // The number of elements of this masked array.
    // This is the number of elements in the underlying Array.
    // <group>
    size_t nelements() const;
    size_t size() const
        { return nelements(); }
    // </group>

    // The number of valid elements of this masked array.
    // This is the number of elements of the mask which are TRUE.
    size_t nelementsValid() const;


    // Check to see if the masked array is consistent. This is about the same
    // thing as checking for invariants. If AIPS_DEBUG is defined, this is
    // invoked after construction and on entry to most member functions.
    bool ok() const;

    // Are the shapes identical?
    // <group>
    bool conform(const array_type &other) const;
    bool conform(const masked_array_type &other) const;
    // </group>

    // The length of each axis.
    const IPosition& shape() const
      { return pArray->shape(); }

    // Is the array read only?
    bool isReadOnly() const
      { return isRO; }

    // Set the array to be read only.
    void setReadOnly() const;


    // Copy the values in inarray to this, only copying those elements
    // for which the corresponding mask element is true.
    // <thrown>
    //    <li> ArrayConformanceError
    //    <li> ArrayError
    // </thrown>
    // TODO rename, see copy assignment operator.
    masked_array_type &operator=(const array_type &inarray);
    masked_array_type &operator=(array_type&& inarray);

    // Copies/moves the values in other to this, only copying those elements
    // for which the logical AND of the corresponding mask elements
    // of both MaskedArrays is true.
    //
    // <thrown>
    //    <li> ArrayConformanceError
    //    <li> ArrayError
    // </thrown>
    //
    // <group>
    // TODO this should be renamed: assignment operator should make
    // obervable state equal, which should thus include getArray().
    masked_array_type &operator=(const masked_array_type &other);
    masked_array_type &operator=(masked_array_type&& other);
    // </group>

    // Set every element of this array to "value", only setting those elements
    // for which the corresponding mask element is true.
    // In other words, a scalar behaves as if it were a constant conformant
    // array.
    //
    // <thrown>
    //    <li> ArrayError
    // </thrown>
    //
    masked_array_type &operator=(const T &value);

    // Return a "compressed" Array containing only the valid
    // elements of the MaskedArray.  The number of elements in the
    // Array will be <src>nelementsValid()</src> for the
    // MaskedArray.  The MaskedArray can have any shape.
    // <group>

    // The returned Array will have dimension one.
    Array<T, ArrayAlloc> getCompressedArray () const;

    // The returned Array will have the input shape.  This shape must
    // give the returned Array the required number of elements.
    //
    // <thrown>
    //    <li> ArrayError
    // </thrown>
    //
    Array<T, ArrayAlloc> getCompressedArray (const IPosition & shape) const;

    // </group>

    // Fill the argument "compressed" Array with only the
    // valid elements of the MaskedArray.  The size of the
    // Array must be <src>nelementsValid()</src> for the MaskedArray.
    // The Array can have any shape which meets this requirement.
    // The MaskedArray can have any shape.
    //
    // <thrown>
    //    <li> ArrayError
    // </thrown>
    //
    void getCompressedArray (array_type & inarr) const;

    // Set only the valid elements of the MaskedArray from the argument
    // "compressed" Array.  The size of the
    // Array must be <src>nelementsValid()</src> for the MaskedArray.
    // The Array can have any shape which meets this requirement.
    // The MaskedArray can have any shape.
    //
    // <thrown>
    //    <li> ArrayError
    // </thrown>
    //
    void setCompressedArray (const array_type& inarr);

    // Manipulate the storage for the underlying Array.
    // See the description of the corresponding Array functions
    // for more information.
    // <group>
    const T * getArrayStorage (bool &deleteIt) const;
    //
    // <thrown>
    //    <li> ArrayError
    // </thrown>
    //
    T * getRWArrayStorage (bool &deleteIt) const;
    //
    void freeArrayStorage(const T *&storage, bool deleteIt) const;
    //
    // <thrown>
    //    <li> ArrayError
    // </thrown>
    //
    void putArrayStorage(T *&storage, bool deleteAndCopy) const;
    // </group>


    // Manipulate the storage for the underlying Mask.
    // See the description of the corresponding Array functions
    // for more information.
    // <group>
    const LogicalArrayElem *getMaskStorage (bool &deleteIt) const;
    //
    void freeMaskStorage(const LogicalArrayElem *&storage, bool deleteIt) const;
    // </group>


protected:
    // The array.
    std::unique_ptr<array_type> pArray;

    // The mask.
    std::unique_ptr<mask_type> pMask;

    // Cache the number of valid elements.
    size_t nelemValid;

    // Is the number of valid elements cache OK?
    // i.e. has it been calculated?
    bool nelemValidIsOK;

    // Is the array read only?
    bool isRO;

};


// <summary> General global functions for MaskedArrays, and MaskedArrays and Arrays. </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMaskedArray">
//
// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> <linkto group="LogiArray.h#LogicalArray">LogicalArray</linkto>
//   <li> <linkto class=MaskedArray>MaskedArray</linkto>
// </prerequisite>
//
// <synopsis>
// These are generally useful global functions which operate on all
// MaskedArrays, or on MaskedArrays and Arrays.
// </synopsis>
//
// <linkfrom anchor="MaskedArray general global functions" classes="MaskedArray Array Vector Matrix Cube">
//   <here>MaskedArray general global functions</here> -- General global
//   functions for MaskedArrays, and between MaskedArrays and Arrays.
// </linkfrom>
//
// <group name="MaskedArray general global functions">


// Test conformance for masked arrays and arrays of different types.
// Are the shapes identical?
//
//   <group name=conform2>
//
template<typename TL, typename ArrayAllocL, typename MaskAllocL, typename TR, typename ArrayAllocR>
  bool conform2 (const MaskedArray<TL, ArrayAllocL, MaskAllocL> &left, const Array<TR, ArrayAllocR> &right);
template<typename TL, typename ArrayAllocL, typename TR, typename ArrayAllocR, typename MaskAllocR>
  bool conform2 (const Array<TL, ArrayAllocL> &left, const MaskedArray<TR, ArrayAllocR, MaskAllocR> &right);
template<typename TL, typename ArrayAllocL, typename MaskAllocL, typename TR, typename ArrayAllocR, typename MaskAllocR>
  bool conform2 (const MaskedArray<TL, ArrayAllocL, MaskAllocL> &left, const MaskedArray<TR, ArrayAllocR, MaskAllocR> &right);
//
//   </group>

// </group>

} //# NAMESPACE CASACORE - END

#include "MaskedArray.tcc"

#endif
