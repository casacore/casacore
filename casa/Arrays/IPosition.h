//# IPosition.h: A vector of integers, used to index into arrays.
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002
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

#ifndef CASA_IPOSITION_H
#define CASA_IPOSITION_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/iosfwd.h>
#include <casacore/casa/BasicSL/String.h>
#include <vector>
#include <cstddef>                  // for ptrdiff_t
#include <sys/types.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;
class LogIO;
template<class T> class Array;
template<class T> class Vector;

// <summary> A Vector of integers, for indexing into Array<T> objects. </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

//# <prerequisite>
//# Classes you should understand before using this one.
//# </prerequisite>

// <etymology>
// IPosition is an Index Position in an n-dimensional array.
// </etymology>

// <synopsis> 
// IPosition is "logically" a Vector<Int> constrained so that its origin
// is zero-based, and in fact that used to be the way it was implemented.
// It was split out into a separate class to make the inheritance from
// Arrays simpler (since Arrays use IPositions). The
// template instantiation mechanism is complicated enough that this
// simplification was felt to be a good idea.
// <p>
// IPosition objects are normally used to index into, and define the shapes
// of, multi-dimensional arrays. For example, if you have a 5 dimensional
// array, you need an IPosition of length 5 to index into the array (or
// to define its shape, etc.).
// <p>
// Unlike Vectors, IPositions always use copy semantics.
// <srcblock>
// IPosition ip1(5);                         // An IPosition of length 5
// ip1(0) = 11; ip1(1) = 5; ... ip1(4) = 6;  // Indices 0-based
// IPosition ip2(ip1);                       // Copy constructor; a COPY
// </srcblock>
//
// Binary operations must take place either with a conformnat (same size)
// IPosition or with an integer, which behaves as if it was an IPosition
// of the same size (i.e., length). All the usual binary arithmetic
// operations are available, as well as logical operations, which return
// Booleans. These all operate "element-by-element".
// <p>
// All non-inlined member functions of IPosition check invariants if the
// preprocessor symbol AIPS_DEBUG is defined.
// That is, the member functions check that ok() is true (constructors
// check after construction, other functions on entry to the function).
// If these tests fail, an AipsError exception is thrown; its message
// contains the line number and source file of the failure (it is thrown
// by the lAssert macro defined in aips/Assert.h).
//
// Constructors and functions exist to construct an IPosition directly from
// a Vector or std::vector object or to convert to it. Furthermore the
// <src>fill</src> and <src>copy</src> can be used to fill from or copy to
// any STL-type iterator.
//
// <example>
// <srcblock>
// IPosition blc(5), trc(5,1,2,3,4,5);
// blc = 0;            // OR IPosition blc(5,0);
// //...
// if (blc > trc) {
//    IPosition tmp;
//    tmp = trc;       // Swap
//    trc = blc;
//    blc = tmp;
// }
// //...
// trc += 5;           // make the box 5 larger in all dimensions
// std::vector<Int> vec(trc.toStdVector());
// </srcblock>
// </example>


class IPosition
{
public:
    enum {MIN_INT = -2147483647};
    // A zero-length IPosition.
    IPosition();

    // An IPosition of size "length." The values in the object are undefined.
    explicit IPosition(uInt length);

    // An IPosition of size "length." The values in the object get
    // initialized to val.
    IPosition(uInt length, ssize_t val);

    // An IPosition of size "length" with defined values. You need to supply
    // a value for each element of the IPosition (up to 10). [Unfortunately
    // varargs might not be sufficiently portable.]
    IPosition (uInt length, ssize_t val0, ssize_t val1, ssize_t val2=MIN_INT, 
	       ssize_t val3=MIN_INT, ssize_t val4=MIN_INT, ssize_t val5=MIN_INT,
	       ssize_t val6=MIN_INT, ssize_t val7=MIN_INT, ssize_t val8=MIN_INT,
	       ssize_t val9=MIN_INT);

    // Makes a copy (copy, NOT reference, semantics) of other.
    IPosition(const IPosition& other);
    
    ~IPosition();

    // Makes this a copy of other. "this" and "other" must either be conformant
    // (same size) or this must be 0-length, in which case it will
    // resize itself to be the same length as other.
    IPosition& operator=(const IPosition& other);

    // Copy "value" into every position of this IPosition.
    IPosition& operator=(ssize_t value);

    // Construct a default axis path consisting of the values 0 .. nrdim-1.
    static IPosition makeAxisPath (uInt nrdim);

    // Construct a full axis path from a (partially) given axis path.
    // It fills the path with the non-given axis.
    // It is checked if the given axis path is valid (i.e. if the axis are
    // < nrdim and if they are not doubly defined).
    // E.g.: in the 4D case an axis path [0,2] is returned as [0,2,1,3].
    static IPosition makeAxisPath (uInt nrdim, const IPosition& partialPath);

    // Make a list of axes which are the axes not given in <src>axes</src>
    // up to the given dimension
    static IPosition otherAxes (uInt nrdim, const IPosition& axes);

    // Convert an IPosition to and from an Array<Int>. In either case, the
    // array must be one dimensional.
    // <group>
    IPosition(const Array<Int>& other);
    Vector<Int> asVector() const;
    // </group>

    // Convert an IPosition to and from an Array<Int>. In either case, the
    // array must be one dimensional.
    // <group>
    IPosition(const std::vector<Int>& other);
    std::vector<Int> asStdVector() const;
    // </group>

    // Resize and fill this IPosition object.
    template<typename InputIterator>
    void fill (uInt size, InputIterator iter)
    {
      resize (size);
      for (uInt i=0; i<size; ++i, ++iter) {
        data_p[i] = *iter;
      }
    }

    // Copy the contents of this IPosition object to the output iterator.
    // The size of the output must be sufficient.
    template<typename OutputIterator>
    void copy (OutputIterator iter) const
    {
      for (uInt i=0; i<size_p; ++i, ++iter) {
        *iter = data_p[i];
      }
    }
  

    // This member functions return an IPosition which has
    // degenerate (length==1) axes removed and the dimensionality reduced 
    // appropriately.
    // Only axes greater than startingAxis are considered (normally one 
    // wants to remove trailing axes.
    // <br>
    // The functions with argument <src>ignoreAxes</src> do
    // not consider the axes given in that argument.
    // <group>
    IPosition nonDegenerate(uInt startingAxis=0) const;
    IPosition nonDegenerate(const IPosition& ignoreAxes) const;
    // </group>

    // Old values are copied on resize if copy==True.
    // If the size increases, values beyond the former size are undefined.
    void resize(uInt newSize, Bool copy=True);

    // Index into the IPosition. Indices are zero-based. If the preprocessor
    // symbol AIPS_ARRAY_INDEX_CHECK is defined, operator() will check
    // "index" to ensure it is not out of bounds. If this check fails, an
    // AipsError will be thrown.
    // <group>
    ssize_t& operator[] (uInt index);
    ssize_t operator[]  (uInt index) const;
    ssize_t& operator() (uInt index);
    ssize_t operator()  (uInt index) const;
    // </group>

    // Make an IPosition by using only the specified elements of the current
    // IPosition. All values of <src>axes</src> must be less than
    // the number of elements of the current object.
    // <example>
    // IPosition ipos(4, 11, 12, 13, 14);
    // // ex1 is IPosition(3, 11, 12, 13);
    // IPosition ex1 = ipos(IPosition(3,0,1,2);
    // // ex2 is IPosition(3, 12, 11)
    // IPosition ex2 = ipos(IPosition(2,2,1);
    // // ex3 is IPosition(4,14,14,14,14)
    // IPosition ex3 = ipos(IPosition(4,3,3,3,3);
    // </example>
    IPosition operator() (const IPosition& axes) const;

    // Index into the IPosition from the end.
    // By default the last value is returned.
    // If the preprocessor symbol AIPS_ARRAY_INDEX_CHECK is defined, it will
    // check if the index is not out of bounds.
    // <group>
    ssize_t& last (uInt index=0);
    ssize_t last (uInt index=0) const;
    // </group>

    // Get the storage.
    const ssize_t *storage() const;

    // Append this IPosition with another one (causing a resize).
    void append (const IPosition& other);

    // Prepend this IPosition with another one (causing a resize).
    void prepend (const IPosition& other);

    // Return an IPosition as the concetanation of this and another IPosition.
    IPosition concatenate (const IPosition& other) const;

    // Set the first values of this IPosition to another IPosition.
    // An exception is thrown if the other IPosition is too long.
    void setFirst (const IPosition& other);

    // Set the last values of this IPosition to another IPosition.
    // An exception is thrown if the other IPosition is too long.
    void setLast (const IPosition& other);

    // Construct an IPosition from the first <src>n</src> values of this
    // IPosition.
    // An exception is thrown if <src>n</src> is too high.
    IPosition getFirst (uInt n) const;

    // Construct an IPosition from the last <src>n</src> values of this
    // IPosition.
    // An exception is thrown if <src>n</src> is too high.
    IPosition getLast (uInt n) const;

    // Return an IPosition where the given axes are reoved.
    IPosition removeAxes (const IPosition& axes) const;

    // Return an IPosition containing the given axes only.
    IPosition keepAxes (const IPosition& axes) const;

    // The number of elements in this IPosition. Since IPosition
    // objects use zero-based indexing, the maximum available index is
    // nelements() - 1.
    // <group>
    uInt nelements() const;
    uInt size() const;
    // </group>

    // Is the IPosition empty (i.e. no elements)?
    Bool empty() const;

    // conform returns true if nelements() == other.nelements().
    Bool conform(const IPosition& other) const;

    // Element-by-element arithmetic.
    // <group>
    void operator += (const IPosition& other);
    void operator -= (const IPosition& other);
    void operator *= (const IPosition& other);
    void operator /= (const IPosition& other);
    void operator += (ssize_t val);
    void operator -= (ssize_t val);
    void operator *= (ssize_t val);
    void operator /= (ssize_t val);
    // </group>

    // Returns 0 if nelements() == 0, otherwise it returns the product of
    // its elements.
    Int64 product() const;

    // Are all elements equal to 1?
    // Useful to check if a given stride is really a stride.
    Bool allOne() const;

    // Element-by-element comparison for equality.
    // It returns True if the lengths and all elements are equal.
    // <br>
    // Note that an important difference between this function and operator==()
    // is that if the two IPositions have different lengths, this function
    // returns False, instead of throwing an exception as operator==() does.
    Bool isEqual (const IPosition& other) const;

    // Element-by-element comparison for equality.
    // It returns True if all elements are equal.
    // When <src>skipDegeneratedAxes</src> is True, axes with
    // length 1 are skipped in both operands.
    Bool isEqual (const IPosition& other, Bool skipDegeneratedAxes) const;

    // Element-by-element comparison for (partial) equality.
    // It returns True if the lengths and the first <src>nrCompare</src>
    // elements are equal.
    Bool isEqual (const IPosition& other, uInt nrCompare) const;

    // Is the other IPosition a subset of (or equal to) this IPosition?
    // It is a subset if zero or more axes of this IPosition do not occur
    // or are degenerated in the other and if the remaining axes are
    // in the same order.
    Bool isSubSet (const IPosition& other) const;

    // Write the IPosition into a String.
    String toString() const;

    // Write an IPosition to an ostream in a simple text form.
    friend std::ostream& operator<<(std::ostream& os, const IPosition& ip);

    // Write an IPosition to an AipsIO stream in a binary format.
    friend AipsIO& operator<<(AipsIO& aio, const IPosition& ip);

    // Write an IPosition to a LogIO stream.
    friend LogIO& operator<<(LogIO& io, const IPosition& ip);

    // Read an IPosition from an AipsIO stream in a binary format.
    // Will throw an AipsError if the current IPosition Version does not match
    // that of the one on disk.
    friend AipsIO& operator>>(AipsIO& aio, IPosition& ip);

    // Is this IPosition consistent?
    Bool ok() const;

    // Define the STL-style iterators.
    // It makes it possible to iterate through all data elements.
    // <srcblock>
    //  IPosition shp(2,0);
    //  for (IPosition::iterator iter=shp.begin(); iter!=shp.end(); iter++) {
    //    *iter += 1;
    //  }
    // </srcblock>
    // <group name=STL-iterator>
    // STL-style typedefs.
    // <group>
    typedef ssize_t               value_type;
    typedef ssize_t*              iterator;
    typedef const ssize_t*        const_iterator;
    typedef value_type*       pointer;
    typedef const value_type* const_pointer; 
    typedef value_type&       reference;
    typedef const value_type& const_reference;
    typedef size_t            size_type;
    typedef ptrdiff_t         difference_type;
    // </group>
    // Get the begin and end iterator object for this object.
    // <group>
    iterator begin()
      { return data_p; }
    const_iterator begin() const
      { return data_p; }
    iterator end()
      { return data_p + size_p; }
    const_iterator end() const
      { return data_p + size_p; }
    // </group>
    // </group>

private:
    // Allocate a buffer with length size_p.
    void allocateBuffer();

    // Throw an index error exception.
    void throwIndexError() const;

    enum { BufferLength = 4 };
    uInt size_p;
    ssize_t buffer_p[BufferLength];
    // When the iposition is length BufferSize or less data is just buffer_p,
    // avoiding calls to new and delete.
    ssize_t *data_p;
};

// <summary>Arithmetic Operations for IPosition's</summary>
// Element by element arithmetic on IPositions.
// <group name="IPosition Arithmetic">
// Each operation is done on corresponding elements of the IPositions. The
// two IPositions must have the same number of elements otherwise an
// exception (ArrayConformanceError) will be thrown.
// <group>
IPosition operator + (const IPosition& left, const IPosition& right);
IPosition operator - (const IPosition& left, const IPosition& right);
IPosition operator * (const IPosition& left, const IPosition& right);
IPosition operator / (const IPosition& left, const IPosition& right);
// </group>
// Each operation is done by appliying the integer argument to all elements
// of the IPosition argument. 
// <group>
IPosition operator + (const IPosition& left, ssize_t val);
IPosition operator - (const IPosition& left, ssize_t val);
IPosition operator * (const IPosition& left, ssize_t val);
IPosition operator / (const IPosition& left, ssize_t val);
// </group>
// Same functions as above but with with the Int argument on the left side.
// <group>
IPosition operator + (ssize_t val, const IPosition& right);
IPosition operator - (ssize_t val, const IPosition& right);
IPosition operator * (ssize_t val, const IPosition& right);
IPosition operator / (ssize_t val, const IPosition& right);
// </group>

// Returns the element by element minimum or maximum.
// <group>
IPosition max (const IPosition& left, const IPosition& right);
IPosition min (const IPosition& left, const IPosition& right);
// </group>
// </group>

// <summary>Logical operations for IPosition's</summary>
// Element by element boolean operations on IPositions. The result is true
// only if the operation yields true for every element of the IPosition.
// <group name="IPosition Logical">
// Each operation is done on corresponding elements of the IPositions. The
// two IPositions must have the same number of elements otherwise an
// exception (ArrayConformanceError) will be thrown.
// <group>
Bool operator == (const IPosition& left, const IPosition& right);
Bool operator != (const IPosition& left, const IPosition& right);
Bool operator <  (const IPosition& left, const IPosition& right);
Bool operator <= (const IPosition& left, const IPosition& right);
Bool operator >  (const IPosition& left, const IPosition& right);
Bool operator >= (const IPosition& left, const IPosition& right);
// </group>
// Each operation is done by appliying the integer argument to all elements
// <group>
Bool operator == (const IPosition& left, ssize_t val);
Bool operator != (const IPosition& left, ssize_t val);
Bool operator <  (const IPosition& left, ssize_t val);
Bool operator <= (const IPosition& left, ssize_t val);
Bool operator >  (const IPosition& left, ssize_t val);
Bool operator >= (const IPosition& left, ssize_t val);
// </group>
// Same functions as above but with with the Int argument on the left side.
// <group>
Bool operator == (ssize_t val, const IPosition& right);
Bool operator != (ssize_t val, const IPosition& right);
Bool operator <  (ssize_t val, const IPosition& right);
Bool operator <= (ssize_t val, const IPosition& right);
Bool operator >  (ssize_t val, const IPosition& right);
Bool operator >= (ssize_t val, const IPosition& right);
// </group>
// </group>

// <summary>Indexing functions for IPosition's</summary>
// Convert between IPosition and offset in an array.
//
// The offset of an element in an array is the number of elements from the
// origin that the element would be if the array were arranged linearly.
// The origin of the array has an offset equal to 0, while the
// "top right corner" of the array has an offset equal to one less than the
// total number of elements in the array.
//
// Two examples of offset would be the index in a carray and the seek position
// in a file.

// <group name="IPosition Indexing">
// Convert from offset to IPosition in an array.
IPosition toIPositionInArray (Int64 offset, const IPosition& shape);

// Convert from IPosition to offset in an array.
Int64 toOffsetInArray (const IPosition& iposition, const IPosition& shape);

// Determine if the given offset or IPosition is inside the array. Returns
// True if it is inside the Array.
// <thrown>
//   <li> ArrayConformanceError: If all the IPositions are not the same length
// </thrown>
// <group>
Bool isInsideArray (const Int64 offset, const IPosition& shape);
Bool isInsideArray (const IPosition& iposition, const IPosition& shape);
// </group>
// </group>


    
//# Inlined member functions for IPosition

inline IPosition::IPosition()
: size_p (0),
  data_p (buffer_p)
{}

inline IPosition IPosition::makeAxisPath (uInt nrdim)
{
    return makeAxisPath (nrdim, IPosition());
}

inline uInt IPosition::nelements() const
{
    return size_p;
}
inline uInt IPosition::size() const
{
    return size_p;
}
inline Bool IPosition::empty() const
{
    return size_p == 0;
}

inline ssize_t& IPosition::operator[](uInt index)
{
    return data_p[index];
}

inline ssize_t IPosition::operator[](uInt index) const
{
    return data_p[index];
}

inline ssize_t& IPosition::operator()(uInt index)
{
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if (index >= nelements()) {
	throwIndexError();
    }
#endif
    return data_p[index];
}

inline ssize_t IPosition::operator()(uInt index) const
{
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if (index >= nelements()) {
	throwIndexError();
    }
#endif
    return data_p[index];
}

inline ssize_t& IPosition::last (uInt index)
{
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if (size_p - index <= 0) {
	throwIndexError();
    }
#endif
    return data_p[size_p-index-1];
}

inline ssize_t IPosition::last (uInt index) const
{
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if (size_p - index <= 0) {
	throwIndexError();
    }
#endif
    return data_p[size_p-index-1];
}

inline const ssize_t *IPosition::storage() const
{
    return data_p;
}

inline Bool IPosition::conform(const IPosition& other) const
{
    return  (size_p == other.size_p);
}

} //# NAMESPACE CASACORE - END

#endif
