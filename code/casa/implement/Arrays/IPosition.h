//# IPosition.h: A vector of integers, used to index into arrays.
//# Copyright (C) 1994,1995,1996,1997,1998
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

#if !defined (AIPS_IPOSITION_H)
#define AIPS_IPOSITION_H

//# Includes
#include <aips/aips.h>

//# Forward Declarations
class AipsIO;
class LogIO;
template<class T> class Array;
template<class T> class Vector;
#if defined(AIPS_STDLIB)
#include <iosfwd.h>
#else
imported class ostream;
#endif


// <summary> A Vector of integers, for indexing into Array<T> objects. </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

//# <prerequisite>
//# Classes you should understand before using this one.
//# </prerequisite>

// <etymology>
// IPosition is an Index Position in an n-dimensional array.
// </etymology>

// <synopsis> 
// IPosition is "logically" a Vector<Int> constrained so that it's origin
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
    IPosition(uInt length, Int val);

    // An IPosition of size "length" with defined values. You need to supply
    // a value for each element of the IPosition (up to 10). [Unfortunately
    // varargs might not be sufficiently portable.]
    IPosition (uInt length, Int val0, Int val1, Int val2=MIN_INT, 
	       Int val3=MIN_INT, Int val4=MIN_INT, Int val5=MIN_INT,
	       Int val6=MIN_INT, Int val7=MIN_INT, Int val8=MIN_INT, 
	       Int val9=MIN_INT);

    // Makes a copy (copy, NOT reference, semantics) of other.
    IPosition(const IPosition& other);
    
    ~IPosition();

    // Makes this a copy of other. "this" and "other" must either be conformant
    // (same size) or this must be 0-length, in which case it will
    // resize itself to be the same length as other.
    IPosition& operator=(const IPosition& other);

    // Copy "value" into every position of this IPosition.
    IPosition& operator=(Int value);

    // Construct a default axis path consisting of the values 0 .. nrdim-1.
    static IPosition makeAxisPath (uInt nrdim);

    // Construct a full axis path from a (partially) given axis path.
    // It fills the path with the non-given axis.
    // It is checked if the given axis path is valid (i.e. if the axis are
    // < nrdim and if they are not doubly defined).
    // E.g.: in the 4D case an axis path [0,2] is returned as [0,2,1,3].
    static IPosition makeAxisPath (uInt nrdim, const IPosition& partialPath);

    // Convert an IPosition to and from an Array<Int>. In either case, the
    // array must be one dimensional.
    // <group>
    IPosition(const Array<Int>& other);
    Vector<Int> asVector() const;
    // </group>

    // This member functions return an IPosition which has
    // degenerate (length==1) axes removed and the dimensionality reduced 
    // appropriately.
    // Only axes greater than startingAxis are considered (normally one 
    // wants to remove trailing axes.
    // <br>
    // The functions with argument <src>ignoreAxes</src> do
    // not consider the axes given in that argument..
    // <group>
    IPosition nonDegenerate(uInt startingAxis=0) const;
    IPosition nonDegenerate(const IPosition& ignoreAxes) const;
    // </group>

    // Old values are copied on resize if copy==True..
    // If the size increases, values beyond the former size are undefined.
    void resize(uInt newSize, Bool copy=True);

    // Index into the IPosition. Indices are zero-based. If the preprocessor
    // symbol AIPS_ARRAY_INDEX_CHECK is defined, "index" will be
    // checked to ensure it is not out of bounds. If this check fails, an
    // AipsError will be thrown.
    // <group>
    Int& operator()(uInt index);
    Int operator()(uInt index) const;
    // </group>

    // Get the storage.
    const Int *storage() const;

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

    // The number of elements in this IPosition. Since IPosition
    // objects use zero-based indexing, the maximum available index is
    // nelements() - 1.
    uInt nelements() const;

    // conform returns true if nelements() == other.nelements().
    Bool conform(const IPosition& other) const;

    // Element-by-element arithmetic.
    // <group>
    void operator += (const IPosition& other);
    void operator -= (const IPosition& other);
    void operator *= (const IPosition& other);
    void operator /= (const IPosition& other);
    void operator += (Int val);
    void operator -= (Int val);
    void operator *= (Int val);
    void operator /= (Int val);
    // </group>

    // Returns 0 if nelements() == 0, otherwise it returns the product of
    // its elements.
    Int product() const;

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

    // Write an IPosition to an ostream in a simple text form.
    friend ostream& operator<<(ostream& os, const IPosition& ip);

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

private:
    // Allocate a buffer with length size_p.
    void allocateBuffer();

    // Throw an index error exception.
    void throwIndexError() const;

    enum { IPositionVersion = 1, BufferLength = 4 };
    uInt size_p;
    Int buffer_p[BufferLength];
    // When the iposition is length BufferSize or less data is just buffer_p,
    // avoiding calls to new and delete.
    Int *data_p;
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
IPosition operator + (const IPosition& left, Int val);
IPosition operator - (const IPosition& left, Int val);
IPosition operator * (const IPosition& left, Int val);
IPosition operator / (const IPosition& left, Int val);
// </group>
// Same functions as above but with with the Int argument on the left side.
// <group>
IPosition operator + (Int val, const IPosition& right);
IPosition operator - (Int val, const IPosition& right);
IPosition operator * (Int val, const IPosition& right);
IPosition operator / (Int val, const IPosition& right);
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
Bool operator == (const IPosition& left, Int val);
Bool operator != (const IPosition& left, Int val);
Bool operator <  (const IPosition& left, Int val);
Bool operator <= (const IPosition& left, Int val);
Bool operator >  (const IPosition& left, Int val);
Bool operator >= (const IPosition& left, Int val);
// </group>
// Same functions as above but with with the Int argument on the left side.
// <group>
Bool operator == (Int val, const IPosition& right);
Bool operator != (Int val, const IPosition& right);
Bool operator <  (Int val, const IPosition& right);
Bool operator <= (Int val, const IPosition& right);
Bool operator >  (Int val, const IPosition& right);
Bool operator >= (Int val, const IPosition& right);
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
// Convert from offset to IPosition in an array. The second of these
// functions requires that type T have shape and origin members which return
// IPositions.
IPosition toIPositionInArray (const uInt offset, const IPosition& shape);

// Convert from IPosition to offset in an array. The second of these
// functions requires that type T have shape and origin members which return
// IPositions.
uInt toOffsetInArray (const IPosition& iposition, const IPosition& shape);

// Determine if the given offset or IPosition is inside the array. Returns
// True if it is inside the Array. The second and fourth of these functions
// require that type T have shape and origin members which return
// IPositions.
// <thrown>
//   <item> ArrayConformanceError: If all the IPositions are not the same length
// </thrown>
// <group>
Bool isInsideArray (const uInt offset, const IPosition& shape);
Bool isInsideArray (const IPosition& iposition, const IPosition& shape);
// </group>
// </group>


    
//# Inlined member functions for IPosition

inline IPosition::IPosition()
: size_p (0),
  data_p (buffer_p)
{}
inline IPosition::IPosition (uInt length)
: size_p (length),
  data_p (buffer_p)
{
    if (length > BufferLength) {
	allocateBuffer();
    }
}

inline IPosition IPosition::makeAxisPath (uInt nrdim)
{
    return makeAxisPath (nrdim, IPosition());
}

inline uInt IPosition::nelements() const
{
    return size_p;
}

inline Int& IPosition::operator()(uInt index)
{
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if (index >= nelements()) {
	throwIndexError();
    }
#endif
    return data_p[index];
}

inline Int IPosition::operator()(uInt index) const
{
#if defined(AIPS_ARRAY_INDEX_CHECK)
    if (index >= nelements()) {
	throwIndexError();
    }
#endif
    return data_p[index];
}

inline const Int *IPosition::storage() const
{
    return data_p;
}

inline Bool IPosition::conform(const IPosition& other) const
{
    return ToBool (size_p == other.size_p);
}
// </group>
#endif
