//# Array.h: A templated N-D Array class with zero origin
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999
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

#if !defined(AIPS_ARRAY_H)
#define AIPS_ARRAY_H


//# Includes
#include <aips/aips.h>
#include <aips/Containers/Block.h>
#include <aips/Utilities/CountedPtr.h>
#include <aips/Arrays/ArrayRtti.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/LogiArray.h>
#include <aips/Arrays/MaskLogiArrFwd.h>
#include <aips/Arrays/IPosition.h>

//# Forward Declarations
class AipsIO;
template<class T> class ArrayIterator;
template<class T> class MaskedArray;
template<class T> class Matrix;
template<class Domain, class Range> class Functional;


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


// <summary> A templated N-D Array class with zero origin </summary>

// Array<T> is a templated, N-dimensional, Array class. The origin is zero,
// but by default indices are zero-based. This Array class is the
// base class for specialized Vector<T>, Matrix<T>, and Cube<T> classes.
//
// Indexing into the array, and positions in general, are given with IPosition
// (essentially a vector of integers) objects. That is, an N-dimensional 
// array requires a length-N IPosition to define a position within the array.
// Unlike C, indexing is done with (), not []. Also, the storage order
// is the same as in FORTRAN, i.e. memory varies most rapidly with the first
// index.
// <srcblock>
//                                     // axisLengths = [1,2,3,4,5]
// IPosition axisLengths(5, 1, 2, 3, 4, 5); 
// Array<Int> ai(axisLengths);         // ai is a 5 dimensional array of
//                                     // integers; indices are 0-based
//                                     // => ai.nelements() == 120
// Array<Int> ai2(axisLengths);        // The first element is at index 0
// IPosition zero(5); zero = 0;        // [0,0,0,0,0]
// //...
// </srcblock>
// Indexing into an N-dimensional array is relatively expensive. Normally
// you will index into a Vector, Matrix, or Cube. These may be obtained from
// an N-dimensional array by creating a reference, or by using an 
// ArrayIterator. The "shape" of the array is an IPosition which gives the
// length of each axis.
//
// An Array may be standalone, or it may refer to another array, or to
// part of another array (by refer we mean that if you change a pixel in 
// the current array, a pixel in the referred to array also changes, i.e.
// they share underlying storage).
// <note role=warning> One way one array can reference another is through the copy 
//        constructor. While this might be what you want, you should
//        probably use the reference() member function to make it explicit.
//        The copy constructor is used when arguments are passed by value;
//        normally functions should not pass Arrays by value, rather they
//        should pass a reference or a const reference. On the positive
//        side, returning an array from a function is efficient since no
//        copying need be done. Later releases of the array classes might
//        have the copy constructor actually make a copy -- comments solicited.
//
// Aside from the explicit reference() member function, a user will
// most commonly encounter an array which references another array
// when he takes an array slice (or section). A slice is a sub-region of
// an array (which might also have a stride: every nth row, every mth column,
// ...).
// <srcblock>
// IPosition lengths(3,10,20,30);
// Array<Int> ai(lengths);         // A 10x20x30 cube
// Cube<Int> ci;
// //...
// ci.reference(ai1);              // ci and ai now reference the same
//                                 // storage
// ci(0,0,0) = 123;                // Can use Cube indexing
// ci.xyPlane(2) = 0;              //     and other member functions
// IPosition zero(3,0,0,0);
// assert(ai(zero) == 123);        // True because ai, ci are references
// //...
// Array<Int> subArray;
// IPosition blc(3,0,0,0), trc(3,5,5,5);
// subArray.reference(ai(blc, trc));
// subArray = 10;                  // All of subArray, which is the
//                                 // subcube from 0,0,0 to 5,5,5 in
//                                 // ai, has the value 10.
// </srcblock>
// While the last example has an array slice referenced explicitly by another
// array variable, normally the user will often only use the slice as
// a temporary in an expresion, for example:
// <srcblock>
//   Array<Complex> array;
//   IPosition blc, trc, offset;
//   //...
//   // Copy from one region of the array into another
//   array(blc, trc) = array(blc+offset, trc+offset);
// </srcblock>
//
// The Array classes are intended to operate on relatively large
// amounts of data. While they haven't been extensively tuned yet,
// they are relatively efficient in terms of speed. Presently they
// are not space efficient -- the overhead is about 15 words. While
// this will be improved (probably to about 1/2 that), these array
// classes are not appropriate for very large numbers of very small
// arrays. The Block<T> class may be what you want in this circumstance.
//
// Element by element mathematical and logical operations are available
// for arrays (defined in aips/ArrayMath.h and aips/ArrayLogical.h).
// Because arithmetic and logical functions are split out, it is possible
// to create an Array<T> (and hence Vector<T> etc) for any type T that has
// a default constructor, assignment operator, and copy constructor. In
// particular, Array<String> works.
//
// If compiled with the preprocessor symbol AIPS_DEBUG symbol, array
// consistency ("invariants") will be checked in most member
// functions, and indexing will be range-checked. This should not be
// defined for production runs.
//
// A tutorial for the ArrayClasses is available in the "AIPS++ Programming
// Manual."
//
// <note role=tip>
// Most of the data members and functions which are "protected" should
// likely become "private".
//
// <todo asof="1999/12/30"
//   <li> Integrate into the Lattice hierarchy
//   <li> Factor out the common functions (shape etc) into a type-independent
//        base class.
// </todo>

template<class T> class Array
{
public:

    // Result has dimensionality of zero, and  nelements is zero.
    Array();

    // Create an array of the given shape, i.e. after construction
    // array.ndim() == shape.nelements() and array.shape() == shape.
    // The origin of the Array is zero.
    explicit Array(const IPosition &shape);

    // Create an array of the given shape and initialize it with the
    // initial value.
    Array(const IPosition &shape, const T &initialValue);

    // After construction, this and other reference the same storage.
    Array(const Array<T> &other);

    // Create an Array of a given shape from a pointer.
    Array(const IPosition &shape, T *storage, StorageInitPolicy policy = COPY);
    // Create an Array of a given shape from a pointer. Because the pointer
    // is const, a copy is always made.
    Array(const IPosition &shape, const T *storage);

    // Frees up storage only if this array was the last reference to it.
    virtual ~Array();

    // Set every element of the array to "value." Also could use the
    // assignment operator which assigns an array from a scalar.
    void set(const T &value);

    // Apply the function to every element of the array. This modifies
    // the array in place.
    // <group>
    // This version takes a function which takes a T and returns a T.
    void apply(T (*function)(T));
    // This version takes a function which takes a const T reference and
    // returns a T.
    void apply(T (*function)(const T &));
    // This version applies a functional.
    void apply(const Functional<T,T> &function);
    // </group>

    // After invocation, this array and other reference the same storage. That
    // is, modifying an element through one will show up in the other. The
    // arrays appear to be identical; they have the same shape.
    virtual void reference(Array<T> &other);

    // Copy the values in other to this. If the array on the left hand
    // side has no elements, then it is resized to be the same size as
    // as the array on the right hand side. Otherwise, the arrays must
    // conform (same shapes).
    // <srcblock>
    // IPosition shape(2,10,10);     // some shape
    // Array<Double> ad(shape);
    // //...
    // Array<Double> ad2;            // N.B. ad2.nelements() == 0
    // ad2 = ad;                     // ad2 resizes, then elements
    //                               //     are copied.
    // shape = 20;
    // Array<Double> ad3(shape);
    // ad3 = ad;                     // Error: arrays do not conform
    // </srcblock>
    virtual Array<T> &operator=(const Array<T> &other);

    // Set every element of this array to "value". In other words, a scalar
    // behaves as if it were a constant conformant array.
    Array<T> &operator=(const T &value);

    // Copy to this those values in marray whose corresponding elements
    // in marray's mask are True.
    //
    // <thrown>
    //    <li> ArrayConformanceError
    // </thrown>
    //
    Array<T> &operator= (const MaskedArray<T> &marray);

    // This makes a copy of the array and returns it. This can be
    // useful for, e.g. making working copies of function arguments
    // that you can write into.
    // <srcblock>
    // void someFunction(const Array<Int> &arg)
    // {
    //     Array<Int> tmp(arg.copy());
    //     // ...
    // }
    // </srcblock>
    // Note that since the copy constructor makes a reference, if we just
    // created used to copy constructor, modifying "tmp" would also
    // modify "arg". Clearly another alternative would simply be:
    // <srcblock>
    // void someFunction(const Array<Int> &arg)
    // {
    //     Array<Int> tmp;
    //     tmp = arg;
    //     // ...
    // }
    // </srcblock>
    // which likely would be simpler to understand. (Should copy() 
    // be deprecated and removed?)
    Array<T> copy() const;                         // Make a copy of this

    // This ensures that this array does not reference any other storage.
    // <note role=tip> When a section is taken of an array with non-unity strides,
    //        storage can be wasted if the array which originally contained
    //        all the data goes away. unique() also reclaims storage. This
    //        is an optimization users don't normally need to understand.
    //
    //        <srcblock>
    //        IPosition shape(...), blc(...), trc(...), inc(...);
    //        Array<Float> af(shape);
    //        inc = 2; // or anything > 1
    //        Array<Float> aSection.reference(af(blc, trc, inc));
    //        af.reference(anotherArray);
    //        // aSection now references storage that has a stride
    //        // in it, but nothing else is. Storage is wasted.
    //        aSection.unique();
    //        </srcblock>
    void unique();

    // It is occasionally useful to have an array which access the same
    // storage appear to have a different shape. For example,
    // turning an N-dimensional array into a Vector.
    // <br>When the array data are contiguous, the array can be reshaped
    // to any form as long as the number of elements stays the same.
    // When not contiguous, it is only possible to remove or add axes
    // with length 1.
    // <srcblock>
    // IPosition squareShape(2,5,5);
    // Array<Float> square(squareShape);
    // IPosition lineShape(1,25);
    // Vector<Float> line(square.reform(lineShape));
    // // "square"'s storage may now be accessed through Vector "line"
    // </srcblock>
    Array<T> reform(const IPosition &shape) const;
    
    // These member functions remove degenerate (ie. length==1) axes from
    // Arrays.  Only axes greater than startingAxis are considered (normally
    // one wants to remove trailing axes. The first two of these function
    // return an Array reference with axes removed. The last of these functions
    // returns a reference to the 'other' array with degenerated axes removed.
    // <br>
    // The functions with argument <src>ignoreAxes</src> do
    // not consider the axes given in that argument..
    // <note role=caution> When the two functions returning void throw
    // are invoked on a derived object (e.g. Matrix), an exception is
    // thrown if removing the degenerate axes from other does not result
    // in a correct number of axes.
    // </note>
    // <group>
    Array<T> nonDegenerate(uInt startingAxis=0);
    const Array<T> nonDegenerate(uInt startingAxis=0) const;
    void nonDegenerate(Array<T> &other, uInt startingAxis=0);
    Array<T> nonDegenerate(const IPosition& ignoreAxes);
    const Array<T> nonDegenerate(const IPosition& ignoreAxes) const;
    void nonDegenerate(Array<T> &other, const IPosition &ignoreAxes)
        { doNonDegenerate (other, ignoreAxes); }
    // </group> 

    // These member functions return an Array reference with the specified
    // number of extra axes, all of length one, appended to the end of the
    // Array. Note that the <src>reform</src> function can also be
    // used to add extra axes.
    // <group> 
    Array<T> addDegenerate(uInt numAxes);
    const Array<T> addDegenerate(uInt numAxes) const;
    // </group>

    // Make this array a different shape. Presently the old values are not
    // copied over to the new array.
    // Resize without argument is equal to resize(IPosition()).
    // <group>
    virtual void resize();
    virtual void resize(const IPosition &newShape);
    // </group>

    // Access a single element of the array. This is relatively
    // expensive. Extensive indexing should be done through one
    // of the Array specializations (Vector, Matrix, Cube). If
    // AIPS_DEBUG is defined, index checking will be performed.
    // <group>
    T &operator()(const IPosition &);
    const T &operator()(const IPosition &) const;
    // </group>

    // Ret a reference to an array which extends from "start" to end."
    //
    // <group>

    Array<T> operator()(const IPosition &start, const IPosition &end);

    // Along the ith axis, every inc[i]'th element is chosen.
    Array<T> operator()(const IPosition &start, const IPosition &end,
			const IPosition &inc);
    // </group>


    // The array is masked by the input LogicalArray.
    // This mask must conform to the array.
    // <group>
    MaskedArray<T> operator() (const LogicalArray &mask) const;
    MaskedArray<T> operator() (const LogicalArray &mask);
    // </group>

    // The array is masked by the input MaskedLogicalArray.
    // The mask is effectively the AND of the internal LogicalArray
    // and the internal mask of the MaskedLogicalArray.
    // The MaskedLogicalArray must conform to the array.
    // <group>
    MaskedArray<T> operator() (const MaskedLogicalArray &mask) const;
    MaskedArray<T> operator() (const MaskedLogicalArray &mask);
    // </group>

    // The number of references the underlying storage has assigned to it.
    // It is 1 unless there are outstanding references to the storage (e.g.,
    // through a slice). Normally you have no need to do this since the
    // arrays handle all of the references for you.
    uInt nrefs() const;

    // The dimensionality of this array.
    uInt ndim() const { return ndimen_p; }

    // How many elements does this array have? Product of all axis lengths.
    uInt nelements() const { return nels_p; }

    // Check to see if the Array is consistent. This is about the same thing
    // as checking for invariants. If AIPS_DEBUG is defined, this is invoked
    // after construction and on entry to most member functions.
    virtual Bool ok() const;

    // Are the shapes identical?
    // <group>
    Bool conform(const Array<T> &other) const;
    Bool conform(const MaskedArray<T> &other) const;
    // </group>

    // The length of each axis.
    const IPosition &shape() const { return length_p; }

    // A convenience function: end(i) = shape(i) - 1; i.e. this
    // is the IPosition of the last element of the Array.
    IPosition end() const;

    // Are the array data contiguous?
    // If they are not contiguous, <src>getStorage</src> (see below)
    // needs to make a copy.
    Bool contiguousStorage() const { return contiguous_p; }

    // Generally use of this should be shunned, except to use a FORTRAN routine
    // or something similar. Because you can't know the state of the underlying
    // data layout (in particular, if there are increments) sometimes the
    // pointer returned will be to a copy, but often this won't be necessary.
    // A boolean is returned which tells you if this is a copy (and hence the
    // storage must be deleted). Note that if you don't do anything unusual,
    // getStorage followed by freeStorage or putStorage will do the deletion
    // for you (if required). e.g.:
    // <srcblock>
    // Array<Int> a(shape); ...
    // Bool deleteIt; Int *storage = a.getStorage(deleteIt);
    // foo(storage, a.nelements()); a.puStorage(storage, deleteIt);
    // // or a.freeStorage(storage, deleteIt) if a is const.
    // </srcblock>
    // NB: However, if you only use getStorage, you will have to delete the
    // pointer yourself using freeStorage().
    //
    // It would probably be useful to have corresponding "copyin" "copyout"
    // functions that used a user supplied buffer.
    // Note that deleteIt is set in this function.
    // <group>
    T *getStorage(Bool &deleteIt);
    const T *getStorage(Bool &deleteIt) const;
    // </group>

    // putStorage() is normally called after a call to getStorage() (cf).
    // The "storage" pointer is set to zero.
    void putStorage(T *&storage, Bool deleteAndCopy);

    // If deleteIt is set, delete "storage". Normally freeStorage calls
    // will follow calls to getStorage. The reason the pointer is "const"
    // is because only const pointers are released from const arrays.
    // The "storage" pointer is set to zero.
    void freeStorage(const T *&storage, Bool deleteIt) const;

    // Replace the data values with those in the pointer <src>storage</src>.
    // The results are undefined is storage does not point at nelements() or
    // more data elements. After takeStorage() is called, <src>unique()</src>
    // is True.
    // <group>
    virtual void takeStorage(const IPosition &shape, T *storage,
		     StorageInitPolicy policy = COPY);
    // Since the pointer is const, a copy is always taken.
    virtual void takeStorage(const IPosition &shape, const T *storage);
    // </group>


    // Used to iterate through Arrays. Derived classes VectorIterator and
    // MatrixIterator are probably more useful.
    friend class ArrayIterator<T>;

    // Needed to be a friend for Matrix<T>::reference()
    friend class Matrix<T>;

    // Array version for major change (used by ArrayIO).
    // enum did not work properly with cfront 3.0.1), so replaced
    // by a static inline function. Users won't normally use this.
    static uInt arrayVersion() {return 3;}

    // Macro to define the typeinfo member functions.
    rtti_dcl_mbrf(Array<T>);


protected:
    // Remove the degenerate axes from the Array object.
    // This is the implementation of the nonDegenerate functions.
    // It has a different name to be able to make it virtual without having
    // the "hide virtual function" message when compiling derived classes.
    virtual void doNonDegenerate(Array<T> &other, const IPosition &ignoreAxes);


    // Number of elements in the array. Cached rather than computed.
    uInt nels_p;

    // Dimensionality of the array.
    uInt ndimen_p;

    // Used to hold the shape, increment into the underlying storage
    // and originalLength of the array.
    IPosition length_p, inc_p, originalLength_p;

    // Reference counted block that contains the storage.
    CountedPtr<Block<T> > data_p;

    // This pointer is adjusted to point to the first element of the array.
    // It is not necessarily the same thing as data->storage() since
    // this array might be a section, e.g. have a blc which shifts us forward
    // into the block.
    T *begin_p;

    // A switch to tell if the array data are contiguous.
    Bool contiguous_p;


    // Various helper functions.
    // <group>
    void validateConformance(const Array<T> &) const;
    void validateIndex(const IPosition &) const;
    Bool isStorageContiguous() const;
    // </group>
};


// <summary> General global functions for Arrays. </summary>
// <reviewed reviewer="" date="" tests="tArray">
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
uInt ArrayVolume(uInt Ndim, const Int *Shape);

// 
// What is the linear index into an "Ndim" dimensional array of the given
// "Shape", "Origin", and "Increment" for a given IPosition Index.
//  An Array helper function.
// <group>
uInt ArrayIndexOffset(uInt Ndim, const Int *Shape, 
		      const Int *Origin, const Int *Inc, 
		      const IPosition &Index);
uInt ArrayIndexOffset(uInt Ndim, const Int *Shape, 
		      const Int *Inc, const IPosition &Index);
// </group>

// 
// Test conformance for two arrays of different types.
// Are the shapes identical?
// <group name=conform2>
template<class T, class U>
  Bool conform2 (const Array<T> &left, const Array<U> &right);
// </group>

// </group>

#endif
