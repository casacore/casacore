//# Array.h: A templated N-D Array class with zero origin
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

#ifndef CASA_ARRAY_H
#define CASA_ARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayBase.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Arrays/MaskLogiArrFwd.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/ostream.h>
#include <iterator>
#if defined(WHATEVER_VECTOR_FORWARD_DEC)
WHATEVER_VECTOR_FORWARD_DEC;
#else
#include <casacore/casa/stdvector.h>
#endif

namespace casacore { //#Begin casa namespace
//# Forward Declarations
class AipsIO;
class Slice;
class Slicer;
template<class T> class Matrix;
template<class T> class ArrayIterator;
template<class T> class MaskedArray;
template<class Domain, class Range> class Functional;
//template <class T, class U> class vector; 


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
// <note role=warning>
//        One way one array can reference another is through the copy 
//        constructor. While this might be what you want, you should
//        probably use the reference() member function to make it explicit.
//        The copy constructor is used when arguments are passed by value;
//        normally functions should not pass Arrays by value, rather they
//        should pass a reference or a const reference. On the positive
//        side, returning an array from a function is efficient since no
//        copying need be done.
// </note>
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
// <note role=tip>
// Most of the data members and functions which are "protected" should
// likely become "private".
// </note>
//
// <todo asof="1999/12/30">
//   <li> Integrate into the Lattice hierarchy
//   <li> Factor out the common functions (shape etc) into a type-independent
//        base class.
// </todo>

template<class T> class Array : public ArrayBase
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

    // Make an empty array of the same template type.
    virtual CountedPtr<ArrayBase> makeArray() const;

    // Assign the other array to this array.
    // If the shapes mismatch, this array is resized.
    // <group>
    virtual void assign (const Array<T>& other);
    virtual void assignBase (const ArrayBase& other, Bool checkType=True);
    // </group>

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
    // <br>Please note that this function makes it possible to reference a
    // const Array, thus effectively it makes a const Array non-const.
    // Although this may seem undesirable at first sight, it is necessary to
    // be able to make references to temporary Array objects, in particular to 
    // Array slices. Otherwise one first needs to use the copy constructor.
    //# The const has been introduced on 2005-Mar-31 because of the hassle
    //# involved in calling the copy ctor before reference.
    virtual void reference(const Array<T> &other);

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
    // Note that the assign function can be used to assign a
    // non-conforming array.
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

    // This function copies the matching part of from array to this array.
    // The matching part is the part with the minimum size for each axis.
    // E.g. if this array has shape [4,5,6] and from array has shape [7,3],
    // the matching part has shape [4,3].
    // <br>Note it is used by the resize function if
    // <src>copyValues==True</src>.
    void copyMatchingPart (const Array<T> &from);

    // This ensures that this array does not reference any other storage.
    // <note role=tip>
    //        When a section is taken of an array with non-unity strides,
    //        storage can be wasted if the array, which originally contained
    //        all the data, goes away. unique() also reclaims storage. This
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
    // </note>
    void unique();

    // Create an STL vector from an Array. The created vector is a linear
    // representation of the Array memory. See
    // <linkto class=Vector>Vector</linkto>  for
    // details of the operation and its reverse (i.e. creating a 
    // <src>Vector</src> from a <src>vector</src>), and for details of
    // definition and instantiation.
    // <group>
    template <class U>
    void tovector(vector<T, U> &out) const;

    vector<T> tovector() const;
    // </group>

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
    // one wants to remove trailing axes). The first two of these functions
    // return an Array reference with axes removed. The latter two functions
    // let this Array object reference the 'other' array with degenerated axes
    // removed.
    // <br>
    // Unless throwIfError is False, an exception will be thrown if
    // startingAxis exceeds the array's dimensionality.
    // <br>
    // The functions with argument <src>ignoreAxes</src> do
    // not consider the axes given in that argument. In this way it can be
    // achieved that degenerate axes are kept.
    // <note role=caution> When the two functions returning <src>void</src>
    // are invoked on a derived object (e.g. Matrix), an exception is
    // thrown if removing the degenerate axes from other does not result
    // in a correct number of axes.
    // </note>
    // <group>
    Array<T> nonDegenerate(uInt startingAxis=0, Bool throwIfError=True) const;
    Array<T> nonDegenerate(const IPosition& ignoreAxes) const;
    void nonDegenerate(const Array<T> &other, uInt startingAxis=0,
		       Bool throwIfError=True);
    void nonDegenerate(const Array<T> &other, const IPosition &ignoreAxes)
        { doNonDegenerate (other, ignoreAxes); }
    // </group> 

    // Remove degenerate axes from this Array object.
    // Note it does not make sense to use these functions on a derived object
    // like Matrix, because it is not possible to remove axes from them.
    // <group>
    void removeDegenerate(uInt startingAxis=0,
                          Bool throwIfError=True);
    void removeDegenerate(const IPosition &ignoreAxes);
    // </group>

    // This member function returns an Array reference with the specified
    // number of extra axes, all of length one, appended to the end of the
    // Array. Note that the <src>reform</src> function can also be
    // used to add extra axes.
    // <group>
    const Array<T> addDegenerate(uInt numAxes) const;
    Array<T> addDegenerate(uInt numAxes);
    // </group>

    // Make this array a different shape. If <src>copyValues==True</src>
    // the old values are copied over to the new array.
    // Copying is done on a per axis basis, thus a subsection with the
    // minimum of the old and new shape is copied.
    // <br>Resize without argument is equal to resize(IPosition()).
    // <br>It is important to note that if multiple Array objects
    // reference the same data storage, this Array object still references
    // the same data storage as the other Array objects if the shape does
    // not change. Otherwise this Array object references newly allocated
    // storage, while the other Array objects still reference the existing
    // data storage.
    // <br>If you want to be sure that the data storage of this Array object
    // is not referenced by other Array objects, the function unique should
    // be called first.
    // <group>
    virtual void resize();
    virtual void resize(const IPosition &newShape, Bool copyValues=False);
    // </group>

    // Access a single element of the array. This is relatively
    // expensive. Extensive indexing should be done through one
    // of the Array specializations (Vector, Matrix, Cube). If
    // AIPS_DEBUG is defined, index checking will be performed.
    // <group>
    T &operator()(const IPosition &);
    const T &operator()(const IPosition &) const;
    // </group>

    // Get a reference to an array section extending
    // from start to end (inclusive).
    // <group>
    Array<T> operator()(const IPosition &start,
                        const IPosition &end);
    const Array<T> operator()(const IPosition &start,
                              const IPosition &end) const;
    // Along the ith axis, every inc[i]'th element is chosen.
    Array<T> operator()(const IPosition &start,
                        const IPosition &end,
			const IPosition &inc);
    const Array<T> operator()(const IPosition &start,
                              const IPosition &end,
                              const IPosition &inc) const;
    // </group>

    // Get a reference to an array section using a Slicer.
    // <group>
    Array<T> operator()(const Slicer&);
    const Array<T> operator()(const Slicer&) const;
    // </group>

    // Get a reference to a section of an array.
    // This is the same as operator(), but can be used in a type-agnostic way.
    virtual CountedPtr<ArrayBase> getSection (const Slicer&) const;

    // Get the subset given by the i-th value of the last axis. So for a cube
    // it returns the i-th xy plane. For a Matrix it returns the i-th row.
    // The returned array references the original array data; its dimensionality
    // is one less. For a 1-dim array it still returns a 1-dim array.
    // <note>This function should not be used in tight loops as it is (much)
    // slower than iterating using begin() and end(), ArrayIter, or
    // ArrayAccessor.</note>
    Array<T> operator[] (size_t i) const;


    // The array is masked by the input LogicalArray.
    // This mask must conform to the array.
    // <group>
    const MaskedArray<T> operator() (const LogicalArray &mask) const;
    MaskedArray<T> operator() (const LogicalArray &mask);
    // </group>

    // The array is masked by the input MaskedLogicalArray.
    // The mask is effectively the AND of the internal LogicalArray
    // and the internal mask of the MaskedLogicalArray.
    // The MaskedLogicalArray must conform to the array.
    // <group>
    const MaskedArray<T> operator() (const MaskedLogicalArray &mask) const;
    MaskedArray<T> operator() (const MaskedLogicalArray &mask);
    // </group>

    // The number of references the underlying storage has assigned to it.
    // It is 1 unless there are outstanding references to the storage (e.g.,
    // through a slice). Normally you have no need to do this since the
    // arrays handle all of the references for you.
    uInt nrefs() const;

    // Check to see if the Array is consistent. This is about the same thing
    // as checking for invariants. If AIPS_DEBUG is defined, this is invoked
    // after construction and on entry to most member functions.
    virtual Bool ok() const;

    // Are the shapes identical?
    // <group>
    Bool conform (const Array<T> &other) const
      { return conform2(other); }
    Bool conform (const MaskedArray<T> &other) const;
    // </group>

    // Get a pointer to the beginning of the array.
    // Note that the array may not be contiguous.
    // <group>
    T* data()
      { return begin_p; }
    const T* data() const
      { return begin_p; }
    // </group>

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
    const T *getStorage(Bool &deleteIt) const
    {
      // The cast is OK because the return pointer will be cast to const
      return const_cast<Array<T>*>(this)->getStorage(deleteIt);
    }
    virtual void *getVStorage(Bool &deleteIt);
    virtual const void *getVStorage(Bool &deleteIt) const;
    // </group>

    // putStorage() is normally called after a call to getStorage() (cf).
    // The "storage" pointer is set to zero.
    void putStorage(T *&storage, Bool deleteAndCopy);
    virtual void putVStorage(void *&storage, Bool deleteAndCopy);

    // If deleteIt is set, delete "storage". Normally freeStorage calls
    // will follow calls to getStorage. The reason the pointer is "const"
    // is because only const pointers are released from const arrays.
    // The "storage" pointer is set to zero.
    void freeStorage(const T *&storage, Bool deleteIt) const;
    void freeVStorage(const void *&storage, Bool deleteIt) const;

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

    // Create an ArrayIterator object of the correct type.
    virtual CountedPtr<ArrayPositionIterator> makeIterator (uInt byDim) const;

    // Needed to be a friend for Matrix<T>::reference()
    friend class Matrix<T>;


    // <group name=STL-iterator>
    // See the function begin() and end() for a detailed description
    // of the STL iterator capability.
    class BaseIteratorSTL
    {
    public:
      // Create the begin const_iterator object for an Array.
      explicit BaseIteratorSTL (const Array<T>&);
      // Create the end const_iterator object for an Array.
      // It also acts as the default constructor.
      explicit BaseIteratorSTL (const T* end = 0)
	: itsPos(end), itsLineEnd(0), itsLineIncr(0), itsLineAxis(0),
	  itsArray(0), itsContig(False) {}

      void nextElem()
      {
	itsPos++;
	if (!itsContig) {
	  itsPos += itsLineIncr;
	  if (itsPos > itsLineEnd) increment();
	}
      }
      void nextLine()
      {
	itsPos = itsLineEnd;
	increment();
      }

      bool operator== (const BaseIteratorSTL& other) const
        { return itsPos == other.itsPos; }

      bool operator!= (const BaseIteratorSTL& other) const
        { return itsPos != other.itsPos; }

      T* getPos()
        { return const_cast<T*>(itsPos); }

      friend ostream& operator<< (ostream& os, const BaseIteratorSTL& iter)
        { os << iter.itsPos; return os; }

    protected:
      // Increment iterator for a non-contiguous array.
      void increment();

      const T*  itsPos;
      const T*  itsLineEnd;
      size_t    itsLineIncr;
      uInt      itsLineAxis;
      IPosition itsCurPos;
      IPosition itsLastPos;
      const Array<T>* itsArray; 
      Bool      itsContig;
    };

    class IteratorSTL: public BaseIteratorSTL
    {
    public:
      // <group name=STL-iterator-typedefs>
      typedef T                 value_type;
      typedef value_type*       pointer;
      typedef value_type&       reference;
      typedef std::size_t       size_type;
      typedef ptrdiff_t         difference_type;
      typedef std::forward_iterator_tag iterator_category;
      // </group>

      // Create the begin iterator object for an Array.
      explicit IteratorSTL (Array<T>& arr)
	: BaseIteratorSTL (arr) {}
      // Create the end iterator object for an Array.
      // It also acts as the default constructor.
      explicit IteratorSTL (const T* end = 0)
	: BaseIteratorSTL (end) {}

      const IteratorSTL& operator++()
      {
	this->nextElem();
	return *this;
      }
      IteratorSTL operator++(int)
      {
	IteratorSTL old(*this);
	this->nextElem();
	return old;
      }

      T& operator*()
        { return *this->getPos(); }
      T* operator->()
        { return this->getPos(); }
    };

  class ConstIteratorSTL: public BaseIteratorSTL
    {
    public:
      // <group name=STL-iterator-typedefs>
      typedef T                 value_type;
      typedef const value_type* pointer;
      typedef const value_type& reference;
      typedef std::size_t       size_type;
      typedef ptrdiff_t         difference_type;
      typedef std::forward_iterator_tag iterator_category;
      // </group>

      // Create the begin const_iterator object for an Array.
      explicit ConstIteratorSTL (const Array<T>& arr)
	: BaseIteratorSTL (arr) {}
      // Create the end const_iterator object for an Array.
      // It also acts as the default constructor.
      explicit ConstIteratorSTL (const T* end = 0)
	: BaseIteratorSTL (end) {}
      // Create from a non-const iterator.
      ConstIteratorSTL (const IteratorSTL& iter)
	: BaseIteratorSTL (iter) {}

      const ConstIteratorSTL& operator++()
      {
	this->nextElem();
	return *this;
      }
      ConstIteratorSTL operator++(int)
      {
	ConstIteratorSTL old(*this);
	this->nextElem();
	return old;
      }

      const T& operator*() const
        { return *this->itsPos; }
      const T* operator->()
        { return this->itsPos; }

      const T* pos() const
        { return this->itsPos; }
    };
    // </group>

    // Define the STL-style iterator functions (only forward iterator).
    // It makes it possible to iterate through all data elements of an array
    // and to use it common STL functions.
    // The end() function is relatively expensive, so it should not be
    // used inside a for statement. It is much better to call it beforehand
    // as shown in the example below. Furthermore it is very important to
    // use <src>++iter</src>, because <src>iter++</src> is 4 times slower.
    // <srcblock>
    //  Array<Int> arr(shape);
    //  Array<Int>::iterator iterend(arr.end());
    //  for (Array<Int>::iterator iter=arr.begin(); iter!=iterend; ++iter) {
    //    *iter += 1;
    //  }
    // </srcblock>
    // The Array class supports random access, so in principle a random
    // iterator could be implemented, but its performance would not be great,
    // especially for non-contiguous arrays.
    // <br>Some other STL like functions exist for performance reasons.
    // If the array is contiguous, it is possible to use the
    // <src>cbegin</src> and <src>cend</src> functions which are
    // about 10% faster.
    // <group name=STL-iterator>
    // STL-style typedefs.
    // <group>
    typedef T                value_type;
    typedef IteratorSTL      iterator;
    typedef ConstIteratorSTL const_iterator;
    typedef T*               contiter;
    typedef const T*         const_contiter;
    // </group>
    // Get the begin iterator object for any array.
    // <group>
    iterator begin()
        { return iterator (*this); }
    const_iterator begin() const
        { return const_iterator (*this); }
    iterator end()
        { return iterator(end_p); }
    const_iterator end() const
        { return const_iterator(end_p); }
    // </group>

    // Get the begin iterator object for a contiguous array.
    // <group>
    contiter cbegin()
        { return begin_p; }
    const_contiter cbegin() const
        { return begin_p; }
    contiter cend()
        { return end_p; }
    const_contiter cend() const
        { return end_p; }
    // </group>

    // </group>


protected:
    // Remove the degenerate axes from the Array object.
    // This is the implementation of the nonDegenerate functions.
    // It has a different name to be able to make it virtual without having
    // the "hide virtual function" message when compiling derived classes.
    virtual void doNonDegenerate(const Array<T> &other,
                                 const IPosition &ignoreAxes);


    // Reference counted block that contains the storage.
    CountedPtr<Block<T> > data_p;

    // This pointer is adjusted to point to the first element of the array.
    // It is not necessarily the same thing as data->storage() since
    // this array might be a section, e.g. have a blc which shifts us forward
    // into the block.
    T *begin_p;

    // The end for an STL-style iteration.
    T* end_p;


    // Fill the steps and the end for a derived class.
    void makeSteps()
      { baseMakeSteps(); this->setEndIter(); }

    // Set the end iterator.
    void setEndIter()
      { end_p = (nels_p==0 ? 0 : (contiguous_p  ?  begin_p + nels_p :
                   begin_p + size_t(length_p(ndim()-1)) * steps_p(ndim()-1))); }
};

}//#End casa namespace
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/Array.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
