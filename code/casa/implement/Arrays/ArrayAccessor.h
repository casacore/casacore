//# ArrayAccessor.h: Fast 1D accessor/iterator for nD array classes
//# Copyright (C) 2002
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

#if !defined(AIPS_ARRAYACCESSOR_H)
#define AIPS_ARRAYACCESSOR_H

//# Includes
#include <aips/aips.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Array.h>

//# Forward Declarations
template <class T> class ArrayBaseAccessor;

//# Hide simple Axis classes names from outside module

namespace {
  // <summary> Class to enumerate constant axis numeration </summary>
  template <uInt AX> struct Axis {
    // Specify the constant axis
    enum { N=AX };
  };
  // <summary>Class to enumerate run-time axis values</summary>
  struct AxisN {
    uInt N;
    // Construct the variable axis
    explicit AxisN(const uInt n) { N = n; }
  };
};

// <summary> Fast 1D accessor/iterator for nD array classes </summary>

//  <use visibility=export>

// <reviewed reviewer="diepen@astron.nl" date="2002/12/01"
// tests="tArrayAccessor" demos="dArrayAccessor">
// </reviewed>

// <prerequisite>
//   <li> Array indexing and access methods
//	 (<linkto class=Array>Array</linkto>)
// </prerequisite>
//
// <etymology>
// Array and access, rather than Iterator, which would suggest some more
// standard interfaces
// </etymology>
//
// <synopsis>
// Accessing a large multi-dimensional array by varying the indices of the
// array can be a slow process. Timing indications are that for a cube
// indexing with 3 indices is about seven times slower than using
// standard 1D C-like index into an array of basic Int types. There are a
// variety of ways to access elements <src>cube(i,j,k)</src>
// addressing:
// <ul>
//   <li> Complete random access in all dimensions will need the
//	use of the indexing: <src>cube(i,j,k);</src> or
//	<src>cube(Iposition(3))</src> as described in the 
//	<linkto class=Array>Array</linkto> class
//   <li> Ordered access of all (or most) elements in an Array
//	(in memory order) can be best achieved by the use of the
//	<linkto class=ArraySTLIterator>ArraySTLIterator</linkto> class.
//	This is the fastest way for non-contiguous arrays, and only slightly
// 	slower than the use of <src>getStorage</src> for contiguous arrays.
//   <li> Ordered access along memory order can also be achieved by the use
// 	of the <linkto class=Array><src>getStorage()</src></linkto> method.
//	For contiguous arrays this could be slightly faster than the use of 
//	<src>ArraySTLIterator</src> (about 10% faster), but slower for 
//	non-contiguous arrays. In addition is needs additional memory
//	resources, which could lead to extra overhead. The use of
//	getStorage is deprecated with the introduction of the ArraySTLIterator.
//   <li> Access along one or more axes of a multi-dimensional array
//	is best achieved using the
//	<linkto class=ArrayAccessor>ArrayAccessor</linkto> class. Its total
//	access time is about 4 times faster than indexing (all for cubes),
//   <li> Special iteration (like in chunks) are catered for by the
//	<linkto class=ArrayIter>ArrayIter</linkto>,
//	<linkto class=MatrixIter>MatrixIter</linkto>,
//	<linkto class=VectorIter>VectorIter</linkto> classes.
// </ul>
// The ArrayAccessor class is an iterator like pointer to the data
// in the array. It is a 1-dimensional accessor. It is created with either
// a constant (at compile time) axis indicator, or with a run-time
// axis selector.
// <srcblock>
// 	Matrix<Double> mat(1000,500); // A 1000*500 matrix
// 	// Fill Matrix ...
// 	// Loop over index 1, than index 0:
//	for (ArrayAccessor<Double, Axis<1> > i(mat); i != i.end(); ++i) {
//	  for (ArrayAccessor<Double, Axis<0> > j(i); j |= j.end(); ++j) {
//	    // Actions on *j (which points to mat(j,i)) or j[n]
//	    // (which points to mat(j+n,i))
//	}}
// </srcblock>
// For run-time indices it would look like:
// <srcblock>
// 	Matrix<Double> mat(1000,500); // A 1000*500 matrix
// 	// Fill Matrix ...
// 	// Loop over index 1, than index 0:
//	for (ArrayAccessor<Double, AxisN> i(mat, AxisN(1)); i != i.end(); ++i) {
//	  for (ArrayAccessor<Double, AxisN> j(i,AxisN(0)); j |= j.end(); ++j) {
//	    // Actions on *j (which points to mat(j,i)) or j[n]
//	    // (which points to mat(j+n,i))
//	}}
// </srcblock>
// Compile-time and run-time axes can be miced in constructors and assignments.
//
// <note role=tip> Like in all comparable situations, memory allocation
// within a loop can slow down processes. For that reason the example above
// can be better written (about 25% faster) as:
// <srcblock>
// 	Matrix<Double> mat(1000,500); // A 1000*500 matrix
//	ArrayAccessor<Double, Axis<0> > j; // accessor pre-allocated
// 	// Fill Matrix ...
// 	// Loop over index 1, than index 0:
//	for (ArrayAccessor<Double, Axis<1> > i(mat); i != i.end(); ++i) {
//	  for (j=i; j |= j.end(); ++j) {
//	    // Actions on *j (which points to mat(j,i)) or j[n]
//	    // (which points to mat(j+n,i))
//	}}
// </srcblock>
// </note>
// The demonstrator program has more examples.
//
// In the standard case the Accessor assumes that loops are <src>simple</src>.
// I.e. it is assumed that any one axis is only addressed once (to enable
// a simple algorithm to determine <src>end()</src>.
// If axes are addressed more than once, (e.g. to zig-zag throgh an array),
// an extra template parameter <src>FULLIterator</src> is necessary.
//
// The accessors can be dererefenced by the dereference operator (<src>*</src>)
// and by the index operator (<src>[Int]</src>), which can be negative.
// Points around the accessor in any axis direction can be addressed
// along any axis by the templated methods <src>next()</src>,
// <src>prev()</src> and <src>index(Int)</src>. Either run-time or
// compile-time axes can be used (see example).
//
// This class is available for <src>Axis<n></src> and <src>AxisN</src>
// specializations only
// </synopsis>
//
// <example>
// <srcblock>
//	// get a cube and fill it
//	Cube<Double> cub(5,2,4);
//	indgen(cub);
//	// Loop over axes 2-0 and use index() over axis 1
//	for (ArrayAccessor<Double, Axis<2> > i(cub); i != i.end() ; ++i) {
//	  for (ArrayAccessor<Double, Axis<0> > j(i);
//	    j != j.end(); ++j) {
//	    // show result
//	    cout << *j << ", " << j.index<Axis<1> >(1) << endl;
//        };
//	};
// </srcblock>
// See the demonstrator program in
// <src>aips/implement/Arrays/test/dArrayAccessor.cc</src> for more examples.
// </example>
//
// <motivation>
// To speed up especially interpolation code
// </motivation>
//
// <templating arg=T>
//    <li> Any valid Array templating argument
// </templating>
// <templating arg=u>
//    <li> A class <src>Axis<n></src>
//    <li> Clas AxisN
// </templating>
//
// <thrown>
//    <li> Exceptions created in the Array clsaa
//    <li> Addressing errors
// </thrown>
//
// <todo asof="2002/11/06">
//   <li> add dependence on <src>FULL</src> or <rc>SIMPLEIteration</src>
//   <li> change the <src>T</src> template to <src>Array</src> (with
//		implicit T) rather than the array type. This could
//		help in creating Accessors for say Lattices
// </todo>

template <class T, class U=Axis<0> > class ArrayAccessor :
public ArrayBaseAccessor<T> {
  public:
  //# Constructors
  // Default ctor. Note only avalailable to accommodate arrays of
  // ArrayAccessors.
  // It should never be used without a call to 'init()'
  ArrayAccessor();
  // Standard ctor for the Array to be traversed. The pointer is set
  // to the begin of the referenced Array.
  explicit ArrayAccessor(const Array<T> &arr);
  // Copy constructor (copy semantics)
  ArrayAccessor(const ArrayAccessor<T, U> &other);
  
  //# Destructor
  ~ArrayAccessor();
  
  //# Operators  
  // Assignment (copy semantics)
  ArrayAccessor &operator=(const ArrayAccessor<T, U> &other);
  // Comparison
  // <group>
  Bool operator==(const ArrayAccessor<T, U> &other) const;
  Bool operator!=(const ArrayAccessor<T, U> &other) const;
  Bool operator==(const T *other) const;
  Bool operator!=(const T *other) const;
  // </group>
  
  //# Methods
  // (Re-)initialization to start of array (i.e. element (0,0,0,...))
  void init(const Array<T> &arr);
  // Get values around accessor
  // <group>
  const T &next() const;
  T &next();
  const T &prev() const;
  T &prev();
  const T &index(const Int ix) const;
  T &next(const Int ix);
  // </group>  

  private: 
  //# Method
  // Initialize
  void initStep();
  
};

// <summary> Axis independent base for the ArrayAccessor classes </summary>
template <class T> class ArrayBaseAccessor {
 protected:
  //# Constructors
  ArrayBaseAccessor() : ptr_p(0), step_p(0), begin_p(0), end_p(0),
    arrayPtr_p(0), axis_p(0) {;}
  explicit ArrayBaseAccessor(const Array<T> &arr) :
    ptr_p(0), step_p(0), begin_p(0), end_p(0),
    arrayPtr_p(&arr), axis_p(0) { begin_p = arrayPtr_p->data();
    ptr_p =  const_cast<T*>(begin_p); }
  ArrayBaseAccessor(const Array<T> &arr, const uInt ax) :
    ptr_p(0), step_p(0), begin_p(0), end_p(0),
    arrayPtr_p(&arr), axis_p(ax) { begin_p = arrayPtr_p->data();
    ptr_p =  const_cast<T*>(begin_p); }
  // Copy constructor (copy semantics)
  // <group>
  ArrayBaseAccessor(const ArrayBaseAccessor<T> &other) :
    ptr_p(other.ptr_p), begin_p(other.begin_p),
    arrayPtr_p(other.arrayPtr_p), axis_p(other.axis_p) {;}
  ArrayBaseAccessor(const ArrayBaseAccessor<T> &other, const uInt ax) :
    ptr_p(other.ptr_p), begin_p(other.begin_p),
    arrayPtr_p(other.arrayPtr_p), axis_p(ax) {;}
  // </group>
  
  // Assignment (copy semantics)
  // <group>
  ArrayBaseAccessor &operator=(const ArrayBaseAccessor<T> &other) {
    if (&other != this) {
      arrayPtr_p = other.arrayPtr_p;
      ptr_p = other.ptr_p; begin_p = other.begin_p;
    }; return *this; }
  // </group>
  
  //# Destructor
  ~ArrayBaseAccessor() {;}
  
  // Initialize
  void init(const Array<T> &arr) { arrayPtr_p = &arr;
  begin_p = arrayPtr_p->data();
  ptr_p = const_cast<T*>(begin_p); }
  
 public:
  //# Operators
  // Iterator operations.
  // <group>
  void operator+=(const uInt ix) { ptr_p += ix*step_p; }
  void operator-=(const uInt ix) { ptr_p -= ix*step_p; }
  void operator++() { ptr_p += step_p; }
  void operator++(int) { ptr_p += step_p; }
  void operator--() { ptr_p -= step_p; }
  void operator--(int) { ptr_p -= step_p; }
  // </group>
  
  // Dereferencing.
  // <group>
  const T &operator*() const { return *ptr_p; }
  T &operator*() { return *ptr_p; }
  T *pointer() { return ptr_p; }
  const Array<T> &baseArray() { return *arrayPtr_p; }
  // </group>
  
  // Index
  // <group>
  const T &operator[](const Int ix) const { return *(ptr_p + ix*step_p); };
  T &operator[](const Int ix) { return *(ptr_p + ix*step_p); }
  // </group>
  
  // End of index on line
  T *end() { return end_p; }
  
  // Start of array
  const T *begin() {return begin_p; }
  
 protected:
  //# Data
  // Current access pointer
  T *ptr_p;
  // The increments along the specified axes (by the U class)
  // The increment to go from one point along an axis, to the next.
  // <group>
  ptrdiff_t step_p;
  // </group>
  // The start element of array
  const T* begin_p;
  // The one element beyond last on line
  T* end_p;
  // The pointer to belonging array
  const Array<T> *arrayPtr_p;
  // Current run-time axis
  uInt axis_p;
  
  //# Method
  
};

// <summary> Specialization for compile-time axes</summary>
template <class T, uInt U> class ArrayAccessor<T, Axis<U> > :
public ArrayBaseAccessor<T> {
 public:
  // Constructors
  // <group>
  ArrayAccessor() : ArrayBaseAccessor<T>() {;}
  explicit ArrayAccessor(const Array<T> &arr) :
    ArrayBaseAccessor<T>(arr) { initStep(); }
  ArrayAccessor(const ArrayAccessor<T, Axis<U> > &other) :
    ArrayBaseAccessor<T>(other) { initStep(); }
  // Construct from other axis accessor
  // <group>
  explicit ArrayAccessor(const ArrayAccessor<T, AxisN > &other) :
    ArrayBaseAccessor<T>(other) { initStep(); }
  template <uInt X>
    explicit ArrayAccessor(const ArrayAccessor<T, Axis<X> > &other) :
    ArrayBaseAccessor<T>(other) { initStep(); }
  // </group>
  // </group>
  
  // Destructor
  ~ArrayAccessor() {;}
  
  // Assignment (copy semantics)
  // <group>
  ArrayAccessor &operator=(const ArrayAccessor<T, Axis<U> > &other) {
    if (&other != this) {
      ArrayBaseAccessor<T>::operator=(other);
      initStep();
    }; return *this; }
  ArrayAccessor &operator=(const ArrayAccessor<T, AxisN> &other) {
    if (&other != this) {
      ArrayBaseAccessor<T>::operator=(other);
      axis_p = other.axis_p; initStep();
    }; return *this; }
  template <uInt X>
    ArrayAccessor &operator=(const ArrayAccessor<T, Axis<X> > &other) {
    if (&other != this) {
      ArrayBaseAccessor<T>::operator=(other);
      initStep();
    }; return *this; }
  // </group>
  
  // (Re-)initialization to start of array (i.e. element (0,0,0,...))
  void init(const Array<T> &arr) { ArrayBaseAccessor<T>::init(arr);
  initStep(); }
  
  // Iterator operations. 
  // <group>
  template <class X>
    const T &next() const
    { return *(ptr_p + arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &next() { return *(ptr_p + arrayPtr_p->steps()[X::N]); }
  template <class X>
    const T &prev() const
    { return *(ptr_p - arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &prev() { return *(ptr_p - arrayPtr_p->steps()[X::N]); }
  const T &next(const AxisN ax) const
    { return *(ptr_p + arrayPtr_p->steps()[ax.N]); }
  T &next(const AxisN ax)
    { return *(ptr_p + arrayPtr_p->steps()[axN]); }
  const T &prev(const AxisN ax) const
    { return *(ptr_p - arrayPtr_p->steps()[ax.N]); }
  T &prev(const AxisN ax)
    { return *(ptr_p - arrayPtr_p->steps()[ax.N]); }
  template <class X>
    const T &index(const Int ix) const 
    { return *(ptr_p + ix*arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &index(const Int ix)
    { return *(ptr_p + ix*arrayPtr_p->steps()[X::N]); }
  const T &index(const Int ix, const AxisN(ax)) const 
    { return *(ptr_p + ix*arrayPtr_p->steps()[ax.N]); }
  T &index(const Int ix, const AxisN(ax))
    { return *(ptr_p + ix*arrayPtr_p->steps()[ax.N]); }
  // </group>
  
  // Comparison
  // <group>
  Bool operator==(const ArrayAccessor<T, Axis<U> > &other) const {
    return ptr_p == other.ptr_p; }
  Bool operator!=(const ArrayAccessor<T, Axis<U> > &other) const {
    return ptr_p != other.ptr_p; }
  Bool operator==(const T *other) const { return ptr_p == other; }
  Bool operator!=(const T *other) const { return ptr_p != other; }
  // </group>
  
 private: 
  // Initialize
  void initStep() { step_p = arrayPtr_p->steps()[Axis<U>::N];
  end_p = ptr_p + arrayPtr_p->shape()[Axis<U>::N]*step_p; }
  
};

// <summary> Specialization for AxisN </summary>
template <class T> class ArrayAccessor<T, AxisN> :
public ArrayBaseAccessor<T> {
 public:
  // Constructors
  // <group>
  ArrayAccessor() : ArrayBaseAccessor<T>() {;}
  explicit ArrayAccessor(Array<T> &arr, const AxisN ax=AxisN(0)) :
    ArrayBaseAccessor<T>(arr, ax.N) { initStep(); }
  ArrayAccessor(ArrayAccessor<T, AxisN> &other) :
    ArrayBaseAccessor<T>(other) { initStep(); }
  template <uInt X>
    explicit ArrayAccessor(ArrayAccessor<T, Axis<X> > &other,
			   const AxisN ax=AxisN(0)) :
    ArrayBaseAccessor<T>(other, ax.N) { initStep(); }
  ArrayAccessor &operator=(const ArrayAccessor<T, AxisN> &other) {
    if (&other != this) {
      ArrayBaseAccessor<T>::operator=(other);
      initStep();
    }; return *this; }
  template <uInt X>
    ArrayAccessor &operator=(const ArrayAccessor<T, Axis<X> > &other) {
    if (&other != this) {
      ArrayBaseAccessor<T>::operator=(other); axis_p = X;
      initStep();
    }; return *this; }
  // </group>

  // Destructor
  ~ArrayAccessor() {;}

  // (Re-)initialize
  void init(const Array<T> &arr, const AxisN ax)
    { ArrayBaseAccessor<T>::init(arr, ax); initStep(); }

  // Iterator operations. 
  // <group>
  template <class X>
    const T &next() const
    { return *(ptr_p + arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &next() { return *(ptr_p + arrayPtr_p->steps()[X::N]); }
  template <class X>
    const T &prev() const
    { return *(ptr_p - arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &prev() { return *(ptr_p - arrayPtr_p->steps()[X::N]); }
  const T &next(const AxisN ax) const
    { return *(ptr_p + arrayPtr_p->steps()[ax.N]); }
  T &next(const AxisN ax)
    { return *(ptr_p + arrayPtr_p->steps()[ax.N]); }
  const T &prev(const AxisN ax) const
    { return *(ptr_p - arrayPtr_p->steps()[ax.N]); }
  T &prev(const AxisN ax)
    { return *(ptr_p - arrayPtr_p->steps()[ax.N]); }
  template <class X>
    const T &index(const Int ix) const 
    { return *(ptr_p + ix*arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &index(const Int ix)
    { return *(ptr_p + ix*arrayPtr_p->steps()[X::N]); }
  const T &index(const Int ix, const AxisN(ax)) const 
    { return *(ptr_p + ix*arrayPtr_p->steps()[ax.N]); }
  T &index(const Int ix, const AxisN(ax))
    { return *(ptr_p + ix*arrayPtr_p->steps()[ax.N]); }
  // </group>

  // Comparisons
  // <group>
  Bool operator==(const ArrayAccessor<T, AxisN> &other) const {
    return ptr_p == other.ptr_p; }
  Bool operator!=(const ArrayAccessor<T, AxisN> &other) const {
    return ptr_p != other.ptr_p; }
  Bool operator==(const T *other) const { return ptr_p == other; }
  Bool operator!=(const T *other) const { return ptr_p != other; }
  // </group>

 private: 
  // Initialize
  void initStep() { step_p = arrayPtr_p->steps()[axis_p];
  end_p = ptr_p + arrayPtr_p->shape()[axis_p]*step_p; }
  
};

#endif

