//# ArrayAccessor.h: Fast 1D accessor/iterator for nD array classes
//# Copyright (C) 2002,2004
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

#ifndef CASA_ARRAYACCESSOR_H
#define CASA_ARRAYACCESSOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //#Begin casa namespace

//# Forward Declarations
template <class T> class ArrayBaseAccessor;
//# Next one suffices as declaration: only (part) specialisations allowed
template <class T, class U> class ArrayAccessor;

//# Hide simple Axis classes names from outside module

namespace {
  // <summary> Class to enumerate compile-time axis numeration </summary>
  template <uInt AX> struct Axis {
    enum {
      // Specify the constant axis
      N=AX
    };
  };
  // <summary>Class to specify run-time axis values</summary>
  struct AxisN {
    // Construct the run-time axis number
    explicit AxisN(const uInt n) : N(n) {}
    // Axis number
    uInt N;
  };
}

// <summary> Axis independent base for the ArrayAccessor classes </summary>
// <use visibility=local>
// <synopsis>
// The ArrayBaseAccessor class implements the axis independent parts of the
// ArrayAccessor class. It can only be used from the ArrayAccessor class.
// </synopsis>

template <class T> class ArrayBaseAccessor {
 protected:
  //# Constructors
  // <group>
  // Default constructor (for use in e.g. containers)
  ArrayBaseAccessor() : arrayPtr_p(0), axis_p(0), ptr_p(0),
    step_p(0), begin_p(0), end_p(0) {;}
  // Construct from an Array
  // <group>
  explicit ArrayBaseAccessor(const Array<T> &arr) :
    arrayPtr_p(&arr), axis_p(0), ptr_p(const_cast<T*>(arrayPtr_p->data())),
    step_p(0), begin_p(0), end_p(0) {;}
  ArrayBaseAccessor(const Array<T> &arr, const uInt ax) :
    arrayPtr_p(&arr), axis_p(ax), ptr_p(const_cast<T*>(arrayPtr_p->data())),
    step_p(0), begin_p(0), end_p(0) {;}
  // </group>
  // Copy constructor (copy semantics)
  // <group>
  ArrayBaseAccessor(const ArrayBaseAccessor<T> &other) :
    arrayPtr_p(other.arrayPtr_p), axis_p(other.axis_p), ptr_p(other.ptr_p),
    step_p(other.step_p), begin_p(other.begin_p), end_p(other.end_p) {;}
  ArrayBaseAccessor(const ArrayBaseAccessor<T> &other, const uInt ax) :
    arrayPtr_p(other.arrayPtr_p), axis_p(ax), ptr_p(other.ptr_p),
    step_p(other.step_p), begin_p(other.begin_p), end_p(other.end_p) {;}
  // </group>
  
  //# Destructor
  // Destructor
  ~ArrayBaseAccessor() {;}
  // </group>
  
  // Assignment (copy semantics)
  ArrayBaseAccessor &operator=(const ArrayBaseAccessor<T> &other) {
    if (&other != this) {
      arrayPtr_p = other.arrayPtr_p; ptr_p = other.ptr_p;
    }; return *this; }
  // (Re-)initialize from Array
  // <group>
  void init(const Array<T> &arr) { arrayPtr_p = &arr;
  ptr_p = const_cast<T*>(arrayPtr_p->data()); }
  void init(const Array<T> &arr, const uInt ax) { arrayPtr_p = &arr;
  axis_p = ax; ptr_p = const_cast<T*>(arrayPtr_p->data()); }
  void init(const uInt ax) { arrayPtr_p = 0; axis_p = ax; ptr_p = 0; }
  // </group>

 public:
  //# Operators
  // Iterator-like operations.
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
  T *data() { return ptr_p; }
  const Array<T> &baseArray() { return *arrayPtr_p; }
  uInt step() { return step_p; }
  // </group>
  
  // Index along current axis
  // <group>
  const T &operator[](const Int ix) const { return *(ptr_p + ix*step_p); };
  T &operator[](const Int ix) { return *(ptr_p + ix*step_p); }
  // </group>
  
  // End of index on line
  // <group>
  const T *end() { return end_p; }
  const T *end(const Int n) { return end_p + n*step_p; }
  // </group>

  // Start of index on line
  // <group>
  const T *begin() { return begin_p; }
  const T *begin(const Int n) { return begin_p + n*step_p; }
  // </group>

  // End when reverse indexing
  // <group>
  const T *rend() { return begin_p-step_p; }
  const T *rend(const Int n) { return begin_p + (n-1)*step_p; }
  // </group>

  // Begin when reverse indexing
  // <group>
  const T *rbegin() { return end_p-step_p; }
  const T *rbegin(const Int n) { return end_p + (n-1)*step_p; }
  // </group>

 protected:
  //# Data
  // The pointer to belonging array
  const Array<T> *arrayPtr_p;
  // Current run-time axis
  uInt axis_p;
  // Current access pointer
  T *ptr_p;
  // The increment to go from one point along an axis, to the next.
  Int step_p;
  // The start element of array
  const T *begin_p;
  // The one element beyond last on line
  const T *end_p;
  
};

// <summary> Fast 1D accessor/iterator for nD array classes </summary>
// <use visibility=export>
// <reviewed reviewer="Ger van Diepen" date="2002/12/01" tests="tArrayAccessor" demos="dArrayAccessor">
// </reviewed>
// <prerequisite>
//   <li> Array indexing and access methods
//	 (<linkto class=Array>Array</linkto>)
// </prerequisite>
//
// <etymology>
// Array and access, rather than Iterator, which would suggest more
// standard-like interfaces
// </etymology>
//
// <synopsis>
// Accessing a large multi-dimensional array by varying the indices of the
// array can be a slow process. Timing indications are that for a cube
// indexing with 3 indices was about seven times slower than using a
// standard 1D C-like index into an array of basic Int types.
// Improvements have made this less, partly due to some pre-calculation
// necessary for this class, but can still be a factor of more than 3
// slower. There are a variety of ways to access elements
// <src>cube(i,j,k)</src>:
// <ul>
//   <li> Complete random access in all dimensions will need the
//	use of the indexing: <src>cube(i,j,k);</src> or
//	<src>cube(IPosition(3))</src> as described in the 
//	<linkto class=Array>Array</linkto> and
//	<linkto class=Cube>Cube</linkto> classes
//   <li> Ordered access of all (or most) elements in an Array
//	(in memory order) can be best achieved by the use of Array's
//	<linkto class="Array#STL-iterator">STLIterator</linkto> classes.
//	This is the fastest way for non-contiguous arrays, and only slightly
// 	slower than the use of <src>getStorage</src> for contiguous arrays.
//   <li> Ordered access along memory order can also be achieved by the use
// 	of the
//	<linkto class="Array:getStorage(Bool&)">
//		<src>getStorage()</src></linkto> method.
//	For contiguous arrays this could be slightly faster than the use of 
//	the <src>STLIterator</src> (about 10% faster), but slower for 
//	non-contiguous arrays. In addition it needs additional memory
//	resources, which will lead to extra overhead. The general use of
//	getStorage is discouraged with the introduction of the STLIterator.
//	It should only be used when an interface to routines in
//	other languages is needed (like Fortran), or when a large Array is
//	known to be contiguous, and the data have to be referenced many times.
//   <li> Access along one or more axes of a (large) multi-dimensional array
//	is best achieved using the ArrayAccessor class. Its total
//	access time is about 2 times faster than indexing (for cubes,
//	more for more indices),
//   <li> Special iteration (like in chunks) are catered for by the
//	<linkto class=ArrayIterator>ArrayIterator</linkto>,
//	<linkto class=MatrixIterator>MatrixIterator</linkto>,
//	<linkto class=VectorIterator>VectorIterator</linkto> classes.
// </ul>
// The ArrayAccessor class is an iterator like pointer to the data
// in the array. It is a 1-dimensional accessor. It is created with either
// a constant (at compile time) axis indicator, or with a run-time
// axis selector. ArrayAccessor constructor accepts a <src>const Array<></src>.
// However, the underlying Array class can be modified at this moment. In
// future a ConstArrayAccessor class is foreseen. 
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
//	for (ArrayAccessor<Double, AxisN> i(mat, AxisN(1));
//           i != i.end(); ++i) {
//	  for (ArrayAccessor<Double, AxisN> j(i,AxisN(0)); j |= j.end(); ++j) {
//	    // Actions on *j (which points to mat(j,i)) or j[n]
//	    // (which points to mat(j+n,i))
//	}}
// </srcblock>
// Compile-time and run-time axes can be mixed in constructors and assignments.
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
// <note role=tip> The underlying Array classes are structured with the
// first index varying fastest. This means that in general (due to caching and 
// swapping) operations are fastest when <src>Axis<0> ></src> is in the
// innermost loop (if possible of course).
// </note>
// The demonstrator and test programs have more examples.
//
// The accessors can be dereferenced by the dereference operator (<src>*</src>)
// and by the index operator (<src>[Int]</src>), which can handle negative
// values.
// Points around the accessor in any axis direction can be addressed
// along any axis by the templated methods <src>next()</src>,
// <src>prev()</src> and <src>index(Int)</src>. Either run-time or
// compile-time axes can be used (see example).
//
// An accessor can be re-initialized with the init() function. It can also
// be reset() to any pointer value. Mthods <src>end()</src>,
// <src>begin()</src>, <src>rbegin()</src> and <src>rend()</src> are available 
// for loop control (like in the STL iterators). In addition each of these
// can have an optional integer argument, specifying an offset (in points
// along the current axis).
//
// Operations <src>++ -- += -=</src> are available.
//
// This class is available for <src>Axis<n></src> and <src>AxisN</src>
// specializations only.
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
// <src>aips/implement/Arrays/test/dArrayAccessor.cc</src> and the
// test program <src>tArrayAccessor</src> for more examples.
// </example>
//
// <motivation>
// To speed up especially interpolation code
// </motivation>
//
// <templating arg=T>
//    <li> Any valid Array templating argument
// </templating>
// <templating arg=U>
//    <li> A class <src>Axis<n></src>
//    <li> Class AxisN
// </templating>
//
// <thrown>
//    <li> Exceptions created in the Array class
//    <li> Addressing errors
// </thrown>
//
// <todo asof="2002/11/06">
//   <li> add a ConstArrayAccessor class
// </todo>
//
template <class T, uInt U> class ArrayAccessor<T, Axis<U> > :
public ArrayBaseAccessor<T> {
 public:
  // Constructors
  // <group>
  // Default ctor. Note only available to accommodate containers of
  // ArrayAccessors. Use <src>init()</src> to initialize.
  ArrayAccessor() : ArrayBaseAccessor<T>() {;}
  // Construct an accessor from specified Array along the selected axis.
  // The accessor will point to the first element along the axis (i.e.
  // at (0,0,...)).
  explicit ArrayAccessor(const Array<T> &arr) :
    ArrayBaseAccessor<T>(arr) { initStep(); }
  // Construct from an ArrayAccessor along same axis. The accessor will point
  // at the same element as the originator.
  ArrayAccessor(const ArrayAccessor<T, Axis<U> > &other) :
    ArrayBaseAccessor<T>(other) {;}
  // Construct from accessor along another (or run-time) axis.
  // The accessor will point to the same element (but will be oriented
  // along another axis).
  // <group>
  template <uInt X>
    explicit ArrayAccessor(const ArrayAccessor<T, Axis<X> > &other) :
    ArrayBaseAccessor<T>(other) { initStep(); }
  explicit ArrayAccessor(const ArrayAccessor<T, AxisN > &other) :
    ArrayBaseAccessor<T>(other) { initStep(); }
  // </group>
  
  // Destructor
  ~ArrayAccessor() {;}
  // </group>
  
  // Assignment (copy semantics)
  // <group>
  // Assign from other compile-time accessor along same axis
  ArrayAccessor &operator=(const ArrayAccessor<T, Axis<U> > &other) {
    if (&other != this) {
      ArrayBaseAccessor<T>::operator=(other); this->step_p = other.step_p;
      this->begin_p = other.begin_p; this->end_p = other.end_p;
    }; return *this; }
  // Assign from other compile-time accessor along another axis
  template <uInt X>
    ArrayAccessor &operator=(const ArrayAccessor<T, Axis<X> > &other) {
    ArrayBaseAccessor<T>::operator=(other); initStep();
    return *this; }
  // Assign from run-time accessor along any axis
  ArrayAccessor &operator=(const ArrayAccessor<T, AxisN> &other) {
    ArrayBaseAccessor<T>::operator=(other); initStep(); return *this; }
  // </group>
  
  // (Re-)initialization to start of array (i.e. element (0,0,0,...))
  void init(const Array<T> &arr) { ArrayBaseAccessor<T>::init(arr);
  initStep(); }

  // Reset to start of dimension or to specified pointer
  // <group>
  void reset() { this->ptr_p = const_cast<T *>(this->begin_p); }
  void reset(const T * p) { this->ptr_p = const_cast<T *>(p); initStep(); }
  // </group>

  // Indexing  operations along another axis than the one of the current
  // object. See for the indexing and iterator operations along the
  // object's axis <linkto class=ArrayBaseAccessor>ArrayBaseAccessor</linkto> 
  // <group>
  // Get the value 'next' along the specified axis (e.g. with 
  // <src>a.next<Axis<2> >()</src>)
  // <group>
  template <class X>
    const T &next() const
    { return *(this->ptr_p + this->arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &next() { return *(this->ptr_p + this->arrayPtr_p->steps()[X::N]); }
  // </group>
  // Get the value 'previous' along the specified axis (e.g. with 
  // <src>a.prev<Axis<2> >()</src>)
  // <group>
  template <class X>
    const T &prev() const
    { return *(this->ptr_p - this->arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &prev() { return *(this->ptr_p - this->arrayPtr_p->steps()[X::N]); }
  // </group>
  // Get the next or previous along the specified run-time axis. E.g.
  // <src>a.prev(AxisN(2))</src>.
  // <group>
  const T &next(const AxisN ax) const
    { return *(this->ptr_p + this->arrayPtr_p->steps()[ax.N]); }
  T &next(const AxisN ax)
    { return *(this->ptr_p + this->arrayPtr_p->steps()[ax.N]); }
  const T &prev(const AxisN ax) const
    { return *(this->ptr_p - this->arrayPtr_p->steps()[ax.N]); }
  T &prev(const AxisN ax)
    { return *(this->ptr_p - this->arrayPtr_p->steps()[ax.N]); }
  // </group>
  // Give the value indexed with respect to the current accessor value
  // along the axis specified as either a compile-time or a run-time
  // axis. E.g. <src>a.index<Axis<3> >(5)</src> or 
  // <src>a.index(5, AxisN(3))</src>.
  // <group>
  template <class X>
    const T &index(const Int ix) const 
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &index(const Int ix)
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[X::N]); }
  const T &index(const Int ix, const AxisN ax) const 
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[ax.N]); }
  T &index(const Int ix, const AxisN ax)
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[ax.N]); }
  // </group>
  // </group>
  
  // Comparison. The comparisons are done for the accessor pointer
  // value. They can be used to control loops.
  // <group>
  Bool operator==(const ArrayAccessor<T, Axis<U> > &other) const {
    return this->ptr_p == other.ptr_p; }
  Bool operator!=(const ArrayAccessor<T, Axis<U> > &other) const {
    return this->ptr_p != other.ptr_p; }
  Bool operator==(const T *other) const { return this->ptr_p == other; }
  Bool operator!=(const T *other) const { return this->ptr_p != other; }
  // </group>
  
 private:
  // Get proper offset
  Int initOff(Int x, uInt ax) {
    uInt st = this->arrayPtr_p->steps()[ax];
    return ((st) ? (ax == Axis<U>::N ? x/st : initOff(x%st, ax-1)) : 0); }
  // Initialize some internal values
  void initStep() {
    this->step_p = this->arrayPtr_p->steps()[Axis<U>::N];
    this->begin_p = this->end_p = this->ptr_p
                    - initOff(this->ptr_p - this->arrayPtr_p->data(),
			      this->arrayPtr_p->ndim()-1)*this->step_p;
    this->end_p += this->arrayPtr_p->shape()[Axis<U>::N]*this->step_p; }
  
};

#define ArrayAccessor_RT ArrayAccessor

// <summary> Specialization for run-time axes </summary>
// <use visibility=export>
// <synopsis>
// This class is a specialization for run-time axis selection within the
// array accessor. The axis is specified in the constructors and in the
// special indexing operators (<src>prev, next, index</src>) with
// a parameter <src>AxisN(n)</src> in stead of a template parameter
// <src><Axis<n> ></src>.
//
// Note that the name of the class is <src>ArrayAccessor</src>. The special
// name is only to bypass cxx2html problems with duplicate class names. 
// </synopsis>
//
template <class T> class ArrayAccessor_RT<T, AxisN> :
public ArrayBaseAccessor<T> {
 public:
  // Constructors
  // <group>
  explicit ArrayAccessor_RT(const AxisN ax=AxisN(0)) :
    ArrayBaseAccessor<T>() { this->axis_p = ax.N; }
  explicit ArrayAccessor_RT(Array<T> &arr, const AxisN ax=AxisN(0)) :
    ArrayBaseAccessor<T>(arr, ax.N) { initStep(); }
  ArrayAccessor_RT(ArrayAccessor_RT<T, AxisN> &other) :
    ArrayBaseAccessor<T>(other) {;}
  explicit ArrayAccessor_RT(ArrayAccessor_RT<T, AxisN> &other,
			    const AxisN ax) :
    ArrayBaseAccessor<T>(other, ax.N) { initStep(); }
  template <uInt X>
    explicit ArrayAccessor_RT(ArrayAccessor_RT<T, Axis<X> > &other,
			      const AxisN ax=AxisN(0)) :
    ArrayBaseAccessor<T>(other, ax.N) { initStep(); }
  ArrayAccessor_RT &operator=(const ArrayAccessor_RT<T, AxisN> &other) {
    if (&other != this) {
      ArrayBaseAccessor<T>::operator=(other);
      initStep();
    }; return *this; }
  template <uInt X>
    ArrayAccessor_RT &operator=(const ArrayAccessor_RT<T, Axis<X> > &other) {
    ArrayBaseAccessor<T>::operator=(other);
    initStep(); return *this; }
  // </group>

  // Destructor
  ~ArrayAccessor_RT() {;}

  // (Re-)initialization to start of array (i.e. element (0,0,0,...)) or
  // re-initialize to an axis.
  // <group>
  void init(const Array<T> &arr, const AxisN ax)
    { ArrayBaseAccessor<T>::init(arr, ax.N); initStep(); }
  void init(const AxisN ax) 
    { ArrayBaseAccessor<T>::init(ax.N); }
  // </group>

  // Reset to start of dimension or to specified pointer
  // <group>
  void reset() { this->ptr_p = const_cast<T *>(this->begin_p); }
  void reset(const T *p) { this->ptr_p = const_cast<T *>(p); initStep(); }
  // </group>

  // Indexing  operations along another axis than the one of the current
  // object. See for the indexing and iterator operations along the
  // object's axis <linkto class=ArrayBaseAccessor>ArrayBaseAccessor</linkto> 
  // <group>
  template <class X>
    const T &next() const
    { return *(this->ptr_p + this->arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &next() { return *(this->ptr_p + this->arrayPtr_p->steps()[X::N]); }
  template <class X>
    const T &prev() const
    { return *(this->ptr_p - this->arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &prev() { return *(this->ptr_p - this->arrayPtr_p->steps()[X::N]); }
  const T &next(const AxisN ax) const
    { return *(this->ptr_p + this->arrayPtr_p->steps()[ax.N]); }
  T &next(const AxisN ax)
    { return *(this->ptr_p + this->arrayPtr_p->steps()[ax.N]); }
  const T &prev(const AxisN ax) const
    { return *(this->ptr_p - this->arrayPtr_p->steps()[ax.N]); }
  T &prev(const AxisN ax)
    { return *(this->ptr_p - this->arrayPtr_p->steps()[ax.N]); }
  template <class X>
    const T &index(const Int ix) const 
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[X::N]); }
  template <class X>
    T &index(const Int ix)
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[X::N]); }
  const T &index(const Int ix, const AxisN(ax)) const 
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[ax.N]); }
  T &index(const Int ix, const AxisN(ax))
    { return *(this->ptr_p + ix*this->arrayPtr_p->steps()[ax.N]); }
  // </group>

  // Comparisons
  // <group>
  Bool operator==(const ArrayAccessor_RT<T, AxisN> &other) const {
    return this->ptr_p == other.ptr_p; }
  Bool operator!=(const ArrayAccessor_RT<T, AxisN> &other) const {
    return this->ptr_p != other.ptr_p; }
  Bool operator==(const T *other) const { return this->ptr_p == other; }
  Bool operator!=(const T *other) const { return this->ptr_p != other; }
  // </group>

 private: 
  // Get proper offset
  Int initOff(Int x, uInt ax) {
    uInt st = this->arrayPtr_p->steps()[ax];
    return ((st) ? (ax == this->axis_p ? x/st : initOff(x%st, ax-1)) : 0); }
  // Initialize some internal values
  void initStep() {
    this->step_p = this->arrayPtr_p->steps()[this->axis_p];
    this->begin_p = this->end_p = this->ptr_p
                    - initOff(this->ptr_p - this->arrayPtr_p->data(),
			      this->arrayPtr_p->ndim()-1)*this->step_p;
    this->end_p += this->arrayPtr_p->shape()[this->axis_p]*this->step_p; }
  
};

#undef ArrayAccessor_RT

} //#End casa namespace
#endif
