//# Lattice.h:  Lattice is an abstract base class for array-like classes
//# Copyright (C) 1994,1995,1996
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

#if !defined(AIPS_LATTICE_H)
#define AIPS_LATTICE_H

#if defined(_AIX)
#pragma implementation ("Lattice.cc")
#endif 

#include <aips/aips.h>
#include <aips/Lattices/IPosition.h>

//# Forward Declarations
template <class T> class Array;
template <class T> class COWPtr;
template <class T> class Lattice;
template <class Domain, class Range> class Functional;
template <class T> class RO_LatticeIterInterface;
template <class T> class LatticeIterInterface;
template <class T> class LatticeValueRef;
class Slicer;
class LatticeNavigator;

// <summary> A templated, abstract base class for array-like objects.</summary>
//
// <use visibility=local>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class="IPosition"> IPosition </linkto>
//   <li> Abstract Base class Inheritance - try "Advanced C++" by James
//        O. Coplien, Ch. 5.
// </prerequisite>
//
// <etymology>
// Lattice: "A regular, periodic configuration of points, particles, 
// or objects, throughout an area of a space..." (American Heritage Directory)
// This definition matches our own: an n-dimensional arrangement of items,
// on regular orthogonal axes.
// </etymology>
//
// <synopsis>
// This pure abstract base class defines the operations which may be performed
// on any concrete class derived from it.  It has only a few non-pure virtual 
// member functions.
// The fundamental contribution of this class, therefore, is that it 
// defines the operations derived classes must provide:
// <ul>
//    <li> how to extract a "slice" (or sub-array, or subsection) from
//         a lattice.
//    <li> how to copy a slice in.
//    <li> how to get and put a single element 
//    <li> how to apply a function to all elements
//    <li> various shape related functions.
// </ul>
//
// <note role=tip> Lattices are always zero origined. </note>
// </synopsis> 
//
// <example>
// Because lattice is an abstract base class, an actual instance of this
// class cannot be constructed. However the interface it defines can be used
// inside a function. I will give one example here and then refer the reader
// to the <linkto class="ArrayLattice"> ArrayLattice </linkto> and 
// <linkto class="PagedArray"> PagedArray </linkto> classes which contain
// further examples using concrete classes (rather than an abstract one). 

// This example calculates the mean of the Lattice. Because Lattices can be
// too large to fit into physical memory it is not good enough to simply use
// getSlice to read all the elements into an Array. Instead the Lattice is
// accessed in chunks which can fit into memory (the size is determined by
// the maxPixels and niceCursorShape functions). The
// LatticeIterator::cursor() function then returns each of these chunks as
// an Array and the standard Array based functions are used to calculate the
// mean on each of these chunks. 
// <srcblock>
// T mean(Lattice<T> &lat) {
//    uInt cursorSize = lat.maxPixels();
//    IPosition cursorShape = lat.niceCursorShape(cursorSize);
//    T currentSum = 0;
//    uInt nPixels = 0;
//    LatticeIterator<T> iter(lat, cursorShape);
//    for (iter.reset(); !iter.atEnd(); iter++){
//       currentSum += sum(iter.cursor());    // 
//       nPixels += iter.cursor().nelements();
//    }
//    return currentSum/nPixels;
// }
// </srcblock>
// </example>
//
// <motivation>
// To reduce the learning curve for Lattice like classes it was decided to
// create a family of classes which were all derived from a single base -
// Lattice.  This allows the derived classes to be treated similarly so that
// an identical interface can be used for a variety of different objects. In
// particular disk and memory based arrays.
// </motivation>
//
// <todo asof="1996/07/01">
//   <li> Remove the latticeCast member when the GNU compiler is 
//        sufficiently robust.
// </todo>

template <class T> class Lattice 
{
public: 
  // destructor
  virtual ~Lattice();
  
  // returns the value of the single element located at the argument 
  // IPosition.  The return type should be assumed to be of the template 
  // <class T>.  The actual return type (LatticeValueRef<T>) may be ignored.
  // For details, see "Advanced C++" by James O. Coplien, pp 49-52.
  LatticeValueRef<T> operator()(const IPosition &where);

  // returns the value of the single element located at the argument
  // IPosition.  
  T operator()(const IPosition &where) const;
  
  // returns the shape of the Lattice (possibly with superfluous axes of
  // length one.)
  virtual IPosition shape() const = 0;
  
  // returns the number of axes in this Lattice. This includes degenerate
  // axes (ie. axes with a length of one)
  virtual uInt ndim() const;
  
  // returns the total number of elements in this Lattice.
  virtual uInt nelements() const;
  
  // returns a value of "True" if this instance of Lattice and 'other' have 
  // the same shape, otherwise returns a value of "False".
  virtual Bool conform(const Lattice <T> &other) const;
  
  // function which extracts an Array of values from a Lattice - a read-only 
  // operation. All the IPosition arguments must have the same number of axes
  // as the underlying Lattice, otherwise, an exception is thrown.
  // getSlice parameters:
  // <ul>
  // <li> buffer: a <src>COWPtr<Array<T>></src> or an <src>Array<T></src>
  // <li> start: The starting position (or Bottom Left Corner), within 
  //      the Lattice, of the data to be extracted.
  // <li> shape: The shape of the data to be extracted.  This is not a
  //      position within the Lattice but the actual shape the buffer will 
  //      have after this function is called.  This argument added
  //      to the "start" argument should be the "Top Right Corner".
  // <li> stride: The increment for each axis.  A stride of
  //      one will return every data element, a stride of two will return
  //      every other element.  The IPosition elements may be different for
  //      each respective axis.  Thus, a stride of IPosition(3,1,2,3) says:
  //      fill the buffer with every element whose position has a first 
  //      index between start(0) and start(0)+shape(0), a second index
  //      which is every other element between start(1) and 
  //      (start(1)+shape(1))*2, and a third index of every third element 
  //      between start(2) and (start(2)+shape(2))*3.
  // <li> removeDegenerateAxes: a Bool which dictates whether to remove 
  //      "empty" axis created in buffer. (e.g. extracting an n-dimensional 
  //      from an (n+1)-dimensional will fill 'buffer' with an array that 
  //      has a degenerate axis (i.e. one axis will have a length = 1.) 
  //      Setting removeDegenerateAxes = True will return a buffer with 
  //      a shape that doesn't reflect these superfluous axes.)
  // </ul>
  // 
  // The derived implementations of these functions should return
  // 'True' if "buffer" is a reference to Lattice data and 'False' if it  
  // is a copy.
  // 
  // <note role=tip> 
  // In most cases, it will be more efficient in execution, if you
  // use a LatticeIterator class to move through the Lattice. 
  // LatticeIterators are optimized for that purpose.  If you are doing 
  // unsystematic traversal, or random gets and puts, then getSlice and 
  // putSlice or operator() may be the right tools to use.
  // </note>
  // <group>   
  virtual Bool getSlice(COWPtr<Array<T> > &buffer, const IPosition &start, 
			const IPosition &shape, const IPosition &stride, 
			Bool removeDegenerateAxes=False) const = 0;
  
  virtual Bool getSlice(Array<T> &buffer, const IPosition &start, 
			const IPosition &shape, const IPosition &stride,
			Bool removeDegenerateAxes=False) = 0;
  
  virtual Bool getSlice(COWPtr<Array<T> > &buffer, const Slicer &theSlice, 
			Bool removeDegenerateAxes=False) const = 0;
  
  virtual Bool getSlice(Array<T> &buffer, const Slicer &theSlice, 
			Bool removeDegenerateAxes=False) = 0;
  // </group>
  
  // function which places an Array of values within this instance of the
  // Lattice at the location specified by the IPosition "where", incrementing 
  // by "stride".  All of the IPosition arguments must be of the same
  // dimensionality as the Lattice.  The sourceBuffer array may (and probably
  // will) have less axes than the Lattice.
  virtual void putSlice(const Array<T> &sourceBuffer, const IPosition &where, 
			const IPosition &stride) = 0;
  
  // function which sets all of the elements in the Lattice to value. A default
  // implementation is provided.
  virtual void set(const T &value);
  
  // replace every element, x, of the lattice with the result of f(x).
  // You must pass in the address of the function -- so the function
  // must be declared and defined in the scope of your program.  
  // All versions of apply require a function that accepts a single 
  // argument of type T (the Lattice template actual type) and returns
  // a result of the same type.  The first apply expects a function with
  // an argument passed by value; the second expects the argument to
  // be passed by const reference; the third requires an instance of the class
  // <src>Functional<T,T></src>.  The first form ought to run faster
  // for the built-in types, which may be an issue for large Lattices
  // stored in memory, where disk access is not an issue.
  // <group>
  virtual void apply(T (*function)(T));
  virtual void apply(T (*function)(const T &));
  virtual void apply(const Functional<T,T> &function);
  // </group>

  // These are the true implementations of the parentheses operator.
  // Not recommended for public use.
  // <group>
  virtual T getAt(const IPosition &where) const = 0;
  virtual void putAt(const T &value, const IPosition &where) = 0;
  // </group>
  
  // These are the true implementations of the Lattice Iterators.
  // Not recommended for public use.
  // <group>
  virtual RO_LatticeIterInterface<T> *makeIter(
				 const LatticeNavigator &navigator) const = 0;
  
  virtual RO_LatticeIterInterface<T> *makeIter(
                                      const IPosition &cursorShape) const = 0;
  virtual LatticeIterInterface<T> *makeIter(
				 const LatticeNavigator &navigator) = 0;
  
  virtual LatticeIterInterface<T> *makeIter(
                                      const IPosition &cursorShape) = 0;
  // </group>

    // This function was put in for the Gnu compiler which presently
    // (1996/07/01) is unable to automatically cast a derived class to a
    // base class reference (or pointer) in a templated global function.
    // <group>
    Lattice<T> &latticeCast() {return *this;}
    const Lattice<T> &latticeCast() const {return *this;}
    // </group>

    // Short-hand for "latticeCast().
    // <group>
    Lattice<T> &lc() {return *this;}
    const Lattice<T> &lc() const {return *this;}
    // </group>

  // Try not to have to have more pixels than this in your iterators if you can.
  // Defaults to 1024*1024.
  virtual uInt maxPixels() const;
  // Default implementation sets up a shape that completely fills as many axes 
  // as possible, but always at least the first axis. For example, given a 
  // 10x20x30 lattice
  // maxPixels = 1   --> 10,1,1
  //             100 --> 10,1,1
  //             300 --> 10,20,1
  //           10000 --> 10,20,30
  // Usually, this will be called with maxPixels() as its argument.
  virtual IPosition niceCursorShape(uInt maxPixels) const;

  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;
};

// <summary> an implementation trick for left-value parentheses operators </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//  <li> none.
// </prerequisite>
//
// <etymology>
// LatticeValueRef derives its name from "references to Lattice element 
// values." 
// </etymology>
//
// <synopsis>
// A LatticeValueRef<T> should be treated like a variable of type T.  The user
// should never define a variable to be of type LatticeValueRef<T>.  This
// class is just a nifty way of making left and right value functions work 
// differently.
//
// The following is a description of what takes place behind the scenes:
// A call to Lattice<T>::operator(const IPosition &where) will return a 
// LatticeValueRef<T>.  The returned object may be assigned a new value, thus 
// changing the value within the Lattice that it represents.  Or it
// may be assigned to a variable of type <class T>.  The conversion operator 
// will automatically change the LatticeValueRef<T> to a T.
// L-value operations are never in doubt again.
// </synopsis>
//
// <example>
// The following will place the value NewValue into the position theLocation
// with the Lattice.
// <srcblock> 
// Float NewValue = 42.0;
// IPosition theLocation(3, 11, 22, 33);
// myLattice(theLocation) = NewValue;
// // the same thing, only more terse...
// myLattice(IPosition(3, 11, 22, 33)) = 42.0;
// </srcblock>
// This example shows how to recover a value from within a Lattice.
// <srcblock>
// IPosition theLocation(3, 11, 22, 33);
// Float NewValue = myLattice(theLocation);
// // of course, we may use temporaries and be more terse...
// Float NewValue = myLattice(IPosition(3, 11, 22, 33));
// </srcblock>
// </example>
//
// <motivation>
// We needed a way to separate L-value operator() from R-value in order to
// maintain efficiency and const-ness.
// </motivation>
//
// <templating arg=T>
//   <li> parentheses operators
//   <li> getAt() and putAt() functions.
// </templating>
// 
// <todo asof="1996/04/01">
//   <li> none.
// </todo>

template <class T> class LatticeValueRef
{
public:

// constructor
LatticeValueRef(Lattice<T> *source, const IPosition &where);

// assignment operator
LatticeValueRef<T> &operator=(const T &val);

// conversion operator
operator T();

private:

Lattice<T> *source_p;
IPosition where_p;

};

#endif
