//# Lattice.h:  Lattice is an abstract base class for array-like classes
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

#if !defined(AIPS_LATTICE_H)
#define AIPS_LATTICE_H


//# Includes
#include <aips/aips.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <trial/Lattices/LatticeCoordinates.h>

//# Forward Declarations
template <class T> class Array;
template <class T> class COWPtr;
template <class Domain, class Range> class Functional;
template <class T> class LatticeIterInterface;
class LatticeNavigator;


// <summary>
// A templated, abstract base class for array-like objects.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="dLattice.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="IPosition"> IPosition </linkto>
//   <li> Abstract Base class Inheritance - try "Advanced C++" by James
//        O. Coplien, Ch. 5.
// </prerequisite>

// <etymology>
// Lattice: "A regular, periodic configuration of points, particles, 
// or objects, throughout an area of a space..." (American Heritage Directory)
// This definition matches our own: an n-dimensional arrangement of items,
// on regular orthogonal axes.
// </etymology>

// <synopsis>
// This pure abstract base class defines the operations which may be performed
// on any concrete class derived from it.  It has only a few non-pure virtual 
// member functions.
// The fundamental contribution of this class, therefore, is that it 
// defines the operations derived classes must provide:
// <ul>
//    <li> how to extract a "slice" (or sub-array, or subsection) from
//         a Lattice.
//    <li> how to copy a slice in.
//    <li> how to get and put a single element 
//    <li> how to apply a function to all elements
//    <li> various shape related functions.
// </ul>
// <note role=tip> Lattices always have a zero origin. </note>
// </synopsis> 

// <example>
// Because Lattice is an abstract base class, an actual instance of this
// class cannot be constructed. However the interface it defines can be used
// inside a function. This is always recommended as it allows functions
// which have Lattices as arguments to work for any derived class.
// <p>
// I will give a few examples here and then refer the reader to the 
// <linkto class="ArrayLattice">ArrayLattice</linkto> class (a memory resident
// Lattice) and the <linkto class="PagedArray">PagedArray</linkto> class (a
// disk based Lattice) which contain further examples with concrete
// classes (rather than an abstract one). All the examples shown below are used
// in the <src>dLattice.cc</src> demo program.
//
// <h4>Example 1:</h4>
// This example calculates the mean of the Lattice. Because Lattices can be too
// large to fit into physical memory it is not good enough to simply use
// <src>getSlice</src> to read all the elements into an Array. Instead the
// Lattice is accessed in chunks which can fit into memory (the size is
// determined by the <src>maxPixels</src> and <src>niceCursorShape</src>
// functions). The <src>LatticeIterator::cursor()</src> function then returns
// each of these chunks as an Array and the standard Array based functions are
// used to calculate the mean on each of these chunks. Functions like this one
// are the recommended way to access Lattices as the 
// <linkto class="LatticeIterator">LatticeIterator</linkto> will correctly
// setup any required caches.
//
// <srcblock>
// Complex latMean(const Lattice<Complex>& lat) {
//   const uInt cursorSize = lat.maxPixels();
//   const IPosition cursorShape = lat.niceCursorShape(cursorSize);
//   const IPosition latticeShape = lat.shape();
//   Complex currentSum = 0.0f;
//   uInt nPixels = 0u;
//   RO_LatticeIterator<Complex> iter(lat, 
// 				   LatticeStepper(latticeShape, cursorShape));
//   for (iter.reset(); !iter.atEnd(); iter++){
//     currentSum += sum(iter.cursor());
//     nPixels += iter.cursor().nelements();
//   }
//   return currentSum/nPixels;
// }
// </srcblock>
//
// <h4>Example 2:</h4>
// Sometimes it will be neccesary to access slices of a Lattice in a nearly
// random way. Often this can be done using the subSection commands in the
// <linkto class="LatticeStepper">LatticeStepper</linkto> class. But it is also
// possible to use the getSlice and putSlice functions. The following example
// does a two-dimensional Real to Complex Fourier transform. This example is
// restricted to four-dimensional Arrays (unlike the previous example) and does
// not set up any caches (caching is currently only used with PagedArrays).  So
// only use getSlice and putSlice when things cannot be done using
// LatticeIterators.
//
// <srcblock>
// void FFT2DReal2Complex(Lattice<Complex>& result, 
// 		       const Lattice<Float>& input){
//   AlwaysAssert(input.ndim() == 4, AipsError);
//   const IPosition shape = input.shape();
//   const uInt nx = shape(0);
//   AlwaysAssert (nx > 1, AipsError);
//   const uInt ny = shape(1);
//   AlwaysAssert (ny > 1, AipsError);
//   const uInt npol = shape(2);
//   const uInt nchan = shape(3); 
//   const IPosition resultShape = result.shape();
//   AlwaysAssert(resultShape.nelements() == 4, AipsError);
//   AlwaysAssert(resultShape(3) == nchan, AipsError);
//   AlwaysAssert(resultShape(2) == npol, AipsError);
//   AlwaysAssert(resultShape(1) == ny, AipsError);
//   AlwaysAssert(resultShape(0) == nx/2 + 1, AipsError);
//
//   const IPosition inputSliceShape(4,nx,ny,1,1);
//   const IPosition resultSliceShape(4,nx/2+1,ny,1,1);
//   COWPtr<Array<Float> > 
//     inputArrPtr(new Array<Float>(inputSliceShape.nonDegenerate()));
//   Array<Complex> resultArray(resultSliceShape.nonDegenerate());
//   FFTServer<Float, Complex> FFT2D(inputSliceShape.nonDegenerate());
//  
//   IPosition start(4,0);
//   Bool isARef;
//   for (uInt c = 0; c < nchan; c++){
//     for (uInt p = 0; p < npol; p++){
//       isARef = input.getSlice(inputArrPtr,
//                               Slicer(start,inputSliceShape), True);
//       FFT2D.fft(resultArray, *inputArrPtr);
//       result.putSlice(resultArray, start);
//       start(2) += 1;
//     }
//     start(2) = 0;
//     start(3) += 1;
//   }
// }
// </srcblock>
// Note that the <linkto class=LatticeFFT>LatticeFFT</linkto> class
// offers a nice way to do lattice based FFTs.
//
// <h4>Example 3:</h4>
// Occasionally you may want to access a few elements of a Lattice without
// all the difficulty involved in setting up Iterators or calling getSlice
// and putSlice. This is demonstrated in the example below.
// Setting a single element can be done with the <src>putAt</src> function,
// while getting a single element can be done with the parenthesis operator.
// Using these functions to access many elements of a Lattice is not
// recommended as this is the slowest access method.
//
// In this example an ideal point spread function will be inserted into an
// empty Lattice. As with the previous examples all the action occurs
// inside a function because Lattice is an interface (abstract) class.
//
// <srcblock>
// void makePsf(Lattice<Float>& psf) {
//   const IPosition centrePos = psf.shape()/2;
//   psf.set(0.0f);       // this sets all the elements to zero
//                        // As it uses a LatticeIterator it is efficient
//   psf.putAt (1, centrePos);  // This sets just the centre element to one
//   AlwaysAssert(near(psf(centrePos), 1.0f, 1E-6), AipsError);
//   AlwaysAssert(near(psf(centrePos*0), 0.0f, 1E-6), AipsError);
// }
// </srcblock>
// </example>

// <motivation>
// Creating an abstract base class which provides a common interface between
// memory and disk based arrays has a number of advantages.
// <ul>
// <li> It allows functions common to all arrays to be written independent
// of the way the data is stored. This is illustrated in the three examples
// above. 
// <li> It reduces the learning curve for new users who only have to become
// familiar with one interface (ie. Lattice) rather than distinct interfaces
// for different array types. 
// </ul>
// </motivation>

// <todo asof="1996/07/01">
//   <li> Remove the latticeCast member when the GNU compiler is 
//        sufficiently robust.
//   <li> Make PagedArray cache functions virtual in this base class.
// </todo>


template <class T> class Lattice 
{
public: 
  // a virtual destructor is needed so that it will use the actual destructor
  // in the derived class
  virtual ~Lattice();

  // Make a copy of the derived object (reference semantics).
  virtual Lattice<T>* clone() const = 0;

  // Is the lattice paged to disk?
  // <br>Default implementation returns False.
  virtual Bool isPaged() const;

  // Is the lattice writable?
  // <br>Default implementation returns True.
  virtual Bool isWritable() const;

  // Return the shape of the Lattice including all degenerate axes
  // (ie. axes with a length of one)
  virtual IPosition shape() const = 0;
  
  // Return the number of axes in this Lattice. This includes all
  // degenerate axes.
  // Default implementation returns shape().nelements().
  virtual uInt ndim() const;
  
  // Return the total number of elements in this Lattice.
  // Default implementation returns shape().product().
  virtual uInt nelements() const;
  
  // Return a value of "True" if this instance of Lattice and 'other' have 
  // the same shape, otherwise returns a value of "False".
  virtual Bool conform(const Lattice<T>& other) const;

  // Return the coordinates of the lattice.
  // The default implementation returns an 'empty' LattCoord object.
  virtual LatticeCoordinates latticeCoordinates() const;

  // Return the value of the single element located at the argument
  // IPosition.  
  // <br> The default implementation uses getSlice.
  // <group>
  T operator() (const IPosition& where) const;
  virtual T getAt (const IPosition& where) const;
  // </group>
  
  // Put the value of a single element.
  // <br> The default implementation uses putSlice.
  virtual void putAt (const T& value, const IPosition& where);

  // Functions which extract an Array of values from a Lattice. All the
  // IPosition arguments must have the same number of axes as the underlying
  // Lattice, otherwise, an exception is thrown. <br>
  // The parameters are:
  // <ul>
  // <li> buffer: a <src>COWPtr<Array<T>></src> or an
  //      <src>Array<T></src>. See example two above for an examples.
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
  // <li> section: Another way of specifying the start, shape and stride
  // <li> removeDegenerateAxes: a Bool which dictates whether to remove 
  //      "empty" axis created in buffer. (e.g. extracting an n-dimensional 
  //      from an (n+1)-dimensional will fill 'buffer' with an array that 
  //      has a degenerate axis (i.e. one axis will have a length = 1.) 
  //      Setting removeDegenerateAxes = True will return a buffer with 
  //      a shape that doesn't reflect these superfluous axes.)
  // </ul>
  // 
  // The derived implementations of these functions return
  // 'True' if "buffer" is a reference to Lattice data and 'False' if it  
  // is a copy. 
  // <group>   
  Bool get (COWPtr<Array<T> >& buffer,
	    Bool removeDegenerateAxes=False) const;
  Bool getSlice (COWPtr<Array<T> >& buffer, const Slicer& section,
		 Bool removeDegenerateAxes=False) const;
  Bool getSlice (COWPtr<Array<T> >& buffer, const IPosition& start, 
		 const IPosition& shape,
		 Bool removeDegenerateAxes=False) const;
  Bool getSlice (COWPtr<Array<T> >& buffer, const IPosition& start, 
		 const IPosition& shape, const IPosition& stride,
		 Bool removeDegenerateAxes=False) const;
  Bool get (Array<T>& buffer,
	    Bool removeDegenerateAxes=False);
  Bool getSlice (Array<T>& buffer, const Slicer& section,
		 Bool removeDegenerateAxes=False);
  Bool getSlice (Array<T>& buffer, const IPosition& start,
		 const IPosition& shape,
		 Bool removeDegenerateAxes=False);
  Bool getSlice (Array<T>& buffer, const IPosition& start,
		 const IPosition& shape, const IPosition& stride,
		 Bool removeDegenerateAxes=False);
  Array<T> get (Bool removeDegenerateAxes=False) const;
  Array<T> getSlice (const Slicer& section,
		     Bool removeDegenerateAxes=False) const;
  Array<T> getSlice (const IPosition& start,
		     const IPosition& shape,
		     Bool removeDegenerateAxes=False) const;
  Array<T> getSlice (const IPosition& start,
		     const IPosition& shape, const IPosition& stride,
		     Bool removeDegenerateAxes=False) const;
  // </group>
  
  // A function which places an Array of values within this instance of the
  // Lattice at the location specified by the IPosition "where", incrementing 
  // by "stride".  All of the IPosition arguments must be of the same
  // dimensionality as the Lattice.  The sourceBuffer array may (and probably
  // will) have less axes than the Lattice. The stride defaults to one if
  // not specified. 
  // <group>   
  void putSlice (const Array<T>& sourceBuffer, const IPosition& where,
		 const IPosition& stride)
    { doPutSlice (sourceBuffer, where, stride); }
  void putSlice (const Array<T>& sourceBuffer, const IPosition& where);
  void put (const Array<T>& sourceBuffer);
  
  // </group>   

  // Set all elements in the Lattice to the given value.
  virtual void set (const T& value);
  
  // Replace every element, x, of the Lattice with the result of f(x).  You
  // must pass in the address of the function -- so the function must be
  // declared and defined in the scope of your program.  All versions of
  // apply require a function that accepts a single argument of type T (the
  // Lattice template type) and return a result of the same type.  The first
  // apply expects a function with an argument passed by value; the second
  // expects the argument to be passed by const reference; the third
  // requires an instance of the class <src>Functional<T,T></src>.  The
  // first form ought to run faster for the built-in types, which may be an
  // issue for large Lattices stored in memory, where disk access is not an
  // issue.
  // <group>
  virtual void apply (T (*function)(T));
  virtual void apply (T (*function)(const T&));
  virtual void apply (const Functional<T,T>& function);
  // </group>

  // This function returns the recommended maximum number of pixels to
  // include in the cursor of an iterator. The default implementation
  // returns a number that is a power of two and includes enough pixels to
  // consume between 4 and 8 MBytes of memory.
  virtual uInt maxPixels() const;

  // Returns a recommended cursor shape for iterating through all the pixels
  // in the Lattice. The default implementation sets up a shape that
  // completely fills as many axes as possible, but always at least the
  // first axis. For example, given a 10x20x30 Lattice 
  // <srcblock>
  // maxPixels = 1     --> niceCursorShape = [10,1,1]
  //             100   --> niceCursorShape = [10,1,1]
  //             300   --> niceCursorShape = [10,20,1] 
  //             10000 --> niceCursorShape = [10,20,30] 
  // </srcblock>
  // The default argument is the result of <src>maxPixels()</src>.
  // <group>
  IPosition niceCursorShape (uInt maxPixels) const
    { return doNiceCursorShape (maxPixels); }
  IPosition niceCursorShape() const
    { return doNiceCursorShape (maxPixels()); }
  // </group>

  // Copy the data from the given lattice to this one.
  // The default implementation uses function <src>copyDataTo</src>.
  virtual void copyData (const Lattice<T>& from);

  // Copy the data from this lattice to the given lattice.
  // The default implementation only copies data (thus no mask, etc.).
  virtual void copyDataTo (Lattice<T>& to) const;

  // Check class internals - used for debugging. Should always return True
  virtual Bool ok() const;

  // These functions are used by the LatticeIterator class to generate an
  // iterator of the correct type for a specified Lattice. Not recommended
  // for general use.
  // <br>The default implementation creates a LatticeIterInterface object.
  virtual LatticeIterInterface<T>* makeIter
				(const LatticeNavigator& navigator) const;

  // These functions were put in for the Gnu compiler which presently
  // (version 2.7.2.1) is unable to automatically cast a derived class to a
  // base class in a templated global function.
  // <group>
  Lattice<T>& latticeCast() {return *this;}
  const Lattice<T>& latticeCast() const {return *this;}
  // </group>

  // Short-hand for <src>latticeCast()</src>
  // <group>
  Lattice<T>& lc() {return *this;}
  const Lattice<T>& lc() const {return *this;}
  // </group>

  // The functions (in the derived classes) doing the actual work.
  // These functions are public, so they can be used internally in the
  // various Lattice classes, which is especially useful for doGetSlice.
  // <br>However, doGetSlice does not call Slicer::inferShapeFromSource
  // to fill in possible unspecified section values. Therefore one
  // should normally use one of the get(Slice) functions. doGetSlice
  // should be used with care and only when performance is an issue.
  // <group>
  virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section) = 0;
  virtual void doPutSlice (const Array<T>& buffer, const IPosition& where,
			   const IPosition& stride) = 0;
  virtual IPosition doNiceCursorShape (uInt maxPixels) const;
  // </group>

protected:
  // Define default constructor to satisfy compiler.
  Lattice() {};

  // Copy constructor and assignment can only be used by derived classes.
  // <group>
  Lattice (const Lattice<T>&) {};
  Lattice<T>& operator= (const Lattice<T>&)
    { return *this; }
  // </group>
};


#endif
