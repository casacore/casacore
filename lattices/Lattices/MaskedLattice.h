//# MaskedLattice.h: Abstract base class for array-like classes with masks
//# Copyright (C) 1998,1999,2000
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

#ifndef LATTICES_MASKEDLATTICE_H
#define LATTICES_MASKEDLATTICE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/Lattice.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LatticeRegion;


// <summary>
// A templated, abstract base class for array-like objects with masks.
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
// <note role=tip> Lattices are always zero origined. </note>
// </synopsis> 

// <example>
// Because Lattice is an abstract base class, an actual instance of this
// class cannot be constructed. However the interface it defines can be used
// inside a function. This is always recommended as it allows Functions
// which have Lattices as arguments to work for any derived class.
//
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
//   const uInt cursorSize = lat.advisedMaxPixels();
//   const IPosition cursorShape = lat.niceCursorShape(cursorSize);
//   const IPosition latticeShape = lat.shape();
//   Complex currentSum = 0.0f;
//   size_t nPixels = 0;
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
//
// <h4>Example 3:</h4>
// Occasionally you may want to access a few elements of a Lattice without
// all the difficulty involved in setting up Iterators or calling getSlice
// and putSlice. This is demonstrated in the example below and uses the
// parenthesis operator, along with the LatticeValueRef companion
// class. Using these functions to access many elements of a Lattice is not
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
//   psf(centrePos) = 1;  // This sets just the centre element to one
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

//# <todo asof="1996/07/01">
//#  <li>
//# </todo>

template <class T> class MaskedLattice : public Lattice<T>
{
  //# Make members of parent class known.
public:
  using Lattice<T>::ndim;
  using Lattice<T>::shape;

public: 
  // Default constructor.
  MaskedLattice()
    : itsDefRegPtr(0) {;}

  // Copy constructor.
  MaskedLattice (const MaskedLattice<T>&);

  // a virtual destructor is needed so that it will use the actual destructor
  // in the derived class
  virtual ~MaskedLattice();

  // Make a copy of the object (reference semantics).
  // <group>
  virtual MaskedLattice<T>* cloneML() const = 0;
  virtual Lattice<T>* clone() const;
  // </group>

  // Has the object really a mask?
  // The default implementation returns True if the MaskedLattice has
  // a region with a mask.
  virtual Bool isMasked() const;

  // Does the lattice have a pixelmask?
  // The default implementation returns False.
  virtual Bool hasPixelMask() const;

  // Get access to the pixelmask.
  // An exception is thrown if the lattice does not have a pixelmask.
  // <group>
  virtual const Lattice<Bool>& pixelMask() const;
  virtual Lattice<Bool>& pixelMask();
  // </group>

  // Get the region used.
  // This is in principle the region pointed to by <src>getRegionPtr</src>.
  // However, if that pointer is 0, it returns a LatticeRegion for the
  // full image.
  const LatticeRegion& region() const;

  // Get the mask or a slice from the mask.
  // This is the mask formed by combination of the possible pixelmask of the
  // lattice and the possible mask of the region taken from the lattice.
  // If there is no mask, it still works fine.
  // In that case it sizes the buffer correctly and sets it to True.
  // <group>   
  Bool getMask (COWPtr<Array<Bool> >& buffer,
		Bool removeDegenerateAxes=False) const;
  Bool getMaskSlice (COWPtr<Array<Bool> >& buffer, const Slicer& section,
		     Bool removeDegenerateAxes=False) const;
  Bool getMaskSlice (COWPtr<Array<Bool> >& buffer, const IPosition& start, 
		     const IPosition& shape,
		     Bool removeDegenerateAxes=False) const;
  Bool getMaskSlice (COWPtr<Array<Bool> >& buffer, const IPosition& start, 
		     const IPosition& shape, const IPosition& stride,
		     Bool removeDegenerateAxes=False) const;
  Bool getMask (Array<Bool>& buffer,
		Bool removeDegenerateAxes=False);
  Bool getMaskSlice (Array<Bool>& buffer, const Slicer& section,
		     Bool removeDegenerateAxes=False);
  Bool getMaskSlice (Array<Bool>& buffer, const IPosition& start,
		     const IPosition& shape,
		     Bool removeDegenerateAxes=False);
  Bool getMaskSlice (Array<Bool>& buffer, const IPosition& start,
		     const IPosition& shape, const IPosition& stride,
		     Bool removeDegenerateAxes=False);
  Array<Bool> getMask (Bool removeDegenerateAxes=False) const;
  Array<Bool> getMaskSlice (const Slicer& section,
			    Bool removeDegenerateAxes=False) const;
  Array<Bool> getMaskSlice (const IPosition& start,
			    const IPosition& shape,
			    Bool removeDegenerateAxes=False) const;
  Array<Bool> getMaskSlice (const IPosition& start,
			    const IPosition& shape, const IPosition& stride,
			    Bool removeDegenerateAxes=False) const;
  // </group>
  
  // The function (in the derived classes) doing the actual work.
  // These functions are public, so they can be used internally in the
  // various Lattice classes.
  // <br>However, doGetMaskSlice does not call Slicer::inferShapeFromSource
  // to fill in possible unspecified section values. Therefore one
  // should normally use one of the getMask(Slice) functions. doGetMaskSlice
  // should be used with care and only when performance is an issue.
  // <br>The default implementation gets the mask from the region
  // and fills the buffer with True values if there is no region.
  virtual Bool doGetMaskSlice (Array<Bool>& buffer, const Slicer& section);

protected:
  // Assignment can only be used by derived classes.
  MaskedLattice<T>& operator= (const MaskedLattice<T>&);

  // Get a pointer to the region used.
  // It can return 0 meaning that the MaskedLattice is the full lattice.
  virtual const LatticeRegion* getRegionPtr() const = 0;

private:
  mutable LatticeRegion* itsDefRegPtr;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/MaskedLattice.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
