//# LatticeStepper.h:  provides 'natural' traversal, by cursor shape
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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

#ifndef LATTICES_LATTICESTEPPER_H
#define LATTICES_LATTICESTEPPER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/LatticeIndexer.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Traverse a Lattice by cursor shape
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tLatticeStepper.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeNavigator> LatticeNavigator </linkto>
// </prerequisite>

// <etymology>
// LatticeStepper is so-called because it performs the calculations
// necessary to step through a Lattice.  The next position is always one
// simple step forward from the current position.  The step-size is
// calculated directly from the size of the LatticeIterator's cursor or
// window.
// </etymology>

// <synopsis> 
// When you wish to traverse a Lattice (say, a PagedArray or an Image) you
// will usually create a LatticeIterator.  Once created, you must attach a
// LatticeNavigator to the iterator. A LatticeStepper, is a concrete class
// derived from the abstract LatticeNavigator that allows you to move
// sequentially through the Lattice.
// <p>
// In constructing a LatticeStepper, you specify the Lattice shape and the
// shape of the "cursor" used to step through the data. The cursor position
// can be incremented or decremented to retrieve the next portion of the
// Lattice.
// The specified cursor shape can (and often will) have fewer dimensions
// that the Lattice itself. For example if we have a 4-dimensional Lattice
// with <src>latticeShape = IPosition(4,64,64,4,16)</src>, then specifying a
// cursor of <src>cursorShape = IPosition(1,64)</src>, will step through the
// hypercube row by row. When the cursor shape has fewer dimensions than the
// Lattice degenerate dimensions are added to the end of the cursor so that
// in the above example the specified cursor is assumed to mean
// <src>cursorShape = IPosition(4,64,1,1,1)</src>. To access the data
// spectrum by spectrum (assuming the last axis is the spectral axis), you
// must use a 1-dimensional cursor of <src>IPosition(4,1,1,1,16)</src>. The
// <src>cursorShape</src> function always returns a shape with as many
// dimensions as the underlying Lattice.
// <p>
// It is an error (and an exception will be thrown) if the cursor has more
// dimensions than the Lattice or if it is larger on any axis than the
// Lattice shape.
// <br>
// Also the cursor shape on all axes must be less than or equal to the Lattice
// shape on that axis. Otherwise an exception will be thrown. 
// <p>
// In principle cursor axes with length 1 are degenerate axes. They
// are removed from the lattice cursor if the
// <linkto class=LatticeIterator>LatticeIterator</linkto> cursor is accessed
// using e.g. the <src>matrixCursor</src> function.
// Using a special LatticeStepper constructor it is, however, possible
// to specify which cursor axes with length 1 have to be treated as
// normal axes. In that way one can be sure that a cursor is, for
// example, always 2D, even if an axis happens to have length 1.
// <srcblock>
// IPosition latticeShape(4,20,16,1,4);
// IPosition cursorAxes(2,1,2);
// IPosition cursorShape(2,16,1);
// IPosition axisPath;
// LatticeStepper stepper(latticeShape, cursorShape,
//                        cursorAxes, axisPath);
// </srcblock>
// This results in a cursor with shape [1,16,1,1]. The first and last
// axis are degenerate, so the cursor can also be accessed using
// <src>matrixCursor</src> (with shape [16,1]).
// Note that the cursor shape could also be specified as [1,16,1,1].
// <p>
// The "path" of the cursor through the Lattice can be controlled by
// specifying an axisPath during construction of the class. This is an
// IPosition which has exactly as many elements as the Lattice
// dimension. Each element must contain an integer between 
// 0 -- Lattice_Dimension-1, and must be unique. For example,
// <srcblock>
// axisPath = IPosition(4,0,1,2,3) or
// axisPath = IPosition(4,3,1,2,0) 
// </srcblock>
// are valid but
// <srcblock>
// axisPath = IPosition(4,1,2,3,4) or
// axisPath = IPosition(4,0,1,1,3) 
// </srcblock>
// are not, given the latticeShape specified above. An exception is thrown
// if the AxisPath is bad. 
// <br>
// The "axis path" defines which axis will be iterated through fastest as
// the cursor moves through the Lattice. With the above mentioned
// 4-dimensional Lattice and a single element cursor
// (<src>cursorShape=IPosition(4,1,1,1,1)</src>) setting an
// <src>axisPath=IPosition(4,0,1,2,3)</src> will move the cursor through all
// the columns, and then onto the next row, and again through all the
// columns in the second row. Once all the rows in the first plane have
// been exhausted the cursor will then iterate to the next plane, and
// eventually to the next spectral channel. If, however, the axisPath was
// <src>axisPath=IPosition(4,3,0,1,2)</src> then the cursor would iterate
// through each spectral channel first, before moving onto the next column in
// the first row.
// <p>
// The cursor never changes dimensionality as it traverses the Lattice.  But it
// may change shape if the cursor shape is not a factor of the Lattice
// shape. A cursor shape is not a factor of the Lattice shape if the Lattice
// shape is not an integer multiple of the cursor shape on all axes.
// The integer multiplier need not to be the same for each axes.
// For example, for a Lattice of shape [10,10,10] a cursor of shape [8,5,2]
// is not a factor but one with a shape of [10,5,1] is.
// <br>
// When the cursor is not congruent with the Lattice moving the cursor through
// the Lattice will sometimes result in part of the cursor hanging over the
// edge of the Lattice. When this occurs the hangOver member function will
// return True. What to do in these situtations is specified by the
// hangOverPolicy enumerator.
// <ol>
// <li>
// If the LatticeStepper::PAD option (the default) is used at construction time
// the cursor shape does not change. The parts of the cursor that hang over the
// edge of the Lattice are filled with a default value, usually zero, that is
// defined by the particular LatticeIterator used.
// <li>
// If the LatticeStepper::RESIZE option is used at construction time the cursor
// shape does change to a smaller value when near the edge of the Lattice so
// that it is just big enough. For example with a Lattice shape of 10x10 and a
// cursor of 8x8 the cursor shape will initally be 8x8, then resize to 2x8 on
// the first step, then resize to 8x2 on the second step and finally resize to
// 2x2. The hangover function will return True for the last three steps, even
// though the cursor has resized.
// </ol>
// The portion of the Lattice that the cursor will traverse can be
// restricted to a region defined by a top right corner, bottom left corner
// and a step size. This is done using the <src>subSection</src> function,
// which also resets the cursor position to the origin of the sub-Lattice.
// The cursor shape will remain unchanged. It is no error when the cursor
// shape exceeds the sub-Lattice shape (instead it is a hangover state).
// <br>
// If a sub-Lattice is defined then cursor positions relative
// to the sub-Lattice origins can be obtained using the
// <src>relativePosition</src> function rather than the
// <src>position</src> function, which always returns positions relative to
// the origin of the main Lattice.
// <br>
// To change the size of the sub-Lattice simply call the
// <src>subSection</src> function again with a different trc, blc &
// inc. This first clears the old sub-Lattice, then imposes the newly
// specified one, and finally moves the cursor to the origin of the
// new sub-Lattice.
// </synopsis> 

// <example>
// This example is of a global function that will iterate through a
// 4-dimensional Lattice. It is assumed that the axes are RA, Dec, Stokes &
// Frequency, and it will calculate the average flux in the I polarization
// on each frequency channel. Imagine it is passed a data set (ie. Lattice)
// of size 256 x 256 x 4 x 1024. This corresponds to 1GByte of data. However
// the iterator will page through this data using a cursor of size 256 x 256
// (or 256kByte) and will only read (because of subsectioning) the relevant
// quarter of the data set. It is usually a good idea to set up the axis
// path as this is gives hints to data cache about which data to retrieve in
// advance.
// <srcblock>
// void averageFluxByChannel(const Lattice<Float>& data)
// {
//   // for convenience, get the shape into a local variable
//   IPosition latticeShape = data.shape();
//   cout << "Data has shape: " << latticeShape << endl;
// 
//   // check that the data has 4 axes.
//   DebugAssert(latticeShape.nelements() == 4, AipsError); 
//     
//   // specify the cursor, or window shape.  Here the cursor is a matrix 
//   // that is the shape of the first plane of our Lattice.
//   // For convenience, get the first two axis lengths into local vars
//   uInt nCols = latticeShape(0);
//   uInt nRows = latticeShape(1);
//   IPosition cursorShape(2, nCols, nRows);
// 
//   // construct a stepper, which needs to know the shape of the lattice
//   // and the shape of the iterator's cursor. By using cursorShape, which
//   // is directly determined by the lattice's shape, we can be sure
//   // that the cursor is a factor of the lattice, and thus that
//   // all elements will be picked up efficiently during the traversal.
//   // Because we will not be iterating through the stokes axis this axis
//   // is made the slowest moving one. 
//   IPosition axisPath(4, 0, 1, 3, 2)
//   LatticeStepper stepper(latticeShape, cursorShape, axisPath);
//
//   // Subsection the stepper so that it only iterates through the I
//   // Stokes parameter (assumed to be when the third axis is zero)
//   uInt nFreqs = latticeShape(3);
//   IPosition blc(4, 0, 0, 0, 0), trc(4, nCols-1, nRows-1, 0, nFreqs-1);
//   stepper.subSection(blc, trc);
//  
//   // construct the iterator.  Since we only want to read the Data,
//   // use the read-only class, which disallows writing back to the cursor
//   // (and hence is more efficient).
//   RO_LatticeIterator<Float> iterator(data, stepper);
// 
//   Vector<Float> spectrum(nFreqs);
//   spectrum = 0.0;
//   uInt channel = 0;
//   for (iterator.reset(); !iterator.atEnd(); iterator++) {
//     const Matrix<Float>& cursor = iterator.matrixCursor();
//     for (uInt col = 0; col < nCols; col++) {
//       for (uInt row = 0; row < nRows; row++) {
//         spectrum(channel) += cursor(col, row);
//       }
//     }
//     channel++;
//   } // for iterator
//   cout << "Average spectrum is: " 
//        << spectrum / cursorShape.product() << endl;
// }
// </srcblock>
// </example>

// <motivation>
// Moving through a Lattice by equal sized chunks, and without regard
// to the nature of the data, is a basic and common procedure.  
// </motivation>

//# <todo asof="1995/08/28">
//# </todo>


class LatticeStepper: public LatticeNavigator
{
public:

  // The hangOverPolicy enumerator is used in the constructors to indicate
  // what this class should do when the cursor shape hangs over the edge
  // of the Lattice. 
  enum hangOverPolicy {
  // PAD is the default and means that the cursor size supplied by the user is
  // kept fixed. But if the cursor overhangs the Lattice the part that
  // overhangs is filled with a default value that is specified by the
  // Iterator. Currently the default value is zero. 
  PAD,
  // RESIZE means that the cursor shape is adjusted whenever it approaches the
  // edges of the Lattice so that it is always the right size to include only
  // the parts of the Lattice that are available. The user specified cursor
  // shape now becomes the default and largest possible cursor shape. 
  RESIZE};

  // The first argument is the shape of the Lattice to be iterated and the
  // second argument is the shape of the cursor. The cursor will increment
  // initially along first axis, then the second and then the third
  // (ie. axisPath = IPosition(ndim,0,1,2,...))
  // The dimensionality of the cursorShape can be less than the
  // dimensionality of the lattice. It will be padded with 1s.
  // <br>The cursorShape axes with length > 1 are seen as the true cursor axes.
  // The other axes are degenerated and are removed by the functions
  // <src>vectorCursor()</src>, etc., in class
  // <linkto class=RO_LatticeIterator>(RO_)LatticeIterator</linkto>.
  LatticeStepper (const IPosition& latticeShape, const IPosition& cursorShape,
		  const uInt hangOverPolicy=PAD);

  // Same as the above constructor except that the axis path is explicitly
  // specified. The axis path is described in the synopsis above. 
  LatticeStepper (const IPosition& latticeShape, const IPosition& cursorShape,
		  const IPosition& axisPath, const uInt hangOverPolicy=PAD);
  
  // Same as the above constructor except that the cursor axes are
  // explicitly specified. This can be useful to avoid that cursor axes
  // with length=1 are treated as degenerated axes by the Iterator classes.
  // The following rules have to be obeyed:
  // <br>- <src>cursorAxes.nelements() <= latticeShape.nelements()</src>
  // <br>- <src>cursorShape.nelements() == latticeShape.nelements()</src>
  // <br>or <src>cursorShape.nelements() == cursorAxes.nelements()</src>
  // The latter means that the cursorShape contains the axes mentioned in
  // cursorAxes.
  // <br>See also the example in the synopsis.
  LatticeStepper (const IPosition& latticeShape, const IPosition& cursorShape,
		  const IPosition& cursorAxes,
		  const IPosition& axisPath, const uInt hangOverPolicy=PAD);
  
  // The copy constructor uses copy semantics.
  LatticeStepper (const LatticeStepper& other);
    
  ~LatticeStepper();

  // The assignment operator uses copy semantics.
  LatticeStepper& operator= (const LatticeStepper& other);

  // Increment operator (postfix version) - move the cursor
  // forward one step. Returns True if the cursor was moved.
  virtual Bool operator++(int);

  // Decrement operator (postfix version) - move the cursor
  // backwards one step. Returns True if the cursor was moved.
  virtual Bool operator--(int);

  // Function to move the cursor to the beginning of the (sub)-Lattice. Also
  // resets the number of steps (<src>nsteps</src> function) to zero. 
  virtual void reset();

  // Function which returns "True" if the cursor is at the beginning of the
  // (sub)-Lattice, otherwise, returns "False"
  virtual Bool atStart() const;

  // Function which returns "True" if an attempt has been made to increment
  // the cursor beyond the end of the (sub)-Lattice.
  virtual Bool atEnd() const;

  // Function to return the number of steps (increments & decrements) taken
  // since construction (or since last reset).  This is a running count of
  // all cursor movement (operator++ or operator--), even though
  // N-increments followed by N-decrements will ALWAYS leave the cursor in
  // the original position.
  virtual uInt nsteps() const;

  // Functions which return the current position of the beginning of the
  // cursor. The <src>position</src> function is relative to the origin
  // in the main Lattice and the <src>relativePosition</src> function is
  // relative to the origin and increment used in the sub-Lattice (defined
  // using the <src>subSection</src> function). If no sub-Lattice is defined
  // the two functions return identical positions.
  // <group>
  virtual IPosition position() const;
  virtual IPosition relativePosition() const;
  // </group>

  // Functions which return the current position of the end of the
  // cursor. The <src>endPosition</src> function is relative to the origin
  // in the main Lattice and the <src>relativeEndPosition</src> function
  // is relative to the origin and increment used in the sub-Lattice
  // (defined using the <src>subSection</src> function). If no sub-Lattice
  // is defined the two functions return identical positions.
  // <note role=caution> It returns the end position in the lattice and
  // does not take overhang into account. </note>
  // <group>
  virtual IPosition endPosition() const;
  virtual IPosition relativeEndPosition() const;
  // </group>

  // Functions which return the shape of the Lattice being iterated
  // through. <src>latticeShape</src> always returns the shape of the main
  // Lattice while <src>subLatticeShape</src> returns the shape of any
  // sub-Lattice defined using the <src>subSection</src> function. 
  // <group>
  virtual IPosition latticeShape() const;
  virtual IPosition subLatticeShape() const;
  // </group>

  // Functions to change the cursor shape to a new one. They always reset
  // the cursor to the beginning of the Lattice (and reset the number of
  // steps to zero).
  // <group>
  void setCursorShape (const IPosition& cursorShape);
  void setCursorShape (const IPosition& cursorShape,
		       const IPosition& cursorAxes);
  // </group>

  // Function which returns the shape of the cursor. This always includes
  // all axes (ie. it includes degenerates axes)
  virtual IPosition cursorShape() const;

  // Function which returns the axes of the cursor.
  virtual IPosition cursorAxes() const;

  // Function which returns "True" if the increment/decrement operators have
  // moved the cursor position such that part of the cursor beginning or end
  // is hanging over the edge of the (sub)-Lattice.
  virtual Bool hangOver() const;

  // Functions to specify a "section" of the Lattice to step over. A section
  // is defined in terms of the Bottom Left Corner (blc), Top Right Corner
  // (trc), and step size (inc), on ALL of its axes, including degenerate
  // axes. The step size defaults to one if not specified.
  // <group>
  virtual void subSection (const IPosition& blc, const IPosition& trc);
  virtual void subSection (const IPosition& blc, const IPosition& trc, 
			   const IPosition& inc);
  // </group>

  // Return the bottom left hand corner (blc), top right corner (trc) or
  // step size (increment) used by the current sub-Lattice. If no
  // sub-Lattice has been defined (with the <src>subSection</src> function)
  // these functions return blc=0, trc=latticeShape-1, increment=1, ie. the
  // entire Lattice.
  // <group>
  virtual IPosition blc() const;
  virtual IPosition trc() const;
  virtual IPosition increment() const;
  // </group>

  // Return the axis path.
  virtual const IPosition& axisPath() const;

  // Function which returns a pointer to dynamic memory of an exact copy 
  // of this instance.  The pointer returned by this function must
  // be deleted externally.
  virtual LatticeNavigator* clone() const;

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. 
  // Returns True if everything is fine otherwise returns False
  virtual Bool ok() const;

  // Calculate the cache size (in tiles) for this type of access to a lattice
  // in the given row of the tiled hypercube.
  virtual uInt calcCacheSize (const IPosition& cubeShape,
                              const IPosition& tileShape,
                              uInt maxCacheSize, uInt bucketSize) const;

private:
  // Prevent the default constructor from being used.
  LatticeStepper();
  // Pad the cursor to the right number of dimensions.
  void padCursor();
  // Check if the cursor shape is a factor of the Lattice shape.
  Bool niceFit() const;


  LatticeIndexer itsIndexer;//# Knows about the (sub)-Lattice shape and how
                            //# to traverse it.
  IPosition itsCursorAxes;  //# the cursor axes
  IPosition itsCursorShape; //# The shape of the cursor
  IPosition itsCursorPos;   //# The current position of the iterator.
  IPosition itsAxisPath;    //# the heading to follow for the cursor 
  uInt itsNsteps;           //# the number of iterator steps taken thus far; 
                            //# set to 0 on reset ()
  Bool itsEnd;              //# is the cursor beyond the end?
  Bool itsStart;            //# is the cursor at the beginning?
  Bool itsNiceFit;          //# if the cursor shape is a sub-multiple of the
                            //# Lattice shape then set this to True. Used to
			    //# avoid needing to test for a cursor hanging
			    //# over the edge of the lattice.
  Bool itsHangover;         //# this data member is set by the increment and
                            //# decrement operators if itsNiceFit == False. It
                            //# is used to tell if the cursor "Hangs over"
                            //# the edge of the lattice shape.
  uInt itsPolicy;           //# what to do if the cursor does hang over
};



} //# NAMESPACE CASACORE - END

#endif
