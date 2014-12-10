//# TiledLineStepper.h: Step a Vector cursor optimally through a tiled Lattice
//# Copyright (C) 1997,1998,1999,2000
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

#ifndef LATTICES_TILEDLINESTEPPER_H
#define LATTICES_TILEDLINESTEPPER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/LatticeIndexer.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Step a Vector cursor optimally through a tiled Lattice.
// </summary>
// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tTiledLineStepper.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeNavigator> LatticeNavigator </linkto>
// </prerequisite>

// <etymology>
// TiledLineStepper is used to step a Vector cursor optimally through
// a Lattice that is tiled.
// </etymology>

// <synopsis> 
// When you wish to traverse a Lattice (say, a PagedArray or an Image) you
// will usually create a LatticeIterator.  Once created, you may attach a
// LatticeNavigator to the iterator. A TiledLineStepper, is a concrete class
// derived from the abstract LatticeNavigator that allows you to move
// a Vector cursor through the Lattice in a way that will minimize the
// amount of cache memory consumed.
// <p>
// Some Lattices (in particular PagedArrays) are stored (on disk) in
// tiles. For an N-dimensional Lattice a tile is an N-dimensional
// subsection with fewer elements along each axis. For example a Lattice of
// shape [512,512,4,32] may have a tile shape of [32,16,4,16], and there
// will be 16*32*1*2 (=1024) tiles in the entire Lattice. To allow efficient
// access of the data in a Lattice some tiles are cached in memory. As each
// tile may consume a fair bit of memory (in this example 128kBytes,
// assuming each element consumes 4 bytes), it is desirable to minimise the
// number of tiles held in the cache. But it is also desirable to minimise
// the number of times a tiles must be read into or written from the
// cache as this may require a time consuming operation like disk I/O.
// <p>
// Now suppose you wanted to traverse a Lattice with a Vector cursor of
// length 512 pixel aligned along the x-axis. Using a
// <linkto class=LatticeStepper>LatticeStepper</linkto>, each Vector is
// retrieved from the Lattice sequentially and without any consideration of
// the underlying tile shape. What is the optimal cache size for the above
// example?
// <p>
// Suppose we have a cache size of 16 ie., the number of tiles along the
// x-axis. Then Vectors beginning at positions [0,0,0,0] to [0,15,0,0] will
// be stored in the cache. But the next Vector beginning at position
// [0,16,0,0] will flush the cache and read in another 16 tiles. This I/O
// takes time and will occur 16 times for each plane in the four dimensional
// Lattice. Further when the cursor moves to position [0,0,1,0] the 16 tiles
// that where initially in the cache will need to be read again. To avoid
// all this cache I/O it is better to have a bigger cache.
// <p>
// Suppose the cache size is 16*32 (=512) ie., enough tiles to contain an
// (x,y)-plane. Then the cache size will not be flushed until the cursor is
// moved to position [0,0,0,16]. Further the cache will never need to read
// back into memory tiles that had previously been stored in there. The
// cache is big enough to store tiles until they have been completely
// used. But this cache is 64MBytes in size, and consumes too much memory
// for many computers.
// <p>
// This where a TiledLineStepper is useful. Because it knows the shape of the
// tiles in the underlying Lattice it moves the cursor to return all the
// Vectors in the smallest possible cache of tiles before moving on to the
// next set of tiles. Using the above example again, the TiledLineStepper will
// move the beginning of the Vector cursor in the following pattern.
// <srcblock>
// [0,0,0,0], [0,1,0,0], [0,2,0,0], ... [0,15,0,0]
// [0,0,1,0], [0,1,1,0],            ... [0,15,1,0], 
//                                  ... [0,15,3,0],
// [0,0,0,1], ...                       [0,15,3,15]
// </srcblock>
// Moving the Vector cursor through all 16*4*16 (=1024 positions) can be
// done by caching only 16 tiles in memory (those along the x-axis). Hence
// the cache size need only be 2MBytes in size. Further once all 1024
// vectors have been returned it is not necessary to read these 16 tiles
// back into memory. All the data in those tiles has already been
// accessed. Using a TiledLineStepper rather than a LatticeStepper has,
// in this example, resulted in a drop in the required cache size from
// 64MBytes down to 2MBytes. 
// <p>
// In constructing a TiledLineStepper, you specify the Lattice shape, the
// tile shape and the axis the Vector cursor will be aligned with. Specifying
// an axis=0 will align the cursor with the x-axis and axis=2 will produce a
// cursor that is along the z-axis. The length of the cursor is always the
// same as the number of elements in the Lattice along the axis the cursor
// is aligned with.
// <br>It is possible to use the function <src>subSection</src> to
// traverse only a subsection of the lattice.
// <p>
// The cursor position can be incremented or decremented to retrieve the next
// or previous Vector in the Lattice. The position of the next Vector in the
// Lattice will depend on the tile shape, and is described above. Within a tile
// the Vector cursor will move first through the x-axis and then the y-axis
// (assuming we have a cursor oriented along the z-axis). In general the lower
// dimensions will be exhausted (within a tile) before moving the cursor
// through higher dimensions. This intra-tile behaviour for cursor movement
// extends to the inter-tile movement of the cursor between tiles. 
// </synopsis> 

// <example>
// This example is of a global function that will do a 2-D inplace
// complex Fourier transform of an arbitrary large Lattice (which
// must have at least two dimensions).
//
// A two dimensional transform is done by successive one dimensional
// transforms along all the rows and then all the columns in the
// lattice. Scoping is used to destroy iterators once they have been
// used. This frees up the cache memory associated with the cursor in each
// iterator.
//
// <srcblock>
// void FFT2DComplex (Lattice<Complex>& cArray,
// 		      const Bool direction)
// {
//   const uInt ndim = cArray.ndim();
//   AlwaysAssert(ndim > 1, AipsError);
//   const IPosition latticeShape = cArray.shape();
//   const uInt nx=latticeShape(0);
//   const uInt ny=latticeShape(1);
//   const IPosition tileShape = cArray.niceCursorShape();
//
//   {
//     TiledLineStepper tsx(latticeShape, tileShape, 0);
//     LatticeIterator<Complex> lix(cArray, tsx);
//     FFTServer<Float,Complex> fftx(IPosition(1, nx));
//     for (lix.reset();!lix.atEnd();lix++) {
//       fftx.fft(lix.rwVectorCursor(), direction);
//     }
//   }
//   {
//     TiledLineStepper tsy(latticeShape, tileShape, 1);
//     LatticeIterator<Complex> liy(cArray, tsy);
//     FFTServer<Float,Complex> ffty(IPosition(1, ny));
//     for (liy.reset();!liy.atEnd();liy++) {
//       ffty.fft(liy.rwVectorCursor(), direction);
//     }
//   }
// }
// </srcblock>
// </example>

// <motivation>
// Moving through a Lattice by equal sized chunks, and without regard
// to the nature of the data, is a basic and common procedure.  
// </motivation>

// <todo asof="1997/03/28">
//  <li> Support for Matrix and higher dimensional cursors can be used.
// </todo>


class TiledLineStepper : public LatticeNavigator
{
public:

  // Construct a TiledLineStepper by specifying the Lattice shape,
  // a tile shape and the axis along which the Vector cursor will lie
  // (0 means the x-axis). Is is nearly always advisable to make the
  // tileShape identical to the Lattice tileShape. This can be obtained by
  // <src>lat.niceCursorShape(lat.advisedMaxPixels())</src>
  // where <src>lat</src> is a Lattice object.
  TiledLineStepper (const IPosition& latticeShape, 
		    const IPosition& tileShape, 
		    const uInt axis);

  // The copy constructor uses copy semantics.
  TiledLineStepper (const TiledLineStepper& other);
    
  ~TiledLineStepper();

  // The assignment operator uses copy semantics.
  TiledLineStepper& operator= (const TiledLineStepper& other);

  // Increment operator (postfix or prefix version) - move the cursor
  // forward one step. Returns True if the cursor was moved.
  virtual Bool operator++(int);

  // Decrement operator (postfix or prefix version) - move the cursor
  // backwards one step. Returns True if the cursor was moved.
  virtual Bool operator--(int);

  // Function to move the cursor to the beginning of the Lattice. Also
  // resets the number of steps (<src>nsteps</src> function) to zero. 
  virtual void reset();

  // Function which returns "True" if the cursor is at the beginning of the
  // Lattice, otherwise, returns "False"
  virtual Bool atStart() const;

  // Function which returns "True" if an attempt has been made to increment
  // the cursor beyond the end of the Lattice.
  virtual Bool atEnd() const;

  // Function to return the number of steps (increments & decrements) taken
  // since construction (or since last reset).  This is a running count of
  // all cursor movement (operator++ or operator--), even though
  // N-increments followed by N-decrements will always leave the cursor in
  // the original position.
  virtual uInt nsteps() const;

  // Function which returns the current position of the beginning of the
  // cursor. The <src>position</src> function is relative to the origin
  // in the main Lattice.
  // <group>
  virtual IPosition position() const;
  // </group>

  // Function which returns the current position of the end of the
  // cursor. The <src>endPosition</src> function is relative to the origin
  // in the main Lattice.
  // <group>
  virtual IPosition endPosition() const;
  // </group>

  // Functions which returns the shape of the Lattice being iterated
  // through. <src>latticeShape</src> always returns the shape of the main
  // Lattice while <src>subLatticeShape</src> returns the shape of any
  // sub-Lattice defined using the <src>subSection</src> function. 
  // <group>
  virtual IPosition latticeShape() const;
  virtual IPosition subLatticeShape() const;
  // </group>

  // Function which returns the shape of the cursor. This always includes
  // all axes (ie. it includes degenerates axes)
  virtual IPosition cursorShape() const;

  // Function which returns the axes of the cursor.
  virtual IPosition cursorAxes() const;

  // Function which returns the shape of the "tile" the cursor will iterate
  // through before moving onto the next tile. THIS IS NOT THE SAME AS THE
  // TILE SHAPE USED BY THE LATTICE. It is nearly the same except that the
  // axis the cursor is aligned with is replaced by the shape of the Lattice
  // on that axis. eg., If a Lattice has a shape of [512,512,4,32] and a
  // tile shape of [32,16,4,16] then <src>tileShape()</src> will return
  // [512,16,4,16] if the cursor is along the x-axis and [32,512,4,16] if the
  // cursor is along the y-axis.
  IPosition tileShape() const;

  // Function which returns "True" if the increment/decrement operators have
  // moved the cursor position such that part of the cursor beginning or end
  // is hanging over the edge of the Lattice. This always returns False.
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
  // See <linkto class=LatticeStepper>LatticeStepper</linkto> for a
  // description and examples.
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
  TiledLineStepper();


  IPosition itsBlc;              //# Bottom Left Corner
  IPosition itsTrc;              //# Top Right Corner
  IPosition itsInc;              //# Increment
  LatticeIndexer itsSubSection;  //# The current subsection
  LatticeIndexer itsIndexer;     //# For moving within a tile
  LatticeIndexer itsTiler;       //# For moving between tiles
  IPosition itsIndexerCursorPos; //# The current position of the iterator.
  IPosition itsTilerCursorPos;   //# The current position of the iterator.
  IPosition itsCursorShape;      //# The shape of the cursor for itsIndexer
  IPosition itsTileShape;        //# The tile shape (= itsTiler cursor shape)
  IPosition itsAxisPath;         //# Path for traversing
  uInt itsNsteps;                //# The number of iterator steps taken so far; 
  uInt itsAxis;                  //# The axis containing the data vector
  Bool itsEnd;                   //# Is the cursor beyond the end?
  Bool itsStart;                 //# Is the cursor at the beginning?
};



} //# NAMESPACE CASACORE - END

#endif
