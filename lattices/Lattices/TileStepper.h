//# TileStepper.h:  Steps a cursor optimally through a tiled Lattice
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

#ifndef LATTICES_TILESTEPPER_H
#define LATTICES_TILESTEPPER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/LatticeIndexer.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// traverse a tiled Lattice optimally with a tile cursor
// </summary>

// <use visibility=export>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tTileStepper.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeNavigator> LatticeNavigator </linkto>
// </prerequisite>

// <etymology>
// TileStepper is used to step optimally through a tiled Lattice.
// </etymology>

// <synopsis> 
// When you wish to traverse a Lattice (say, a PagedArray or an Image) you
// will usually create a LatticeIterator.  Once created, you may attach a
// LatticeNavigator to the iterator. A TileStepper is a concrete class
// derived from the abstract LatticeNavigator that allows you to step
// through the Lattice in a way that will minimize the amount of cache
// memory consumed and maximize the speed.
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
// TileStepper steps through a lattice in a tile-by-tile way.
// This means that the cache contains 1 tile only and that a tile is
// accessed only once.
// It should be clear that traversing a lattice in this way cannot
// be used if an entire vector or plane is needed. It is, however, very
// well suited for purposes like initialising a lattice, where the
// order in which the lattice pixels are accessed is not important.
// <p>
// In constructing a TileStepper, you specify the Lattice shape, the
// tile shape and optionally the axis path. The axis path defines the order
// in which the tiles are fetched from the lattice. Default is the natural
// order (thus x-axis in the inner loop).
// <br>It is possible to use the function <src>subSection</src> to
// traverse only a subsection of the lattice.
// <p>
// The cursor position can be incremented or decremented to retrieve the next
// or previous tile in the Lattice. The position of the next tile in the
// Lattice will depend on the tile shape, and is described above. 
// <br>Note that the cursor shape does not need to be constant when iterating
// through the lattice. If the lattice shape is not an integer multiple of
// the tile shape, the cursor will be smaller on the edges of the lattice.
// </synopsis> 

// <example>
// This example initializes a lattice with the given value.
// <srcblock>
// void init (Lattice<Complex>& cArray, Complex value)
// {
//   const IPosition latticeShape = cArray.shape();
//   const IPosition tileShape = cArray.niceCursorShape();
//   TileStepper tsx(latticeShape, tileShape);
//   LatticeIterator<Complex> lix(cArray, tsx);
//   for (lix.reset();!lix.atEnd();lix++)
//     lix.woCursor() = value;
//   }
// }
// </srcblock>
// Note that a TileStepper is the default navigator for an iterator.
// So the code above could be made simpler like shown below.
// Also note that this example is a bit artificial, because the Lattice::set()
// function should be used to initialize a lattice.
// <srcblock>
// void init (Lattice<Complex>& cArray, Complex value)
// {
//   LatticeIterator<Complex> lix(cArray);
//   for (lix.reset();!lix.atEnd();lix++)
//     lix.woCursor() = value;
//   }
// }
// </srcblock>
// </example>

// <motivation>
// This class makes it possible to traverse a lattice in the optimal way.
// </motivation>
//
//# <todo asof="1997/11/21">
//#  <li>
//# </todo>


class TileStepper: public LatticeNavigator
{
public:

  // Construct a TileStepper by specifying the Lattice shape, a tile shape,
  // and an optional axis path (default is natural order).
  // Is is nearly always advisable to make the tileShape identical
  // to the Lattice tileShape. This can be obtained by
  // <src>lat.niceCursorShape()</src> where <src>lat</src> is
  // a Lattice object.
  // <group>
  TileStepper (const IPosition& latticeShape, 
	       const IPosition& tileShape);
  TileStepper (const IPosition& latticeShape, 
	       const IPosition& tileShape,
	       const IPosition& axisPath);
  // </group>

  // Copy constructor (copy semantics).
  TileStepper (const TileStepper& other);
    
  ~TileStepper();

  // Assignment (copy semantics).
  TileStepper& operator= (const TileStepper& other);

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
  virtual IPosition position() const;

  // Function which returns the current position of the end of the
  // cursor. The <src>endPosition</src> function is relative the origin
  // in the main Lattice.
  virtual IPosition endPosition() const;

  // Functions which return the shape of the Lattice being iterated
  // through. <src>latticeShape</src> always returns the shape of the main
  // Lattice while <src>subLatticeShape</src> returns the shape of any
  // sub-Lattice defined using the <src>subSection</src> function. 
  // <group>
  virtual IPosition latticeShape() const;
  virtual IPosition subLatticeShape() const;
  // </group>

  // Function which returns the shape of the cursor. This always includes
  // all axes (i.e. it includes degenerates axes)
  virtual IPosition cursorShape() const;

  // Function which returns the axes of the cursor.
  virtual IPosition cursorAxes() const;

  // Function which returns the shape of the "tile" the cursor will iterate
  // through before moving onto the next tile.
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
  TileStepper();


  IPosition itsBlc;              //# Bottom Left Corner
  IPosition itsTrc;              //# Top Right Corner
  IPosition itsInc;              //# Increment
  LatticeIndexer itsSubSection;  //# The current subsection
  LatticeIndexer itsTiler;       //# For moving between tiles
  IPosition itsTilerCursorPos;   //# The current position of the iterator
  IPosition itsTileShape;        //# The tile shape (= itsTiler cursor shape)
  IPosition itsAxisPath;         //# Path for traversing
  IPosition itsCurBlc;           //# Blc of the current position.
  IPosition itsCurTrc;           //# Trc of the current position.
  uInt itsNsteps;                //# The number of iterator steps taken so far
  Bool itsEnd;                   //# Is the cursor beyond the end?
  Bool itsStart;                 //# Is the cursor at the beginning?
};



} //# NAMESPACE CASACORE - END

#endif
