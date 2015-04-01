//# LatticeIndexer.h: A helper class for stepping through Lattices
//# Copyright (C) 1994,1995,1996,1997,1998,1999
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

#ifndef LATTICES_LATTICEINDEXER_H
#define LATTICES_LATTICEINDEXER_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/IPosition.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A helper class for stepping through Lattices.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tLatticeIndexer">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Lattice"> Lattice </linkto>
//   <li> <linkto class="IPosition"> IPosition </linkto>
// </prerequisite>

// <etymology>
// This class does various calculations involved with indexing in
// Lattices. LatticeIndexer is not a good name, but it is
// better than the previous name of LatticeLayout.
// </etymology>

// <synopsis> 
// A LatticeIndexer contains all the information necessary to define the
// shape of a Lattice or sub-Lattice. It is currently a repository of
// functions that provide indexing calculations.
// <p>
// A sub-Lattice is a section of a Lattice defined by a bottom left corner
// (blc), a top right corner (trc), and a step size or increment on each
// axis. The blc and trc pixels will always be included in the sub-Lattice
// if the step increment is one. If the step increment is greater than one,
// the pixel in top right corner may not be included in the sub-Lattice. 
// <p>
// This class knows the shape of the parent Lattice (including all
// degenerate axes), and allows the user to specify a sub-Lattice that is
// embedded in the parent Lattice. The default sub-Lattice, if none is
// specified, is one identical in shape to the main Lattice. 
// <p>
// A sub-Lattice can be defined on the Lattice by specifying a trc, blc,
// and step increment using the <src>subSection</src> function, or the
// appropriate constructor. A sub-Lattice must be smaller than (or the same
// size as) the Lattice that it is derived from. A sub-Lattice can be further
// created from an already existing sub-Lattice eg.
// <br>
// If we have a 128 by 128 Lattice, we can specify the centre quarter by
// using blc=[32,32] and trc=[95,95]. Then specifying a sub-Lattice of
// blc=[0,0] and trc = [31,31] results in a sub-Lattice that has a blc
// of [32,32] and trc of [63,63] with respect to the parent Lattice.
// <p>
// The only way to increase the size of a sub-Lattice is to first revert to
// the parent Lattice (using the <src>fullSize</src> function) and then
// generate the new, bigger sub-Lattice.
// <p>
// Indexing calculations (eg. the <src>tiledCursorMove</src> or the
// <src>isInside</src> function) are performed on the specified sub-Lattice.
// <p>
// The role of this class is to centralise the information and functions
// needed to operate on sub-Lattices. It will normally be used by other
// Lattice classes, and is currently used by navigator classes like 
// <linkto class="LatticeStepper">LatticeStepper</linkto>.
// </synopsis>

// <motivation>
// The shape, structure or geometry of a lattice is quite separable from
// its actual contents, and the operations you can do on the contents.  Also,
// there are operations which apply only to the layout such as subsectioning. 
// </motivation>

//# <todo asof="1997/01/12">
//# </todo>


class LatticeIndexer
{
public:
  // Default constructor (one dimensional, unit-length instance).
  LatticeIndexer();

  // Specify the size of the Lattice. Assume a full size sub-Lattice. 
  explicit LatticeIndexer (const IPosition& shape);

  // Specify a Lattice and define a sub-Lattice within it.
  LatticeIndexer (const IPosition& shape, const IPosition& blc,
		  const IPosition& trc, const IPosition& inc);
  
  // The copy constructor uses copy semantics. 
  LatticeIndexer (const LatticeIndexer& other);

  ~LatticeIndexer();

  // The assignment operator uses copy semantics.
  LatticeIndexer& operator= (const LatticeIndexer& other);

  // Function to change the shape of the Lattice. Resets the sub-Lattice to
  // fullsize.
  void resize (const IPosition& newShape);

  // Returns the length of each axis (or the requested one) in the parent
  // Lattice.
  // <group>
  const IPosition& fullShape() const;
  uInt fullShape (uInt axis) const;
  // </group>

  // Returns the length of each axis (or the requested one) in the sub-Lattice.
  // <group>
  const IPosition& shape() const;
  uInt shape (uInt axis) const;
  // </group>

  // Function to return the increments along each axis (or the requested
  // one) of the Lattice.
  // <group>
  const IPosition& increment() const;
  uInt increment (uInt axis) const;
  // </group>

  // Function to return the offset (on a specified axis) between the
  // sub-Lattice and the parent one.
  // <group>
  const IPosition& offset() const;
  uInt offset (uInt axis) const;
  // </group>

  // Function which returns the number of dimensions in the Lattice (or
  // sub-Lattice).
  uInt ndim() const;

  // Revert from a sub-Lattice description back to the main Lattice. This is
  // the only way to "increase" the the size of the sub-Lattice used by the
  // LatticeIndexer.
  void fullSize();

  // Function which returns the number of elements in the sub-Lattice; 
  // this value is equal to the product of shape().
  size_t nelements() const;

  // Function which increments (incr=True) or decrements (incr=False) the
  // cursor position (the first IPosition argument) by a cursor shape (the
  // second IPosition argument), tiling to the next/previous axis if
  // necessary.  The path of movement is based upon the third IPosition
  // argument (a cursor heading) that is zero-based e.g. IPosition(3,0,2,1)
  // implies starting movement along the x-axis, then the z-axis, and then
  // the y-axis.  Returns a value of False if the beginning/end of the
  // sub-Lattice is reached. The cursorPosition is relative to the origin of
  // the sub-Lattice. To get its location relative to the main Lattice use
  // the absolutePosition() function. 
  Bool tiledCursorMove (Bool incr, IPosition& cursorPos, 
			const IPosition& cursorShape,
			const IPosition& cursorHeading) const;
  
  // Function which returns a value of True if the IPosition argument
  // is within the sub-Lattice.  Returns False if the IPosition argument is 
  // outside the sub-Lattice or if the argument doesn't conform to the 
  // data members.
  // <note role=warning> Due to zero-origins, an index argument equal to the
  // shape of this sub-Lattice lies outside and returns False. 
  // </note>
  Bool isInside (const IPosition& index) const;

  // Function which subsections a LatticeIndexer.  The argument IPositions
  // specify "bottom left" and "upper right" corners and axis increments
  // (which default to one).  The origins are cumulative. i.e. specifying a
  // blc of (2,2), and then (1,1) results in the sub-Lattice having an
  // origin at pixel (3,3) in the parent Lattice. Similarly the increment is
  // cumulative, i.e. an increment of 2 on top of an increment of 3 results
  // in a total increment of 6. This function can only decrease the size of
  // the sub-Lattice (i.e. blc >= 0, and trc <= shape(), and inc >= 1). The
  // fullSize() function should be used to revert back to the maximum
  // possible Lattice size.  Also note that the trc might not be used if an
  // integral number of increments does not end on the trc (in which case
  // the last position below the trc will be used).
  // <group>
  void subSection (const IPosition& blc, const IPosition& trc,
		   const IPosition& inc);
  void subSection (const IPosition& blc, const IPosition& trc);
  // </group>

  // Function which returns an IPosition in the parent Lattice given an
  // IPostion in the sub-Lattice.  Accounting is taken of any offsets and
  // increments caused by subSectioning. No checks are made to ensure the
  // supplied IPosition or the returned one are within the bounds of the
  // Lattice(s).
  IPosition absolutePosition (const IPosition& position) const;

  //# function which returns True if all the elements in this 
  //# LatticeIndexer, or LatticeIndexer subsection, are arranged contiguously, 
  //# i.e. without any gaps caused by increments or subSectioning.
//#   Bool isContiguous() const;

  // Is this LatticeIndexer consistent, i.e. are the class invariants valid?
  // Returns True if every thing is fine otherwise returns False
  Bool ok() const;

private:
  IPosition itsFullShape;  //# Size of the main-Lattice.
  uInt      itsNdim;       //# Number of dimensions in the main/sub-Lattice
  IPosition itsShape;      //# Shape of the sub-Lattice
  IPosition itsAxisInc;    //# Increment along each axis of main Lattice
  IPosition itsOffset;     //# Offset between a sub-Lattice and the main one.
};


inline const IPosition& LatticeIndexer::fullShape() const
{
  return itsFullShape;
}
inline const IPosition& LatticeIndexer::shape() const
{
  return itsShape;
}
inline const IPosition& LatticeIndexer::increment() const
{
  return itsAxisInc;
}
inline const IPosition& LatticeIndexer::offset() const
{
  return itsOffset;
}
inline uInt LatticeIndexer::ndim() const
{
  return itsNdim;
}
inline size_t LatticeIndexer::nelements() const
{
  return itsShape.product();
}




} //# NAMESPACE CASACORE - END

#endif
