//# LatticeNavigator.h: Abstract base class to steer lattice iterators
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

#ifndef LATTICES_LATTICENAVIGATOR_H
#define LATTICES_LATTICENAVIGATOR_H


//# Includes
#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class IPosition;
class ROTiledStManAccessor;


// <summary>
// Abstract base class to steer lattice iterators.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" tests="tLatticeStepper.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LatticeIterator>LatticeIterator</linkto>
//   <li> <linkto class=Lattice>Lattice</linkto>
// </prerequisite>

// <etymology>
// Lattice iteration can proceed with a number of different strategies -
// all of which answer the question:  where do I go from here?
// You could travel through by making calculations on the lattice subscripts,
// viewing ascending planes in an image cube, for example, or you could 
// travel through by making calculations on the data, viewing small 
// subimage planes in order of descending brightness over the whole cube.  
// Concrete classes derived  from this base class implement different
// navigation strategies - but they are all "navigators".
// </etymology>

// <synopsis> 
// This abstract base class defines the interface for objects which generate
// positions for LatticeIterators. This position is not just a single point
// in the Lattice but a region or "cursor" that is moved through the
// Lattice. The LatticeIterator classes actually retrieve the data in the
// cursor from the Lattice. The class decribed here (and those derived from it)
// are responsible for moving the cursor to the next position and determining
// its shape.
//
// There may eventually be a large collection of tools for traversing
// Lattices.  At this writing (December 1999) there are three concrete
// classes derived from LatticeNavigator: 
// <linkto class="LatticeStepper">LatticeStepper</linkto>,
// <linkto class="TiledLineStepper">TiledLineStepper</linkto>, and
// <linkto class="TileStepper">TileStepper</linkto>. 
//
// The <src>LatticeStepper</src> class moves through a Lattice in fixed
// steps defined by the user specified cursor, incrementing to the next
// portion of the Lattice with each step, and wrapping around axes as
// needed.  Other position generators might follow the brightest pixel,
// traverse a number of predefined subregions, or change size automatically
// when near the edges.
//
// The <src>TiledLineStepper</src> class moves a Vector cursor through a
// Lattice, until all the lines in the set of tiles along the specified
// axis have been exhausted. It then moves to the next set of tiles. This is
// a memory-efficient way to move a Vector cursor through a Lattice.
//
// The most important member functions of this class are those which move
// the cursor to the next position. These are the <src>operator++</src> and
// <src>operator--</src> member functions, (in postfix and prefix forms). 
//
// The cursor shape need not be constant as it moves through the Lattice,
// but may change depending on its current position. For the LatticeStepper
// and TiledLineStepper classes , however, the cursor shape is constant
// as it steps through the Lattice.
//
// It is not possible to randomly move the cursor to an arbitrary place in
// the Lattice, although the cursor can be moved to the starting position at
// any time using the <src>reset</src> member function.
//
// The position of the cursor can be queried at any time using the
// <src>position</src> member function. This gives the position of the
// bottom left hand corner of the cursor. The position of the top right hand
// corner of the cursor is obtained using the <src>endPosition</src> member
// function, and the current cursor shape is obtained using the
// <src>cursorShape</src> member function. Note that the endPosition
// does not take an overhang into account.
//
// It is possible that for some positions of the cursor, part of it will
// "hang over" the edge of the Lattice. When this occurs the
// <src>hangOver</src> member function will return True. This will occur
// with a LatticeStepper if the Lattice shape is not a multiple of the
// cursor shape. Hangover cannot occur with the TiledLineStepper as the length
// of the Vector cursor is defined by the Lattice Shape.
//
// It may be possible (depending on the concrete LatticeNavigator actually
// used) to specify that only a region of the Lattice (defined by a top
// right hand corner, bottom left hand corner, and step increment) be
// traversed by the LatticeNavigator. This is done using the
// <src>subSection</src> member function. At any time the region can be
// redefined by calling the <src>subSection</src> function again. This
// replaces the previously defined region with the new one.
//
// Using the subSection function always sets the cursor position to the
// origin of the currently defined sub-lattice. This is a backdoor way to
// move the cursor to random locations in the Lattice.
//
// It is an error to define a sub-lattice that is bigger than the current
// Lattice. If using a LatticeStepper it may also be necessary to resize the
// cursor (using the <src>setCursorShape</src> member function) prior to
// calling the subSection function as the cursor cannot be bigger than the
// sub-Lattice on any axis.
//
// The arguments (<src>trc</src>, <src>blc</src> and <src>inc</src>)
// to the <src>subSection</src> function are always
// relative to the main Lattice. This is also true of the <src>position</src>
// and <src>endPosition</src> functions. To get the position of the cursor
// relative to the currently defined sub-Lattice use the
// <src>relativePosition</src> and <src>relativeEndPosition</src> member
// functions.
//
// Many of the LatticeIterator member functions are directly forwarded to
// virtual functions of this class, and classes derived from it. For
// instance, LatticeIterator<T>::operator++() calls
// LatticeIterInterface->operator++() which calls
// LatticeNavigator->operator++() which might resolve to
// LatticeStepper->operator++(). Other functions like this are documented in
// the <linkto class="LatticeIterator">LatticeIterator</linkto> class.
// </synopsis> 

// <example>
// See the examples in the 
// <linkto class="LatticeStepper">LatticeStepper</linkto> class, the 
// <linkto class="TiledLineStepper">TiledLineStepper</linkto> class, and the
// <linkto class="TileStepper">TileStepper</linkto> class.
// </example>
//
// <motivation>
// Iterator classes are quite common in C++.  What's novel about the design
// which includes this class is the separation of iterator mechanisms from
// traversal strategy.  The iterator provides a lot of functionality: it
// provides a cursor, damage notification and tracking, and reading and
// writing to the underlying data structure.  Traversal strategies can and
// should be isolated from these things. Because every LatticeIterator
// uses a Navigator, it gets the benefits of a derived concrete navigator
// without getting involved in its mechanism.
// </motivation>
//
// <todo asof="1997/31/01">
//  <li> Think about how to implement Navigators which can traverse
//  arbitrary shaped regions.
// </todo>


class LatticeNavigator {
public:
  // Default constructor.
  LatticeNavigator()
    {;}

  // Copy constructor.
  LatticeNavigator (const LatticeNavigator&)
    {;}

  // Assignment.
  LatticeNavigator& operator= (const LatticeNavigator&)
    { return *this; }

  // A virtual destructor.  A virtual is needed to ensure that derived
  // classes accessed through pointers to a LatticeNavigator will scope
  // their destructor to the derived class destructor.
  virtual ~LatticeNavigator();

  // Increment operator - increment the cursor to the next position. The
  // implementation of the prefix operator calls the postfix one.
  // <group>
  virtual Bool operator++(int) = 0;
  Bool operator++();
  // </group>

  // Decrement operator - decrement the cursor to the previous position. The
  // implementation of the prefix operator calls the postfix one.
  // <group>
  virtual Bool operator--(int) = 0;
  Bool operator--();
  // </group>

  // Function to reset the cursor to the beginning of the Lattice and
  // reset the number of steps taken to zero.
  virtual void reset() = 0;

  // Function which returns "True" if the cursor is at the beginning of the
  // Lattice, otherwise, returns "False"
  virtual Bool atStart() const = 0;

  // Function which returns "True" if an attempt has been made to increment
  // the cursor beyond the end of the Lattice.
  virtual Bool atEnd() const = 0;

  // Function to return the number of steps (increments or decrements) taken
  // since construction (or since last reset).  This is a running count of
  // all cursor movement since doing N increments followed by N decrements
  // does not necessarily put the cursor back at the origin of the Lattice.
  virtual uInt nsteps() const = 0;

  // Functions which return the current position of the beginning of the
  // cursor. The <src>position</src> function is relative to the origin in
  // the main Lattice and the <src>relativePosition</src> function is
  // relative to the origin and increment used in the sub-Lattice (defined
  // using the <src>subSection</src> function).
  // The returned IPosition will have the same number of axes as
  // the underlying Lattice.
  // <br>The default implementation of the <src>relativePosition</src>
  // function returns <src>(position() - blc()) / increment()</src>.
  // <group>
  virtual IPosition position() const = 0;
  virtual IPosition relativePosition() const;
  // </group>

  // Functions which return the current position of the end of the
  // cursor. The <src>endPosition</src> function is relative to the origin in
  // the main Lattice and the <src>relativeEndPosition</src> function is
  // relative to the origin and increment used in the sub-Lattice (defined
  // using the <src>subSection</src> function).
  // The returned IPosition will have the same number of axes as
  // the underlying Lattice.
  // <note role=caution> It returns the end position in the lattice and
  // does not take overhang into account. </note>
  // <br>The default implementation of the <src>relativeEndPosition</src>
  // function returns <src>(endPosition() - blc()) / increment()</src>.
  // <group>
  virtual IPosition endPosition() const = 0;
  virtual IPosition relativeEndPosition() const;
  // </group>
  
  // Functions which return the shape of the Lattice being iterated
  // through. <src>latticeShape</src> always returns the shape of the main
  // Lattice while <src>subLatticeShape</src> returns the shape of any
  // sub-Lattice defined using the <src>subSection</src> function.  In the
  // default implementation of this class it is not possible to use the
  // <src>subsection</src> function (it throws an exception) so the default
  // implementation of the <src>subLatticeShape</src> function calls the
  // <src>latticeShape</src> function.  The returned IPosition will always
  // have the same number of axes as the underlying Lattice.
  // <group>
  virtual IPosition latticeShape() const = 0;
  virtual IPosition subLatticeShape() const;
  // </group>

  // Function which returns the current shape of the cursor which is
  // iterating through the Lattice.  The returned IPosition will have the
  // same number of axes as the underlying Lattice.
  virtual IPosition cursorShape() const = 0;  

  // Function which returns the axes of the cursor.
  // These are the axes which should not be removed by the
  // iterator functions <src>vectorCursor()</src>, etc..
  virtual IPosition cursorAxes() const = 0;

  // Function which returns "True" if the increment/decrement operators have
  // moved the cursor position such that part of the cursor is hanging over
  // the edge of the Lattice. This function may always return a value of
  // "False" for some iteration methods that do not move the cursor past the
  // Lattice boundaries.
  virtual Bool hangOver() const = 0;

  // Functions which return the "bottom left corner" and the "top right corner"
  // of the cursor that does not hangover. Use these functions to extract the
  // valid part of the cursor when the hangover member function is true. If
  // there is no hangover then hangOverBLC returns an IPosition of zero and
  // hangOverTRC() returns the cursorShape - 1;
  // <group>
  virtual IPosition hangOverBlc() const;
  virtual IPosition hangOverTrc() const;
  // </group>


  // Function to specify a "section" of the Lattice to Navigate over. A
  // section is defined in terms of the Bottom Left Corner (blc), Top Right
  // Corner (trc), and step size (inc), on ALL of its axes, including
  // degenerate axes. The step size defaults to one if not specified.  
  // In the default implementation of this class subsectioning is not
  // supported and using the <src>subsection</src> function will throw an
  // exception (AipsError).
  // <group>
  virtual void subSection(const IPosition& blc, const IPosition& trc);
  virtual void subSection(const IPosition& blc, const IPosition& trc, 
			  const IPosition& inc);
  // </group>

  // Return the bottom left hand corner (blc), top right corner (trc) or
  // step size (increment) used by the current sub-Lattice. In the default
  // implementation of this class sub-sectioning is not supported and these
  // functions will always return blc=0, trc=latticeShape-1, increment=1,
  // ie. the entire Lattice.
  // <group>
  virtual IPosition blc() const;
  virtual IPosition trc() const;
  virtual IPosition increment() const;
  // </group>

  // Return the axis path.
  // See <linkto class=LatticeStepper>LatticeStepper</linkto> for a
  // description and examples.
  virtual const IPosition& axisPath() const = 0;

  // Calculate the cache size (in tiles) for this type of access to a lattice
  // in the given row of the tiled hypercube.
  // A zero bucket size indicates that the data are not tiled, but in memory.
  // Then a cache size of 0 is returned.
  virtual uInt calcCacheSize (const IPosition& cubeShape,
                              const IPosition& tileShape,
                              uInt maxCacheSize, uInt bucketSize) const = 0;

  // Function which returns a pointer to dynamic memory of an exact copy 
  // of this LatticeNavigator. It is the responsibility of the caller to
  // release this memory. 
  virtual LatticeNavigator* clone() const = 0;

  // Function which checks the internals of the class for consistency.
  // Returns True if everything is fine otherwise returns False. The default
  // implementation always returns True.
  virtual Bool ok() const;
};


inline Bool LatticeNavigator::operator++()
{
  return operator++(0);
}
inline Bool LatticeNavigator::operator--()
{
  return operator--(0);
}



} //# NAMESPACE CASACORE - END

#endif
