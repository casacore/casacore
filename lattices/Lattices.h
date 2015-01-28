//# Lattices.h: Regular N-dimensional data structures.
//# Copyright (C) 1996,1997,1998,1999,2003
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

#ifndef LATTICES_LATTICES_H
#define LATTICES_LATTICES_H


//#include <casacore/casa/Arrays/ArrayLattice.h>
//#include <casacore/casa/Arrays/PagedArray.h>
//#include <casacore/casa/Arrays/TempLattice.h>
//#include <casacore/casa/Arrays/LatticeLocker.h>
//#include <casacore/casa/Arrays/TiledShape.h>

//#include <casacore/casa/Arrays/LatticeApply.h>
//#include <casacore/casa/Arrays/LatticeIterator.h>
//#include <casacore/casa/Arrays/LatticeStepper.h>
//#include <casacore/casa/Arrays/TileStepper.h>
//#include <casacore/casa/Arrays/TiledLineStepper.h>

//#include <casacore/lattices/Lattices/SubLattice.h>

//#include <casacore/lattices/LRegions.h>
//#include <casacore/lattices/LEL.h>
//#include <casacore/lattices/LatticeMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary>
// Regular N-dimensional data structures.
// </summary>

// <prerequisite>
//   <li> Programmers of new Lattice classes should understand Inheritance
//   <li> Users of the Lattice classes should understand Polymorphism.
//   <li> class <linkto class=IPosition>IPosition</linkto>
//   <li> class <linkto class=Array>Array</linkto>
// </prerequisite>

// <reviewed reviewer="Peter Barnes" date="1999/10/30" demos="">
// </reviewed>

// <etymology>
// Lattice: "A regular, periodic configuration of points, particles, or
// objects, throughout an area of a space..." (American Heritage Directory)
// This definition matches our own: an N-dimensional arrangement of data
// on regular orthogonal axes.
// <p>
// In Casacore, we have used the ability to call many things by one generic
// name (Lattice) to create a number of classes which have different storage
// techniques (e.g. core memory, disk, etc...).  The name Lattice should
// make the user think of a class interface (or member functions) which all
// Lattice objects have in common.  If functions require a Lattice
// argument, the classes described here may be used interchangeably, even
// though their actual internal workings are very different.
// </etymology>

// <synopsis>
// The Lattice module may be broken up into a few areas:
// <ol> 
//
// <li> Lattices - the actual holders of lattice-like data which all share a
// common <linkto class="Lattice">interface</linkto>.  The following items
// are all Lattices and may be used polymorphically wherever a Lattice is
// called for.
//  <ul>
//   <li>The <linkto class="ArrayLattice">ArrayLattice</linkto> class adds
//   the interface requirements of a Lattice to a Casacore 
//   <linkto class="Array">Array</linkto>. The data inside an ArrayLattice
//   are not stored on disk.  This n-dimensional array class is the simplest
//   of the Lattices.  Users construct the ArrayLattice with an argument
//   which is either an IPosition which describes the array shape or a
//   previously instantiated Array object that may already contain data. In
//   the former case, some Lattice operation must be done to fill the data.
//   The ArrayLattice, like all Lattices, may be iterated through with a
//   <linkto class=LatticeIterator>LatticeIterator</linkto> (see below).
//   <br>Iteration can also be done using
//   <linkto class=LatticeApply>LatticeApply</linkto> and some helper
//   classes. It makes it possible to concentrate on the algorithm.
// <srcblock>
// // Make an Array of shape 3x4x5
// 
// Array<Float> simpleArray(IPosition(3,3,4,5));
//
// // fill it with a gradient
//
// for (Int k=0; k<5; k++)
//   for (Int j=0; j<4; j++)
//     for (Int i=0; i<3; i++) 
//       simpleArray(IPosition(3,i,j,k)) = i+j+k;
//
// // use the array to create an ArrayLattice.
//
// ArrayLattice<Float> lattice(simpleArray);
// </srcblock>
//
//   <li>The <linkto class="PagedArray">PagedArray</linkto> class stores its
//   data on disk in the Table format
//   and pages it into random access memory for use.  Paging is
//   used here to describe the process of getting pieces of data small
//   enough to fit into active memory even if the whole data set is much too
//   large.  This class "feels" like an array but may hold very large amounts 
//   of data.  The paging has an added effect: all the data may be made
//   persistent, so it stays around after the application ends.
//   When you use PagedArrays - use 
//   them because you need persistent data and/or paging into large data sets.
//   <br>
//   The persistence is done using a <linkto module="Tables">Table</linkto>,
//   and uses the <linkto module="Tables:TiledStMan">tiled storage
//   manager</linkto>.  This means that accessing the data along any axis is
//   equally efficient (depending on the tile shape used).
//   <br>
//   A PagedArray constructor allows previously created PagedArrays to be
//   recalled from disk.  Much of the time, the PagedArray will be
//   constructed with a <linkto class=TiledShape>TiledShape</linkto>
//   argument which describes the array and tile shape
//   and a Table argument for use as the place of storage.  Then the
//   PagedArray may be filled using any of the access functions of Lattices
//   (like the LatticeIterator.)
//
// <srcblock>
// // Create a PagedArray from a Table already existing on disk.  
//
// PagedArray<Float> lattice(fileName);
//
// // Create a LatticeIterator to access the Lattice in optimal tile
// // shaped chunks.
//
// LatticeIterator<Float> iter(lattice);
//
// // Iterate through and do something simple; here we just 
// // sum up all the values in the Lattice
//
// Float dSum = 0;
// for(iter.reset(); !iter.atEnd(); iter++) {
//   dSum += sum(iter.cursor());
// }
// </srcblock>
//
//   <li>The <linkto class="HDF5Lattice">HDF5Lattice</linkto> class stores its
//   data on disk in <a href="http://www.hdfgroup.org/HDF5">HDF5</a> format.
//   It works in the same way as PagedArray.
//
//  </ul>
//
// <li> <linkto class="LatticeIterator">LatticeIterator</linkto> - the
// object which allows iteration through any Lattice's data. This comes in
// two types: the <src>RO_LatticeIterator</src> which should be used if you
// are not going to change the Lattice's data, and the
// <src>LatticeIterator</src> if you need to change the data in the Lattice.
// <br>Note that iteration can also be done using
// <linkto class=LatticeApply>LatticeApply</linkto> and some helper
// classes. It makes it possible to concentrate on the algorithm.
//  <ul>
//  <li> The <linkto class="RO_LatticeIterator">RO_LatticeIterator</linkto>
//  class name reflects its role as a means of iterating a "Read-Only" array
//  (hereafter refered to as a "cursor") through a Lattice based object,
//  from beginning to end.  Think of a window into the Lattice that moves to
//  a new location when requested.  The Lattice doesn't change but you may
//  see all or part of its data as the cursor "window" moves around.  This
//  class allows optimized read-only iteration through any instance of a
//  class derived from Lattice.   The cursor's shape is defined by the user and
//  moved through the Lattice in an orderly fashion also defined by the user.
//  Since the cursor is "read-only" it can only be used to "get" the data
//  out of the Lattice.  RO_LatticeIterators are constructed with the Lattice
//  to be iterated as the first argument. The optional second constructor 
//  argument is either an IPosition which defines the shape of the cursor 
//  or a <linkto class=LatticeNavigator>LatticeNavigator</linkto> argument.
//  The IPosition argument cause the iterator
//  to move the cursor in a simple pattern; the cursor starts at the Lattice's
//  origin and moves in the direction of the x-axis, then the y-axis, then 
//  the z-axis, etc..  If a LatticeNavigator argument is given, more
//  control over the cursor shape and path are available. If no second
//  argument is given, the optimal
//  <linkto class=TileStepper>TileStepper</linkto> navigator will be used.
// <srcblock>
// // simple route - define a cursor shape that is the xy plane of our
// lattice.
//
// IPosition cursorShape(2, lattice.shape()(0), lattice.shape()(1));
// LatticeIterator<Float> iter(lattice, cursorShape);
// for (iter.reset(); !iter.atEnd(); iter++) {
//   minMax(iter.cursor(), min, max);
// }
// </srcblock>
//
//   <li> The <linkto class="LatticeIterator">LatticeIterator</linkto> class
//   name reflects its role as a means of iterating a read and write cursor
//   through a Lattice based object.  Not only does the cursor allow you to 
//   inspect the Lattice data but you may also change the Lattice via
//   operations on the cursor. This class provides optimized read and write
//   iteration through any class derived from Lattice.  The technique is
//   identical to the RO_LatticeIterator.  But the cursor, in this case, is
//   a reference back to the data in the Lattice.  This means that changes
//   made to the cursor propagate back to the Lattice.  This is especially
//   useful for the PagedArray and PagedImage classes.  These two classes
//   are constructed empty and need iteration to fill in the Lattice data.
// <srcblock>
// // make an empty PagedArray and fill it.   The Table that stores the 
// // PagedArray is deleted when the PagedArray goes out of scope
//
// PagedArray<Float> lattice(IPosition(4,100,200,300,50));
// LatticeIterator<Float> iter(lattice, IPosition(2, 100, 200));
//
// // fill each plane with the "distance" of the iterator from the origin
//
// for(iter.reset();!iter.atEnd(); iter++) {
//    iter.woCursor() = iter.nsteps();
// }
// </srcblock>
//  </ul>
//
// <li> LatticeNavigators - the objects which define the method and path used
// by a LatticeIterator to move the cursor through a Lattice.   Many
// different paths are possible.  We leave it you to choose the
// <linkto class=LatticeNavigator>LatticeNavigator</linkto>
// (method and path) when using a LatticeIterator.
// <ul>
//   <li> The <linkto class="LatticeStepper">LatticeStepper</linkto> class
//   is used to define the steps which the cursor takes during its path
//   through the Lattice.  Every element of the Lattice will be covered,
//   starting at the origin and ending at the "top right corner."  This
//   class provides the information needed by a LatticeIterator to do
//   non-standard movements of the cursor during iteration.  The shape of
//   the cursor is specified by the second IPosition argument of the
//   LatticeStepper.  The order of the axis is important. An IPosition(1,5)
//   is a five element vector along the x-axis.  An IPosition(3,1,1,5) is a
//   five element vector along the z-axis.  The degenerate axes (axes with
//   lengths of one) act as place holders.  The third argument in the
//   LatticeStepper constructor is the "orientation" IPosition.  This
//   describes the order of the axis for the cursor to follow.  Again, we
//   treat the elements, in order, of the IPosition as the designators of
//   the appropriate axis.  The zeroth element indicates which axis is the
//   fastest moving, the first element indicates which axis is the second
//   fastest moving etc. eg. The IPosition(3,2,0,1) says the LatticeIterator
//   should start with the z-axis, next follow the x-axis, and finish with
//   the y-axis.  A single element cursor would thus move through a cube of
//   dimension(x,y,z) from (0,0,0) up the z-axis until reaching the maximum
//   (0,0,z-1) and then start on (1,0,0) and move to (1,0,z-1), etc.
// <srcblock>
// // The shape of our Lattice - a 4 dimensional image of shape (x,y,z,t) -
// // and the shape of the cursor
//
// IPosition latticeShape(image.shape());
// IPosition cursorShape(3, lattticeShape(0), 1, latticeShape(2));
//
// // Define the path the cursor should follow, we list x and z first, even though
// // no iterations will be done along those axes since the cursor is an 
// // integral subshape of the Lattice. The cursor will move along the y-axis
// // and then increment the t-axis.  The construct the Navigator and Iterator
//
// IPosition order(4,0,2,1,3);
// LatticeStepper nav(latticeShape, cursorShape, order);
// LatticeIterator<Float> iter(image, nav);
// </srcblock>
//
// <li> 
//  The <linkto class="TiledLineStepper">TiledLineStepper</linkto> class
//  allows you to iterate through a Lattice with a Vector cursor.
//  However, it steps through the Lattice in an order which is
//  optimum with regard to the I/O of the tiles with which the Lattice is
//  constructed.
//
// <srcblock>
//
// // Set up a TiledLineStepper to return profiles along the specified
// // axis from a PagedArray (not all Lattices have the tileShape member
// // function).  Then create the iterator as well.
// 
// TiledLineStepper nav(lattice.shape(), lattice.tileShape(), axis);
// LatticeIterator<Complex> nav(lattice, nav);
// </srcblock>
//
// <li> 
//  The <linkto class="TileStepper">TileStepper</linkto> class
//  allows you to iterate through a Lattice in the optimum way.
//  It steps through the lattice tile by tile minimizing I/O and memory usage.
//  It is very well suited for pixel based operations.
//  However, its iteration order is such that it cannot be used for
//  a certain subset of pixels (e.g. a vector) is needed.
//  <br>This navigator is the default when no navigator is given when
// constructing a (RO_)LatticeIterator.
//
// </ul>
//
// <li> <linkto class="MaskedLattice">MaskedLattice</linkto> - a
// Lattice with a mask. It is an abstract base class for
// various types of MaskedLattices. A MaskedLattice does not need
// to contain a mask (see e.g. SubLattice below), although the user
// can always ask for the mask. The function <src>isMasked()</src>
// tells if there is really a mask. If not, users could take
// advantage by shortcutting some code for better performance.
// I.e. a function can test if a the MaskedLattice is really masked
// and can take a special route if not.
// Of course, doing that requires more coding, so it should only
// be done where performance is a real issue.
//  <ul>
//  <li> A <linkto class="SubLattice">SubLattice</linkto> represents
//  a rectangular subset of a Lattice. The SubLattice can be a simple
//  box, but it can also be a circle, polygon, etc.
//  In the latter case the SubLattice contains a mask
//  telling which pixels in the bounding box actually belong to the
//  circle or polygon. In the case of a box there is no mask, because
//  there is no need to (because a box is already rectangular).
//  <br> A SubLattice can be constructed from any Lattice and a
//  <linkto class=LatticeRegion>LatticeRegion</linkto> telling which
//  part to take from the Lattice.
//  If the SubLattice is constructed from a <src>const Lattice</src>,
//  the SubLattice is not writable. Otherwise it is writable if the
//  lattice is writable.
//  <p>
//  There is a rich variety of <linkto class=LCRegion>region</linkto>
//  classes which can be used to define a LatticeRegion in pixel coordinates.
//  They are described in module
//  <a href="group__LRegions__module.html">LRegions</a>.
//  
//  <li> Module <a href="group__LEL__module.html">LEL</a> contains classes to
//  form a mathematical expression of lattices. All standard operators, regions,
//  and many, many <linkto class=LatticeExprNode>functions</linkto>
//  can be used in an expression.
//  </ul>
//
// <li> <linkto class=LatticeLocker>LatticeLocker</linkto> 
// can be used to acquire a (user) lock on a lattice.
// The lock can be a read or write lock.
// The destructor releases the lock when needed.
// <br>Lattices on disk can be used (read and write) by multiple processes.
// The Table locking/synchronization mechanism takes care that sharing
// such a lattice is done in an orderly way.
// Usually the default locking mechanism is sufficient.
// LatticeLocker is useful when finer locking control is needed for a
// disk-based lattice.
// 
// <note role=warning> The following are listed for low-level programmers.  
// Lattice users need not understand them.</note>  The Lattice directory
// contains several files relevant only to implementation.
//
// <ul>
//   <li> <linkto class="LatticeBase">LatticeBase</linkto> - a non-templated
//   abstract base class defining the type-independent interface to classes
//   which must act as Lattices do.
//   <li> <linkto class="Lattice">Lattice</linkto> - a templated
//   abstract base class (derived from LatticeBase)
//   defining the interface to classes which must act as Lattices do.
//   The user simply publicly inherits from Lattice and defines the member
//   functions declared as pure abstract in the Lattice header file.
//   <li> The <linkto class="LatticeNavigator">LatticeNavigator</linkto>
//   class name defines the interface used for navigating through a Lattice
//   by iteration.  This class is an abstract base.  Classes derived from
//   this (currently 
//   <linkto class="LatticeStepper">LatticeStepper</linkto>,
//   <linkto class="TiledLineStepper">TiledLineStepper</linkto>, and
//   <linkto class="TileStepper">TileStepper</linkto>) must
//   define the path the iterator cursor follows, the size of the movement
//   of the cursor with each iteration, and the behaviour of that cursor
//   shape as it moves through a Lattice.
//   <li> <linkto class="LatticeIndexer">LatticeIndexer</linkto> - this
//   class contains the currently defined Lattice and sub-Lattice shape. It
//   is used only by navigator classes as it contains
//   member functions for moving a cursor through a defined sub-Lattice.
//   <li> The 
//   <linkto class="LatticeIterInterface">LatticeIterInterface</linkto>
//   class defines the interface for a specific Lattice's iterator.  This
//   class is a base class with a default iterator implementation.
//   Lattice based classes may need to derive an iterator from
//   LatticeIterInterface to optimize for the LatticeIterator
//   internals which impact upon the new Lattice.
//   <li> <linkto class="PagedArrIter">PagedArrIter</linkto> - this class is
//   the PagedArray's optimized method of iterating. This class is a
//   "letter" utilized within the LatticeIterator "envelope" and cannot
//   be instantiated by any user.
//   <li> <linkto class="LCRegion">LCRegion</linkto> - this class is the
//   (abstract) base class for regions in pixel coordinates.
//  </ul>
// </ol>
// </synopsis>

// <motivation>
// Lattices allow the various holders of data to assume a general method 
// of treatment; by making interfaces in terms of the Lattice class,
// the programmer can  polymorphically operate on objects derived  from the
// Lattice class.
// </motivation>

// <todo asof="1998/10/10">
//  <li> Make MaskedIterator class?
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
