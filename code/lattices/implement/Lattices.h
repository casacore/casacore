//# Lattices.h:  a module for the Lattice related classes
//# Copyright (C) 1996,1997
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

#if !defined (AIPS_LATTICES_H)
#define AIPS_LATTICES_H

// #include <trial/Lattices/ArrLatticeIter.h>
// #include <trial/Lattices/ArrayLattice.h>
// #include <trial/Lattices/CopyLattice.h>
// #include <trial/Lattices/Lattice.h>
// #include <trial/Lattices/LatticeIndexer.h>
// #include <trial/Lattices/LatticeIterInterface.h>
// #include <trial/Lattices/LatticeIterator.h>
// #include <trial/Lattices/LatticeNavigator.h>
// #include <trial/Lattices/LatticeStepper.h>
// #include <trial/Lattices/PagedArrIter.h>
// #include <trial/Lattices/PagedArray.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slice.h>
#include <aips/Lattices/Slicer.h>

// <module>
//
// <summary>Regular N-dimensional data structures</summary>
//
// <prerequisite>
//   <li> Programmers of new Lattice classes should understand inheritance
// </prerequisite>
//
// <reviewed reviewer="" date="yyyy/mm/dd" demos="">
// </reviewed>
//
// <etymology>
//
// Lattice: "A regular, periodic configuration of points, particles, or
// objects, throughout an area of a space..." (American Heritage Directory)
// This definition matches our own: an n-dimensional arrangement of stuff,
// on regular orthogonal axes.
//
// In AIPS++, we have used the ability to call many things by one generic
// name (Lattice) to create a number of classes which have different storage
// techniques (e.g. simple vs. on-disk, etc...).  The name Lattice should
// make the user think of class interface (or member functions) which all
// Lattice objects have in common.  If functions require a Lattice
// argument, the classes described here may be used interchangeably, even
// though their actual internal workings are very different.
//
// </etymology>
//
// <synopsis>
// The Lattice module may be broken up into five areas:
// <ol> 
// <li> Orthogonal n-space descriptors - useful when a shape of a Lattice is
// needed or when a sub-region within a Lattice is required.
// <ul>
//   <li> The <linkto class="IPosition">IPosition</linkto> class name is a
//   concatenation of "Integer Position."  IPosition objects are normally
//   used to index into, and define the shapes of, Arrays and Lattices. For
//   example, if you have a 5-dimensional array, you need an IPosition of
//   length 5 to index into the array (or to define its shape, etc.).  It is
//   essentially a vector of integers.  The IPosition vector may point to
//   the "top right corner" of some shape, or it may be an indicator of a
//   specific position in n-space.  The interpretation is context dependent.
//   The constructor consists of an intial argument which specifies the
//   number of axes, followed by the appropriate number of respective axis
//   lengths.  IPositions have the standard integer math relationships
//   defined, but the dimensionality of the operator arguments must be the
//   same.
//<srcblock>
// // Make a shape with three axes, x = 24, y = 48, z = 16;
// IPosition threeSpace(3, 24, 48, 16);
// // get the value of the ith axis (note: C++ is zero based!)
// Int xAxis = threeSpace(0);
// Int zAxis = threeSpace(2);
// // construct another with all axis values equal to 666;
// IPosition threeSpaceAlso(3,666);
// // do math with the IPositions...
// threeSpace += threeSpaceAlso;
// AlwaysAssert(threeSpace(1) == 714, AipsError);
// </srcblock>
//   <li> The <linkto class="Slicer">Slicer</linkto> class name may be
//   thought of as a short form of "n-Dimensional Slice Specifier."  Some
//   confusion is possible between class "Slice" and this class. We plan to
//   absorb Slice into Slicer in the future, eliminating that
//   confusion.  This object is used to bundle into one place all the
//   information necessary to specify a regular subregion within an Array or
//   Lattice.  In other words, Slicer holds the location of a "slice" of a
//   greater whole.  Construction is with 3 IPositions: the location of the
//   subspace within the greater space; the shape or end location of the
//   subspace within the greater space; and the stride, or multipilier to be
//   used for each axis.  The stride gives the user the chance to use every
//   ith piece of data, rather than every position on the axis.
// <srcblock>
// // Define the shape of an array.
// IPosition shape(2,20,30);
// // Also define an origin.
// IPosition origin(2,-5,15);
// // Now define some Slicer's, initially only specify the blc
// Slicer ns0(IPosition(2,0,24));
// // make some IPositions as holders for the rest of the information
// IPosition blc,trc,inc;
// // Use the shape and origin to fill our holders assuming we want to use
// // as much of the Array as possible. // 
// ns0.inferShapeFromSource (shape, origin, blc,trc,inc);
// // print out the new info ie. blc=[-5,15],trc=[14,44],inc=[1,1]
// cout << blc << trc << inc << endl;
// // Build a slicer with temporaries for arguments. The arguments are:
// // start position, end position and step increment. The Slicer::endIsLast
// // argument specifies that the end position is the trc. The alternatice
// // is Slicer::endIsLength which specifies that the end argument is the
// // shape of the resulting subregion.
// Slicer ns1(IPosition(2,3,5), IPosition(2,13,21), IPosition(2,3,2),
//            Slicer::endIsLast);
// </srcblock>
//   </ul>

// <li> Lattices - the actual holders of lattice-like data which all share a
// common <linkto class="Lattice">interface</linkto>.  The following items
// are all Lattices and may be used polymorphically wherever a Lattice is
// called for.
//  <ul>
//   <li>The <linkto class="ArrayLattice">ArrayLattice</linkto> class adds
//   the interface requirements of a Lattice to an AIPS++ 
//   <linkto class="Array">Array</linkto>. The data inside an ArrayLattice
//   are not stored on disk.  This n-dimensional array class is the simplest
//   of the Lattices.  Users construct the ArrayLattice with an argument
//   which is either an IPosition which describes the array shape or a
//   previously instantiated AIPS++ Array that may already contain data.  In
//   the former case, some Lattice operation must be done to fill the data.
//   The ArrayLattice like all Lattices may be iterated through with a
//   LatticeIterator (see below).
// <srcblock>
// // Make an Array of shape 3x4x5
// Array<Float> simpleArray(IPosition(3,3,4,5));
// // fill it with a gradient
// for (Int k=0; k<5; i++)
//   for (Int j=0; j<4; k++)
//     for (Int i=0; i<3; j++) 
//       simpleArray(IPosition(3,i,j,k)) = i+j+k;
// // use the array to create an ArrayLattice
// ArrayLattice<Float> lattice(simpleArray);
// // apply a function to the values within the Lattice.
// lattice.apply(&floatFunction);
// </srcblock>

//   <li>The <linkto class="PagedArray">PagedArray</linkto> class stores its
//   data on disk and pages it into random access memory for use.  Paging is
//   used here to describe the process of getting pieces of data small
//   enough to fit into active memory even if the whole data set is much too
//   large.  An analogy might be: One doesn't read a whole book at once, but
//   exposes the pages one at a time.  This class "feels" like an array but
//   may hold very large amounts of data.  The paging has an added effect,
//   all the data may be made persistant so it stays around after the
//   application ends.  When you use PagedArrays - use them because you need
//   persistant data and/or paging into large data sets.
//
//   The persistance is done using a <linkto module="Tables">Table</linkto>,
//   and uses the <linkto module="Tables#Tables:TiledStMan">tiled storage
//   manager</linkto>.  This means that accessing the data along any axis is
//   equally efficient.
//
//   A PagedArray constructor allows previously created PagedArrays to be
//   recalled from disk.  Much of the time, the PagedArray will be
//   constructed with an IPosition argument which describes the array shape
//   and a Table argument for use as the place of storage.  Then the
//   PagedArray may be filled using any of the access functions of Lattices
//   (like the LatticeIterator.)
//
// <srcblock>
// // The PagedArray is best used for very large bodies of data
// // and requires a table in order to reside on disk - make one here.
// SetupNewTable newSetup("STANDARD", TableDesc("", TableDesc::Scratch),
//                        Table::New);
// Table standard(newSetup);
// // define a shape for the Lattice
// IPosition bigShape(3, 512, 1024, 2048);
// // make our PagedArray
// PagedArray<DComplex> bigLattice(bigShape, standard);
// // create a LatticeIterator to fill the Lattice.
// // If we are interested in accessing all the elements of the Lattice
// // sequentially it is possible to ask the PagedArray what cursor shape is
// // the most effecient to use. This is done using the niceCursorShape
// // function, which takes as an argument the maximum number of pixels to use
// // in the cursor. 
// IPosition cursorShape(niceCursorShape(maxPixels()));
// LatticeIterator<DComplex> iter(bigLattice, cursorShape);
// // we need to make a cursor which will move through the data...
// Array<DComplex> & cursor(iter.cursor());
// // iterate through and fill each page from some mythical function
// // which returns an Array given some integer.
// for(iter.reset(); !iter.atEnd(); iter++) 
//   cursor = mythFunc(iter.nsteps());
// </srcblock>
//  </ul>
//
// <li> <linkto class="LatticeIterator">LatticeIterator</linkto> - the
// object which allows iteration through any Lattice's data. This comes in
// two types: the <src>RO_LatticeIterator</src> which should be used if you
// are not going to change the Lattice's data, and the
// <src>LatticeIterator</src> if you need to change the data in the Lattice
//  <ul>
//  <li> The <linkto class="RO_LatticeIterator">RO_LatticeIterator</linkto>
//  class name reflects its role as a means of iterating a "Read-Only" array
//  (hereafter refered to as a "cursor") through a Lattice based object,
//  from beginning to end.  Think of a window into the Lattice that moves to
//  a new location when requested.  The Lattice doesn't change but you may
//  see all or part of it's data as the cursor "window" moves around.  This
//  class allows optimized read-only iteration through any instance of a
//  class derived from Lattice.  The RO_LatticeIterator has a cursor, for
//  which the users may define a shape.  That shape is then moved through
//  the Lattice in an orderly fashion defined by the user.  The cursor is
//  "read only" and therefore may only be used to "get" the data out of the
//  Lattice.  But for problems involving analysis of the Lattice, changing
//  the Lattice is hopefully not necessary.  LatticeIterators are
//  constructed with the Lattice to be iterated as the first argument.  The
//  second constructor argument is either an IPosition which defines the
//  shape of the cursor or a LatticeNavigator argument.  The IPosition
//  argument cause the iterator to follow a path that seems generic, the
//  cursor starts at the Lattice's origin and moves in the direction of the
//  x-axis, then the y-axis, then the z-axis, etc...  If a LatticeNavigator
//  argument is given, more control on the cursor shape and path are
//  available.
// <srcblock>
// // simple route - define a cursor shape that is the xy plane of our lattice.
// IPosition curShape(2, lattice.shape()(0), lattice.shape()(1));
// // make our iterator...
// LatticeIterator<Float> iter(lattice, curShape);
// // make a cursor to use...
// const Array<Float> & cursor(iter.cursor());
// // and use the moving cursor values.
// for (iter.reset(); !iter.atEnd(); iter++) 
//   minMax(cursor, min, max);
// </srcblock>
//   <li> The <linkto class="LatticeIterator">LatticeIterator</linkto> class
//   name reflects its role as a means of iterating a read and write cursor
//   through a Lattice based object, from origin to "end".  Not only does
//   the cursor allow you to inspect the Lattice data but you may also
//   change the Lattice via operations on the cursor.  The analogy of the
//   window used above becomes a door.  The door (cursor) moves methodically
//   through the Lattice data, providing a way of looking in or reaching in
//   to make changes.  This class provides optimized read and write
//   iteration through any class derived from Lattice.  The techinque is
//   identical to the RO_LatticeIterator.  But the cursor, in this case, is
//   a reference back to the data in the Lattice.  This means that changes
//   made to the cursor propagate back to the Lattice.  This is especially
//   useful for the PagedArray and PagedImage classes.  These two classes
//   are constructed empty and need iteration to fill in the Lattice data.
// <srcblock>
// // make an empty PagedArray...
// PagedArray<Float> bigOne(IPosition(4,300,400,500,64));
// // create a iterator to fill it one plane at a time
// LatticeIterator<Float> iter(bigOne, IPosition(2, 300, 400));
// // fill each plane with the "distance" from the origin
// for(iter.reset();!iter.atEnd(); iter++)
//    iter.cursor() = iter.nsteps();
// </srcblock>
//  </ul>
// <li> LatticeNavigators - the
// objects which define the method and path used by a
// LatticeIterator to move through a Lattice.  The process of
// iterating may be seen like an ocean exploring ship: the Lattice is the
// ocean, deep and wide, with many secrets; the ship represents the
// LatticeIterator, moving about on the ocean, doing the exploration; and
// the path the ship (cursor) follows is dictated by the Navigator.  Many
// different paths are possible.  We leave it you to choose the
// LatticeNavigator (method and path) when using a LatticeIterator.
// <ul>
//   <li> The <linkto class="LatticeStepper">LatticeStepper</linkto> class
//   is used to define the steps which the cursor takes during it's path
//   through the Lattice.  Every element of the Lattice will be covered,
//   starting at the origin and ending at the "top right corner."  This
//   class provides the information needed by a LatticeIterator to do
//   non-standard movements of the cursor during iteration.  The shape of
//   the cursor is specified by the second IPosition argument of the
//   LatticeStepper.  The order of the axis is important. An IPosition(1,5)
//   is a five element vector along the x-axis.  An IPosition(3,1,1,5) is a
//   five element vector along the z-axis.  The degenerate axis (axis with
//   lengths of one) act as place holders.  The third argument in the
//   LatticeStepper constructor is the "orientation" IPosition.  This
//   describes the order of the axis for the cursor to follow.  Again, we
//   treat the elements, in order, of the IPosition as the designators of
//   the appropriate axis.  The zeroth element indicates which axis is the
//   fastest moving, the first element indicates which axis is the second
//   fastes moving etc. eg. The IPosition(3,2,0,1) says the LatticeIterator
//   should start with the z-axis, next follow the x-axis, and finish with
//   the y-axis.  A single element cursor would thus move through a cube of
//   dimension(x,y,z) from (0,0,0) up the z-axis until reaching the maximum
//   (0,0,z-1) and then start on (1,0,0) and move to (1,0,z-1), etc.
// <srcblock>
// // The shape of our Lattice - a 4 dimensional image of shape (x,y,z,t).
// IPosition latShape(image.shape());
// // the shape of the cursor (an xz-matrix), we use the actual lengths of
// // the Lattice's x and z axes so that the cursor, as an integral shape, is
// // is more efficient.
// IPosition curShape(2, latShape(0), 1, latShape(2));
// // the path the cursor should follow, we list x and z first, even though
// // no iterations will be done along those axes since the cursor is an 
// // integral subshape of the Lattice. The cursor will move along the y-axis
// // and then increment the t-axis.
// IPosition orientation(4,0,2,1,3);
// // use the above IPositions to construct a LatticeStepper
// LatticeStepper curInfo(latShape, curShape, orientation);
// // Build an iterator
// LatticeIterator<Float> iter(image, curInfo);
// </srcblock>
// </ul>
// <li> Global functions which operate on Lattices.
// <ul> <linkto group="CopyLattice.h#Lattice Copy Functions">CopyLattice</linkto>
// - as the name implies, this function will transfer the lattice-like data
// from one Lattice to another. It can also copy regions of one Lattice to
// regions of another Lattice.

// </ul>
// </ol>
//
// <note role=warning> The following are listed for low-level programmers.  
// Lattice users need not understand them.</note>  The Lattice directory
// contains several files relevent only to implementation.
//
// <ul>
//   <li> <linkto class="Lattice">Lattice</linkto> - an abstract base class
//   defining the interface to classes which must act as Lattices do.
//   The user simply publicly inherits from Lattice and defines the member
//   functions declared as pure abstract in the Lattice header file.
//   <li> The <linkto class="LatticeNavigator">LatticeNavigator</linkto>
//   class name defines the interface used for navigating through a Lattice
//   by iteration.  This class is an abstract base.  Classes derived from
//   this (currently only 
//   <linkto class="LatticeStepper">LatticeStepper</linkto>) must
//   define the path the iterator cursor follows, the size of the movement
//   of the cursor with each iteration, and the behavior of that cursor
//   shape as it moves through a Lattice.
//   <li> The 
//   <linkto class="LatticeIterInterface">LatticeIterInterface</linkto>
//   class defines the interface for a specific Lattice's iterator.  This
//   class is an abstract base class i.e. it may not be instantiated but
//   solely is used through derivation. In order to utilize the
//   LatticeIterator class, Lattice based classes need to
//   derive an iterator from LatticeIterInterface.  This method will
//   allow authors of new Lattices to optimize for the LatticeIterator
//   internals which impact upon the new Lattice.
//   <li> <linkto class="ArrLatticeIter">ArrLatticeIter</linkto> - this
//   class is the ArrayLattice's optimized method of iterating.  It is
//   derived from LatticeIterInterface.  This class is a "letter" utilized
//   within the LatticeIterator "envelope" and would normally not be
//   instantiated by any user.
//   <li> <linkto class="PagedArrIter">PagedArrIter</linkto> - this class is
//   the PagedArray's optimized method of iterating. This class is a
//   "letter" utilized within the LatticeIterator "envelope" and should not
//   normally be instantiated by any user.
//   <li> <linkto class="LatticeIndexer">LatticeIndexer</linkto> - this
//   class contains the currently defined Lattice and sub-Lattice shape. It
//   is currently used only by the LatticeStepper class as it conatins
//   member functions for moving a cursor through a defined sub-Lattice. I
//   cannot see why users would every be interested in this class. It is
//   mainly for use when writing classes derived from LatticeNavigator.
//   </ul>
// </ol>
// </synopsis>
//
// <example>
// <srcblock>
// // lets create a PagedArray, iterate through it, and then take slices.
// // First, we need a Table to hold our data on disk.
// Table table(SetupNewTable("myData", TableDesc("", TableDesc::Scratch), 
//                           Table::New));
// // create the shape of the Lattice we are interested in.
// IPosition shape(3,500,500,64);
// // now create the PagedArray, with the above shape, and stored in the 
// // table above.
// PagedArray<Float> array(shape, table);
// // This PagedArray is empty so we must fill it - let's use an Iterator
// // The shape of the cursor will be a 500x500 plane
// IPosition curshape(3,500,500,1);
// // the path will be from the origin, up the "z-axis"
// IPosition path(3,0,1,2);
// // make a LatticeStepper
// LatticeStepper navigator(shape, curshape, path);
// // create the iterator
// LatticeIterator<Float> iter(array, navigator);
// // we want to start at the "other" end from the origin, so...
// While (!iter.atEnd()) iter++;
// // okay, the iterator is at the far end of the Lattice, lets work 
// // back to the origin, filling the Lattice as we go.
// // Here we create a cursor in order to alter the internal values of the
// // lattice.
// Matrix<Float> &cursor(iter.matrixCursor());
// for(;!iter.atStart();iter--) {
//   // fill the matrix with a gradient of values
//   for(int i=0;i<500;i++)
//     for(int j=0;j<500;j++)
//       cursor(IPosition(2,i,j)) = iter.nsteps() - 64 + i + j;
// }
// // now lets get a slice of data from the Lattice
// // First, we designate the particulars...
// // here we pick the middle of the Lattice as our starting point
// IPosition start(3,249,249,31);
// // we want a 10x10 slice along the x,z plane
// IPosition sliceShape(3,10,1,10);
// // we want every other piece of data, or a stride of 2.
// IPosition stride(3,2);
// // Now, we create a Slicer...
// Slicer slice(start, sliceShape, stride);
// // create the buffer to hold our data...
// Matrix<Float> buffer;
// // get the slice into the buffer
// array.getSlice(buffer,slice,true);
// // Make an ArrayLattice with our slice...
// ArrayLattice<Float> anotherLattice(buffer);
// // apply a function to every element of the ArrayLattice.
// anotherLattice.apply(&cos);
// </srcblock>
// </example>
//
// <motivation>
// Lattices allow the various holders of data to assume a general method 
// of treatment.  No more guessing, if it holds data, it is probably a 
// Lattice.  Thus methods that one applies in one area may be carried over to
// another.  The learning curve for additional data containers is, therefore,
// reduced.
// </motivation>
//
// <todo asof="1997/01/30">
//   <li> More ways of navigating through the Lattice are needed. In
//   particular Navigators which optimally allow you to access all the data
//   in a Lattice are required. This would deprecate the need for users to
//   use the nice cursorshape function.
//   <li> Another round of optimisation is required.
// </todo>
//
// </module>

#endif

