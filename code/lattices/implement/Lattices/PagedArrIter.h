//# PagedArrIter.h: A concrete iterator for use with PagedArray's.
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

#if !defined(AIPS_PAGEDARRITER_H)
#define AIPS_PAGEDARRITER_H

//# Includes
#include <aips/aips.h>
#include <trial/Lattices/LatticeIterInterface.h>
#include <trial/Lattices/PagedArray.h>
#include <aips/Arrays/Array.h>
#include <aips/Utilities/CountedPtr.h>
#include <trial/Lattices/LatticeNavigator.h>

//# Forward Declarations
class IPosition;
template <class T> class Cube;
template <class T> class Matrix;
template <class T> class Vector;


// <summary>
// A read/write Lattice iterator for PagedArray's
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeIterator.cc" demos="dPagedArray.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="PagedArray">PagedArray</linkto>
//   <li> <linkto class="LatticeIterator">LatticeIterator</linkto>
//   <li> <linkto class="LatticeIterInterface">LatticeIterInterface
//        </linkto>
//   <li> letter/envelope schemes, eg. Coplien, "Advanced C++", ch 5.5
// </prerequisite>

// <etymology>
// The PagedArrIter class name is a contraction of Paged Array Iterator 
// and reflects its role as the methods for iterating through Lattices which 
// are resident on disk.
// </etymology>

// <synopsis>
// This class is not meant for general use. Instead class
// <linkto class="LatticeIterator">LatticeIterator</linkto> should be used
// to iterate through a <src>PagedArray</src> or any other
// <linkto class="Lattice">Lattice</linkto> object
// (like a <linkto class="ArrayLattice">ArrayLattice</linkto>).
// <p>
// PagedArrIter is derived from LatticeIterInterface and implements
// the iterator for a <linkto class=PagedArray>PagedArray</linkto>
// object. This iterator is somewhat special because it sets the
// PagedArray cache size at the start of an iteration.
// </synopsis>

// <motivation>
// Make for each derivation of Lattice as efficient an iterator as possible.
// The letter/envelope scheme allowed us to hide the special bits in
// classes like the one you see here.
// </motivation>

// <templating arg=T>
//  <li> Restricted to the type of the PagedArray argument in the 
//   constructors
// </templating>

//# <todo asof="1997/01/31">
//#   <li>
//# </todo>


template <class T> class PagedArrIter : public LatticeIterInterface<T>
{
friend class PagedArray<T>;

protected:
  // Construct the Iterator with the supplied data, and iteration strategy
  PagedArrIter (const PagedArray<T>& data, const LatticeNavigator& method);

  // The copy constructor uses reference sematics for the PagedArray and
  // copy semantics for the cursor and Navigator. This way the newly
  // constructed PagedArrIter can independently iterate through the same
  // data set. (with the same cursor shape etc.)
  PagedArrIter (const PagedArrIter<T>& other);

  // Destructor (cleans up dangling references and releases cursor memory)
  virtual ~PagedArrIter();
  
  // The assignment operator uses reference sematics for the PagedArray and
  // copy semantics for the cursor and Navigator. This way the
  // PagedArrIter's share the same data set but independently iterate
  // with cursors of the same size.
  PagedArrIter<T>& operator= (const PagedArrIter<T>& other);

  // Clone the object.
  virtual LatticeIterInterface<T>* clone() const;

private:
  // Setup the cache in the tiled storage manager.
  void setupTileCache();


  // reference to the PagedArray
  PagedArray<T> itsData;
};


#endif
