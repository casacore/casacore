//# PagedArrIter.h: A concrete iterator for use with PagedArray's.
//# Copyright (C) 1994,1995,1996,1997
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

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tPagedArray.cc,tPagedArrIter.cc" demos="dPagedArray.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="PagedArray">PagedArray</linkto>
//   <li> <linkto class="Lattice">Lattice</linkto>
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
// This class can be used by a user if it is known that the user will always
// be iterating through an <linkto class="PagedArray">PagedArray</linkto>
// object. Usually it will be more convienient to use the more general
// <linkto class="LatticeIterator">LatticeIterator</linkto> class to iterate
// through a <src>PagedArray</src> as this class can also iterate through
// any other object derived from the
// <linkto class="Lattice">Lattice</linkto> base class
// (like an <linkto class="ArrayLattice">ArrayLattice</linkto>).
//
// The usage of this class is identical to the 
// <linkto class="LatticeIterator">LatticeIterator</linkto> class, and
// reference should be made to documentation of that class. This
// documentation will only describe details specific to the
// <src>PagedArrIter</src> class.
// </synopsis>

// <motivation>
// We are hoping to allow each derivation of Lattice to make as efficient an
// iterator as possible.  The letter/envelope scheme allowed us to hide the 
// juicy bits in classes like the one you see here.
// </motivation>

// <templating arg=T>
//    <li> Restricted to the type of the PagedArray argument in the 
//constructors
// </templating>

// <todo asof="1997/01/31">
//   <li> Ensure that the cursor is using reference semantics wherever possible
//   <li> Fixup the code when the cursor is overhanging the Lattice
//   <li> try and be more efficient about when the cursor needs to be updated.
// </todo>


template <class T>
class PagedArrIter : public LatticeIterInterface<T>
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

  // destructor (cleans up dangling references and releases cursor memory)
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
