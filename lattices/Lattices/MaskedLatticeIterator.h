//# MaskedLatticeIterator.h: Iterators for Masked Lattices: readonly
//# Copyright (C) 2003
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

#ifndef LATTICES_MASKEDLATTICEITERATOR_H
#define LATTICES_MASKEDLATTICEITERATOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Utilities/CountedPtr.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// A readonly iterator for masked Lattices.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tMaskedLatticeIterator.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="MaskedLattice">MaskedLattice</linkto>
//   <li> <linkto class="RO_LatticeIterator">RO_LatticeIterator</linkto>
// </prerequisite>

// <etymology>
// The leading "RO" is shorthand for "readonly", which indicates that an
// RO_MaskedLatticeIterator is used for traversing a masked lattice,
// examining and possibly extracting its contents, but not for modifying it.
// </etymology>

// <synopsis> 
// This class provides a convenient way to traverse any class derived from
// MaskedLattice. It is derived from class
// <linkto class=RO_LatticeIterator>RO_LatticeIterator</linkto>, so it
// provides the same iterator capabilities.
// On top of that it offers the function <src>getMask</src> to get the
// contents of the mask at the current iterator position.
//
// In principle, iteration through a MaskedLattice can be done as:
// <srcblock>
// void someFunc (const MaskedLattice<Float>& lattice)
// {
//   RO_LatticeIterator<Float> iter(lattice);
//   Array<Bool> mask;
//   while (! iter.atEnd()) {
//     const Array<Float>& array = iter.cursor();
//     lattice.getMaskSlice (mask, iter.position(), array.shape());
//     iter++;
//   }
// }
// </srcblock>
// Using a MaskedLatticeIterator makes getting the mask slightly more
// convenient.
// <srcblock>
// void someFunc (const MaskedLattice<Float>& lattice)
// {
//   RO_MaskedLatticeIterator<Float> iter(lattice);
//   Array<Bool> mask;
//   while (! iter.atEnd()) {
//     const Array<Float>& array = iter.cursor();
//     iter.getMask (mask);
//     iter++;
//   }
// }
// </srcblock>
// However, the most important reason to use MaskedLatticeIterator is
// performance. If the underlying lattice is a LatticeExpr object,
// the expression will be evaluated twice if a LatticeIterator object
// is used. The reason is that the lattice in the LatticeIterator is
// a different object from the lattice object used to get the mask.
// Hence, the optimization put in LatticeExpr is not used.
// When using a MaskedLatticeIterator the same lattice object is used
// to get data and mask.
// </synopsis>

// <motivation>
// The performance gain for LatticeExpr was the most important reason
// to develop this class.
// </motivation>

//# <todo asof="2003/11/10">
//#  <li>
//# </todo>


template<class T> class RO_MaskedLatticeIterator: public RO_LatticeIterator<T>
{
  //# Make members of parent class known.
public:
  using RO_LatticeIterator<T>::isNull;
  using RO_LatticeIterator<T>::position;
  using RO_LatticeIterator<T>::endPosition;
  using RO_LatticeIterator<T>::cursorShape;

public:
  // The default constructor creates an empty object which is practically
  // unusable.
  // It can only be used as the source or target of an assignment. It can
  // also be used as the source for the copy constructor and the copy function.
  // Other functions do not check if the object is empty and will usually
  // give a segmentation fault.
  // The function isNull() can be used to test if the object is empty.
  RO_MaskedLatticeIterator();

  // Construct the Iterator with the supplied data.
  // It uses a TileStepper as the default iteration strategy.
  // useRef=True means that if possible the cursor arrays returned
  // reference the data in the underlying lattice. This is only possible
  // for ArrayLattice objects (or e.g. a SubLattice using it).
  explicit RO_MaskedLatticeIterator (const MaskedLattice<T>& data,
				     Bool useRef=True);

  // Construct the Iterator with the supplied data, and iteration strategy
  RO_MaskedLatticeIterator (const MaskedLattice<T>& data,
			    const LatticeNavigator& method,
			    Bool useRef=True);

  // Construct the Iterator with the supplied data.
  // It uses a LatticeStepper with the supplied cursor shape as the
  // iteration strategy.
  RO_MaskedLatticeIterator (const MaskedLattice<T>& data,
			    const IPosition& cursorShape,
			    Bool useRef=True);

  // The copy constructor uses reference semantics (ie. NO real copy is made).
  // The function <src>copy</src> can be used to make a true copy.
  RO_MaskedLatticeIterator (const RO_MaskedLatticeIterator<T>& other);
 
  // Destructor (cleans up dangling references and releases memory)
  ~RO_MaskedLatticeIterator();

  // Assignment uses reference semantics (ie. NO real copy is made).
  // The function <src>copy</src> can be used to make a true copy.
  RO_MaskedLatticeIterator<T>& operator= (const RO_MaskedLatticeIterator<T>&);

  // Make a copy of the iterator object.
  // This means that an independent navigator object is created to
  // be able to iterate independently through the same MaskedLattice.
  // The position in the copied navigator is the same as the original.
  // The reset function has to be used to start at the beginning.
  // <br>Note that if the MaskedLattice uses a cache (e.g. PagedArray), the
  // cache is shared by the iterators.
  RO_MaskedLatticeIterator<T> copy() const;

  // Return the underlying MaskedLattice object.
  MaskedLattice<T>& lattice() const
    { return const_cast<MaskedLattice<T>&>(*itsMaskLattPtr); }

  // Is the underlying MaskedLattice really masked?
  Bool isMasked() const
    { return itsMaskLattPtr->isMasked(); }

  // Get the mask for the current position.
  // It returns the same flag as
  // <linkto class=MaskedLattice>MaskedLattice::getMaskSlice</linkto>.
  // <group>
  Bool getMask (COWPtr<Array<Bool> >&, Bool removeDegenerateAxes=False) const;
  Bool getMask (Array<Bool>&, Bool removeDegenerateAxes=False) const;
  Array<Bool> getMask (Bool removeDegenerateAxes=False) const;
  // </group>

private:
  // Construct from a LatticeIterator (for copy function).
  RO_MaskedLatticeIterator (const RO_LatticeIterator<T>&,
			    const RO_MaskedLatticeIterator<T>&);

  // Fill the pointer with a pointer to the masked lattice.
  // This pointer is a casted copy of the lattice pointer in the base class.
  // In this way they share the same MaskedLattice object, which is needed
  // for optimal performance of e.g. LatticeExpr.
  // Otherwise getting data from the lattice and from the mask would
  // result in 2 evaluations of the expression.
  // However, the lattice can be a PagedArray (for example, for PagedImage).
  // In that case a clone of the original MaskedLattice is used.
  void fillPtr (const MaskedLattice<T>& mlattice);

  CountedPtr<MaskedLattice<T> > itsMaskLattPtr;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/MaskedLatticeIterator.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
