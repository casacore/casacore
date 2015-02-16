//# LatticeIter.cc: defines the RO_LatticeIterator and LatticeIterator classes
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2003
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

#ifndef LATTICES_LATTICEITERATOR_TCC
#define LATTICES_LATTICEITERATOR_TCC

#include <casacore/casa/aips.h>

#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeNavigator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TileStepper.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Utilities/Assert.h> 
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
RO_LatticeIterator<T>::RO_LatticeIterator()
{
  DebugAssert(ok(), AipsError);
}

template <class T>
RO_LatticeIterator<T>::RO_LatticeIterator (const Lattice<T>& lattice,
					   Bool useRef)
: itsIterPtr (lattice.makeIter (TileStepper (lattice.shape(),
					     lattice.niceCursorShape()),
				useRef))
{
  DebugAssert(ok(), AipsError);
}

template <class T>
RO_LatticeIterator<T>::RO_LatticeIterator (const Lattice<T>& lattice,
					   const LatticeNavigator& method,
					   Bool useRef)
: itsIterPtr (lattice.makeIter (method, useRef))
{
  DebugAssert(ok(), AipsError);
}

template <class T>
RO_LatticeIterator<T>::RO_LatticeIterator (const Lattice<T>& lattice,
					   const IPosition& cursorShape,
					   Bool useRef)
: itsIterPtr (lattice.makeIter (LatticeStepper(lattice.shape(), cursorShape),
				useRef))
{
  DebugAssert(ok(), AipsError);
}

template <class T>
RO_LatticeIterator<T>::RO_LatticeIterator (const RO_LatticeIterator<T>& other)
: itsIterPtr (other.itsIterPtr)
{
  DebugAssert(ok(), AipsError);
}

template <class T>
RO_LatticeIterator<T>::~RO_LatticeIterator()
{
  // CountedPtr destructor takes care of deletion of the LatticeIterInterface
}

template <class T>
RO_LatticeIterator<T>& RO_LatticeIterator<T>::operator=
                                 (const RO_LatticeIterator<T>& other)
{
  DebugAssert(ok(), AipsError);
  if (this != &other) {
    itsIterPtr = other.itsIterPtr;
  }
  return *this;
}

template <class T>
RO_LatticeIterator<T> RO_LatticeIterator<T>::copy() const
{
  RO_LatticeIterator<T> tmp;
  if (!isNull()) {
    tmp.itsIterPtr = itsIterPtr->clone();
  }
  return tmp;
}

template <class T>
Bool RO_LatticeIterator<T>::operator++(int)
{
  return itsIterPtr->operator++(0);
}
 
template <class T>
Bool RO_LatticeIterator<T>::operator++()
{
  return itsIterPtr->operator++();
}
 
template <class T>
Bool RO_LatticeIterator<T>::operator--(int)
{
  return itsIterPtr->operator--(0);
}

template <class T>
Bool RO_LatticeIterator<T>::operator--()
{
  return itsIterPtr->operator--();
} 

template <class T>
void RO_LatticeIterator<T>::reset()
{
  itsIterPtr->reset();
}

template <class T>
Bool RO_LatticeIterator<T>::atStart() const
{
  return itsIterPtr->atStart();
}

template <class T>
Bool RO_LatticeIterator<T>::atEnd() const
{
  return itsIterPtr->atEnd();
}

template <class T>
uInt RO_LatticeIterator<T>::nsteps() const
{
  return itsIterPtr->nsteps();
}

template <class T>
IPosition RO_LatticeIterator<T>::position() const
{
  return itsIterPtr->position();
}

template <class T>
IPosition RO_LatticeIterator<T>::endPosition() const
{
  return itsIterPtr->endPosition();
}

template <class T>
IPosition RO_LatticeIterator<T>::latticeShape() const
{
  return itsIterPtr->latticeShape();
}

template <class T>
IPosition RO_LatticeIterator<T>::cursorShape() const
{
  return itsIterPtr->cursorShape();
}

template <class T>
const Vector<T>& RO_LatticeIterator<T>::vectorCursor() const
{
  return itsIterPtr->vectorCursor (True, False);
}

template <class T>
const Matrix<T>& RO_LatticeIterator<T>::matrixCursor() const
{
  return itsIterPtr->matrixCursor (True, False);
}

template <class T>
const Cube<T>& RO_LatticeIterator<T>::cubeCursor() const
{
  return itsIterPtr->cubeCursor (True, False);
}

template <class T>
const Array<T>& RO_LatticeIterator<T>::cursor() const
{
  return itsIterPtr->cursor (True, False);
}


template <class T>
Bool RO_LatticeIterator<T>::ok() const
{
  if (!isNull()) {
    if (! itsIterPtr->ok()) {
      throw AipsError ("The actual Lattice Iterator class is inconsistent");
      return False;
    }
  }
  return True;
}



template <class T>
LatticeIterator<T>::LatticeIterator()
{}

template <class T>
LatticeIterator<T>::LatticeIterator (Lattice<T>& lattice, Bool useRef)
: RO_LatticeIterator<T> (lattice, useRef)
{
  if (! lattice.isWritable()) {
    throw (AipsError ("LatticeIterator cannot be constructed; "
		      "lattice is not writable"));
  }
}

template <class T>
LatticeIterator<T>::LatticeIterator (Lattice<T>& lattice,
				     const LatticeNavigator& method,
				     Bool useRef)   
: RO_LatticeIterator<T> (lattice, method, useRef)
{
  if (! lattice.isWritable()) {
    throw (AipsError ("LatticeIterator cannot be constructed; "
		      "lattice is not writable"));
  }
}

template <class T>
LatticeIterator<T>::LatticeIterator (Lattice<T>& lattice,
				     const IPosition& cursorShape,
				     Bool useRef)
 
: RO_LatticeIterator<T> (lattice, cursorShape, useRef)
{
  if (! lattice.isWritable()) {
    throw (AipsError ("LatticeIterator cannot be constructed; "
		      "lattice is not writable"));
  }
}

template <class T>
LatticeIterator<T>::LatticeIterator (const LatticeIterator<T>& other)
: RO_LatticeIterator<T> (other)
{}

template <class T>
LatticeIterator<T>::~LatticeIterator()
{}

template <class T>
LatticeIterator<T>& LatticeIterator<T>::operator=
                                      (const LatticeIterator<T>& other)
{
  RO_LatticeIterator<T>::operator= (other);
  return *this;
}

template <class T>
LatticeIterator<T> LatticeIterator<T>::copy() const
{
  LatticeIterator<T> tmp;
  if (!isNull()) {
    tmp.itsIterPtr = itsIterPtr->clone();
  }
  return tmp;
}

template <class T>
Vector<T>& LatticeIterator<T>::rwVectorCursor()
{
  return itsIterPtr->vectorCursor (True, True);
}

template <class T>
Matrix<T>& LatticeIterator<T>::rwMatrixCursor()
{
  return itsIterPtr->matrixCursor (True, True);
}

template <class T>
Cube<T>& LatticeIterator<T>::rwCubeCursor()
{
  return itsIterPtr->cubeCursor (True, True);
}

template <class T>
Array<T>& LatticeIterator<T>::rwCursor()
{
  return itsIterPtr->cursor (True, True);
}

template <class T>
Vector<T>& LatticeIterator<T>::woVectorCursor()
{
  return itsIterPtr->vectorCursor (False, True);
}

template <class T>
Matrix<T>& LatticeIterator<T>::woMatrixCursor()
{
  return itsIterPtr->matrixCursor (False, True);
}

template <class T>
Cube<T>& LatticeIterator<T>::woCubeCursor()
{
  return itsIterPtr->cubeCursor (False, True);
}

template <class T>
Array<T>& LatticeIterator<T>::woCursor()
{
  return itsIterPtr->cursor (False, True);
}

} //# NAMESPACE CASACORE - END


#endif
