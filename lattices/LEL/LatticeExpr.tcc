//# LatticeExpr.cc:  this defines LatticeExpr.cc
//# Copyright (C) 1997,1998,1999,2000,2003
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

#ifndef LATTICES_LATTICEEXPR_TCC
#define LATTICES_LATTICEEXPR_TCC

#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/LEL/LELArray.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h> 


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T>
LatticeExpr<T>::LatticeExpr()
: lastChunkPtr_p (0)
{}

template <class T>
LatticeExpr<T>::LatticeExpr (const LatticeExprNode& expr)
: shape_p        (expr.shape()),
  lastChunkPtr_p (0)
{
    // Check if an expression array has a shape.
    if (!expr.isScalar()  &&  shape_p.nelements() == 0) {
       throw AipsError ("LatticeExpr cannot be constructed from a lattice "
			"expression with an undefined shape");
    }
    init (expr);
}

template <class T>
LatticeExpr<T>::LatticeExpr (const LatticeExprNode& expr,
			     const IPosition& latticeShape)
: shape_p        (latticeShape),
  lastChunkPtr_p (0)
//
// Construct from a LatticeExprNode object.  The LEN type is
// converted to match the template type if possible
//
{
    // Check if the expression has a shape.
    if (!expr.isScalar()  &&  expr.shape().nelements() > 0
    &&  !(shape_p.isEqual(expr.shape()))) {
       throw AipsError ("LatticeExpr::constructor - "
			"given shape mismatches expression's shape");
    }
    init (expr);
}

template <class T>
void LatticeExpr<T>::init (const LatticeExprNode& expr)
{
    DataType thisDT = whatType (static_cast<T*>(0));
    if (expr.dataType() == thisDT) {
	expr_p = expr;
    } else {
	if (expr.dataType() == TpBool) {
	    throw (AipsError ("LatticeExpr::constructor - "
			      "Bool expression cannot be converted to "
			      "a numeric type"));
	}
	switch (thisDT) {
	case TpFloat:
	    expr_p = expr.makeFloat();
	    break;
	case TpDouble:
	    expr_p = expr.makeDouble();
	    break;
	case TpComplex:
	    expr_p = expr.makeComplex();
	    break;
	case TpDComplex:
	    expr_p = expr.makeDComplex();
	    break;
	default:
	    throw (AipsError ("LatticeExpr::constructor - "
			      "A numeric type cannot be converted to Bool"));
	}
    }
}

template <class T>
LatticeExpr<T>::~LatticeExpr()
{
   delete lastChunkPtr_p;
}

template <class T>
LatticeExpr<T>::LatticeExpr (const LatticeExpr<T>& other)
: MaskedLattice<T>(),
  expr_p          (other.expr_p),
  shape_p         (other.shape_p),
  lastChunkPtr_p  (0)
{}

template <class T>
LatticeExpr<T>& LatticeExpr<T>::operator=(const LatticeExpr<T>& other)
{
   if (this != &other) {
      expr_p  = other.expr_p;
      shape_p = other.shape_p;
      delete lastChunkPtr_p;
      lastChunkPtr_p = 0;
      lastSlicer_p = Slicer();
   }
   return *this;
}


template <class T>
MaskedLattice<T>* LatticeExpr<T>::cloneML() const
{
   return new LatticeExpr (*this);
}

template <class T>
Bool LatticeExpr<T>::isMasked() const
{
   return expr_p.isMasked();
}

template <class T>
const LatticeRegion* LatticeExpr<T>::getRegionPtr() const
{
   return 0;
}

template <class T>
Bool LatticeExpr<T>::isWritable() const
{
   return False;
}

template<class T>
Bool LatticeExpr<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return expr_p.lock (type, nattempts);
}
template<class T>
void LatticeExpr<T>::unlock()
{
  expr_p.unlock();
}
template<class T>
Bool LatticeExpr<T>::hasLock (FileLocker::LockType type) const
{
  return expr_p.hasLock (type);
}
template<class T>
void LatticeExpr<T>::resync()
{
  expr_p.resync();
}

template <class T>
IPosition LatticeExpr<T>::shape() const
{
   return shape_p;
}
  
template <class T>
IPosition LatticeExpr<T>::doNiceCursorShape (uInt) const
{
   return expr_p.getAttribute().tileShape();
}
  
template <class T>
LELCoordinates LatticeExpr<T>::lelCoordinates() const
{
   return expr_p.getAttribute().coordinates();
}
  
template <class T>
Bool LatticeExpr<T>::doGetSlice (Array<T>& buffer,
				 const Slicer& section)
{
// Evaluate the expression if not accessing the same section again.
   if (!(section==lastSlicer_p)) {
      delete lastChunkPtr_p;
      lastChunkPtr_p = new LELArray<T> (section.length());
      lastSlicer_p = section;
      expr_p.eval (*lastChunkPtr_p, section);
   }
   buffer.reference (lastChunkPtr_p->value());
   return True;
}

template <class T>
Bool LatticeExpr<T>::doGetMaskSlice (Array<Bool>& buffer,
				     const Slicer& section)
{
// Evaluate if masked and if different section.
   if (expr_p.isMasked()) {
      if (!(section==lastSlicer_p)) {
	 delete lastChunkPtr_p;
	 lastChunkPtr_p = new LELArray<T> (section.length());
	 lastSlicer_p = section;
	 expr_p.eval (*lastChunkPtr_p, section);
      }
      if (lastChunkPtr_p->isMasked()) {
	 buffer.reference (lastChunkPtr_p->mask());
	 return True;
      }
   }
// Not masked, so we can simply fill the buffer with True values.
   buffer.resize (section.length());
   buffer = True;
   return False;
}

template <class T>
void LatticeExpr<T>::doPutSlice (const Array<T>&, const IPosition&,
				 const IPosition&)
{
   throw (AipsError ("LatticeExpr::putSlice - is not possible"));
}

template<class T>
void LatticeExpr<T>::copyDataTo (Lattice<T>& to) const
{
  // If a scalar, set lattice to its value.
  // Otherwise use the Lattice copyDataTo function.
  if (expr_p.isScalar()) {
    // Check the lattice is writable.
    AlwaysAssert (to.isWritable(), AipsError);
    T value;
    expr_p.eval (value);
    to.set (value);
  } else {
    Lattice<T>::copyDataTo (to);
  }
}

template<class T>
void LatticeExpr<T>::handleMathTo (Lattice<T>& to, int oper) const
{
  // If a scalar, apply its value to the lattice.
  // Otherwise use the Lattice handleMathTo function.
  if (expr_p.isScalar()) {
    T value;
    expr_p.eval (value);
    // Check the lattice is writable.
    AlwaysAssert (to.isWritable(), AipsError);
    // Create an iterator for the output.
    // If possible, use reference semantics in the iterator.
    LatticeIterator<T> iter(to, True);
    switch (oper) {
    case 0:
      for (iter.reset(); !iter.atEnd(); iter++) {
	iter.rwCursor() += value;
      }
      break;
    case 1:
      for (iter.reset(); !iter.atEnd(); iter++) {
	iter.rwCursor() -= value;
      }
      break;
    case 2:
      for (iter.reset(); !iter.atEnd(); iter++) {
	iter.rwCursor() *= value;
      }
      break;
    case 3:
      for (iter.reset(); !iter.atEnd(); iter++) {
	iter.rwCursor() /= value;
      }
      break;
    default:
      throw AipsError ("LatticeExpr::handleMathTo - Unknown operator");
    }
  } else {
    Lattice<T>::handleMathTo (to, oper);
  }
}

} //# NAMESPACE CASACORE - END


#endif
