//# LatticeExpr.cc:  this defines LatticeExpr.cc
//# Copyright (C) 1997,1998,1999
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

#include <trial/Lattices/LatticeExpr.h>
#include <trial/Lattices/LELArray.h>
#include <aips/Arrays/Array.h>
#include <aips/Lattices/Slicer.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h> 

typedef LELArray<Bool> gppbug_latticeexpr_bool;
typedef LELArray<Float> gppbug_latticeexpr_float;
typedef LELArray<Double> gppbug_latticeexpr_double;
typedef LELArray<Complex> gppbug_latticeexpr_complex;
typedef LELArray<DComplex> gppbug_latticeexpr_dcomplex;


template <class T>
LatticeExpr<T>::LatticeExpr()
{}

template <class T>
LatticeExpr<T>::LatticeExpr (const LatticeExprNode& expr, uInt)
: lastChunkPtr_p (0)
//
// Construct from a LatticeExprNode object.  The LEN type is
// converted to match the template type if possible
//
{
    DataType thisDT = whatType ((T*)0);
    if (expr.dataType() == thisDT) {
	expr_p = expr;
    } else {
	if (expr.dataType() != TpBool) {
	    throw (AipsError ("LatticeExpr::constructor - "
			      "Bool cannot be converted to a numeric type"));
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
: expr_p (other.expr_p),
  lastChunkPtr_p (0)
//
// Copy constructor.  Uses reference semantics
//
{}

template <class T>
LatticeExpr<T>& LatticeExpr<T>::operator=(const LatticeExpr<T>& other)
//
// Assignment. Uses reference semantics
//
{
   if (this != &other) {
      expr_p = other.expr_p;
      delete lastChunkPtr_p;
      lastChunkPtr_p = 0;
      lastSlicer_p = Slicer();
   }
   return *this;
}


template <class T>
Lattice<T>* LatticeExpr<T>::clone() const
//
// Return a copy of the LatticeExpr object. Uses
// reference semantics.
{
   return new LatticeExpr (*this);
}

template <class T>
MaskedLattice<T>* LatticeExpr<T>::cloneML() const
//
// Return a copy of the LatticeExpr object. Uses
// reference semantics.
{
   return new LatticeExpr (*this);
}

template <class T>
Bool LatticeExpr<T>::isMasked() const
{
   return expr_p.isMasked();
}

template <class T>
const LatticeRegion& LatticeExpr<T>::region() const
{
   return region_p;
}

template <class T>
Bool LatticeExpr<T>::isWritable() const
//
// A LatticeExpr lattice is not writable
//
{
   return False;
}

template <class T>
IPosition LatticeExpr<T>::shape() const
{
   return expr_p.shape();
}
  
template <class T>
IPosition LatticeExpr<T>::doNiceCursorShape (uInt) const
{
   return expr_p.getAttribute().tileShape();
}
  
template <class T>
LatticeCoordinates LatticeExpr<T>::coordinates() const
{
   return expr_p.getAttribute().coordinates();
}
  
template <class T>
Bool LatticeExpr<T>::doGetSlice (Array<T>& buffer,
				 const Slicer& section)
{
// Evaluate the expression if not accessing the same section again.
   if (!(section.start().isEqual (lastSlicer_p.start())
     &&  section.end().isEqual (lastSlicer_p.end())
     &&  section.stride().isEqual (lastSlicer_p.stride()))) {
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
      if (!(section.start().isEqual (lastSlicer_p.start())
        &&  section.end().isEqual (lastSlicer_p.end())
        &&  section.stride().isEqual (lastSlicer_p.stride()))) {
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
    T value;
    expr_p.eval (value);
    to.set (value);
  } else {
    Lattice<T>::copyDataTo (to);
  }
}
