//# LatticeExpr.cc:  this defines LatticeExpr.cc
//# Copyright (C) 1997,1998
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
#include <trial/Lattices/PixelBox.h>
#include <aips/Arrays/Array.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h> 


template <class T>
LatticeExpr<T>::LatticeExpr()
{}

template <class T>
LatticeExpr<T>::LatticeExpr (const LatticeExprNode& expr, uInt)
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
//
// Destructor does nothing
//
{}

template <class T>
LatticeExpr<T>::LatticeExpr (const LatticeExpr<T>& other)
: expr_p (other.expr_p)
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
IPosition LatticeExpr<T>::niceCursorShape (uInt) const
{
   return expr_p.getAttribute().tileShape();
}
  
template <class T>
LatticeCoordinates LatticeExpr<T>::coordinates() const
{
   return expr_p.getAttribute().coordinates();
}
  
template <class T>
Bool LatticeExpr<T>::getSlice (COWPtr<Array<T> >& buffer,
			       const IPosition& start,
			       const IPosition& shape,
			       const IPosition& stride, 
			       Bool removeDegenerateAxes) const
{
   return getSlice (buffer, Slicer(start, shape, stride), 
                    removeDegenerateAxes);
}

template<class T>
Bool LatticeExpr<T>::getSlice (COWPtr<Array<T> >& buffer,
			       const Slicer& section, 
			       Bool removeDegenerateAxes) const
{
// I can remove the constness because the buffer is never returned by
// reference. The COWPtr takes over the pointer to the array.

   Array<T>* arr = new Array<T>;
   LatticeExpr<T>* This = (LatticeExpr<T>*) this;
   Bool isARef = This->getSlice (*arr, section, removeDegenerateAxes);
   buffer = COWPtr<Array<T> > (arr, True, isARef);
   return False;
}

template <class T>
Bool LatticeExpr<T>::getSlice(Array<T>& buffer,
			      const IPosition& start,
			      const IPosition& shape,
			      const IPosition& stride,
			      Bool removeDegenerateAxes)
{
   return getSlice (buffer, Slicer(start, shape, stride), removeDegenerateAxes);
}
template <class T>
Bool LatticeExpr<T>::getSlice(Array<T>& buffer,
			      const Slicer& section,
			      Bool removeDegenerateAxes)
//
// This is the version of getSlice where the implementation
// is fundamental.  The others are implemented in terms of
// this one
// 
{
// removeDegenerateAxes is not supported.

   AlwaysAssert (!removeDegenerateAxes, AipsError);

// Turn the section into a PixelBox. This ensures that unspecified
// section values are filled in. So use the box thereafter as the section.
// Resize the buffer when empty. Otherwise check its shape.

   PixelBox region(section, expr_p.shape());
   if (buffer.nelements() == 0) {
      buffer.resize (region.box().length());
   } else {
      AlwaysAssert (buffer.shape().isEqual(region.box().length()), AipsError);
   }

// Evaluate the expression

   expr_p.eval (buffer, region);

   return False;
}

template <class T>
void LatticeExpr<T>::putSlice (const Array<T>&, const IPosition&)
{
   throw (AipsError ("LatticeExpr::putSlice - is not possible"));
}
template <class T>
void LatticeExpr<T>::putSlice (const Array<T>&, const IPosition&,
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
