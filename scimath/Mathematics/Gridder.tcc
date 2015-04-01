//# Gridder.cc: Nearest Neighbour Gridder
//# Copyright (C) 1996,1997,1999,2003
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
//#
//# $Id$

#ifndef SCIMATH_GRIDDER_TCC
#define SCIMATH_GRIDDER_TCC

#include <casacore/scimath/Mathematics/Gridder.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class Domain, class Range>
Gridder<Domain, Range>::Gridder() {}

template <class Domain, class Range>
Gridder<Domain, Range>::~Gridder() {}

template <class Domain, class Range>
Gridder<Domain, Range>::Gridder(const IPosition& ishape,
				const Vector<Domain>& iscale,
				const Vector<Domain>& ioffset) 
{
  // Scaling from world to pixels
  scale=iscale;
  offset=ioffset;

  // Shape of array
  shape=ishape;
  ndim=shape.nelements();
  offsetVec.resize(ndim);
  offsetVec=0;

  // Time-savers
  locVec.resize(ndim);
  shapeVec=shape.asVector();
  zeroShapeVec.resize(ndim);
  zeroShapeVec=0;

  posVec.resize(ndim);
}

// Turn a Domain position into a grid location. This should move
// to the nearest grid point (27.8->28, 28.2->28, -27.4->-27, etc)
template <class Domain, class Range>
Vector<Int>& Gridder<Domain, Range>::location(Vector<Int>& loc, const Vector<Domain>& pos)
{
  for (Int axis=0;axis<ndim;axis++) {
    loc(axis)=nint(scale(axis)*pos(axis)+offset(axis));
  }  
  return loc;
}

// Turn a Domain position into a grid position
template <class Domain, class Range>
Vector<Domain>& Gridder<Domain, Range>::position(Vector<Domain>& gpos,
						 const Vector<Domain>& pos)
{
  for (Int axis=0;axis<ndim;axis++) {
    gpos(axis)=scale(axis)*pos(axis)+offset(axis);
  }  
  return gpos;
}


// Is the location on the grid?
template <class Domain, class Range>
Bool Gridder<Domain, Range>::onGrid(const Vector<Int>& loc)
{
  for (Int i=0;i<ndim;i++) {
    if(loc(i)>=shapeVec(i)) return False;
    if(loc(i)<0) return False;
  }
  return True;
}

// Is the location (plus of minus deltas) on the grid?
template <class Domain, class Range>
Bool Gridder<Domain, Range>::onGrid(const Vector<Int>& loc, const Vector<Int>& delta)
{
  for (Int i=0;i<ndim;i++) {
    if((loc(i)+delta(i))>=shapeVec(i)) return False;
    if((loc(i)+delta(i))<0) return False;
    if((loc(i)-delta(i))>=shapeVec(i)) return False;
    if((loc(i)-delta(i))<0) return False;
  }
  return True;
}

// Is the position on the grid?
template <class Domain, class Range>
Bool Gridder<Domain, Range>::onGrid(const Vector<Domain>& pos)
{
  Int loc;
  for (Int i=0;i<ndim;i++) {
    loc=nint(scale(i)*pos(i)+offset(i));
    if(loc>=shapeVec(i)) return False;
    if(loc<0) return False;
  }  
  return True;
}

// Set the offset IP
template <class Domain, class Range>
void Gridder<Domain, Range>::setOffset(const Vector<Int>& off)
{
  offsetVec=off;
}

template <class Domain, class Range>
void Gridder<Domain, Range>::setOffset(const IPosition& off)
{
  offsetVec=off.asVector();
}

// Return correction factor. This is the value that
// must be divided to get a correct flux.
template <class Domain, class Range>
Range Gridder<Domain, Range>::correct(const IPosition& loc) 
{
  Range factor=1.0;
  for (Int dim=0;dim<ndim;dim++) {
    factor*=correctionVectors(dim)(loc(dim));
  }
  return factor;
}

// Return correction factor. This is the value that
// must be divided to get a correct flux.
template <class Domain, class Range>
void Gridder<Domain, Range>::correctX1D(Vector<Range>& factor,
					const Int locy)
{
  factor=correctionVectors(0);
  Range yFactor;
  yFactor=correctionVectors(1)(locy);
  factor*=yFactor;
}

// Fill in the cache of corrections. This must be in the
// constructor of the derived class. We use the virtual
// method for the 1D case.
template <class Domain, class Range>
void Gridder<Domain, Range>::fillCorrectionVectors()
{
  correctionVectors.resize(ndim);
  for (Int dim=0;dim<ndim;dim++) {
    correctionVectors(dim).resize(shape(dim));
    Range tmp; // need to split this up for egcs1.1.1 on alpha
    tmp = 1.0;
    correctionVectors(dim)= tmp;
    //    correctionVectors(dim)=Range(1.0);
    if(shape(dim)>1) {
      for (Int loc=0;loc<shape(dim);loc++) {
	correctionVectors(dim)(loc)=correctionFactor1D(loc, shape(dim));
      }
    }
  }
}



} //# NAMESPACE CASACORE - END


#endif
