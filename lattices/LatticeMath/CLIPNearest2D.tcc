//# CLIPNearest2D.h: Nearest neighbour interpolator for CurvedLattice2D
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
//# You should have receied a copy of the GNU Library General Public License
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

#ifndef LATTICES_CLIPNEAREST2D_TCC
#define LATTICES_CLIPNEAREST2D_TCC


#include <casacore/lattices/LatticeMath/CLIPNearest2D.h>
#include <casacore/lattices/Lattices/PixelCurve1D.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/casa/Arrays/ArrayIter.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
CLIPNearest2D<T>::CLIPNearest2D()
{}

template<class T>
CLIPNearest2D<T>* CLIPNearest2D<T>::clone() const
{
  return new CLIPNearest2D<T> (*this);
}

template<class T>
void CLIPNearest2D<T>::getData (Array<T>& buffer,
				const Vector<Float>& x,
				const Vector<Float>& y,
				const Slicer& section)
{
  // Determine the shape and positions w.r.t. the original lattice.
  IPosition shp  = itsAxesMap.shapeToOld (buffer.shape());
  shp[itsAxis1] = 1;
  shp[itsAxis2] = 1;
  IPosition blc  = itsAxesMap.posToOld (section.start());
  IPosition leng = itsAxesMap.shapeToOld (section.length());
  leng[itsAxis1] = 1;
  leng[itsAxis2] = 1;
  IPosition incr = itsAxesMap.shapeToOld (section.stride());
  // We have to get the data from the lattice chunk by chunk.
  // Each chunk is determined by a pixel on the curve.
  // If the curve axis is the last axis, we can use ArrayIterator
  // (which is faster (I think) than slicing arrays ourselves).
  if (itsCurveAxis == buffer.ndim() - 1) {
    shp.append (IPosition(1, buffer.shape()[itsCurveAxis]));
    Array<T> data = buffer.reform (shp);
    ArrayIterator<T> iter(data, shp.nelements()-1);
    for (uInt i=0; i<x.nelements(); i++) {
      blc[itsAxis1] = Int(x[i]+0.5);
      blc[itsAxis2] = Int(y[i]+0.5);
      // Some lattices (e.g. ArrayLattice) return an Array referencing
      // the original data. That would destroy the ArrayIter internals,
      // so in that case we copy the data.
      if (itsIsRef) {
	iter.array() = itsLatticePtr->getSlice (blc, leng, incr);
      } else {
	Bool isRef = itsLatticePtr->getSlice (iter.array(), blc, leng, incr);
	// Just make sure it is not a reference.
	AlwaysAssert (!isRef, AipsError);
      }
      iter.next();
    }
  } else {
    IPosition start = IPosition(buffer.ndim(), 0);
    IPosition end   = buffer.shape() - 1;
    for (uInt i=0; i<x.nelements(); i++) {
      start(itsCurveAxis) = i;
      end(itsCurveAxis) = i;
      Array<T> data = buffer(start,end).reform(shp);
      blc[itsAxis1] = Int(x[i]+0.5);
      blc[itsAxis2] = Int(y[i]+0.5);
      if (itsIsRef) {
	data = itsLatticePtr->getSlice (blc, leng, incr);
      } else {
	Bool isRef = itsLatticePtr->getSlice (data, blc, leng, incr);
	AlwaysAssert (!isRef, AipsError);
      }
      itsLatticePtr->getSlice (data, blc, leng, incr);
    }
  }
}
 

template<class T>
void CLIPNearest2D<T>::getMask (Array<Bool>& buffer,
				const Vector<Float>& x,
				const Vector<Float>& y,
				const Slicer& section)
{
  // Determine the shape and positions w.r.t. the original lattice.
  IPosition shp  = itsAxesMap.shapeToOld (buffer.shape());
  shp[itsAxis1] = 1;
  shp[itsAxis2] = 1;
  IPosition blc  = itsAxesMap.posToOld (section.start());
  IPosition leng = itsAxesMap.shapeToOld (section.length());
  leng[itsAxis1] = 1;
  leng[itsAxis2] = 1;
  IPosition incr = itsAxesMap.shapeToOld (section.stride());
  // We have to get the data from the lattice chunk by chunk.
  // Each chunk is determined by a pixel on the curve.
  // If the curve axis is the last axis, we can use ArrayIterator
  // (which is faster (I think) than slicing arrays ourselves).
  if (itsCurveAxis == buffer.ndim() - 1) {
    shp.append (IPosition(1, buffer.shape()[itsCurveAxis]));
    Array<Bool> data = buffer.reform (shp);
    ArrayIterator<Bool> iter(data, shp.nelements()-1);
    for (uInt i=0; i<x.nelements(); i++) {
      blc[itsAxis1] = Int(x[i]+0.5);
      blc[itsAxis2] = Int(y[i]+0.5);
      // Some lattices (e.g. ArrayLattice) return an Array referencing
      // the original data. That would destroy the ArrayIter internals,
      // so we use a reference and copy the data if needed.
      Array<Bool> ref(iter.array());
      Bool isRef = itsLatticePtr->getMaskSlice (ref, blc, leng, incr);
      if (isRef) {
	iter.array() = ref;
      }
      iter.next();
    }
  } else {
    IPosition start = IPosition(buffer.ndim(), 0);
    IPosition end   = buffer.shape() - 1;
    for (uInt i=0; i<x.nelements(); i++) {
      start(itsCurveAxis) = i;
      end(itsCurveAxis) = i;
      Array<Bool> data = buffer(start,end).reform(shp);
      blc[itsAxis1] = Int(x[i]+0.5);
      blc[itsAxis2] = Int(y[i]+0.5);
      Array<Bool> ref(data);
      Bool isRef = itsLatticePtr->getMaskSlice (ref, blc, leng, incr);
      if (isRef) {
	data = ref;
      }
    }
  }
}

} //# NAMESPACE CASACORE - END


#endif
