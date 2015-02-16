//# CurvedLattice2D.cc: A lattice crosscut based on a curve in a plane
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

#ifndef LATTICES_CURVEDLATTICE2D_TCC
#define LATTICES_CURVEDLATTICE2D_TCC

#include <casacore/lattices/Lattices/CurvedLattice2D.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h> 


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
CurvedLattice2D<T>::CurvedLattice2D()
: itsLatticePtr   (0),
  itsInterpolator (0),
  itsAxis1        (0),
  itsAxis2        (0),
  itsCurveAxis    (0)
{}

template<class T>
CurvedLattice2D<T>::CurvedLattice2D (const MaskedLattice<T>& lattice,
				     const CLInterpolator2D<T>& interp,
				     const PixelCurve1D& curve,
				     uInt axis1, uInt axis2, Int curveAxis)
: itsLatticePtr   (lattice.cloneML()),
  itsInterpolator (interp.clone()),
  itsCurve        (curve)
{
  if (lattice.ndim() < 2) {
    throw AipsError ("CurvedLattice2D: input lattice " + lattice.name() +
		     " must have more than 1 dimension");
  }
  makeMapping (axis1, axis2, curveAxis);
  itsInterpolator->set (itsLatticePtr, itsAxesMap,
			itsAxis1, itsAxis2, itsCurveAxis);
}

template<class T>
CurvedLattice2D<T>::CurvedLattice2D (const CurvedLattice2D<T>& other)
: MaskedLattice<T>(),
  itsLatticePtr   (0),
  itsInterpolator (0)
{
  operator= (other);
}

template<class T>
CurvedLattice2D<T>::~CurvedLattice2D()
{
  delete itsLatticePtr;
  delete itsInterpolator;
}

template<class T>
CurvedLattice2D<T>& CurvedLattice2D<T>::operator= 
                                         (const CurvedLattice2D<T>& other)
{
  if (this != &other) {
    delete itsLatticePtr;
    itsLatticePtr = other.itsLatticePtr->cloneML();
    delete itsInterpolator;
    itsInterpolator = other.itsInterpolator->clone();
    itsCurve        = other.itsCurve;
    itsAxis1        = other.itsAxis1;
    itsAxis2        = other.itsAxis2;
    itsCurveAxis    = other.itsCurveAxis;
    itsAxesMap      = other.itsAxesMap;
  }
  return *this;
}

template<class T>
MaskedLattice<T>* CurvedLattice2D<T>::cloneML() const
{
  return new CurvedLattice2D<T> (*this);
}

template<class T>
void CurvedLattice2D<T>::makeMapping (uInt axis1, uInt axis2, Int curveAxis)
{
  uInt ndim = itsLatticePtr->ndim();
  if (axis1 >= ndim  ||  axis2 >= ndim  ||  axis1 == axis2) {
    throw AipsError ("CurvedLattice2D - invalid axis1 or axis2 given");
  }
  itsAxis1 = axis1;
  itsAxis2 = axis2;
  if (curveAxis < 0) {
    itsCurveAxis = ndim - 2;     // last output axis
  } else {
    itsCurveAxis = curveAxis;
  }
  if (itsCurveAxis >= ndim-1) {
    throw AipsError ("CurvedLattice2D - invalid curveAxis given");
  }
  IPosition old2new(ndim, -1);
  uInt nr=0;
  for (uInt i=0; i<ndim; i++) {
    if (nr == itsCurveAxis) nr++;
    if (i != axis1  &&  i != axis2) {
      old2new[i] = nr++;
    }
    if (nr == itsCurveAxis) nr++;
  }
  old2new(axis1) = itsCurveAxis;
  itsAxesMap = AxesMapping (old2new);
}

template<class T>
Bool CurvedLattice2D<T>::isMasked() const
{
  return itsLatticePtr->isMasked();
}

template<class T>
Bool CurvedLattice2D<T>::isPaged() const
{
  return itsLatticePtr->isPaged();
}

template<class T>
Bool CurvedLattice2D<T>::isWritable() const
{
  return False;
}

template<class T>
Bool CurvedLattice2D<T>::lock (FileLocker::LockType type, uInt nattempts)
{
  return itsLatticePtr->lock (type, nattempts);
}
template<class T>
void CurvedLattice2D<T>::unlock()
{
  itsLatticePtr->unlock();
}
template<class T>
Bool CurvedLattice2D<T>::hasLock (FileLocker::LockType type) const
{
  return itsLatticePtr->hasLock (type);
}
template<class T>
void CurvedLattice2D<T>::resync()
{
  itsLatticePtr->resync();
}

template<class T>
void CurvedLattice2D<T>::flush()
{
  itsLatticePtr->flush();
}

template<class T>
void CurvedLattice2D<T>::tempClose()
{
  itsLatticePtr->tempClose();
}

template<class T>
void CurvedLattice2D<T>::reopen()
{
  itsLatticePtr->reopen();
}


template<class T>
const LatticeRegion* CurvedLattice2D<T>::getRegionPtr() const
{
  return 0;
}

template<class T>
IPosition CurvedLattice2D<T>::shape() const
{
  IPosition shp (itsLatticePtr->shape());
  shp(itsAxis1) = itsCurve.npoints();
  shp[itsAxis2] = 1;
  return itsAxesMap.shapeToNew (shp);
}

template<class T>
String CurvedLattice2D<T>::name (Bool stripPath) const
{
  return itsLatticePtr->name(stripPath);
}

template<class T>
Bool CurvedLattice2D<T>::doGetSlice (Array<T>& buffer,
				     const Slicer& section)
{
  // Convert the curve pixel numbers to lattice pixel numbers.
  Vector<Float> x,y;
  itsCurve.getPixelCoord (x, y,
			  section.start()[itsCurveAxis],
			  section.end()[itsCurveAxis],
			  section.stride()[itsCurveAxis]);
  // Let the interpolator get all pixels for the given section.
  buffer.resize (section.length());
  itsInterpolator->getData (buffer, x, y, section);
  return False;
}

template<class T>
void CurvedLattice2D<T>::doPutSlice (const Array<T>&,
				     const IPosition&, 
				     const IPosition&)
{
  throw (AipsError ("CurvedLattice2D::putSlice - non-writable lattice"));
}


template<class T>
uInt CurvedLattice2D<T>::advisedMaxPixels() const
{
  return itsLatticePtr->advisedMaxPixels();
}

template<class T>
IPosition CurvedLattice2D<T>::doNiceCursorShape (uInt maxPixels) const
{
  IPosition cursorShape (itsLatticePtr->niceCursorShape (maxPixels));
  cursorShape[itsAxis1] = 1;
  cursorShape[itsAxis2] = 1;
  return itsAxesMap.shapeToNew (cursorShape);
}

template<class T>
Bool CurvedLattice2D<T>::doGetMaskSlice (Array<Bool>& buffer,
					 const Slicer& section)
{
  buffer.resize (section.length());
  // Evaluate only if masked.
  if (itsLatticePtr->isMasked()) {
    // Convert the curve pixel numbers to lattice pixel numbers.
    Vector<Float> x,y;
    itsCurve.getPixelCoord (x, y,
			    section.start()[itsCurveAxis],
			    section.end()[itsCurveAxis],
			    section.stride()[itsCurveAxis]);
    // Let the interpolator get all mask pixels for the given section.
    itsInterpolator->getMask (buffer, x, y, section);
  } else {
    // Not masked, so we can simply fill the buffer with True values.
    buffer = True;
  }
  return False;
}


template <class T>
Bool CurvedLattice2D<T>::ok() const
{
  return itsLatticePtr->ok();
}

} //# NAMESPACE CASACORE - END


#endif
