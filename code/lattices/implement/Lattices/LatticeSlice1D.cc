//# LatticeSlice1D.cc: 1-D slice from a Lattice
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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


#include <trial/Lattices/LatticeSlice1D.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <trial/Lattices/MaskedLattice.h>
#include <trial/Lattices/PixelCurve1D.h>
#include <aips/Mathematics/Math.h>
#include <trial/Mathematics/Interpolate2D.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


template <class T>
LatticeSlice1D<T>::LatticeSlice1D(const MaskedLattice<T>& lattice, Method method)
: itsLatticePtr(lattice.cloneML())
{
  makeInterpolator (method);
  itsPos.resize(2);
}


template<class T>
LatticeSlice1D<T>::LatticeSlice1D (const LatticeSlice1D<T>& other)
: itsLatticePtr(0),
  itsInterp(0)
{
  operator= (other);
}

template<class T>
LatticeSlice1D<T>::~LatticeSlice1D()
{
   delete itsLatticePtr;
   itsLatticePtr = 0;
//
   delete itsInterp;
   itsInterp = 0;
}

template<class T>
LatticeSlice1D<T>& LatticeSlice1D<T>::operator=(const LatticeSlice1D<T>& other)
{
  if (this != &other) {
    delete itsLatticePtr;
    itsLatticePtr = other.itsLatticePtr->cloneML();
//
    delete itsInterp;
    makeInterpolator (other.interpolationMethod());
//
    itsPos.resize(0);
    itsPos = other.itsPos;
//
    itsX.resize(0);
    itsX = other.itsX;
    itsY.resize(0);
    itsY = other.itsY;
//
    itsAxis0 = other.itsAxis0;    
    itsAxis1 = other.itsAxis1;
  }
  return *this;
}

template<class T>
void LatticeSlice1D<T>::getSlice (Vector<T>& data, Vector<Bool>& mask,
                                  const PixelCurve1D& curve, uInt axis0, uInt axis1,
                                  const IPosition& blc, const IPosition& trc)
{

// Check PixelCurve is in lattice domain, set blc/trc in plane
// and set x,y vectors

   itsAxis0 = axis0;
   itsAxis1 = axis1;
   IPosition blcFull(blc), trcFull(trc);
   checkCurve (blcFull, trcFull, curve);

// Get Slice

   doGetSlice (data, mask, curve, blcFull, trcFull);
}



template<class T>
void LatticeSlice1D<T>::getSlice (Vector<T>& data, Vector<Bool>& mask,
                                  const IPosition& blc, const IPosition& trc,
                                  uInt nPts) 
{

// Find plane of slice

   findPlane (blc, trc);

// Generate PixelCurve

   double x1(blc(itsAxis0)), x2(trc(itsAxis0));   
   double y1(blc(itsAxis1)), y2(trc(itsAxis1));
   PixelCurve1D curve(x1, y1, x2, y2, nPts);
   curve.getPixelCoord (itsX, itsY, 0, curve.npoints()-1, 1);

// Get Slice

   doGetSlice (data, mask, curve, blc, trc);
}


template<class T>
void LatticeSlice1D<T>::getPosition (uInt& axis0, uInt& axis1,
                                     Vector<Float>& x, Vector<Float>& y,
                                     Vector<Float>& distance) const
{
   x.resize(0);
   x = itsX;
   y.resize(0);
   y = itsY;
//
   distance.resize(x.nelements());
   distance[0] = 0.0;
   for (uInt i=1; i<x.nelements(); i++) {
      distance[i] = sqrt(square(x[i]-x[i-1]) + square(y[i]-y[i-1])) + distance[i-1];
   }
//
   axis0 = itsAxis0;
   axis1 = itsAxis1;
}


// Private


template<class T>
void LatticeSlice1D<T>::checkCurve (IPosition& blc, IPosition& trc, 
                                    const PixelCurve1D& curve)
{

// Check

   const uInt nDim = itsLatticePtr->ndim();
   if (blc.nelements() != nDim) {
      throw(AipsError("blc must be of length number of image dimensions"));
   }
   if (trc.nelements() != nDim) {
      throw(AipsError("trc must be of length number of image dimensions"));
   }

// Check curve in domain of lattice [-0.5 -> shape-0.5]

   const IPosition shape = itsLatticePtr->shape();
   const uInt nPts = curve.npoints();
   curve.getPixelCoord (itsX, itsY, 0u, nPts-1, 1u);
   if (itsX[0]<-0.5 || itsY[0]<-0.5) {
      throw(AipsError("x or y start of curve falls outside of lattice"));
   }
   if (itsX[nPts-1]>(shape(itsAxis0)-0.5) || itsY[nPts-1]>(shape(itsAxis1)-0.5)) {
      throw(AipsError("x or y end of curve falls outside of lattice"));
   }

// Set plane blc/trc

   blc(itsAxis0) = 0;
   blc(itsAxis1) = 0;
   trc(itsAxis0) = shape(itsAxis0) - 1;
   trc(itsAxis1) = shape(itsAxis1) - 1;

// Presently the blc and trc must be in a plane

   uInt n = 0;
   for (uInt i=0; i<shape.nelements(); i++) {
      if (trc(i)-blc(i) > 0) n++;
   }
   if (n > 2) {
      throw (AipsError("blc & trc must lie in a plane"));
   }
}



template<class T>
void LatticeSlice1D<T>::findPlane (const IPosition& blc, 
                                   const IPosition& trc)
{
   const uInt nDim = itsLatticePtr->ndim();
//
   if (blc.nelements() != nDim) {
      throw(AipsError("blc must be of length number of image dimensions"));
   }
   if (trc.nelements() != nDim) {
      throw(AipsError("trc must be of length number of image dimensions"));
   }

// Find the plane; first two axes with non-unit shapes are
// assumed to hold the plane to extract the slice from.

   IPosition shape = trc - blc + 1;
   Int axis0 = -1;
   Int axis1 = -1;
   uInt n = 0;
   for (uInt i=0; i<shape.nelements(); i++) {
      if (shape(i) > 1) {
         n++;
         if (axis0==-1) {
            axis0 = i;
         } else {
            if (axis1==-1) axis1 = i;
         }
      }
   }
//
   if (n > 2) {
      throw (AipsError("blc & trc must lie in a plane"));
   }
//
   if (axis0==-1 || axis1==-1) {
      throw (AipsError("Could not find plane of slice from blc/trc coordinates"));
   }
//
   itsAxis0 = axis0;
   itsAxis1 = axis1;
}



template<class T>
void LatticeSlice1D<T>::doGetSlice (Vector<T>& data, Vector<Bool>& mask,
                                    const PixelCurve1D& curve, 
                                    const IPosition& blc, const IPosition& trc)
{

// Get plane holding slice - we know the returned Arrays will be 
// 2D (checked) so can assign to Matrix safely

   const IPosition shape = trc - blc + 1;
   const Matrix<T>& dataIn = itsLatticePtr->getSlice (blc, shape, True);
   const Matrix<Bool>& maskIn = itsLatticePtr->getMaskSlice (blc, shape, True);

// Interpolate

   const uInt nPts = itsX.nelements();
   T result;
   data.resize(nPts);
   mask.resize(nPts);
   for (uInt i=0; i<nPts; i++) {
      itsPos[0] = itsX[i];
      itsPos[1] = itsY[i];
      mask[i] = itsInterp->interp (result, itsPos, dataIn, maskIn);
      data[i] = result;
   }
}


template<class T>
void LatticeSlice1D<T>::makeInterpolator (Method method)
{
  if (method==NEAREST) {
     itsInterp = new Interpolate2D(Interpolate2D::NEAREST);
  } else if (method==LINEAR) {
     itsInterp = new Interpolate2D(Interpolate2D::LINEAR);
  } else if (method==CUBIC) {
     itsInterp = new Interpolate2D(Interpolate2D::CUBIC);
  }
  itsMethod = method;
}


template<class T>
typename LatticeSlice1D<T>::Method LatticeSlice1D<T>::stringToMethod (const String& method)
{
   String typeU = method;
   typeU.upcase();
//
   Method method2;
   String tmp = String(typeU.at(0,1));
   if (tmp==String("N")) {
      method2 = NEAREST;
   } else if (tmp==String("L")) {
      method2 = LINEAR;
   } else if (tmp==String("C")) {
      method2 = CUBIC;
   } else {
      throw (AipsError("Illegal interpolation method"));
   }
//
   return method2;
}
