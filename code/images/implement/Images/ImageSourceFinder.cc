//# ImageSourceFinder.cc:  find sources
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001
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
//
#include <trial/Images/ImageSourceFinder.h>

#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Slicer.h>
#include <trial/Arrays/AxesSpecifier.h>
#include <aips/Containers/Block.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ComponentList.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <aips/Fitting/FitLSQ.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageInfo.h>
#include <trial/Images/ImageUtilities.h>
#include <trial/Images/SubImage.h>
#include <trial/Lattices/LatticeStatistics.h>
#include <trial/Lattices/LCBox.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/NumericTraits.h> 
#include <aips/Measures/Stokes.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/Unit.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Mathematics/Math.h>



template <class T>
ImageSourceFinder<T>::ImageSourceFinder (const ImageInterface<T>& image)
: pImage_p(image.cloneII())
{}

template <class T> 
ImageSourceFinder<T>::ImageSourceFinder (const ImageSourceFinder<T> &other)
: pImage_p(other.pImage_p->cloneII())
{}

template <class T> 
ImageSourceFinder<T>::~ImageSourceFinder ()
{ 
   delete pImage_p;
}

template <class T>
ImageSourceFinder<T> &ImageSourceFinder<T>::operator=(const ImageSourceFinder<T> &other)
{
   if (this != &other) {
      delete pImage_p;
      pImage_p = other.pImage_p->cloneII();
   }
   return *this;
}


template <class T> 
Bool ImageSourceFinder<T>::setNewImage (const ImageInterface<T>& image)
//
// Reassign pointer.  
//
{
   const ImageInterface<T>* pTemp;
   pTemp = &image;
   if (pTemp == 0) {
      return False;
   } else {
      pImage_p = pTemp;
      return True;
   }
}

template <class T>
ComponentList ImageSourceFinder<T>::findSources (LogIO& os, Int nMax, 
                                                 Double cutoff, Bool absFind,
                                                 Bool doPoint)
{
   return findSources(os, *pImage_p, nMax, cutoff, absFind, doPoint);
}


template <class T>
SkyComponent ImageSourceFinder<T>::findSourceInSky (LogIO& os, Vector<Double>& absPixel,
                                                    Double cutoff, Bool absFind, Bool doPoint)
{

// Find sky
   
   Int dC;
   String errorMessage;
   Vector<Int> pixelAxes, worldAxes;
   CoordinateSystem cSys = pImage_p->coordinates();
   if (!CoordinateUtil::findSky(errorMessage, dC, pixelAxes,
                                worldAxes, cSys)) {
     os << errorMessage << LogIO::EXCEPTION;
   } 

// Find maximum/minimum
   
   LatticeStatistics<T> stats(*pImage_p, os, True);
   IPosition minPos, maxPos;
   if (!stats.getMinMaxPos(minPos, maxPos)) {
      os << stats.errorMessage() << LogIO::EXCEPTION;
   }
      
// Make new little SubImage around location in plane of sky
         
   IPosition shape = pImage_p->shape();
   const uInt nDim = pImage_p->ndim();
// 
   absPixel.resize(nDim);
   IPosition blc;
   IPosition trc;
   if (absFind) {

// Find positive only

      blc = maxPos;
      trc = maxPos;
      for (uInt i=0; i<nDim; i++) absPixel(i) = maxPos(i);
   } else {

// Find positive or negative only
   
      Float valueMin = pImage_p->getAt(minPos);
      Float valueMax = pImage_p->getAt(maxPos);
//
      if (abs(valueMax) > abs(valueMin)) {
         blc = maxPos;
         trc = maxPos;
         for (uInt i=0; i<nDim; i++) absPixel(i) = maxPos(i);
      } else {
         blc = minPos;
         trc = minPos;
         for (uInt i=0; i<nDim; i++) absPixel(i) = maxPos(i);
      }
   }
// 
   blc(pixelAxes(0)) -= 5;
   blc(pixelAxes(1)) -= 5;
   trc(pixelAxes(0)) += 5;
   trc(pixelAxes(1)) += 5;
//
   IPosition inc(nDim,1);
   LCBox::verify (blc, trc, inc, shape);
   Slicer slicer(blc, trc, inc, Slicer::endIsLast);
   AxesSpecifier axesSpec(False);   // drop degenerate
   const SubImage<T> subImage(*pImage_p, slicer, axesSpec);
   
// Find one source

   const uInt nMax = 1;
   ComponentList list = findSources (os, subImage, nMax, cutoff, absFind, doPoint);
//
   SkyComponent sky = list.component(0);
//
   DirectionCoordinate dCoord = cSys.directionCoordinate(dC);
   MDirection mDir = sky.shape().refDirection();
   Vector<Double> dirPixel(2);
   if (!dCoord.toPixel(dirPixel, mDir)) {
     os << dCoord.errorMessage() << LogIO::EXCEPTION;
   }
//
   absPixel(pixelAxes(0)) = dirPixel(0);
   absPixel(pixelAxes(1)) = dirPixel(1);
//
   return sky;
}




template <class T>
ComponentList ImageSourceFinder<T>::findSources (LogIO& os, 
                                                 const ImageInterface<T>& image,
                                                 Int nMax, 
                                                 Double cutoff, Bool absFind,
                                                 Bool doPoint)
{

// Make sure the Image is 2D and that it holds the sky.  Exception if not.

   const CoordinateSystem& cSys = image.coordinates();
   Bool xIsLong = CoordinateUtil::isSky(os, cSys);

// Width support

   Int w = 5;
   Int off = 2;
   Int off2 = 1;
   if (doPoint) {
      w = 3;
      off = 1;
      off2 = 0;
   }

   NumericTraits<T>::PrecisionType **mat = 0;
   mat = new NumericTraits<T>::PrecisionType*[w];
   for(Int i=0;i<w;i++){
      mat[i] = new NumericTraits<T>::PrecisionType[w];
   }

// Results matrix

   Matrix<NumericTraits<T>::PrecisionType> rs(nMax, 3);    // flux, x, y
   Matrix<NumericTraits<T>::PrecisionType> ss(nMax, 3);    // maj, min, pa
   rs = 0.0;
   ss = 0.0;
    
// Assume only positive
    
   Double asign(1.0);

// Fitting data
 
   FitLSQ fit(6);
   Vector<T> gel(6);
   uInt rank;
   Vector<T> sol(6);
   T sd, mu;
   
// Input data arrays

   IPosition inShape = image.shape();
   Int nx = inShape(0);
   Int ny = inShape(1);
   if (ny <= w) {
     os << "Need at least " << w << " rows in image" << LogIO::EXCEPTION;
   }  
//
   IPosition inSliceShape(2, nx, 1);
   Block<COWPtr<Array<T> > > inPtr(w);
   Block<COWPtr<Array<Bool> > > inMaskPtr(w);
   Matrix<Bool> inDone(w,nx);
   inDone = False;
   for (Int j=0; j<w; j++) {
     inPtr[j] = COWPtr<Array<T> >(new Array<T>(inSliceShape));
     inMaskPtr[j] = COWPtr<Array<Bool> >(new Array<Bool>(inSliceShape));
   }
      
// Read first w-1 lines 

   Int inp = 0;
   IPosition start(inShape);
   start = 0;  
   IPosition pos(1,0);
   Bool isRef, isMaskRef;   
//
   for (Int j=0; j<(w-1); j++) {
      isRef = image.getSlice(inPtr[inp+j], Slicer(start, inSliceShape), True);
      isMaskRef = image.getMaskSlice(inMaskPtr[inp+j], Slicer(start, inSliceShape), True);
      for (Int i=0; i<nx; i++) {
         pos(0) = i;
         inDone(inp+j, i) = inMaskPtr[inp+j].ref()(pos);
      }
      start(1) += 1;
   }
   
   
// Loop through remaining lines  
               
   for (Int j=(w-1); j<ny; j++) {
      inp++;
      inp %= w;
      isRef = image.getSlice(inPtr[(inp+1)%w], Slicer(start, inSliceShape), True);
      isMaskRef = image.getMaskSlice(inMaskPtr[(inp+1)%w], Slicer(start, inSliceShape), True);
      for (Int i=0; i<nx; i++) {
         pos(0) = i;
         inDone((inp+1)%w, i) = !(inMaskPtr[(inp+1)%w].ref()(pos));
      }
      start(1) += 1;
         
// All points

      for (Int i=off; i<(nx-off); i++) {
         if (inDone(inp, i)) continue;             // point already used or masked
//
         pos(0) = i;
         NumericTraits<T>::PrecisionType v(inPtr[inp].ref()(pos));
         if (absFind) {                            // find pos/neg
            asign = (v<0) ? -1.0 : 1.0;
            v = abs(v);
         }  
         if (v<0.8*cutoff*abs(rs(0,0)) ||
             v<0.8*abs(rs(nMax-1,0))) continue;      // too small
         
// Make local data field
            
         Bool xt = False;
         for (Int jj=-off; jj<(off+1); jj++) {
            for (Int ii=-off; ii<(off+1); ii++) {
               if (inDone((inp+jj+w)%w, i+ii)) {    // already used
                  xt = True; 
                  break;
               }
//
               pos(0) = i+ii;
               mat[jj+off][ii+off] = inPtr[(inp+jj+w)%w].ref()(pos);
               mat[jj+off][ii+off] *= asign;            // make abs
            }
            if (xt) break;
         }
         if (xt) continue;
                     
// Test if a local peak
                
         if (v<=abs(mat[0+off2][1+off2]) || v<=abs(mat[2+off2][1+off2]) ||
             v<=abs(mat[1+off2][0+off2]) || v<=abs(mat[1+off2][2+off2])) continue;

// Solve general ellipsoid
    
         fit.set(6);
         for (Int jj=-off; jj<(off+1); jj++) {
            for (Int ii=-off; ii<(off+1); ii++) {
               gel(0)= 1;
               gel(1) = jj;
               gel(2) = ii;
               gel(3) = jj*jj;
               gel(4) = ii*ii;
               gel(5) = jj*ii;
               fit.makeNorm(gel, 1.0 - 0.5*(abs(ii)+abs(jj)) + 0.25*abs(jj*ii),
                            mat[jj+off][ii+off]);
            }
         }
//
         if (!fit.invert(rank)) continue;        // Cannot solve
         fit.solve(sol, sd, mu);
   
// Find max

         NumericTraits<T>::PrecisionType r1(sol(5)*sol(5) - 4*sol(3)*sol(4));       // dx
         if (r1 == 0) continue;                            // forget
         NumericTraits<T>::PrecisionType r0((2*sol(2)*sol(3) - sol(1)*sol(5))/r1);  // dy
         r1 = (2*sol(1)*sol(4) - sol(2)*sol(5))/r1;
         if (abs(r0)>1 || abs(r1)>1) continue;             // too far away from peak
   
// Amplitude
   
         sol(0) += sol(1)*r0 + sol(2)*r1 + sol(3)*r0*r0 + sol(4)*r1*r1 + sol(5)*r0*r1;
         v = sol(0);
         if (absFind) {
            v = abs(v);
            sol(0) = asign*sol(0);
         }
         if (v<cutoff*abs(rs(0,0))) continue;             // too small
//
         for (Int k=0; k<nMax; k++) {
            if (v>=rs(k,0)) {
               for (Int l=nMax-1; l>k; l--) {
                  for (uInt i0=0; i0<3; i0++) {
                     rs(l,i0) = rs(l-1,i0);
                     ss(l,i0) = ss(l-1,i0);
                  }
               }
//
               rs(k,0) = sol(0);                      // Peak
               rs(k,1) = j+r1-1;                      // Y
               rs(k,2) = i+r0;                        // X

// Wim will fill in these three numbers.

               ss(k,0) = 3.01;                        // major
               ss(k,1) = 3.0;                         // minor
               ss(k,2) = 0.0;                         // pa
//
               for (Int jj=-off; jj<(off+1); jj++) {
                  for (Int ii=-off; ii<(off+1); ii++) {
                     inDone((inp+jj+w)%w, i+ii) = True;
                  }
               }
               break;
            }
         }
      }
   }
                       
// Find the number filled
                       
   Int nFound = 0;
   NumericTraits<T>::PrecisionType x = cutoff*abs(rs(0,0));
   for (Int k=0; k<nMax; k++) {
     if (abs(rs(k,0)) < x || rs(k,0) == 0) break;
     nFound++;   
   }      
   
// What Stokes is the plane we are finding in ?
      
   Stokes::StokesTypes stokes(Stokes::Undefined);
   stokes = CoordinateUtil::findSingleStokes (os, cSys, 0);
  
// Fill SkyComponents

   ComponentList listOut;
   if (nFound==0) {
      os << LogIO::WARN << "No sources were found" << LogIO::POST;
   } else {
      os << LogIO::NORMAL << "Found " << nFound << " sources" << LogIO::POST;
      const ImageInfo& info = image.imageInfo();
      const Unit& bU = image.units();
      Double rat;
//
      Vector<Double> pars;
      ComponentType::Shape cType;
      if (doPoint) {
        cType = ComponentType::POINT;
        pars.resize(3);
      } else {
        cType = ComponentType::GAUSSIAN;
        pars.resize(6);
      }
//
      for (Int k=0; k<nFound; k++) {
         pars(0) = rs(k,0);
         pars(1) = rs(k,2);            // x & y flipped
         pars(2) = rs(k,1);

//
         if (!doPoint) {
            pars(3) = ss(k,0);
            pars(4) = ss(k,1);
            pars(5) = ss(k,2);
         }
// 
         listOut.add(ImageUtilities::encodeSkyComponent (os, rat, info, cSys, bU,
                                                         cType, pars, stokes, xIsLong));
      }
   } 
//
   delete [] mat;
   return listOut;
}
