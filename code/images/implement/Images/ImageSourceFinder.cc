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
   blc(pixelAxes(0)) -= 3;
   blc(pixelAxes(1)) -= 3;
   trc(pixelAxes(0)) += 3;
   trc(pixelAxes(1)) += 3;
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

// Results matrix

   Matrix<NumericTraits<T>::PrecisionType> rs(nMax, 3);    // flux, x, y
   Matrix<NumericTraits<T>::PrecisionType> ss(nMax, 3);    // maj, min, pa
   rs = 0.0;
   ss = 0.0;
    
// Assume only positive
    
   Double asign(1.0);

// Fitting data
 
   NumericTraits<T>::PrecisionType mat[3][3];
   FitLSQ fit(6);
   Vector<T> gel(6);
   uInt rank;
   Vector<T> sol(6);
   T sd, mu;
   
// Input data arrays

   IPosition inShape = image.shape();
   uInt nx(inShape(0));
   uInt ny(inShape(1));
   if (ny <=3) {
     os << "Need at least 3 rows in image" << LogIO::EXCEPTION;
   }  
//
   IPosition inSliceShape(2, nx, 1);
   Block<COWPtr<Array<T> > > inPtr(3);
   Block<COWPtr<Array<Bool> > > inMaskPtr(3);
   Matrix<Bool> inDone(3,nx);
   inDone = False;
   for (uInt i=0; i<3; i++) {
     inPtr[i] = COWPtr<Array<T> >(new Array<T>(inSliceShape));
     inMaskPtr[i] = COWPtr<Array<Bool> >(new Array<Bool>(inSliceShape));
   }
   Int inp(0);
//
   IPosition start(inShape);
   start = 0;  
      
// Read first line and set mask  

   Bool isRef, isMaskRef;   
   isRef = image.getSlice(inPtr[inp], Slicer(start, inSliceShape), True);
   isMaskRef = image.getMaskSlice(inMaskPtr[inp], Slicer(start, inSliceShape),
                                  True);
   for (uInt i0=0; i0<nx; i0++) inDone(inp, i0) =
				  inMaskPtr[inp].ref()(IPosition(1, i0));
   start(1) += 1;
   
// Read 2nd line and set mask

   isRef = image.getSlice(inPtr[inp+1], Slicer(start, inSliceShape), True);
   isMaskRef = image.getMaskSlice(inMaskPtr[inp+1], Slicer(start, inSliceShape),
				     True);
   for (uInt i0=0; i0<nx; i0++) {
      inDone(inp+1, i0) = inMaskPtr[inp+1].ref()(IPosition(1, i0));
   }
   start(1) += 1;
   
// Do all remaining lines  
               
   for (uInt i=2; i<ny; i++) {
      inp++;
      inp %= 3;
      isRef = image.getSlice(inPtr[(inp+1)%3],
                             Slicer(start, inSliceShape), True);
      isMaskRef = image.getMaskSlice(inMaskPtr[(inp+1)%3],
                                     Slicer(start, inSliceShape), True);
      for (uInt i0=0; i0<nx; i0++) inDone((inp+1)%3, i0) =
				     !(inMaskPtr[(inp+1)%3].ref()(IPosition(1, i0)));
      start(1) += 1;
         
// All points

      for (uInt j=1; j<nx-1; j++) {
         if (inDone(inp, j)) continue;             // point already used or masked
//
         NumericTraits<T>::PrecisionType x(inPtr[inp].ref()(IPosition(1,j)));
         if (absFind) {                            // find pos/neg
            asign = (x<0) ? -1.0 : 1.0;
            x = abs(x);
         }  
         if (x<0.8*cutoff*abs(rs(0,0)) ||
             x<0.8*abs(rs(nMax-1,0))) continue;      // too small
         
// Make local data field
            
         Bool xt(False);
         for (Int i0=-1; i0<2; i0++) {
            for (Int i1=-1; i1<2; i1++) {
               if (inDone((inp+i0+3)%3, j+i1)) {    // already used
                  xt = True; 
                  break;
               }
               mat[i0+1][i1+1] = inPtr[(inp+i0+3)%3].ref()(IPosition(1, j+i1));
               mat[i0+1][i1+1] *= asign;            // make abs
            }
            if (xt) break;
         }
         if (xt) continue;
                     
// Test if a local peak
                
         if (x<=abs(mat[0][1]) || x<=abs(mat[2][1]) ||
             x<=abs(mat[1][0]) || x<=abs(mat[1][2])) continue;

// Solve general ellipsoid
    
         fit.set(6);
         for (Int i0=-1; i0<2; i0++) {
            for (Int i1=-1; i1<2; i1++) {
               gel(0)= 1;
               gel(1) = i0;
               gel(2) = i1;
               gel(3) = i0*i0;
               gel(4) = i1*i1;
               gel(5) = i0*i1;
               fit.makeNorm(gel, 1.0 - 0.5*(abs(i1)+abs(i0)) + 0.25*abs(i0*i1),
                            mat[i0+1][i1+1]);
            }
         }
         if (!fit.invert(rank)) continue;         // Cannot solve
         fit.solve(sol, sd, mu);
   
// Find max

         NumericTraits<T>::PrecisionType r1(sol(5)*sol(5) - 4*sol(3)*sol(4));       // dx
         if (r1 == 0) continue;                            // forget
         NumericTraits<T>::PrecisionType r0((2*sol(2)*sol(3) - sol(1)*sol(5))/r1);  // dy
         r1 = (2*sol(1)*sol(4) - sol(2)*sol(5))/r1;
         if (abs(r0)>1 || abs(r1)>1) continue;             // too far away from peak
   
// Amplitude
   
         sol(0) += sol(1)*r0 + sol(2)*r1 + sol(3)*r0*r0 + sol(4)*r1*r1 + sol(5)*r0*r1;
         x = sol(0);
         if (absFind) {
            x = abs(x);
            sol(0) = asign*sol(0);
         }
         if (x<cutoff*abs(rs(0,0))) continue;             // too small
         for (Int k=0; k<nMax; k++) {
            if (x>=rs(k,0)) {
               for (Int l=nMax-1; l>k; l--) {
                  for (uInt i0=0; i0<3; i0++) rs(l,i0) = rs(l-1,i0);
               }
               rs(k,0) = sol(0);                      // Peak
               rs(k,1) = i+r1-1;                      // Y
               rs(k,2) = j+r0;                        // X
//
               ss(k,0) = 3.01;                        // major
               ss(k,1) = 3.0;                         // minor
               ss(k,2) = 0.0;                         // pa
//
               for (Int l=-1; l<2; l++) {
                  for (Int m=-1; m<2; m++) {
                     inDone((inp+l+3)%3, j+m) = True;
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
   for (Int i=0; i<nMax; i++) {
     if (abs(rs(i,0)) < x || rs(i,0) == 0) break;
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
      const ImageInfo& ii = image.imageInfo();
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
      for (Int i=0; i<nFound; i++) {
         pars(0) = rs(i,0);
         pars(1) = rs(i,2);
         pars(2) = rs(i,1);

//
         if (!doPoint) {
            pars(3) = ss(i,0);
            pars(4) = ss(i,1);
            pars(5) = ss(i,2);
         }
// 
         listOut.add(ImageUtilities::encodeSkyComponent (os, rat, ii, cSys, bU,
                                                         cType, pars, stokes, xIsLong));
      }
   } 
//
   return listOut;
}
