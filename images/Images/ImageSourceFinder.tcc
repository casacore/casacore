//# ImageSourceFinder.cc:  find sources
//# Copyright (C) 1995-2003,2004
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
#include <images/Images/ImageSourceFinder.h>

#include <casa/aips.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Slicer.h>
#include <casa/Arrays/AxesSpecifier.h>
#include <casa/Containers/Block.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <coordinates/Coordinates/StokesCoordinate.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <components/ComponentModels/ComponentShape.h>
#include <components/ComponentModels/ComponentList.h>
#include <components/ComponentModels/SkyComponent.h>
#include <scimath/Fitting/LSQaips.h>
#include <scimath/Fitting/NonLinearFitLM.h>
#include <lattices/LatticeMath/Fit2D.h>
#include <scimath/Functionals/Gaussian2D.h>
#include <images/Images/ImageInterface.h>
#include <images/Images/ImageInfo.h>
#include <images/Images/ImageUtilities.h>
#include <images/Images/SubImage.h>
#include <lattices/Lattices/LatticeStatistics.h>
#include <lattices/Lattices/LCBox.h>
#include <casa/Logging/LogIO.h>
#include <scimath/Mathematics/AutoDiff.h> 
#include <scimath/Mathematics/NumericTraits.h> 
#include <measures/Measures/Stokes.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/Unit.h>
#include <casa/Utilities/COWPtr.h>
#include <casa/BasicMath/Math.h>
#include <casa/iostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN

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
                                                 Bool doPoint, Int width)
{
   return findSources(os, *pImage_p, nMax, cutoff, absFind, doPoint, width);
}



template <class T>
SkyComponent ImageSourceFinder<T>::findSourceInSky (LogIO& os, Vector<Double>& absPixel,
                                                    Double cutoff, Bool absFind, 
                                                    Bool doPoint, Int width)
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
      
// Make SubImage of plane of sky holding maximum or minimum
         
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
   blc(pixelAxes(0)) = 0;
   blc(pixelAxes(1)) = 0;
   trc(pixelAxes(0)) = shape(pixelAxes(0))-1;
   trc(pixelAxes(1)) = shape(pixelAxes(1))-1;
//
   IPosition inc(nDim,1);
   LCBox::verify (blc, trc, inc, shape);
   Slicer slicer(blc, trc, inc, Slicer::endIsLast);
   AxesSpecifier axesSpec(False);   // drop degenerate
   const SubImage<T> subImage(*pImage_p, slicer, axesSpec);
   
// Find one source

   const uInt nMax = 1;
   ComponentList list = findSources (os, subImage, nMax, cutoff, absFind, 
                                     doPoint, width);
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
                                                 Bool doPoint, Int width)
{
// Output

   ComponentList listOut;

// Make sure the Image is 2D and that it holds the sky.  Exception if not.

   const CoordinateSystem& cSys = image.coordinates();
   Bool xIsLong = CoordinateUtil::isSky(os, cSys);

// Width support for fast source finder.
// Can go to w/off/off2 = 5/2/1 but craps out if bigger.

   Int w = 3;
   Int off = 1;
   Int off2 = 0;

// Results matrix

   Matrix<typename NumericTraits<T>::PrecisionType> mat(w,w);
   Matrix<typename NumericTraits<T>::PrecisionType> rs(nMax, 3);  // flux, x, y
   rs = 0.0;
    
// Assume only positive
    
   Double asign(1.0);

// Fitting data
 
   LSQaips fit(6);
   Vector<T> gel(6);
   uInt rank;
   Vector<T> sol(6);
   
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
         typename NumericTraits<T>::PrecisionType v(inPtr[inp].ref()(pos));
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
               mat(jj+off,ii+off) = inPtr[(inp+jj+w)%w].ref()(pos);
               mat(jj+off,ii+off) *= asign;            // make abs
            }
            if (xt) break;
         }
         if (xt) continue;
                     
// Test if a local peak

         if (v<=abs(mat(0+off2,1+off2)) || v<=abs(mat(2+off2,1+off2)) ||
             v<=abs(mat(1+off2,0+off2)) || v<=abs(mat(1+off2,2+off2))) continue;

// Solve general ellipsoid
    
         Int k = 0;
         fit.set(6);
         for (Int jj=-off; jj<(off+1); jj++) {
            for (Int ii=-off; ii<(off+1); ii++) {
               gel(0)= 1;
               gel(1) = jj;
               gel(2) = ii;
               gel(3) = jj*jj;
               gel(4) = ii*ii;
               gel(5) = jj*ii;
               fit.makeNorm(gel.data(),
			    1.0 - 0.5*(abs(ii)+abs(jj)) + 0.25*abs(jj*ii),
                            mat(jj+off,ii+off));
               k++;
            }
         }
//
         if (!fit.invert(rank)) continue;        // Cannot solve
         fit.solve(sol);
   
// Find max

         typename NumericTraits<T>::PrecisionType r1(sol(5)*sol(5) - 4*sol(3)*sol(4));       // dx
         if (r1 == 0) continue;                            // forget
         typename NumericTraits<T>::PrecisionType r0((2*sol(2)*sol(3) - sol(1)*sol(5))/r1);  // dy
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
                  }
               }
//
               rs(k,0) = sol(0);                      // Peak
               rs(k,1) = i+r0;                        // X
               rs(k,2) = j+r1-1;                      // Y
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
   typename NumericTraits<T>::PrecisionType x = cutoff*abs(rs(0,0));
   for (Int k=0; k<nMax; k++) {
     if (abs(rs(k,0)) < x || rs(k,0) == 0) break;
     nFound++;   
   }      
//
   if (nFound==0) {
      os << LogIO::WARN << "No sources were found" << LogIO::POST;
      return listOut;
   }

// Generate more accurate fit if required giveing shape information

   Matrix<typename NumericTraits<T>::PrecisionType> ss(nFound, 3);    // major, minor, pa
   Matrix<typename NumericTraits<T>::PrecisionType> rs2(rs.copy());   // copy
//
   if (!doPoint) {

// Loop over found sources

      for (Int k=0; k<nFound; k++) {
         if (width <= 0) {

// This means we want just the default shape estimates only

            ss(k,0) = width;
            ss(k,1) = width;
            ss(k,2) = 0.0;
         } else {

// See if we can do this source

            Int iCen = Int(rs(k,1));
            Int jCen = Int(rs(k,2));
            IPosition blc0(image.ndim(),0);
            IPosition trc0(image.ndim(),0);
            blc0(0) = iCen - width;
            blc0(1) = jCen - width;
            trc0(0) = iCen + width;
            trc0(1) = jCen + width;
//
            if (blc0(0)<0 || trc0(0)>=inShape(0) ||
                blc0(1)<0 || trc0(1)>=inShape(1)) {
               os << LogIO::WARN << "Component " << k << " is too close to the image edge for" << endl;
               os << "  shape-fitting - resorting to default shape estimates" << LogIO::POST;
               ss(k,0) = width;
               ss(k,1) = width;
               ss(k,2) = 0.0;
            } else {

// Fish out data to fit

               IPosition shp = trc0 - blc0 + 1;
               Array<T> dataIn = image.getSlice(blc0, shp, False);               
               Array<Bool> maskIn = image.getMaskSlice(blc0, shp, False);
               Array<T> sigmaIn(dataIn.shape(),1.0);

// Make fitter, add model and fit

               Fit2D fit2d(os);
               Vector<typename NumericTraits<T>::PrecisionType> model = 
                  fit2d.estimate(Fit2D::GAUSSIAN, dataIn, maskIn);
               model(0) = rs(k,0);
               fit2d.addModel(Fit2D::GAUSSIAN, model);
               Fit2D::ErrorTypes ret = fit2d.fit(dataIn, maskIn, sigmaIn);
//              
               if (ret==Fit2D::OK) {
                  Vector<typename NumericTraits<T>::PrecisionType> solution = 
                     fit2d.availableSolution();
//
                  rs(k,0) = solution(0);
                  rs(k,1) = solution(1) + blc0(0);
                  rs(k,2) = solution(2) + blc0(1);
//
                  ss(k,0) = solution(3);
                  ss(k,1) = solution(4);
                  ss(k,2) = solution(5);
               } else {
                  os << LogIO::WARN << "Fit did not converge, resorting to default shape estimates" << LogIO::POST;
                  ss(k,0) = width;
                  ss(k,1) = width;
                  ss(k,2) = 0.0;
               }
            }
         }
      }
   }

// Fill SkyComponents

   os << LogIO::NORMAL << "Found " << nFound << " sources" << LogIO::POST;
   const ImageInfo& info = image.imageInfo();
   const Unit& bU = image.units();
   Double rat;

// What Stokes is the plane we are finding in ?
      
   Stokes::StokesTypes stokes(Stokes::Undefined);
   stokes = CoordinateUtil::findSingleStokes (os, cSys, 0);
//
   Vector<Double> pars;
   ComponentType::Shape cType(ComponentType::POINT);
   pars.resize(3);
   if (!doPoint) {
      cType = ComponentType::GAUSSIAN;
      pars.resize(6, True);
   }
//
   for (Int k=0; k<nFound; k++) {
      pars(0) = rs(k,0);
      pars(1) = rs(k,1); 
      pars(2) = rs(k,2);
//
      if (!doPoint) {
         pars(3) = ss(k,0);
         pars(4) = ss(k,1);
         pars(5) = ss(k,2);
      }
//   
      try {
         SkyComponent sky = ImageUtilities::encodeSkyComponent (os, rat, info, cSys, bU,
                                                                cType, pars, stokes, xIsLong);
         listOut.add(sky);
      }  catch (AipsError x) {
         os << LogIO::WARN << "Could not convert fitted pixel parameters to world for source " << k+1 << endl;
         os << "Probably this means the fitted parameters were wildly wrong" << endl;
         os << "Reverting to original POINT source parameters for this source " << LogIO::POST;
//
         Vector<Double> pars2(3);
         pars2(0) = rs2(k,0);
         pars2(1) = rs2(k,1); 
         pars2(2) = rs2(k,2);
         SkyComponent sky = ImageUtilities::encodeSkyComponent (os, rat, info, cSys, bU,
                                                                ComponentType::POINT, 
                                                                pars2, stokes, xIsLong);
         listOut.add(sky);
      }
   } 
   return listOut;
}

} //# NAMESPACE CASA - END

