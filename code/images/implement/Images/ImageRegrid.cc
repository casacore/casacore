//# ImageRegrid.cc: Regrids images
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#include <trial/Images/ImageRegrid.h>

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Block.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/ObsInfo.h>
#include <trial/Images/TempImage.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/SubImage.h>
#include <aips/Lattices/ArrayLattice.h>
#include <trial/Lattices/MaskedLattice.h> 
#include <aips/Lattices/LatticeStepper.h>
#include <aips/Lattices/LatticeNavigator.h>
#include <aips/Lattices/LatticeIterator.h>
#include <trial/Lattices/LCSlicer.h>
#include <aips/Lattices/TempLattice.h>
#include <aips/Lattices/TiledShape.h>
#include <aips/Lattices/TiledLineStepper.h>
#include <trial/Lattices/SubLattice.h>
#include <trial/Lattices/LCRegion.h>
#include <trial/Mathematics/InterpolateArray1D.h>
#include <trial/Mathematics/Interpolate2D.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Logging/LogIO.h>
#include <aips/OS/Timer.h>
#include <aips/Quanta/MVDirection.h>
#include <trial/Tasking/ProgressMeter.h>
#include <aips/Utilities/Assert.h>

#include <aips/strstream.h>
#include <aips/fstream.h>


template<class T>
ImageRegrid<T>::ImageRegrid()
: itsShowLevel(0),
  itsDisableConversions(False)
{;}

template<class T>
ImageRegrid<T>::ImageRegrid(const ImageRegrid& other)  
: itsShowLevel(other.itsShowLevel),
  itsDisableConversions(other.itsDisableConversions)
{;}


template<class T>
ImageRegrid<T>::~ImageRegrid()
{;}

template<class T>
ImageRegrid<T>& ImageRegrid<T>::operator=(const ImageRegrid& other) 
{
  if (this != &other) {
    itsShowLevel = other.itsShowLevel;
    itsDisableConversions = other.itsDisableConversions;
  }
  return *this;
}


template<class T>
void ImageRegrid<T>::regrid(ImageInterface<T>& outImage,
                            typename Interpolate2D::Method method,
                            const IPosition& outPixelAxesU,
                            const ImageInterface<T>& inImage,
                            Bool replicate, Bool showProgress)
{
   LogIO os(LogOrigin("ImageRegrid", "regrid(...)", WHERE));
//
   IPosition outShape = outImage.shape();
   IPosition inShape = inImage.shape();
   const uInt nDim = inImage.ndim();
   if (nDim != outImage.ndim()) {
      throw(AipsError("The input and output images must have the same number of axes"));
   }
   const Bool outIsMasked = outImage.isMasked() && outImage.hasPixelMask() &&
                            outImage.pixelMask().isWritable();
//
   const CoordinateSystem& inCoords = inImage.coordinates();
   CoordinateSystem outCoords = outImage.coordinates();
   IPosition outPixelAxes = outPixelAxesU;

// Find world and pixel axis maps

   Vector<Int> pixelAxisMap1, pixelAxisMap2;
   findMaps (nDim, pixelAxisMap1, pixelAxisMap2, inCoords, outCoords);

// Check user pixel axes specifications

   checkAxes(outPixelAxes, inShape, outShape, pixelAxisMap1, outCoords);
   const uInt nOutPixelAxes = outPixelAxes.nelements();
   if (itsShowLevel>0) {
      cerr << "outPixelAxes = " << outPixelAxes << endl;
   }

// Set output shape.  This shape is incremental, for each regridding 
// pass it incrementally changes from the input shape to the output shape
// We account here for different pixel axis orders

   IPosition outShape2(nDim);
   for (uInt paOut=0; paOut<nDim; paOut++) {
      outShape2(paOut) = inShape(pixelAxisMap1(paOut));
   }

// Specify input and output lattices for each regridding pass

   MaskedLattice<T>* inPtr = 0;
   MaskedLattice<T>* outPtr = 0;
   MaskedLattice<T>* finalOutPtr = &outImage;
//
   Vector<Bool> doneOutPixelAxes(outCoords.nPixelAxes(), True);
   for (uInt i=0; i<nOutPixelAxes; i++) {
      doneOutPixelAxes(outPixelAxes(i)) = False;
   }

// Loop over specified pixel axes of output image
   
   for (uInt i=0; i<nOutPixelAxes; i++) {
      if (!doneOutPixelAxes(outPixelAxes(i))) {

// Set input and output images for this pass. The new  input must be the last 
// output image.  We end up with at least one temporary image. Could
// probably improve this. 

        if (!inPtr) {
           inPtr = inImage.cloneML();
        } else {
           delete inPtr;
           inPtr = outPtr;
           outPtr = 0;
        }

// Regrid one Coordinate, pertaining to this axis. If the axis
// belongs to a DirectionCoordinate, it will also regrid the 
// other axis.  After the first pass, the output image is in the
// final order.  

         CoordinateSystem inCoords2(inCoords);
         if (i>0) {
            inCoords2 = outCoords;
            indgen(pixelAxisMap1, 0);
            indgen(pixelAxisMap2, 0);
         } 
         regridOneCoordinate (os, outShape2, doneOutPixelAxes,
                              finalOutPtr,
                              inPtr, outPtr, outCoords, inCoords2,
                              pixelAxisMap1, pixelAxisMap2, 
                              outPixelAxes(i), inImage, outShape,
                              replicate, outIsMasked, 
                              showProgress, method);
      }
   }
//
   delete inPtr;
   inPtr = 0;
}




template<class T>
void ImageRegrid<T>::regridOneCoordinate (LogIO& os, IPosition& outShape2,
                                          Vector<Bool>& doneOutPixelAxes,
                                          MaskedLattice<T>* &finalOutPtr,
                                          MaskedLattice<T>* &inPtr,
                                          MaskedLattice<T>* &outPtr,
                                          CoordinateSystem& outCoords,
                                          const CoordinateSystem& inCoords,
                                          const Vector<Int>& pixelAxisMap1, 
                                          const Vector<Int>& pixelAxisMap2,
                                          Int outPixelAxis, 
                                          const ImageInterface<T>& inImage,    
                                          const IPosition& outShape,
                                          Bool replicate,
                                          Bool outIsMasked, Bool showProgress,
                                          typename Interpolate2D::Method method)
{
// Find equivalent world axis

   Int outWorldAxis = outCoords.pixelAxisToWorldAxis(outPixelAxis);
//
   Int outCoordinate, outAxisInCoordinate;
   Int inCoordinate, inAxisInCoordinate;
   outCoords.findPixelAxis(outCoordinate, outAxisInCoordinate, outPixelAxis);
   Coordinate::Type type = outCoords.type(outCoordinate);

// Find Coordinate in input image.

   Int inPixelAxis = pixelAxisMap1(outPixelAxis);
   Int inWorldAxis = inCoords.pixelAxisToWorldAxis(inPixelAxis);
   inCoords.findPixelAxis(inCoordinate, inAxisInCoordinate, inPixelAxis);
   if (inCoordinate==-1 || inAxisInCoordinate==-1) {
      ostrstream oss1;
      ostrstream oss2;
      oss1 << outCoords.showType(outCoordinate);
      oss2 << outPixelAxis+1;
      String msg = String("Output axis (") + String(oss2) + 
                   String(") of coordinate type ") + String(oss1) +
                   String("does not have a coordinate in the input CoordinateSystem");
      os << msg << LogIO::EXCEPTION;
    }

// Where are the input and output pixel axes for this  coordinate ?  Some coordinates,
// (apart from DirectionCoordinate), e.g. LinearCoordinate may have more than 
// one pixel axis. But we will do them in multiple passes; only DirectionCoordinate
// is coupled

    Vector<Int> outPixelAxes = outCoords.pixelAxes(outCoordinate);
    Vector<Int>  inPixelAxes = inCoords.pixelAxes(inCoordinate);
    Int maxMemoryInMB = 0;

// Now we need to break the polymorphic nature  of coordinates.  

    if (type==Coordinate::DIRECTION) {
       ostrstream oss;
       oss << "Regridding output axes " << outPixelAxes + 1 
           << " which are of Coordinate type " 
           << outCoords.showType(outCoordinate) << endl;
       os << LogIO::NORMAL << String(oss) << LogIO::POST;

// Note that we will do two pixel axes in this pass

       doneOutPixelAxes(outPixelAxes(0)) = True;
       doneOutPixelAxes(outPixelAxes(1)) = True;

// Update the incremental output image shape.

       outShape2(outPixelAxes(0)) = outShape(outPixelAxes(0));
       outShape2(outPixelAxes(1)) = outShape(outPixelAxes(1));
       if (outShape2(outPixelAxes(0))==1 && outShape2(outPixelAxes(1))==1) {
          os << "You cannot regrid the DirectionCoordinate as it is of shape [1,1]" 
             << LogIO::EXCEPTION;
       }

// Attach mask if out is masked.  Don't init mask because it will be overwritten

       if (allEQ(doneOutPixelAxes, True)) {

// We are on the last pass.  So assign the output pointer directly
// to the final output image to save an extra copy.

          outPtr = finalOutPtr;
       } else {
          outPtr = new TempImage<T>(TiledShape(outShape2), outCoords, maxMemoryInMB);
          if (outIsMasked) {
             String maskName("mask0");
             TempImage<T>* tmpPtr = dynamic_cast<TempImage<T>*>(outPtr);
             tmpPtr->makeMask(maskName, True, True, False);
          }
       }

// Get DirectionCoordinates for input and output

       Vector<String> units(2);
       units.set("deg");
       DirectionCoordinate inDir = inCoords.directionCoordinate(inCoordinate);
       DirectionCoordinate outDir = outCoords.directionCoordinate(outCoordinate);
       if (!inDir.setWorldAxisUnits(units)) {
          os << "Failed to set input DirectionCoordinate units to degrees" << LogIO::EXCEPTION;
       }
       if (!outDir.setWorldAxisUnits(units)) {
          os << "Failed to set output DirectionCoordinate units to degrees" << LogIO::EXCEPTION;
       }

// Possibly make Direction reference conversion machine

       MDirection::Convert machine;
       Bool madeIt = False;
       if (!itsDisableConversions) {
          madeIt = CoordinateUtil::makeDirectionMachine(os, machine, inDir, outDir, 
                        inCoords.obsInfo(), outCoords.obsInfo());
       }

// Get scaling factor to conserve flux in Jy/pixel

       Double scale = findScaleFactor(inImage.units(), inDir, outDir, os);

// Regrid 

       if (itsShowLevel>0) {
          cerr << "usemachine=" << madeIt << endl;
       }
       regrid2D (*outPtr, *inPtr, inDir, outDir, inPixelAxes,
                 outPixelAxes, pixelAxisMap1, pixelAxisMap2, method,
                 machine, replicate, madeIt, showProgress, scale);
    } else {
       ostrstream oss;
       oss << "Regridding output axis " << outPixelAxis+1 
           << " which is of Coordinate type " 
           << outCoords.showType(outCoordinate) << endl;
       os << LogIO::NORMAL << String(oss) << LogIO::POST;

// Note that will do one pixel axis in this pass

       doneOutPixelAxes(outPixelAxes(outAxisInCoordinate)) = True;

// Update the incremental output image shape.

       outShape2(outPixelAxes(outAxisInCoordinate)) = 
           outShape(outPixelAxes(outAxisInCoordinate));

       if (allEQ(doneOutPixelAxes, True)) {

// We are on the last pass.  So assign the output pointer directly
// to the final output image to save an extra copy.

          outPtr = finalOutPtr;
       } else {

// Attach mask if out is masked.  

          outPtr = new TempImage<T>(TiledShape(outShape2), outCoords, maxMemoryInMB);
          if (outIsMasked) {
             String maskName("mask0");
             TempImage<T>* tmpPtr = dynamic_cast<TempImage<T>*>(outPtr);
             tmpPtr->makeMask(maskName, True, True, True, True);
          }
       }

// Set world axis units for input and output coordinates for this pixel
// axis to be the same.  We can only do this via the CoordinateSystem (or casting)

       Vector<String> inUnits = inCoords.worldAxisUnits();
       Vector<String> outUnits = outCoords.worldAxisUnits();
       outUnits(outWorldAxis) = inUnits(inWorldAxis);
       if (!outCoords.setWorldAxisUnits(outUnits)) {
          os << "Failed to set output CoordinateSystem units" << LogIO::EXCEPTION;
       }
//
       const Coordinate& inCoord = inCoords.coordinate(inCoordinate);
       const Coordinate& outCoord = outCoords.coordinate(outCoordinate);

// Possibly make Frequency reference conversion machine

       Bool madeIt = False;
       MFrequency::Convert machine;
       if (!itsDisableConversions && type==Coordinate::SPECTRAL) {
          madeIt = CoordinateUtil::makeFrequencyMachine(os, machine, 
                       inCoordinate, outCoordinate, inCoords, outCoords);
       }

// Regrid 

       if (itsShowLevel>0) {
          cerr << "usemachine=" << madeIt << endl;
       }
       regrid1D (*outPtr, *inPtr, inCoord, outCoord, inPixelAxes,
                 outPixelAxes, inAxisInCoordinate, outAxisInCoordinate,
                 pixelAxisMap2, method, machine, replicate, 
                 madeIt, showProgress);
    }
}



template<class T>
Bool ImageRegrid<T>::insert (ImageInterface<T>& outImage,
                             const Vector<Double>& outReferencePixel,
                             const ImageInterface<T>& inImage) 
{
   LogIO os(LogOrigin("ImageRegrid", "insert(...)", WHERE));
//
   if (outImage.ndim()!=inImage.ndim()) {
      os << "The input and output images must have the same number of dimensions" << LogIO::EXCEPTION;
   }
//
   const CoordinateSystem& inCoords = inImage.coordinates();
   const uInt nPixelAxes = inCoords.nPixelAxes();
   AlwaysAssert(outImage.shape().nelements()==nPixelAxes,AipsError);
   AlwaysAssert(outReferencePixel.nelements()==nPixelAxes,AipsError);
//
   CoordinateSystem outCoords = inImage.coordinates();
   if (!outCoords.setReferencePixel(outReferencePixel)) {
      os << outCoords.errorMessage() << LogIO::EXCEPTION;
   }
   if (!outImage.setCoordinateInfo(outCoords)) {
      os << "Failed to set new reference pixel in output image" << LogIO::EXCEPTION;
   }
//
   const IPosition& inShape = inImage.shape();
   const IPosition& outShape = outImage.shape();
   const Vector<Double>& inReferencePixel = inCoords.referencePixel();
   if (itsShowLevel>0) {
     cerr << "in, out reference pixel = " << inReferencePixel << outReferencePixel << endl;
   }

// Where are the output blc/trc after placing the input image
// No trimming yet

   IPosition outBlc(nPixelAxes), outTrc(nPixelAxes);
   IPosition inBlc(nPixelAxes), inTrc(nPixelAxes);
   Int coordinate, axisInCoordinate;
   for (uInt i=0; i<nPixelAxes; i++) {
      outCoords.findPixelAxis(coordinate, axisInCoordinate, i);
      if (coordinate==-1 || axisInCoordinate==-1) {
         ostrstream oss;
         oss << "Pixel axis " << i << " has been removed from the output CoordinateSystem" << endl;
         os << String(oss) << LogIO::EXCEPTION;
      }
      Coordinate::Type type = outCoords.type(coordinate);
      if (type==Coordinate::STOKES && outShape(i)!=inShape(i)) {
         os << "It is not possible to change the shape of the Stokes axis" << LogIO::EXCEPTION;
      }
//
      outBlc(i) = static_cast<Int>(outReferencePixel(i) - inReferencePixel(i));
      outTrc(i) = outBlc(i) + inShape(i) - 1;
//
      inBlc(i) = 0;
      inTrc(i) = inShape(i) - 1;
   }
   if (itsShowLevel>0) {
      cerr << "inBlc, inTrc = " << inBlc << inTrc << endl;
      cerr << "outBlc, outTrc = " << outBlc << outTrc << endl;
   }


// Does the input miss the output entirely ?

   Bool missedIt = True;
   for (uInt i=0; i<nPixelAxes; i++) {
      if ( (outTrc(i)>=0 && outTrc(i)<outShape(i)) ||
           (outBlc(i)>=0 && outTrc(i)<outShape(i)) ||
           (outBlc(i)>=0 && outBlc(i)<outShape(i)) ) {
         missedIt = False;
         break;
      }
   }
   if (itsShowLevel>0) {
      cerr << "missedIt = " << missedIt << endl;
   }

// Init output data and mask then overwrite.  There
// will be some duplication of effort because of this

   const Bool outIsMasked = outImage.isMasked() && outImage.hasPixelMask() &&
                            outImage.pixelMask().isWritable();
   outImage.set(0.0);
   if (outIsMasked) {
      Lattice<Bool>& mask = outImage.pixelMask();
      mask.set(False);
   }
   if (missedIt) {
      return False;
   }

// Now trim blc/trc

   for (uInt i=0; i<nPixelAxes; i++) {
      Int t = outBlc(i);
      outBlc(i) = max(0,outBlc(i));
      Int d = t - outBlc(i);
      inBlc(i) -= d;
//
      t = outTrc(i);
      outTrc(i) = min(outShape(i)-1, outTrc(i));
      d = t - outTrc(i);
      inTrc(i) -= d;
   }

   if (itsShowLevel>0) {
      cerr << "After trimming " << endl;
      cerr << "inBlc, inTrc = " << inBlc << inTrc << endl;
      cerr << "outBlc, outTrc = " << outBlc << outTrc << endl;
   }


// Copy the relevant portion

   Slicer inBox(inBlc, inTrc, Slicer::endIsLast);
   Slicer outBox(outBlc, outTrc, Slicer::endIsLast);
   SubImage<T> inSub(inImage, inBox);
   SubImage<T> outSub(outImage, outBox, True);
//
   if (outIsMasked) {
      Lattice<Bool>& outMask = outImage.pixelMask();
      outMask.set(False);
      SubLattice<Bool> outSubMask(outMask, outBox, True);
      copyDataAndMask(outSub, inSub);
   } else {
      outSub.copyData(inSub);
   }
//
   return True;
}


 

template<class T>
CoordinateSystem ImageRegrid<T>::makeCoordinateSystem(LogIO& os,
                                                      const CoordinateSystem& cSysTo,
                                                      const CoordinateSystem& cSysFrom,
                                                      const IPosition& outPixelAxes) 
//
// For regrid axes
//   Copy from cSysTo   if Coordinate type provided in cSysTo
//   Copy from cSysFrom if Coordinate type not provided in cSysTo
//
// For non regrid axes
//   Copy from cSysFrom  
//
// The output CoordinateSystem must have the same number and types of 
// coordinates as the 'from' image.
//
{
   const uInt nCoordsFrom = cSysFrom.nCoordinates();
   const uInt nPixelAxesFrom = cSysFrom.nPixelAxes();

// Create output CS.  Copy the output ObsInfo over first.

   CoordinateSystem cSysOut;
   cSysOut.setObsInfo(cSysFrom.obsInfo());

// If specified axes are empty, set to all

   IPosition outPixelAxes2;
   if (outPixelAxes.nelements()==0) {
      outPixelAxes2 = IPosition::makeAxisPath(nPixelAxesFrom);
   } else {
      outPixelAxes2 = outPixelAxes;
   }

// Loop over output pixel axes from output CS. 

   Int iCoordFrom, axisInCoordinateFrom;
   Int iCoordTo;
   Vector<Bool> done(nCoordsFrom,False);  
   for (uInt i=0; i<nPixelAxesFrom; i++) {
 
// Is this output CS axis one to be regridded ?

      Bool regridIt = False;                 
      for (uInt j=0; j<outPixelAxes2.nelements(); j++) {
         if (Int(i)==outPixelAxes2(j)) {              
            regridIt = True;  
            break;
         }
      }

// Find out what type of coordinate this axis is and which
// coordinate it is.
   
      cSysFrom.findPixelAxis(iCoordFrom, axisInCoordinateFrom, i);
      if (!done(iCoordFrom)) {
         const Coordinate& coordFrom = cSysFrom.coordinate(iCoordFrom);
         Coordinate::Type type = coordFrom.type();
//    
         if (regridIt) {

// This is a regridding axis.   See if this coordinate has been
// provided in the 'to' CS.  If it has we copy that Coordinate.
// If it hasn't, we take the Coordinate from the 'from' CS

            Int afterCoord = -1;
            iCoordTo = cSysTo.findCoordinate(type, afterCoord);
//
            if (iCoordTo >= 0) {
               const Coordinate& coordTo = cSysTo.coordinate(iCoordTo);
               cSysOut.addCoordinate(coordTo);
            } else {
               cSysOut.addCoordinate(coordFrom);
            }
         } else {
         
// We don't want to regrid this axis.  Copy the Coordinate from the 
// 'from' CS

            cSysOut.addCoordinate(coordFrom);
         }                                   
//                                           
         done(iCoordFrom) = True;
      }
   }
// 
   return cSysOut;
}


template<class T>
void ImageRegrid<T>::regrid2D (MaskedLattice<T>& outLattice,
                               const MaskedLattice<T>& inLattice,
                               const DirectionCoordinate& inCoord,
                               const DirectionCoordinate& outCoord,
                               const Vector<Int> inPixelAxes,
                               const Vector<Int> outPixelAxes,
                               const Vector<Int> pixelAxisMap1,
                               const Vector<Int> pixelAxisMap2,
                               typename Interpolate2D::Method method,
                               MDirection::Convert& machine,
                               Bool replicate,
                               Bool useMachine, Bool showProgress, Double scale)
//
// Compute output coordinate, find region around this coordinate
// in input, interpolate. Any output mask is overwritten
//
{
   LogIO os(LogOrigin("ImageRegrid", "regrid2D(...)", WHERE));
//
   AlwaysAssert(inPixelAxes.nelements()==2, AipsError);
   AlwaysAssert(outPixelAxes.nelements()==2, AipsError);
//
   Bool resample = replicate;
//
   Bool inIsMasked = inLattice.isMasked();
   Bool outIsMasked = outLattice.isMasked() && outLattice.hasPixelMask() &&
                      outLattice.pixelMask().isWritable();
//
   const IPosition inShape = inLattice.shape();
   const IPosition outShape = outLattice.shape();
   const uInt nDim = inLattice.ndim();
//
   if (itsShowLevel>0) {
      cerr << "Resample = " << resample << endl;
      cerr << "inPixelAxes = " << inPixelAxes << endl;
      cerr << "outPixelAxes = " << outPixelAxes << endl;
      cerr << "inIsMasked " << inIsMasked << endl;
      cerr << "outIsMasked " << outIsMasked << endl;
   }
   if (itsShowLevel>0) {
      if (method==Interpolate2D::NEAREST) {
         cerr << "Method is nearest" << endl;
      } else if (method==Interpolate2D::LINEAR) {
         cerr << "Method is linear" << endl;
      } else if (method==Interpolate2D::CUBIC) {
         cerr << "Method is cubic" << endl;
      }
   }

// We iterate through the output image by tile.  We iterate through
// each tile by matrix holding the Direction Coordinate axes (in 
// some order).  We have a matrix(i=1:nrows,j=1:ncols). 
//
// pixelAxisMap1(i) says where pixel axis i in the output image is in the input  image
// pixelAxisMap2(i) says where pixel axis i in the  input image is in the output image
// pixelAxes(0)    says where Lon  is in image (in or out)
// pixelAxes(1)    says where Lat  is in image (in or out)
//
// xOutAxis        is the first direction axis in the output image (associated with i)
// yOutAxis        is the second direction axis in the output image (associated with j)
// xInCorrAxis     is the corresponding axis to xOutAxis in the input image
// yInCorrAxis     is the corresponding axis to yOutAxis in the input image
//
// xInAxis         is the first direction axis in the input image
// yInAxis         is the second direction axis in the input image
// xOutCorrAxis    is the corresponding axis to xInAxis in the output image
// yOutCorrAxis    is the corresponding axis to yInAxis in the output image

// Example: 
//
//   Regrid ra/dec axes with input order ra/dec/freq and output order freq/dec/ra
//
//   input  image shape = [20, 30, 40] (ra/dec/freq)
//   output image shape = [40, 90, 60] (freq/dec/ra) - we are making ra/dec shape 3x input
//
//   outPixelAxes = [2,1] = [lon,lat] 
//   The cursor matrix is of shape [nrow,ncol] = [90,60]
//       xOutAxis = 1   (dec)
//       yOutAxis = 2   (ra)
//    xInCorrAxis = 1   (dec)
//    yInCorrAxis = 0   (ra)
//        xInAxis = 0   (ra)
//        yInAxis = 1   (dec)
//   xOutCorrAxis = 2   (ra)
//   yOutCorrAxis = 1   (dec)
//
//
   const uInt xOutAxis = min(outPixelAxes(0), outPixelAxes(1));
   const uInt yOutAxis = max(outPixelAxes(0), outPixelAxes(1));
   uInt xInCorrAxis = pixelAxisMap1(xOutAxis);
   uInt yInCorrAxis = pixelAxisMap1(yOutAxis);
//
   const uInt xInAxis = min(inPixelAxes(0), inPixelAxes(1));
   const uInt yInAxis = max(inPixelAxes(0), inPixelAxes(1));
   uInt xOutCorrAxis = pixelAxisMap2(xInAxis);
   uInt yOutCorrAxis = pixelAxisMap2(yInAxis);

// Make navigator and iterator for output data and mask.  It is vital that
// the "niceShape" is the same for both iterators.  Because the mask and 
// lattice are both TempLattices, one might be on disk, one in core.
// Hence we pick one nice shape and use it on both iterators, although
// it may be suboptimal for one of the lattices.

   IPosition niceShape = outLattice.niceCursorShape();
   LatticeStepper outStepper(outShape, niceShape, LatticeStepper::RESIZE);
   LatticeIterator<T> outIter(outLattice, outStepper);
//
   if (itsShowLevel>0) {
      cerr << "xOutAxis, yOutAxis = " << xOutAxis << ", " << yOutAxis << endl;
      cerr << "xInCorrAxis, yInCoorrAxis = " << xInCorrAxis << ", " << yInCorrAxis << endl;
      cerr << "xInAxis, yInAxis = " << xInAxis << ", " << yInAxis << endl;
      cerr << "xOutCorrAxis, yOutCoorrAxis = " << xOutCorrAxis << ", " << yOutCorrAxis << endl;
//
      cerr << "cursor shape = " << niceShape << endl;
      cerr << "shape in, shape out" << inShape << outShape << endl;  
   }

// Deal with mask.  Stepper will make a reference copy of the mask

   LatticeIterator<Bool>* outMaskIterPtr = 0;
   if (outIsMasked) {
      Lattice<Bool>& outMask = outLattice.pixelMask();
      outMaskIterPtr = new LatticeIterator<Bool>(outMask, outStepper);
   }

// These tell us which chunk of input data we need to service each
// iteration through the output image
   
   IPosition inChunkBlc(nDim);
   IPosition inChunkTrc(nDim);

// These tell us which 2D piece of inChunk to read.  This is  what we regrid from.  

   IPosition inChunkBlc2D(nDim);
   IPosition inChunkTrc2D(nDim);

// Coordinate conversion vectors
  
   Vector<Double> world(2), inPixel(2), outPixel(2);
//
   Vector<Double> pixelScale(nDim); 
   pixelScale = 1.0;
   pixelScale(xInAxis) = Float(outShape(xOutCorrAxis)) / Float(inShape(xInAxis));
   pixelScale(yInAxis) = Float(outShape(yOutCorrAxis)) / Float(inShape(yInAxis));
   if (itsShowLevel > 0) {
      cerr << "pixelScale = " << pixelScale << endl;
   }

// 2D interpolator

   Interpolate2D interp(method);

// Various things needed along the way

   Vector<Double> pix2DPos2(2);
   IPosition outPos4, outPos3, outPos2, inPos;
   Double minInX, minInY, maxInX, maxInY;

// Generate full plane of coordinates mapping each output
// pixel to an input pixel

   Cube<Double> pix2DPosFull(outShape(xOutAxis), outShape(yOutAxis), 2);
   Matrix<Bool> failedFull(outShape(xOutAxis), outShape(yOutAxis));
   Bool allFailedFull = False;
   Bool missedItFull = True;
   IPosition outPosFull(outLattice.ndim(),0);
//   Timer timer0;
   if (resample) {
      make2DCoordinateGrid (pix2DPosFull, minInX, minInY, maxInX, maxInY,
                            pixelScale, xInAxis, yInAxis, xOutAxis, yOutAxis,
                            xInCorrAxis, yInCorrAxis, xOutCorrAxis, yOutCorrAxis, 
                            outPosFull, outShape);
      missedItFull = False;
      allFailedFull = False;
      failedFull.set(False);
   } else {
      make2DCoordinateGrid (allFailedFull, missedItFull, minInX, minInY, maxInX, maxInY,
                            pix2DPosFull, failedFull, machine, inCoord, outCoord, 
                            xInAxis, yInAxis, xOutAxis, yOutAxis,
                            inPixelAxes, outPixelAxes, inShape, outPosFull, 
                            outShape, useMachine);
   }
//   cerr << "Full plane make2DCoordinateGrid took " << timer0.all() << endl;
//   timer0.show();
//
   if (missedItFull || allFailedFull) {
      outLattice.set(0.0);
      if (outIsMasked) outLattice.pixelMask().set(False);
      return;
   }

// Progress meter

   ProgressMeter* pProgressMeter = 0;
   if (showProgress) {
     Double nMin = 0.0;
     Double nMax = Double(outLattice.shape().product());
     ostrstream oss;
     oss << "Axes " << outPixelAxes + 1 << " : Pixels Regridded" << ends;
     pProgressMeter = new ProgressMeter(nMin, nMax, String(oss),
                                        String("Regridding"),
                                        String(""), String(""),
                                        True, 
                                        max(1,Int(nMax/20)));
   }

// Iterate through output image

   Double iPix = 0.0;
   Int i2;
   for (outIter.reset(); !outIter.atEnd(); outIter++) {
      const IPosition& outCursorShape = outIter.cursorShape();
      const IPosition& outPos = outIter.position();
//
      if (itsShowLevel>0) cerr << endl;
      if (itsShowLevel>0) {
         cerr << "Output lattice iterator position = " <<  outPos << endl;
         cerr << "Shape of cursor = " << outIter.cursor().shape() << endl;
      }

// Now get a chunk of input data which we will access over and over
// as we interpolate it.   

      Bool missedIt = True;
      Bool allFailed = True;
      findXYExtent (missedIt, allFailed, minInX, minInY, maxInX, maxInY,  pix2DPosFull,
                    failedFull, xInAxis, yInAxis, xOutAxis, yOutAxis, outPos, 
                    outCursorShape, inShape);
      if (itsShowLevel>0) {
         cerr << "missedIt, allFailed, minInX, maxInX, minInY, maxInY = " << 
                 missedIt << ", " << allFailed << ", " <<
                 minInX << ", " << maxInX << ", " <<  minInY << ", " << maxInY << endl;
      }
//
      if (missedIt || allFailed) {
         outIter.rwCursor().set(0.0);
         if (outIsMasked) outMaskIterPtr->rwCursor().set(False);
         if (showProgress) {
            pProgressMeter->update(iPix);
            iPix += Double(outCursorShape.product());
         }
      } else {

// For the non-regrid axes, the input and output shapes, and hence positions, are the same. 
// pixelAxisMap2(i) says where pixel axis i in the input image is in the output image.  

         for (uInt k=0; k<nDim; k++) {
            inChunkBlc(k) = outPos(pixelAxisMap2(k));
            inChunkTrc(k) = outIter.endPosition()(pixelAxisMap2(k));
         }

// Now overwrite the blc/trc for the regrid axes. The interpolation schemes (Interpolate2D) use 
// a small grid about the pixel of interest I happen to know that allowing 3 
// pixels on either side is enough.   If this should change, the interpolation 
// would return False at the edges

         i2 = static_cast<Int>(floor(minInX)) - 3;
         inChunkBlc(xInAxis) = max(0,i2);
         i2 = static_cast<Int>(floor(minInY)) - 3;
         inChunkBlc(yInAxis) = max(0,i2);
//
         i2 = static_cast<Int>(ceil(maxInX)) + 3;
         inChunkTrc(xInAxis) = min(inShape(xInAxis)-1,i2);
         i2 = static_cast<Int>(ceil(maxInY)) + 3;
         inChunkTrc(yInAxis) = min(inShape(yInAxis)-1,i2);
         IPosition inChunkShape = inChunkTrc - inChunkBlc + 1;
         if (itsShowLevel>0) {
            cerr << "inChunkShape = " << inChunkShape << endl;
            cerr << "inChunkBlc, inChunkTrc " << inChunkBlc << inChunkTrc << endl;
         }

// Get the input data and mask

         Array<T> inDataChunk = inLattice.getSlice(inChunkBlc, inChunkShape);
         Array<Bool>* inMaskChunkPtr = 0;
         if (inIsMasked) {
            inMaskChunkPtr = new Array<Bool>(inLattice.getMaskSlice(inChunkBlc, inChunkShape));
         }

// Iterate through the output cursor by Matrices, each holding a Direction plane.
// This gets us just a few percent speed up over iterating through pixel by pixel.    

         ArrayLattice<T> outCursor(outIter.rwCursor());    // reference copy
         if (inChunkShape(xInAxis)==1 && inChunkShape(yInAxis)==1) {
            os << "Cannot regrid degenerate DirectionCoordinate plane" << LogIO::EXCEPTION;
         } else if (inChunkShape(xInAxis)==1 || inChunkShape(yInAxis)==1) {
            os << "Cannot yet handle DirectionCoordinate plane with one degenerate axis" << LogIO::EXCEPTION;
//          regrid2DVector();
         } else {
            regrid2DMatrix(outCursor, outMaskIterPtr, interp,
                           pProgressMeter, iPix, nDim, 
                           xInAxis, yInAxis, xOutAxis, yOutAxis, 
                           scale, inIsMasked, outIsMasked,
                           outPos, outCursorShape, inChunkShape,
                           inChunkBlc, pixelAxisMap2,
                           inDataChunk, inMaskChunkPtr,
                           pix2DPosFull, failedFull);
   
         }
      }
      if (outIsMasked) (*outMaskIterPtr)++;
   } 
//
   if (outIsMasked) delete outMaskIterPtr;
   if (showProgress) delete pProgressMeter;
}



template<class T>
void ImageRegrid<T>::make2DCoordinateGrid (Bool& allFailed, Bool&missedIt,
                                           Double& minInX, Double& minInY, 
                                           Double& maxInX, Double& maxInY,  
                                           Cube<Double>& in2DPos,
                                           Matrix<Bool>& failed,
                                           MDirection::Convert& machine,
                                           const DirectionCoordinate& inCoord,
                                           const DirectionCoordinate& outCoord,
                                           uInt xInAxis, uInt yInAxis,
                                           uInt xOutAxis, uInt yOutAxis,
                                           const IPosition& inPixelAxes,
                                           const IPosition& outPixelAxes,
                                           const IPosition& inShape,
                                           const IPosition& outPos,
                                           const IPosition& outCursorShape,
                                           Bool useMachine)
{
   Vector<Double> world(2), inPixel(2), outPixel(2);
   minInX =  100000000.0;
   minInY =  100000000.0;
   maxInX = -100000000.0;
   maxInY = -100000000.0;
   allFailed = True;
   Bool ok1, ok2;
   MVDirection inMVD, outMVD;
//
   uInt ni = outCursorShape(xOutAxis);
   uInt nj = outCursorShape(yOutAxis);

// Where in the Direction Coordinates are X and Y ?
// pixelAxes(0) says where Lon is in DirectionCoordinate
// pixelAxes(1) says where Lat is in DirectionCoordinate
// The X axis is always the direction axis that appears first in the image
//
   uInt inXIdx = 0;         // [x,y] = [lon,lat]
   uInt inYIdx = 1;
   if (inPixelAxes(0)==Int(yInAxis)) {
     inXIdx = 1;            // [x,y] = [lat,lon]
     inYIdx = 0;
   }
//
   uInt outXIdx = 0;         
   uInt outYIdx = 1;
   if (outPixelAxes(0)==Int(yOutAxis)) {
     outXIdx = 1;         
     outYIdx = 0;
   }
//
   if (itsShowLevel > 0) {                 
      cerr << "inXIdx, inYIdx = " << inXIdx << ", " << inYIdx << endl;
      cerr << "outXIdx, outYIdx = " << outXIdx << ", " << outYIdx << endl;
   }
//
   if (useMachine) {
      for (uInt j=0; j<nj; j++) {
         for (uInt i=0; i<ni; i++) {
            outPixel(outXIdx) = i + outPos(xOutAxis);
            outPixel(outYIdx) = j + outPos(yOutAxis);

// Do coordinate conversions (outpixel to world to inpixel)
// for the axes of interest
            
            ok1 = outCoord.toWorld(outMVD, outPixel);            
            ok2 = False;
            if (ok1) {
               inMVD = machine(outMVD).getValue();
               ok2 = inCoord.toPixel(inPixel, inMVD);
            } 
//
            if (!ok1 || !ok2) {
               failed(i,j) = True;
            } else {

// This gives the 2D input pixel coordinate (relative to the start of the full Lattice)
// to find the interpolated result at.  (,,0) pertains to inX and (,,1) to inY

               in2DPos(i,j,0) = inPixel(inXIdx);
               in2DPos(i,j,1) = inPixel(inYIdx);
               allFailed = False;
               failed(i,j) = False;
//
               minInX = min(minInX,inPixel(inXIdx));
               minInY = min(minInY,inPixel(inYIdx));
               maxInX = max(maxInX,inPixel(inXIdx));
               maxInY = max(maxInY,inPixel(inYIdx));
            }
         }
      }
   } else {
/*
      Double s1 = 0.0;
      Double s2 = 0.0;
      Double s3 = 0.0;
      Timer t1,t2,t3;
*/


// There is no performance benefit using the *Many functions because
// of the loading and unloading

// Load

/*
      t1.mark();
      Matrix<Double> worldMany(2,ni*nj), inPixelMany(2,ni*nj), outPixelMany(2,ni*nj);
      uInt i,j;
      for (uInt k=0; k<ni*nj; k++) {
         j = k/ni;
         i = k - j*ni;
//
         outPixelMany(outXIdx,k) = i + outPos(xOutAxis);
         outPixelMany(outYIdx,k) = j + outPos(yOutAxis);
      }
      s1 = t1.all();

// Convert

      t2.mark();
      Vector<Int> failures1, failures2;
      uInt nFail1 = outCoord.toWorldMany(worldMany, outPixelMany, failures1);
      uInt nFail2 = inCoord.toPixelMany(inPixelMany, worldMany, failures2);
      s2 = t2.all();


// Unload

      t3.mark();
      for (uInt k=0; k<ni*nj; k++) {
         j = k/ni;
         i = k - j*ni;
//
         if ( (k<nFail1 && failures1(k)) || 
              (k<nFail2 && failures2(k)) ) {
            failed(i,j) = True;
         } else {

// This gives the 2D input pixel coordinate (relative to the start of the full Lattice)
// to find the interpolated result at.  (,,0) pertains to inX and (,,1) to inY

            in2DPos(i,j,0) = inPixelMany(inXIdx,k);
            in2DPos(i,j,1) = inPixelMany(inYIdx, k);
            allFailed = False;
            failed(i,j) = False;
//
            minInX = min(minInX,inPixelMany(inXIdx,k));
            minInY = min(minInY,inPixelMany(inYIdx,k));
            maxInX = max(maxInX,inPixelMany(inXIdx,k));
            maxInY = max(maxInY,inPixelMany(inYIdx,k));
         }
      }
      s3 = t3.all();
*/

//
      uInt k = 0;
      for (uInt j=0; j<nj; j++) {
         for (uInt i=0; i<ni; i++,k++) {
//            t1.mark();
            outPixel(outXIdx) = i + outPos(xOutAxis);
            outPixel(outYIdx) = j + outPos(yOutAxis);
//            s1 += t1.all();

// Do coordinate conversions (outpixel to world to inpixel)
// for the axes of interest
            
//            t2.mark();
            ok1 = outCoord.toWorld(world, outPixel);
            ok2 = False;
            if (ok1) ok2 = inCoord.toPixel(inPixel, world);
//            s2 += t2.all();
//
//            if (itsShowLevel>1) {
//              cerr << "outPos, world, inPos, ok1, ok2 = ";
//              cerr.precision(12);
//              cerr  << outPixel << world << inPixel << ", " << ok1 << ", " << 
//                    ok2 << endl;
//            }

//
//            t3.mark();
            if (!ok1 || !ok2) {
               failed(i,j) = True;
            } else {

// This gives the 2D input pixel coordinate (relative to the start of the full Lattice)
// to find the interpolated result at.  (,,0) pertains to inX and (,,1) to inY

               in2DPos(i,j,0) = inPixel(inXIdx);
               in2DPos(i,j,1) = inPixel(inYIdx);
               allFailed = False;
               failed(i,j) = False;
//
               minInX = min(minInX,inPixel(inXIdx));
               minInY = min(minInY,inPixel(inYIdx));
               maxInX = max(maxInX,inPixel(inXIdx));
               maxInY = max(maxInY,inPixel(inYIdx));
            }
//            s3 += t3.all();
         }
      }

//cerr << "s1, s2, s3 = " << s1 << ", " << s2 << ", " << s3 << endl;

   }


// Does the output map to anywhere on the input ?

   missedIt = False;
   if (!allFailed) {
      Double ijMin = -0.5;
      Double iMax = inShape(xInAxis) - 0.5;
      Double jMax = inShape(yInAxis) - 0.5;
//
      missedIt  = (minInX<ijMin && maxInX<ijMin)  ||
                  (minInX>iMax && maxInX>iMax) ||
                  (minInY<ijMin && maxInY<ijMin)  ||
                  (minInY>jMax && maxInY>jMax);
   }
   if (itsShowLevel>0) {
      cerr << "allFailed, missedIt  = " << allFailed << ", " << missedIt << endl;
   }
   if (itsShowLevel>1) {
      cerr << "failed  = " << failed << endl;
   }
}



template<class T>
void ImageRegrid<T>::make2DCoordinateGrid (Cube<Double>& in2DPos,
                                           Double& minInX, Double& minInY, 
                                           Double& maxInX, Double& maxInY,  
                                           const Vector<Double>& pixelScale,
                                           uInt xInAxis, uInt yInAxis,
                                           uInt xOutAxis, uInt yOutAxis,
                                           uInt xInCorrAxis, uInt yInCorrAxis,
                                           uInt xOutCorrAxis, uInt yOutCorrAxis,
                                           const IPosition& outPos, 
                                           const IPosition& outCursorShape)

{
   Double oX = -0.5 + (1.0/2/pixelScale(xInAxis));
   Double oY = -0.5 + (1.0/2/pixelScale(yInAxis));
//
   uInt ni = outCursorShape(xOutAxis);
   uInt nj = outCursorShape(yOutAxis);
//
   if (xOutAxis == xOutCorrAxis) {                      

// First output Direction axis corresponds to the first input Direction axis

      Double t0 = (outPos(xOutCorrAxis) / pixelScale(xInAxis)) + oX;
      Double t1 = (outPos(yOutCorrAxis) / pixelScale(yInAxis)) + oY;
//
      for (uInt j=0; j<nj; j++) {
         for (uInt i=0; i<ni; i++) {
           in2DPos(i,j,0) = Double(i) / pixelScale(xInAxis) + t0;
           in2DPos(i,j,1) = Double(j) / pixelScale(yInAxis) + t1;
         }
      }  
   } else if (xOutAxis == yOutCorrAxis) {

// First output Direction axis corresponds to the second input Direction axis

      Double t0 = (outPos(yOutCorrAxis) / pixelScale(xInAxis)) + oX;
      Double t1 = (outPos(xOutCorrAxis) / pixelScale(yInAxis)) + oY;
//
      for (uInt j=0; j<nj; j++) {
         for (uInt i=0; i<ni; i++) {
           in2DPos(i,j,0) = Double(j) / pixelScale(xInAxis) + t0;
           in2DPos(i,j,1) = Double(i) / pixelScale(yInAxis) + t1;
         }
      }  
   } else {
      throw(AipsError("Big trouble in make2CoordinateGrid"));
   }
//
   minInX = in2DPos(0,0,0);
   minInY = in2DPos(0,0,1);
   maxInX = in2DPos(ni-1,nj-1,0);
   maxInY = in2DPos(ni-1,nj-1,1);
}


template<class T>
void ImageRegrid<T>::findXYExtent (Bool& missedIt, Bool& allFailed,
                                   Double& minInX, Double& minInY, 
                                   Double& maxInX, Double& maxInY,  
                                   const Cube<Double>& in2DPos,
                                   const Matrix<Bool>& failed,
                                   uInt xInAxis, uInt yInAxis,
                                   uInt xOutAxis, uInt yOutAxis,
                                   const IPosition& outPos,
                                   const IPosition& outCursorShape,
                                   const IPosition& inShape) const

{
   minInX =  100000000.0;
   minInY =  100000000.0;
   maxInX = -100000000.0;
   maxInY = -100000000.0;
   uInt ni = outCursorShape(xOutAxis);
   uInt nj = outCursorShape(yOutAxis);

// outPos is the BLC of this chunk in the output lattice

   uInt iOff = outPos(xOutAxis);
   uInt jOff = outPos(yOutAxis);
//
   allFailed = True;
   uInt ii = 0;
   uInt jj = 0;
   for (uInt j=0; j<nj; j++) {
      jj = j + jOff;
      for (uInt i=0; i<ni; i++) {
         ii = i + iOff;
//
         if (!failed(ii,jj)) {
           minInX = min(minInX,in2DPos(ii,jj,0));
           minInY = min(minInY,in2DPos(ii,jj,1));
           maxInX = max(maxInX,in2DPos(ii,jj,0));
           maxInY = max(maxInY,in2DPos(ii,jj,1));
         }
      }
   }
   if (maxInX > minInX) allFailed = False;
//
   Double ijMin = -0.5;
   Double iMax = inShape(xInAxis) - 0.5;
   Double jMax = inShape(yInAxis) - 0.5;
   missedIt  = (minInX<ijMin && maxInX<ijMin)  ||
               (minInX>iMax && maxInX>iMax) ||
               (minInY<ijMin && maxInY<ijMin)  ||
               (minInY>jMax && maxInY>jMax);
}



template<class T>
void ImageRegrid<T>::regrid2DMatrix(Lattice<T>& outCursor, 
                                    LatticeIterator<Bool>*& outMaskIterPtr,
                                    const Interpolate2D& interp,
                                    ProgressMeter*& pProgressMeter,
                                    Double& iPix,
                                    uInt nDim, 
                                    uInt xInAxis, uInt yInAxis, 
                                    uInt xOutAxis, uInt yOutAxis, 
                                    Double scale,
                                    Bool inIsMasked, Bool outIsMasked,
                                    const IPosition& outPos,
                                    const IPosition& outCursorShape,
                                    const IPosition& inChunkShape,
                                    const IPosition& inChunkBlc,
                                    const IPosition& pixelAxisMap2,
                                    Array<T>& inDataChunk,
                                    Array<Bool>*& inMaskChunkPtr,
                                    const Cube<Double>& pix2DPos,
                                    const Matrix<Bool>& failed)
// 
// Iterate through a stack of DirectionCoordinate planes and interpolate them
//
{
//cerr << "Enter regrid2Dmatrix" << endl;
//cerr << "Pix2Dpos shape = " << pix2DPos.shape() << endl;

// Setup Navigator and tell it which axes are the Direction ones in case
// of other degenerate axes

   IPosition axisPath;
   IPosition outCursorAxes(2, xOutAxis, yOutAxis);
   IPosition outCursorIterShape(2, outCursorShape(xOutAxis), outCursorShape(yOutAxis));
   LatticeStepper outCursorIterStepper(outCursor.shape(), outCursorIterShape, outCursorAxes, axisPath);
   LatticeIterator<T> outCursorIter(outCursor, outCursorIterStepper);
//
   LatticeIterator<Bool>* outMaskCursorIterPtr = 0;
   Lattice<Bool>* outMaskCursorPtr = 0;
   if (outIsMasked) {
     outMaskCursorPtr = new ArrayLattice<Bool>(outMaskIterPtr->rwCursor());
     outMaskCursorIterPtr = new LatticeIterator<Bool>(*outMaskCursorPtr, outCursorIterStepper);
   }
/*
   if (itsShowLevel>0) {
      cerr << "outCursorAxisPath = " << axisPath << endl;
      cerr << "outCursorAxes" << outCursorAxes << endl;
      cerr << "outCursorIterShape = " << outCursorIterShape << endl;
      cerr << "outMatrixCursorIterShape = " << outCursorIter.matrixCursor().shape() << endl;
   }
*/
//
   IPosition inChunkBlc2D(nDim, 0);
   IPosition inChunkTrc2D(nDim);
   inChunkTrc2D = inChunkShape - 1;
//
   IPosition inChunk2DShape(2);
   inChunk2DShape(0) = inChunkTrc2D(xInAxis) - inChunkBlc2D(xInAxis) + 1;
   inChunk2DShape(1) = inChunkTrc2D(yInAxis) - inChunkBlc2D(yInAxis) + 1;
//
   Vector<Double> pix2DPos2(2);
   IPosition outPos3;
   Bool interpOK;
   T result(0);
   for (outCursorIter.reset(); !outCursorIter.atEnd(); outCursorIter++) {

// outCursorIter.position is the location of the BLC of the current matrix within the current
// cursor (tile) of data. outPos3 is the location of the BLC of the current matrix within
// the full lattice

      outPos3 = outPos + outCursorIter.position();

// Fish out the 2D piece of the inChunk relevant to this plane of the cursor

      for (uInt k=0; k<nDim; k++) {
         if (k!=xInAxis&& k!=yInAxis) {
            inChunkBlc2D(k) = outPos3(pixelAxisMap2(k)) - inChunkBlc(k);
            inChunkTrc2D(k) = inChunkBlc2D(k);
         } 
      }
//
/*
      if (itsShowLevel>1) {
         cerr << "inChunkBlc2D, inChunkTrc2D, inChunk2DShape " << inChunkBlc2D 
              << inChunkTrc2D << inChunk2DShape << endl;
      }
*/
      const Matrix<T> inDataChunk2D = inDataChunk(inChunkBlc2D, inChunkTrc2D).reform(inChunk2DShape);
      Matrix<Bool>* inMaskChunk2DPtr = 0;
      if (inIsMasked) {
         inMaskChunk2DPtr = 
            new Matrix<Bool>((*inMaskChunkPtr)(inChunkBlc2D, inChunkTrc2D).reform(inChunk2DShape));
      }

// Now work through each output pixel in the data Matrix and do the interpolation

      uInt nCol = outCursorIter.matrixCursor().ncolumn();
      uInt nRow = outCursorIter.matrixCursor().nrow();
      uInt ii = 0;
      uInt jj = 0;
      for (uInt j=0; j<nCol; j++) {
         for (uInt i=0; i<nRow; i++) {
            if (failed(i,j)) {
               outCursorIter.rwMatrixCursor()(i,j) = 0.0;
               if (outIsMasked) {
                  outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False;
               }
            } else {

// Now do the interpolation if all points are unmasked in the input grid
// pix2DPos(i,j,) is the absolute input pixel coordinate in the input lattice
// for the current output pixel.

               ii = outPos3(xOutAxis) + i;
               jj = outPos3(yOutAxis) + j;
               pix2DPos2(0) = pix2DPos(ii,jj,0) - inChunkBlc(xInAxis);
               pix2DPos2(1) = pix2DPos(ii,jj,1) - inChunkBlc(yInAxis);
/*
               if (itsShowLevel>2) {
                  cerr << "For output [i,j] = " << " [" << i << "," << j << "]" 
                       << " interpolate input at " << pix2DPos2;
               }
*/
//
               if (inIsMasked) {                     
                  interpOK = interp.interp(result, pix2DPos2, inDataChunk2D, *inMaskChunk2DPtr);
               } else {
                  interpOK = interp.interp(result, pix2DPos2, inDataChunk2D);
               }
               if (interpOK) {
/*
                  if (itsShowLevel>2) {
                     cerr  << " giving result " << result << endl;
                  }
*/
                  outCursorIter.rwMatrixCursor()(i,j) = scale * result;
                  if (outIsMasked) {
                     outMaskCursorIterPtr->rwMatrixCursor()(i,j) = True; 
                  }
               } else {
/*
                  if (itsShowLevel>2) {
                      cerr  << " giving Fail" << endl;
                  }
*/
                  outCursorIter.rwMatrixCursor()(i,j) = 0.0;
                  if (outIsMasked) {
                     outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False; 
                  }
               }
            }
            if (pProgressMeter) {
               pProgressMeter->update(iPix); 
               iPix++;
            }
        }
      }
//
/*
      if (itsShowLevel>2) {
         cerr << "output data matrix  = " << outCursorIter.matrixCursor() << endl;
         if (outIsMasked) {
            cerr << "output mask matrix  = " << outMaskCursorIterPtr->matrixCursor() << endl;
         }
      }
*/
      if (outIsMasked) (*outMaskCursorIterPtr)++;
      if (inIsMasked) delete inMaskChunk2DPtr;
   }
//
   if (inIsMasked) delete inMaskChunkPtr;
   if (outIsMasked) {
      delete outMaskCursorIterPtr;
      delete outMaskCursorPtr;
   }
}


/*
template<class T>
void ImageRegrid<T>::regrid2DVector (Lattice<T>& outCursor, 
                                    LatticeIterator<Bool>*& outMaskIterPtr,
                                    const Interpolate2D& interp,
                                    ProgressMeter*& pProgressMeter,
                                    Double& iPix,
                                    uInt nDim, 
                                    uInt xInAxis, uInt yInAxis, 
                                    uInt xOutAxis, uInt yOutAxis, 
                                    Double scale,
                                    Bool inIsMasked, Bool outIsMasked,
                                    const IPosition& outPos,
                                    const IPosition& outCursorShape,
                                    const IPosition& inChunkShape,
                                    const IPosition& inChunkBlc,
                                    const IPosition& pixelAxisMap2,
                                    Array<T>& inDataChunk,
                                    Array<Bool>*& inMaskChunkPtr,
                                    const Cube<Double>& pix2DPos,
                                    const Matrix<Bool>& failed)
// 
// Iterate through a stack of DirectionCoordinate planes and
// interpolate them.  Now in this case, one or more of the DirectionCoordinate
// axes is only of length 1.  We have to special case this, else the
// 2d interpolator will mask all the output.
//
{
//--> I am here. This is copief from Regrid2DMatrix 
// Urk.
// Setup Navigator and tell it which axes are the Direction ones in case
// of other degenerate axes

   IPosition axisPath;
   IPosition outCursorAxes(2, xOutAxis, yOutAxis);
   IPosition outCursorIterShape(2, outCursorShape(xOutAxis), outCursorShape(yOutAxis));
   LatticeStepper outCursorIterStepper(outCursor.shape(), outCursorIterShape, outCursorAxes, axisPath);
   LatticeIterator<T> outCursorIter(outCursor, outCursorIterStepper);
//
   LatticeIterator<Bool>* outMaskCursorIterPtr = 0;
   Lattice<Bool>* outMaskCursorPtr = 0;
   if (outIsMasked) {
     outMaskCursorPtr = new ArrayLattice<Bool>(outMaskIterPtr->rwCursor());
     outMaskCursorIterPtr = new LatticeIterator<Bool>(*outMaskCursorPtr, outCursorIterStepper);
   }
   if (itsShowLevel>0) {
      cerr << "outCursorAxisPath = " << axisPath << endl;
      cerr << "outCursorAxes" << outCursorAxes << endl;
      cerr << "outCursorIterShape = " << outCursorIterShape << endl;
      cerr << "outMatrixCursorIterShape = " << outCursorIter.matrixCursor().shape() << endl;
   }
//
   IPosition inChunkBlc2D(nDim, 0);
   IPosition inChunkTrc2D(nDim);
   inChunkTrc2D = inChunkShape - 1;
//
   IPosition inChunk2DShape(2);
   inChunk2DShape(0) = inChunkTrc2D(xInAxis) - inChunkBlc2D(xInAxis) + 1;
   inChunk2DShape(1) = inChunkTrc2D(yInAxis) - inChunkBlc2D(yInAxis) + 1;
//
   Vector<Double> pix2DPos2(2);
   IPosition outPos3;
   Bool interpOK;
   T result(0);
   for (outCursorIter.reset(); !outCursorIter.atEnd(); outCursorIter++) {

// outCursorIter.position is the location of the BLC of the current matrix within the current
// cursor (tile) of data. outPos3 is the location of the BLC of the current matrix within
// the full lattice

      outPos3 = outPos + outCursorIter.position();

// Fish out the 2D piece of the inChunk relevant to this plane of the cursor

      for (uInt k=0; k<nDim; k++) {
         if (k!=xInAxis&& k!=yInAxis) {
            inChunkBlc2D(k) = outPos3(pixelAxisMap2(k)) - inChunkBlc(k);
            inChunkTrc2D(k) = inChunkBlc2D(k);
         } 
      }
//
      if (itsShowLevel>0) {
         cerr << "inChunkBlc2D, inChunkTrc2D, inChunk2DShape " << inChunkBlc2D 
              << inChunkTrc2D << inChunk2DShape << endl;
      }
      const Matrix<T> inDataChunk2D = inDataChunk(inChunkBlc2D, inChunkTrc2D).reform(inChunk2DShape);
      Matrix<Bool>* inMaskChunk2DPtr = 0;
      if (inIsMasked) {
         inMaskChunk2DPtr = 
            new Matrix<Bool>((*inMaskChunkPtr)(inChunkBlc2D, inChunkTrc2D).reform(inChunk2DShape));
      }

// Now work through each output pixel in the data Matrix and do the interpolation

      uInt nCol = outCursorIter.matrixCursor().ncolumn();
      uInt nRow = outCursorIter.matrixCursor().nrow();
      for (uInt j=0; j<nCol; j++) {
         for (uInt i=0; i<nRow; i++) {
            if (failed(i,j)) {
               outCursorIter.rwMatrixCursor()(i,j) = 0.0;
               if (outIsMasked) {
                  outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False;
               }
            } else {

// Now do the interpolation if all points are unmasked in the input grid
// pix2DPos(i,j,) is the absolute input pixel coordinate in the input lattice
// for the current output pixel.

               pix2DPos2(0) = pix2DPos(i,j,0) - inChunkBlc(xInAxis);
               pix2DPos2(1) = pix2DPos(i,j,1) - inChunkBlc(yInAxis);
               if (itsShowLevel>1) {
                  cerr << "For output [i,j] = " << " [" << i << "," << j << "]" 
                       << " interpolate input at " << pix2DPos2;
               }
//
               if (inIsMasked) {                     
                  interpOK = interp.interp(result, pix2DPos2, inDataChunk2D, *inMaskChunk2DPtr);
               } else {
                  interpOK = interp.interp(result, pix2DPos2, inDataChunk2D);
               }
               if (interpOK) {
                  if (itsShowLevel>1) {
                     cerr  << " giving result " << result << endl;
                  }
                  outCursorIter.rwMatrixCursor()(i,j) = scale * result;
                  if (outIsMasked) {
                     outMaskCursorIterPtr->rwMatrixCursor()(i,j) = True; 
                  }
               } else {
                  if (itsShowLevel>1) {
                      cerr  << " giving Fail" << endl;
                  }
                  outCursorIter.rwMatrixCursor()(i,j) = 0.0;
                  if (outIsMasked) {
                     outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False; 
                  }
               }
            }
            if (pProgressMeter) {
               pProgressMeter->update(iPix); 
               iPix++;
            }
        }
      }
//
      if (itsShowLevel>1) {
         cerr << "output data matrix  = " << outCursorIter.matrixCursor() << endl;
         if (outIsMasked) {
            cerr << "output mask matrix  = " << outMaskCursorIterPtr->matrixCursor() << endl;
         }
      }
      if (outIsMasked) (*outMaskCursorIterPtr)++;
      if (inIsMasked) delete inMaskChunk2DPtr;
   }
//
   if (inIsMasked) delete inMaskChunkPtr;
   if (outIsMasked) {
      delete outMaskCursorIterPtr;
      delete outMaskCursorPtr;
   }
}
*/


template<class T>
void ImageRegrid<T>::regrid1D (MaskedLattice<T>& outLattice,
                               const MaskedLattice<T>& inLattice,
                               const Coordinate& inCoord,
                               const Coordinate& outCoord,
                               const Vector<Int>& inPixelAxes,
                               const Vector<Int>& outPixelAxes,
                               Int inAxisInCoordinate,
                               Int outAxisInCoordinate,
                               const Vector<Int> pixelAxisMap,
                               typename Interpolate2D::Method method,
                               MFrequency::Convert& machine,
                               Bool replicate,
                               Bool useMachine, Bool showProgress)

//
// Any output mask is overwritten
//
{
   const Bool inIsMasked = inLattice.isMasked();
   const Bool outIsMasked = outLattice.isMasked() && outLattice.hasPixelMask() &&
                            outLattice.pixelMask().isWritable();
//
   if (itsShowLevel>0) {
      cerr << "inIsMasked = " << inIsMasked << endl;
      cerr << "outIsMasked = " << outIsMasked << endl;
   }
//
   const IPosition& inShape = inLattice.shape();
   const IPosition& outShape = outLattice.shape();
   const uInt nDim = inLattice.ndim();
   const Int inPixelAxis = inPixelAxes(inAxisInCoordinate);
   const Int outPixelAxis = outPixelAxes(outAxisInCoordinate);

// Generate vector of pixel coordinates

   const uInt nLine = outShape(outPixelAxis);
   Vector<Bool> failed(nLine);
   Block<Float> outX(nLine);
   Bool allFailed = False;
   Bool allGood = True;
//
   if (replicate) {
      Float pixelScale = Float(outShape(outPixelAxis)) / Float(inShape(inPixelAxis));
      make1DCoordinateGrid (outX, pixelScale);
   } else {
      make1DCoordinateGrid (outX, failed, allFailed, allGood,
                            inCoord, outCoord, inAxisInCoordinate,
                            outAxisInCoordinate, machine, useMachine);
   }

// Short cut if all conversions cactus

   if (allFailed) {
      outLattice.set(0.0);
      if (outIsMasked) {
         Lattice<Bool>& outMask = outLattice.pixelMask();
         outMask.set(False);
      }
      return;
   }

// Generate vector of input X values for interpolator

   const uInt nIn = inShape(inPixelAxis);
   Block<Float> inX(nIn);
   if (itsShowLevel>0) cerr << "inX = ";
   for (uInt i=0; i<nIn; i++) {
      inX[i] = Float(i);
      if (itsShowLevel>0) cerr << inX[i] << ",";
   }
   if (itsShowLevel>0) cerr << endl;
//
   if (itsShowLevel>0) cerr << "outX = ";
   for (uInt i=0; i<outX.nelements(); i++) {
      if (itsShowLevel>0) cerr << outX[i] << ",";
   }
   if (itsShowLevel>0) cerr << endl;
//

// Make navigator and iterator for output data and mask. It is vital that
// the "niceShape" is the same for both iterators.  Because the mask and 
// lattice are both TempLattices, one might be on disk, one in core.
// Hence we pick one nice shape and use it on both iterators

   IPosition niceShape = outLattice.niceCursorShape();
   TiledLineStepper outStepper(outShape, niceShape, outPixelAxis);
   LatticeIterator<T> outIter(outLattice, outStepper);
//
   LatticeIterator<Bool>* outMaskIterPtr = 0;
   if (outIsMasked) {
      Lattice<Bool>& outMask = outLattice.pixelMask();    
      TiledLineStepper outMaskStepper(outShape, niceShape, outPixelAxis);
      outMaskIterPtr = new LatticeIterator<Bool>(outMask, outMaskStepper);
   }
//
   IPosition inSubShape(nDim,1);
   IPosition inPos(nDim);
   inSubShape(inPixelAxis) = inShape(inPixelAxis);
//
   if (itsShowLevel>0) {
      cerr << "in, out pixel axis = " << inPixelAxis << ", " << outPixelAxis << endl;  
      cerr << "shape in, shape out" << inShape << outShape << endl;  
      cerr << "inSubShape=" << inSubShape << endl;
   }

// Set interpolator method

   InterpolateArray1D<Float,Float>::InterpolationMethod method1D =
        InterpolateArray1D<Float,Float>::linear;
   if (method==Interpolate2D::NEAREST) {
      method1D = InterpolateArray1D<Float,Float>::nearestNeighbour;
      if (itsShowLevel>0) {
         cerr << "Method = nearest" << endl;
      }
   } else if (method==Interpolate2D::LINEAR) {
      method1D = InterpolateArray1D<Float,Float>::linear;
      if (itsShowLevel>0) {
         cerr << "Method = linear" << endl;
      }
   } else if (method==Interpolate2D::CUBIC) {
      method1D = InterpolateArray1D<Float,Float>::spline;
      if (itsShowLevel>0) {
         cerr << "Method = cubic spline" << endl;
      }
   }

// Progress meter

   ProgressMeter* pProgressMeter = 0;
   if (showProgress) {
     Double nMin = 0.0;
     Double nMax = Double(outLattice.shape().product()) / Double(outIter.cursorShape().product());
     ostrstream oss;
     oss << "Axis " << outPixelAxis + 1 << " : Lines Regridded" << ends;
     pProgressMeter = new ProgressMeter(nMin, nMax, String(oss),
                                        String("Regridding"),
                                        String(""), String(""),
                                        True, max(1,Int(nMax/20)));
   }

// Iterate through output image by line

   Bool goodIsTrue = True;
   Bool extrapolate = False;
   Vector<Bool> dummyOutMask(nLine);
   for (outIter.reset(); !outIter.atEnd(); outIter++) {
      const IPosition& outPos = outIter.position();
      if (itsShowLevel>1) {
         cerr << endl;
         cerr << "Output lattice iterator position = " <<  outPos << endl;
         cerr << "Output lattice iterator cursor shape = " <<  outIter.cursorShape()<< endl;
      }

// Get input vector of data and mask

      for (uInt i=0; i<nDim; i++) {
         inPos(i) = outPos(pixelAxisMap(i));
      }
      const Vector<T>& inY = inLattice.getSlice(inPos, inSubShape, True);
      const Vector<Bool>& inMask = inLattice.getMaskSlice(inPos, inSubShape, True);

      if (itsShowLevel>1) {
         cerr << "inPos=" << inPos << endl;
         cerr << "inY=" << inY << endl;
         cerr << "inY=" << inMask << endl;
      }
//
      if (allGood) {
         if (outIsMasked) {
            InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                     outMaskIterPtr->rwVectorCursor(),
                                                     outX, inX, inY, inMask,
                                                     method1D, goodIsTrue, extrapolate);
         } else {
            InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                     dummyOutMask,
                                                     outX, inX, inY, inMask,
                                                     method1D, goodIsTrue, extrapolate);
         }
      } else {

// AND the coordinate conversion success vector and the input mask

         if (outIsMasked) {
            InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                     outMaskIterPtr->rwVectorCursor(),
                                                     outX, inX, inY, 
                                                    (failed && inMask),
                                                     method1D, goodIsTrue, extrapolate);
         } else {
            InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                     dummyOutMask,
                                                     outX, inX, inY, 
                                                    (failed && inMask),
                                                     method1D, goodIsTrue, extrapolate);
         }
      }
//
      if (itsShowLevel>1) {
         cerr << "outY = " << outIter.rwVectorCursor() << endl;
         if (outIsMasked) cerr << "outMask = " << outMaskIterPtr->rwVectorCursor() << endl;
      }
//
      if (outIsMasked) (*outMaskIterPtr)++;
      if (showProgress) pProgressMeter->update(Double(outIter.nsteps()));
   } 
//
   if (outIsMasked) delete outMaskIterPtr;
   if (showProgress) delete pProgressMeter;
}



template<class T>
void ImageRegrid<T>::make1DCoordinateGrid (Block<Float>& outX,
                                           Vector<Bool>& failed,
                                           Bool& allFailed,
                                           Bool& allGood,
                                           const Coordinate& inCoord,
                                           const Coordinate& outCoord,
                                           Int inAxisInCoordinate,
                                           Int outAxisInCoordinate,
                                           MFrequency::Convert& machine,
                                           Bool useMachine)
{
// Precompute vector of output coordinates to interpolate data at

   Double outPixel2, inPixel2;
   Vector<Double> world, inPixel;
   Vector<Double> outPixel = outCoord.referencePixel().copy();
//
   const uInt nLine = outX.nelements();
   failed.resize(nLine);
   allFailed = True;
   allGood = True;
   Bool ok1 = False;
   Bool ok2 = False;
   MFrequency inMVF, outMVF;
//
   if (useMachine) {

// If we are going to Stoke up the MFrequency machine it means
// we have a SpectralCoordinate; cast to it

      const SpectralCoordinate& inSpecCoord = dynamic_cast<const SpectralCoordinate&>(inCoord);
      const SpectralCoordinate& outSpecCoord = dynamic_cast<const SpectralCoordinate&>(outCoord);
//
      for (uInt i=0; i<nLine; i++) {

// Fill Coordinate pixel locations
/*
         if (itsShowLevel>1) {
            cerr.setf(ios::scientific, ios::floatfield);
            cerr.precision(12);
            cerr << "pixel out, world out, pixel in  = " << outPixel;
         }
*/
//
         outPixel2 = i;
         ok1 = outSpecCoord.toWorld(outMVF, outPixel2);
         if (ok1) {
            inMVF = machine(outMVF).getValue();
            ok2 = inSpecCoord.toPixel(inPixel2, inMVF);
         } 
//
         if (!ok1 || !ok2) {
            failed(i) = True;
            allGood = False;
         } else {
/*
            if (itsShowLevel>1) {
               cerr.setf(ios::scientific, ios::floatfield);
               cerr.precision(12);
               cerr << ", " << world << " " << inPixel << endl;
            }
*/

// This one ok

            outX[i] = inPixel2;
            failed(i) = False;
            allFailed = False;
         }
      }
   } else {
      for (uInt i=0; i<nLine; i++) {

// Fill Coordinate pixel locations

/*
         if (itsShowLevel>1) {
            cerr.setf(ios::scientific, ios::floatfield);
            cerr.precision(12);
            cerr << "pixel out, world out, pixel in  = " << outPixel;
         }
*/
//
         outPixel(outAxisInCoordinate) = i;
         ok1 = outCoord.toWorld(world, outPixel);
         if (ok1) ok2 = inCoord.toPixel(inPixel, world);
//
         if (!ok1 || !ok2) {
            failed(i) = True;
            allGood = False;
         } else {
/*
            if (itsShowLevel>1) {
               cerr.setf(ios::scientific, ios::floatfield);
               cerr.precision(12);
               cerr << ", " << world << " " << inPixel << endl;
            }
*/

// This one ok

            outX[i] = inPixel(inAxisInCoordinate);
            failed(i) = False;
            allFailed = False;
         }
      }
   }
//
   if (itsShowLevel>0) {
      cerr << "allFailed=" << allFailed << endl;
      cerr << "allGood =" << allGood << endl;
   }
   if (itsShowLevel>1) {
       cerr << "failed = " << failed << endl;
       cerr << "outX=";
       for (uInt i=0;i<nLine;i++) {
          cerr << outX[i] << ", ";
       }
       cerr << endl;
   }
}



template<class T>
void ImageRegrid<T>::make1DCoordinateGrid (Block<Float>& outX,
                                           Float pixelScale) const
{
   Float oX = -0.5 + (1.0/2/pixelScale);
   const uInt nLine = outX.nelements();
   for (uInt i=0; i<nLine; i++) {
      outX[i]  = (Float(i) / pixelScale) + oX;
   }
}


template<class T>
void ImageRegrid<T>::checkAxes(IPosition& outPixelAxes,
                               const IPosition& inShape,
                               const IPosition& outShape,
                               const Vector<Int>& pixelAxisMap1,
                               const CoordinateSystem& outCoords)
{
   LogIO os(LogOrigin("ImageRegrid", "regrid(...)", WHERE));
   if (inShape.nelements()==0) {
      os << "The input shape is illegal" << LogIO::EXCEPTION;
   }
   if (outShape.nelements()==0) {
      os << "The output shape is illegal" << LogIO::EXCEPTION;
   }
//
   Int n1 = outPixelAxes.nelements();
   const Int nOut = outShape.nelements();
   if (n1 > nOut) {
      os << "You have specified more pixel axes than there are dimensions" << LogIO::EXCEPTION;
   }

// Fill in all axes if null pixelAxes given

   if (n1==0) {
      outPixelAxes = IPosition::makeAxisPath(nOut);
      n1 = outPixelAxes.nelements();
   }

// Check for Stokes and discard

   Int outCoordinate, outAxisInCoordinate;
   Int j = 0;
   for (Int i=0; i<n1; i++) {

// Find pixel axis in output coordinates if not yet done

      outCoords.findPixelAxis(outCoordinate, outAxisInCoordinate, 
                              outPixelAxes(i));
      if (outCoordinate==-1 || outAxisInCoordinate==-1) {
         ostrstream oss;
         oss << "Pixel axis " << outPixelAxes(i)+1 << 
                " has been removed from the output CoordinateSystem" << endl;
         os << String(oss) << LogIO::EXCEPTION;         
      }

// Find out the coordinate type and don't allow Stokes
 
      Coordinate::Type type = outCoords.type(outCoordinate);

      if (type==Coordinate::STOKES) {
         os << LogIO::WARN << "The Stokes axis cannot be regridded - removing from list" << endl;
      } else {
         Bool ok = True;
         if (outShape(outPixelAxes(i))==1) {

// We can (will be able to) handle DirectionCoordinates which
// are degenerate in one axis.  The test for this is in Regrid2d
// Otherwise, regridding a one-pixel axis is useless.

            if (type!=Coordinate::DIRECTION) {
              os << LogIO::WARN << "Cannot regrid axis " << outPixelAxes(i)+1 << " because it is of unit length - removing from list" << endl;
              ok = False;
            }
         } 
//
         if (ok) {
            outPixelAxes(j) = outPixelAxes(i);
            j++;
         }
      }
   }
   outPixelAxes.resize(j,True);
   n1 = outPixelAxes.nelements();

// Check for range

   Vector<Bool> found(nOut, False);
   for (Int i=0; i<n1; i++) {
      if (outPixelAxes(i)<0 || outPixelAxes(i)>=nOut) {
         os << "Pixel axes are out of range" << LogIO::EXCEPTION;
      }
//
      if (found(outPixelAxes(i))) {
         os << "Specified pixel axes " << outPixelAxes+1
            << " are not unique" << LogIO::EXCEPTION;
      } else {
         found(outPixelAxes(i)) = True;
      }
   }

// CHeck non-regriddded axis shapes are ok

   for (Int i=0; i<nOut; i++) {
      Bool foundIt = False;
      for (Int j=0;j<n1; j++) {
         if (outPixelAxes(j)==i) {
            foundIt = True;
            break;
         }
      }

// pixelAxisMap1(i) says where pixel axis i in the output image
// is in the input image

      if (!foundIt && outShape(i) != inShape(pixelAxisMap1(i))) {
           os << "Any axis not being regridded must have the same "
              << "input and output shapes" << LogIO::EXCEPTION;
      }  
   }
}


template<class T>
void ImageRegrid<T>::findMaps (uInt nDim, 
                               Vector<Int>& pixelAxisMap1,
                               Vector<Int>& pixelAxisMap2,
                               const CoordinateSystem& inCoords,
                               const CoordinateSystem& outCoords) const
{

// Find mapping between CoordinateSystems
//
// worldAxisMap(i) is the location of world axis i (from the supplied
// coordinate system, cSys, in the current coordinate system. 
// worldAxisTranspose(i) is the location of world axis i (from the current
// coordinate system) in the supplied coordinate system, cSys.  

   Vector<Int> worldAxisTranspose, worldAxisMap;
   Vector<Bool> worldRefChange;
   if (!outCoords.worldMap(worldAxisMap, worldAxisTranspose,
                           worldRefChange, inCoords)) {
      throw(AipsError(inCoords.errorMessage()));
   }

// pixelAxisMap1(i) says where pixel axis i in the output coordinate system
// is in the input coordinate system
// pixelAxisMap2(i) says where pixel axis i in the input coordinate system
// is in the output coordinate system

   pixelAxisMap1.resize(nDim);
   pixelAxisMap2.resize(nDim);
   for (uInt paOut=0; paOut<nDim; paOut++) {
      Int waOut = outCoords.pixelAxisToWorldAxis(paOut);
      Int waIn = worldAxisTranspose(waOut);
      Int paIn = inCoords.worldAxisToPixelAxis(waIn);      
//
      pixelAxisMap1(paOut) = paIn;
      pixelAxisMap2(paIn) = paOut;
   }
//
   if (itsShowLevel>0) {
      cerr << "worldmap, worldtranspose, refChange = " <<
             worldAxisMap << worldAxisTranspose << worldRefChange << endl;
      cerr << "pixelaxismap{1,2} = " << pixelAxisMap1 << pixelAxisMap2 << endl;
   }
}   



template<class T>
void ImageRegrid<T>::copyDataAndMask(MaskedLattice<T>& out,
                                     MaskedLattice<T>& in) const
{   
// Use the same stepper for input and output.
   
   IPosition cursorShape = out.niceCursorShape(); 
   LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);
   Bool doMask = out.isMasked() && out.hasPixelMask() && out.pixelMask().isWritable();
   if (itsShowLevel>0) {
      cerr << "outIsMasked = " << out.isMasked() << endl;
      cerr << "out.pixelMask.isWritable" << out.pixelMask().isWritable() << endl;
      cerr << "doMask=" << doMask << endl;
   }
   
// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.
 
   LatticeIterator<T> dummyIter(out);
   RO_LatticeIterator<T> iter(in, stepper);
   Lattice<Bool>* maskOutPtr = 0;
   if (doMask) maskOutPtr = &out.pixelMask();
   for (iter.reset(); !iter.atEnd(); iter++) {
      out.putSlice (iter.cursor(), iter.position());
      if (doMask) {
         maskOutPtr->putSlice(in.getMaskSlice(iter.position(),
                              iter.cursorShape()), iter.position());
      }
   }
}


template<class T>
Double ImageRegrid<T>::findScaleFactor(const Unit& units, 
                                       const DirectionCoordinate& dirIn,
                                       const DirectionCoordinate& dirOut,
                                       LogIO& os) const
//
// Direction coordinates have been set to same axis units
//
{
   Double fac = 1.0;
   String t = units.getName();
   t.upcase();
   if (t==String("JY/PIXEL")) {
      Vector<Double> incIn = dirIn.increment();
      Vector<Double> incOut = dirOut.increment();
//
      fac = abs(incOut(0)*incOut(1) / incIn(0) / incIn(1));
      os << "Applying Jy/pixel scale factor of " << fac << endl;
   }
   return fac;
}

