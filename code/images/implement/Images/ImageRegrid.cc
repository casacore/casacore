//# ImageRegrid.cc: Regrids images
//# Copyright (C) 1996,1997,1998,1999,2000
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
#include <aips/Arrays/Matrix.h>
#include <aips/Containers/Block.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <trial/Coordinates/ObsInfo.h>
#include <trial/Images/TempImage.h>
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
#include <trial/Mathematics/InterpolateArray1D.h>
#include <trial/Mathematics/Interpolate2D.h>
#include <aips/Measures/MDirection.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MeasTable.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Logging/LogIO.h>
#include <aips/Quanta/MVEpoch.h>
#include <aips/Quanta/MVPosition.h>
#include <aips/Utilities/Assert.h>

#include <strstream.h>
#include <fstream.h>


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
                            Interpolate2D<T>::Method method,
                            const IPosition& outPixelAxesU,
                            const ImageInterface<T>& inImage)
//
// The whole thing is complicated by masks.  We have code
// that take shortcuts if masks are not involved, but at
// some cost to complexity.
//
{
   IPosition outShape = outImage.shape();
   IPosition inShape = inImage.shape();
   const uInt nDim = inImage.ndim();
   if (nDim != outImage.ndim()) {
      throw(AipsError("The input and output images must have the same number of axes"));
   }
   const Bool outIsMasked = outImage.isMasked() && outImage.hasPixelMask() &&
                            outImage.pixelMask().isWritable();
   LogIO os(LogOrigin("ImageRegrid", "regrid(...)", WHERE));
//
   const CoordinateSystem& inCoords = inImage.coordinates();
   CoordinateSystem outCoords = outImage.coordinates();
   IPosition outPixelAxes = outPixelAxesU;

// Find world and pixel axis maps

   Vector<Int> worldAxisMap, worldAxisTranspose;
   Vector<Int> pixelAxisMap1, pixelAxisMap2;
   Vector<Bool> worldRefChange;
   findMaps (nDim, worldAxisMap, worldAxisTranspose,
             worldRefChange, pixelAxisMap1,
             pixelAxisMap2, inCoords, outCoords);

// Check user pixel axes specifications

   checkAxes(outPixelAxes, inShape, outShape, pixelAxisMap2, outCoords);
   const uInt nOutPixelAxes = outPixelAxes.nelements();

// Set output shape.  This shape is incremental, for each regridding 
// pass it incrementally changes from the input shape to the output shape
// We account here for different pixel axis orders

   IPosition outShape2(nDim);
   for (uInt paOut=0; paOut<nDim; paOut++) {
      outShape2(paOut) = inShape(pixelAxisMap1(paOut));
   }

// Specify input and output for each regridding pass

   MaskedLattice<T>* inPtr = 0;
   MaskedLattice<T>* outPtr = 0;

// Loop over specified pixel axes of output image

   Int outCoordinate, outAxisInCoordinate;
   Int inCoordinate, inAxisInCoordinate;
   Vector<Bool> doneOutPixelAxes(outCoords.nPixelAxes());
   doneOutPixelAxes.set(False);
//
   for (uInt i=0; i<nOutPixelAxes; i++) {

// Find equivalent world axis

      Int outWorldAxis = outCoords.pixelAxisToWorldAxis(outPixelAxes(i));

// Find pixel axis in output coordinates if not yet done

      if (!doneOutPixelAxes(i)) {
         outCoords.findPixelAxis(outCoordinate, outAxisInCoordinate, 
                                 outPixelAxes(i));
         Coordinate::Type type = outCoords.type(outCoordinate);

// Find Coordinate in input image.

         Int inPixelAxis = pixelAxisMap1(outPixelAxes(i));
         Int inWorldAxis = inCoords.pixelAxisToWorldAxis(inPixelAxis);
         inCoords.findPixelAxis(inCoordinate, inAxisInCoordinate, inPixelAxis);
         if (inCoordinate==-1 || inAxisInCoordinate==-1) {
           ostrstream oss1;
           ostrstream oss2;
           oss1 << outCoords.showType(outCoordinate);
           oss2 << outPixelAxes(i)+1;
           String msg = String("Output axis (") + String(oss2) + 
                        String(") of coordinate type ") + String(oss1) +
                        String("does not have a coordinate in the input CoordinateSystem");
           os << msg << LogIO::EXCEPTION;
         }

// Where are the input and output pixel axes for this  coordinate ?  Some coordinates,
// (apart form DirectionCoordinate), e.g. LinearCoordinate may have more than 
// one pixel axis. But we will do them in multiple passes; only DirectionCoordinate
// is coupled

         Vector<Int> outCoordPixelAxes = outCoords.pixelAxes(outCoordinate);
         Vector<Int> inCoordPixelAxes = inCoords.pixelAxes(inCoordinate);

// Now we need to break the polymorphic nature  of coordinates.  The
// ultimate solution is to have regridding stuff in them...  One day.

         if (type==Coordinate::DIRECTION) {
            ostrstream oss;
            oss << "Regridding output axes " << outCoordPixelAxes + 1 << " which are of Coordinate type " << 
                   outCoords.showType(outCoordinate) << endl;
            os << LogIO::NORMAL << String(oss) << LogIO::POST;

// Update the incremental output image shape.

            outShape2(outCoordPixelAxes(0)) = outShape(outCoordPixelAxes(0));
            outShape2(outCoordPixelAxes(1)) = outShape(outCoordPixelAxes(1));
            if (outShape2(outCoordPixelAxes(0))==1 && outShape2(outCoordPixelAxes(1))==1) {
               os << "You cannot regrid the DirectionCoordinate as its plane is of shape [1,1]" << LogIO::EXCEPTION;
            }

// Set input and output images for this pass. The new  input must be the last 
// output image.  We end up with at least one temporary image. Could
// probably improve this. 

            if (i==0) {
               inPtr = inImage.cloneML();
            } else {
               delete inPtr;
               inPtr = outPtr;
            }

// Attach mask if out is masked.  Don't init mask because it will be overwritten

            TiledShape tmp(outShape2);
            outPtr = new TempImage<T>(tmp, outCoords);
            if (outIsMasked) {
               TempLattice<Bool> mask(tmp);
               TempImage<T>* tmpPtr = dynamic_cast<TempImage<T>*>(outPtr);
               tmpPtr->attachMask(mask);
            }

// Get DirectionCoordinates for input and output

            Vector<String> units(2);
            units.set("deg");
            DirectionCoordinate inDir = inCoords.directionCoordinate(inCoordinate);
            DirectionCoordinate outDir = outCoords.directionCoordinate(outCoordinate);
            if (!inDir.setWorldAxisUnits(units, True)) {
               os << "Failed to set input DirectionCoordinate units to degrees" << LogIO::EXCEPTION;
            }
            if (!outDir.setWorldAxisUnits(units, True)) {
               os << "Failed to set output DirectionCoordinate units to degrees" << LogIO::EXCEPTION;
            }

// Possibly make Direction reference conversion machine
   
            MDirection::Convert machine;
            Bool madeIt = False;
            if (!itsDisableConversions) {
               madeIt = makeDirectionMachine(os, machine, inDir, outDir, 
                                             inCoords.obsInfo(),
                                             outCoords.obsInfo());
            }

// Regrid 

            if (itsShowLevel>0) {
               cerr << "usemachine=" << madeIt << endl;
            }
            regrid2D (*outPtr, *inPtr, inDir, outDir, inCoordPixelAxes,
                      outCoordPixelAxes, pixelAxisMap2, method,
                      machine, madeIt);

// Note that we have done two pixel axes in this pass

            doneOutPixelAxes(outCoordPixelAxes(0)) = True;
            doneOutPixelAxes(outCoordPixelAxes(1)) = True;
         } else {
            ostrstream oss;
            oss << "Regridding output axis " << outPixelAxes(i)+1 << " which is of Coordinate type " << 
                   outCoords.showType(outCoordinate) << endl;
            os << LogIO::NORMAL << String(oss) << LogIO::POST;


// Update the incremental output image shape.

            outShape2(outCoordPixelAxes(outAxisInCoordinate)) = 
                  outShape(outCoordPixelAxes(outAxisInCoordinate));

// Set input and output images for this pass. The new  input must be the last 
// output image.  We end up with at least one temporary image. Could
// probably improve this.

            if (i==0) {
               inPtr = inImage.cloneML();
            } else {
               delete inPtr;
               inPtr = outPtr;
            }

// Attach mask if out is masked.  

            TiledShape tmp(outShape2);
            outPtr = new TempImage<T>(tmp, outCoords);
            if (outIsMasked) {
               TempLattice<Bool> mask(tmp);
               mask.set(True);
               TempImage<T>* tmpPtr = dynamic_cast<TempImage<T>*>(outPtr);
               tmpPtr->attachMask(mask);
            }

// Set world axis units for input and output coordinates for this pixel
// axis to be the same.  We can only do this via the CoordinateSystem (or casting)


            Vector<String> inUnits = inCoords.worldAxisUnits();
            Vector<String> outUnits = outCoords.worldAxisUnits();
            outUnits(outWorldAxis) = inUnits(inWorldAxis);
            if (!outCoords.setWorldAxisUnits(outUnits, True)) {
               os << "Failed to set output CoordinateSystem units" << LogIO::EXCEPTION;
            }
//
            const Coordinate& inCoord = inCoords.coordinate(inCoordinate);
            const Coordinate& outCoord = outCoords.coordinate(outCoordinate);

// Possibly make Frequency reference conversion machine

            Bool madeIt = False;
            MFrequency::Convert machine;
            if (!itsDisableConversions && type==Coordinate::SPECTRAL) {
               madeIt = makeFrequencyMachine(os, machine, 
                                             inCoordinate, outCoordinate,
                                             inCoords, outCoords,
                                             inCoords.obsInfo(),
                                             outCoords.obsInfo());
            }

// Regrid 

            if (itsShowLevel>0) {
               cerr << "usemachine=" << madeIt << endl;
            }
            regrid1D (*outPtr, *inPtr, inCoord, outCoord, inCoordPixelAxes,
                      outCoordPixelAxes, inAxisInCoordinate, outAxisInCoordinate,
                      pixelAxisMap2, method, machine, madeIt);

// Note that we have done two pixel axes in this pass

            doneOutPixelAxes(outCoordPixelAxes(outAxisInCoordinate)) = True;
         }
      }
   }

// Copy final pass to output

   delete inPtr;
   if (outIsMasked) {
      copyDataAndMask(outImage, *outPtr);
   } else {
      outImage.copyData(*outPtr);
   }
   delete outPtr;
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
      copyDataAndMask(outSub, outSubMask, inSub);
   } else {
      outSub.copyData(inSub);
   }
//
   return True;
}


template<class T>
void ImageRegrid<T>::regrid2D (MaskedLattice<T>& outLattice,
                               const MaskedLattice<T>& inLattice,
                               const DirectionCoordinate& inCoord,
                               const DirectionCoordinate& outCoord,
                               const Vector<Int> inCoordPixelAxes,
                               const Vector<Int> outCoordPixelAxes,
                               const Vector<Int> pixelAxisMap,
                               Interpolate2D<T>::Method method,
                               MDirection::Convert& machine,
                               Bool useMachine)
//
// If something other than DirectionCoordinate ever needs to use 2D 
// I will need to generalize this slightly. 
// Any output mask is overwritten
//
{
   LogIO os(LogOrigin("ImageRegrid", "regrid2D(...)", WHERE));
//   ofstream outf("junk.txt");

// Setup

   AlwaysAssert(inCoordPixelAxes.nelements()==2, AipsError);
   AlwaysAssert(outCoordPixelAxes.nelements()==2, AipsError);
   Bool inIsMasked = inLattice.isMasked();
   Bool outIsMasked = outLattice.isMasked() && outLattice.hasPixelMask() &&
                      outLattice.isMaskWritable();
   if (itsShowLevel>0) {
      cerr << LogIO::WARN << "inIsMasked " << inIsMasked << endl;
      cerr << LogIO::WARN << "outIsMasked " << outIsMasked << endl;
   }
//
   const Int& inPixelAxis0 = inCoordPixelAxes(0);
   const Int& inPixelAxis1 = inCoordPixelAxes(1);
   const IPosition& inShape = inLattice.shape();
   const IPosition& outShape = outLattice.shape();
   const uInt nDim = inLattice.ndim();
   const IPosition inCoordPixelAxes2(inCoordPixelAxes);

// Set up navigation path for output.   We get the specified 2 axes first.

   IPosition outPath = IPosition::makeAxisPath(nDim, IPosition(outCoordPixelAxes));
   if (itsShowLevel>0) cerr << "out path = " << outPath << endl;

// Make navigator and iterator for output data and mask.  It is vital that
// the "niceShape" is the same for both iterators.  Because the mask and 
// lattice are both TempLattices, one might be on disk, one in core.
// Hence we pick one nice shape and use it on both iterators

   IPosition niceShape = outLattice.niceCursorShape();
   LatticeStepper outStepper(outShape, niceShape, outPath, LatticeStepper::RESIZE);
   LatticeIterator<T> outIter(outLattice, outStepper);

// Deal with mask.  Stepper will make a reference copy of the mask

   LatticeIterator<Bool>* outMaskIterPtr = 0;
   if (outIsMasked) {
      Lattice<Bool>& outMask = outLattice.pixelMask();
      LatticeStepper outMaskStepper(outShape, niceShape, outPath, LatticeStepper::RESIZE);
      outMaskIterPtr = new LatticeIterator<Bool>(outMask, outMaskStepper);
   }

// Get shape of the little array we are going to use for each pixel's regridding.  
// Depends on interpolation scheme. Need this information to come from 
// Interpolate2D really Its 2x2 for bilinear and 4x4 for bicubic  (blc locations
// of the grid differ by 1)

   Int xOff = 0, yOff = 0;
   Int width = 2, widthm1;
   if (method==Interpolate2D<T>::CUBIC) {
      xOff = -1;
      yOff = -1;
      width = 4;
   } 
   if (itsShowLevel>0) cerr << "Offsets, width=" << xOff << yOff << width << endl;
   widthm1 = width - 1;

// These tell us which chunk of input data we need to service each
// iteration through the output image
   
   IPosition inChunkBlc(nDim);
   IPosition inChunkTrc(nDim);

// These IPositions tells us which little piece of the input image
// to read.  This is what we regrid from.  

   IPosition inBlc(nDim);
   IPosition inTrc(nDim);
   IPosition inSubShape(2,width,width);

// Create coordinate conversion vectors
  
   Vector<Double> world(2), inPixel(2), outPixel(2);

// Create 2D interpolator

   Interpolate2D<T> interp(method);

// Iterate through output image

   Vector<Double> in2DPos2(2);
   IPosition outPos4, outPos3, outPos2, inPos;
   T result = 0.0;
   Bool anyBad;
   Int minInX, minInY, maxInX, maxInY;
//
   for (outIter.reset(); !outIter.atEnd(); outIter++) {
      const IPosition& cursorShape = outIter.cursor().shape();
      const IPosition& outPos = outIter.position();

      if (itsShowLevel>0) cerr << endl;

// outPos is the location of the blc of the lattice iterator cursor

      if (itsShowLevel>1) {
         cerr << "Output lattice iterator position = " <<  outPos << endl;
         cerr << "Shape of cursor = " << outIter.cursor().shape() << endl;
      }

// Compute a Matrix of coordinates for this cursor.   The axes of interest 
// are the first two axes of the cursor

      Cube<Double> in2DPos(cursorShape(0), cursorShape(1), 2);
      Cube<Int> inBlc2D(cursorShape(0), cursorShape(1), 2);
      Matrix<Bool> failed(cursorShape(0), cursorShape(1));
      minInX =  100000000;
      minInY =  100000000;
      maxInX = -100000000;
      maxInY = -100000000;
      Bool allFailed = True, ok1, ok2;
      MDirection inMD, outMD;
      MVDirection inMVD, outMVD;
      for (Int j=0; j<cursorShape(1); j++) {
         for (Int i=0; i<cursorShape(0); i++) {
            outPixel(0) = i + outPos(0);
            outPixel(1) = j + outPos(1);

// Do coordinate conversions. out pixel to world to in pixel
// for the axes of interest
            
            if (useMachine) {
              ok1 = outCoord.toWorld(outMD, outPixel);            
              ok2 = False;
              if (ok1) {
                 inMD = machine(outMD);
                 ok2 = inCoord.toPixel(inPixel, inMD);
              } 
            } else { 
               world = 0.0;
               ok1 = outCoord.toWorld(world, outPixel);
               ok2 = False;
               inPixel = 0.0;
               if (ok1) ok2 = inCoord.toPixel(inPixel, world);
//
               if (itsShowLevel>1) {
                 cerr << "outPos, world, inPos, ok1, ok2 = " 
                      << outPixel << world << inPixel << ", " << ok1 << ", " << 
                       ok2 << endl;
               }
            }
            if (!ok1 || !ok2) {
               failed(i,j) = True;
            } else {

// Find pixel to left and below point of interest in input grid for
// pixel axes of interest

               inBlc2D(i,j,0) = static_cast<Int>(floor(inPixel(0))) + xOff;
               inBlc2D(i,j,1) = static_cast<Int>(floor(inPixel(1))) + yOff;
               minInX = min(minInX,inBlc2D(i,j,0)); 
               minInY = min(minInY,inBlc2D(i,j,1)); 
               maxInX = max(maxInX,inBlc2D(i,j,0)); 
               maxInY = max(maxInY,inBlc2D(i,j,1)); 
               allFailed = False;

// This gives the 2D input pixel coordinate to find the interpolated
// result at

               in2DPos(i,j,0) = inPixel(0) - inBlc2D(i,j,0);
               in2DPos(i,j,1) = inPixel(1) - inBlc2D(i,j,1);
               failed(i,j) = False;
            }
         }
      }

// Does the output map to anywhere on the input ?

      Bool missedIt = False;
      if (!allFailed) {
         missedIt  = (minInX<0 && maxInX<0)  ||
                     (minInX>=inShape(inPixelAxis0) && maxInX>inShape(inPixelAxis0)) ||
                     (minInY<0 && maxInY<0)  ||
                     (minInY>=inShape(inPixelAxis1) && maxInY>inShape(inPixelAxis1));
      }
      if (itsShowLevel>0) {
         cerr << "allFailed, missedIt  = " << allFailed << ", " << missedIt << endl;
      }
      if (itsShowLevel>1) {
         cerr << "failed  = " << failed << endl;
      }
//
      if (missedIt || allFailed) {

// If all our transformations failed, take a short cut
// Probably the whole thing will end up masked anyway

         outIter.rwCursor().set(0.0);
         if (outIsMasked) outMaskIterPtr->rwCursor().set(False);
       } else {

// Now get a chunk of input data which we will access over and over
// as we interpolate it.  If we just get hold of little 4x4 bits as 
// needed, we pay a large speed price.  For the axes that are not 
// being regridded, the input and output shapes are the same.  This
// chunk will generally just dangle over the tile boundaries
// which is very annoying, as it makes it less efficient

         if (itsShowLevel>0) {
            cerr << "minInX, maxInX, minInY, maxInY = " << 
                     minInX << ", " << maxInX << ", " <<  minInY << ", " << maxInY << endl;
         }
         for (uInt k=0; k<nDim; k++) {
            inChunkBlc(k) = outPos(pixelAxisMap(k));
            inChunkTrc(k) = outIter.endPosition()(pixelAxisMap(k));
         }
         inChunkBlc(inPixelAxis0) = max(0,minInX);
         inChunkBlc(inPixelAxis1) = max(0,minInY);
//
         inChunkTrc(inPixelAxis0) = min(inShape(inPixelAxis0)-1, (maxInX+widthm1));
         inChunkTrc(inPixelAxis1) = min(inShape(inPixelAxis1)-1, (maxInY+widthm1));
         if (itsShowLevel>0) {
            cerr << "inCHunkBlc, inChunkTrc " << inChunkBlc << inChunkTrc << endl;
         }
//
         IPosition inChunkShape = inChunkTrc - inChunkBlc + 1;
         Array<T> inDataChunk = inLattice.getSlice(inChunkBlc, inChunkShape);
         Array<Bool>* inMaskChunkPtr = 0;
         if (inIsMasked) {
            inMaskChunkPtr = new Array<Bool>(inLattice.getMaskSlice(inChunkBlc, inChunkShape));
         }

// Iterate through the output cursor by Matrix.  This gets us just a few
// percent speed up over iterating through pixel by pixel. We have set 
// up the axis path so that the specified 2 axes come first in the cursor

         ArrayLattice<T> outCursor(outIter.rwCursor());
         IPosition outCursorIterShape(nDim,1);
         outCursorIterShape(0) = cursorShape(0);
         outCursorIterShape(1) = cursorShape(1);
         if (itsShowLevel>0) {
            cerr << "outCursorIterShape = " << outCursorIterShape << endl;
         }
//
         LatticeIterator<T> outCursorIter(outCursor, outCursorIterShape);
         LatticeIterator<Bool>* outMaskCursorIterPtr = 0;
         if (outIsMasked) {
            ArrayLattice<Bool> outMaskCursor(outMaskIterPtr->rwCursor());
            outMaskCursorIterPtr = new LatticeIterator<Bool>(outMaskCursor, outCursorIterShape);
         }
//
         for (outCursorIter.reset(); !outCursorIter.atEnd(); outCursorIter++) {

// outPos2 in the location of the BLC of the current matrix within
// the current cursor (tile) of data

            const IPosition& outPos2 = outCursorIter.position();

// outPos3 is the location of the BLC of the current matrix within
// the full lattice
 
            outPos3 = outPos + outPos2;
            for (Int j=0; j<cursorShape(1); j++) {
               for (Int i=0; i<cursorShape(0); i++) {
                  if (itsShowLevel>1) {
                     outPos4 = outPos3; 
                     outPos4(0) = outPos4(0) + i;
                     outPos4(1) = outPos4(1) + j;
                  }

                  if (failed(i,j)) {
                     outCursorIter.rwMatrixCursor()(i,j) = 0.0;
                     if (outIsMasked) {
                        outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False;
                     }
                     if (itsShowLevel>1) cerr << "Coord fail - Putting zero to image at " << outPos4 << endl;
                  } else {

// Define input pixel grid relative to the input data chunk we read in.
// I.e. relative to inChunkBlc

                     for (uInt k=0; k<nDim; k++) {
                        inBlc(k) = outPos3(pixelAxisMap(k)) - inChunkBlc(k);
                        inTrc(k) = outPos3(pixelAxisMap(k)) - inChunkBlc(k);
                     }
                     inBlc(inPixelAxis0) = inBlc2D(i,j,0) - inChunkBlc(inPixelAxis0);
                     inBlc(inPixelAxis1) = inBlc2D(i,j,1) - inChunkBlc(inPixelAxis1);
                     inTrc(inPixelAxis0) = inBlc(inPixelAxis0) + widthm1;
                     inTrc(inPixelAxis1) = inBlc(inPixelAxis1) + widthm1;
                     if (itsShowLevel>1) cerr << "inBlc, inTrc = " << inBlc << inTrc << endl;

// See if the input grid is contained within the input image
// We only have to worry about the regridding axes.

                     if (inBlc(inPixelAxis0) < 0 || inBlc(inPixelAxis1) < 0 || 
                        inTrc(inPixelAxis0) >= inChunkShape(inPixelAxis0) ||
                        inTrc(inPixelAxis1) >= inChunkShape(inPixelAxis1)) {
                        outCursorIter.rwMatrixCursor()(i,j) = 0.0;
                        if (outIsMasked) {
                           outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False;
                        }
                     } else {

// Get hold of the input pixels and mask in the little piece required 
// for regridding.  After stripping off degenerate axes, the shape 
// should be 2D.  If not, an error will result in Interpolate2D.  
// It's faster to create these little arrays here rather than 
// pre-create and assign (conformance checking ?)

                        const Matrix<T>& inDataSub = 
                           inDataChunk(inBlc, inTrc).reform(inSubShape);
                        anyBad = False;
                        if (inIsMasked) {
                           const Matrix<Bool>& inMaskSub = 
                              (*inMaskChunkPtr)(inBlc, inTrc).reform(inSubShape);
                           anyBad = anyBadPixels(width, inMaskSub);
                        }

// Now do the interpolation if all points are unmasked in the input grid

                        if (anyBad) {
                           outCursorIter.rwMatrixCursor()(i,j) = 0.0;
                           if (outIsMasked) {
                              outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False;
                           }
                        } else {
                           in2DPos2(0) = in2DPos(i,j,0);
                           in2DPos2(1) = in2DPos(i,j,1);
//
                           if (itsShowLevel>1) {
                              cerr << "Interpolate array of shape " << 
                                    inDataSub.shape() << " at " << in2DPos2 << endl;
                           }
//
                           if(interp.interp(result, in2DPos2, inDataSub)) {
                              if (itsShowLevel>1) {
                                 cerr << "result = " << result << endl;
                                 cerr << "OK   - Putting " << result << " to image at" << outPos4 << endl;
                              }
                              outCursorIter.rwMatrixCursor()(i,j) = result;
                              if (outIsMasked) {
                                 outMaskCursorIterPtr->rwMatrixCursor()(i,j) = True; 
                              }
                           } else {
                              if (itsShowLevel>1) cerr << "Fail - Putting zero to image at " << outPos4 << endl;
                              outCursorIter.rwMatrixCursor()(i,j) = 0.0;
                              if (outIsMasked) {
                                 outMaskCursorIterPtr->rwMatrixCursor()(i,j) = False; 
                              }
                           }
                        }
                     }
                  }
               }
            }
            if (outIsMasked) (*outMaskCursorIterPtr)++;
         }
         if (inIsMasked) delete inMaskChunkPtr;
         if (outIsMasked) delete outMaskCursorIterPtr;
      }
      if (outIsMasked) (*outMaskIterPtr)++;
   } 
//
   if (outIsMasked) delete outMaskIterPtr;
}



template<class T>
void ImageRegrid<T>::regrid1D (MaskedLattice<T>& outLattice,
                               const MaskedLattice<T>& inLattice,
                               const Coordinate& inCoord,
                               const Coordinate& outCoord,
                               const Vector<Int>& inCoordPixelAxes,
                               const Vector<Int>& outCoordPixelAxes,
                               Int inAxisInCoordinate,
                               Int outAxisInCoordinate,
                               const Vector<Int> pixelAxisMap,
                               Interpolate2D<T>::Method method,
                               MFrequency::Convert& machine,
                               Bool useMachine)

//
// Any output mask is overwritten
//
{
   const Bool inIsMasked = inLattice.isMasked();
   const Bool outIsMasked = outLattice.isMasked() && outLattice.hasPixelMask() &&
                            outLattice.pixelMask().isWritable();

   if (itsShowLevel>0) {
      cerr << "inIsMasked = " << inIsMasked << endl;
      cerr << "outIsMasked = " << outIsMasked << endl;
   }
//
   const IPosition& inShape = inLattice.shape();
   const IPosition& outShape = outLattice.shape();
   const uInt nDim = inLattice.ndim();
   const Int inPixelAxis = inCoordPixelAxes(inAxisInCoordinate);
   const Int outPixelAxis = outCoordPixelAxes(outAxisInCoordinate);

// If we are going to Stoke up the MFrequency machine it means
// we have a SpectralCoordinate, do cast to it
  
   const SpectralCoordinate& inSpecCoord = dynamic_cast<const SpectralCoordinate&>(inCoord);
   const SpectralCoordinate& outSpecCoord = dynamic_cast<const SpectralCoordinate&>(outCoord);

// Precompute vector of output coordinates to interpolate data at
  
   Vector<Double> world, inPixel;
   Vector<Double> outPixel = outCoord.referencePixel().copy();
   Double outPixel2, inPixel2;
//
   const uInt nLine = outShape(outPixelAxis);
   Vector<Bool> failed(nLine);
   Block<Float> outX(nLine);
   Bool allFailed = True;
   Bool allGood = True;
   Bool ok1, ok2;
   MFrequency inMF, outMF;
//
   for (uInt i=0; i<nLine; i++) {

// Fill Coordinate pixel locations

      if (itsShowLevel>1) {
         cerr.setf(ios::scientific, ios::floatfield);
         cerr.precision(12);
         cerr << "pixel out, world out, pixel in  = " << outPixel;
      }
//
      if (useMachine) {
         outPixel2 = i;
         ok1 = outSpecCoord.toWorld(outMF, outPixel2);
         if (ok1) {
            inMF = machine(outMF);
            ok2 = inSpecCoord.toPixel(inPixel2, inMF);
         } 
      } else {
         outPixel(outAxisInCoordinate) = i;
         ok1 = outCoord.toWorld(world, outPixel);
         if (ok1) ok2 = inCoord.toPixel(inPixel, world);
         inPixel2 = inPixel(inAxisInCoordinate);
      }
      if (!ok1 || !ok2) {
         failed(i) = True;
         allGood = False;
      } else {
         if (itsShowLevel>1) {
            cerr.setf(ios::scientific, ios::floatfield);
            cerr.precision(12);
            cerr << ", " << world << " " << inPixel << endl;
         }

// This one ok

         outX[i] = inPixel2;
         failed(i) = False;
         allFailed = False;
      }
   }
//
   if (itsShowLevel>0) {
      cerr << "allFailed=" << allFailed << endl;
      cerr << "allGood =" << allGood << endl;
//
       cerr << "outX=";
       for (uInt i=0;i<nLine;i++) {
          cerr << outX[i] << ", ";
       }
       cerr << endl;
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
   if (itsShowLevel) cerr << "inX = ";
   for (uInt i=0; i<nIn; i++) {
      inX[i] = Float(i);
      if (itsShowLevel) cerr << inX[i] << ",";
   }
   if (itsShowLevel>0) cerr << endl;

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

   InterpolateArray1D<Float,Float>::InterpolationMethod method1D;
   if (method==Interpolate2D<T>::NEAREST) {
      method1D = InterpolateArray1D<Float,Float>::nearestNeighbour;
      if (itsShowLevel>0) {
         cerr << "Method = nearest" << endl;
      }
   } else if (method==Interpolate2D<T>::LINEAR) {
      method1D = InterpolateArray1D<Float,Float>::linear;
      if (itsShowLevel>0) {
         cerr << "Method = linear" << endl;
      }
   } else if (method==Interpolate2D<T>::CUBIC) {
      method1D = InterpolateArray1D<Float,Float>::spline;
      if (itsShowLevel>0) {
         cerr << "Method = cubic spline" << endl;
      }
   }

// Iterate through output image

   Bool goodIsTrue = True;
   Vector<Bool> dummyMask(nLine);
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
      if (itsShowLevel>1) cerr << "inPos=" << inPos << endl;
      if (itsShowLevel>1) cerr << "inY=" << inY << endl;
//

      if (inIsMasked) {

// We must AND the coordinate conversion failure vector and the 
// actual mask.  Take short cuts where we can.

         if (allGood) {
            const Vector<Bool>& inMask = 
               (inLattice.getMaskSlice(inPos, inSubShape, True));
            if (itsShowLevel>1) {
               cerr << "inMask=" << inMask << endl;
            }
            if (outIsMasked) {
               InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                    outMaskIterPtr->rwVectorCursor(),
                                                    outX, inX, inY, 
                                                    inMask, method1D, goodIsTrue);
            } else {
               InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                    dummyMask,
                                                    outX, inX, inY, 
                                                    inMask, method1D, goodIsTrue);
            }
         } else {
            const Vector<Bool>& inMask = 
               (failed && inLattice.getMaskSlice(inPos, inSubShape, True));
            if (itsShowLevel>1) {
               cerr << "inMask=" << inMask << endl;
            }
            if (outIsMasked) {
               InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                    outMaskIterPtr->rwVectorCursor(),
                                                    outX, inX, inY, 
                                                    inMask, method1D, goodIsTrue);
            } else {
               InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                    dummyMask,
                                                    outX, inX, inY, 
                                                    inMask, method1D, goodIsTrue);
            }
         }
      } else {
         if (allGood) {
            if (itsShowLevel>1) {
               cerr << "inMask all T" << endl;
            }
            InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                 outX, inX, inY, 
                                                 method1D);
            if (outIsMasked) outMaskIterPtr->rwVectorCursor().set(True);
         } else {
            const Vector<Bool>& inMask = failed;
            if (itsShowLevel>1) {
               cerr << "inMask=" << inMask << endl;
            }
            if (outIsMasked) {
               InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                    outMaskIterPtr->rwVectorCursor(),
                                                    outX, inX, inY, 
                                                    inMask, method1D, goodIsTrue);
            } else {
               InterpolateArray1D<Float,T>::interpolate(outIter.rwVectorCursor(),
                                                    dummyMask,
                                                    outX, inX, inY, 
                                                    inMask, method1D, goodIsTrue);
            }
         }
      }
//
      if (itsShowLevel>1) {
         cerr << "outY = " << outIter.rwVectorCursor() << endl;
         if (outIsMasked) cerr << "outMask = " << outMaskIterPtr->rwVectorCursor() << endl;
      }
//
      if (outIsMasked) (*outMaskIterPtr)++;
   } 
//
   if (outIsMasked) delete outMaskIterPtr;
}


template<class T>
void ImageRegrid<T>::checkAxes(IPosition& outPixelAxes,
                               const IPosition& inShape,
                               const IPosition& outShape,
                               const Vector<Int>& pixelAxisMap,
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

// FIll in all axes if null pixelAxes given

   if (n1==0) {
      outPixelAxes = IPosition::makeAxisPath(nOut);
      n1 = outPixelAxes.nelements();
   }

// Check for Stokes and discard

   Int outCoordinate, outAxisInCoordinate;
   uInt j = 0;
   for (uInt i=0; i<n1; i++) {

// Find equivalent world axis

      Int outWorldAxis = outCoords.pixelAxisToWorldAxis(outPixelAxes(i));

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
         outPixelAxes(j) = outPixelAxes(i);
         j++;
      }
   }
   outPixelAxes.resize(j,True);

// CHeck for range

   for (Int i=0; i<n1; i++) {
      if (outPixelAxes(i)<0 || outPixelAxes(i)>=nOut) {
         os << "Pixel axes are illegal" << LogIO::EXCEPTION;
      }
   }

// CHeck non-regriddded axis shapes are ok

   for (Int i=0; i<nOut; i++) {
      Bool found = False;
      for (Int j=0;j<n1; j++) {
         if (outPixelAxes(j)==i) {
            found = True;
            break;
         }
      }
//
      if (!found && inShape(i) != outShape(pixelAxisMap(i))) {
           os << "Any axis not being regridded must have the same "
              << "input and output shapes" << LogIO::EXCEPTION;
      }  
   }
}


template<class T>
void ImageRegrid<T>::findMaps (uInt nDim, Vector<Int>& worldAxisMap,
                               Vector<Int>& worldAxisTranspose,
                               Vector<Bool>& worldRefChange,
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

   if (!outCoords.worldMap(worldAxisMap, worldAxisTranspose,
                           worldRefChange, inCoords)) {
      throw(AipsError(inCoords.errorMessage()));
   }

// pixelAxisMap1(i) says where pixel axis i in the output image
// is in the input image
// pixelAxisMap2(i) says where pixel axis i in the input image
// is in the output image

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
      cerr << "outMaskIsWritable" << out.isMaskWritable() << endl;
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
void ImageRegrid<T>::copyDataAndMask(MaskedLattice<T>& out,
                                     Lattice<Bool>& outMask,
                                     MaskedLattice<T>& in) const
//  
// The pixels and mask come from in.
// The output pixels go into the pixels of out.
// The output mask goes into outMask (which will then
// be associated with out)
//
// This function exists because when you make a SubImage,
// the mask associated with the SUbImage is no longer
// writable via putMaskSLice. So you have to make a SUbLattice<Bool> of the
// mask and access that directly.
//
{ 
// Use the same stepper for input and output.

   IPosition cursorShape = out.niceCursorShape();
   LatticeStepper stepper (out.shape(), cursorShape, LatticeStepper::RESIZE);

// Create an iterator for the output to setup the cache.
// It is not used, because using putSlice directly is faster and as easy.

   LatticeIterator<T> dummyIter(out);
   RO_LatticeIterator<T> iter(in, stepper);
   for (iter.reset(); !iter.atEnd(); iter++) {
      out.putSlice (iter.cursor(), iter.position());
      outMask.putSlice(in.getMaskSlice(iter.position(),
                       iter.cursorShape()), iter.position());
   }
}


template<class T>
Bool ImageRegrid<T>::anyBadPixels(uInt width, const Matrix<Bool>& mask) 
//
// This is much faster than using anyEQ
//
{
   for (uInt j=0; j<width; j++) {
      for (uInt i=0; i<width; i++) {
         if (!mask(i,j)) return True;
      }
   }
   return False;
}


template<class T>
Bool ImageRegrid<T>::makeDirectionMachine(LogIO& os, MDirection::Convert& machine,
                                          const DirectionCoordinate& dirCoordTo,
                                          const DirectionCoordinate& dirCoordFrom,
                                          const ObsInfo& obsTo,
                                          const ObsInfo& obsFrom) const
//
// We need MDirection type, position on earth and epoch.  But 
// maybe not all of them...
//
{
   const MDirection::Types& typeFrom = dirCoordFrom.directionType();
   const MDirection::Types& typeTo = dirCoordTo.directionType();
   Bool typesEqual = (typeTo==typeFrom);
//
   MEpoch epochFrom = obsFrom.obsDate();
   MEpoch epochTo = obsTo.obsDate();
   Double t1 = epochFrom.getValue().get();
   Double t2 = epochTo.getValue().get();
   Bool epochEqual = near(t1,t2);
//
   String telFrom = obsFrom.telescope();
   String telTo = obsTo.telescope();
   Bool posEqual = (telFrom==telTo);

// Everything is the same for input and output so we don't 
// need a machine to convert anything

   if (typesEqual && epochEqual && posEqual) return False;
//
   if (itsShowLevel>0) {
      cerr << "Make DirectionConvert machine" << endl;
   }

// Start with simplest machine, just MDirection::Types.  If it does 
// the conversion, that's all we need.  If not, we need more.

   MDirection::Ref refFrom(typeFrom);
   MDirection::Ref refTo(typeTo);
   machine = MDirection::Convert(refFrom, refTo);
//
   MDirection fromMD;
   dirCoordFrom.toWorld(fromMD, dirCoordFrom.referencePixel());
   Bool ok = True;
   try {
      MDirection toMD = machine(fromMD);
   } catch (AipsError x) {
      ok = False;
   }
   if (ok) {
      if (itsShowLevel>0) {
         cerr << "It appears I don't need observatory and epoch" << endl;
      }
      if (typeFrom==typeTo) {
         if (itsShowLevel>0) {
            cerr << "Direction Types are the same so I don't need a machine" << endl;
         }
         return False;
      } else {          
         if (itsShowLevel>0) {
            cerr << "Direction Types are different so I need a machine" << endl;
         }
         return True;
      }
   } else {
      if (itsShowLevel>0) {
         cerr << "It appears I do need either/both observatory and epoch" << endl;
      }
    }

// The conversion failed, so we need either or both of epoch 
// and position in the machine.  

   Double t = epochFrom.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The output image has no valid epoch" << LogIO::EXCEPTION;
   }
   t = epochTo.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The input image has no valid epoch" << LogIO::EXCEPTION;
   }

// Now add the epoch to the machine and see if that works

   {
      MeasFrame frameFrom;
      MeasFrame frameTo;
//
      frameFrom.set(epochFrom);
      frameTo.set(epochTo);
      MDirection::Ref refFrom(typeFrom, frameFrom);
      MDirection::Ref refTo(typeTo, frameTo);
      machine = MDirection::Convert(refFrom, refTo);
//
      ok = True;
      try {
         MDirection toMD = machine(fromMD);
      } catch (AipsError x) {
         ok = False;
      }
      if (ok) {
         if (itsShowLevel>0) {
            cerr << "Needed epoch as well only" << endl;
         }
         return True;
      }
   }

// Now add the position to the machine and see if that works

   if (telFrom==String("UNKNOWN")) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The output image has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
   if (telTo==String("UNKNOWN")) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The input image has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
//
   MPosition posFrom, posTo;
   Bool found = MeasTable::Observatory(posFrom, telFrom);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telFrom << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
   found = MeasTable::Observatory(posTo, telTo);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telTo << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
//
   {
      MeasFrame frameFrom;
      MeasFrame frameTo;
//
      frameFrom.set(posFrom);
      frameTo.set(posTo);
      MDirection::Ref refFrom(typeFrom, frameFrom);
      MDirection::Ref refTo(typeTo, frameTo);
      machine = MDirection::Convert(refFrom, refTo);
//
      ok = True;
      try {
         MDirection toMD = machine(fromMD);
      } catch (AipsError x) {
         ok = False;
      }
      if (ok) {
         if (itsShowLevel>0) {
            cerr << "Needed position as well only" << endl;
         }
         return True;
      }
   }

// Well looks like we need both

   {
      MeasFrame frameFrom(posFrom, epochFrom);
      MeasFrame frameTo(posTo, epochTo);
//
      MDirection::Ref refFrom(typeFrom, frameFrom);
      MDirection::Ref refTo(typeTo, frameTo);
//
      machine = MDirection::Convert(refFrom, refTo);
      ok = True;
      try {
         MDirection toMD = machine(fromMD);
      } catch (AipsError x) {
         ok = False;
      }
      if (ok) {
         if (itsShowLevel>0) {
            cerr << "Needed both position and epoch" << endl;
         }
      } else {
         os << "Unable to convert between the inputand output  " <<
               "DirectionCoordinates - this is surprising !" << LogIO::EXCEPTION;
      }
   }
//
   return True;
}
 


template<class T>
Bool ImageRegrid<T>::makeFrequencyMachine(LogIO& os, MFrequency::Convert& machine,
                                          Int coordinateTo, Int coordinateFrom,
                                          const CoordinateSystem& coordsTo,
                                          const CoordinateSystem& coordsFrom,
                                          const ObsInfo& obsTo,
                                          const ObsInfo& obsFrom) const
//
// We need MDirection type, position on earth and epoch.  But 
// maybe not all of them...
//
{
   const SpectralCoordinate& specCoordTo = coordsTo.spectralCoordinate(coordinateTo);
   const SpectralCoordinate& specCoordFrom = coordsFrom.spectralCoordinate(coordinateFrom);
//
   if (itsShowLevel>0) {
      cerr << "Make FrequencyConvert machine" << endl;
   }

// We need to have a DirectionCoordinate without which we can't make any
// conversions

   Int afterCoord = -1;
   Int coordinateDirTo = coordsTo.findCoordinate(Coordinate::DIRECTION, afterCoord);
   if (coordinateDirTo==-1) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "No DirectionCoordinate was found in the input CoordinateSystem" << LogIO::EXCEPTION;
   }
   afterCoord = -1;
   Int coordinateDirFrom = coordsFrom.findCoordinate(Coordinate::DIRECTION, afterCoord);
   if (coordinateDirFrom==-1) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "No DirectionCoordinate was found in the output CoordinateSystem" << LogIO::EXCEPTION;
   }
//
   const DirectionCoordinate& dirCoordTo = coordsTo.directionCoordinate(coordinateDirTo);
   const DirectionCoordinate& dirCoordFrom = coordsFrom.directionCoordinate(coordinateDirFrom);
   Bool dirCoordEqual = dirCoordTo.near(&dirCoordFrom);

// See if we need machine

   const MFrequency::Types typeTo = specCoordTo.frequencySystem();
   const MFrequency::Types typeFrom = specCoordFrom.frequencySystem();
   Bool typesEqual = (typeTo==typeFrom);
//
   MEpoch epochFrom = obsFrom.obsDate();
   MEpoch epochTo = obsTo.obsDate();
   Double t1 = epochFrom.getValue().get();
   Double t2 = epochTo.getValue().get();
   Bool epochEqual = near(t1,t2);
//
   String telFrom = obsFrom.telescope();
   String telTo = obsTo.telescope();
   Bool posEqual = (telFrom==telTo);

// Bug out if everything is the same

   if (dirCoordEqual && typesEqual && epochEqual && posEqual) return False;

// Create frames

   MeasFrame frameFrom;
   MeasFrame frameTo;

// Add Direction

   MDirection MDFrom, MDTo;
   if (!dirCoordFrom.toWorld(MDFrom, dirCoordFrom.referencePixel())) {
      os << "DirectionCoordinate coordinate conversion failed because " +
            dirCoordFrom.errorMessage() << LogIO::EXCEPTION;
   }
   if (!dirCoordFrom.toWorld(MDFrom, dirCoordFrom.referencePixel())) {
      os << "DirectionCoordinate coordinate conversion failed because " +
            dirCoordTo.errorMessage() << LogIO::EXCEPTION;
   }
   frameFrom.set(MDFrom);
   frameTo.set(MDTo);

// Add Epoch   

   Double t = epochFrom.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "The output image has no valid epoch" << LogIO::EXCEPTION;
   }
   t = epochTo.getValue().get();
   if (near(t,0.0)) {
      os << "In setting up the SpectralCoordinate conversion machinery" << endl;
      os << "The input image has no valid epoch" << LogIO::EXCEPTION;
   }
   frameFrom.set(epochFrom);
   frameTo.set(epochTo);

// Add the position 

   if (telFrom==String("UNKNOWN")) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The output image has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
   if (telTo==String("UNKNOWN")) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "The input image has no valid observatory name - cannot divine its position" << LogIO::EXCEPTION;
   }
//
   MPosition posFrom, posTo;
   Bool found = MeasTable::Observatory(posFrom, telFrom);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telFrom << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
   found = MeasTable::Observatory(posTo, telTo);
   if (!found) {
      os << "In setting up the DirectionCoordinate conversion machinery" << endl;
      os << "Cannot lookup the observatory name " << telTo << " in the AIPS++" << endl;
      os << "data base.  Please request that it be added" << LogIO::EXCEPTION;
   }
   frameFrom.set(posFrom);
   frameTo.set(posTo);

// Make the machine

   MFrequency::Ref refFrom(typeFrom, frameFrom);
   MFrequency::Ref refTo(typeTo, frameTo);
   machine = MFrequency::Convert(refFrom, refTo);

// Test conversion

   Bool ok = True;
   MFrequency MFTo, MFFrom;
   if (!specCoordFrom.toWorld(MFFrom, specCoordFrom.referencePixel()(0))) {
      os << "SpectralCoordinate coordinate conversion failed because " +
            specCoordFrom.errorMessage() << LogIO::EXCEPTION;
   }
   try {
      MFTo = machine(MFFrom);
   } catch (AipsError x) {
      ok = False;
   }
   if (!ok) {
      os << "Unable to convert between the input and output SpectralCoordinates" << endl;
      os << "SpectralCoordinates - this probably means one is in the REST frame" << endl;
      os << "which requires the radial velocity (not implemented yet)" << LogIO::EXCEPTION;
   }
//
   return True;
}
 
