//# ImageUtilities2.cc:  Helper class for accessing images
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
//# $Id$
//

#include <trial/Images/ImageUtilities.h>

#include <trial/Coordinates/CoordinateUtil.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/Coordinates/StokesCoordinate.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/RegionHandler.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/SubImage.h>
#include <trial/Images/FITSImage.h>
#include <trial/Images/MIRIADImage.h>
#include <trial/Images/TempImage.h>
#include <trial/Lattices/LCRegion.h>
#include <aips/Lattices/LatticeIterator.h>
#include <trial/Lattices/SubLattice.h>
#include <aips/Logging/LogIO.h>
#include <aips/OS/File.h>
#include <trial/Tasking/NewFile.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/PtrHolder.h>



void ImageUtilities::openImage (ImageInterface<Float>*& pImage,
                                const String& fileName, LogIO& os)
{
   if (fileName.empty()) {
      os << "The image filename is empty" << LogIO::EXCEPTION;   
   }
   File file(fileName);
   if (!file.exists()) {
      os << "File '" << fileName << "' does not exist" << LogIO::EXCEPTION;
   }
//
   ImageUtilities::ImageTypes type = ImageUtilities::imageType(fileName);
   if (type==ImageUtilities::AIPSPP) {
      if (!Table::isReadable(fileName)) {
         os << "The aips++ image file " << fileName << " is not readable" << LogIO::EXCEPTION;
      }
      pImage = new PagedImage<Float>(fileName);
   } else if (type==ImageUtilities::FITS) { 
      pImage = new FITSImage(fileName);
   } else if (type==ImageUtilities::MIRIAD) {
      pImage = new MIRIADImage(fileName);
   } else {
      pImage = 0;
      os << "Unrecognized image type, presently aips++, FITS and Miriad images are supported" 
         << LogIO::EXCEPTION;
   }
}


void ImageUtilities::openImage (PtrHolder<ImageInterface<Float> >& image,
                                const String& fileName, LogIO& os)
{
   ImageInterface<Float>* p = 0;
   ImageUtilities::openImage(p, fileName, os);
   image.set(p);
}


void ImageUtilities::addDegenerateAxes (LogIO& os, 
                                        PtrHolder<ImageInterface<Float> >& outImage,
                                        ImageInterface<Float>& inImage,
                                        const String& outFile, Bool direction,
                                        Bool spectral, const String& stokes,
                                        Bool linear, Bool overwrite)
{
// Verify output file
      
   if (!overwrite && !outFile.empty()) {
      NewFile validfile;
      String errmsg;
      if (!validfile.valueOK(outFile, errmsg)) {
          os << errmsg << LogIO::EXCEPTION;
      }
   }
//
   IPosition shape = inImage.shape();
   CoordinateSystem cSys = inImage.coordinates();
   IPosition keepAxes = IPosition::makeAxisPath(shape.nelements());
   Int afterCoord;
//
   Bool didSomething = False;
   if (direction) {
      afterCoord = -1;
      Int iC = cSys.findCoordinate(Coordinate::DIRECTION, afterCoord);
      if (iC<0) {
         CoordinateUtil::addDirAxes(cSys);
         uInt n = shape.nelements();
         shape.resize(n+2,True); 
         shape(n) = 1;
         shape(n+1) = 1;
         didSomething = True;
      } else {
         os << "Image already contains a DirectionCoordinate" << LogIO::EXCEPTION;
      }
   }
//
   if (spectral) {
      afterCoord = -1;
      Int iC = cSys.findCoordinate(Coordinate::SPECTRAL, afterCoord);
      if (iC<0) {
         CoordinateUtil::addFreqAxis(cSys);
         uInt n = shape.nelements();
         shape.resize(n+1,True);
         shape(n) = 1;
         didSomething = True;
      } else {
         os << "Image already contains a SpectralCoordinate" << LogIO::EXCEPTION;
      }
   }
//
   if (!stokes.empty()) {
      afterCoord = -1;
      Int iC = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
      if (iC<0) {
         Vector<Int> which(1);
         String tmp = upcase(stokes);
         which(0) = Stokes::type(tmp);
         StokesCoordinate sc(which);
         cSys.addCoordinate(sc);
//
         uInt n = shape.nelements();
         shape.resize(n+1,True);
         shape(n) = 1;
         didSomething = True;
      } else {
         os << "Image already contains a StokesCoordinate" << LogIO::EXCEPTION;
      }
   }
   if (linear) {
      afterCoord = -1;
      Int iC = cSys.findCoordinate(Coordinate::LINEAR, afterCoord);
      if (iC<0) {
         Vector<String> names(1);
         Vector<String> units(1);
         Vector<Double> refVal(1);
         Vector<Double> refPix(1);
         Vector<Double> incr(1);
         names(0) = "Axis1";
         units(0) = "km";
         refVal(0) = 0.0;
         refPix(0) = 0.0;
         incr(0) = 1.0;
         Matrix<Double> pc(1,1);
         pc.set(0.0);
         pc.diagonal() = 1.0;
         LinearCoordinate lc(names, units, refVal, incr, pc, refPix);
         cSys.addCoordinate(lc);
//  
         uInt n = shape.nelements();
         shape.resize(n+1,True);
         shape(n) = 1;
         didSomething = True;
      } else {
         os << "Image already contains a LinearCoordinate" << LogIO::EXCEPTION;
      }
   }
   if (!didSomething) {
      os << "No degenerate axes specified" << LogIO::EXCEPTION;
   }
//       
   if (outFile.empty()) {
      os << LogIO::NORMAL << "Creating (temp)image of shape "
         << shape << LogIO::POST;
      outImage.set(new TempImage<Float>(shape, cSys));
   } else {
      os << LogIO::NORMAL << "Creating image '" << outFile << "' of shape "
         << shape << LogIO::POST;
      outImage.set(new PagedImage<Float>(shape, cSys, outFile));
   }   
   ImageInterface<Float>* pOutImage = outImage.ptr();
         
// Generate output masks
         
   Vector<String> maskNames = inImage.regionNames(RegionHandler::Masks);
   const uInt nMasks = maskNames.nelements();
   if (nMasks > 0) { 
      for (uInt i=0; i<nMasks; i++) {
         pOutImage->makeMask(maskNames(i), True, False, True);
      }
   }
   pOutImage->setDefaultMask(inImage.getDefaultMask());

// Generate SubImage to copy the data into
   
   AxesSpecifier axesSpecifier(keepAxes);
   SubImage<Float> subImage(*pOutImage, True, axesSpecifier);

// Copy masks (directly, can't do via SubImage)
      
   if (nMasks > 0) {
      for (uInt i=0; i<nMasks; i++) {
         ImageUtilities::copyMask(*pOutImage, inImage, maskNames(i), maskNames(i),
                                  axesSpecifier);  
      }  
   }
   
// Copy data
    
   subImage.copyData(inImage);
 
// Copy miscellaneous
   
   ImageUtilities::copyMiscellaneous(*pOutImage, inImage);
}



void ImageUtilities::copyMask (ImageInterface<Float>& out,
                               const ImageInterface<Float>& in,
                               const String& maskOut, const String& maskIn,
                               const AxesSpecifier outSpec)
//
// Because you can't write to the mask of a SubImage, we pass
// in an AxesSpecifier to be applied to the output mask.
// In this way the dimensionality of in and out can be made
// the same.
//
{     
// Get masks
   
   ImageRegion iRIn = in.getRegion(maskIn, RegionHandler::Masks);
   const LCRegion& regionIn = iRIn.asMask();

   ImageRegion iROut = out.getRegion(maskOut, RegionHandler::Masks);
   LCRegion& regionOut = iROut.asMask();
   SubLattice<Bool> subRegionOut(regionOut, True, outSpec);
         
// Copy
               
   LatticeIterator<Bool> maskIter(subRegionOut);
   for (maskIter.reset(); !maskIter.atEnd(); maskIter++) {
      subRegionOut.putSlice(regionIn.getSlice(maskIter.position(),
                            maskIter.cursorShape()),  maskIter.position());
   }   
}  

