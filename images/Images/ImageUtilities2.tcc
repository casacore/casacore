//# ImageUtilities2.tcc:  Implement templates functions
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

#ifndef IMAGES_IMAGEUTILITIES2_TCC
#define IMAGES_IMAGEUTILITIES2_TCC
//

#include <casacore/images/Images/ImageUtilities.h>

#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/TabularCoordinate.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageOpener.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/images/Images/RebinImage.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/tables/LogTables/NewFile.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <typename T> void ImageUtilities::addDegenerateAxes(
	LogIO& os, PtrHolder<ImageInterface<T> >& outImage,
	const ImageInterface<T>& inImage, const String& outFile,
	Bool direction, Bool spectral, const String& stokes,
	Bool linear, Bool tabular, Bool overwrite,
	Bool silent
) {
	// Verify output file
	if (!overwrite && !outFile.empty()) {
		NewFile validfile;
		String errmsg;
		if (!validfile.valueOK(outFile, errmsg)) {
			ThrowCc(errmsg);
		}
	}
	IPosition shape = inImage.shape();
	CoordinateSystem cSys = inImage.coordinates();
	IPosition keepAxes = IPosition::makeAxisPath(shape.nelements());

	uInt nExtra = CoordinateUtil::addAxes (
		cSys, direction, spectral, stokes,
		linear, tabular, silent
	);

	if (nExtra > 0) {
		uInt n = shape.nelements();
		shape.resize(n+nExtra,True);
		for (uInt i=0; i<nExtra; i++) {
			shape(n+i) = 1;
		}
	}

	if (outFile.empty()) {
		os << LogIO::NORMAL << "Creating (temp)image of shape "
			<< shape << LogIO::POST;
		outImage.set(new TempImage<T>(shape, cSys));
	}
	else {
		os << LogIO::NORMAL << "Creating image '" << outFile << "' of shape "
			<< shape << LogIO::POST;
		outImage.set(new PagedImage<T>(shape, cSys, outFile));
	}
	ImageInterface<T>* pOutImage = outImage.ptr();

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
	SubImage<T> subImage(*pOutImage, True, axesSpecifier);

	// Copy masks (directly, can't do via SubImage)
	if (nMasks > 0) {
		for (uInt i=0; i<nMasks; i++) {
			ImageUtilities::copyMask(*pOutImage, inImage, maskNames(i), maskNames(i),
					axesSpecifier);
		}
	}
	subImage.copyData(inImage);
	ImageUtilities::copyMiscellaneous(*pOutImage, inImage);
}


template <typename T, typename U> 
void ImageUtilities::copyMiscellaneous (ImageInterface<T>& out,
                                        const ImageInterface<U>& in,
                                        Bool copyImageInfo)
{
    out.setMiscInfo(in.miscInfo());
    if (copyImageInfo) {
        out.setImageInfo(in.imageInfo());
    }
    out.setUnits(in.units());
    out.appendLog(in.logger());
    copyAttributes (out.attrHandler(True), in.roAttrHandler());
}


template <typename T> 
void ImageUtilities::bin (MaskedArray<T>& out, Coordinate& coordOut,
                          const MaskedArray<T>& in, const Coordinate& coordIn,
                          uInt axis, uInt bin)
{

// Check

   AlwaysAssert(coordIn.nPixelAxes()==1 && coordIn.nWorldAxes()==1, AipsError);
   AlwaysAssert(coordOut.nPixelAxes()==1 && coordOut.nWorldAxes()==1, AipsError);
//
   AlwaysAssert(coordIn.type()==coordOut.type(),AipsError);
   Coordinate::Type type = coordIn.type();
   AlwaysAssert(type==Coordinate::LINEAR || type==Coordinate::SPECTRAL ||
                type==Coordinate::TABULAR, AipsError);
//  
   const IPosition shapeIn = in.shape();
   const uInt nDim = shapeIn.nelements();
   AlwaysAssert(axis<nDim, AipsError);

// Create CS

   CoordinateSystem cSysIn;
   LinearCoordinate linCoord;
   for (uInt i=0; i<nDim; i++) {

      if (i==axis) {
         cSysIn.addCoordinate(coordIn);
      } else {
         cSysIn.addCoordinate(linCoord);
      }
   }

// Make Image

   TiledShape tShapeIn(shapeIn);
   TempImage<T> im(tShapeIn, cSysIn);

// Set data

   im.put(in.getArray());
   TempLattice<Bool> pixelMask(shapeIn);
   pixelMask.put(in.getMask());
   im.attachMask(pixelMask);

// Create binner

   IPosition factors(nDim,1);
   factors(axis) = bin;
   RebinImage<T> binIm(im, factors);

// Assign output MA

   MaskedArray<T> tmp(binIm.get(), binIm.getMask());
   out = tmp;

// Handle coordinate.  

   const CoordinateSystem cSysOut = binIm.coordinates();
   if (type==Coordinate::LINEAR) {
      const LinearCoordinate& cIn = cSysOut.linearCoordinate(axis);
      LinearCoordinate& cOut = dynamic_cast<LinearCoordinate&>(coordOut);
      cOut = cIn;
   } else if (type==Coordinate::SPECTRAL) { 
      const SpectralCoordinate& cIn = cSysOut.spectralCoordinate(axis);
      SpectralCoordinate& cOut = dynamic_cast<SpectralCoordinate&>(coordOut);
      cOut = cIn;
   } else if (type==Coordinate::TABULAR) {
      const TabularCoordinate& cIn = cSysOut.tabularCoordinate(axis);
      TabularCoordinate& cOut = dynamic_cast<TabularCoordinate&>(coordOut);
      cOut = cIn;
   }
}

template <typename T, typename U> void ImageUtilities::copyMask (
	ImageInterface<T>& out,
	const ImageInterface<U>& in,
	const String& maskOut, const String& maskIn,
	const AxesSpecifier outSpec
) {
//
// Because you can't write to the mask of a SubImage, we pass
// in an AxesSpecifier to be applied to the output mask.
// In this way the dimensionality of in and out can be made
// the same.
//
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

template <typename T> void ImageUtilities::openImage(
	ImageInterface<T>*& pImage,
	const String& fileName
) {
    ThrowIf(
		fileName.empty(),
		"The image filename is empty"
	);
	File file(fileName);
	ThrowIf(
		! file.exists(),
		"File '" + fileName + "' does not exist"
	);
	LatticeBase* lattPtr = ImageOpener::openImage (fileName);
    ThrowIf(
		lattPtr == 0,
		"Image " + fileName + " cannot be opened; its type is unknown"
	);
    T x = 0;
    ThrowIf(
        lattPtr->dataType() != whatType(&x),
        "Logic Error: " + fileName
        + " has a different data type than the data type of the requested object"
    );
	pImage = dynamic_cast<ImageInterface<T> *>(lattPtr);
	ThrowIf(
		pImage == 0,
		"Unrecognized image data type, "
	    "presently only Float and Complex images are supported"
	);
}

template <typename T> void ImageUtilities::openImage(
	PtrHolder<ImageInterface<T> >& image,
	const String& fileName
) {
   ImageInterface<T>* p = 0;
   ImageUtilities::openImage(p, fileName);
   image.set(p);
}

template <typename T>
SHARED_PTR<ImageInterface<T> > ImageUtilities::openImage
(const String& fileName)
{
   ImageInterface<T>* p = 0;
   ImageUtilities::openImage(p, fileName);
   return SHARED_PTR<ImageInterface<T> > (p);
}

} //# NAMESPACE CASACORE - END


#endif
