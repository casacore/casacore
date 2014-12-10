//# ImageFITSConverter.cc: this defines templated conversion from FITS to a Casacore Float image
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
//#
//# $Id$

#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/images/Regions/RegionHandler.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/LCPagedMask.h>
#include <casacore/lattices/Lattices/LCMask.h>
#include <casacore/lattices/Lattices/LCRegionSingle.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>

#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/System/ProgressMeter.h>

#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// At least the Coordinate and header related things could be factored out
// into template independent code.
template<class HDUType>
void ImageFITSConverterImpl<HDUType>::FITSToImage(
	ImageInterface<Float>*& pNewImage, String &error,
	const String &newImageName,
	const uInt whichRep, HDUType &fitsImage,
	const String& fitsFilename, const DataType dataType,
	const uInt memoryInMB, const Bool zeroBlanks
) {
	LogIO os(LogOrigin("ImageFITSConverterImpl", __FUNCTION__, WHERE));
	// Crack the header and get what we need out of it.  DOn't get tricked
	// by the fact that HDUType is referring to the template type, not
	// to the enum HDUType in class FITS !

	// ndim

	uInt ndim = fitsImage.dims();

	// shape

	IPosition shape(ndim);
	for (Int i=0; i<Int(ndim); i++) {
		shape(i) = fitsImage.dim(i);
	}

	// Get header as Vector of strings

	Vector<String> header = fitsImage.kwlist_str(True);

	// Get Coordinate System.  Return un-used FITS cards in a Record for further use.

	Record headerRec;
	Bool dropStokes = True;
	Int stokesFITSValue = 1;
	CoordinateSystem coords =  ImageFITSConverter::getCoordinateSystem(
			stokesFITSValue, headerRec,  header,
			os, whichRep, shape, dropStokes
	);
	ndim = shape.nelements();
	// Create image

	try {
		if (newImageName.empty()) {
			pNewImage = new TempImage<Float>(shape, coords);
			os << LogIO::NORMAL << "Created (temp)image of shape " << shape << LogIO::POST;
		} else {
			pNewImage = new PagedImage<Float>(shape, coords, newImageName);
			os << LogIO::NORMAL << "Created image of shape " << shape << LogIO::POST;
		}
	}
	catch (const AipsError& x) {
		if (pNewImage) {
			delete pNewImage;
		}
		pNewImage = 0;
		error = String("Error creating or writing file ") +
				newImageName + ":" + x.getMesg();
		return;
	}
	if (pNewImage == 0) {
		error = String("Unknown error writing ") + newImageName;
		return;
	}

	// Brightness Unit

	Unit bu = ImageFITSConverter::getBrightnessUnit(headerRec, os);
	pNewImage->setUnits(bu);

	// BITPIX

	Int bitpix;
	Record subRec = headerRec.asRecord("bitpix");
	subRec.get("value", bitpix);
	headerRec.removeField("bitpix");

	// BLANK Find out if we are blanked.  This is only relevant to
	// BITPIX > 0  For 32 bit floating point is is not required
	// by FITS (illegal ?) and Casacore does not write it out.
	// Other packages may write it out, so a bit of code below
	// to handle it.

	Bool isBlanked = fitsImage.isablank();
	Int blankVal = fitsImage.blank();
	if (bitpix < 0 && isBlanked) {
		if (blankVal != -1) {
			// Warn that we only deal with NaN blanked FP image HDU's.
			os << LogIO::WARN << WHERE <<
					"For floating point images, BLANK may only be set to -1<n" <<
					blankVal << " is invalid. Ignoring (but will pass through "
					"NaN's."  << LogIO::POST;
		}
	}

	// Get rid of anything else we don't want to end up in MiscInfo
	// that will have passed through the FITS parsing process

	Vector<String> ignore(12);
	ignore(0) = "^datamax$";
	ignore(1) = "^datamin$";
	ignore(2) = "^origin$";
	ignore(3) = "^extend$";
	ignore(4) = "^blocked$";
	ignore(5) = "^blank$";
	ignore(6) = "^simple$";
	ignore(7) = "bscale";
	ignore(8) = "bzero";
	ignore(9) = "xtension";
	ignore(10) = "pcount";
	ignore(11) = "gcount";
	FITSKeywordUtil::removeKeywords(headerRec, ignore);
	// Put whatever is left in the header into the MiscInfo bucket

	Record miscInfo;
	ImageFITSConverter::extractMiscInfo (miscInfo, headerRec);
	pNewImage->setMiscInfo(miscInfo);

	// Restore the logtable from HISTORY (this could be moved to non-templated code)

	LoggerHolder& logger = pNewImage->logger();
	ConstFitsKeywordList kw = fitsImage.kwlist();
	ImageFITSConverter::restoreHistory (logger, kw);

	IPosition cursorShape(ndim), cursorOrder(ndim);
	String report;
	cursorShape =
			ImageFITSConverter::copyCursorShape(report, shape, sizeof(Float),
					sizeof(typename HDUType::ElementType),
					memoryInMB);

	os << LogIO::NORMAL << "Copy FITS file to '" << pNewImage->name() << "' " <<
			report << LogIO::POST;
	LatticeStepper imStepper(shape, cursorShape, IPosition::makeAxisPath(ndim));
	LatticeIterator<Float> imIter(*pNewImage, imStepper);
	Int nIter = max(1,pNewImage->shape().product()/cursorShape.product());
	Int iUpdate = max(1,nIter/20);
	ProgressMeter meter(0.0, Double(pNewImage->shape().product()),
			"FITS to Image", "Pixels copied", "", "",  True, 
			iUpdate);
	Double nPixPerIter = cursorShape.product();
	Double meterValue;

	// With floating point, we don't know ahead of time if there
	// are blanks or not.   SO we have to make the mask, and then
	// delete it if its not needed.

	ImageRegion maskReg;
	LatticeIterator<Bool>* pMaskIter = 0;
	Bool madeMask = False;
	if (bitpix<0 || isBlanked) {
		maskReg = pNewImage->makeMask ("mask0", False, False);
		LCRegion& mask = maskReg.asMask();
		LatticeStepper pMaskStepper (shape, cursorShape,
				IPosition::makeAxisPath(ndim));
		pMaskIter = new LatticeIterator<Bool>(mask, pMaskStepper);
		pMaskIter->reset();
		madeMask = True;
	}

	// Do the work. Iterate through in chunks.
	Bool hasBlanks = False;
	try {
		Int bufferSize = cursorShape.product();
		for (imIter.reset(),meterValue=0.0; !imIter.atEnd(); imIter++) {
			Array<Float>& cursor = imIter.woCursor();
			fitsImage.read(bufferSize);                  // Read from FITS
			meterValue += nPixPerIter*1.0/2.0;
			meter.update(meterValue);

			if (fitsImage.err()) {
				error = "Error reading from FITS image";
				delete pNewImage;
				pNewImage = 0;
				return;
			}

			Bool deletePtr;
			Float *ptr = cursor.getStorage(deletePtr);   // Get Image ptr
			fitsImage.copy(ptr, bufferSize);             // Copy from fits

			// Deal with mask if necessary
			if (madeMask) {
				Array<Bool>& maskCursor = pMaskIter->woCursor();
				Bool deleteMaskPtr;
				Bool* mPtr = maskCursor.getStorage(deleteMaskPtr);
				if (zeroBlanks) {
					for (uInt i=0; i<maskCursor.nelements(); i++) {
						if (isNaN(ptr[i])) {
							mPtr[i] = False;
							hasBlanks = True;
							ptr[i] = 0.0;
						}
						else {
							mPtr[i] = True;
						}
					}
				}
				else {
					for (uInt i=0; i<maskCursor.nelements(); i++) {
						if (isNaN(ptr[i])) {
							mPtr[i] = False;
							hasBlanks = True;
						}
						else {
							mPtr[i] = True;
						}
					}
				}
				maskCursor.putStorage(mPtr, deleteMaskPtr);
				pMaskIter->operator++();
			}
			else {
				if (zeroBlanks) {
					for (uInt i=0; i<cursor.nelements(); i++) {
						if (isNaN(ptr[i])) {
							hasBlanks = True;
							ptr[i] = 0.0;
						}
					}
				}
			}
			cursor.putStorage(ptr, deletePtr);
			meterValue += nPixPerIter*1.0/2.0;
			meter.update(meterValue);
		}

		// Now attach the mask to the image if required

		if (madeMask) {
			if (hasBlanks) {
				os << LogIO::NORMAL << "Storing mask with name 'mask0'" << endl;
				pNewImage->defineRegion ("mask0", maskReg, RegionHandler::Masks);
				pNewImage->setDefaultMask(String("mask0"));
			}
			// Clean up pointers

			delete pMaskIter;
		}
	}
	catch (const AipsError& x) {
		error = String("Error writing pixel values to image: " ) + x.getMesg();
		delete pNewImage;
		pNewImage = 0;
	}
	// ImageInfo (removes any consumed keywords)
	ImageInfo imageInfo = ImageFITSConverter::getImageInfo(headerRec);

	// If we had one of those unofficial pseudo-Stokes on the Stokes axis, store it in the imageInfo

	if (stokesFITSValue != -1) {
		ImageInfo::ImageTypes type = ImageInfo::imageTypeFromFITS(stokesFITSValue);
		if (type!= ImageInfo::Undefined) {
			imageInfo.setImageType(type);
		}
	}
	// Try and find the restoring beam in the history cards if
	// its not in the header
	if (
		! imageInfo.hasBeam()
		&& ! imageInfo.getRestoringBeam(logger)
	) {
		if (
			headerRec.isDefined(ImageFITSConverter::CASAMBM)
			&& headerRec.asRecord(ImageFITSConverter::CASAMBM).asBool("value")
		) {
			ImageFITSConverter::readBeamsTable(imageInfo, fitsFilename, dataType);
		}
		else{
		        os << LogIO::NORMAL << "No usable restoring beam information found." << LogIO::POST;
		        imageInfo.removeRestoringBeam();
		}
	}
	pNewImage->setImageInfo(imageInfo);
	return;
}




} //# NAMESPACE CASACORE - END

