//# ImageFITSConverter.cc: this defines templated conversion from FITS to an aips++ Float image
//# Copyright (C) 1996,1997,1998,1999
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

#include <trial/Images/ImageFITSConverter.h>
#include <trial/Images/PagedImage.h>
#include <trial/Images/RegionHandler.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/LCPagedMask.h>
#include <aips/FITS/fitsio.h>
#include <aips/FITS/hdu.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/FITS/FITSUtil.h>

#include <aips/Quanta/UnitMap.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Mathematics/Math.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Containers/Record.h>
#include <trial/Tasking/ProgressMeter.h>


#if defined(__GNUG__)
// These might not all be necessary
typedef Vector<Double> gppbug1;
typedef Vector<String> gppbug2;
typedef PagedImage<Float> gppbug3;
#endif

// At least the Coordinate and header related things could be factored out
// into template independent code.
template<class HDUType>
void ImageFITSConverterImpl<HDUType>::FITSToImage(PagedImage<Float>*& newImage,
						  String &error,
						  const String &imageName,
						  HDUType &fitsImage,
						  uInt memoryInMB)
{
    LogIO os(LogOrigin("ImageFITSConverterImpl", "FITSToImage", WHERE));

    // Crack the header and get what we need out of it.  DOn't get tricked
    // by the fact that HDUType is referring to the template type, not
    // to the enum HDUType in class FITS !

    // ndim
    const uInt ndim = fitsImage.dims();

    // shape
    IPosition shape(ndim);
    for (Int i=0; i<Int(ndim); i++) {
	shape(i) = fitsImage.dim(i);
    }

    CoordinateSystem coords;
    Record header;

    Vector<String> ignore(0); // You will have to increase this if
                               // you ignore more

    
    Bool ok = FITSKeywordUtil::getKeywords(header, fitsImage.kwlist(), ignore);
    if (! ok) {
	os << LogIO::SEVERE << "Error retrieving keywords from fits header.\n"
	    "Coordinate system may be in error." << LogIO::POST;
    }

    ok = CoordinateSystem::fromFITSHeader(coords, header, True);
    if (! ok) {
	os << LogIO::WARN << 
	  "Error creating coordinate system from FITS keywords.\n" 
	  "I will use a linear coordinate along each axis instead.\n"
	  "If you your FITS file actually does contain a coordinate system\n"
	  "please submit a bug report."  << LogIO::POST;
	CoordinateSystem empty;
	LinearCoordinate linear(shape.nelements());
	Vector<Double> crval(shape.nelements());
	crval = 1.0;
	linear.setReferenceValue(crval);
	empty.addCoordinate(linear);
	coords = empty;
    }

    try {
	newImage = new PagedImage<Float>(shape, coords, imageName);
    } catch (AipsError x) {
	if (newImage) {
	    delete newImage;
	}
	newImage = 0;
	error = String("Error creating or writing file ") + 
	    imageName + ":" + x.getMesg();
	return;
    } end_try;

    if (newImage == 0) {
	error = String("Unknown error writing ") + imageName;
	return;
    }

    // Set the unit if possible
    if (header.isDefined("bunit") && header.dataType("bunit") == TpString) {
	String unitString;
	header.get("bunit", unitString);
	header.removeField("bunit");
	UnitMap::addFITS();
	if (UnitVal::check(unitString)) {
	    newImage->setUnits(Unit(unitString));
	} else {
	    os << "FITS unit " << unitString << " unknown to AIPS++ - ignoring."
	       << LogIO::POST;
	}
    }

    // BITPIX
    Int bitpix;
    header.get("bitpix", bitpix);

    // BLANK Find out if we are blanked.  This is only relevant to
    // BITPIX > 0  For 32 bit floating point is is not required 
    // by FITS (illegal ?) and aips++ does not write it out.
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
//
    ignore.resize(21); // resize as necessary
    ignore(0) = "^datamax$";  // Image pixels might change
    ignore(1) = "^datamin$";
    ignore(2) = "^date-map$";
    ignore(3) = "^simple$";
    ignore(4) = "^naxis";
    ignore(5) = "^bitpix$";
    ignore(6) = "^origin$";
    ignore(7) = "^projp$";
    ignore(8) = "^pc$";
    ignore(9) = "^extend$";
    ignore(10) = "^blocked$";
    ignore(11) = "^blank$";
    ignore(12) = "^equinox$";
    ignore(13) = "^epoch$";
    ignore(14) = "^.type";
    ignore(15) = "^.unit";
    ignore(16) = "^.rpix";
    ignore(17) = "^.rval";
    ignore(18) = "^.rota";
    ignore(19) = "^.delt";
    ignore(20) = "^bunit$";
    FITSKeywordUtil::removeKeywords(header, ignore);
    // Remove any that might have been in ObsInfo of coordinates.
    FITSKeywordUtil::removeKeywords(header, ObsInfo::keywordNamesFITS());

    newImage->setMiscInfo(header);

    // Restore the logtable from HISTORY (this could be moved to non-templated
    // code.
    if (newImage->logSink().localSink().isTableLogSink()) {
	TableLogSink &logTable = 
	    newImage->logSink().localSink().castToTableLogSink();
	Vector<String> lines;
	String groupType;
	ConstFitsKeywordList kw = fitsImage.kwlist();
	kw.first();
	uInt n;
	while ((n = FITSHistoryUtil::getHistoryGroup(lines, groupType, kw)) !=
	       0) {
	    if (groupType == "LOGTABLE") {
		FITSHistoryUtil::fromHISTORY(logTable, lines, n, True);
	    } else if (groupType == "") {
		FITSHistoryUtil::fromHISTORY(logTable, lines, n, False);
	    }
	}
    }

    // Cool, now we just need to write it.
    IPosition cursorShape(ndim), cursorOrder(ndim);
    String report;
    cursorShape = 
      ImageFITSConverter::copyCursorShape(report, shape, sizeof(Float),
                                          sizeof(HDUType::ElementType),
                                          memoryInMB);

    os << LogIO::NORMAL << "Copy FITS file to '" << newImage->name() << "' " <<
	report << LogIO::POST;
    LatticeStepper imStepper(shape, cursorShape, IPosition::makeAxisPath(ndim));
    LatticeIterator<Float> imIter(*newImage, imStepper);

    Int nIter = max(1,newImage->shape().product()/cursorShape.product());
    Int iUpdate = max(1,nIter/20);
    ProgressMeter meter(0.0, Double(newImage->shape().product()),
			"FITS to Image", "Pixels copied", "", "",  True, 
			iUpdate);
    Double nPixPerIter = cursorShape.product();
    Double meterValue;

// With floating point, we don't know ahead of time if there
// are blanks or not.   SO we have to make the mask, and then
// delete it if its not needed.

    LCPagedMask* pMask = 0;
    LatticeStepper* pMaskStepper = 0;
    LatticeIterator<Bool>* pMaskIter = 0;

    Bool madeMask = False;
    if (bitpix<0 || isBlanked) {
       pMask = new LCPagedMask(RegionHandler::makeMask (*newImage, "mask0"));
       pMaskStepper = new LatticeStepper(shape, cursorShape, 
                                         IPosition::makeAxisPath(ndim));
       pMaskIter = new LatticeIterator<Bool>(*pMask, *pMaskStepper);
       pMaskIter->reset();
       madeMask = True;
    }
// 
// Do the work. Iterate through in chunks.
//
    Bool hasBlanks = False;
    try {
	Int bufferSize = cursorShape.product();

	for (imIter.reset(),meterValue=0.0; !imIter.atEnd(); imIter++) {
	    Array<Float> &cursor = imIter.woCursor();
	    fitsImage.read(bufferSize);                  // Read from FITS
            meterValue += nPixPerIter*1.0/2.0;
            meter.update(meterValue);
	    if (fitsImage.err()) {
		error = "Error reading from FITS image";
		delete newImage;
		newImage = 0;
		return;
	    }
	    Bool deletePtr;
	    Float *ptr = cursor.getStorage(deletePtr);   // Get Image ptr
	    fitsImage.copy(ptr, bufferSize);             // Copy from fits
// 
// Deal with mask if necessary
//
            if (madeMask) {
               Array<Bool>& maskCursor = pMaskIter->woCursor();
               Bool deleteMaskPtr;
               Bool* mPtr = maskCursor.getStorage(deleteMaskPtr);   
               for (uInt i=0; i<maskCursor.nelements(); i++) {
                  if (isNaN(ptr[i])) {
//                  if (isnanf(ptr[i])) {
//                  if (ptr[i] != ptr[i]) {
                     mPtr[i] = False; 
                     hasBlanks = True;
                  } else {
                     mPtr[i] = True;  
                  }
               }
               maskCursor.putStorage(mPtr, deleteMaskPtr);
               pMaskIter->operator++();
            }
//
	    cursor.putStorage(ptr, deletePtr);
//
            meterValue += nPixPerIter*1.0/2.0;
            meter.update(meterValue);
         }
//
// Now either delete the mask or attach it properly to the image
//
         if (madeMask) {
            if (hasBlanks) {
               os << LogIO::NORMAL << "Storing mask with name 'mask0'" << endl;
//
// Make mask known to the image 
//
               newImage->defineRegion ("mask0", ImageRegion(*pMask), 
                                        RegionHandler::Masks);
//
// Make the mask the default mask. Hereafter the mask is writable
// through newImage.
//
               newImage->setDefaultMask(String("mask0"));
            } else {
//
// There were no blanks, so we don't need the mask
// 
               pMask->handleDelete();
            }
//
// Clean up pointers
//
            delete pMask;
            delete pMaskStepper;
            delete pMaskIter;
         }
    } catch (AipsError x) {
	error = String("Error writing pixel values to image: " ) + x.getMesg();
	delete newImage;
	newImage = 0;
    } end_try;

    // Successful
    return;
}
