//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1996,1997,1998
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
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <aips/FITS/fitsio.h>
#include <aips/FITS/hdu.h>
#include <trial/Coordinates/LinearCoordinate.h>
#include <trial/FITS/FITSUtil.h>

#include <aips/Quanta/UnitMap.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Exceptions/Error.h>

#include <aips/Logging/LogIO.h>

#include <aips/Containers/Record.h>

#include <trial/Tasking/ProgressMeter.h>

#include <strstream.h>

#if defined(__GNUG__)
// These might not all be necessary
typedef Vector<Double> gppbug1;
typedef Vector<String> gppbug2;
typedef PagedImage<Float> gppbug3;
#endif

// At least the Coordinate and header related things could be factored out
// into template independent code.
template<class HDUType>
void ImageFITSConverterImpl<HDUType>::FITSToImage(PagedImage<Float> *&newImage,
						  String &error,
						  const String &imageName,
						  HDUType &fitsImage,
						  uInt memoryInMB)
{
    LogIO os(LogOrigin("ImageFITSConverterImpl", "FITSToImage", WHERE));

    // Crack the header and get what we need out of it

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

    // BLANK Find out if we are blanked.
    Bool isBlanked = fitsImage.isablank();
    Int blankVal = fitsImage.blank();
    if (bitpix < 0 && isBlanked) {
	isBlanked = False; // For FP we just pass NaN's through.
	if (blankVal != -1) {
	    // Warn that we only deal with NaN blanked FP image HDU's.
	    os << LogIO::WARN << WHERE <<
		"For floating point images, BLANK may only be set to -1<n" <<
		blankVal << " is invalid. Ignoring (but will pass through "
		"NaN's."  << LogIO::POST;
	}
    }
    
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
    cursorShape = ImageFITSConverter::copyCursorShape(report,
					      shape,
					      sizeof(Float),
					      sizeof(HDUType::ElementType),
					      memoryInMB);

    os << LogIO::NORMAL << "Copy FITS file to " << newImage->name() << ". " <<
	report << LogIO::POST;
    LatticeStepper stepper(shape, cursorShape, IPosition::makeAxisPath(ndim));
    LatticeIterator<Float> imiter(*newImage, stepper);

    Int nIter = max(1,newImage->shape().product()/cursorShape.product());
    Int iUpdate = max(1,nIter/20);
    ProgressMeter meter(0.0, Double(newImage->shape().product()),
			"FITS to Image", "Pixels copied", "", "",  True, 
			iUpdate);
    Double nPixPerIter = cursorShape.product();
    Double meterValue;

    try {
	Int bufferSize = cursorShape.product();
	for (imiter.reset(),meterValue=0.0; !imiter.atEnd(); imiter++) {
	    Array<Float> &cursor = imiter.woCursor();
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
	    fitsImage.copy(ptr, bufferSize);             // Copy
	    cursor.putStorage(ptr, deletePtr);
            meterValue += nPixPerIter*1.0/2.0;
            meter.update(meterValue);
	}
    } catch (AipsError x) {
	error = String("Error writing pixel values to image: " ) + x.getMesg();
	delete newImage;
	newImage = 0;
    } end_try;

    // Successful
    return;
}
