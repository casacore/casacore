//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1996,1997
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

#include <aips/Utilities/Regex.h>
#include <aips/Measures/UnitMap.h>
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
    for (Int i=0; i<ndim; i++) {
	shape(i) = fitsImage.dim(i);
    }

    CoordinateSystem coords;
    Record header;

    Vector<String> ignore;
    Bool ok = FITSKeywordUtil::getKeywords(header, fitsImage.kwlist(), ignore);
    if (! ok) {
	os << LogIO::SEVERE << "Error retrieving keywords from fits header.\n"  <<
	    "Coordinate system may be in error." << LogIO::POST;
    }

    ok = CoordinateSystem::fromFITSHeader(coords, header, True);
    if (! ok) {
	os << LogIO::SEVERE << "Error creating coordinate system from FITS " <<
	    "keywords." << LogIO::POST;
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
    
    // We should have the software remove the fields that it uses.
    // OK, add all the fields that aren't boring to miscinfo.
    const uInt nfields = header.nfields();
    // Go backwards because the field numbers are decremented.
    for (i=nfields-1; i>=0; i--) {
	String name = downcase(header.name(i));
	if (name.contains(Regex("^simple"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^naxis"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^bitpix"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^origin"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^.type"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^.unit"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^.rval"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^.rpix"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^.rota"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^.delt"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^projp"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^pc"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^extend"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^blocked"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^bscale"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^bzero"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^equinox"))) {
	    header.removeField(i); continue;
	}
	if (name.contains(Regex("^epoch"))) {
	    header.removeField(i); continue;
	}
    }

    newImage->setMiscInfo(header);


    // Cool, now we just need to write it.
    IPosition cursorShape(ndim), cursorOrder(ndim);
    String report;
    cursorShape = ImageFITSConverter::copyCursorShape(report,
					      shape,
					      sizeof(Float),
					      sizeof(HDUType::ElementType),
					      memoryInMB);
    for (i=0; i<ndim; i++) {
	cursorOrder(i) = i;
    }

    os << LogIO::NORMAL << "Copy FITS file to " << newImage->name() << ". " <<
	report;

    LatticeStepper stepper(shape, cursorShape, cursorOrder);
    LatticeIterator<Float> imiter(*newImage, stepper);

    ProgressMeter meter(0.0, 1.0*newImage->shape().product(), "FITS to Image",
			"Pixels copied", "", "", 
			True, newImage->shape().product()/cursorShape.product()/100);
    try {
	Int bufferSize = cursorShape.product();
	for (imiter.reset(); !imiter.atEnd(); imiter++) {
	    fitsImage.read(bufferSize);                  // Read from FITS
	    if (fitsImage.err()) {
		error = "Error reading from FITS image";
		delete newImage;
		newImage = 0;
		return;
	    }
	    Array<Float> &cursor = imiter.cursor();
	    Bool deletePtr;
	    Float *ptr = cursor.getStorage(deletePtr);   // Get Image ptr
	    fitsImage.copy(ptr, bufferSize);             // Copy
	    cursor.putStorage(ptr, deletePtr);
	}
    } catch (AipsError x) {
	error = String("Error writing pixel values to image: " ) + x.getMesg();
	delete newImage;
	newImage = 0;
    } end_try;

    // Successful
    return;
}
