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
//# $Id: ImageFITS2Converter.cc,v 8.1 1997/03/14 00:46:54 bglenden Exp nkilleen

#include <aips/version.h>

#include <trial/Images/ImageFITSConverter.h>
#include <trial/Images/PagedImage.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <aips/FITS/fitsio.h>
#include <aips/FITS/hdu.h>
#include <trial/FITS/FITSUtil.h>
#include <trial/Coordinates/LinearCoordinate.h>

#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Measures/UnitMap.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Record.h>

#include <aips/OS/File.h>
#include <aips/OS/Directory.h>

#include <aips/Utilities/Assert.h>
#include <aips/Logging/LogIO.h>
#include <trial/Tasking/ProgressMeter.h>

#include <strstream.h>
#include <iomanip.h>

// Cure some problems seen on dec alpha - something is defining macros
// major and minor
#if defined(major)
#undef major
#endif
#if defined(minor)
#undef minor
#endif


void ImageFITSConverter::FITSToImage(PagedImage<Float> *&newImage,
				     String &error,
				     const String &imageName,
				     const String &fitsName, 
				     uInt whichHDU,
				     uInt memoryInMB)
{
    newImage = 0;
    error = "";

    // First make sure that imageName is writable and does not already
    // exist.

    File imfile(imageName);
    if (imfile.exists()) {
	error = imageName + " already exists, will not overwrite.";
	return;
    }
    Directory imdir = imfile.path().dirName();
    if (!imdir.exists() || !imdir.isWritable()) {
	error = String("Direcotry ") + imdir.path().originalName() + 
	  " does not exist or is not writable";
	return;
    }
    

    File fitsfile(fitsName);
    if (!fitsfile.exists() || !fitsfile.isReadable() || !fitsfile.isRegular()) {
        error = fitsName + " does not exist or is not readable";
	return;
    }

    // OK, now see if we can attach the FITS reading classes
    FitsInput infile(fitsfile.path().expandedName(), FITS::Disk);
    if (infile.err()) {
        error = String("Cannot open file (or other I/O error): ") + fitsName;
	return;
    }

    // Advance to the right HDU
    for (uInt i=0; i<whichHDU; i++) {
	infile.skip_hdu();
	if (infile.err()) {
	    error = "Error advancing to image in file: " + fitsName;
	    return;
	}
    }

    // Make sure the current spot in the FITS file is an image
    if (infile.rectype() != FITS::HDURecord ||
	infile.hdutype() != FITS::PrimaryArrayHDU ||
	infile.hdutype() != FITS::ImageExtensionHDU) {
	error = "No image at specified location in file " + fitsName;
    }
    
    // The rest has to be done in a type dependent way - hand over to template
    // functions.
    
    switch(infile.datatype()) {
    case FITS::BYTE:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<unsigned char> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<unsigned char> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    } else {
		ImageExtension<unsigned char> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<unsigned char> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    }
        }
    break;
    case FITS::SHORT:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<short> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<short> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    } else {
		ImageExtension<short> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<short> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    }
        }
        break;
    case FITS::LONG:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<FitsLong> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<FitsLong> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    } else {
		ImageExtension<FitsLong> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<FitsLong> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    }
        }
    case FITS::FLOAT:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<Float> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<Float> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    } else {
		ImageExtension<Float> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<Float> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    }
        }
        break;
    case FITS::DOUBLE:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<Double> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<Double> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    } else {
		ImageExtension<Double> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<Double> >::FITSToImage(
		       newImage, error, imageName, fitsdata, memoryInMB);
	    }
        }
        break;
    default:
        error = "Unknown datatype  - no data returned";
	return;
    }

}

Bool ImageFITSConverter::ImageToFITS(String &error,
				     const PagedImage<Float> &image,
				     const String &fitsName, 
				     uInt memoryInMB,
				     Bool preferVelocity,
				     Bool opticalVelocity)
{
    LogIO log(LogOrigin("ImageFITSConverter", "ImageToFITS", WHERE));
    error = "";


    // Make sure that the fits file does not already exist, and that we
    // can write to the directory
    File fitsfile(fitsName);
    if (fitsfile.exists()) {
	error = fitsName + " already exists, will not overwrite.";
	return False;
    }
    Directory fitsdir = fitsfile.path().dirName();
    if (!fitsdir.exists() || !fitsdir.isWritable()) {
	error = String("Directory ") + fitsdir.path().originalName() + 
	    " does not exist or is not writable";
	return False;
    }
    
    // OK, it appears to be a writable etc. file, let's try opening it.
    FitsOutput outfile(fitsfile.path().expandedName(), FITS::Disk);
    if (outfile.err()) {
	error = String("Cannot open file for writing: ") + fitsName;
	return False;
    }

    CoordinateSystem coordsys = image.coordinates();
    if (coordsys.nWorldAxes() != coordsys.nPixelAxes()) {
	error = "FITS requires that the number of world and pixel axes be"
	    " identical.";
	return False;
    }

    Record header;

    const IPosition shape = image.shape();
    const uInt ndim = shape.nelements();
    header.define("BITPIX", -32);
    header.setComment("BITPIX", "Floating point (32 bit)");

    Vector<Int> naxis(ndim);
    for (Int i=0; i < ndim; i++) {
        naxis(i) = shape(i);
    }
    header.define("NAXIS", naxis);

    header.define("BSCALE", 1.0);
    header.define("BZERO", 0.0);

    header.define("COMMENT1", ""); // inserts spaces

    // I should FITS-ize the units
    header.define("BUNIT", upcase(image.units().getName()).chars());
    header.setComment("BUNIT", "Brightness (pixel) unit");

    Bool ok = coordsys.toFITSHeader(header, True, 'c', False, preferVelocity,
				    opticalVelocity);
    if (!ok) {
	log << LogIO::SEVERE << "Could not make a standard FITS header. Setting"
	    " a simple linear coordinate system." << LogIO::POST;
	uInt n = coordsys.nWorldAxes();
	Matrix<Double> pc(n,n); pc=0.0; pc.diagonal() = 1.0;

	LinearCoordinate linear(coordsys.worldAxisNames(), 
				coordsys.worldAxisUnits(),
				coordsys.referenceValue(),
				coordsys.increment(),
				coordsys.linearTransform(),
				coordsys.referencePixel());

	CoordinateSystem empty;
	coordsys = empty;
	coordsys.addCoordinate(linear);

	Record empty2;
	header = empty2;

	Bool ok = coordsys.toFITSHeader(header, True);
	if (!ok) {
	    error = "Fallback linear coordinate system fails also.";
	    return False;
	}

    }
    
    // ORIGIN
    ostrstream buffer;
    buffer << "AIPS++ version ";
    VersionInfo::report(buffer);
    header.define("ORIGIN", String(buffer));

    // Add in the fields from miscInfo that we can
    const uInt nmisc = image.miscInfo().nfields();
    for (i=0; i<nmisc; i++) {
	String miscname = image.miscInfo().name(i);
	if (miscname != "end" && miscname != "END" && 
	    !header.isDefined(miscname)) {
	    DataType misctype = image.miscInfo().dataType(i);
	    switch(misctype) {
	    case TpBool:
		header.define(miscname, image.miscInfo().asBool(i));
		break;
	    case TpChar:
	    case TpUChar:
	    case TpShort:
	    case TpUShort:
	    case TpInt:
	    case TpUInt:
		header.define(miscname, image.miscInfo().asInt(i));
		break;
	    case TpFloat:
		header.define(miscname, image.miscInfo().asfloat(i));
		break;
	    case TpDouble:
		header.define(miscname, image.miscInfo().asdouble(i));
		break;
	    case TpComplex:
		header.define(miscname, image.miscInfo().asComplex(i));
		break;
	    case TpDComplex:
		header.define(miscname, image.miscInfo().asDComplex(i));
		break;
	    case TpString:
		header.define(miscname, image.miscInfo().asString(i));
		break;
	    // These should be the cases that we actually see. I don't think
	    // asArray* converts types.
	    case TpArrayBool:
		header.define(miscname, image.miscInfo().asArrayBool(i));
		break;
	    case TpArrayInt:
		header.define(miscname, image.miscInfo().asArrayInt(i));
		break;
	    case TpArrayFloat:
		header.define(miscname, image.miscInfo().asArrayfloat(i));
		break;
	    case TpArrayDouble:
		header.define(miscname, image.miscInfo().asArraydouble(i));
		break;
	    case TpArrayString:
		header.define(miscname, image.miscInfo().asArrayString(i));
		break;
	    default:
		{
		    ostrstream os;
		    os << misctype;
		    log << LogIO::NORMAL << "Not writing miscInfo field '" <<
			miscname << "' - cannot handle type " << String(os) <<
			LogIO::POST;
		}
	    }
	}
    }


    // Set up the FITS header
    FitsKeywordList kw = FITSKeywordUtil::makeKeywordList();

    ok = FITSKeywordUtil::addKeywords(kw, header);
    if (! ok) {
	error = "Error creating initial FITS header";
	return False;
    }

    // END
    kw.end();

    String report;
    IPosition cursorShape = copyCursorShape(report,
					    shape,
					    sizeof(Float),
					    sizeof(Float),
					    memoryInMB);

    log << "Copying " << image.name() << " to " << fitsName << report << 
      LogIO::POST;

    // If this fails, more development is needed
    AlwaysAssert(sizeof(Float) == sizeof(float), AipsError);
    PrimaryArray<Float> fits(kw);
    if (fits.err()) {
	error = "Error creating FITS file from keywords";
	return False;
    }

    IPosition cursorOrder(ndim);
    for (i=0; i<ndim; i++) {
	cursorOrder(i) = i;
    }

    try {
	if (fits.write_hdr(outfile)) {
	    error = "Error writing FITS header";
	    return False;
	}

	ProgressMeter meter(0.0, 1.0*image.shape().product(),
			    "Image to FITS", "Pixels copied", "", "");
	uInt count = 0;
	Double curpixels = 1.0*cursorShape.product();

	LatticeStepper stepper(shape, cursorShape, cursorOrder);
	RO_LatticeIterator<Float> iter(image, stepper);
	const Int bufferSize = cursorShape.product();

	for (iter.reset(); !iter.atEnd(); iter++) {
	    const Array<Float> &cursor = iter.cursor();
	    Bool deleteIt;
	    const Float *ptr = cursor.getStorage(deleteIt);
	    fits.store(ptr, bufferSize);
	    if (! fits.err()) {
		Int n = fits.write(outfile);
		if (n != bufferSize) {
		    error = "Write failed (full disk?)";
		    return False;
		}
	    } else {
		error = "Unknown I/O error";
		return False;
	    }
	    cursor.freeStorage(ptr, deleteIt);
	    if (fits.err() || outfile.err()) {
		error = String("Error writing into ") + fitsName;
		return False;
	    }
	    count++;
	    meter.update(count*curpixels);
	}
    } catch (AipsError x) {
	error = "Unknown error copying image to FITS file";
	return False;
    } end_try;

    return True;
}

IPosition ImageFITSConverter::copyCursorShape(String &report,
					      const IPosition &shape, 
					      uInt imagePixelSize,
					      uInt fitsPixelSize,
					      uInt memoryInMB)
{

    // We could make this more sophisticated by querying the actual tile
    // shape. However, the image will basically always need all but the
    // last dimension in memory for efficient traversal.
    // This function should err on the side of making a too-small cursor.

    const uInt ndim = shape.nelements();

    // *2 because the pixels might exist in a buffer as well. We should
    // be able to do away with that.
    uInt maxPixels = memoryInMB * 1024 * 1024 / 
	(imagePixelSize*2 + fitsPixelSize*2);

    maxPixels /= 2; // because 1/2 the pixels are in FITS, 1/2 in Image

    Int axis = ndim - 1;
    if (shape.product() > maxPixels) {
	while (--axis >= 0 && shape(axis) == 1) {
	    ; // Nothing
	}
    }

    if (axis < 0) {
	axis = 0; // If we have a 1D image
    }

    uInt prod = 1;
    for (uInt i=0; i<=axis; i++) {
	prod *= shape(i);
    }
    // Correct for the probable tile shape
    for (i=axis+1; i<ndim; i++) {
	if (shape(i) > 1) {
	    if (shape(i) < 32) {
		prod *= shape(i);
	    } else {
		prod *= 32;
	    }
	}
    }
    // If the image slice is still too large drop back another axis.
    // The image will still be taking too much space, but the FITS buffering
    // won't contribute to the delinquency.
    if (prod > maxPixels) {
	while (--axis >= 0 && shape(axis) == 1) {
	    ; // Nothing
	}
    }

    if (axis < 0) {
	axis = 0; // If we have a 1D image
    }

    IPosition cursorShape(ndim);
    cursorShape = 1;
    for (i=0; i<=axis; i++) {
	cursorShape(i) = shape(i);
    }

    ostrstream buffer;
    if (axis == ndim - 1) {
	buffer << "All pixels fit in memory";
    } else {
	switch(axis) {
	case 0: buffer << "Copying row by row"; break;
	case 1: buffer << "Copying plane by plane"; break;
	case 2: buffer << "Copying cube by cube"; break;
	default:  buffer << "Copying hypercube by hypercube"; 
	}
    }
    buffer << " (" << cursorShape.product() << " pixels).";
    report = String(buffer);
    return cursorShape;
}
