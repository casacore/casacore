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
#include <aips/Quanta/UnitMap.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Containers/Record.h>

#include <aips/OS/File.h>
#include <aips/OS/RegularFile.h>
#include <aips/OS/SymLink.h>
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
    if (!fitsfile.exists() || !fitsfile.isReadable() || 
	!fitsfile.isRegular()) {
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
        break;
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
				     const ImageInterface<Float> &image,
				     const String &fitsName, 
				     uInt memoryInMB,
				     Bool preferVelocity,
				     Bool opticalVelocity,
				     Int BITPIX, Float minPix, Float maxPix,
				     Bool allowOverwrite)
{
    LogIO log(LogOrigin("ImageFITSConverter", "ImageToFITS", WHERE));
    error = "";

    FitsOutput *outfile = 0;

    if (fitsName == "-") {
	// Write to stdout
	outfile = new FitsOutput(cout);
    } else {
	// Make sure that the fits file does not already exist, and that we
	// can write to the directory
	File fitsfile(fitsName);
	if (fitsfile.exists()) {
	    if (allowOverwrite) {
		String msg;
		try {
		    if (fitsfile.isRegular()) {
			RegularFile rfile(fitsfile);
			rfile.remove();
		    } else if (fitsfile.isDirectory()) {
			Directory dfile(fitsfile);
			dfile.remove();
		    } else if (fitsfile.isSymLink()) {
			SymLink sfile(fitsfile);
			sfile.remove();
		    } else {
			msg = "Cannot remove file - unknown file type";
		    }
		} catch (AipsError x) {
		    msg = x.getMesg();
		} end_try;
		if (fitsfile.exists()) {
		    error = "Could not remove " + fitsName;
		    if (msg != "") {
			error += ": (" + msg + ")";
		    }
		    return False;
		}
	    } else {
		error = fitsName + " already exists, will not overwrite.";
		return False;
	    }
	}
	Directory fitsdir = fitsfile.path().dirName();
	if (!fitsdir.exists() || !fitsdir.isWritable()) {
	    error = String("Directory ") + fitsdir.path().originalName() + 
		" does not exist or is not writable";
	    return False;
	}
    
	// OK, it appears to be a writable etc. file, let's try opening it.
	outfile = new FitsOutput(fitsfile.path().expandedName(), FITS::Disk);
    }

    if (outfile == 0 || outfile->err()) {
	error = String("Cannot open file for writing: ") + fitsName;
	if (outfile != 0) { 
	    delete outfile;
	}
	return False;
    }


    CoordinateSystem coordsys = image.coordinates();
    if (coordsys.nWorldAxes() != coordsys.nPixelAxes()) {
	error = "FITS requires that the number of world and pixel axes be"
	    " identical.";
	return False;
    }

    Record header;

    Double bscale, bzero;
    const Short maxshort = 32767;
    const Short minshort = -32768;
    Bool hasBlanks = True;
    if (BITPIX == -32) {
        bscale = 1.0;
        bzero = 0.0;
	header.define("bitpix", BITPIX);
	header.setComment("bitpix", "Floating point (32 bit)");
    } else if (BITPIX == 16) {
	header.define("bitpix", BITPIX);
	header.setComment("bitpix", "Short integer (16 bit)");
        if (minPix > maxPix) {
	    // Find the min and max of the image
	    LogIO os;
	    os << LogOrigin("ImageFitsConverter", "ImageToFITS", WHERE);
	    os << LogIO::NORMAL << 
	      "Finding scaling factors for BITPIX=16 and look for blanks" <<
		LogIO::POST;
	    hasBlanks = False;

	    IPosition cursorShape(image.niceCursorShape(image.maxPixels()));
	    IPosition shape = image.shape();
	    RO_LatticeIterator<Float> iter(image, 
		   LatticeStepper(shape, cursorShape, LatticeStepper::RESIZE));
	    ProgressMeter meter(0.0, 1.0*shape.product(), "Searching pixels", ""
				"", True, 
				shape.product()/cursorShape.product()/50);
	    uInt count = 0;
	    for (iter.reset(); !iter.atEnd(); iter++) {
		const Array<Float> &cur = iter.cursor();
		Bool del;
		const Float *cptr = cur.getStorage(del);
		const uInt n = cur.nelements();
		for (uInt i=0; i<n; i++) {
		    if (cptr[i] != cptr[i]) {  // For a Nan, x != x
			hasBlanks = True;
		    } else {
			// Not a NaN
			if (minPix > maxPix) {
			    // First non-Nan we have run into. Init.
			    minPix = maxPix = cptr[i];
			} else {
			    if (cptr[i] < minPix) minPix = cptr[i];
			    if (cptr[i] > maxPix) maxPix = cptr[i];
			}
		    }
		}
		count += n;
		meter.update(count*1.0);
		cur.freeStorage(cptr, del);
	    }
        }
	if (hasBlanks) {
	    bscale = Double(maxPix - minPix)/Double(Int(maxshort) - 
						    Int(minshort+1));
	    bzero  = Double(minPix) + bscale * (-Double(minshort+1));
	} else {
	    bscale = Double(maxPix - minPix)/Double(Int(maxshort) - 
						    Int(minshort));
	    bzero  = Double(minPix) + bscale * (-Double(minshort));
	}
    } else {
	error = 
            "BITPIX must be -32 (floating point) or 16 (short integer)";
        return False;
    }

    const IPosition shape = image.shape();
    const uInt ndim = shape.nelements();
    header.setComment("bitpix", "Floating point (32 bit)");

    Vector<Int> naxis(ndim);
    uInt i;
    for (i=0; i < ndim; i++) {
        naxis(i) = shape(i);
    }
    header.define("NAXIS", naxis);

    header.define("bscale", bscale);
    header.setComment("bscale", "PHYSICAL = PIXEL*BSCALE + BZERO");
    header.define("bzero", bzero);
    if (BITPIX > 0 && hasBlanks) {
	header.define("blank", minshort);
	header.setComment("blank", "Pixels with this value are blank");
    }

    header.define("COMMENT1", ""); // inserts spaces

    // I should FITS-ize the units
    header.define("BUNIT", upcase(image.units().getName()).chars());
    header.setComment("BUNIT", "Brightness (pixel) unit");

    IPosition shapeCopy = shape;
    Bool ok = coordsys.toFITSHeader(header, shapeCopy, True, 'c', False, 
				    preferVelocity,
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

	IPosition shapeCopy = shape;
	Bool ok = coordsys.toFITSHeader(header, shapeCopy, True);
	if (!ok) {
	    error = "Fallback linear coordinate system fails also.";
	    return False;
	}

    }
    if (naxis.nelements() != shapeCopy.nelements()) {
        naxis.resize(shapeCopy.nelements());
	for (uInt j=0; j < shapeCopy.nelements(); j++) {
	    naxis(j) = shapeCopy(j);
	}
	header.define("NAXIS", naxis);
    }
    

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
		if (miscname.contains("date") && miscname != "date") {
		    // Try to canonicalize dates (i.e. solve Y2K)
		    String outdate;
		    // We only need to convert the date, the timesys we'll just
		    // copy through
		    if (FITSDateUtil::convertDateString(outdate, 
					image.miscInfo().asString(i))) {
			// Conversion worked - change the header
			header.define(miscname, outdate);
		    } else {
			// conversion failed - just copy the existing date
			header.define(miscname, image.miscInfo().asString(i));
		    }
		} else {
		    // Just copy non-date strings through
		    header.define(miscname, image.miscInfo().asString(i));
		}
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
	if (header.isDefined(miscname)) {
	    header.setComment(miscname, image.miscInfo().comment(i));
	}
    }


    // DATE
    String date, timesys;
    Time nowtime;
    MVTime now(nowtime);
    FITSDateUtil::toFITS(date, timesys, now);
    header.define("date", date);
    header.setComment("date", "Date FITS file was written");
    if (!header.isDefined("timesys") && !header.isDefined("TIMESYS")) {
	header.define("timesys", timesys);
	header.setComment("timesys", "Time system for HDU");
    }

    // ORIGIN
    ostrstream buffer;
    buffer << "AIPS++ version ";
    VersionInfo::report(buffer);
    header.define("ORIGIN", String(buffer));

    // Set up the FITS header
    FitsKeywordList kw = FITSKeywordUtil::makeKeywordList();

    ok = FITSKeywordUtil::addKeywords(kw, header);
    if (! ok) {
	error = "Error creating initial FITS header";
	return False;
    }

    // HISTORY
    if (image.logSink().localSink().isTableLogSink()) {
	const TableLogSink &logTable = 
	    image.logSink().localSink().castToTableLogSink();
	Vector<String> historyChunk;
	uInt nstrings;
	Bool aipsppFormat;
	uInt firstLine = 0;
	while (1) {
	    firstLine = FITSHistoryUtil::toHISTORY(historyChunk, aipsppFormat, 
						   nstrings, firstLine,
						   logTable);
	    if (nstrings == 0) {
		break;
	    }
	    String groupType;
	    if (aipsppFormat) {
		groupType = "LOGTABLE";
	    }
	    FITSHistoryUtil::addHistoryGroup(kw, historyChunk, nstrings,
					     groupType);
	}
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
    AlwaysAssert(sizeof(Short) == sizeof(short), AipsError);

    IPosition cursorOrder(ndim);
    for (i=0; i<ndim; i++) {
	cursorOrder(i) = i;
    }

    try {
        Int nIter = max(1,image.shape().product()/cursorShape.product());
        Int iUpdate = max(1,nIter/20);
	ProgressMeter meter(0.0, 1.0*image.shape().product(),
			    "Image to FITS", "Pixels copied", "", 
                            "", True, iUpdate);
	uInt count = 0;
	Double curpixels = 1.0*cursorShape.product();

	LatticeStepper stepper(shape, cursorShape, cursorOrder);
	RO_LatticeIterator<Float> iter(image, stepper);
	const Int bufferSize = cursorShape.product();

	PrimaryArray<Float> *fits32 = 0;
	PrimaryArray<Short> *fits16 = 0;
 	if (BITPIX == -32) {
	    fits32 = new PrimaryArray<Float>(kw);
 	    if (fits32==0 || fits32->err()) {
 		error = "Error creating FITS file from keywords";
 		return False;
 	    }
 	    if (fits32->write_hdr(*outfile)) {
 		error = "Error writing FITS header";
		delete outfile;
 		return False;
 	    }
 	} else if (BITPIX == 16) {
 	    fits16 = new PrimaryArray<Short>(kw);
 	    if (fits16==0 || fits16->err()) {
 		error = "Error creating FITS file from keywords";
 		return False;
 	    }
 	    if (fits16->write_hdr(*outfile)) {
		delete outfile;
 		error = "Error writing FITS header";
 		return False;
 	    }
 	} else {
 	    AlwaysAssert(0, AipsError); // NOTREACHED
 	}

	Short *buffer16 = 0; // Use this to write the scaled shorts into
	if (fits16) {
	    buffer16 = new Short[bufferSize];
	    AlwaysAssert(buffer16, AipsError);
	}
	for (iter.reset(); !iter.atEnd(); iter++) {
	    const Array<Float> &cursor = iter.cursor();
	    Bool deleteIt;
	    const Float *ptr = cursor.getStorage(deleteIt);
	    meter.update((count*1.0 - 0.5)*curpixels);

	    error= "";
	    Int n = 0;
	    if (fits32) {
		fits32->store(ptr, bufferSize);
		if (!fits32->err()) {
		    n = fits32->write(*outfile);
		    if (n != bufferSize) {
			delete outfile;
			error = "Write failed (full disk or tape?)";
			return False;
		    }
		} else {
		    error = "Unknown I/O error";
		    return False;
		}
	    } else if (fits16) {
		short blankOffset = hasBlanks ? 1 : 0;
		for (Int j=0; j<bufferSize; j++) {
		    if (ptr[j] != ptr[j]) {
			// NaN
			buffer16[j] = minshort;
		    } else {
			// Not a NaN
			if (ptr[j] > maxPix) {
			    buffer16[j] = maxshort;
			} else if (ptr[j] < minPix) {
			    buffer16[j] = minshort + blankOffset;
			} else {
			    buffer16[j] = Short((ptr[j] - bzero)/bscale);
			}
		    }
		}
		fits16->store(buffer16, bufferSize);
		if (!fits16->err()) {
		    n = fits16->write(*outfile);
		    if (n != bufferSize) {
			delete outfile;
			error = "Write failed (full disk or tape?";
			return False;
		    }
		} else {
		    error = "Unknown I/O error";
		    return False;
		}
	    } else {
		AlwaysAssert(0, AipsError); // NOTREACHED
	    }
	    cursor.freeStorage(ptr, deleteIt);
	    if ((fits32 && fits32->err()) ||
		(fits16 && fits16->err()) ||
		outfile->err()) {
		error = String("Error writing into ") + fitsName;
		delete outfile;
		return False;
	    }
	    count++;
	    meter.update(count*curpixels);
	}
	if (fits32) {
	    delete fits32; fits32 = 0;
	} else if (fits16) {
	    delete fits16; fits16 = 0;
	    delete buffer16; buffer16 = 0;
	} else {
	    AlwaysAssert(0, AipsError); // NOTREACHED
	}
    } catch (AipsError x) {
	error = "Unknown error copying image to FITS file";
	if (outfile) {
	    delete outfile;
	}
	return False;
    } end_try;

    delete outfile;
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
    if (shape.product() > Int(maxPixels)) {
	while (--axis >= 0 && shape(axis) == 1) {
	    ; // Nothing
	}
    }

    if (axis < 0) {
	axis = 0; // If we have a 1D image
    }

    uInt prod = 1;
    uInt i;
    for (i=0; Int(i)<=axis; i++) {
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
    for (i=0; Int(i)<=axis; i++) {
	cursorShape(i) = shape(i);
    }

    ostrstream buffer;
    if (axis == Int(ndim) - 1) {
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
