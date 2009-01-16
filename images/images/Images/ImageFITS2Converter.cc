//# ImageFITS2Converter.cc : non-templated FITS<->aips++ image conversion 
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

//#include <casa/version.h>

#include <images/Images/ImageFITSConverter.h>
#include <images/Images/PagedImage.h>
#include <images/Images/ImageInfo.h>
#include <lattices/Lattices/MaskedLatticeIterator.h>
#include <lattices/Lattices/LatticeStepper.h>
#include <fits/FITS/fitsio.h>
#include <fits/FITS/hdu.h>
#include <fits/FITS/FITSDateUtil.h>
#include <fits/FITS/FITSKeywordUtil.h>
#include <fits/FITS/FITSHistoryUtil.h>
#include <coordinates/Coordinates/LinearCoordinate.h>
#include <coordinates/Coordinates/StokesCoordinate.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/ObsInfo.h>

#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Quanta/UnitMap.h>
#include <casa/Arrays/IPosition.h>
#include <casa/BasicMath/Math.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>

#include <casa/Quanta/MVTime.h>

#include <casa/OS/File.h>
#include <casa/OS/RegularFile.h>
#include <casa/OS/SymLink.h>
#include <casa/OS/Directory.h>

#include <casa/Utilities/Assert.h>
#include <casa/Logging/LogIO.h>
#include <casa/System/ProgressMeter.h>

#include <casa/sstream.h>
#include <casa/iomanip.h>


namespace casa { //# NAMESPACE CASA - BEGIN

Bool ImageFITSConverter::FITSToImage(ImageInterface<Float> *&newImage,
				     String &error,
				     const String &imageName,
				     const String &fitsName, 
				     uInt whichRep,
				     uInt whichHDU,
				     uInt memoryInMB,
				     Bool allowOverwrite,
				     Bool zeroBlanks)
{
    newImage = 0;
    error = "";
    AlwaysAssert(whichRep>=0,AipsError);

// First make sure that imageName is writable and does not already
// exist.  Optionally remove it if it does.  If imageName is empty,
// great.  That means we are going to make a TempImage

    if (!imageName.empty()) {
       File imfile(imageName);
       if (!ImageFITSConverter::removeFile (error, imfile, imageName, allowOverwrite)) return False;
//
       Directory imdir = imfile.path().dirName();
       if (!imdir.exists() || !imdir.isWritable()) {
          error = String("Directory ") + imdir.path().originalName() + 
                  " does not exist or is not writable";
          return False;
       }
   }
//
    File fitsfile(fitsName);
    if (!fitsfile.exists() || !fitsfile.isReadable() || 
	!fitsfile.isRegular()) {
        error = fitsName + " does not exist or is not readable";
	return False;
    }
//
// OK, now see if we can attach the FITS reading classes
//
    FitsInput infile(fitsfile.path().expandedName().chars(), FITS::Disk);
    if (infile.err()) {
        error = String("Cannot open file (or other I/O error): ") + fitsName;
	return False;
    }
//
// Advance to the right HDU
//
    for (uInt i=0; i<whichHDU; i++) {
	infile.skip_hdu();
	if (infile.err()) {
	    error = "Error advancing to image in file: " + fitsName;
	    return False;
	}
    }
//
// Make sure the current spot in the FITS file is an image
//
    if (infile.rectype() != FITS::HDURecord ||
	(infile.hdutype() != FITS::PrimaryArrayHDU &&
         infile.hdutype() != FITS::ImageExtensionHDU)) {
	error = "No image at specified location in file " + fitsName;
        return False;
    }
//    
// The rest has to be done in a type dependent way - hand over to template
// functions.
//
    switch(infile.datatype()) {
    case FITS::BYTE:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<unsigned char> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<unsigned char> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    } else {
		ImageExtension<unsigned char> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<unsigned char> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    }
        }
    break;
    case FITS::SHORT:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<short> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<short> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    } else {
		ImageExtension<short> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<short> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    }
        }
        break;
    case FITS::LONG:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<FitsLong> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<FitsLong> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    } else {
		ImageExtension<FitsLong> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<FitsLong> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    }
        }
        break;
    case FITS::FLOAT:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<Float> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<Float> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    } else {
		ImageExtension<Float> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<Float> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    }
        }
        break;
    case FITS::DOUBLE:
        {
	    if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<Double> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<Double> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    } else {
		ImageExtension<Double> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<Double> >::FITSToImage(
		       newImage, error, imageName, whichRep, fitsdata, memoryInMB, zeroBlanks);
	    }
        }
        break;
    default:
        error = "Unknown datatype  - no data returned";
	return False;
    }
    return True;

}


Bool ImageFITSConverter::ImageToFITS(String &error,
				     ImageInterface<Float>& image,
				     const String &fitsName, 
				     uInt memoryInMB,
				     Bool preferVelocity,
				     Bool opticalVelocity,
				     Int BITPIX, Float minPix, Float maxPix,
				     Bool allowOverwrite, Bool degenerateLast,
                                     Bool verbose)
{
//
// Make a logger
//
    LogIO os;
    os << LogOrigin("ImageFitsConverter", "ImageToFITS", WHERE);
//
    error = "";
    FitsOutput *outfile = 0;

    if (fitsName == "-") {
	// Write to stdout
	outfile = new FitsOutput();
    } else {

// Make sure that the fits file does not already exist, and that we
// can write to the directory

	File fitsfile(fitsName);
        if (!ImageFITSConverter::removeFile (error, fitsfile, fitsName, allowOverwrite)) return False;
//
	Directory fitsdir = fitsfile.path().dirName();
	if (!fitsdir.exists() || !fitsdir.isWritable()) {
	    error = String("Directory ") + fitsdir.path().originalName() + 
		" does not exist or is not writable";
	    return False;
	}
//    
// OK, it appears to be a writable etc. file, let's try opening it.
//
	outfile = new FitsOutput(fitsfile.path().expandedName().chars(),
				 FITS::Disk);
    }
//
    if (outfile == 0 || outfile->err()) {
	error = String("Cannot open file for writing: ") + fitsName;
	if (outfile != 0) { 
	    delete outfile;
	}
	return False;
    }
//
// Get coordinates and test that axis removal has been
// mercifully absent
//
    CoordinateSystem cSys = image.coordinates();
    if (cSys.nWorldAxes() != cSys.nPixelAxes()) {
	error = "FITS requires that the number of world and pixel axes be"
	    " identical.";
	return False;
    }

//
// Make degenerate axes last if requested
//
   IPosition shape = image.shape();
   IPosition newShape = shape;
   const uInt ndim = shape.nelements();
   if (degenerateLast) {
      IPosition shape2(ndim);
      IPosition cursorShape2(ndim);
      Vector<Int> order(ndim);
      uInt j = 0;
      for (uInt i=0; i<ndim; i++) {
         if (shape(i)>1) {
            order(j) = i;
            newShape(j) = shape(i);
            j++;
         }
      }
      for (uInt i=0; i<ndim; i++) {
         if (shape(i)==1) {
            order(j) = i;
            newShape(j) = shape(i);
            j++;
         }
      }
//
      cSys.transpose(order,order);
   }
//
    Bool applyMask = False;
    Array<Bool>* pMask = 0;
    if (image.isMasked()) {
       applyMask = True;
       pMask = new Array<Bool>(IPosition(0,0));
    } 
//
// Find scale factors
//
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
//
// We don't yet know if the image has blanks or not, so assume it does.
//
        hasBlanks = True;
    } else if (BITPIX == 16) {
	header.define("bitpix", BITPIX);
	header.setComment("bitpix", "Short integer (16 bit)");
        if (minPix > maxPix) {
	    // Find the min and max of the image
            if (verbose) {
   	       os << LogIO::NORMAL << 
	      "Finding scaling factors for BITPIX=16 and look for masked or blanked values" <<
		LogIO::POST;
            }
	    hasBlanks = False;
//
// Set up iterator
//
            IPosition cursorShape(image.niceCursorShape());
	    RO_MaskedLatticeIterator<Float> iter(image, 
		   LatticeStepper(shape, cursorShape, LatticeStepper::RESIZE));
	    ProgressMeter meter(0.0, 1.0*shape.product(),
				"Searching pixels", "",
				"", "", True, 
				shape.product()/cursorShape.product()/50);
//
// Iterate
//
	    uInt count = 0;
            Bool deleteMaskPtr, deletePtr;
	    for (iter.reset(); !iter.atEnd(); iter++) {
		const Array<Float> &cursor = iter.cursor();
		const Float *cptr = cursor.getStorage(deletePtr);
		const uInt n = cursor.nelements();
// 
                if (applyMask) {
                   if (!pMask->shape().isEqual(cursor.shape())) pMask->resize(cursor.shape());
                   (*pMask) = iter.getMask(False);
                   const Bool* maskPtr = pMask->getStorage(deleteMaskPtr);
//
// If a pixel is a NaN or the mask is False, it goes out as a NaN
//
                   for (uInt i=0; i<n; i++) {
                      if (isNaN(cptr[i]) || !maskPtr[i]) {
                         hasBlanks = True;
                      } else {
                         if (minPix > maxPix) {
                            minPix = maxPix = cptr[i];
                         } else {
                            if (cptr[i] < minPix) minPix = cptr[i];
                            if (cptr[i] > maxPix) maxPix = cptr[i];
                         }
                      }
                   }
                   pMask->freeStorage(maskPtr, deleteMaskPtr);
                } else {
                   for (uInt i=0; i<n; i++) {
                      if (isNaN(cptr[i])) {
                         hasBlanks = True;
                      } else {
                         if (minPix > maxPix) {
// First non-NaN we have run into. Init.
                            minPix = maxPix = cptr[i];
                         } else {
                            if (cptr[i] < minPix) minPix = cptr[i];
                            if (cptr[i] > maxPix) maxPix = cptr[i];
                         }
                      }
                   }
                }
		count += n;
		meter.update(count*1.0);
		cursor.freeStorage(cptr, deletePtr);
	    }
        }

// Make sure bscale does not come out to be zero

        if (::casa::near(minPix, maxPix)) {
           if (::casa::near(Float(0.0), maxPix)) {
              maxPix = 1.0;
           } else {
              maxPix = maxPix + 0.01*maxPix;
           }
        } 
//
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


// At this point, for 32 floating point, we must apply the given
// mask.  For 16bit, we may know that there are in fact no blanks
// in the image, so we can dispense with looking at the mask again.

    if (applyMask && !hasBlanks) applyMask = False;
//
    Vector<Int> naxis(ndim);
    uInt i;
    for (i=0; i < ndim; i++) {
        naxis(i) = newShape(i);
    }
    header.define("naxis", naxis);
    header.define("bscale", bscale);
    header.setComment("bscale", "PHYSICAL = PIXEL*BSCALE + BZERO");
    header.define("bzero", bzero);
    if (BITPIX>0 && hasBlanks) {
	header.define("blank", minshort);
	header.setComment("blank", "Pixels with this value are blank");
    }
    if (BITPIX>0) {
        header.define("datamin", minPix);
        header.define("datamax", maxPix);
    }
//
    ImageInfo ii = image.imageInfo();
    if (!ii.toFITS (error, header)) return False;
//
    header.define("COMMENT1", ""); // inserts spaces

// I should FITS-ize the units

    header.define("BUNIT", upcase(image.units().getName()).chars());
    header.setComment("BUNIT", "Brightness (pixel) unit");
//
    IPosition shapeCopy = newShape;
    Record saveHeader(header);
    Bool ok = cSys.toFITSHeader(header, shapeCopy, True, 'c', True, // use WCS 
                                preferVelocity, opticalVelocity);

    if (!ok) {
	os << LogIO::SEVERE << "Could not make a standard FITS header. Setting"
	    " a simple linear coordinate system." << LogIO::POST;
//
	uInt n = cSys.nWorldAxes();
	Matrix<Double> pc(n,n); pc=0.0; pc.diagonal() = 1.0;
	LinearCoordinate linear(cSys.worldAxisNames(), 
				cSys.worldAxisUnits(),
				cSys.referenceValue(),
				cSys.increment(),
				cSys.linearTransform(),
				cSys.referencePixel());
	CoordinateSystem linCS;
	linCS.addCoordinate(linear);

// Recover old header before it got mangled by toFITSHeader

	header = saveHeader;
	IPosition shapeCopy = newShape;
	Bool ok = linCS.toFITSHeader(header, shapeCopy, True, 'c', False); // don't use WCS
	if (!ok) {
	    error = "Fallback linear coordinate system fails also.";
	    return False;
	}
    }

// When this if test is True, it means some pixel axes had been removed from 
// the coordinate system and degenerate axes were added.

    if (naxis.nelements() != shapeCopy.nelements()) {
        naxis.resize(shapeCopy.nelements());
	for (uInt j=0; j < shapeCopy.nelements(); j++) {
	    naxis(j) = shapeCopy(j);
	}
	header.define("NAXIS", naxis);
    }
    
//
// Add in the fields from miscInfo that we can
//
    const uInt nmisc = image.miscInfo().nfields();
    for (i=0; i<nmisc; i++) {
 	String tmp0 = image.miscInfo().name(i);
        String miscname(tmp0.at(0,8));
        if (tmp0.length() > 8) {
           os  << LogIO::WARN << "Truncating miscinfo field " << tmp0 
               << " to " << miscname << LogIO::POST;
        }
//         
	if (miscname != "end" && miscname != "END") {
          if (header.isDefined(miscname)) {
// These warnings just cause confusion.  They are usually
// from the alt* keywords which FITSSpectralUtil writes.
// They may also have been preserved in miscInfo when an
// image came from FITS and hence the conflict.

/*
             os << LogIO::WARN << "FITS keyword " << miscname 
                << " is already defined so dropping it" << LogIO::POST;
*/
          } else {
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
		    ostringstream os;
		    os << misctype;
		    os << LogIO::NORMAL << "Not writing miscInfo field '" <<
			miscname << "' - cannot handle type " << String(os) <<
			LogIO::POST;
		}
	    }
	}
	if (header.isDefined(miscname)) {
	    header.setComment(miscname, image.miscInfo().comment(i));
	}
      }
    }

//
// DATE
//
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
//
// ORIGIN
//
    ostringstream buffer;
    buffer << "CASA casacore alma-evla ";
    // VersionInfo::report(buffer);
    header.define("ORIGIN", String(buffer));

    // Set up the FITS header
    FitsKeywordList kw = FITSKeywordUtil::makeKeywordList();
    ok = FITSKeywordUtil::addKeywords(kw, header);
    if (! ok) {
	error = "Error creating initial FITS header";
	return False;
    }

//
// HISTORY
//
  LoggerHolder& logger = image.logger();
//
  Vector<String> historyChunk;
  uInt nstrings;
  Bool aipsppFormat;
  uInt firstLine = 0;
  while (1) {
     firstLine = FITSHistoryUtil::toHISTORY(historyChunk, aipsppFormat, 
                                            nstrings, firstLine, logger);
     if (nstrings == 0) {
        break;
     }
     String groupType;
     if (aipsppFormat) groupType = "LOGTABLE";
     FITSHistoryUtil::addHistoryGroup(kw, historyChunk, nstrings, groupType);
  }    
//
// END
//
    kw.end();

//
// Finally get around to copying the data
//
    String report;
    IPosition newCursorShape = copyCursorShape(report,
					    shape,
					    sizeof(Float),
					    sizeof(Float),
					    memoryInMB);

    if (verbose) {
       os << "Copying '" << image.name() << "' to '" << fitsName << "'   "
          << report << LogIO::POST;
    }
//
// If this fails, more development is needed
//
    AlwaysAssert(sizeof(Float) == sizeof(float), AipsError);
    AlwaysAssert(sizeof(Short) == sizeof(short), AipsError);

    IPosition cursorOrder(ndim);
    for (i=0; i<ndim; i++) {
	cursorOrder(i) = i;
    }

    try {
        Int nIter = max(1,shape.product()/newCursorShape.product());
        Int iUpdate = max(1,nIter/20);
//
	ProgressMeter* pMeter = 0;
        if (verbose) pMeter = new ProgressMeter(0.0, 1.0*shape.product(),
                                                "Image to FITS", "Pixels copied", "", 
                                                "", True, iUpdate);
	uInt count = 0;
	Double curpixels = 1.0*newCursorShape.product();
//
	LatticeStepper stepper(shape, newCursorShape, cursorOrder);
	RO_MaskedLatticeIterator<Float> iter(image, stepper);
	const Int bufferSize = newCursorShape.product();
//
	PrimaryArray<Float>* fits32 = 0;
	PrimaryArray<Short>* fits16 = 0;
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
//
// Iterate through the image.  
//
	for (iter.reset(); !iter.atEnd(); iter++) {
	    const Array<Float>& cursor = iter.cursor();
	    Bool deletePtr;
	    const Float* ptr = cursor.getStorage(deletePtr);
//
	    const Bool* maskPtr = 0;
            Bool deleteMaskPtr;
            if (applyMask) {
               if (!pMask->shape().isEqual(cursor.shape())) {
                  pMask->resize(cursor.shape());
               }
               (*pMask) = iter.getMask(False);
               maskPtr = pMask->getStorage(deleteMaskPtr);
            }
//
//	    pMeter->update((count*1.0 - 0.5)*curpixels);
//
            const uInt nPts = cursor.nelements();
	    error= "";
	    Int n = 0;
	    if (fits32) {
                if (applyMask) {
                   Float* ptr2 = new float[nPts];
                   for (uInt j=0; j<nPts; j++) {
                      if (maskPtr[j]) {
                         ptr2[j] = ptr[j];
                      } else {
                         ptr2[j] = ptr[j];
                         setNaN(ptr2[j]);
                      }
                   }
                   fits32->store(ptr2, bufferSize);
                   delete [] ptr2;
                } else {
                   fits32->store(ptr, bufferSize);
                }
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
//
                if (applyMask) { 
                   for (Int j=0; j<bufferSize; j++) {
//                    if (ptr[j] != ptr[j] || maskPtr[j]) {
                      if (isNaN(ptr[j]) || !maskPtr[j]) {
                         buffer16[j] = minshort;
                      } else {
                         if (ptr[j] > maxPix) {
                            buffer16[j] = maxshort; 
                         } else if (ptr[j] < minPix) {
                            buffer16[j] = minshort + blankOffset;
                         } else {
                            buffer16[j] = Short((ptr[j] - bzero)/bscale);
                         }
                      }
                   }
                } else {
                   for (Int j=0; j<bufferSize; j++) {
//                    if (ptr[j] != ptr[j]) {
                      if (isNaN(ptr[j])) {
                         buffer16[j] = minshort;
                      } else {
                         if (ptr[j] > maxPix) {
                            buffer16[j] = maxshort; 
                         } else if (ptr[j] < minPix) {
                            buffer16[j] = minshort + blankOffset;
                         } else {
                            buffer16[j] = Short((ptr[j] - bzero)/bscale);
                         }
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
//
	    cursor.freeStorage(ptr, deletePtr);
	    if (applyMask) pMask->freeStorage(maskPtr, deleteMaskPtr);
//
	    if ((fits32 && fits32->err()) ||
		(fits16 && fits16->err()) ||
		outfile->err()) {
		error = String("Error writing into ") + fitsName;
		delete outfile;
		return False;
	    }
	    count++;
	    if (verbose) pMeter->update(count*curpixels);
	}
	if (fits32) {
	    delete fits32; fits32 = 0;
	} else if (fits16) {
	    delete fits16; fits16 = 0;
	    delete buffer16; buffer16 = 0;
	} else {
	    AlwaysAssert(0, AipsError); // NOTREACHED
	}
//
        if (pMeter) delete pMeter;
        if (pMask!=0) delete pMask;
    } catch (AipsError x) {
	error = "Unknown error copying image to FITS file";
	if (outfile) {
	    delete outfile;
	}
	return False;
    } 

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

    ostringstream buffer;
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


Bool ImageFITSConverter::removeFile (String& error, const File& outFile, 
                                     const String& outName, Bool allowOverwrite)
{
   if (outFile.exists()) {
      if (allowOverwrite) {
         String msg;
         try {
            if (outFile.isRegular()) {
		RegularFile rfile(outFile);
		rfile.remove();
	    } else if (outFile.isDirectory()) {
		Directory dfile(outFile);
		dfile.removeRecursive();
	    } else if (outFile.isSymLink()) {
		SymLink sfile(outFile);
		sfile.remove();
	    } else {
		msg = "Cannot remove file - unknown file type";
	    }
         } catch (AipsError x) {
            msg = x.getMesg();
         } 
//
         if (outFile.exists()) {
	    error = "Could not remove file " + outName;
	    if (msg != "") {
		error += ": (" + msg + ")";
	    }
	    return False;
         }
      } else {
         error = outName + " already exists, will not overwrite.";
         return False;
      }
   }
   return True;
}



CoordinateSystem ImageFITSConverter::getCoordinateSystem (Int& stokesFITSValue, 
                                                          RecordInterface& headerRec,
                                                          const Vector<String>& header,
                                                          LogIO& os,
                                                          uInt whichRep,
                                                          IPosition& shape,
                                                          Bool dropStokes)
{

// Get CS and return un-used cards in a Record for further use

    CoordinateSystem cSys;
    if (!CoordinateSystem::fromFITSHeader (stokesFITSValue, cSys, headerRec, header, 
                                          shape, whichRep)) {
        os << LogIO::WARN <<
          "Cannot create the coordinate system from FITS keywords.\n"
          "I will use a dummy linear coordinate along each axis instead.\n"
          "If you your FITS file actually does contain a coordinate system\n"
          "please submit a bug report."  << LogIO::POST;
//
        CoordinateSystem cSys2;
        Vector<String> names(shape.nelements());
        for (uInt i=0; i<names.nelements(); i++) {
           ostringstream oss;
           oss << i;
           names(i) = String("linear") + String(oss);
        }   
        CoordinateUtil::addLinearAxes(cSys2, names, shape);
        cSys = cSys2;
    }

// Check shape and CS consistency.  Add dummy axis to shape if possible

    if (shape.nelements() != cSys.nPixelAxes()) {
       IPosition shape2;
       if (cSys.nPixelAxes() > shape.nelements()) {
          Int nDeg = cSys.nPixelAxes() - shape.nelements();
          shape2.resize(cSys.nPixelAxes());
          shape2 = 1;
          for (uInt i=0; i<shape.nelements(); i++) shape2(i) = shape(i);
          shape.resize(0);
          shape = shape2;
//
          os << LogIO::WARN << "Image dimension appears to be less than number of pixel axes in CoordinateSystem" << endl;
          os << "Adding " << nDeg << " degenerate trailing axes" << LogIO::POST;
       } else {
          os << "Image contains more dimensions than the CoordinateSystem defines" << LogIO::EXCEPTION;
       }
    }

// Drop Stokes axis IF it's of length 1 AND there is an unoffical 
// pseudo-STokes value (e.g. optical dpeth) on it.  This is stored
// in ImageInfo instead.

    Int after = -1;
    Int c = cSys.findCoordinate(Coordinate::STOKES, after);
    if (dropStokes && c >= 0 && stokesFITSValue >= 0) {
       uInt nS = cSys.stokesCoordinate(c).stokes().nelements();
       if (nS==1) {
          CoordinateSystem cSys2;
          for (uInt i=0; i<cSys.nCoordinates(); i++) {
             if (cSys.type(i) != Coordinate::STOKES) {
                cSys2.addCoordinate(cSys.coordinate(i));
             } 
          }
//
          uInt dropAxis = cSys.pixelAxes(c)(0);
          cSys = cSys2;
          IPosition shape2(cSys.nPixelAxes());
          uInt j = 0;
          for (uInt i=0; i<shape.nelements(); i++) {
             if (i!=dropAxis) {
                shape2(j) = shape(i);
                j++;
             }
          }
//
          shape.resize(0);
          shape = shape2;
       }
    }

// Remove unwanted left-over Coordinate-related keywords

    Vector<String> ignore(4);
    ignore(0) = "^date-map$";
    ignore(1) = "date";
    ignore(2) = "^naxis";
    ignore(3) = "^naxis$";
    FITSKeywordUtil::removeKeywords(headerRec, ignore);
//
    return cSys;
}


// CoordinateSystem ImageFITSConverter::getCoordinateSystemOld (Int& stokesFITSValue,
//                                                           RecordInterface& header,
//                                                           LogIO& os,
//                                                           IPosition& shape,
//                                                           Bool dropStokes)
// {
//     CoordinateSystem cSys;
//     Char prefix = 'c';
//     if (!CoordinateSystem::fromFITSHeaderOld (stokesFITSValue, cSys, header, shape, True, prefix)) {
//         os << LogIO::WARN <<
//           "Cannot create the coordinate system from FITS keywords.\n"
//           "I will use a dummy linear coordinate along each axis instead.\n"
//           "If you your FITS file actually does contain a coordinate system\n"
//           "please submit a bug report."  << LogIO::POST;
// //
//         CoordinateSystem cSys2;
//         Vector<String> names(shape.nelements());
//         for (uInt i=0; i<names.nelements(); i++) {
//            ostringstream oss;
//            oss << i;
//            names(i) = String("linear") + String(oss);
//         }
//         CoordinateUtil::addLinearAxes(cSys2, names, shape);
//         cSys = cSys2;
//     }

// // Check shape and CS consistency.  Add dummy axis to shape if possible

//     if (shape.nelements() != cSys.nPixelAxes()) {
//        IPosition shape2;
//        if (cSys.nPixelAxes() > shape.nelements()) {
//           Int nDeg = cSys.nPixelAxes() - shape.nelements();
//           shape2.resize(cSys.nPixelAxes());
//           shape2 = 1;
//           for (uInt i=0; i<shape.nelements(); i++) shape2(i) = shape(i);
//           shape.resize(0);
//           shape = shape2;
// //
//           os << LogIO::WARN << "Image dimension appears to be less than number of pixel axes in CoordinateSystem" << endl;
//           os << "Adding " << nDeg << " degenerate trailing axes" << LogIO::POST;
//        } else {
//           os << "Image contains more dimensions than the CoordinateSystem defines" << LogIO::EXCEPTION;
//        }
//     }

// // Drop Stokes axis IF it's of length 1 AND there is an unoffical
// // pseudo-STokes value (e.g. optical dpeth) on it.  This is stored
// // in ImageInfo instead.

//     Int after = -1;
//     Int c = cSys.findCoordinate(Coordinate::STOKES, after);
//     if (dropStokes && c >= 0 && stokesFITSValue >= 0) {
//        uInt nS = cSys.stokesCoordinate(c).stokes().nelements();
//        if (nS==1) {
//           CoordinateSystem cSys2;
//           for (uInt i=0; i<cSys.nCoordinates(); i++) {
//              if (cSys.type(i) != Coordinate::STOKES) {
//                 cSys2.addCoordinate(cSys.coordinate(i));
//              }
//           }
// //
//           uInt dropAxis = cSys.pixelAxes(c)(0);
//           cSys = cSys2;
//           IPosition shape2(cSys.nPixelAxes());
//           uInt j = 0;
//           for (uInt i=0; i<shape.nelements(); i++) {
//              if (i!=dropAxis) {
//                 shape2(j) = shape(i);
//                 j++;
//              }
//           }
// //
//           shape.resize(0);
//           shape = shape2;
//        }
//     }

// // Remove keywords

//     Vector<String> ignore(14);
//     ignore(0) = "^date-map$";
//     ignore(1) = "^simple$";
//     ignore(2) = "^naxis";
//     ignore(3) = "^projp$";
//     ignore(4) = "^pc$";
//     ignore(5) = "^equinox$";
//     ignore(6) = "^epoch$";
//     ignore(7) = "ctype";
//     ignore(8) = "crpix";
//     ignore(9) = "crval";
//     ignore(10) = "crota";
//     ignore(11) = "cdelt";
//     ignore(12) = "bscale";
//     ignore(13) = "bzero";
//     FITSKeywordUtil::removeKeywords(header, ignore);

// // Remove any ObsInfo keywords

//     FITSKeywordUtil::removeKeywords(header, ObsInfo::keywordNamesFITS());
// //
//     after = -1;
//     if (cSys.findCoordinate(Coordinate::SPECTRAL, after) >= 0) {
//        ignore.resize(1);
//        ignore(0) = "restfreq";
//        FITSKeywordUtil::removeKeywords(header, ignore);
//     }

// // Fix up Direction coordinate so that longitudes is in range [-180,l80]
// // as assumed by wcs.

//     String errMsg;
//     if (!CoordinateUtil::cylindricalFix (cSys, errMsg, shape)) {
//        os << errMsg << LogIO::EXCEPTION;
//     }
// //
//     return cSys;
// }


ImageInfo ImageFITSConverter::getImageInfo (RecordInterface& header)
{
   ImageInfo ii;
   Vector<String> errors;
   Bool ok = ii.fromFITS (errors, header);
   if (!ok) {
      LogIO log(LogOrigin("ImageFITSConverter::getImageInfo", "ImageToFITS", WHERE));
      log << errors << endl;
   }
//
   FITSKeywordUtil::removeKeywords(header, ImageInfo::keywordNamesFITS());
//
   return ii;
}


// ImageInfo ImageFITSConverter::getImageInfoOld (RecordInterface& header)
// {
//    ImageInfo ii;
//    Vector<String> errors;
//    Bool ok = ii.fromFITSOld (errors, header);
//    if (!ok) {
//       LogIO log(LogOrigin("ImageFITSConverter::getImageInfoOld", "ImageToFITS", WHERE));
//       log << errors << endl;
//    }
// //
//    FITSKeywordUtil::removeKeywords(header, ImageInfo::keywordNamesFITS());
// //
//    return ii;
// }


Unit ImageFITSConverter::getBrightnessUnit (RecordInterface& header, LogIO& os)
{
   Unit u;
   if (header.isDefined("bunit")) {
      Record subRec = header.asRecord("bunit");
      if (subRec.dataType("value") == TpString) {
         String unitString;
         subRec.get("value", unitString);
//
         UnitMap::addFITS();
         if (UnitVal::check(unitString)) {
            
// Translate units from FITS units to true aips++ units
// There is no scale factor in this translation.
                
             u = UnitMap::fromFITS(Unit(unitString));
         } else {
	     UnitMap::putUser("\""+unitString+"\"", UnitVal::UnitVal(1.0, UnitDim::Dnon), "\""+unitString+"\"");
	     os << LogIO::WARN << "FITS unit \"" << unitString << "\" unknown to CASA - will treat it as non-dimensional."
		<< LogIO::POST;
	     u.setName("\""+unitString+"\"");
	     u.setValue(UnitVal::UnitVal(1.0, UnitDim::Dnon));
         }
      }
      header.removeField("bunit");
   }       
   return u;
}


// Unit ImageFITSConverter::getBrightnessUnitOld (RecordInterface& header, LogIO& os)
// {
//    Unit u;
//    if (header.isDefined("bunit") && header.dataType("bunit") == TpString) {
//       String unitString;
//       header.get("bunit", unitString);
//       header.removeField("bunit");
//       UnitMap::addFITS();
//       if (UnitVal::check(unitString)) {

// // Translate units from FITS units to true aips++ units
// // There is no scale factor in this translation.

//           u = UnitMap::fromFITS(Unit(unitString));
//       } else {
//           os << "FITS unit " << unitString << " unknown to CASA - ignoring."
//              << LogIO::POST;
//       }
//    }
//    return u;
// }

Bool ImageFITSConverter::extractMiscInfo (RecordInterface& miscInfo, const RecordInterface& header)
//
// The new FITS parsing stuff puts the cards into a Record with structure
//     name
//        "value"
//        "comment"
//
// However the old MiscInfo structure is just a name-value pair
// and the new structure wouldn't reflect through the FITS interface
// So just discard the comment line and preserve the old structure
//
{
   Bool ok = True;
   const uInt n = header.nfields();
   for (uInt i=0; i<n; i++) {
      String name = header.name(i);
      if (header.type(i) == TpRecord) {
         Record subRec  = header.asRecord(i);
         if (subRec.isDefined("value")) {
	    DataType type = subRec.dataType("value");
            if (type==TpString) {
               miscInfo.define(name, subRec.asString("value"));
            } else if (type==TpFloat) {
               miscInfo.define(name, subRec.asFloat("value"));
            } else if (type==TpInt) {
               miscInfo.define(name, subRec.asInt("value"));
            } else if (type==TpBool) {
               miscInfo.define(name, subRec.asBool("value"));
            } else if (type==TpComplex) {
               miscInfo.define(name, subRec.asComplex("value"));
            } else {
               cerr << "Unknown data type " << type << " in parsing MiscInfo remnants" << endl;
            }
         }
      } else {
         cerr << "Unexpected non-record in parsing MiscInfo remnant" << endl;
      }
   }
//
   return ok;
}


void ImageFITSConverter::restoreHistory (LoggerHolder& logger,
                                         ConstFitsKeywordList& kw) 
{
    Vector<String> lines;
    String groupType;
    kw.first();
    uInt n;
    while ((n=FITSHistoryUtil::getHistoryGroup(lines, groupType, kw))!=0) {
       if (groupType == "LOGTABLE") {
          FITSHistoryUtil::fromHISTORY(logger, lines, n, True);
        } else if (groupType == "") {
          FITSHistoryUtil::fromHISTORY(logger, lines, n, False);
        }
    }
}
 

} //# NAMESPACE CASA - END

