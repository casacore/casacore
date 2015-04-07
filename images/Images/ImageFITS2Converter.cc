//# ImageFITS2Converter.cc : non-templated FITS<->Casacore image conversion 
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
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/FITSQualityImage.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/lattices/Lattices/MaskedLatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/FITSTable.h>
#include <casacore/fits/FITS/BinTable.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/FITSDateUtil.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/fits/FITS/FITSHistoryUtil.h>
#include <casacore/coordinates/Coordinates/LinearCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/coordinates/Coordinates/QualityCoordinate.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/ObsInfo.h>

#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Quanta/UnitMap.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Quanta/MVTime.h>

#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/SymLink.h>
#include <casacore/casa/OS/Directory.h>

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/System/ProgressMeter.h>
#include <casacore/casa/version.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

const String ImageFITSConverter::CASAMBM = "casambm";

Bool ImageFITSConverter::FITSToImage(
	ImageInterface<Float> *&newImage, String &error,
	const String &imageName, const String &fitsName,
	uInt whichRep, Int whichHDU, uInt memoryInMB,
	Bool allowOverwrite, Bool zeroBlanks
) {
        LogIO os(LogOrigin("ImageFITSConverter"));

	newImage = 0;
	error = "";
	// First make sure that imageName is writable and does not already
	// exist.  Optionally remove it if it does.  If imageName is empty,
	// great.  That means we are going to make a TempImage

	if (!imageName.empty()) {
		File imfile(imageName);
		if (!ImageFITSConverter::removeFile (error, imfile, imageName, allowOverwrite)) return False;

		Directory imdir = imfile.path().dirName();
		if (!imdir.exists() || !imdir.isWritable()) {
			error = String("Directory ") + imdir.path().originalName() +
					" does not exist or is not writable";
			return False;
		}
	}

	File fitsfile(fitsName);
	if (
		! fitsfile.exists() || !fitsfile.isReadable()
		|| ! fitsfile.isRegular()
	) {
		error = fitsName + " does not exist or is not readable";
		return False;
	}

	// OK, now see if we can attach the FITS reading classes

	FitsInput infile(fitsfile.path().expandedName().chars(), FITS::Disk);

	if (infile.err()) {
		error = String("Cannot open file (or other I/O error): ") + fitsName;
		return False;
	}
	//
	// Advance to the right HDU
	//
	Int theHDU = whichHDU;
	Int numHDU = infile.getnumhdu();
	if(whichHDU<0){
		// look for first readable HDU
		for(Int i=0; i<numHDU; i++){
		  os << LogIO::NORMAL << "Processing HDU " << i << LogIO::POST;
		  if (infile.err() ||
		      infile.rectype() != FITS::HDURecord ||
		      (infile.hdutype() != FITS::PrimaryArrayHDU &&
		       infile.hdutype() != FITS::ImageExtensionHDU)) {
		    infile.skip_hdu();
		  }
		  else{
		    theHDU = i;
		    break;
		  }
		}
		if(theHDU<0){
		  stringstream ss;
		  ss << numHDU;
		  error = "There are " + String(ss.str()) + " HDUs in FITS file " +fitsName + " . None of them is an image.";
		  return False;
		}		  
	}
	else{
	       for (Int i=0; i<whichHDU; i++) {
		 if (infile.skip_hdu() || infile.err()) {
		   error = "Error advancing to image in file: " + fitsName;
		   return False;
		 }
	       }
	       if (infile.rectype() != FITS::HDURecord ||
		   (infile.hdutype() != FITS::PrimaryArrayHDU &&
		    infile.hdutype() != FITS::ImageExtensionHDU)) {
		 error = "No image at specified location in file " + fitsName;
		 return False;
	       }
	}

	// The rest has to be done in a type dependent way - hand over to template
	// functions.
	Bool success=False;
	while(!success && theHDU<numHDU){
	  try{
	    switch(infile.datatype()) {
	    case FITS::BYTE:
	      if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<unsigned char> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<unsigned char> >::FITSToImage(
										  newImage, error, imageName, whichRep,
										  fitsdata, fitsName, TpChar, memoryInMB, zeroBlanks
										  );
	      }
	      else {
		ImageExtension<unsigned char> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<unsigned char> >::FITSToImage(
										    newImage, error, imageName, whichRep,
										    fitsdata, fitsName, TpChar, memoryInMB, zeroBlanks
										    );
	      }
	      success = True;
	      break;
	    case FITS::SHORT:
	      if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<short> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<short> >::FITSToImage(
									  newImage, error, imageName,
									  whichRep, fitsdata, fitsName, TpShort, memoryInMB, zeroBlanks
									  );
	      }
	      else {
		ImageExtension<short> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<short> >::FITSToImage(
									    newImage, error, imageName, whichRep,
									    fitsdata, fitsName, TpShort, memoryInMB, zeroBlanks
									    );
	      }
	      success = True;
	      break;
	    case FITS::LONG:
	      if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<FitsLong> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<FitsLong> >::FITSToImage(
									     newImage, error, imageName, whichRep, fitsdata,
									     fitsName, TpInt, memoryInMB, zeroBlanks
									     );
	      }
	      else {
		ImageExtension<FitsLong> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<FitsLong> >::FITSToImage(
									       newImage, error, imageName, whichRep, fitsdata,
									       fitsName, TpInt, memoryInMB, zeroBlanks
									       );
	      }
	      success = True;
	      break;
	    case FITS::FLOAT: 
	      if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<Float> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<Float> >::FITSToImage(
									  newImage, error, imageName, whichRep,
									  fitsdata, fitsName, TpFloat, memoryInMB, zeroBlanks
									  );
	      }
	      else {
		ImageExtension<Float> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<Float> >::FITSToImage(
									    newImage, error, imageName, whichRep,
									    fitsdata, fitsName, TpFloat, memoryInMB, zeroBlanks
									    );
	      }
	      success = True;
	      break;
	    case FITS::DOUBLE: 
	      if (infile.hdutype() == FITS::PrimaryArrayHDU) {
		PrimaryArray<Double> fitsdata(infile);
		ImageFITSConverterImpl<PrimaryArray<Double> >::FITSToImage(
									   newImage, error, imageName, whichRep,
									   fitsdata, fitsName, TpDouble,  memoryInMB, zeroBlanks
									   );
	      }
	      else {
		ImageExtension<Double> fitsdata(infile);
		ImageFITSConverterImpl<ImageExtension<Double> >::FITSToImage(
									     newImage, error, imageName, whichRep,
									     fitsdata, fitsName, TpDouble, memoryInMB, zeroBlanks);
	      }
	      success = True;
	      break;
	    default:
	      if(whichHDU>=0){
		error = "Unknown datatype  - no data returned";
		return False;
	      }
	      success = False;
	    }
	  }
	  catch(const AipsError& x){
	    if(whichHDU>=0){
	      throw(x);
	    }
	    else{
	      success = False;
	    }
	  }
	  if(whichHDU>=0 || success){
	    break;
	  }
	  else if(!success){
	    // skip to next useful HDU 
	    while(theHDU<numHDU){
	      os << LogIO::WARN << "This HDU (" << theHDU << ") did not contain a legible image." << LogIO::POST;  
	      theHDU++;
              while (theHDU<numHDU && (infile.err() ||
                                       infile.rectype() != FITS::HDURecord ||
                                       (infile.hdutype() != FITS::PrimaryArrayHDU &&
                                        infile.hdutype() != FITS::ImageExtensionHDU))
                   ) {
                infile.skip_hdu();
                theHDU++;
              }
              os << LogIO::WARN << "Next candidate image HDU is #" << theHDU 
                   << "\n (note: use the whichhdu parameter to address HDUs directly)" << LogIO::POST;  
              break;
	    }
	  }
	}
	if(!success){
	  stringstream ss;
	  ss << numHDU;
	  error = "There are " + String(ss.str()) + " HDUs in FITS file " +fitsName + " . None of them is a legible image.";
	  return False;
	}
	return True;
}

Bool ImageFITSConverter::ImageToFITS(
	String &error, ImageInterface<Float>& image,
	const String &fitsName, uInt memoryInMB,
	Bool preferVelocity, Bool opticalVelocity,
	Int BITPIX, Float minPix, Float maxPix,
	Bool allowOverwrite, Bool degenerateLast,
	Bool verbose, Bool stokesLast,
	Bool preferWavelength, Bool airWavelength,
	const String& origin,
	Bool history
) {
	LogIO os;
	os << LogOrigin("ImageFitsConverter", __FUNCTION__, WHERE);
	//
	error = "";
	FitsOutput *outfile=0;
	// create the FITS output
	if (!ImageFITSConverter::openFitsOutput(error, outfile, fitsName, allowOverwrite)){
		return False;
	}
	// get the coo-sys and check for a quality axis
	CoordinateSystem cSys= image.coordinates();
	if (cSys.hasQualityAxis()){
		// put the image to the FITSOut
		if (!ImageFITSConverter::QualImgToFITSOut(error, os, image, outfile, memoryInMB,
				preferVelocity, opticalVelocity, BITPIX, minPix, maxPix, degenerateLast,
				verbose, stokesLast, preferWavelength, airWavelength, origin, history)){
			return False;
		}
	}
	else{
		// put the image to the FITSOut
		if (!ImageFITSConverter::ImageToFITSOut(error, os, image, outfile, memoryInMB,
				preferVelocity, opticalVelocity, BITPIX, minPix, maxPix, degenerateLast,
				verbose, stokesLast, preferWavelength, airWavelength, True, True, origin, history)){
			return False;
		}
	}
	if (outfile) {
		delete outfile;
	}
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
    if (!CoordinateSystem::fromFITSHeader(stokesFITSValue, cSys, headerRec, header, 
                                          shape, whichRep)) {
        os << LogIO::WARN << "No proper coordinate system defined in FITS file. Using dummy linear system instead." << LogIO::POST;

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
          os << LogIO::NORMAL << "Image dimension appears to be less than number of pixel axes in CoordinateSystem" << endl;
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

    Vector<String> ignore(6);
    ignore(0) = "^date-map$";
    ignore(1) = "date";
    ignore(2) = "^naxis";
    ignore(3) = "^naxis$";
    ignore(4) = "^pc.....";
    ignore(5) = "^pc......";
    FITSKeywordUtil::removeKeywords(headerRec, ignore);
//
    return cSys;
}



ImageInfo ImageFITSConverter::getImageInfo (
	RecordInterface& header
) {
   ImageInfo ii;
   Vector<String> errors;
   Bool ok = ii.fromFITS (errors, header);
   if (!ok) {
      LogIO log(LogOrigin("ImageFITSConverter::getImageInfo", "ImageToFITS", WHERE));
      log << errors << endl;
   }
   FITSKeywordUtil::removeKeywords(header, ImageInfo::keywordNamesFITS());
   return ii;
}

void ImageFITSConverter::readBeamsTable(
		ImageInfo& info, const String& filename, const DataType type
) {
	FitsInput input(File(filename).path().expandedName().chars(), FITS::Disk);
	switch (type) {
	// advance to correct location in the input
	case TpFloat: {
		PrimaryArray<Float> fitsImage(input);
		break;
	}
	case TpDouble: {
		PrimaryArray<Double> fitsImage(input);
		break;
	}
	case TpInt: {
		PrimaryArray<Int> fitsImage(input);
		break;
	}
	case TpShort: {
		PrimaryArray<Short> fitsImage(input);
		break;
	}
	default: {
		throw AipsError("Unhandled FITS type");
		break;
	}
	}
	Table beamTable;
	while (input.rectype() != FITS::EndOfFile && !input.err()) {
		if (input.hdutype() != FITS::BinaryTableHDU) {
			input.skip_hdu();
		}
		else {
			BinaryTable binTab(input);
			String type = binTab.extname();
			if (type.contains("BEAMS")) {
				beamTable = binTab.fullTable();
				break;
			}
		}
	}
	if (beamTable.nrow() == 0) {
		// no beam table found
		return;
	}
	LogIO os;
	os << LogOrigin("ImageFITSConverter", __FUNCTION__)
				<< LogIO::NORMAL << "Loading multiple beams from BEAMS table"
				<< LogIO::POST;
	uInt nChan = beamTable.keywordSet().asuInt("NCHAN");
	uInt nPol  = beamTable.keywordSet().asuInt("NPOL");

	info.setAllBeams(nChan, nPol, GaussianBeam::NULL_BEAM);
	ScalarColumn<Float> bmaj(beamTable, "BMAJ");
	ScalarColumn<Float> bmin(beamTable, "BMIN");
	ScalarColumn<Float> bpa(beamTable, "BPA");
	ScalarColumn<Int> chan(beamTable, "CHAN");
	ScalarColumn<Int> pol(beamTable, "POL");

	String bmajUnit = bmaj.keywordSet().asString("TUNIT");
	String bminUnit = bmin.keywordSet().asString("TUNIT");
	String bpaUnit = bpa.keywordSet().asString("TUNIT");
	GaussianBeam beam;
	Quantity xmaj(0, bmajUnit);
	Quantity xmin(0, bminUnit);
	Quantity xpa(0, bpaUnit);
	for (uInt i=0; i<beamTable.nrow(); i++) {
		xmaj.setValue(bmaj(i));
		xmin.setValue(bmin(i));
		xpa.setValue(bpa(i));
		beam.setMajorMinor(xmaj, xmin);
		beam.setPA(xpa);
		info.setBeam(chan(i), pol(i), beam);
	}
}

Unit ImageFITSConverter::getBrightnessUnit (RecordInterface& header, LogIO& os)
{
   Unit u;
   if (header.isDefined("bunit")) {
      Record subRec = header.asRecord("bunit");
      if (subRec.dataType("value") == TpString) {
         String unitString;
         subRec.get("value", unitString);

         UnitMap::addFITS();
         if (UnitVal::check(unitString)) {
	     // Translate units from FITS units to true Casacore units
	     // There is no scale factor in this translation.
             u = UnitMap::fromFITS(Unit(unitString));
         } 
	 else { // unit check failed
	     Bool uFixed = False;
	     // try to recover by removing bracketed comments like in "K (Tb)"
	     String::size_type bracketPos;
	     String truncUnitString(unitString);
	     if((bracketPos = unitString.find("("))!=String::npos 
		|| (bracketPos = unitString.find("["))!=String::npos){
		 // remove everything beginning at bracketPos from the string
		 truncUnitString = unitString.substr(0, bracketPos);
		 os << LogIO::WARN << "FITS unit \"" << unitString << "\" unknown to CASA, was truncated to \""
		    << truncUnitString << "\"."
		    << LogIO::POST;
		 if (UnitVal::check(truncUnitString)) {
		     u = UnitMap::fromFITS(Unit(truncUnitString));
		     uFixed = True;
		 }
	     }
	     if(!uFixed){ // try adding the most common problematic units occuring in BUNIT
		 UnitMap::putUser("Pixel",UnitVal(1.0),"Pixel unit");
		 UnitMap::putUser("Beam",UnitVal(1.0),"Beam area");
		 if (UnitVal::check(truncUnitString)) {
		     uFixed = True;
		     u = UnitMap::fromFITS(Unit(truncUnitString));
		     os << LogIO::NORMAL << "FITS unit \"" << truncUnitString << "\" does not conform to the FITS standard."
			<< endl << "Correct units are always lower case except when derived from a name." 
			<< endl << "Please use \"beam\" instead of \"Beam\", \"pixel\" instead of \"Pixel\"." 
			<< LogIO::POST;
		 }
	     }

	     if(!uFixed){ // recovery attempt failed as well
		 UnitMap::putUser("\""+unitString+"\"",
                                  UnitVal(1.0, UnitDim::Dnon),
                                  "\""+unitString+"\"");
		 os << LogIO::WARN << "FITS unit \"" << unitString
                    << "\" unknown to CASA - will treat it as non-dimensional." << endl
		    << " See http://fits.gsfc.nasa.gov/fits_standard.html for a list of valid units."
		    << LogIO::POST;
		 u.setName("\""+unitString+"\"");
		 u.setValue(UnitVal(1.0, UnitDim::Dnon));
	     }
	 }
      }
      header.removeField("bunit");
   }       
   return u;
}



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
            } else if (type==TpDouble) {
               miscInfo.define(name, subRec.asDouble("value"));
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

Bool ImageFITSConverter::removeFile (String& error, const File& outFile,
                                     const String& outName, Bool allowOverwrite)
{
    String basename = outFile.path().baseName();
    if (basename.empty() || basename == "." || basename == "..") {
	throw AipsError(
	    "Invalid file path " + outFile.path().absoluteName() + ". You really don't want me to delete the directory you're in."
	    );
    }
    if (outFile.exists()) {
	if (allowOverwrite) {
	    String msg;
	    try {
		if (outFile.isSymLink()) {
		    SymLink sfile(outFile);
		    sfile.remove();
		} else if (outFile.isRegular()) {
		    RegularFile rfile(outFile);
		    rfile.remove();
		} else if (outFile.isDirectory()) {
		    Directory dfile(outFile);
		    dfile.removeRecursive();
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

Bool ImageFITSConverter::ImageToFITSOut(
	String &error, LogIO &os, const ImageInterface<Float>& image,
	FitsOutput *outfile, uInt memoryInMB, Bool preferVelocity,
	Bool opticalVelocity, Int BITPIX, Float minPix, Float maxPix,
	Bool degenerateLast, Bool verbose, Bool stokesLast,
	Bool preferWavelength, Bool airWavelength, Bool primHead,
	Bool allowAppend, const String& origin, Bool history
) {
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
	// and make Stokes the very last if requested
	//
	IPosition shape = image.shape();
	IPosition newShape = shape;
	const uInt ndim = shape.nelements();
	IPosition cursorOrder(ndim); // to be used later in the actual data copying
	for (uInt i=0; i<ndim; i++) {
		cursorOrder(i) = i;
	}
	Bool needNonOptimalCursor = False; // the default value for the case no axis reordering is necessary
	if(stokesLast || degenerateLast){
		Vector<Int> order(ndim);
		Vector<String> cNames = cSys.worldAxisNames();
		uInt nStokes = 0; // number of stokes axes
		if(stokesLast){
			for (uInt i=0; i<ndim; i++) { // loop over axes
				order(i) = i;
				newShape(i) = shape(i);
			}
			for (uInt i=0; i<ndim; i++) { // loop over axes
				if (cNames(i) == "Stokes") { // swap to back
					nStokes++;
					order(ndim-nStokes) = i;
					newShape(ndim-nStokes) = shape(i);
					order(i) = ndim-nStokes;
					newShape(i) = shape(ndim-nStokes);
				}
			}
		}
		if(nStokes>0){ // apply the stokes reordering
			cSys.transpose(order,order);
		}
		if (degenerateLast) {
			// make sure the stokes axes stay where they are now
			for (uInt i=ndim-nStokes; i<ndim; i++) {
				order(i) = i;
			}
			uInt j = 0;
			for (uInt i=0; i<ndim-nStokes; i++) { // loop over axes
				if (shape(i)>1) { // axis is not degenerate
					order(j) = i; // put it in front, keeping order
					newShape(j) = shape(i);
					j++;
				}
			}
			for (uInt i=0; i<ndim-nStokes; i++) { // loop over axes again
				if (shape(i)==1) { // axis is degenerate
					order(j) = i;
					newShape(j) = shape(i);
					j++;
				}
			}
			cSys.transpose(order,order); // apply the degenerate reordering
		}
		for (uInt i=0; i<ndim; i++) {
			cursorOrder(i) = order(i);
			if(order(i)!=(Int)i){
				needNonOptimalCursor=True;
			}
		}

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
			RO_MaskedLatticeIterator<Float> iter =
					RO_MaskedLatticeIterator<Float>(image,
							LatticeStepper(shape,
									cursorShape,
									LatticeStepper::RESIZE));
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

		if (::casacore::near(minPix, maxPix)) {
			if (::casacore::near(Float(0.0), maxPix)) {
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
	if (allowAppend)
		header.define("extend", True);
	if (!primHead){
		header.define("PCOUNT", 0);
		header.define("GCOUNT", 1);
	}
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
	if (!ii.toFITS (error, header)) {
		return False;
	}
	//
	header.define("COMMENT1", ""); // inserts spaces
	// I should FITS-ize the units

	header.define("BUNIT", image.units().getName().chars());
	header.setComment("BUNIT", "Brightness (pixel) unit");
	//
	IPosition shapeCopy = newShape;
	Record saveHeader(header);
	Bool ok = cSys.toFITSHeader(header, shapeCopy, True, 'c', True, // use WCS
			preferVelocity, opticalVelocity,
			preferWavelength, airWavelength);
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
			os  << LogIO::NORMAL << "Truncating miscinfo field " << tmp0
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
				case TpArrayChar:
				case TpArrayUShort:
				case TpArrayInt:
				case TpArrayUInt:
				case TpArrayInt64:
					header.define(miscname, image.miscInfo().toArrayInt(i));
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
	if (origin.empty()) {
		header.define("ORIGIN", "casacore-" + getVersion());
	} else {
		header.define("ORIGIN", origin);
	}

	// Set up the FITS header
	FitsKeywordList kw;
	kw = FITSKeywordUtil::makeKeywordList(primHead, True);
	if (ii.hasMultipleBeams()) {
		header.define(CASAMBM, True);
		header.setComment(CASAMBM, "CASA multiple BEAMS table present");

	}
	//kw.mk(FITS::EXTEND, True, "Tables may follow");
	// add the general keywords for WCS and so on
	ok = FITSKeywordUtil::addKeywords(kw, header);
	if (! ok) {
		error = "Error creating initial FITS header";
		return False;
	}

	if(history){
	  //
	  // HISTORY
	  //
	  const LoggerHolder& logger = image.logger();
	  //
	  vector<String> historyChunk;
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
	}
	//
	// END
	//
	kw.end();
	//
	// Finally get around to copying the data
	//
	String report;
	IPosition newCursorShape = ImageFITSConverter::copyCursorShape(report,
			shape,
			sizeof(Float),
			sizeof(Float),
			memoryInMB);
	if(needNonOptimalCursor && newShape.nelements()>0){
		// use cursor the size of one image row in order to enable axis re-ordering
		newCursorShape.resize(1);
		newCursorShape=newShape(0);
	}

	if (verbose) {
		os << "Copying '" << image.name() << "' to file "
				<< report << LogIO::POST;
	}

	//
	// If this fails, more development is needed
	//
	AlwaysAssert(sizeof(Float) == sizeof(float), AipsError);
	AlwaysAssert(sizeof(Short) == sizeof(short), AipsError);
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

		PrimaryArray<Float>* fits32 = 0;
		PrimaryArray<Short>* fits16 = 0;

		if (BITPIX == -32) {
			if (primHead) {
				fits32 = new PrimaryArray<Float>(kw);
			}
			else {
				fits32 = new ImageExtension<Float>(kw);
			}
			if (fits32==0 || fits32->err()) {
				error = "Error creating FITS file from keywords";
				return False;
			}
			if (fits32->write_hdr(*outfile)) {
				error = "Error writing FITS header";
				delete outfile;
				return False;
			}
		}
		else if (BITPIX == 16) {
			if (primHead) {
				fits16 = new PrimaryArray<Short>(kw);
			}
			else {
				fits16 = new ImageExtension<Short>(kw);
			}
			if (fits16==0 || fits16->err()) {
				error = "Error creating FITS file from keywords";
				return False;
			}
			if (fits16->write_hdr(*outfile)) {
				delete outfile;
				error = "Error writing FITS header";
				return False;
			}
		}
		else {
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
						}
						else {
							ptr2[j] = ptr[j];
							setNaN(ptr2[j]);
						}
					}
					fits32->store(ptr2, bufferSize);
					delete [] ptr2;
				}
				else {
					fits32->store(ptr, bufferSize);
				}
				Int hduErr = 0;
				if (!(hduErr = fits32->err())){
					n = fits32->write(*outfile);
					if (n != bufferSize) {
						delete outfile;
						error = "Write failed (full disk or tape?)";
						return False;
					}
				} else {
					error = "ImageFITS2Converter: Storing FITS primary Float array failed with HDU error code "
							+ String::toString(hduErr);
					return False;
				}
			}
			else if (fits16) {
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
				}
				else {
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
				Int hduErr = 0;
				if (!(hduErr = fits16->err())) {
					n = fits16->write(*outfile);
					if (n != bufferSize) {
						delete outfile;
						error = "Write failed (full disk or tape?";
						return False;
					}
				}
				else {
					error = "ImageFITS2Converter: Storing FITS primary Short array failed with HDU error code "
							+ String::toString(hduErr);
					return False;
				}
			}
			else {
				AlwaysAssert(0, AipsError); // NOTREACHED
			}
			//
			cursor.freeStorage(ptr, deletePtr);
			if (applyMask) pMask->freeStorage(maskPtr, deleteMaskPtr);
			//
			if ((fits32 && fits32->err()) ||
					(fits16 && fits16->err()) ||
					outfile->err()) {
				error = String("Error writing into file!");
				delete outfile;
				return False;
			}
			count++;
			if (verbose) pMeter->update(count*curpixels);
		}
		if (fits32) {
			delete fits32; fits32 = 0;
		}
		else if (fits16) {
			delete fits16; fits16 = 0;
			delete buffer16; buffer16 = 0;
		}
		else {
			AlwaysAssert(0, AipsError); // NOTREACHED
		}
		//
		if (pMeter) delete pMeter;
		if (pMask!=0) delete pMask;
	}
	catch (const AipsError& x) {
		error = "Unknown error copying image to FITS file";
		if (outfile) {
			delete outfile;
		}
		return False;
	}
	if (ii.hasMultipleBeams()) {
		_writeBeamsTable(outfile, ii);
	}

	return True;
}

void ImageFITSConverter::_writeBeamsTable(
        FitsOutput *const &outfile, const ImageInfo& info
) {
	// write multiple beams to a table
	RecordDesc desc;
	Record stringLengths; // no strings
	Record units;
	GaussianBeam beam = *info.getBeamSet().getBeams().begin();
	desc.addField("BMAJ", TpFloat);
	units.define("BMAJ", beam.getMajor().getUnit());
	desc.addField("BMIN", TpFloat);
	units.define("BMIN", beam.getMinor().getUnit());
	desc.addField("BPA", TpFloat);
	units.define("BPA", beam.getPA(True).getUnit());
	desc.addField("CHAN", TpInt);
	desc.addField("POL", TpInt);
	Record extraKeywords;
	extraKeywords.define("EXTNAME", "BEAMS");
	extraKeywords.define("EXTVER", 1);
	extraKeywords.define("XTENSION", "BINTABLE");
	extraKeywords.setComment("XTENSION", "Binary extension");
	extraKeywords.define("NCHAN", (Int)info.getBeamSet().nchan());
	extraKeywords.define("NPOL", (Int)info.getBeamSet().nstokes());
	FITSTableWriter writer(
		outfile, desc, stringLengths, info.getBeamSet().nelements(),
		extraKeywords, units, False
	);
	RecordFieldPtr<Float> bmaj(writer.row(), "BMAJ");
	RecordFieldPtr<Float> bmin(writer.row(), "BMIN");
	RecordFieldPtr<Float> bpa(writer.row(), "BPA");
	RecordFieldPtr<Int> chan(writer.row(), "CHAN");
	RecordFieldPtr<Int> pol(writer.row(), "POL");
	const ImageBeamSet& beamSet = info.getBeamSet();
	IPosition axisPath(2, 0, 1);
        ArrayPositionIterator iter(beamSet.shape(), axisPath, False);
        while (! iter.pastEnd()) {
          const IPosition& pos = iter.pos();
          GaussianBeam beam = beamSet(pos[0], pos[1]);
          *chan = pos[0];
          *pol = pos[1];
          *bmaj = beam.getMajor().getValue();
          *bmin = beam.getMinor().getValue();
          *bpa = beam.getPA("deg", True);
          writer.write();
          iter.next();
	}
}

Bool ImageFITSConverter::QualImgToFITSOut(String &error,
		LogIO &os,
		ImageInterface<Float> &image,
		FitsOutput *outfile,
		uInt memoryInMB,
		Bool preferVelocity,
		Bool opticalVelocity,
		Int BITPIX, Float minPix, Float maxPix,
		Bool degenerateLast,
		Bool verbose, Bool stokesLast,
		Bool preferWavelength,
		Bool airWavelength,
		const String& origin,
		Bool history)
{
	// check whether the image is a generic FITS image
   FITSQualityImage *fitsQI=dynamic_cast<FITSQualityImage *>(&image);
   if (fitsQI){

   	// Background: When writing to FITS and image that was generated
   	//             from a FITS image, it makes more sense to load in
   	//             and write out the data and error extension directly.
   	//             This avoids that meta-data get screwed when read into
   	//             a CASA image. Doing so, there is e.g. no need to take care
   	//             for the header keywords declaring the data and error
   	//             extension and so on, since they exist properly in the
   	//             respective FITS extensions.

   	// load the data extension
   	FITSImage *fitsImg = new FITSImage(fitsQI->name(False), 0, fitsQI->whichDataHDU());

   	// put the data extension to FITSOut
   	if (!ImageFITSConverter::ImageToFITSOut(error, os, *fitsImg, outfile, memoryInMB,
   			preferVelocity, opticalVelocity, BITPIX, minPix, maxPix, degenerateLast,
			verbose, stokesLast, preferWavelength, airWavelength, True, True, origin, history)){
   		if (fitsImg)
   			delete fitsImg;
   		return False;
   	}
		delete fitsImg;
		fitsImg=0;

   	// load the error extension
   	fitsImg = new FITSImage(fitsQI->name(False), 0, fitsQI->whichErrorHDU());

   	// put the error extension  to the FITSOut
   	if (!ImageFITSConverter::ImageToFITSOut(error, os, *fitsImg, outfile, memoryInMB,
   			preferVelocity, opticalVelocity, BITPIX, minPix, maxPix, degenerateLast,
			verbose, stokesLast, preferWavelength, airWavelength, False, False, origin, history)){
   		if (fitsImg)
   			delete fitsImg;
   		return False;
   	}
		delete fitsImg;

   }
   else {
	   TableRecord dataExtMiscInfo;
   	TableRecord errorExtMiscInfo;

   	// get the metadata (extension names etc.) for the data and the error
   	if (!FITSQualityImage::qualFITSInfo(error, dataExtMiscInfo, errorExtMiscInfo, image.miscInfo())){
   		return False;
   	}

   	// find the quality axis
   	CoordinateSystem cSys = image.coordinates();
		Int qualAx = cSys.findCoordinate(Coordinate::QUALITY);
		Vector<Int> nPixelQual = cSys.pixelAxes(qualAx);
		uInt nAxisQual=nPixelQual(0);

		// build a slicer for the data
		Int qualIndex;
		if (!(cSys.qualityCoordinate(qualAx)).toPixel(qualIndex, Quality::DATA)){
			error = "Could not locate DATA index in quality coordinate!";
			return False;
		}
		IPosition startPos(image.ndim(), 0);
		IPosition lengthPos=image.shape();
		startPos(nAxisQual)  = qualIndex;
		lengthPos(nAxisQual) = 1;
		Slicer subSlicer(startPos, lengthPos, Slicer::endIsLength);

		// create the data sub-image and set the metadata
		SubImage<Float> *subData = new SubImage<Float>(image, subSlicer, AxesSpecifier(False));
		subData->setMiscInfo(dataExtMiscInfo);

   	// put the data sub-image to FITSOut
		if (!ImageFITSConverter::ImageToFITSOut(error, os, *subData, outfile, memoryInMB,
   			preferVelocity, opticalVelocity, BITPIX, minPix, maxPix, degenerateLast,
			verbose, stokesLast, preferWavelength, airWavelength, True, True, origin, history)){
   		if (subData)
   			delete subData;
   		return False;
   	}
	   delete subData;

		// build the error slicer
		if (!(cSys.qualityCoordinate(qualAx)).toPixel(qualIndex, Quality::ERROR)){
			error = "Could not locate ERROR index in quality coordinate!";
			return False;
		}
	   startPos(nAxisQual)=qualIndex;
	   subSlicer=Slicer(startPos, lengthPos, Slicer::endIsLength);

		// create the error sub-image and set the metadata
	   SubImage<Float> *subError = new SubImage<Float>(image, subSlicer, AxesSpecifier(False));
	   subError->setMiscInfo(errorExtMiscInfo);

   	// put the error sub-image to FITSOut
	   if (!ImageFITSConverter::ImageToFITSOut(error, os, *subError, outfile, memoryInMB,
	   		preferVelocity, opticalVelocity, BITPIX, minPix, maxPix, degenerateLast,
			verbose, stokesLast, preferWavelength, airWavelength, False, False, origin, history)){
	   	if (subError)
	   		delete subError;
	   	return False;
	   }
	   delete subError;
   }
	return True;
}

Bool ImageFITSConverter::openFitsOutput(String &error, FitsOutput *(&fitsOut),
		                                  const String &fitsName, const Bool &allowOverwrite){
	if (fitsName == "-") {
		// Write to stdout
		fitsOut = new FitsOutput();
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
		fitsOut = new FitsOutput(fitsfile.path().expandedName().chars(), FITS::Disk);
	}
	//
	if (fitsOut == 0 || fitsOut->err()) {
		error = String("Cannot open file for writing: ") + fitsName;
		if (fitsOut != 0) {
			delete fitsOut;
                        fitsOut = 0;
		}
		return False;
	}
	return True;
}
} //# NAMESPACE CASACORE - END

