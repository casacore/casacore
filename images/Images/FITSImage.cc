//# FITSImage.cc: Class providing native access to FITS images
//# Copyright (C) 2001,2002
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

#include <casacore/images/Images/FITSImage.h>

#include <casacore/images/Images/FITSImgParser.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/images/Images/ImageInfo.h>
#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/images/Images/MaskSpecifier.h>
#include <casacore/images/Images/ImageOpener.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/LRegions/FITSMask.h>
#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/tables/LogTables/LoggerHolder.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

FITSImage::FITSImage (const String& name, uInt whichRep, uInt whichHDU)
: ImageInterface<Float>(),
  name_p      (name),
  fullname_p  (name),
  pPixelMask_p(0),
  scale_p     (1.0),
  offset_p    (0.0),
  shortMagic_p (0),
  uCharMagic_p (0),
  longMagic_p (0),
  hasBlanks_p (False),
  dataType_p  (TpOther),
  fileOffset_p(0),
  isClosed_p  (True),
  filterZeroMask_p(False),
  whichRep_p(whichRep),
  whichHDU_p(whichHDU),
  _hasBeamsTable(False)
{
   setup();
}

FITSImage::FITSImage (const String& name, const MaskSpecifier& maskSpec, uInt whichRep, uInt whichHDU)
: ImageInterface<Float>(),
  name_p      (name),
  fullname_p  (name),
  maskSpec_p  (maskSpec),
  pPixelMask_p(0),
  scale_p     (1.0),
  offset_p    (0.0),
  shortMagic_p (0),
  uCharMagic_p (0),
  longMagic_p (0),
  hasBlanks_p (False),
  dataType_p  (TpOther),
  fileOffset_p(0),
  isClosed_p  (True),
  filterZeroMask_p(False),
  whichRep_p(whichRep),
  whichHDU_p(whichHDU),
  _hasBeamsTable(False)
{
   setup();
}

FITSImage::FITSImage (const FITSImage& other)
: ImageInterface<Float>(other),
  name_p      (other.name_p),
  fullname_p  (other.fullname_p),
  maskSpec_p  (other.maskSpec_p),
  pTiledFile_p(other.pTiledFile_p),
  pPixelMask_p(0),
  shape_p     (other.shape_p),
  scale_p     (other.scale_p),
  offset_p    (other.offset_p),
  shortMagic_p (other.shortMagic_p),
  uCharMagic_p (other.uCharMagic_p),
  longMagic_p (other.longMagic_p),
  hasBlanks_p (other.hasBlanks_p),
  dataType_p  (other.dataType_p),
  fileOffset_p(other.fileOffset_p),
  isClosed_p  (other.isClosed_p),
  filterZeroMask_p(other.filterZeroMask_p),
  whichRep_p(other.whichRep_p),
  whichHDU_p(other.whichHDU_p),
  _hasBeamsTable(other._hasBeamsTable)

{
   if (other.pPixelMask_p != 0) {
      pPixelMask_p = other.pPixelMask_p->clone();
   }
}
 
FITSImage& FITSImage::operator=(const FITSImage& other)
// 
// Assignment. Uses reference semantics
//
{
   if (this != &other) {
      ImageInterface<Float>::operator= (other);
//
      pTiledFile_p = other.pTiledFile_p;             // Counted pointer
//
      delete pPixelMask_p;
      pPixelMask_p = 0;
      if (other.pPixelMask_p != 0) {
	 pPixelMask_p = other.pPixelMask_p->clone();
      }
//
      shape_p     = other.shape_p;
      name_p      = other.name_p;
      fullname_p  = other.fullname_p;
      maskSpec_p  = other.maskSpec_p;
      scale_p     = other.scale_p;
      offset_p    = other.offset_p;
      shortMagic_p = other.shortMagic_p;
      uCharMagic_p = other.uCharMagic_p;
      longMagic_p = other.longMagic_p;
      hasBlanks_p = other.hasBlanks_p;
      dataType_p  = other.dataType_p;
      fileOffset_p= other.fileOffset_p;
      isClosed_p  = other.isClosed_p;
      filterZeroMask_p = other.filterZeroMask_p;
      whichRep_p = other.whichRep_p;
      whichHDU_p = other.whichHDU_p;
      _hasBeamsTable = other._hasBeamsTable;
   }
   return *this;
} 
 
FITSImage::~FITSImage()
{
   delete pPixelMask_p;
}


LatticeBase* FITSImage::openFITSImage (const String& name,
				       const MaskSpecifier& spec)
{
  return new FITSImage (name, spec);
}

void FITSImage::registerOpenFunction()
{
  ImageOpener::registerOpenImageFunction (ImageOpener::FITS,
					  &openFITSImage);
}

//
String FITSImage::get_fitsname(const String &fullname)
{
	String fullname_l;
	String fitsname;
	Int close_bracepos, open_bracepos, fullname_length;

	fullname_l = fullname;
	fullname_l.trim();
	fullname_length = fullname_l.length();

	//cerr << "Initial name: " << fullname_l << endl;
	// check whether the strings ends with "]"
	if (fullname_l.compare(fullname_length-1, 1, "]", 1))
	{
		// check for an open brace
		open_bracepos = fullname_l.rfind("[", fullname_length);
		if (open_bracepos > 0) {

			// check for a closing brace
			close_bracepos = fullname_l.rfind("]", fullname_length);

			// an open brace at the end indicates a mal-formed name
			if (close_bracepos < 0 || (open_bracepos > close_bracepos))
				throw (AipsError(fullname_l + " has opening brace, but no closing brace."));
		}

		// just copy the input
		fitsname = fullname_l;
	}
	else
	{
		// check for the last "["
		open_bracepos = fullname_l.rfind("[", fullname_length);
		if (open_bracepos < 0) {
			throw (AipsError(fullname_l + " has closing brace, but no opening brace."));
		}
		else {
			// separate the filename an the extension name
			//extexpr_p = String(fullname_l, open_bracepos+1, fullname_length-open_bracepos-2);
			fitsname =String(fullname_l, 0, open_bracepos);
		}
	}
	//cerr << "FITS name: " << fitsname <<endl;

	return fitsname;
}

//
uInt FITSImage::get_hdunum(const String &fullname)
{
	String extname=String("");

	String fullname_l;
	String fitsname;
	String extstring;
	Int fullname_length, comma_pos;

	Int  extver=-1;
	Int  extindex=-1;
	Int fitsindex=-1;
	uInt hduindex=0;

	fullname_l = fullname;
	fullname_l.trim();
	fullname_length = fullname_l.length();

	// determine the FITS name
	fitsname = FITSImage::get_fitsname(fullname_l);

	// check whether there is an extension
	// specification in the full name
	if (fitsname != fullname_l) {

		// isolate the extension specification
		extstring = String(fullname_l, fitsname.length()+1, fullname_length-fitsname.length()-2);

		// check for the comma
		comma_pos = extstring.rfind(",", extstring.length());
		if (comma_pos < 0) {
			extstring.trim();

			// check whether an index is given
			if (String::toInt(extstring)){
				// get the index
				extindex = String::toInt(extstring);
			}
			// explicitly check for the literal "0"
			else if (!extstring.compare(0, 1, "0", 1)){
				extindex = 0;
			}
			else {
				// just copy the extension name
				extname = extstring;
			}
		}
		else {
			// separate the extension name
			extname = String(extstring, 0, comma_pos);

			// find the extension version
			extver = String::toInt(String(extstring, comma_pos+1, extstring.length()-1));

			if (!extver){
				throw (AipsError(String(extstring, comma_pos+1, extstring.length()-1) + " Extension version not an integer"));
				//cerr << "Extension version not an Integer: " << String(extstring, comma_pos+1, extstring.length()-1)<< endl;
				//exit(0);
			}
			else if (extver < 0) {
				throw (AipsError(extstring + " Extension version must be >0."));
				//cerr << "Extension version must be >0: " << extver << endl;
				//exit(0);
			}

		}
		// make it pretty
		extname.trim();
		extname.upcase();
	}

	//cerr << "Opening image parser with: "<< fitsname <<endl;
	FITSImgParser fip = FITSImgParser(fitsname);

	if (extname.length() > 0 || extindex > -1) {
		FITSExtInfo   fei = FITSExtInfo(fip.fitsname(True), extindex, extname, extver, True);
		fitsindex = fip.get_index(fei);
		if (fitsindex > -1)
			hduindex = (uInt)fitsindex;
		else
			throw (AipsError("Extension " + extstring + " does not exist in " + fitsname));
	}
	else {
		hduindex = fip.get_firstdata_index();
		if (hduindex > 1 || hduindex == fip.get_numhdu())
			throw (AipsError("No data in the zeroth or first extension of " + fitsname));
	}

	// return the index
	return hduindex;
}

ImageInterface<Float>* FITSImage::cloneII() const
{
   return new FITSImage (*this);
}


String FITSImage::imageType() const
{
   return "FITSImage";
}

Bool FITSImage::isMasked() const
{
   return hasBlanks_p;
}

const LatticeRegion* FITSImage::getRegionPtr() const
{
   return 0;
}

IPosition FITSImage::shape() const  
{
   return shape_p.shape();
}

uInt FITSImage::advisedMaxPixels() const
{
   return shape_p.tileShape().product();
}

IPosition FITSImage::doNiceCursorShape (uInt) const  
{
   return shape_p.tileShape();
}

void FITSImage::resize(const TiledShape&)
{
   throw (AipsError ("FITSImage::resize - a FITSImage is not writable"));
}

Bool FITSImage::doGetSlice(Array<Float>& buffer,
                           const Slicer& section)
{
   reopenIfNeeded();
   if (pTiledFile_p->dataType() == TpFloat) {
      pTiledFile_p->get (buffer, section);
   } else if (pTiledFile_p->dataType() == TpDouble) {
      Array<Double> tmp;
      pTiledFile_p->get (tmp, section);
      buffer.resize(tmp.shape());
      convertArray(buffer, tmp);
   } else if (pTiledFile_p->dataType() == TpInt) {
      pTiledFile_p->get (buffer, section, scale_p, offset_p,
			 longMagic_p, hasBlanks_p);
   } else if (pTiledFile_p->dataType() == TpShort) {
      pTiledFile_p->get (buffer, section, scale_p, offset_p,
			 shortMagic_p, hasBlanks_p);
   } else if (pTiledFile_p->dataType() == TpUChar) {
      pTiledFile_p->get (buffer, section, scale_p, offset_p,
			uCharMagic_p, hasBlanks_p);
   }
   return False;                            // Not a reference
} 
   

void FITSImage::doPutSlice (const Array<Float>&, const IPosition&,
                            const IPosition&)
{
   throw (AipsError ("FITSImage::putSlice - "
		     "is not possible as FITSImage is not writable"));
}


String FITSImage::name (Bool stripPath) const
{
   Path path(name_p);
   if (stripPath) {
      return path.baseName();
   } else {
      return path.absoluteName();
   }
}


Bool FITSImage::isPersistent() const
{
  return True;
}

Bool FITSImage::isPaged() const
{
  return True;
}

Bool FITSImage::isWritable() const
{  
// Its too hard to implement putMaskSlice becuase
// magic blanking is used. It measn we lose
// the data values if the mask is put somewhere

   return False;
}


Bool FITSImage::ok() const
{
   return True;
}  

Bool FITSImage::doGetMaskSlice (Array<Bool>& buffer, const Slicer& section)
{
   if (!hasBlanks_p) {
      buffer.resize (section.length());
      buffer = True;
      return False;
   }
//
   reopenIfNeeded();
   return pPixelMask_p->getSlice (buffer, section);
}


Bool FITSImage::hasPixelMask() const
{
   return hasBlanks_p;
}  

const Lattice<Bool>& FITSImage::pixelMask() const
{
   if (!hasBlanks_p) {
      throw (AipsError ("FITSImage::pixelMask - no pixelmask used"));
   }
   return *pPixelMask_p;
}

Lattice<Bool>& FITSImage::pixelMask()
{
   if (!hasBlanks_p) {
      throw (AipsError ("FITSImage::pixelMask - no pixelmask used"));
   }
   return *pPixelMask_p;
}

void FITSImage::tempClose()
{
   if (! isClosed_p) {
      delete pPixelMask_p;
      pPixelMask_p = 0;
//
      pTiledFile_p = 0;
      isClosed_p = True;
   }
}

void FITSImage::reopen()
{
   if (isClosed_p) {
      open();
   }
}

uInt FITSImage::maximumCacheSize() const
{
   reopenIfNeeded();
   return pTiledFile_p->maximumCacheSize() / ValType::getTypeSize(dataType_p);
}

void FITSImage::setMaximumCacheSize (uInt howManyPixels)
{
   reopenIfNeeded();
   const uInt sizeInBytes = howManyPixels * ValType::getTypeSize(dataType_p);
   pTiledFile_p->setMaximumCacheSize (sizeInBytes);
}

void FITSImage::setCacheSizeFromPath (const IPosition& sliceShape, 
				      const IPosition& windowStart,
				      const IPosition& windowLength,
				      const IPosition& axisPath)
{
   reopenIfNeeded();
   pTiledFile_p->setCacheSize (sliceShape, windowStart,
			       windowLength, axisPath);
}

void FITSImage::setCacheSizeInTiles (uInt howManyTiles)  
{  
   reopenIfNeeded();
   pTiledFile_p->setCacheSize (howManyTiles);
}


void FITSImage::clearCache()
{
   if (! isClosed_p) {
      pTiledFile_p->clearCache();
   }
}

void FITSImage::showCacheStatistics (ostream& os) const
{
   reopenIfNeeded();
   os << "FITSImage statistics : ";
   pTiledFile_p->showCacheStatistics (os);
}



void FITSImage::setup()
{
// Separate the FITS filename from any
// possible extension specification

   name_p = get_fitsname(fullname_p);

// Determine the HDU index from the extension specification
   uInt HDUnum = get_hdunum(fullname_p);


// Compare the HDU index given directly and
// the one extracted from the name
   if (HDUnum != whichHDU_p){

// if an extension information was given,
// the index extracted from it wins
	   if (name_p != fullname_p){
		   whichHDU_p = HDUnum;
	   }
	   else {

// if the index given directly is zero (which means the default),
// the zeroth index might be emptied and the index retrieved
// in the method above (which is 1) is used
		   if (!whichHDU_p) {
			   whichHDU_p = HDUnum;
		   }
	   }
   }

   if (name_p.empty()) {
      throw AipsError("FITSImage: given file name is empty");
   }
//
   if (!maskSpec_p.name().empty()) {
      throw AipsError("FITSImage " + name_p + " has no named masks");
   }
   Path path(name_p);
   String fullName = path.absoluteName();

// Fish things out of the FITS file

   CoordinateSystem cSys;
   IPosition shape;
   ImageInfo imageInfo;
   Unit brightnessUnit;
   Int recno;
   Int recsize;          // Should be 2880 bytes (unless blocking used)
   FITS::ValueType dataType;
   Record miscInfo;

// hasBlanks only relevant to Integer images.  Says if 'blank' value defined in header

   getImageAttributes(cSys, shape, imageInfo, brightnessUnit, miscInfo, 
                      recsize, recno, dataType, scale_p, offset_p, 
		      uCharMagic_p, shortMagic_p,
                      longMagic_p, hasBlanks_p, fullName,  whichRep_p, whichHDU_p);
   // shape must be set before image info in cases of multiple beams
   shape_p = TiledShape (shape, TiledFileAccess::makeTileShape(shape));
   setMiscInfoMember (miscInfo);

// set ImageInterface data

   setCoordsMember (cSys);

// Set FITSImage data

   setUnitMember (brightnessUnit);

// By default, ImageInterface makes a memory-based LoggerHolder
// which is all we need.  We will fill it in later

// I don't understand why I have to subtract one, as the
// data should begin in the NEXT record. BobG surmises
// the FITS classes read ahead...
// MK: I think there is an additional read() and hence
// count-up of recno when the file is first accessed and
// then for every skipped hdu, thats where the "-1 - whichHDU comes from"
   fileOffset_p += (recno - 1 - whichHDU_p) * recsize;
//
   dataType_p = TpFloat;
   if (dataType == FITS::DOUBLE) {
      dataType_p = TpDouble;
   } else if (dataType == FITS::SHORT) {
      dataType_p = TpShort;
   } else if (dataType == FITS::LONG) {
      dataType_p = TpInt;
   } else if (dataType == FITS::BYTE) {
      dataType_p = TpUChar;
   }

// See if there is a mask specifier.  Defaults to apply mask.

   if (maskSpec_p.useDefault()) {

// We would like to use any mask.  For 32 f.p. bit we don't know if there
// are masked pixels (they are NaNs).  For Integer types we do know if there
// the magic value has been set (suggests there are masked pixels) and 
// hasBlanks_p was set to T or F by getImageAttributes

      if (dataType_p==TpFloat || dataType_p== TpDouble) hasBlanks_p = True;
   } else {

// We don't want to use the mask

      hasBlanks_p = False;
   }

// Open the image.
   open();

   // Finally, read any supported extensions, like a BEAMS table
   if (_hasBeamsTable) {
     ImageFITSConverter::readBeamsTable(imageInfo, fullName, dataType_p);
   }
   setImageInfoMember (imageInfo);
}


void FITSImage::open()
{
   Bool writable = False;
   Bool canonical = True;    

// The tile shape must not be a subchunk in all dimensions

   pTiledFile_p = new TiledFileAccess(name_p, fileOffset_p,
				      shape_p.shape(), shape_p.tileShape(),
                                      dataType_p, TSMOption(),
				      writable, canonical);

// Shares the pTiledFile_p pointer. Scale factors for integers

   FITSMask* fitsMask=0;
   if (hasBlanks_p) {
      if (dataType_p == TpFloat) {
         fitsMask = new FITSMask(&(*pTiledFile_p));
      } else if (dataType_p == TpDouble) {
         fitsMask = new FITSMask(&(*pTiledFile_p));
      } else if (dataType_p == TpUChar) {
         fitsMask = new FITSMask(&(*pTiledFile_p), scale_p, offset_p, 
                                 uCharMagic_p, hasBlanks_p);
      } else if (dataType_p == TpShort) {
         fitsMask = new FITSMask(&(*pTiledFile_p), scale_p, offset_p, 
                                 shortMagic_p, hasBlanks_p);
      } else if (dataType_p == TpInt) {
         fitsMask = new FITSMask(&(*pTiledFile_p), scale_p, offset_p, 
                                 longMagic_p, hasBlanks_p);
      }
      if (fitsMask) {
        fitsMask->setFilterZero(filterZeroMask_p);
        pPixelMask_p = fitsMask;
      }
   }

// Ok, it is open now.

   isClosed_p = False;
}


void FITSImage::getImageAttributes (CoordinateSystem& cSys,
                                    IPosition& shape, ImageInfo& imageInfo,
                                    Unit& brightnessUnit,
                                    RecordInterface& miscInfo,
                                    Int& recordsize, Int& recordnumber, 
                                    FITS::ValueType& dataType, 
                                    Float& scale, Float& offset, 
				    uChar& uCharMagic, Short& shortMagic,
                                    Int& longMagic, Bool& hasBlanks, const String& name,
                                    uInt whichRep, uInt whichHDU)
{
    LogIO os(LogOrigin("FITSImage", "getImageAttributes", WHERE));
    File fitsfile(name);
    if (!fitsfile.exists() || !fitsfile.isReadable() || !fitsfile.isRegular()) {
       throw (AipsError(name + " does not exist or is not readable"));
    }
//
   ImageOpener::ImageTypes type = ImageOpener::imageType(name_p);
   if (type != ImageOpener::FITS) {
       throw (AipsError(name + " is not a FITS image"));
   }
//
    FitsInput infile(fitsfile.path().expandedName().chars(), FITS::Disk);
    if (infile.err()) {
        throw (AipsError("Cannot open file " + name +
			 " (or other I/O error)"));
    }
    recordsize = infile.fitsrecsize();


//
// Advance to the right HDU
//
    for (uInt i=0; i<whichHDU; i++) {
        infile.skip_hdu();
        if (infile.err()) {
            throw(AipsError("Error advancing to image in file " + name));
        }
        // add the size of the skipped HDU
        // to the fileOffset
        fileOffset_p += infile.getskipsize();
    }

// Check type
	dataType = infile.datatype();
	if (dataType != FITS::FLOAT &&
	    dataType != FITS::DOUBLE &&
	    dataType != FITS::SHORT &&
	    dataType != FITS::LONG &&
	    dataType != FITS::BYTE)	  
	  {
	   throw AipsError("FITS file " + name +
			   " should contain float, double, short or long data");
	}


// 
// Make sure the current spot in the FITS file is an image
//
    if (infile.rectype() != FITS::HDURecord ||
        (infile.hdutype() != FITS::PrimaryArrayHDU &&
         infile.hdutype() != FITS::ImageExtensionHDU)) {
        throw (AipsError("No image at specified location in file " + name));
    }

// Check that the header type fits to the extension number
    if (!whichHDU && infile.hdutype()!= FITS::PrimaryArrayHDU) {
    	throw (AipsError("The first extension of the image must be a PrimaryArray in "
			"FITS file " + name));
    }
    else if (whichHDU && infile.hdutype()!=FITS::ImageExtensionHDU)
    {
    	throw (AipsError("The image must be stored in an ImageExtension of"
			"FITS file " + name));
    }

// Crack header
    if (!whichHDU_p)
    {
    	if (dataType==FITS::FLOAT) {
    		crackHeader<Float>(cSys, shape, imageInfo, brightnessUnit, miscInfo,  scale,
    				offset, uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::DOUBLE) {
    		crackHeader<Double>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale,
    				offset, uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::LONG) {
    		crackHeader<Int>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale,
    				offset, uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::SHORT) {
	  crackHeader<Short>(cSys, shape, imageInfo, brightnessUnit, 
			     miscInfo, scale, offset, uCharMagic, shortMagic, 
			     longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::BYTE) {
	  crackHeader<uChar>(cSys, shape, imageInfo, brightnessUnit, 
			     miscInfo, scale, offset, uCharMagic, shortMagic, 
			     longMagic, hasBlanks, os, infile, whichRep);
    	}
    }
    else
    {
    	if (dataType==FITS::FLOAT) {
    		crackExtHeader<Float>(cSys, shape, imageInfo, brightnessUnit, miscInfo,  scale,
    				offset, uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::DOUBLE) {
    		crackExtHeader<Double>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale,
    				offset, uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::LONG) {
    		crackExtHeader<Int>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale,
    				offset, uCharMagic, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::SHORT) {
	  crackExtHeader<Short>(cSys, shape, imageInfo, brightnessUnit, 
				miscInfo, scale, offset, uCharMagic, shortMagic, 
				longMagic, hasBlanks, os, infile, whichRep);
    	} else if (dataType==FITS::BYTE) {
	  crackExtHeader<uChar>(cSys, shape, imageInfo, brightnessUnit, 
				miscInfo, scale, offset, uCharMagic, shortMagic, 
				longMagic, hasBlanks, os, infile, whichRep);
    	}
    }
//  }

// Get recordnumber 
   
    recordnumber = infile.recno();
}

void FITSImage::setMaskZero(Bool filterZero)
{
	// set the zero masking on the
	// current mask
	if (pPixelMask_p)
		dynamic_cast<FITSMask *>(pPixelMask_p)->setFilterZero(True);

	// set the flag, such that an later
	// mask created in 'open()' will be OK
	// as well
	filterZeroMask_p = filterZero;
}

} //# NAMESPACE CASACORE - END

