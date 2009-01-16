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

#include <images/Images/FITSImage.h>

#include <fits/FITS/hdu.h>
#include <fits/FITS/fitsio.h>
#include <fits/FITS/FITSKeywordUtil.h>
#include <images/Images/ImageInfo.h>
#include <images/Images/ImageFITSConverter.h>
#include <images/Images/MaskSpecifier.h>
#include <images/Images/ImageOpener.h>
#include <lattices/Lattices/TiledShape.h>
#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/FITSMask.h>
#include <tables/Tables/TiledFileAccess.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <coordinates/Coordinates/CoordinateUtil.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Slicer.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <tables/LogTables/LoggerHolder.h>
#include <casa/Logging/LogIO.h>
#include <casa/BasicMath/Math.h>
#include <casa/OS/File.h>
#include <casa/Quanta/Unit.h>
#include <casa/Utilities/CountedPtr.h>
#include <casa/Utilities/ValType.h>
#include <casa/BasicSL/String.h>
#include <casa/Exceptions/Error.h>

#include <casa/iostream.h>



namespace casa { //# NAMESPACE CASA - BEGIN

FITSImage::FITSImage (const String& name, uInt whichRep)
: ImageInterface<Float>(),
  name_p      (name),
  pTiledFile_p(0),
  pPixelMask_p(0),
  scale_p     (1.0),
  offset_p    (0.0),
  shortMagic_p (0),
  longMagic_p (0),
  hasBlanks_p (False),
  dataType_p  (TpOther),
  fileOffset_p(0),
  isClosed_p  (True),
  whichRep_p(whichRep)
{
   setup();
}

FITSImage::FITSImage (const String& name, const MaskSpecifier& maskSpec, uInt whichRep)
: ImageInterface<Float>(),
  name_p      (name),
  maskSpec_p  (maskSpec),
  pTiledFile_p(0),
  pPixelMask_p(0),
  scale_p     (1.0),
  offset_p    (0.0),
  shortMagic_p (0),
  longMagic_p (0),
  hasBlanks_p (False),
  dataType_p  (TpOther),
  fileOffset_p(0),
  isClosed_p  (True),
  whichRep_p(whichRep)
{
   setup();
}

FITSImage::FITSImage (const FITSImage& other)
: ImageInterface<Float>(other),
  name_p      (other.name_p),
  maskSpec_p  (other.maskSpec_p),
  pTiledFile_p(other.pTiledFile_p),
  pPixelMask_p(0),
  shape_p     (other.shape_p),
  scale_p     (other.scale_p),
  offset_p    (other.offset_p),
  shortMagic_p (other.shortMagic_p),
  longMagic_p (other.longMagic_p),
  hasBlanks_p (other.hasBlanks_p),
  dataType_p  (other.dataType_p),
  fileOffset_p(other.fileOffset_p),
  isClosed_p  (other.isClosed_p),
  whichRep_p(other.whichRep_p)
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
      maskSpec_p  = other.maskSpec_p;
      scale_p     = other.scale_p;
      offset_p    = other.offset_p;
      shortMagic_p = other.shortMagic_p;
      longMagic_p = other.longMagic_p;
      hasBlanks_p = other.hasBlanks_p;
      dataType_p  = other.dataType_p;
      fileOffset_p= other.fileOffset_p;
      isClosed_p  = other.isClosed_p;
      whichRep_p = other.whichRep_p;
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
                      recsize, recno, dataType, scale_p, offset_p, shortMagic_p,
                      longMagic_p, hasBlanks_p, fullName,  whichRep_p);
   setMiscInfoMember (miscInfo);

// set ImageInterface data

   setCoordsMember (cSys);
   setImageInfoMember (imageInfo);

// Set FITSImage data

   setUnitMember (brightnessUnit);

// By default, ImageInterface makes a memory-based LoggerHolder
// which is all we need.  We will fill it in later

// I don't understand why I have to subtract one, as the
// data should begin in the NEXT record. BobG surmises
// the FITS classes read ahead...

   fileOffset_p = (recno - 1) * recsize;
//
   dataType_p = TpFloat;
   if (dataType == FITS::DOUBLE) {
      dataType_p = TpDouble;
   } else if (dataType == FITS::SHORT) {
      dataType_p = TpShort;
   } else if (dataType == FITS::LONG) {
      dataType_p = TpInt;
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

// Form the tile shape.

   shape_p = TiledShape (shape, TiledFileAccess::makeTileShape(shape));

// Open the image.
   open();
}


void FITSImage::open()
{
   uInt maxCacheSize = 0;
   Bool writable = False;
   Bool canonical = True;    

// The tile shape must not be a subchunk in all dimensions

   pTiledFile_p = new TiledFileAccess(name_p, fileOffset_p,
				      shape_p.shape(), shape_p.tileShape(),
                                      dataType_p, maxCacheSize,
				      writable, canonical);

// Shares the pTiledFile_p pointer. Scale factors for 16bit and 32 bit integers

   if (hasBlanks_p) {
      if (dataType_p == TpFloat) {
         pPixelMask_p = new FITSMask(&(*pTiledFile_p));
      } else if (dataType_p == TpDouble) {
         pPixelMask_p = new FITSMask(&(*pTiledFile_p));
      } else if (dataType_p == TpShort) {
         pPixelMask_p = new FITSMask(&(*pTiledFile_p), scale_p, offset_p, 
   				      shortMagic_p, hasBlanks_p);
      } else if (dataType_p == TpInt) {
         pPixelMask_p = new FITSMask(&(*pTiledFile_p), scale_p, offset_p, 
   				      longMagic_p, hasBlanks_p);
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
                                    Float& scale, Float& offset, Short& shortMagic,
                                    Int& longMagic, Bool& hasBlanks, const String& name,
                                    uInt whichRep)
{
// Open sesame

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

// Check type

    dataType = infile.datatype();
    if (dataType != FITS::FLOAT && 
        dataType != FITS::DOUBLE &&
        dataType != FITS::SHORT && 
        dataType != FITS::LONG) {
       throw AipsError("FITS file " + name +
		       " should contain float, double, short or long data");
    }

//
// Advance to the right HDU
//
    uInt whichHDU = 0;
    for (uInt i=0; i<whichHDU; i++) {
        infile.skip_hdu();
        if (infile.err()) {
            throw(AipsError("Error advancing to image in file " + name));
        }
    }
// 
// Make sure the current spot in the FITS file is an image
//
    if (infile.rectype() != FITS::HDURecord ||
        (infile.hdutype() != FITS::PrimaryArrayHDU &&
         infile.hdutype() != FITS::ImageExtensionHDU)) {
        throw (AipsError("No image at specified location in file " + name));
    }

// Only handle PrimaryArray

    if (infile.hdutype()!= FITS::PrimaryArrayHDU) { 
       throw (AipsError("The image must be stored in the PrimaryArray of"
			"FITS file " + name));
    }

// Crack header

    if (dataType==FITS::FLOAT) {
	crackHeader<Float>(cSys, shape, imageInfo, brightnessUnit, miscInfo,  scale,
			   offset, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType==FITS::DOUBLE) {
	crackHeader<Double>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale,
			    offset, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } else if (dataType==FITS::LONG) {
	crackHeader<Int>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale,
			 offset, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    } if (dataType==FITS::SHORT) {
	crackHeader<Short>(cSys, shape, imageInfo, brightnessUnit, miscInfo, scale,
			   offset, shortMagic, longMagic, hasBlanks, os, infile, whichRep);
    }
//  }

// Get recordnumber 
   
    recordnumber = infile.recno();
}

} //# NAMESPACE CASA - END

