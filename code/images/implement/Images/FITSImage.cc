//# FITSImage.cc: defines the FITSImage class giving direct access to FITS images
//# Copyright (C) 1998,1999,2000,2001
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

#include <trial/Images/FITSImage.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Slicer.h>
#include <aips/Containers/Record.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <aips/Exceptions/Error.h>
#include <aips/FITS/fitsio.h>
#include <aips/FITS/hdu.h>
#include <trial/FITS/FITSUtil.h>
#include <trial/Images/ImageInfo.h>
#include <trial/Images/ImageFITSConverter.h>
#include <aips/Lattices/TiledShape.h>
#include <aips/Lattices/TempLattice.h>
#include <trial/Lattices/FITSMask.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/OS/File.h>
#include <aips/Quanta/Unit.h>
#include <trial/Tables/TiledFileAccess.h>
#include <aips/Utilities/CountedPtr.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>

#include <iostream.h>



FITSImage::FITSImage (const String& name)
: ImageInterface<Float>(),
  name_p(name),
  pTiledFile_p(0),
  pPixelMask_p(0),
  scale_p(1.0),
  offset_p(0.0),
  magic_p(0),
  hasBlanks_p(False)
{
   if (name_p.empty()) throw(AipsError("Given file name is empty"));
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
   getImageAttributes(cSys, shape, imageInfo, brightnessUnit, rec_p, 
                      recsize, recno, dataType, scale_p, offset_p, magic_p,
                      hasBlanks_p, fullName);

// set ImageInterface data

   setCoordsMember (cSys);
   setImageInfoMember (imageInfo);

// We need to put the history in some memory based LogSink
// setLogMember(logSink);

// Set FITSImage data

   unit_p = brightnessUnit;

// I don't understand why I have to subtract one, as the
// data should begin in the NEXT record. BobG surmises
// the FITS classes read ahead...

   Int64 fileOffset = 0;
   fileOffset = (recno -1 ) * recsize;
//
   uInt maxCacheSize = 0;
   Bool writable = False;
   Bool canonical = True;    

// The tile shape must not be a subchunk in all dimensions
// It can be nx * ny * 1
//
   tileShape_p.resize(shape.nelements());
   tileShape_p = 1;
   if (shape.nelements()>0) tileShape_p(0) = shape(0);
   if (shape.nelements()>1) tileShape_p(1) = shape(1);
   DataType type = TpFloat;
   if (dataType==FITS::SHORT) type = TpShort;
   pTiledFile_p = new TiledFileAccess(fullName, fileOffset, shape, tileShape_p,
                                      type, maxCacheSize, writable, canonical);

// Shares the pTiledFile_p pointer. Scale factors for 16bit integers

   pPixelMask_p = new FITSMask(&(*pTiledFile_p), scale_p, offset_p, 
                               magic_p, hasBlanks_p);
}

FITSImage::FITSImage (const FITSImage& other)
: ImageInterface<Float>(other),
  name_p(other.name_p),
  unit_p(other.unit_p),
  rec_p(other.rec_p),
  tileShape_p(other.tileShape_p),
  pTiledFile_p(other.pTiledFile_p),
  pPixelMask_p(other.pPixelMask_p->clone()),
  scale_p(other.scale_p),
  offset_p(other.offset_p),
  magic_p(other.magic_p)
{}
 
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
      pPixelMask_p = other.pPixelMask_p->clone();    // Raw pointer
//
      name_p = other.name_p;
      unit_p = other.unit_p;
      rec_p = other.rec_p;
      tileShape_p.resize(0);
      tileShape_p = other.tileShape_p;
//
      scale_p = other.scale_p;
      offset_p = other.offset_p;
      magic_p = other.magic_p;
   }
   return *this;
} 
 
FITSImage::~FITSImage()
{
   delete pPixelMask_p;
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

// There is never a region pointer but always a pixel
// mask so the FITSImage is alwasy masked.

   return True;
}

const LatticeRegion* FITSImage::getRegionPtr() const
{
   return 0;
}

IPosition FITSImage::shape() const  
{ 
   return pTiledFile_p->shape();
}

void FITSImage::resize(const TiledShape&)
{
   throw (AipsError ("FITSImage::resize - a FITSImage is not writable"));
}

Bool FITSImage::doGetSlice(Array<Float>& buffer,
                           const Slicer& section)
{
   if (pTiledFile_p->dataType()==TpFloat) {
      pTiledFile_p->get(buffer, section);
   } else {
      pTiledFile_p->get(buffer, section, scale_p, offset_p, magic_p, hasBlanks_p);
   }
   return False;                            // Not a reference
} 
   

void FITSImage::doPutSlice (const Array<Float>&, const IPosition&,
                            const IPosition&)
{
   throw (AipsError ("FITSImage::putSlice - "
		     "is not possible as FITSImage is not writable"));
}

Bool FITSImage::setUnits(const Unit& unit)
{  
   unit_p = unit;
   return True;
}
   
 Unit FITSImage::units() const
{  
  return unit_p;
}


String FITSImage::name (Bool strippath) const
{
   Path path(name_p);
   if (strippath) {
      return path.baseName();
   } else {
      return path.absoluteName();
   }
}


const RecordInterface& FITSImage::miscInfo() const
{
  return rec_p;
}
 

Bool FITSImage::setMiscInfo(const RecordInterface& rec)
{ 
   rec_p = rec;
   return True;
}
 
Bool FITSImage::isWritable() const
{  
// Its too hard to implement putMaskSLice becuase
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
   return pPixelMask_p->getSlice(buffer, section);
}





void FITSImage::getImageAttributes (CoordinateSystem& cSys,
                                    IPosition& shape, ImageInfo& imageInfo,
                                    Unit& brightnessUnit, Record& miscInfo, 
                                    Int& recordsize, Int& recordnumber, 
                                    FITS::ValueType& dataType, 
                                    Double& scale, Double& offset, Short& magic,
                                    Bool& hasBlanks, const String& name)
{
// Open sesame

    LogIO os(LogOrigin("FITSImage", "getImageAttributes", WHERE));
    File fitsfile(name);
    if (!fitsfile.exists() || !fitsfile.isReadable() || !fitsfile.isRegular()) {
        throw (AipsError(name + " does not exist or is not readable"));
    }
//
    FitsInput infile(fitsfile.path().expandedName(), FITS::Disk);
    if (infile.err()) {
        throw (AipsError("Cannot open file (or other I/O error)"));
    }
    recordsize = infile.fitsrecsize();

// We only handle FLOAT or SHORT

    dataType = infile.datatype();
    if (dataType != FITS::FLOAT && dataType != FITS::SHORT) {
       throw (AipsError("ONly floating point and short integer FITS files are supported"));
    }

//
// Advance to the right HDU
//
    uInt whichHDU = 0;
    for (uInt i=0; i<whichHDU; i++) {
        infile.skip_hdu();
        if (infile.err()) {
            throw(AipsError("Error advancing to image in file"));
        }
    }
// 
// Make sure the current spot in the FITS file is an image
//
    if (infile.rectype() != FITS::HDURecord ||
        (infile.hdutype() != FITS::PrimaryArrayHDU &&
         infile.hdutype() != FITS::ImageExtensionHDU)) {
        throw (AipsError("No image at specified location in file"));
    }

// Only handle PrimaryArray

    if (infile.hdutype()!= FITS::PrimaryArrayHDU) { 
       throw (AipsError("The image must be stored in the PrimaryArray"));
    }
//
    if (dataType==FITS::FLOAT) {
       crackHeaderFloat (cSys, shape, imageInfo, brightnessUnit, miscInfo, os, infile);
    } else {
       crackHeaderShort (cSys, shape, imageInfo, brightnessUnit, miscInfo, 
                         scale, offset, magic, hasBlanks, os, infile);
    }

// Get recordnumber 
   
    recordnumber = infile.recno();
}



Bool FITSImage::hasPixelMask() const
{

// FITSImage always has a pixel mask

   return True;
}  

const Lattice<Bool>& FITSImage::pixelMask() const
{
  return *pPixelMask_p;
}

Lattice<Bool>& FITSImage::pixelMask()
{
  return *pPixelMask_p;
}


void FITSImage::crackHeaderFloat (CoordinateSystem& cSys,
                                  IPosition& shape, ImageInfo& imageInfo,
                                  Unit& brightnessUnit, Record& miscInfo,
                                  LogIO& os, FitsInput& infile)
{
   
// Shape

    PrimaryArray<Float> fitsImage(infile);
    uInt ndim = fitsImage.dims();
    shape.resize(ndim);
    for (uInt i=0; i<ndim; i++) {
       shape(i) = fitsImage.dim(i);
    }

// Get header

    Vector<String> ignore(0); 
    Record header;
    if (!FITSKeywordUtil::getKeywords(header, fitsImage.kwlist(), ignore)) {
       throw (AipsError("Error retrieving keywords from fits header"));
    }

// BITPIX

    Int bitpix;   
    header.get("bitpix", bitpix);
    header.removeField("bitpix");   
    if (bitpix != -32) {
       throw (AipsError("bitpix card inconsistent with data type: expected bitpix = -32"));
    }  

// CoordinateSystem

    cSys = ImageFITSConverter::getCoordinateSystem(header, os, shape);
    ndim = shape.nelements();


// Brightness Unit

    brightnessUnit = ImageFITSConverter::getBrightnessUnit(header, os);


// ImageInfo

    imageInfo = ImageFITSConverter::getImageInfo(header);

// Get rid of anything else
        
    ignore.resize(6);
    ignore(0) = "^datamax$";
    ignore(1) = "^datamin$";
    ignore(2) = "^origin$";
    ignore(3) = "^extend$";
    ignore(4) = "^blocked$";
    ignore(5) = "^blank$";
    FITSKeywordUtil::removeKeywords(header, ignore);

// MiscInfo is whats left

   miscInfo = header;

// Read history so as to make sure we have read all
// of the pre-data records.  Eventually I may even store it
// somewhere.

    Vector<String> lines;
    String groupType;
    ConstFitsKeywordList kw = fitsImage.kwlist();
    kw.first();
//
    uInt n;
    while ((n = FITSHistoryUtil::getHistoryGroup(lines, groupType, kw)) !=  0) {
/*
       if (groupType == "LOGTABLE") {
          FITSHistoryUtil::fromHISTORY(logTable, lines, n, True);
       } else if (groupType == "") { 
          FITSHistoryUtil::fromHISTORY(logTable, lines, n, False);
       }
*/
    }
}

void FITSImage::crackHeaderShort (CoordinateSystem& cSys,
                                  IPosition& shape, ImageInfo& imageInfo,
                                  Unit& brightnessUnit, Record& miscInfo,
                                  Double& scale, Double& offset, Short& magic,
                                  Bool& hasBlanks, LogIO& os, FitsInput& infile)
{
// Shape

    PrimaryArray<Short> fitsImage(infile);
    uInt ndim = fitsImage.dims();
    shape.resize(ndim);
    for (uInt i=0; i<ndim; i++) {
       shape(i) = fitsImage.dim(i);
    }

// Get header

    Vector<String> ignore(0); 
    Record header;
    if (!FITSKeywordUtil::getKeywords(header, fitsImage.kwlist(), ignore)) {
       throw (AipsError("Error retrieving keywords from fits header"));
    }

// BITPIX

    Int bitpix;   
    header.get("bitpix", bitpix);
    header.removeField("bitpix");   
    if (bitpix != 16) {
       throw (AipsError("bitpix card inconsistent with data type: expected bitpix = 16"));
    }  

// Scale and blank

    header.get("bscale", scale);
    header.get("bzero", offset);
    header.removeField("bscale");
    header.removeField("bzero");
    hasBlanks = False;
    if (header.isDefined("blank")) {
       Int m;
       header.get("blank", m);
       header.removeField("blank");
       magic = m;
       hasBlanks = True;
    }

// CoordinateSystem

    cSys = ImageFITSConverter::getCoordinateSystem(header, os, shape);
    ndim = shape.nelements();

// Brightness Unit

    brightnessUnit = ImageFITSConverter::getBrightnessUnit(header, os);

// ImageInfo

    imageInfo = ImageFITSConverter::getImageInfo(header);

// Get rid of anything else
        
    ignore.resize(6);
    ignore(0) = "^datamax$";
    ignore(1) = "^datamin$";
    ignore(2) = "^origin$";
    ignore(3) = "^extend$";
    ignore(4) = "^blocked$";
    ignore(5) = "^blank$";
    FITSKeywordUtil::removeKeywords(header, ignore);

// MiscInfo is whats left

   miscInfo = header;

// Read history so as to make sure we have read all
// of the pre-data records.  Eventually I may even store it
// somewhere.

    Vector<String> lines;
    String groupType;
    ConstFitsKeywordList kw = fitsImage.kwlist();
    kw.first();
//
    uInt n;
    while ((n = FITSHistoryUtil::getHistoryGroup(lines, groupType, kw)) !=  0) {
/*
       if (groupType == "LOGTABLE") {
          FITSHistoryUtil::fromHISTORY(logTable, lines, n, True);
       } else if (groupType == "") { 
          FITSHistoryUtil::fromHISTORY(logTable, lines, n, False);
       }
*/
    }
}
