//# FITS2Image.cc: Class implementing templated functions for FITSImage
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
//# $Id: 


#include <images/Images/FITSImage.h>

#include <casa/Arrays/IPosition.h>
#include <coordinates/Coordinates/CoordinateSystem.h>
#include <casa/Containers/Record.h>
#include <casa/Exceptions/Error.h>
#include <fits/FITS/fitsio.h>
#include <fits/FITS/hdu.h>
#include <fits/FITS/FITSKeywordUtil.h>
#include <images/Images/ImageInfo.h>
#include <images/Images/ImageFITSConverter.h>
#include <casa/Logging/LogIO.h>
#include <casa/BasicMath/Math.h>
#include <casa/Quanta/Unit.h>
#include <casa/Utilities/DataType.h>
#include <casa/BasicSL/String.h>


template <typename T>
void FITSImage::crackHeader (CoordinateSystem& cSys,
                             IPosition& shape, ImageInfo& imageInfo,
                             Unit& brightnessUnit, RecordInterface& miscInfo,
                             Float& scale, Float& offset, Short& magicShort, 
                             Int& magicInt, Bool& hasBlanks, 
                             LogIO& os, FitsInput& infile)
{
   
// Shape

    PrimaryArray<T> fitsImage(infile);
    Int ndim = fitsImage.dims();
    shape.resize(ndim);
    for (Int i=0; i<ndim; i++) {
       shape(i) = fitsImage.dim(i);
    }

// Get header

    Vector<String> ignore(0); 
    Record header;
    if (!FITSKeywordUtil::getKeywords(header, fitsImage.kwlist(), ignore)) {
       throw (AipsError("Error retrieving keywords from fits header"));
    }

// BITPIX

    T* t;
    DataType dataType = whatType(t);
//
    Int bitpix;   
    header.get("bitpix", bitpix);
    header.removeField("bitpix");   
    if (dataType==TpFloat) {
       if (bitpix != -32) {
          throw (AipsError("bitpix card inconsistent with data type: expected bitpix = -32"));
       }  
    } else if (dataType==TpDouble) {
       if (bitpix != -64) {
          throw (AipsError("bitpix card inconsistent with data type: expected bitpix = -64"));
       }  
    } else if (dataType==TpInt) {
       if (bitpix != 32) {
          throw (AipsError("bitpix card inconsistent with data type: expected bitpix = 32"));
       }  
    } else if (dataType==TpShort) {
       if (bitpix != 16) {
          throw (AipsError("bitpix card inconsistent with data type: expected bitpix = 16"));
       }  
    } else {
       throw (AipsError("Unsupported Template type; Float & Double only are supported"));
    }

// Add naxis into header (not in the keyword list).  People
// provide headers with funny mixtures of CTYPEi and  naxis so
// we need to do this

   header.define("naxis", shape.asVector());

// Scale and blank (will only be present for Int and Short)

    Double s = 1.0;
    Double o = 0.0;
    if (header.isDefined("bscale")) {
       header.get("bscale", s);
       header.removeField("bscale");
    }
    if (header.isDefined("bzero")) {
       header.get("bzero", o);
       header.removeField("bzero");
    }
    scale = s; 
    offset = o;

// Will only be present for Int and Short

    hasBlanks = False;
    if (header.isDefined("blank")) {
       Int m;
       header.get("blank", m);
       header.removeField("blank");
       if (dataType==TpShort) {
          magicShort = m;
       } else if (dataType==TpInt) {
          magicInt = m;
       } else {
          magicShort = m;
          magicInt = m;
       }
       hasBlanks = True;
    }

// CoordinateSystem

    Bool dropStokes = True;
    Int stokesFITSValue = 1;
    cSys = ImageFITSConverter::getCoordinateSystem(stokesFITSValue, header, os, shape, dropStokes);
    ndim = shape.nelements();

// Brightness Unit

    brightnessUnit = ImageFITSConverter::getBrightnessUnit(header, os);

// ImageInfo

    imageInfo = ImageFITSConverter::getImageInfo(header);

// If we had one of those unofficial pseudo-Stokes on the Stokes axis, store it in the imageInfo

    if (stokesFITSValue != -1) {
       ImageInfo::ImageTypes type = ImageInfo::imageTypeFromFITS(stokesFITSValue);
       if (type!= ImageInfo::Undefined) {
          imageInfo.setImageType(type);
       }
    }

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

// Get and store history.

    Vector<String> lines;
    String groupType;
    ConstFitsKeywordList kw = fitsImage.kwlist();
    kw.first();

// Set the contents of the ImageInterface logger object (history)

    LoggerHolder& log = logger();
    ImageFITSConverter::restoreHistory(log, kw);

// Try and find the restoring beam in the history cards if
// its not in the header

    if (imageInfo.restoringBeam().nelements() != 3) {
       imageInfo.getRestoringBeam(log);
    }
}

template void FITSImage::crackHeader<Double> (CoordinateSystem &, IPosition &, ImageInfo &, Unit &, RecordInterface &, Float &, Float &, Short &, Int &, Bool &, LogIO &, FitsInput &);
template void FITSImage::crackHeader<Float> (CoordinateSystem &, IPosition &, ImageInfo &, Unit &, RecordInterface &, Float &, Float &, Short &, Int &, Bool &, LogIO &, FitsInput &);
template void FITSImage::crackHeader<Int> (CoordinateSystem &, IPosition &, ImageInfo &, Unit &, RecordInterface &, Float &, Float &, Short &, Int &, Bool &, LogIO &, FitsInput &);
template void FITSImage::crackHeader<Short> (CoordinateSystem &, IPosition &, ImageInfo &, Unit &, RecordInterface &, Float &, Float &, Short &, Int &, Bool &, LogIO &, FitsInput &);
