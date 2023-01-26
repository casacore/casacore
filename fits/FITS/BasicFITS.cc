//# FITS.cc: Transform a Casacore Array to or from a FITS disk file.
//# Copyright (C) 1993,1994,1995,1996,1998,1999,2001,2003
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

#include <casacore/fits/FITS/BasicFITS.h>
#include <casacore/fits/FITS/FITS2.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>

#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Array<float> ReadFITS(const char *FileName, bool &ok, String &ErrorMessage,
		      String *unitName,
		      Vector<String> *axisNames,
		      Vector<float> *refPixel,
		      Vector<float> *refLocation,
		      Vector<float> *delta,
		      std::map<String, double> *keywords,
                      String *objectName)
{
    Array<float> data;

    ok = true;
    FitsInput infile(FileName, FITS::Disk);
    if (infile.err()) {
	ok = false;
	ErrorMessage = String("Cannot open file ") + String(FileName);
	return data;
    }
    if (infile.rectype() != FITS::HDURecord ||
	infile.hdutype() != FITS::PrimaryArrayHDU) {
	ok = false;
	ErrorMessage = "FITS file is not an image, or is malformed "
	    "(or something)";
	return data;
    }
    
    switch(infile.datatype()) {
    case FITS::BYTE:
	{
	    PrimaryArray<unsigned char> fitsdata(infile);
	    ReadFITSin(fitsdata, data, ok, ErrorMessage, unitName, axisNames,
		     refPixel, refLocation, delta, keywords, objectName);
	}
	break;
    case FITS::SHORT:
	{
	    PrimaryArray<short> fitsdata(infile);
	    ReadFITSin(fitsdata, data, ok, ErrorMessage, unitName, 
		     axisNames, refPixel, refLocation, delta, keywords, objectName);
	}
	break;
    case FITS::LONG:
	{
	    PrimaryArray<FitsLong> fitsdata(infile);
	    ReadFITSin(fitsdata, data, ok, ErrorMessage, unitName, 
		     axisNames, refPixel, refLocation, delta, keywords, objectName);
	}
	break;
    case FITS::FLOAT:
	{
	    PrimaryArray<float> fitsdata(infile);
	    ReadFITSin(fitsdata, data, ok, ErrorMessage, unitName, 
		     axisNames, refPixel, refLocation, delta, keywords, objectName);
	}
	break;
    case FITS::DOUBLE:
	{
	    PrimaryArray<double> fitsdata(infile);
	    ReadFITSin(fitsdata, data, ok, ErrorMessage, unitName, 
		     axisNames, refPixel, refLocation, delta, keywords, objectName);
	}
	break;
    default:
	ok = false;
	ErrorMessage = "Unknown datatype  - no data returned";
    }

    return data;
}

bool WriteFITS(const char *FileName, const Array<float> &array,
	       String &ErrorMessage,
	       const char *unitName,
	       const Vector<String> *axisNames,
	       const Vector<float> *refPixel,
	       const Vector<float> *refLocation,
	       const Vector<float> *delta,
	       const std::map<String, double> *keywords,
               const char *objectName,
	       int32_t BITPIX, float minPix, float maxPix)
{
    FitsOutput outfile(FileName, FITS::Disk);
    if (outfile.err()) {
	ErrorMessage = String("Cannot open file for writing: ")
	    + String(FileName);
	return false;
    }

    FitsKeywordList kw;
    kw.mk(FITS::SIMPLE,true);

    double bscale, bzero;
    const int16_t maxshort = 32767;
    const int16_t minshort = -32768;
    if (BITPIX == -32) {
	bscale = 1.0;
	bzero = 0.0;
	kw.mk(FITS::BITPIX, -32, "Floating point");
    } else if (BITPIX == 16) {
	kw.mk(FITS::BITPIX, 16, "Short integer");
	if (minPix > maxPix) {
	    minMax(minPix, maxPix, array);
	}
	bscale = double(maxPix - minPix)/double(int32_t(maxshort) - int32_t(minshort));
	bzero  = double(minPix) + bscale * (-double(minshort));
    } else {
	ErrorMessage = 
	    "BITPIX must be -32 (floating point) or 16 (short integer)";
	return false;
    }

    kw.mk(FITS::NAXIS, int(array.ndim()));
    for (int32_t i=0; i < int32_t(array.ndim()); i++) {
	kw.mk(i+1, FITS::NAXIS, int(array.shape()(i)));
    }
    kw.mk(FITS::BSCALE, bscale, "physical = pixel*BSCALE + BZERO");
    kw.mk(FITS::BZERO, bzero);
    // Set the "optional" keywords
    if (unitName) {
	kw.mk(FITS::BUNIT, unitName);
    }
    if (axisNames) {
	if (axisNames->nelements() != array.ndim()) {
	    ErrorMessage = String("axisNames wrong length");
	    return false;
	}
	for (int32_t i=0; i < int32_t(array.ndim()); i++) {
	    kw.mk(i+1, FITS::CTYPE, (*axisNames)(i).chars());
	}
    }
    if (refPixel) {
	if (refPixel->nelements() != array.ndim()) {
	    ErrorMessage = String("refPixel wrong length");
	    return false;
	}
	for (int32_t i=0; i < int32_t(array.ndim()); i++) {
	    kw.mk(i+1, FITS::CRPIX, (*refPixel)(i) + 1.0f);
	}
    }
    if (refLocation) {
	if (refLocation->nelements() != array.ndim()) {
	    ErrorMessage = String("refLocation wrong length");
	    return false;
	}
	for (int32_t i=0; i < int32_t(array.ndim()); i++) {
	    kw.mk(i+1, FITS::CRVAL, (*refLocation)(i));
	}
    }
    if (delta) {
	if (delta->nelements() != array.ndim()) {
	    ErrorMessage = String("delta wrong length");
	    return false;
	}
	for (int32_t i=0; i < int32_t(array.ndim()); i++) {
	    kw.mk(i+1, FITS::CDELT, (*delta)(i));
	}
    }
    if (keywords) {
        for (const auto& elem : *keywords) {
            String key (elem.first);
	    double val (elem.second);
	    // FITS requires upper case, length=8 (or less) keywords
	    key.upcase();
	    if (key.length() > 8) {
		key = key.at(0,8);
	    }
	    kw.mk(key.chars(), val);
	}
    }
    if (objectName) {
	kw.mk(FITS::OBJECT, objectName);
    }

    ostringstream os;
    os << "Written by casacore ";
    kw.history(os.str().data());
    kw.end();


    switch (BITPIX) {
    case -32:
	{
	    PrimaryArray<float> pa(kw);
	    if (pa.err()) {
		ErrorMessage = "Error constructing primary array from keywords";
		return false;
	    }

	    bool deleteIt;
	    const float *storage = array.getStorage(deleteIt);
	    //*** Cast needed because of misdeclaration (I believe) in hdu.h
	    pa.store((float *)storage);

	    // I don't think the following adequately check for errors on write.
	    if (pa.write_hdr(outfile)) {
		array.freeStorage(storage, deleteIt);
		ErrorMessage = "Write error writing keywords";
		return false;
	    }
	    if (pa.write(outfile) != int32_t(array.nelements())) {
		array.freeStorage(storage, deleteIt);
		ErrorMessage = "Write error writing data";
		return false;
	    }
	    array.freeStorage(storage, deleteIt);
	}
	break;
    case 16:
	{
	    PrimaryArray<int16_t> pa(kw);
	    if (pa.err()) {
		ErrorMessage = "Error constructing primary array from keywords";
		return false;
	    }

	    bool deleteIt;
	    const float *storage = array.getStorage(deleteIt);

	    Block<int16_t> storage16(array.nelements());
	    const uint32_t n = array.nelements();
	    for (uint32_t i=0; i<n; i++) {
		if (storage[i] <= minPix) {
		    storage16[i] = minshort;
		} else if (storage[i] >= maxPix) {
		    storage16[i] = maxshort;
		} else {
		    storage16[i] = int16_t((storage[i] - bzero)/bscale);
		}
	    }

	    //*** Cast needed because of misdeclaration (I believe) in hdu.h
	    pa.store(storage16.storage());

	    // I don't think the following adequately check for errors on write.
	    if (pa.write_hdr(outfile)) {
		array.freeStorage(storage, deleteIt);
		ErrorMessage = "Write error writing keywords";
		return false;
	    }
	    if (pa.write(outfile) != int32_t(array.nelements())) {
		array.freeStorage(storage, deleteIt);
		ErrorMessage = "Write error writing data";
		return false;
	    }
	    array.freeStorage(storage, deleteIt);
	}
	break;
    default:
	ErrorMessage = "Impossible error in WriteFITS!";
	return false;
    }



    return true;
}

} //# NAMESPACE CASACORE - END

