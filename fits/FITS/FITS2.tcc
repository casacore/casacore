//# FITS2.cc: Transform a Casacore Array to or from a FITS disk file (helper functions)
//# Copyright (C) 1994,1995,1998,1999,2001
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

#ifndef FITS_FITS2_TCC
#define FITS_FITS2_TCC

#include <casacore/fits/FITS/FITS2.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class StorageType>
void ReadFITSin(PrimaryArray<StorageType> &fitsdata,
	      Array<Float> &data,
	      Bool &ok, 
	      String &ErrorMessage,
	      String *unitName,
	      Vector<String> *axisNames,
	      Vector<Float> *refPixel,
	      Vector<Float> *refLocation,
	      Vector<Float> *delta,
	      Map<String, Double> *keywords,
	      String *objectName)
{

    IPosition shape;
    Bool deleteIt;
	
    shape.resize(fitsdata.dims());
    for (uInt i=0; i < shape.nelements(); i++) shape(i) = fitsdata.dim(i);
    data.resize(shape);
    if (fitsdata.read() != Int(data.nelements())) {
	ErrorMessage = "Could not real all data";
	ok = False;
	return;
    }
    Float *storage = data.getStorage(deleteIt);
    fitsdata.copy(storage);
    data.putStorage(storage, deleteIt);
    
    // Fill in the "optional" things
    if (unitName) {
	(*unitName) = fitsdata.bunit();
	// Get rid of trailing blanks
	(*unitName).rtrim(' ');
    }
    if (axisNames) {
	(*axisNames).resize(fitsdata.dims());
	for (Int i=0; i<fitsdata.dims(); i++) {
	    (*axisNames)(i) = fitsdata.ctype(i);
	    (*axisNames)(i).rtrim(' ');
	}
    }
    if (refPixel) {
	(*refPixel).resize(fitsdata.dims());
	for (Int i=0; i<fitsdata.dims(); i++) {
	    (*refPixel)(i) = fitsdata.crpix(i) - 1.0f; // FITS is 1-relative
	}
    }
    if (refLocation) {
	(*refLocation).resize(fitsdata.dims());
	for (Int i=0; i<fitsdata.dims(); i++) {
	    (*refLocation)(i) = fitsdata.crval(i); // FITS is 1-relative
	}
    }
    if (delta) {
	(*delta).resize(fitsdata.dims());
	for (Int i=0; i<fitsdata.dims(); i++) {
	    (*delta)(i) = fitsdata.cdelt(i); // FITS is 1-relative
	}
    }
    if (keywords) {
	String kwname;
	ConstFitsKeywordList &kwl = fitsdata.kwlist();
	kwl.first();
	const FitsKeyword *next = kwl.next();
	while (next) {
	    kwname = next->name();
	    if (kwname == "SIMPLE" ||
		kwname == "BITPIX" ||
		kwname == "END" ||
		kwname == "BSCALE" ||
		kwname == "BZERO" ||
		kwname == "BUNIT" ||
		kwname.at(0,5) == "CRVAL" ||
		kwname.at(0,5) == "CRPIX" ||
		kwname.at(0,5) == "CDELT" ||
		kwname.at(0,5) == "NAXIS") {
		next=kwl.next();
		continue;
	    }
	    switch (next->type()) {
	    case  FITS::DOUBLE:  (*keywords)(kwname) = next->asDouble(); break;
	    case FITS::FLOAT:    (*keywords)(kwname) = next->asFloat(); break;
            case FITS::LONG:     (*keywords)(kwname) = next->asInt(); break;
            default:             break;
			     }
	    next = kwl.next();
	}
     }
     if (objectName) {
         // This would not be necessary if ContFitsKeywordList had an exists()
         const FitsKeyword *kw = fitsdata.kwlist()(FITS::OBJECT);
       if (kw) {
           (*objectName) = String(kw->asString(),kw->valStrlen());
       } else {
           (*objectName) = "";
       }
       // Get rid of trailing blanks
       (*objectName).rtrim(' ');
    }
}

} //# NAMESPACE CASACORE - END


#endif
