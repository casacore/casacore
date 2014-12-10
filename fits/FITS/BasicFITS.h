//# FITS.h: Transform a Casacore Array to or from a FITS disk file.
//# Copyright (C) 1993,1994,1995,1999
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

#ifndef FITS_BasicFITS_H
#define FITS_BasicFITS_H

#include <casacore/casa/aips.h>
//# Would like to forward declare
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/Map.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class String;        // Forward declarations

//<summary>  read a FITS file from a Casacore array</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<linkfrom anchor=ReadFITS classes=FitsInput>
// <here>ReadFITS</here> Casacore interface routines.
//</linkfrom>

//<motivation>
// Quick and dirty interface to the FITS classes for turning Casacore
// arrays into FITS files and back. N.B. this will have many more features
// in the future, also some files should be renamed since we now have
// FITS.h and fits.h. 
//</motivation>
//<synopsis>
// Read FITS from a file into a Casacore Array. Sets "ok" to False if there 
// is any problem. We only deal with data in the primary data array.
// If ReadFITS fails, the state of array is undefined. Trailing
// degenerate (length==1) axes are NOT removed. If desired, you may do
// this yourself with the nonDegenerate array member function.
// If ok is false, ErrorMessage will contain an information error message.
// If necessary, the data is converted from whatever type it is stored as 
// to Float. BSCALE and BZERO are applied. Blanks are not handled.
//
// If unitName is non-null, the string it points to is filled with the FITS
// BUNIT keyword. If axisNames is name of the axes (CTYPEn).
// If refPixel is non-null, it is set to the reference pixel of the FITS file
// (CRPIX). Similarly refLocation is set to the position 
// (image coordinates) of the reference pixel (CRVALn) and delta is 
// set to the increment along each axis (CDELTn). All
// the vectors are resized if necessary. Note that FITS pixel indexing is
// one-based, Casacore is 0-based, this correction is made. unitName and
// axisNames have trailing blanks (a FITS "feature") removed.
//
// If "keywords" is non-null, the integral and floating point keywords
// (excluding NAXIS*, BSCALE, BZERO) are read into keywords. Case is not
// changed.
//</synopsis>
//
// If objectName is non-null, the string it points to is set to the
// value of the FITS OBJECT keyword.
//
//<note role=caution> This will only work properly on an IEEE big-endian
//                    machine at the moment. 
//</note>


//<group name=ReadFITS>
// blabla
Array<Float> ReadFITS(const char *FileName, Bool &ok, String &ErrorMessage,
		      String *unitName = 0,
		      Vector<String> *axisNames = 0,
		      Vector<Float> *refPixel = 0,
		      Vector<Float> *refLocation = 0,
		      Vector<Float> *delta = 0,
		      Map<String, Double> *keywords = 0,
                      String *objectName = 0);
//</group>

//<summary> write a FITS file to a Casacore array</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<linkfrom anchor=WriteFITS classes=FitsOutput>
// <here>WriteFITS</here> Casacore interface routines.
//</linkfrom>
//<synopsis>
// Write a FITS file from a Casacore Array. Returns False if there is any
// proglem. The data is written into the primary data array, and the data
// is written in floating point (BITPIX=-32). If the operation fails, 
// ErrorMessage will contain an informative error. At the moment this
// probably isn't bulletproof enough at finding errors on output.
//
// If any of unitName, axisNames, refPixel, refLocation, or delta are
// non-null, the corresponding FITS keywords (BUNIT, CTYPEn, CRPIXn,
// CRVALn, CDELTn) are set. CRVALn is corrected for the difference in
// addressing between FITS and Casacore (1 vs. 0). If a Vector pointer
// is non-null, then that vector must be the correct length.
//
// If keywords is non-null, the contents are written out as FITS keywords.
// The names are upper-cased and truncated to 8 characters (yuck). No other
// validation is done (e.g. that SIMPLE or NAXIS is not in the map).
//
// If objectName is non-null, the OBJECT keyword is set.
//
// BITPIX can presently be set to -32 or 16 only. When BITPIX is 16 it will
// write BSCALE and BZERO into the FITS file. If minPix is greater than maxPix
// the minimum and maximum pixel values will be determined from the array,
// otherwise the supplied values will be used and pixels outside that range
// will be truncated to the minimum and maximum pixel values (note that this
// truncation does not occur for BITPIX=-32).
//</synopsis>

//<group name=WriteFITS>
// blabla
Bool WriteFITS(const char *FileName, const Array<Float> &array,
	       String &ErrorMessage,
	       const char *unitName = 0,
	       const Vector<String> *axisNames = 0,
	       const Vector<Float> *refPixel = 0,
	       const Vector<Float> *refLocation = 0,
	       const Vector<Float> *delta = 0,
	       const Map<String, Double> *keywords = 0,
	       const char *objectName = 0,
	       Int BITPIX=-32,
	       Float minPix = 1.0, Float maxPix = -1.0);
//</group>

} //# NAMESPACE CASACORE - END

#endif
