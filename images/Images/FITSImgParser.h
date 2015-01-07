//# FITSImgParser.h: Class for parsing multi-extension FITS images
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

#ifndef IMAGES_FITSImgParser_H
#define IMAGES_FITSImgParser_H

#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class FITSExtInfo;
class HeaderDataUnit;

// <summary>
// Class for handling FITS Image extensions
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFITSImgParser.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=FITSExtInfo>FITSExtInfo</linkto>
//   <li> <linkto class=HeaderDataUnit>HeaderDataUnit</linkto>
// </prerequisite>

// <etymology>
//  This class parses through a FITS image and stores essential information
//  for each extension.
// </etymology>

// <synopsis>
// The class parses through a FITS image and extracts information
// on its extensions, e.g. the extension name and the extension version.
// It is possible to identify a certain extension and to get the its
// extension index.
//
// It is also explored whether some of the FITS extensions can be
// loaded as a quality image (data + error + mask).
// </synopsis>

// <example>
// <srcblock>
//    FITSImgParser fitsImg("in.fits");
//    uInt numHDU    = fitsImg.get_numhdu();                // get the total number of HDU's
//    uInt firstdata = fitsImg.get_firstdata_index();       // get the first HDU with data
//    String allExts = fitsImg.get_extlist_string(String("\n"));  // get a string representation of all extensions
//    String hasQual = fitsImg.has_qualityimg();            // check whether some of the extensions form quality image
// </srcblock>
// </example>

// <motivation>
// Investigate and select FITS extensions
// </motivation>

//# <todo asof="2011/08/16">
//# </todo>

class FITSImgParser
{
public: 
  // Construct a parser from the FITS file.
  FITSImgParser(const String& name);

  // Copy constructor (reference semantics).
  FITSImgParser(const FITSImgParser& other);

  // Destructor, does not much.
  ~FITSImgParser();

  // Assignment (reference semantics).
  FITSImgParser& operator=(const FITSImgParser& other);

  // Returns the name of the disk file.
  String fitsname (Bool stripPath=False) const;

  // Identify the index of an extension.
  Int get_index(const FITSExtInfo &extinfo);

  // Find an extension; return -1 if not found.
  Int find_extension(const String &extname, const Int &extversion=-1);

  // Get the index of the first extension with data.
  uInt get_firstdata_index(void);

  // Get the number of extensions.
  uInt get_numhdu(void) { return numhdu_p;};

  // Get a string representation of the extension list.
  String get_extlist_string(const String &delimiter, const String &qualmarker="",
		  const String &fitsmarker="", const Bool &listall=True);

  // Get the flag indicating at least one quality image.
  Bool has_qualityimg(void) {return qualimglist_p.size() > 0 ? True : False;};

  // Check whether the extensions named in the extension expression
  // can be loaded as a quality image.
  Bool is_qualityimg(const String &extexpr);

  // Find all necessary access information for the extensions to be loaded
  // as a quality image.
  Bool get_quality_data(const String &extexpr, Int &data_HDU, Int &error_HDU,
        String &error_type, Int &mask_HDU, String &mask_type, Int &mask_value);

private:  
  String         name_p;
  uInt           numhdu_p;

  FITSExtInfo   *extensions_p;
  Vector<String>  qualimglist_p;

  Bool           hasmeasurement_p;

  static const char *storeKwords_p[];
  static const int   nKwords_p;

  // Setup the object (used by constructors).
  void setup(void);

  // Get the information on an extension.
  void process_extension(HeaderDataUnit *h, const uInt &extindex);

  // Extract the list of extensions from the extension expression.
  Bool get_extlist(const String &extexpr, Vector<String> &extlist);

  // Get the first extension with HDU type "data" from the
  // list of indices. Returns "-1" if there is none.
  Int get_dataindex(const Vector<Int> &extindex);

  // Get the error extension name for the given data extension.
  String get_errorext(const Int &ext_index);

  // Get the mask extension name for the given data extension.
  String get_maskext(const Int &ext_index);

  // Check the keywords with fixed values
  Bool confirm_fix_keywords(const Int &ext_index);

  // Check whether the extension has a certain HDU type.
  Bool index_is_HDUtype(const Int &ext_index, const String &hdutype);

  // Find and store all set of extensions
  // that can be loaded as a quality image.
  Bool find_qualimgs(void);
};


//class FitsKeywordList;

// <summary>
// Class for storing FITS Image extension information
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFITSImgParser.cc">
// </reviewed>

// <prerequisite>
// </prerequisite>

// <etymology>
// The class stores the essential information on a FITS
// image extension.
// </etymology>

// <synopsis>
// The class stores the essential information on a FITS image extension,
// which is the FITS file name, the extension name, the extension version,
// the index within the FITS file.
// </synopsis>
//
// <example>
// <srcblock>
//    FITSImgParser fitsImg("in.fits");
//    FITSExtInfo extinfo("in.fits", 0, "SCI", 1, True);
//    Int index = fitsImg.get_index(extinfo);              // get the index of extension "[SCI, 1]"
// </srcblock>
// </example>
//
// <motivation>
// Helper class for accessing multi-extension FITS files.
// </motivation>
//
//# <todo asof="2011/02/17">
//# </todo>
class FITSExtInfo
{
public:
	// Construct the object
	FITSExtInfo(const String &name, const uInt &extindex, const String &extname,
			const Int &extversion, const Bool &hasdata);

	// Construct the object
	FITSExtInfo()
	{
		FITSExtInfo("", 0, "", 0, False);
	};

	// Copy constructor (reference semantics)
	FITSExtInfo(const FITSExtInfo& other);

	// Destructor does nothing.
	~FITSExtInfo();

	// Assignment (reference semantics).
	FITSExtInfo& operator=(const FITSExtInfo& other);

	// Relational operator.
	Bool operator==(const FITSExtInfo &extinfo);

	// All extension information as a string.
	String get_extexpr(void);

	// Return the extension name.
	String get_extname(void){return extname_p;};

	// Return the extension version.
	Int get_extversion(void){return extversion_p;};

	// Return whether there is data.
	Bool has_data(void){return hasdata_p;};

	// Add a list of keywords.
	void add_kwlist(FitsKeywordList &kwlist);

	// Return a keyword.
	FitsKeyword *get_keyword(const String kname){return kwlist_p(kname.c_str());};

private:
	String name_p;
	uInt   extindex_p;
	String extname_p;
	Int    extversion_p;
	Bool   hasdata_p;
	FitsKeywordList kwlist_p;
};

} //# NAMESPACE CASACORE - END

#endif


