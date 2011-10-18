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

#ifndef IMAGES_FITSIMGPARSER_H
#define IMAGES_FITSIMGPARSER_H

#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/String.h>
//#include <casa/Utilities/DataType.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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
//  This class parses through a FITS image and stores for
//  each extension the essential access information.
// </etymology>

// <synopsis>
// The class parses through a FITS image and extracts information
// on its extensions. Via its methods it provides information on
// whether a certain extension exists in the image.
// </synopsis>

// <example>
// <srcblock>
//    FITSImgParser fitsImg("in".fits);
//    uInt numHDU    = fitsImg.get_numhdu();                // get the total number of HDU's
//    uInt firstdata = fitsImg.get_firstdata_index();       // get the first HDU with data
//    String allExts = fitsImg.get_ext_list(String("\n"));  // get a string representation of all extensions
//    FITSImage im("in.fits");
// </srcblock>
// </example>

// <motivation>
// Investigate and select FITS extensions
// </motivation>

//# <todo asof="2011/02/17">
//# </todo>

class FITSImgParser
{
public: 
  // Construct a parser from the FITS file
  FITSImgParser(const String& name);

  // Destructor not much
  ~FITSImgParser();

  // Returns the name of the disk file.
  String fitsname (Bool stripPath=False) const;

  // Identify the index of an extension
  Int  get_index(const FITSExtInfo &extinfo);

  // Get the index of the first extension with data
  uInt get_firstdata_index(void);

  // Get the number of extensions
  uInt get_numhdu(void) { return numhdu_p;}

  // Get a string representation of an instance
  String get_ext_list(const String &delimiter);

private:  
  String         name_p;
  uInt           numhdu_p;
  FITSExtInfo   *extensions_p;

  // Setup the object (used by constructors).
  void setup(void);

  // Get the information on an extension
  void process_extension(HeaderDataUnit *h, const uInt &extindex);
};


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
// The class stores the essential information on a FITS
// image extension, which is the FITS file name, the extension
// name, the extension version, the index within the FITS file.
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

	// Destructor does nothing
	~FITSExtInfo();

	// Relational operator
	Bool operator==(const FITSExtInfo &extinfo);

	// All extension information as a string
	String get_extexpr(void);

	// Return whether there is data
	Bool has_data(void){return hasdata_p;};

private:
	String name_p;
	uInt   extindex_p;
	String extname_p;
	Int    extversion_p;
	Bool   hasdata_p;
};

} //# NAMESPACE CASA - END

#endif


