//# FITSErrorImage.h: Class providing native access to FITS images
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

#ifndef IMAGES_FITSERRORIMAGE_H
#define IMAGES_FITSERRORIMAGE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Array;

//
class MaskSpecifier;
class IPosition;
class Slicer;

// <summary>
// Class providing native access to FITS Error images.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tFITSErrorImage.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=FITSImage>FITSImage</linkto>
//   <li> <linkto class=FITSMask>FITSMask</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis> 
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// This provides native access to FITS error images.
// </motivation>

//# <todo asof="2011/08/17">
//# </todo>

class FITSErrorImage: public FITSImage
{
public: 

	// The enum describes which types of error images exist. The type is fixed
	// during object creation and can not be changed at a later time.
	enum ErrorType
	{
		MSE,          // the values are "mean squared error" (=variance)
		RMSE,         // the values are "root mean squared error" (=sigma)
		INVMSE,       // the values are inverse "means squared error"
		INVRMSE,      // the values are inverse "root mean squared error"
		UNKNOWN,      // unknown type
		DEFAULT=MSE
	};

	// Construct a FITSImage from the disk FITS file name  and extension and apply mask.
  explicit FITSErrorImage(const String& name, uInt whichRep=0, uInt whichHDU=0, FITSErrorImage::ErrorType errtype=MSE);

  // Construct a FITSImage from the disk FITS file name and extension and apply mask or not.
  FITSErrorImage(const String& name, const MaskSpecifier& mask, uInt whichRep=0, uInt whichHDU=0, FITSErrorImage::ErrorType errtype=MSE);

  // Copy constructor (reference semantics)
  FITSErrorImage(const FITSErrorImage& other);

  // Destructor
  virtual ~FITSErrorImage();

  // Assignment (reference semantics)
  FITSErrorImage& operator=(const FITSErrorImage& other);

  // Make a copy of the object with new (reference semantics).
  virtual ImageInterface<Float>* cloneII() const;

  // Get the image type (returns "FITSErrorImage").
  virtual String imageType() const;

  // Do the actual get of the data.
  // Returns False as the data do not reference another Array
  virtual Bool doGetSlice (Array<Float>& buffer, const Slicer& theSlice);

  // The FITSImage is not writable, so this throws an exception.
  virtual void doPutSlice (const Array<Float>& sourceBuffer,
			   const IPosition& where,
			   const IPosition& stride);

  // Return the error type.
  virtual FITSErrorImage::ErrorType errorType() const
		  {return errtype_p;};

  // Convert an image type to String.
  static FITSErrorImage::ErrorType stringToErrorType(String errorTypeStr);

  // Convert a String to an image type.
  static String errorTypeToString(FITSErrorImage::ErrorType errType);

private:

  // Set the correct masking.
  void setupMask();

  Array<Float>              buffer_p;
  FITSErrorImage::ErrorType errtype_p;
};



} //# NAMESPACE CASACORE - END

#endif


