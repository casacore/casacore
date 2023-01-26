//# FITSErrorImage.cc: Class providing native access to FITS images
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

#include <casacore/images/Images/FITSErrorImage.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/lattices/LRegions/FITSMask.h>

#include <casacore/casa/iostream.h>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

FITSErrorImage::FITSErrorImage (const String& name, uint32_t whichRep, uint32_t whichHDU, FITSErrorImage::ErrorType errtype)
: FITSImage(name, whichRep, whichHDU),
  errtype_p(errtype)
{
	setupMask();
}

FITSErrorImage::FITSErrorImage (const String& name, const MaskSpecifier& maskSpec, uint32_t whichRep, uint32_t whichHDU, FITSErrorImage::ErrorType errtype)
: FITSImage (name, maskSpec, whichRep, whichHDU),
  errtype_p(errtype)
{
	setupMask();
}

FITSErrorImage::FITSErrorImage (const FITSErrorImage& other)
: FITSImage(other),
  errtype_p(other.errtype_p)
{
	setupMask();
}
 
FITSErrorImage& FITSErrorImage::operator=(const FITSErrorImage& other)
// 
// Assignment. Uses reference semantics
//
{
   if (this != &other) {
      FITSImage::operator= (other);

      errtype_p = other.errtype_p;
      setupMask();
   }
   return *this;
} 

FITSErrorImage::~FITSErrorImage()
{
}

ImageInterface<float>* FITSErrorImage::cloneII() const
{
   return new FITSErrorImage (*this);
}


String FITSErrorImage::imageType() const
{
   return "FITSErrorImage";
}

bool FITSErrorImage::doGetSlice(Array<float>& buffer,
                           const Slicer& section)
{
	// set up the arrays
	IPosition shp = section.length();
	if (!buffer.shape().isEqual(shp)) buffer.resize(shp);
	if (!buffer_p.shape().isEqual(shp)) buffer_p.resize(shp);

	// get the data+....////
	FITSImage::doGetSlice(buffer_p, section);

	//
	bool deletePtrD;
	const float* pData = buffer_p.getStorage(deletePtrD);
	bool deletePtrM;
	float* pBuffer = buffer.getStorage(deletePtrM);

	// depending on the error type,
	// fill the resulting array with variance values
	switch (errtype_p)
	{
	case MSE:
		for (uint32_t i=0; i<buffer.nelements(); i++)
			pBuffer[i] = pData[i];
		break;
	case RMSE:
		for (uint32_t i=0; i<buffer.nelements(); i++)
			pBuffer[i] = pData[i]*pData[i];
		break;
	case INVMSE:
		for (uint32_t i=0; i<buffer.nelements(); i++)
			if (pData[i])
				pBuffer[i] = 1.0/pData[i];
			else
				pBuffer[i] = NAN;
		break;
	case INVRMSE:
		for (uint32_t i=0; i<buffer.nelements(); i++)
			if (pData[i])
				pBuffer[i] = 1.0/(pData[i]*pData[i]);
			else
				pBuffer[i] = NAN;
		break;
	default:
		// this should not happen:
		throw (AipsError ("FITSErrorImage::doGetSlice - "
				"can not give values for this error type!"));
		break;
	}

	// re-shuffle the arrays
	buffer_p.freeStorage(pData, deletePtrD);
	buffer.putStorage(pBuffer, deletePtrM);

	return false;                            // Not a reference
}

void FITSErrorImage::doPutSlice (const Array<float>&, const IPosition&,
                            const IPosition&)
{
	// the image is read-only
	throw (AipsError ("FITSErrorImage::putSlice - "
			"is not possible as FITSErrorImage is not writable"));
}

FITSErrorImage::ErrorType FITSErrorImage::stringToErrorType(const String errorTypeStr){
	// convert the string to an error type
	if (!errorTypeStr.compare("MSE"))
		return MSE;
	else if (!errorTypeStr.compare("RMSE"))
		return RMSE;
	else if (!errorTypeStr.compare("INVMSE"))
		return INVMSE;
	else if (!errorTypeStr.compare("INVRMSE"))
		return INVRMSE;
	else
		return UNKNOWN;
}

String FITSErrorImage::errorTypeToString(const FITSErrorImage::ErrorType errType){
	// convert the error type to a string
	switch(errType) {
	case MSE:
		return "MSE";
	case RMSE:
		return "RMSE";
	case INVMSE:
		return "INVMSE";
	case INVRMSE:
		return "INVRMSE";
	case UNKNOWN:
		return "UNKNOWN";
	default:
		return ""; // unknown
	}
}

void FITSErrorImage::setupMask()
{
	// for the inverse error types, switch on
	// the masking of values 0.0 (in the FITS file)
	if (errtype_p == INVMSE || errtype_p == INVRMSE){
		setMaskZero(true);
	}
	// throw an error for type "UNKNOWN", since
	// it is now known what to do.
	else if (errtype_p == UNKNOWN)
		throw (AipsError ("FITSErrorImage::setupMask - "
				"error type UNKNOWN is not accepted!"));
	}
} //# NAMESPACE CASACORE - END

