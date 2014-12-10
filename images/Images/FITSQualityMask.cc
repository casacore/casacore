//# FITSMask.cc: an on-the-fly mask for FITS images
//# Copyright (C) 1997,1998,1999,2000,2001,2002
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


#include <casacore/images/Images/FITSQualityMask.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/images/Images/FITSErrorImage.h>

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

FITSQualityMask::FITSQualityMask(FITSImage *fitsData, FITSErrorImage *fitsError)
: itsFitsData(fitsData),
  itsFitsError(fitsError),
  itsFilterZero(False)

{
	// check the shapes are equal!
	AlwaysAssert(itsFitsData->shape()==itsFitsError->shape(), AipsError);
}

FITSQualityMask::FITSQualityMask (const FITSQualityMask& other)
: Lattice<Bool>(other),
  itsFitsData(other.itsFitsData),
  itsFitsError(other.itsFitsError),
  itsFilterZero(other.itsFilterZero)
{
}

FITSQualityMask::~FITSQualityMask()
{}

FITSQualityMask& FITSQualityMask::operator= (const FITSQualityMask& other)
{
	if (this != &other) {
		itsFitsData = other.itsFitsData;
		itsFitsError = other.itsFitsError;
		itsBuffer.resize();
		itsBuffer = other.itsBuffer.copy();
		itsFilterZero = other.itsFilterZero;
	}
	return *this;
}

Lattice<Bool>* FITSQualityMask::clone() const
{
  return new FITSQualityMask (*this);
}


Bool FITSQualityMask::isWritable() const
{
  return False;
}

IPosition FITSQualityMask::shape() const
{
	IPosition data_shape=itsFitsData->shape();
	IPosition mm_shape(data_shape.nelements()+1);

	// set the shape
	for (uInt index=0; index<data_shape.nelements(); index++)
		mm_shape(index) = data_shape(index);
	mm_shape(mm_shape.nelements()-1)=2;

	return mm_shape;
}

Bool FITSQualityMask::doGetSlice (Array<Bool>& buffer, const Slicer& section)
{
	// get the section dimension
	IPosition shp = section.length();
	uInt ndim=section.ndim();

	// resize the buffer
	if (!buffer.shape().isEqual(shp)) buffer.resize(shp);

	// set the in all except the last dimension
	IPosition tmpStart(ndim-1);
	IPosition tmpEnd(ndim-1);
	IPosition tmpStride(ndim-1);
	for (uInt index=0; index<ndim-1; index++) {
		tmpStart(index)  = section.start()(index);
		tmpEnd(index)    = section.end()(index);
		tmpStride(index) = section.stride()(index);
	}

	// generate a slicer for all except the last dimension;
	// used for getting the data from the individual extensions
	Slicer subSection(tmpStart, tmpEnd, tmpStride, Slicer::endIsLast);

	// analyze the request
	if (section.start()(ndim-1) != section.end()(ndim-1)){

		// data and error is requested
		Array<Bool> subData;
		Array<Bool> subError;
		Array<Bool> tmp;

		// prepare the call
		// for data mask
		IPosition subStart(ndim);
		IPosition subEnd(ndim);
		for (uInt index=0; index<ndim-1; index++) {
			subStart(index)  = 0;
			subEnd(index)    = shp(index)-1;
		}
		subStart(ndim-1) = 0;
		subEnd(ndim-1)   = 0;

		// re-size the buffer
		if (!subData.shape().isEqual(subSection.length())) subData.resize(subSection.length());

		// get the data mask
		//reopenDataIfNeeded();
		itsFitsData->doGetMaskSlice(subData, subSection);
		//tempCloseData();

		// insert the retrieved data
		// into the output buffer
		tmp.reference(buffer(subStart, subEnd));
		tmp=subData.addDegenerate(1);

		// prepare the call
		// for error mask values
		subStart(ndim-1) = 1;
		subEnd(ndim-1)   = 1;

		// re-size the buffer
		if (!subError.shape().isEqual(subSection.length())) subError.resize(subSection.length());

		// get the error mask
		//reopenErrorIfNeeded();
		itsFitsError->doGetMaskSlice(subError, subSection);
		//tempCloseError();

		// insert the retrieved data
		// into the output buffer
		tmp.reference(buffer(subStart, subEnd));
		tmp=subError.addDegenerate(1);
	}

	else if (section.start()(ndim-1)==0) {

		// only data is requested
		Array<Bool> subData;
		Array<Bool> tmp;

		// prepare the call
		// for data mask values
		IPosition subStart(ndim);
		IPosition subEnd(ndim);
		for (uInt index=0; index<ndim-1; index++) {
			subStart(index)  = 0;
			subEnd(index)    = shp(index)-1;
		}
		subStart(ndim-1) = 0;
		subEnd(ndim-1)   = 0;

		// re-size the buffer
		if (!subData.shape().isEqual(subSection.length())) subData.resize(subSection.length());

		// get the data mask
		//reopenDataIfNeeded();
		itsFitsData->doGetMaskSlice(subData, subSection);
		//tempCloseData();

		// insert the retrieved data
		// into the output buffer
		tmp.reference(buffer(subStart, subEnd));
		tmp=subData.addDegenerate(1);

	}

	else if (section.start()(ndim-1)==1) {

		// only error is requested
		Array<Bool> subError;
		Array<Bool> tmp;

		// prepare the call
		// for error mask values
		IPosition subStart(ndim);
		IPosition subEnd(ndim);
		for (uInt index=0; index<ndim-1; index++) {
			subStart(index)  = 0;
			subEnd(index)    = shp(index)-1;
		}
		subStart(ndim-1) = 0;
		subEnd(ndim-1)   = 0;

		// re-size the buffer
		if (!subError.shape().isEqual(subSection.length())) subError.resize(subSection.length());

		// get the error mask
		//reopenErrorIfNeeded();
		itsFitsError->doGetMaskSlice(subError, subSection);
		//tempCloseError();

		// insert the retrieved data
		// into the output buffer
		tmp.reference(buffer(subStart, subEnd));
		tmp=subError.addDegenerate(1);
	}

	/* maybe this is needeed?
	// Apply the according filtering
	if (!itsFilterZero)
		ok = filterNaN(pMask, pData, mask.nelements());
	else
		ok = filterZeroNaN(pMask, pData, mask.nelements());

	itsBuffer.freeStorage(pData, deletePtrD);
	mask.putStorage(pMask, deletePtrM);
	*/

	return False;            // Not a reference
}

void FITSQualityMask::doPutSlice (const Array<Bool>&,
                           const IPosition&, 	
                           const IPosition&)
{
   throw(AipsError("FITSQualityMask object is not writable"));
}

void FITSQualityMask::setFilterZero(Bool filterZero)
{
	itsFilterZero = filterZero;
}

Bool FITSQualityMask::filterNaN(Bool *pMask, const Float *pData, const uInt nelems)
{
	// loop over all elements
	for (uInt i=0; i<nelems; i++) {
		// set defaults;
		// blanked values are NaNs.
		pMask[i] = True;
		if (isNaN(pData[i])) pMask[i] = False;
	}

	return True;
}

Bool FITSQualityMask::filterZeroNaN(Bool *pMask, const Float *pData, const uInt nelems)
{
	// loop over all elements
	for (uInt i=0; i<nelems; i++) {
		// set defaults;
		// blanked values are NaNs and "0.0"
		pMask[i] = True;
		if (isNaN(pData[i]) || pData[i] == (Float)0.0) pMask[i] = False;
	}
	return True;
}

} //# NAMESPACE CASACORE - END

