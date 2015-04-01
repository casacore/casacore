//# tFITSImage.cc:  test the FITSImage class
//# Copyright (C) 1994,1995,1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>

#include <casacore/images/Images/FITSErrorImage.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Bool allNear (const Array<Float>& data, const Array<Bool>& dataMask,
              const Array<Float>& fits, const Array<Bool>& fitsMask, Float tol=1.0e-5);

Bool cleanZero(Array<Float>& data, Array<Bool>& dataMask);

void printArray(const FITSErrorImage fitsErrImage, const Int size);

int main (int argc, const char* argv[])
{
try {

   LogIO os(LogOrigin("tFITSErrorImage", "main()", WHERE));

   // Get inputs
   Input inputs(1);
   inputs.create("in", "", "Input FITS file");
   inputs.create("hdunum", "0", "HDU number");
   inputs.create("print", "F", "Print some data");
   inputs.create("size", "5", "Size to print");
   inputs.create("verbose", "F", "Put feedback on screen");

   inputs.readArguments(argc, argv);
   String in          = inputs.getString("in");
   uInt hdunum        = (uInt)inputs.getInt("hdunum");
   const Bool print   = inputs.getBool("print");
   const Int size     = inputs.getInt("size");
   const Bool verbose = inputs.getBool("verbose");

   // Give a default image and extension
   if (in.empty()) {
   	in = "qualityimage.fits";
   }   
   Path p(in);
   if (!hdunum)
	   hdunum = 2;

   // Give some feedback
   if (verbose)
	   cerr << "Input image: " << in << " extension: " << hdunum << endl;

   // test the conversion between error types and String
   AlwaysAssert(FITSErrorImage::stringToErrorType("MSE")==FITSErrorImage::MSE, AipsError);
   AlwaysAssert(FITSErrorImage::errorTypeToString(FITSErrorImage::RMSE)=="RMSE", AipsError);
   AlwaysAssert(FITSErrorImage::stringToErrorType("INVMSE")==FITSErrorImage::INVMSE, AipsError);
   AlwaysAssert(FITSErrorImage::errorTypeToString(FITSErrorImage::INVRMSE)=="INVRMSE", AipsError);
   AlwaysAssert(FITSErrorImage::MSE==FITSErrorImage::DEFAULT, AipsError);
   AlwaysAssert(FITSErrorImage::stringToErrorType("WHATSUP")==FITSErrorImage::UNKNOWN, AipsError);

   //
   // Open FITSErrorImage as a MSE error image,
   // which corresponds to the default
   //
   FITSErrorImage fitsErrImage(in, 0, hdunum);
   fitsErrImage.tempClose();

   // Checking the image type
   AlwaysAssert(fitsErrImage.imageType()=="FITSErrorImage", AipsError);
   if (verbose)
	   cerr << "Checked the image type." << endl;

   // Checking the default error type
   FITSErrorImage::ErrorType defErrType=FITSErrorImage::MSE;
   AlwaysAssert(fitsErrImage.errorType()==defErrType, AipsError);
   if (verbose)
	   cerr << "Checked for ErrorType." << endl;

   // Checking the unit
   Unit unit("Jy/beam");
   AlwaysAssert(fitsErrImage.setUnits(unit), AipsError);
   AlwaysAssert(fitsErrImage.units().getName()=="Jy/beam", AipsError);
   if (verbose)
	   cerr << "Checked setUnits()." << endl;

   // Check some basic mask properties
   fitsErrImage.reopen();
   AlwaysAssert(fitsErrImage.hasPixelMask() == fitsErrImage.isMasked(), AipsError);
   if (fitsErrImage.hasPixelMask()) {
      Lattice<Bool>& pMask = fitsErrImage.pixelMask();
      AlwaysAssert(pMask.shape()==fitsErrImage.shape(), AipsError);
   }
   if (verbose)
	   cerr << "Checked some basic mask properties." << endl;

   // Check some simple methods, mostly implemented
   // in the base classes
   AlwaysAssert(fitsErrImage.getRegionPtr()==0, AipsError);
   AlwaysAssert(fitsErrImage.isWritable()==False, AipsError);
   AlwaysAssert(fitsErrImage.name(False)==p.absoluteName(),AipsError);
   AlwaysAssert(fitsErrImage.ok(), AipsError);
   if (verbose)
	   cerr << "Checked some simple methods." << endl;


   // Print some data and mask values if desired
   fitsErrImage.tempClose();
   if (print)
	   printArray(fitsErrImage, size);

   // Convert from FITS as a comparison
   String error;
   ImageInterface<Float>* pTempImage = 0;
   String imageName;
   if (!ImageFITSConverter::FITSToImage(pTempImage, error,
		   imageName, in, 0, hdunum)) {
	   os << error << LogIO::EXCEPTION;
   }
   if (verbose)
	   cerr << "Converted from FITS as comparison." << endl;


   // Get the data and compare the array, masks and coords
   Array<Float> fitsArray = fitsErrImage.get();
   Array<Float> dataArray = pTempImage->get();
   Array<Bool> fitsMask = fitsErrImage.getMask();
   Array<Bool> dataMask = pTempImage->getMask();
   CoordinateSystem fitsCS = fitsErrImage.coordinates();
   CoordinateSystem dataCS = pTempImage->coordinates();
   delete pTempImage;
   AlwaysAssert(allNear(dataArray, dataMask, fitsArray, fitsMask), AipsError);
   AlwaysAssert(fitsCS.near(dataCS), AipsError);
   if (verbose)
	   cerr << "Compared data, mask as well as coordinate systems." << endl;

   // Test Clone
   // Get the data and compare the array, masks and coords
   ImageInterface<Float>* pFitsImage = fitsErrImage.cloneII();
   Array<Float> fitsArray2  = pFitsImage->get();
   Array<Bool> fitsMask2    = pFitsImage->getMask();
   CoordinateSystem fitsCS2 = pFitsImage->coordinates();
   delete pFitsImage;
   if (verbose)
	   cerr << "Checked the clone operator." << endl;
   AlwaysAssert(allNear(dataArray, dataMask, fitsArray2, fitsMask2), AipsError);
   AlwaysAssert(fitsCS2.near(dataCS), AipsError);
   if (verbose)
	   cerr << "Compared cloned data, mask as well as coordinate systems." << endl;


   // Test copy
   FITSErrorImage fitsErrImage2 = FITSErrorImage(fitsErrImage);
   if (verbose)
	   cerr << "Checked the copy constructor." << endl;
   AlwaysAssert(fitsErrImage2.errorType()==defErrType, AipsError);
   if (verbose)
	   cerr << "Checked for ErrorType in copied instance." << endl;

   // Get the data and compare the array, masks and coords
   Array<Float> fitsArray3  = fitsErrImage2.get();
   Array<Bool> fitsMask3    = fitsErrImage2.getMask();
   CoordinateSystem fitsCS3 = fitsErrImage2.coordinates();
   AlwaysAssert(allNear(dataArray, dataMask, fitsArray3, fitsMask3), AipsError);
   AlwaysAssert(fitsCS2.near(dataCS), AipsError);
   if (verbose)
	   cerr << "Compared copied data, mask and coordinate systems." << endl;

   // Test assignment
   fitsErrImage2 = fitsErrImage;
   if (verbose)
	   cerr << "Checked the assignment operator." << endl;
   AlwaysAssert(fitsErrImage2.errorType()==defErrType, AipsError);
   if (verbose)
	   cerr << "Checked for ErrorType in copied instance." << endl;

   // Get the data and compare the array, masks and coords
   fitsArray3  = fitsErrImage2.get();
   fitsMask3   = fitsErrImage2.getMask();
   fitsCS3     = fitsErrImage2.coordinates();
   AlwaysAssert(allNear(dataArray, dataMask, fitsArray3, fitsMask3), AipsError);
   AlwaysAssert(fitsCS2.near(dataCS), AipsError);
   if (verbose)
	   cerr << "Compared assigned data, mask and coordinate systems." << endl;

   //
   // Open FITSErrorImage as a RMSE error image
   //
   fitsErrImage2 = FITSErrorImage(in, 0, hdunum, FITSErrorImage::RMSE);
   fitsErrImage2.tempClose();
   if (verbose)
	   cerr << "Opened a RMSE error image." << endl;

   // Check the image type and the error type
   AlwaysAssert(fitsErrImage2.imageType()=="FITSErrorImage", AipsError);
   AlwaysAssert(fitsErrImage2.errorType()==FITSErrorImage::RMSE, AipsError);
   if (verbose)
	   cerr << "Checked type and error type" << endl;

   // Get the data and compare the array, masks and coords
   fitsArray3  = fitsErrImage2.get();
   fitsMask3   = fitsErrImage2.getMask();
   fitsCS3     = fitsErrImage2.coordinates();
   Array<Float> tmpData = dataArray*dataArray;
   AlwaysAssert(allNear(tmpData, dataMask, fitsArray3, fitsMask3), AipsError);
   AlwaysAssert(fitsCS2.near(dataCS), AipsError);
   if (verbose)
	   cerr << "Compared data and coordinate systems in RMSE image." << endl;

   // Print some data and mask values if desired
   fitsErrImage2.tempClose();
   if (print)
	   printArray(fitsErrImage2, size);


   //
   // Open FITSErrorImage as a INVMSE (inverse mean squared error) error image
   //
   fitsErrImage2 = FITSErrorImage(in, 0, hdunum, FITSErrorImage::INVMSE);
   fitsErrImage2.tempClose();
   if (verbose)
	   cerr << "Opened a INVMSE error image." << endl;

   // Check the image type and the error type
   AlwaysAssert(fitsErrImage2.imageType()=="FITSErrorImage", AipsError);
   AlwaysAssert(fitsErrImage2.errorType()==FITSErrorImage::INVMSE, AipsError);
   if (verbose)
	   cerr << "Checked type and error type" << endl;

   // Get the data and compare the array, masks and coords
   fitsArray3  = fitsErrImage2.get();
   fitsMask3   = fitsErrImage2.getMask();
   fitsCS3     = fitsErrImage2.coordinates();
   tmpData = (Float)1.0/dataArray; // somehow, the (Float) is essential
   cleanZero(dataArray, dataMask);
   AlwaysAssert(allNear(tmpData, dataMask, fitsArray3, fitsMask3), AipsError);
   AlwaysAssert(fitsCS2.near(dataCS), AipsError);
   if (verbose)
	   cerr << "Compared data and coordinate systems in INVMSE image." << endl;

   // Print some data and mask values if desired
   fitsErrImage2.tempClose();
   if (print)
	   printArray(fitsErrImage2, size);


   //
   // Open FITSErrorImage as a INVRMSE (inverse root mean squared) error image
   //
   fitsErrImage2 = FITSErrorImage(in, 0, hdunum, FITSErrorImage::INVRMSE);
   fitsErrImage2.tempClose();
   if (verbose)
	   cerr << "Opened an INVRMSE error image." << endl;

   // Check the image type and the error type
   AlwaysAssert(fitsErrImage2.imageType()=="FITSErrorImage", AipsError);
   AlwaysAssert(fitsErrImage2.errorType()==FITSErrorImage::INVRMSE, AipsError);
   if (verbose)
	   cerr << "Checked type and error type" << endl;

   // Get the data and compare the array, masks and coords
   fitsArray3  = fitsErrImage2.get();
   fitsMask3   = fitsErrImage2.getMask();
   fitsCS3     = fitsErrImage2.coordinates();
   tmpData = (Float)1.0/(dataArray*dataArray); // somehow, the (Float) is essential
   AlwaysAssert(allNear(tmpData, dataMask, fitsArray3, fitsMask3), AipsError);
   AlwaysAssert(fitsCS2.near(dataCS), AipsError);
   if (verbose)
	   cerr << "Compared data and coordinate systems in an INVRMSE image." << endl;

   // Print some data and mask values if desired
   fitsErrImage2.tempClose();
   if (print)
	   printArray(fitsErrImage2, size);

   cerr << "ok " << endl;

} catch (AipsError x) {
   cerr << "aipserror: error " << x.getMesg() << endl;
   return 1;
}
  return 0;
}

Bool allNear (const Array<Float>& data, const Array<Bool>& dataMask,
              const Array<Float>& fits, const Array<Bool>& fitsMask,
              Float tol)
{
   Bool deletePtrData, deletePtrDataMask, deletePtrFITS, deletePtrFITSMask;
   const Float* pData = data.getStorage(deletePtrData);
   const Float* pFITS = fits.getStorage(deletePtrFITS);
   const Bool* pDataMask = dataMask.getStorage(deletePtrDataMask);
   const Bool* pFITSMask = fitsMask.getStorage(deletePtrFITSMask);
//
   for (uInt i=0; i<data.nelements(); i++) {
      if (pDataMask[i] != pFITSMask[i]) {
         cerr << "masks differ" << endl;
         return False;
      }
      //if (pDataMask[i]) {
      if (pFITSMask[i]) {
         if (!near(pData[i], pFITS[i], tol)) {
            cerr << "data differ, tol = " << tol << endl;
            cerr << pData[i] << ", " << pFITS[i] << endl;
            return False;
         }
      }
   }
//
   data.freeStorage(pData, deletePtrData);
   dataMask.freeStorage(pDataMask, deletePtrDataMask);
   fits.freeStorage(pFITS, deletePtrFITS);
   fitsMask.freeStorage(pFITSMask, deletePtrFITSMask);
   return True;
}

Bool cleanZero(Array<Float>& data, Array<Bool>& dataMask)
{
	Bool deletePtrData, deletePtrDataMask;
	const Float* pData = data.getStorage(deletePtrData);
	Bool*  pDataMask = dataMask.getStorage(deletePtrDataMask);

	//
	for (uInt i=0; i<data.nelements(); i++) {
		if (pDataMask[i] && pData[i]== (Float)0.0) {
			pDataMask[i] = False;
		}
	}

	//
	data.freeStorage(pData, deletePtrData);
	return True;
}

void printArray(const FITSErrorImage fitsErrImage, const Int size)
{
	IPosition start (fitsErrImage.ndim(),0);
	IPosition shape(fitsErrImage.shape());
		for (uInt i=0; i<fitsErrImage.ndim(); i++) {
			if (shape(i) > size) shape(i) = size;
	    }
	cerr << "Data = " << fitsErrImage.getSlice(start, shape) << endl;
	cerr << "Mask = " << fitsErrImage.getMaskSlice(start, shape) << endl;
}


