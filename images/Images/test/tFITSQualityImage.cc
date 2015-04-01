//# tFITSQualityImage.cc:  test the tFITSQualityImage class
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
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>

#include <casacore/measures/Measures/Quality.h>

#include <casacore/images/Images/FITSQualityImage.h>
#include <casacore/images/Images/FITSImage.h>
#include <casacore/images/Images/FITSErrorImage.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageStatistics.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Bool allNear (const Array<Float>& data, const Array<Bool>& dataMask,
		const Array<Float>& fits, const Array<Bool>& fitsMask, Float tol=1.0e-5);
Bool checkRecFieldString(String &error, const RecordInterface &theRec, const String &theField, const String &theValue);
Bool testQualFITSInfo(const TableRecord &dataInfo, const TableRecord &errorInfo, const String &sciHDU, const String &errHDU, const String &errType);
Bool testQualImg(FITSQualityImage &fQualImg, const String &in, const uInt &hdu_sci,
		 const uInt &hdu_err, const Bool &print, const Int &size);

template <class T> void printArray (T array, Int size, String pre="printArray");

int main (int argc, const char* argv[])
{
try {

   LogIO os(LogOrigin("tFITSQualityImage", "main()", WHERE));

// Get inputs

   Input inputs(1);
   inputs.create("in_fits", "",  "Input FITS file");
   inputs.create("in_ext", "",  "Input FITS extension expression");
   inputs.create("hdu_sci", "1", "HDU number");
   inputs.create("hdu_err", "2", "HDU number");
   inputs.create("print",   "F", "Print some data");
   inputs.create("size",    "5", "Size to print");
//
   inputs.readArguments(argc, argv);
   String in_fits    = inputs.getString("in_fits");
   String in_ext     = inputs.getString("in_ext");
   const uInt hdu_sci = inputs.getInt("hdu_sci");
   const uInt hdu_err = inputs.getInt("hdu_err");
   const Bool print   = inputs.getBool("print");
   const Int size     = inputs.getInt("size");
//
   if (in_fits.empty()) {
   	in_fits = "qualityimage.fits";
   }   
   Path p(in_fits);

   if (in_ext.empty()) {
   	// for the entire format including ask images use this:
   	//in_ext = "qualityimage.fits[IFU1.SCI,IFU1.ERR,IFU1.DQ]";

   	// for data and error extension only use this:
   	in_ext = "qualityimage.fits[IFU1.SCI,IFU1.ERR]";
   }
   Path pII(FITSImage::get_fitsname(in_ext));

   // create the quality image with the extension indices
   FITSQualityImage fitsQI(in_fits, hdu_sci, hdu_err);
   {
   	// check the file names
   	if (fitsQI.name(False) != p.absoluteName()){
   		String msg = String("The names differ: " + fitsQI.name(False) + " <<>> " + p.absoluteName());
           throw(AipsError(msg));
   	}
   }

   {

   	// create the default header information for data and error
   	TableRecord dataInfo, errorInfo, miscInfo;
   	String sciHDU("DATA"), errHDU("ERROR"), errType("MSE");
   	String error;
   	// get the default values for data-info and error-info
   	if (!FITSQualityImage::qualFITSInfo(error, dataInfo, errorInfo, miscInfo)){
   		String msg = String("General problem to define the miscInfo for the data and error extension!");
   		throw(AipsError(msg));
   	}

   	// check the data- and error-header information
   	testQualFITSInfo(dataInfo, errorInfo, sciHDU, errHDU, errType);
   }

   {
   	// create the header information for data and error
   	// with dedicated extension names
   	TableRecord dataInfo, errorInfo, miscInfo;
   	String sciHDU("IFU1.SCI"), errHDU("IFU1.ERR"), errType("INVMSE");
   	String error;
   	miscInfo.define("sciextname", sciHDU);
   	miscInfo.define("errextname", errHDU);
   	miscInfo.define("hduclas3", errType);

   	// get the data-info and error-info default values
   	if (!FITSQualityImage::qualFITSInfo(error, dataInfo, errorInfo, miscInfo)){
   		String msg = String("General problem to define the miscInfo for the data and error extension: "+error);
   		throw(AipsError(msg));
   	}

   	// check the data- and error-header information
   	testQualFITSInfo(dataInfo, errorInfo, sciHDU, errHDU, errType);
   }

   {
   	// try to create header information which includes
   	// a non-existing error type. This has to fail!
   	TableRecord dataInfo, errorInfo, miscInfo;
   	String sciHDU("IFU1.SCI"), errHDU("IFU1.ERR");
   	String error;

   	// give a non-existing error type
   	String errType("WHATEVER");
   	miscInfo.define("sciextname", sciHDU);
   	miscInfo.define("errextname", errHDU);
   	miscInfo.define("hduclas3", errType);

   	// this one HAS to bark!
   	if (FITSQualityImage::qualFITSInfo(error, dataInfo, errorInfo, miscInfo)){
   		String msg = String("The error type: " + errType + " was wrongly accepted");
   		throw(AipsError(msg));
   	}
   }

   // create the quality image with the extension expression
   FITSQualityImage fitsQI_II(in_ext);
   {
   	// check the file names
   	if (fitsQI_II.name(False) != pII.absoluteName()){
   		String msg = String("The names differ: " + fitsQI_II.name(False) + " <<>> " + pII.absoluteName());
           throw(AipsError(msg));
   	}
   }

   // test the quality image
   testQualImg(fitsQI_II, in_ext, hdu_sci, hdu_err, print, size);

   // test the FITS info routine
   //testQualFITSInfo();

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
      if (pDataMask[i]) { 
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

Bool checkRecFieldString(String &error, const RecordInterface &theRec, const String &theField, const String &theValue){
	String tmpString;

	// check the existence of the desired field in the record
	if (!(theRec.fieldNumber(theField)>-1 && theRec.type(theRec.fieldNumber(theField)==TpString))){
		error = String("Field "+theField+" does not exits!");
		return False;
	}

	// check the value of the field
	theRec.get(String(theField), tmpString);
	if (tmpString.compare(theValue)){
		error = String("Field "+theField+" has NOT the value: "+theValue+ " but: "+tmpString);
        return False;
	}

	return True;
}

Bool testQualFITSInfo(const TableRecord &dataInfo, const TableRecord &errorInfo,
		const String &sciHDU, const String &errHDU, const String &errType){
	String error;


	// check the data-info for "HDUCLASS"
	if (!checkRecFieldString(error, dataInfo, String("hduclass"), String("ESO"))){
        throw(AipsError(error));
	}

	// check the data-info for "HDUDOC"
	if (!checkRecFieldString(error, dataInfo, String("hdudoc"), String("DICD"))){
        throw(AipsError(error));
	}

	// check the data-info for "HDUVERS"
	if (!checkRecFieldString(error, dataInfo, String("hduvers"), String("DICD version 6"))){
        throw(AipsError(error));
	}

	// check the data-info for "HDUCLAS1"
	if (!checkRecFieldString(error, dataInfo, String("hduclas1"), String("IMAGE"))){
        throw(AipsError(error));
	}

	// check the data-info for "EXTNAME"
	if (!checkRecFieldString(error, dataInfo, String("extname"), sciHDU)){
        throw(AipsError(error));
	}

	// check the data-info for "HDUTYPE"
	if (!checkRecFieldString(error, dataInfo, String("hduclas2"), Quality::name(Quality::DATA))){
        throw(AipsError(error));
	}

	// check the data-info for "ERRDATA"
	if (!checkRecFieldString(error, dataInfo, String("errdata"), errHDU)){
        throw(AipsError(error));
	}

	// check the error-info for "EXTNAME"
	if (!checkRecFieldString(error, errorInfo, String("extname"), errHDU)){
        throw(AipsError(error));
	}

	// check the error-info for "HDUTYPE"
	if (!checkRecFieldString(error, errorInfo, String("hduclas2"), Quality::name(Quality::ERROR))){
        throw(AipsError(error));
	}

	// check the error-info for "SCIDATA"
	if (!checkRecFieldString(error, errorInfo, String("scidata"), sciHDU)){
        throw(AipsError(error));
	}

	// check the error-info for "ERRTYPE"
	if (!checkRecFieldString(error, errorInfo, String("hduclas3"), errType)){
        throw(AipsError(error));
	}

	return True;
}

Bool testQualImg(FITSQualityImage &fitsQI, const String &in, const uInt &hdu_sci,
		 const uInt &hdu_err, const Bool &print, const Int &size)
{
   {
	   // make sure the last axis has two pixels
	   uInt ndim       = fitsQI.ndim();
	   IPosition shape = fitsQI.shape();
	   if (shape(ndim-1)!=2) {
		   String msg = String("Last dimension should be 2 but is: ") + String::toString(shape(ndim-1));
           throw(AipsError(msg));

	   }
   }
   {
	   // make sure a quality coordinate axis exists
	   CoordinateSystem cSys = fitsQI.coordinates();
	   Int qCoord = cSys.findCoordinate(Coordinate::QUALITY);
	   if (qCoord < 0){
		   String msg = String("The image does not contain a quality coordinate axis!");
           throw(AipsError(msg));
	   }
   }
   {
	   // check the image type
	   if (fitsQI.imageType()!="FITSQualityImage"){
		   String msg = String("The image has wrong type!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure the object is masked
	   if (!fitsQI.isMasked()){
		   String msg = String("The object MUST be masked!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure the object has a pixel mask
	   if (!fitsQI.hasPixelMask()){
		   String msg = String("The object MUST have a pixel mask!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure the region pointer returned is 0
	   if (fitsQI.getRegionPtr()!=0){
		   String msg = String("The object MUST return a 0 as region pointer!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure the object is persistent
	   if (!fitsQI.isPersistent()){
		   String msg = String("The object MUST be persistent!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure the object is paged
	   if (!fitsQI.isPaged()){
		   String msg = String("The object MUST be aged!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure the object is not writable
	   if (fitsQI.isWritable()){
		   String msg = String("The object MUST NOT be writable!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure the object is OK
	   if (!fitsQI.ok()){
		   String msg = String("The object MUST be OK!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure there is the right science HDU
	   if (fitsQI.whichDataHDU()!=hdu_sci){
		   String msg = String("The object has the wrong science HDU number!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // make sure there is the right error HDU
	   if (fitsQI.whichErrorHDU()!=hdu_err){
		   String msg = String("The object has the wrong error HDU number!");
		   throw(AipsError(msg));
	   }
   }
   {
   	// get the pointer to the data
   	FITSImage *fromQI = fitsQI.fitsData();

   	// make sure there is the right science HDU
	   if (fromQI->whichHDU()!=hdu_sci){
		   String msg = String("The data in the object has the wrong HDU number!");
		   throw(AipsError(msg));
	   }
   }
   {
   	// get the pointer to the error
   	FITSErrorImage *fromQI = fitsQI.fitsError();

   	// make sure there is the right science HDU
   	if (fromQI->whichHDU()!=hdu_err){
   		String msg = String("The error in the object has the wrong HDU number!");
   		throw(AipsError(msg));
   	}
   }

   // extract the FITS file name
   String fitsname = FITSImage::get_fitsname(in);

   // open the data error extensions independently
   FITSImage fitsDataImg  = FITSImage(fitsname, 0, hdu_sci);
   FITSImage fitsErrorImg = FITSImage(fitsname, 0, hdu_err);

   {
   	// make sure the quality image and the
   	// extension image have the same units
   	if (fitsQI.units() != fitsDataImg.units()){
   		String msg = String("Quality image and it data extension must have identical units!");
   		throw(AipsError(msg));
   	}
   }
   {
	   Array<Float> mmData;
	   Array<Bool>  mmMask;

	   // dimension the start and end points
	   // target data and error values
	   IPosition start (fitsQI.ndim(), 0);
	   IPosition end   (fitsQI.shape()-1);
	   IPosition stride(fitsQI.ndim(), 1);

	   // get a slicer and get the values and mask
	   Slicer mmSection(start, end, stride, Slicer::endIsLast);
	   fitsQI.doGetSlice(mmData, mmSection);
	   fitsQI.doGetMaskSlice(mmMask, mmSection);
	   if (print){
		   printArray (mmData,size, "Data = ");
		   printArray (mmMask,size, "Mask = ");
	   }

	   Array<Float> fitsDData;
	   Array<Bool>  fitsDMask;
	   Array<Float> fitsEData;
	   Array<Bool>  fitsEMask;

	   // dimension the start and end points
	   // for the individual extensions
	   IPosition fStart (fitsQI.ndim()-1, 0);
	   IPosition fStride(fitsQI.ndim()-1, 1);
	   IPosition fEnd (fitsQI.ndim()-1);
	   for (uInt i=0; i<fitsQI.ndim()-1; i++){
		   fEnd(i) = end(i);
	   }

	   // get a slicer for the individual extensions
	   // get the data and masks from the individual extensions
	   Slicer fitsSection(fStart, fEnd, fStride, Slicer::endIsLast);
	   fitsDataImg.doGetSlice(fitsDData, fitsSection);
	   fitsDataImg.doGetMaskSlice(fitsDMask, fitsSection);
	   if (print){
		   printArray (fitsDData,size, "fData = ");
		   printArray (fitsDMask,size, "fMask = ");
	   }
	   fitsErrorImg.doGetSlice(fitsEData, fitsSection);
	   fitsErrorImg.doGetMaskSlice(fitsEMask, fitsSection);
	   if (print){
		   printArray (fitsEData,size, "feData = ");
		   printArray (fitsEMask,size, "feMask = ");
	   }

	   Array<Float> tmpData;
	   Array<Bool>  tmpMask;

	   // extract the data values from the quality
	   // array and compare to the individual extension
	   start(fitsQI.ndim()-1)=0;
	   end(fitsQI.ndim()-1)=0;
	   tmpData.reference(mmData(start, end).nonDegenerate());
	   tmpMask.reference(mmMask(start, end).nonDegenerate());
	   if (print){
		   printArray (tmpData,size, "tmpData: ");
		   printArray (tmpMask,size, "tmpMask: ");
	   }
	   if (!allNear (tmpData, tmpMask, fitsDData, fitsDMask)){
		   String msg = String("The data arrays are not the same!");
		   throw(AipsError(msg));
	   }

	   // extract the error values from the quality
	   // array and compare to the individual extension array
	   start(fitsQI.ndim()-1)=1;
	   end(fitsQI.ndim()-1)=1;
	   tmpData.reference(mmData(start, end).nonDegenerate());
	   tmpMask.reference(mmMask(start, end).nonDegenerate());
	   if (print){
		   printArray (tmpData,size, "tmpData: ");
		   printArray (tmpMask,size, "tmpMask: ");
	   }
	   if (!allNear (tmpData, tmpMask, fitsEData, fitsEMask)){
		   String msg = String("The error arrays are not the same!");
		   throw(AipsError(msg));
	   }
   }
   {
	   Array<Float> mmData;
	   Array<Bool>  mmMask;

	   // dimension the start and end points
	   // target only data values
	   IPosition start (fitsQI.ndim(), 0);
	   IPosition end   (fitsQI.shape()-1);
	   IPosition stride(fitsQI.ndim(), 1);
	   end(fitsQI.ndim()-1) = 0;

	   // get a slicer and get the values and mask
	   Slicer mmSection(start, end, stride, Slicer::endIsLast);
	   fitsQI.doGetSlice(mmData, mmSection);
	   fitsQI.doGetMaskSlice(mmMask, mmSection);
	   if (print){
		   printArray (mmData,size, "DataII = ");
		   printArray (mmMask,size, "MaskII = ");
	   }

	   Array<Float> fitsDData;
	   Array<Bool>  fitsDMask;

	   // dimension the start and end points
	   // for the individual extension
	   IPosition fStart (fitsQI.ndim()-1, 0);
	   IPosition fStride(fitsQI.ndim()-1, 1);
	   IPosition fEnd (fitsQI.ndim()-1);
	   for (uInt i=0; i<fitsQI.ndim()-1; i++){
		   fEnd(i) = end(i);
	   }

	   // get a slicer for the individual extension
	   // get the data and masks from the individual extensions
	   Slicer fitsSection(fStart, fEnd, fStride, Slicer::endIsLast);
	   fitsDataImg.doGetSlice(fitsDData, fitsSection);
	   fitsDataImg.doGetMaskSlice(fitsDMask, fitsSection);
	   if (print){
		   printArray (fitsDData,size, "fDataII = ");
		   printArray (fitsDMask,size, "fMaskII = ");
	   }


	   Array<Float> tmpData;
	   Array<Bool>  tmpMask;

	   // extract the error values from the quality
	   // array and compare to the individual extension array
	   start(fitsQI.ndim()-1)=0;
	   end(fitsQI.ndim()-1)=0;
	   tmpData.reference(mmData(start, end).nonDegenerate());
	   tmpMask.reference(mmMask(start, end).nonDegenerate());
	   if (print){
		   printArray (tmpData,size, "tmpDataII: ");
		   printArray (tmpMask,size, "tmpMaskII: ");
	   }
	   if (!allNear (tmpData, tmpMask, fitsDData, fitsDMask)){
		   String msg = String("The data II arrays are not the same!");
		   throw(AipsError(msg));
	   }
   }
   {
	   Array<Float> mmData;
	   Array<Bool>  mmMask;

	   // dimension the start and end points
	   // target only error values
	   IPosition start (fitsQI.ndim(), 0);
	   IPosition end   (fitsQI.shape()-1);
	   IPosition stride(fitsQI.ndim(), 1);
	   start(fitsQI.ndim()-1) = 1;
	   end(fitsQI.ndim()-1)   = 1;

	   // get a slicer and get the values and mask
	   Slicer mmSection(start, end, stride, Slicer::endIsLast);
	   fitsQI.doGetSlice(mmData, mmSection);
	   fitsQI.doGetMaskSlice(mmMask, mmSection);
	   if (print){
		   printArray (mmData,size, "DataIII = ");
		   printArray (mmMask,size, "MaskIII = ");
	   }

	   Array<Float> fitsEData;
	   Array<Bool>  fitsEMask;

	   // dimension the start and end points
	   // for the individual extensions
	   IPosition fStart (fitsQI.ndim()-1, 0);
	   IPosition fStride(fitsQI.ndim()-1, 1);
	   IPosition fEnd (fitsQI.ndim()-1);
	   for (uInt i=0; i<fitsQI.ndim()-1; i++){
		   fEnd(i) = end(i);
	   }

	   // get a slicer for the individual extensions
	   // get the data and masks from the individual extensions
	   Slicer fitsSection(fStart, fEnd, fStride, Slicer::endIsLast);
	   fitsErrorImg.doGetSlice(fitsEData, fitsSection);
	   fitsErrorImg.doGetMaskSlice(fitsEMask, fitsSection);
	   if (print){
		   printArray (fitsEData,size, "feDataIII = ");
		   printArray (fitsEMask,size, "feMaskIII = ");
	   }


	   Array<Float> tmpData;
	   Array<Bool>  tmpMask;

	   // extract the data values from the quality
	   // array and compare to the individual extension
	   start(fitsQI.ndim()-1)=0;
	   end(fitsQI.ndim()-1)=0;
	   tmpData.reference(mmData(start, end).nonDegenerate());
	   tmpMask.reference(mmMask(start, end).nonDegenerate());
	   if (print){
		   printArray (tmpData,size, "tmpDataIII: ");
		   printArray (tmpMask,size, "tmpMaskIII: ");
	   }
	   if (!allNear (tmpData, tmpMask, fitsEData, fitsEMask)){
		   String msg = String("The error III arrays are not the same!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // test assignment
	   FITSQualityImage  secImg = fitsQI;

	   Array<Float> mmData;
	   Array<Bool>  mmMask;
	   Array<Float> mmDataII;
	   Array<Bool>  mmMaskII;

	   // dimension the start and end points
	   // target data and error values
	   IPosition start (fitsQI.ndim(), 0);
	   IPosition end   (fitsQI.shape()-1);
	   IPosition stride(fitsQI.ndim(), 1);

	   // generate a slicer
	   Slicer mmSection(start, end, stride, Slicer::endIsLast);

	   // get the values and mask of the original image
	   fitsQI.doGetSlice(mmData, mmSection);
	   fitsQI.doGetMaskSlice(mmMask, mmSection);

	   // get the values and mask of the original image
	   secImg.doGetSlice(mmDataII, mmSection);
	   secImg.doGetMaskSlice(mmMaskII, mmSection);
	   if (print){
		   printArray (mmData,size,   "Data orig. = ");
		   printArray (mmMask,size,   "Mask orig. = ");
		   printArray (mmDataII,size, "Data assig.= ");
		   printArray (mmMaskII,size, "Mask assig.= ");
	   }
	   if (!allNear (mmData, mmMask, mmDataII, mmMaskII)){
		   String msg = String("The assigned image has different values than the original!");
		   throw(AipsError(msg));
	   }
   }
   {
	   // test the clone method
	   ImageInterface<Float>* pFitsMM = fitsQI.cloneII();
	   Array<Float> fCloneArray = pFitsMM->get();
	   Array<Bool>  fCloneMask  = pFitsMM->getMask();
	   CoordinateSystem fCloneCS = pFitsMM->coordinates();

	   Array<Float> fOrigArray = fitsQI.get();
	   Array<Bool>  fOrigMask  = fitsQI.getMask();
	   CoordinateSystem fOrigCS = fitsQI.coordinates();
	   if (print){
		   printArray (fOrigArray,size,  "Data orig. = ");
		   printArray (fOrigMask,size,   "Mask orig. = ");
		   printArray (fCloneArray,size, "Data clone = ");
		   printArray (fCloneMask,size,  "Mask clone = ");
	   }

	   if (!allNear (fOrigArray, fOrigMask, fCloneArray, fCloneMask)){
		   String msg = String("The cloned image has different values than the original!");
		   throw(AipsError(msg));
	   }

	   if (!fCloneCS.near(fOrigCS)){
		   String msg = String("The cloned image has different coord-sys than the original!");
		   throw(AipsError(msg));
	   }

	   delete pFitsMM;
   }
   // TODO: add some more quantitative tests for the mask!!
   {
	   // check the pixel mask
	   Lattice<Bool> &theMask = fitsQI.pixelMask();
	   if (theMask.shape() != fitsQI.shape()){
		   String msg = String("The mask shape must be identical to the shape of the data!");
		   throw(AipsError(msg));
	   }
   }
   {
	   FITSImage *fData = fitsQI.fitsData();
	   if (fData->shape() != fitsDataImg.shape()){
		   String msg = String("The FITS data image shapes must be identical!");
		   throw(AipsError(msg));
	   }
   }
   {
	   FITSErrorImage *fError = fitsQI.fitsError();
	   if (fError->shape() != fitsErrorImg.shape()){
		   String msg = String("The FITS error image shapes must be identical!");
		   throw(AipsError(msg));
	   }
   }
   return True;
}


template <class T> void printArray (T array, Int size, String pre)
{
	T tmpArray;
	IPosition start (array.ndim(), 0);
	IPosition end   (array.shape()-1);
	for (uInt i=0; i<array.ndim(); i++)
		if (end(i) > size-1) end(i) = size-1;
	tmpArray.reference(array(start, end));
	cerr << "\n" << pre << tmpArray;
}
