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

#include <casacore/images/Images/FITSImage.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Bool allNear (const Array<Float>& data, const Array<Bool>& dataMask,
              const Array<Float>& fits, const Array<Bool>& fitsMask, Float tol=1.0e-5);

int main (int argc, const char* argv[])
{
try {

   LogIO os(LogOrigin("tFITSExtImageII", "main()", WHERE));

// Get inputs

   Input inputs(1);
   inputs.create("in", "", "Input FITS file");
   inputs.create("hdunum", "0", "HDU number");
   inputs.create("print", "F", "Print some data");
   inputs.create("size", "5", "Size to print");
//
   inputs.readArguments(argc, argv);
   String in = inputs.getString("in");
   uInt hdunum = 0;
   const Bool print = inputs.getBool("print");
   const Int size = inputs.getInt("size");

// Give a default image and extension
   if (in.empty()) {
     in = "mexinputtest.fits[sci,2]";
   }
   Path p(FITSImage::get_fitsname(in));

// Get the HDU index
   hdunum = FITSImage::get_hdunum(in);

// Open FITSImage

   FITSImage fitsImage(in);
   fitsImage.tempClose();


   AlwaysAssert(fitsImage.imageType()=="FITSImage", AipsError);
   Unit unit("Jy/beam");
   AlwaysAssert(fitsImage.setUnits(unit), AipsError);
   AlwaysAssert(fitsImage.units().getName()=="Jy/beam", AipsError);
   Record rec;
   rec.define("field1", 0.0);
   rec.define("field2", "doggies");
   AlwaysAssert(fitsImage.setMiscInfo(rec), AipsError);
   fitsImage.reopen();
   Record rec2 = fitsImage.miscInfo();
   AlwaysAssert(rec.isDefined("field1"), AipsError);  
   AlwaysAssert(rec.isDefined("field2"), AipsError);  
   AlwaysAssert(rec.asFloat("field1")==0.0, AipsError);
   AlwaysAssert(rec.asString("field2")=="doggies", AipsError);
   AlwaysAssert(fitsImage.hasPixelMask() == fitsImage.isMasked(), AipsError);
   if (fitsImage.hasPixelMask()) {
      Lattice<Bool>& pMask = fitsImage.pixelMask();
      AlwaysAssert(pMask.shape()==fitsImage.shape(), AipsError);
   }
   AlwaysAssert(fitsImage.getRegionPtr()==0, AipsError);
   AlwaysAssert(fitsImage.isWritable()==False, AipsError);
   AlwaysAssert(fitsImage.name(False)==p.absoluteName(),AipsError);
   AlwaysAssert(fitsImage.ok(), AipsError);
//
   fitsImage.tempClose();

   if (print) {
      IPosition start (fitsImage.ndim(),0);
      IPosition shape(fitsImage.shape());
      for (uInt i=0; i<fitsImage.ndim(); i++) {
         if (shape(i) > size) shape(i) = size;
      }
      cerr << "Data = " << fitsImage.getSlice(start, shape) << endl;
      cerr << "Mask = " << fitsImage.getMaskSlice(start, shape) << endl;
   }


// Convert from FITS as a comparison

   String error;
   ImageInterface<Float>* pTempImage = 0;
   String imageName;
   if (!ImageFITSConverter::FITSToImage(pTempImage, error, 
                                        imageName, FITSImage::get_fitsname(in), 0, hdunum)) {
      os << error << LogIO::EXCEPTION;
   }
//
   Array<Float> fitsArray = fitsImage.get();
   Array<Float> dataArray = pTempImage->get();
   Array<Bool> fitsMask = fitsImage.getMask();
   Array<Bool> dataMask = pTempImage->getMask();
   CoordinateSystem fitsCS = fitsImage.coordinates();
   CoordinateSystem dataCS = pTempImage->coordinates();
   delete pTempImage;
//
   AlwaysAssert(fitsCS.near(dataCS), AipsError);
   AlwaysAssert(allNear(dataArray, dataMask, fitsArray, fitsMask), AipsError);


// Test Clone

   ImageInterface<Float>* pFitsImage = fitsImage.cloneII();
   Array<Float> fitsArray2 = pFitsImage->get();
   Array<Bool> fitsMask2 = pFitsImage->getMask();
   CoordinateSystem fitsCS2 = pFitsImage->coordinates();
   delete pFitsImage;
//
   AlwaysAssert(allNear(dataArray, dataMask, fitsArray2, fitsMask2), AipsError);
   AlwaysAssert(fitsCS2.near(dataCS), AipsError);
//

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


