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

#include <aips/aips.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Containers/Record.h>
#include <aips/Inputs/Input.h>
#include <aips/OS/Path.h>
#include <aips/Tasking/Aipsrc.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>

#include <trial/Images/FITSImage.h>
#include <trial/Images/ImageInterface.h>
#include <trial/Images/ImageFITSConverter.h>
#include <trial/Coordinates/CoordinateSystem.h>

#include <iostream.h>


main (int argc, char **argv)
{
try {

   LogIO os(LogOrigin("tFITSImage", "main()", WHERE));

// Get inputs

   Input inputs(1);
   inputs.create("in", "", "Input FITS file");
   inputs.create("print", "F", "Print some data");
//
   inputs.readArguments(argc, argv);
   String in = inputs.getString("in");
   const Bool print = inputs.getBool("print");
//
   if (in.empty()) {
     String root = Aipsrc::aipsRoot();
     in = root + "/data/demo/Images/imagetestimage.fits";
   }   
   Path p(in);

// Open FITSImage

   FITSImage fitsImage(in);
   AlwaysAssert(fitsImage.imageType()=="FITSImage", AipsError);
   Unit unit("Jy/beam");
   AlwaysAssert(fitsImage.setUnits(unit), AipsError);
   AlwaysAssert(fitsImage.units().getName()=="Jy/beam", AipsError);
   Record rec;
   rec.define("field1", 0.0);
   rec.define("field2", "doggies");
   AlwaysAssert(fitsImage.setMiscInfo(rec), AipsError);
   Record rec2 = fitsImage.miscInfo();
   AlwaysAssert(rec.isDefined("field1"), AipsError);  
   AlwaysAssert(rec.isDefined("field2"), AipsError);  
   AlwaysAssert(rec.asFloat("field1")==0.0, AipsError);
   AlwaysAssert(rec.asString("field2")=="doggies", AipsError);
   AlwaysAssert(fitsImage.isMasked(), AipsError);
   AlwaysAssert(fitsImage.hasPixelMask(), AipsError);
   {
      const Lattice<Bool>& pMask = fitsImage.pixelMask();
      AlwaysAssert(pMask.shape()==fitsImage.shape(), AipsError);
   }
   {
      Lattice<Bool>& pMask = fitsImage.pixelMask();
      AlwaysAssert(pMask.shape()==fitsImage.shape(), AipsError);
   }
   AlwaysAssert(fitsImage.getRegionPtr()==0, AipsError);
   AlwaysAssert(fitsImage.isWritable()==False, AipsError);
   AlwaysAssert(fitsImage.name(False)==p.absoluteName(),AipsError);
   AlwaysAssert(fitsImage.ok(), AipsError);


// Convert from FITS as a comparison

   String error;
   ImageInterface<Float>* pTempImage = 0;
   String imageName;
   if (!ImageFITSConverter::FITSToImage(pTempImage, error, 
                                        imageName, in, 0)) {
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
   AlwaysAssert(allNear(fitsArray, dataArray, 1.0e-6), AipsError);
   AlwaysAssert(allEQ(fitsMask, dataMask), AipsError);
   AlwaysAssert(fitsCS.near(dataCS), AipsError);
//
   if (print) {
      IPosition start (fitsImage.ndim(),0);
      IPosition size(fitsImage.shape());
      for (uInt i=0; i<fitsImage.ndim(); i++) {
         if (size(i) > 5) size(i) = 5;
      }
      cerr << "Data = " << fitsImage.getSlice(start, size) << endl;
      cerr << "Mask = " << fitsImage.getMaskSlice(start, size) << endl;
   }
//
   cerr << "ok " << endl;

} catch (AipsError x) {
   cerr << "aipserror: error " << x.getMesg() << endl;
   exit(1);
}
  exit(0);
}


