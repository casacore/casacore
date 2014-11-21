//# tMIRIADImage.cc:  test the MIRIADImage class
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

#include <casacore/images/Images/MIRIADImage.h>
#include <casacore/images/Images/ImageInterface.h>
#include <casacore/images/Images/ImageFITSConverter.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Bool allNear (const Array<Float>& data, const Array<Bool>& dataMask,
              const Array<Float>& mir, const Array<Bool>& mirMask, Float tol=1.0e-5);

int main (int argc, const char* argv[])
{
try {

   LogIO os(LogOrigin("tMIRIADImage", "main()", WHERE));

// Get inputs

   Input inputs(1);
   inputs.create("in", "", "Input MIRIAD file");
   inputs.create("print", "F", "Print some data");
   inputs.create("size", "5", "Size to print");
//
   inputs.readArguments(argc, argv);
   String in = inputs.getString("in");
   const Bool print = inputs.getBool("print");
   const Int size = inputs.getInt("size");
//
   if (in.empty()) {
#if 0
     in = "imagetestimage.mir";
#else
     in = "test2.mir";
#endif
   }   
   Path p(in);

   cout << p.originalName() << endl;

// Open MIRIADImage

   MIRIADImage mirImage(in);
   mirImage.tempClose();
   AlwaysAssert(mirImage.imageType()=="MIRIADImage", AipsError);
   Unit unit("Jy/beam");
   AlwaysAssert(mirImage.setUnits(unit), AipsError);
   AlwaysAssert(mirImage.units().getName()=="Jy/beam", AipsError);
   Record rec;
   rec.define("field1", 0.0);
   rec.define("field2", "doggies");
   AlwaysAssert(mirImage.setMiscInfo(rec), AipsError);
   mirImage.reopen();
   Record rec2 = mirImage.miscInfo();
   AlwaysAssert(rec.isDefined("field1"), AipsError);  
   AlwaysAssert(rec.isDefined("field2"), AipsError);  
   AlwaysAssert(rec.asFloat("field1")==0.0, AipsError);
   AlwaysAssert(rec.asString("field2")=="doggies", AipsError);
   AlwaysAssert(mirImage.hasPixelMask() == mirImage.isMasked(), AipsError);
#if 0
   if (mirImage.hasPixelMask()) {
      Lattice<Bool>& pMask = mirImage.pixelMask();
      AlwaysAssert(pMask.shape()==mirImage.shape(), AipsError);
   }
#endif
   AlwaysAssert(mirImage.getRegionPtr()==0, AipsError);
   AlwaysAssert(mirImage.isWritable()==False, AipsError);
   AlwaysAssert(mirImage.name(False)==p.absoluteName(),AipsError);
   AlwaysAssert(mirImage.ok(), AipsError);
//
   mirImage.tempClose();
   if (print) {
      IPosition start (mirImage.ndim(),0);
      IPosition shape(mirImage.shape());
      for (uInt i=0; i<mirImage.ndim(); i++) {
         if (shape(i) > size) shape(i) = size;
      }
      cerr << "Data = " << mirImage.getSlice(start, shape) << endl;
      cerr << "Mask = " << mirImage.getMaskSlice(start, shape) << endl;
   }

// Convert from MIRIAD as a comparison

   String error;
   ImageInterface<Float>* pTempImage = 0;
   String imageName;
#if 1
   if (!ImageFITSConverter::FITSToImage(pTempImage, error, 
                                        imageName, in+".fits", 0)) {
      os << error << LogIO::EXCEPTION;
   }

   //  need to fill pTempImage ....

   Array<Float> mirArray = mirImage.get();
   Array<Float> dataArray = pTempImage->get();
   Array<Bool> mirMask = mirImage.getMask();
   Array<Bool> dataMask = pTempImage->getMask();
   CoordinateSystem mirCS = mirImage.coordinates();
   CoordinateSystem dataCS = pTempImage->coordinates();
   delete pTempImage;
//
   AlwaysAssert(allNear(dataArray, dataMask, mirArray, mirMask), AipsError);
   AlwaysAssert(mirCS.near(dataCS), AipsError);

// Test Clone

   ImageInterface<Float>* pMirImage = mirImage.cloneII();
   Array<Float> mirArray2 = pMirImage->get();
   Array<Bool> mirMask2 = pMirImage->getMask();
   CoordinateSystem mirCS2 = pMirImage->coordinates();
   delete pMirImage;
//
   AlwaysAssert(allNear(dataArray, dataMask, mirArray2, mirMask2), AipsError);
   AlwaysAssert(mirCS2.near(dataCS), AipsError);
//
#endif

   cerr << "ok " << endl;


} catch (AipsError x) {
   cerr << "aipserror: error " << x.getMesg() << endl;
   return 1;
}


  return 0;
}

Bool allNear (const Array<Float>& data, const Array<Bool>& dataMask,
              const Array<Float>& mir,  const Array<Bool>& mirMask,
              Float tol)
{
   Bool deletePtrData, deletePtrDataMask, deletePtrMIRIAD, deletePtrMIRIADMask;
   const Float* pData = data.getStorage(deletePtrData);
   const Float* pMIRIAD = mir.getStorage(deletePtrMIRIAD);
   const Bool* pDataMask = dataMask.getStorage(deletePtrDataMask);
   const Bool* pMIRIADMask = mirMask.getStorage(deletePtrMIRIADMask);
//
   for (uInt i=0; i<data.nelements(); i++) {
      if (pDataMask[i] != pMIRIADMask[i]) {
         cerr << "masks differ" << endl;
         return False;
      }
      if (pDataMask[i]) { 
         if (!near(pData[i], pMIRIAD[i], tol)) {
            cerr << "data differ, tol = " << tol << endl;
            cerr << pData[i] << ", " << pMIRIAD[i] << endl;
            return False;
         }
      }
   }
//
   data.freeStorage(pData, deletePtrData);
   dataMask.freeStorage(pDataMask, deletePtrDataMask);
   mir.freeStorage(pMIRIAD, deletePtrMIRIAD);
   mirMask.freeStorage(pMIRIADMask, deletePtrMIRIADMask);
   return True;
}


