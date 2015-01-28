//# tLatticeUtilities.cc:
//# Copyright (C) 1997,1998,1999,2000,2001
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/MaskArrLogi.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/LatticeStepper.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/Lattices/LatticeUtilities.h>
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/ArrayLattice.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

void doCopy();
void doReplicate();
void doBin();

int main()
{
  try {

// Copy

     doCopy();

// Replicate

     doReplicate();

// Bin

     doBin();

  } catch (const AipsError& x) {
    cout<< "FAIL"<< endl;
    cerr << x.getMesg() << endl;
    return 1;
  } 
  cout<< "OK"<< endl;
  return 0;
}



void doCopy ()
{
    cerr << "copyDataAndMask" << endl;
    LogIO os(LogOrigin("tLatticeUtilities", "doCopy", WHERE));
//
    IPosition shape(2, 5, 10);
    IPosition pos(2,0);
    ArrayLattice<Float> latIn(shape);
    latIn.set(1.0);
    SubLattice<Float> mLatIn(latIn, True);
    ArrayLattice<Bool> maskIn(shape);
    maskIn.set(True);
    mLatIn.setPixelMask(maskIn, True);

// Unmasked output

    {       
       cerr << "  Unmasked output" << endl;
//
       ArrayLattice<Float> latOut(shape);
       SubLattice<Float> mLatOut(latOut, True);
//
       LatticeUtilities::copyDataAndMask (os, mLatOut, mLatIn, False);
       AlwaysAssert(allNear(mLatOut.get(), Float(1.0), 1.0e-6), AipsError);
       AlwaysAssert(allEQ(mLatOut.getMask(), True), AipsError);
    }
//
    {       
       cerr << "  Masked output" << endl;
//
       ArrayLattice<Float> latOut(shape);
       SubLattice<Float> mLatOut(latOut, True);
       ArrayLattice<Bool> latMaskOut(shape);       
       latMaskOut.set(False);
       mLatOut.setPixelMask(latMaskOut, True);
//
       LatticeUtilities::copyDataAndMask (os, mLatOut, mLatIn, False);
       AlwaysAssert(allNear(mLatOut.get(), Float(1.0), 1.0e-6), AipsError);
       AlwaysAssert(allEQ(mLatOut.getMask(), True), AipsError);

// Now set one mask value to False so the output pixel should be zero


       Lattice<Bool>& pixelMaskIn = mLatIn.pixelMask();
       pixelMaskIn.set(True);
       pixelMaskIn.putAt(False,pos);
       LatticeUtilities::copyDataAndMask (os, mLatOut, mLatIn, True);
//
       {
          Array<Float> dataOut = mLatOut.get();
          Array<Bool> maskOut = mLatOut.getMask();
          AlwaysAssert(near(dataOut(pos), Float(0.0), 1.0e-6), AipsError);
          AlwaysAssert(maskOut(pos)==False, AipsError);
       }
   }
}

void doReplicate ()
{
   cerr << "Replicate" << endl;
   IPosition shapeLat(2,10,20);
   ArrayLattice<Float> lat(shapeLat);

// Just use full lattice.  Having the region in the function
// call is pretty useless

   IPosition start(2,0,0);
   IPosition end(shapeLat-1);
   Slicer slice(start, end, Slicer::endIsLast);

// Replcicate array 4 times

   {
      IPosition shapePixels(2,5,10);
      Array<Float> arr(shapePixels);
      indgen(arr);
      LatticeUtilities::replicate (lat, slice, arr);
//
      Double tol = 1.0e-6;
      LatticeStepper stepper(shapeLat, shapePixels, LatticeStepper::RESIZE);
      LatticeIterator<Float> iter(lat, stepper);
      for (iter.reset(); !iter.atEnd(); iter++) {
         AlwaysAssert(allNear(iter.cursor(),arr,tol), AipsError);
      }
   }

// Forced errro

   {
      IPosition shapePixels(2,7,13);
      Array<Float> arr(shapePixels);
      indgen(arr);
      try {
         LatticeUtilities::replicate (lat, slice, arr);
         throw(AipsError("replicate unexpectedly did not fail"));
      } catch (AipsError x) {
         cerr << "Expected error = " << x.getMesg() << endl;
      }
   }
}


void doBin ()
{
   cerr << "Bin" << endl;
   IPosition shape(2,16,20);
   Array<Float> data(shape);
   Array<Bool> mask(shape);
   Float val = 1.0;
   data.set(val);
   mask.set(True);
   MaskedArray<Float> mArrIn(data,mask);
//
   MaskedArray<Float> mArrOut;
   uInt axis = 0;
   Int bin = 4;
   LatticeUtilities::bin(mArrOut, mArrIn, axis, bin);
   IPosition shapeOut = mArrOut.shape();
   for (uInt i=0; i<shape.nelements(); i++) {
      if (i==axis) {
         AlwaysAssert(shapeOut(i)==shape(i)/bin,AipsError);
      } else {
         AlwaysAssert(shapeOut(i)==shape(i),AipsError);
      }
   }
//
   Double tol = 1.0e-6;
   AlwaysAssert(allNear(mArrOut.getArray(),val,tol), AipsError);
   AlwaysAssert(allEQ(mArrOut.getMask(),True), AipsError);
}

