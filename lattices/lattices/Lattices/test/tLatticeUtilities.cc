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

#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/MaskArrLogi.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Slicer.h>
#include <casa/BasicMath/Math.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/Utilities/Assert.h>
#include <lattices/Lattices/TempLattice.h>
#include <lattices/Lattices/LatticeStepper.h>
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/LatticeUtilities.h>
#include <lattices/Lattices/SubLattice.h>
#include <lattices/Lattices/LatticeExprNode.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/ArrayLattice.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
void doMinMax();
void doCollapse();
void doCopy();
void doReplicate();
void doBin();

int main()
{
  try {
    
// minMax

     doMinMax();

// Collapse

     doCollapse();

// Copy

     doCopy();

// Replicate

     doReplicate();

// Bin

     doBin();

  } catch (AipsError x) {
    cout<< "FAIL"<< endl;
    cerr << x.getMesg() << endl;
    return 1;
  } 
  cout<< "OK"<< endl;
  return 0;
}




void doMinMax()
{
    cerr << "minMax " << endl;
    TempLattice<Float> lat(IPosition(3,512,512,10), 1.0);
    lat.set(0.0);
    lat.putAt( 1.0, IPosition(3, 10, 10, 0) );
    lat.putAt( -1.0, IPosition(3, 20, 20, 2) );
    lat.putAt( 1.0, IPosition(3, 500, 400, 3) );
    lat.putAt( -1.0, IPosition(3, 502, 490, 4) );
    lat.putAt( 2.0, IPosition(3, 400, 500, 5) );
    lat.putAt( -2.0, IPosition(3, 10, 400, 6) );
    lat.putAt( 3.0, IPosition(3, 400, 100, 7) );
    lat.putAt( -3.0, IPosition(3, 500, 100, 8) );

    Float lmin, lmax;
    IPosition lminPos(3, 0);
    IPosition lmaxPos(3, 0);

    minMax(lmin, lmax, lminPos, lmaxPos, lat);

    IPosition trueMaxPos = IPosition(3, 400, 100, 7);
    IPosition trueMinPos = IPosition(3, 500, 100, 8);
    AlwaysAssert(trueMaxPos == lmaxPos && lmax == 3.0, AipsError);
    AlwaysAssert(trueMinPos == lminPos && lmin == -3.0, AipsError);
}


void doCollapse ()
{
    cerr << "Collapse" << endl;
    IPosition shape(3, 10, 20, 30);
    ArrayLattice<Float> latIn(shape);
    latIn.set(1.0);
    Array<Float> data;
    Array<Bool> mask;
    IPosition axes(2); axes(0) = 1; axes(1) = 2;

// Unmasked input

    {       
       cerr << "  Unmasked" << endl;
       SubLattice<Float> mLatIn(latIn);
//
       LatticeUtilities::collapse (data, axes, mLatIn, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
//
       LatticeUtilities::collapse (data, mask, axes, mLatIn, True, True, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(mask.ndim()==1, AipsError);
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(mask.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
       AlwaysAssert(allEQ(mask, True), AipsError);
    }

// Masked Input

    {       
       cerr << "  Masked" << endl;
       SubLattice<Float> mLatIn(latIn);
//
       ArrayLattice<Bool> maskLat(shape);
       maskLat.set(True);       
       mLatIn.setPixelMask(maskLat, True);
//
       LatticeUtilities::collapse (data, axes, mLatIn, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
//
       LatticeUtilities::collapse (data, mask, axes, mLatIn, True, True, True);
       AlwaysAssert(data.ndim()==1, AipsError);       
       AlwaysAssert(mask.ndim()==1, AipsError);
       AlwaysAssert(data.shape()(0)==shape(0), AipsError);
       AlwaysAssert(mask.shape()(0)==shape(0), AipsError);
       AlwaysAssert(allNear(data, Float(1.0), 1.0e-6), AipsError);
       AlwaysAssert(allEQ(mask, True), AipsError);
    }
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

