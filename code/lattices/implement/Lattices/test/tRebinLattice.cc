//# tRebinLattice.cc: test RebinLattice
//# Copyright (C) 2001,2002,2004
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
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

//# Includes

#include <aips/Inputs/Input.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Containers/Block.h>
#include <trial/Lattices/MaskedLattice.h> 
#include <trial/Lattices/RebinLattice.h> 
#include <trial/Lattices/SubLattice.h>
#include <aips/Lattices/TempLattice.h>
#include <aips/Lattices/TiledShape.h>
#include <trial/Lattices/LatticeUtilities.h>
#include <aips/Logging/LogIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/iostream.h>


void doit1 (const IPosition& shape, const Vector<uInt>& factors);
void doit2 ();
void doit3 ();
void doit4 (RebinLattice<Float>& rb, const IPosition& shape, 
            const Vector<uInt>& factors);



int main (int argc, char **argv) {

try {

   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

   inputs.create("shape", "-10", "shape");
   inputs.create("factors", "-10", "factors");
   inputs.readArguments(argc, argv);
   const Block<Int> factorsU(inputs.getIntArray("factors"));
   const Block<Int> shapeU(inputs.getIntArray("shape"));

// Convert inputs

   IPosition shapeIn;
   if (shapeU.nelements()>0) {
      if (shapeU.nelements()==1 && shapeU[0]==-10) {
         shapeIn = IPosition(2, 10, 10);
      } else {
         shapeIn.resize(shapeU.nelements());
         for (uInt i=0; i<shapeIn.nelements(); i++) shapeIn(i) = shapeU[i];
      }
   }
   uInt nDim = shapeIn.nelements();
//
   Vector<uInt> factors;
   if (factorsU.nelements()>0) {
      if (factorsU.nelements()==1 && factorsU[0]==-10) {
         factors.resize(nDim);
         factors = 2;
      } else {
         AlwaysAssert(factorsU.nelements()==nDim, AipsError);
         factors.resize(nDim);
         for (uInt i=0; i<nDim; i++) factors(i) = factorsU[i];
      }
   }

// Test user inputs

   doit1(shapeIn, factors);

// Test other choices

   {
      IPosition shape(1,10);
      Vector<uInt> fac(1,1);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      Vector<uInt> fac(1,2);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      Vector<uInt> fac(1,3);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      Vector<uInt> fac(1,7);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      Vector<uInt> fac(1,11);
      doit1 (shape, fac);
   }

//
   {
      IPosition shape(2,20,24);
      Vector<uInt> fac(2,1); 
      doit1 (shape, fac);
   }
   {
      IPosition shape(2,20,24);
      Vector<uInt> fac(2,1); fac[0] = 2;
      doit1 (shape, fac);
   }
   {
      IPosition shape(2,20,24);
      Vector<uInt> fac(2,1); fac[1] = 2;
      doit1 (shape, fac);
   }
   {
      IPosition shape(2,20,24);
      Vector<uInt> fac(2,2); 
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,10,20,30);
      Vector<uInt> fac(3, 1);
      doit1 (shape, fac);
   }
   {
      IPosition shape(3,23,28,31);
      Vector<uInt> fac(3,1); fac[0] = 3;
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,23,28,31);
      Vector<uInt> fac(3,1); fac[1] = 3;
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,23,28,31);
      Vector<uInt> fac(3,1); fac[2] = 3;
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,23,27,31);
      Vector<uInt> fac(3,1); fac[0] = 3; fac[1] = 4; fac[2] = 9;
      doit1 (shape, fac);
   }


// More careful test of rebinned values

   doit2();

// Test other functions

   doit3();


} catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
} 

return 0;

}






void doit1 (const IPosition& shapeIn, const Vector<uInt>& factors)
{

// Make input ML

   TiledShape shape2(shapeIn);
   TempLattice<Float> inLat(shape2);
   inLat.set(1.0);
   TempLattice<Bool> inMask(shape2);
   inMask.set(True);
//
   SubLattice<Float> inML(inLat, True);
// inML.pixelMask() = inMask;

// Make rebinner

   RebinLattice<Float> reBinLat(inML, factors);
   
// Make output ML

   IPosition shapeOut = reBinLat.shape();
   TiledShape shapeOut2(shapeOut);
   TempLattice<Float> outLat(shapeOut2);
   TempLattice<Bool> outMask(shapeOut2);
   outMask.set(True);
//
   SubLattice<Float> outML(outLat, True);
   outML.setPixelMask(outMask, True);

// Rebin via copy

   cerr << endl << endl;
   cerr << "factors = " << factors << endl;
   cerr << "shapeIn, shapeOut = " << shapeIn << shapeOut << endl;
   LogIO os(LogOrigin("tRebinLattice", "main()", WHERE));
   LatticeUtilities::copyDataAndMask(os, outML, reBinLat);
//
   const Array<Float>& data = outML.get();
   Float val(1.0);
   Bool ok = ::allNear(data, val, 1.0e-6);
   AlwaysAssert(ok, AipsError);
//
   const Array<Bool>& mask = outML.getMask();
   ok = ::allEQ(mask, True);
   AlwaysAssert(ok, AipsError);
/*
   cerr << "Data = " << endl;
   cerr << "in = " << inML.get() << endl;
   cerr << "out = " << outML.get() << endl;

   cerr << "Masks = " << endl;
   cerr << "in = " << inML.getMask() << endl;
   cerr << "out = " << outML.getMask() << endl;
*/
}


void doit2 ()
{

// Make data

   Vector<uInt> factors(1, 2);
   IPosition shapeIn(1, 6);
   Array<Float> dataIn(shapeIn);
   IPosition pos(1);
   for (Int j=0; j<shapeIn(0); j++) {
      pos(0) = j;
      dataIn(pos) = Float(j);
   }
   IPosition shapeOut(1, 3);
   Array<Float> dataOut(shapeOut);
   for (Int j=0; j<shapeOut(0); j++) {
      pos(0) = j;
      dataOut(pos) = 2*Float(j) + 0.5;
   }

// Make MLs   

   TiledShape shape2(shapeIn);
   TempLattice<Float> inLat(shape2);
   inLat.put(dataIn);
   TempLattice<Bool> inMask(shape2);
   inMask.set(True);
//
   SubLattice<Float> inML(inLat, True);
   inML.setPixelMask(inMask, True);
//
   cerr << endl << endl;
   cerr << "factors = " << factors << endl;
   cerr << "shapeIn, shapeOut = " << shapeIn << shapeOut << endl;

// Make rebinner

   RebinLattice<Float> reBinLat(inML, factors);
   
// Make output ML

   IPosition shapeOut2 = reBinLat.shape();
   TiledShape tShapeOut2(shapeOut);
   TempLattice<Float> outLat(tShapeOut2);
   TempLattice<Bool> outMask(tShapeOut2);
   outMask.set(True);
//
   SubLattice<Float> outML(outLat, True);
   outML.setPixelMask(outMask, True);

// Rebin

   LogIO os(LogOrigin("tRebinLattice", "main()", WHERE));
   LatticeUtilities::copyDataAndMask(os, outML, reBinLat);
//
   const Array<Float>& dataOut2 = outML.get();
   Bool ok = ::allNear(dataOut, dataOut2, 1.0e-6);
   AlwaysAssert(ok, AipsError);
//
   const Array<Bool>& maskOut2 = outML.getMask();
   ok = ::allEQ(maskOut2, True);
   AlwaysAssert(ok, AipsError);
/*
   cerr << "Data = " << endl;
   cerr << "in = " << inML.get() << endl;
   cerr << "expected out = " << dataOut << endl;
   cerr << "out = " << dataOut2 << endl;
*/
/*/
   cerr << "Masks = " << endl;
   cerr << "in = " << inML.getMask() << endl;
   cerr << "out = " << outML.getMask() << endl;
*/
}



void doit3 ()
{

// Make data

   Vector<uInt> factors(1, 2);
   IPosition shapeIn(1, 6);
   Array<Float> dataIn(shapeIn);
   IPosition pos(1);
   for (Int j=0; j<shapeIn(0); j++) {
      pos(0) = j;
      dataIn(pos) = Float(j);
   }

// Make MLs   

   TiledShape shape2(shapeIn);
   TempLattice<Float> inLat(shape2);
   inLat.put(dataIn);
   TempLattice<Bool> inMask(shape2);
   inMask.set(True);
//
   SubLattice<Float> inML(inLat, True);
   inML.setPixelMask(inMask, True);
cerr << "in is masked = " << inML.isMasked() << endl;

// Make rebinner

   RebinLattice<Float> rb(inML, factors);

// Test it

   doit4(rb, shapeIn, factors);

// Copy constructor

   RebinLattice<Float> rb2(rb);
   doit4(rb2, shapeIn, factors);

// Assignment

   RebinLattice<Float> rb3;
   rb3 = rb;
   doit4(rb3, shapeIn, factors);
}

void doit4 (RebinLattice<Float>& rb, const IPosition& shape, 
            const Vector<uInt>& factors)
{

//   AlwaysAssert(rb.isMasked(), AipsError);
   AlwaysAssert(!rb.isPaged(), AipsError);
   AlwaysAssert(!rb.isWritable(), AipsError);
//
   AlwaysAssert(rb.lock(FileLocker::Read,1), AipsError);
   AlwaysAssert(rb.hasLock(FileLocker::Read), AipsError);
   rb.unlock();
//
   rb.resync();
   rb.flush();
   rb.tempClose();
   rb.reopen();
//
   AlwaysAssert(rb.getRegionPtr()==0, AipsError);
   AlwaysAssert(rb.shape()(0)==shape(0)/Int(factors(0)), AipsError);
   String name = rb.name();
   uInt nMaxPix = rb.advisedMaxPixels();
   AlwaysAssert(rb.ok(), AipsError);
}

