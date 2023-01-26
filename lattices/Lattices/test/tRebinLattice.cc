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

//# Includes

#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/lattices/Lattices/MaskedLattice.h> 
#include <casacore/lattices/Lattices/RebinLattice.h> 
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/lattices/Lattices/LatticeUtilities.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doit1 (const IPosition& shape, const IPosition& factors);
void doit2 ();
void doit3 ();
void doit4 (RebinLattice<float>& rb, const IPosition& shape, 
            const IPosition& factors);



int main (int argc, const char* argv[])
{

try {

   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

   inputs.create("shape", "-10", "shape");
   inputs.create("factors", "-10", "factors");
   inputs.readArguments(argc, argv);
   const Block<int32_t> factorsU(inputs.getIntArray("factors"));
   const Block<int32_t> shapeU(inputs.getIntArray("shape"));

// Convert inputs

   IPosition shapeIn;
   if (shapeU.nelements()>0) {
      if (shapeU.nelements()==1 && shapeU[0]==-10) {
         shapeIn = IPosition(2, 10, 10);
      } else {
         shapeIn.resize(shapeU.nelements());
         for (uint32_t i=0; i<shapeIn.nelements(); i++) shapeIn(i) = shapeU[i];
      }
   }
   uint32_t nDim = shapeIn.nelements();
//
   IPosition factors;
   if (factorsU.nelements()>0) {
      if (factorsU.nelements()==1 && factorsU[0]==-10) {
         factors.resize(nDim);
         factors = 2;
      } else {
         AlwaysAssert(factorsU.nelements()==nDim, AipsError);
         factors.resize(nDim);
         for (uint32_t i=0; i<nDim; i++) factors(i) = factorsU[i];
      }
   }

// Test user inputs

   doit1(shapeIn, factors);

// Test other choices

   {
      IPosition shape(1,10);
      IPosition fac(1,1);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      IPosition fac(1,2);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      IPosition fac(1,3);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      IPosition fac(1,7);
      doit1 (shape, fac);
   }
   {
      IPosition shape(1,10);
      IPosition fac(1,11);
      doit1 (shape, fac);
   }

//
   {
      IPosition shape(2,20,24);
      IPosition fac(2,1,1); 
      doit1 (shape, fac);
   }
   {
      IPosition shape(2,20,24);
      IPosition fac(2,2,1);
      doit1 (shape, fac);
   }
   {
      IPosition shape(2,20,24);
      IPosition fac(2,1,2);
      doit1 (shape, fac);
   }
   {
      IPosition shape(2,20,24);
      IPosition fac(2,2,2); 
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,10,20,30);
      IPosition fac(3, 1,1,1);
      doit1 (shape, fac);
   }
   {
      IPosition shape(3,23,28,31);
      IPosition fac(3, 3,1,1);
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,23,28,31);
      IPosition fac(3, 1,3,1); fac[1] = 3;
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,23,28,31);
      IPosition fac(3, 1,1,3); fac[2] = 3;
      doit1 (shape, fac);
   }
//
   {
      IPosition shape(3,23,27,31);
      IPosition fac(3, 3,4,9);
      doit1 (shape, fac);
   }


// More careful test of rebinned values

   doit2();

// Test other functions

   doit3();


} catch (std::exception& x) {
     cerr << "aipserror: error " << x.what() << endl;
     return 1;
} 

return 0;

}




void doit1 (const IPosition& shapeIn, const IPosition& factors)
{

// Make input ML

   TiledShape shape2(shapeIn);
   TempLattice<float> inLat(shape2);
   inLat.set(1.0);
   SubLattice<float> inML(inLat, true);

// Unmasked input

   {
 
// Make rebinner

      RebinLattice<float> reBinLat(inML, factors);
//
      const Array<float>& data = reBinLat.get();
      float val(1.0);
      bool ok = ::allNear(data, val, 1.0e-6);
      AlwaysAssert(ok, AipsError);
//
      const Array<bool>& mask = reBinLat.getMask();
      ok = ::allEQ(mask, true);
      AlwaysAssert(ok, AipsError);
    }

// Masked input

   {

      TempLattice<bool> inMask(shape2);
      inMask.set(true);
      inML.setPixelMask(inMask, true);
 
// Make rebinner

      RebinLattice<float> reBinLat(inML, factors);
//
      const Array<float>& data = reBinLat.get();
      float val(1.0);
      bool ok = ::allNear(data, val, 1.0e-6);
      AlwaysAssert(ok, AipsError);
//
      const Array<bool>& mask = reBinLat.getMask();
      ok = ::allEQ(mask, true);
      AlwaysAssert(ok, AipsError);
    }
}


void doit2 ()
{

// Make data

   IPosition factors(1, 2);
   IPosition shapeIn(1, 6);
   Array<float> dataIn(shapeIn);
   IPosition pos(1);
   for (int32_t j=0; j<shapeIn(0); j++) {
      pos(0) = j;
      dataIn(pos) = float(j);
   }
   IPosition shapeOut(1, 3);
   Array<float> dataOut(shapeOut);
   for (int32_t j=0; j<shapeOut(0); j++) {
      pos(0) = j;
      dataOut(pos) = 2*float(j) + 0.5;
   }

// Make MLs   

   TiledShape shape2(shapeIn);
   TempLattice<float> inLat(shape2);
   inLat.put(dataIn);
   TempLattice<bool> inMask(shape2);
   inMask.set(true);
//
   SubLattice<float> inML(inLat, true);
   inML.setPixelMask(inMask, true);
//
   cerr << endl << endl;
   cerr << "factors = " << factors << endl;
   cerr << "shapeIn, shapeOut = " << shapeIn << shapeOut << endl;

// Make rebinner

   RebinLattice<float> reBinLat(inML, factors);
//
   const Array<float>& dataOut2 = reBinLat.get();
   bool ok = ::allNear(dataOut, dataOut2, 1.0e-6);
   AlwaysAssert(ok, AipsError);
//
   const Array<bool>& maskOut2 = reBinLat.getMask();
   ok = ::allEQ(maskOut2, true);
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
   cerr << "out = " << reBinLat.getMask() << endl;
*/
}



void doit3 ()
{

// Make data

   IPosition factors(1, 2);
   IPosition shapeIn(1, 6);
   Array<float> dataIn(shapeIn);
   IPosition pos(1);
   for (int32_t j=0; j<shapeIn(0); j++) {
      pos(0) = j;
      dataIn(pos) = float(j);
   }

// Make MLs   

   TiledShape shape2(shapeIn);
   TempLattice<float> inLat(shape2);
   inLat.put(dataIn);
   TempLattice<bool> inMask(shape2);
   inMask.set(true);
//
   SubLattice<float> inML(inLat, true);
   inML.setPixelMask(inMask, true);

// Make rebinner

   RebinLattice<float> rb(inML, factors);

// Test it

   doit4(rb, shapeIn, factors);

// Copy constructor

   RebinLattice<float> rb2(rb);
   doit4(rb2, shapeIn, factors);

// Assignment

   RebinLattice<float> rb3;
   rb3 = rb;
   doit4(rb3, shapeIn, factors);
}

void doit4 (RebinLattice<float>& rb, const IPosition& shape, 
            const IPosition& factors)
{

   AlwaysAssert(rb.isMasked(), AipsError);
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
   AlwaysAssert(rb.shape()(0)==shape(0)/factors(0), AipsError);
   rb.name();
   rb.advisedMaxPixels();
   AlwaysAssert(rb.ok(), AipsError);
}

