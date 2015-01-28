//# tLatticeSlice1D.cc: test LatticeSlice1D
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

#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/lattices/Lattices/PixelCurve1D.h> 
#include <casacore/lattices/Lattices/MaskedLattice.h> 
#include <casacore/lattices/LatticeMath/LatticeSlice1D.h> 
#include <casacore/lattices/Lattices/SubLattice.h>
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doit1 ();
void doit2 ();
void doit3 ();


int main (int argc, const char* argv[])
{

try {

   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

   inputs.create("shape", "-10", "shape");
   inputs.readArguments(argc, argv);
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

// Test 

   doit1();
   doit2();
   doit3();


} catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
} 

return 0;

}



void doit1 ()
{
   AlwaysAssert(LatticeSlice1D<Float>::stringToMethod("NEAREST")==LatticeSlice1D<Float>::NEAREST, AipsError);
   AlwaysAssert(LatticeSlice1D<Float>::stringToMethod("LINEAR")==LatticeSlice1D<Float>::LINEAR, AipsError);
   AlwaysAssert(LatticeSlice1D<Float>::stringToMethod("CUBIC")==LatticeSlice1D<Float>::CUBIC, AipsError);
//
   uInt nDim = 3;
   uInt nPts = 100;
   IPosition shape(nDim, 5, 10, 15);
   TiledShape shape2(shape);
   TempLattice<Float> inLat(shape2);
   inLat.set(1.0);
   SubLattice<Float> inML(inLat, True);
//
   LatticeSlice1D<Float> slicer(inML, LatticeSlice1D<Float>::LINEAR);
   AlwaysAssert(slicer.interpolationMethod()==LatticeSlice1D<Float>::LINEAR, AipsError);
//
   Vector<Float> data, x, y, distance;
   Vector<Bool> mask;
   IPosition blc(nDim), trc(nDim);
   uInt axis0, axis1;
//
   {
      cerr << "Slice in X-Y plane" << endl;
      blc = 0;
      trc = 0;
      trc(0) = shape(0) - 1;
      trc(1) = shape(1) - 1;
//
      slicer.getSlice (data, mask, blc, trc, nPts);
      AlwaysAssert(data.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask, True), AipsError);
//
      slicer.getPosition (axis0, axis1, x, y, distance);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==0&&axis1==1), AipsError);
   }
//
   {
      cerr << "Slice in X-Z plane" << endl;
      blc = 0;
      trc = 0;
      trc(0) = shape(0) - 1;
      trc(2) = shape(2) - 1;
      slicer.getSlice (data, mask, blc, trc, nPts);
      AlwaysAssert(data.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask, True), AipsError);
//
      slicer.getPosition (axis0, axis1, x, y, distance);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==0&&axis1==2), AipsError);
   }
   {
      cerr << "Slice in Y-Z plane" << endl;
      blc = 0;
      trc = 0;
      trc(1) = shape(1) - 1;
      trc(2) = shape(2) - 1;
      slicer.getSlice (data, mask, blc, trc, nPts);
      AlwaysAssert(data.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask, True), AipsError);
//
      slicer.getPosition (axis0, axis1, x, y, distance);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==1&&axis1==2), AipsError);
   }
   {
      cerr << "Slice in XYZ plane" << endl;
      blc = 0;
      trc = shape - 1;
      try {
        slicer.getSlice (data, mask, blc, trc, nPts);
      } catch (AipsError x) {
        cerr << "Caught expected exception " << x.getMesg() << endl;
      }
   }
}

void doit2 ()
{
   uInt nDim = 3;
   uInt nPts = 100;
   IPosition shape(nDim, 5, 10, 15);
   TiledShape shape2(shape);
   TempLattice<Float> inLat(shape2);
   inLat.set(1.0);
   SubLattice<Float> inML(inLat, True);
//
   LatticeSlice1D<Float> slicer(inML, LatticeSlice1D<Float>::CUBIC);
   AlwaysAssert(slicer.interpolationMethod()==LatticeSlice1D<Float>::CUBIC, AipsError);
//
   Vector<Float> data, x, y, distance;
   Vector<Float> data2, x2, y2, distance2;
   Vector<Bool> mask, mask2;
   IPosition blc(nDim), trc(nDim);
   uInt axis0, axis1;
//
   {
      cerr << "Slice in X-Y plane" << endl;
      blc = 0;
      trc = 0;
      trc(0) = shape(0) - 1;
      trc(1) = shape(1) - 1;
//
      slicer.getSlice (data, mask, blc, trc, nPts);
      AlwaysAssert(data.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask, True), AipsError);
//
      slicer.getPosition (axis0, axis1, x, y, distance);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==0&&axis1==1), AipsError);
   }

// Copy constructor

   {
      cerr << "Copy constructor" << endl;
      LatticeSlice1D<Float> slicer2(slicer);
      AlwaysAssert(slicer2.interpolationMethod()==LatticeSlice1D<Float>::CUBIC, AipsError);
//
      slicer2.getSlice (data2, mask2, blc, trc, nPts);
      AlwaysAssert(data2.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data2, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask2, True), AipsError);
      AlwaysAssert(allNear(data, data2, Double(1.0e-6)), AipsError);
//
      slicer2.getPosition (axis0, axis1, x2, y2, distance2);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==0&&axis1==1), AipsError);
      AlwaysAssert(allNear(x, x2, Double(1.0e-6)), AipsError);
      AlwaysAssert(allNear(y, y2, Double(1.0e-6)), AipsError);
      AlwaysAssert(allNear(distance, distance2, Double(1.0e-6)), AipsError);   }

// Assignment

   {
      cerr << "Assignment" << endl;
      LatticeSlice1D<Float> slicer2;
      try {
        slicer2.getSlice (data, mask, blc, trc, nPts);
      } catch (AipsError x) {
        cerr << "Caught expected exception " << x.getMesg() << endl;
      }
//
      LatticeSlice1D<Float> slicer3(inML, LatticeSlice1D<Float>::CUBIC);
      slicer2 = slicer3;
      AlwaysAssert(slicer2.interpolationMethod()==LatticeSlice1D<Float>::CUBIC, AipsError);
//
      slicer2.getSlice (data2, mask2, blc, trc, nPts);
      AlwaysAssert(data2.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data2, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask2, True), AipsError);
      AlwaysAssert(allNear(data, data2, Double(1.0e-6)), AipsError);
//
      slicer2.getPosition (axis0, axis1, x2, y2, distance2);
      AlwaysAssert(x2.nelements()==nPts, AipsError);
      AlwaysAssert(y2.nelements()==nPts, AipsError);
      AlwaysAssert(distance2.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==0&&axis1==1), AipsError);
      AlwaysAssert(allNear(x, x2, Double(1.0e-6)), AipsError);
      AlwaysAssert(allNear(y, y2, Double(1.0e-6)), AipsError);
      AlwaysAssert(allNear(distance, distance2, Double(1.0e-6)), AipsError);   
   }
}
      

void doit3 ()
{
   uInt nDim = 3;
   IPosition shape(nDim,20,40,60);
   TiledShape shape2(shape);
   TempLattice<Float> inLat(shape2);
   inLat.set(1.0);
   SubLattice<Float> inML(inLat, True);
//
   LatticeSlice1D<Float> slicer(inML, LatticeSlice1D<Float>::LINEAR);
   AlwaysAssert(slicer.interpolationMethod()==LatticeSlice1D<Float>::LINEAR, AipsError);
//
   Vector<Double> xIn(3), yIn(3);
   uInt nPts = 100;
   Vector<Float> data, x, y, distance;
   Vector<Bool> mask;
   IPosition coord(nDim,0);
   uInt axis0, axis1;
//
   {
      cerr << "Polyline slice in X-Y plane" << endl;
      xIn(0) = 0.0; xIn(1) = 10.0; xIn(2) = 18.0;
      yIn(0) = 2.3; yIn(1) = 15.4; yIn(2) = 35.0;
      PixelCurve1D curve(xIn,yIn,nPts);
      slicer.getSlice (data, mask, curve, 0, 1, coord);
      AlwaysAssert(data.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask, True), AipsError);
//
      slicer.getPosition (axis0, axis1, x, y, distance);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==0&&axis1==1), AipsError);
   }
   {
      cerr << "Polyline slice in X-Z plane" << endl;
      xIn(0) = 0.0; xIn(1) = 10.0; xIn(2) = 18.0;
      yIn(0) = 2.3; yIn(1) = 15.4; yIn(2) = 35.0;
      PixelCurve1D curve(xIn,yIn,nPts);
      slicer.getSlice (data, mask, curve, 0, 2, coord);
      AlwaysAssert(data.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask, True), AipsError);
//
      slicer.getPosition (axis0, axis1, x, y, distance);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==0&&axis1==2), AipsError);
   }
   {
      cerr << "Polyline slice in Y-Z plane" << endl;
      xIn(0) = 0.0; xIn(1) = 10.0; xIn(2) = 18.0;
      yIn(0) = 2.3; yIn(1) = 15.4; yIn(2) = 35.0;
      PixelCurve1D curve(xIn,yIn,nPts);
      slicer.getSlice (data, mask, curve, 1, 2, coord);
      AlwaysAssert(data.nelements()==nPts, AipsError);
      AlwaysAssert(allNear(data, Float(1.0), Double(1.0e-6)), AipsError);
      AlwaysAssert(allEQ(mask, True), AipsError);
//
      slicer.getPosition (axis0, axis1, x, y, distance);
      AlwaysAssert(x.nelements()==nPts, AipsError);
      AlwaysAssert(y.nelements()==nPts, AipsError);
      AlwaysAssert(distance.nelements()==nPts, AipsError);
      AlwaysAssert((axis0==1&&axis1==2), AipsError);
   }
}

