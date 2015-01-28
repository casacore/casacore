//# tWCBox.cc: Test program for WCBox class
//# Copyright (C) 1997,1999,2000,2001
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
#include <casacore/casa/Arrays.h>
#include <casacore/measures/Measures.h>
#include <casacore/coordinates/Coordinates.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/images/Regions/WCBox.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void setValues (IPosition& blcI,
                IPosition& trcI,
                IPosition& shape,
                LCBox& checkBox,
                Vector<Quantum<Double> >& wBlc,
                Vector<Quantum<Double> >& wTrc,
                const CoordinateSystem& cSys);


void listBB(const LCRegion* pLCRegion);

void list (const RecordInterface& record);


int main()
{
try {


// Create default Coordinate System, [ra, dec, freq]

   Vector<Int> absRel;
   CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();

// Create vectors

   IPosition shape, blcI, trcI;
   Vector<Quantum<Double> > wBlc, wTrc;
   LCBox checkBox;

// Create WCBox

   setValues (blcI, trcI, shape, checkBox, wBlc, wTrc, cSys);
   WCBox box(wBlc, wTrc, cSys, absRel);

 // Create null box

   WCBox box2;

// Test comparison

   AlwaysAssert(box == box, AipsError);
   AlwaysAssert(box2 != box, AipsError)

// Test assignment

   box2 = box;   
   AlwaysAssert(box2 == box, AipsError);
   AlwaysAssert(!(box2 != box), AipsError);

// Test copy constructor

   WCBox box3(box);
   AlwaysAssert(box3 == box, AipsError);

// Test clone region

   WCRegion* pWCRegion = box.cloneRegion();
   const WCBox* pBox = (const WCBox*)pWCRegion;
   AlwaysAssert(*pBox == box, AipsError);
   if (pWCRegion != 0) delete pWCRegion;

// Test splitBox
   {
      IPosition axes(3,2,0,1);
      WCBox sbox1(box.splitBox (axes));
      Vector<Quantum<Double> > blc2(axes.nelements());
      Vector<Quantum<Double> > trc2(axes.nelements());
      for (uInt i=0; i<axes.nelements(); i++) {
	blc2(i) = wBlc(axes(i));
	trc2(i) = wTrc(axes(i));
      }
      WCBox sbox2(blc2, trc2, axes, cSys, absRel);
      AlwaysAssert (sbox1 == sbox2, AipsError);
      IPosition axesa(2,2,1);
      WCBox sbox1a(sbox1.splitBox (axesa));
      Vector<Quantum<Double> > blc2a(axesa.nelements());
      Vector<Quantum<Double> > trc2a(axesa.nelements());
      IPosition axesa2(2);
      for (uInt i=0; i<axesa.nelements(); i++) {
	axesa2(i) = axes(axesa(i));
	blc2a(i) = blc2(axesa(i));
	trc2a(i) = trc2(axesa(i));
      }
      WCBox sbox2a(blc2a, trc2a, axesa2, cSys, absRel);
      AlwaysAssert (sbox1a == sbox2a, AipsError);
   }

// Test record saving
   {
     TableRecord rec =  box.toRecord("");
      
//     list (rec);
     WCBox* pWCBox = WCBox::fromRecord(rec, "");
     AlwaysAssert(*pWCBox == box, AipsError);
     if (pWCBox !=0) delete pWCBox;
   }

// Can extend is True

   AlwaysAssert(box.canExtend(), AipsError);

// Class Name is "WCBox"

   AlwaysAssert(WCBox::className()=="WCBox", AipsError);
   AlwaysAssert(box.type()==WCBox::className(), AipsError);

// Test conversion

//   cout << endl;
   {
//      cout << "[ra,dec,freq], [ra,dec,freq]" << endl;
//      cout << "toLCRegion called with shape = " << shape << endl;
      LCRegion* pLCRegion = box.toLCRegion(cSys, shape);
      AlwaysAssert(*pLCRegion==checkBox, AipsError);
      delete pLCRegion;
   }
//   cout << endl;

// Test conversion to LCRegion with unit change

   {
//      cout << "[ra,dec,freq], [ra,dec,freq]" << endl;
//      cout << "'   '   Hz     deg  deg Hz " << endl;
      CoordinateSystem cSys2(cSys);
      Vector<String> units = cSys2.worldAxisUnits();
      units(0) = "deg";
      units(1) = "deg";
      AlwaysAssert(cSys2.setWorldAxisUnits(units), AipsError);
//      cout << "toLCRegion called with shape = " << shape << endl;
      LCRegion* pLCRegion = box.toLCRegion(cSys2, shape);
      AlwaysAssert(*pLCRegion==checkBox, AipsError);
      delete pLCRegion;
   }
//   cout << endl;

// Test auto extension

   {
      LCBox checkBox2;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();      
      setValues (blcI, trcI, shape, checkBox2, wBlc, wTrc, cSys2);
      WCBox box2(wBlc, wTrc, cSys2, absRel);
//
      IPosition shape3(3), blc3(3), trc3(3);
      shape3(0) = shape(0);
      shape3(1) = shape(1);
      shape3(2) = 30;
      CoordinateSystem cSys3 = CoordinateUtil::defaultCoords3D(); 
//      cout << "toLCRegion called with shape = " << shape3 << endl;
      LCRegion* pLCRegion = box2.toLCRegion(cSys3, shape3);
//
      blc3(0) = blcI(0);
      blc3(1) = blcI(1);
      blc3(2) = 0;
      trc3(0) = trcI(0);
      trc3(1) = trcI(1);
      trc3(2) = shape3(2) - 1;
      LCBox checkBox3(blc3, trc3, shape3);
      AlwaysAssert(*pLCRegion==checkBox3, AipsError);
      if (pLCRegion != 0) delete pLCRegion;
   }
//   cout << endl;


// Test null blc/trc box 

   {
//      cout << "[ra,dec], [ra,dec]" << endl;
//      cout << "Null blc/trc" << endl;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();      
      wBlc.resize(0);
      wTrc.resize(0);
      WCBox box2(wBlc, wTrc, cSys2, absRel);
//
      IPosition shape2(2), blc2(2), trc2(2);
      shape2(0) = 10;
      shape2(1) = 20;
//      cout << "toLCRegion called with shape = " << shape2 << endl;
      LCRegion* pLCRegion = box2.toLCRegion(cSys2, shape2);

      blc2(0) = 0;
      blc2(1) = 0;
      trc2(0) = shape2(0)-1;
      trc2(1) = shape2(1)-1;
      LCBox checkBox2(blc2, trc2, shape2);
      AlwaysAssert(*pLCRegion==checkBox2, AipsError);
      if (pLCRegion != 0) delete pLCRegion;
   }
//   cout << endl;


// Test conversion to LCRegion with less world axes
   {
//      cout << "[ra,dec,freq], [ra,dec]" << endl;
      CoordinateSystem cSys3 = CoordinateUtil::defaultCoords3D();
      setValues (blcI, trcI, shape, checkBox, wBlc, wTrc, cSys3);
      WCBox box3(wBlc, wTrc, cSys3, absRel);
//
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      IPosition shape2(cSys2.nPixelAxes());
      for (uInt i=0; i<shape2.nelements(); i++) shape2(i) = shape(i);
//      cout << "toLCRegion called with shape = " << shape2 << endl; 
//
      Bool ok = False;
      try {
         LCRegion* pLCRegion = box3.toLCRegion(cSys2, shape2);
         if (pLCRegion != 0) delete pLCRegion;
      } catch (AipsError x) {
//         cout << "aipserror: caught error " << x.getMesg() << endl;
         ok = True;
      } 
      if (!ok) {
         throw(AipsError("Conversion to LCRegion did not fail as expected"));
      }
   }
//   cout << endl;



// Test conversion to LCRegion with inconsistent spectral axes

   {
//      cout << "world names [ra,dec,freq], [ra,dec,freq]" << endl;
//      cout << "freq systems         LSR           TOPO" << endl;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Int iSpec = cSys2.findCoordinate(Coordinate::SPECTRAL);
      uInt iSpec2 = iSpec;
      SpectralCoordinate sCoord = cSys2.spectralCoordinate(iSpec2);
      sCoord.setFrequencySystem(MFrequency::TOPO);
      cSys2.replaceCoordinate(sCoord, iSpec2);
    
      LCRegion* pLCRegion = 0;
//      cout << "toLCRegion called with shape = " << shape << endl;
      Bool ok = False;
      try {
         pLCRegion =  box.toLCRegion(cSys2, shape);
         if (pLCRegion != 0) delete pLCRegion;
      } catch (AipsError x) {
//         cout << "aipserror: caught error " << x.getMesg() << endl;
         ok = True;
      } 
      if (!ok) {
         throw(AipsError("Conversion to LCRegion did not fail as expected"));
      }
   }
   cout << endl;



// More world axes

   {
//      cout << "world names [ra,dec,freq], [ra,dec,stokes,freq]" << endl;
      CoordinateSystem cSys3 = CoordinateUtil::defaultCoords3D();      
      LCBox checkBox3;
      setValues (blcI, trcI, shape, checkBox3, wBlc, wTrc, cSys3);
      WCBox box3(wBlc, wTrc, cSys3, absRel);
//
      CoordinateSystem cSys4 = CoordinateUtil::defaultCoords4D();
      IPosition shape4(cSys4.nPixelAxes(),10);
      shape4(0) = shape(0); 
      shape4(1) = shape(1); 
      shape4(2) = 4;          // Stokes
      shape4(3) = shape(2);   // Spectral
//
      LCRegion* pLCRegion = 0;
//      cout << "toLCRegion called with shape = " << shape4 << endl;
      pLCRegion = box3.toLCRegion(cSys4, shape4);
//
      IPosition blc4(4), trc4(4);
      blc4(0) = blcI(0);
      blc4(1) = blcI(1);
      blc4(2) = 0;
      blc4(3) = blcI(2);
//
      trc4(0) = trcI(0);
      trc4(1) = trcI(1);
      trc4(2) = shape4(2) - 1;
      trc4(3) = trcI(2);
      LCBox checkBox4(blc4, trc4, shape4); 
      AlwaysAssert(*pLCRegion==checkBox4, AipsError);
      if (pLCRegion != 0) delete pLCRegion;
   }
//   cout << endl;



// Remove spectral axis
   {
/*
      cout << "world names [ra,dec], [ra,dec,freq]" << endl;
      cout << "world axes   0   1     0  1   -1" << endl;
      cout << "pixel axes   0   1     0  1   -1" << endl;
*/
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      setValues (blcI, trcI, shape, checkBox, wBlc, wTrc, cSys1);
      WCBox box1(wBlc, wTrc, cSys1, absRel);

      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Int pSpec = CoordinateUtil::findSpectralAxis(cSys2);
      Int wSpec = cSys2.pixelAxisToWorldAxis(pSpec);
      cSys2.removeWorldAxis(wSpec, cSys.referenceValue()(wSpec));
//
      IPosition shape2(cSys2.nPixelAxes(),10);
      for (uInt i=0; i<min(shape.nelements(),shape2.nelements()); i++) {
         shape2(i) = shape(i);
      }
      LCRegion* pLCRegion = 0;
//      cout << "toLCRegion called with shape = " << shape2 << endl;
      pLCRegion = box1.toLCRegion(cSys2, shape2);
      AlwaysAssert(*pLCRegion==checkBox, AipsError);
      if (pLCRegion != 0) delete pLCRegion;
   }
//   cout << endl;

// No common axes
   {
//      cout << "world names [ra,dec], [freq]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      setValues (blcI, trcI, shape, checkBox, wBlc, wTrc, cSys1);
      WCBox box1(wBlc, wTrc, cSys1, absRel);

      CoordinateSystem cSys2;
      CoordinateUtil::addFreqAxis(cSys2);
      IPosition shape2(cSys2.nPixelAxes(),10);

      LCRegion* pLCRegion = 0;
//      cout << "toLCRegion called with shape = " << shape2 << endl;
      Bool ok = False;
      try {
         pLCRegion = box1.toLCRegion(cSys2, shape2);
         if (pLCRegion != 0) delete pLCRegion;
       } catch (AipsError x) {
//          cout << "aipserror: caught error " << x.getMesg() << endl;
          ok = True;
       } 
      if (!ok) {
         throw(AipsError("Conversion to LCRegion did not fail as expected"));
      }
   }
//   cout << endl;

// Supply wrong shape

   {
//      cout << "world names [ra,dec], [ra, dec]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      setValues (blcI, trcI, shape, checkBox, wBlc, wTrc, cSys1);
      WCBox box1(wBlc, wTrc, cSys1, absRel);

      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      IPosition shape2(cSys2.nPixelAxes()+1,10);
      LCRegion* pLCRegion = 0;

      Bool ok = False;
      try {
//         cout << "toLCRegion called with shape = " << shape2 << endl;
         pLCRegion = box1.toLCRegion(cSys2, shape2);
         if (pLCRegion != 0) delete pLCRegion;
      } catch (AipsError x) {
//         cout << "aipserror: caught expected error " << x.getMesg() << endl;
         ok = True;
      } 
      if (!ok) { 
         throw(AipsError("Conversion to LCRegion did not fail as expected"));
      }
   }
   cout << endl;




// Test constructor with less world values than cSys
// and specify axes

   {
/*
      cout << "world names [ra,dec,freq], [ra,dec,freq]" << endl;
      cout << "blc              x   x" << endl;
      cout << "trc              x   x" << endl;
*/
      CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();      
      setValues (blcI, trcI, shape, checkBox, wBlc, wTrc, cSys);

      IPosition pixelAxes(2); 
      pixelAxes(0) = 1;
      pixelAxes(1) = 2;
      Vector<Quantum<Double> > blc(2); 
      blc(0) = wBlc(pixelAxes(0));
      blc(1) = wBlc(pixelAxes(1));
      Vector<Quantum<Double> > trc(2); 
      trc(0) = wTrc(pixelAxes(0));
      trc(1) = wTrc(pixelAxes(1));
//      cout << "Construction with specified pixel axes" << endl;
//      cout << "pixelAxes = " << pixelAxes << endl;
      WCBox box2(blc, trc, pixelAxes, cSys, absRel);
//      cout << "toLCRegion called with shape = " << shape << endl;
      LCRegion* pLCRegion = box2.toLCRegion(cSys, shape);
//
      blcI(0) = 0;
      trcI(0) = shape(0)-1;
      LCBox checkBox2(blcI, trcI, shape);
      AlwaysAssert(*pLCRegion==checkBox2, AipsError);
      if (pLCRegion != 0) delete pLCRegion;
   }
//   cout << endl;


} catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      cout << "not ok" << endl; 
      return 1;
}

  cout << "ok" << endl; 
  return 0;
 
}
 


void listBB(const LCRegion* pLCRegion)
{
   if (pLCRegion == 0) {
     cout << "You gave me a null pointer" << endl;
     return;
   }
   cout << "Bounding box = " << pLCRegion->boundingBox().start() 
        << pLCRegion->boundingBox().end() << endl;
}



void setValues (IPosition& blcI,
                IPosition& trcI,
                IPosition& shape,
                LCBox& checkBox,
                Vector<Quantum<Double> >& wBlc,
                Vector<Quantum<Double> >& wTrc,
                const CoordinateSystem& cSys)
{ 
   uInt nDim = cSys.nPixelAxes();
   shape.resize(nDim);
   blcI.resize(nDim);
   trcI.resize(nDim);
   uInt i;
   for (i=0; i<nDim; i++) {
      shape(i) = 10*(i+1) + 1;
      blcI(i) = 2*(i+1);
      trcI(i) = shape(i) - blcI(i);
   }
   LCBox tmp(blcI, trcI, shape);
   checkBox = tmp;

// Make some world values

   Vector<Double> pBlc(nDim);
   Vector<Double> pTrc(nDim);
   Vector<Double> wBlc2(nDim);
   Vector<Double> wTrc2(nDim);

   for (i=0; i<nDim; i++) {
      pBlc(i) = blcI(i);
      pTrc(i) = trcI(i);
   }
/*
   cout << "shape = "<< shape << endl;
   cout << "pBlc = " << pBlc << endl;
   cout << "pTrc = " << pTrc << endl;
*/

// The order of the world values reflects
// the mapping of pixel axis to world axis

   cSys.toWorld(wBlc2, pBlc);
   cSys.toWorld(wTrc2, pTrc);

   uInt j;
   for (j=0; j<wBlc2.nelements();j++) {
//      cout << "i, wBlc = " << j << ", " << wBlc2(j) << endl;
   }
   for (j=0; j<wTrc2.nelements();j++) {
//      cout << "i, wTrc = " << j << ", " << wTrc2(j) << endl;
   }


// Fill quanta

   wBlc.resize(wBlc2.nelements());
   wTrc.resize(wTrc2.nelements());

   for (i=0;i<nDim;i++){
      Int worldAxis = cSys.pixelAxisToWorldAxis(i);
      if (worldAxis >=0) {
         wBlc(i) = Quantum<Double>(wBlc2(i), cSys.worldAxisUnits()(worldAxis));
         wTrc(i) = Quantum<Double>(wTrc2(i), cSys.worldAxisUnits()(worldAxis));
      }
   }
}


void list (const RecordInterface& record)
{
   for (uInt j=0; j<record.nfields(); j++) {
        cout << "field " << record.name(j) << " is of type " << record.type(j) << endl;
   }

   Vector<Int> axes = Vector<Int>(record.asArrayInt ("pixelAxes"));
   Vector<Int> absRel = Vector<Int>(record.asArrayInt("absrel"));
   cout << "axes=" << axes << endl;
   cout << "absRel=" << absRel << endl;
}
