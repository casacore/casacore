//# tWCBox.cc: Test program for WCBox class
//# Copyright (C) 1997
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

#include <aips/aips.h>
#include <aips/Arrays.h>
#include <trial/Coordinates.h>
#include <aips/Exceptions/Error.h>
#include <aips/Glish/GlishRecord.h>
#include <trial/Images/WCBox.h>
#include <trial/Lattices/LCRegion.h>
#include <trial/Lattices/LCBox.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>

#include <iostream.h>

void setValues (IPosition& blcI,
                IPosition& trcI,
                IPosition& shape,
                Vector<Double>& wBlc,
                Vector<Double>& wTrc,
                const CoordinateSystem& cSys);


Bool check(const LCBox& box,
           const LCRegion* pLCRegion);


main (int argc, char **argv)
{
try {


// Create default Coordinate System

   CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();

// Create vectors

   IPosition shape, blcI, trcI;
   Vector<Double> wBlc, wTrc;

// Create WCBox

   setValues (blcI, trcI, shape, wBlc, wTrc, cSys);
   WCBox box(wBlc, wTrc, cSys);

// Create null box

   WCBox box2;

// Test comparison

   AlwaysAssertExit (box == box);
   AlwaysAssertExit (box2 != box)

// Test assignment

   box2 = box;   
   AlwaysAssertExit (box2 == box);
   AlwaysAssertExit (!(box2 != box));

// Test copy constructor

   WCBox box3(box);
   AlwaysAssertExit (box3 == box);

// Test clone region

   WCRegion* pWCRegion = box.cloneRegion();
   const WCBox* pBox = (const WCBox*)pWCRegion;
   AlwaysAssertExit (*pBox == box);
   if (pWCRegion != 0) delete pWCRegion;

// Test record saving

   TableRecord rec =  box.toRecord("");
   WCBox* pWCBox = WCBox::fromRecord(rec, "");
   AlwaysAssertExit (*pWCBox == box);
   if (pWCBox !=0) delete pWCBox;


// Test conversion

   {
      cout << "[ra,dec,freq], [ra,dec,freq]" << endl;
      cout << "toLCRegion called with shape = " << shape << endl;
      LCRegion* pLCRegion = box.toLCRegion(cSys, shape);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;

// Test conversion to LCRegion with unit change

   {
      cout << "[ra,dec,freq], [ra,dec,freq]" << endl;
      cout << "'   '   Hz     deg  deg Hz " << endl;
      CoordinateSystem cSys2(cSys);
      Vector<String> units = cSys2.worldAxisUnits();
      units(0) = "deg";
      units(1) = "deg";
      Bool ok = cSys2.setWorldAxisUnits(units, True);
      if (!ok) cout << "Failed to set axis units" << endl;

      cout << "toLCRegion called with shape = " << shape << endl;
      LCRegion* pLCRegion = box.toLCRegion(cSys2, shape);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;


// Test conversion to LCRegion with less world axes

   {
      cout << "[ra,dec,freq], [ra,dec]" << endl;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      IPosition shape2(cSys2.nPixelAxes());
      for (uInt i=0; i<shape2.nelements(); i++) shape2(i) = shape(i);
      cout << "toLCRegion called with shape = " << shape2 << endl;
      LCRegion* pLCRegion = box.toLCRegion(cSys2, shape2);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;


// Test conversion to LCRegion with pixel axis removal

   {
      cout << "world names [ra,dec,freq], [ra,dec,freq]" << endl;
      cout << "world axes  [0, 1,  2   ]  [0, 1,  2]" << endl;      
      cout << "pixel axes  [0, 1,  2   ]  [-1,0,  1]" << endl;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      cSys2.removePixelAxis(0,0.0);
      IPosition shape2(cSys2.nPixelAxes());
      for (uInt i=0; i<shape2.nelements(); i++) shape2(i) = shape(i+1);
     
      LCRegion* pLCRegion = 0;
      cout << "toLCRegion called with shape = " << shape2 << endl;
      pLCRegion = box.toLCRegion(cSys2, shape2);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;

// Test conversion to LCRegion with inconsistent spectral axes

   {
      cout << "world names [ra,dec,freq], [ra,dec,freq]" << endl;
      cout << "freq systems         LSR           TOPO" << endl;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Int iSpec = cSys2.findCoordinate(Coordinate::SPECTRAL);
      uInt iSpec2 = iSpec;
      SpectralCoordinate sCoord = cSys2.spectralCoordinate(iSpec2);
      sCoord.setFrequencySystem(MFrequency::TOPO);
      cSys2.replaceCoordinate(sCoord, iSpec2);
    
      LCRegion* pLCRegion = 0;
      cout << "toLCRegion called with shape = " << shape << endl;
      pLCRegion =  box.toLCRegion(cSys2, shape);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;


// More world axes
   {
      cout << "world names [ra,dec,freq], [ra,dec,stokes,freq]" << endl;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords4D();
      IPosition shape2(cSys2.nPixelAxes(),10);
      for (uInt i=0; i<min(shape.nelements(),shape2.nelements()); i++) 
        shape2(i) = shape(i);

      LCRegion* pLCRegion = 0;
      cout << "toLCRegion called with shape = " << shape2 << endl;
      pLCRegion = box.toLCRegion(cSys2, shape2);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;


// Remove spectral axis
   {
      cout << "world names [ra,dec], [ra,dec,freq]" << endl;
      cout << "world axes   0   1     0  1   -1" << endl;
      cout << "pixel axes   0   1     0  1   -1" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      setValues (blcI, trcI, shape, wBlc, wTrc, cSys1);
      WCBox box1(wBlc, wTrc, cSys1);

      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Int iSpec = cSys2.findCoordinate(Coordinate::SPECTRAL);
      uInt iSpec2 = iSpec;
      Vector<Int> worldAxes = cSys2.worldAxes(iSpec2);
      Vector<Int> pixelAxes = cSys2.pixelAxes(iSpec2);
      cSys2.removeWorldAxis(worldAxes(0), 0.0);
      cSys2.removePixelAxis(pixelAxes(0), 0.0);
      IPosition shape2(cSys2.nPixelAxes(),10);
      for (uInt i=0; i<min(shape.nelements(),shape2.nelements()); i++) shape2(i) = shape(i);

      LCRegion* pLCRegion = 0;
      cout << "toLCRegion called with shape = " << shape2 << endl;
      pLCRegion = box1.toLCRegion(cSys2, shape2);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;

// No common axes
   {
      cout << "world names [ra,dec], [freq]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      setValues (blcI, trcI, shape, wBlc, wTrc, cSys1);
      WCBox box1(wBlc, wTrc, cSys1);

      CoordinateSystem cSys2;
      CoordinateUtil::addFreqAxis(cSys2);
      IPosition shape2(cSys2.nPixelAxes(),10);

      LCRegion* pLCRegion = 0;
      cout << "toLCRegion called with shape = " << shape2 << endl;
      pLCRegion = box1.toLCRegion(cSys2, shape2);
      check(LCBox(blcI,trcI,shape), pLCRegion);
      if (pLCRegion != 0) delete pLCRegion;
   }
   cout << endl;

// Supply wrong shape

   {
      cout << "world names [ra,dec], [ra, dec]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      setValues (blcI, trcI, shape, wBlc, wTrc, cSys1);
      WCBox box1(wBlc, wTrc, cSys1);

      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      IPosition shape2(cSys2.nPixelAxes()+1,10);
      LCRegion* pLCRegion = 0;

      try {
         cout << "toLCRegion called with shape = " << shape2 << endl;
         pLCRegion = box1.toLCRegion(cSys2, shape2);
         check(LCBox(blcI,trcI,shape), pLCRegion);
      } catch (AipsError x) {
         cout << "aipserror: caught expected error " << x.getMesg() << endl;
      } end_try;
      if (pLCRegion != 0) delete pLCRegion;
   }

   cout << endl;

} catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      exit(1);
}end_try;
 
  exit(0);
 
}
 


Bool check(const LCBox& box,
           const LCRegion* pLCRegion)
{
   if (pLCRegion == 0) {
     cout << "You gave me a null pointer" << endl;
    return False;
   }
   cout << "Bounding box = " << pLCRegion->box().start() 
        << pLCRegion->box().end() << endl;

   return True;
}



   void setValues (IPosition& blcI,
                   IPosition& trcI,
                   IPosition& shape,
                   Vector<Double>& wBlc,
                   Vector<Double>& wTrc,
                   const CoordinateSystem& cSys)
{ 
   uInt nDim = cSys.nPixelAxes();
   shape.resize(nDim);
   blcI.resize(nDim);
   trcI.resize(nDim);
   for (uInt i=0; i<nDim; i++) {
      shape(i) = 10*(i+1) + 1;
      blcI(i) = 2*(i+1);
      trcI(i) = shape(i) - blcI(i);
   }

// Make some world values

   Vector<Double> pBlc(nDim);
   Vector<Double> pTrc(nDim);
   wBlc.resize(nDim);
   wTrc.resize(nDim);

   for (uInt j=0; j<nDim; j++) {
      pBlc(j) = blcI(j);
      pTrc(j) = trcI(j);
   }
   cout << "shape = "<< shape << endl;
   cout << "pBlc = " << pBlc.ac() << endl;
   cout << "pTrc = " << pTrc.ac() << endl;

   cSys.toWorld(wBlc, pBlc);
   cSys.toWorld(wTrc, pTrc);

   cout << "wBlc = " << wBlc.ac() << endl;
   cout << "wTrc = " << wTrc.ac() << endl;
}

