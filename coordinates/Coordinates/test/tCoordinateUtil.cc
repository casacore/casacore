//# tCoordinateUtil.cc: Test program for CoordinateUtil class
//# Copyright (C) 1998,1999,2000,2001,2003
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
//#
//# $Id$

#include <casacore/casa/Arrays/MaskArrLogi.h>
#include <casacore/coordinates/Coordinates.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void test0();
void test1();
void test2();
void test3();
void test4();

int main()
{
try {
   test0();
   test1();
   test2();
   test3();
   test4();
}
catch (const AipsError& x) {
	cerr << "aipserror: error " << x.getMesg() << endl;
	return 1;
}
 
  return 0;

}


void test0()
{
  CoordinateSystem csys2 = CoordinateUtil::defaultCoords2D();
  CoordinateSystem csys3 = CoordinateUtil::defaultCoords3D();
  CoordinateSystem csys4 = CoordinateUtil::defaultCoords4D();
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys2, csys2) == 0);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys3, csys3) == 0);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys4, csys4) == 0);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys2, csys3) == -1);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys2, csys4) == -1);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys3, csys4) == -1);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys3, csys2) == 1);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys4, csys2) == 1);
  AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys4, csys3) == 1);

  IPosition newAxes, stretchAxes;
  AlwaysAssertExit (CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
						    IPosition(4,5,6,1,5),
						    IPosition(4,5,1,1,1),
						    csys4, csys4));
  AlwaysAssertExit (newAxes.isEqual (IPosition()));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition(2,1,3)));
  AlwaysAssertExit (CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
						    IPosition(3,5,6,3),
						    IPosition(2,5,6),
						    csys3, csys2));
  AlwaysAssertExit (newAxes.isEqual (IPosition(1,2)));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition()));
  AlwaysAssertExit (CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
						    IPosition(4,5,6,3,4),
						    IPosition(2,5,6),
						    csys4, csys2));
  AlwaysAssertExit (newAxes.isEqual (IPosition(2,2,3)));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition()));
  AlwaysAssertExit (CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
						    IPosition(4,5,6,3,4),
						    IPosition(3,5,6,1),
						    csys4, csys3));
  AlwaysAssertExit (newAxes.isEqual (IPosition(1,2)));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition(1,3)));
}

void test1()
{
  IPosition newAxes, stretchAxes;
  {
    CoordinateSystem csys1;
    CoordinateSystem csys2;
    CoordinateUtil::addDirAxes (csys1);
    CoordinateUtil::addFreqAxis (csys2);
    AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys1, csys2) == 9);
    AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys2, csys1) == 9);
    AlwaysAssertExit (! CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
							IPosition(2,5,6),
							IPosition(1,8),
							csys1, csys2));

    CoordinateUtil::addDirAxes (csys2);
    AlwaysAssertExit (CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
						      IPosition(3,8,5,6),
						      IPosition(2,1,1),
						      csys2, csys1));
    AlwaysAssertExit (newAxes.isEqual (IPosition(1,0)));
    AlwaysAssertExit (stretchAxes.isEqual (IPosition(2,1,2)));

    CoordinateUtil::addFreqAxis (csys1);
    AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys1, csys2) == 9);
    AlwaysAssertExit (CoordinateUtil::compareCoordinates (csys2, csys1) == 9);
    AlwaysAssertExit (! CoordinateUtil::findExtendAxes (newAxes, stretchAxes,
							IPosition(3,5,6,3),
							IPosition(3,3,4,6),
							csys1, csys2));
  }
}


void test2 ()
{
// 
// DirectionCoordinate
//
   cout << "" << endl;
   cout << "DirectionCoordinate" << endl;
   cout << "*******************" << endl;
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      Vector<Int> pixelAxes, worldAxes;
      Int coordinate;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys);
      cout << "Pixel axes= " << pixelAxes << endl;
      cout << "World axes= " << worldAxes << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "Remove pixel axis 1 (DEC)" << endl;
      cSys.removePixelAxis(1, 0.0);
      Vector<Int> pixelAxes, worldAxes;
      Int coordinate;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys);
      cout << "Pixel axes= " << pixelAxes << endl;
      cout << "World axes= " << worldAxes << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "Remove world axis 1 (DEC)" << endl;
      cSys.removeWorldAxis(1, 0.0);
      Vector<Int> pixelAxes, worldAxes;
      Int coordinate;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys);
      cout << "Pixel axes= " << pixelAxes << endl;
      cout << "World axes= " << worldAxes << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "Remove pixel axis 0 (RA)" << endl;
      cout << "Remove world axis 1 (DEC)" << endl;
      cSys.removePixelAxis(0, 0.0);
      cSys.removeWorldAxis(1, 0.0);
      Vector<Int> pixelAxes, worldAxes;
      Int coordinate;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys);
      cout << "Pixel axes= " << pixelAxes << endl;
      cout << "World axes= " << worldAxes << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "Remove world axis 0 (RA)" << endl;
      cout << "Remove world axis 1 (DEC)" << endl;
      cSys.removeWorldAxis(0, 0.0);
      cSys.removeWorldAxis(0, 0.0);     // Shuffle down one
      Vector<Int> pixelAxes, worldAxes;
      Int coordinate;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys);
      cout << "Pixel axes= " << pixelAxes << endl;
      cout << "World axes= " << worldAxes << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2 << endl << endl;
   }
// 
// SpectralCoordinate
//
   cout << "" << endl;
   cout << "Spectral Coordinate" << endl;
   cout << "*******************" << endl;
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "No spectral axis" << endl;
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findSpectralAxis(cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findSpectralAxis(cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
      cout << "Remove pixel axis 2 (Spectral)" << endl;
      cSys.removePixelAxis(2, 0.0);
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findSpectralAxis(cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
      cout << "Remove world axis 2 (Spectral)" << endl;
      cSys.removeWorldAxis(2, 0.0);
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findSpectralAxis(cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }

// 
// StokesCoordinate
//
   cout << "" << endl;
   cout << "Stokes Coordinate" << endl;
   cout << "*******************" << endl;
   Vector<Stokes::StokesTypes> whichPols;
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "No stokes axis" << endl;
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findStokesAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findStokesAxis(whichPols, cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords4D();
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findStokesAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findStokesAxis(whichPols, cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords4D();
      cout << "Remove pixel axis 2 (Stokes)" << endl;
      cSys.removePixelAxis(2, 0.0);
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findStokesAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findStokesAxis(whichPols, cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords4D();
      cout << "Remove world axis 2 (Stokes)" << endl;
      cSys.removeWorldAxis(2, 0.0);
      Int pixelAxis, worldAxis;
      Int coordinate;
      CoordinateUtil::findStokesAxis(pixelAxis, worldAxis, coordinate, cSys);
      cout << "Pixel axis= " << pixelAxis << endl;
      cout << "World axis= " << worldAxis  << endl;
      cout << "Coordinate = " << coordinate << endl;
      Int pixelAxis2 = 
        CoordinateUtil::findStokesAxis(whichPols, cSys);
      cout << "Pixel axis2 = " << pixelAxis2 << endl << endl;
   }

// addStokesAxis

   {
      CoordinateSystem cSys;
      CoordinateUtil::addStokesAxis(cSys, 4);
      Int afterCoord = -1;
      Int coordinate = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
      uInt nPixelAxes = cSys.nPixelAxes();
      uInt nWorldAxes = cSys.nWorldAxes();
      if (coordinate!=0 || nPixelAxes!=1 || nWorldAxes!=1 ||
          cSys.type(coordinate)!=Coordinate::STOKES) {          
         throw(AipsError("addStokesAxis failed"));
      }
   }   

// addLinearAxes

   {
      const uInt n = 4;
      CoordinateSystem cSys;
      Vector<String> names(n);
      names(0) = "axis0";
      names(1) = "axis1";
      names(2) = "axis2";
      names(3) = "axis3";
      IPosition shape;
      CoordinateUtil::addLinearAxes(cSys, names, shape);
      Int coordinate;
      Int afterCoord = -1;
      coordinate = cSys.findCoordinate(Coordinate::LINEAR, afterCoord);
//      
      uInt nPixelAxes = cSys.nPixelAxes();
      uInt nWorldAxes = cSys.nWorldAxes();
      Vector<Double> refPix = cSys.referencePixel();
//
      if (coordinate!=0 || nPixelAxes!=n || nWorldAxes!=n ||
          cSys.type(coordinate)!=Coordinate::LINEAR ||
          !::allNear(refPix, Double(0.0), Double(1.0e-6))) {
         throw(AipsError("addLinearAxes failed"));
      }
   }   
   {
      const uInt n = 2;
      CoordinateSystem cSys;
      Vector<String> names(n);
      names(0) = "axis0";
      names(1) = "axis1";
      IPosition shape(n, 100);
      CoordinateUtil::addLinearAxes(cSys, names, shape);
      Int coordinate;
      Int afterCoord = -1;
      coordinate = cSys.findCoordinate(Coordinate::LINEAR, afterCoord);
//      
      uInt nPixelAxes = cSys.nPixelAxes();
      uInt nWorldAxes = cSys.nWorldAxes();
      Vector<Double> refPix = cSys.referencePixel();
//
      if (coordinate!=0 || nPixelAxes!=n || nWorldAxes!=n ||
          cSys.type(coordinate)!=Coordinate::LINEAR ||
          !::allNear(refPix, Double(50.0), Double(1e-6))) {
         throw(AipsError("addLinearAxes failed"));
      }
   }   
// 
// makeCoordinateSYstem.  A lot of built in knowledge in
// the verification process.  Urk.
//
   {
      IPosition shape(1, 10);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shape);
      Int coordinate;
      Int afterCoord = -1;
      coordinate = cSys.findCoordinate(Coordinate::SPECTRAL, afterCoord);
      if (coordinate!=0 || cSys.nPixelAxes()!=1 || cSys.nWorldAxes()!=1 ||
          cSys.type(coordinate)!=Coordinate::SPECTRAL) {
         throw(AipsError("makeCoordinateSystem 1 failed"));
      }
   }   
   {
      IPosition shape(2, 10, 10);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shape);
      Int coordinate;
      Int afterCoord = -1;
      coordinate = cSys.findCoordinate(Coordinate::DIRECTION, afterCoord);
      if (coordinate!=0 || cSys.nPixelAxes()!=2 || cSys.nWorldAxes()!=2 ||
          cSys.type(coordinate)!=Coordinate::DIRECTION) {
         throw(AipsError("makeCoordinateSystem 2 failed"));
      }
   }   
   {
      IPosition shape(3, 10, 10, 4);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shape);
      Int c0, c1;
      Int afterCoord = -1;
      c0 = cSys.findCoordinate(Coordinate::DIRECTION, afterCoord);
      c1 = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
      if (c0 !=0 || cSys.type(c0)!=Coordinate::DIRECTION ||
          c1 !=1 || cSys.type(c1)!=Coordinate::STOKES) {
         throw(AipsError("makeCoordinateSystem 3a failed"));
      }
      if (cSys.nPixelAxes()!=3 || cSys.nWorldAxes()!=3) {
         throw(AipsError("makeCoordinateSystem 3b failed"));
      }
   }   
   {
      IPosition shape(4, 10, 10, 4, 16);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shape);
      Int c0, c1, c2;
      Int afterCoord = -1;
      c0 = cSys.findCoordinate(Coordinate::DIRECTION, afterCoord);
      c1 = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
      c2 = cSys.findCoordinate(Coordinate::SPECTRAL, afterCoord);
      if (c0 !=0 || cSys.type(c0)!=Coordinate::DIRECTION ||
          c1 !=1 || cSys.type(c1)!=Coordinate::STOKES ||
          c2 !=2 || cSys.type(c2)!=Coordinate::SPECTRAL) {
         throw(AipsError("makeCoordinateSystem 4a failed"));
      }
      if (cSys.nPixelAxes()!=4 || cSys.nWorldAxes()!=4) {
         throw(AipsError("makeCoordinateSystem 4b failed"));
      }
   }   
   {
      IPosition shape(4, 10, 10, 16, 4);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shape);
      Int c0, c1, c2;
      Int afterCoord = -1;
      c0 = cSys.findCoordinate(Coordinate::DIRECTION, afterCoord);
      c1 = cSys.findCoordinate(Coordinate::SPECTRAL, afterCoord);
      c2 = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
      if (c0 !=0 || cSys.type(c0)!=Coordinate::DIRECTION ||
          c1 !=1 || cSys.type(c1)!=Coordinate::SPECTRAL ||
          c2 !=2 || cSys.type(c2)!=Coordinate::STOKES) {
         throw(AipsError("makeCoordinateSystem 5a failed"));
      }
      if (cSys.nPixelAxes()!=4 || cSys.nWorldAxes()!=4) {
         throw(AipsError("makeCoordinateSystem 5b failed"));
      }
   }   
   {
      IPosition shape(6, 10, 10, 16, 4, 2, 3);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shape);
      Int c0, c1, c2, c3;
      Int afterCoord = -1;
      c0 = cSys.findCoordinate(Coordinate::DIRECTION, afterCoord);
      c1 = cSys.findCoordinate(Coordinate::SPECTRAL, afterCoord);
      c2 = cSys.findCoordinate(Coordinate::STOKES, afterCoord);
      c3 = cSys.findCoordinate(Coordinate::LINEAR, afterCoord);
      if (c0 !=0 || cSys.type(c0)!=Coordinate::DIRECTION ||
          c1 !=1 || cSys.type(c1)!=Coordinate::SPECTRAL ||
          c2 !=2 || cSys.type(c2)!=Coordinate::STOKES ||
          c3 !=3 || cSys.type(c3)!=Coordinate::LINEAR) {
         throw(AipsError("makeCoordinateSystem 6a failed"));
      }
      if (cSys.nPixelAxes()!=6 || cSys.nWorldAxes()!=6) {
         throw(AipsError("makeCoordinateSystem 6b"));
      }
   }   
}


void test3 ()
// 
// Test function dropRemovedAxes.  4D makes Direction, Stokes, Spectral
//
{

// No removed axes

   {
      CoordinateSystem cSysIn = CoordinateUtil::defaultCoords4D();      
      CoordinateSystem cSysOut;
      Bool dropped = CoordinateUtil::dropRemovedAxes(cSysOut, cSysIn);
      AlwaysAssert(dropped==False, AipsError);
      AlwaysAssert(cSysIn.near(cSysOut), AipsError);
   }

// Remove world&pixel axis for spectral axis - can be fully dropped

   {
      CoordinateSystem cSysIn = CoordinateUtil::defaultCoords4D();      
      Int pixelAxis, worldAxis, coord;
      CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coord, cSysIn);      
      cSysIn.removeWorldAxis(worldAxis, 0.0);
//
      CoordinateSystem cSysOut;
      Bool dropped = CoordinateUtil::dropRemovedAxes(cSysOut, cSysIn);
      AlwaysAssert(dropped==True, AipsError);
      AlwaysAssert(cSysOut.nCoordinates()==(cSysIn.nCoordinates()-1), AipsError);
      AlwaysAssert(cSysOut.nPixelAxes()==cSysIn.nPixelAxes(), AipsError);
      AlwaysAssert(cSysOut.nWorldAxes()==cSysIn.nWorldAxes(), AipsError);
      CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coord, cSysOut);
      AlwaysAssert(coord==-1, AipsError);
      AlwaysAssert(worldAxis==-1, AipsError);
      AlwaysAssert(pixelAxis==-1, AipsError);
   }

// Remove pixel axis only for spectral axis - cannot be fully dropped

   {
      CoordinateSystem cSysIn = CoordinateUtil::defaultCoords4D();      
      Int pixelAxis, worldAxis, coord;
      CoordinateUtil::findSpectralAxis(pixelAxis, worldAxis, coord, cSysIn);      
      cSysIn.removePixelAxis(pixelAxis, 0.0);
//
      CoordinateSystem cSysOut;
      Bool dropped = CoordinateUtil::dropRemovedAxes(cSysOut, cSysIn);
      AlwaysAssert(dropped==False, AipsError);
      AlwaysAssert(cSysOut.nCoordinates()==cSysIn.nCoordinates(), AipsError);
      AlwaysAssert(cSysOut.nPixelAxes()==cSysIn.nPixelAxes(), AipsError);
      AlwaysAssert(cSysOut.nWorldAxes()==cSysIn.nWorldAxes(), AipsError);
      Vector<Int> pixelAxes = cSysOut.pixelAxes(coord);
      Vector<Int> worldAxes = cSysOut.worldAxes(coord);
      AlwaysAssert(pixelAxes(0)==-1, AipsError);
      AlwaysAssert(worldAxes(0)==worldAxis, AipsError);
   }

// Remove world and pixel axis for half of DirectionCoordinate - cannot be fully dropped

   {
      CoordinateSystem cSysIn = CoordinateUtil::defaultCoords4D();      
      Vector<Int> pixelAxes, worldAxes;
      Int coord;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coord, cSysIn);      
      cSysIn.removeWorldAxis(worldAxes(0), 0.0);
//
      CoordinateSystem cSysOut;
      Bool dropped = CoordinateUtil::dropRemovedAxes(cSysOut, cSysIn);
      AlwaysAssert(dropped==False, AipsError);
      AlwaysAssert(cSysOut.nCoordinates()==cSysIn.nCoordinates(), AipsError);
      AlwaysAssert(cSysOut.nPixelAxes()==cSysIn.nPixelAxes(), AipsError);
      AlwaysAssert(cSysOut.nWorldAxes()==cSysIn.nWorldAxes(), AipsError);
      Vector<Int> pixelAxesOut = cSysOut.pixelAxes(coord);
      Vector<Int> worldAxesOut = cSysOut.worldAxes(coord);
      AlwaysAssert(pixelAxesOut(0)==-1, AipsError);
      AlwaysAssert(worldAxesOut(0)==-1, AipsError);
      AlwaysAssert(pixelAxesOut(1)>=0, AipsError);
      AlwaysAssert(worldAxesOut(1)>=0, AipsError);
   }

// Remove world and pixel axis for all  of DirectionCoordinate - can be fully dropped

   {
      CoordinateSystem cSysIn = CoordinateUtil::defaultCoords4D();      
      Vector<Int> pixelAxes, worldAxes;
      Int coord;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coord, cSysIn);      
      cSysIn.removeWorldAxis(worldAxes(1), 0.0);
      cSysIn.removeWorldAxis(worldAxes(0), 0.0);
//
      CoordinateSystem cSysOut;
      Bool dropped = CoordinateUtil::dropRemovedAxes(cSysOut, cSysIn);
      AlwaysAssert(dropped==True, AipsError);
      AlwaysAssert(cSysOut.nCoordinates()==cSysIn.nCoordinates()-1, AipsError);
      AlwaysAssert(cSysOut.nPixelAxes()==cSysIn.nPixelAxes(), AipsError);
      AlwaysAssert(cSysOut.nWorldAxes()==cSysIn.nWorldAxes(), AipsError);
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coord, cSysOut);
      AlwaysAssert(coord==-1, AipsError);
      AlwaysAssert(worldAxes.nelements()==0, AipsError);
      AlwaysAssert(pixelAxes.nelements()==0, AipsError);
   }

   {
	   // axis order is preserved when dropping an axis.
	   CoordinateSystem cSysIn = CoordinateUtil::defaultCoords4D();
	   Vector<Int> order(4);
	   order[0] = 0;
	   order[1] = 1;
	   order[2] = 3;
	   order[3] = 2;
	   cSysIn.transpose(order, order);
	   cSysIn.removePixelAxis(0, 0.0);
	   CoordinateSystem cSysOut;
	   Bool dropped = CoordinateUtil::dropRemovedAxes(cSysOut, cSysIn, False);
	   AlwaysAssert(dropped==False, AipsError);
	   AlwaysAssert(
	       cSysOut.spectralAxisNumber() != cSysIn.spectralAxisNumber(),
	       AipsError
	   );
	   AlwaysAssert(
           cSysOut.polarizationAxisNumber() != cSysIn.polarizationAxisNumber(),
           AipsError
       );
	   AlwaysAssert(
		   cSysOut.worldAxes(cSysOut.spectralCoordinateNumber())[0]
		   != cSysIn.worldAxes(cSysIn.spectralCoordinateNumber())[0],
		   AipsError
	   );
	   AlwaysAssert(
	       cSysOut.worldAxes(cSysOut.polarizationCoordinateNumber())[0]
	       != cSysIn.worldAxes(cSysIn.polarizationCoordinateNumber())[0],
	   	   AipsError
	   );
	   cSysOut = CoordinateSystem();
	   dropped = CoordinateUtil::dropRemovedAxes(cSysOut, cSysIn, True);

	   AlwaysAssert(
	       cSysOut.spectralAxisNumber() == cSysIn.spectralAxisNumber(),
	   	   AipsError
	   );
	   AlwaysAssert(
	       cSysOut.polarizationAxisNumber() == cSysIn.polarizationAxisNumber(),
	       AipsError
	   );
	   AlwaysAssert(
	       cSysOut.worldAxes(cSysOut.spectralCoordinateNumber())[0]
	   	   == cSysIn.worldAxes(cSysIn.spectralCoordinateNumber())[0],
	   	   AipsError
	   );
	   AlwaysAssert(
	       cSysOut.worldAxes(cSysOut.polarizationCoordinateNumber())[0]
	       == cSysIn.worldAxes(cSysIn.polarizationCoordinateNumber())[0],
	       AipsError
	   );
   }
}


void test4 ()
//
// test function axisLabel
//
{
   CoordinateSystem cSys = CoordinateUtil::defaultCoords4D();      
//
   uInt axis = 0;
   Bool doWorld = True;
   Bool doAbs = True;
   Bool doVel = False;
   String label;
//
    for (uInt i=0; i<cSys.nCoordinates(); i++) {
       Coordinate::Type cType = cSys.type(i);
       for (uInt j=0; j<cSys.worldAxes(i).nelements(); j++) {
          axis = j;    
//
          if (cType==Coordinate::SPECTRAL) {
             doVel = False;
             label = CoordinateUtil::axisLabel (cSys.coordinate(i), axis,  
                                                doWorld, doAbs, doVel);
             cerr << "Label = " << label << endl;
//
             doVel = True;
             label = CoordinateUtil::axisLabel (cSys.coordinate(i), axis,  
                                                doWorld, doAbs, doVel);
             cerr << "Label = " << label << endl;
          } else {
             label = CoordinateUtil::axisLabel (cSys.coordinate(i), axis,  
                                                doWorld, doAbs, doVel);
             cerr << "Label = " << label << endl;
          }
       }
    }
    cerr << endl;
}

