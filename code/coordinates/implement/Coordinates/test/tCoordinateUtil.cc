//# tCoordinateUtil.cc: Test program for CoordinateUtil class
//# Copyright (C) 1998,1999,2000
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

#include <aips/Arrays/ArrayLogical.h> 
#include <trial/Coordinates.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Exceptions/Error.h>


#include <iostream.h>


int main()
{
try {
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
   catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return 1;
  }
 
  return 0;

}

