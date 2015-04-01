//# tImageRegrid.cc: This program test Measure functionsimage regridding
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
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/images/Images/TempImage.h>
#include <casacore/images/Images/SubImage.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/ImageRegrid.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/lattices/Lattices/MaskedLattice.h> 
#include <casacore/lattices/LRegions/LCPagedMask.h> 
#include <casacore/lattices/Lattices/TempLattice.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/scimath/Mathematics/Interpolate2D.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <set>



#include <casacore/casa/namespace.h>
int main (int argc, const char* argv[])
{

try {

   Input inputs(1);
   inputs.version ("$Revision$");

// Get inputs

   inputs.create("in", "", "Input image name");
   inputs.create("axes", "-10", "axes");
   inputs.create("method", "linear", "Method");
   inputs.create("save", "False", "Save output ?");
   inputs.create("shape", "-10", "Shape");
   inputs.create("replicate", "False", "Replicate ?");
   inputs.create("decimate", "0", "Decimation factor");
   inputs.create("disk", "False", "Image on disk");
   inputs.create("reuse", "False", "Reuse coordinate grid");
   inputs.create("dbg", "0", "Debug level");
   inputs.create("double", "0", "Double size ?");
   inputs.create("force", "False", "Force regridding ?");
   inputs.readArguments(argc, argv);
   const String in = inputs.getString("in");
   const Bool save = inputs.getBool("save");
   const String method = inputs.getString("method");
   const Block<Int> axesU(inputs.getIntArray("axes"));
   const Block<Int> shapeU(inputs.getIntArray("shape"));
   const Bool replicate = inputs.getBool("replicate");
   const Int decimate = inputs.getInt("decimate");
   const Bool onDisk = inputs.getBool("disk");
   const Bool dbl = inputs.getBool("double");
   const Int dbg = inputs.getInt("dbg");
   const Bool force = inputs.getBool("force");
   const Bool reuse = inputs.getBool("reuse");
//
   Int maxMBInMemory = -1;
   if (onDisk) maxMBInMemory = 0;
//
   ImageInterface<Float>* pIm = 0;

   IPosition shapeIn;
   if (in.empty()) {
      if (shapeU.nelements()>0) {
         if (shapeU.nelements()==1 && shapeU[0]==-10) {
            shapeIn = IPosition(2, 256, 256);
         } else {
            shapeIn.resize(shapeU.nelements());
            for (uInt i=0; i<shapeIn.nelements(); i++) shapeIn(i) = shapeU[i];
         }
      }
//
      TiledShape shape2(shapeIn);
      CoordinateSystem cSys = CoordinateUtil::makeCoordinateSystem(shapeIn, False);
//
      pIm = new TempImage<Float>(shape2, cSys, maxMBInMemory);
      pIm->set(1.0);
//
      TempLattice<Bool> inMask(shape2, maxMBInMemory);
      inMask.set(True);
      TempImage<Float>* pTemp = dynamic_cast<TempImage<Float>*>(pIm);
      pTemp->attachMask(inMask);
   } else {
      pIm = new PagedImage<Float>(in);
      shapeIn = pIm->shape();
   }
//
   IPosition axes = IPosition::makeAxisPath(pIm->ndim());
   if (axesU.nelements()>0) {
      if (axesU.nelements()==1 && axesU[0]==-10) {
      } else {
         axes.resize(axesU.nelements());
         for (uInt i=0; i<axes.nelements(); i++) axes(i) = axesU[i];
      }
   }
//
   IPosition shapeOut;
   CoordinateSystem cSysOut = pIm->coordinates();
   if (dbl) {
      Vector<Double> incr = cSysOut.increment().copy();
      Vector<Double> refp  = cSysOut.referencePixel().copy();
      Vector<Double> refv  = cSysOut.referenceValue().copy();
//
      shapeOut = shapeIn;
      for (uInt i=0; i<axes.nelements(); i++) {
         uInt j = axes(i);
         shapeOut(j) = 2 * shapeIn(j);
         incr(j) = incr(j) / 2.0;
         refp(j) = shapeOut(j) / 2.0;              // Center
      }
      cSysOut.setReferencePixel(refp);
      cSysOut.setIncrement(incr);
   } else {
      if (shapeU.nelements()==1 && shapeU[0]==-10) {
         shapeOut = 2*shapeIn;
      } else if (shapeU.nelements() > 0) {
         for (uInt i=0; i<shapeU.nelements(); i++) {
            shapeOut(i) = shapeU[i];
         }
      }
   }
   cerr << "shapeIn, shapeOut = " << shapeIn << shapeOut << endl;
//
   ImageRegrid<Float> regridder;
   {
      ImageInterface<Float>* pImOut = 0;
      if (save) {
         pImOut = new PagedImage<Float>(shapeOut, cSysOut, String("outFile"));
      } else {
         pImOut = new TempImage<Float>(shapeOut, cSysOut, maxMBInMemory);
      }
      String maskName = pImOut->makeUniqueRegionName(String("mask"), 0);    
      pImOut->makeMask(maskName, True, True, True, True);
//
      Interpolate2D::Method emethod = Interpolate2D::stringToMethod(method);
      regridder.showDebugInfo(dbg);
      regridder.regrid(*pImOut, emethod, axes, *pIm, replicate, decimate, False, force);
      delete pImOut;
    }
//
    if (reuse) {
      ImageInterface<Float>* pImOut = 0;
      if (save) {
         pImOut = new PagedImage<Float>(shapeOut, cSysOut, String("outFileReused"));
      } else {
         pImOut = new TempImage<Float>(shapeOut, cSysOut, maxMBInMemory);
      }
      String maskName = pImOut->makeUniqueRegionName(String("mask"), 0);    
      pImOut->makeMask(maskName, True, True, True, True);
//
      Interpolate2D::Method emethod = Interpolate2D::stringToMethod(method);
      Cube<Double> grid;
      Matrix<Bool> gridMask;
      regridder.get2DCoordinateGrid(grid, gridMask);
      regridder.set2DCoordinateGrid(grid, gridMask);
      regridder.regrid(*pImOut, emethod, axes, *pIm, replicate, decimate, False, force);
//
      grid.resize();
      gridMask.resize();
      regridder.set2DCoordinateGrid(grid, gridMask);
      regridder.regrid(*pImOut, emethod, axes, *pIm, replicate, decimate, False, force);
//
      delete pImOut;
    }

      {
    	  cout << "*** Test makeCoordinateSystem" << endl;
    	  CoordinateSystem cIn = CoordinateUtil::defaultCoords2D();
    	  CoordinateSystem cTo = CoordinateUtil::defaultCoords3D();
    	  LogIO os;
    	  cout << "1" << endl;
    	  std::set<Coordinate::Type> coordsToRegrid;
    	  CoordinateSystem cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition(2, 0, 1));
    	  AlwaysAssert(coordsToRegrid.size() == 1, AipsError);
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::DIRECTION) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::SPECTRAL) == coordsToRegrid.end(),
    	      AipsError
    	  );
    	  cIn = CoordinateUtil::defaultCoords3D();
    	  cTo = CoordinateUtil::defaultCoords2D();
    	  cout << "2" << endl;

    	  cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition(2, 0, 1));
    	  AlwaysAssert(coordsToRegrid.size() == 1, AipsError);
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::DIRECTION) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::SPECTRAL) == coordsToRegrid.end(),
    		  AipsError
    	  );
    	  cIn = CoordinateUtil::defaultCoords3D();
    	  cTo = CoordinateUtil::defaultCoords3D();
    	  cout << "3" << endl;

    	  cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition(2, 0, 1));
    	  AlwaysAssert(coordsToRegrid.size() == 1, AipsError);
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::DIRECTION) != coordsToRegrid.end(),
    		  AipsError
    	  );
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::SPECTRAL) == coordsToRegrid.end(),
    		  AipsError
    	  );
    	  cout << "4" << endl;

    	  cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition(1, 2));
    	  AlwaysAssert(coordsToRegrid.size() == 1, AipsError);
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::DIRECTION) == coordsToRegrid.end(),
    	      AipsError
    	  );
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::SPECTRAL) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  cout << "5" << endl;

    	  cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition());
    	  AlwaysAssert(coordsToRegrid.size() == 2, AipsError);
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::DIRECTION) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::SPECTRAL) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  cout << "6" << endl;

    	  cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition(3, 0, 1, 2));
    	  AlwaysAssert(coordsToRegrid.size() == 2, AipsError);
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::DIRECTION) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::SPECTRAL) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  cout << "7" << endl;
    	  cIn = CoordinateUtil::defaultCoords4D();
    	  cTo = CoordinateUtil::defaultCoords4D();
    	  cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition());
    	  AlwaysAssert(coordsToRegrid.size() == 2, AipsError);
    	  AlwaysAssert(
    	      coordsToRegrid.find(Coordinate::DIRECTION) != coordsToRegrid.end(),
    	      AipsError
    	  );
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::SPECTRAL) != coordsToRegrid.end(),
    		  AipsError
    	  );
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::STOKES) == coordsToRegrid.end(),
    		  AipsError
    	  );
    	  cout << "8" << endl;
    	  cOut = ImageRegrid<Float>::makeCoordinateSystem(os, coordsToRegrid, cTo, cIn, IPosition(3, 0, 1, 2));
    	  AlwaysAssert(coordsToRegrid.size() == 1, AipsError);
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::DIRECTION) != coordsToRegrid.end(),
    		  AipsError
    	  );
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::SPECTRAL) == coordsToRegrid.end(),
    		  AipsError
    	  );
    	  AlwaysAssert(
    		  coordsToRegrid.find(Coordinate::STOKES) == coordsToRegrid.end(),
    		  AipsError
    	  );
    }
//
    delete pIm;
    cout << "OK" << endl;
} catch (AipsError x) {
     cerr << "aipserror: error " << x.getMesg() << endl;
     return 1;
} 

return 0;

}


