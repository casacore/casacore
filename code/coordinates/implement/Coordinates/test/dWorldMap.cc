#include <aips/aips.h>
#include <aips/Arrays.h>
#include <aips/Inputs/Input.h>
#include <aips/Utilities/String.h>
#include <aips/Exceptions/Error.h>
#include <trial/Coordinates.h>
#include <iostream.h>

void list (Bool ok, Vector<Int>& map,
           Vector<Int>& transpose,
           CoordinateSystem& cSys1,
           CoordinateSystem& cSys2);


main (int argc, char **argv)
//
// Test out the worldMap function in CoordinateSYstem
{
try {

   Vector<Int> map;
   Vector<Int> transpose;

   {
      cout << "2D [ra, dec] & 0D" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      CoordinateSystem cSys2;
      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "0D & 2D [ra, dec]" << endl;
      CoordinateSystem cSys1;
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }


   {
      cout << "3D [ra, dec, spec] & 3D [ra, dec, spec]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords3D();
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "2D [ra, dec] & 3D [ra, dec, spec]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "3D [ra, dec, spec] & 2D [ra, dec]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords3D();
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();
      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "3D [ra, dec, spec] & 3D [dec, spec, ra]" << endl;
     
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords3D();
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Vector<Int> worldOrder(cSys2.nWorldAxes());
      Vector<Int> pixelOrder(cSys2.nPixelAxes());
      worldOrder(0) = 1; worldOrder(1) = 2; worldOrder(2) = 0;
      pixelOrder = worldOrder;
      cSys2.transpose(worldOrder, pixelOrder);
  
      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "2D [ra,dec] & 3D [ra, dec, spec] " << endl;
      cout << "   [0, 1]   &    [0, 1, -1]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();
      Int iSpec = cSys2.findCoordinate(Coordinate::SPECTRAL);
      uInt iSpec2 = iSpec;
      Vector<Int> worldAxes = cSys2.worldAxes(iSpec2);
      Vector<Int> pixelAxes = cSys2.pixelAxes(iSpec2);
      cSys2.removeWorldAxis(worldAxes(0), 0.0);
      cSys2.removePixelAxis(pixelAxes(0), 0.0);

      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "2D [ra,dec] & 3D [spec, dec, ra] " << endl;
      cout << "   [0, 1]   &    [-1, 0, 1]" << endl;
      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords3D();

      Vector<Int> worldOrder(cSys2.nWorldAxes());
      Vector<Int> pixelOrder(cSys2.nPixelAxes());
      worldOrder(0) = 2; worldOrder(1) = 1; worldOrder(2) = 0;
      pixelOrder = worldOrder;
      cSys2.transpose(worldOrder, pixelOrder);

      Int iSpec = cSys2.findCoordinate(Coordinate::SPECTRAL);
      uInt iSpec2 = iSpec;
      Vector<Int> worldAxes = cSys2.worldAxes(iSpec2);
      Vector<Int> pixelAxes = cSys2.pixelAxes(iSpec2);
      cSys2.removeWorldAxis(worldAxes(0), 0.0);
      cSys2.removePixelAxis(pixelAxes(0), 0.0);

      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "3D [spec, dec, ra] & 2D [ra,dec]" << endl;
      cout << "   [-1, 0, 1]      &    [0,  1]" << endl;

      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords3D();
      CoordinateSystem cSys2 = CoordinateUtil::defaultCoords2D();

      Vector<Int> worldOrder(cSys1.nWorldAxes());
      Vector<Int> pixelOrder(cSys1.nPixelAxes());
      worldOrder(0) = 2; worldOrder(1) = 1; worldOrder(2) = 0;
      pixelOrder = worldOrder;
      cSys1.transpose(worldOrder, pixelOrder);

      Int iSpec = cSys1.findCoordinate(Coordinate::SPECTRAL);
      uInt iSpec2 = iSpec;
      Vector<Int> worldAxes = cSys1.worldAxes(iSpec2);
      Vector<Int> pixelAxes = cSys1.pixelAxes(iSpec2);
      cSys1.removeWorldAxis(worldAxes(0), 0.0);
      cSys1.removePixelAxis(pixelAxes(0), 0.0);

      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }

   {
      cout << "2D [ra, dec] & 2D [spec, stokes]" << endl;

      CoordinateSystem cSys1 = CoordinateUtil::defaultCoords2D();
      CoordinateSystem cSys2;
      CoordinateUtil::addFreqAxis(cSys2);
      CoordinateUtil::addIQUVAxis(cSys2);

      Bool ok = cSys1.worldMap(map, transpose, cSys2);
      list (ok, map, transpose, cSys1, cSys2);
      cout << endl << endl;
   }



}
   catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      exit(1);
  }end_try;
 
  exit(0);
 
}

void list (Bool ok, Vector<Int>& map,
           Vector<Int>& transpose,
           CoordinateSystem& cSys1,
           CoordinateSystem& cSys2)
{
   cout << endl;
   if (!ok) {
      cout << "Failed with message " << cSys1.errorMessage() << endl;
   } else {
      cout << "Success returned" << endl;
    }
   cout << "cSys1.worldAxisNames = " << cSys1.worldAxisNames().ac() << endl;
   cout << "cSys2.worldAxisNames = " << cSys2.worldAxisNames().ac() << endl;
   cout << "map = " << map.ac() << endl;
   cout << "transpose = " << transpose.ac() << endl << endl;
}
