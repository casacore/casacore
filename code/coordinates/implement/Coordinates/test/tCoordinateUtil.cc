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
      cout << "Pixel axes= " << pixelAxes.ac() << endl;
      cout << "World axes= " << worldAxes.ac() << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2.ac() << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "Remove pixel axis 1 (DEC)" << endl;
      cSys.removePixelAxis(1, 0.0);
      Vector<Int> pixelAxes, worldAxes;
      Int coordinate;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys);
      cout << "Pixel axes= " << pixelAxes.ac() << endl;
      cout << "World axes= " << worldAxes.ac() << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2.ac() << endl << endl;
   }
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cout << "Remove world axis 1 (DEC)" << endl;
      cSys.removeWorldAxis(1, 0.0);
      Vector<Int> pixelAxes, worldAxes;
      Int coordinate;
      CoordinateUtil::findDirectionAxes(pixelAxes, worldAxes, coordinate, cSys);
      cout << "Pixel axes= " << pixelAxes.ac() << endl;
      cout << "World axes= " << worldAxes.ac() << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2.ac() << endl << endl;
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
      cout << "Pixel axes= " << pixelAxes.ac() << endl;
      cout << "World axes= " << worldAxes.ac() << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2.ac() << endl << endl;
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
      cout << "Pixel axes= " << pixelAxes.ac() << endl;
      cout << "World axes= " << worldAxes.ac() << endl;
      cout << "Coordinate = " << coordinate << endl;
      Vector<Int> pixelAxes2 = 
        CoordinateUtil::findDirectionAxes(cSys);
      cout << "Pixel axes2 = " << pixelAxes2.ac() << endl << endl;
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
   Vector<Int> whichPols;
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


}
   catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return 1;
  }end_try;
 
  return 0;

}
