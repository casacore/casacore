#include <aips/aips.h>
#include <aips/Arrays.h>
#include <trial/Coordinates.h>
#include <aips/Exceptions/Error.h>
#include <aips/Logging/LogIO.h>
#include <aips/Utilities/String.h>

#include <iostream.h>
#include <iomanip.h>


void listIt (const CoordinateSystem& cSys,
             const Vector<Double>& worldReplacement,
             const Vector<Double>& pixelReplacement);


main (int argc, char **argv)
{
try {

   Vector<Double> worldReplacement;
   Vector<Double> pixelReplacement;   
   {
     cout << "remove all axes = [0, 1] from [ra, dec, freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(1);
     list(0) = 2;
     Bool remove = False;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    pixelReplacement, list, 
                                    remove, True)) {
       listIt (cSys, worldReplacement, pixelReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove all axes = [2] from [ra, dec, freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(1);
     list(0) = 2;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    pixelReplacement, list, 
                                    remove, True)) {
        listIt (cSys, worldReplacement, pixelReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove all axes = [0, 2] from [ra,dec,freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(2);
     list(0) = 0;
     list(1) = 2;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    pixelReplacement, list, 
                                    remove, True)) {
        listIt (cSys, worldReplacement, pixelReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove all axes = [0] from [ra, dec, freq]" << endl;
     cout << "and then all axes = [0, 1]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(1);
     list(0) = 0;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    pixelReplacement, list, 
                                    remove, True)) {
        list.resize(2);
        list(0) = 0;
        list(1) = 1;
        if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                       pixelReplacement, list, 
                                       remove, True)) {
           listIt (cSys, worldReplacement, pixelReplacement);
        } else {
          cout << "failed" << endl;
        }

      } else {
        cout << "failed" << endl;
      }
   }   
   cout << endl << endl;
   {
     cout << "remove axes = [0, 2] from [ra,dec,freq]" << endl;
     cout << "specified world replacement values = [1.0,2e9]" << endl;
     cout << "specified pixel replacement values = [10.0,20.0]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(2);
     list(0) = 0;
     list(1) = 2;
     Bool remove = True;
     worldReplacement.resize(2);
     worldReplacement(0) = 1.0;
     worldReplacement(1) = 2e9;
     pixelReplacement.resize(2);
     pixelReplacement(0) = 10.0;
     pixelReplacement(1) = 20.0;
     
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    pixelReplacement, list, 
                                    remove, False)) {
       listIt (cSys, worldReplacement, pixelReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove world axes = [0, 2] from [ra,dec,freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(2);
     list(0) = 0;
     list(1) = 2;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    pixelReplacement, list, 
                                    remove, False)) {
       listIt (cSys, worldReplacement, pixelReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove world axes = [0, 2] from [ra,dec,freq]" << endl;
     cout << "specified replacement values = [1.0,2e9]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(2);
     list(0) = 0;
     list(1) = 2;
     Bool remove = True;
     worldReplacement.resize(2);
     worldReplacement(0) = 1.0;
     worldReplacement(1) = 2e9;
     pixelReplacement.resize(0);
     
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    pixelReplacement, list, 
                                    remove, False)) {
       listIt (cSys, worldReplacement, pixelReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;



}
   catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return 1;
  }end_try;
 
  return 0;
 
}


void listIt (const CoordinateSystem& cSys,
             const Vector<Double>& worldReplacement,
             const Vector<Double>& pixelReplacement)
{
    cout << "nWorldAxes = " << cSys.nWorldAxes() << endl;
    cout << "nPixelAxes = " << cSys.nPixelAxes() << endl;
    cout << "world axis names = " << cSys.worldAxisNames().ac() << endl;
    cout << "reference pixels = " << cSys.referencePixel().ac() << endl;
    cout << "replacement world values = " << worldReplacement.ac() << endl;
    cout << "replacement pixel values = " << pixelReplacement.ac() << endl;
}

