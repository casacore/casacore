//# dRemoveAxes.cc: demonstrate use of CoordinateUtil::removeAxes function
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000
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
#include <aips/Logging/LogIO.h>
#include <aips/Utilities/String.h>

#include <iostream.h>
#include <iomanip.h>


void listIt (const CoordinateSystem& cSys,
             const Vector<Double>& worldReplacement);


main (int argc, char **argv)
{
try {

   Vector<Double> worldReplacement;
   {
     cout << "remove world axes = [0, 1] and associated pixel axes from [ra, dec, freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();
     Vector<uInt> list(1);
     list(0) = 2;
     Bool remove = False;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    list,  remove)) {
       listIt (cSys, worldReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove world axes = [2] and associated pixel axes from [ra, dec, freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();

     Vector<uInt> list(1);
     list(0) = 2;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    list, remove)) {
        listIt (cSys, worldReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove world axes = [0, 2] and associated pixel axes from [ra,dec,freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();

     Vector<uInt> list(2);
     list(0) = 0;
     list(1) = 2;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    list, remove)) {
        listIt (cSys, worldReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove world axes = [0] and associated pixel axes from [ra, dec, freq]" << endl;
     cout << "and then world axes = [0, 1] and associated pixel axes " << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();

     Vector<uInt> list(1);
     list(0) = 0;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    list, remove)) {
        list.resize(2);
        list(0) = 0;
        list(1) = 1;
        if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                       list, remove)) {
           listIt (cSys, worldReplacement);
        } else {
          cout << "failed" << endl;
        }

      } else {
        cout << "failed" << endl;
      }
   }   
   cout << endl << endl;
   {
     cout << "remove world axes = [0, 2] and associated pixel axes from [ra,dec,freq]" << endl;

     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();

     Vector<uInt> list(2);
     list(0) = 0;
     list(1) = 2;
     Bool remove = True;
     Vector<Double> incr = cSys.increment();
     Vector<Double> refVal = cSys.referenceValue();
     Vector<Double> refPix = cSys.referencePixel();

     worldReplacement.resize(2);
     worldReplacement(0) = (-1 - refPix(list(0)))*incr(list(0)) + refVal(list(0));
     worldReplacement(1) = (10 - refPix(list(1)))*incr(list(1)) + refVal(list(1));

     cout << "specified world replacement values = " << worldReplacement << endl;
     
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    list, remove)) {
       listIt (cSys, worldReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;
   {
     cout << "remove world axes = [0, 2] and associated pixel axes from [ra,dec,freq]" << endl;
     CoordinateSystem cSys = CoordinateUtil::defaultCoords3D();

     Vector<uInt> list(2);
     list(0) = 0;
     list(1) = 2;
     Bool remove = True;
     if (CoordinateUtil::removeAxes(cSys, worldReplacement,
                                    list, remove)) {
       listIt (cSys, worldReplacement);
     } else {
       cout << "failed" << endl;
     }
   }   
   cout << endl << endl;


}
   catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return 1;
  }
 
  return 0;
 
}


void listIt (const CoordinateSystem& cSys,
             const Vector<Double>& worldReplacement)
{
    cout << "nWorldAxes = " << cSys.nWorldAxes() << endl;
    cout << "nPixelAxes = " << cSys.nPixelAxes() << endl;
    cout << "world axis names = " << cSys.worldAxisNames() << endl;
    cout << "reference pixels = " << cSys.referencePixel() << endl;
    cout << "replacement world values = " << worldReplacement << endl;
}

