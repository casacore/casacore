//# tLinearXform.cc: Test program for LinearXform class
//# Copyright (C) 1998
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
 
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Mathematics/Math.h>
#include <trial/Coordinates/LinearXform.h>
#include <aips/Exceptions/Error.h>

#include <iostream.h>


int main()
{
   try {

// Constructors

      {
         LinearXform xf(1);
      }
      {
         Vector<Double> crpix(3), cdelt(3);
         crpix(0) = 10.0; crpix(1) = 20.0; crpix(2) = 30.0;
         cdelt(0) = 1.0; cdelt(1) = 1.5; cdelt(2) = 2.0;
//
         LinearXform lxf(crpix, cdelt);
//
         Matrix<Double> px(3,3);
         px = 0.0; px.diagonal() = 1.0;
         LinearXform lxf2(crpix, cdelt, px);
     }

// Test near function

     {
         Vector<Double> crpix(3), cdelt(3);
         crpix(0) = 10.0; crpix(1) = 20.0; crpix(2) = 30.0;
         cdelt(0) = 1.0; cdelt(1) = 1.5; cdelt(2) = 2.0;
         LinearXform lxf(crpix, cdelt);
         LinearXform lxf2(crpix, cdelt);
         if (!lxf.near(lxf2)) {
            throw(AipsError("Failed near test 1"));
         }
         Vector<Int> excludeAxes(1,1);
         if (!lxf.near(lxf2, excludeAxes)) {
            throw(AipsError("Failed near test 2"));
         }
     } 

// Test copy constructor

     {
         Vector<Double> crpix(3), cdelt(3);
         crpix(0) = 10.0; crpix(1) = 20.0; crpix(2) = 30.0;
         cdelt(0) = 1.0; cdelt(1) = 1.5; cdelt(2) = 2.0;
         LinearXform lxf(crpix, cdelt);
         LinearXform lxf2(lxf);
         if (!lxf.near(lxf2)) {
            throw(AipsError("Failed copy constructor test"));
         }
     } 

// Test assignment

     {
         Vector<Double> crpix(3), cdelt(3);
         crpix(0) = 10.0; crpix(1) = 20.0; crpix(2) = 30.0;
         cdelt(0) = 1.0; cdelt(1) = 1.5; cdelt(2) = 2.0;
         LinearXform lxf(crpix, cdelt);
         LinearXform lxf2; 
         lxf2 = lxf;
         if (!lxf.near(lxf2)) {
            throw(AipsError("Failed assignment test"));
         }
     } 

// Test member functions
   
     {
         Vector<Double> crpix(3), cdelt(3);
         crpix(0) = 10.0; crpix(1) = 20.0; crpix(2) = 30.0;
         cdelt(0) = 1.0; cdelt(1) = 1.5; cdelt(2) = 2.0;
         Matrix<Double> px(3,3);
         px = 0.0; px.diagonal() = 1.0;
         LinearXform lxf(crpix, cdelt, px);
//
         if (lxf.nWorldAxes() != 3) {
            throw(AipsError("Failed worldAxes test"));
         }
//
         if (!allEQ(crpix.ac(), lxf.crpix().ac())) {
            throw(AipsError("Failed crpix recovery test"));
         }
//
         if (!allEQ(cdelt.ac(), lxf.cdelt().ac())) {
            throw(AipsError("Failed cdelt recovery test"));
         }
//
         if (!allEQ(px.ac(), lxf.pc().ac())) {
            throw(AipsError("Failed pc recovery test"));
         }
//       
         crpix = crpix.ac() * 2.0;
         lxf.crpix(crpix);
         if (!allEQ(crpix.ac(), lxf.crpix().ac())) {
            throw(AipsError("Failed crpix set/recovery test"));
         }
//
         cdelt = cdelt.ac() * 2.0;
         lxf.cdelt(cdelt);
         if (!allEQ(cdelt.ac(), lxf.cdelt().ac())) {
            throw(AipsError("Failed cdelt set/recovery test"));
         }
//
         px.diagonal() = 2.0;
         lxf.pc(px);   
         if (!allEQ(px.ac(), lxf.pc().ac())) {
            throw(AipsError("Failed pc set/recovery test"));
         }
      }

// Test conversion

     {
         Vector<Double> crpix(3), cdelt(3);
         crpix(0) = 10.0; crpix(1) = 20.0; crpix(2) = 30.0;
         cdelt(0) = 1.0; cdelt(1) = 1.5; cdelt(2) = 2.0;
         Matrix<Double> px(3,3);
         px = 0.0; px.diagonal() = 1.0;
         LinearXform lxf(crpix, cdelt, px);
//
         Vector<Double> world(crpix.copy());
         Vector<Double> pixel(3);
         String error;
         Bool ok = lxf.forward(world, pixel, error);
         if (!ok) {
            throw(AipsError(String("Forwards conversion failed because ") +
                            error));
         }
         for (uInt i=0; i<crpix.nelements(); i++) {
            if (!near(pixel(i), (world(i)/cdelt(i))+crpix(i), 1e-6)) {
               throw(AipsError("Forwards conversion gave wrong answer"));
            }
         }
//
         world(0) = 1.123; world(1) = 232.121; world(2) = -10.2;
         ok = lxf.forward(world, pixel, error);
         if (!ok) {
            throw(AipsError(String("Forwards conversion failed because ") +
                            error));
         }
         Vector<Double> world2;
         ok = lxf.reverse(world2, pixel, error);
         if (!allNear(world.ac(), world2.ac(), 1e-6)) {
            throw(AipsError("Conversion reflection failed"));
         }
      }
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}
