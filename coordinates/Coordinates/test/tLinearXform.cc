//# tLinearXform.cc: Test program for LinearXform class
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
//# $Id$
//#
 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/scimath/Mathematics/MatrixMathLA.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/coordinates/Coordinates/LinearXform.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>


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
         if (!allEQ(crpix, lxf.crpix())) {
            throw(AipsError("Failed crpix recovery test"));
         }
//
         if (!allEQ(cdelt, lxf.cdelt())) {
            throw(AipsError("Failed cdelt recovery test"));
         }
//
         if (!allEQ(px, lxf.pc())) {
            throw(AipsError("Failed pc recovery test"));
         }
//       
         crpix = crpix * 2.0;
         lxf.crpix(crpix);
         if (!allEQ(crpix, lxf.crpix())) {
            throw(AipsError("Failed crpix set/recovery test"));
         }
//
         cdelt = cdelt * 2.0;
         lxf.cdelt(cdelt);
         if (!allEQ(cdelt, lxf.cdelt())) {
            throw(AipsError("Failed cdelt set/recovery test"));
         }
//
         px.diagonal() = 2.0;
         lxf.pc(px);   
         if (!allEQ(px, lxf.pc())) {
            throw(AipsError("Failed pc set/recovery test"));
         }
      }
//
// Test Fourier inverter.  Can only do this by knowing the algorithm...
//
     {
         Vector<Double> crpix(2), cdelt(2);
         crpix(0) = 10.0; crpix(1) = 20.0; 
         cdelt(0) = 1.0; cdelt(1) = 1.5; 
         Matrix<Double> pc(2,2);
         Vector<Double> diag(2);
         diag(0) = 1.0; diag(1) = 2.0;
         pc = 0.0; 
         pc.diagonal() = diag;
         LinearXform lxf(crpix, cdelt, pc);

// All axes

         Vector<Bool> axes(2, True);
         Vector<Double> crpix2(2);
         Vector<Double> scale(2);
         crpix2(0) = 1.0; crpix2(1) = 2.0;
         scale(0) = 5.0; scale(1) = 2.0;
         String errMsg;
//
         {
            LinearXform* lxf2 = lxf.fourierInvert(errMsg, axes, crpix2, scale);
            if (!casacore::allNear(crpix2, lxf2->crpix(),1e-13)) {
               throw(AipsError("fourierInvert (1) crpix test failed"));         
            }
            Vector<Double> tmp(2);
            tmp(0) = 1.0 / cdelt(0) * scale(0);
            tmp(1) = 1.0 / cdelt(1) * scale(1);
            if (!casacore::allNear(tmp, lxf2->cdelt(),1e-13)) {
               throw(AipsError("fourierInvert (1) cdelt test failed"));         
            }
            if (!near(1.0/diag(0),lxf2->pc()(0,0)) || !near(1.0/diag(1),lxf2->pc()(1,1)) ||
                !near(0.0,lxf2->pc()(0,1)) || !near(0.0,lxf2->pc()(1,0))) {
               throw(AipsError("fourierInvert (1) pc test failed"));         
            }
            delete lxf2;
         }

// Not all axes

         {
            axes(1) = False;
            LinearXform* lxf2 = lxf.fourierInvert(errMsg, axes, crpix2, scale);
            if (!near(crpix2(0), lxf2->crpix()(0),1e-13) ||
                !near(lxf.crpix()(1), lxf2->crpix()(1),1e-13)) {
               throw(AipsError("fourierInvert (2) crpix test failed"));         
            }
            Vector<Double> tmp(2);
            tmp(0) = 1.0 / cdelt(0) * scale(0);
            if (!near(tmp(0), lxf2->cdelt()(0),1e-13) ||
                !near(cdelt(1), lxf2->cdelt()(1),1e-13)) {
               throw(AipsError("fourierInvert (2) cdelt test failed"));         
            }
            if (!near(1.0/diag(0),lxf2->pc()(0,0)) || !near(diag(1),lxf2->pc()(1,1)) ||
                !near(0.0,lxf2->pc()(0,1)) || !near(0.0,lxf2->pc()(1,0))) {
               throw(AipsError("fourierInvert (2) pc test failed"));         
            }
            delete lxf2;
         }

// Non-diagonal pc matrix all axes

         {
            axes.set(True);
            pc(1,0) = 2.0;
            pc(0,1) = 2.0;
            lxf.pc(pc);
            LinearXform* lxf2 = lxf.fourierInvert(errMsg, axes, crpix2, scale);
            if (!casacore::allNear(crpix2, lxf2->crpix(),1e-13)) {
               throw(AipsError("fourierInvert (3) crpix test failed"));         
            }
            Vector<Double> tmp(2);
            tmp(0) = 1.0 / cdelt(0) * scale(0);
            tmp(1) = 1.0 / cdelt(1) * scale(1);
            if (!casacore::allNear(tmp, lxf2->cdelt(),1e-13)) {
               throw(AipsError("fourierInvert (3) cdelt test failed"));         
            }
            if (!casacore::allNear(invert(pc), lxf2->pc(), 1e-13)) {
               throw(AipsError("fourierInvert (3) pc test failed"));         
            }
            delete lxf2;
         }


// Non-diagonal pc matrix not axes

         {
            axes.set(True);
            axes(1) = False;
            pc(1,0) = 2.0;
            pc(0,1) = 2.0;
            lxf.pc(pc);
            LinearXform* lxf2 = lxf.fourierInvert(errMsg, axes, crpix2, scale);
            if (lxf2) {
               delete lxf2;
               throw(AipsError("Failed to induce forced fourierInvert error"));
            }
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
         Bool ok = lxf.forward(pixel, world, error);
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
         ok = lxf.forward(pixel, world, error);
         if (!ok) {
            throw(AipsError(String("Forwards conversion failed because ") +
                            error));
         }
         Vector<Double> world2;
         ok = lxf.reverse(world2, pixel, error);
         if (!casacore::allNear(world, world2, 1e-6)) {
            throw(AipsError("Conversion reflection failed"));
         }
      }
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}
