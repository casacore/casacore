//# tProjection.cc: Test program for Projection class
//# Copyright (C) 1998,1999
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
 
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <trial/Coordinates/Projection.h>
#include <aips/Exceptions/Error.h>

#include <iostream.h>

void testSin();

int main()
{
   try {

// Test parameter setting and recovery

      for (Int i=0; i<Projection::N_PROJ; i++) {
         uInt nP = Projection::nParameters(Projection::Type(i));
         Vector<Double> pars(nP);
         for (uInt j=0; j<nP; j++) {
           pars(j) = Double(j);
         }
         Projection::Type type = (Projection::Type)i;
         Projection proj(type, pars);
//
         if (proj.type() != type) {
           throw(AipsError("Type recovery inconsistent"));
         }
         if (!allEQ(proj.parameters().ac(),pars.ac())) {
           throw(AipsError("Parameters recovery inconsistent"));
         }
         if (proj.name() != Projection::name(type)) {
           throw(AipsError("Name recovery inconsistent"));
         }
         if (proj.type(proj.name()) != type) {
           throw(AipsError("Type recovery inconsistent"));
         }
      }
      
// Test near function

      Vector<Double> pars(2);
      pars(0) = 0.1; pars(1) = 0.2;
      {
         Projection proj(Projection::SIN, pars);
         Projection proj2(Projection::SIN, pars);
         if (!proj.near(proj2,1e-6)) {
           throw(AipsError("Near function fails"));
         }
      }

// Test assignment (and zero par constructor)

      {
         Projection proj(Projection::SIN, pars);
         Projection proj2(Projection::TAN);
         proj2 = proj;
//
         if (proj2.name() != proj.name() ||
             !allEQ(proj2.parameters().ac(),proj.parameters().ac()) ||
             proj2.type() != proj.type() ||
             !proj2.near(proj, 1e-6)) {
           throw(AipsError("Assignment fails"));
         }
//
         pars.resize(0);
         Projection proj3(Projection::SIN, pars);
      }
      testSin();
//
// Test copy constructor
//
      {
         Projection proj(Projection::SIN, pars);
         Projection proj2(proj);
         if (proj2.name() != proj.name() ||
             !allEQ(proj2.parameters().ac(),proj.parameters().ac()) ||
             proj2.type() != proj2.type() ||
             !proj2.near(proj, 1e-6)) {
           throw(AipsError("Copy constructor fails"));
         }
      }
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}

void testSin()
//
// Special case for fail. In its own function as
// catch does not catch twice
//
{
   Vector<Double> pars;
   pars.resize(1); pars(0) = 1.0;
   try {
      Projection proj4(Projection::SIN, pars);
   } catch (AipsError x) {
      return;
   }end_try;
   throw(AipsError("Expected error did not occur"));
}

