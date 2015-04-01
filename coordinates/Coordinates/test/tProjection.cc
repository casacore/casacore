//# tProjection.cc: Test program for Projection class
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
 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/coordinates/Coordinates/Projection.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>

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
         if (!allEQ(proj.parameters(),pars)) {
           throw(AipsError("Parameters recovery inconsistent"));
         }
         if (proj.name() != Projection::name(type)) {
           throw(AipsError("Name recovery inconsistent"));
         }
         if (proj.type(proj.name()) != type) {
           throw(AipsError("Type recovery inconsistent"));
         }
//
         Bool isZen = Projection::isZenithal (type);
         Bool ok = False;
         if (isZen) {
            if (type==Projection::AZP || type==Projection::TAN || type==Projection::SIN ||      
                type==Projection::STG || type==Projection::ARC || type==Projection::ZPN ||      
                type==Projection::ZEA || type==Projection::AIR || type==Projection::SZP) ok = True;
         } else {
            if (type!=Projection::AZP && type!=Projection::TAN && type!=Projection::SIN &&
                type!=Projection::STG && type!=Projection::ARC && type!=Projection::ZPN &&      
                type!=Projection::ZEA && type!=Projection::AIR && type!=Projection::SZP) ok = True;
         }
         if (!ok) {
           throw(AipsError("isZenithal fails"));
         }

// Test second FITS constructor

         {
            String p(proj.name());
            String ctypeLong("RA---");
            String ctypeLat("DEC--");
//
            Projection proj2(ctypeLong.append(p), 
                             ctypeLat.append(p), pars);
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
             !allEQ(proj2.parameters(),proj.parameters()) ||
             proj2.type() != proj.type() ||
             !proj2.near(proj, 1e-6)) {
           throw(AipsError("Assignment fails"));
         }
//
         pars.resize(0);
         Projection proj3(Projection::SIN, pars);
      }

//
// Test copy constructor
//
      {
         Projection proj(Projection::SIN, pars);
         Projection proj2(proj);
         if (proj2.name() != proj.name() ||
             !allEQ(proj2.parameters(),proj.parameters()) ||
             proj2.type() != proj2.type() ||
             !proj2.near(proj, 1e-6)) {
           throw(AipsError("Copy constructor fails"));
         }
      }

   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}


