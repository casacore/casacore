//# tProfileFit1D.cc: test the ProfileFit1D class
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2004
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

#include <casa/aips.h>
#include <components/SpectralComponents/ProfileFit1D.h>

#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Vector.h>
#include <casa/Utilities/Assert.h>
#include <components/SpectralComponents/SpectralElement.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
void makeData (Vector<Double>& x, Vector<Double>& y, Vector<Bool>& m, 
               Double& amp, Double& cen, Double& sigma, 
               Double& p0, Double& p1);
void check (Double amp, Double cen, Double sigma, Double p0, Double p1,
            const SpectralList& l);
void checkMasks (uInt n, const ProfileFit1D<Double>& fitter, Int start,
		 Int end);

int main() {

try {

  {

// Data

      Vector<Double> x,y;
      Vector<Bool> m;
      Double amp, cen, sig, p0, p1;
      makeData(x, y, m, amp, cen, sig, p0, p1);
      const uInt n = x.nelements();
 
// Make fitter, set data and fit

      ProfileFit1D<Double> fitter;
      fitter.setData (x,y,m);
      fitter.setGaussianElements (1);
      SpectralElement p(1);
      fitter.addElement(p);
      AlwaysAssert(fitter.fit(), AipsError);

// Check ok

      AlwaysAssert(fitter.getDataMask().nelements()==n, AipsError);
      AlwaysAssert(allEQ(fitter.getDataMask(), True), AipsError);
      AlwaysAssert(fitter.getRangeMask().nelements()==0, AipsError);
      AlwaysAssert(fitter.getTotalMask().nelements()==n, AipsError);
      AlwaysAssert(allEQ(fitter.getTotalMask(), True), AipsError);
//
      {
         const SpectralList& fitList = fitter.getList(True);
         check (amp, cen, sig, p0, p1, fitList);
      }
//
      {
         ProfileFit1D<Double> fitter2(fitter);
         const SpectralList& fitList = fitter2.getList(True);
         check (amp, cen, sig, p0, p1, fitList);
      }
      {
         ProfileFit1D<Double> fitter2; 
         fitter2 = fitter;
         const SpectralList& fitList = fitter2.getList(True);
         check (amp, cen, sig, p0, p1, fitList);
      }

// Set a range mask via indices

      {
         Vector<uInt> start(1), end(1);
         start(0) = n/2; end(0) = start(0) + n/10;
         fitter.setRangeMask (start, end, True);

// Check masks

         checkMasks (n, fitter, start(0), end(0));

// Now set range mask via abcissa values

         Vector<Double> startF(1), endF(1);
         startF(0) = x(start(0)); endF(0) = x(end(0));
         fitter.setRangeMask (startF, endF, True);

// Check masks

         checkMasks (n, fitter, start(0), end(0));
       }
    }
//
   cout << "OK" << endl;
   return 0;
 } catch (AipsError err) {
    cerr << err.getMesg() << endl;
 }
}

void makeData (Vector<Double>& x, Vector<Double>& y, Vector<Bool>& m,
               Double& amp, Double& cen, Double& sigma, 
               Double& p0, Double& p1)
{
   Int n = 256;
   x.resize(n);
   y.resize(n);
   m.resize(n);
   indgen(x);
   x *= (2.3);
   x += (1.0);
   m = True;
//
   amp = 10.0;
   cen = x(n/2);
   sigma = (x[n-1] - x[0]) / 50.0;
   p0 = 0.15;
   p1 = 1.2;
   SpectralElement g(SpectralElement::GAUSSIAN, amp, cen, sigma);
   cerr << "Gaussian: " << amp << ", " << cen << ", " << sigma << endl;
   cerr << "Polynomial: " << p0 << ", " << p1 << endl;
//
   Vector<Double> pars(2);
   pars(0) = p0;
   pars(1) = p1;
   SpectralElement p(SpectralElement::POLYNOMIAL, pars);
   for (uInt i=0; i<x.nelements(); i++) {
     y(i) = g(x[i]) + p(x[i]);
   }
}

void check (Double amp, Double cen, Double sig, Double p0, Double p1,
            const SpectralList& list)
{
      Double tol(1e-4);
      Vector<Double> p;
      SpectralElement elG = list[0];
      SpectralElement elP = list[1];
//
      elG.get(p);
      AlwaysAssert(near(amp, p[0], tol), AipsError);
      AlwaysAssert(near(cen, p[1], tol), AipsError);
      AlwaysAssert(near(sig, p[2], tol), AipsError);
//
      elP.get(p);
      AlwaysAssert(near(p0, p[0], tol), AipsError);
      AlwaysAssert(near(p1, p[1], tol), AipsError);
}


void checkMasks (uInt n, const ProfileFit1D<Double>& fitter, Int start,
		 Int end)
{
   Vector<Bool> rangeMask = fitter.getRangeMask();
   Vector<Bool> totalMask = fitter.getTotalMask();
//
   AlwaysAssert(rangeMask.nelements()==n, AipsError);
   AlwaysAssert(totalMask.nelements()==n, AipsError);
   AlwaysAssert(allEQ(rangeMask, totalMask), AipsError);
//
   IPosition iStart(1), iEnd(1);
  {
     iStart(0) = 0;
     iEnd(0) = start-1;
     Vector<Bool> tmp = rangeMask(iStart, iEnd);
     AlwaysAssert(allEQ(tmp, False), AipsError);
  }
  {
     iStart(0) = start;
     iEnd(0) = end;
     Vector<Bool> tmp = rangeMask(iStart, iEnd);
     AlwaysAssert(allEQ(tmp, True), AipsError);
  }
  {
     iStart(0) = end+1;
     iEnd(0) = n-1;
     Vector<Bool> tmp = rangeMask(iStart, iEnd);
     AlwaysAssert(allEQ(tmp, False), AipsError);
  }
}
