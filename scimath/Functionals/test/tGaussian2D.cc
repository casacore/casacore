//# tGaussian2D.cc:  Test the Gaussian2D class
//# Copyright (C) 1996,1998,1999,2000,2001,2002
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

//# Includes
#include <casacore/scimath/Functionals/Gaussian2D.h>

#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  try {
    bool anyFailures = false;
    /*    {
      bool failed = false;
      Gaussian2D<float> g;
      if (g.ndim() != 2) failed = true;
      Vector<float> z(2);
      z = 0;
      float sum = 0; 
      float inc = 0.1;
      for (float x = -3; x < 3.01; x+=inc) {
	z(0) = x;
	for (float y = -3; y < 3.01; y+=inc) {
	  z(1) = y; 
	  if (!near(double(g(z)),
		    exp(-log(16.0)*(z(0)*z(0)+z(1)*z(1))), 1E-4)) {
	    failed = true;
	    cout << "Expected value for g(" << z << ") is " 
		 << exp(-log(16.0)*(z(0)*z(0)+z(1)*z(1)))
		 << " calculated value is " 
		 << g(z) << endl;
	  }
	  sum += g(z);
	}
      }
      if (failed) cout << "Failed";
      else cout << "Passed";
      cout << " the default Gaussian test" << endl;

      if (!failed) {
	if (!near(double(sum*inc*inc), C::pi/log(16.0), 1E-6)) {
	  failed = true;
	  cout << "Failed (value was " << sum*inc*inc << " instead of "
	       << C::pi/log(16.0) << ")" ;
	} else cout << "Passed";
	cout << " the total flux test" << endl;
      }
      if (!failed) {
	if (!near(C::pi/log(16.0), double(g.flux()), 1E-7)) {
	  failed = true;
	} else {
	  g.setFlux(2*g.flux());
	  if (!near(g(0,0), 2.0f)) failed = true;
	}
	if (!failed) cout << "Passed";
	else cout << "Failed";
	cout << " the set/get Flux test" << endl;
      }
      if (failed) anyFailures = true;
    }
    {
      bool failed = false;
      Gaussian2D<double> g;
      g.setHeight(2.0);
      if (near(g.height(), 2.0)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get height test" << endl;

      g.setXcenter(-10.0);
      if (near(g.xCenter(), -10.0)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get X centre test" << endl;

      g.setYcenter(0.1);
      if (near(g.yCenter(), 0.1)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get Y centre test" << endl;

      Vector<double> c(2);
      c(0) = -2.0; c(1) = .5;
      g.setCenter(c);
      if (allNear(g.center(), c, 1E-10)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get centre test" << endl;

      g.setMajorAxis(10.0);
      if (near(g.majorAxis(), 10.0)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get major axis length test" << endl;

      g.setMinorAxis(0.1);
      if (near(g.minorAxis(), 0.1)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get minor axis length test" << endl;

      Vector<double> w(2); w(0) = 2.0; w(1) = 1.0;
      g.setWidth(w);
      if (allNear(g.width(), w, 1E-10)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get width test" << endl;

      g.setAxialRatio(1.0);
      if (near(g.axialRatio(), 1.0)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the set/get axial ratio test" << endl;

      g.setPA(-C::pi_2);
      if (near(g.PA(), C::pi_2)) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }

      cout << " the set/get PA test" << endl;
      if (!failed) {
	///	if (g.nAvailableParams() != 6) failed = true;
	///	Vector<double> parms = g.getAvailableParams();
	Vector<double> parms = g.parameters().getParameters();
	Vector<double> expectedParms(6);
	expectedParms(0) = 2.;
	expectedParms(1) = -2.;
	expectedParms(2) = 0.5;
	expectedParms(3) = 2.0;
	expectedParms(4) = 1.0;
	expectedParms(5) = -C::pi_2;
	if (!allNear(expectedParms, parms, 1E-12)) failed = true;
	parms = -1.0*parms;
	///	g.setAvailableParams(parms);
	g.parameters().setParameters(parms);
	///	parms = g.getAvailableParams();
	parms = g.parameters().getParameters();
	if (!allNear(-1.0*expectedParms, parms, 1E-10)) failed = true;

	// Mask parameters 5 and 6
    */
	/*	g.setAvailableParamMask(4, false);
	g.setAvailableParamMask(5, false);
	for (uint32_t i = 0; i < 4; i++) {
	  if (g.getAvailableParamMask(i) == false) failed = true;
	}
	if (g.getAvailableParamMask(4) == true) failed = true;
	if (g.getAvailableParamMask(5) == true) failed = true;
	*///
    /*
	if (!failed) cout << "Passed";
	else {
	  cout << "Failed";
	  failed=true;
	}
	cout << " the set/get parameters test" << endl;
      }
      if (failed) anyFailures = true;
    }*/
    {
      bool failed = false;
      Vector<double> mean(2), fwhm(2);
      mean(0) = .5; mean(1) = -1;
      
      fwhm(0) = 2; fwhm(1) = .5;
      double pa = C::pi/6,  height = 2;
      Gaussian2D<double> g(height, mean, fwhm, pa), g1;
      Gaussian2D<double> g2(height, mean(0), mean(1), fwhm(0), 
			      fwhm(1)/fwhm(0), pa);
 
      double x = mean(0), y = mean(1);
      if (!near(g(x,y), height)) failed = true;
      g1 = g;
      x -= sin(pa)*fwhm(0)/2;
      y += cos(pa)*fwhm(0)/2;
      if (!near(g1(x,y), height/2.0, 1E-6)) failed = true;
      if (!near(g2(x,y), height/2.0, 1E-6)) failed = true;
 
      Gaussian2D<double> g3(g);
      
      x = mean(0) - cos(pa)*fwhm(1)/2;
      y = mean(1) - sin(pa)*fwhm(1)/2;
      if (!near(g3(x,y), height/2.0, 1E-6)) failed = true;
      if (!failed) cout << "Passed";
      else {
	cout << "Failed";
	failed=true;
      }
      cout << " the arbitrary Gaussian test" << endl;

      if (failed) anyFailures = true;
    
      // Test Auto differentiation - specialized
      double fww(fwhm[1]/fwhm[0]);
      AutoDiff<double> adheight(height,6,0);
      AutoDiff<double> admean0(mean[0],6,1);
      AutoDiff<double> admean1(mean[1],6,2);
      AutoDiff<double> adfwhm0(fwhm[0],6,3); 
      AutoDiff<double> adfww(fww,6,4);
      AutoDiff<double> adpa(pa,6,5);
      Gaussian2D<AutoDiff<double> > g4(adheight,
					 admean0,
					 admean1,
					 adfwhm0, 
					 adfww,
					 adpa);
      cout << "Value: " << g2(x,y) << endl;
      double adx(x);
      double ady(y);
      cout << "Specialized:  " << g4(adx, ady) << endl;
      // Test Auto differentiation
      AutoDiffA<double> adaheight(height,6,0);
      AutoDiffA<double> adamean0(mean[0],6,1);
      AutoDiffA<double> adamean1(mean[1],6,2);
      AutoDiffA<double> adafwhm0(fwhm[0],6,3); 
      AutoDiffA<double> adafww(fww,6,4);
      AutoDiffA<double> adapa(pa,6,5);
      Gaussian2D<AutoDiffA<double> > g5(adaheight,
					  adamean0,
					  adamean1,
					  adafwhm0, 
					  adafww,
					  adapa);
      AutoDiffA<double> adax(x);
      AutoDiffA<double> aday(y);
      cout << "Generic:      " << g5(adax, aday) << endl;
      AlwaysAssertExit(near(g4(adx, ady).value(), g5(adax, aday).value()) &&
		       allNearAbs(g4(adx, ady).derivatives(),
				  g5(adax, aday).derivatives(), 1e-13));
   }
    if (anyFailures) {
      cout << "FAIL" << endl;
      return 1;
    } else {
      cout << "OK" << endl;
      return 0;
    }

  } catch (std::exception& x) {
    cerr << x.what() << endl;
    cout << "Failed" << endl;
    return 1;
  } 
}
