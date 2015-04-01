//# tGaussianND.cc:  
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
//#
//# $Id$

//# Includes
#include <casacore/scimath/Functionals/GaussianND.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/scimath/Mathematics/MatrixMathLA.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main(){
  try {
    Bool anyFailures = False;
    {
      Bool failed = False;
      GaussianND<Float> default2D;
      Vector<Float> z(2);
      z = 0;
      Float sum = 0; 
      Float inc = .5;
      for (Float x = -5; x < 5; x+=inc) {
	z(0) = x;
	for (Float y = -5; y < 5; y+=inc) {
	  z(1) = y; 
	  if (!near(Double(default2D(z)),
		    exp(-(z(0)*z(0)+z(1)*z(1))/2)/(2.*C::pi),1E-5)) {
	    failed = True;
	    cout << "Expected value for f(" << z << ") is " 
		 << exp(-(z(0)*z(0)+z(1)*z(1))/2)/(C::_2pi) 
		 << " calculated value is " 
		 << default2D(z) << endl;
	  }
	  sum += default2D(z);
	}
      }
      if (failed) cout << "Failed";
      else cout << "Passed";
      cout << " the default Gaussian test" << endl;
    
      if (!failed) {
	if (!near(sum*inc*inc, Float(1.0))) {
	  failed = True;
	  cout << "Failed (value was " << sum*inc*inc << ")";
	} else cout << "Passed";
	cout << " the total flux test" << endl;
      }
      if (failed) anyFailures = True;
    }
    { 
      Bool failed = False;
      Vector<Float> w(1);
      GaussianND<Float> g(1), g2;
      GaussianND<Float> g1(g); 
      g1.setHeight(2.0f);
      w(0) = 2.0f;
      g1.setVariance(w);
      g2 = g; g2.setHeight(3.0f);
      w(0) = 3.0f;
      g2.setVariance(w);
      Vector<Float> z(1);
      z = 0;
      for (Float x = -2; x < 2; x+=.1) {
	z(0) = x;
	if (!near(Double(g(z)),
		  exp(-(z(0)*z(0))/2)/sqrt(C::_2pi),1E-5)) {
	  failed = True;
	  cout << "Expected value for g(" << z << ") is " 
	       << exp(-(z(0)*z(0)/2))/sqrt(C::_2pi) 
	       << " calculated value is " 
	       << g(z) << endl;
	}
	if (!near(Double(g1(z)),
		  2.0*exp(-(z(0)*z(0))/2/2),1E-5)) {
	  failed = True;
	  cout << "Expected value for g1(" << z << ") is " 
	       << 2.0*exp(-(z(0)*z(0)/2/2))
	       << " calculated value is " 
	       << g1(z) << endl;
	}
	if (!near(Double(g2(z)),
		  3.0*exp(-(z(0)*z(0))/2/3),1E-5)) {
	  failed = True;
	  cout << "Expected value for g2(" << z << ") is " 
	       << 3.0*exp(-(z(0)*z(0)/2/3))
	       << " calculated value is " 
	       << g2(z) << endl;
	}
      }
      if (!failed) cout << "Passed";
      else cout << "Failed";
      cout << " the default 1-D and copy semantics test" << endl;
      if (failed) anyFailures = True;
    }
    {
      Bool failed = False;
      GaussianND<Float> gauss3D(3, 2.0);
      gauss3D.setHeight(1.0);
      if (near(gauss3D.height(), 1.0f)) cout << "Passed";
      else {
	cout << "Failed";
	failed=True;
      }
      cout << " the set/get height test" << endl;

      Vector<Float> z(3);
      z = 0;
      Float sum = 0; 
      Float inc = .5;
      for (Float x = -5; x < 5; x+=inc) {
	z(0) = x;
	for (Float y = -5; y < 5; y+=inc) {
	  z(1) = y; 
	  for (Float p = -5; p < 5; p+=inc) {
	    z(2) = p; 
	    if (!near(Double(gauss3D(z)),
		      exp(-(z(0)*z(0)+z(1)*z(1)+z(2)*z(2))/2),
		      1E-5)) {
	      failed = True;
	      cout << "Expected value for f(" << z << ") is " 
		   << exp(-(z(0)*z(0)+z(1)*z(1)+z(2)*z(2))/2)
  		   << " calculated value is " 
  		   << gauss3D(z) << endl;
	    }
	    sum += gauss3D(z);
	  }
	}
      }
      if (failed) cout << "Failed";
      else cout << "Passed";
      cout << " the 3-D specified height test" << endl;

      if (!failed) {
	if (!near(sum*inc*inc*inc, pow(Float(C::_2pi),Float(1.5)), 1E-5)) {
	  failed = True;
	  cout << "Failed (value was " << sum*inc*inc*inc 
	       << " not " << pow(Float(C::_2pi),Float(1.5))
	       << ")";
	} else cout << "Passed";
	cout << " the total flux test" << endl;
      }
      if (failed) anyFailures = True;
    }
    {
      Bool failed = False;
      Vector<Double> mean(2);
      mean(0) = .5;
      mean(1) = -1;
      GaussianND<Double> gauss2D(2, 1.0, mean);
      mean *= 2.;
      gauss2D.setMean(mean);
      if ((allNear(gauss2D.mean(), mean, 1E-7))) cout << "Passed";
      else {
	cout << "Failed";
	failed=True;
      }
      cout << " the set/get mean test" << endl;

      Vector<Double> z(2); z = 0;
      Double sum = 0; 
      Double inc = .25;
      for (Double x = -4; x < 6; x+=inc) {
	z(0) = x;
	for (Double y = -7; y < 3; y+=inc) {
	  z(1) = y; 
	  if (!near(gauss2D(z),
		    exp(-(square(z(0)-mean(0))+
			  square(z(1)-mean(1)))/2),
		    1E-5)) {
	    failed = True;
	    cout << "Expected value for f(" << z << ") is " 
		 << exp(-(square(z(0)-mean(0))+square(z(1)-mean(1)))/2)
  		 << " calculated value is "
  		 << gauss2D(z) << endl;
	  }
	  sum += gauss2D(z);
	}
      }
      if (failed) cout << "Failed";
      else cout << "Passed";
      cout << " the 2-D specified height & mean test" << endl;
    
      if (!failed) {
	if (!near(sum*inc*inc, C::_2pi, 1E-5)) {
	  failed = True;
	  cout << "Failed (value was " << sum*inc*inc
	       << " not " << C::_2pi
	       << ")";
	} else cout << "Passed";
	cout << " the total flux test" << endl;
      }
      if (failed) anyFailures = True;
    }
    {
      Bool failed = False;
      Vector<Double> mean(3);
      mean(0) = .6; mean(1) = -.1; mean(2) = -.5;
      Vector<Double> variance(3);
      variance(0) = 1; variance(1) = .25; variance(2) = 0.75;
      Double height = 2.0;
      GaussianND<Double> gauss3D(3, height, mean, variance);

      variance *= 2.;
      gauss3D.setVariance(variance);
      if ((allNear(gauss3D.variance(), variance, 1E-7))) cout << "Passed";
      else {
	cout << "Failed";
	failed=True;
      }
      cout << " the set/get variance test" << endl;

      Vector<Double> z(3);
      z = 0;
      Double sum = 0; 
      Double inc = .5;
      for (Double x = -5; x < 7; x+=inc) {
	z(0) = x;
	for (Double y = -5; y < 5; y+=inc) {
	  z(1) = y; 
	  for (Double p = -5; p < 5; p+=inc) {
	    z(2) = p; 
	    if (!near(gauss3D(z),
		      height*
		      exp(-(square(z(0)-mean(0))/variance(0)
			    +square(z(1)-mean(1))/variance(1)
			    +square(z(2)-mean(2))/variance(2))/2))) {
	      failed = True;
	      cout << "Expected value for f(" << z << ") is " 
		   << height*
		exp(-(square(z(0)-mean(0))/variance(0)
		      +square(z(1)-mean(1))/variance(1)
		      +square(z(2)-mean(2))/variance(2))/2)
		   << " calculated value is "
		   << gauss3D(z) << endl;
	    }
	    sum += gauss3D(z);
	  }
	}
      }
      if (failed) cout << "Failed";
      else cout << "Passed";
      cout << " the 3-D specified height, mean & variance test" << endl;
    
      if (!failed) {
	if (!near(sum*inc*inc*inc, 
		  height*sqrt(variance(0)*variance(1)*variance(2))
		  *pow(C::_2pi,1.5)
		  , 1E-4)) {
	  failed = True;
	  cout << "Failed (value was " << sum*inc*inc*inc 
	       << " not " 
	       << height*sqrt(variance(0)*variance(1)*variance(2))
	    *pow(C::_2pi,1.5)
	       << ")";
	} else cout << "Passed";
	cout << " the total flux test" << endl;
      }
      if (failed) anyFailures = True;
    }
    {
      Bool failed = False;
      Vector<Float> mean(3);
      mean(0) = .6; mean(1) = -.1; mean(2) = -.5;
      mean(0) = 0; mean(1) = 0; mean(2) = 0;
      Matrix<Float> corr(3,3), covariance(3,3);
      Vector<Float> variance = covariance.diagonal();
      covariance = Float(0);
      corr(1,0) = .5; corr(2,0) = .4; corr(2,1) = -.3;
      variance(0) = 4.5; variance(1) = .125; variance(2) = 0.50;
      covariance(1,0) = corr(1,0)*sqrt(variance(1)*variance(0));
      covariance(2,0) = corr(2,0)*sqrt(variance(2)*variance(0));
      covariance(2,1) = corr(2,1)*sqrt(variance(2)*variance(1));
      Float height = 2.0;
      GaussianND<Float> gauss3D(3, height, mean, covariance);

      covariance *= Float(2.);
      covariance(0,1) = covariance(1,0);
      covariance(0,2) = covariance(2,0);
      covariance(1,2) = covariance(2,1);
      gauss3D.setCovariance(covariance);
      if ((allNear(gauss3D.covariance(), covariance, 1E-5))) cout << "Passed";
      else {
	cout << "Failed";
	failed=True;
      }
      cout << " the set/get covariance test" << endl;
      if (!failed) {
	Vector<Float> z(3); z = 0;
	Float sum = 0; 
	Float inc = 0.5;
	Float ev;
	z = 0;
	for (Float x = -12; x < 12; x+=inc) {
	  z(0) = x;
	  for (Float y = -2; y < 2; y+=inc) {
	    z(1) = y; 
	    for (Float p = -5; p < 5; p+=inc) {
	      z(2) = p; 
	      ev = height*
		exp(-0.5*(square(z(0)-mean(0))*
			  (1-square(corr(2,1)))/variance(0)
			  +square(z(1)-mean(1))*
			  (1-square(corr(2,0)))/variance(1)
			  +square(z(2)-mean(2))*
			  (1-square(corr(1,0)))/variance(2)
			  -2*(z(0)-mean(0))*(z(1)-mean(1))*
			  (corr(1,0)-corr(2,1)*corr(2,0))/
			  sqrt(variance(0)*variance(1))
			  -2*(z(0)-mean(0))*(z(2)-mean(2))*
			  (corr(2,0)-corr(1,0)*corr(2,1))/
			  sqrt(variance(0)*variance(2))
			  -2*(z(1)-mean(1))*(z(2)-mean(2))*
			  (corr(2,1)-corr(1,0)*corr(2,0))/
			  sqrt(variance(1)*variance(2)))
		    /(1-square(corr(1,0))-square(corr(2,0))-square(corr(2,1))
		      +2*corr(1,0)*corr(2,0)*corr(2,1)));
	      if (!nearAbs(gauss3D(z), ev, 3.1E-4)) {
		failed = True;
		cout << "Expected value for f(" << z << ") is " << ev
		     << " calculated value is " << gauss3D(z) << endl;
	      }
	      sum += gauss3D(z);
	    }
	  }
	}
	if (failed) cout << "Failed";
	else cout << "Passed";
	cout << " the 3-D specified height, mean & covariance test" << endl;
    
	if (!failed) {
	  if (!nearAbs(sum*inc*inc*inc, gauss3D.flux(), 1E-2)) {
	    failed = True;
	    cout << "Failed (value was " << sum*inc*inc*inc 
		 << " not " 
		 << gauss3D.flux()
		 << ")";
	  } else cout << "Passed";
	  cout << " the total flux test" << endl;
	}
      }

      if (!failed) {
	if (gauss3D.nparameters() != 10) failed=True;
	if (!failed) {
	  Vector<Float> parms(10);
	  parms(0) = 10;
	  parms(1) = 1;
	  parms(2) = 2;
	  parms(3) = 3;
	  parms(4) = 10;
	  parms(5) = 20;
	  parms(6) = 30;
	  parms(7) = 0.1;
	  parms(8) = 0.2;
	  parms(9) = 0.3;

	  Matrix<Float> cov(3,3); 
	  cov(0,0) = parms(4); cov(1,1) = parms(5); cov(2,2) = parms(6);
	  cov(0,1) = parms(7); cov(1,0) = cov(0,1);
	  cov(0,2) = parms(8); cov(2,0) = cov(0,2);
	  cov(1,2) = parms(9); cov(2,1) = cov(1,2);
	  Matrix<Float> invertCov(3,3);
	  invertCov = invertSymPosDef(cov);
	  gauss3D.parameters().setParameters(parms);
	  if (!near(gauss3D.height(), parms(0))) failed = True;
	  Vector<Float> mean(3); 
	  mean(0) = parms(1);
	  mean(1) = parms(2);
	  mean(2) = parms(3);
	  if (!(allNear(gauss3D.mean(), mean, 1E-6))) failed=True;
	  Vector<Float> var(3); 
	  var(0) = invertCov(0,0);
	  var(1) = invertCov(1,1);
	  var(2) = invertCov(2,2);
	  if (!(allNear(gauss3D.variance(), var, 1E-6))) failed=True;

	  if (!(allNear(gauss3D.covariance(), invertCov, 1E-6))) failed=True;

	  for (uInt i = 0; i < 10; i++)
	    gauss3D[i] =  Float(2)*gauss3D[i];
	  parms *= Float(2);
	  if (!(allNear(gauss3D.parameters().getParameters(), parms, 1E-6))) {
	    failed=True;
	  }
	  if (failed) cout << "Failed";
	  else cout << "Passed";
	  cout << " the Parameters tests" << endl;
	}
      }
    
      if (failed) anyFailures = True;
    }
    if (anyFailures) {
      cout << "FAIL" << endl;
      return 1;
    }
    cout << "OK" << endl;
    return 0;
  }

  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
}
