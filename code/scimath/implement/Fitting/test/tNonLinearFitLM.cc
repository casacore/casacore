//# tLQNonLinearFitLM.cc: Test nonlinear least squares classes
//# Copyright (C) 1995,1996,1999,2000,2001,2002
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

#include <trial/Fitting/LQNonLinearFitLM.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffIO.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Random.h>
#include <aips/Functionals/Function.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Functionals/Gaussian2D.h>
#include <aips/Functionals/CompoundFunction.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/OS/Timer.h>
#include <aips/iostream.h>

extern "C" float dtime(float *p);

int main() {
  LQNonLinearFitLM<Double> fitter;
  Vector<Double> solution;
  Double user_time;
  Double oldChiSquare;
  Double newChiSquare;
  const uInt n = 100;
  Vector<Double> x(n); 
  Vector<Double> y(n); 
  Vector<Double> sigma(n);
  Matrix<Double> z(n,2);
  Double value;

  MLCG generator; 
  Normal noise(&generator, 0.0, 1.0);  

  sigma = 1.0;

  fitter.setMaxIter(100);

  // Set converge criteria.  Default is 0.001
  fitter.setCriteria(0.0001);

  // ***** test one: fit 1D gaussian function to data ****** 

  // Make some fake data sets
  //  20.0 * exp (-((x-25)/4)^2) 
  NQGaussian1D<Double> gauss1(20, 25.0, 4.0);  
  for (uInt j=0; j<n; j++) x(j) = j*0.5;
  for (uInt i=0; i<n; i++) {
    value = gauss1(x(i));
    y(i) = abs(value);
  };  
    
  // Construct a gaussian function for fitting
  // It has to be a NQGaussian1D instantiated with an AutoDiff. 

  NQGaussian1D<AutoDiff<Double> > gauss;

  // Must give an initial guess for the set of fitted parameters.

  Vector<Double> v(3);
  v(0) = 2;
  v(1) = 20;
  v(2) = 10;
  for (uInt i=0; i<3; i++) gauss[i] = AutoDiff<Double>(v[i], 3, i);
  // Set the function
  fitter.setFunction(gauss);
  
  Timer timer1;
  timer1.mark();

  // perform fit
  solution = fitter.fit(x, y, sigma);

  user_time = timer1.user ();

  // Compute chi-square for the initial guess
  oldChiSquare = fitter.chiSquare();

  // compute new chi-square for the solution
  newChiSquare = fitter.chiSquare();

  if (fitter.converged()) {
    cout << "****** Test One: fit a 1D gaussian function ******" << endl;
    cout << "User time:   " << user_time << endl;
    cout << "Converged after "<<fitter.currentIteration()<<" iterations"<<endl;
    cout << "Initial guess for fitted parameters " << (Array<Double>)v <<endl;
    cout << "chi-square for initial guess " <<  oldChiSquare << endl;
    cout << "chi-square after convergence " <<  newChiSquare << endl;
    cout << "Converge criteria " << fitter.getCriteria() << endl;
    Matrix<Double> covariance = fitter.compuCovariance();
    
    cout << "Covariance matrix " << covariance;
    // Compare solution with gauss1 parameters
    for (uInt i=0; i<gauss.parameters().nMaskedParameters(); i++) {
      cout << "Expected Parameter Value " <<
	gauss1.parameters().getMaskedParameters()[i]; 
      cout << " Computed Value " << solution(i) << " Std Dev " <<
	sqrt(covariance(i,i)) << endl;
    };
    
    // See if they are within 3*sigma. 
    for (uInt i=0; i<gauss.nparameters(); i++) {
      Int factor=3;
      AlwaysAssertExit(nearAbs(abs(solution(i)), 
      			       gauss1[i],
      			       factor*sqrt(covariance(i,i))));
    };
    cout << "Test one succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test one failed" << endl;
    return 1;
  };

  // ***** test oneA: fit 1D gaussian function using non Autodiff param ****** 
    
  // Construct a gaussian function for fitting
  // It has to be a NQGaussian1D instantiated with an AutoDiff. 

  NQGaussian1D<AutoDiff<Double> > gaussA;
  for (uInt i=0; i<3; i++) gaussA[i] = v[i];
  // Set the function
  fitter.setFunction(gaussA);
  timer1.mark();

  // perform fit
  solution = fitter.fit(x, y, sigma);
  user_time = timer1.user ();

  // compute new chi-square for the solution
  newChiSquare = fitter.chiSquare();

  if (fitter.converged()) {
    cout << "****** Test oneA: fit a 1D gaussian (non-auto param) ******" <<
      endl;
    cout << "User time:   " << user_time << endl;
    cout << "Converged after "<< fitter.currentIteration() <<
      " iterations" <<endl;
    cout << "Initial guess for fitted parameters " << v <<endl;
    cout << "chi-square after convergence " <<  newChiSquare << endl;
    cout << "Converge criteria " << fitter.getCriteria() << endl;
    Matrix<Double> covariance = fitter.compuCovariance();
    
    cout << "Covariance matrix " << covariance;
    // Compare solution with gauss1 parameters
    for (uInt i=0; i<gauss1.parameters().nMaskedParameters(); i++) {
      cout << "Expected Parameter Value " <<
	gauss1.parameters().getMaskedParameters()[i]; 
      cout << " Computed Value " << solution(i) << " Std Dev " <<
	sqrt(covariance(i,i)) << endl;
    };
    
    // See if they are within 3*sigma. 
    for (uInt i=0; i<gaussA.nparameters(); i++) {
      Int factor=3;
      AlwaysAssertExit(nearAbs(abs(solution(i)), 
      			       gauss1[i],
      			       factor*sqrt(covariance(i,i))));
    };
    cout << "Test oneA succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test oneA failed" << endl;
    return 1;
  };

  
  // ***** test oneB: fit 1D gaussian function using compound ****** 
    
  // Construct a gaussian function for fitting
  // It has to be a NQGaussian1D instantiated with an AutoDiff. 

  NQGaussian1D<AutoDiff<Double> > gaussB0;
  for (uInt i=0; i<3; i++) {
    gaussB0[i] = AutoDiff<Double>(v[i], gaussB0.nparameters(), i);
  };
  NQCompoundFunction<AutoDiff<Double> > gaussB;
  gaussB.addFunction(gaussB0);
  // Set the function
  fitter.setFunction(gaussB);
  timer1.mark();

  // perform fit
  solution = fitter.fit(x, y, sigma);
  user_time = timer1.user ();

  // compute new chi-square for the solution
  newChiSquare = fitter.chiSquare();

  if (fitter.converged()) {
    cout << "****** Test oneB: fit a 1D gaussian (use compound)  ******" << 
      endl;
    cout << "User time:   " << user_time << endl;
    cout << "Converged after "<< fitter.currentIteration() <<
      " iterations" <<endl;
    cout << "Initial guess for fitted parameters " << v <<endl;
    cout << "chi-square after convergence " <<  newChiSquare << endl;
    cout << "Converge criteria " << fitter.getCriteria() << endl;
    Matrix<Double> covariance = fitter.compuCovariance();
    
    cout << "Covariance matrix " << covariance;
    // Compare solution with gauss1 parameters
    for (uInt i=0; i<gauss1.parameters().nMaskedParameters(); i++) {
      cout << "Expected Parameter Value " <<
	gauss1.parameters().getMaskedParameters()[i]; 
      cout << " Computed Value " << solution(i) << " Std Dev " <<
	sqrt(covariance(i,i)) << endl;
    };
    
    // See if they are within 3*sigma. 
    for (uInt i=0; i<gaussB.nparameters(); i++) {
      Int factor=3;
      AlwaysAssertExit(nearAbs(abs(solution(i)), 
      			       gauss1[i],
      			       factor*sqrt(covariance(i,i))));
    };
    cout << "Test oneB succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test oneB failed" << endl;
    return 1;
  };
  
  // ***** test two: fit a 1D gaussian function to data but hold the center 
  // ***** of the gaussian fixed
  
  // Give the center a value since its value will be held fixed
  gauss[1] = AutoDiff<Double>(24.5, 3, 1);
  gauss[0] = AutoDiff<Double>(2, 3, 0);
  gauss[2] = AutoDiff<Double>(10, 3, 2);

  // Set the mask of center to false to mask it
  gauss.mask(1) = False;

  // Set the function
  fitter.setFunction(gauss);

  // Must give an initial guess for the set of fitted parameters.  
  v.resize(2);
  v(0) = 2;
  v(1) = 10;

  // perform fit
  solution.resize(0);
  solution = fitter.fit(x, y, sigma);

  // Compute chi-square for the initial guess
  oldChiSquare = fitter.chiSquare();

  // compute new chi-square for the solution
  newChiSquare = fitter.chiSquare();

  if (fitter.converged()) {
    cout << endl;
    cout << "****** Test Two: fit a 1D gaussian function with center fixed ";
    cout << "******" << endl;
    cout << "Converged after "<<fitter.currentIteration()<<" iterations"<<endl;
    cout << "Initial guess for fitted parameters " << (Array<Double>&)v <<endl;
    cout << "chi-square for initial guess " <<  oldChiSquare << endl;
    cout << "chi-square after convergence " <<  newChiSquare << endl;
    cout << "Converge criteria " << fitter.getCriteria() << endl;

    Matrix<Double> covariance = fitter.compuCovariance();
    
    cout << "Covariance matrix " << covariance << endl;
    // Compare solution with gauss1 parameters
    for (uInt i=0; i<gauss.nparameters(); i++) {
      cout << "Expected Parameter Value " << gauss1[i];
      if (!gauss.mask(i)) cout << " Fixed Parameter Value ";
      else cout << " Computed Value ";
      cout << solution[i] << " Std Dev " << sqrt(covariance(i,i)) << endl;
    };
    // See if they are within 3*sigma.
    Int factor=3;
    for (uInt i=0; i<gauss.nparameters(); i++) {
      if (gauss.mask(i)) {
	AlwaysAssertExit(nearAbs(abs(solution(i)), 
				 gauss1[i],
				 factor*sqrt(covariance(i,i))));
      };
    };
    cout << "Test two succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test two failed" << endl;
    return 1;
  };


  // ***** test three: fit a 2D gaussian function to noncircular data

  // generate 2D data. 
  // f(x,y) = h*exp{-[(x-x0)*cos(theta)+(y-y0)*sin(theta)]^2/Wx^2 
  //                -[-(x-x0)*sin(theta)+(y-y0)*cos(theta)]^2/(Wx*r)^2}
  // with h = 1, x0 = y0 = 0, theta = 1, Wx = 2.0, r = 0.5.
  NQGaussian2D<Double> gauss2d1;
  gauss2d1.setMajorAxis(2.0);
  gauss2d1.setAxialRatio(0.5);
  gauss2d1.setPA(1);

  // randomly generate data on a 2D plane. data is perturbed with some noise
  for (uInt i=0; i<n; i++) {
    z(i,0) = noise()/2.0;
    z(i,1) = noise()/2.0;
    // Note, for speed reasons the following does not work: expects contigous
    // vector
    //    value = gauss2d1(z.row(i));
    value = gauss2d1(z(i,0), z(i,1));
    y(i) = value;
  };

  // construct the function to be fitted
  NQGaussian2D<AutoDiff<Double> > gauss2d;
  Vector<AutoDiff<Double> > V2(2);
  V2(0) = AutoDiff<Double>(0.05,6,NQGaussian2D<AutoDiff<Double> >::XCENTER);
  V2(1) = AutoDiff<Double>(0.05,6,NQGaussian2D<AutoDiff<Double> >::YCENTER);
  gauss2d.setHeight(AutoDiff<Double>
		    (1.0,6,NQGaussian2D<AutoDiff<Double> >::HEIGHT));
  gauss2d[NQGaussian2D<AutoDiff<Double> >::YWIDTH] =
    AutoDiff<Double>(2.0,6,NQGaussian2D<AutoDiff<Double> >::YWIDTH);
  gauss2d[NQGaussian2D<AutoDiff<Double> >::RATIO] =
    AutoDiff<Double>(0.5,6,NQGaussian2D<AutoDiff<Double> >::RATIO);
  gauss2d.setPA(
    AutoDiff<Double>(0.05,6,NQGaussian2D<AutoDiff<Double> >::PANGLE));
  gauss2d.setCenter(V2);

  // Note: For circular NQGaussian fitting, the axial ratio should be set to one
  // (default value is one if not set) and the rotation angle should be set
  // to zero (default value is zero if not set), and the two parameters
  // should be masked nonadjustable.  Noncircular NQGaussian fitting, the
  // initial guess for the axial ratio cannot be equal to one.  If noncircular
  // NQGaussian is used to fit precisely circular NQGaussian data.  The rotation 
  // angle becomes meaningless as the fitted NQGaussian function becomes circular
  // and the fitting process may fail.

  // The current parameter values are used as the initial guess. Save them
  // for later checking
  Vector<Double> parameters(gauss2d.nparameters());
  for (uInt i=0; i<gauss2d.nparameters(); i++) {
    parameters[i] = gauss2d[i].value();
  };

  // Set the function
  fitter.setFunction(gauss2d);

  fitter.setCriteria(0.0001);


  solution.resize(0);
  Timer timer;
  timer.mark();

  // perform fit xxx
  solution = fitter.fit(z, y, sigma);

  user_time = timer.user ();

  // compute new chi-square for the solution
  newChiSquare = fitter.chiSquare();

  if (fitter.converged()) {
    cout << endl;
    cout << "*** Test Three: fit a noncircular 2D gaussian function to noncircular data ***" << endl;
    cout << "User time:   " << user_time << endl; 
    cout << "Converged after "<<fitter.currentIteration()<<" iterations"<<endl;
    cout << "Expected values for fitted parameters " <<
      gauss2d1.parameters().getParameters() << endl;
    cout << "Initial guess for fitted parameters " << parameters << endl;
    cout << "chi-square after convergence " <<  newChiSquare << endl;
    cout << "Converge criteria " << fitter.getCriteria() << endl;

    Matrix<Double> covariance = fitter.compuCovariance();
    
    cout << "Covariance matrix " << covariance;

    // Compare solution with gauss2d1 parameters
    for (uInt j=0; j<gauss2d.parameters().nMaskedParameters(); j++) {
      if (j == 5 && solution(j) < 0) solution(j) = 3.1415926 + solution(j);
      cout << "Expected Parameter Value " << gauss2d1.parameters().
	getMaskedParameters()[j];
      cout << " Computed Value " << solution(j) << " Std Dev " 
	<< sqrt(covariance(j,j)) << endl;
    };
       
    // See if they are within 3*sigma.
    Int factor=3;
    for (uInt j=0; j<gauss2d.nparameters(); j++) {
      AlwaysAssertExit(nearAbs(abs(solution(j)), 
			       gauss2d1[j],
      			       factor*sqrt(covariance(j,j))));
    };
    cout << "Test three succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test three failed" << endl;
    return 1;
  };


  // ***** test four: fit a 2D circular gaussian function to circular data

  // generate 2D data. 
  // f(x,y) = h*exp{-[(x-x0)*cos(theta)+(y-y0)*sin(theta)]^2/Wx^2 
  //                -[-(x-x0)*sin(theta)+(y-y0)*cos(theta)]^2/(Wx*r)^2}
  // with h = 1, x0 = y0 = 0, theta = 1, Wx = 2.0, r = 0.5.
  gauss2d1[NQGaussian2D<Double>::YWIDTH] = 2.0;
  gauss2d1[NQGaussian2D<Double>::RATIO] = 1.0;
  gauss2d1.setPA(0);

  // randomly generate data on a 2D plane. data is perturbed with some noise
  for (uInt i=0; i<n; i++) {
    z(i,0) = noise()/2.0;
    z(i,1) = noise()/2.0;
    value = gauss2d1(z(i,0),z(i,1));
    y(i) = value + 0.1*value*noise()/2.5;
  };
  
  // construct the function to be fitted
  NQGaussian2D<AutoDiff<Double> > gauss2d_auto;
  Vector<AutoDiff<Double> > V(2);
  V(0) = AutoDiff<Double>(0.05,6,NQGaussian2D<AutoDiff<Double> >::XCENTER);
  V(1) = AutoDiff<Double>(0.05,6,NQGaussian2D<AutoDiff<Double> >::YCENTER);
  gauss2d_auto.setHeight(AutoDiff<Double>
			 (1.0,6,NQGaussian2D<AutoDiff<Double> >::HEIGHT));
  gauss2d_auto.setCenter(V);
  gauss2d_auto[NQGaussian2D<AutoDiff<Double> >::YWIDTH] = 
    AutoDiff<Double>(2.0,6,NQGaussian2D<AutoDiff<Double> >::YWIDTH);
  gauss2d_auto[NQGaussian2D<AutoDiff<Double> >::RATIO] =
    AutoDiff<Double>(1.0,6,NQGaussian2D<AutoDiff<Double> >::RATIO);
  gauss2d_auto.setPA(AutoDiff<Double>
		     (0.05,6,NQGaussian2D<AutoDiff<Double> >::PANGLE));
  gauss2d_auto.mask(4) = False;
  gauss2d_auto.mask(5) = False;
  
  NQGaussian2D<AutoDiff<Double> > gauss2d2 = gauss2d_auto;

  // Note: For circular NQGaussian fitting, the axial ratio should be set to one
  // (default value is one if not set) and the rotation angle should be set
  // to zero (default value is zero if not set), and the two parameters
  // should be masked nonadjustable.  Noncircular NQGaussian fitting, the
  // initial guess for the axial ratio cannot be equal to one.  If noncircular
  // NQGaussian is used to fit precisely circular NQGaussian data.  The rotation 
  // angle becomes meaningless as the fitted NQGaussian function becomes 
  // circular and the fitting process may fail.

  LQNonLinearFitLM<Double> afitter;

  // The current parameter values are used as the initial guess.  A 
  // slight perturbation is given to them.
  
  for (uInt i=0; i<gauss2d2.nparameters(); i++) {
    parameters[i] = gauss2d2[i].value();
    if (gauss2d2.mask(i)) {
      parameters[i] = parameters[i] + parameters[i]*0.5;
      if (parameters[i] == 0.0) parameters[i] = 0.5;
      gauss2d2[i].value() = parameters[i];
    };
  };
  // Set the function
  afitter.setFunction(gauss2d2);

  afitter.setMaxIter(100);
  afitter.setCriteria(0.00001);

  // perform fit
  solution.resize(0);
  solution = afitter.fit(z, y, sigma);

  // compute new chi-square for the solution
  newChiSquare = afitter.chiSquare();

  if (afitter.converged()) {
    cout << endl;
    cout << "*** Test Four: fit a circular 2D gaussian "
      "function to circular data ***" << endl;
    cout << "Converged after " <<
      afitter.currentIteration() << " iterations" << endl;
    cout << "Expected values for fitted parameters " << 
      gauss2d1.parameters().getParameters() << endl;
    cout << "Initial guess for fitted parameters " <<
      parameters << endl;
    cout << "chi-square after convergence " <<  newChiSquare << endl;
    cout << "Converge criteria " << afitter.getCriteria() << endl;

    Matrix<Double> covariance = afitter.compuCovariance();
    
    cout << "Covariance matrix " << covariance;

    // Compare solution with gauss2d1 parameters
    for (uInt j=0; j<gauss2d2.nparameters(); j++) {
      cout << "Expected Parameter Value " << gauss2d1[j];
      cout << " Computed Value " << solution(j) << " Std Dev " <<
	sqrt(covariance(j,j)) << endl;
    };
        
    // See if they are within 3*sigma.
    Int factor=3;
    for (uInt j=0; j<gauss2d2.nparameters(); j++) {
      if (gauss2d2.mask(j)) {
	AlwaysAssertExit(nearAbs(abs(solution(j)), 
				 gauss2d1[j],
				 factor*sqrt(covariance(j,j))));
      };
    };
    cout << "Test four succeeded" << endl;
  } else {
    cout << "Did not converge after " << afitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test four failed" << endl;    
    return 1;
  };
  
  cout << "OK" << endl;
  return 0;
}
