//# tNonLinearFitLM.cc: Test nonlinear least squares classes
//# Copyright (C) 1995,1996,1999,2000,2001,2002,2004,2005
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

#include <casacore/scimath/Fitting/NonLinearFitLM.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/scimath/Functionals/CompiledFunction.h>
#include <casacore/scimath/Functionals/CompoundFunction.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/scimath/Functionals/Gaussian2D.h>
#include <casacore/scimath/Functionals/HyperPlane.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
extern "C" float dtime(float *p);

int main() {
  NonLinearFitLM<Double> fitter;
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
  
  // ***** test one: fit 1Gaussian function to data ****** 

  // Make some fake data sets
  //  20.0 * exp (-((x-25)/4)^2) 
  Gaussian1D<Double> gauss1(20, 25.0, 4.0);  
  for (uInt j=0; j<n; j++) x(j) = j*0.5;
  for (uInt i=0; i<n; i++) {
    value = gauss1(x(i));
    y(i) = abs(value);
  }  
    
  // Construct a gaussian function for fitting
  // It has to be a Gaussian1D instantiated with an AutoDiff. 

  Gaussian1D<AutoDiff<Double> > gauss;

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
    cout << "****** Test One: fit a 1D Gaussian function ******" << endl;
    cout << "User time:   " << user_time << endl;
    cout << "Converged after "<< fitter.currentIteration() <<
      " iterations" << endl;
    cout << "Initial guess for fitted parameters " << v << endl;
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
    }
    
    // See if they are within 3*sigma. 
    for (uInt i=0; i<gauss.nparameters(); i++) {
      Int factor=3;
      AlwaysAssertExit(nearAbs(abs(solution(i)), 
      			       gauss1[i],
      			       factor*sqrt(covariance(i,i))));
    }
    cout << "Test one succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test one failed" << endl;
    return 1;
  }

  // ***** test oneA: fit 1D Gaussian function using non Autodiff param ****** 
    
  // Construct a gaussian function for fitting
  // It has to be a Gaussian1D instantiated with an AutoDiff. 

  Gaussian1D<AutoDiff<Double> > gaussA;
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
    cout << "****** Test oneA: fit a 1D Gaussian (non-auto param) ******" <<
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
    }
    
    // See if they are within 3*sigma. 
    for (uInt i=0; i<gaussA.nparameters(); i++) {
      Int factor=3;
      AlwaysAssertExit(nearAbs(abs(solution(i)), 
      			       gauss1[i],
      			       factor*sqrt(covariance(i,i))));
    }
    cout << "Test oneA succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test oneA failed" << endl;
    return 1;
  }

  
  // ***** test oneB: fit 1D Gaussian function using compound ****** 
    
  // Construct a gaussian function for fitting
  // It has to be a Gaussian1D instantiated with an AutoDiff. 

  Gaussian1D<AutoDiff<Double> > gaussB0;
  for (uInt i=0; i<3; i++) {
    gaussB0[i] = AutoDiff<Double>(v[i], gaussB0.nparameters(), i);
  }
  CompoundFunction<AutoDiff<Double> > gaussB;
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
    cout << "****** Test oneB: fit a 1D Gaussian (use compound)  ******" << 
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
    }
    
    // See if they are within 3*sigma. 
    for (uInt i=0; i<gaussB.nparameters(); i++) {
      Int factor=3;
      AlwaysAssertExit(nearAbs(abs(solution(i)), 
      			       gauss1[i],
      			       factor*sqrt(covariance(i,i))));
    }
    cout << "Test oneB succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test oneB failed" << endl;
    return 1;
  }
  
  // ***** test two: fit a 1D Gaussian function to data but hold the center 
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
  v[0] = 2;
  v[1] = 10;

  // perform fit
  solution.resize(0);
  solution = fitter.fit(x, y, sigma);

  // Compute chi-square for the initial guess
  oldChiSquare = fitter.chiSquare();

  // compute new chi-square for the solution
  newChiSquare = fitter.chiSquare();

  if (fitter.converged()) {
    cout << endl;
    cout << "****** Test Two: fit a 1D Gaussian function with center fixed ";
    cout << "******" << endl;
    cout << "Converged after " << fitter.currentIteration() <<
      " iterations" << endl;
    cout << "Initial guess for fitted parameters " << v <<endl;
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
    }
    // See if they are within 3*sigma.
    Int factor=3;
    for (uInt i=0; i<gauss.nparameters(); i++) {
      if (gauss.mask(i)) {
	AlwaysAssertExit(nearAbs(abs(solution(i)), 
				 gauss1[i],
				 factor*sqrt(covariance(i,i))));
      }
    }
    cout << "Test two succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test two failed" << endl;
    return 1;
  }


  // ***** test three: fit a 2D gaussian function to noncircular data

  // generate 2D data. 
  // f(x,y) = h*exp{-[(x-x0)*cos(theta)+(y-y0)*sin(theta)]^2/Wx^2 
  //                -[-(x-x0)*sin(theta)+(y-y0)*cos(theta)]^2/(Wx*r)^2}
  // with h = 1, x0 = y0 = 0, theta = 1, Wx = 2.0, r = 0.5.
  Gaussian2D<Double> gauss2d1;
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
  }

  // construct the function to be fitted
  Gaussian2D<AutoDiff<Double> > gauss2d;
  Vector<AutoDiff<Double> > V2(2);
  V2(0) = AutoDiff<Double>(0.05,6,Gaussian2D<AutoDiff<Double> >::XCENTER);
  V2(1) = AutoDiff<Double>(0.05,6,Gaussian2D<AutoDiff<Double> >::YCENTER);
  gauss2d.setHeight(AutoDiff<Double>
		    (1.0,6,Gaussian2D<AutoDiff<Double> >::HEIGHT));
  gauss2d[Gaussian2D<AutoDiff<Double> >::YWIDTH] =
    AutoDiff<Double>(2.0,6,Gaussian2D<AutoDiff<Double> >::YWIDTH);
  gauss2d[Gaussian2D<AutoDiff<Double> >::RATIO] =
    AutoDiff<Double>(0.5,6,Gaussian2D<AutoDiff<Double> >::RATIO);
  gauss2d.setPA(
    AutoDiff<Double>(0.5,6,Gaussian2D<AutoDiff<Double> >::PANGLE));
  gauss2d.setCenter(V2);

  // Note: For circular Gaussian fitting, the axial ratio should be set to one
  // (default value is one if not set) and the rotation angle should be set
  // to zero (default value is zero if not set), and the two parameters
  // should be masked nonadjustable.  Noncircular Gaussian fitting, the
  // initial guess for the axial ratio cannot be equal to one.  If noncircular
  // Gaussian is used to fit precisely circular Gaussian data.  The rotation 
  // angle becomes meaningless as the fitted Gaussian function becomes circular
  // and the fitting process may fail.

  // The current parameter values are used as the initial guess. Save them
  // for later checking
  Vector<Double> parameters(gauss2d.nparameters());
  for (uInt i=0; i<gauss2d.nparameters(); i++) {
    parameters[i] = gauss2d[i].value();
  }

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
    cout << "Converged after " << fitter.currentIteration() <<
      " iterations" << endl;
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
    }
       
    // See if they are within 3*sigma.
    Int factor=3;
    for (uInt j=0; j<gauss2d.nparameters(); j++) {
      AlwaysAssertExit(nearAbs(abs(solution(j)), 
			       gauss2d1[j],
      			       factor*sqrt(covariance(j,j))));
    }
    cout << "Test three succeeded" << endl;
  } else {
    cout << "Did not converge after " << fitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test three failed" << endl;
    return 1;
  }


  // ***** test four: fit a 2D circular gaussian function to circular data

  // generate 2D data. 
  // f(x,y) = h*exp{-[(x-x0)*cos(theta)+(y-y0)*sin(theta)]^2/Wx^2 
  //                -[-(x-x0)*sin(theta)+(y-y0)*cos(theta)]^2/(Wx*r)^2}
  // with h = 1, x0 = y0 = 0, theta = 1, Wx = 2.0, r = 0.5.
  gauss2d1[Gaussian2D<Double>::YWIDTH] = 2.0;
  gauss2d1[Gaussian2D<Double>::RATIO] = 1.0;
  gauss2d1.setPA(0);

  // randomly generate data on a 2D plane. data is perturbed with some noise
  for (uInt i=0; i<n; i++) {
    z(i,0) = noise()/2.0;
    z(i,1) = noise()/2.0;
    value = gauss2d1(z(i,0),z(i,1));
    y(i) = value + 0.1*value*noise()/2.5;
  }
  
  // construct the function to be fitted
  Gaussian2D<AutoDiff<Double> > gauss2d_auto;
  Vector<AutoDiff<Double> > V(2);
  V(0) = AutoDiff<Double>(0.05,6,Gaussian2D<AutoDiff<Double> >::XCENTER);
  V(1) = AutoDiff<Double>(0.05,6,Gaussian2D<AutoDiff<Double> >::YCENTER);
  gauss2d_auto.setHeight(AutoDiff<Double>
			 (1.0,6,Gaussian2D<AutoDiff<Double> >::HEIGHT));
  gauss2d_auto.setCenter(V);
  gauss2d_auto[Gaussian2D<AutoDiff<Double> >::YWIDTH] = 
    AutoDiff<Double>(2.0,6,Gaussian2D<AutoDiff<Double> >::YWIDTH);
  gauss2d_auto[Gaussian2D<AutoDiff<Double> >::RATIO] =
    AutoDiff<Double>(1.0,6,Gaussian2D<AutoDiff<Double> >::RATIO);
  gauss2d_auto.setPA(AutoDiff<Double>
		     (0.05,6,Gaussian2D<AutoDiff<Double> >::PANGLE));
  gauss2d_auto.mask(4) = False;
  gauss2d_auto.mask(5) = False;
  
  Gaussian2D<AutoDiff<Double> > gauss2d2 = gauss2d_auto;

  // Note: For circular Gaussian fitting, the axial ratio should be set to one
  // (default value is one if not set) and the rotation angle should be set
  // to zero (default value is zero if not set), and the two parameters
  // should be masked nonadjustable.  Noncircular Gaussian fitting, the
  // initial guess for the axial ratio cannot be equal to one.  If noncircular
  // Gaussian is used to fit precisely circular Gaussian data.  The rotation 
  // angle becomes meaningless as the fitted Gaussian function becomes 
  // circular and the fitting process may fail.

  NonLinearFitLM<Double> afitter;

  // The current parameter values are used as the initial guess.  A 
  // slight perturbation is given to them.
  
  for (uInt i=0; i<gauss2d2.nparameters(); i++) {
    parameters[i] = gauss2d2[i].value();
    if (gauss2d2.mask(i)) {
      parameters[i] = parameters[i] + parameters[i]*0.5;
      if (parameters[i] == 0.0) parameters[i] = 0.5;
      gauss2d2[i].value() = parameters[i];
    }
  }
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
    }
        
    // See if they are within 3*sigma.
    Int factor=3;
    for (uInt j=0; j<gauss2d2.nparameters(); j++) {
      if (gauss2d2.mask(j)) {
	AlwaysAssertExit(nearAbs(abs(solution(j)), 
				 gauss2d1[j],
				 factor*sqrt(covariance(j,j))));
      }
    }
    cout << "Test four succeeded" << endl;
  } else {
    cout << "Did not converge after " << afitter.currentIteration();
    cout << " interations." << endl;
    cout << "Test four failed" << endl;    
    return 1;
  }

  // *********** Test constraint one *************
  // fit data to measured angles
  {
    // The fitter
    NonLinearFitLM<Double> fitter;
    // Generate fake data (3 angles)
    const uInt n = 100;
    Matrix<Double> arg(3*n,3);
    arg = 0.0;
    Vector<Double> y(3*n);
    Vector<Double> angle(3);
    angle[0] = 50; angle[1] = 60; angle[2] = 70;
    for (uInt i=0; i<3; ++i) {
      for (uInt j=0; j<n; ++j) {
	arg(n*i+j,i) = 1;
	y[n*i+j] = angle[i];
      }
    }
      
    // Add noise
    MLCG generator; 
    Normal noise(&generator, 0.0, 10.0);   
    for (uInt i=0; i<3*n; ++i) y[i] += noise();
    
    // Specify functional
    HyperPlane<AutoDiff<Double> > combination(3);
    fitter.setFunction(combination);
    
    cout << endl << "******** test constraint one *************" << endl;
    // Perform fit
    Vector<Double> solution = fitter.fit(arg, y);
    Matrix<Double> covariance = fitter.compuCovariance();
    Vector<Double> errors = fitter.errors();
    // Get the residuals
    Vector<Double> yres(3*n);
    yres = y;
    AlwaysAssertExit(fitter.residual(yres, arg));
    yres = yres*yres;
    // Print actual parameters and computed parameters
    for (uInt i = 0; i < combination.nparameters(); i++) {
      cout << "Expected: " << angle[i] << 
	" Computed: " << solution[i]  << 
	" Std Dev: " << errors[i] << endl;
    }
    cout <<"Sum solution: " << sum(solution) << endl;
    cout << "Expected ChiSquare: " << sum(yres) << 
      " Computed ChiSquare: " << fitter.chiSquare() << endl;
    cout << "Missing rank: " << fitter.fittedNumber()-fitter.getRank() << 
      ", fitted: " << fitter.fittedNumber() << ", rank: " <<
      fitter.getRank() << endl;

    // Compare actualParameters with the solution vector 
    AlwaysAssertExit(allNearAbs(angle, solution, 0.5));
    AlwaysAssertExit(nearAbs(sum(solution), 180.0, 0.1));
    // Compare ChiSquare
    AlwaysAssertExit(nearAbs(sum(yres), fitter.chiSquare(), 1.0e-5));
    AlwaysAssertExit(fitter.fittedNumber()-fitter.getRank() == 0);

    // Redo it to see if same

    cout << endl << "******** test constraint repeat *************" << endl;
    solution = fitter.fit(arg, y);
    covariance = fitter.compuCovariance();
    errors = fitter.errors();

    // Get the residual
    yres = y;
    AlwaysAssertExit(fitter.residual(yres, arg));
    yres = yres*yres;
    
    // Print actual parameters and computed parameters
    for (uInt i = 0; i < combination.nparameters(); i++) {
      cout << "Expected: " << angle[i] << 
	" Computed: " << solution[i]  << 
	" Std Dev: " << errors[i] << endl;
    }
    cout <<"Sum solution: " << sum(solution) << endl;
    cout << "Expected ChiSquare: " << sum(yres) << 
      " Computed ChiSquare: " << fitter.chiSquare() << endl;
    cout << "Missing rank: " << fitter.fittedNumber()-fitter.getRank() << 
      ", fitted: " << fitter.fittedNumber() << ", rank: " <<
      fitter.getRank() << endl;

    // Compare actualParameters with the solution vector 
    AlwaysAssertExit(allNearAbs(angle, solution, 0.5));
    AlwaysAssertExit(nearAbs(sum(solution), 180.0, 0.1));
    // Compare ChiSquare
    AlwaysAssertExit(nearAbs(sum(yres), fitter.chiSquare(), 1.0e-5));
    AlwaysAssertExit(fitter.fittedNumber()-fitter.getRank() == 0);

    // Add constraint -------------------------

    // Specify functional
    HyperPlane<AutoDiff<Double> > combinationA(3);
    fitter.setFunction(combinationA);
    // Specify constraint
    Vector<Double> constrArg(3, 1.0);
    HyperPlane<AutoDiff<Double> > constrFun(3);
    fitter.addConstraint(constrFun, constrArg, 180.0);
        
    cout << endl << "******** test constraint sum to 180 ********" << endl;
    // Perform least-squares fit
    solution = fitter.fit(arg, y);
    covariance = fitter.compuCovariance();
    errors = fitter.errors();
    // Get the residual
    yres = y;
    AlwaysAssertExit(fitter.residual(yres, arg));
    yres = yres*yres;
    
    // Print actual parameters and computed parameters
    for (uInt i = 0; i < combination.nparameters(); i++) {
      cout << "Expected: " << angle[i] << 
	" Computed: " << solution[i]  << 
	" Std Dev: " << errors[i] << endl;
    }
    cout <<"Sum solution: " << sum(solution) << endl;
    cout << "Expected ChiSquare: " << sum(yres) << 
      " Computed ChiSquare: " << fitter.chiSquare() << endl;
    cout << "Missing rank: " << fitter.fittedNumber()-fitter.getRank() << 
      ", fitted: " << fitter.fittedNumber() << ", rank: " <<
      fitter.getRank() << endl;

    // Compare actualParameters with the solution vector 
    AlwaysAssertExit(allNearAbs(angle, solution, 0.6));
    AlwaysAssertExit(nearAbs(sum(solution), 180.0, 1.0e-20));
    // Compare ChiSquare
    AlwaysAssertExit(nearAbs(sum(yres), fitter.chiSquare(), 1.0e-5));
    AlwaysAssertExit(fitter.fittedNumber()-fitter.getRank() == 0);

    cout << endl << "******** test constraint to 180 (repeat)********" << endl;
    solution = fitter.fit(arg, y);
    covariance = fitter.compuCovariance();
    errors = fitter.errors();
    // Get the residual
    yres = y;
    AlwaysAssertExit(fitter.residual(yres, arg));
    yres = yres*yres;
    
    // Print actual parameters and computed parameters
    for (uInt i = 0; i < combination.nparameters(); i++) {
      cout << "Expected: " << angle[i] << 
	" Computed: " << solution[i]  << 
	" Std Dev: " << errors[i] << endl;
    }
    cout <<"Sum solution: " << sum(solution) << endl;
    cout << "Expected ChiSquare: " << sum(yres) << 
      " Computed ChiSquare: " << fitter.chiSquare() << endl;
    cout << "Missing rank: " << fitter.fittedNumber()-fitter.getRank() << 
      ", fitted: " << fitter.fittedNumber() << ", rank: " <<
      fitter.getRank() << endl;

    // Compare actualParameters with the solution vector 
    AlwaysAssertExit(allNearAbs(angle, solution, 0.6));
    AlwaysAssertExit(nearAbs(sum(solution), 180.0, 1.0e-20));
    // Compare ChiSquare
    AlwaysAssertExit(nearAbs(sum(yres), fitter.chiSquare(), 1.0e-5));
    AlwaysAssertExit(fitter.fittedNumber()-fitter.getRank() == 0);
  }	
  
  {
    // ***** test constraint: fit two 1D Gaussian functions to data ****** 
    cout << endl << "****** Fit a double 1D Gaussian function *****" << endl;
    // Make some fake data sets
    //  20.0 * exp (-((x-25)/4)^2) 
    //  10.0 * exp (-((x-23)/4)^2) 
    // Noise generation
    MLCG generator; 
    Normal noise(&generator, 0.0, 0.3);   
    // Must give an initial guess for the set of fitted parameters.
    Double v[7]  = {20, 10, 4, 10, 33, 4, 10};
    Double vi[7] = {22, 11, 5, 10, 30, 5, 9};
    NonLinearFitLM<Double> fitter;
    fitter.setMaxIter(100);
    CompiledFunction<AutoDiff<Double> > gauss;
    gauss.setFunction("p6+p0*exp(-((x-p1)/p2)^2) + p3*exp(-((x-p4)/p5)^2)");
    for (uInt i=0; i<7; ++i) gauss[i] = v[i];
    for (uInt i=0; i<n; ++i) {
      x[i] = i*0.5;
      y[i] = gauss(x[i]).value() + noise();
      sigma[i] = 1.0;
    } 
    // Set the function and initial guess
    for (uInt i=0; i<7; ++i) gauss[i] = vi[i];
    fitter.setFunction(gauss);
    // Perform fit
    Vector<Double> solution = fitter.fit(x, y, sigma);
    // compute new chi-square for the solution
    newChiSquare = fitter.chiSquare();

    if (fitter.converged()) {
      cout << "Converged after " << fitter.currentIteration()
	   <<"  iterations" <<endl;
      cout << "Initial guess for fitted parameters:" << endl << "[";
      for (uInt i=0; i<gauss.nparameters()-1; ++i) cout << vi[i] << ", "; 
      cout << vi[gauss.nparameters()-1] << "]" << endl;
      cout << "Solution for fitted parameters:" << endl << solution <<endl;
      cout << "chi-square after convergence " <<  newChiSquare << endl;
      cout << "Converge criteria " << fitter.getCriteria() << endl;
      Matrix<Double> covariance = fitter.compuCovariance();
      Vector<Double> errors = fitter.errors();
      // Compare solution with gauss1 parameters
      for (uInt i=0; i<gauss.parameters().nMaskedParameters(); i++) {
	cout << "Expected, Computed Parameter " << v[i];
	cout << ", " << solution[i] << " Std Dev " <<
	  errors[i] << endl;
      }
      
      // See if they are within 3*sigma. 
      for (uInt i=0; i<gauss.nparameters(); i++) {
	AlwaysAssertExit(nearAbs(abs(solution(i)), v[i],
				 3*errors[i]+1e-5));
      }
      cout << "Test one for 2 Gaussians succeeded" << endl;
    } else {
      cout << "Did not converge after " << fitter.currentIteration();
      cout << " interations." << endl;
      cout << "Test one for 2 Gaussians failed" << endl;
      return 1;
    }

    // ***** test constraint: add constraint A0/A1=2 to Gaussians ****** 
    cout << endl << "****** Constrain a double Gaussian's amplitudes *****" <<
      endl;
    // Set the function and initial guess
    NonLinearFitLM<Double> fittera;
    fittera.setMaxIter(100);
    CompiledFunction<AutoDiff<Double> > gaussa;
    gaussa.setFunction("p6+p0*exp(-((x-p1)/p2)^2) + p3*exp(-((x-p4)/p5)^2)");
    for (uInt i=0; i<7; ++i) gaussa[i] = v[i];
    for (uInt i=0; i<n; ++i) {
      x[i] = i*0.5;
      y[i] = gaussa(x[i]).value() + noise();
      sigma[i] = 1.0;
    } 
    // Set the function and initial guess
    for (uInt i=0; i<7; ++i) gaussa[i] = AutoDiff<Double>(vi[i],7,i);
    fittera.setFunction(gaussa);
    // Add constraint
    Vector<Double> constrArg(7, 0.0);
    constrArg[0] = 1.0; constrArg[3] = -2.0;
    fittera.addConstraint(constrArg);
    // Perform fit
    solution = fittera.fit(x, y, sigma);
    // compute new chi-square for the solution
    newChiSquare = fittera.chiSquare();
    
    if (fittera.converged()) {
      cout << "Converged after " << fittera.currentIteration()
	   <<"  iterations" <<endl;
      cout << "Initial guess for fitted parameters:" << endl << "[";
      for (uInt i=0; i<gaussa.nparameters()-1; ++i) cout << vi[i] << ", "; 
      cout << vi[gaussa.nparameters()-1] << "]" << endl;
      cout << "Solution for fitted parameters:" << endl << solution <<endl;
      cout << "chi-square after convergence " <<  newChiSquare << endl;
      cout << "Converge criteria " << fittera.getCriteria() << endl;
      Matrix<Double> covariance = fittera.compuCovariance();
      Vector<Double> errors = fittera.errors();
      // Compare solution with gauss1 parameters
      for (uInt i=0; i<gaussa.parameters().nMaskedParameters(); i++) {
	cout << "Expected, Computed Parameter " << v[i];
	cout << ", " << solution[i] << " Std Dev " <<
	  errors[i] << endl;
      }
      
      // See if they are within 3*sigma. 
      for (uInt i=0; i<gaussa.nparameters(); i++) {
	AlwaysAssertExit(nearAbs(abs(solution(i)), v[i],
				 3*errors[i]+1e-5));
      }
      cout << "Test two for 2 Gaussians succeeded" << endl;
    } else {
      cout << "Did not converge after " << fittera.currentIteration();
      cout << " interations." << endl;
      cout << "Test two for 2 Gaussians failed" << endl;
      return 1;
    }
    
    // ***** test constraint: add constraint W1=W2 to Gaussians ****** 
    
    cout << endl << "****** Constrain a double 1D Gaussian's widths " <<
	 endl;
    {
      NonLinearFitLM<Double> fittera;
      fittera.setFunction(gaussa);
      // Add constraint
      constrArg = 0.0;
      constrArg[2] = 1.0; constrArg[5] = -1.0;
      fittera.addConstraint(constrArg);
      
      // Perform fit
      solution = fittera.fit(x, y, sigma);
      // compute new chi-square for the solution
      newChiSquare = fittera.chiSquare();
      
      if (fittera.converged()) {
	cout << "Converged after " << fittera.currentIteration()
	     <<"  iterations" <<endl;
	cout << "Initial guess for fitted parameters:" << endl << "[";
	for (uInt i=0; i<gaussa.nparameters()-1; ++i) cout << vi[i] << ", "; 
	cout << vi[gaussa.nparameters()-1] << "]" << endl;
	cout << "Solution for fitted parameters:" << endl << solution <<endl;
	cout << "chi-square after convergence " <<  newChiSquare << endl;
	cout << "Converge criteria " << fittera.getCriteria() << endl;
	Matrix<Double> covariance = fittera.compuCovariance();
	Vector<Double> errors = fittera.errors();
	// Compare solution with gauss1 parameters
	for (uInt i=0; i<gaussa.parameters().nMaskedParameters(); i++) {
	  cout << "Expected, Computed Parameter " << v[i];
	  cout << ", " << solution[i] << " Std Dev " <<
	    errors[i] << endl;
	}
	
	// See if they are within 3*sigma. 
	for (uInt i=0; i<gaussa.nparameters(); i++) {
	  AlwaysAssertExit(nearAbs(abs(solution(i)), v[i],
				   3*errors[i]+1e-5));
	}
	cout << "Test three for 2 Gaussians succeeded" << endl;
      } else {
	cout << "Did not converge after " << fittera.currentIteration();
	cout << " interations." << endl;
	cout << "Test thress for 2 Gaussians failed" << endl;
	return 1;
      }
    }
    // ***** test constraint: add also constraint w1=4.0 to Gaussians ****** 
    
    cout << endl << "****** Constrain a double 1D Gaussian's amplitudes, "
	 << "widths  *****" << endl;
    {
      NonLinearFitLM<Double> fittera;
      fittera.setFunction(gaussa);
      // Add constraints
      constrArg = 0.0;
      constrArg[2] = 1.0; constrArg[5] = -1.0;
      fittera.addConstraint(constrArg);
      constrArg = 0.0;
      constrArg[2] = 1.0;
      fittera.addConstraint(constrArg, 4.0);
      constrArg = 0.0;
      constrArg[0] = 1.0; constrArg[3] = -2.0;
      fittera.addConstraint(constrArg);
      // Perform fit
      solution = fittera.fit(x, y, sigma);
      // compute new chi-square for the solution
      newChiSquare = fittera.chiSquare();

      if (fittera.converged()) {
	cout << "Converged after " << fittera.currentIteration()
	     <<"  iterations" <<endl;
	cout << "Initial guess for fitted parameters:" << endl << "[";
	for (uInt i=0; i<gaussa.nparameters()-1; ++i) cout << vi[i] << ", "; 
	cout << vi[gaussa.nparameters()-1] << "]" << endl;
	cout << "Solution for fitted parameters:" << endl << solution <<endl;
	cout << "chi-square after convergence " <<  newChiSquare << endl;
	cout << "Converge criteria " << fittera.getCriteria() << endl;
	Matrix<Double> covariance = fittera.compuCovariance();
	Vector<Double> errors = fittera.errors();
	// Compare solution with gauss1 parameters
	for (uInt i=0; i<gaussa.parameters().nMaskedParameters(); i++) {
	  cout << "Expected, Computed Parameter " << v[i];
	  cout << ", " << solution[i] << " Std Dev " <<
	    errors[i] << endl;
	}
	
	// See if they are within 3*sigma. 
	for (uInt i=0; i<gaussa.nparameters(); i++) {
	  AlwaysAssertExit(nearAbs(abs(solution(i)), v[i],
				   3*errors[i]+1e-5));
	}
	cout << "Test four for 2 Gaussians succeeded" << endl;
      } else {
	cout << "Did not converge after " << fittera.currentIteration();
	cout << " interations." << endl;
	cout << "Test four for 2 Gaussians failed" << endl;
	return 1;
      }
    }
  }
  cout << endl;
  
  cout << "OK" << endl;
  return 0;
}
