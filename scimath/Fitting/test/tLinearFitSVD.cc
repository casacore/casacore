//# tLinearFitSVD.cc: Test linear least squares classes
//# Copyright (C) 1995,1996,1999,2000,2001,2002,2004
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

#include <casacore/scimath/Fitting/LinearFitSVD.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/scimath/Functionals/CombiFunction.h>
#include <casacore/scimath/Functionals/FunctionWrapper.h>
#include <casacore/scimath/Functionals/HyperPlane.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Primes.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
// Some C++ functions
// To use them in Fitting, have to have parameters and also AutoDiff
static Double func0(const Vector<Double> &) {return 1;}            // 1
static Double func1(const Vector<Double> &x) {return x(0);}         // x
static Double func2(const Vector<Double> &x) {return sin(x(1));}    // sin(y)
static Double func3(const Vector<Double> &x) {return x(0)*x(0);}    // x^2

void checkLinearFit(LinearFitSVD<Double> &fitter) {
  //*********** Test one *************
  // fit data to polynomial
  {
    // Generate fake data
    const uInt nPrimes = 20;
    Vector<Double> primesTable(nPrimes);
    Vector<Double> x(nPrimes);
    Vector<Double> sigma(nPrimes);
    indgen((Array<Double>&)x, 1.0);  // 1, 2, ...
    primesTable(0) = 2;
    for (uInt i=1; i < nPrimes; i++) {
      primesTable(i) = Primes::nextLargerPrimeThan(Int(primesTable(i-1)+0.01));
    }   
    sigma = 1.0;
    Vector<Double> actualParameters(3);
    actualParameters(0) = -1.92368;
    actualParameters(1) = 2.2055;
    actualParameters(2) = 0.0746753;
    Matrix<Double> actualCovariance(3, 3);
    actualCovariance(0,0) = 0.553509;
    actualCovariance(0,1) = -0.107895;
    actualCovariance(0,2) = 0.00438596;
    actualCovariance(1,0) = -0.107895;
    actualCovariance(1,1) = 0.0266234;
    actualCovariance(1,2) = -0.00119617;
    actualCovariance(2,0) = 0.00438596;
    actualCovariance(2,1) = -0.00119617;
    actualCovariance(2,2) = 0.0000569606;
    Double actualChiSquare = 22.9901;
    
    
    // construct a linear combination of functions: a(0)+a(1)*x+a(2)*x^2    
    
    Polynomial<AutoDiff<Double> > combination(2);
    combination.setCoefficient(0, 1.0);
    combination.setCoefficient(1, 1.0);
    combination.setCoefficient(2, 1.0);
    // perform least-squares fit
    fitter.setFunction(combination);
    Vector<Double> solution = fitter.fit(x,primesTable,sigma);
    Matrix<Double> covariance = fitter.compuCovariance();
    
    // Get the residual
    Vector<Double> yres(nPrimes);
    yres = primesTable;
    AlwaysAssertExit(fitter.residual(yres, x));
    yres = yres*yres;
    
    cout << "******** test one *************" << endl;
    // Print actual parameters and computed parameters
    for (uInt i = 0; i < combination.nparameters(); i++) {
      cout << "Actual Parameter " << actualParameters(i) << 
	", Computed Parameter " << solution(i) << endl;
    }
    // Print actual covariance and computed covariance
    for (uInt i = 0; i < combination.nparameters(); i++) {
      for (uInt j = 0; j < combination.nparameters(); j++) {
	cout << "Actual Covariance " << actualCovariance(i,j) << 
	  ", Computed Covariance " << covariance(i,j) << endl;
      }
    }
    cout << "actual ChiSquare " << actualChiSquare << 
      " Computed ChiSquare " <<
      fitter.chiSquare() << endl;
    cout << "fromResidual ChiSquare: " << sum(yres) << endl;
    cout << "Missing rank: " << fitter.fittedNumber()-fitter.getRank() << endl;
    
    // Compare actualParameters with the solution vector 
    AlwaysAssertExit(allNear(actualParameters, 
			     solution, 1.0e-5));
    // Compare actualCovariance with the covariance matrix 
    AlwaysAssertExit(allNear(actualCovariance, 
			     covariance, 1.0e-5));
    // Compare actualChiSquare with the chiSquare value
    AlwaysAssertExit(near(actualChiSquare,
			  fitter.chiSquare(),
			  1.0e-5));
    AlwaysAssertExit(near(actualChiSquare, sum(yres),
			  1.0e-5));
    AlwaysAssertExit(fitter.fittedNumber()-fitter.getRank() == 0);
  }

  //****** Test two ************

  // fitting polynomial to data with some coefficient being held fixed
  {
    Int j;
    // Make some fake data sets
    // -1 + 6*x + 10*x^2 + 3*x^3
    Polynomial<Double> poly(3);
    poly.setCoefficient(0, -1.0); 
    poly.setCoefficient(1, 6.0);
    poly.setCoefficient(2, 10.0);
    poly.setCoefficient(3, 3.0);
    const uInt n = 1000;
    Vector<Double> x(n); 
    Vector<Double> y(n);
    Vector<Double> sigma(n); 
    indgen((Array<Double>&)x); 
    x /= Double(Double(n)/10); // 0.00 - 9.99
    MLCG generator; 
    Normal noise(&generator, 0.0, 1.0);   
    for (uInt i=0; i < n; i++) {
      // -1 + 6*x + 10*x^2 + 3*x^3 + unit gaussian noise
      y(i) = poly(x(i)) + noise();
    }
    // Uniform variances
    sigma = 1.0;


    // construct a linear combination of functions: 
    // a(0)+a(1)*x+a(2)*x^2+a(3)*x^3
    Polynomial<AutoDiff<Double> > combination(3);
    for (uInt i=0; i<4; i++) combination[i] = 1.0;

    // Hold the coefficient for square fixed
    combination.mask(2) = False;
    // set the parameter value
    combination[2] = 10;

    // Indicate which function to fit
    fitter.setFunction(combination);
    Vector<Double> solution = fitter.fit(x, y, sigma);
    Matrix<Double> covariance = fitter.compuCovariance();

    cout << endl << "******** test two *************" << endl;
    cout << "Expect -1 + 6*x + 10*x^2 + 3*x^3 " << endl;
    for (uInt i = 0; i < solution.nelements(); i++) {
      if (i == 2) cout << "Fixed coefficient ";
      else cout << "Computed ";
      cout << solution(i) << " Std Dev " << sqrt(covariance(i,i)) << endl;
    }
    cout << "Solved for " << fitter.fittedNumber() << " parameters" << endl;
    AlwaysAssertExit(fitter.fittedNumber() ==
		     combination.parameters().nMaskedParameters());
    AlwaysAssertExit(fitter.fittedNumber()-fitter.getRank() == 0);
    cout << "Missing rank: " << fitter.fittedNumber()-fitter.getRank() << endl;
    
    // compare solution with poly parameters. See if they are within 3*sigma.
    Int factor = 3;
    j = 0;
    for (uInt i = 0; i < solution.nelements(); i++) {
      if (i == 2) {
	j++;
	continue;
      }
      AlwaysAssertExit(nearAbs(solution(i), poly[j],
			       factor*sqrt(covariance(i,i))));
      j++;
    }
  }
  //************ test three ****************

  {
    // fitting a 2D polynomial to data points:
    // f(x,y) = a0 + a1*x+ a2*y + a3*x*x
    {    
      // Convert C++ functions to Functionals
      FunctionWrapper<Double> Func0(func0,2);
      FunctionWrapper<Double> Func1(func1,2);
      FunctionWrapper<Double> Func2(func2,2);
      FunctionWrapper<Double> Func3(func3,2);
      
      CombiFunction<Double> combination;
      
      // form linear combination of functions
      // f(x,y) = a0 + a1*x+ a2*sin(y) + a3*x*x
      combination.addFunction(Func0);
      combination.addFunction(Func1);
      combination.addFunction(Func2);
      combination.addFunction(Func3);
      
      // Now use this combination to generate some fake data
      combination[0] = 4;
      combination[1] = 5;
      combination[2] = 6;
      combination[3] = 0.2;
      
      Int npoints = 100;
      Matrix<Double> x(npoints,2);       // coordinates
      Vector<Double> z(npoints);         // data values
      Vector<Double> sigma(npoints);     // standard deviation   
      MLCG generator; 
      Normal noise(&generator, 0.0, 1.0);   
      for (Int i = 0; i < npoints; i++) {
	x(i,0) = 0.2*i;
	x(i,1) = x(i,0)*2;
	Double nois = noise()/4.0;
	z(i) = combination(x.row(i)) + nois;
      }
      sigma = 1.0;
      cout << endl << "******** test three *************" << endl;
      Vector<Double> z0(2);
      z0[0] = 2; z0[1] = 3;
      cout << "x,y: " << z0[0] << ", " << z0[1] << endl;
      cout << "Expect: " << 4 + 5*z0[0]+ 6*sin(z0[1]) + 0.2*z0[0]*z0[0] <<
	endl;
      cout << "Got:    " << combination(z0) << endl;
      // For fitting the functions have to have AutoDiff and parameters
      // A combi did create problems when cleaning at exit the
      // static PoolStack data: crashed in memory
      /*
      fitter.setFunction(combination);
      Vector<Double> solution = fitter.fit(x,z,sigma);
      Matrix<Double> covariance = fitter.compuCovariance();    
      
      cout << "Expect f(x,y) = 4 + 5*x+ 6*sin(y) + 0.2*x*x" << endl;
      cout << "Computed " << (Array<Double>&)solution << endl;
      cout << "Std Dev  ";
      for (uInt i = 0; i < solution.nelements(); i++) {
	cout << sqrt(covariance(i,i)) << " ";
      }
      cout << endl;
      
      // See if they are within 3*sigma.
      Int factor = 3;
      for (uInt i = 0; i < solution.nelements(); i++) {
	AlwaysAssertExit(nearAbs(solution(i), combination[i],
				 factor*sqrt(covariance(i,i))));
      }
      AlwaysAssertExit(fitter.fittedNumber()-fitter.getRank() == 0);
      cout << "Missing rank: " << fitter.fittedNumber()-fitter.getRank() <<
	endl;
   */
    }
     
  }
   
}

//****** Test on complex fitting ************

void checkComplexLinearFit(LinearFitSVD<Complex> &fitter) {

  // fitting polynomial to data
  const uInt n = 1000;
  
  // Make some fake data sets
  // (-1.0,2.0) + (6.0,4.0)*x + (10.0,-1.5)*x^2 + (3.0,2.3)*x^3
  Polynomial<Complex> poly(3);
  poly.setCoefficient(0, Complex(-1.0,2.0)); 
  poly.setCoefficient(1, Complex(6.0,4.0));
  poly.setCoefficient(2, Complex(10.0,-1.5));
  poly.setCoefficient(3, Complex(3.0,2.3));

  Vector<Complex> x(n); 
  Vector<Complex> y(n);
  Vector<Complex> sigma(n); 

  MLCG generator; 
  Normal noise(& generator, 0.0, 1.0);  

  // randomly generate data on a complex plane. 
  for (uInt i = 0; i < n; i++) {
    x(i) = Complex(noise(), noise());
    y(i) = poly(x(i))+Complex(noise())/Complex(2.0);
  }

  sigma = Complex(1.0,1.0);
   
  // construct a linear combination of functions: 
  // a(0)+a(1)*x+a(2)*x^2+a(3)*x^3
  Polynomial<AutoDiff<Complex> > combination(3);
  combination.setCoefficient(0, AutoDiff<Complex>(1.0,4,0));   // 1
  combination.setCoefficient(1, AutoDiff<Complex>(1.0,4,1));     // x
  combination.setCoefficient(2, AutoDiff<Complex>(1.0,4,2));     // x^2
  combination.setCoefficient(3, AutoDiff<Complex>(1.0,4,3));     // x^3

  // Indicate which function to fit
  fitter.setFunction(combination);
  Vector<Complex> solution = fitter.fit(x, y, sigma);

  Matrix<Double> covariance = fitter.compuCovariance();
    
  cout << endl << "******** test four complex fitting*************" << endl;
  cout << "fitted function ";
  cout << "(-1.0,2.0) + (6.0,4.0)*x + (10.0,-1.5)*x^2 + (3.0,2.3)*x^3" << endl;
  for (uInt i = 0; i < solution.nelements(); i++) {
    cout << "Expected: (" <<
      poly[i].real() << "," << poly[i].imag() << ") ";
    cout << "Computed: (" <<
      solution(i).real() << "," << solution(i).imag() << ") ";
    cout << "Std Dev: " << sqrt(covariance(i,i)) << endl;
  }
  cout << "Missing rank: " << 2*fitter.fittedNumber()-fitter.getRank() << endl;

  // compare solution with poly parameters. See if they are within 3*sigma.
  Double factor = 3;
  for (uInt i = 0; i < solution.nelements(); i++) {
    AlwaysAssertExit(nearAbs(abs(solution(i)), 
			     abs(poly[i]),
			     factor*abs(sqrt(covariance(i,i)))));
  }
  AlwaysAssertExit(2*fitter.fittedNumber()-fitter.getRank() == 0);
}

void checkConstraintLinearFit(LinearFitSVD<Double> &fitter) {
  //*********** Test constraint one *************
  // fit data to measured angles
  {
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
  cout << endl;
}

  int main() {
  LinearFitSVD<Double> fitsvd;
  checkLinearFit(fitsvd);

  LinearFitSVD<Complex> fit_complex;
  checkComplexLinearFit(fit_complex);
  
  LinearFitSVD<Double> fitcon;
  checkConstraintLinearFit(fitcon);

  cout << "OK" << endl;
  return 0;
}











