//# dConstraints.cc.cc: Demo nonlinear least squares classes with constraints
//# Copyright (C) 2004
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
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Functionals/CompiledFunction.h>
#include <casacore/scimath/Functionals/CompoundFunction.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Functionals/Gaussian1D.h>
#include <casacore/scimath/Functionals/HyperPlane.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

int main (int argc, const char* argv[])
{

  uChar tp = '0';			// # of constraints
  uChar ft = '0';			// Function type (compiled or compound)
  if (argc>1) tp = argv[1][0];
  if (argc>2) ft = argv[2][0];
  NonLinearFitLM<Double> fitter;
  Vector<Double> solution;
  Double newChiSquare;
  const uInt n = 100;
  Vector<Double> x(n); 
  for (uInt i=0; i<n; ++i) x[i] = i*0.5;
  Vector<Double> y(n); 
  Vector<Double> sigma(n, 1.0);
  Matrix<Double> z(n,2);
  Vector<Double> constrArg(7, 0.0);
  
  MLCG generator; 
  Normal noise(&generator, 0.0, 0.3);  
  
  sigma = 1.0;
  
  fitter.setMaxIter(100);
  
  // Set converge criteria.  Default is 0.001
  ///fitter.setCriteria(0.0001);
  
  // Function
  Function<AutoDiff<Double>, AutoDiff<Double> > *gauss;

  cout << endl << "****** Fit a double 1D gaussian function *****" << endl;
  cout << "Run program as " << endl <<
    "dConstraints x y" << endl <<
    "with x = # of constraints ([0],1,2,3)," << endl <<
    "     y = compiled/compound function ([0]/1)" << endl;
  // Make some fake data sets
  //  20.0 * exp (-((x-10)/4)^2) + 10.0 * exp (-((x-33)/4)^2) + 10
  Double v[7]  = {20, 10, 4, 10, 33, 4, 10};
  // Must give an initial guess for the set of fitted parameters.
  Double vi[7] = {22, 11, 5, 10, 30, 5, 9};

  // Select compiled or Gaussian1Ds
  switch (ft) {
  case '1': {
    v[2]  = v[5]  = 4.0*sqrt(log(16.0));
    vi[2] = vi[5] = 5.0*sqrt(log(16.0));
    gauss = new CompoundFunction<AutoDiff<Double> >;
    Gaussian1D<AutoDiff<Double> > g1;
    Gaussian1D<AutoDiff<Double> > g2;
    Polynomial<AutoDiff<Double> > p1(0);
    dynamic_cast<CompoundFunction<AutoDiff<Double> > *>(gauss)
      ->addFunction(g1);
    dynamic_cast<CompoundFunction<AutoDiff<Double> > *>(gauss)
      ->addFunction(g2);
    dynamic_cast<CompoundFunction<AutoDiff<Double> > *>(gauss)
      ->addFunction(p1);
    ///    for (uInt i=0; i<7; ++i) (*gauss)[i] = AutoDiff<Double>(v[i],7,i);
    cout << "Using a Compound of Gaussians and Polynomial" << endl;
  }
  break;
  default: {
    gauss = new CompiledFunction<AutoDiff<Double> >;
    dynamic_cast<CompiledFunction<AutoDiff<Double> > *>(gauss)
      ->setFunction("p6+p0*exp(-((x-p1)/p2)^2) + p3*exp(-((x-p4)/p5)^2)");
    ///for (uInt i=0; i<7; ++i) (*gauss)[i] = v[i];
    cout << "Using a Compiled string function" << endl;
  }
  break;
  }

  for (uInt i=0; i<7; ++i) (*gauss)[i] = AutoDiff<Double>(v[i],7,i);
  for (uInt i=0; i<n; ++i) y[i] = (*gauss)(x[i]).value() + noise();
  // Set the function and initial guess
  for (uInt i=0; i<7; ++i) (*gauss)[i] = vi[i];
  fitter.setFunction(*gauss);
  
  try {
    // Set constraints
    switch (tp) {
    case '3':					// W1=4
      constrArg = 0.0;
      constrArg[2] = 1.0;
      fitter.addConstraint(constrArg, v[2]);
    case '2':					// W1==W2
      constrArg = 0.0;
      constrArg[2] = 1.0; constrArg[5] = -1.0;
      fitter.addConstraint(constrArg);
    case '1':					// A1/A2=2
      constrArg = 0.0;
      constrArg[0] = 1.0; constrArg[3] = -2.0;
      fitter.addConstraint(constrArg);
    default:
      break;
    }
    
    // Perform fit
    Vector<Double> solution = fitter.fit(x, y, sigma);
    // compute new chi-square for the solution
    newChiSquare = fitter.chiSquare();
    
    // Show data
    cout << "# solutions, parameters, constraints: " << 
      solution.nelements() << ", " << gauss->nparameters() << ", " <<
      fitter.nConstraints() << endl;
    cout << "Converged after " << fitter.currentIteration()
	 <<"  iterations" <<endl;
    cout << "Initial guess for fitted parameters:" << endl << "[";
    for (uInt i=0; i<gauss->nparameters()-1; ++i) cout << vi[i] << ", "; 
    cout << vi[gauss->nparameters()-1] << "]" << endl;
    cout << "Solution for fitted parameters:" << endl << solution <<endl;
    cout << "chi-square after convergence " <<  newChiSquare << endl;
    cout << "Converge criteria " << fitter.getCriteria() << endl;
    Matrix<Double> covariance = fitter.compuCovariance();
    Vector<Double> errors = fitter.errors();
    // Compare solution with gauss1 parameters
    for (uInt i=0; i<gauss->parameters().nMaskedParameters(); i++) {
      cout << "Expected, Computed Parameter " << v[i];
      cout << ", " << solution[i] << " Std Dev " <<
	errors[i] << endl;
    }
    
    delete gauss;

    cout << "---------------------------------------------------" << endl;
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  }
  
  return 0;
}
