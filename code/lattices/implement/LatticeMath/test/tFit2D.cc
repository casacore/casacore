//# tFit2D.cc: Test nonlinear least squares classes
//# Copyright (C) 1995,1996,1998
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

#include <trial/Fitting.h>
#include <trial/Fitting/Fit2D.h>
#include <aips/Functionals/Gaussian2D.h>
#include <aips/Inputs/Input.h>
#include <aips/Logging.h>
#include <aips/Mathematics/Math.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Mathematics/Random.h>
#include <aips/Utilities/Assert.h>

int main(int argc, char **argv)
{

  try {
 
   Input inputs(1);
   inputs.Version ("$Revision$");
   inputs.Create("noise", "0.0", "Noise");
   inputs.Create("norm", "False", "Normalize");
   inputs.Create("mask", "1,1,1,1,1,1", "Mask");
   inputs.Create("nx", "10", "nx");
   inputs.Create("ny", "10", "ny");   
   inputs.Create("include", "0.0", "include");
   inputs.Create("exclude", "0.0", "exclude");
   inputs.ReadArguments(argc, argv);
   const Bool norm = inputs.GetBool("norm");
   const Double noise = inputs.GetDouble("noise");
   const Block<Int> mask = inputs.GetIntArray("mask");
   const Block<Double> includeRange = inputs.GetDoubleArray("include");
   const Block<Double> excludeRange = inputs.GetDoubleArray("exclude");
   const Int nx = inputs.GetInt("nx");   
   const Int ny = inputs.GetInt("ny");   

   IPosition shape(2,nx,ny);
   LogOrigin or("tFit2D", "main()", WHERE);
   LogIO logger(or);

//

   MLCG generator; 
   Normal noiseGen(0.0, noise, &generator);  

   Gaussian2D<Double> gauss2d;
   gauss2d.setHeight(3.0);
   gauss2d.setMajorAxis(2.0);
   gauss2d.setAxialRatio(0.5);
   gauss2d.setPA(-0.1);
   gauss2d.setXcenter(Double(shape(0)/2));
   gauss2d.setYcenter(Double(shape(1)/2));

//
   Array<Float> pixels(shape);
   Array<Float> sigma;
   Array<Float> resid;
   Array<Bool> pixelMask;
   IPosition off(2);
//
   IPosition loc(2);
   for (Int j=0; j<shape(1); j++) {
      for (Int i=0; i<shape(0); i++) {
         loc(0) = i;
         loc(1) = j;
//
         pixels(loc) = gauss2d(Double(i), Double(j)) + noiseGen();
      }
   }

//cout << "pixels=" << pixels.ac() << endl;

   Vector<Double> parameters(gauss2d.nAvailableParams());
   Vector<Bool> parameterMask(parameters.nelements(), True);
   Vector<Int> iMask(parameters.nelements(), 1);
   for (uInt i=0; i<parameters.nelements(); i++) {
      parameters(i) = gauss2d.getAvailableParam(i);
      if (mask[i]==0) {
         parameterMask(i) = False;
         iMask(i) = 0;
      }
   }
   cout << "True values  = " << parameters.ac() << endl;
   cout << "      mask   = " << iMask.ac() << endl;

   for (uInt i=0; i<parameters.nelements(); i++) {
      parameters(i) = parameters(i) * 0.9;
   }
   cout << "Start values = " << parameters.ac() << endl;
//
   Fit2D fitter(logger);
   fitter.addModel (Fit2D::Gaussian, parameters, parameterMask);
   if (includeRange.nelements()==2) {
      fitter.setIncludeRange(includeRange[0], includeRange[1]);
   }
   if (excludeRange.nelements()==2) {
      fitter.setExcludeRange(excludeRange[0], excludeRange[1]);
   }
   cout << "Number of models = " << fitter.nModels() << endl;
//

cout << endl << endl;
/*
cout << "pixels=" << pixels.ac() << endl;
cout << "mask=" << pixelMask.ac() << endl;
cout << "sigma=" << sigma.ac() << endl;
cout << "norm=" << norm << endl;
cout << endl << endl;
*/

   Fit2D::ErrorTypes status = fitter.fit(pixels, pixelMask, sigma, norm);
   if (status==Fit2D::OK) {
      cout << "Number of iterations = " << fitter.numberIterations() << endl;
      cout << "Number of points     = " << fitter.numberPoints() << endl;
      cout << "Chi squared = " << fitter.chiSquared() << endl << endl;
      cout << "Global available solution = " << fitter.availableSolution().ac() << endl;
      cout << "Global solution           = " << fitter.solution().ac() << endl;
      for (uInt i=0; i<fitter.nModels(); i++) {
        cout << "Model " << i << " of type " << Fit2D::type(fitter.type(i)) <<
                " has available solution "  << fitter.availableSolution(i).ac() << endl;
//
        cout << "Model " << i << " of type " << Fit2D::type(fitter.type(i)) <<
                " has solution           "  << fitter.solution(i).ac() << endl;
      }
//
      fitter.residual(resid, pixels);
      cout << "Residual min and max = " << min(resid.ac()) 
             << ", " << max(resid.ac()) << endl;
   } else {
     logger << fitter.errorMessage() << endl;
   }

// Test copy constructor
  
   {
      cout << endl << endl << "Test copy constructor" << endl;
      Fit2D fitter2(fitter);
      Fit2D::ErrorTypes status = fitter2.fit(pixels, pixelMask, sigma, norm);      
      if (!allEQ(fitter.availableSolution().ac(),fitter2.availableSolution().ac()) ||
          fitter.numberIterations() != fitter2.numberIterations() ||
          fitter.chiSquared() != fitter2.chiSquared() ||
          fitter.numberPoints() != fitter2.numberPoints()) {
         cout << "Failed copy constructor test" << endl;
      } else {
         cout << "Copy constructor test ok" << endl;
      }
   }

// Test assignment

   {
      cout << endl << endl << "Test assignment operator" << endl;
      Fit2D fitter2(logger);
      fitter2 = fitter;
      Fit2D::ErrorTypes status = fitter2.fit(pixels, pixelMask, sigma, norm);
      if (!allEQ(fitter.availableSolution().ac(),fitter2.availableSolution().ac()) ||
          fitter.numberIterations() != fitter2.numberIterations() ||
          fitter.chiSquared() != fitter2.chiSquared() ||
          fitter.numberPoints() != fitter2.numberPoints()) {
         cout << "Failed assignment test" << endl;
      } else {
         cout << "Assignment test ok" << endl;
      }
   }
 } catch (AipsError x) {
      cout << "Failed with message " << x.getMesg() << endl;
 } end_try;  
 
}


