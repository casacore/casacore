//# tFit2D.cc: Test nonlinear least squares classes
//# Copyright (C) 1995,1996,1998,1999,2000
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

#include <aips/Fitting.h>
#include <trial/Fitting/Fit2D.h>
#include <trial/Tasking/PGPlotter.h>
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

//
// Inputs
// 
   Input inputs(1);
   inputs.version ("$Revision$");
   inputs.create("noise", "0.0", "Noise");
   inputs.create("height", "3.0", "Height");
   inputs.create("xcen", "0.0", "xcen");
   inputs.create("ycen", "0.0", "ycen");
   inputs.create("major", "10.0", "major");
   inputs.create("minor", "5.0", "minor");
   inputs.create("pa", "45", "pa");
//
   inputs.create("nx", "20", "nx");
   inputs.create("ny", "20", "ny");   
   inputs.create("min", "0.0", "min");  
   inputs.create("max", "0.0", "max");  
   inputs.create("nbins", "20", "nbins"); 
   inputs.create("plotter", "", "plotter"); 
//
   inputs.create("norm", "False", "Normalize");
   inputs.create("mask", "1,1,1,1,1,1", "Mask");
   inputs.create("include", "0.0", "include");
   inputs.create("exclude", "0.0", "exclude");
//
   inputs.readArguments(argc, argv);
   const Double noise = inputs.getDouble("noise");
   const Double height = inputs.getDouble("height");
   const Double xcen = inputs.getDouble("xcen");
   const Double ycen = inputs.getDouble("ycen");
   const Double major = inputs.getDouble("major");
   const Double minor= inputs.getDouble("minor");
   const Double pa = inputs.getDouble("pa") * 3.1415926 / 180.0;
//
   const Int nx = inputs.getInt("nx");   
   const Int ny = inputs.getInt("ny");   
   const Int nbins = inputs.getInt("nbins");   
   const Double minD = inputs.getDouble("min");
   const Double maxD = inputs.getDouble("max"); 
   const String device = inputs.getString("plotter"); 
//
   const Bool norm = inputs.getBool("norm");
   const Block<Int> mask = inputs.getIntArray("mask");
   const Block<Double> includeRange = inputs.getDoubleArray("include");
   const Block<Double> excludeRange = inputs.getDoubleArray("exclude");
//
// Make data
//
   LogOrigin or("tFit2D", "main()", WHERE);
   LogIO logger(or);
   MLCG generator; 
   Normal noiseGen(&generator, 0.0, noise);  

   Gaussian2D<Double> gauss2d;
   gauss2d.setHeight(height);
   gauss2d.setMajorAxis(major);
   gauss2d.setMinorAxis(minor);
   if (xcen==0.0 && ycen==0.0) {
      gauss2d.setXcenter(Double(nx/2));
      gauss2d.setYcenter(Double(ny/2));
   } else {
      gauss2d.setXcenter(xcen);
      gauss2d.setYcenter(ycen);
   }
   gauss2d.setPA(pa);
//
   IPosition shape(2,nx,ny);
   Array<Float> pixels(shape);
   Array<Float> sigma(shape);
   Array<Float> resid;
   Array<Bool> pixelMask;
//
   Vector<Float> data(nx*ny);
   Float dMin = 1e30;
   Float dMax = -1e30;
//
   IPosition loc(2);
   uInt k = 0;
   for (Int j=0; j<shape(1); j++) {
      for (Int i=0; i<shape(0); i++) {
         loc(0) = i;
         loc(1) = j;
//
         pixels(loc) = gauss2d(Double(i), Double(j)) + noiseGen();
         if (noise==0.0) {
             sigma(loc) = 1.0;
         } else {
             sigma(loc) = noise;
         }
//
         data(k) = pixels(loc);
         dMin = min(dMin,data(k));
         dMax = max(dMax,data(k));
         k++;
      }
   }
//
// Plot data
//
   if (!device.empty()) {
      PGPlotter plotter(device);
      if (minD==0.0 && maxD==0.0) {
         plotter.hist(data, dMin, dMax, nbins, 0);
      } else {
         plotter.hist(data, Float(minD), Float(maxD), nbins, 0);
      }
   }

//
//cout << "pixels=" << pixels << endl;
//cout << "sigma=" << sigma << endl;
//cout << "norm=" << norm << endl;
//cout << endl << endl;

//
// Set mask
//
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

//
// convert axial ratio to minor axis (availableParameter
// interface uses axial ratio)
//
   parameters(4) = parameters(4)*parameters(3);  
   cout << "      mask   = " << iMask << endl;
   cout << "True values  = " << parameters << endl;

//
// Set starting guess
//
   Vector<Double> startParameters(parameters.copy());
   for (uInt i=0; i<parameters.nelements(); i++) {
      startParameters(i) = parameters(i) * 0.9;
   }
   cout << "Start values = " << startParameters << endl;
//
// Make fitter and set state
//
   Fit2D fitter(logger);
   fitter.addModel (Fit2D::GAUSSIAN, startParameters, parameterMask);
   if (includeRange.nelements()==2) {
      fitter.setIncludeRange(includeRange[0], includeRange[1]);
   }
   if (excludeRange.nelements()==2) {
      fitter.setExcludeRange(excludeRange[0], excludeRange[1]);
   }
//

   Fit2D::ErrorTypes status = fitter.fit(pixels, pixelMask, sigma, norm);
   if (status==Fit2D::OK) {
      cout << "Solution     = " << fitter.availableSolution() << endl;
      Vector<Double> cv = fitter.covariance().diagonal();
      cout << "Covariance     = " << cv << endl;
//      cout << "SNR        = " << fitter.availableSolution() / 
//                             sqrt(cv) << endl;
      cout << "Chi squared = " << fitter.chiSquared() << endl << endl;
      cout << "Number of iterations = " << fitter.numberIterations() << endl;
      cout << "Number of points     = " << fitter.numberPoints() << endl;
//
//   when i return errors, make a test to 3sigma or summfink
//      if (!allNear(fitter.availableSolution(), parameters, 1e-6)) {
//         throw (AipsError("Solution not accurate to 1e-6"));
//      }
//
      cout << "Number of models = " << fitter.nModels() << endl;
      for (uInt i=0; i<fitter.nModels(); i++) {
        cout << "Model " << i << " of type " << Fit2D::type(fitter.type(i)) <<
                " has solution "  << fitter.availableSolution(i) << endl;
      }
//
      fitter.residual(resid, pixels);
      cout << "Residual min and max = " << min(resid) << " " << max(resid) << endl;
   } else {
     logger << fitter.errorMessage() << endl;
   }

// Test copy constructor
   {
      cout << endl << endl << "Test copy constructor" << endl;
      Fit2D fitter2(fitter);
      Fit2D::ErrorTypes status = fitter2.fit(pixels, pixelMask, sigma, norm);      
      if (!allEQ(fitter.availableSolution(),fitter2.availableSolution()) ||
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
      if (!allEQ(fitter.availableSolution(),fitter2.availableSolution()) ||
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
 }   
 
}


