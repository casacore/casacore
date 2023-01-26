//# tFit2D.cc: Test nonlinear least squares classes for 2D Gaussian
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002,2003
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

#include <casacore/scimath/Fitting.h>
#include <casacore/lattices/LatticeMath/Fit2D.h>
#include <casacore/casa/System/PGPlotter.h>
#include <casacore/scimath/Functionals/Gaussian2D.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Logging.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
Gaussian2D<double> addModel (Array<float>& pixels, double height, double x, double y, 
                               double major,  double minor, double pa);

void addNoise (Array<float>& pixels, Array<float>& sigma, double noise);

int main(int argc, const char *argv[])
{

  try {

//
// Inputs
// 
   Input inputs(1);
   inputs.version ("$Revision$");
   inputs.create("nmodels", "1", "nmodels"); 
   inputs.create("noise", "0.0001", "Noise");
   inputs.create("major", "10.0", "major");
   inputs.create("minor", "5.0", "minor");
   inputs.create("pa", "45", "pa");                  // +x -> +y
   inputs.create("nx", "64", "nx");
   inputs.create("ny", "64", "ny");   
   inputs.create("norm", "False", "Normalize");
   inputs.create("mask", "1,1,1,1,1,1", "Mask");
   inputs.create("include", "0.0", "include");
   inputs.create("exclude", "0.0", "exclude");
//
   inputs.readArguments(argc, argv);
   const int32_t nModels  = inputs.getInt("nmodels");   
   const double noise = inputs.getDouble("noise");
   double major = inputs.getDouble("major");
   double minor= inputs.getDouble("minor");
   double pa = inputs.getDouble("pa") * C::pi / 180.0;          // +x -> +y
   const int32_t nx = inputs.getInt("nx");   
   const int32_t ny = inputs.getInt("ny");   
   ///const bool norm = inputs.getBool("norm");
   const Block<int32_t> mask = inputs.getIntArray("mask");
   const Block<double> includeRange = inputs.getDoubleArray("include");
   const Block<double> excludeRange = inputs.getDoubleArray("exclude");
//
   LogOrigin lor("tFit2D", "main()", WHERE);
   LogIO logger(lor);
//
   Fit2D fitter(logger);
//
   IPosition shape(2,nx,ny);
   Array<float> pixels(shape, float(0));
   Array<float> sigma(shape);
   Matrix<double> saveEstimate(nModels, 6);
//
   double xsep = nx / nModels;
   double ysep = ny / nModels;
   double xPos, yPos;
   if (nModels==1) {
      xPos = nx / 2.0;
      yPos = ny / 2.0;     
   } else {
      xPos = xsep / 2.0;
      yPos = ysep / 2.0;
   }
   double height = 1.0;

//
   Vector<double> trueHeight(nModels);
   Vector<double> trueX(nModels);
   Vector<double> trueY(nModels);
   Vector<double> trueMajor(nModels);
   Vector<double> trueMinor(nModels);
   Vector<double> truePA(nModels);
//
   Vector<bool> saveMask;
   Vector<double> startParameters;
   Vector<bool> parameterMask;
   for (int32_t i=0; i<nModels; i++) {

// Add model to data array

      Gaussian2D<double> gauss2d = addModel(pixels, height, xPos, yPos, major, minor, pa);
      trueHeight(i) = height;
      trueX(i) = xPos;
      trueY(i) = yPos;
      trueMajor(i) = major;
      trueMinor(i) = minor;
      truePA(i) = pa;

// Set Parameters mask

      Vector<double> parameters(gauss2d.nparameters());
      parameterMask = Vector<bool>(gauss2d.nparameters(), true);
      for (uint32_t j=0; j<parameters.nelements(); j++) {
         parameters(j) = gauss2d[j];
         if (mask[j]==0) {
            parameterMask(j) = false;
         }
      }
      if(i==0) saveMask = parameterMask;
      parameters(5) = Fit2D::paFromGauss2D(parameters(5));

// convert axial ratio to minor axis (availableParameter
// interface uses axial ratio)

      parameters(4) = parameters(4)*parameters(3);  
/*
      cout << "True pa (+x -> +y) = " << parameters(5) * 180.0 / C::pi << endl;
*/

// Set starting guess

      startParameters = parameters.copy();
      for (uint32_t j=0; j<parameters.nelements(); j++) {
         startParameters(j) = parameters(j) * 0.9;
      }
      saveEstimate.row(i) = startParameters;
/*
      cout << "Start pa (+x -> +y) = " << startParameters(5) * 180.0 / C::pi << endl;
*/

// Add model to fitter

      fitter.addModel (Fit2D::GAUSSIAN, startParameters, parameterMask);

// Update model

      height *= 0.75;
      xPos += xsep;
      yPos += ysep;
//
      major *= 0.9;
      minor *= 0.9;
      pa += C::pi / 180 * 20.0;
      if (pa > C::pi) pa -= C::pi;
      cerr << endl;
   }

// Add noise
 
   addNoise (pixels, sigma, noise);

// Set other state of fitter

   if (includeRange.nelements()==2) {
      fitter.setIncludeRange(includeRange[0], includeRange[1]);
   }
   if (excludeRange.nelements()==2) {
      fitter.setExcludeRange(excludeRange[0], excludeRange[1]);
   }

// Make fit

   Fit2D::ErrorTypes status = fitter.fit(pixels, sigma);
   if (status==Fit2D::OK) {
      cout << "Chi squared = " << fitter.chiSquared() << endl << endl;
      cout << "Number of iterations = " << fitter.numberIterations() << endl;
      cout << "Number of points     = " << fitter.numberPoints() << endl;
//
//   when i return errors, make a test to 3sigma or summfink
//      if (!allNear(fitter.availableSolution(), parameters, 1e-6)) {
//         throw (AipsError("Solution not accurate to 1e-6"));
//      }
//
      cout << endl << "Number of models = " << fitter.nModels() << endl;
      for (uint32_t i=0; i<fitter.nModels(); i++) {
        Vector<double> xx(5);
        xx(0) = trueHeight(i); xx(1) = trueX(i); 
        xx(2) = trueY(i); xx(3) = trueMajor(i); xx(4) = truePA(i);
//
        Vector<double> solution = fitter.availableSolution(i);
        Vector<double> errors = fitter.availableErrors(i);

        cout << "Model " << i << " of type " << Fit2D::type(fitter.type(i)) << endl;
        cout << "   Estimate      = " << saveEstimate.row(i) << endl;
        cout << "   Mask          = " << saveMask << endl;
        cout << "   Actual values = " << xx << endl;
        cout << "   Solution      = " << solution  << endl;
        cout << "   Errors        = " << errors  << endl;
        cout << "   SNR           = " << solution / errors << endl;

      }
//
      Array<float> resid;
      Array<float> model;
      fitter.residual(resid, model, pixels);
      cout << "Residual min and max = " << min(resid) << " " << max(resid) << endl;
   } else {
     logger << fitter.errorMessage() << endl;
   }

// Test copy constructor

   {
      cout << endl << endl << "Test copy constructor" << endl;
      Fit2D fitter2(fitter);
      fitter2.fit(pixels, sigma);
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
      fitter2.fit(pixels, sigma);
      if (!allEQ(fitter.availableSolution(),fitter2.availableSolution()) ||
         fitter.numberIterations() != fitter2.numberIterations() ||
         fitter.chiSquared() != fitter2.chiSquared() ||
         fitter.numberPoints() != fitter2.numberPoints()) {
         cout << "Failed assignment test" << endl;
      } else {
         cout << "Assignment test ok" << endl;
      }
   }

// Test Estimate

   {
    
       cout << endl << endl << "Test estimator" << endl;
       IPosition shape(2,128,128);
       Array<float> psf(shape);
       IPosition index(2);
       const double fwhm2sigma = sqrt(8.*log(2.));
       for (index[0] = 0; index[0]<128; ++index[0]) {
           for (index[1] = 0; index[1]<128; ++index[1]) {
               const double xOffset = (double(index[0])-64.);
               const double yOffset = (double(index[1])-64.);
               const double expFactor = exp(-square(xOffset/2*fwhm2sigma)/2.-
                            square(yOffset/2.*fwhm2sigma)/2.);
               psf(index) = expFactor;
           }
       }
 
       LogIO logger;
       Fit2D fitter2(logger);
       Vector<double> param = fitter2.estimate(Fit2D::GAUSSIAN, psf);
       
       cout << "Estimate " << param << endl;

       if (abs(param[1]-64)>1e-5 || abs(param[2]-64)>1e-6)
          throw (AipsError("Estimate position not accurate to 1e-6"));

       fitter2.addModel(Fit2D::GAUSSIAN, param);
       Array<float> sigma(psf.shape());
       sigma.set(1.);
       if (fitter2.fit(psf,sigma) == Fit2D::OK) {
          param = fitter2.availableSolution();
          cout << "Fitted: " << param << endl;
          if (abs(param[1]-64)>1e-5 || abs(param[2]-64)>1e-6)
              throw (AipsError("Fit position not accurate to 1e-6"));

       }
       else {
          cerr << "Failed to fit" << endl;
       }
       cout << endl << endl << "Estimate test ok" << endl;
   }

   Fit2D fitter3(logger);
   fitter3.addModel(Fit2D::LEVEL, Vector<double>(1, 4.5));
   Array<float> pixels3 = pixels.copy();
   pixels3.set(4.5);

   double noise3 = 1;
   //cout << "noise " << noise3 << endl;


   addNoise (pixels3, sigma, noise3);

   fitter3.fit(pixels3, sigma);
   cout << "const solution " << fitter3.availableSolution() << endl;
   cout << "const error " << fitter3.availableErrors() << endl;
   cout << "Chi squared = " << fitter3.chiSquared() << endl << endl;
   cout << "Number of iterations = " << fitter3.numberIterations() << endl;
   cout << "Number of points     = " << fitter3.numberPoints() << endl;

   Fit2D fitter4(logger);
   Array<float> pixels4 = pixels3;
   pixels4.set(5);
   pixels4 += pixels.copy();

   fitter4.addModel (Fit2D::GAUSSIAN, startParameters, parameterMask);
   fitter4.addModel(Fit2D::LEVEL, Vector<double>(1, 4.5));
   fitter4.fit(pixels4, sigma);
   cout << "const solution " << fitter4.availableSolution() << endl;
   cout << "const error " << fitter4.availableErrors() << endl;
   cout << "Chi squared = " << fitter4.chiSquared() << endl << endl;
   cout << "Number of iterations = " << fitter4.numberIterations() << endl;
   cout << "Number of points     = " << fitter4.numberPoints() << endl;


/*
   fitter.addModel(Fit2D::LEVEL, Vector<double>(1, 4.5));
   Array<float> pixels4 = pixels + pixels3;
   fitter.fit(pixels4, sigma);
   cout << "const solution " << fitter.availableSolution() << endl;
   cout << "const error " << fitter.availableErrors() << endl;
   cout << "Chi squared = " << fitter.chiSquared() << endl << endl;
   cout << "Number of iterations = " << fitter.numberIterations() << endl;
   cout << "Number of points     = " << fitter.numberPoints() << endl;
*/

 } catch (std::exception& x) {
      cout << "Failed with message " << x.what() << endl;
 }   

}

Gaussian2D<double> addModel (Array<float>& pixels, double height, double xcen, double ycen,
                               double major,  double minor, double pa)
{
   Gaussian2D<double> gauss2d;
   gauss2d.setHeight(height);
   gauss2d.setMajorAxis(major);
   gauss2d.setMinorAxis(minor);
   gauss2d.setXcenter(xcen);
   gauss2d.setYcenter(ycen);
   gauss2d.setPA(Fit2D::paToGauss2D(pa));          // +y -> -x
//
   IPosition shape = pixels.shape();
   IPosition loc(2);
   for (int32_t j=0; j<shape(1); j++) {
      for (int32_t i=0; i<shape(0); i++) {
         loc(0) = i;
         loc(1) = j;
         pixels(loc) += gauss2d(double(i), double(j));
      }
   }
   return gauss2d;
}


void addNoise (Array<float>& pixels, Array<float>& sigma, double noise)
{
   sigma = 1.0;
   if (noise>0.0) sigma = noise;
//
   MLCG generator; 
   Normal noiseGen(&generator, 0.0, noise);  
//
   bool deleteIt;
   float* pData = pixels.getStorage(deleteIt);
   for (int32_t k=0; k<pixels.shape().product(); k++){
      pData[k] += noiseGen();
   }
   pixels.putStorage(pData, deleteIt);
}

