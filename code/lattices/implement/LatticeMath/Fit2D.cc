//# Fit2D.cc: Class to fit 2D objects to a Lattice or Array
//# Copyright (C) 1997,1998
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

#include <trial/Fitting/Fit2D.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <trial/Fitting/NonLinearFitLM.h>
#include <aips/Functionals/Gaussian2D.h>
#include <aips/Functionals/SumFunction.h>
#include <trial/Functionals/FuncWithAutoDerivs.h>
#include <trial/Lattices/Lattice.h>
#include <trial/Lattices/MaskedLattice.h>
#include <aips/Logging/LogIO.h>
#include <aips/Mathematics/Math.h>
#include <trial/Mathematics/AutoDiff.h>
#include <aips/Utilities/Assert.h>

#include <iostream.h>
#include <strstream.h>


Fit2D::Fit2D(LogIO& logger)
: itsLogger(logger),
  itsValid(False),
  itsValidSolution(False),
  itsChiSquared(0.0)
{
}

Fit2D::Fit2D(const Fit2D& other)
: itsLogger(other.itsLogger),                  // Reference semantics
  itsValid(other.itsValid),
  itsValidSolution(other.itsValidSolution),
  itsIsNormalized(other.itsIsNormalized),
  itsHasSigma(other.itsHasSigma),
  itsInclude(other.itsInclude),
  itsPixelRange(other.itsPixelRange.copy()),   // Copy semantics
  itsFunction(other.itsFunction),              // Copy semantics
  itsSolution(other.itsSolution.copy()),       // Copy semantics
  itsChiSquared(other.itsChiSquared),
  itsErrorMessage(other.itsErrorMessage),
  itsNumberPoints(other.itsNumberPoints),
  itsTypeList(other.itsTypeList.copy()),       // Copy semantics
  itsNormVal(other.itsNormVal),
  itsNormPos(other.itsNormPos)
{
// 
// Note that the variable itsFitter is not copied.
// This is because the fitting classes have no 
// assignment operator or copy constructor.  However,
// it doesn't matter, because the fitter is always
// set as needed by the "fit" function.  The fact that it
// is private is just to avoid creating it over and over
}


Fit2D::~Fit2D()
{
}

Fit2D& Fit2D::operator=(const Fit2D& other)
// 
// Note that the variable itsFitter is not copied.
// This is because the fitting classes have no 
// assignment operator or copy constructor.  However,
// it doesn't matter, because the fitter is always
// set as needed by the "fit" function.  The fact that it
// is private is just to avoid creating it over and over
//
{
   if (this != &other) {
      itsLogger = other.itsLogger;                  // Reference semantics
      itsValid = other.itsValid;
      itsValidSolution = other.itsValidSolution;
      itsIsNormalized = other.itsIsNormalized;
      itsHasSigma = other.itsHasSigma;
      itsInclude = other.itsInclude;
      itsPixelRange = other.itsPixelRange.copy();   // Copy semantics
      itsFunction = other.itsFunction;              // Copy semantics
      itsSolution = other.itsSolution.copy();       // Copy semantics
      itsChiSquared = other.itsChiSquared;
      itsErrorMessage = other.itsErrorMessage;
      itsNumberPoints = other.itsNumberPoints;
      itsTypeList = other.itsTypeList.copy();       // Copy semantics
      itsNormVal = other.itsNormVal;
      itsNormPos = other.itsNormPos;
   }
   return *this;
}




uInt Fit2D::addModel (Fit2D::Types type,
                      const Vector<Double>& parameters,
                      const Vector<Bool>& parameterMask)
{
   const uInt nModels = itsTypeList.nelements() + 1;
   itsTypeList.resize(nModels,True);
//
   if (type==Fit2D::LEVEL) {
      itsLogger << LogIO::SEVERE << "Fit2D - Level fitting not yet implemented" << LogIO::POST;
   } else if (type==Fit2D::DISK) {
      itsLogger << LogIO::SEVERE << "Fit2D - Disk fitting not yet implemented" << LogIO::POST;
   } else if (type==Fit2D::GAUSSIAN) {
// 
// Create functional
//
      Gaussian2D<AutoDiff<Double> > gauss2d;
      if (parameters.nelements() != gauss2d.nAvailableParams()) {
         itsLogger << LogIO::SEVERE << "Fit2D - illegal number of parameters" << LogIO::POST;
      }
      if (parameterMask.nelements() != gauss2d.nAvailableParams()) {
         itsLogger << LogIO::SEVERE << "Fit2D - illegal number of mask parameters" << LogIO::POST;
      }
//
// Set parameters.  0 (flux), 1 (x), 2 (y), 3 (major), 4 (minor), 5 (pa)
//
      for (uInt i=0; i<gauss2d.nAvailableParams(); i++) {
         if (i==4) {
//
// Convert minor axis specification to an axial ratio.
//
            Double ratio = parameters(4) / parameters(3);
            gauss2d.setAvailableParam(i, AutoDiff<Double>(ratio));
         } else {
            gauss2d.setAvailableParam(i, AutoDiff<Double>(parameters(i)));
         }
         gauss2d.setAvailableParamMask(i, parameterMask(i));
      }
//
// Add it to function we are going to fit
//
      itsFunction.addFunction(gauss2d);
      itsTypeList(nModels-1) = Fit2D::GAUSSIAN;
   }
//
   itsValid = True;
   return nModels - 1;
}



uInt Fit2D::addModel (Fit2D::Types type,
                      const Vector<Double>& parameters)
{
   Vector<Bool> parameterMask(parameters.nelements(),True);
   return addModel(type, parameters, parameterMask);
}


uInt Fit2D::nModels() const
{
   return itsFunction.nFunctions();
}


Vector<Bool> Fit2D::convertMask (const String mask,
                                   Fit2D::Types type)
{
   Vector<Bool> parameterMask;
   if (type==Fit2D::LEVEL) {
      throw (AipsError("Fit2D - Level fitting not yet implemented"));
   } else if (type==Fit2D::DISK) {
      throw (AipsError("Fit2D - Disk fitting not yet implemented"));
   } else if (type==Fit2D::GAUSSIAN) {
      parameterMask.resize(6);
      parameterMask = True;
      if (mask.contains("f")) parameterMask(0) = False;
      if (mask.contains("x")) parameterMask(1) = False;
      if (mask.contains("y")) parameterMask(2) = False;
      if (mask.contains("a")) parameterMask(3) = False;
      if (mask.contains("b")) parameterMask(4) = False;
      if (mask.contains("p")) parameterMask(5) = False;
   }
   return parameterMask;
}

uInt Fit2D::nParameters(Fit2D::Types type)
{
    uInt n = 0;
   if (type==Fit2D::LEVEL) {
      throw (AipsError("Fit2D - Level fitting not yet implemented"));
   } else if (type==Fit2D::DISK) {
      throw (AipsError("Fit2D - Disk fitting not yet implemented"));
   } else if (type==Fit2D::GAUSSIAN) {
      n = 6;
   }
   return n;
}

 
Fit2D::ErrorTypes Fit2D::fit(const MaskedLattice<Float>& data, 
                             const Lattice<Float>& sigma, Bool norm)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
//
// Get data
//
   Array<Float> pixels = data.get(True);
   IPosition shape = pixels.shape();
   if (shape.nelements() !=2) {
      itsLogger << LogIO::SEVERE << "Fit2D::fit - Region must be 2-dimensional" << LogIO::POST;
   }
   Array<Bool> mask = data.getMask(True);
//
// Do fit
//
   if (sigma.ndim()==0) {
      Array<Float> sigma2;
      return fit(pixels, mask, sigma2, norm);
   } else {
      Array<Float> sigma2 = sigma.get(True);
      return fit(pixels, mask, sigma2, norm);
   }
}




Fit2D::ErrorTypes Fit2D::fit(const Lattice<Float>& data, 
                             const Lattice<Float>& sigma,
                             Bool norm)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
//
   Array<Float> pixels = data.get(True);
   IPosition shape = pixels.shape();
   if (shape.nelements() !=2) {
      itsLogger << LogIO::SEVERE << "Fit2D::fit - Region must be 2-dimensional" << LogIO::POST;
   }
   Array<Bool> mask;
//
   if (sigma.ndim()==0) {
      Array<Float> sigma2;
      return fit(pixels, mask, sigma2, norm);
   } else {
      Array<Float> sigma2 = sigma.get(True);
      return fit(pixels, mask, sigma2, norm);
   }
}


Fit2D::ErrorTypes Fit2D::fit(const Array<Float>& data, 
                             const Array<Float>& sigma, 
                             Bool norm)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   if (data.ndim() !=2) {
      itsLogger << LogIO::SEVERE << "Fit2D::fit - Array must be 2-dimensional" << LogIO::POST;
   }
   if (sigma.nelements() !=0) {
      if (!data.shape().isEqual(sigma.shape())) {
         itsLogger << LogIO::SEVERE << "Fit2D::fit - Sigma and pixel arrays must have the same shape" << LogIO::POST;
      }
   }
//
   itsIsNormalized = norm;
//
   Matrix<Double> pos;
   Vector<Double> values;
   Vector<Double> weights;
   Array<Bool> mask;
   if (!normalizeData (pos, values, weights, data, mask, sigma)) {
      itsErrorMessage = String("There were no selected data points");
      return Fit2D::NOGOOD;
   }
   if (itsIsNormalized) normalizeModels (0);
//
   return fit(values, pos, weights);
}


Fit2D::ErrorTypes Fit2D::fit(const Array<Float>& data,
                             const Array<Bool>& mask, 
                             const Array<Float>& sigma, 
                             Bool norm)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   if (data.ndim() !=2) {
      itsLogger << LogIO::SEVERE << "Fit2D::fit - Array must be 2-dimensional" << LogIO::POST;
   }
   if (mask.nelements() !=0) {
      if (!data.shape().isEqual(mask.shape())) {
         itsLogger << LogIO::SEVERE << "Fit2D::fit - Mask and pixel arrays must have the same shape" << LogIO::POST;
      }
   }
   if (sigma.nelements() !=0) {
      if (!data.shape().isEqual(sigma.shape())) {
         itsLogger << LogIO::SEVERE << "Fit2D::fit - Sigma and pixel arrays must have the same shape" << LogIO::POST;
      }
   }
//
   itsIsNormalized = norm;
//
   Matrix<Double> pos;
   Vector<Double> values;
   Vector<Double> weights;
   if (!normalizeData (pos, values, weights, data, mask, sigma)) {
      itsErrorMessage = String("There were no selected data points");
      return Fit2D::NOGOOD;
   }
   if (itsIsNormalized) normalizeModels (0);
//
   return fit(values, pos, weights);
}


Fit2D::ErrorTypes Fit2D::residual(Array<Float>& resid,
                                  const Array<Float>& data)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   if (!itsValidSolution) {
      return Fit2D::FAILED;
   }
//
   if (data.ndim() !=2) {
      itsLogger << LogIO::SEVERE << "Fit2D::fit - Array must be 2-dimensional" << LogIO::POST;
   }
   IPosition shape = data.shape();
//
   if (resid.nelements() ==0) {
      resid.resize(shape);
   } else {
      if (!shape.isEqual(resid.shape())) {
         itsLogger << LogIO::SEVERE << "Fit2D::fit - Residual and pixel arrays must have the same shape" << LogIO::POST;
      }
   }
//
// Create a functional with the solution (no axis conversion
// necesary because functional interface takes axial ratio)
//
   SumFunction<AutoDiff<Double>,AutoDiff<Double> > sumFunction(itsFunction);
   Vector<Double> sol = getAvailableSolution();
   for (uInt i=0; i<sol.nelements(); i++) {
      sumFunction.setAvailableParam(i, AutoDiff<Double>(sol(i)));
   }
//
   IPosition loc(2);
   Vector<AutoDiff<Double> > pos(2);
   AutoDiff<Double> t1, t2;
//
   for (Int j=0; j<shape(1); j++) {
      for (Int i=0; i<shape(0); i++) {
         loc(0) = i;
         loc(1) = j;
         t1.value() = Double(i);
         t2.value() = Double(j);
         pos(0) = t1;
         pos(1) = t2;
         resid(loc) = data(loc) - sumFunction(pos).value();
      }
   }
   return Fit2D::OK;
}


Fit2D::ErrorTypes Fit2D::residual(Array<Float>& resid,
                                  const MaskedLattice<Float>& data)
{
   Array<Float> pixels = data.get(True);
   return residual(resid, pixels);
}

Fit2D::ErrorTypes Fit2D::residual(Array<Float>& resid,
                                  const Lattice<Float>& data)
{
   Array<Float> pixels = data.get(True);
   return residual(resid, pixels);
}

void Fit2D::setIncludeRange (Double minVal, Double maxVal)
{
   itsPixelRange.resize(2);
   itsPixelRange(0) = min(minVal, maxVal);
   itsPixelRange(1) = max(minVal, maxVal);
   itsInclude = True;
}

void Fit2D::setExcludeRange (Double minVal, Double maxVal)
{
   itsPixelRange.resize(2);
   itsPixelRange(0) = min(minVal, maxVal);
   itsPixelRange(1) = max(minVal, maxVal);
   itsInclude = False;
}

String Fit2D::type(Fit2D::Types type)
{
   if (type==Fit2D::LEVEL) {
      return String("Level");
   } else if (type==Fit2D::DISK) {
      return String("Disk");
   } else if (type==Fit2D::GAUSSIAN) {
      return String("Gaussian");
   }
   return String("");
}


Fit2D::Types Fit2D::type(uInt which) 
{
   if (which+1 > itsFunction.nFunctions()) {
      itsLogger << LogIO::SEVERE << "Fit2D::type - illegal model index" << LogIO::POST;
   }
   return (Fit2D::Types)itsTypeList(which);
}
 


Vector<Double> Fit2D::availableSolution () 
//
// Conversion of Gaussian models from axial ratio
// to minor axis is done
//
{
   const uInt nF = itsFunction.nFunctions();
   Vector<Double> sol;
   uInt l = 0;
   for (uInt i=0; i<nF; i++) {
      Vector<Double> sol2 = availableSolution(i);
      sol.resize(l+sol2.nelements(),True);
      for (uInt j=0; j<sol2.nelements(); j++) {
         sol(l+j) = sol2(j);
      }
      l = sol.nelements();
   }
   return sol;
} 
   
Vector<Double> Fit2D::availableSolution (uInt which) 
// 
//  For Gaussian models, convert axial ratio to minor axis.
// 
{
   if (!itsValidSolution) {
      Vector<Double> tmp;
      return tmp;
   }
//
   if (which+1 > itsFunction.nFunctions()) {
      itsLogger << LogIO::SEVERE << "Fit2D::solution - illegal model index" << LogIO::POST;
   }
//
// Find the mask and parameters for this model. Recover these when the 
// mask is False (fixed) otherwise recover the solution
//
   uInt iStart;
   Vector<Double> sol = getSolution(iStart, which);
   Vector<Bool> mask = itsFunction.function(which)->getAvailableParamMasks();
   Vector<Double> params = getParams(which);
   Vector<Double> sol2(params.nelements());
   uInt idx = 0;
   for (uInt i=0; i<sol2.nelements(); i++) {
      if (mask(i)) {
         sol2(i) = sol(idx);
         idx++;
      } else {
         sol2(i) = params(i);
      }
   }
//
// Convert Gaussian solution axial ratio to major/minor axis
// The solution may have a negative axial ratio
//
   if (itsTypeList(which)==Fit2D::GAUSSIAN) {
      Double major = max(abs(sol2(3)), abs(sol2(3)*sol2(4)));
      Double minor = min(abs(sol2(3)), abs(sol2(3)*sol2(4)));
      sol2(3) = major;
      sol2(4) = minor;
   }
//
   return sol2;
}



String Fit2D::errorMessage () const
{
   return itsErrorMessage;
}


uInt Fit2D::numberIterations() const
{
   return itsFitter.currentIteration();
}

Double Fit2D::chiSquared () const
{
   if (!itsValidSolution) {
      return -1.0;
   }
   return itsChiSquared;
}


uInt Fit2D::numberPoints () const
{
   return itsNumberPoints;
}



Vector<Double> Fit2D::getParams(uInt which) const
//
// Recover the available parameters for this model
// from the SumFunction
//
{
   Vector<Double> params(itsFunction.function(which)->nAvailableParams());
   for (uInt i=0; i<params.nelements(); i++) {
     params(i) = itsFunction.function(which)->getAvailableParams()(i).value();
   }
   return params;
}

void Fit2D::setParams(const Vector<Double>& params, uInt which)
//
// Set the available parameters for this model
// from the SumFunction
//
{
   Vector<AutoDiff<Double> > params2(params.nelements());
   for (uInt i=0; i<params2.nelements(); i++) {
     params2(i) = AutoDiff<Double>(params(i));
   }
   itsFunction.function(which)->setAvailableParams(params2);
}



// Private functions

Bool Fit2D::normalizeData (Matrix<Double>& pos, Vector<Double>& values, 
                           Vector<Double>& weights, const Array<Float>& pixels,
                           const Array<Bool>& mask, const Array<Float>& sigma)
//
// Fish out the unmasked data, and optionally normalize it.
// They may help rounding problems for many parameter fits.
//
// If the mask is of zero length all pixels are assumed good.
// If the sigma array is of zero length the weights are given
// the value 1.0
//
// If there are no good pixels returns False
//
{
   IPosition shape = pixels.shape();
   uInt nPoints = shape.product();
//
// Handle pixel ranges
//
   Vector<Float> pixelRange(2);
   Int includeThem = 0;
   if (itsPixelRange.nelements()==2) {
      pixelRange(0) = itsPixelRange(0);
      pixelRange(1) = itsPixelRange(1);
      if (itsInclude) {
         includeThem = 1;
      } else {
         includeThem = -1;
      }
   }
//
// Do we have sigmas ?
//
   itsHasSigma = False;
   if (sigma.nelements() != 0) itsHasSigma = True;
//
// Find first unmasked oint
//
   Bool hasMask = True;
   if (mask.nelements()==0) hasMask = False;
   Float minVal, maxVal;
   if (hasMask) {
      Bool deleteIt1, deleteIt2;
      const Bool* p1 = mask.getStorage(deleteIt1);
      const Float* p2 = pixels.getStorage(deleteIt2);
      for (uInt i=0; i<nPoints; i++) {
         if (p1[i]) {
            minVal = p2[i];
            maxVal = p2[i];
            break;
         }
      }
      mask.freeStorage(p1, deleteIt1);
      pixels.freeStorage(p2, deleteIt2);
   } else {
      minVal = pixels(IPosition(2,0,0));
      maxVal = pixels(IPosition(2,0,0));
   }
//
// Find min/max and select data
//
   values.resize(nPoints);
   weights.resize(nPoints);
   Vector<Int> locX(nPoints);   
   Vector<Int> locY(nPoints);
   IPosition loc(2);
//
   itsNumberPoints = 0;
   for (Int j=0; j<shape(1); j++) {
      for (Int i=0; i<shape(0); i++) {
         loc(0) = i;
         loc(1) = j;
         if (!hasMask || (hasMask && mask(loc))) {
            if (includeIt(pixels(loc), pixelRange, includeThem)) {
               values(itsNumberPoints) = pixels(loc);
               if (itsHasSigma) {
                  weights(itsNumberPoints) = sigma(loc);
               } else {
                  weights(itsNumberPoints) = 1.0;
               }
               locX(itsNumberPoints) = i;
               locY(itsNumberPoints) = j;
               minVal = min(minVal, values(itsNumberPoints));
               maxVal = max(maxVal, values(itsNumberPoints));
//
               itsNumberPoints++;
            }
         }
      }
   }
   if (itsNumberPoints==0) return False;
//
// Resize arrays for actual number of selected points
//
   pos.resize(itsNumberPoints,2);
   values.resize(itsNumberPoints, True);
   weights.resize(itsNumberPoints, True);
   locX.resize(itsNumberPoints, True);
   locY.resize(itsNumberPoints, True);
//
   itsNormPos = max(shape(0)-1, shape(1)-1);
   itsNormVal = max(abs(maxVal),abs(minVal));
//
   if (itsIsNormalized) {
//
// Normalize the data
//
      Double dummyWidth;
      for (uInt k=0; k<itsNumberPoints; k++) {
         pos(k,0) = locX(k);
         pos(k,1) = locY(k);
         normalize (values(k), pos(k,0), pos(k,1), dummyWidth,
                    itsNormPos, itsNormVal);
         if (itsHasSigma) weights(k) = weights(k) / itsNormVal;
      }
   } else {
//
// Just fill in the position matrix
//
      for (uInt k=0; k<itsNumberPoints; k++) {
         pos(k,0) = locX(k);
         pos(k,1) = locY(k);
      }
   }
//   cout << "Data = " << values.ac() << endl;
//   cout << "weights = " << weights.ac() << endl;
//   cout << "Pos = " << pos.ac() << endl;
   if (itsIsNormalized) {
//      cout << "normVal = " << itsNormVal << endl;
//      cout << "normPos = " << itsNormPos << endl;
   }

   return True;
}

void Fit2D::normalizeModels (uInt direction) 
//
// Normalize the parameter estimates in the
// SumFunction in the same way that the data were
// normalized.  
//
// direction = 0 means normalize
// direction = 1 means un-normalize
//
{
   const uInt nModels = itsFunction.nFunctions();
   for (uInt i=0; i<nModels; i++) {
      Vector<Double> params = getParams(i);
      Fit2D::Types type = (Fit2D::Types)itsTypeList(i);
//
      if (direction==0) {
         if (type==Fit2D::LEVEL) {
            itsLogger << LogIO::SEVERE << "Fit2D - Level fitting not yet implemented" << LogIO::POST;
         } else if (type==Fit2D::DISK) {
            itsLogger << LogIO::SEVERE << "Fit2D - Disk fitting not yet implemented" << LogIO::POST;
         } else if (type==Fit2D::GAUSSIAN) {
            normalize (params(0), params(1), params(2), params(3),
                       itsNormPos, itsNormVal);
            setParams(params, i);
         }
      } else {
         if (type==Fit2D::LEVEL) {
            itsLogger << LogIO::SEVERE << "Fit2D - Level fitting not yet implemented" << LogIO::POST;
         } else if (type==Fit2D::DISK) {
            itsLogger << LogIO::SEVERE << "Fit2D - Disk fitting not yet implemented" << LogIO::POST;
         } else if (type==Fit2D::GAUSSIAN) {
            unNormalize (params(0), params(1), params(2), params(3),
                         itsNormPos, itsNormVal);
            setParams(params, i);
         }
      }
   }
}

 
Fit2D::ErrorTypes Fit2D::fit(const Vector<Double>& values, 
                             const Matrix<Double>& pos,
                             const Vector<Double>& weights)
//
// Do the actual fit
//
{

// Set maximum number of iterations to 1000
 
   itsFitter.setMaxIter(1000);
   
// Set converge criteria.  Default is 0.001
 
   itsFitter.setCriteria(0.001);
   
// Set the function and initial values

   FuncWithAutoDerivs<Double,Double> func(itsFunction); 
   itsFitter.setFunction(func);
   
// Find the solution

   itsChiSquared = 0;
   itsErrorMessage = "";
   Fit2D::ErrorTypes status = Fit2D::OK;
   itsSolution.resize(0);
   try {
      itsSolution = itsFitter.fit(pos, values, weights);
      if(!itsFitter.converged()) {
         itsErrorMessage = String("The fit did not converge");
         status = Fit2D::NOCONVERGE;
      }
//
// Find chi-squared.  Account for normalization factors
//
      itsChiSquared = itsFitter.chiSquare(pos, values, weights, itsSolution);
      if (itsIsNormalized && !itsHasSigma) {
         itsChiSquared = itsChiSquared * itsNormVal * itsNormVal;
      }    
//
// Un-normalize the fit and model
//
      if (itsIsNormalized) {
         normalizeSolution();
         normalizeModels(1);
      }
//
// A valid solution includes non-convergence
//
      itsValidSolution = True;
   } catch (AipsError x) {
      itsErrorMessage = String("Fitting failed because ") + x.getMesg();
      status = Fit2D::FAILED;
   } end_try;
//
   return status;
}



void Fit2D::normalizeSolution()
{
   const uInt nModels = itsFunction.nFunctions();
   uInt iStart;
//
   for (uInt i=0; i<nModels; i++) {
//
// The solution vector contains only adjustable parameters
//
      Vector<Double> sol = getSolution(iStart, i);
//
// The mask shows which parameters were fixed and which adjustable
//
      Vector<Bool> mask = itsFunction.function(i)->getAvailableParamMasks();

      Fit2D::Types type = (Fit2D::Types)itsTypeList(i);
//
      if (type==Fit2D::LEVEL) {
         itsLogger << LogIO::SEVERE << "Fit2D - Level fitting not yet implemented" << LogIO::POST;
      } else if (type==Fit2D::DISK) {
         itsLogger << LogIO::SEVERE << "Fit2D - Disk fitting not yet implemented" << LogIO::POST;
      } else if (type==Fit2D::GAUSSIAN) {
         uInt idx = 0;
         if (mask(0)) {                         // Value
            sol(idx) = sol(idx) * itsNormVal;
            idx ++;
         }
         if (mask(1)) {                         // x cen
            sol(idx) = sol(idx) * itsNormPos;
            idx++;
         }
         if (mask(2)) {                         // y cen
            sol(idx) = sol(idx) * itsNormPos;
            idx++;
         }
         if (mask(3)) {                         // axis width
            sol(idx) = sol(idx) * itsNormPos;
            idx++;
         }
// 
// Replace in main solution vector
//
         for (uInt j=0; j<sol.nelements(); j++) {
            itsSolution(iStart+j) = sol(j);
         }
      }
   }
}


Vector<Double> Fit2D::getSolution(uInt& iStart, uInt which) 
{
// 
// Loop over models and figure out where the model of
// interest starts in the solution vector. Returns
// only adjustable parameters
//
   iStart = 0;
   for (uInt i=0; i<which; i++) {
//
// See how many adjustable parameters this model has
//
      uInt nP = itsFunction.function(i)->getParameters().nelements();
      iStart += nP;
   }
//
// Find the number of available parameters for the model of interest
//
   uInt nP = itsFunction.function(which)->getParameters().nelements();
   if (itsSolution.nelements() < iStart+nP) {
      itsLogger << LogIO::SEVERE 
                << "Fit2D::getSolution - solution vector is not long enough; did you call function fit ?"    
                << LogIO::POST;
   }
//
   Vector<Double> sol(nP);
   for (uInt i=0; i<nP; i++) {
      sol(i) = itsSolution(iStart+i);
   }
   return sol;
}



Vector<Double> Fit2D::getAvailableSolution()  const
//
// Returns available parameters (adjustable plus fixed)
{
   if (!itsValidSolution) {
      Vector<Double> tmp;
      return tmp;
   }   
//
// Find the mask and parameters for the SumFunction.
// Recover these when the mask is False (fixed)
// otherwise recover the solution
//
   const uInt nParams = itsFunction.nAvailableParams(); 
   Vector<Double> sol2(nParams);
   uInt idx = 0;
   for (uInt i=0; i<nParams; i++) {
      if (itsFunction.getAvailableParamMask(i)) {
         sol2(i) = itsSolution(idx);
         idx++;
      } else {
         sol2(i) = itsFunction.getAvailableParam(i).value();
      }
   }
   return sol2;
} 

