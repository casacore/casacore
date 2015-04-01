//# Fit2D.cc: Class to fit 2D objects to a Lattice or Array
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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
//#   $Id$

#include <casacore/lattices/LatticeMath/Fit2D.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/scimath/Functionals/Gaussian2D.h>
#include <casacore/scimath/Functionals/ConstantND.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
  itsHasSigma(other.itsHasSigma),
  itsInclude(other.itsInclude),
  itsPixelRange(other.itsPixelRange.copy()),   // Copy semantics
  itsFunction(other.itsFunction),        
  itsSolution(other.itsSolution.copy()), 
  itsErrors(other.itsErrors.copy()),     
  itsChiSquared(other.itsChiSquared),
  itsErrorMessage(other.itsErrorMessage),
  itsNumberPoints(other.itsNumberPoints),
  itsTypeList(other.itsTypeList.copy())        // Copy semantics
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
      itsHasSigma = other.itsHasSigma;
      itsInclude = other.itsInclude;
      itsPixelRange = other.itsPixelRange.copy();   // Copy semantics
      itsFunction = other.itsFunction;          
      itsSolution = other.itsSolution.copy();   
      itsErrors = other.itsErrors.copy();       
      itsChiSquared = other.itsChiSquared;
      itsErrorMessage = other.itsErrorMessage;
      itsNumberPoints = other.itsNumberPoints;
      itsTypeList = other.itsTypeList.copy();       // Copy semantics
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
	   ConstantND<AutoDiff<Double> > myconst(2);
	   myconst[0] = AutoDiff<Double>(parameters(0), 1, 0);
	   myconst.mask(0) = parameterMask(0);
	   itsFunction.addFunction(myconst);
	   itsTypeList(nModels-1) = Fit2D::LEVEL;
   } else if (type==Fit2D::DISK) {
      itsLogger << "Fit2D - Disk fitting not yet implemented" <<
	LogIO::EXCEPTION;
   } else if (type==Fit2D::PLANE) {
	   HyperPlane<AutoDiff<Double> > plane(3);
	   if (parameters.nelements() != 3) {
		   itsLogger << "Fit2D - illegal number of parameters in addModel" <<
		   LogIO::EXCEPTION;
	   }

   } else if (type==Fit2D::GAUSSIAN) {
// 
// Create functional
//
      Gaussian2D<AutoDiff<Double> > gauss2d;
      if (parameters.nelements() != gauss2d.nparameters()) {
         itsLogger << "Fit2D - illegal number of parameters in addModel" <<
	   LogIO::EXCEPTION;
      }
      if (parameterMask.nelements() != gauss2d.nparameters()) {
         itsLogger <<
	   "Fit2D - illegal number of mask parameters in addModel" <<
	   LogIO::EXCEPTION;
      }
//
// Set parameters.  0 (flux), 1 (x), 2 (y), 3 (FWHM major), 4 (FWHM minor), 
// 5 (pa - in radians).  Convert p.a. from positive +x -> +y
// to +y -> -x for Gaussian2D.  Note that fixing the ratio is not
// the same as fixing the minor axis, which is what the Fit2D interface
// claims to do.  I don't know how to solve this presently.
//
      Int ii = Gaussian2D<Float>::HEIGHT;
      gauss2d[ii] = AutoDiff<Double>(parameters(0), gauss2d.nparameters(), ii);   // flux
      gauss2d.mask(ii) = parameterMask(0);

      ii = Gaussian2D<Float>::XCENTER;
      gauss2d[ii] = AutoDiff<Double>(parameters(1), gauss2d.nparameters(), ii);   // x
      gauss2d.mask(ii) = parameterMask(1);

      ii = Gaussian2D<Float>::YCENTER;
      gauss2d[ii] = AutoDiff<Double>(parameters(2), gauss2d.nparameters(), ii);   // y
      gauss2d.mask(ii) = parameterMask(2);

      ii = Gaussian2D<Float>::YWIDTH;
      gauss2d[ii] = AutoDiff<Double>(parameters(3), gauss2d.nparameters(), ii);   // major
      gauss2d.mask(ii) = parameterMask(3);

      ii = Gaussian2D<Float>::RATIO;
      Double ratio = parameters(4) / parameters(3);
      gauss2d[ii] = AutoDiff<Double>(ratio, gauss2d.nparameters(), ii);           // ratio
      gauss2d.mask(ii) = parameterMask(4);

      ii = Gaussian2D<Float>::PANGLE;
      Double pa = paToGauss2D(parameters(5));
      piRange(pa);
      gauss2d[ii] = AutoDiff<Double>(pa, gauss2d.nparameters(), ii);              // p.a.
      gauss2d.mask(ii) = parameterMask(5);
//
// Add it to function we are going to fit
//
      itsFunction.addFunction(gauss2d);
      itsTypeList(nModels-1) = Fit2D::GAUSSIAN;
   }
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
   String cmask = mask;
   cmask.downcase();
   if (type==Fit2D::LEVEL) {
	   parameterMask.resize(1);
	   parameterMask = True;
	   if (cmask.contains("l")) {
		   parameterMask(0) = False;
	   }
   } else if (type==Fit2D::DISK || type==Fit2D::GAUSSIAN) {
      parameterMask.resize(6);
      parameterMask = True;
      if (cmask.contains("f")) {
    	  parameterMask(0) = False;
      }
      if (cmask.contains("x")) {
    	  parameterMask(1) = False;
      }
      if (cmask.contains("y")) {
    	  parameterMask(2) = False;
      }
      if (cmask.contains("a")) {
    	  parameterMask(3) = False;
      }
      if (cmask.contains("b")) {
    	  parameterMask(4) = False;
      }
      if (cmask.contains("p")) {
    	  parameterMask(5) = False;
      }
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

 
Fit2D::ErrorTypes Fit2D::fit(const MaskedLattice<Float>& data, const Lattice<Float>& sigma)
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
      itsLogger << "Fit2D::fit - Region must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   Array<Bool> mask = data.getMask(True);
//
// Do fit
//
   if (sigma.ndim()==0) {
      Array<Float> sigma2;
      return fit(pixels, mask, sigma2);
   } else {
      Array<Float> sigma2 = sigma.get(True);
      return fit(pixels, mask, sigma2);
   }
}




Fit2D::ErrorTypes Fit2D::fit(const Lattice<Float>& data, 
                             const Lattice<Float>& sigma)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
//
   Array<Float> pixels = data.get(True);
   IPosition shape = pixels.shape();
   if (shape.nelements() !=2) {
      itsLogger << "Fit2D::fit - Region must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   Array<Bool> mask;
//
   if (sigma.ndim()==0) {
      Array<Float> sigma2;
      return fit(pixels, mask, sigma2);
   } else {
      Array<Float> sigma2 = sigma.get(True);
      return fit(pixels, mask, sigma2);
   }
}


Fit2D::ErrorTypes Fit2D::fit(const Array<Float>& data, 
                             const Array<Float>& sigma)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   if (data.ndim() !=2) {
      itsLogger << "Fit2D::fit - Array must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   if (sigma.nelements() !=0) {
      if (!data.shape().isEqual(sigma.shape())) {
         itsLogger << "Fit2D::fit - Sigma and pixel arrays must "
	   "have the same shape" << LogIO::EXCEPTION;
      }
   }
//
   Matrix<Double> pos;
   Vector<Double> values;
   Vector<Double> weights;
   Array<Bool> mask;
   if (!selectData (pos, values, weights, data, mask, sigma)) {
      itsErrorMessage = String("There were no selected data points");
      return Fit2D::NOGOOD;
   }
//
   return fitData(values, pos, weights);
}


Fit2D::ErrorTypes Fit2D::fit(const Array<Float>& data,
                             const Array<Bool>& mask, 
                             const Array<Float>& sigma)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   if (data.ndim() !=2) {
      itsLogger << "Fit2D::fit - Array must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   if (mask.nelements() !=0) {
      if (!data.shape().isEqual(mask.shape())) {
         itsLogger << "Fit2D::fit - Mask and pixel arrays must "
	   "have the same shape" << LogIO::EXCEPTION;
      }
   }
   if (sigma.nelements() !=0) {
      if (!data.shape().isEqual(sigma.shape())) {
         itsLogger << "Fit2D::fit - Sigma and pixel arrays must "
	   "have the same shape" << LogIO::EXCEPTION;
      }
   }

   Matrix<Double> pos;
   Vector<Double> values;
   Vector<Double> weights;
   if (!selectData (pos, values, weights, data, mask, sigma)) {
      itsErrorMessage = String("There were no selected data points");
      return Fit2D::NOGOOD;
   }

   return fitData(values, pos, weights);

}

Fit2D::ErrorTypes Fit2D::residual(
		Array<Float>& resid, Array<Float>& model,
        const Array<Float>& data, Int xOffset, int yOffset
) const {
   ThrowIf(
      ! itsValid,
      "No models have been set - use function addModel"
   );
   if (!itsValidSolution) {
      return Fit2D::FAILED;
   }

   ThrowIf(data.ndim() !=2, "Array must be 2-dimensional");
   IPosition shape = data.shape();

   if (resid.nelements() ==0) {
       resid.resize(shape);
   } else {
       ThrowIf(
          ! shape.isEqual(resid.shape()),
          "Residual and pixel arrays must be the same shape"
       );
   }
   if (model.nelements() ==0) {
       model.resize(shape);
   }
   else {
       ThrowIf(
    	!shape.isEqual(model.shape()),
          "Residual and pixel arrays must "
       );
    }

// Create a functional with the solution (no axis conversion
// necessary because functional interface takes axial ratio)

   PtrHolder<Function<AutoDiff<Double> > > sumFunction(itsFunction.clone());
   for (uInt i=0; i<itsSolution.nelements(); i++) {
	   (*sumFunction)[i] = itsSolution[i];
   }
   IPosition loc(2);
   for (Int j=0; j<shape(1); j++) {
     loc(1) = j;
      for (Int i=0; i<shape(0); i++) {
         loc(0) = i;
         model(loc) = (*sumFunction)(Double(i + xOffset), Double(j + yOffset)).value();
         resid(loc) = data(loc) - model(loc);
      }
   }
   return Fit2D::OK;
}

Fit2D::ErrorTypes Fit2D::residual(Array<Float>& resid, Array<Float>& model,
                                  const MaskedLattice<Float>& data)
{
   Array<Float> pixels = data.get(True);
   return residual(resid, model, pixels);
}

Fit2D::ErrorTypes Fit2D::residual(Array<Float>& resid, Array<Float>& model,
                                  const Lattice<Float>& data)
{
   Array<Float> pixels = data.get(True);
   return residual(resid, model, pixels);
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

void Fit2D::resetRange()
{
   itsPixelRange.resize(0);
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


Fit2D::Types Fit2D::type(const String& type)
{
   String t0 = type;
   String tmp = upcase(t0.at(0,1));
   Fit2D::Types tmp2;
   if (tmp==String("L")) {
      tmp2 = Fit2D::LEVEL;
   } else if (tmp==String("D")) {
      tmp2 = Fit2D::DISK;
   } else if (tmp==String("G")) {
      tmp2 = Fit2D::GAUSSIAN;
   } else {
      throw(AipsError("Fit2D::type - illegal model type"));
   }
   return tmp2;
}


Fit2D::Types Fit2D::type(uInt which) 
{
   if (which >= itsFunction.nFunctions()) {
      itsLogger << "Fit2D::type - illegal model index" << LogIO::EXCEPTION;
   }
   return (Fit2D::Types)itsTypeList(which);
}
 


Vector<Double> Fit2D::availableSolution ()  const
//
// Conversion of Gaussian models from axial ratio
// to minor axis is done
//
{
   const uInt nF = itsFunction.nFunctions();
   Vector<Double> sol(itsFunction.nparameters());
   for (uInt i=0, l=0; i<nF; i++) {
      Vector<Double> sol2 = availableSolution(i).copy();
      for (uInt j=0; j<sol2.nelements(); j++) sol(l++) = sol2(j);
   }
   return sol;
} 
   
Vector<Double> Fit2D::availableSolution (uInt which)  const
// 
//  For Gaussian models, convert axial ratio to minor axis
//  and fiddle position angle to be that of the major axis,
//  positive +x -> +y
// 
{
   if (!itsValidSolution) {
      Vector<Double> tmp;
      return tmp;
   }
//
   if (which >= itsFunction.nFunctions()) {
      itsLogger << "Fit2D::availableSolution - illegal model index" <<
	LogIO::EXCEPTION;
   }
//
   uInt iStart;
   Vector<Double> sol = availableSolution(iStart, which).copy();
//
// Convert Gaussian solution axial ratio to major/minor axis.
// sol2(3) may be the major or minor axis after fitting.
// The solution may have a negative axial ratio
//
   if (itsTypeList(which)==Fit2D::GAUSSIAN) {
      Int iY = Gaussian2D<Float>::YWIDTH;
      Int iR = Gaussian2D<Float>::RATIO;
      Int iPA = Gaussian2D<Float>::PANGLE;
//
      Double other = abs(sol(iY) * sol(iR));
      Double ywidth = abs(sol(iY));
      Double major, minor, pa;
      if (ywidth > other) {
         major = ywidth;
         minor = other;
         pa = sol(iPA);
      } else {
         major = other;
         minor = ywidth;
         pa = sol(iPA) + C::pi_2;   // pa off by 90
      }
//
// Convert the position angle from positive 
// +y -> -x to   positive +x -> +y
//
      sol(3) = major;
      sol(4) = minor;
      sol(5) = paFromGauss2D(pa);
      piRange(sol(5));
   }
//
   return sol;
}

Vector<Double> Fit2D::availableSolution(uInt& iStart, uInt which)  const
{
// 
// Loop over models and figure out where the model of
// interest starts in the solution vector. Returns
// all (adjustable + fixed) parameters directly as solved
// for; no axial or position angle conversion 
//
   iStart = itsFunction.parameterOffset(which);
//
// Find the number of available parameters for the model of interest
//
   uInt nP = itsFunction.function(which).nparameters();
   if (itsSolution.nelements() < iStart+nP) {
     itsLogger << LogIO::SEVERE 
	       << "Fit2D::availableSolution - "
       "solution vector is not long enough; did you call function fit ?"
	       << LogIO::POST;
   }
//
   Vector<Double> sol(nP);
   for (uInt i=0; i<nP; i++) sol(i) = itsSolution(iStart+i);
   return sol;
}


Vector<Double> Fit2D::availableErrors ()  const
//
// Conversion of Gaussian models from axial ratio
// to minor axis is done
//
{
   const uInt nF = itsFunction.nFunctions();
   Vector<Double> errors(itsFunction.nparameters());
   for (uInt i=0, l=0; i<nF; i++) {
      Vector<Double> errors2 = availableErrors(i).copy();
       for (uInt j=0; j<errors2.nelements(); j++) errors(l++) = errors2(j);
   }
   return errors;
} 
   
Vector<Double> Fit2D::availableErrors (uInt which)  const
// 
//  For Gaussian models, convert axial ratio to minor axis
// 
{
   if (!itsValidSolution) {
      Vector<Double> tmp;
      return tmp;
   }
//
   if (which >= itsFunction.nFunctions()) {
      itsLogger << "Fit2D::availableErrors - illegal model index" <<
	LogIO::EXCEPTION;
   }
//
   uInt iStart;
   Vector<Double> errors = availableErrors (iStart, which).copy();
   Vector<Double> sol = availableSolution (iStart, which).copy();
//
// Convert Gaussian solution axial ratio to major/minor axis.
// ratio  = other / YWIDTH
// sol(4) = other / sol(3)
//
//
   if (itsTypeList(which)==Fit2D::GAUSSIAN) {
      Int iY = Gaussian2D<Float>::YWIDTH;
      Int iR = Gaussian2D<Float>::RATIO;
//
      Double other = abs(sol(iY) * sol(iR));
      Double yWidth = abs(sol(iY));
      Double ratio = abs(sol(iR));
//
      Double sigRatio = errors(iR);
      Double sigYWidth = errors(iY);

/*
// Use standard propagation of errors to get error in other

      Double f1 = sigRatio * sigRatio / ratio / ratio;
      Double f2 = sigYWidth * sigYWidth / yWidth / yWidth;
      Double sigOther = other * sqrt(f1 + f2);
*/

// The propagation errors are too large.  Try using
// same fractional error...  I need to find better ways
// to deal with the Gaussian as wdith and ratio 

      Double sigOther = other * (sigRatio/ratio);

/*
cerr << "ratio, major, other = " << ratio << ", " << yWidth << ", " << other << endl;
cerr << "sigRatio, sigMajor, sigOther = " << sigRatio << ", " << sigYWidth << ", " << sigOther << endl;
*/
      if (yWidth > other) {

// ywidth is major, other is minor

         errors(4) = sigOther;          // minor
         errors(3) = sigYWidth;         // major
      } else {

// ywidth is minor, other is major

         errors(4) = sigYWidth;         // minor
         errors(3) = sigOther;          // major
      }
   }   
//
   return errors;
}

Vector<Double> Fit2D::availableErrors (uInt& iStart, uInt which)  const
{
// 
// Loop over models and figure out where the model of
// interest starts in the solution vector. 
//
   iStart = itsFunction.parameterOffset(which);
//
// Find the number of available parameters for the model of interest
//
   uInt nP = itsFunction.function(which).nparameters();
   if (itsErrors.nelements() < iStart+nP) {
     itsLogger << LogIO::SEVERE 
	       << "Fit2D::availableErrors - "
       "errors vector is not long enough; did you call function fit ?"
	       << LogIO::POST;
   }
//
   Vector<Double> errors(nP,0.0);
   for (uInt i=0; i<nP; i++) errors(i) = itsErrors(iStart+i);
   return errors;
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
   Vector<Double> params(itsFunction.function(which).nparameters());
   for (uInt i=0; i<params.nelements(); i++) {
     params(i) =
       itsFunction.function(which).parameters().getParameters()(i).value();
   }
   return params;
}

void Fit2D::setParams(const Vector<Double> &params, uInt which)
//
// Set the available parameters for this model
// from the SumFunction
//
{
  for (uInt i=0; i<params.nelements(); i++) {
    itsFunction[itsFunction.parameterOffset(which)+i].value() = params[i];
  }
}


Vector<Double> Fit2D::estimate(Fit2D::Types type,
                               const MaskedLattice<Float>& data) 
{
   if (data.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Lattice must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   Array<Float> pixels = data.get(True);
   Array<Bool> mask = data.getMask(True);
   return estimate(type, pixels, mask);
}

Vector<Double> Fit2D::estimate(Fit2D::Types type, 
                               const Lattice<Float>& data) 
{
   if (data.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Lattice must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   Array<Float> pixels = data.get(True);
   Array<Bool> mask(pixels.shape(),True);
   return estimate(type, pixels, mask);
}

Vector<Double> Fit2D::estimate(Fit2D::Types type,
                               const Array<Float>& data)
{
   if (data.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Array must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   Array<Bool> mask(data.shape(),True);
   return estimate(type, data, mask);
}


Vector<Double> Fit2D::estimate(Fit2D::Types type,
                               const Array<Float>& data,
                               const Array<Bool>& mask) 
// 
// Work out an initial estimate to the solution using Bob Sault's 
// probabilistic approach from Miriad imfit.for   Only works
// for single models.  Honours and inclusion/exclusion pixel range
//
// PA sign convention in pixel coordinate is +x -> +y is positive
//
{
   if (type!=Fit2D::GAUSSIAN  && type==Fit2D::DISK) {
      itsLogger << "Only Gaussian and disk models are currently supported" <<
	LogIO::EXCEPTION;
   }
//
   Vector<Double> parameters;
   IPosition shape = data.shape();
   if (shape.nelements() !=2) {
      itsLogger << "Fit2D::estimate - Array must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   if (mask.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Mask must be 2-dimensional" << 
	LogIO::EXCEPTION;
   }
// 
// Find min and max
//
   MaskedArray<Float> pixels(data, mask);
   Float minVal, maxVal;   
   IPosition minPos(2), maxPos(2);
   minMax(minVal, maxVal, minPos, maxPos, pixels);
//
// For the purposed of the estimate, chuck away pixels
// below abs(5%) of the peak
//
   Float clip = 0.05 * max(abs(minVal), abs(maxVal));
//
// Accumulate sums.  Array indexing is not fast.
//
   Int includeThem = 0;
   if (itsPixelRange.nelements()==2) {
     if (itsInclude) {
        includeThem = 1;
     } else {
        includeThem = 2;
     }
   }
//
   Double P, XP, YP, XYP, XXP, YYP;
   Float t, fac, SP;
   P = XP = YP = XYP = XXP = YYP = 0.0;
   SP = 0.0;
//
   IPosition pos(2);
   Float ri, rj;
   uInt nPts = 0;
   for (Int j=0; j<shape(1); j++) {
     for (Int i=0; i<shape(0); i++) {
        pos(0) = i; pos(1) = j;
//
        const Float& val = data(pos);
        t = abs(val);
        if (mask(pos) && includeIt(val, itsPixelRange,
				   includeThem) && t>clip) {
           ri = i; rj = j;
//
           SP += val;
           P  += t;                        
           XP += t*ri;
           YP += t*rj;
           XYP += t*ri*rj;
           XXP += t*ri*ri;
           YYP += t*rj*rj;
           nPts++;
         }
      }
   }
   if (nPts==0) {
      itsLogger << LogIO::WARN <<
	"There are not enough good points in the array for a good estimate" <<
	LogIO::POST;
      return parameters;
   }
//
   Double t2;
   if (type==Fit2D::GAUSSIAN || type==Fit2D::DISK) {
      parameters.resize(6);
//
      fac = 4*log(2.0);
      XP  = XP / P;
      YP  = YP / P;
      XYP = XYP / P - XP*YP;
      XXP = XXP / P - XP*XP;
      YYP = YYP / P - YP*YP;
//
      parameters(1) = XP;
      parameters(2) = YP;
//    
      parameters(3)  = sqrt(fac*(XXP + YYP +
                        sqrt( square(XXP-YYP) + 4*square(XYP) )));
      parameters(4) = sqrt(fac*(XXP + YYP -
                       sqrt( square(XXP-YYP) + 4*square(XYP) )));

      t2 = 0.5*atan2(2*XYP,YYP-XXP);
      parameters(5) = paFromGauss2D(-t2);
      piRange(parameters(5));
//
      Float sn = 1.0;
      if (SP<0) sn = -1.0;
      parameters(0) = sn * fac * P / ( C::pi * parameters(3) * parameters(4));
   } else if (type==Fit2D::LEVEL) {
      itsLogger << "Level models are not currently supported" <<
	LogIO::EXCEPTION;
   }
// 
   parameters(3) *= 0.95;   // In case estimate is circular
//
   return parameters;
}


// Private functions

Bool Fit2D::selectData (Matrix<Double>& pos, Vector<Double>& values, 
                        Vector<Double>& weights,
		        const Array<Float>& pixels,
		        const Array<Bool>& mask,
		        const Array<Float>& sigma)
//
// Fish out the unmasked data.
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
// Find first unmasked point
//
   Bool hasMask = True;
   if (mask.nelements()==0) hasMask = False;
   Double minVal(0);
   Double maxVal(0);
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
// Just fill in the position matrix
//
   for (uInt k=0; k<itsNumberPoints; k++) {
      pos(k,0) = locX(k);
      pos(k,1) = locY(k);
   }
//   cout << "Data = " << values << endl;
//   cout << "weights = " << weights << endl;
//   cout << "Pos = " << pos << endl;

   return True;
}

 
Fit2D::ErrorTypes Fit2D::fitData(const Vector<Double>& values, 
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

   itsFitter.setFunction(itsFunction);
   
// Find the solution

   itsChiSquared = 0;
   itsErrorMessage = "";
   Fit2D::ErrorTypes status = Fit2D::OK;
   itsSolution.resize(0);
   try {

// itsSolution and itsErrors holds values and errors for adjustable and fixed parameters

      itsSolution = itsFitter.fit(pos, values, weights);
      itsErrors = itsFitter.errors();
      if(!itsFitter.converged()) {
         itsErrorMessage = String("The fit did not converge");
         status = Fit2D::NOCONVERGE;
      }
//
// Find chi-squared.  
//
      itsChiSquared = itsFitter.chiSquare();
//
// A valid solution includes non-convergence
//
      itsValidSolution = True;
   } catch (AipsError x) {
      itsErrorMessage = String("Fitting failed because ") + x.getMesg();
      status = Fit2D::FAILED;
   } 
//
   return status;
}



void Fit2D::piRange (Double& pa) const
//
// Put angle in radians in range +/- pi
//
{
    MVAngle pa2(pa);
    pa2();
    pa = pa2.radian();
}


} //# NAMESPACE CASACORE - END

