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
  itsValid(false),
  itsValidSolution(false),
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




uint32_t Fit2D::addModel (Fit2D::Types type,
                      const Vector<double>& parameters,
                      const Vector<bool>& parameterMask)
{
   const uint32_t nModels = itsTypeList.nelements() + 1;
   itsTypeList.resize(nModels,true);
//
   if (type==Fit2D::LEVEL) {
	   ConstantND<AutoDiff<double> > myconst(2);
	   myconst[0] = AutoDiff<double>(parameters(0), 1, 0);
	   myconst.mask(0) = parameterMask(0);
	   itsFunction.addFunction(myconst);
	   itsTypeList(nModels-1) = Fit2D::LEVEL;
   } else if (type==Fit2D::DISK) {
      itsLogger << "Fit2D - Disk fitting not yet implemented" <<
	LogIO::EXCEPTION;
   } else if (type==Fit2D::PLANE) {
	   HyperPlane<AutoDiff<double> > plane(3);
	   if (parameters.nelements() != 3) {
		   itsLogger << "Fit2D - illegal number of parameters in addModel" <<
		   LogIO::EXCEPTION;
	   }

   } else if (type==Fit2D::GAUSSIAN) {
// 
// Create functional
//
      Gaussian2D<AutoDiff<double> > gauss2d;
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
      int32_t ii = Gaussian2D<float>::HEIGHT;
      gauss2d[ii] = AutoDiff<double>(parameters(0), gauss2d.nparameters(), ii);   // flux
      gauss2d.mask(ii) = parameterMask(0);

      ii = Gaussian2D<float>::XCENTER;
      gauss2d[ii] = AutoDiff<double>(parameters(1), gauss2d.nparameters(), ii);   // x
      gauss2d.mask(ii) = parameterMask(1);

      ii = Gaussian2D<float>::YCENTER;
      gauss2d[ii] = AutoDiff<double>(parameters(2), gauss2d.nparameters(), ii);   // y
      gauss2d.mask(ii) = parameterMask(2);

      ii = Gaussian2D<float>::YWIDTH;
      gauss2d[ii] = AutoDiff<double>(parameters(3), gauss2d.nparameters(), ii);   // major
      gauss2d.mask(ii) = parameterMask(3);

      ii = Gaussian2D<float>::RATIO;
      double ratio = parameters(4) / parameters(3);
      gauss2d[ii] = AutoDiff<double>(ratio, gauss2d.nparameters(), ii);           // ratio
      gauss2d.mask(ii) = parameterMask(4);

      ii = Gaussian2D<float>::PANGLE;
      double pa = paToGauss2D(parameters(5));
      piRange(pa);
      gauss2d[ii] = AutoDiff<double>(pa, gauss2d.nparameters(), ii);              // p.a.
      gauss2d.mask(ii) = parameterMask(5);
//
// Add it to function we are going to fit
//
      itsFunction.addFunction(gauss2d);
      itsTypeList(nModels-1) = Fit2D::GAUSSIAN;
   }
   itsValid = true;
   return nModels - 1;
}



uint32_t Fit2D::addModel (Fit2D::Types type,
                      const Vector<double>& parameters)
{
   Vector<bool> parameterMask(parameters.nelements(),true);
   return addModel(type, parameters, parameterMask);
}


uint32_t Fit2D::nModels() const
{
  return itsFunction.nFunctions();
}


Vector<bool> Fit2D::convertMask (const String mask,
                                   Fit2D::Types type)
{
   Vector<bool> parameterMask;
   String cmask = mask;
   cmask.downcase();
   if (type==Fit2D::LEVEL) {
	   parameterMask.resize(1);
	   parameterMask = true;
	   if (cmask.contains("l")) {
		   parameterMask(0) = false;
	   }
   } else if (type==Fit2D::DISK || type==Fit2D::GAUSSIAN) {
      parameterMask.resize(6);
      parameterMask = true;
      if (cmask.contains("f")) {
    	  parameterMask(0) = false;
      }
      if (cmask.contains("x")) {
    	  parameterMask(1) = false;
      }
      if (cmask.contains("y")) {
    	  parameterMask(2) = false;
      }
      if (cmask.contains("a")) {
    	  parameterMask(3) = false;
      }
      if (cmask.contains("b")) {
    	  parameterMask(4) = false;
      }
      if (cmask.contains("p")) {
    	  parameterMask(5) = false;
      }
   }
   return parameterMask;
}

uint32_t Fit2D::nParameters(Fit2D::Types type)
{
   uint32_t n = 0;
   if (type==Fit2D::LEVEL) {
      throw (AipsError("Fit2D - Level fitting not yet implemented"));
   } else if (type==Fit2D::DISK) {
      throw (AipsError("Fit2D - Disk fitting not yet implemented"));
   } else if (type==Fit2D::GAUSSIAN) {
      n = 6;
   }
   return n;
}

Fit2D::ErrorTypes Fit2D::residual(Array<float>& resid, Array<float>& model,
                                  const MaskedLattice<float>& data)
{
   Array<float> pixels = data.get(true);
   return residual(resid, model, pixels);
}

Fit2D::ErrorTypes Fit2D::residual(Array<float>& resid, Array<float>& model,
                                  const Lattice<float>& data)
{
   Array<float> pixels = data.get(true);
   return residual(resid, model, pixels);
}

void Fit2D::setIncludeRange (double minVal, double maxVal)
{
   itsPixelRange.resize(2);
   itsPixelRange(0) = min(minVal, maxVal);
   itsPixelRange(1) = max(minVal, maxVal);
   itsInclude = true;
}

void Fit2D::setExcludeRange (double minVal, double maxVal)
{
   itsPixelRange.resize(2);
   itsPixelRange(0) = min(minVal, maxVal);
   itsPixelRange(1) = max(minVal, maxVal);
   itsInclude = false;
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


Fit2D::Types Fit2D::type(uint32_t which) 
{
   if (which >= itsFunction.nFunctions()) {
      itsLogger << "Fit2D::type - illegal model index" << LogIO::EXCEPTION;
   }
   return (Fit2D::Types)itsTypeList(which);
}
 


Vector<double> Fit2D::availableSolution ()  const
//
// Conversion of Gaussian models from axial ratio
// to minor axis is done
//
{
   const uint32_t nF = itsFunction.nFunctions();
   Vector<double> sol(itsFunction.nparameters());
   for (uint32_t i=0, l=0; i<nF; i++) {
      Vector<double> sol2 = availableSolution(i).copy();
      for (uint32_t j=0; j<sol2.nelements(); j++) sol(l++) = sol2(j);
   }
   return sol;
} 
   
Vector<double> Fit2D::availableSolution (uint32_t which)  const
// 
//  For Gaussian models, convert axial ratio to minor axis
//  and fiddle position angle to be that of the major axis,
//  positive +x -> +y
// 
{
   if (!itsValidSolution) {
      Vector<double> tmp;
      return tmp;
   }
//
   if (which >= itsFunction.nFunctions()) {
      itsLogger << "Fit2D::availableSolution - illegal model index" <<
	LogIO::EXCEPTION;
   }
//
   uint32_t iStart;
   Vector<double> sol = availableSolution(iStart, which).copy();
//
// Convert Gaussian solution axial ratio to major/minor axis.
// sol2(3) may be the major or minor axis after fitting.
// The solution may have a negative axial ratio
//
   if (itsTypeList(which)==Fit2D::GAUSSIAN) {
      int32_t iY = Gaussian2D<float>::YWIDTH;
      int32_t iR = Gaussian2D<float>::RATIO;
      int32_t iPA = Gaussian2D<float>::PANGLE;
//
      double other = abs(sol(iY) * sol(iR));
      double ywidth = abs(sol(iY));
      double major, minor, pa;
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

Vector<double> Fit2D::availableSolution(uint32_t& iStart, uint32_t which)  const
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
   uint32_t nP = itsFunction.function(which).nparameters();
   if (itsSolution.nelements() < iStart+nP) {
     itsLogger << LogIO::SEVERE 
	       << "Fit2D::availableSolution - "
       "solution vector is not long enough; did you call function fit ?"
	       << LogIO::POST;
   }
//
   Vector<double> sol(nP);
   for (uint32_t i=0; i<nP; i++) sol(i) = itsSolution(iStart+i);
   return sol;
}


Vector<double> Fit2D::availableErrors ()  const
//
// Conversion of Gaussian models from axial ratio
// to minor axis is done
//
{
   const uint32_t nF = itsFunction.nFunctions();
   Vector<double> errors(itsFunction.nparameters());
   for (uint32_t i=0, l=0; i<nF; i++) {
      Vector<double> errors2 = availableErrors(i).copy();
       for (uint32_t j=0; j<errors2.nelements(); j++) errors(l++) = errors2(j);
   }
   return errors;
} 
   
Vector<double> Fit2D::availableErrors (uint32_t which)  const
// 
//  For Gaussian models, convert axial ratio to minor axis
// 
{
   if (!itsValidSolution) {
      Vector<double> tmp;
      return tmp;
   }
//
   if (which >= itsFunction.nFunctions()) {
      itsLogger << "Fit2D::availableErrors - illegal model index" <<
	LogIO::EXCEPTION;
   }
//
   uint32_t iStart;
   Vector<double> errors = availableErrors (iStart, which).copy();
   Vector<double> sol = availableSolution (iStart, which).copy();
//
// Convert Gaussian solution axial ratio to major/minor axis.
// ratio  = other / YWIDTH
// sol(4) = other / sol(3)
//
//
   if (itsTypeList(which)==Fit2D::GAUSSIAN) {
      int32_t iY = Gaussian2D<float>::YWIDTH;
      int32_t iR = Gaussian2D<float>::RATIO;
//
      double other = abs(sol(iY) * sol(iR));
      double yWidth = abs(sol(iY));
      double ratio = abs(sol(iR));
//
      double sigRatio = errors(iR);
      double sigYWidth = errors(iY);

/*
// Use standard propagation of errors to get error in other

      double f1 = sigRatio * sigRatio / ratio / ratio;
      double f2 = sigYWidth * sigYWidth / yWidth / yWidth;
      double sigOther = other * sqrt(f1 + f2);
*/

// The propagation errors are too large.  Try using
// same fractional error...  I need to find better ways
// to deal with the Gaussian as wdith and ratio 

      double sigOther = other * (sigRatio/ratio);

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

Vector<double> Fit2D::availableErrors (uint32_t& iStart, uint32_t which)  const
{
// 
// Loop over models and figure out where the model of
// interest starts in the solution vector. 
//
   iStart = itsFunction.parameterOffset(which);
//
// Find the number of available parameters for the model of interest
//
   uint32_t nP = itsFunction.function(which).nparameters();
   if (itsErrors.nelements() < iStart+nP) {
     itsLogger << LogIO::SEVERE 
	       << "Fit2D::availableErrors - "
       "errors vector is not long enough; did you call function fit ?"
	       << LogIO::POST;
   }
//
   Vector<double> errors(nP,0.0);
   for (uint32_t i=0; i<nP; i++) errors(i) = itsErrors(iStart+i);
   return errors;
}




String Fit2D::errorMessage () const
{
   return itsErrorMessage;
}


uint32_t Fit2D::numberIterations() const
{
   return itsFitter.currentIteration();
}

double Fit2D::chiSquared () const
{
   if (!itsValidSolution) {
      return -1.0;
   }
   return itsChiSquared;
}


uint32_t Fit2D::numberPoints () const
{
   return itsNumberPoints;
}



Vector<double> Fit2D::getParams(uint32_t which) const
//
// Recover the available parameters for this model
// from the SumFunction
//
{
   Vector<double> params(itsFunction.function(which).nparameters());
   for (uint32_t i=0; i<params.nelements(); i++) {
     params(i) =
       itsFunction.function(which).parameters().getParameters()(i).value();
   }
   return params;
}

void Fit2D::setParams(const Vector<double> &params, uint32_t which)
//
// Set the available parameters for this model
// from the SumFunction
//
{
  for (uint32_t i=0; i<params.nelements(); i++) {
    itsFunction[itsFunction.parameterOffset(which)+i].value() = params[i];
  }
}

// Private functions

Fit2D::ErrorTypes Fit2D::fitData(const Vector<double>& values, 
                                 const Matrix<double>& pos,
                                 const Vector<double>& weights)
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
      itsValidSolution = true;
   } catch (std::exception& x) {
      itsErrorMessage = String("Fitting failed because ") + x.what();
      status = Fit2D::FAILED;
   } 
//
   return status;
}



void Fit2D::piRange (double& pa) const
//
// Put angle in radians in range +/- pi
//
{
    MVAngle pa2(pa);
    pa2();
    pa = pa2.radian();
}


} //# NAMESPACE CASACORE - END

